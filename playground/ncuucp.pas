{  $Id$

   OpenXP UUCP netcall class
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus F"aerber
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

{ OpenXP UUCP netcall class }
unit ncuucp;

interface

uses ncmodem,timer,fidoglob,xpglobal,classes;

type
  TUUCPNetcall = class(TModemNetcall)
  private
    function    TestBRK:  Boolean;

  protected
    function    InitHandshake : Char;
    procedure   FinalHandshake;

  public
    function  PerformNetcall: Integer;
    property  UserBrk       : Boolean read TestBRK;

  public
    UUremote      : string;  (* remote system name              *)
    UUname        : string;  (* local system name               *)
    UUprotos      : string;  (* UUCP protocols to use           *)

    CommandFile   : string;  (* path to command file            *)
    DownSpool     : string;  (* path for received spool files   *)
    FilePath      : string;  (* path for other received files   *)

    MaxWinSize    : byte;    (* maximum UUCP window size        *)
    MaxPacketSize : word;    (* maximum packet size             *)
    VarPacketSize : boolean; (* use variable packet size        *)
    ForcePktSize  : boolean; (* use forced packet size          *)

    SizeNego      : boolean; (* use size negotiation            *)
    MaxFSize      : LongInt; (* size limit for incoming packets *)
  end;

implementation

uses zmodem, ipcclass, resource, sysutils, typeform, debug, montage, crc,
xpdiff, objcom, fileio, inout, keys, xpnetcall, netcall;

(*
  UUCP protocol implementation

  planned class hierarchy:

  TUUCProtocol
   +-- TUUCProtocolSimple
   |     +-- TUUCProtocolT      (t protocol)
   |     +-- TUUCProtocolE      (e protocol)
   |     \-- TUUCProtocolGFZ
   |           +-- TUUCProtocolG      (gGv protocols)
   |           \-- TUUCProtocolFZ     (fz protocols)
   +-- TUUCProtocolA (not yet implemented)
   \-- TUUCProtocolI (not yet implemented)

  TUUCPNetcall will connect to the UUCP host, do the initial handshake
  (protocol selection) and create a TUUCProtocol object, which it will then
  transfer control to.

  TUUCProtocolSimple is a common base class for the tgGfez protocols, which have
  a very similar structure (i.e. send files, become slave, receive).
  More advanced protocols (such as UUCP-i) would be derieved directly from
  TUUCProtocol and only implement RunProtocol (besides con-/destructor).
*)

type TUUCProtocol = class
  private
                FNetcall: TUUCPNetcall;
    function    FCommObj: TPCommObj;
    function    FBreak  : Boolean;

  protected
    property    Netcall:  TUUCPNetcall read FNetcall;
    property    CommObj:  TPCommObj    read FCommObj;
    property    UserBrk:  Boolean      read FBreak;

  public
    constructor InitProtocol(caller: tuucpnetcall);
    destructor  ExitProtocol; virtual;
    function    RunProtocol: integer; virtual; abstract;
  end;

function TUUCProtocol.FCommObj:TPCommObj;
begin
  if assigned(FNetcall) then
    result:=FNetcall.CommObj
  else
    result:=nil;
end;

function TUUCProtocol.FBreak: Boolean;
begin
  result:=Netcall.TestBRK;
end;

type TUUCProtocolSimple = class(TUUCProtocol)
  public
    constructor InitProtocol(caller: tuucpnetcall);
    destructor  ExitProtocol; override;
    function    RunProtocol: integer;  override;

  protected
    (* abstract protocol implementation functions *)
    function SendCommand(s:string):boolean; virtual; abstract;
    function GetCommand:string; virtual; abstract;
    function SendFile(fn:string; offset:longint):shortint; virtual; abstract;
    function RecFile(fn:string):Shortint; virtual; abstract;

    (* common protocol functions *)
    function RepeatGetcommand(c:char):string;
    function RepeatSendFile(fn:string; offset:longint):boolean;
    function RepeatRecFile(fn:string; size:longint):boolean;

    (* helper functions *)
    procedure WriteTransfer;
    procedure ResetTransdat(blksize:word);
    procedure StartFile(send:boolean; fn:string; size:longint);
    procedure ShowFtype(var data; len:word);

    (* high level protocol functions *)
    (* master: process command file *)
    function Master:boolean;
    (* slave: receive commands *)
    function Slave:boolean;

  protected
    (* global vars *)
    transdata : record
      connstart   : longint;      { Ticks: CONNECT }
      files       : longint;
      filesize    : longint;      { aktuelle Datei }
      filestart   : longint;
      transferred : longint;      { davon uebertrg. }
      blocksize   : word;
      errors      : longint;
      total       : longint;
      totalfrom   : longint;      { Ticks: Sende/Empfangsstart }
      sending     : boolean;
    end;

    starttime     : longint;
    onlinetime    : longint;
    resopen       : boolean;
    dlogopen      : boolean;
    ulogopen      : boolean;
    filetrans     : boolean;      { fuer rerrmsg }
    ShowTime      : boolean;
    FileStarted   : boolean;

    FileRetries: shortint;
end;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      (* uu_parerr  = 1; *)
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;
      uu_xfererr = 7;  { send or receive error }
      uu_unknown = 15; { unknown error }

      fileOK        =   0;    { Datei ok          }
      fileRepeat    =   1;    { Datei wiederholen }
      fileError     =   2;    { Datei - Abbruch   }

{ --- UUCP protocol exceptions -------------------------------- }

type EUUCProtocol = class (ENetcall);     (* generic protocol problem  *)
type EUUCProtFile = class (EUUCProtocol); (* generic file xfer problem *)

{ --- UUCP command/response parser ---------------------------- }
{ - - classes - - - - - - - - - - - - - - - - - - - - - - - - - }

type TUUCPCommand = object (* class is too much *)
public
  cmd, src, dest, user, opts, temp, ntfy, exec: string;
  mode: integer; size,restart: longint;
  procedure Parse(s:string);
end;

type TUUCPResponse = object (* class is too much *)
public
  cmd: string;  ok:  boolean;
  reason, mode:integer; restart: longint;
  procedure Parse(s:string);
  function  ReasonMsg:string;
end;

{ - - helper functions  - - - - - - - - - - - - - - - - - - - - }

function ModeVal(s:string):integer;
begin
  result:=CVal(s);
  if result > 511 {==0666 okt} then
    result:=iVal(s); { wrong number format }
end;

function FPosVal(s:string):LongInt;
begin
  if s<>'' then result:=CVal(s)
  else result:=-1;
end;

{ - - parser implementations  - - - - - - - - - - - - - - - - - }

procedure TUUCPCommand.Parse(s:string);
var tmp: string;

  function NextToken:string;
  begin
    result:=GetTokenC(s,' '#9#10);
  end;

begin
  cmd  := ''; src  := ''; dest := ''; user := ''; opts := '';
  temp := ''; ntfy := ''; exec := '';
  mode :=  0; size := -1; restart := -1;

  cmd  := NextToken;

  if (cmd<>'S') and (cmd<>'R') and (cmd<>'X') and
     (cmd<>'E') and (cmd<>'H') then exit; (* not supported *)

  src  := NextToken;
  dest := NextToken;
  user := NextToken;

  opts := NextToken;

  if LeftStr(opts,1)='-' then opts:=mid(opts,2)
  else begin opts:=''; exit; end;     (* illegal options *)

  (* X from to user -options *)

  (* R from to user -options size [Taylor UUCP] *)
  (* R from to user -options dummy mode owner temp restart [SRV4] *)
  if cmd='R' then
  begin
    tmp := NextToken;

    if tmp <> 'dummy' then
    begin (* SRV4 *)
      mode := ModeVal(NextToken);
      ntfy := NextToken;
      temp := NextToken;
      restart := FPosVal(NextToken);
    end else
      size := FPosVal(NextToken);
  end else

  (* S from to user -options temp mode notify size *)
  (* E from to user -options temp mode notify size command *)
  if (cmd='S') or (cmd='E') then
  begin
    temp := NextToken;
    mode := ModeVal(NextToken);
    ntfy := NextToken;
    size := FPosVal(NextToken);

    if cmd='E' then
      exec := NextToken;
  end;
end;

procedure TUUCPResponse.Parse(s:string);

  function NextToken:string;
  begin
    result:=GetTokenC(s,' '#9#10);
  end;

begin
  cmd:=''; ok:=false;
  reason:=0; mode:=0; restart:=-1;

  cmd:= LeftStr(s,1);
  ok := Copy(s,2,1)='Y';
  reason := IVal(Mid(NextToken,3));

  if cmd='S' then
    restart:=FPosVal(NextToken)
  else

  if cmd='R' then begin
    mode   := ModeVal(NextToken);
    restart:= FPosVal(NextToken);
  end;
end;

function TUUCPResponse.ReasonMsg;
begin
  case reason of
    02: result:='permission denied or file error';
    04: result:='unable to create temporary file';
    05: result:='temporary file could not be moved into the final location';
    06: result:='file too large to transfer at the moment';
    07: result:='file too large to ever transfer';
    08: result:='file already received previously';
    09: result:='unable to open another channel';
    10: result:='file too large';
    else
        result:='unknown error';
  end;
end;

{ --- Individual protocol implementations --------------------- }

{$I ncuucp-t.inc}
{ $I ncuucp-g.inc}
{ $I ncuucp-e.inc}
{ $I ncuucp-fz.inc}

{ --- TUUCProtocol base class -------------------------------------- }

constructor TUUCProtocol.InitProtocol(caller: tuucpnetcall); begin FNetcall:=caller; end;
destructor  TUUCProtocol.ExitProtocol; begin end;

{ --- UUCP-Verteiler ------------------------------------------------ }

constructor TUUCProtocolSimple.InitProtocol(caller: TUUCPNetcall);
begin
  inherited InitProtocol(caller);

  with transdata do begin
    connstart := 0;
    files := 0;
    filesize := 0;
    filestart := 0;
    transferred := 0;
    blocksize := 0;
    errors := 0;
    total := 0;
    totalfrom := 0;
    sending := false;
  end;

  starttime     := 0;
  onlinetime    := 0;
  resopen       := false;
  dlogopen      := false;
  ulogopen      := false;
  filetrans     := false;      { fuer rerrmsg }
  ShowTime      := true;
  FileStarted   := false;

  FileRetries := 0;
end;

destructor TUUCProtocolSimple.ExitProtocol;
begin
end;

function TUUCProtocolSimple.RepeatGetcommand(c:char):string;
var n : integer;
    s : string;
begin
  n:=10;              { 10 x 1 Min. Timeout }
  repeat
    s:=GetCommand;
    if (s<>'') and (LeftStr(s,1)<>c) then
      Netcall.Log(lcError,'unexpected command: '+s);
    dec(n);
  until (LeftStr(s,1)=c) or (n=0) or (not CommObj.Carrier);
  RepeatGetcommand:=s;
end;

function TUUCProtocolSimple.RepeatSendFile(fn:string; offset:longint):boolean;
var sfresult : shortint;
begin
  if offset <0 then offset:=0;
  repeat
    startfile(true,extractfilename(fn),_filesize(fn));
    sfresult:=SendFile(fn,offset);
    if sfresult=fileRepeat then
      Netcall.log(lcStop,'error sending file - resending file');
  until (sfresult=fileOK) or (sfresult=fileError);
  if sfresult<>fileOK then
    Netcall.log(lcStop,'error sending file');
  RepeatSendFile:=(sfresult=fileOK);
end;

function TUUCProtocolSimple.RepeatRecFile(fn:string; size:longint):boolean;    { true = ok }
var rfresult : shortint;
    FileRetries: integer;
begin
  FileRetries:=0;
  repeat
    inc(FileRetries);
    startfile(false,extractfilename(fn),size);
    rfresult:=RecFile(fn);
    if rfresult=fileRepeat then
      Netcall.log(lcStop,'error receiving file - repeating file');
  until (rfresult=fileOK) or (rfresult=fileError);
  if rfresult<>fileOK then
    Netcall.log(lcStop,'error receiving file');
  RepeatRecFile:=(rfresult=fileOK);
end;

procedure TUUCProtocolSimple.StartFile(send:boolean; fn:string; size:longint);
begin
  inc(transdata.files);

  if send then begin
    Netcall.LogTxFile(fn);
(*    Netcall.WriteIPC(mcVerbose,'Sending %s (%d bytes)',fn,size);  *)
  end else begin
    Netcall.LogRxFile(fn);
(*    Netcall.WriteIPC(mcVerbose,'Receiving %s (%d bytes)',fn,size);  *)
  end;

// if ParDebug then begin
//    if send then
   //  Netcall.WriteIPC('Sending '+fn);
//    wrdebug('sending ')
//    else
   //   Netcall.WriteIPC('Receiving '+fn);
//    wrdebug('receiving ');
//  wrdebug('file: '+fn+' ');
//  end
//  else begin
//    attrtxt(col.colmailer);
//    clwin(mx+1,mx+lwdt-1,my+4,my+11);   { Dateifenster neu aufbauen }
//    with UWin do begin
//      if fn<>'' then
//        wrt(mx+2,my+4,iifs(send,senden,empfangen)); { 'Senden' / 'Empfangen' }
//      wrt(mx+27,my+4,dateien);                    { 'Dateien' }
//      wrt(mx+2,my+7,uebertragen);                 { 'uebertragen' }
//      wrt(mx+2,my+8,gesamt);                      { 'gesamt' }
//      wrt(mx+2,my+6,dateigroesse);                { 'Dateigroesse' }
//      wrt(mx+2,my+10,restzeit);                   { 'Restzeit' }
//      wrt(mx+27,my+6,blockgroesse);               { 'Blockgroesse' }
//      wrt(mx+27,my+7,durchsatz);                  { 'Durchsatz          cps' }
//      wrt(mx+27,my+8,dgesamt);                    { 'gesamt             cps' }
//      wrt(mx+27,my+10,fehler);                    { 'Fehler' }
//      end;
//    wrt(mx+2,my+12,sp(lwdt-2));
//    wrt(mx+2,my+hgh,sp(lwdt-2));
//    attrtxt(col.colmailerhi);
//    wrt(mx+13,my+4,fn);
//    wrt(mx+39,my+4,strsn(transdata.files,5));
//    end;
//  with transdata do begin
//    sending:=send;
//    filesize:=size; transferred:=0; errors:=0;
//    filestart:=ticker;
//    WriteTransfer;
//    end;

  FileStarted:=true;
end;

procedure tuucprotocolsimple.ResetTransdat(blksize:word);
begin
  transdata.total:=0;
  transdata.totalfrom:=ticker;
  transdata.files:=0;
  transdata.blocksize:=blksize;
end;

procedure TUUCProtocolSimple.ShowFtype(var data; len:word);
var s    : string;
    user : string;
    p,p2 : byte;
begin
  setlength(s,min(255,len));
  move(data,s[1],length(s));
  p:=cpos(#10,s);
  if p>0 then TruncStr(s,p-1);
  if LeftStr(s,3)='#! ' then
    if copy(s,4,5)='rnews' then s:=getres2(2300,60) else      { 'ungepacktes Newspaket' }
    if copy(s,4,8)='cunbatch' then s:=getres2(2300,61) else   { 'gepacktes Newspaket (compress)' }
    if copy(s,4,8)='funbatch' then s:=getres2(2300,62) else   { 'gepacktes Newspaket (freeze)' }
    if copy(s,4,8)='gunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    if copy(s,4,8)='zunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    if copy(s,4,8)='bunbatch' then s:=getres2(2300,69) else   { 'gepacktes Newspaket (bzip2)' }
    s:=''
  else
    if LeftStr(s,5)='HELO '  then s:=getres2(2300,64) else   { 'ungepacktes Mailpaket' }
    if LeftStr(s,2)=#$1f#$9d then s:=getres2(2300,65) else   { 'gepackte Datei (compress)' }
    if LeftStr(s,2)=#$1f#$9f then s:=getres2(2300,66) else   { 'gepackte Datei (freeze) }
    if LeftStr(s,2)=#$1f#$8b then s:=getres2(2300,67) else   { 'gepackte Datei (gzip) }
    if LeftStr(s,2)=#$42#$5a then s:=getres2(2300,70) else   { 'gepackte Datei (bzip2) }
    begin
      LoString(s);
      if (LeftStr(s,5)='>from') or (LeftStr(s,4)='from') then begin
        p:=pos('remote from',s);
        if p>0 then begin
          delete(s,1,blankpos(s));
          user:=LeftStr(s,blankpos(s)-1);
          p2:=length(user);
          while (p2>0) and (user[p2]<>'!') do dec(p2);
          if p2>0 then begin
            s:=getres2(2300,68)+mid(user,p2+1)+'@';
            user:=LeftStr(user,p2-1);
            p2:=length(user);
            while (p2>0) and (user[p2]<>'!') do dec(p2);
            s:=s+mid(user,p2+1);
            end
          else begin
            p:=pos('remote from',s);
            s:=trim(mid(s,p+11));
            p:=blankpos(s);
            if p<>0 then TruncStr(s,p-1);
            s:=getres2(2300,68)+user+'@'+s;
            end;
          end
        else s:='Mail';
        end
      else s:={'Mail'} '';  { UUCP-Filerequest! }
      end;
(* if s<>'' then rmsg(s); *)
end;

procedure TUUCProtocolSimple.WriteTransfer;
var sec,cps : longint;
begin
(*  if send then begin *)
(*  Netcall.LogTxFile(fn);
    Netcall.WriteIPC(mcInfo,'Sent %s (%d bytes)',fn,size); *)
(*  end else begin *)
(*  Netcall.LogRxFile(fn);
    Netcall.WriteIPC(mcInfo,'Received %s (%d bytes)',fn,size); *)
(*  end; *)

(* if ParDebug then exit; *)
//  attrtxt(col.colmailerhi);
//  with transdata do begin
//    if filesize>0 then begin
//      gotoxy(mx+13,my+6); write(filesize:8);
//      end;
//    gotoxy(mx+13,my+7); write(transferred:8);
//    gotoxy(mx+13,my+8); write(total:8);
//    gotoxy(mx+39,my+6); write(blocksize:5);
//    gotoxy(mx+39,my+10); write(errors:5);
//    sec:=secondsfrom(filestart);
//    cps:=0;
//    if sec>0 then begin
//      cps:=transferred div sec;
//      gotoxy(mx+39,my+7);
//      write(cps:5);
//      if (filesize>0) and (cps>20) then begin
//        gotoxy(mx+13,my+10);
//        write(timeform((filesize-transferred)div cps));
//        end;
//      end;
//    sec:=secondsfrom(totalfrom);
//    if sec>0 then begin
//      if files>1 then
//        cps:=total div sec;
//      if cps>20 then begin
//        gotoxy(mx+39,my+8);
//        write(cps:5);
//        end;
//      end;
//    if filesize>0 then
//      wrt(mx+2,my+12,
//          dup(min(lwdt-3,system.round((lwdt-3)*(transferred/filesize))),'±'));
//    end;
//  WriteOnline;
  if UserBrk then;
end;

function TUUCPNetcall.TestBRK: Boolean;
begin
  if FGotUserBreak then
    result:=true
  else if keypressed and (readkey=#27) then begin
    result:=true;
    FGotUserBreak:=true;
(* rmsg(getres2(2300,51)); *)
    Log(lcExit,'User break.');
    WriteIPC(mcInfo,'User break - aborting...',[0]);
  end else
    result:=false;
end;

{ --- TUUCPNetcall ------------------------------------------------------ }

(*
    UUprotos      : string = "gG";

    MaxWinSize    : byte = 7;
    MaxPacketSize : word = 64;
    VarPacketSize : boolean = false;
    ForcePktSize  : boolean = false;

    SizeNego      : boolean = false;

    FilePath      : string = 'FILES\';
    commandfile   : string = '';
*)

function TUUCPNetcall.PerformNetcall: Integer;
var pprot : TUUCProtocol;
begin
  result:=el_noconn; pprot:=nil;
  if not Connect then exit;

  case InitHandshake of
    't': pprot:=TUUCProtocolT.InitProtocol(self);
(*  'g','G','v': pprot := TUUCProtocolG.InitProtocol(self); *)
(*
    'e':         pprot := TUUCProtocolE.InitProtocol(self);
    'f':         pprot := TUUCProtocolFZ.InitProtocol(self,true);
    'z':         pprot := TUUCProtocolFZ.InitProtocol(self,false);
*)
  else Log(lcError,'Protocol unimplemented'); exit; end;

  if assigned(pprot) then begin
    result := pprot.RunProtocol;
    pprot.ExitProtocol;
  end else begin
    WriteIPC(mcError,'UUCP protocol initialization failed',[0]);
    Log(lcError     ,'UUCP protocol initialization failed');
    result := el_nologin;
  end;

  Disconnect;
end;

function TUUCPNetcall.InitHandshake:char;
var n,i : integer;                            { --- uucp - Init-Handshake }
    s   : string;
    ti  : longint;
    proto: char;

  function GetUUStr:string;        { uucp-String empfangen }
  var recs: string;
  begin
    repeat
      if CommObj.CharAvail then recs:=recs+CommObj.GetChar;
    until (* timeout(true) or *) UserBRK or
          ((recs<>'') and (recs[length(recs)] in [#0,#4,#10,#13]));

    if LeftStr(recs,1)=^P then delete(recs,1,1);
    if recs='' then GetUUStr:='' else GetUUStr:=LeftStr(recs,length(recs)-1);
  end;

begin
  result:=#0;
  mdelay(500);

  WriteIPC(mcVerbose,'UUCP Initial Handshake',[0]);

  CommObj.PurgeInBuffer;
  CommObj.SendString(^P'S'+UUName+iifs(SizeNego,' -N','')+#0,false);

  for n := 1 to 5 do begin
    s:=GetUUStr;
    if s='' then exit;
    if s[1]='R' then break;
  end;

  if LeftStr(s,3)<>'ROK' then begin
    if s='RLOGIN' then s:=s+' - wrong login name';
    if not CommObj.Carrier then
      Log(lcError,'connection terminated by remote site')
    else
      Log(lcStop,'got "'+s+'" - aborting');
    exit;
    end;
  Log(lcInfo,'UUCP connection established');

  SizeNego:=(s='ROKN');         { size negotiation }
  if SizeNego then begin
 (*  rmsg(getres2(2300,38));  { 'Using size negotiation' } *)
    log(lcInfo,'using size negotiation');
    WriteIPC(mcInfo,'UUCP connection established w/ size negitiation',[0]);
  end else
    WriteIPC(mcInfo,'UUCP connection established',[0]);

  for n := 1 to 5 do begin
    s:=GetUUStr; dec(n);
  (* if timeout(true) then break; *)
  (* if break then exit; *)
    if s[1]='P' then break;
  end;

  if LeftStr(s,1)<>'P' then begin
    Log(lcStop,'got '+s+' - aborting');
    WriteIPC(mcError,'Got %s, aborting',[s]);
    exit;
  end;

  delete(s,1,1);
  Log('~','remote protocols: '+s);
  if not multipos(uuprotos,s) then begin
    CommObj.SendString(^P'UN'#0,false);
(*  rmsg(getreps2(2300,37,s));    { 'no common protocol (remote supports %s)' } *)
    Log(lcStop,'no common protocol');
    WriteIPC(mcError,'no common protocol - remote supports: %s',[s]);
    result:=#0;
    exit;
    end;
  for i:=length(uuprotos) downto 1 do     { bestes Protokoll ermitteln }
    if cpos(uuprotos[i],s)>0 then proto:=uuprotos[i];
  CommObj.SendString(^P'U'+proto+#0,false);
  Log(lcInfo,'selected protocol'+sp(cpos(proto,s))+'^');
  WriteIPC(mcError,'selected protocol: %s',[proto]);

  result:=proto;
end;


procedure TUUCPNetcall.FinalHandshake;           { --- uucp - Handshake vor Hangup }
var b : byte;
begin
  if not CommObj.Carrier then begin
    CommObj.SendString(^P'OOOOOO',false);
    CommObj.PurgeInBuffer;
  end;
end;

{----- TUUCProtocolSimple: Mails/News/Dateien senden, Dateien anfordern ---------------- }

function TUUCProtocolSimple.RunProtocol: integer;
begin
  if not Master then
    result:= uu_senderr
  else
  if not Slave then
    result:= uu_recerr
  else
    result:=uu_ok;
end;

{----- TUUCProtocolSimple.Master: send files and file requests ------------------------- }

function U2DOSfile(var s:string):string;
var i : integer;
    b : byte;
begin (* FIXME: What happens if length(s)<6 ? *)
  s:=s[1]+'-'+RightStr(s,5);
  b:=0;
  for i:=0 to 3 do            { Schreibweise in einem Byte codieren }
    if (s[i+4]>='A') and (s[i+4]<='Z') then
      inc(b,1 shl i);
  result:=s+hex(b,1);
end;


function TUUCProtocolSimple.Master:Boolean;
var cin : text;
    s   : string;        { unparsed UUCP command }
    c   : TUUCPCommand;  { parsed UUCP command }
    a   : string;        { unparsed UUCP response }
    r   : TUUCPResponse; { parsed UUCP response }
    oops: boolean;

  procedure LocalFile(var src:string; down: boolean );
  var p: integer;
  begin
    if not multipos(_MPMask,src) then begin
      (* kein Pfad => gleiches Verzeichnis wie CommandFile *)
      (*              auîerdem Schreibweise anpassen       *)
      if down then
        src:=U2DOSFile(src)
      else begin
        (* fn:= fn[length(fn)-4]+'-'+RightStr(fn,4)+'.OUT'; *)
        src:=RightStr(src,5)+'.OUT';
        Insert('-',src,2);
      end;
      src := ExtractFilePath(Netcall.CommandFile)+src;
    end else
      if down then
        UniqueDownloadName(src,Netcall.FilePath)
    {$IFNDEF UnixFS}
      else
        (* nur unter DOS: Pfadseparatoren wieder anpassen *)
        for p:=1 to length(src) do
          if src[p]='/' then src[p]:='\'
    {$ENDIF};
  end;

  (* Handle S/E command as master *)

  procedure Do_SE;
  begin
    LocalFile(c.src,false);

    if fileexists(c.src) then
    begin
      Netcall.Log('+','sending '+c.src+' as '+c.dest);
      Netcall.WriteIPC(mcVerbose,'Sending %s as %s (%d bytes)',[c.src,c.dest,c.size]);

      if SendCommand(s) then
      begin
        r.Parse(RepeatGetCommand(c.cmd[1])); { SY/SN or EY/EN }
        if r.ok then begin
          if RepeatSendFile(c.src,r.restart) then begin
            if r.restart <> 0 then Netcall.WriteIPC(mcVerbose,'Sending %s (%d bytes, restart at %d)',[c.src,c.size,r.restart]);
            r.Parse(RepeatGetcommand('C'));
            if r.ok then
            begin
              Netcall.WriteIPC(mcInfo,'Sent %s as %s (%d bytes)',[c.src,c.dest,c.size]);
              Netcall.Log('*','sent file'); (* xxx bytes yyy cps *)
              DeleteFile(c.src);
            end else
            begin
              Netcall.WriteIPC(mcInfo,'Sending %s failed: %s',[c.src,r.reasonmsg]);
              Netcall.Log(lcStop,'remote error: '+r.reasonmsg+' (#'+strs(r.reason)+')');
            end;
          end { !SendFile } else
          begin
            Netcall.WriteIPC(mcInfo,'Remote refused %s: %s',[c.src,r.reasonmsg]);
            Netcall.Log(lcStop,'remote refuses file: '+r.reasonmsg+' (#'+strs(r.reason)+')');
          end;
        end; { r.ok }
      end; { !SendCommand }
    end else
    begin
      Netcall.Log(lcError,'File not found: '+c.src);
      Netcall.WriteIPC(mcError,'File not found: %s',[c.src]);
    end;
  end;

  (* handle R command as master *)

  procedure Do_R;
  begin
    LocalFile(c.dest,true);

    if ValidFileName(c.dest) then
    begin
      Netcall.Log('+','requesting '+c.src+' as '+c.dest);

      if SendCommand(s) then
      begin
        r.Parse(RepeatGetCommand('R')); { RY/RN }
        if r.ok then
        begin
          if RepeatRecFile(c.src,r.restart) then begin
            SendCommand('CY');
            Netcall.Log('*','received file'); (* xxx bytes yyy cps *)
          end else begin
            SendCommand('CN');
            Netcall.Log(lcStop,'receive error');
          end; { RecFile }
        end else begin { !r.ok }
          Netcall.Log(lcStop,'remote refuses to send file: '+r.reasonmsg+' (#'+strs(r.reason)+')');
        end; {!r.ok}
      end; { !SendCommand }
    end else
    begin
      Netcall.Log(lcError,'invalid filename: '+c.src);
      Netcall.WriteIPC(mcError,'Invalid filename: %s',[c.src]);
    end;

  end;

begin { TUUCProtocolSimple.Master:Boolean; }
  oops:=false;

  if (not (Netcall.CommandFile='')) and (_FileSize(Netcall.CommandFile)<>0) then
  begin
    Netcall.WriteIPC(mcInfo,'UUCICO running as master: %s',[Netcall.CommandFile]);
    assign(cin,Netcall.CommandFile); reset(cin);

    while not (oops or eof(cin)) do
    begin
      if not CommObj.Carrier then begin oops:=true; break; end;
      if     UserBrk         then begin oops:=true; break; end;

      readln(cin,s);
      c.Parse(s);

      if (c.cmd='S') or (c.cmd='E') then Do_SE else
      if c.cmd='R'                  then Do_R  else
      begin
        Netcall.Log(lcError,'Unknown/unsupported UUCP command: '+s);
        Netcall.WriteIPC(mcError,'Unknown/unsupported UUCP command: %s',[s]);
      end;
    end; { while not eof(t) }

    close(cin);
    erase(cin);
  end;

  result:=not oops;
end;

function TUUCProtocolSimple.Slave:Boolean;
var s   : string;       { unparsed incoming command }
    c   : TUUCPCommand; { parsed incoming command }
    oops: boolean;

  function BecomeSlave:boolean; (* was named GetSlave in orig. XP ;-))) *)
  var r: TUUCPResponse;
      a: string;
  begin
    if SendCommand('H') then begin
      r.Parse(RepeatGetCommand('H')); { HY/HN }
      if r.cmd<>'' then begin
        if r.ok then begin
          Netcall.Log('+','no files to receive');
          SendCommand('HY');
        end else
          Netcall.Log('+','remote has data for you');
        result:=not r.ok;
        exit; (* quit *)
      end;
    end;

    oops:=true;
    result:=false;
  end;

  (* handle S command as slave *)

  procedure Do_S;
    var s:string;
  begin
    s:=iifs(c.dest<>'',c.dest,c.src);

    if s='' then begin                     { S ohne Dateiname }
      Netcall.Log(lcError,'got S command without file name');
      if not SendCommand('SN2') then oops:=true;
    end else
    if (c.size>0) and (Netcall.maxfsize>0) and (c.size>1024*Netcall.maxfsize) then begin
      Netcall.Log(lcError,'file too large ('+StrS(c.size div 1000)+' kB)');
      if SendCommand('SN7') then exit;
    end else
    if SendCommand('SY 0x0') then
    begin
      if ((LeftStr(s,2)='D.') or (LeftStr(s,2)='X.')) and not Multipos(_MPMask,s) then
        s:=AddDirSepa(Netcall.DownSpool)+U2DOSfile(s)
      else
        UniqueDownloadName(s,Netcall.FilePath);

      Netcall.Log('+','receiving '+c.src+' as '+s);
      if RepeatRecFile(s,c.size) then begin
        Netcall.Log('*','received file'); (* xxx bytes yyy cps zzz errors *)
        SendCommand('CY');
        exit;
      end;
    end; {!SendCommand}
    oops:=true;
  end;

  (* handle H command as slave = hangup sequenc = hangup sequencee *)

  procedure Do_H;
  begin
    if SendCommand('HY') then
      GetCommand;         { = HY }
  end;

begin
  oops:=false;

  if not BecomeSlave then 
    Netcall.WriteIPC(mcInfo,'Remote has no files to receive',[0])
  else 
  begin
    Netcall.WriteIPC(mcInfo,'UUCIO running as slave.',[0]);
    while not oops do
    begin
      c.Parse(GetCommand);
      if c.cmd = '' then begin oops:=true; break; end;
  
      case c.cmd[1] of
        'R': begin SendCommand('RN2'); end;       (* refuse recv requests *)
        'X': begin SendCommand('XN'); end;        (* refuse xfer requests *)
        'E': begin SendCommand('EN'); end;        (* refuse exec requests *)
        'S': begin Do_S; end;
        'H': begin Do_H; break; end;
        else begin SendCommand(LeftStr(c.cmd,1)+'N'); end;
      end;
    end;
  end;

  result:=not oops;
end;

end.

{
  $Log$
  Revision 1.6  2001/03/13 00:23:05  cl
  - fixes for UUCP netcalls (first working version)

  Revision 1.5  2001/02/28 22:35:32  cl
  - UUCP connection, login and initial handshake working

  Revision 1.2  2001/02/21 17:45:53  cl
  - first things for TUUCPNetcall

  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground, was uucico.pas
  Revision 1.6  2000/11/18 14:07:48  fe
  Replaced exist() with fileexists().

  Revision 1.5  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.4  2000/11/02 21:27:04  fe
  bzip2 support added.

  Revision 1.3  2000/10/27 16:14:29  fe
  uucico notduerftig uebersetzbar gemacht.
}

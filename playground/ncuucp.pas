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

{$i xpdefine.inc}

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

    UUremote      : string;
    UUname        : string;
    UUprotos      : string;

    FilePath      : string;
    CommandFile   : string;

    MaxWinSize    : byte;
    MaxPacketSize : word;
    VarPacketSize : boolean;
    ForcePktSize  : boolean;

    SizeNego      : boolean;
    MaxFSize      : LongInt;
  end;

implementation

uses zmodem, ipcclass, resource, sysutils, typeform, debug, montage, crc,
xpdiff, objcom, fileio, inout, keys, xpnetcall;

(*
  UUCP protocol implementation

  planned class hierarchy:

  TUUCProtocol
   +-- TUUCProtocolSimple
   |     +-- TUUCProtocolG 	(gGv protocols)
   |     +-- TUUCProtocolE      (e protocol)
   |     \-- TUUCProtocolFZ     (fz protocols)
   +-- TUUCProtocolA (not yet implemented)
   \-- TUUCProtocolI (not yet implemented)

  TUUCPNetcall will connect to the UUCP host, do the initial handshake
  (protocol selection) and create a UUCProtocol object, which it will then
  transfer control to.  

  TUUCProtocolSimple is a common base class for the gGfez protocols, which have
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
    property	Netcall:  TUUCPNetcall read FNetcall; 
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
    destructor  ExitProtocol; virtual; override;
    function    RunProtocol: integer; virtual; override;

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

{ --- UUCP command parser ------------------------------------- }

type TUUCPCommand = object (* class is too much *)
public
  cmd, src, dest, user, opts, temp, ntfy, exec: string;
  mode: integer; size,restart: longint; 
  constructor Parse(s:string);
end;

type TUUCPResponse = object (* class is too much *)
public
  cmd: string;  ok:  boolean;
  reason, mode:integer; restart: longint;
  constructor Parse(s:string);
  function ReasonMsg:string;
end;

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

constructor TUUCPCommand.Parse(s:string);
var tmp: string;

  function NextToken:string; 
  begin 
    result:=GetToken(s,' '#9#10); 
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

constructor TUUCPResponse.Parse(s:string);

  function NextToken:string; 
  begin 
    result:=GetToken(s,' '#9#10); 
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
  repeat
    startfile(true,extractfilename(fn),_filesize(fn));
    sfresult:=SendFile(fn,offset);
    if sfresult=fileRepeat then
      Netcall.log(lcStop,'error - resending file');
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
      Netcall.log(lcStop,'error - repeating file');
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
  UserBrk;
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
    WriteIPC(mcInfo,'User break - aborting...',[]);
  end else
    result:=false;
end;

{ --- TUUCPNetcall ------------------------------------------------------ }

(*
    UUprotos	  : string = "gG";
    
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
    '*': pprot:=nil;
(*  'g','G','v': pprot := TUUCProtocolG.InitProtocol(self); *)
(*   
    'e':         pprot := TUUCProtocolE.InitProtocol(self);
    'f':         pprot := TUUCProtocolFZ.InitProtocol(self,true);
    'z':	 pprot := TUUCProtocolFZ.InitProtocol(self,false); 
*)
  else exit end;

  if assigned(pprot) then begin
    result := pprot.RunProtocol;
    pprot.ExitProtocol;
  end else begin
    WriteIPC(mcError,'UUCP protocol initialization failed',[]);
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

  WriteIPC(mcVerbose,'UUCP Initial Handshake',[]);

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
    WriteIPC(mcInfo,'UUCP connection established w/ size negitiation',[]);
  end else
    WriteIPC(mcInfo,'UUCP connection established',[]);

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
  else if not Slave then
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
var t   : text;
    s	: string;        { unparsed UUCP command }
    c   : TUUCPCommand;  { parsed UUCP command }
    a   : string;        { unparsed UUCP response }
    r   : TUUCPResponse; { parsed UUCP response }
    oops: boolean;
  
  procedure LocalFile(var src:string; down: boolean );
  var p: integer;
    

  begin
    if not multipos(_MPMask,src) then begin
      (* kein Pfad => gleiches Verzeichnis wie CommandFile *)
      (*              auﬂerdem Schreibweise anpassen       *)
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

  (* Handle S command as master *)

  procedure Do_S;
  begin
    LocalFile(c.src,false);
 
    if fileexists(c.src) then 
    begin
      Netcall.Log('+','sending '+c.src+' as '+c.dest);

      if SendCommand(s) then 
      begin
        r.Parse(RepeatGetCommand('S')); { SY/SN }
	if r.ok then begin
          if RepeatSendFile(c.src,r.restart) then begin
            r.Parse(RepeatGetcommand('C'));
	    if r.ok then
	      Netcall.Log('*','sent file') (* xxx bytes yyy cps *)
            else
	      Netcall.Log(lcStop,'remote error: '+r.reasonmsg+' (#'+strs(r.reason)+')');
          end { !SendFile } else
	    Netcall.Log(lcStop,'remote refuses file: '+r.reasonmsg+' (#'+strs(r.reason)+')');
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

  assign(t,Netcall.CommandFile); reset(t);

  while (not oops) and (not eof(t)) do
  begin
    if not CommObj.Carrier then begin oops:=true; break; end;
    if     UserBrk         then begin oops:=true; break; end;
   
    readln(t,s);

    c.Parse(s);    

    if c.cmd='S ' then Do_S else
    if c.cmd='R ' then Do_R else 
    begin
      Netcall.Log(lcError,'Unknown/unsupported UUCP command: '+s);
      Netcall.WriteIPC(mcError,'Unknown/unsupported UUCP command: %s',[s]); 
    end;

  end; { while not eof(^t) }

  close(t);
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
          Netcall.Log('+','HY: no files to receive');
          SendCommand('HY');
	end else
          Netcall.Log('+','HN: remote has data for you');
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
        s:=ExtractFilePath(Netcall.CommandFile)+U2DOSfile(s)
      else
        UniqueDownloadName(s,Netcall.FilePath);

      Netcall.Log('+','receiving '+c.src+' as '+s);
      if RepeatRecFile(c.src,c.size) then begin
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

  if BecomeSlave then while not oops do 
  begin
    c.Parse(GetCommand);
    if c.cmd = '' then begin oops:=true; break; end;

    case c.cmd[1] of 
      'R': begin SendCommand('RN2'); end;	(* refuse file requests *)
      'X': begin SendCommand('XN'); end;	(* refuse exec requests *)
      'S': begin Do_S; end;
      'H': begin Do_H; break; end;
      else begin SendCommand(LeftStr(c.cmd,1)+'N'); end;
    end;

  end;

  result:=not oops;
end;

{$IF 0}
var ok    : boolean;
    s     : string;
    fn    : string;
    c     : char;
    p     : byte;
    ti,tf : longint;
    size  : longint;
    fs    : longint;
    secs  : longint;
    n     : integer;
    source: string;

label ende;

  procedure getfilesize;
  var ss   : string;
      pp,i : byte;
  begin
    ss:=s;
    for i:=1 to 6 do begin
      pp:=blankpos(ss);
      ss:=trim(mid(ss,pp+1));
      end;
    pp:=blankpos(ss);
    if pp>0 then ss:=LeftStr(ss,pp-1);
    size:=Cval(ss);                  { size negotiation - Dateigroesse }
  end;

begin
  ti:=ticker;
  RecFiles:=false;
  if not GetSlave(ok) then begin
    RecFiles:=ok;
    exit;
    end;
  c:=' ';
  WrdLn;
  ResetTransdat(grBufSize);
  n:=5;
  repeat                   { Slave-Schleife fuer eingehende Befehle }
    s:=GetCommand;
    if s='' then
      if n=0 then goto ende
      else dec(n)
    else n:=5;
    if s<>'' then begin
      c:=s[1];
      case c of
        'R' : begin
                logerror('host refuses file request');
                if SendCommand('RN2') then;   { Request-Anforderung }
              end;
        'X' : begin
                logerror('host refuses program execution');
                if SendCommand('XN') then;    { Execute-Anforderung }
              end;
        'S' : begin                  { remote uucp-Sendeanforderung }
                size:=0;
                s:=trim(mid(s,2));
                p:=blankpos(s);
                if p>0 then begin
                  source:=LeftStr(s,p-1);   { Quelldatei auf anderem Rechner }
                  s:=trim(mid(s,p));
                  p:=blankpos(s);
                  if p>0 then begin
                    if SizeNego then getFilesize;
                    s:=LeftStr(s,p-1);
                    end;
                  end;
                if s='' then s:=source;
                if s='' then begin                     { S ohne Dateiname }
                  logerror('got S command without file name');
                  if not SendCommand('SN2') then goto ende;
                  end
                else if (size>0) and (maxfsize>0) and (size>1024*maxfsize)
                then begin
                  logerror('file too large ('+strs(size div 1024)+' KB)');
                  if not SendCommand('SN7') then goto ende;
                  end
                else begin
                  tf:=ticker;
                  if not SendCommand('SY 0x0') then goto ende;
                  if (LeftStr(s,2)='D.') or (LeftStr(s,2)='X.') then begin
                    fn:=XFerDir+U2DOSfile(s);
                    netcall.log('+','receiving '+s+' as '+uppercase(fn));
                    end
                  else begin
                    s:=Unix2DOSfile(s,FilePath);
                    if s='' then s:=Unix2DOSfile(source,FilePath);
                    if s='' then s:='unnamed';
                    fn:=FilePath+s;
                    netcall.log('S','receiving '+s+' as '+uppercase(fn));
                    end;
                  if not RecFile(fn,size) then
                    goto ende;
                  fs:=_filesize(fn);
                  secs:=max(1,secondsfrom(tf));
                  netcall.log('*','received '+strs(fs)+' bytes, '+strs(fs div secs)+
                            ' cps, '+strs(transdata.errors)+' errors');
                  if SendCommand('CY') then;
                  end;
              end;
      else    if c<>'H' then begin
                netcall.log(lcStop,'unknown UUCP command: '+s);
                if SendCommand(c+'N') then;    { ungueltiger Befehl }
              end;
      end;
    end;
  until (c='H') or NoCarrier or break;           { Hangup }
  if c='H' then begin
    RecFiles:=true;
    if SendCommand('HY') then
      s:=GetCommand;         { = HY }
    end;
ende:
  inc(rectime,secondsfrom(ti));
end;

// function fuucico(start:longint; var ende:boolean;
//                 var waittime:integer; var sendtime,rectime:longint):integer;
// begin
//   SizeNego:=sizenego;
//   fillchar(transdata,sizeof(transdata),0);
//   transdata.connstart:=start;
//   InitInterface;
//   if ParDebug then begin             { UUDEBUG.LOG: ucico-Debug-Logfile }
//     new(deblog);
//     assign(deblog^,DebugLog);
//     rewrite(deblog^);
//     dlogopen:=true;
//     end;
//   if uulogfile<>'' then begin        { Temp: XPUUCP.LOG }
//     new(uulog);
//     assign(uulog^,uulogfile);
//     rewrite(uulog^);
//     writeln(uulog^,'----------  ',date,' -> ',boxname);
//     ulogopen:=true;
//     end
//   else
//     uulog:=nil;
//   (* if ParDebug then mcur:=curon
//   else mcur:=curoff; *)
//   ShowWindow;
//   recs:=''; _ende:=false;
//   sendtime:=0; rectime:=0; waittime:=0;
// 
//   if InitHandshake(waittime) then begin     { enthaelt InitProtocol }
//     if SendFiles(CommandFile,sendtime,rectime) then
//       if RecFiles(rectime) then begin
//         fuucico:=uu_ok; _ende:=true; end
//       else fuucico:=uu_recerr
//     else fuucico:=uu_senderr;
//     ExitProtocol;
//     end
//   else
//     fuucico:=uu_nologin;
// 
//   if NoCarrier and not break then
//     LogError('carrier lost');
//   FinalHandshake;
//   rmsg(getres2(2300,50));    { 'Modem auflegen' }
//   Log('+','hanging up');
//   ExitInterface;
//   CloseLogfiles;
//   closewindow;
//   freeres;
//   ende:=_ende;
// end;
 
end.

function TUUCProtocolSimple_SendFiles(CommandFile:string; var sendtime,rectime:longint):boolean;
var t   : ^text;
    s   : string;
    s2  : string;
    sf  : string;
    o   : longint;
    fn  : string;
    ti  : longint;
    p   : byte;
    fs  : longint;
    secs: longint;
    size: longint;

label next,ende;

  procedure addtime(var t:longint);
  begin
    inc(t,secondsfrom(ti));
  end;

  procedure GetRequestFilesize;   { RY Mode Size }
  var p : byte;
  begin
    p:=blankpos(s2);
    if p>0 then s2:=trim(mid(s2,p)) else s:='';    { RY entfernen }
    p:=blankpos(s2);
    if p>0 then s2:=trim(mid(s2,p)) else s:='';    { Mode entfernen }
    p:=blankpos(s2);
    if p=0 then p:=length(s2)+1;
    size:=Cval(LeftStr(s2,p-1));
  end;

begin
  WrdLn;
  SendFiles:=false;
  new(t);
  assign(t^,CommandFile);
  reset(t^);
  ResetTransdat(gsBufSize);
  while not eof(t^) and not NoCarrier do begin
    if NoCarrier or break then goto ende;
    readln(t^,s);

    if LeftStr(s,2)='S ' then begin       { ----- Datei senden }
      ti:=ticker;
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p>0 then begin
        fn:=LeftStr(sf,p-1);
        sf:=mid(sf,p+1);
        sf:=LeftStr(sf,blankpos(sf)-1);
        end
      else
        fn:=sf;
      if multipos(':/',fn) then begin     { File Attach }
        for p:=1 to length(fn) do
          if fn[p]='/' then fn[p]:='\';
        end
      else
        fn:=XFerDir+fn[length(fn)-4]+'-'+RightStr(fn,4)+'.OUT';
      if fileexists(fn) then begin
        if not SizeNego and (pos('""',s)>0) then
          s:=trim(LeftStr(s,pos('""',s)-1));
        netcall.Log('+','sending '+fn+' as '+sf);
        if not SendCommand(s) then begin
          addtime(sendtime); goto ende; end;
        s2:=RepeatGetcommand('S');   { SY / SN }
        if s2='' then begin
          addtime(sendtime); goto ende; end;
        if LeftStr(s2,2)='SY' then begin
          s2:=trim(mid(s2,4));
          p:=blankpos(s2);
          if p>0 then s2:=LeftStr(s2,p-1);
          o:=Cval(s2);
          if not SendFile(fn,o) then begin
            addtime(sendtime); goto ende;
            end;
          fs:=_filesize(fn);
          secs:=max(1,secondsfrom(ti));
          netcall.Log('*','sent '+strs(fs)+' bytes, '+strs(fs div secs)+' cps, '+
                    strs(transdata.errors)+' errors');
          s2:=RepeatGetcommand('C');   { CY / CN }
          if s2='' then begin
            addtime(sendtime); goto ende; end;
          if LeftStr(s2,2)='CN' then
            netcall.Log(lcStop,'remote error: could not move file');
          end
        else if LeftStr(s2,2)='SN' then begin
          case s2[3] of
            '2' : begin
                    rmsg(getres2(2300,42));   { 'Fehler 2' }
                    netcall.log(lcStop,'remote refuses file');
                  end;
            '4' : begin
                    rmsg(getres2(2300,44));   { 'Fehler 4 - Temporaerdatei kann nicht erzeugt werden' }
                    netcall.log(lcStop,'remote can''t create temp file');
                  end;
            '6','7' : begin
                        rmsg(getres2(2300,46));  { 'Fehler 6 - kein Platz fuer Datei' }
                        netcall.log(lcStop,'remote disk full');
                      end;
            '8' : begin
                    rmsg(getres2(2300,48));   { 'Fehler 8 - Datei schon vorhanden' }
                    netcall.log(lcStop,'file does already exist');
                  end;
          else    netcall.log(lcStop,'remote refuses file');
          end;
          end;
        end
      else
        netcall.log(lcStop,'outgoing file not found: '+fn);
      addtime(sendtime);
      end else

    if LeftStr(s,2)='R ' then begin       { ----- Datei anfordern }
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p=0 then goto next;
      fn:=trim(mid(sf,p+1));
      sf:=LeftStr(sf,p-1);
      p:=blankpos(fn);
      if p=0 then goto next;
      fn:=LeftStr(fn,p-1);
      if not validfilename(FilePath+fn) then begin
        netcall.log(lcStop,'invalid request destination file: '+fn);
        goto next;
        end;
      while fileexists(FilePath+fn) do begin      { Neuen Dateinamen erzeugen }
        p:=cpos('.',fn);
        if p=0 then
          fn:=fn+'.001'
        else
          if RightStr(fn,1)='9' then
            fn:=LeftStr(fn,p)+formi(min(999,ival(mid(fn,p+1)))+1,3)
          else
            fn:=LeftStr(fn,length(fn)-1)+strs(ival(RightStr(fn,1))+1);
        end;
      ti:=ticker;
      if SizeNego then
        s:=s+' '+strs(diskfree(ord(FilePath[1])-64) div 3);
      netcall.log('+','requesting file: '+sf);
      if not SendCommand(s) then begin
        addtime(rectime); exit; end;
      s2:=GetCommand;
      if LeftStr(s2,2)='RY' then begin
        netcall.log(' ','request accepted');
        GetRequestFilesize;
        if RecFile(FilePath+fn,size) then begin
          fs:=_filesize(filepath+fn);
          secs:=max(1,secondsfrom(ti));
          netcall.log('*','received '+fn+', '+strs(fs)+' bytes, '+strs(fs div secs)+
                    ' cps, '+strs(transdata.errors)+' errors');
          if SendCommand('CY') then;
          end;
        end
      else
        if LeftStr(s2,3)='RN6' then
          netcall.log(lcStop,'file too large')
        else if LeftStr(s2,2)='RN' then
          netcall.log(lcStop,'request refused or file not found')
        else
          netcall.log(lcStop,'unknown error');
      addtime(rectime);
      end;

  next:
    end;
  SendFiles:=true;
ende:
  close(t^);
  dispose(t);
end;
{$ENDIF}

end.

{
  $Log$
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

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

uses ncmodem,timer,fidoglob,xpglobal,classes,xpmessagewindow;

type
  TUUCPNetcall = class(TModemNetcall)
  protected
    function    InitHandshake : Char;
    procedure   FinalHandshake;

  public
    function  PerformNetcall: Integer;

  public
    UUProtocol    : char;    (* selected protocol               *)

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
   |     +-- TUUCProtocolG      (gGv protocols) TODO
   |     \-- TUUCProtocolFZ     (fz protocols)  TODO
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
             FDialog:  TXPMessageWindowDialog;
             FCommObj: TPCommObj;

  protected
    property    Netcall:  TUUCPNetcall read FNetcall;
    property    CommObj:  TPCommObj    read FCommObj;
    property    Dialog:   TXPMessageWindowDialog read FDialog;

    procedure   AssignUp  (var f:file;var fn:string;var ftype:integer); (* fn is var to be able to   *)
    procedure   AssignDown(var f:file;var fn:string;var ftype:integer); (* use it for output/logging *)

  protected
    Total_Start:  Double; 	(* ticks of xfer start *)
    Total_Size:   LongInt;	(* file size           *)
    Total_Files:  Integer;	(* files               *)
    Total_Errors: Integer;	(* total errors        *)

  public
    constructor InitProtocol(caller: tuucpnetcall);
    destructor  ExitProtocol; virtual;
    function    RunProtocol: integer; virtual; abstract;
end;


type TUUCProtocolSimple = class(TUUCProtocol)
  public
    constructor InitProtocol(caller: tuucpnetcall);
    destructor  ExitProtocol; override;
    function    RunProtocol: integer;  override;

  protected
    (* abstract protocol implementation functions *)
    procedure SendCommand(s:string);                virtual; abstract;
    function  GetCommand: string;                   virtual; abstract;
    procedure SendFile(var f:file; offset:longint); virtual; abstract;
    procedure RecFile (var f:file);                 virtual; abstract;

    (* statistics functions *)
    procedure FileStart(fn:string;send:boolean;size:longint);
    procedure FileReStart;
    procedure FileAdvance(var buf;addsize:longint);
    procedure FileDone; 
    function  File_Str:string;
    
    (* per file statistics variables *)
    File_Start:	Double;		(* ticks of file start *)
    File_Pos:   LongInt;	(* transferred         *)
    File_Type:  integer;        (* 0=Dateitransfer, 1=Daten, 2=Steuerung *)
    File_Errors:integer;	(* errors per file     *)

//    procedure WriteTransfer;
//    procedure ResetTransdat(blksize:word);
//    procedure StartFile(send:boolean; fn:string; size:longint);
//    procedure ShowFtype(var data; len:word);

  private
    (* common protocol functions *)
    function  RepeatGetCommand(c:char):string;
    procedure RepeatSendFile  (var f:file; offset:longint);
    procedure RepeatRecFile   (var f:file; size:longint);

    (* high level protocol functions *)
    (* master: process command file *)
    function Master:boolean;
    (* slave: receive commands *)
    function Slave:boolean;
end;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      (* uu_parerr  = 1; *)
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;
      uu_xfererr = 7;  { send or receive error }
      uu_unknown = 15; { unknown error }

{ --- UUCP protocol exceptions -------------------------------- }

type EUUCProtocol = class (ENetcall);
type EUUCProtFile = class (EUUCProtocol);
type EUUCProtFileRepeat=class(EUUCProtFile);

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
  function  Parse(s:string):boolean;
  function  ReasonMsg:string;
end;

{ - - helper functions  - - - - - - - - - - - - - - - - - - - - }

function ModeVal(s:string):integer;
begin
  result:=CVal(s);
  if result > 511 {==0777 okt} then
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

function TUUCPResponse.Parse(s:string):boolean;

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

  result:=ok;
end;

function TUUCPResponse.ReasonMsg;
begin
  if reason in [2,4..10] then
    result:=GetRes2(2300,100+reason)
  else
    result:='unknown error';
end;

{ --- determine packet type --------------------------------------------- }

function PacketType(var data;len:integer):string;
var s:string;
    p,q:integer;
begin
  SetLength(s,min(len,512)); Move(data,s[1],min(len,512)); 
  p:=cpos(#10,s); if p>0 then TruncStr(s,p-1);

  if LeftStr(s,8)='#! rnews'     then result:=getres2(2300,60) else { 'ungepacktes Newspaket' }
  if LeftStr(s,11)='#! cunbatch' then result:=getres2(2300,61) else { 'gepacktes Newspaket (compress)' }
  if LeftStr(s,11)='#! funbatch' then result:=getres2(2300,62) else { 'gepacktes Newspaket (freeze)' }
  if LeftStr(s,11)='#! gunbatch' then result:=getres2(2300,63) else { 'gepacktes Newspaket (gzip)' }
  if LeftStr(s,11)='#! zunbatch' then result:=getres2(2300,63) else { 'gepacktes Newspaket (gzip)' }
  if LeftStr(s,11)='#! bunbatch' then result:=getres2(2300,64) else { 'gepacktes Newspaket (bzip2)' }
  if LeftStr(s,5)='HELO '        then result:=getres2(2300,71) else { 'ungepacktes Mailpaket' }
  if LeftStr(s,2)=#$1f#$9d       then result:=getres2(2300,80) else { 'gepackte Datei (compress)' }
  if LeftStr(s,2)=#$1f#$9f       then result:=getres2(2300,81) else { 'gepackte Datei (freeze) }
  if LeftStr(s,2)=#$1f#$8b       then result:=getres2(2300,82) else { 'gepackte Datei (gzip) }
  if LeftStr(s,2)=#$42#$5a       then result:=getres2(2300,83) else { 'gepackte Datei (bzip2) }
  if (UpperCase(LeftStr(s,5))='>FROM') or (UpperCase(LeftStr(s,4))='FROM') then 
  begin
    delete(s,1,blankpos(s)); (* delete FROM *)
    result:=getres2(2300,70)+' <'+LeftStr(s,blankpos(s)-1); { 'E-Mail von ' }
    result:=Mid(result,RightPos('!',result));
    p:=pos(result,'@');
    if p>0 then begin
      p:=pos(UpperCase(S),' REMOTE FROM ');
      if p>0 then result:=result+'@'+Mid(s,p+14); end;
    result:=result+'>';
  end else 
    result:= GetRes2(2300,79);
end;

procedure ShowFtype(var data; len:word);
var s    : string;
    user : string;
    p,p2 : byte;
begin
  setlength(s,min(255,len));
end;

{ --- Individual protocol implementations --------------------- }

{$I ncuucp-t.inc}
{ $I ncuucp-g.inc}
{$I ncuucp-e.inc}
{ $I ncuucp-fz.inc}

{ --- TUUCProtocol base class -------------------------------------- }

function Tick: Double;
var H,M,S,S100: smallWord;
begin
  DecodeTime(now,H,M,S,S100);
  result := Double(S100)/1000 + S + M * 60 + H * 60 * 60
end;

constructor TUUCProtocol.InitProtocol(caller: TUUCPNetcall); 
begin 
  FNetcall:=Caller; 
  FCommObj:=Caller.CommObj;
  if Caller.IPC is TXPMessageWindowDialog then
    FDialog := TXPMessageWindowDialog(Caller.IPC) else 
    FDialog := Nil;

  Total_Start := Tick;
  Total_Size  := 0;
  Total_Files := 0;
  Total_Errors:= 0;
end;

destructor  TUUCProtocol.ExitProtocol; begin end;

{ --- UUCP-Verteiler ------------------------------------------------ }

constructor TUUCProtocolSimple.InitProtocol(caller: TUUCPNetcall);
begin
  inherited InitProtocol(caller);
(*
+- Kangaroo (uucp.muc.de) -------------------------00:00:04-+
| Protokoll: [UUCP-X]  Paketgröße [9999]/[9999] (erzwungen) |
+-----------------------------------------------------------+
| Empfangen:  [XXXXXXXX.XXX                               ] |
| Dateiart:   [                                           ] |
| Übertragen: [99999999] / [99999999] Bytes  [99999] Byte/s |
+-----------------------------------------------------------+
| Gesamt: [9999] Dateien / [99999999] Bytes  [99999] Byte/s |
+-----------------------------------------------------------+
¦ Reveived D.00AY as SPOOL\D-C00AYC (287 bytes)             ¦
¦ Reveived D.X00C0 as SPOOL\X-N00C04 (115 bytes)            ¦
¦ Reveived D.00AZ as SPOOL\D-C00AZC (287 bytes)             ¦
¦ Reveived D.X00C1 as SPOOL\X-N00C14 (115 bytes)            ¦
¦ Reveived D.00B0 as SPOOL\D-C00B04 (287 bytes)             ¦
¦ Reveiving D.X00C2 as SPOOL\X-N00C24 (115 bytes)           ¦
+-----------------------------------------------------------+
*)
  if assigned(Dialog) then begin
    Dialog.ResizeSplit(60,[1,3,1,3]);
    Dialog.WrtText( 2,1,getres2(2300,1)+Netcall.UUProtocol); {'Protokoll: UUCP-'}
  
    Dialog.WrtText(26,5,getres2(2300,2)); {'/'         }
    Dialog.WrtText(26,7,getres2(2300,2)); 
    Dialog.WrtText(39,5,getres2(2300,3)); {'Bytes'     } 
    Dialog.WrtText(39,7,getres2(2300,3)); 
    Dialog.WrtText(54,5,getres2(2300,4)); {'Byte/s'    }
    Dialog.WrtText(54,7,getres2(2300,4));

    Dialog.WrtText(02,3,getres2(2300,5)); {'Dateiname:'}
    Dialog.WrtText(02,4,getres2(2300,6)); {'Dateiart:' }

    Dialog.WrtText(02,5,getres2(2300,7)); {'Übertragen'}
    Dialog.WrtText(02,7,getres2(2300,8)); {'Gesamt'    }
    Dialog.WrtText(18,7,getres2(2300,9)); {'Dateien'   }

    Dialog.WrtData(15,3,'',46,true);
    Dialog.WrtData(15,4,'',46,true); 
    Dialog.WrtData(15,5,'',10,true);
    Dialog.WrtData(28,5,'',10,true); 
    Dialog.WrtData(46,5,'',07,true);
    Dialog.WrtData(11,7,'',06,true);
    Dialog.WrtData(28,7,'',10,true); 
    Dialog.WrtData(46,7,'',07,true);
  end;
end;

destructor TUUCProtocolSimple.ExitProtocol;
begin
  inherited ExitProtocol;
  if assigned(Dialog) then
    Dialog.Resize(60,10);
end;

function TUUCProtocolSimple.RepeatGetcommand(c:char):string;
var n : integer;
    s : string;
begin
  n:=10;              { 10 x 1 Min. Timeout }
  repeat
    Netcall.TestBreak;

    s:=GetCommand;
    if (s<>'') and (LeftStr(s,1)<>c) then
      Netcall.Log(lcError,'unexpected command: '+s);
    dec(n);
    if n<= 0 then
      raise EUUCProtocol.Create('unexpected command - retry count reached');
  until (LeftStr(s,1)=c);
  RepeatGetcommand:=s;
end;

procedure TUUCProtocolSimple.RepeatSendFile(var f:file; offset:longint);
var FileRetries: integer;
begin
  FileRetries:=10;
  if offset <0 then offset:=0;
  try
    try
      FileRestart;
      SendFile(f,offset);
    except
      on e:EUUCProtFileRepeat do begin
        FileRetries:=FileRetries-1;
	File_Errors:=File_Errors+1;
        if FileRetries<=0 then raise EUUCProtFile.Create(e.message+' - maximum retry count reached')
        else Netcall.log(lcStop,e.message+' - repeating file');
      end;
    end;
  finally
    close(f);
  end;
  FileDone;
end;

procedure TUUCProtocolSimple.RepeatRecFile(var f:file; size:longint);
var FileRetries: integer;
begin
  FileRetries:=10;
try
  try
    FileRestart;
    RecFile(f);
  except
    on e:EUUCProtFileRepeat do begin
      FileRetries:=FileRetries-1;
      File_Errors:=File_Errors+1;
      if FileRetries<=0 then raise EUUCProtFile.Create(e.message+' - maximum retry count reached')
      else Netcall.log(lcStop,e.message+' - repeating file');
    end; // on EUUCProtFileRepeat
  end;
except
  close(f); erase(f); raise;
end;
  close(f);
  FileDone;
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
var pprot: TUUCProtocol;
begin
  pprot:=nil;

  if not Connect then begin
    result:=el_noconn; exit;
  end;

  if IPC is TXPMessageWindow then
    if Phonenumber<>'' then
      TXPMessageWindow(IPC).Headline:=UUname+' ('+Phonenumber+')' (*
    else if CommObj is TRawIPObj then
      TXPMessageWindow(IPC).Headline:=UUname+' ('+TRawIPObj(CommObj).Hostname+', '+TRawIPObj(CommObj).IPAddr+')' *);

try
  case InitHandshake of
    't':         pprot := TUUCProtocolT.InitProtocol(self);
//  'g','G','v': pprot := TUUCProtocolG.InitProtocol(self);
    'e':         pprot := TUUCProtocolE.InitProtocol(self);
//  'f':         pprot := TUUCProtocolFZ.InitProtocol(self,true);
//  'z':         pprot := TUUCProtocolFZ.InitProtocol(self,false);
  else
    raise EUUCProtocol.Create('Protocol unimplemented');
  end;

  if not assigned(pprot) then
    raise EUUCProtocol.Create('Protocol initialization failed');

  result := pprot.RunProtocol;
  pprot.ExitProtocol;

except
  on Ex:Exception do begin
    WriteIPC(mcError,'%s',[ex.message]);
//  Log     (lcError,      ex.message); -- BUG: crashes?!
    result := el_nologin;
  end;
end;

  Disconnect;
end;

function TUUCPNetcall.InitHandshake:char;
var n,i : integer;                            { --- uucp - Init-Handshake }
    s   : string;
    ti  : longint;

  function GetUUStr:string;        { uucp-String empfangen }
  var recs: string;
  begin
    repeat
      if CommObj.CharAvail then recs:=recs+CommObj.GetChar;
      TestBreak;
    until ((recs<>'') and (recs[length(recs)] in [#0,#4,#10,#13]));

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

  if LeftStr(s,3)<>'ROK' then
    if s='RLOGIN' then
      raise EUUCProtocol.Create('wrong login name')
    else
      raise EUUCProtocol.Create('got '''+s+''' - aborting');

  Log(lcInfo,'UUCP connection established');

  SizeNego:=(s='ROKN');         { size negotiation }
  if SizeNego then begin
    log(lcInfo,'using size negotiation');
    WriteIPC(mcInfo,'UUCP connection established w/ size negitiation',[0]);
  end else
    WriteIPC(mcInfo,'UUCP connection established',[0]);

  for n := 1 to 5 do begin
    s:=GetUUStr; dec(n);
    if s[1]='P' then break;
  end;

  if LeftStr(s,1)<>'P' then
    raise EUUCProtocol.Create('got '''+s+''' - aborting');

  delete(s,1,1);
  Log('~','remote protocols: '+s);
  if not multipos(uuprotos,s) then begin
    CommObj.SendString(^P'UN'#0,false);
    raise EUUCProtocol.Create('no common protocol - remote supports: '+s);
  end;

  for i:=length(uuprotos) downto 1 do     { bestes Protokoll ermitteln }
    if cpos(uuprotos[i],s)>0 then UUProtocol:=uuprotos[i];
  CommObj.SendString(^P'U'+UUProtocol+#0,false);
  Log(lcInfo,'selected protocol'+sp(cpos(UUProtocol,s))+'^');
  WriteIPC(mcError,'selected protocol: %s',[UUProtocol]);

  result := UUProtocol;
end;

procedure TUUCPNetcall.FinalHandshake;           { --- uucp - Handshake vor Hangup }
var b : byte;
begin
  if CommObj.Carrier then begin
    CommObj.SendString(^P'OOOOOO',false);
    CommObj.PurgeInBuffer;
  end;
end;

{ --- TUUCProtocolSimple: Mails/News/Dateien senden, Dateien anfordern ---------------- }

function TUUCProtocolSimple.RunProtocol: integer;
begin
  if not Master then
    result:= uu_senderr
  else
  if not Slave then
    result:= uu_recerr
  else
    result:=uu_ok;

  if result<>uu_ok then
    MDelay(750);
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

procedure TUUCProtocol.AssignUp  (var f:file;var fn:string;var ftype:integer);
{$IFNDEF UnixFS}
var p: integer;
{$ENDIF}
begin
  if not multipos(_MPMask,fn) then
  begin
   (* Schreibweise anpassen *)
    fn :=RightStr(fn,5)+'.OUT'; Insert('-',fn,2);
   (* Set file type *)
    ftype:=iif(fn[1]='D',1,2);
   (* Gleiches Verzeichnis wie CommandFile *)
    fn := ExtractFilePath(Netcall.CommandFile)+fn;
  end
  else begin
{$IFNDEF UnixFS}
   (* nur unter DOS: Pfadseparatoren wieder anpassen *)
    for p:=1 to length(fn) do
     if fn[p]='/' then fn[p]:='\';
{$ENDIF};
    ftype:=0;
  end;

  assign(f,fn);
  resetfm(f,0);
  IOExcept(EUUCProtFile);
end;

procedure TUUCProtocol.AssignDown(var f:file;var fn:string;var ftype:integer);
begin
  if ((LeftStr(fn,2)='D.') or (LeftStr(fn,2)='X.')) and not Multipos(_MPMask,fn) then
  begin
    ftype:=iif(fn[1]='D',1,2);
    fn:=AddDirSepa(Netcall.DownSpool)+U2DOSfile(fn);
    (* Bug: not thread safe *)
    assign(f,fn);
    rewrite(f,1);
  end else begin
    AssignUniqueDownloadName(f,fn,Netcall.FilePath);
    ftype:=0; end;
  IOExcept(EUUCProtFile);
end;

function TUUCProtocolSimple.Master:Boolean;
var cin : text;
    s   : string;        { unparsed UUCP command }
    c   : TUUCPCommand;  { parsed UUCP command }
    a   : string;        { unparsed UUCP response }
    r   : TUUCPResponse; { parsed UUCP response }

  (* Handle S/E command as master *)

  procedure Do_SE;
  var f:file;
  begin
    AssignUp(f,c.src,file_type);
  try
    FileStart(c.src,true,c.size);
    Netcall.Log('+','sending '+c.src+' as '+c.dest);
    Netcall.WriteIPC(mcVerbose,'Sending %s as %s (%d bytes)',[c.src,c.dest,c.size]);

    SendCommand(s);

    if not r.Parse(RepeatGetCommand(c.cmd[1])) { SY/SN or EY/EN } then
      raise EUUCProtFile.Create('Remote refused to accept '+c.src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');

    RepeatSendFile(f,r.restart);

    if not r.Parse(RepeatGetcommand('C')) then
      raise EUUCProtFile.Create('Remote error '+c.src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');

    Netcall.Log('*','sent file - '+File_Str);
    Netcall.WriteIPC(mcInfo,'Sent %s as %s (%s)',[c.src,c.dest,file_str]);
  finally
    close(f);
  end;
    erase(f);
  end;

  (* handle R command as master *)

  procedure Do_R;
  var f:file;
  begin
    AssignDown(f,c.dest,file_type);
    FileStart(c.dest,false,0);
    
    Netcall.Log('+','requesting '+c.src+' as '+c.dest);
    Netcall.WriteIPC(mcVerbose,'Requesting %s as %s',[c.src,c.dest]);
  try
    SendCommand(s);

    if not r.Parse(RepeatGetCommand('R')) then { RY/RN }
      raise EUUCProtFile.Create('Remote refused to send '+c.src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');
  try
    RepeatRecFile(f,r.restart);
    Netcall.Log('*','received file - '+file_str);
    Netcall.WriteIPC(mcInfo,'Requested %s as %s (%s)',[c.src,c.dest,file_str]);
  except
    SendCommand('CN'); raise;
  end;
    SendCommand('CY');
  except
    close(f); erase(f); raise;
  end;
    close(f);
  end;

begin { TUUCProtocolSimple.Master:Boolean; }
  result:=true;

  if (Netcall.CommandFile='') or (_FileSize(Netcall.CommandFile)=0) then
    exit;

  Netcall.WriteIPC(mcInfo,'UUCICO running as master:',[0]);
  Netcall.WriteIPC(mcInfo,'Command file: %s',[Netcall.CommandFile]);
 
  assign(cin,Netcall.CommandFile); 
  reset (cin);

try
  while ((IOResult=0) or true) and not SeekEof(cin) do
  begin
    readln(cin,s);
    c.Parse(s);
  try
    if (c.cmd='S') or (c.cmd='E') then Do_SE else
    if (c.cmd='R')                then Do_R  else
      raise EUUCProtFile.Create('Unknown/unsupported UUCP command: '+s);
  except
    on e:EUUCProtFile do begin
      Netcall.Log(lcError,e.message);
      Netcall.WriteIPC(mcError,'%s',[e.message]);
    end;
  end;
    Netcall.TestBreak;
  end; { while !eof }

except
  on e:Exception do begin
    Netcall.Log(lcError,e.message);
    Netcall.WriteIPC(mcError,'%s',[e.message]);
    result:=false;
  end;
end;
  close(cin);
  erase(cin);
end;

function TUUCProtocolSimple.Slave:Boolean;
var s   : string;       { unparsed incoming command }
    c   : TUUCPCommand; { parsed incoming command }

  function BecomeSlave:boolean; (* was named GetSlave in orig. XP ;-))) *)
  var r: TUUCPResponse;
      a: string;
  begin
    SendCommand('H');
    if r.Parse(RepeatGetCommand('H')) { HY/HN } then begin
      Netcall.Log('+','no files to receive');
      SendCommand('HY');
    end else
      Netcall.Log('+','remote has data for you');
    result:=not r.ok;
  end;

  (* handle S command as slave *)

  procedure Do_S;
    var s:string;
        f:file;
  begin
    s:=ExtractFileName(c.dest); if s='' then
    s:=ExtractFileName(c.src);

 (* if s='' then begin                     { S ohne Dateiname }
      Netcall.Log(lcError,'got S command without file name');
      SendCommand('SN2');
    end else -- will just be named "NONAME" *)
    if (c.size>0) and (Netcall.maxfsize>0) and (c.size>1024*Netcall.maxfsize) then begin
      Netcall.Log(lcError,'file too large ('+StrS(c.size div 1000)+' kB)');
      SendCommand('SN7');
    end else
    try
      AssignDown(f,s,file_type);
      FileStart(s,false,c.size);

      Netcall.Log('+','receiving '+c.src+' as '+s);
      Netcall.WriteIPC(mcVerbose,'Receiving %s as %s (%d bytes)',[c.src,s,c.size]);
      SendCommand('SY 0x0');
    except
      SendCommand('SN4');
      raise;
    end;
    try
      RepeatRecFile(f,c.size);
      IOExcept(EUUCProtFile);

      Netcall.Log('*','received file - '+File_Str);
      Netcall.WriteIPC(mcInfo,'Received %s as %s (%s)',[c.src,s,file_str]);
    except
      SendCommand('CN');
      raise;
    end;
      SendCommand('CY');
  end;

  (* handle H command as slave *)

  procedure Do_H;
  begin
    SendCommand('HY');
    GetCommand; { = HY }
  end;

begin
  result:=true;

  if not BecomeSlave then
    Netcall.WriteIPC(mcInfo,'Remote has no files to receive',[0])
  else
  begin
    Netcall.WriteIPC(mcInfo,'UUCICO running as slave.',[0]);
  try
    while true do
    begin
      c.Parse(GetCommand);
      if c.cmd='' then raise EUUCProtFile.Create('no command received');
    try
      case c.cmd[1] of
        'R': begin SendCommand('RN2'); end;       (* refuse recv requests *)
        'X': begin SendCommand('XN'); end;        (* refuse xfer requests *)
        'E': begin SendCommand('EN'); end;        (* refuse exec requests *)
        'S': begin Do_S; end;
        'H': begin Do_H; exit; end;
        else begin SendCommand(LeftStr(c.cmd,1)+'N'); end;
      end;
    except
      on e:EUUCProtFile do begin
        Netcall.Log(lcError,e.message);
        Netcall.WriteIPC(mcError,'%s',[e.message]);
      end;
    end; //try
    end;
    result:=true;
  except
    on e:Exception do begin
      Netcall.Log(lcError,e.message);
      Netcall.WriteIPC(mcError,'%s',[e.message]);
      result:=false;
    end;
  end; //try
  end;
end;

{ --- TUUCProtocolSimple: Dialogbox ---------------------------------------------------- }
(*
+- Kangaroo (uucp.muc.de) -------------------------00:00:04-+
| Protokoll: [UUCP-X]  Paketgröße [9999]/[9999] (erzwungen) |
+-----------------------------------------------------------+
| Empfangen:  [XXXXXXXX.XXX                               ] |
| Dateiart:   [                                           ] |
| Übertragen: [99999999] / [99999999] Bytes  [99999] Byte/s |
+-----------------------------------------------------------+
| Gesamt: [9999] Dateien / [99999999] Bytes  [99999] Byte/s |
+-----------------------------------------------------------+
¦ Reveived D.00AY as SPOOL\D-C00AYC (287 bytes)             ¦
¦ Reveived D.X00C0 as SPOOL\X-N00C04 (115 bytes)            ¦
¦ Reveived D.00AZ as SPOOL\D-C00AZC (287 bytes)             ¦
¦ Reveived D.X00C1 as SPOOL\X-N00C14 (115 bytes)            ¦
¦ Reveived D.00B0 as SPOOL\D-C00B04 (287 bytes)             ¦
¦ Reveiving D.X00C2 as SPOOL\X-N00C24 (115 bytes)           ¦
+-----------------------------------------------------------+

+- Kangaroo -----------------------------------------00:03:34-+
¦ Protokoll: UUCP-e                                           ¦
+-------------------------------------------------------------¦
¦ Empfangen:    Linuxhandbuch-5.0.tar.gz                      ¦
¦ Dateiart:     Dateitransfer                                 ¦
¦ Übertragen:      97500  /    526476  Bytes    6579  Bytes/s ¦
+-------------------------------------------------------------¦
¦ Gesamt:    181  Dateien /    231990  Bytes    1013  Bytes/s ¦
+-------------------------------------------------------------¦
¦ Reveived D.X01NH as SPOOL\X-N01NHC (127 bytes)              ¦
¦ Reveived /usr/share/doc/Books/Erst_Installation.eps as FILES¦
¦ Reveived /usr/share/doc/Books/Linuxhandbuch-5.0.lsm as FILES¦
¦ Reveiving /usr/share/doc/Books/Linuxhandbuch-5.0.tar.gz as F¦
+-------------------------------------------------------------+
*)

function TUUCProtocolSimple.File_Str:string;
var T:Double;
begin
  T:=Tick;
  result:=StrS(file_pos)+' bytes';
  if T>file_start then
  result:=result+', '+StrS(System.Round(file_pos/(T-file_start)))+' bytes/s';
  if file_errors>0 then
  result:=result+', '+StrS(file_errors)+' errors';
end;

procedure TUUCProtocolSimple.FileStart(fn:string;send:boolean;size:longint);
begin
  File_Errors:=0;
  if not assigned(Dialog) then exit;
  Dialog.WrtText(02,3,iifs(send,getres2(2300,11),getres2(2300,12))); {'Senden:  '/'Empfangen:'}
  Dialog.WrtData(15,3,ExtractFileName(fn),46,false);
  Dialog.WrtData(15,4,getres2(2300,77+File_Type),46,false); 
  Dialog.WrtData(28,5,iifs(Size>0,StrS(Size),getres2(2300,10)),10,true); {'unbekannt'}
  Dialog.WrtData(11,7,StrS(Total_Files+1),6,true);
end;

procedure TUUCProtocolSimple.FileReStart;
begin
  File_Start:=Tick;
  File_Pos  :=0;
  if not assigned(Dialog) then exit;
  Dialog.WrtData(15,5,'0',10,true);
end;

procedure TUUCProtocolSimple.FileAdvance(var buf;addsize:longint);
var T:Double;
    b:boolean;
begin
  T := Tick;
  b := File_Pos=0; 
  File_Pos := File_Pos+AddSize;

  if not assigned(Dialog) then exit;

  T:=Tick; Dialog.WriteFmt(mcInfo,'',[0]);
  Dialog.WrtData(15,5,StrS(File_Pos           ),10,true);
  Dialog.WrtData(28,7,StrS(File_Pos+Total_Size),10,true);
  if File_Start <T then Dialog.WrtData(46,5,StrS(Min(99999,System.Round((File_Pos           )/(T-File_Start )))),7,true);
  if Total_Start<T then Dialog.WrtData(46,7,StrS(Min(99999,System.Round((File_Pos+Total_Size)/(T-Total_Start)))),7,true);
  if (file_type=2) and b then Dialog.WrtData(15,4,PacketType(buf,addsize),46,false);
end;

procedure TUUCProtocolSimple.FileDone;
begin
  Total_Size  :=Total_Size  +File_Pos;
  Total_Files :=Total_Files +1;
  Total_Errors:=Total_Errors+File_Errors;
end;


end.

{
  $Log$
  Revision 1.7  2001/03/16 23:02:34  cl
  - transfer statistics
  - fixes

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

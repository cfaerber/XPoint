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

{ ------------------------------ } INTERFACE { ------------------------------- }

uses ncmodem,timer,fidoglob,xpglobal,classes,xpprogressoutputwindow,xp1;

type
  TUUCPNetcall = class(TModemNetcall)
  protected
    function    InitHandshake : Char;
    procedure   FinalHandshake;

  public
    function    PerformNetcall: Integer;

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
    ECommand	  : boolean; (* use UUCP E command		*)
    MaxFSize      : LongInt; (* size limit for incoming packets *)
  end;

{ ---------------------------- } IMPLEMENTATION { ---------------------------- }

uses typeform, zmodem, progressoutput, resource, sysutils, debug,
xpdiff, objcom, fileio, inout, keys, xpnetcall, netcall, Math, ipaddr
{$IFDEF Unix} ,xpcurses {$ENDIF}
{$IFDEF Win32} ,xpwin32 {$ENDIF}
{$IFDEF DOS32} ,xpdos32 {$ENDIF};

{ - - Planned class hierarchy: - - - - - - - - - - - - - - - - - - - - - - - - }
{                                                                              }
{  TUUCProtocol                                                                }
{   +-- TUUCProtocolSimple                                                     }
{   |     +-- TUUCProtocolT      (t protocol)                                  }
{   |     +-- TUUCProtocolE      (e protocol)                                  }
{   |     +-- TUUCProtocolG      (gGv protocols)                               }
{   |     \-- TUUCProtocolFZ     (fz protocols)                                }
{   +-- TUUCProtocolA (not yet implemented)                                    }
{   \-- TUUCProtocolI (not yet implemented)                                    }
{                                                                              }
{  TUUCPNetcall will connect to the UUCP host, do the initial handshake        }
{  (protocol selection) and create a TUUCProtocol object, which it will then   }
{  transfer control to.                                                        }
{                                                                              }
{  TUUCProtocolSimple is a common base class for the tgGfez protocols, which   }
{  have a very similar structure (i.e. send files, become slave, receive).     }
{  More advanced protocols (such as UUCP-i) would be derieved directly from    }
{  TUUCProtocol and only implement RunProtocol (besides con-/destructor).      }

{ --- UUCP protocol exceptions ----------------------------------------------- }

type
  EUUCProtocol = class (ENetcall) end;
  EUUCProtFile = class (EUUCProtocol) end;

{ --- UUCICO timeout constants ----------------------------------------------- }

const InitTimeout   =   5;    { g: Repeat-Timeout bei INIT-Packets      }
      AckTimeout    =  10;    { g: Repeat-Timeout bei Warten auf ACK    }
      ExitTimeout   =   2;    { g: Repeat-Timeout bei CLOSE             }
      RecvTimeout   =  15;    { g: Repeat-Timeout beim Warten auf Daten }
      DataTimeout  =   60;    { g: Timeout bei šbertragung eines Datenpakets }
      ProtTimeout   =  90;    { e/z: Timeout beim Warten auf Daten      }

{ --- Some forward declarations ---------------------------------------------- }

function PacketType(var data;len:integer):string;forward;

{ --- TUUCProtocol base class ------------------------------------------------ }

type TUUCProtocol = class
  public
    constructor Create(caller: tuucpnetcall);
    destructor  Destroy; override;
    function    RunProtocol: integer; virtual; abstract;

  private
    FNetcall: TUUCPNetcall;
    FDialog:  TProgressOutputWindowDialog;
    FCommObj: TCommStream;
    FTimerObj:TPTimer;

  protected
    property    Netcall:  TUUCPNetcall read FNetcall;
    property    CommObj:  TCommStream  read FCommObj;
    property    Dialog:   TProgressOutputWindowDialog read FDialog;
    property    TimerObj: TPTimer      read FTimerObj;

    procedure   AssignUp  (var f:file;var fn:string;var ftype:integer); (* fn is var to be able to   *)
    procedure   AssignDown(var f:file;var fn:string;var ftype:integer); (* use it for output/logging *)
    procedure   AssignExec(var f:text;var fn:string);

  protected
    Total_Start:  Double;       (* ticks of xfer start *)
    Total_Size:   LongInt;      (* file size           *)
    Total_Files:  Integer;      (* files               *)
    Total_Errors: Integer;      (* total errors        *)
end;

{ --- TUUCProtocolSimple Base class ------------------------------------------ }

type TUUCProtocolSimple = class(TUUCProtocol)
  public
    constructor Create(caller: tuucpnetcall);
    destructor  Destroy; override;
    function    RunProtocol: integer; override;

  protected
   (* protocol startup/shutdown functions *)
    function  InitProtocol:boolean; virtual;
    procedure ExitProtocol;         virtual;

   (* abstract protocol implementation functions *)
    procedure SendCommand(s:string);                virtual; abstract;
    function  GetCommand: string;                   virtual; abstract;
    procedure SendFile(var f:file; offset:longint); virtual; abstract;
    procedure RecFile (var f:file);                 virtual; abstract;

   (* statistics functions *)
    procedure FileStart(fn:string;send:boolean;size:longint);
    procedure FileReStart;
    procedure FileAdvance(var buf;addsize:longint);
    procedure FileError;
    procedure FileDone;
    procedure ShowPacketSize(psin,psout:integer;forced:boolean);

    function  File_Str:string;

  protected
   (* per file statistics variables *)
    File_Start: Double;         (* ticks of file start *)
    File_Pos:   LongInt;        (* transferred         *)
    File_Type:  integer;        (* 0=Dateitransfer, 1=Daten, 2=Steuerung *)
    File_Errors:integer;        (* errors per file     *)

  private
    (* common protocol functions *)
    function  RepeatGetCommand(c:char):string;
    procedure RepeatSendFile  (var f:file; offset:longint);
    procedure RepeatRecFile   (var f:file);

    (* high level protocol functions *)
    (* master: process command file *)
    function Master:boolean;
    (* slave: receive commands *)
    function Slave:boolean;
end;

{ --- UUCP command/response parser ---------------------------- }

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

function ModeStr(i:integer):string;
begin
  result:='0XXX';
  result[2]:=chr($30+((i div 64) mod 8));
  result[3]:=chr($30+((i div  8) mod 8));
  result[4]:=chr($30+( i         mod 8));
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
      exec := s
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
    result:=GetRes2(2300,100);
end;

{ --- Individual protocol implementations ----------------------------------- }

{$I ncuucp-t.inc}
{$I ncuucp-g.inc}
{$I ncuucp-e.inc}
{$I ncuucp-fz.inc}

{ --- TUUCPNetcall -------------------------------------------------------- }

function TUUCPNetcall.PerformNetcall: Integer;
var pprot: TUUCProtocol;
    ip:    TIP;
begin
  pprot:=nil;

  if ProgressOutput is TProgressOutputWindow then
    if Phonenumber<>'' then
    begin
      Log('=','Connected with: '+Phonenumber);
      TProgressOutputWindow(ProgressOutput).Headline:=UUname+' ('+Phonenumber+')'
    end
{$IFDEF Sockets}
    else if CommObj is TRawIPStream then 
    begin
      ip := TIP.Create;
      ip.Raw := TRawIPStream(CommObj).RemoteIP;
      TProgressOutputWindow(ProgressOutput).Headline:=UUname+' ('+
	ip.AsString+':'+StrS(TRawIPStream(CommObj).RemotePort)+')';
      Log('=','Connected with: '+
	ip.AsString+', Port: '+StrS(TRawIPStream(CommObj).RemotePort));
      ip.Free;
    end;
{$ELSE};{$ENDIF}

  try
    case InitHandshake of
      't':         pprot := TUUCProtocolT.Create(self);
      'g','G','v': pprot := TUUCProtocolG.Create(self);
      'e':         pprot := TUUCProtocolE.Create(self);
      'f':         pprot := TUUCProtocolFZ.Create(self,true);
      'z':         pprot := TUUCProtocolFZ.Create(self,false);
    else
      raise EUUCProtocol.Create('Protocol unimplemented');
    end;

    if not assigned(pprot) then
      raise EUUCProtocol.Create('Protocol initialization failed');

    result := pprot.RunProtocol;

  except
    on Ex:Exception do begin
      Output(mcError,'%s',[ex.message]);
      result := el_nologin;
    end;
  end;

  if result<>el_ok then MDelay(750);

  if assigned(pprot) then
    pprot.Free;
end;

{ - - UUCP protocol and parameter negotiation - - - - - - - - - - - - - - - - }

function TUUCPNetcall.InitHandshake:char;
var n,i : integer;                            { --- uucp - Init-Handshake }
    s   : string;

  function GetUUStr:string;        { uucp-String empfangen }
  var recs: string;
  begin
    repeat
      if CommObj.CharAvail then recs:=recs+CommObj.GetChar;
      TestBreak;
    until ((recs<>'') and (LastChar(recs) in [#0,#4,#10,#13]));

    TrimFirstChar(recs, ^P);
    if recs='' then GetUUStr:='' else GetUUStr:=LeftStr(recs,length(recs)-1);
  end;

begin
  result:=#0;
  mdelay(500);

  Output(mcVerbose,'UUCP Initial Handshake',[0]);

  CommObj.PurgeInBuffer;
  CommObj.SendString(^P'S'+UUName+
    iifs(ECommand,iifs(SizeNego,' -N05',' -N04'),
                  iifs(SizeNego,' -N',   ''      ))+#0,false);

  for n := 1 to 5 do begin
    s:=GetUUStr;
    if s='' then continue;
    if s[1]='R' then break;
  end;

  if LeftStr(s,3)<>'ROK' then
    if s='RLOGIN' then
      raise EUUCProtocol.Create('wrong login name')
    else
      raise EUUCProtocol.Create('got '''+s+''' - aborting');

  Log(lcInfo,'UUCP connection established: '+s);

  if (Length(s)>=4) and (s[4]='N') then
  begin
    n:=CVal(Mid(s,5));
    if n=0 then begin
      SizeNego:=true;		{ size negotiation 	}
      ECommand:=false;		{ E command		}
    end else begin
      SizeNego:=0<>(n and 1);	{ size negotiation 	}
      ECommand:=0<>(n and 4);	{ E command		}
    end;
  end else begin
    SizeNego:=false;		{ size negotiation 	}
    ECommand:=false;		{ E command		}
  end;
  
  if SizeNego then 
    log(lcInfo,'using size negotiation');
  if ECommand then 
    log(lcInfo,'using UUCP E command');

  Output(mcInfo,'UUCP connection established'+
    iifs(ECommand,iifs(SizeNego,' w/ size neg. and E command',' w/ E command'),
                  iifs(SizeNego,' w/ size negotiation',       ''             )),[0]);

  for n := 1 to 5 do begin
    s:=GetUUStr; {$IFNDEF Delphi}dec(n); {$ENDIF }// Achtung: Loop-Variable wird verändert
    if s[1]='P' then break;
  end;

  if FirstChar(s)<>'P' then
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
  Output(mcError,'selected protocol: %s',[UUProtocol]);

  result := UUProtocol;
end;

{ - - UUCP protocol shutdown - - - - - - - - - - - - - - - - - - - - - - - - }

procedure TUUCPNetcall.FinalHandshake;           { --- uucp - Handshake vor Hangup }
begin
  Output(mcVerbose,'UUCP Final Handshake',[0]);

  if CommObj.Carrier then begin
    CommObj.SendString(^P'OOOOOO',false);
    CommObj.PurgeInBuffer;
  end;
end;

{ - - UUCP file handling - - - - - - - - - - - - - - - - - - - - - - - - - - }

function U2DOSfile(s:string;e:boolean):string;
var i : integer;
    b : byte;
begin
  s:=s[1]+'-'+RightStr(s,5);
  if e then s[1]:='X';
  b:=0;
  for i:=0 to 3 do            { Schreibweise in einem Byte codieren }
    if (s[i+4]>='A') and (s[i+4]<='Z') then
      inc(b,1 shl i);
  if e then s:=s+chr(90-b) else s:=s+hex(b,1);
  result := UpperCase(s);
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
  if ((LeftStr(fn,2)='D.') or (LeftStr(fn,2)='X.')) and not Multipos('/',fn) then
  begin                                         { NOTE: Unix dir separators only! }
    ftype:=iif(fn[1]='D',1,2);
    fn:=AddDirSepa(Netcall.DownSpool)+U2DOSfile(fn,false);
    (* Bug: not thread safe *)
    assign(f,fn);
    rewrite(f,1);
  end else begin
    AssignUniqueDownloadName(f,fn,Netcall.FilePath);
    ftype:=0; end;
  IOExcept(EUUCProtFile);
end;

procedure TUUCProtocol.AssignExec(var f:text;var fn:string);
begin
  fn:=AddDirSepa(Netcall.DownSpool)+U2DOSfile(fn,true);
  assign(f,fn);
  rewrite(f);
  IOExcept(EUUCProtFile);
end;

{ --- TUUCProtocol base class ------------------------------------------------ }

{ - - initialization/destruction - - - - - - - - - - - - - - - - - - - - - - - }

constructor TUUCProtocol.Create(caller: TUUCPNetcall);
begin
  FNetcall:=Caller;
  FCommObj :=Caller.CommObj;
  FTimerObj:=Caller.Timer;

  if Caller.ProgressOutput is TProgressOutputWindowDialog then
    FDialog := TProgressOutputWindowDialog(Caller.ProgressOutput) else
    FDialog := Nil;

  Total_Start := (GetTicks/100.0);
  Total_Size  := 0;
  Total_Files := 0;
  Total_Errors:= 0;
end;

destructor  TUUCProtocol.Destroy; begin end;

{ --- TUUCProtocolSimple Base class ------------------------------------------ }

{ - - initialization/destruction - - - - - - - - - - - - - - - - - - - - - - - }

constructor TUUCProtocolSimple.Create(caller: TUUCPNetcall);
begin
  inherited Create(caller);

  if assigned(Dialog) then begin
    Dialog.ResizeSplit(60,[1,3,1,{$IFDEF DEBUG}SysGetScreenLines-15{$ELSE}4{$ENDIF}]);

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

destructor TUUCProtocolSimple.Destroy;
begin
  inherited Destroy;
  if assigned(Dialog) then
    Dialog.Resize(60,10);
end;

{ - - protocol main function  - - - - - - - - - - - - - - - - - - - - - - - - - }

function TUUCProtocolSimple.RunProtocol: integer;
begin
  if not InitProtocol then
    result:=EL_nologin
  else
  try
    if not Master then
      result:= EL_senderr
    else
    if not Slave then
      result:= EL_recerr
    else
      result:=EL_ok;
  finally
    ExitProtocol;
  end;
end;

{ - - protocol startup/shutdown (quasi-abstract)  - - - - - - - - - - - - - - }

function  TUUCProtocolSimple.InitProtocol:boolean; begin result:=true; end; (* not necessary for *)
procedure TUUCProtocolSimple.ExitProtocol;         begin               end; (* all protocols     *)

{ - - Master: send/request files  - - - - - - - - - - - - - - - - - - - - - - }

function TUUCProtocolSimple.Master:Boolean;
var cin : text;
    s   : string;        { unparsed UUCP command }
    c   : TUUCPCommand;  { parsed UUCP command }
    r   : TUUCPResponse; { parsed UUCP response }

  (* Handle S/E command as master *)

  procedure Do_SE(var f:file;s,src,dest:string;size:Longint);
  begin
    FileStart(src,true,size);
    Netcall.Log('+','sending '+src+' as '+dest);
    Netcall.Output(mcVerbose,'Sending %s as %s (%d bytes)',[src,dest,size]);

    SendCommand(s);

    if not r.Parse(RepeatGetCommand(s[1])) { SY/SN or EY/EN } then
      raise EUUCProtFile.Create('Remote refused to accept '+src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');

    RepeatSendFile(f,r.restart);

    if not r.Parse(RepeatGetcommand('C')) then
      raise EUUCProtFile.Create('Remote error '+src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');

    Netcall.Log('*','sent file - '+File_Str);
    Netcall.Output(mcInfo,'Sent %s as %s (%s)',[src,dest,file_str]);
  end;

  procedure Do_S;
  var f:file;
  begin
  try
    AssignUp(f,c.src,file_type);
    Do_SE(f,s,c.src,c.dest,c.size);
  finally
    close(f);
  end;
    erase(f);
  end;
    
  procedure Do_E;
  var fd,fx,fs: string;
      f,f2: file;
  begin
    if Netcall.ECommand then
      Do_S		{ just do it }
    else begin
    { create eXexution file }
      fd:='C '+c.exec+#10
         +'U '+c.user+' '+Netcall.UUName+#10
         +'I '+c.dest+#10
         +'F '+c.dest+#10;
      fx:=TempS(length(fd));
      fs:=c.src;
      assign(f2,fx);
      rewrite(f2,1);
      blockwrite(f2,fd[1],length(fd));
      close(f2);
      IOExcept(EUUCProtFile);
    { send data file }
      s:=Format('S %s %s %s -%s %s %s',
        [c.src,c.dest,c.user,c.opts,c.temp,ModeStr(c.mode)]);
      if c.ntfy <> '' then begin s:=s+' '+c.ntfy;
        if c.size > 0 then s:=s+' '+StrS(c.size); end;
    try
      AssignUp(f,c.src,file_type);
      Do_SE(f,s,c.src,c.dest,c.size);
    finally
      close(f);
    end;
      if IOResult<>0 then ;
    { send execution file }
    try
      resetfm(f2,0);
      seek(f2,0);
      s:=Format('S %s %s %s - %s 0666',
        ['X.'+Mid(fs,3),'X.'+Mid(c.dest,3),c.user,'X.'+Mid(fs,3)]);
      if c.ntfy <> '' then begin s:=s+' '+c.ntfy;
        if FileSize(F2) > 0 then s:=s+' '+StrS(FileSize(f2)); end;
      Do_SE(f2,s,fx,'X.'+Mid(c.dest,3),FileSize(f2)); 
    finally
      close(f2);
      erase(f2);
    end; // try
      erase(f);
    end;
  end;
  
  (* handle R command as master *)

  procedure Do_R;
  var f:file;
  begin
    AssignDown(f,c.dest,file_type);
    FileStart(c.dest,false,0);

    Netcall.Log('+','requesting '+c.src+' as '+c.dest);
    Netcall.Output(mcVerbose,'Requesting %s as %s',[c.src,c.dest]);
  try
    SendCommand(s);

    if not r.Parse(RepeatGetCommand('R')) then { RY/RN }
      raise EUUCProtFile.Create('Remote refused to send '+c.src+': '+r.reasonmsg+' (#'+strs(r.reason)+')');
  try
    RepeatRecFile(f);
    Netcall.Log('*','received file - '+file_str);
    Netcall.Output(mcInfo,'Requested %s as %s (%s)',[c.src,c.dest,file_str]);
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

  Netcall.Output(mcInfo,'UUCICO running as master:',[0]);
  Netcall.Output(mcInfo,'Command file: %s',[Netcall.CommandFile]);

  assign(cin,Netcall.CommandFile);
  reset (cin);

  try
    while ((IOResult=0) or true) and not SeekEof(cin) do
    begin
      readln(cin,s);
      c.Parse(s);
      if (c.cmd='S') then Do_S else
      if (c.cmd='E') then Do_E else
      if (c.cmd='R') then Do_R else
        raise EUUCProtFile.Create('Unknown/unsupported UUCP command: '+s);

      Netcall.TestBreak;
      if IOResult<>0 then ;
    end; { while !eof }

  except
    on e:Exception do begin
      Netcall.Log(lcError,e.message);
      Netcall.Output(mcError,'%s',[e.message]);
      result:=false;
    end;
  end;

  close(cin);
  erase(cin);
end;

{ - - Slave: receive files - - - - - - - - - - - - - - - - - - - - - - - - - - }

function TUUCProtocolSimple.Slave:Boolean;
var c   : TUUCPCommand; { parsed incoming command }

  function BecomeSlave:boolean; (* was named GetSlave in orig. XP ;-))) *)
  var r: TUUCPResponse;
  begin
    SendCommand('H');
    if r.Parse(RepeatGetCommand('H')) { HY/HN } then begin
      Netcall.Log('+','no files to receive');
    end else
      Netcall.Log('+','remote has data for you');
    result:=not r.ok;
  end;

  (* handle S/E command as slave *)

  procedure Do_SE(e:boolean);
    var s,se:string;
        f:file;
	t:text;
  begin
    s:=c.dest; if(s='')or(LastChar(s)='/')then { NOTE: Unix dir separators only! }
      if cpos('/',c.src)>=0 then
        s:=c.src else s:='/'+c.src;
    if e then se:=s;	

 (* if s='' then begin                     { S ohne Dateiname }
      Netcall.Log(lcError,'got S command without file name');
      SendCommand('SN2');
    end else -- will just be named "NONAME" *)
    if (c.size>0) and (Netcall.maxfsize>0) and (c.size>1024*Netcall.maxfsize) then begin
      Netcall.Log(lcError,'file too large ('+StrS(c.size div 1000)+' kB)');
      SendCommand(iifs(e,'EN7','SN7'));
    end else
    try
      AssignDown(f,s,file_type);
      FileStart(s,false,c.size);

      Netcall.Log('+','receiving '+c.src+' as '+s);
      Netcall.Output(mcVerbose,'Receiving %s as %s (%d bytes)',[c.src,s,c.size]);
      SendCommand(iifs(e,'EY 0x0','SY 0x0'));
    except
      SendCommand(iifs(e,'EN4','SN4'));
      raise;
    end;
    try
      RepeatRecFile(f);
      Netcall.Log('*','received file - '+File_Str);
      Netcall.Output(mcInfo,'Received %s as %s (%s)',[c.src,s,file_str]);
      if e then begin
	assignExec(t,se);
	write(t,'C ',c.exec,#10);
	write(t,'F ',c.dest,#10);
	write(t,'I ',c.dest,#10);
        close(t);
        if IOResult<>0 then ;
        Netcall.Log('*','created execution file '+se);
      end;
    except
      on E:EUUCProtFile do begin
        SendCommand('CN');
        raise;
      end;
    end;
      SendCommand('CY');
  end;

  (* handle H command as slave *)

  procedure Do_H;
  begin
    try
      SendCommand('HY');
      GetCommand; { = HY }
    except
    end; (* ignore errors, we're hanging up anyway! *)
  end;

begin
  if not BecomeSlave then
  begin
    try SendCommand('HY'); except end;
    Netcall.Output(mcInfo,'Remote has no files to receive',[0]);
    result:=true;
    exit;
  end;

  try
    Netcall.Output(mcInfo,'UUCICO running as slave.',[0]);
    while true do
    begin
      c.Parse(GetCommand);
      if c.cmd='' then raise EUUCProtFile.Create('no command received');
    try
      case c.cmd[1] of
        'R': begin SendCommand('RN2'); end;       (* refuse recv requests *)
        'X': begin SendCommand('XN'); end;        (* refuse xfer requests *)
//      'E': begin SendCommand('EN'); end;        (* refuse exec requests *)
        'S': begin Do_SE(false); end;
        'E': begin Do_SE(true);  end;
        'H': begin Do_H; result:=true; exit; end;
        else begin SendCommand(FirstChar(c.cmd)+'N'); end;
      end;
    except
      on e:EUUCProtFile do begin
        Netcall.Log(lcError,e.message);
        Netcall.Output(mcError,'%s',[e.message]);
      end;
    end; //try
    end;

  except
    on e:Exception do begin
      Netcall.Log(lcError,e.message);
      Netcall.Output(mcError,'%s',[e.message]);
      result:=false;
      exit;
    end;
  end; //try

end;

{ - - higher level protocol implementation function  - - - - - - - - - - - - - }

function TUUCProtocolSimple.RepeatGetcommand(c:char):string;
var n : integer;
    s : string;
begin
  n:=10;              { 10 x 1 Min. Timeout }
  repeat
    Netcall.TestBreak;

    s:=GetCommand;
    if FirstChar(s)<>c then
      Netcall.Log(lcError,'unexpected command: '+s);
    dec(n);
    if n<= 0 then
      raise EUUCProtocol.Create('unexpected command - retry count reached');
  until (FirstChar(s)=c);
  RepeatGetcommand:=s;
end;

procedure TUUCProtocolSimple.RepeatSendFile(var f:file; offset:longint);
begin
try
  if offset<0 then offset:=0;
  seek(f,offset);
  SendFile(f,offset);
finally
  close(f);
end;
  FileDone;
end;

procedure TUUCProtocolSimple.RepeatRecFile(var f:file);
begin
  try
    seek(f,0); truncate(f);
    RecFile(f);
  except
    close(f); erase(f); raise;
  end;

  close(f);
  FileDone;
end;

{ --- Statistics Dialogue ---------------------------------------------------- }

function PacketType(var data;len:integer):string;
var s:string;
    p:integer;
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
    result:=getres2(2300,70)+'<'+LeftStr(s,blankpos(s)-1); { 'E-Mail von ' }
    result:=Mid(result,RightPos('!',result));
    p:=pos(result,'@');
    if p<=0 then begin
      p:=pos(UpperCase(s),' REMOTE FROM ');
      if p>0 then result:=result+'@'+Mid(s,p+14); end;
    result:=result+'>';
  end else
  if UpperCase(LeftStr(s,13))='RETURN-PATH: ' then
    result:=GetRes2(2300,70)+Mid(s,14)
  else
    result:=GetRes2(2300,78);
end;

{ --- TUUCProtocolSimple Dialogue -------------------------------------------- }

{      +- Kangaroo -----------------------------------------00:03:34-+         }
{      ¦ Protokoll: UUCP-e                                           ¦         }
{      +-------------------------------------------------------------¦         }
{      ¦ Empfangen:   [Linuxhandbuch-5.0.tar.gz                    ] ¦         }
{      ¦ Dateiart:    [Dateitransfer                               ] ¦         }
{      ¦ Übertragen:  [   97500] / [  526476] Bytes  [ 6579] Bytes/s ¦         }
{      +-------------------------------------------------------------¦         }
{      ¦ Gesamt:  [ 181] Dateien / [  231990] Bytes  [ 1013] Bytes/s ¦         }
{      +-------------------------------------------------------------¦         }
{      ¦ Received D.X01NH as SPOOL\X-N01NHC (127 bytes)              ¦         }
{      ¦ Received /usr/share/doc/Books/Erst_Installation.eps as FILES¦         }
{      ¦ Received /usr/share/doc/Books/Linuxhandbuch-5.0.lsm as FILES¦         }
{      ¦ Receiving /usr/share/doc/Books/Linuxhandbuch-5.0.tar.gz as F¦         }
{      +-------------------------------------------------------------+         }

function TUUCProtocolSimple.File_Str:string;
var T:Double;
begin
  T:=(GetTicks/100.0);
  result:=StrS(file_pos)+' bytes';
  if T>file_start then
  result:=result+', '+StrS(System.Round(file_pos/(T-file_start)))+' bytes/s';
  if file_errors>0 then
  result:=result+', '+StrS(file_errors)+' errors';
end;

procedure TUUCProtocolSimple.FileStart(fn:string;send:boolean;size:longint);
begin
  File_Errors:=0;
  FileRestart;
  if not assigned(Dialog) then exit;
  Dialog.WrtText(02,3,iifs(send,getres2(2300,11),getres2(2300,12))); {'Senden:  '/'Empfangen:'}
  Dialog.WrtData(15,3,ExtractFileName(fn),46,false);
  Dialog.WrtData(15,4,getres2(2300,77+File_Type),46,false);
  Dialog.WrtData(28,5,iifs(Size>0,StrS(Size),getres2(2300,10)),10,true); {'unbekannt'}
  Dialog.WrtData(11,7,StrS(Total_Files+1),6,true);
end;

procedure TUUCProtocolSimple.FileReStart;
begin
  File_Start:=(GetTicks/100.0);
  File_Pos  :=0;
  if not assigned(Dialog) then exit;
  Dialog.WrtData(15,5,'0',10,true);
end;

procedure TUUCProtocolSimple.FileAdvance(var buf;addsize:longint);
var T:Double;
    b:boolean;
begin
  T := (GetTicks/100.0);
  b := File_Pos=0;
  File_Pos := File_Pos+AddSize;

  if not assigned(Dialog) then exit;

  T:=(GetTicks/100.0); Dialog.WriteFmt(mcInfo,'',[0]);
  Dialog.WrtData(15,5,StrS(File_Pos           ),10,true);
  Dialog.WrtData(28,7,StrS(File_Pos+Total_Size),10,true);
  if File_Start <T then Dialog.WrtData(46,5,StrS(Min(99999,System.Round((File_Pos           )/(T-File_Start )))),7,true);
  if Total_Start<T then Dialog.WrtData(46,7,StrS(Min(99999,System.Round((File_Pos+Total_Size)/(T-Total_Start)))),7,true);
  if (file_type=1) and b then Dialog.WrtData(15,4,PacketType(buf,addsize),46,false);
end;

procedure TUUCProtocolSimple.FileError;
begin
  inc(file_errors);
end;

procedure TUUCProtocolSimple.FileDone;
begin
  Total_Size  :=Total_Size  +File_Pos;
  Total_Files :=Total_Files +1;
  Total_Errors:=Total_Errors+File_Errors;
end;

procedure TUUCProtocolSimple.ShowPacketSize(psin,psout:integer;forced:boolean);
var n:integer;
    s:string;
begin
  if not assigned(Dialog) then exit;
  n:=61;

  if forced then
  begin
    s:=GetRes2(2300,21); n:=n-Length(s);
    Dialog.WrtText(n,1,s);
    n:=n-2;
  end;

  n:=n-6;
  Dialog.WrtData(n+1,1,StrS(psin ),6,true);

  s:=GetRes2(2300,2);
  n:=n-Length(s);
  Dialog.WrtText(n,1,s);

  n:=n-8;
  Dialog.WrtData(n+1,1,StrS(psout),6,true);

  s:=GetRes2(2300,20);
  n:=n-Length(s);
  Dialog.WrtText(n,1,s);
end;

{ ------------------------------------------------------------------------------ }

end.

{
  $Log$
  Revision 1.18  2002/02/13 12:35:35  cl
  - fixed "Range Check Error" on UUCP startup with simple UUCP callees
    (see also <mid:xpbm5535843@dirk.deimeke.net>)

  Revision 1.17  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.16  2001/09/08 16:29:45  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.15  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.14  2001/08/12 20:05:23  cl
  - FIX: delay at UUCICO start

  Revision 1.13  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.12  2001/08/04 14:23:43  cl
  - Show IP address:port in progress window

  Revision 1.11  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.10  2001/07/31 13:10:38  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.9  2001/07/30 19:07:44  cl
  - support of UUCP E command for outgoing messages

  Revision 1.8  2001/07/30 12:42:26  cl
  - support for UUCP E command as slave

  Revision 1.7  2001/07/29 17:10:38  cl
  - fixed return value of TUUCProtocolSimple.Slave

  Revision 1.6  2001/05/16 01:59:16  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.5  2001/03/28 22:26:22  cl
  - better UUCP startup roboustness/error handling

  Revision 1.4  2001/03/26 22:51:04  cl
  - proper shutdown with some protocols (ignore errors due to hangup)
  - range check fix for VPascal

  Revision 1.3  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.2  2001/03/24 23:43:08  cl
  - fixes for DOS32

  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  --- import from playground

  Revision 1.4  2000/11/02 21:27:04  fe
  bzip2 support added.
}

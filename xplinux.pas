{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Matthias Leonhardt, http://www.openxp.de }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ XP - Linux - Supportroutinen }
{ $Id$ }

unit xplinux;

{$I XPDEFINE.INC }

{$IFNDEF unix}
  {$FATAL Die Unit XPLINUX kann nur unter Linux compiliert werden }
{$ENDIF }

{$IFDEF FPC }
  {$mode objfpc}
{$ENDIF }

interface

{$IFDEF FPC }
  { C default packing is dword }
  {$linklib c}
  {$PACKRECORDS 4}
{$ENDIF }

uses
  linux,
  strings,
  sysutils,
  xpglobal;

{$IFDEF VP }
const
  STAT_IRUSR = S_IRUSR;
  STAT_IWUSR = S_IWUSR;
  STAT_IXUSR = S_IXUSR;
  STAT_IRWXU = S_IRWXU;
{$ENDIF }

const                           { Common Environment-Vars }
  envHome               = 'HOME';
  envShell              = 'SHELL';
  envXPHome             = 'XPHOME'; { XP-Basis-Verzeichnis }

  A_USER                = STAT_IRUSR or STAT_IWUSR;     { User lesen/schreiben }
  A_USERX               = A_USER or STAT_IXUSR;         { dito + ausfuehren }

{$ifdef UseSysLog}
  LOG_EMERG             = 0;
  LOG_ALERT             = 1;
  LOG_CRIT              = 2;
  LOG_ERR               = 3;
  LOG_WARNING           = 4;
  LOG_NOTICE            = 5;
  LOG_INFO              = 6;
  LOG_DEBUG             = 7;
{$endif}

type
  TTestAccess           = (             { Zugriffsrechte (wird nach Bedarf erweitert) }
                            taUserR,
                            taUserW,
                            taUserRW,
                            taUserX,
                            taUserRX,
                            taUserRWX
                          );
{$IFDEF VP }
  Stat = TStat;
{$ENDIF }


function SysExec(const Path, CmdLine: String): Integer;


{ Verzeichnis-/Datei-Routinen ------------------------------------------ }

function MakeDir(p: string; a: longint): boolean;
function TestAccess(p: string; ta: TTestAccess): boolean;
function ResolvePathName(p: string): string;
procedure SetAccess(p: string; ta: TTestAccess);

{ Zugriffe ueber /proc/* ----------------------------------------------- }

function GetShortVersion: string;

{$ifdef UseSysLog}

{ XPLog gibt eine Logmeldung im Syslog aus }
procedure XPLog(Level: integer; format_s: string; args: array of const);
procedure XPLogMsg(Level: integer; logmsg: string);
procedure XPDebugLog(logmsg: string);
procedure XPInfoLog(logmsg: string);
procedure XPNoticeLog(logmsg: string);
procedure XPWarningLog(logmsg: string);
procedure XPErrorLog(logmsg: string);

{$endif}

function SysOutputRedirected: boolean;

implementation

uses
  typeform,
  fileio;


{$ifdef UseSysLog}

const
  LOG_PRIMASK           = $07;
  LOG_KERN              = 0 shl 3;
  LOG_USER              = 1 shl 3;
  LOG_MAIL              = 2 shl 3;
  LOG_DAEMON            = 3 shl 3;
  LOG_AUTH              = 4 shl 3;
  LOG_SYSLOG            = 5 shl 3;
  LOG_LPR               = 6 shl 3;
  LOG_NEWS              = 7 shl 3;
  LOG_UUCP              = 8 shl 3;
  LOG_CRON              = 9 shl 3;
  LOG_AUTHPRIV          = 10 shl 3;
  LOG_FTP               = 11 shl 3;
  LOG_LOCAL0            = 16 shl 3;
  LOG_LOCAL1            = 17 shl 3;
  LOG_LOCAL2            = 18 shl 3;
  LOG_LOCAL3            = 19 shl 3;
  LOG_LOCAL4            = 20 shl 3;
  LOG_LOCAL5            = 21 shl 3;
  LOG_LOCAL6            = 22 shl 3;
  LOG_LOCAL7            = 23 shl 3;
  LOG_NFACILITIES       = 24;
  LOG_FACMASK           = $03f8;
  INTERNAL_NOPRI        = $10;
  INTERNAL_MARK         = LOG_NFACILITIES shl 3;

type
  TSysLogCode = record
                  name  : string[10];
                  value : longint;
                end;
const
  PrioNames: array[1..12] of TSysLogCode = (
        (name : 'alert';        value: LOG_ALERT ),
        (name : 'crit';         value: LOG_CRIT ),
        (name : 'debug';        value: LOG_DEBUG ),
        (name : 'emerg';        value: LOG_EMERG ),
        (name : 'err';          value: LOG_ERR ),
        (name : 'error';        value: LOG_ERR ),
        (name : 'info';         value: LOG_INFO ),
        (name : 'none';         value: INTERNAL_NOPRI ),
        (name : 'notice';       value: LOG_NOTICE ),
        (name : 'panic';        value: LOG_EMERG ),
        (name : 'warn';         value: LOG_WARNING ),
        (name : 'warning';      value: LOG_WARNING ) );

   FacNames: array[1..22] of TSysLogCode = (
        (name : 'auth';         value: LOG_AUTH ),
        (name : 'authpriv';     value: LOG_AUTHPRIV ),
        (name : 'cron';         value: LOG_CRON ),
        (name : 'daemon';       value: LOG_DAEMON ),
        (name : 'ftp';          value: LOG_FTP ),
        (name : 'kern';         value: LOG_KERN ),
        (name : 'lpr';          value: LOG_LPR ),
        (name : 'mail';         value: LOG_MAIL ),
        (name : 'mark';         value: INTERNAL_MARK ),
        (name : 'news';         value: LOG_NEWS ),
        (name : 'security';     value: LOG_AUTH ),
        (name : 'syslog';       value: LOG_SYSLOG ),
        (name : 'user';         value: LOG_USER ),
        (name : 'uucp';         value: LOG_UUCP ),
        (name : 'local0';       value: LOG_LOCAL0 ),
        (name : 'local1';       value: LOG_LOCAL1 ),
        (name : 'local2';       value: LOG_LOCAL2 ),
        (name : 'local3';       value: LOG_LOCAL3 ),
        (name : 'local4';       value: LOG_LOCAL4 ),
        (name : 'local5';       value: LOG_LOCAL5 ),
        (name : 'local6';       value: LOG_LOCAL6 ),
        (name : 'local7';       value: LOG_LOCAL7 ));

const
  LOG_PID       = $01;
  LOG_CONS      = $02;
  LOG_ODELAY    = $04;
  LOG_NDELAY    = $08;
  LOG_NOWAIT    = $10;
  LOG_PERROR    = $20;

  log_installed: boolean = false;

{$endif}

const
  fnProcVersion         = '/proc/version';      { Versionsinfos }

var
  SavedExitProc: Pointer;
{$ifdef UseSysLog}
  LogPrefix: array[0..255] of char;
  LogString: array[0..1024] of char;
{$endif}

{ Interface-Routinen fÅr Virtual Pascal -------------------------------- }

{$IFDEF VP }
function Chmod(path:pathstr;Newmode:longint):Boolean;
var
  AStr: AnsiString;
begin
  Astr := Path;
  ChMod := LnxChMod(PCHar(AStr), NewMode) = 0;
end;

function GetPid: LongInt;
begin
  GetPid := LnxGetPid;
end;
{$ENDIF }


{ Verzeichnis-Routinen ------------------------------------------------- }
{$IFDEF VP }
function StrP(s: String): AnsiString;
begin
  StrP := s;
end;
{$ENDIF }

function MakeDir(p: string; a: longint): boolean;
begin
  mkdir(p);
  if (ioresult<>0) then
    MakeDir:= false
  else
    MakeDir:= chmod(p, a);
end;

function TestAccess(p: String; ta: TTestAccess): boolean;
var
  info: stat;
begin
{$IFDEF FPC }
  if not (fstat(p, info)) then
{$ELSE }
  if Lnxstat(PChar(StrP(p)), info) <> 0 then
{$ENDIF }
    TestAccess:= false
  else with info do begin
    case ta of
{$IFDEF FPC }
      taUserR:   TestAccess:= (mode and STAT_IRUSR) <> 0;
      taUserW:   TestAccess:= (mode and STAT_IWUSR) <> 0;
      taUserRW:  TestAccess:= (mode and (STAT_IRUSR or STAT_IWUSR)) <> 0;
      taUserRWX: TestAccess:= (mode and STAT_IRWXU) <> 0;
      taUserX:   TestAccess:= (mode and STAT_IXUSR) <> 0;
      taUserRX:  TestAccess:= (mode and (STAT_IRUSR or STAT_IXUSR)) <> 0;
{$ELSE }
      taUserR:   TestAccess:= (st_mode and S_IRUSR) <> 0;
      taUserW:   TestAccess:= (st_mode and S_IWUSR) <> 0;
      taUserRW:  TestAccess:= (st_mode and (S_IRUSR or S_IWUSR)) <> 0;
      taUserRWX: TestAccess:= (st_mode and S_IRWXU) <> 0;
      taUserX:   TestAccess:= (st_mode and S_IXUSR) <> 0;
      taUserRX:  TestAccess:= (st_mode and (S_IRUSR or S_IXUSR)) <> 0;
{$ENDIF }
    end;
  end;
end;

procedure SetAccess(p: string; ta: TTestAccess);
begin
  case ta of
    taUserR:   chmod(p, STAT_IRUSR);
    taUserW:   chmod(p, STAT_IWUSR);
    taUserRW:  chmod(p, STAT_IRUSR or STAT_IWUSR);
    taUserRWX: chmod(p, STAT_IRWXU);
    taUserX:   chmod(p, STAT_IXUSR);
    taUserRX:  chmod(p, STAT_IRUSR or STAT_IXUSR);
  end;
end;

function ResolvePathName(p: string): string;
var
  s: string;
begin
  if (Length(p)=0) then
    s:= ''
  else
    s:= iifs((p[1]='~'),getenv(envHome)+copy(p,2,length(p)-1),''+p);
{$ifdef UseSysLog}
  if (s<>'') then
    XPDebugLog('Resolved: "'+p+'" -> "'+s+'"');
{$ENDIF}
  ResolvePathName:=s;
end;

{ Zugriffe ueber /proc/* ----------------------------------------------- }

function GetShortVersion: string;
var
  f: text;
  s: string;
begin
  assign(f,fnProcVersion);
  reset(f);
  if (ioresult=0) then begin
    readln(f,s);
    close(f);
    GetShortVersion:= copy(s,1,cpos('(',s)-2);
  end else
    GetShortVersion:= '?.??';
end;

{ SysLog-Interface ----------------------------------------------------- }
{$ifdef UseSysLog}

{$IFDEF FPC }
procedure closelog;cdecl;external;
procedure openlog(__ident:pchar; __option:longint; __facilit:longint);cdecl;external;
function setlogmask(__mask:longint):longint;cdecl;external;
procedure syslog(__pri:longint; __fmt:pchar; args:array of const);cdecl;external;
{$ELSE }
procedure closelog;begin end;
procedure openlog(__ident:pchar; __option:longint; __facilit:longint);begin end;
function setlogmask(__mask:longint):longint;begin end;
procedure syslog(__pri:longint; __fmt:pchar; args:array of const);begin end;
{$ENDIF }

{ Log-Proceduren ------------------------------------------------------- }

procedure XPLog(Level: integer; format_s: string; args: array of const);
var
  s: string;
begin
  if not (log_installed) then           { Kann beim Init der Unit's vorkommen }
    exit;
  { Da FPC einen Internal Error bei syslog(level,p,args) erzeugt,
    muessen wir die Wandelung selbst vornehmen }
  s:= Format(format_s, args);
  StrPCopy(LogString, s);
  syslog(Level, LogString, [0]);
end;

procedure XPLogMsg(Level: integer; logmsg: string);
begin
  if not (log_installed) then
    exit;
  StrPCopy(LogString, logmsg);
  syslog(Level, LogString, [0]);
end;

procedure XPDebugLog(logmsg: string);
begin
  XPLogMsg(LOG_DEBUG, logmsg);
end;

procedure XPInfoLog(logmsg: string);
begin
  XPLogMsg(LOG_INFO, logmsg);
end;

procedure XPNoticeLog(logmsg: string);
begin
  XPLogMsg(LOG_NOTICE, logmsg);
end;

procedure XPWarningLog(logmsg: string);
begin
  XPLogMsg(LOG_WARNING, logmsg);
end;

procedure XPErrorLog(logmsg: string);
begin
  XPLogMsg(LOG_ERR, logmsg);
end;
{$endif}
{ Exit-Proc ------------------------------------------------------------ }

procedure XPLinuxExit;
begin
  ExitProc:= SavedExitProc;
{$ifdef UseSysLog}
  { Log-File schliessen }
  SysLog(LOG_INFO, 'Ends', [0]);
  CloseLog;
{$endif}
end;

{$ifdef UseSysLog}
procedure InitLogStream;
var
  s: string;
begin
{$IFDEF DEBUG }
  if (log_installed) then begin
    XPErrorLog('Panic: Unit initilized twice!');
    WriteLn('Zweite Initialisierung der Unit XPLinux!');
    halt;
  end;
{$ENDIF }
  s:= ExtractFileName(ParamStr(0));
  s:= s+'['+IntToStr(GetPid)+']';
  StrPCopy(LogPrefix, s);
{$IFDEF DEBUG }
  OpenLog(LogPrefix, LOG_NOWAIT, LOG_DEBUG);
{$ELSE }
{$IFDEF Beta }
  OpenLog(LogPrefix, LOG_NOWAIT, LOG_INFO);
{$ELSE }
  OpenLog(LogPrefix, LOG_NOWAIT, LOG_NOTICE);
{$ENDIF }
{$ENDIF }
  SysLog(LOG_INFO, 'Starting', [1]);
  log_installed:= true;
  { In die Exit-Kette einhaengen }
  SavedExitProc:= ExitProc;
  ExitProc:= @XPLinuxExit;
end;
{$endif}
function SysOutputRedirected: boolean;
begin
  // ToDo
  Result := false;
end;

function SysExec(const Path, CmdLine: String): Integer;
begin
  result:= Linux.Shell(AddDirSepa(path)+CmdLine);
end;

{$ifdef UseSysLog}

begin
  InitLogStream;
{$endif}
end.
{
  $Log$
  Revision 1.23  2000/11/19 16:51:02  hd
  - SysExec

  Revision 1.22  2000/11/16 19:23:53  hd
  - SysLog abgeschaltet (kann mit UseSysLog aktiviert werden

  Revision 1.21  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.20  2000/11/11 19:28:13  ml
  - bugfix TestAccess (rwx are modes, not uids

  Revision 1.19  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.18  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.17  2000/05/15 14:01:51  hd
  Fix Bedingte Compilierung

  Revision 1.16  2000/05/15 13:56:53  hd
  - Linux: Env-Var XPHOME uebersteuert nun die Vorgabe ~/.openxp

  Revision 1.15  2000/05/14 17:29:04  ml
  - VP-Portierung fÅr linux

  Revision 1.14  2000/05/14 15:04:52  hd
  - Anpassungen Linux

  Revision 1.13  2000/05/14 12:22:47  hd
  - ResolvePathName: Loest ~/ nach $HOME auf

  Revision 1.12  2000/05/13 11:01:28  hd
  Fix: Falsche Unit

  Revision 1.11  2000/05/13 09:42:26  hd
  xpglobal wird benoetigt (Typen)

  Revision 1.10  2000/05/09 15:52:15  hd
  - SetAccess definiert

  Revision 1.9  2000/05/08 18:22:50  hd
  - Unter Linux wird jetzt $HOME/openxp/ als Verzeichnis benutzt.

  Revision 1.8  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.7  2000/05/05 15:00:09  hd
  - SysLog-Interface (Linux)

  Revision 1.6  2000/05/02 14:35:30  hd
  - Konvertierungsroutine entfernt. Die Konvertierung fuer den
    Bildschirm findet nun in XPCURSES.PAS statt.
  - Leere Unit bleibt bestehen, fuer Linux-spezifische Erweiterungen

  Revision 1.5  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.4  2000/04/09 13:27:07  ml
  Diverse ƒnderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.3  2000/03/26 11:04:10  ml
  zpr-Anzeige in linux geht jetzt

  Revision 1.2  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.1  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

}

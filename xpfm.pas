{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC }

unit XPFM;

interface

const
  FilePath  : string    = '';      { eingehende Dateien        }
  MailPath  : string    = '';      { Arcmail- und PKT-Dateien  }
  zmtempfile: string    = 'zmtemp';  { ZM-Parameter/-file }
  UserName  : string = '';      { eigener Name              }
  OwnAddr   : string = '';      { zone:net/node.point       }
  OwnDomain : string = '';
  DestAddr  : string = '';      { zone:net/node.point       }
  Password  : string = '';
  txt       : string = 'Netcalling ...';
  addtxt    : string = '';
  SendEmpty : boolean = false;      { moeglichst leerer Upload-Batch }
  DebugMode : boolean = false;
  Logfile   : string = 'xp-fm.log';
  Lognew    : boolean = false;
  UseEMSI   : boolean = true;
  AKAs      : string  = '';
  SysName   : string = '';
  SerNr     : string = '';
  SetTime   : boolean = false;
  SendTrx   : boolean = false;
  ExtfNames : boolean = true;
{$ifndef UnixFS}
  zmprog    : string = 'zm.exe';
{$endif}
  Tarifzone : string = '';

  ModemLine : byte       = 1;       { COMn }
  ModemPort : word       = 0;
  IRQ       : byte       = 0;
  tlevel    : byte       = 8;       { FIFO-Triggerlevel }
  Baud      : longint    = 2400;
  ModemInit : string = '';
  CommInitString: string = '';      { ObjCOM comm object init string}
  CommandModemDial  : string = '';      { ATDx }
  Phone     : string = '';      { eine oder mehrere Nummern }
  IgCTS     : boolean    = false;
  IgCD      : boolean    = false;
  UseRTS    : boolean    = false;
  TimeoutConnectionEstablish  : integer    = 60;      { warten auf CONNECT }
  RedialWait: integer    = 60;
  RedWait2  : integer    = 30;
  RedialMax : integer    = 100;
  MaxConn   : integer    = 5;       { maximale fehlerhafte CONNECTS }
  Fossil    : boolean    = false;
  MinCps    : integer    = 100;     { minimale cps-Rate (PD-Zmodem) }
  ZMoptions : string = '';

  mailing   : boolean   = false;    { Transfer gestartet }
  timediff  : longint   = 0;
  //resopen   : boolean   = false;

  FilesToSend : String  = '';

function DoXPFM: integer;

implementation

uses
{$IFDEF NCRT}
  xpcurses,
{$ELSE}
  crt,
{$ENDIF}
  sysutils,typeform,resource,fileio,xpdiff,xpglobal,
  montage,inout,winxp,crc,ObjCOM,Modem,ZModem,Timer,Debug,
  xp0,xp1;

const aresult    : byte = 0;
      brk_result : byte = EL_Break;

      ErrChar   = '#';
      DisplayTextHeight    = 10;
      DisplayTextWidth     = 46;


type  FidoAdr   = record
                    zone,net   : word;
                    node,point : word;
                  end;

var
      FA        : FidoAdr;
      logf      : text;
      nocarrier : string;   { Carrier futsch }

      //ColText   : byte;     { Bildschirm }
      //ColStatus : byte;
      //ColXfer   : byte;
      DisplayWinX,DisplayWinY   : Byte;     { linke obere Fensterecke }
      lastdispline  : string;
      disppos   : byte;

      Connects  : integer;

      CommObj   : tpCommObj;
      TimerObj  : tTimer;
      //Oldexit   : Pointer;

{ --- Allgemeine Routinen ------------------------------------------- }

procedure logwithtime(typ:char; time,txt:string);
begin
  Debuglog('XPFM','Log: '+Typ+' '+Txt,DLInform);
  writeln(logf,typ,' ',time,'  ',txt);
end;

procedure log(typ:char; txt:string);
begin
  logwithtime(typ,time,txt);
end;

procedure logcarrier;
begin
  log(ErrChar,'carrier lost');
end;

procedure splitfido(adr:string; var frec:fidoadr);
var p1,p2,p3 : byte;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if (p2<>0) and (p1<p2) and ((p3=0) or (p3>p2)) then begin
      if p1>0 then
        zone:=minmax(ival(LeftStr(adr,p1-1)),0,65535);
      net:=minmax(ival(copy(adr,p1+1,p2-p1-1)),0,65535);
      if p3>0 then
        point:=minmax(ival(mid(adr,p3+1)),0,65535)
      else
        p3:=length(adr)+1;
      node:=minmax(ival(copy(adr,p2+1,p3-p2-1)),0,65535);
    end;
  end;
end;

procedure TestConfig;
begin
  if FilePath='' then
    trfehler1(2399,getres2(30004,80),30);       { 'InPath missing' }
  FilePath:= AddDirSepa(FilePath);
  if not ValidFilename(FilePath+'1$2$3.9x9') then
    trfehler1(2399,getres2(30004,81),30);       { 'Illegal InPath' }
  if MailPath='' then
    trfehler1(2399,getres2(30004,82),30);       { 'MailPath missing' }
  MailPath:=AddDirSepa(MailPath);
  if not ValidFilename(MailPath+'1$2$3.9x9') then
    trfehler1(2399,getres2(30004,83),30);       { 'Illegal MailPath' }
  if not ValidFilename(zmtempfile) then
    trfehler1(2399,getres2(30004,84),30);       { 'Illegal temporary file name' }
  if not validfilename(logfile) then
    trfehler1(2398,logfile,30);                 { 'Illegal logfile name: %s' }
{  for i:=1 to sendfiles do
    if not fileexists(sendfile[i]) then
      rerror1(105,sendfile[i]);}    {* Ueberpruefung wieder einfuegen 'File missing:  %s' }
  if username='' then
    trfehler1(2399,getres2(30004,85),30);       { 'Name missing' }
  if OwnAddr='' then
    trfehler1(2399,getres2(30004,86),30);       { 'Address missing' }
  SplitFido(OwnAddr,FA);
  if fa.net=0 {or (fa.node=0)} then
    trfehler1(2397,OwnAddr,30);                 { 'Illegal / Incomplete address:  %s' }
  if (baud<300) or (115200 mod baud<>0) then
    trfehler1(2399,getres2(30004,87),30);       { 'illegal baudrate' }
{$ifndef Unix}
  if not fossil then begin
    if ModemPort=0 then
      trfehler1(2399,getres2(30004,88),30);     { 'Port address missing' }
    if IRQ=0 then
      trfehler1(2399,getres2(30004,89),30);     { 'IRQ No missing' }
  end;
{$endif}
end;

function CountPhoneNumbers:integer;
{Anzahl angegebene Telefonnummern zaehlen}
var n : integer;
    s : string;
begin
  s:=trim(phone);
  n:=1;
  while cpos(' ',s)>0 do begin
    s:=trim(mid(s,cpos(' ',s)));
    inc(n);
  end;
  CountPhoneNumbers:=n;
end;

procedure InitVar;
begin
{$ifdef UnixFS}
  { Search for standard Linux components }
  //if not ExecutableExists('rz') then error(getres(118));
  //if not ExecutableExists('sz') then error(getres(119));
{$else}
  if ModemPort=0 then
    case ModemLine of
      1 : ModemPort:=$3f8;    2 : ModemPort:=$2f8;
      3 : ModemPort:=$3e8;    4 : ModemPort:=$2e8;
    end;
  if IRQ=0 then
    case ModemLine of
      1,3 : IRQ:=4;
      2,4 : IRQ:=3;
    end;
{$endif}
  DisplayWinX:=15;
  DisplayWinY:=6;
  if SysName='' then SysName:=UserName;
  if (redialmax=1) and (CountPhoneNumbers>1) then begin
    redialmax:=2;
    redialwait:=redwait2;
  end;
  lastdispline:=''; disppos:=1;
end;

procedure OpenLog;

begin
  assign(logf,logfile);
  if lognew or not existf(logf) then
    rewrite(logf)
  else
    append(logf);
  writeln(logf);
  writeln(logf,'----------  ',date,', XP-FM ',verstr,betastr);
end;


{ --- Bildschirmanzeige --------------------------------------------- }

procedure DisplayTime(const l: longint);
begin
  if l<0 then
    exit;
  TextAttr:=col.colmailer;
  fwrt(DisplayWinX+DisplayTextWidth-6,DisplayWinY,formi(l div 3600,2)+':'+formi((l mod 3600) div 60,2)+':'+formi(l mod 60,2));
end;

procedure DisplayStatus(const s:string; const UseNewLine: Boolean);
begin
  Debuglog('XPFM','Display: '+S,DLInform);
  if s<>'' then
  begin
    if UseNewLine and(lastdispline<>'')then
    begin
      TextAttr:=col.colmailer;
      fwrt(DisplayWinX+2,DisplayWinY+1+disppos,forms(lastdispline,DisplayTextWidth));
      disppos:=(disppos mod DisplayTextHeight)+1;
    end;
    lastdispline:=time+'  '+s;
    TextAttr:=col.colmailerhigh;
    fwrt(DisplayWinX+2,DisplayWinY+1+disppos,forms(lastdispline,DisplayTextWidth));
    TextAttr:=col.colmailer;
  end;
end;

procedure DisplayAndLog(const c:char; const s:string);
begin
  DisplayStatus(s,True);
  log(c,s);
end;

{ --- Display-Routine fuer Modem.DialUp --- }
var
  LastState: tStateDialup;
procedure DialUpDisplay;
begin
  case DUDState of
    SDInitialize     : DisplayStatus(getres2(30004,4),True);  { 'Modem initialisieren' }
    SDSendDial       : begin
                         DisplayStatus(reps(getreps2(30004,5,DUDNumber),Strs(DUDTry)),True);   { 'Wähle %s (Versuch %s) ...' }
                         log('+','Calling '+txt+', '+DUDNumber);
                       end;
    SDWaitForConnect : DisplayTime(System.Round(DUDTimer));
    SDModemAnswer    : DisplayStatus(ModemAnswer,True);
    SDConnect,
    SDNoConnect      : begin TimerObj.Start; Log('=',ModemAnswer)end;
    SDWaitForNextCall: if LastState=SDWaitForNextCall then
                         DisplayTime(System.Round(DUDTimer))
                       else
                         DisplayStatus(getres2(30004,6),True);   { 'warte auf naechsten Anruf ...' }
    SDUserBreak      : DisplayStatus(getres2(30004,7),True); { 'abgebrochen' }
  end;
  LastState:=DUDState;
end;

{$I XPFM.INC}          { YooHoo - Mailer }

function DoXPFM: integer;
const
  MessageBoxTitle: String = 'Fidomailer';
begin
  Debuglog('XPFM','performing fido netcall',DLInform);
  TestConfig;
  InitVar;
  OpenLog;
  TimerObj.Init;
  if not CommInit(CommInitString,CommObj) then begin
    trfehler1(2340,CommInitString,30); { 'Fehler bei Portinitialisierung' }
    result:= EL_par;
    exit;
  end;
  Modem.CommObj:=CommObj;
  TextAttr:=col.colmailer;
  OpenBox(DisplayTextWidth+4, DisplayTextHeight+4, MessageBoxTitle,DisplayWinX, DisplayWinY, col.colmboxrahmen,col.colmbox);
  DisplayStatus(GetRepS2(30004,1,DestAddr),True);   { 'Anruf bei %s' }
  if addtxt<>'' then
    DisplayStatus(addtxt,True);
  connects:=0;
  repeat
    aresult:=EL_ok;
    mailing:=false;
    Modem.DisplayProc:=DialUpDisplay;
    LastState:=SDConnect;
    if DialUp(Phone,ModemInit,CommandModemDial,RedialMax,TimeoutConnectionEstablish,RedialWait)then begin
      inc(Connects);
      YooHooMailer; {Im Mailer wird Mailing True gesetzt}
      if aresult<>0 then
        brk_result:=aresult;
    end else if DUDState=SDUserBreak then
      aresult:=EL_break;
    DisplayStatus(GetRes2(30004,2),True); { 'Modem auflegen' }
    if not HangUp then
      DisplayStatus(getres2(30004,3),True); { 'Modem evtl. nicht aufgelegt?!' }
    if mailing then
      log('+','mail transfer '+iifs(aresult=EL_ok,'completed','aborted'));
  until(aresult=EL_ok)or(aresult=EL_break)or(connects=MaxConn);
  if(connects=0)then
    aresult:=EL_noconn;
  if aresult=EL_break then
    aresult:=brk_result;
  log('-','exiting');
  CommObj^.PurgeInBuffer;
  CommObj^.SetDTR(False);
  CommObj^.Close;
  Dispose(CommObj,Done);
  if timediff<>0 then
    set_time(secsfrom70+timediff);
  CloseBox;
  freeres;
  Debuglog('XPFM','Result code: '+strs(aresult),DLInform);
  close(logf);
  result:= aresult;
end;


end.
{
        $Log$
        Revision 1.7  2001/01/04 21:21:10  ma
        - added/refined debug logs

        Revision 1.6  2000/12/25 22:50:45  mk
        - MarkPos in FirstMarked should be 0

        Revision 1.5  2000/12/25 19:08:47  mk
        - some optical improvements

        Revision 1.4  2000/12/25 16:26:45  mk
        - paint a nice window during fido poll

        Revision 1.3  2000/12/22 10:17:49  mk
        - compatibility update for VP

        Revision 1.2  2000/12/14 16:28:33  hd
        - Fido netcall as unit
          - does not work proper now, just to the CONNECT :-(
          - display is not beautified now

        Revision 1.1  2000/12/14 14:12:40  hd
        - Init: Unit XPFM
          - token from XP-FM.PAS
          - merged resources to XP-?.RQ and adjusted the calls

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Fido-Mailer fuer CrossPoint }
{ (c) 06/92 by PM             }
{ (c) 2000 OpenXP-Team        }

{Debugfaehig: Environmentvariablen
 DEBUG=C:\LOGFILE.TXT
 XPFM=10
 erzeugen ausfuehrliches Logfile.
 Levels: 1 - Standard-Logs
         2 - Bildschirmnachrichten
         3 - Modembefehle/antworten
         4 - EMSI
         5 - Mailer states}

{$I XPDEFINE.INC }

program xp_fm;

uses
{$IFDEF NCRT}
  xpcurses,
{$ELSE}
  crt,
{$ENDIF}
  sysutils,dos,typeform,resource,fileio,xpdiff,xpglobal,
  montage,inout,winxp,crc,ObjCOM,Modem,ZModem,Timer,Debug;

const aresult    : byte = 0;
      brk_result : byte = EL_Break;

      ErrChar   = '#';
      gl        = 10;
      width     = 46;

      Language  : string  = 'D';
      FilePath  : pathstr    = '';      { eingehende Dateien        }
      MailPath  : pathstr    = '';      { Arcmail- und PKT-Dateien  }
      zmtempfile: pathstr    = 'zmtemp';  { ZM-Parameter/-file }
      UserName  : string = '';      { eigener Name              }
      OwnAddr   : string = '';      { zone:net/node.point       }
      OwnDomain : string = '';
      DestAddr  : string = '';      { zone:net/node.point       }
      Password  : string = '';
      txt       : string = 'NetCalling ...';
      addtxt    : string = '';
      SendEmpty : boolean = false;      { moeglichst leerer Upload-Batch }
      DebugMode : boolean = false;
      Logfile   : pathstr = 'xp-fm.log';
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
      CommInitString: string = '';      { EleCOM comm object init string}
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
      OS2time   : shortint   = -1;      { Rechenzeitfreigabe }
      MinCps    : integer    = 100;     { minimale cps-Rate (PD-Zmodem) }
      ZMoptions : string = '';

      mailing   : boolean   = false;    { Transfer gestartet }
      timediff  : longint   = 0;
      resopen   : boolean   = false;

type  FidoAdr   = record
                    zone,net   : word;
                    node,point : word;
                  end;

var   FilesToSend  : String;
      FA        : FidoAdr;
      logf      : text;
      nocarrier : string;   { Carrier futsch }

      ColText   : byte;     { Bildschirm }
      ColStatus : byte;
      ColXfer   : byte;
      scx,scy   : Byte;     { linke obere Fensterecke }
      scrsave   : pointer;
      mx,my     : byte;
      lastdispline  : string;
      disppos   : byte;
      Connects  : integer;

      CommObj   : tpCommObj;
      TimerObj  : tTimer;
      Oldexit   : Pointer;

{ --- Allgemeine Routinen ------------------------------------------- }

procedure logo;
{$ifdef Unix}
begin
  writeln;
  writeln('XP-FM  Fido Mailer ',verstr,pformstr, betastr, ' (c) ''93-99 by Peter Mandrella');
  writeln('(c) 2000 OpenXP Team');
  writeln;
{$else}
var t : text;
begin
  assign(t,''); rewrite(t);
  writeln(t);
  writeln(t,'XP-FM  Fido Mailer ',verstr,pformstr, betastr, ' (c) ''93-99 by Peter Mandrella');
  writeln(t,'(c) 2000 OpenXP Team');
  writeln(t);
  close(t);
{$endif}
end;

procedure helppage;
var t : text;
begin
  assign(t,''); rewrite(t);
  writeln(t,'XP-FM  <CommandFile>');
  close(t);
  halt(EL_par);
end;

procedure error(txt:string);
begin
  DebugLog('XPFM',Txt,1);
  writeln(txt,#7);
  SleepTime(2000);
  if resopen then CloseResource;
  halt(EL_par);
end;

procedure logwithtime(typ:char; time,txt:string);
begin
  DebugLog('XPFM','Log: '+Typ+' '+Txt,1);
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

procedure ReadConfig;
var t    : text;
    s0,s : string;
    p    : byte;
    id   : string;

  procedure GetColors;
  var p : byte;
  begin
    p:=blankpos(s);
    if p>0 then begin
      coltext:=ival(LeftStr(s,p-1));
      s:=trim(mid(s,p));
      p:=blankpos(s);
      if p>0 then begin
        colxfer:=ival(LeftStr(s,p-1));
        colstatus:=ival(mid(s,p+1));
      end;
    end;
  end;

begin
  if paramcount<>1 then Helppage;
  assign(t,paramstr(1));
  reset(t);
  FilesToSend:='';
  if ioresult<>0 then error('CommandFile missing: '+FileUpperCase(paramstr(1)));
  while not eof(t) do begin
    readln(t,s0);
    s:=trim(s0);
    p:=pos('=',s);
    if (s<>'') and (LeftStr(s,1)<>';') and (LeftStr(s,1)<>'#') then
      if p=0 then error('Unknown Command: '+s)
    else begin
      id:=LowerCase(trim(LeftStr(s,p-1))); s:=trim(mid(s,p+1));
      if id='language'   then language:=s else
      if id='logfile'    then logfile:=s else
      if id='lognew'     then begin logfile:=s; lognew:=true; end else
      if id='modeminit'  then modeminit:=s else
      if id='dialcommand' then CommandModemDial:=s else
      if id='phone'      then phone:=trim(s) else
      if id='cts'        then IgCTS:=(UpperCase(s)='N') else
      if id='cd'         then IgCD:=(UpperCase(s)='N') else
      if id='rts'        then UseRTS:=(UpperCase(s)<>'N') else
      if id='connwait'   then TimeoutConnectionEstablish:=minmax(ival(s),0,240) else
      if id='redialwait' then RedialWait:=minmax(ival(s),2,1000) else
      if id='redialwait2'then RedWait2:=minmax(ival(s),2,1000) else
      if id='redialmax'  then RedialMax:=minmax(ival(s),0,1000) else
      if id='maxconn'    then MaxConn:=minmax(ival(s),0,25) else
      if id='inpath'     then FilePath:=s else
      if id='mailpath'   then MailPath:=s else
      if id='zmtempfile' then zmtempfile:=s else
      if id='line'       then ModemLine:=minmax(ival(s),1,4) else
      if id='fossil'     then Fossil:=(UpperCase(s)<>'N') else
      if id='port'       then ModemPort:=hexval(s) else
      if id='irq'        then IRQ:=minmax(ival(s),0,15) else
      if id='triggerlevel' then tlevel:=minmax(ival(s),2,14) else
      if id='baud'       then Baud:=ival(s) else
      if id='name'       then UserName:=s else
      if id='address'    then OwnAddr:=s else
      if id='domain'     then OwnDomain:=s else
      if id='called'     then DestAddr:=s else
      if id='password'   then Password:=s else
      if id='text'       then txt:=s else
      if id='show'       then addtxt:=s else
      if id='sendempty'  then sendempty:=(UpperCase(s)<>'N') else
      if id='debug'      then DebugMode:=(UpperCase(s)<>'N') else
      if id='emsi'       then UseEMSI:=(UpperCase(s)<>'N') else
      if id='aka'        then AKAs:=s else
      if id='sysname'    then SysName:=s else
      if id='sn'         then SerNr:=s else
      if id='settime'    then SetTime:=(UpperCase(s)<>'N') else
      if id='sendtrx'    then SendTrx:=(UpperCase(s)<>'N') else
      if id='colors'     then GetColors else
      if id='releasetime'then os2time:=minmax(ival(s),0,3) else
      if id='extendedfilenames' then ExtFNames:=(UpperCase(s)<>'N') else
      if id='cpsmin'     then MinCps:=minmax(ival(s),0,9999) else
      if id='zmoptions'  then ZMoptions:=s else
      if id='phonezone'  then tarifzone:=s else
      if id='comminit'     then CommInitString:=s else
      if id='send' then if FilesToSend='' then
                          FilesToSend:=FileUppercase(s)
                        else
                          FilesToSend:=FilesToSend+#9+FileUppercase(s)
      { keine bekannte Option angegeben: }
      else begin DebugLog('XPFM','Unknown option: '+id+'='+s,1); writeln('Warning - unknown Option: '+s0)end;
    end;
  end;
  close(t);
  if CommInitString='' then
    begin
      CommInitString:=GetEnv('COMMINIT');
      if CommInitString='' then
        begin
          if fossil then CommInitString:='FOSSIL ' else CommInitString:='SERIAL ';
          CommInitString:=CommInitString+'PORT:'+StrS(ModemLine)+' SPEED:'+StrS(Baud);
          if igcd then CommInitString:=CommInitString+' IGNORECD';
        end;
    end;
  DebugLog('XPFM','Port initialization: '+CommInitString,1);
  if fileexists('RTS.FM') then UseRTS:=true;
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

procedure SetLanguage;
begin
  if not fileexists(FileUpperCase('xpfm-'+language+'.res')) then language:='e';
  if not fileexists(FileUpperCase('xpfm-'+language+'.res')) then
    error(FileUpperCase('xpfm-*.res')+' not found');
  OpenResource(FileUpperCase('xpfm-'+language+'.res'),40000);
  resopen:=true; nocarrier:=getres(195);
end;

procedure TestConfig;
var perr : string;

  procedure rerror(nr:word);
  begin
    error(perr+getres(nr));
  end;

  procedure rerror1(nr:word; txt:string);
  begin
    error(perr+getreps(nr,txt));
  end;

begin
  perr:=getres(100);
  if FilePath='' then rerror(112);    { 'InPath missing' }
  if RightStr(FilePath,1)<>'\' then FilePath:=FilePath+'\';
  if not ValidFilename(FilePath+'1$2$3.9x9') then
    rerror(101);                      { 'Illegal InPath' }
  if MailPath='' then rerror(102);    { 'MailPath missing' }
  if RightStr(MailPath,1)<>DirSepa then MailPath:=MailPath+DirSepa;
  if not ValidFilename(MailPath+'1$2$3.9x9') then
    rerror(103);                      { 'Illegal MailPath' }
  if not ValidFilename(zmtempfile) then
    rerror(114);                      { 'Illegal temporary file name' }
  if not validfilename(logfile) then
    rerror1(104,UpperCase(logfile));     { 'Illegal logfile name: %s' }
{  for i:=1 to sendfiles do
    if not fileexists(sendfile[i]) then
      rerror1(105,sendfile[i]);}    {* Ueberpruefung wieder einfuegen 'File missing:  %s' }
  if username='' then rerror(106);  { 'Name missing' }
  if OwnAddr='' then rerror(107);   { 'Address missing' }
  SplitFido(OwnAddr,FA);
  if fa.net=0 {or (fa.node=0)} then
    rerror1(108,OwnAddr);           { 'Illegal / Incomplete address:  %s' }
  if (baud<300) or (115200 mod baud<>0) then
    rerror(109);                    { 'illegal baudrate' }
{$ifndef Unix}
  if not fossil then begin
    if ModemPort=0 then
      rerror(110);                    { 'Port address missing' }
    if IRQ=0 then
      rerror(111);                    { 'IRQ No missing' }
  end;
{$endif}
end;

function getscreenlines:byte;
begin
   getscreenlines:=24;
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
  if not ExecutableExists('rz') then error(getres(118));
  if not ExecutableExists('sz') then error(getres(119));
{$else}
  if not ExecutableExists(zmprog) then error(getres(115));   { 'ZM.EXE fehlt' }
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
  scx:=15;
  scy:=GetScreenlines div 2 - 6;
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

const wdt    = width+4;
      hgh    = gl+4;
      scsize = 2500;

procedure Col(b:byte);
begin
  textcolor(b and $8f);
  textbackground((b and $7f) shr 4);
end;

procedure PushWindow;
begin
  Cursor(curoff);
  mx:=wherex; my:=wherey;
  getmem(scrsave,scsize);
  col(ColText);
  // window(scx,scy,scx+wdt-1,scy+hgh-1);
  clrscr;
  // window(1,1,80,25);
  inc(windmax,$1900);
  wrt(scx,scy,'Õ'+dup(wdt-2,'Í')+'¸');
  col(ColStatus);
  wrt(scx+2,scy+1,LeftStr(#16+' '+txt,38));
end;

procedure PopWindow;
begin freemem(scrsave,scsize); gotoxy(mx,my); Cursor(curnorm)end;

procedure DisplayTime(l: longint);
begin
  if l<0 then exit;
  Col(ColText); wrt(scx+wdt-10,scy+1,formi(l div 3600,2)+':'+formi((l mod 3600) div 60,2)+':'+formi(l mod 60,2));
end;

procedure DisplayStatus(s:string; UseNewLine: Boolean);
begin
  DebugLog('XPFM','Display: '+S,2);
  if s<>'' then begin
    if UseNewLine and(lastdispline<>'')then begin
      Col(ColText); wrt(scx+2,scy+2+disppos,forms(lastdispline,width));
      disppos:=(disppos mod gl)+1;
    end;
    lastdispline:=time+'  '+s;
    Col(ColXFer); wrt(scx+2,scy+2+disppos,forms(lastdispline,width)); Col(ColText);
  end;
end;

procedure DisplayAndLog(c:char; s:string);
begin DisplayStatus(s,True); log(c,s)end;

{ --- Display-Routine fuer Modem.DialUp --- }
var LastState: tStateDialup;
procedure DialUpDisplay;
begin
  case DUDState of
    SDInitialize: DisplayStatus(getres(161),True);     { 'Modem initialisieren' }
    SDSendDial: begin
                  DisplayStatus(reps(getreps(162,DUDNumber),Strs(DUDTry)),True);   { 'Wähle %s (Versuch %s) ...' }
                  log('+','Calling '+txt+', '+DUDNumber);
                end;
    SDWaitForConnect: DisplayTime(System.Round(DUDTimer));
    SDModemAnswer: DisplayStatus(ModemAnswer,True);
    SDConnect,SDNoConnect: begin TimerObj.Start; Log('=',ModemAnswer)end;
    SDWaitForNextCall: if LastState=SDWaitForNextCall then
                         DisplayTime(System.Round(DUDTimer))
                       else
                         DisplayStatus(getres(165),True);   { 'warte auf naechsten Anruf ...' }
    SDUserBreak: DisplayStatus(getres(160),True); { 'abgebrochen' }
  end;
  LastState:=DUDState;
end;

{$I XP-FM.INC}          { YooHoo - Mailer }

procedure newexit;
begin
  if ioresult<>0 then;
  close(logf);
  if ioresult<>0 then;
  Cursor(curnorm);
  exitproc:=oldexit;
end;

begin
  test8086:=0;
  logo;
  ReadConfig;
  SetLanguage;
  TestConfig;
  InitVar;
  OpenLog;
  TimerObj.Init;
  oldexit:=exitproc;
  exitproc:=@newexit;

  if not CommInit(CommInitString,CommObj)then
    error(getres(117)+' "'+CommInitString+'"'); { 'Fehler bei Portinitialisierung' }
  Modem.CommObj:=CommObj;

  PushWindow;
  DisplayStatus(getreps(166,DestAddr),True);   { 'Anruf bei %s' }
  if addtxt<>'' then DisplayStatus(addtxt,True);

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
    DisplayStatus(getres(164),True); { 'Modem auflegen' }
    if not HangUp then
      DisplayStatus(getres(167),True); { 'Modem evtl. nicht aufgelegt?!' }
    if mailing then
      log('+','mail transfer '+iifs(aresult=EL_ok,'completed','aborted'));
  until(aresult=EL_ok)or(aresult=EL_break)or(connects=MaxConn);

  if(connects=0)then aresult:=EL_noconn;
  if aresult=EL_break then aresult:=brk_result;

  log('-','exiting');
  CommObj^.PurgeInBuffer; CommObj^.SetDTR(False); CommObj^.Close;
  Dispose(CommObj,Done);
  if timediff<>0 then set_time(secsfrom70+timediff);
  PopWindow;
  CloseResource;
  DebugLog('XPFM','Result code: '+strs(aresult),3);
  halt(aresult);
end.

{
  $Log$
  Revision 1.30  2000/11/19 17:52:31  ma
  - compiles again

  Revision 1.29  2000/11/14 22:35:05  fe
  Replaced "exist()" by "fileexists()".

  Revision 1.28  2000/11/14 14:47:52  hd
  - Anpassung an Linux

  Revision 1.27  2000/11/09 19:44:30  hd
  - Anpassungen an Linux

  Revision 1.26  2000/11/09 18:51:42  hd
  - Anpassungen an Linux

  Revision 1.25  2000/10/17 10:05:45  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.24  2000/09/25 17:58:31  mk
  - Window ausgeklammert, da in 32 Bit Version nicht erlaubt

  Revision 1.23  2000/07/14 00:00:40  ma
  - Kosmetik
  - Debuglogs aufgeraeumt

  Revision 1.22  2000/07/12 16:52:10  ma
  - Modemroutinen in Unit Modem ausgelagert
  - kleinere Aenderungen fuer Ansistrings
  - Ausgaberoutinen veraendert (Anzeige scrollt jetzt
    nicht mehr)
  - Finalization wieder weggemacht (gibt's leider nur bei Units)

  Revision 1.21  2000/07/09 09:09:55  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.20  2000/07/04 12:04:19  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.19  2000/07/04 09:59:03  mk
  - Sysutils eingefuegt

  Revision 1.18  2000/06/29 13:00:52  mk
  - 16 Bit Teile entfernt
  - OS/2 Version laeuft wieder
  - Jochens 'B' Fixes uebernommen
  - Umfangreiche Umbauten fuer Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.17  2000/06/22 22:23:56  ma
  - zur Anwahl wird jetzt State Machine benutzt
  - verschiedene Aufraeumarbeiten und Umbenennungen
  - Copyright-Meldung angepasst (+'(C) 2000 OpenXP Team')

  Revision 1.16  2000/06/22 19:53:29  mk
  - 16 Bit Teile ausgebaut

  Revision 1.15  2000/06/22 17:29:20  mk
  - sprechendere Variablen- und Prozedurnamen
  - auf Unit ObjCOM/Timer/Debug umgestellt
  - viele andere Veraenderungen (SendCommand, TestCarrier, DialUp...)

}

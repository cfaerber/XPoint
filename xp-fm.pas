{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Fido-Mailer fÅr CrossPoint }
{ (c) 06/92 by PM            }

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

{$IFDEF BP }
  {$M 16384,30000,40000}
{$ENDIF }

program xp_fm;

uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,uart,resource,fileio,xpdiff,crc,
  xpglobal, montage, inout, winxp;

const aresult    : byte = 0;
      brk_result: byte = EL_break;

      maxfiles  = 500;
      TickFreq  = 18.206512451;
      ErrChar   = '#';
      gl        = 10;
      width     = 46;

      Language  : string[3]  = 'D';
      FilePath  : pathstr    = '';      { eingehende Dateien        }
      MailPath  : pathstr    = '';      { Arcmail- und PKT-Dateien  }
      zmtempfile: pathstr    = 'zmtemp';  { ZM-Parameter/-Logfile }
      UserName  : string[60] = '';      { eigener Name              }
      OwnAddr   : string[20] = '';      { zone:net/node.point       }
      OwnDomain : string[30] = '';
      DestAddr  : string[20] = '';      { zone:net/node.point       }
      Password  : string[20] = '';
      txt       : string[40] = 'NetCalling ...';
      addtxt    : string[40] = '';
      SendEmpty : boolean = false;      { mîglichst leerer Upload-Batch }
      DebugMode : boolean = false;
      Logfile   : pathstr = 'xp-fm.log';
      Lognew    : boolean = false;
      UseEMSI   : boolean = true;
      AKAs      : string  = '';
      SysName   : string[80] = '';
      SerNr     : string[15] = '';
      SetTime   : boolean = false;
      SendTrx   : boolean = false;
      ExtfNames : boolean = true;
      zmprog    : string[80] = 'zm.exe';
      Tarifzone : string[40] = '';

      ModemLine : byte       = 1;       { COMn }
      ModemPort : word       = 0;
      IRQ       : byte       = 0;
      tlevel    : byte       = 8;       { FIFO-Triggerlevel }
      Baud      : longint    = 2400;
      ModemInit : string[80] = '';
      DialComm  : string[20] = '';      { ATDx }
      Phone     : string[80] = '';      { eine oder mehrere Nummern }
      IgCTS     : boolean    = false;
      IgCD      : boolean    = false;
      UseRTS    : boolean    = false;
      ConnWait  : integer    = 60;      { warten auf CONNECT }
      RedialWait: integer    = 60;
      RedWait2  : integer    = 30;
      RedialMax : integer    = 100;
      MaxConn   : integer    = 5;       { maximale fehlerhafte CONNECTS }
      Fossil    : boolean    = false;
      OS2time   : shortint   = -1;      { Rechenzeitfreigabe }
      MinCps    : integer    = 100;     { minimale cps-Rate (PD-Zmodem) }
      ZMoptions : string[60] = '';

      mailing   : boolean   = false;    { Transfer gestartet }
      timediff  : longint   = 0;
      resopen   : boolean   = false;

type  FidoAdr   = record
                    zone,net   : word;
                    node,point : word;
                  end;
      pathptr   = ^pathstr;

var   sendfile  : array[1..maxfiles] of pathptr;
      SendFiles : integer;
      FA        : FidoAdr;
      oldexit   : pointer;
      sh        : word;     { Handle fÅr serielle Schnittstelle }
      bauddetect: longint;
      online    : longint;  { Connect-Zeitpunkt }
      logf      : text;
      nocarrier : string[30];   { Carrier futsch }

      ColText   : byte;     { Bildschirm }
      ColStatus : byte;
      ColXfer   : byte;
      starty    : byte;
      scx,scy   : Byte;     { linke obere Fensterecke }
      scrsave   : pointer;
{$IFDEF BP }
      scrbase   : word;
{$ENDIF }
      mx,my     : byte;
      displine  : array[1..gl] of string[width];
      disppos   : byte;
      dials     : integer;
      Connects  : integer;


{ --- Allgemeine Routinen ------------------------------------------- }

procedure mdelay(msec:word);   { genaues Delay }
var t      : longint;
    i,n    : word;
{$IFDEF BP }
    regs   : registers;
{$ENDIF }

  procedure idle;
  begin
{$IFDEF BP }
    case os2time of
      2 : intr($28,regs);
      3 : inline($b8/$00/$00/$99/$fb/$f4/$35/$ca/$90);
      4 : with regs do begin
            ax:=$1680;
            if meml[0:$2f*4]<>0 then intr($2f,regs);
          end;
    end;
{$ENDIF }
  end;

begin
{$IFDEF BP }
  if os2time=1 then with regs do begin
    ah:=$86;
    cx:=(longint(msec)*1000) shr 16;
    dx:=(longint(msec)*1000) and $ffff;
    intr($15,regs);
  end
  else begin
    n:=system.round(msec/54.925401155);
    if n=0 then
      idle
    else begin
      t:=ticker;
      for i:=1 to n do begin
        while t=ticker do
          idle;
        if t<ticker then
          inc(t)
        else
          t:=ticker;
      end;
    end;
  end;
{$ENDIF }
end;


procedure logo;
var t : text;
begin
  assign(t,''); rewrite(t);
  writeln(t);
  writeln(t,'XP-FM  Fido Mailer ',verstr,pformstr, betastr, ' (c) ''93-99 by Peter Mandrella');
  writeln(t);
  if FOSSILdetect then begin
    writeln(t,'FOSSIL driver detected');
    writeln(t);
  end;
  close(t);
  starty:=wherey;
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
  writeln(txt,#7);
  delay(2000);
  if resopen then CloseResource;
  halt(EL_par);
end;

procedure logwithtime(typ:char; time,txt:string);
begin
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
    id   : string[20];

  procedure GetColors;
  var p : byte;
  begin
    p:=blankpos(s);
    if p>0 then begin
      coltext:=ival(left(s,p-1));
      s:=trim(mid(s,p));
      p:=blankpos(s);
      if p>0 then begin
        colxfer:=ival(left(s,p-1));
        colstatus:=ival(mid(s,p+1));
      end;
    end;
  end;

begin
  if paramcount<>1 then Helppage;
  sendfiles:=0;
{$IFNDEF Ver32}
  if mem[Seg0040:$49]<>7 then begin
    ColText:=$70; ColStatus:=$7e; ColXfer:=$7f;
    scrbase:=SegB800;
  end
  else begin
    ColText:=7; ColStatus:=$f; ColXFer:=$f;
    scrbase:=SegB000;
  end;
{$ENDIF }
  assign(t,paramstr(1));
  reset(t);
  if ioresult<>0 then error('CommandFile missing: '+ustr(paramstr(1)));
  while not eof(t) do begin
    readln(t,s0);
    s:=trim(s0);
    p:=cpos('=',s);
    if (s<>'') and (left(s,1)<>';') and (left(s,1)<>'#') then
      if p=0 then error('Unknown Command:  '+s)
    else begin
      id:=lstr(trim(left(s,p-1)));
      s:=trim(mid(s,p+1));
      if id='language'   then language:=s else
      if id='logfile'    then logfile:=s else
      if id='lognew'     then begin logfile:=s; lognew:=true; end else
      if id='modeminit'  then modeminit:=s else
      if id='dialcommand' then dialcomm:=s else
      if id='phone'      then phone:=trim(s) else
      if id='cts'        then IgCTS:=(ustr(s)='N') else
      if id='cd'         then IgCD:=(ustr(s)='N') else
      if id='rts'        then UseRTS:=(ustr(s)<>'N') else
      if id='connwait'   then ConnWait:=minmax(ival(s),0,240) else
      if id='redialwait' then RedialWait:=minmax(ival(s),2,1000) else
      if id='redialwait2'then RedWait2:=minmax(ival(s),2,1000) else
      if id='redialmax'  then RedialMax:=minmax(ival(s),0,1000) else
      if id='maxconn'    then MaxConn:=minmax(ival(s),0,25) else
      if id='inpath'     then FilePath:=s else
      if id='mailpath'   then MailPath:=s else
      if id='zmtempfile' then zmtempfile:=s else
      if id='line'       then ModemLine:=minmax(ival(s),1,4) else
      if id='fossil'     then Fossil:=(ustr(s)<>'N') else
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
      if id='sendempty'  then sendempty:=(ustr(s)<>'N') else
      if id='debug'      then DebugMode:=(ustr(s)<>'N') else
      if id='emsi'       then UseEMSI:=(ustr(s)<>'N') else
      if id='aka'        then AKAs:=s else
      if id='sysname'    then SysName:=s else
      if id='sn'         then SerNr:=s else
      if id='settime'    then SetTime:=(ustr(s)<>'N') else
      if id='sendtrx'    then SendTrx:=(ustr(s)<>'N') else
      if id='colors'     then GetColors else
      if id='releasetime'then os2time:=minmax(ival(s),0,3) else
      if id='extendedfilenames' then ExtFNames:=(ustr(s)<>'N') else
      if id='cpsmin'     then MinCps:=minmax(ival(s),0,9999) else
      if id='zmoptions'  then ZMoptions:=s else
      if id='phonezone'  then tarifzone:=s else
      if id='send' then begin
        if sendfiles<maxfiles then begin
          inc(sendfiles);
          getmem(sendfile[sendfiles],length(s)+1);
          sendfile[sendfiles]^:=ustr(s);
        end
      { keine bekannte Option angegeben: }
      end else
        writeln('Warning - unknown Option:  '+s0);
    end;
  end;
  close(t);
  if not FOSSILdetect then fossil:=false;
  if fossil then begin
    writeln('Using FOSSIL driver');
    writeln;
  end;
  if exist('RTS.FM') then UseRTS:=true;
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
        zone:=minmax(ival(left(adr,p1-1)),0,65535);
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
  if not exist('XPFM-'+language+'.RES') then
    language:='E';
  if not exist('XPFM-'+language+'.RES') then
    error('XPFM-*.RES not found');
  OpenResource('XPFM-'+language+'.RES',40000);
  resopen:=true;
  nocarrier:=getres(195);
end;

procedure TestConfig;
var i    : integer;
    perr : string[40];

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
  if right(FilePath,1)<>'\' then FilePath:=FilePath+'\';
  if not ValidFilename(FilePath+'1$2$3.9x9') then
    rerror(101);                      { 'Illegal InPath' }
  if MailPath='' then rerror(102);    { 'MailPath missing' }
  if right(MailPath,1)<>'\' then MailPath:=MailPath+'\';
  if not ValidFilename(MailPath+'1$2$3.9x9') then
    rerror(103);                      { 'Illegal MailPath' }
  if not ValidFilename(zmtempfile) then
    rerror(114);                      { 'Illegal temporary file name' }
  if not validfilename(logfile) then
    rerror1(104,ustr(logfile));     { 'Illegal logfile name: %s' }
  for i:=1 to sendfiles do
    if not exist(sendfile[i]^) then
      rerror1(105,sendfile[i]^);    { 'File missing:  %s' }
  if username='' then rerror(106);  { 'Name missing' }
  if OwnAddr='' then rerror(107);   { 'Address missing' }
  SplitFido(OwnAddr,FA);
  if fa.net=0 {or (fa.node=0)} then
    rerror1(108,OwnAddr);           { 'Illegal / Incomplete address:  %s' }
  if (baud<300) or (115200 mod baud<>0) then
    rerror(109);                    { 'illegal baudrate' }
  if not fossil then begin
    if ModemPort=0 then
      rerror(110);                    { 'Port address missing' }
    if IRQ=0 then
      rerror(111);                    { 'IRQ No missing' }
  end;
end;

function getscreenlines:byte;
{$IFDEF BP }
var regs : registers;
{$ENDIF }
begin
{$IFDEF BP }
  with regs do begin
    dl:=0;
    ax:=$1130;
    bh:=0;
    intr($10,regs);
    if (dl<24) or (dl>49) then
      getscreenlines:=25
    else
      getscreenlines:=dl+1;
  end;
{$ELSE }
   getscreenlines:=25
{$ENDIF }
end;

function TeleCount:integer;
var n : integer;
    s : string[80];
begin
  s:=trim(phone);
  n:=1;
  while cpos(' ',s)>0 do begin
    s:=trim(mid(s,cpos(' ',s)));
    inc(n);
  end;
  TeleCount:=n;
end;

procedure InitVar;
begin
  if not exist(zmprog) then
    zmprog:=fsearch(zmprog,getenv('PATH'));
  if zmprog='' then
    error(getres(115));   { 'ZM.EXE fehlt' }
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
  scx:=15;
  scy:=GetScreenlines div 2 - 6;
  if SysName='' then SysName:=UserName;
  if (redialmax=1) and (telecount>1) then begin
    redialmax:=2;
    redialwait:=redwait2;
  end;
  fillchar(displine,sizeof(displine),0);
  disppos:=1;
  bauddetect:=14400;   { fÅr IgCD/Nullmodem }
end;

procedure OpenLog;
{$IFNDEF DPMI }
var
  fi : FossilInfo;
  s  : string;
{$ENDIF }
begin
  assign(logf,logfile);
  if lognew or not existf(logf) then
    rewrite(logf)
  else
    append(logf);
  writeln(logf);
  writeln(logf,'----------  ',date,', XP-FM ',verstr,betastr);
  {$IFNDEF DPMI}
    if fossil and GetFossilInfo(modemline,fi) then begin
      s:='FOSSIL driver: ';
      while (length(s)<65) and (fi.IdAdr^<>#0) do begin
        s:=s+fi.IdAdr^;
        inc(longint(fi.IdAdr));
      end;
      log('%',s);
    end;
  {$ENDIF}
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
{$IFDEF BP }
var i : integer;
{$ENDIF }
begin
  Cursor(curoff);
  mx:=wherex; my:=wherey;
  getmem(scrsave,scsize);
{$IFDEF BP }
  FastMove(mem[scrbase:pred(scy)*160],scrsave^,scsize);
{$ENDIF }
  col(ColText);
  window(scx,scy,scx+wdt-1,scy+hgh-1);
  clrscr;
  window(1,1,80,25);
  inc(windmax,$1900);
  wrt(scx,scy,'’'+dup(wdt-2,'Õ')+'∏');
{$IFDEF BP }
  for i:=scy+1 to scy+hgh-1 do begin
    wrt(scx,i,'≥'); wrt(scx+wdt-1,i,'≥');
    mem[scrbase:(i-1)*160+(scx+wdt)*2-1]:=8;
  end;
  wrt(scx,scy+hgh-1,'‘'+dup(wdt-2,'Õ')+'æ');
  wrt(scx,scy+2,'√'+dup(wdt-2,'ƒ')+'¥');
  for i:=scx+1 to scx+wdt do
    mem[scrbase:(scy+hgh-1)*160+(i*2)-1]:=8;
{$ENDIF BP }
  col(ColStatus);
  wrt(scx+2,scy+1,left(#16+' '+txt,38));
end;

procedure PopWindow;
begin
{$IFDEF BP }
  FastMove(scrsave^,mem[scrbase:pred(scy)*160],scsize);
{$ENDIF }
  freemem(scrsave,scsize);
  gotoxy(mx,my);
  Cursor(curnorm);
end;

function timeform(l:longint):string;
begin
  timeform:=formi(l div 3600,2)+':'+formi((l mod 3600) div 60,2)+':'+
            formi(l mod 60,2);
end;

procedure wrtime(l:longint);
begin
  Col(ColText);
  wrt(scx+wdt-10,scy+1,timeform(l));
end;

procedure wrstatus(s:string);
var i : integer;
begin
  if s<>'' then begin
    if disppos=gl then begin
      if displine[1]='' then
        Move(displine[2],displine[1],(gl-1)*sizeof(displine[1]))
      else begin
        displine[2]:='          ...';
        Move(displine[4],displine[3],(gl-3)*sizeof(displine[1]));
      end
    end else
      inc(disppos);
    displine[disppos]:=time+'  '+s;
  end;
  for i:=1 to gl do begin
    if i=disppos then
      Col(ColXFer)
    else
      Col(ColText);
    wrt(scx+2,scy+2+i,forms(displine[i],width));
  end;
  Col(ColText);
end;

procedure WrLog(c:char; s:string);
begin
  wrstatus(s);
  log(c,s);
end;

function secondsfrom(t:longint):longint;
const day = 1573042;
var ticks : longint;
begin
  if ticker>=t then
    ticks:=ticker-t
  else
    ticks:=(day-t)+ticker;
  secondsfrom:=system.round(ticks/TickFreq);
end;

procedure wrOnlineTime;
begin
  WrTime(secondsFrom(online));
end;


{ --- Interface-Routinen / Anwahl ----------------------------------- }

var recs      : string;
    WaitConn  : boolean;
    connstr   : string[80];
    timer     : smallword;
    sec       : rtlword;
    break     : boolean;
    cps       : cpsrec;


procedure InitInterface;
begin
  sh:=ModemLine;
  SetComParams(sh,fossil,ModemPort,IRQ {+8,1 shl IRQ} );
  if lo(DosVersion)<10 then
    SaveComState(sh,cps);
  SetTriggerLevel(tlevel);
  if fossil then ActivateCom(sh,8192,true);
  if not SetUART(sh,baud,PNone,8,1,not IgCTS) then begin
    if fossil then ReleaseCom(sh);
    error(getres(113));     { 'UngÅltige Baudrate' }
  end;
  if not fossil then ActivateCom(sh,8192,true);
end;

procedure ExitInterface;
begin
  log('-','exiting');
  flushinput(sh);
  DropDtr(sh);
  ReleaseCom(sh);
  if lo(DosVersion)<10 then
    RestComState(sh,cps);    { wÅrde unter OS/2 DTR=1 setzen }
end;

procedure time(t:word);   { Sekunden-Timer setzen }
var h,m,s100 : rtlword;
begin
  timer:=t;
  gettime(h,m,sec,s100);
end;

function carrier:boolean;
begin
  carrier:=IgCD or uart.carrier(sh);
end;

function _timeout(ctest:boolean):boolean;
begin
  _timeout:=(timer=0) or (ctest and not IgCD and not Carrier);
end;

procedure testbyte(idle:boolean);
var c : char;
begin
  if Receive(sh,byte(c)) then begin
    if (c=#13) or (c=#10) then begin
      if DebugMode and (recs<>'') then
        WrStatus(recs);
      if WaitConn and (recs<>'') then begin
        connstr:=recs;
        WaitConn:=false;
      end;
      recs:='';
    end else
    if length(recs)<255 then begin
      inc(byte(recs[0]));
      recs[length(recs)]:=c;
    end;
  end else
    if idle then mdelay(0);
end;

procedure tb(idle:boolean);
var h,m,s,s100 : rtlword;
begin
  testbyte(idle);
  if timer>0 then begin
    gettime(h,m,s,s100);
    if s<>sec then begin
      sec:=s;
      dec(timer);
    end;
  end;
end;

procedure sendstr(s:string);
var i : byte;
begin
  for i:=1 to length(s) do begin
    if IgCTS then
      SendByte(sh,byte(s[i]))
    else
      hSendByte(sh,byte(s[i]));
  end;
end;

procedure sendblock(var buf; len:word);
var i : integer;
    b : array[0..65000] of byte absolute buf;
begin
  for i:=0 to len-1 do begin
    if IgCTS then
      SendByte(sh,b[i])
    else
      hSendByte(sh,b[i]);
    testbyte(false);
  end;
end;

procedure esctime0(space:boolean);
var c : char;
begin
  if keypressed then begin
    c:=readkey;
    case c of
      #27 : begin
              time(0);
              connstr:=getres(160);    { 'abgebrochen' }
              break:=true;
            end;
      '+' : inc(timer);
      '-' : if timer>0 then dec(timer);
      ' ' : if space then time(0);
    end;
  end;
end;

procedure sendcomm(s:string);
var p : byte;
begin
  flushinput(sh);
  recs:='';
  s:=trim(s);
  if s<>'' then begin
    repeat
      p:=cpos('~',s);
      if p>0 then begin
        sendstr(left(s,p-1));
        delete(s,1,p);
        mdelay(150);
        while received(sh) do tb(false);
        mdelay(800);
      end;
    until p=0;
    sendstr(s+#13);
    time(5);
    repeat
      tb(true);
      esctime0(false);
    until _timeout(false) or (recs='OK') or (recs='0');
    repeat
      tb(true);
      esctime0(false);
    until _timeout(false) or (recs='');   { auf CR warten }
    mdelay(500);
  end;
end;

procedure sendmstr(s:string);
var p : byte;
begin
  while (length(trim(s))>1) and not _timeout(false) do begin
    p:=pos('\\',s);
    if p=0 then p:=length(s)+1;
    sendcomm(trim(left(s,p-1)));
    s:=trim(mid(s,p+2));
    esctime0(false);
  end;
end;

procedure TestCarrier;    { Carrier bei Programmstart vorhanden? }
var lt,n,i : byte;
begin
  if IgCD then exit;
  mdelay(100);
  if carrier then begin
    aresult:=EL_carrier;
    ExitInterface;
    PopWindow;
    col(7);
    gotoxy(1,starty+1);
    n:=res2anz(170);
    for i:=1 to n do
      writeln(getreps2(170,i,strs(ModemLine)));
    writeln(#7);
    write(getres(171),'    ');
    freeres;
    time(60);
    lt:=0;
    while not (keypressed and (readkey<>#1)) and not _timeout(false) do begin
      tb(false);
      if timer<>lt then begin
        lt:=timer;
        write(#8#8#8,lt:3);
      end;
    end;
    CloseResource;
    halt(aresult);
  end;
end;


function DialUp:boolean;
var nummer   : string[40];
    s        : string;

  function GetTelefon:string;
  var p : byte;
  begin
    p:=cpos(' ',phone);
    if p=0 then
      GetTelefon:=phone
    else begin                         { Nummern rotieren }
      GetTelefon:=left(phone,p-1);
      phone:=trim(mid(phone,p))+' '+left(phone,p-1);
    end;
  end;

  procedure SetBauddetect;
  var p : byte;
  begin
    p:=1;
    while (p<=length(connstr)) and ((connstr[p]<'0') or (connstr[p]>'9')) do
      inc(p);
    delete(connstr,1,p-1);
    p:=1;
    while (p<=length(connstr)) and (connstr[p]>='0') and (connstr[p]<='9') do
      inc(p);
    bauddetect:=ival(left(connstr,p-1));
    if (bauddetect<300) or (115200 mod bauddetect<>0) then
      bauddetect:=baud;
  end;

  function ohnestrich(nummer:string):string;
  begin
    while cpos('-',nummer)>0 do
      delete(nummer,cpos('-',nummer),1);
    ohnestrich:=nummer;
  end;

begin
  DialUp:=IgCD;
  recs:='';
  WaitConn:=false;
  WrStatus(getres(161));     { 'Modem initialisieren' }
  break:=false;
  Time(60);
  if dialcomm<>'' then begin
    sendstr(#13); mdelay(150);
    sendstr(#13); mdelay(300);
    flushinput(sh);
    sendcomm('AT');
    esctime0(false);
  end;
  if not _timeout(false) then
    sendmstr(ModemInit)
  else begin
    aresult:=EL_break;
    exit;
  end;
  TestCarrier;
  while not Carrier and (dials<RedialMax) do begin
    inc(dials);
    nummer:=GetTelefon;
    WrStatus(reps(getreps(162,nummer),strs(dials)));   { 'WÑhle %s (Versuch %s) ...' }
    log('+','Calling '+txt+', '+nummer);
    s:=dialcomm;
    while pos('\\',s)>0 do begin
      sendcomm(left(s,pos('\\',s)-1));
      delete(s,1,pos('\\',s)+1);
    end;
    if s+nummer<>'' then
      sendstr(s+ohnestrich(nummer)+#13);        { wÑhlen }
    mdelay(500);
    flushinput(sh);
    recs:='';
    time(ConnWait);
    WaitConn:=true; ConnStr:='';    { warten auf RÅckmeldung }
    repeat
      tb(true); esctime0(false);
      wrtime(timer);
      if (connstr='RINGING') or (connstr='RRING') then begin
        if not DebugMode then wrstatus(connstr);
        dec(disppos);
        connstr:=''; WaitConn:=true;
      end;
    until _timeout(false) or (connstr<>'');
    if break then
      break:=false
    else
      if _timeout(false) then connstr:=getres(163);   { 'keine Verbindung' }
{!} if left(connstr,7)='CARRIER' then
      connstr:='CONNECT'+mid(connstr,8);
    log('=',connstr);
    mdelay(500);
    if not DebugMode then WrStatus(connstr);
    if Carrier or (pos('CONNECT',ustr(connstr))>0) or (left(ustr(connstr),7)='CARRIER')
    then begin
      SetBaudDetect;
      Online:=ticker;
      if not carrier then mdelay(500);  { falls Carrier nach CONNECT kommt }
      if not carrier then mdelay(1000);
      DialUp:=true;
    end else begin
      sendstr(#13);
      mdelay(1000);
      WrStatus(getres(165));   { 'warte auf nÑchsten Anruf ...' }
      if dials<RedialMax then begin
        time(RedialWait);
        repeat
          tb(true); esctime0(true);
          wrtime(timer);
        until _timeout(false);
        if break then begin
          aresult:=EL_break;
          exit;
        end;
      end else
        mdelay(500);
    end;
  end; { while }
  if not Carrier then
    aresult:=EL_noconn;
end;


procedure Hangup;
var i : integer;
begin
  if Carrier then begin
    WrStatus(getres(164));     { 'Modem auflegen' }
    DropDtr(sh);
    for i:=1 to 6 do
      if carrier then mdelay(500);
    SetDtr(sh);
    mdelay(500);
    if carrier and GetCTS(sh) then begin
      sendstr('+++');
      for i:=1 to 4 do
        if carrier then mdelay(500);
      if carrier and GetCTS(sh) then
        sendstr('AT H0'#13);
    end;
  end else begin
    DropDtr(sh);
    mdelay(200);
    SetDtr(sh);
    mdelay(500);
    if GetCTS(sh) then sendstr(#13);
    mdelay(300);
    if GetCTS(sh) then sendstr(#13);
  end;
end;


{$I XP-FM.INC}          { YooHoo - Mailer }

{$IFDEF BP }
  {$F+,S-}
{$ENDIF }
procedure newexit;
begin
  if ioresult<>0 then;
  close(logf);
  if ioresult<>0 then;
  Cursor(curnorm);
  exitproc:=oldexit;
end;
{$IFDEF BP }
  {$F+,S+}
{$ENDIF }


begin
  test8086:=0;
  logo;
  ReadConfig;
  SetLanguage;
  TestConfig;
  InitVar;
  OpenLog;
  oldexit:=exitproc;
  exitproc:=@newexit;
  InitInterface;
  PushWindow;
  WrStatus(getreps(166,DestAddr));   { 'Anruf bei %s' }
  if addtxt<>'' then WrStatus(addtxt);

  dials:=0; connects:=0;
  repeat
    aresult:=EL_ok;
    if DialUp then begin
      YooHooMailer;
      if aresult<>0 then brk_result:=aresult;
      inc(connects);
    end;
    HangUp;
    if mailing then
      log('+','mail transfer '+iifs(aresult=EL_ok,'completed','aborted'));
    mailing:=false;
  until (aresult=EL_ok) or (aresult=EL_break) or
        (connects=MaxConn) or (dials=RedialMax);

  if (dials=redialmax) and (connects=0) then
    aresult:=EL_noconn;
  if aresult=EL_break then
    aresult:=brk_result;

  ExitInterface;
  if timediff<>0 then
    set_time(secsfrom70+timediff);
  PopWindow;
  CloseResource;
  halt(aresult);
end.
{
  $Log$
  Revision 1.14  2000/06/20 22:22:21  ma
  - letzte Version des alten Mailers

  Revision 1.13  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.12  2000/04/15 12:30:58  mk
  - Compilierfaehigkeit mit VP wieder hergestellt

  Revision 1.11  2000/03/23 23:58:49  oh
  Blockstrukturen sauber formatiert

  Revision 1.10  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.8  2000/02/28 08:57:05  mk
  - Version auf 3.20 RC1 geandert

  Revision 1.7  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

{  $Id$

   OpenXP UUCP netcall class
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
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
unit ncuucico;

interface

uses ncmodem;

type
  TUUCPNetcall = class(TModemNetcall)

  protected

  public

  end;

implementation

uses dos,
     sysutils,
     math,
{$IFDEF NCRT }
     xpcurses,
{$ELSE }
     crt,
{$ENDIF }
{$IFDEF Win32 }
     xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
     xpdos32,
{$ENDIF }
{$IFDEF OS2 }
     xpos2,
{$ENDIF }
     xpglobal,uart,typeform,fileio,inout,resource,winxp;

type  pathstr = string;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      (* uu_parerr  = 1; *)
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;
      XFerDir    = 'SPOOL\';
      uucicores  = 'uucicor.tmp';
      verstr     = 'v3.2';

      ParDebug      : boolean = false;
      DebugWinX1    : byte = 0;
      DebugWinX2    : byte = 0;
      DebugWinY1    : byte = 0;
      DebugWinY2    : byte = 0;

      Language      : string[3] = 'D';
      boxname       : string[20] = '';
      pointname     : string[30] = '';
      MaxWinSize    : byte = 7;
      MaxPacketSize : word = 64;
      VarPacketSize : boolean = false;
      ForcePktSize  : boolean = false;
      UUprotos      : string[10] = 'gfe';
      SizeNego      : boolean = false;
      FilePath      : pathstr = 'FILES\';
      commandfile   : pathstr = '';
      uulogfile     : pathstr = '';
      fossil        : boolean = false;
      releasetime   : byte    = 0;
      maxfsize      : longint = 0;

      ModemPort     : word    = $3f8;
      IRQ           : byte    = 4;
      tlevel        : byte    = 8;
      baud          : longint = 2400;
      comnr         : byte    = 0;
      igncd         : boolean = false;
      igncts        : boolean = false;
      userts        : boolean = true;

      starttime     : longint = 0;
      onlinetime    : longint = 0;
      resopen       : boolean = false;
      dlogopen      : boolean = false;
      ulogopen      : boolean = false;
      break         : boolean = false;      { <Esc> gedrueckt }
      filetrans     : boolean = false;      { fuer rerrmsg }
      ShowTime      : boolean = true;
      FileStarted   : boolean = false;


var   omx,omy      : byte;
      oldexit      : pointer;

      debugfile    : file of byte;     { Schnittstellen-Debug-Logfile  }
      (* siminput     : file of byte; *)     { Eingabedatei fuer sim. Netcall }
      deblog,uulog : ^text;            { DebugMode-Logfile }
      col          : record
                       colmailer     : byte;
                       colmailerhi   : byte;
                       colmailerstat : byte;
                     end;
      UWin         : record
                       senden      : string[15];
                       empfangen   : string[15];
                       dateien     : string[15];
                       uebertragen : string[15];
                       gesamt      : string[15];
                       dateigroesse: string[15];
                       restzeit    : string[15];
                       blockgroesse: string[15];
                       durchsatz   : string[30];
                       dgesamt     : string[30];
                       fehler      : string[15];
                     end;

{$I uucp-g.inc}
{$I uucp-e.inc}
{$I uucp-fz.inc}

{ --- UUCP-Verteiler ------------------------------------------------ }

function SendCommand(s:string):boolean;   { true = ok }
var scresult : boolean;
begin
  wrldebug('sending command: '+s);
  case proto of
    'g','G' : scresult:=g_SendCommand(s);
    'e'     : scresult:=e_SendCommand(s);
    'f','z' : scresult:=fz_SendCommand(s);
  end;
  if not scresult then begin
    if blankpos(s)>0 then s:=LeftStr(s,blankpos(s)-1);
    LogError('error sending command "'+s+'"');
    end;
  SendCommand:=scresult;
end;

function GetCommand:string;               { '' = Fehler }
var s : string;
begin
  case proto of
    'g','G' : s:=g_GetCommand;
    'e'     : s:=e_GetCommand;
    'f','z' : s:=fz_GetCommand;
  end;
  wrldebug('received command: '+s);
  if s='' then
    LogError('error receiving UUCP command');
  GetCommand:=s;
end;

function RepeatGetcommand(c:char):string;
var n : integer;
    s : string;
begin
  n:=10;              { 10 x 1 Min. Timeout }
  repeat
    s:=GetCommand;
    if (s<>'') and (LeftStr(s,1)<>c) then
      wrldebug('unexpected command: '+s);
    dec(n);
  until (LeftStr(s,1)=c) or (n=0) or nocarrier;
  RepeatGetcommand:=s;
end;

function SendFile(fn:pathstr; offset:longint):boolean;   { true = ok }
var sfresult : shortint;
begin
  repeat
    startfile(true,extractfilename(fn),_filesize(fn));
    case proto of
      'g','G' : sfresult:=g_SendFile(fn,offset);
      'e'     : sfresult:=e_SendFile(fn,offset);
      'f'     : sfresult:=fz_SendFile(true,fn,offset);
      'z'     : sfresult:=fz_SendFile(false,fn,offset);
    end;
    if sfresult=fileRepeat then
      logerror('error - resending file');
  until (sfresult=fileOK) or (sfresult=fileError);
  if sfresult<>fileOK then
    logerror('error sending file');
  SendFile:=(sfresult=fileOK);
end;

function RecFile(fn:pathstr; size:longint):boolean;    { true = ok }
var rfresult : shortint;
begin
  FileRetries:=0;
  repeat
    inc(FileRetries);
    startfile(false,extractfilename(fn),size);
    case proto of
      'g','G' : rfresult:=g_RecFile(fn);
      'e'     : rfresult:=e_RecFile(fn);
      'f'     : rfresult:=fz_RecFile(true,fn);
      'z'     : rfresult:=fz_RecFile(false,fn);
    end;
    if rfresult=fileRepeat then
      logerror('error - repeating file');
  until (rfresult=fileOK) or (rfresult=fileError);
  if rfresult<>fileOK then
    logerror('error receiving file');
  RecFile:=(rfresult=fileOK);
end;

function InitProtocol:boolean;
begin
  case proto of
    'g','G' : InitProtocol:=g_InitProtocol;
    'e','z' : InitProtocol:=true;
    'f'     : InitProtocol:=f_InitProtocol;
  end;
end;

procedure ExitProtocol;
begin
  case proto of
    'g','G' : g_ExitProtocol;
    'f'     : f_ExitProtocol;
  end;
end;


{ --- uucico -------------------------------------------------------- }

function InitHandshake(var waittime:integer):boolean;
var n,i : integer;                            { --- uucp - Init-Handshake }
    s   : string[80];
    ti  : longint;
begin
  ti:=ticker;
  writeln;                  { Shere: s. XP7.login }
  {$ifdef sim}
    assign(siminput,'uusim');        { Eingabedatei fuer simulierten Netcall }
    reset(siminput);
  {$endif}
  {$ifdef debug}
    assign(debugfile,'uu-debug');    { Mitschnitt aller empfangenen Zeichen }
    rewrite(debugfile);
  {$endif}
  InitHandshake:=false;
  mdelay(500);
  flushinput(comnr);
  SendStr(^P'S'+pointname+iifs(SizeN,' -N','')+#0);
  time(LoginTimeout);
  n:=5;
  repeat                     { warten auf ROK oder Fehlermeldung }
    s:=GetStr; dec(n);
  until timeout(true) or (n=0) or (LeftStr(s,1)='R') or break;
  if break then exit;
  if LeftStr(s,3)<>'ROK' then begin
    if s='RLOGIN' then s:=s+' - wrong login name';
    if nocarrier then
      LogError('connection terminated by remote site')
    else
      LogError('got "'+s+'" - aborting');
    exit;
    end;
  WrLog(' ','UUCP connection established');
  SizeN:=(s='ROKN');         { size negotiation }
  if SizeN then begin
    rmsg(getres2(2300,38));  { 'Using size negotiation' }
    wrlog(' ','using size negotiation');
    end;
  n:=5;
  time(LoginTimeout);
  repeat
    s:=GetStr; dec(n);
  until timeout(true) or (n=0) or (LeftStr(s,1)='P') or break;
  if break then exit;
  if LeftStr(s,1)<>'P' then begin
    LogError('got '+s+' - aborting');
    exit;
    end;
  delete(s,1,1);
  WrLog('~','remote protocols: '+s);
  if not multipos(uuprotos,s) then begin
    SendStr(^P'UN'#0);
    rmsg(getreps2(2300,37,s));    { 'no common protocol (remote supports %s)' }
    LogError('no common protocol');
    _ende:=true; exit;
    end;
  for i:=length(uuprotos) downto 1 do     { bestes Protokoll ermitteln }
    if cpos(uuprotos[i],s)>0 then proto:=uuprotos[i];
  SendStr(^P'U'+proto+#0);
  WrLog(' ','selected protocol'+sp(cpos(proto,s))+'^');
  ShowProto(s);
  if InitProtocol then
    InitHandshake:=true
  else
    ExitProtocol;
  waittime:=secondsfrom(ti);
end;


procedure FinalHandshake;           { --- uucp - Handshake vor Hangup }
var b : byte;
begin
  if not NoCarrier then begin
    sendstr(^P'OOOOOO');
    mdelay(800);
    if ParDebug then
      while receive(comnr,b) do
        wrdebug(chr(b));
    flushinput(comnr);
    end;
  {$ifdef debug}
    close(debugfile);
  {$endif}
  {$ifdef sim}
    close(siminput);
  {$endif}
end;


{ Mails/News/Dateien senden, Dateien anfordern }

function SendFiles(CommandFile:pathstr; var sendtime,rectime:longint):boolean;
var t   : ^text;
    s   : string;
    s2  : string;
    sf  : string;
    o   : longint;
    fn  : pathstr;
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
        if not SizeN and (pos('""',s)>0) then
          s:=trim(LeftStr(s,pos('""',s)-1));
        WrLog('+','sending '+fn+' as '+sf);
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
          WrLog('*','sent '+strs(fs)+' bytes, '+strs(fs div secs)+' cps, '+
                    strs(transdata.errors)+' errors');
          s2:=RepeatGetcommand('C');   { CY / CN }
          if s2='' then begin
            addtime(sendtime); goto ende; end;
          if LeftStr(s2,2)='CN' then
            LogError('remote error: could not move file');
          end
        else if LeftStr(s2,2)='SN' then begin
          case s2[3] of
            '2' : begin
                    rmsg(getres2(2300,42));   { 'Fehler 2' }
                    logerror('remote refuses file');
                  end;
            '4' : begin
                    rmsg(getres2(2300,44));   { 'Fehler 4 - Temporaerdatei kann nicht erzeugt werden' }
                    logerror('remote can''t create temp file');
                  end;
            '6','7' : begin
                        rmsg(getres2(2300,46));  { 'Fehler 6 - kein Platz fuer Datei' }
                        logerror('remote disk full');
                      end;
            '8' : begin
                    rmsg(getres2(2300,48));   { 'Fehler 8 - Datei schon vorhanden' }
                    logerror('file does already exist');
                  end;
          else    logerror('remote refuses file');
          end;
          end;
        end
      else
        LogError('outgoing file not found: '+fn);
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
        logerror('invalid request destination file: '+fn);
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
      if SizeN then
        s:=s+' '+strs(diskfree(ord(FilePath[1])-64) div 3);
      wrlog('+','requesting file: '+sf);
      if not SendCommand(s) then begin
        addtime(rectime); exit; end;
      s2:=GetCommand;
      if LeftStr(s2,2)='RY' then begin
        wrlog(' ','request accepted');
        GetRequestFilesize;
        if RecFile(FilePath+fn,size) then begin
          fs:=_filesize(filepath+fn);
          secs:=max(1,secondsfrom(ti));
          wrlog('*','received '+fn+', '+strs(fs)+' bytes, '+strs(fs div secs)+
                    ' cps, '+strs(transdata.errors)+' errors');
          if SendCommand('CY') then;
          end;
        end
      else
        if LeftStr(s2,3)='RN6' then
          logerror('file too large')
        else if LeftStr(s2,2)='RN' then
          logerror('request refused or file not found')
        else
          logerror('unknown error');
      addtime(rectime);
      end;

  next:
    end;
  SendFiles:=true;
ende:
  close(t^);
  dispose(t);
end;


function GetSlave(var ok:boolean):boolean;
var s : string[20];
    n : byte;
begin
  GetSlave:=false; ok:=false;
  if not SendCommand('H') then exit;   { uucp-Befehl "H" }
  n:=10;
  repeat
    dec(n);
    if n=0 then exit;
    s:=LeftStr(GetCommand,2);
  until (s='HN') or (s='HY') or NoCarrier or break;
  if not NoCarrier and not break then begin
    ok:=true;
    if s='HN' then begin            { HN -> Daten empfangen; HY -> Ende }
      GetSlave:=true;
      wrlog('+','HN: remote has data for you');
      end
    else begin
      wrlog('+','HY: no files to receive');
      if SendCommand('HY') then;
      end;
    end;
end;


function U2DOSfile(s:string):string;
var i : integer;
    b : byte;
begin
  s:=s[1]+'-'+RightStr(s,5);
  b:=0;
  for i:=0 to 3 do            { Schreibweise in einem Byte codieren }
    if (s[i+4]>='A') and (s[i+4]<='Z') then
      inc(b,1 shl i);
  U2DOSfile:=s+hex(b,1);
end;


{$I xpfiles.inc}


{ function Unix2DOSfile(fn,destdir:pathstr):pathstr; }


{ Mails/News empfangen }

function RecFiles(var rectime:longint):boolean;
var ok    : boolean;
    s     : string;
    fn    : pathstr;
    c     : char;
    p     : byte;
    ti,tf : longint;
    size  : longint;
    fs    : longint;
    secs  : longint;
    n     : integer;
    source: pathstr;

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
                    if SizeN then getFilesize;
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
                    wrlog('+','receiving '+s+' as '+uppercase(fn));
                    end
                  else begin
                    s:=Unix2DOSfile(s,FilePath);
                    if s='' then s:=Unix2DOSfile(source,FilePath);
                    if s='' then s:='unnamed';
                    fn:=FilePath+s;
                    wrlog('S','receiving '+s+' as '+uppercase(fn));
                    end;
                  if not RecFile(fn,size) then
                    goto ende;
                  fs:=_filesize(fn);
                  secs:=max(1,secondsfrom(tf));
                  wrlog('*','received '+strs(fs)+' bytes, '+strs(fs div secs)+
                            ' cps, '+strs(transdata.errors)+' errors');
                  if SendCommand('CY') then;
                  end;
              end;
      else    if c<>'H' then begin
                logerror('unknown UUCP command: '+s);
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


procedure CloseLogfiles;
begin
  if ulogopen then begin
    writeln(uulog^);
    close(uulog^);
    dispose(uulog);
    ulogopen:=false;
    end;
  if dlogopen then begin
    close(deblog^);
    dispose(deblog);
    dlogopen:=false;
    end;
end;


procedure InitInterface;
begin
  SetComParams(comnr,fossil,ModemPort,IRQ);
  if not fossil then begin   { FOSSIL -> Schnittstelle ist noch aktiviert }
    SetTriggerLevel(tlevel);
    if not SetUART(comnr,baud,PNone,8,1,not IgnCTS) then
      StopError(getres(107));     { 'Ungueltige Baudrate' }
    ActivateCom(comnr,8192,true);
    end
  else
    AllocComBuffer(comnr,8192);
end;

procedure ExitInterface;
begin
{  log('-','exiting'); }
  flushinput(comnr);
  DropDtr(comnr);
  mdelay(300);
  if not fossil then
    ReleaseCom(comnr)
  else
    FreeComBuffer(comnr);
end;


function fuucico(start:longint; var ende:boolean;
                var waittime:integer; var sendtime,rectime:longint):integer;
begin
  SizeN:=sizenego;
  fillchar(transdata,sizeof(transdata),0);
  transdata.connstart:=start;
  InitInterface;
  if ParDebug then begin             { UUDEBUG.LOG: ucico-Debug-Logfile }
    new(deblog);
    assign(deblog^,DebugLog);
    rewrite(deblog^);
    dlogopen:=true;
    end;
  if uulogfile<>'' then begin        { Temp: XPUUCP.LOG }
    new(uulog);
    assign(uulog^,uulogfile);
    rewrite(uulog^);
    writeln(uulog^,'----------  ',date,' -> ',boxname);
    ulogopen:=true;
    end
  else
    uulog:=nil;
  (* if ParDebug then mcur:=curon
  else mcur:=curoff; *)
  ShowWindow;
  recs:=''; _ende:=false;
  sendtime:=0; rectime:=0; waittime:=0;

  if InitHandshake(waittime) then begin     { enthaelt InitProtocol }
    if SendFiles(CommandFile,sendtime,rectime) then
      if RecFiles(rectime) then begin
        fuucico:=uu_ok; _ende:=true; end
      else fuucico:=uu_recerr
    else fuucico:=uu_senderr;
    ExitProtocol;
    end
  else
    fuucico:=uu_nologin;

  if NoCarrier and not break then
    LogError('carrier lost');
  FinalHandshake;
  rmsg(getres2(2300,50));    { 'Modem auflegen' }
  WrLog('+','hanging up');
  ExitInterface;
  CloseLogfiles;
  closewindow;
  freeres;
  ende:=_ende;
end;

{
  $Log$
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

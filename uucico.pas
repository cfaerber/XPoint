{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - UUCICO }
{$I XPDEFINE.INC }

{ define debug}    { Empfangs-Logfile UU-DEBUG.                }
{ define sim}      { Simulierter Netcall; Eingabedatei: UUSIM. }

uses  crt,dos,uart,typeform,fileio,video,winxp,inout,resource,xpglobal,lfn;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      uu_parerr  = 1;
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;
      XFerDir    = 'SPOOL\';
      uucicores  = 'uucicor.tmp';

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
      FilePath      : string = 'FILES\';
      commandfile   : string = '';
      uulogfile     : string = '';
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
      break         : boolean = false;      { <Esc> gedrÅckt }
      filetrans     : boolean = false;      { fÅr rerrmsg }
      ShowTime      : boolean = true;
      FileStarted   : boolean = false;
      Uselfn: boolean = false;


var   omx,omy      : byte;
      oldexit      : pointer;

      debugfile    : file of byte;     { Schnittstellen-Debug-Logfile  }
      siminput     : file of byte;     { Eingabedatei fÅr sim. Netcall }
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



{ --- Initialisierungen; Allgemeines ---------------------------------- }

procedure logo;
var t : text;
begin
  assign(t,''); rewrite(t);
  writeln(t);
  writeln(t,'XP UUCICO ',verstr,'  (c) ''93-99 by Peter Mandrella');
  writeln(t);
  writeln(t, 'OpenXP-Version ',verstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln(t);
  if FOSSILdetect then begin
    writeln(t,'FOSSIL driver detected');
    writeln(t);
    end;
  close(t);
  omx:=wherex; omy:=wherey;
end;


procedure InitVar;
begin
  uulog:=nil; deblog:=nil;
  with col do
    if color then begin
      colmailer:=$70; colmailerhi:=$7f; colmailerstat:=$7e; end
    else begin
      colmailer:=7; colmailerhi:=$f; colmailerstat:=$f; end;
end;


procedure StopError(txt:string);
begin
  writeln(txt,#7);
  delay(2000);
  if resopen then CloseResource;
  halt;
end;


procedure ReadConfig;
var t    : text;
    s0,s : string;
    p    : byte;
    id   : string[20];
    bool : boolean;

  procedure helppage;
  var t : text;
  begin
    assign(t,''); rewrite(t);
    writeln(t,'UUCICO <ConfigFile> [CommandFile]');
    close(t);
    halt;
  end;

  procedure GetColors;
  var p : byte;
  begin
    p:=blankpos(s);
    if p>0 then begin
      col.colmailer:=ival(left(s,p-1));
      s:=trim(mid(s,p));
      p:=blankpos(s);
      if p=0 then
        col.colmailerhi:=ival(s)
      else begin
        col.colmailerhi:=ival(left(s,p-1));
        col.colmailerstat:=ival(mid(s,p+1));
        end;
      end;
  end;

  procedure GetByte(var b:byte);
  var p : byte;
  begin
    p:=blankpos(s);
    if p=0 then p:=length(s)+1;
    b:=ival(left(s,p-1));
    s:=trim(mid(s,p+1));
  end;

  procedure GetWindow;
  begin
    getbyte(DebugWinX1);
    getbyte(DebugWinX2);
    getbyte(DebugWinY1);
    getbyte(DebugWinY2);
    if DebugWinY2=0 then DebugWinX1:=0;
  end;

begin
  if (paramcount<1) or (paramcount>2) then Helppage;
  if paramcount=2 then CommandFile:=paramstr(2);
  assign(t,paramstr(1));
  reset(t);
  if ioresult<>0 then StopError('Config file missing: '+ustr(paramstr(1)));
  while not eof(t) do begin
    readln(t,s0);
    s:=trim(s0);
    p:=cpos('=',s);
    if (s<>'') and (left(s,1)<>';') and (left(s,1)<>'#') then
      if p=0 then StopError('Unknown option:  '+s)
      else begin
        id:=lstr(trim(left(s,p-1)));
        s:=trim(mid(s,p+1));
        bool:=(ustr(s)<>'N');
        if id='language'   then language:=s else
        if id='debug'      then ParDebug:=bool else
        if id='debugwindow'then GetWindow else
        if id='colors'     then GetColors else
        if id='server'     then Boxname:=s else
        if id='node'       then Pointname:=s else
        if id='maxwinsize' then maxwinsize:=minmax(ival(s),3,7) else
        if id='maxpacketsize'   then maxpacketsize:=minmax(ival(s),64,4096) else
        if id='varpacketsize'   then varpacketsize:=bool else
        if id='forcepacketsize' then forcepktsize:=bool else
        if id='protocols'       then UUprotos:=s else
        if id='sizenegotiation' then SizeNego:=bool else
        if id='filereqpath'     then FilePath:=s else
        if id='c-file'     then CommandFile:=s else
        if id='uulogfile'  then UUlogfile:=s else
        if id='fossil'     then fossil:=bool else
        if id='portnr'     then comnr:=minmax(ival(s),0,4) else
        if id='portadr'    then ModemPort:=hexval(s) else
        if id='irq'        then IRQ:=minmax(ival(s),0,15) else
        if id='triggerlevel' then tlevel:=minmax(ival(s),2,14) else
        if id='baud'       then Baud:=ival(s) else
        if id='ignorecd'   then IgnCD:=bool else
        if id='ignorects'  then IgnCTS:=bool else
        if id='userts'     then UseRTS:=bool else
        if id='onlinetime' then OnlineTime:=ival(s) else
        if id='releasetime' then ReleaseTime:=minmax(ival(s),0,3) else
        if id='maxfilesize' then maxfsize:=minmax(ival(s),0,9999) else
        if id='lfn' then begin EnableLFN; Uselfn := true end else
          writeln('Warning - unknown Option:  '+s0);
        end;
    end;
  close(t);
  if not FOSSILdetect then fossil:=false;
  if fossil then begin
    writeln('Using FOSSIL driver');
    writeln;
    end;
end;


procedure SetLanguage;
begin
  if not exist('XPUU-'+language+'.RES') then
    language:='E';
  if not exist('XPUU-'+language+'.RES') then
    StopError('XPUU-'+language+'.RES not found');
  OpenResource('XPUU-'+language+'.RES',40000);
  resopen:=true;
  with UWin do begin
    senden:=getres2(2300,10);
    empfangen:=getres2(2300,11);
    dateien:=getres2(2300,12);
    uebertragen:=getres2(2300,13);
    gesamt:=getres2(2300,14);
    dateigroesse:=getres2(2300,15);
    restzeit:=getres2(2300,16);
    blockgroesse:=getres2(2300,17);
    durchsatz:=getres2(2300,18);
    dgesamt:=getres2(2300,19);
    fehler:=getres2(2300,20);
    end;
end;


procedure TestConfig;
var i    : integer;
    perr : string[40];

  procedure rerror(nr:word);
  begin
    StopError(perr+getres(nr));
  end;

  procedure rerror1(nr:word; txt:string);
  begin
    StopError(perr+getreps(nr,txt));
  end;

begin
  perr:=getres(100);
  if Boxname='' then rerror(101);     { 'Server name missing' }
  if Pointname='' then rerror(102);   { 'Point name missing'  }
  if (Commandfile='') or not exist(CommandFile) then
    rerror(103); { 'UUCP Command file missing' }
  if comnr=0 then rerror(104);        { 'Port number missing' }
  if right(FilePath,1)<>'\' then FilePath:=FilePath+'\';
  if not ValidFilename(FilePath+'1$2$3.9x9') then
    rerror(105);                      { 'Illegal File Path' }
  if (uulogfile<>'') and not validfilename(uulogfile) then
    rerror1(106,ustr(uulogfile));     { 'Illegal logfile name: %s' }
  if onlinetime=0 then onlinetime:=ticker;
end;



{ --- uucico ---------------------------------------------------------- }

{ Dateinamen: X.nnnngssss <-> X-gssss }

const LoginTimeout  =  60;    { Hangup-Timeout bei uucp-Starthandshake  }
      InitTimeout   =   5;    { g: Repeat-Timeout bei INIT-Packets      }
      AckTimeout    =  10;    { g: Repeat-Timeout bei Warten auf ACK    }
      ExitTimeout   =   2;    { g: Repeat-Timeout bei CLOSE             }
      DataTimeout   =  15;    { g: Repeat-Timeout beim Warten auf Daten }
      DataTimeout2  =  60;    { g: Timeout bei öbertragung eines Datenpakets }
      eProtTimeout  =  90;    { e/z: Timeout beim Warten auf Daten      }

      DebugLog      =  'UUDEBUG.LOG';

      fileOK        =   0;    { Datei ok          }
      fileRepeat    =   1;    { Datei wiederholen }
      fileError     =   2;    { Datei - Abbruch   }


var   SizeN  : boolean;      { Size negotiation }
      recs   : string;
      proto  : char;         { ausgewÑhltes uucp-Protokoll }
      _ende  : boolean;      { Netcall vollstÑndig beenden }
      mcur   : curtype;

      transdata : record
                    connstart   : longint;      { Ticks: CONNECT }
                    files       : longint;
                    filesize    : longint;      { aktuelle Datei }
                    filestart   : longint;
                    transferred : longint;      { davon Åbertrg. }
                    blocksize   : word;
                    errors      : longint;
                    total       : longint;
                    totalfrom   : longint;      { Ticks: Sende/Empfangsstart }
                    sending     : boolean;
                  end;

      FileRetries: shortint;


procedure rmsg(txt:string); forward;
procedure LogError(txt:string); forward;

procedure testbrk;
begin
  if keypressed and (readkey=#27) then begin
    break:=true;
    rmsg(getres2(2300,51));
    logerror('user break');
    delay(500);
    end;
end;

function test_break:boolean;
begin
  testbrk;
  test_break:=break;
end;


{$ifdef sim}

  procedure sendbyte(b:byte);
  begin
  end;

  procedure sendblock(var data; size:word);
  begin
  end;

{$else}

  procedure sendbyte(b:byte);
  begin
    if IgnCTS then uart.SendByte(comnr,b)
    else HSendByte(comnr,b);
  end;

  procedure sendblock(var data; size:word);
  begin
    if IgnCTS then uart.sendblock(comnr,data,size)
    else hsendblock(comnr,data,size);
  end;

{$endif}


procedure sendstr(s:string);   { String 1:1 versenden }
var i : byte;
begin
  for i:=1 to length(s) do
    sendbyte(byte(s[i]));
  testbrk;
end;

procedure time(l:longint);
begin
  zaehler[2]:=l;
end;

{$ifdef sim}
  function NoCarrier:boolean;
  begin
    NoCarrier:=false;
  end;
{$else}
  function NoCarrier:boolean;
  begin
    NoCarrier:=(not IgnCD and not carrier(comnr)) or break;
  end;
{$endif}

function timeout(ctest:boolean):boolean;
begin
  {$ifdef sim}
    if zaehler[2]>0 then zaehler[2]:=5;    { Timeout auf 5 Sek. abkÅrzen }
  {$endif}
  timeout:=(zaehler[2]=0) or (ctest and NoCarrier);
end;

{$ifdef sim}
function receive(comnr:word; var b:byte):boolean;
begin {$I-}
  read(siminput,b);
  receive:=(ioresult=0);
end; {$I+}

{$else}
{$ifdef debug}

function receive(comnr:word; var b:byte):boolean;
begin
  if uart.receive(comnr,b) then begin
    write(debugfile,b);
    receive:=true;
    end
  else
    receive:=false;
end;
{$endif}   { debug }
{$endif}   { sim }

procedure wrdebug(s:string);
begin
  write(s);
  write(deblog^,s);
  testbrk;
end;

procedure wrldebug(s:string);
begin
  if ParDebug then begin
    writeln(s);
    writeln(deblog^,s);
    end;
end;

procedure WrdLn;
begin
  wrldebug('');
end;

procedure WrLog(c:char; txt:string);
begin
  if uulog<>nil then
    writeln(uulog^,c,' ',typeform.time,'  ',txt);
end;

procedure LogError(txt:string);
begin
  wrlog('#',txt);
end;

procedure tb;
var b : byte;
begin
  if receive(comnr,b) and (length(recs)<255) then begin
    inc(byte(recs[0]));
    recs[length(recs)]:=chr(b);
    if ParDebug then
      if b=0 then wrdebug('˙')
      else wrdebug(chr(b));
    end;
  multi2;
  testbrk;
end;

function GetStr:string;        { uucp-String empfangen }
begin
  repeat
    tb;
  until timeout(true) or break or
        ((recs<>'') and (recs[length(recs)] in [#0,#4,#10,#13]));
  if left(recs,1)=^P then
    delete(recs,1,1);
  if recs='' then GetStr:=''
  else GetStr:=left(recs,length(recs)-1);
  recs:='';
end;

{ warten, bis fÅr <secs> Sekunden kein Zeichen mehr empfangen wurde }
procedure flushserial(secs:byte);
var b : byte;
begin
  time(secs);
  repeat
    if receive(comnr,b) then time(secs)
    else multi2;
  until timeout(true);
  flushinput(comnr);
end;

procedure ComDebug;
var b : byte;
begin
  b:=0;
  repeat
    if receive(comnr,b) then
      write(hex(b,2),' ');
    if keypressed then begin
      b:=ord(readkey);
      if b<>27 then SendByte(b);
      end;
  until b=27;
end;

function Cval(s:string):longint;
begin
  LoString(s);
  if left(s,2)='0x' then Cval:=hexval(mid(s,3))
  else Cval:=ival(s);
end;


procedure blockwrite(var f:file; var data; size:word);
begin
  if UseRTS then DropRTS(comnr);
  system.blockwrite(f,data,size);
  if UseRTS then SetRTS(comnr);
end;


procedure blockread(var f:file; var data; size:word; var rr:word);
begin
  if UseRTS then DropRTS(comnr);
  system.blockread(f,data,size,rr);
  if UseRTS then SetRTS(comnr);
end;


{ --- Frontend ------------------------------------------------------ }

const wdt = 50;
      hgh = 14;

var   lwdt     : byte;
      mx,my    : byte;     { mx/my=Hauptfenster, fy=Pos. im File-Fenster }


function timeform(l:longint):string;
begin
  timeform:=formi(l div 3600,2)+':'+formi((l mod 3600) div 60,2)+':'+
            formi(l mod 60,2);
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


{$F+} procedure WriteOnline; {$F-}
const lasttick : longint = 0;
begin
  if not ParDebug and showtime and (abs(ticker-lasttick)>10) then begin
    dphback:=col.colmailerstat;
    disphard(mx+41,my+1,timeform(secondsfrom(onlinetime)));
    lasttick:=ticker;
    end;
end;


procedure ShowWindow;
var i     : integer;
    xx,xy : byte;
begin
  if ParDebug then begin
    if DebugWinX1<>0 then begin
      xx:=wherex; xy:=wherey;
      window(DebugWinX1,DebugWinY1,DebugWinX2,DebugWinY2);
      gotoxy(xx,xy+DebugWinY1-1);
      end;
    end
  else begin
    savecursor;
    cursor(curoff);
    window(1,1,80,25);
    mx:=15; my:=getscreenlines div 2 - 6;
    attrtxt(col.colmailer); forcecolor:=true;
    wpushs(mx,mx+wdt+1,my,my+hgh+1,'');
    inc(windmax,$1900);
    forcecolor:=false;
    lwdt:=50;
    wrt(mx,my+3,'√'+dup(lwdt,'ƒ')+'¥');
    wrt(mx,my+13,'√'+dup(lwdt,'ƒ')+'¥');
    wrt(mx+2,my+1,getres2(2300,1));      { 'Server' }
    wrt(mx+2,my+2,getres2(2300,2));      { 'Protokoll' }
    wrt(mx+25,my+1,getres2(2300,5));     { 'Online-Zeit' }
    attrtxt(col.colmailerstat);
    wrt(mx+13,my+1,boxname);
    WriteOnline;
    end;
end;

procedure closewindow;
begin
  if ParDebug then exit;
  wpop;
  restcursor;
end;

procedure rmsg(txt:string);
begin
  if ParDebug then
    wrldebug(txt)
  else begin
    attrtxt(col.colmailer);
    wrt(mx+2,my+hgh,forms(txt,lwdt-2));
    end;
end;

procedure rerrmsg(txt:string);
begin
  rmsg(iifs(filetrans,strs(transdata.transferred)+': ','')+txt);
end;

procedure ShowProto(all:string);
begin
  rmsg(reps(getreps2(2300,3,proto),all));   { 'Using UUCP protocol: %s (of %s)' }
  if ParDebug then exit;
  attrtxt(col.colmailerstat);
  wrt(mx+13,my+2,'UUCP-'+proto);
  case proto of
    'g','G' : begin
                attrtxt(col.colmailer);
                wrt(mx+25,my+2,getres2(2300,4));    { 'Paketgrî·e' }
              end;
  end;
end;

procedure ShowPacketSize(packet1,packet2:word);
begin
  if ParDebug then
    wrldebug(getres2(2300,6)+strs(packet1)+'/'+strs(packet2))   { 'packet size: ' }
  else begin
    attrtxt(col.colmailerstat);
    gotoxy(mx+38,my+2);
    write(packet1:4);
    attrtxt(col.colmailer);
    write(' / ');
    attrtxt(col.colmailerstat);
    write(packet2:4);
    end;
end;

procedure ShowWinsize(winsize1,winsize2:word);
begin
  if ParDebug then
    wrldebug(getres2(2300,7)+strs(winsize1)+'/'+strs(winsize2))   { 'window size: ' }
  else begin
  { attrtxt(col.colmailerstat);
    gotoxy(mx+38,my+2);
    write(winsize1:4);
    attrtxt(col.colmailer);
    write(' / ');
    attrtxt(col.colmailerstat);
    write(winsize2:4); }
    end;
end;

procedure WriteTransfer;
var sec,cps : longint;
begin
  if ParDebug then exit;
  attrtxt(col.colmailerhi);
  with transdata do begin
    if filesize>0 then begin
      gotoxy(mx+13,my+6); write(filesize:8);
      end;
    gotoxy(mx+13,my+7); write(transferred:8);
    gotoxy(mx+13,my+8); write(total:8);
    gotoxy(mx+39,my+6); write(blocksize:5);
    gotoxy(mx+39,my+10); write(errors:5);
    sec:=secondsfrom(filestart);
    cps:=0;
    if sec>0 then begin
      cps:=transferred div sec;
      gotoxy(mx+39,my+7);
      write(cps:5);
      if (filesize>0) and (cps>20) then begin
        gotoxy(mx+13,my+10);
        write(timeform((filesize-transferred)div cps));
        end;
      end;
    sec:=secondsfrom(totalfrom);
    if sec>0 then begin
      if files>1 then
        cps:=total div sec;
      if cps>20 then begin
        gotoxy(mx+39,my+8);
        write(cps:5);
        end;
      end;
    if filesize>0 then
      wrt(mx+2,my+12,
          dup(min(lwdt-3,system.round((lwdt-3)*(transferred/filesize))),'±'));
    end;
  WriteOnline;
  testbrk;
end;

procedure ResetTransdat(blksize:word);
begin
  transdata.total:=0;
  transdata.totalfrom:=ticker;
  transdata.files:=0;
  transdata.blocksize:=blksize;
end;

procedure StartFile(send:boolean; fn:string; size:longint);
begin
  inc(transdata.files);
  if ParDebug then begin
    if send then wrdebug('sending ')
    else wrdebug('receiving ');
    wrdebug('file: '+fn+' ');
    end
  else begin
    attrtxt(col.colmailer);
    clwin(mx+1,mx+lwdt-1,my+4,my+11);   { Dateifenster neu aufbauen }
    with UWin do begin
      if fn<>'' then
        wrt(mx+2,my+4,iifs(send,senden,empfangen)); { 'Senden' / 'Empfangen' }
      wrt(mx+27,my+4,dateien);                    { 'Dateien' }
      wrt(mx+2,my+7,uebertragen);                 { 'Åbertragen' }
      wrt(mx+2,my+8,gesamt);                      { 'gesamt' }
      wrt(mx+2,my+6,dateigroesse);                { 'Dateigrî·e' }
      wrt(mx+2,my+10,restzeit);                   { 'Restzeit' }
      wrt(mx+27,my+6,blockgroesse);               { 'Blockgrî·e' }
      wrt(mx+27,my+7,durchsatz);                  { 'Durchsatz          cps' }
      wrt(mx+27,my+8,dgesamt);                    { 'gesamt             cps' }
      wrt(mx+27,my+10,fehler);                    { 'Fehler' }
      end;
    wrt(mx+2,my+12,sp(lwdt-2));
    wrt(mx+2,my+hgh,sp(lwdt-2));
    attrtxt(col.colmailerhi);
    wrt(mx+13,my+4,fn);
    wrt(mx+39,my+4,strsn(transdata.files,5));
    end;
  with transdata do begin
    sending:=send;
    filesize:=size; transferred:=0; errors:=0;
    filestart:=ticker;
    WriteTransfer;
    end;
  FileStarted:=true;
end;

procedure NewError;
begin
  inc(transdata.errors);
  if FileStarted then
    WriteTransfer
  else
    StartFile(false,'',0);
end;

procedure ShowFtype(var data; len:word);
var s    : string;
    user : string;
    p,p2 : byte;
begin
  s[0]:=chr(min(255,len));
  move(data,s[1],length(s));
  p:=cpos(#10,s);
  if p>0 then TruncStr(s,p-1);
  if left(s,3)='#! ' then
    if copy(s,4,5)='rnews' then s:=getres2(2300,60) else      { 'ungepacktes Newspaket' }
    if copy(s,4,8)='cunbatch' then s:=getres2(2300,61) else   { 'gepacktes Newspaket (compress)' }
    if copy(s,4,8)='funbatch' then s:=getres2(2300,62) else   { 'gepacktes Newspaket (freeze)' }
    if copy(s,4,8)='gunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    if copy(s,4,8)='zunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    s:=''
  else
    if left(s,5)='HELO '  then s:=getres2(2300,64) else   { 'ungepacktes Mailpaket' }
    if left(s,2)=#$1f#$9d then s:=getres2(2300,65) else   { 'gepackte Datei (compress)' }
    if left(s,2)=#$1f#$9f then s:=getres2(2300,66) else   { 'gepackte Datei (freeze) }
    if left(s,2)=#$1f#$8b then s:=getres2(2300,67) else   { 'gepackte Datei (gzip) }
    begin
      LoString(s);
      if (left(s,5)='>from') or (left(s,4)='from') then begin
        p:=pos('remote from',s);
        if p>0 then begin
          delete(s,1,blankpos(s));
          user:=left(s,blankpos(s)-1);
          p2:=length(user);
          while (p2>0) and (user[p2]<>'!') do dec(p2);
          if p2>0 then begin
            s:=getres2(2300,68)+mid(user,p2+1)+'@';
            user:=left(user,p2-1);
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
  if s<>'' then rmsg(s);
end;


{$I uucp-g.inc}


{ --- UUCP-e -------------------------------------------------------- }

function e_SendCommand(s:string):boolean;   { 0-terminierten Befehl senden }
var i : byte;
begin
  SendStr(s+#0);
  e_SendCommand:=not NoCarrier;
end;


function e_GetCommand:string;               { 0-terminierten Befehl holen }
var s   : string;
    l,b : byte;
begin
  l:=0;
  time(eProtTimeout);
  b:=255;
  repeat
    if receive(comnr,b) then begin
      time(eprotTimeout);
      if (b>=32) and (l<255) then begin
        inc(l);
        s[l]:=chr(b);
        end;
      end
    else begin
      multi2;
      testbrk;
      end;
  until (b=0) or timeout(true) or break;
  if timeout(true) or break then l:=0;
  s[0]:=chr(l);
  e_GetCommand:=s;
end;


procedure WreSize;
begin
  if ParDebug then begin
    write(#13,transdata.transferred,' Bytes');
    writeln(deblog^,strs(transdata.transferred)+' Bytes');
    end
  else
    WriteTransfer;
end;


function e_SendFile(fn:string; offset:longint):shortint;   { Datei senden }
const bufsize = 1024;
var f   : file;
    buf : array[0..bufsize-1] of byte;
    rr  : word;
    i   : integer;
begin
  assign(f,fn);
  resetfm(f,0);
  seek(f,offset);
  transdata.filesize:=filesize(f)-offset;
  if not ParDebug then
    WriteTransfer;         { Grî·e anzeigen }
  SendStr(left(strs(transdata.filesize)+dup(20,#0),20));   { LÑnge senden }
  if not NoCarrier then
    repeat
      blockread(f,buf,bufsize,rr);
      inc(transdata.transferred,rr);
      inc(transdata.total,rr);
      transdata.blocksize:=rr;
      for i:=0 to rr-1 do
        SendByte(buf[i]);
      WreSize;
    until eof(f) or NoCarrier or break;
  close(f);
  WrdLn;
  e_SendFile:=iif(NoCarrier,fileError,fileOK);
end;


function e_RecFile(fn:string):shortint;
const bufsize = 1024;
var len   : string[20];
    i     : integer;
    fs    : longint;
    b     : byte;
    f     : file;
    buf   : array[0..bufsize-1] of byte;
    bp    : word;
    ftyped: boolean;
begin
  e_RecFile:=fileError;
  time(eProtTimeout);
  i:=0;
  while (i<20) and not timeout(true) and not break do
    if receive(comnr,b) then begin
      inc(i);
      len[i]:=chr(b);
      time(eProtTimeout);
      end
    else begin
      multi2;
      testbrk;
      end;
  if timeout(true) or break then exit;
  while (i>0) and (len[i]=#0) do dec(i);
  len[0]:=chr(i);
  fs:=ival(len);        { Dateigrî·e }
  transdata.filesize:=fs;
  WriteTransfer;      { Grî·e anzeigen }
  assign(f,fn);
  rewrite(f,1);
  time(eProtTimeout);
  ftyped:=false;
  bp:=0;
  while (fs>0) and not timeout(true) and not break do begin
    if receive(comnr,b) then begin
      buf[bp]:=b; inc(bp);
      dec(fs);
      time(eProtTimeout);
      end
    else
      testbrk;
    if not ftyped and (bp>110) then begin
      ShowFtype(buf,bp);
      ftyped:=true;
      end;
    if (bp=bufsize) or (fs=0) then begin
      inc(transdata.transferred,bp);
      inc(transdata.total,bp);
      transdata.blocksize:=bp;
      WreSize;
      blockwrite(f,buf,bp);
      bp:=0;
      end;
    end;
  if not timeout(true) then
    e_Recfile:=fileOK;
  if bp>0 then begin
    WreSize;
    blockwrite(f,buf,bp);
    end;
  close(f);
  WrdLn;
end;


{ --- UUCP-f/z ------------------------------------------------------ }

function fz_SendCommand(s:string):boolean;  { CR-terminierten Befehl senden }
var i : byte;
begin
  SendStr(s+#13);
  fz_SendCommand:=not NoCarrier;
end;


function fz_GetCommand:string;              { CR-terminierten Befehl holen }
var s   : string;
    l,b : byte;
begin
  l:=0;
  time(eProtTimeout);
  b:=255;
  repeat
    if receive(comnr,b) then begin
      time(eProtTimeout);
      if (b>=32) and (l<255) then begin
        inc(l);
        s[l]:=chr(b);
        end;
      end
    else begin
      multi2;
      testbrk;
      end;
  until (b=13) or timeout(true) or break;
  if timeout(true) then l:=0;
  s[0]:=chr(l);
  fz_GetCommand:=s;
end;


procedure WrzSize;
begin
  if ParDebug then begin
    write(#13,transdata.transferred,' Bytes');
    writeln(deblog^,strs(transdata.transferred)+' Bytes');
    end
  else
    WriteTransfer;
end;


function fz_SendFile(fprot:boolean; fn:string; offset:longint):shortint;
const bufsize = 1024;                                     { Datei senden }
var f   : file;
    buf : array[0..bufsize-1] of byte;
    rr  : word;
    i   : integer;
    chk : word;
    b   : byte;
    cmd : string[10];
begin
  assign(f,fn);
  resetfm(f,0);
  seek(f,offset);
  transdata.filesize:=filesize(f)-offset;
  WriteTransfer;         { Grî·e anzeigen }
  chk:=$ffff;
  if not NoCarrier then
    repeat
      blockread(f,buf,bufsize,rr);
      inc(transdata.transferred,rr);
      inc(transdata.total,rr);
      transdata.blocksize:=rr;
      for i:=0 to rr-1 do begin
        b:=buf[i];
        if chk>$7fff then chk:=chk shl 1 + 1
        else chk:=chk shl 1;
        inc(chk,b);
        if fprot then                        { f-Protokoll }
          case b of
              0..$1f : begin SendByte($7a); SendByte(b+$40); end;
            $20..$79 : SendByte(b);
            $7a..$7f : begin SendByte($7b); SendByte(b-$40); end;
            $80..$9f : begin SendByte($7c); SendByte(b-$40); end;
            $a0..$f9 : begin SendByte($7d); SendByte(b-$80); end;
            $fa..$ff : begin SendByte($7e); SendByte(b-$c0); end;
          end
        else
          if (b<$7a) or (b>$7f) then         { z-Protokoll }
            SendByte(b)
          else begin
            SendByte($7b);                   { $7a..$7f escapen }
            SendByte(b-$40);
            end;
        end;
      WrzSize;
    until eof(f) or NoCarrier or break;
  if not NoCarrier then
    SendStr(#$7e#$7e+hex(chk,4)+#13);      { Checksumme senden }
  close(f);
  WrdLn;
  if NoCarrier then
    fz_SendFile:=fileError
  else begin
    cmd:=fz_GetCommand;
    if cmd='G' then fz_SendFile:=fileOK else
    if cmd='R' then fz_SendFile:=fileRepeat
    else fz_SendFile:=fileError;
    end;
end;


function fz_RecFile(fprot:boolean; fn:string):shortint;
const bufsize = 1024;
var b       : byte;
    f       : file;
    buf     : array[0..bufsize-1] of byte;
    bp,chk  : word;
    ende    : boolean;
    fehler  : boolean;
    special : byte;
    chex    : string[5];
    len     : byte;
    ftyped  : boolean;

  procedure putchar(b:byte);
  begin
    buf[bp]:=b; inc(bp);
    if chk>$7fff then chk:=chk shl 1 + 1
    else chk:=chk shl 1;
    inc(chk,b);
  end;

  procedure wrongbyte;
  begin
    wrldebug(' got wrong byte: '+lstr(hex(b,2)));
  end;

  procedure fescape(range0,range1:byte; ofs:integer);
  begin
    if fprot then begin
      if (b>=range0) and (b<=range1) then inc(b,ofs)
      else wrongbyte;
      putchar(b);
      special:=0;
      end
    else begin
      wrongbyte;
      if (b>=$7a) and (b<=$7e) then
        special:=b
      else begin
        putchar(b);
        special:=0;
        end;
      end;
  end;

begin
  assign(f,fn);
  rewrite(f,1);
  time(eProtTimeout);
  bp:=0;
  ende:=false; fehler:=false; ftyped:=false;
  chk:=$ffff; special:=0;

  while not (ende or fehler or timeout(true) or break) do begin
    if receive(comnr,b) then begin
      case special of
        0   : if (b>=$7a) and (b<=$7e) then    { 0 oder anderes Zeichen }
                special:=b
              else begin
                putchar(b);
                special:=0;
                end;
        $7a : fescape($40,$5f,-$40);
        $7b : begin
                if (b>=$3a) and (b<=$3f) then inc(b,$40)
                else wrongbyte;
                putchar(b);
                special:=0;
              end;
        $7c : fescape($40,$5f,$40);
        $7d : fescape($20,$79,$80);
        $7e : if b=$7e then begin      { Ende }
                len:=0;
                while not timeout(true) and (len<5) do
                  if receive(comnr,b) then begin
                    inc(len); chex[len]:=chr(b and $7f);
                    end;
                chex[0]:=chr(len);
                wrldebug('Got Checksum: '+left(chex,4)+'  expected: '+lstr(hex(chk,4)));
                if timeout(true) or (chex[5]<>#13) or
                   (hexval(left(chex,4))<>chk) then
                  fehler:=true
                else
                  ende:=true;
                end
              else
                fescape($3a,$3f,$c0);
        else  begin
                wrongbyte;
                putchar(b);
                special:=0;
              end;
      end;
      time(eProtTimeout);
      end
    else begin
      multi2;
      testbrk;
      end;
    if not ftyped and (bp>110) then begin
      ShowFtype(buf,bp);
      ftyped:=true;
      end;
    if (bp=bufsize) or ende then begin
      inc(transdata.transferred,bp);
      inc(transdata.total,bp);
      transdata.blocksize:=bp;
      WrzSize;
      blockwrite(f,buf,bp);
      bp:=0;
      end;
    end;    { while }

  if bp>0 then begin
    WrzSize;
    blockwrite(f,buf,bp);
    end;
  fz_Recfile:=fileError;
  if not NoCarrier then
    if timeout(false) or fehler then
      if FileRetries<iif(filesize(f)<10000,4,3) then begin
        flushserial(1);
        wrldebug('File error - Repeating ...');
        if fz_SendCommand('R') then     { Repeat }
          fz_Recfile:=fileRepeat;
        end
      else begin
        wrldebug('Error - quitting ...');
        if fz_SendCommand('Q') then;    { Quit }
        end
    else begin
      wrldebug('File good');
      if fz_SendCommand('G') then;      { Good }
      fz_Recfile:=fileOK;
      end;
  close(f);
  WrdLn;
end;


function f_InitProtocol:boolean;
begin
  mdelay(1000);
  f_Initprotocol:=true;
end;

procedure f_ExitProtocol;
begin
  mdelay(1000);
end;


{ --- UUCP-Verteiler ------------------------------------------------ }

function SendCommand(s:string):boolean;   { true = ok }
var result : boolean;
begin
  wrldebug('sending command: '+s);
  case proto of
    'g','G' : result:=g_SendCommand(s);
    'e'     : result:=e_SendCommand(s);
    'f','z' : result:=fz_SendCommand(s);
  end;
  if not result then begin
    if blankpos(s)>0 then s:=left(s,blankpos(s)-1);
    LogError('error sending command "'+s+'"');
    end;
  SendCommand:=result;
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
    if (s<>'') and (left(s,1)<>c) then
      wrldebug('unexpected command: '+s);
    dec(n);
  until (left(s,1)=c) or (n=0) or nocarrier;
  RepeatGetcommand:=s;
end;

function SendFile(fn:string; offset:longint):boolean;   { true = ok }
var result : shortint;
begin
  repeat
    startfile(true,getFileName(fn),_filesize(fn));
    case proto of
      'g','G' : result:=g_SendFile(fn,offset);
      'e'     : result:=e_SendFile(fn,offset);
      'f'     : result:=fz_SendFile(true,fn,offset);
      'z'     : result:=fz_SendFile(false,fn,offset);
    end;
    if result=fileRepeat then
      logerror('error - resending file');
  until (result=fileOK) or (result=fileError);
  if result<>fileOK then
    logerror('error sending file');
  SendFile:=(result=fileOK);
end;

function RecFile(fn:string; size:longint):boolean;    { true = ok }
var result : shortint;
begin
  FileRetries:=0;
  repeat
    inc(FileRetries);
    startfile(false,getFileName(fn),size);
    case proto of
      'g','G' : result:=g_RecFile(fn);
      'e'     : result:=e_RecFile(fn);
      'f'     : result:=fz_RecFile(true,fn);
      'z'     : result:=fz_RecFile(false,fn);
    end;
    if result=fileRepeat then
      logerror('error - repeating file');
  until (result=fileOK) or (result=fileError);
  if result<>fileOK then
    logerror('error receiving file');
  RecFile:=(result=fileOK);
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
    assign(siminput,'uusim');        { Eingabedatei fÅr simulierten Netcall }
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
  until timeout(true) or (n=0) or (left(s,1)='R') or break;
  if break then exit;
  if left(s,3)<>'ROK' then begin
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
  until timeout(true) or (n=0) or (left(s,1)='P') or break;
  if break then exit;
  if left(s,1)<>'P' then begin
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

function SendFiles(CommandFile:string; var sendtime,rectime:longint):boolean;
var t   : ^text;
    s   : string;
    sf  : string;
    s2  : string[20];
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
    size:=Cval(left(s2,p-1));
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

    if left(s,2)='S ' then begin       { ----- Datei senden }
      ti:=ticker;
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p>0 then begin
        fn:=left(sf,p-1);
        sf:=mid(sf,p+1);
        sf:=left(sf,blankpos(sf)-1);
        end
      else
        fn:=sf;
      if multipos(':/',fn) then begin     { File Attach }
        for p:=1 to length(fn) do
          if fn[p]='/' then fn[p]:='\';
        end
      else
        fn:=XFerDir+fn[length(fn)-4]+'-'+right(fn,4)+'.OUT';
      if exist(fn) then begin
        if not SizeN and (pos('""',s)>0) then
          s:=trim(left(s,pos('""',s)-1));
        WrLog('+','sending '+fn+' as '+sf);
        if not SendCommand(s) then begin
          addtime(sendtime); goto ende; end;
        s2:=RepeatGetcommand('S');   { SY / SN }
        if s2='' then begin
          addtime(sendtime); goto ende; end;
        if left(s2,2)='SY' then begin
          s2:=trim(mid(s2,4));
          p:=blankpos(s2);
          if p>0 then s2:=left(s2,p-1);
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
          if left(s2,2)='CN' then
            LogError('remote error: could not move file');
          end
        else if left(s2,2)='SN' then begin
          case s2[3] of
            '2' : begin
                    rmsg(getres2(2300,42));   { 'Fehler 2' }
                    logerror('remote refuses file');
                  end;
            '4' : begin
                    rmsg(getres2(2300,44));   { 'Fehler 4 - TemporÑrdatei kann nicht erzeugt werden' }
                    logerror('remote can''t create temp file');
                  end;
            '6','7' : begin
                        rmsg(getres2(2300,46));  { 'Fehler 6 - kein Platz fÅr Datei' }
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

    if left(s,2)='R ' then begin       { ----- Datei anfordern }
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p=0 then goto next;
      fn:=trim(mid(sf,p+1));
      sf:=left(sf,p-1);
      p:=blankpos(fn);
      if p=0 then goto next;
      fn:=left(fn,p-1);
      if not validfilename(FilePath+fn) then begin
        logerror('invalid request destination file: '+fn);
        goto next;
        end;
      while exist(FilePath+fn) do begin      { Neuen Dateinamen erzeugen }
        p:=cpos('.',fn);
        if p=0 then
          fn:=fn+'.001'
        else
          if right(fn,1)='9' then
            fn:=left(fn,p)+formi(min(999,ival(mid(fn,p+1)))+1,3)
          else
            fn:=left(fn,length(fn)-1)+strs(ival(right(fn,1))+1);
        end;
      ti:=ticker;
      if SizeN then
        s:=s+' '+strs(fileio.diskfree(ord(FilePath[1])-64) div 3);
      wrlog('+','requesting file: '+sf);
      if not SendCommand(s) then begin
        addtime(rectime); exit; end;
      s2:=GetCommand;
      if left(s2,2)='RY' then begin
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
        if left(s2,3)='RN6' then
          logerror('file too large')
        else if left(s2,2)='RN' then
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
    s:=left(GetCommand,2);
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
  s:=s[1]+'-'+right(s,5);
  b:=0;
  for i:=0 to 3 do            { Schreibweise in einem Byte codieren }
    if (s[i+4]>='A') and (s[i+4]<='Z') then
      inc(b,1 shl i);
  U2DOSfile:=s+hex(b,1);
end;


{$I xpfiles.inc}    { function Unix2DOSfile(fn,destdir:string):string; }


{ Mails/News empfangen }

function RecFiles(var rectime:longint):boolean;
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
  var ss   : string[100];
      pp,i : byte;
  begin
    ss:=s;
    for i:=1 to 6 do begin
      pp:=blankpos(ss);
      ss:=trim(mid(ss,pp+1));
      end;
    pp:=blankpos(ss);
    if pp>0 then ss:=left(ss,pp-1);
    size:=Cval(ss);                  { size negotiation - Dateigrî·e }
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
  repeat                   { Slave-Schleife fÅr eingehende Befehle }
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
                  source:=left(s,p-1);   { Quelldatei auf anderem Rechner }
                  s:=trim(mid(s,p));
                  p:=blankpos(s);
                  if p>0 then begin
                    if SizeN then getFilesize;
                    s:=left(s,p-1);
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
                  if (left(s,2)='D.') or (left(s,2)='X.') then begin
                    fn:=XFerDir+U2DOSfile(s);
                    wrlog('+','receiving '+s+' as '+ustr(fn));
                    end
                  else begin
                    s:=Unix2DOSfile(s,FilePath, UseLFN);
                    if s='' then s:=Unix2DOSfile(source,FilePath, UseLFN);
                    if s='' then s:='unnamed';
                    fn:=FilePath+s;
                    wrlog('S','receiving '+s+' as '+ustr(fn));
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
                if SendCommand(c+'N') then;    { ungÅltiger Befehl }
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
      StopError(getres(107));     { 'UngÅltige Baudrate' }
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


function uucico(start:longint; var ende:boolean;
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
  if ParDebug then mcur:=curon
  else mcur:=curoff;
  ShowWindow;
  recs:=''; _ende:=false;
  sendtime:=0; rectime:=0; waittime:=0;

  if InitHandshake(waittime) then begin     { enthÑlt InitProtocol }
    if SendFiles(CommandFile,sendtime,rectime) then
      if RecFiles(rectime) then begin
        uucico:=uu_ok; _ende:=true; end
      else uucico:=uu_recerr
    else uucico:=uu_senderr;
    ExitProtocol;
    end
  else
    uucico:=uu_nologin;

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



{ --- Hauptprogramm ------------------------------------------------- }

var ende     : boolean;
    waittime : integer;
    sendtime,rectime : longint;
    result   : integer;


procedure WriteResultFile;
var t : text;
begin
  assign(t,uucicores);
  rewrite(t);
  writeln(t,'# XP uucico result file');
  writeln(t);
  writeln(t,'Result=',result);
  writeln(t,'StopDialing=',iifc(ende,'Y','N'));
  writeln(t,'Waittime=',waittime);
  writeln(t,'Sendtime=',sendtime);
  writeln(t,'Rectime=',rectime);
  close(t);
end;


{$F+}
procedure newexit;
begin
  if ioresult<>0 then;
  CloseLogfiles;
  gotoxy(omx,omy);
  exitproc:=oldexit;
end;
{$F-}


begin
  logo;
  if exist(uucicores) then era(uucicores);
  InitVar;
  ReadConfig;
  SetLanguage;
  TestConfig;
  oldexit:=exitproc;
  exitproc:=@newexit;
  multi3:=WriteOnline;
  result:=uucico(starttime,ende,waittime,sendtime,rectime);
  WriteResultFile;
  CloseResource; resopen:=false;
end.

{
  $Log$
  Revision 1.1.2.4  2001/01/01 12:12:10  mk
  - verbesserte LFN-UnterstÅtzung

  Revision 1.1.2.3  2000/12/31 11:35:54  mk
  - fileio.disksize statt lfn.disksize benutzen

  Revision 1.1.2.2  2000/12/30 10:56:45  mk
  - LFN Unterstuetzung

}
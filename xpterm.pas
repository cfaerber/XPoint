{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Terminal und Scripts }

{$I XPDEFINE.INC}

unit xpterm;

interface


uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  sysutils,dos,typeform,fileio,inout,keys,uart,datadef,database,maus2,video,
{$IFDEF CAPI }
  capi,
{$ENDIF }
      resource,xpglobal, xp0,xp1,xp1o2,xp1input;


function RunScript(test:boolean; scriptfile:string;
                   online,relogin:boolean; slog:textp):shortint;
procedure termscr;
procedure terminal(direct:boolean);

procedure TermDeactivateCom;    { fÅr FKey-Shell }
function  TermGetfilename(nr,nn:byte):string;


implementation  { -------------------------------------------------- }

uses  xp1o,xpkeys,xp2,xp2c,xp9bp,xp10, winxp;

const ansimode : boolean = true;
      ansimax  = 40;       { max. LÑnge von ANSI-Codes }
      log2     : textp   = nil;

      ShellReleased : boolean = false;

      coltab : array[0..7] of byte = (0,red,green,brown,blue,magenta,cyan,7);

      ANSI_curup     = #27'[A';
      ANSI_curdown   = #27'[B';
      ANSI_curleft   = #27'[D';
      ANSI_curright  = #27'[C';
      ANSI_home      = #27'[H';
      ANSI_end       = #27'[K';

var termlines    : byte;
    IgnCD,IgnCTS : boolean;
    comnr        : byte;
    ISDN         : boolean;
    recs,lrecs   : string;
    display      : boolean;
    log          : boolean;
    logfile      : text;
    la           : byte;
    open_log     : boolean;
    in7e1,out7e1 : boolean;
    cps          : cpsrec;
    orgfossil    : boolean;

    ansichar     : boolean;       { ANSI-Sequenz aktiv }
    ansiseq      : string;
    ansilen      : byte;          { length(ansiseq) - fÅr schnellen Zugriff }
    ansifg,ansibg: byte;          { Farbattribute }
    ansihigh     : byte;          { add fÅr helle Farben }
    ansirev      : boolean;       { reverse Darstellung }


procedure ShowTermStatus;
begin
  moff;
  attrtxt(col.colmenu[0]);
  write(forms(' CrossTerm',80));
  gotoxy(17,1);
  attrtxt(col.ColMenuHigh[0]);
  write('F1');
  attrtxt(col.ColMenu[0]);
  write(' '+getres2(2000,1)+'    ');   { 'Hilfe' }
  attrtxt(col.ColMenuHigh[0]);
  write('Alt-O');
  attrtxt(col.ColMenu[0]);
  write(' '+getres2(2000,2));  { 'Einstellungen' }
  attrtxt(7);
  m2t:=true;
  mon;
end;


procedure termscr;
begin
  attrtxt(7);
  window(1,1,80,25);
  moff;
  clrscr;
  mon;
  if TermStatus then begin
    termlines:=screenlines-1;
    ShowTermstatus;
    window(1,2,80,termlines+1);
    end
  else begin
    termlines:=screenlines;
    m2t:=false;
    window(1,1,80,termlines);
    windmax:=zpz-1+(termlines-1)*256;
    end;
  writeln;
  disp_DT;
  inout.cursor(curon);
  aufbau:=true;
end;


procedure savewin;
var mx,my : byte;
begin
  savecursor;
  mx:=wherex; my:=wherey;
  inout.cursor(curoff);
  window(1,1,80,screenlines);
  gotoxy(mx,my+iif(TermStatus,1,0));
  la:=lastattr;
end;

procedure restwin;
begin
  if TermStatus then
    window(1,2,80,termlines+1)
  else
    window(1,1,80,termlines);
  restcursor;
  windmax:=zpz-1+256*(screenlines-1);
  attrtxt(la);
end;


procedure openlog(fn:string);
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
begin
  savewin;
  if not multipos(':\',fn) then
    fn:=logpath+fn;
  if not validfilename(fn) then
    rfehler(2001)
  else begin
    rmessage(2001);
    assign(logfile,fn);
    if existf(logfile) then append(logfile)
    else rewrite(logfile);
    log:=true;
    attrtxt(col.colmenu[0]);
    fsplit(fn,dir,name,ext);
    mwrt(53,1,forms(UpperCase(name+ext),12));
    wkey(1,false);
    closebox;
    end;
  restwin;
end;

procedure closelog;
begin
  rmessage(2002);    { 'Schlie·e Logfile ...' }
  close(logfile);
  log:=false;
  attrtxt(col.colmenu[0]);
  mwrt(53,1,sp(12));
  wkey(1,false);
  closebox;
end;

procedure SwitchLogfile;
var fn      : string;
    useclip : boolean;
begin
  savewin;
  if log then
    if ReadJN(getres(2003),true) then  { Logfile schlie·en }
      CloseLog
    else
  else begin
    fn:=boxpar^.o_logfile;
    useclip:=false;
    if ReadFilename('Logfile',fn,true,useclip) then
      Openlog(fn);
    end;
  restwin;
end;


procedure GetComData;
begin
  with boxpar^,ComN[boxpar^.bport] do begin
    IgnCD:=IgCD; IgnCTS:=IgCTS;
    comnr:=bport;
    ISDN:=(comnr>4);
    in7e1:=uucp7e1; out7e1:=uucp7e1;
    end;
end;


procedure sendstr(s:string); forward;


procedure add_ansi(c:char);
const maxpar = 20;
      amx    : byte = 1;
      amy    : byte = 1;

var parcount : byte;
    ansipar  : array[1..maxpar] of integer;
    qmark    : boolean;
    p,mx,my  : byte;
    i        : integer;

  procedure set1;
  begin
    if ansipar[1]=0 then ansipar[1]:=1;
  end;

  procedure set2;
  begin
    if ansipar[2]=0 then ansipar[2]:=1;
  end;

  procedure savecur;
  begin
    mx:=wherex; my:=wherey;
  end;

  procedure restcur;
  begin
    gotoxy(mx,my);
  end;

  procedure setcol;
  var at : byte;
  begin
    if ansirev then
      at:=(ansifg and 7) shl 4 + ansibg shr 4
    else
      at:=ansifg + 16*ansibg + ansihigh;
    attrtxt(at);
  end;

begin
  if (c<' ') or (ansilen>=ansimax) then
    ansichar:=false
  else if c>' ' then
    if ((c>='A') and (c<='Z')) or ((c>='a') and (c<='z')) then begin
      SetLength(ansiseq, ansilen);
      delete(ansiseq,1,1);        { '[' lîchen }
      if ansiseq<>'' then begin
        qmark:=(ansiseq[1]='?');
        if qmark then delete(ansiseq,1,1);
        end;
      parcount:=0;
      fillchar(ansipar,sizeof(ansipar),0);      { Parameterliste ermitteln }
      while (ansiseq<>'') and (parcount<maxpar) do begin
        p:=1;
        while (p<=length(ansiseq)) and (ansiseq[p]<>';') do inc(p);
        inc(parcount);
        ansipar[parcount]:=minmax(ival(left(ansiseq,p-1)),0,255);
        delete(ansiseq,1,p);
        end;
      moff;
      case c of
        'n'     : case ansipar[1] of
                    6 : sendstr(#27'['+strs(wherey)+';'+strs(wherex)+'R');
                  end;
        'A'     : begin
                    set1;
                    gotoxy(wherex,max(1,wherey-ansipar[1]));
                  end;
        'B'     : begin
                    set1;
                    gotoxy(wherex,min(termlines,wherey+ansipar[1]));
                  end;
        'D'     : begin
                    set1;
                    gotoxy(max(1,wherex-ansipar[1]),wherey);
                  end;
        'C'     : begin
                    set1;
                    gotoxy(min(80,wherex+ansipar[1]),wherey);
                   end;

        'H','f' : begin
                    set1; set2;
                    gotoxy(minmax(ansipar[2],1,80),minmax(ansipar[1],1,termlines));
                  end;

        'J'     : case ansipar[1] of
                    0 : begin              { Lîschen bis Bildende }
                          savecur;
                          clreol;
                          inc(windmax,$100);
                          for i:=wherey+1 to termlines do
                            wrt(1,i,sp(80));
                          dec(windmax,$100);
                          restcur;
                        end;
                    1 : begin              { Lîschen bis Bildanfang }
                          savecur;
                          wrt(1,wherey,sp(wherex));
                          for i:=1 to wherey-1 do
                            wrt(1,i,sp(80));
                          restcur;
                        end;
                    2 : clrscr;
                  end;

        'K'     : case ansipar[1] of
                    0 : clreol;                   { Zeilenende lîschen }
                    1 : wrt(1,wherey,sp(wherex)+#8);  {!! Zeilenanfang lîschen }
                    2 : begin                     { Zeile lîschen }
                          savecur;
                          inc(windmax,$100);
                          wrt(1,wherey,sp(80));
                          dec(windmax,$100);
                          restcur;
                        end;
                  end;

        'L'     : for i:=1 to min(ansipar[1],termlines+1) do insline;
        'M'     : for i:=1 to min(ansipar[1],termlines+1) do delline;

        { @ / P : n Zeichen einfÅgen / lîschen }

        's'     : begin
                    Amx:=wherex; Amy:=wherey;
                  end;
        'u'     : gotoxy(Amx,Amy);

        'm'     : begin
                    for i:=1 to parcount do
                      case ansipar[i] of
                        0 : begin
                              ansifg:=7; ansibg:=0;
                              ansihigh:=0; ansirev:=false;
                            end;
                        1 : ansihigh:=ansihigh or 8;
                        2 : ansihigh:=0;
                        3 : ansihigh:=ansihigh or $80;
                        7 : ansirev:=true;
                        8 : begin ansifg:=ansibg; ansihigh:=0; end;
                       25 : ansihigh:=ansihigh and $7f;
                       27 : ansirev:=false;
                   30..37 : ansifg:=coltab[minmax(ansipar[i]-30,0,7)];
                   40..47 : ansibg:=coltab[minmax(ansipar[i]-40,0,7)];
                      end;
                    setcol;
                  end;
        end;  { of case }
      mon;
      ansichar:=false;
      end
    else begin
      inc(ansilen);
      ansiseq[ansilen]:=c;
      if (ansilen=1) and (c<>'[') then
        ansichar:=false;
      end;
end;


{$R-}

procedure testbyte;
var b : byte;

  procedure BiosWrite(c:char);
  begin
    directvideo:=false;
    write(c);
    directvideo:=true;
  end;

begin
{$IFDEF CAPI }
   if (ISDN and CAPI_getchar(char(b))) or (not ISDN and receive(comnr,b)) then
{$ELSE }
   if receive(comnr,b) then
{$ENDIF }
  if ansichar then add_ansi(char(b))
  else
    if ansimode and (b=27) then begin
      ansichar:=true;
      ansilen:=0;
      end
    else begin
      if in7e1 then b:=b and $7f;
      if display then begin
        moff;
        if b=9 then write(sp(8-(wherex-1) mod 8))
        else if b=12 then clrscr
        else if b<>0 then
          if termbios then BiosWrite(chr(b))
          else write(chr(b));
        mon;
        end;
      if (b=13) or (b=10) then begin
        if open_log then begin
          openlog(boxpar^.o_logfile);
          open_log:=false;
          end;
        if b=13 then begin
          if log then writeln(logfile,recs);
          if log2<>nil then writeln(log2^,recs);
          end;
        recs:=''; lrecs:='';
        end
      else
        if length(recs)<255 then begin
          SetLength(recs, Length(recs)+1); {inc(byte(recs[0]));}
          recs[length(recs)]:=chr(b);
          SetLength(lrecs, Length(lrecs)+1); {inc(byte(lrecs[0]));}
          lrecs[length(lrecs)]:=LoCase(chr(b));
          end;
      end;
end;

{$IFDEF Debug }
  {$R+}
{$ENDIF }


procedure tb;
begin
  testbyte;
  multi2;
end;

procedure sendstr(s:string);
var i : byte;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_Sendstr(s)
  else
{$ENDIF }
    for i:=1 to length(s) do begin
      testbyte;
      if out7e1 then SetParity(byte(s[i]),true);
      if IgnCTS then SendByte(comnr,byte(s[i]))
      else HSendByte(comnr,byte(s[i]));
      end;
end;

procedure mdelay(msec:word; show:boolean);   { genaues Delay }
var t      : longint;
    i,n    : word;
begin
  n:=system.round(msec/54.925401155);
  for i:=1 to n do begin
    t:=ticker;
    repeat
      if show then tb;
    until t<>ticker;
    end;
end;

function Carrier:boolean;
begin
{$IFDEF CAPI }
  if ISDN then
    Carrier:=CAPI_Carrier
  else
{$ENDIF }
  Carrier:=uart.carrier(comnr);
end;

procedure flushin;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_flushinput
  else
{$ENDIF }
    flushinput(comnr);
end;


procedure aufhaengen;
var n,i : byte;
begin
  moff;
  writeln;
  attrtxt(15); write(getres(2004)); attrtxt(7);   { trenne Verbindung }
  writeln;
  mon;
{$IFDEF CAPI }
  if ISDN then
    CAPI_hangup
  else
{$ENDIF }
  begin
    n:=5;
    zaehler[2]:=100;
    while carrier and not IgnCD and (n>0) do begin
      tb;tb;tb;
      dec(n);
      DropDtr(comnr);
      i:=1;
      while (i<=4) and carrier do begin
        mdelay(500,true); tb;
        inc(i);
        end;
      if carrier then begin
        SendStr('+++');
        mdelay(500,true);
        multi2;
        mdelay(500,true);
        end;
      flushin;
      end;
    setdtr(comnr);
    sendstr(#13); mdelay(300,true);
    flushin;
    if Comn[comnr].MExit^<>'' then begin
      SendStr(Comn[comnr].MExit^+#13);
      mdelay(500,true);
      end;
    end;
end;


procedure FossilTest;
begin
  if comn[comnr].fossil and not FOSSILdetect then begin
    trfehler(732,30);   { 'Kein FOSSIL-Treiber installiert - verwende eingebauten Treiber' }
    comn[comnr].fossil:=false;
    end;
end;


procedure Activate;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_resume
  else
{$ENDIF }
    ActivateCom(comnr,2000,COMn[comnr].u16550);
end;


procedure sendcomm(s:string);
var p : byte;
begin
  flushin;
  if not HayesComm or ISDN then exit;
  repeat
    p:=cpos('~',s);
    if p>0 then begin
      sendstr(left(s,p-1));
      delete(s,1,p);
      mdelay(200,true);
      while received(comnr) do tb;
      mdelay(850,true);
      end;
  until p=0;
  sendstr(s+#13);
  zaehler[3]:=COMn[comnr].warten;
  repeat
    tb;
  until (zaehler[3]=0) or (recs='OK') or (recs='0') or (recs='ERROR');
  repeat
    tb;
  until (zaehler[3]=0) or (recs='');   { auf CR warten }
  mdelay(500,true);
end;


procedure sendmstr(s:string);
var p : byte;
begin
  if not ISDN then
    while length(trim(s))>1 do begin
      p:=pos('\\',s);
      if p=0 then p:=length(s)+1;
      sendcomm(trim(left(s,p-1)));
      s:=trim(mid(s,p+2));
      end;
end;


function initcom:boolean;   { !! ISDN! }
var d  : DB;
    fn : string;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(DefaultBox));
  fn:= dbReadStr(d,'dateiname');
  dbClose(d);
  ReadBox(0,fn,boxpar);
  InitCom := true; { MK 12/99 wir gehen davon aus, das die Initialisierung ok lÑuft }
  if TermCOM<>0 then boxpar^.bport:=TermCOM;
  if TermBaud<>0 then boxpar^.baud:=TermBaud;
  with boxpar^,ComN[boxpar^.bport] do begin
    GetComData;    { IgnCD etc. }

{$IFDEF CAPI }
  (* 04.02.2000 MH: HinzugefÅgt! *)
   if ISDN and not (CAPI_Installed and (CAPI_Register=0)) then begin
     rfehler(740);   { 'ISDN-CAPI-Treiber fehlt oder ist falsch konfiguriert' }
      initcom:=false;
     exit;
    end;
{$ENDIF }

    orgfossil:=fossil;
    FossilTest;

  if not ISDN then begin (* 04.02.2000 MH: Um Kollision zu vermeiden *)

    SetComParams(bport,fossil,Cport,Cirq);
    if OStype<>OS_2 then
      SaveComState(bport,cps);
    SetTriggerLevel(tlevel);
    if SetUart(bport,baud,PNone,8,1,not IgnCTS) then;   { fest auf 8n1 ... }
    gotoxy(1,4);   { wegen BNU }

  end;

    Activate;

     IgnCD:=IgCD; IgnCTS:=IgCTS;
      mdelay(300,false);
     flushin;

{    mdelay(100,false);}
    if not IgnCTS and not GetCTS(comnr) then mdelay(400,false);
       { ^^ falls CTS von DTR abhÑngt.. }
    if not IgnCTS and not GetCTS(comnr) then begin

{$IFDEF CAPI }
    if ISDN then
      CAPI_release { 04.02.2000 MH: bei ISDN-CAPI abmelden }
    else
{$ENDIF }

      releasecom(comnr);
      if OStype<>OS_2 then
        RestComState(comnr,cps);
      rfehler(2002);     { 'Modem nicht bereit.' }
      initcom:=false;
      end
    else begin
      termscr;
      moff;
      writeln(getres(2005));
      writeln;
      mon;
      if not carrier then begin
        mdelay(100,true);
      if not ISDN then
        sendbyte(comnr,13);
        mdelay(200,true);
        if TermInit<>'' then SendMstr(TermInit);
        end;
      end;
    end;
end;


procedure TermDeactivateCom;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_suspend
  else
{$ENDIF }
  begin
    ReleaseCom(comnr);
    ShellReleased:=true;
  end;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

{ Parameter nr, nn werden zwar nicht gebraucht, aber durch
  Prozedurvariable sind sie nîtig }
function TermGetfilename(nr,nn:byte):string;
begin
  rfehler(2003);    { '$FILE-Makro ist hier nicht mîglich.' }
  TermGetfilename:='';
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }


procedure UpDown(auto,download:boolean);
var s       : string;
    useclip : boolean;
label ende;
begin
  if exist('zm.exe') then begin
    SaveWin;
    SaveCursor;
    if not download then begin
      s:='*.*';
      useclip:=false;
      if not ReadFilename(getres(2007),s,true,useclip) then begin  { 'Upload' }
        RestCursor;
        RestWin;
        if auto then begin
          flushin;
          sendstr(dup(7,^X));
          mdelay(500,true);
          end;
        goto ende;
        end;
      end;
    RestCursor;
    TermDeactivateCom;
    with comn[comnr] do
      shell('zm.exe -c'+iifs(fossil,strs(comnr)+' -f',
                                    hex(Cport,3)+','+strs(CIrq))+
                    iifs(IgCD,' -d','')+
                    iifs(IgCTS,' -h','')+
                    iifs(UseRTS,' -rts','')+
                    iifs(u16550,'',' -n')+
                    iifs(u16550,' -tl'+strs(tlevel),'')+
                    ' -q '+
                    iifs(download,'rz '+FilePath,'sz '+s),
                    300,0);
    if ShellReleased then begin
      Activate;
      ShellReleased:=false;
      end;
    RestWin;
    end;
ende:
  recs:=''; lrecs:='';
  flushin;
  clearkeybuf;
end;


procedure SwitchStatus;
var p     : pointer;
    mx,my : byte;
begin
  mx:=wherex; my:=wherey;
  getmem(p,(screenlines-1)*zpz*2);
  moff;
{$IFNDEF ver32}
  Move(mem[base:iif(TermStatus,0,2*zpz)],p^,(screenlines-1)*zpz*2);
  termscr;
  Move(p^,mem[base:iif(TermStatus,2*zpz,0)],(screenlines-1)*zpz*2);
{$ENDIF}
  mon;
  freemem(p,(screenlines-1)*zpz*2);
  termlines:=iif(TermStatus,screenlines-1,screenlines);
  gotoxy(mx,min(my,termlines));
end;


procedure Options;
var stat : boolean;
    init : string;
    mcom : byte;        { ** }
    mbaud: longint;     { ** }
begin
  TermCOM:=boxpar^.bport;  mcom:=TermCom;   { ** }
  TermBaud:=boxpar^.baud;  mbaud:=TermBaud; { ** }
  stat:=TermStatus;
  init:=TermInit;
  SaveWin;
  TerminalOptions;
  RestWin;
  if TermStatus<>stat then
    SwitchStatus;
  if (TermCOM<>boxpar^.bport) or (TermBaud<>boxpar^.baud) or
     ((TermInit<>init) and not carrier) then begin
    if not carrier then
      if not ISDN then DropDtr(boxpar^.bport);
    ReleaseCom(boxpar^.bport);    { !! ISDN }
    if not initcom then begin   { Fehler -> zurÅck auf alte Schnittstelle }
      comn[boxpar^.bport].fossil:=orgfossil;
      TermCOM:=mcom;      { ** }
      TermBaud:=mbaud;    { ** }
      SaveConfig;
      if initcom then;    { ** }
      end;
    end;
end;


{ direct = /XPoint/Terminal }

procedure terminal(direct:boolean);
var
  t    : taste;
  ende : boolean;
  connected : boolean;
  p    : pointer;
  rest : boolean;
begin
  connected:=not direct;
  open_log:=false; log:=false;
  in7e1:=false; out7e1:=false;
  display:=true;
  ansimode:=true;
  if direct and not initcom then begin
    comn[boxpar^.bport].fossil:=orgfossil;
    m2t:=true;
    exit;
    end;
  with boxpar^,ComN[boxpar^.bport] do begin
    IgnCD:=IgCD; IgnCTS:=IgCTS;
    comnr:=bport;
    ISDN:=(comnr>4);
    open_log:=not direct and (o_logfile<>'');
    recs:=''; lrecs:='';
    inout.cursor(curon);
    display:=true;
    pushhp(66);
    ende:=false;
    p:=@fnproc[0,9]; fnproc[0,9]:=DummyFN;

    ansichar:=false;
    ansifg:=7; ansibg:=0;
    ansihigh:=0;
    ansirev:=false;

    while not ende do
      if connected and not IgnCD and not carrier then begin
        moff;
        writeln;
        attrtxt(15); write(getres(2006)); attrtxt(7);   { 'Verbindung getrennt - Ende mit <Alt X>' }
        writeln;
        mon;
        connected:=false;
        end
      else begin
        multi2;

        testbyte;
        if AutoDownload and (pos('*'^X'B00',recs)>0) then
          UpDown(true,true)
        else if AutoUpload and (pos('*'^X'B01',recs)>0) then
          UpDown(true,false);
        if keypressed then begin
          get(t,curon); inout.cursor(curon);
          if t=keyf6 then begin
            savewin;
            Makroliste(7);
            restwin;
            end;
          Xmakro(t,64);
          if t=mausleft then t:=copychr(_mausx,_mausy);
          if t=keyup then sendstr(ANSI_curup) else
          if t=keydown then sendstr(ANSI_curdown) else
          if t=keyleft then sendstr(ANSI_curleft) else
          if t=keyrght then sendstr(ANSI_curright) else
          if t=keydel then sendstr(#127) else
          if t=keyhome then sendstr(ANSI_home) else
          if t=keyend then sendstr(ANSI_end) else
          if t=keyaltx then ende:=true else
          if t=keyalth then aufhaengen else
          if t=keyaltl then SwitchLogfile else
          if t=keychom then begin moff; clrscr; mon; end else
          if t=keypgup then UpDown(false,false) else
          if t=keypgdn then UpDown(false,true) else
          if t=keyaltd then begin
                              TermStatus:=not TermStatus;
                              SwitchStatus;
                            end else
          if (t=keyalto) or (t=mausright) then begin
                              Options;
                              connected:=carrier;
                            end
          else
            if t=keyf9 then begin
              { DropRTS(comnr); - Vorsicht, ZyXEL-Problem }
              if not comn[bport].fossil then ReleaseCom(bport);
              savewin;
              dosshell;
              restwin;
              if not comn[bport].fossil then Activate;
              if not ISDN then SetRTS(comnr);
              end
          else begin
            FuncExternal:=true;     { nur externe F-Tasten zugelassen }
            PreExtProc:=TermDeactivateCom;
            getfilename:=TermGetfilename;
            if test_fkeys(t) then begin
              if shellreleased then Activate;
              shellreleased:=false;
              end
            else
              if t[1]>#0 then
              begin
                if out7e1 then SetParity(byte(t[1]),true);
{$IFDEF CAPI }
                if ISDN then
                  CAPI_Sendstr(t[1])
                else
{$ENDIF }
                  if IgnCTS then SendByte(comnr,byte(t[1]))
                  else HSendByte(comnr,byte(t[1]));
              end;
            FuncExternal:=false;
            PreExtProc:=nil;
            end;
          end;

        if not connected and carrier then
          connected:=true;
        end;

    @fnproc[0,9]:=p;

    if log then begin
      savewin;
      closelog;
      restwin;
      end;
    pophp;
    m2t:=true;
    showscreen(true);
    if direct then begin
      if not carrier then begin
        if not ISDN then DropDtr(bport);
        { DropRts(bport); - Vorsicht, ZyXEL-Problem }
        end;
      rest:=not carrier;
      ReleaseCom(bport);    { !! ISDN }
      if (OStype<>OS_2) and rest then
        RestComState(bport,cps);
      comn[bport].fossil:=orgfossil;
      end;
    end;
end;


{ --- Scripts ------------------------------------------------------- }

function RunScript(test:boolean; scriptfile:string;
                   online,relogin:boolean; slog:textp):shortint;

const MaxLines  = 500;
      Maxlabels = 100;
{     MaxlabelLen = 20;}
      Shortstr  = 8;

      pEndOK    = 0;      { RÅckgabewerte von RunScript      }
      pEndError = 1;      { = num. Parameter des END-Befehls }
      pEndFail  = 2;
      pEndSyntax= 3;      { Syntax Error }

      pDispOn   = 1;      { num. Parameter des DISPLAY-Befehls }
      pDispOff  = 2;
      pAnsiOn   = 1;      { num. Parameter des ANSI-Befehls }
      pAnsiOff  = 2;

      cmdWaitfor = 1;    cmdWrite   = 6;     cmdFlush = 11;    cmdANSI = 16;
      cmdSend    = 2;    cmdWriteln = 7;     cmdCls   = 12;
      cmdGoto    = 3;    cmdDisplay = 8;     cmdCall  = 13;
      cmdEnd     = 4;    cmdTimer   = 9;     cmdReturn= 14;
      cmdDelay   = 5;    cmdRead    = 10;    cmdBreak = 15;

      varPoint     = 1;   { Pointname }
      varUser      = 2;   { Username }
      varPassword  = 3;   { Netcall-Pa·wort }
      varOpassword = 4;   { Login-Pa·wort }
      varLogin     = 5;   { UUCP/QM-Login }
      varProtocol  = 6;   { MausTausch-Protokollkennung }
      varSerialNo  = 7;   { Maggi/Z-Seriennummer }

type  stringp  = ^string;
      ScrRec   = record
                   txtline   : integer;           { Zeilennr. in .SCR   }
                   onflag    : byte;              { 0=nix, 1=ON, 2=Timeout }
                                                  { 3=Online, 4=Relogin }
                   onstrp    : stringp;           { .. String-Parameter }
                   onstr     : string;
                   command   : shortint;          { Befehlsnummer       }
                   numpar    : longint;           { .. num. Parameter   }
                   cr,lf     : boolean;
                   strparp   : stringp;           { .. String-Parameter }
                   strpar    : string;
                 end;
      ScrArr   = array[0..MaxLines] of scrrec;
      ScrArrP  = ^ScrArr;

var   script   : ScrArrP;
      lines    : integer;
      logins   : integer;       { verbleibende Login-Anzahl }
      RunScriptRes: ShortInt;   { MK 01/2000 }


function LoadScript:boolean;
type labela = array[1..maxlabels] of record
                                       name : string;
                                       line : integer;
                                     end;
     lap    = ^labela;
var t      : text;
    s,s0   : string;
    errlog : text;
    labels : integer;
    _label : lap;
    line   : integer;    { lfd. Zeile }
    errors : integer;
    stringflag : boolean;
    ok     : boolean;

  procedure serror(nr:word; txt:string);
  begin
    writeln(errlog,getreps2(2010,0,strs(line))+getreps2(2010,nr,txt));
    ok:=false;
    inc(errors);
    fillchar(script^[lines+1],sizeof(ScrRec),0);
  end;

  procedure GetWord;     { nÑchstes Wort lesen }
  var p : byte;
  begin
    stringflag:=false;
    if s='' then
      serror(4,'')          { 'unerwartetes Zeilenende' }
    else if s[1]='"' then begin      { String-Konstante }
      p:=2;
      while (p<=length(s)) and (s[p]<>'"') do inc(p);
      if p>length(s) then
        serror(1,'')     { '"' fehlt }
      else begin
        s0:=copy(s,2,p-2);
        s:=trim(mid(s,p+1));
        stringflag:=true;
        p:=1;
        while p<length(s0) do begin
          inc(p);
          if s0[p-1]='^' then begin
            case s0[p] of
              '0'      : s0[p]:='^';
              ''''     : s0[p]:='"';
              'A'..'_' : s0[p]:=chr(ord(s0[p])-64);
              'a'..'z' : s0[p]:=chr(ord(s0[p])-96);
              else       delete(s0,p,1);
            end;
            delete(s0,p-1,1);
            end;
          end;
        end;
      end
    else begin
      p:=blankpos(s);
      if p=0 then p:=length(s)+1;
      s0:=left(s,p-1);
      s:=trim(mid(s,p+1));
      end;
  end;

  function comment(var s:string):boolean;
  begin
    comment:=(s='') or (s[1]='#') or (s[1]=';');
  end;

  procedure GetString;
  var s : string;
  begin
    GetWord;
    s:='*';
    if not stringflag then begin
      if comment(s0) then s:='' else
      if UpperCase(s0)='CR' then s:=#13 else
      if UpperCase(s0)='LF' then s:=#10 else
      if UpperCase(s0)='CRLF' then s:=#13#10;
      if s<>'*' then begin
        s0:=s;
        stringflag:=true;
        end;
      end;
  end;

  function SeekLabel:integer;
  var p : byte;
  begin
    p:=1;
    while (p<=labels) and (_label^[p].name<>s0) do inc(p);
    if p<=labels then SeekLabel:=_label^[p].line
    else SeekLabel:=0;
  end;

  procedure AddLabel;
  begin
    dellast(s0);
    LoString(s0);
    if SeekLabel<>0 then
      serror(2,s0)                 { 'Sprungmarke "%s" existiert bereits' }
    else if labels=MaxLabels then
      serror(3,strs(MaxLabels))    { 'Max. %s Sprungmarken mîglich!' }
    else begin
      inc(labels);
      _label^[labels].name:=s0;
      _label^[labels].line:=lines+1;
      end;
  end;

  procedure AddCommand; forward;

  procedure AddOnCommand;
  begin
    GetString;
    if ok then with script^[lines+1] do begin
      if stringflag then LoString(s0)
      else UpString(s0);
      if onflag>0 then begin
        serror(6,'');     { 'verschachtelte ON-Befehle sind nicht erlaubt' }
        s:='';
        end
      else begin
        if stringflag then begin
          onflag:=1;                          { ON "..." <Command> }
          if length(s0)<=ShortStr then
            onstr:=s0
          else begin
            getmem(onstrp,length(s0)+1);
            onstrp^:=s0;
            end;
          end
        else if s0='TIMEOUT' then             { ON TIMEOUT <Command> }
          onflag:=2
        else if s0='ONLINE' then              { ON ONLINE <Command>  }
          onflag:=3
        else if s0='NETCALL' then             { ON NETCALL <Command> }
          onflag:=4
        else if s0='RELOGIN' then             { ON RELOGIN <Command> }
          onflag:=5
        else begin
          serror(5,'');     { 'ungÅltige ON-Funktion ' }
          s:='';
          end;
        if onflag<>0 then begin
          GetWord;
          AddCommand;
          end;
        end;
      end;
  end;

  procedure AddStrComm(cmd:integer);
  var ss : string;
  begin
    GetString;
    if ok then
      if not stringflag then
        if s0[1]<>'$' then
          serror(7,'')      { 'Text-Parameter erwartet' }
        else begin
          inc(lines);
          with script^[lines] do begin
            command:=cmd;
            ss:=UpperCase(s0);
            if ss='$POINT'     then numpar:=varPoint else
            if ss='$USER'      then numpar:=varUser else
            if ss='$PASSWORD'  then numpar:=varPassword else
            if ss='$OPASSWORD' then numpar:=varOPassword else
            if ss='$LOGIN'     then numpar:=varLogin else
            if ss='$PROTOCOL'  then numpar:=varProtocol else
            if ss='$SERIALNO'  then numpar:=varSerialNo else
            begin
              serror(14,ss);     { 'ungÅltiges Textmakro: %s' }
              dec(lines);
              end;
            end;
          end
      else begin
        inc(lines);
        with script^[lines] do begin
          command:=cmd;
          if length(s0)<=ShortStr then
            strpar:=s0
          else begin
            getmem(strparp,length(s0)+1);
            strparp^:=s0;
            end;
          end;
        end;
    if ok and not comment(s) then
      with script^[lines] do begin
        GetWord;
        UpString(s0);
        if s0='CR' then cr:=true else
        if s0='LF' then lf:=true else
        if s0='CRLF' then begin cr:=true; lf:=true; end else
        serror(16,s0);    { 'ungÅltiger Parameter: %s' }
        end;
  end;

  procedure AddComm(cmd:integer);
  begin
    inc(lines);
    script^[lines].command:=cmd;
  end;

  procedure AddDisplayCommand;
  begin
    GetWord;
    UpString(s0);
    if (s0<>'ON') and (s0<>'OFF') then
      serror(9,'')      { 'ON oder OFF erwartet' }
    else begin
      inc(lines);
      script^[lines].command:=cmdDisplay;
      script^[lines].numpar:=iif(s0='ON',pDispOn,pDispOff);
      end;
  end;

  procedure AddAnsiCommand;
  begin
    GetWord;
    UpString(s0);
    if (s0<>'ON') and (s0<>'OFF') then
      serror(9,'')      { 'ON oder OFF erwartet' }
    else begin
      inc(lines);
      script^[lines].command:=cmdANSI;
      script^[lines].numpar:=iif(s0='ON',pAnsiOn,pAnsiOff);
      end;
  end;

  procedure AddEndCommand;
  begin
    if (s='') or comment(s) then s0:=''
    else GetWord;
    UpString(s0);
    if (s0<>'') and (s0<>'ERROR') and (s0<>'FAIL') then
      serror(10,'')      { 'ERROR oder FAIL erwartet' }
    else begin
      inc(lines);
      script^[lines].command:=cmdEnd;
      script^[lines].numpar:=iif(s0='',pEndOk,iif(s0='ERROR',pEndError,pEndFail));
      end;
  end;

  procedure AddDelayCommand;
  begin
    GetWord;
    if rval(s0)=0 then
      serror(11,'')      { 'ungÅltiger Delay-Parameter (Zahl erwartet)' }
    else begin
      inc(lines);
      script^[lines].command:=cmdDelay;
      script^[lines].numpar:=system.round(rval(s0)*1000);
      if s<>'' then begin
        GetWord;
        UpString(s0);
        if s0='SHOW' then
          script^[lines].strpar:=s0
        else if not comment(s0) then
          serror(8,'');
        end;
      end;
  end;

  procedure AddIntComm(cmd:integer; isint:boolean);
  begin
    GetWord;
    if stringflag then
      serror(12,'')      { 'numerischer Parameter erwartet' }
    else begin
      inc(lines);
      with script^[lines] do begin
        command:=cmd;
        numpar:=ival(s0);
        if isint then
          numpar:=minmax(numpar,-maxint,maxint);
        end;
      end;
  end;

  procedure AddJump(cmd:integer);
  begin
    GetWord;
    LoString(s0);
    inc(lines);
    with script^[lines] do begin
      command:=cmd;
      numpar:=SeekLabel;
      if numpar=0 then begin              { Label (noch) nicht vorhanden }
        getmem(strparp,length(s0)+1);
        strparp^:=s0;
        end;
      end;
  end;

  procedure AddCommand;
  begin
    UpString(s0);
    if s0='ON'      then AddOnCommand            else
    if s0='WAITFOR' then AddStrComm(cmdWaitfor)  else
    if s0='SEND'    then AddStrComm(cmdSend)     else
    if s0='WRITE'   then AddStrComm(cmdWrite)    else
    if s0='WRITELN' then if s<>'' then AddStrComm(cmdWriteln) else
                                       AddComm(cmdWriteln) else
    if s0='DISPLAY' then AddDisplayCommand       else
    if s0='END'     then AddEndCommand           else
    if s0='DELAY'   then AddDelayCommand         else
    if s0='GOTO'    then AddJump(cmdGoto)        else
    if s0='TIMER'   then AddIntComm(cmdTimer,true) else
    if s0='READ'    then AddComm(cmdRead)     else
    if s0='FLUSH'   then AddComm(cmdFlush)    else
    if s0='CLS'     then AddComm(cmdCls)      else
    if s0='CALL'    then AddJump(cmdCall)     else
    if s0='RETURN'  then AddComm(cmdReturn)   else
    if s0='BREAK'   then AddComm(cmdBreak)    else
    if s0='ANSI'    then AddAnsiCommand       else
    serror(15,s0);    { 'ungÅltiger Befehl: %s' }
    if ok and not comment(s) then
      serror(8,'');    { 'ÅberflÅssige Daten am Zeilenende' }
  end;

  procedure TestLabels;
  var i : integer;
  begin
    for i:=1 to lines do with Script^[i] do
      if ((command=cmdGoto) or (command=cmdCall)) and (numpar=0) then begin
        s0:=strparp^;
        numpar:=SeekLabel;
        if numpar=0 then
          serror(13,s0);      { 'Sprungmarke fehlt: %s' }
        freemem(strparp,length(s0)+1);
        strparp:=nil;
        end;
  end;

begin
  new(_label);
  new(script);
  fillchar(script^,sizeof(script^),0);
  labels:=0;
  assign(t,scriptfile);
  reset(t);
  assign(errlog,LogPath+ScerrLog);
  rewrite(errlog);
  lines:=0; line:=0; errors:=0;
  while not eof(t) and (lines<MaxLines) do begin
    ok:=true;
    inc(line);
    script^[lines+1].txtline:=line;
    readln(t,s);
    s:=trim(s);
    if not comment(s) then begin
      GetWord;
      if ok then begin
        if s0[length(s0)]=':' then begin
          AddLabel;
          GetWord;
          end;
        AddCommand;
        end;
      end;
    end;
  TestLabels;
  freeres;
  close(errlog);
  close(t);
  dispose(_label);
  LoadScript:=(errors=0);
end;


function ExecuteScript:shortint;
const maxstack = 50;
var ip   : integer;
    ende : boolean;
    par  : string;
    trace: text;
    tn   : byte;
    stack: array[1..maxstack] of integer;
    sp   : integer;
    ExecuteScriptRes: shortint;

  procedure RunError(nr:word);
  begin
    writeln;
    writeln(getres2(2011,3)+' '+getres2(2011,nr)+#7);
    logerror(getres2(2011,3)+' '+getres2(2011,nr));
    ende:=true;
   end;

  function timeout:boolean;
  begin
    timeout:=not (IgnCD or carrier) or (zaehler[2]=0);
  end;

  function GetPar:string;
  var crlf : string;
  begin
    with script^[ip],boxpar^ do begin
      if lf then
        if cr then crlf:=#13#10
        else crlf:=#10
      else
        if cr then crlf:=#13
        else crlf:='';
      case numpar of
        0            : if strparp=nil then getpar:=strpar+crlf
                       else getpar:=strparp^+crlf;
        varPoint     : GetPar:=pointname+crlf;
        varUser      : GetPar:=username+crlf;
        varPassword  : GetPar:=passwort+crlf;
        varOPassword : begin
                         GetPar:=o_passwort+crlf;
                         dec(logins);
                         if logins=0 then begin
                           RunScriptRes:=pEndError;
                           ende:=true;
                           end;
                       end;
        varLogin     : GetPar:=LoginName+crlf;
        varProtocol  : GetPar:=ProtoTyp+crlf;
        varSerialNo  : GetPar:=ZerbID+crlf;
      end;
    end;
  end;

  procedure TestKey;
  var c : char;
  begin
    if keypressed then begin
      c:=readkey;
      if c=#27 then begin
        ende:=true;
        ExecuteScriptRes:=pEndError;
        end
      else if c>#0 then
        sendstr(c);
      end;
  end;

  procedure interprete;
  var doit : boolean;
  begin
    with script^[ip] do begin
      case onflag of
        0 : doit:=true;
        1 : begin
              if onstrp=nil then par:=onstr
              else par:=onstrp^;
              doit:=(right(lrecs,length(par))=par);
              if doit then begin
                if log2<>nil then write(log2^,recs);
                recs:=''; lrecs:='';
                end;
            end;
        2 : begin
              multi2;
              doit:=(zaehler[3]=0);
            end;
        3 : doit:=online;
        4 : doit:=not online;
        5 : doit:=relogin;
      end;
      if doit then
        case command of
          cmdWaitfor  : begin
                          par:=LowerCase(getpar);
                          repeat
                            tb;
                            testkey;
                          until timeout or (right(lrecs,length(par))=par) or ende;
                          if log2<>nil then write(log2^,recs);
                          recs:=''; lrecs:='';
                        end;
          cmdSend     : SendStr(GetPar);
          cmdGoto     : ip:=numpar-1;
          cmdEnd      : begin
                          ende:=true;
                          ExecuteScriptRes:=numpar;
                        end;
          cmdDelay    : mdelay(numpar,strpar='SHOW');
          cmdWrite    : begin moff;
                          write(GetPar); mon;
                        end;
          cmdWriteln  : begin moff; writeln(Getpar); mon; end;
          cmdDisplay  : Display:=(numpar=pDispOn);
          cmdTimer    : zaehler[3]:=numpar;
          cmdRead     : tb;
          cmdFlush    : flushin;
          cmdCls      : clrscr;
          cmdCall     : if sp=maxstack then begin
                          runerror(5);      { 'StapelÅberlauf' }
                          sp:=0;
                          end
                        else begin
                          inc(sp);
                          stack[sp]:=ip;
                          ip:=numpar-1;
                        end;
          cmdReturn   : if sp=0 then       { 'RETURN ohne CALL' }
                          runerror(6)
                        else begin
                          ip:=stack[sp];
                          dec(sp);
                        end;
          cmdBreak    : SendBreak(comnr);
          cmdANSI     : begin
                          ansimode:=(numpar=pAnsiOn);
                          if not ansimode then ansichar:=false;
                        end;
        end;
      inc(ip);
      end;
  end;

begin    { of ExecuteScript }
  GetComData;
  ip:=1;
  recs:=''; lrecs:='';
  zaehler[2]:=boxpar^.LoginWait;
  zaehler[3]:=0;
  logins:=boxpar^.retrylogin+1;
  ende:=false;
  display:=true;
  ansimode:=true;
  sp:=0;
  if ParTrace then begin                       { Trace-Log îffnen }
    assign(trace,LogPath+'TRACE.LOG');
    if existf(trace) then append(trace)
    else rewrite(trace);
    writeln(trace,getres2(2011,1),' ',scriptfile,' / ',date,' / ',time);
    writeln(trace);
    tn:=0;
    end;
  repeat
    if ParTrace then begin                     { Trace-Zeile schreiben }
      write(trace,script^[ip].txtline,' ');
      inc(tn);
      if tn>17 then begin
        tn:=0; writeln(trace);
        end;
      end;
    interprete;
    Testkey;

    if ip>lines then begin
      ende:=true;
      ExecuteScriptRes:=pEndOk;
      end;
  until ende or timeout;
  if sp>0 then
    runerror(4);           { 'CALL ohne RETURN' }
  if ParTrace then begin
    if tn>0 then writeln(trace);
    writeln(trace);
    writeln(trace,getres2(2011,2),' ',time);
    writeln(trace);
    writeln(trace);
    close(trace);
    end;
  freeres;
  if timeout then
    ExecuteScriptRes:=pEndError;
  ExecuteScript := ExecuteScriptRes;
end;


procedure ReleaseScript;
var i : integer;
begin
  for i:=lines downto 1 do with script^[i] do begin
    if onstrp<>nil then
      freemem(onstrp,length(onstrp^)+1);
    if strparp<>nil then
      freemem(strparp,length(strparp^)+1);
    end;
  dispose(script);
end;


begin     { of RunScript }
  log2:=slog;
  if not LoadScript then
    RunScriptRes:=pEndSyntax
  else
    if not test and (lines>0) then
      RunScriptRes:=ExecuteScript
    else
      RunScriptRes:=pEndOK;
  RunScript := RunScriptRes;
  ReleaseScript;
  log2:=nil;
end;

end.
{
  $Log$
  Revision 1.17  2000/07/12 13:15:02  hd
  - Ansistring

  Revision 1.16  2000/07/11 21:39:23  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.15  2000/07/05 12:47:29  hd
  - AnsiString

  Revision 1.14  2000/07/05 10:59:53  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.13  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.12  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.11  2000/07/02 14:24:55  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.10  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.9  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/23 07:58:54  mk
  - OS/2-Portierung

  Revision 1.7  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}

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
{ 2000 OpenXP Team }

{$I XPDEFINE.INC}

unit xpterm;

interface

uses
  {$IFDEF NCRT }xpcurses,{$ELSE }crt,{$ENDIF }
  sysutils,dos,typeform,fileio,inout,keys,datadef,database,maus2,
  resource,xpglobal,xp0,xp1,xp1o2,xp1input,ObjCOM,Modem,Debug;

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
    recs,lrecs   : string;
    display      : boolean;
    log          : boolean;
    logfile      : text;
    la           : byte;
    open_log     : boolean;
    in7e1,out7e1 : boolean;
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
//  window(1,1,screenwidth,screenlines);
  moff;
  clrscr;
  mon;
  if TermStatus then begin
    termlines:=screenlines-1;
    ShowTermstatus;
//    window(1,2,screenwidth,termlines+1);
    end
  else begin
    termlines:=screenlines;
    m2t:=false;
//    window(1,1,screenwidth,termlines);
    windmax:=ScreenWidth-1+(termlines-1)*256;
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
//  window(1,1,screenwidth,screenlines);
  gotoxy(mx,my+iif(TermStatus,1,0));
  la:=lastattr;
end;

procedure restwin;
begin
  if TermStatus then
//    window(1,2,screenwidth,termlines+1)
  else
//    window(1,1,screenwidth,termlines);
  restcursor;
  windmax:=ScreenWidth-1+256*(screenlines-1);
  attrtxt(la);
end;


procedure openlog(fn:string);
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
    mwrt(53,1,forms(UpperCase(ExtractFileName(fn)),12));
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
    in7e1:=uucp7e1; out7e1:=uucp7e1;
    end;
end;

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
  begin if ansipar[1]=0 then ansipar[1]:=1 end;

  procedure set2;
  begin if ansipar[2]=0 then ansipar[2]:=1 end;

  procedure savecur;
  begin mx:=wherex; my:=wherey end;

  procedure restcur;
  begin gotoxy(mx,my)end;

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
        ansipar[parcount]:=minmax(ival(LeftStr(ansiseq,p-1)),0,255);
        delete(ansiseq,1,p);
        end;
      moff;
      case c of
        'n'     : case ansipar[1] of
                    6 : CommObj^.SendString(#27'['+strs(wherey)+';'+strs(wherex)+'R',False);
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
  if CommObj^.CharAvail then begin
    b:=Ord(CommObj^.GetChar);
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
{$ifdef Unix}
          { Muss noch ueberarbeitet werden }
          else if (b<>0) and (b<>13) then
            write(chr(b));
{$else}
          else if b<>0 then
            if termbios then BiosWrite(chr(b))
            else write(chr(b));
{$endif}
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
        else begin recs:=recs+chr(b); lrecs:=lrecs+LoCase(chr(b))end;
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

procedure aufhaengen;
var n,i : byte;
begin
  moff;
  writeln;
  attrtxt(15); write(getres(2004)); attrtxt(7);   { trenne Verbindung }
  writeln;
  mon;
  Modem.Hangup;
end;

procedure Activate;
begin
{    ActivateCom(comnr,2000,COMn[comnr].u16550);}
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
  InitCom := true;

  if TermCOM<>0 then boxpar^.bport:=TermCOM;
  if TermBaud<>0 then boxpar^.baud:=TermBaud;
  with boxpar^,ComN[boxpar^.bport] do GetComData; { IgnCD etc. }

  InitCom:=CommInit('serial port:2',CommObj);
  {* Bitte NACH WELCHEN KRITERIEN wird im Original der Comport gewaehlt?!}
  Modem.CommObj:=CommObj;
end;

procedure TermDeactivateCom;
begin
  CommObj^.Close; CommObj^.Done;
  ShellReleased:=true;
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
begin
end;

procedure SwitchStatus;
var p     : pointer;
    mx,my : byte;
begin
  mx:=wherex; my:=wherey;
  getmem(p,(screenlines-1)*ScreenWidth*2);
  moff;
{$IFNDEF ver32}
  Move(mem[base:iif(TermStatus,0,2*ScreenWidth)],p^,(screenlines-1)*ScreenWidth*2);
  termscr;
  Move(p^,mem[base:iif(TermStatus,2*ScreenWidth,0)],(screenlines-1)*ScreenWidth*2);
{$ENDIF}
  mon;
  freemem(p,(screenlines-1)*ScreenWidth*2);
  termlines:=iif(TermStatus,screenlines-1,screenlines);
  gotoxy(mx,min(my,termlines));
end;


procedure Options;
var stat : boolean;
    init : string;
    mcom : byte;        { ** }
    mbaud: longint;     { ** }
begin
{  TermCOM:=boxpar^.bport;  mcom:=TermCom;
  TermBaud:=boxpar^.baud;  mbaud:=TermBaud;
  stat:=TermStatus;
  init:=TermInit;
  SaveWin;
  TerminalOptions;
  RestWin;
  if TermStatus<>stat then SwitchStatus;
  if (TermCOM<>boxpar^.bport) or (TermBaud<>boxpar^.baud) or
     ((TermInit<>init) and not CommObj^.Carrier) then begin
    if not CommObj^.Carrier then
      if not ISDN then DropDtr(boxpar^.bport);
    ReleaseCom(boxpar^.bport);
    if not initcom then begin
      comn[boxpar^.bport].fossil:=orgfossil;
      TermCOM:=mcom;
      TermBaud:=mbaud;
      SaveConfig;
      if initcom then;
      end;
    end;}
end;


{ direct = /XPoint/Terminal }

{$ifdef Unix}

procedure terminal(direct:boolean);
var
  ende          : boolean;
  connected     : boolean;
  t             : taste;
  p             : pointer;
  rest          : boolean;
  nr            : integer;      { Welches Ger‰t }
  s             : string;
  win           : TWinDesc;     { Fenster }
begin
  Debug.DebugLog('XPFM','Terminal called',1);
  connected:= not direct;
  ende:= false;
  if not (direct) then begin
    trfehler(799,30);
    exit;
  end;
  { Ger‰t w‰hlen }
  pushhp(24001);
  s:= '^1 "'+COMn[1].MCommInit
        +'",^2 "'+COMn[2].MCommInit
        +'",^3 "'+COMn[3].MCommInit
        +'",^4 "'+COMn[4].MCommInit+'"';
  nr:= minisel(0,0,getres2(30001,4),s,1);
  pophp;
  if nr<0 then begin
    freeres;
    menurestart:= true;
    exit;
  end;
  if not CommInit(COMn[nr].MCommInit,CommObj) then begin
    trfehler(744,30);
    freeres;
    exit;
  end;
  Modem.CommObj:=CommObj;
  MakeWindow(win, 1, 4, SysGetScreenCols, SysGetScreenLines-3, '', false);
  Scroll(win, true);
  attrtxt(15);
  writeln('OpenXP ', verstr, betastr, pformstr);
  writeln('Terminal Emulation Ready (',CommObj^.GetBPSRate,')');
  attrtxt(7);
  writeln;
  open_log:=false;
  log:=false;
  in7e1:=false;
  out7e1:=false;
  display:=true;
  ansimode:=true;
  IgnCD:=COMn[nr].IgCD;
  IgnCTS:=COMn[nr].IgCTS;
  recs:='';
  lrecs:='';
  inout.cursor(curon);
  display:=true;
  pushhp(66);
  p:=@fnproc[0,9];
  fnproc[0,9]:=DummyFN;
  ansichar:=false;
  ansifg:=7; ansibg:=0;
  ansihigh:=0;
  ansirev:=false;
  while not ende do begin
    if connected and not IgnCD and not CommObj^.Carrier then begin
      moff;
      writeln;
      attrtxt(15);
      write(getres(2006));
      attrtxt(7);   { 'Verbindung getrennt - Ende mit <Alt X>' }
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
        get(t,curon);
        inout.cursor(curon);
        if t=keyf6 then begin
          savewin;
          Makroliste(7);
          restwin;
        end;
        Xmakro(t,64);
        if t=mausleft then t:=copychr(_mausx,_mausy);
        if t=keyup    then CommObj^.SendString(ANSI_curup,False) else
        if t=keydown  then CommObj^.SendString(ANSI_curdown,False) else
        if t=keyleft  then CommObj^.SendString(ANSI_curleft,False) else
        if t=keyrght  then CommObj^.SendString(ANSI_curright,False) else
        if t=keydel   then CommObj^.SendString(#127,False) else
        if t=keyhome  then CommObj^.SendString(ANSI_home,False) else
        if t=keyend   then CommObj^.SendString(ANSI_end,False) else
        if t=keyaltx  then ende:=true else
        if t=keyalth  then aufhaengen else
        if t=keyaltl  then SwitchLogfile else
        if t=keychom  then begin moff; clrscr; mon; end else
        if t=keypgup  then UpDown(false,false) else
        if t=keypgdn  then UpDown(false,true) else
        if t=keyaltd  then begin
                             TermStatus:=not TermStatus;
                             SwitchStatus;
                           end else
        if (t=keyalto) or (t=mausright) then begin
                             Options;
                             connected:=CommObj^.Carrier;
                           end else
        if t=keyf9    then begin
{               if not comn[bport].fossil then ReleaseCom(bport);
            savewin;
            dosshell;
            restwin;
            if not comn[bport].fossil then Activate;
            SetRTS(comnr);}
                      end
        else begin
          FuncExternal:=true;     { nur externe F-Tasten zugelassen }
          PreExtProc:=TermDeactivateCom;
          getfilename:=TermGetfilename;
          if test_fkeys(t) then begin
            if shellreleased then Activate;
            shellreleased:=false;
          end else
            if t[1]>#0 then begin
              if out7e1 then
                SetParity(byte(t[1]),true);
              CommObj^.SendChar(t[1])
            end;
          FuncExternal:=false;
          PreExtProc:=nil;
        end;
      end;

      if not connected and CommObj^.Carrier then
        connected:=true;
    end;

  end; { while }
  RestoreWindow(win);
  @fnproc[0,9]:=p;
  pophp;
  m2t:=true;
  rest:=not CommObj^.Carrier;
  TermDeactivateCom;
  showscreen(true);
end;

{$else}

procedure terminal(direct:boolean);
var
  t    : taste;
  ende : boolean;
  connected : boolean;
  p    : pointer;
  rest : boolean;
begin
  Debug.DebugLog('XPFM','Terminal called',1);
  connected:=not direct;
  open_log:=false; log:=false;
  in7e1:=false; out7e1:=false;
  display:=true;
  ansimode:=true;
  if direct then
    if not initcom then begin
      comn[boxpar^.bport].fossil:=orgfossil;
      m2t:=true;
      exit;
      end;
  with boxpar^,ComN[boxpar^.bport] do begin
    IgnCD:=IgCD; IgnCTS:=IgCTS;
    comnr:=bport;
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
      if connected and not IgnCD and not CommObj^.Carrier then begin
        moff;
        writeln;
        attrtxt(15); write(getres(2006)); attrtxt(7);   { 'Verbindung getrennt - Ende mit <Alt X>' }
        writeln;
        mon;
        connected:=false;
        end
      else begin
        multi2; testbyte;
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
          if t=keyup then CommObj^.SendString(ANSI_curup,False) else
          if t=keydown then CommObj^.SendString(ANSI_curdown,False) else
          if t=keyleft then CommObj^.SendString(ANSI_curleft,False) else
          if t=keyrght then CommObj^.SendString(ANSI_curright,False) else
          if t=keydel then CommObj^.SendString(#127,False) else
          if t=keyhome then CommObj^.SendString(ANSI_home,False) else
          if t=keyend then CommObj^.SendString(ANSI_end,False) else
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
                              connected:=CommObj^.Carrier;
                            end
          else
            if t=keyf9 then begin
{*              if not comn[bport].fossil then ReleaseCom(bport);
              savewin;
              dosshell;
              restwin;
              if not comn[bport].fossil then Activate;
              SetRTS(comnr);}
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
                CommObj^.SendChar(t[1])
              end;
            FuncExternal:=false;
            PreExtProc:=nil;
            end;
          end;

        if not connected and CommObj^.Carrier then connected:=true;
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
{*      if not CommObj^.Carrier then DropDtr(bport);}
      rest:=not CommObj^.Carrier;
      TermDeactivateCom;
      comn[bport].fossil:=orgfossil;
      end;
    end;
end;

{$endif}

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

type  ScrRec   = record
                   txtline   : integer;           { Zeilennr. in .SCR   }
                   onflag    : byte;              { 0=nix, 1=ON, 2=Timeout }
                                                  { 3=Online, 4=Relogin }
                   onstr     : string;            { .. String-Parameter }
                   command   : shortint;          { Befehlsnummer       }
                   numpar    : longint;           { .. num. Parameter   }
                   cr,lf     : boolean;
                   strpar    : string;            { .. String-Parameter }
                 end;
      ScrArr   = array[0..MaxLines] of scrrec;

var   script   : ScrArr;
      lines    : integer;
      logins   : integer;       { verbleibende Login-Anzahl }
      RunScriptRes: ShortInt;


function LoadScript:boolean;
type labela = array[1..maxlabels] of record
                                       name : string;
                                       line : integer;
                                     end;
var t      : text;
    s,s0   : string;
    errlog : text;
    labels : integer;
    _label : labela;
    line   : integer;    { lfd. Zeile }
    errors : integer;
    stringflag : boolean;
    ok     : boolean;

  procedure serror(nr:word; txt:string);
  begin
    writeln(errlog,getreps2(2010,0,strs(line))+getreps2(2010,nr,txt));
    ok:=false;
    inc(errors);
{*    for i:=0 to MaxLines do script[i]:='';}
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
      s0:=LeftStr(s,p-1);
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
    while (p<=labels) and (_label[p].name<>s0) do inc(p);
    if p<=labels then SeekLabel:=_label[p].line
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
      _label[labels].name:=s0;
      _label[labels].line:=lines+1;
      end;
  end;

  procedure AddCommand; forward;

  procedure AddOnCommand;
  begin
    GetString;
    if ok then with script[lines+1] do begin
      if stringflag then LoString(s0)
      else UpString(s0);
      if onflag>0 then begin
        serror(6,'');     { 'verschachtelte ON-Befehle sind nicht erlaubt' }
        s:='';
        end
      else begin
        if stringflag then begin
          onflag:=1;                          { ON "..." <Command> }
          onstr:=s0
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
          with script[lines] do begin
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
        with script[lines] do begin
          command:=cmd;
          strpar:=s0
        end;
      end;
    if ok and not comment(s) then
      with script[lines] do begin
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
    script[lines].command:=cmd;
  end;

  procedure AddDisplayCommand;
  begin
    GetWord;
    UpString(s0);
    if (s0<>'ON') and (s0<>'OFF') then
      serror(9,'')      { 'ON oder OFF erwartet' }
    else begin
      inc(lines);
      script[lines].command:=cmdDisplay;
      script[lines].numpar:=iif(s0='ON',pDispOn,pDispOff);
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
      script[lines].command:=cmdANSI;
      script[lines].numpar:=iif(s0='ON',pAnsiOn,pAnsiOff);
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
      script[lines].command:=cmdEnd;
      script[lines].numpar:=iif(s0='',pEndOk,iif(s0='ERROR',pEndError,pEndFail));
      end;
  end;

  procedure AddDelayCommand;
  begin
    GetWord;
    if rval(s0)=0 then
      serror(11,'')      { 'ungÅltiger Delay-Parameter (Zahl erwartet)' }
    else begin
      inc(lines);
      script[lines].command:=cmdDelay;
      script[lines].numpar:=system.round(rval(s0)*1000);
      if s<>'' then begin
        GetWord;
        UpString(s0);
        if s0='SHOW' then
          script[lines].strpar:=s0
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
      with script[lines] do begin
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
    with script[lines] do begin
      command:=cmd;
      numpar:=SeekLabel;
      if numpar=0 then begin              { Label (noch) nicht vorhanden }
{*        getmem(strparp,length(s0)+1);}
        strpar:=s0;
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
    for i:=1 to lines do with script[i] do
      if ((command=cmdGoto) or (command=cmdCall)) and (numpar=0) then begin
        s0:=strpar;
        numpar:=SeekLabel;
        if numpar=0 then serror(13,s0);      { 'Sprungmarke fehlt: %s' }
        strpar:='';
        end;
  end;

begin
  fillchar(script,sizeof(script),0);
  labels:=0;
  assign(t,scriptfile);
  reset(t);
  assign(errlog,LogPath+ScerrLog);
  rewrite(errlog);
  lines:=0; line:=0; errors:=0;
  while not eof(t) and (lines<MaxLines) do begin
    ok:=true;
    inc(line);
    script[lines+1].txtline:=line;
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
    timeout:=not (IgnCD or CommObj^.Carrier) or (zaehler[2]=0);
  end;

  function GetPar:string;
  var crlf : string;
  begin
    with script[ip],boxpar^ do begin
      if lf then
        if cr then crlf:=#13#10
        else crlf:=#10
      else
        if cr then crlf:=#13
        else crlf:='';
      case numpar of
        0            : if strpar='' then getpar:=strpar+crlf
                       else getpar:=strpar+crlf;
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
        CommObj^.SendString(c,False);
      end;
  end;

  procedure interprete;
  var doit : boolean;
  begin
    with script[ip] do begin
      case onflag of
        0 : doit:=true;
        1 : begin
              par:=onstr;
              doit:=(RightStr(lrecs,length(par))=par);
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
                          until timeout or (RightStr(lrecs,length(par))=par) or ende;
                          if log2<>nil then write(log2^,recs);
                          recs:=''; lrecs:='';
                        end;
          cmdSend     : CommObj^.SendString(GetPar,False);
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
          cmdFlush    : CommObj^.PurgeInbuffer;
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
          cmdBreak    : {*SendBreak(comnr)};
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
      write(trace,script[ip].txtline,' ');
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
  for i:=lines downto 1 do
    with script[i] do begin onstr:=''; strpar:=''; end;
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
  Revision 1.26  2000/11/15 23:00:44  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.25  2000/11/14 14:47:52  hd
  - Anpassung an Linux

  Revision 1.24  2000/11/12 17:28:45  hd
  - Terminal funktioniert (aber nur im Direkten Modus)

  Revision 1.23  2000/11/10 19:21:31  hd
  - Erste Vorbereitungen fuer das Terminal unter Linux
    - Funktioniert prinzipiell, aber noch nicht wirklich

  Revision 1.22  2000/10/17 10:06:01  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.21  2000/09/30 19:54:44  ma
  - auf ObjCOM umgestellt
  - *grosses* Minenfeld: Bildschirmdarstellung noch kaputt,
    Initialisierung fest einkompiliert (suche nach "CommInit"),
    Skripte hoechstwahrscheinlich kaputt (wg. Ansistrings),
    Up-/Downloads gehen genausowenig wie externe Aufrufe
  - ansonsten aber alles in Butter. ;-)

  Revision 1.20  2000/07/30 08:49:54  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.19  2000/07/27 10:13:05  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.18  2000/07/12 14:43:48  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

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

{  $Id$

   OpenXP terminal unit
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I XPDEFINE.INC}

{ OpenXP terminal unit }
unit xpterminal;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF}maske,
  sysutils,typeform,fileio,inout,keys,datadef,database,maus2,
  resource,xpglobal,xp0,xp1,xp1o2,xp1input,ObjCOM,Modem,Debug;

procedure termscr;
procedure telnet;
procedure terminal(direct:boolean);

procedure TermDeactivateCom;    { fÅr FKey-Shell }
function  TermGetfilename(nr,nn:byte):string;


implementation

uses  xp1o,xpkeys,xp9bp,xp10, winxp;

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
    IgnCD        : boolean;
    comnr        : byte;
    recs,lrecs   : string;
    display      : boolean;
    log          : boolean;
    logfile      : text;
    la           : byte;
    open_log     : boolean;
    in7e1,out7e1 : boolean;
    is_telnet	 : boolean;

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
  Wrt2(forms(iifs(is_telnet,' OpenXP-Telnet',' OpenXP-Term'),80));
  gotoxy(17,1);
  attrtxt(col.ColMenuHigh[0]);
  Wrt2('F1');
  attrtxt(col.ColMenu[0]);
  Wrt2(' '+getres2(2000,1)+'    ');   { 'Hilfe' }
  attrtxt(col.ColMenuHigh[0]);
  Wrt2('Alt-O');
  attrtxt(col.ColMenu[0]);
  Wrt2(' '+getres2(2000,2));  { 'Einstellungen' }
  attrtxt(7);
  m2t:=true;
  mon;
end;


procedure termscr;
begin
  attrtxt(7);
  moff;
  clrscr;
  mon;
  if TermStatus then begin
    termlines:=screenlines-1;
    ShowTermstatus;
    end
  else begin
    termlines:=screenlines;
    m2t:=false;
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
  gotoxy(mx,my+iif(TermStatus,1,0));
  la:=lastattr;
end;

procedure restwin;
begin
  restcursor;
  windmax:=ScreenWidth-1+256*(screenlines-1);
  attrtxt(la);
end;


procedure openlog(fn:string);
begin
  savewin;
  if not multipos(_MPMask,fn) then
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
begin
  moff;
  writeln;
  attrtxt(15); write(getres(2004)); attrtxt(7);   { trenne Verbindung }
  writeln;
  mon;
  Modem.Hangup;
end;

// Modem-Befehl senden, wertet \\ aus, z.B. ATZ\\ATI1
procedure sendmstr(s:string);
var
  p : integer;
begin
  while length(trim(s))>1 do
  begin
    p:=pos('\\',s);
    if p=0 then p:=length(s)+1;
    CommObj^.SendString((trim(leftstr(s,p-1)))+#13, false);
    s:=trim(mid(s,p+2));
  end;
end;

function initcom:boolean;
var d  : DB;
    fn : string;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(DefaultBox));
  fn:= dbReadStr(d,'dateiname');
  dbClose(d);
  ReadBox(0,fn,boxpar);
  if TermCOM<>0 then boxpar^.bport:=TermCOM;
  if TermBaud<>0 then boxpar^.baud:=TermBaud;
  ComNr := BoxPar^.BPort;

  IgnCD:=COMn[ComNr].IgCD;
//  IgnCTS:=COMn[ComNr].IgCTS;

  InitCom:=CommInit('serial port:' + IntToStr(BoxPar^.BPort) + ' speed:' +
    IntToStr(BoxPar^.Baud),CommObj);
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
(*var stat : boolean;
    init : string;
    mcom : byte;        { ** }
    mbaud: longint;     { ** } *)
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

procedure terminal_main(connected:boolean);
var
  ende          : boolean;
  t             : taste;
  p             : pointer;
{$IFDEF NCRT }
  win           : TWinDesc;     { Fenster }
{$ENDIF }
begin
  ende:=false;
  Modem.CommObj:=CommObj;
{$IFDEF NCRT }
  MakeWindow(win, 1, 4, SysGetScreenCols, SysGetScreenLines-3, '', false);
  Scroll(win, true);
{$ENDIF }
  termscr;
  moff;
  attrtxt(15);
  writeln;
  writeln(getres(iif(is_telnet,2008,2005)));
  attrtxt(7);
  writeln;

  if not is_telnet then
  begin
    CommObj^.SendString(#13, false);
    mdelay(200,true);
    if TermInit<>'' then SendMStr(TermInit);
  end;

  open_log:=false;
  log:=false;
  in7e1:=false;
  out7e1:=false;
  display:=true;
  ansimode:=true;
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
(*      if AutoDownload and (pos('*'^X'B00',recs)>0) then UpDown(true,true)
      else if AutoUpload and (pos('*'^X'B01',recs)>0) then UpDown(true,false); *)
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
	if t=keycr    then CommObj^.SendString(iifs(is_telnet,#13#10,keycr),False) else
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
        if t=keypgup  then { UpDown(false,false) } else
        if t=keypgdn  then {UpDown(false,true) }else
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
            SetRTS(comnr);}
                      end
        else begin
          FuncExternal:=true;     { nur externe F-Tasten zugelassen }
          PreExtProc:=TermDeactivateCom;
          getfilename:=TermGetfilename;
          if test_fkeys(t) then begin
//            if shellreleased then Activate;
//            shellreleased:=false;
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
{$IFDEF NCRT }
  RestoreWindow(win);
{$ENDIF }
  @fnproc[0,9]:=p;
  pophp;
  m2t:=true;
  TermDeactivateCom;
  showscreen(true);
end;

procedure terminal(direct:boolean);
var
  connected     : boolean;
begin
  Debug.DebugLog('XPFM','Terminal called',1);
  
  is_telnet:=false;
  connected:= not direct;

  if not (direct) then begin
    trfehler(799,30);
    exit;
  end;

  if not InitCom then
  begin
    trfehler(744,30); // Das GerÑt kann nicht angesprochen werden!
    freeres;
    exit;
  end;
  
  terminal_main(connected);
end;

procedure telnet;
var Host: String;
    Port: Integer;
    x,y:  Byte;
    brk:  Boolean;
begin
  Debug.DebugLog('XPFM','Telnet called',1);
  Host:='localhost';
  Port:=23;

  dialog(50,3,getres2(2009,0),x,y);
  maddstring( 3,2,getres2(2009,1),Host,18,60,'');
  maddint   (33,2,getres2(2009,2),Port,5,5,1,65535);
    mappsel(false,'23');
  freeres;
  readmask(brk);
  enddialog;

  if brk then exit;

  is_telnet:=true;
  IgnCD :=false;

  if not CommInit('telnet '+Host+':'+IntToStr(Port),CommObj) then
  begin
    tfehler(CommObj^.ErrorStr,30); // Das GerÑt kann nicht angesprochen werden!
    freeres;
    exit;
  end;

  terminal_main(true);
end;

end.

{
  $Log$
  Revision 1.2  2001/01/07 20:35:23  cl
  OpenXP-Telnet (hpts. zum Debuggen der TCP/IP-Unterst¸tzung)

  Revision 1.1  2001/01/04 16:06:49  ma
  - renamed, was xpterm.pas (partly)

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp1o2;

interface


uses  xpglobal, crt,typeform,keys,fileio,inout,maus2,datadef,database,
  stack,resource, xp0;

procedure wkey(sec:word; count:boolean);
function  DruckWiederholen:boolean;
procedure ICP(var ICR:dbIndexCRec);      { Index-Kontrollprozedur }


implementation  { --------------------------------------------------- }

uses xp1,xp1input;


procedure wkey(sec:word; count:boolean);
var t,t0   : longint;
    rest   : longint;
    last   : integer;
    c      : curtype;
    forward: boolean;
label again;
begin
  t:=ticker;
  t0:=t+system.round(sec*18.2);
  last:=-1;
  CondClearKeybuf;
  waitkey:='';
again:
  while (ticker<t0) and (ticker>=t) and not keypressed do begin
    multi2(curoff);
    { XPIdle; }
    rest:=system.round((t0-ticker)/18.2);
    if count and (rest mod 60<>last) then begin
      moff;
      write(formi(rest div 60,2),':',formi(rest mod 60,2),#8#8#8#8#8);
      mon;
      last:=rest mod 60;
      end;
    end;
  if keypressed {and (forwardkeys='')} then begin
    forward:=(forwardkeys<>'');
    c:=lastcur;
    if ParWintime then begin
      waitkey:=readkey;
      if waitkey=#0 then waitkey:=waitkey+readkey;
      end
    else
      get(waitkey,lastcur);
    cursor(c);
    if (waitkey>=mausfirstkey) and (waitkey<=mauslastkey) and
       (waitkey<>mausunleft) and (waitkey<>mausunright) then
      goto again
    else
      if forward then
        _keyboard(waitkey);
    end;
  { CondClearKeybuf; }
end;


function DruckWiederholen:boolean;
var x,y   : byte;
    t     : taste;
begin
  diabox(32,5,'',x,y);
  mwrt(x+2,y+1,getres(124));   { 'Drucker nicht bereit!' }
  errsound;
  t:='';
  case readbutton(x+2,y+3,2,getres(125),1,true,t) of  { ' ^Wiederholen , ^Abbruch ' }
    0,2 : DruckWiederholen:=true;
    1   : DruckWiederholen:=false;
  end;
  closebox;
end;


procedure ICP(var ICR:dbIndexCRec);      { Index-Kontrollprozedur }
const x: byte = 0;
      y: byte = 0;
      lastper : byte = 101;
begin
  with ICR do
    case command of

      icIndexNum:    if df=MsgFile then indexnr:=2
                     else if df=BrettFile then indexnr:=4
                     else if df=UserFile then indexnr:=2
                     else if df=BoxenFile then indexnr:=2
                     else if df=GruppenFile then indexnr:=2
                     else if df=SystemFile then indexnr:=1
                     else if df=DupeFile then indexnr:=1
                     else if df=AutoFile then indexnr:=1
                     else if df=PseudoFile then indexnr:=1
                     else if df=BezugFile then indexnr:=2
                     else if df=MimetFile then indexnr:=2
                     else interr('icIndexNum: falsche Datei: '+df);

      icIndex:       if df=MsgFile then
                       case indexnr of
                         miBrett    : indexstr:='BRETT/EMPFDATUM/INT_NR';
                         miGelesen  : indexstr:='BRETT/GELESEN/EMPFDATUM/INT_NR';
                       end
                     else if df=BrettFile then
                       case indexnr of
                         biBrett    : indexstr:='+BRETTNAME';
                         biGruppe   : indexstr:='GRUPPE';
                         biIntNr    : indexstr:='INT_NR';
                         biIndex    : indexstr:='INDEX';
                       end
                     else if df=UserFile then
                       case indexnr of
                         uiName     : indexstr:='+USERNAME';
                         uiAdrbuch  : indexstr:='ADRBUCH/+USERNAME';
                       end
                     else if df=BoxenFile then
                       case indexnr of
                         boiName    : indexstr:='+BOXNAME';
                         boiDatei   : indexstr:='+DATEINAME';
                       end
                     else if df=GruppenFile then
                       case indexnr of
                         giName     : indexstr:='+NAME';
                         giIntnr    : indexstr:='INT_NR';
                       end
                     else if df=SystemFile then
                       indexstr:='+NAME'
                     else if df=DupeFile then
                       indexstr:='HALTEFLAGS/+BRETT/ORIGDATUM/MSGID'{/EMPFDATUM'}
                     else if df=AutoFile then
                       indexstr:='+BETREFF/EMPFAENGER'
                     else if df=PseudoFile then
                       indexstr:='+KURZNAME'
                     else if df=BezugFile then
                       case indexnr of
                         beiMsgID   : indexstr:='MSGID';
                         beiRef     : indexstr:='REF';
                       end
                     else if df=MimetFile then
                       case indexnr of
                         mtiTyp     : indexstr:='+TYP/+EXTENSION';
                         mtiExt     : indexstr:='+EXTENSION';
                       end
                     else interr('icIndex: falsche Datei: '+df);

      icOpenWindow:  begin
                       msgbox(26,4,'',x,y);
                       mwrt(x+2,y+1,getreps(126,df));
                       mwrt(x+2,y+2,getres(127));  { 'bitte warten...     %' }
                       attrtxt(col.colmboxhigh);
                     end;
      icShowIx,
      icShowConvert,
      icShowPack,
      icShowKillX:   if percent<>lastper then begin
                       lastper:=percent;
                       gotoxy(x+18,y+2);
                       moff;
                       write(icr.percent:3);
                       mon;
                       multi2(curoff);
                       end;

      icCloseWindow: begin
                       CloseBox;
                       lastper:=101;
                     end;

      icOpenPWindow: begin
                       msgbox(25,4,'',x,y);
                       mwrt(x+2,y+1,getreps(128,df));
                       mwrt(x+2,y+2,getres(127));
                       attrtxt(col.colmboxhigh);
                     end;

      icOpenCWindow,
      icOpenKwindow: begin
                       msgbox(31,4,'',x,y);
                       mwrt(x+2,y+1,getreps(129,df));
                       mwrt(x+2,y+2,getres(127));
                       attrtxt(col.colmboxhigh);
                     end;

    end;
end;


end.


{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team (Urversion von Martin Wodrich )            }
{ , http://www.openxp.de                                          }
{                                                                 }
{ Xp-Easy-Konfigurationsmodus (OpenXP)                            }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}
{$IFDEF BP}
  {$O+,F+}
{$ENDIF}

unit XpEasy;

interface

uses crt,dos,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
     maus2,mouse,resource,xpglobal,
     xp0,xp1,xp1o,xp1o2,xp1input,xp2c;

function NeuBenutzergruss:boolean;
procedure EasyMainDialog;

implementation

function NeuBenutzergruss:boolean;
  var x,y,i : byte;
      msglines    : byte;
      z     : taste;
      s:String;
  begin
    msglines:=ival(getres2(14000,0));
    msgbox(73,msglines+7,'',x,y);
    moff;
    wrt(x+3,y+1,'Cross \\//    '+
                 Right('           ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 50));
    wrt(x+3,y+2,'      //\\ Point');
    s:=x_copyright + ' ' + author_name;
    wrt(x+67-length(s),y+2,s);
    for i:=1 to msglines do
    begin
      s:=getres2(14000,i);
      wrt(x+3,y+3+i, s);
    end;
    mon;
    pushhp(1550);
    NeuBenutzergruss := (ReadButton(x+35,y+msglines+5,2,'*'+getres2(14000,30),1,true,z) <> 1);
    pophp;
    closebox;
    freeres;
  end;

  procedure EasyMainDialog;
  var x,y,i : byte;
      msglines    : byte;
      z     : taste;
      s:String;
      brk : boolean;
  begin
    {Adressconfig fuer Easy}
    msglines:=ival(getres2(14001,0));
    dialog(ival(getres2(252,100)),8+msglines,getres2(252,101),x,y);  { 'Adreáeinstellungen (ZCONNECT / RFC)' }
    msglines:=ival(getres2(14001,0));
    for i:=1 to msglines do
    begin
      maddtext(3,i+1,getres2(14001,i),col.coldialog);
    end;
    maddstring(3,3+msglines,getres2(252,103),postadresse^,47,PostadrLen,'');   { 'Postanschrift ' }
    msetvfunc(TestPostanschrift);
    maddstring(3,4+msglines,getres2(252,104),telefonnr^,47,TeleLen,'>VFBQP +-0123456789');
    msetvfunc(TestTelefon);                                 { 'Telefon       ' }
    maddstring(3,5+msglines,getres2(252,105),wwwHomepage^,47,Homepagelen,range(' ','~'));
    msetvfunc(TestUrl);
    maddbool(3,7+msglines,getres2(252,109),adrpmonly);   { 'Adresse, Telefon und Homepage nur in PMs' }
    readmask(brk);
    closemask;
    closebox;

  end;
end.
{
  $Log$
  Revision 1.4  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf ver„ndert

  Revision 1.3  2000/04/22 20:08:15  mw

  - EASY-Modus per Compilerschalter abschaltbar (damit in office-Beta
    noch nicht drin !!!)
  - Elegantere Programmierung des Aufrufs
  - Umbennenung der neuen Procedure und Function
  - Fehler durch unvollst„ndiges Init beseitigt (FirstBox wurde noch nicht
    angelegt
  - Organisation wird nicht mehr im EASY-Mode abgefragt, Rest der Adressconfig
    aber weiterhin
  - Beseitigung unn”tiger MW-Verweise

  Revision 1.2  2000/04/22 18:24:05  mw

  - Erste Dialoge des Easy-Mode
  Achtung: Easy-Mode ist noch unvollst„ndig
           Man kann sich aber schon die ersten Dialoge ansehen
           Derzeit aber nur in der deutschen Version !!!

  Revision 1.1  2000/04/22 14:30:51  mw

  - EASY-Mode Teil 2 begonnen

}

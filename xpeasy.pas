{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team (Urversion von Martin Wodrich)             }
{ http://www.openxp.de                                            }
{                                                                 }
{ Xp-Easy-Konfigurationsmodus (OpenXP)                            }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I xpdefine.inc}

unit xpeasy;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
  maus2,mouse,resource,xpglobal,
  xp0,xp1,xp1o,xp1o2,xp1input,xp2c;

function NeuBenutzergruss:boolean;
procedure EasyMainDialog;

implementation

function NeuBenutzergruss:boolean;
  var x,y,i : integer;
      msglines    : byte;
      z     : taste;
      s:String;
  begin
    msglines:=ival(getres2(14000,0));
    msgbox(73,msglines+7,'',x,y);
    moff;
//    wrt(x+3,y+1,'Cross \\//    '+
//                 RightStr('           ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 50));
//    wrt(x+3,y+2,'      //\\ Point');
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
  var x,y,i : Integer;
      msglines: Integer;
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

    maddstring(3,3+msglines,getres2(252,103),postadresse,47,PostadrLen,'');   { 'Postanschrift ' }
    msetvfunc(TestPostanschrift);
    maddstring(3,4+msglines,getres2(252,104),telefonnr,47,TeleLen,'>VFBQP +-0123456789');
    msetvfunc(TestTelefon);                                 { 'Telefon       ' }
    maddstring(3,5+msglines,getres2(252,105),wwwHomepage,47,Homepagelen,range(' ','~'));
    msetvfunc(TestUrl);
    maddbool(3,7+msglines,getres2(252,109),adrpmonly);   { 'Adresse, Telefon und Homepage nur in PMs' }
    readmask(brk);
    closemask;
    closebox;

  end;

end.

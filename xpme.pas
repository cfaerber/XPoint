{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ CrossPoint-Menueeditor }
// !! not tested yet!!
//ToDo: support for other languages but German...

{$I xpdefine.inc }

unit xpme;

interface

procedure StartXPME;
{ procedure name conforming to calls in main.StartInternalTools }
procedure StartCommandLineXPME;

implementation

uses  //last check: 2002-12-03 DoDi
{$IFDEF unix}
  xplinux,
{$ENDIF }
  winxp,
{$IFDEF NCRT }
  xpcurses,
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
  osdepend, sysutils, typeform,fileio,keys,maus2,inout,resource,xpglobal, xp1;

const menus      = 99;
      maxhidden  = 500;
      menufile   = 'xpmenu.dat';  //todo: filename!
      colcfgfile = 'xpoint.col';  //todo: filename!
      meversion  = 1;     { Versionsnummer Menuedatenformat }

      menupos : array[0..menus] of byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
      modi  : boolean = false;
      ropen : boolean = false;
      saved : boolean = false;
      hcursor : boolean = false;   { Blindencursor }

{.$I xpmecol.inc}   { Farben }

type  ColArr = array[0..3] of byte;
      ColRec = record
                  ColMenu       : ColArr; { Normaler Menuetext       }
                  ColMenuHigh   : ColArr; { Direkt-Buchstaben        }
                  ColMenuInv    : ColArr; { Menue-Balken             }
                  ColMenuInvHi  : ColArr; { Menue-Balken/Buchstabe   }
                  ColMenuDis    : ColArr; { Menue disabled           }
                  ColMenuSelDis : ColArr; { Menue disabled/gewaehlt  }
                  ColKeys       : byte;   { Direkttasten             }
                  ColKeysHigh   : byte;   { Direkttasten-Buchstaben  }
                  ColKeysAct    : byte;   { aktivierte Taste         }
                  ColKeysActHi  : byte;   { aktivierter Buchstabe    }
                  ColTLine      : byte;   { Trennlinie               }
                  ColBretter    : byte;   { User / Bretter           }
                  ColBretterInv : byte;   { User / Bretter, gewaehlt }
                  ColBretterHi  : byte;   { User / Bretter, markiert }
                  ColBretterTr  : byte;   { Trennzeile               }
                  ColMsgs       : byte;   { Msgs                     }
                  ColMsgsHigh   : byte;   { Msgs, markiert           }
                  ColMsgsInv    : byte;   { Msgs, gewaehlt           }
                  ColMsgsInfo   : byte;   { Msgs, 1. Zeile           }
                  ColMsgsUser   : byte;   { PM-archivierte Msgs      }
                  ColMsgsInvUser: byte;   { gewaehlt+hervorgehoben   }
                  ColMbox       : byte;   { Meldungs-Box, Text       }
                  ColMboxRahmen : byte;   { Meldungs-Box, Rahmen     }
                  ColMboxHigh   : byte;   { Meldungs-Box, hervorgeh. }
                  ColDialog     : byte;   { Dialoge, Feldnamen u.ae. }
                  ColDiaRahmen  : byte;   { Dialogbox, Rahmen        }
                  ColDiaHigh    : byte;   { Dialogbox, hervorgeh.T.  }
                  ColDiaInp     : byte;   { Dialogbox, Eingabefeld   }
                  ColDiaMarked  : byte;   { Dial., markierter Text   }
                  ColDiaArrows  : byte;   { Pfeile bei Scrollfeldern }
                  ColDiaSel     : byte;   { Masken-Auswahlliste      }
                  ColDiaSelBar  : byte;   {            "             }
                  ColDiaButtons : byte;   { Check/Radio-Buttons      }
                  ColSelbox     : byte;   { Auswahlbox               }
                  ColSelRahmen  : byte;   { Auswahlbox, Rahmen       }
                  ColSelHigh    : byte;   { Auswahlbox, hervorgeh.   }
                  ColSelBar     : byte;   { Auswahlbox, Balken       }
                  ColSel2box    : byte;   { Auswahlbox / dunkel      }
                  ColSel2Rahmen : byte;   { Auswahlbox, Rahmen       }
                  ColSel2High   : byte;   { Auswahlbox, hervorgeh.   }
                  ColSel2Bar    : byte;   { Auswahlbox, Balken       }
                  ColButton     : byte;   { Button                   }
                  ColButtonHigh : byte;   { Button - Hotkeys         }
                  ColButtonArr  : byte;   { aktiver Button: Pfeile   }
                  ColUtility    : byte;   { Kalender u.ae.           }
                  ColUtiHigh    : byte;
                  ColUtiInv     : byte;
                  ColHelp       : byte;   { Hilfe normal            }
                  ColHelpHigh   : byte;   { hervorgehobener Text    }
                  ColHelpQVW    : byte;   { Querverweis             }
                  ColHelpSlQVW  : byte;   { gewaehlter Querverweis  }
                  ColListText   : byte;   { Lister, normaler Text   }
                  ColListMarked : byte;   { Lister, markiert        }
                  ColListSelbar : byte;   { Lister, Auswahlbalken   }
                  ColListFound  : byte;   { Lister, nach Suche mark.}
                  ColListStatus : byte;   { Lister, Statuszeile     }
                  ColListQuote  : byte;   { Quote-Zeilen + Maps"J"  }
                  ColListScroll : byte;   { vertikaler Scroller     }
                  ColListHeader : byte;   { Nachrichtenkopf         }
                  ColListHigh   : byte;   { *hervorgehoben*         }
                  ColListQHigh  : byte;   { Quote / *hervorgehoben* }
                  ColEditText   : byte;   { Editor, normaler Text   }
                  ColEditStatus : byte;   { Editor, Statuszeile     }
                  ColEditMarked : byte;   { Editor, markierter Blck.}
                  ColEditMessage: byte;   { Editor-Meldung          }
                  ColEditHead   : byte;   { TED: Info-Kopf          }
                  ColEditQuote  : byte;   { TED: farbige Quotes     }
                  ColEditEndmark: byte;   { TED: Endmarkierung      }
                  ColEditMenu   : byte;   { TED: Menue              }
                  ColEditMenuHi : byte;   { TED: Hotkey             }
                  ColEditMenuInv: byte;   { TED: Selbar             }
                  ColEditHiInv  : byte;   { TED: gewaehlter Hotkey  }
                  ColArcStat    : byte;   { Status-Zeile ArcViewer  }
                  ColMapsBest   : byte;   { bestellte Bretter       }
                  ColMailer     : byte;   { Fido-Mailer/uucico      }
                  ColMailerhigh : byte;   { .. hervorgehoben #1     }
                  ColMailerhi2  : byte;   { .. hervorgehoben #2     }
                  ColBorder     : byte;   { Rahmenfarbe             }

                  ColBack       : byte;   { XPME: Hintergrund       }
                  ColHBox       : byte;   { XPME: Hinweisfenster    }
                  ColHboxHi     : byte;
               end;


var   col   : colrec;


procedure defaultcolors;
begin
  with col do
  begin
    colmenu[0]:=$70; colmenu[1]:=$70; colmenu[2]:=$70; colmenu[3]:=$70;
    colmenuhigh[0]:=$74; colmenuhigh[1]:=$74; colmenuhigh[2]:=$74; colmenuhigh[3]:=$74;
    colmenuinv[0]:=$17; colmenuinv[1]:=$17; colmenuinv[2]:=$17; colmenuinv[3]:=$17;
    colmenuinvhi:=colmenuinv;
    colmenudis[0]:=$78; colmenudis[1]:=$78; colmenudis[2]:=$78; colmenudis[3]:=$78;
    colmenuseldis[0]:=$13; colmenuseldis[1]:=$13; colmenuseldis[2]:=$13;
    colmenuseldis[3]:=$13;
    colkeys:=3; colkeyshigh:=14; colkeysact:=$13; colkeysacthi:=$1e;
    coltline:=4;
    colbretter:=7; colbretterinv:=$30; colbretterhi:=2; colbrettertr:=3;
    colmsgs:=7; colmsgshigh:=2; colmsgsinv:=$30; colmsgsinfo:=2;
    colmsgsuser:=lightred; colmsgsinvuser:=$30+red;
    colmbox:=$70; colmboxrahmen:=$70; colmboxhigh:=$7f;
    coldialog:=$70; coldiarahmen:=$70; coldiahigh:=$7e;
    coldiainp:=$1e; coldiaarrows:=$1a;
    coldiamarked:=$2f;
    coldiasel:=$30; coldiaselbar:=7;
    colselbox:=$70; colselrahmen:=$70; colselhigh:=$7f; colselbar:=$1e;
    colsel2box:=$87; colsel2rahmen:=$87; colsel2high:=$8f; colsel2bar:=$4e;
    colhelp:=$70; colhelphigh:=$7e; colhelpqvw:=$71; colhelpslqvw:=$30;
    coldiabuttons:=$8f;
    colbutton:=$17; colbuttonhigh:=$1f; colbuttonarr:=$1b;
    colutility:=$30; colutihigh:=$3e; colutiinv:=11;
    collisttext:=7; collistselbar:=$30; collistmarked:=green;
    collistfound:=$71; colliststatus:=lightred; collistquote:=3;
    collistscroll:=7; collistheader:=7; collisthigh:=$f; collistqhigh:=11;
    coledittext:=7; coleditmarked:=$17; coleditstatus:=$17; coleditmessage:=$1e;
    coledithead:=$70; coleditquote:=3; coleditendmark:=7;
    coleditmenu:=$70; coleditmenuhi:=$74; coleditmenuinv:=$17;
    coledithiinv:=$17;
    colarcstat:=3; colmapsbest:=lightred;
    colmailer:=$70; colmailerhigh:=$7f; colmailerhi2:=$7e;
    colborder:=0;
    colback:=8; colhbox:=$13; colhboxhi:=$1e;
  end
end;


procedure readcol;
const maxcol = 15;
var t       : text;
    s       : string;
    ca      : array[1..maxcol] of byte;
    n,p     : byte;
    msk,mnr : byte;
    s1      : string[20];
    l       : longint;
    res     : integer;
    buf     : array[1..512] of byte;

  procedure getb(var b:byte);
  var i : byte;
  begin
    if n>0 then begin
      b:=ca[1];
      dec(n);
      for i:=1 to n do
        ca[i]:=ca[i+1];
      end;
  end;

begin
  defaultcolors;
  assign(t,colcfgfile);
  if not existf(t) then exit;
  msk:=$ff;
  settextbuf(t,buf);
  reset(t);
  while not eof(t) do with col do begin
    readln(t,s);
    s:=LowerCase(trim(s));
    p:=cpos('=',s);
    if (s<>'') and (s[1]<>'#') and (p>0) then begin
      s1:=copy(s,1,min(p-1,20));
      s:=trim(Mid(s,p+1))+' ';
      n:=0;
      repeat
        p:=cpos(' ',s);
        if p>0 then begin
          val(LeftStr(s,p-1),l,res);
          delete(s,1,p);
          while (FirstChar(s)=' ') do DeleteFirstChar(s);
          if (res=0) and (l>=0) and (l<$100) then begin
            inc(n); ca[n]:=l and msk;
            end;
          end;
      until (p=0) or (n=maxcol);
      if (s1>='menue0') and (s1<='menue3') then begin
        mnr:=ival(s1[6]);
        getb(colmenu[mnr]); getb(colmenuhigh[mnr]); getb(colmenuinv[mnr]);
        getb(colmenuinvhi[mnr]); getb(colmenudis[mnr]); getb(colmenuseldis[mnr]);
        end
      else if s1='hotkeys' then begin
        getb(colkeys); getb(colkeyshigh); getb(colkeysact); getb(colkeysacthi);
        end
      else if s1='trennlinien' then
        getb(coltline)
      else if s1='bretter' then begin
        getb(colbretter); getb(colbretterinv); getb(colbretterhi);
        getb(colbrettertr);
        end
      else if s1='msgs' then begin
        getb(colmsgs); getb(colmsgshigh); getb(colmsgsinv);
        getb(colmsgsinfo); getb(colmsgsuser); getb(colmsgsinvuser);
        end
      else if s1='mbox' then begin
        getb(colmbox); getb(colmboxrahmen); getb(colmboxhigh);
        end
      else if s1='dialog' then begin
        getb(coldialog); getb(coldiarahmen); getb(coldiahigh);
        getb(coldiainp); getb(coldiamarked); getb(coldiaarrows);
        getb(coldiasel); getb(coldiaselbar); getb(coldiabuttons);
        end
      else if s1='sel1' then begin
        getb(colselbox); getb(colselrahmen); getb(colselhigh); getb(colselbar);
        end
      else if s1='sel2' then begin
        getb(colsel2box); getb(colsel2rahmen); getb(colsel2high);
        getb(colsel2bar);
        end
      else if s1='buttons' then begin
        getb(colbutton); getb(colbuttonhigh); getb(colbuttonarr);
        end
      else if s1='utility' then begin
        getb(colutility); getb(colutihigh); getb(colutiinv);
        end
      else if s1='hilfe' then begin
        getb(colhelp); getb(colhelphigh); getb(colhelpqvw); getb(colhelpslqvw);
        end
      else if s1='lister' then begin
        getb(collisttext); getb(collistmarked); getb(collistselbar);
        getb(collistfound); getb(colliststatus); getb(collistquote);
        getb(collistscroll); getb(collistheader); getb(collisthigh);
        getb(collistqhigh);
        end
      else if s1='editor' then begin
        getb(coledittext); getb(coleditmarked); getb(coleditstatus);
        getb(coleditmessage); getb(coledithead); getb(coleditquote);
        getb(coleditendmark); getb(coleditmenu); getb(coleditmenuhi);
        getb(coleditmenuinv); getb(coledithiinv);
        end
      else if s1='arcviewer' then
        getb(colarcstat)
      else if s1='maps' then
        getb(colmapsbest)
      else if s1='mailer' then begin
        getb(colmailer); getb(colmailerhigh); getb(colmailerhi2);
        end
      else if s1='border' then
        getb(colborder);
      end;
    end;
  close(t);
end;


{ Button-Abfrage ----------------------------------------------------- }
{ x,y     : Position des linken Buttons                                }
{ abs     : Leerabstand zwischen Buttons                               }
{ buttons : '^Butt1,^Butt2...'                                         }
{ default : Startwert fuer p                                           }
{ homeend : die Tasten Home/End sind zugelassen                        }
{ retkey  : '' -> Normale Abfrage. '*' -> bei jeder unbekannten Taste  }
{           wird die Taste in 'retkey' und readbutton<0 zurueckgegeben }
{           '!' -> nur Anzeige der Buttons, und direkt zurueck         }
{ RETURN:  0 oder p bei normaler Abfrage, -p bei retkey='*' und Esc    }

function readbutton(x,y,abs:byte; buttons:string; default:shortint;
                    homeend:boolean; var retkey:taste):shortint;
const maxbutt = 8;
var p,n,p1,i : byte;
    butt     : array[1..maxbutt] of string[30];
    butthigh : array[1..maxbutt] of byte;
    buttsp   : array[1..maxbutt] of byte;
    hot      : string[maxbutt];
    t        : taste;
    stop     : boolean;
    spenter  : boolean;

  procedure display;
  var i : byte;
  begin
    gotoxy(x,y);
    attrtxt(col.colbutton);
    moff;
    for i:=1 to n do begin
      if buttsp[i]>0 then
        gotoxy(wherex+buttsp[i],wherey);
      if i=p then begin
        attrtxt(col.colbuttonarr);
        wrt2(#16);
        end
      else
        wrt2(' ');
      attrtxt(col.colbutton);
      wrt2(LeftStr(butt[i],butthigh[i]-1));
      attrtxt(col.colbuttonhigh);
      wrt2(hot[i]);
      attrtxt(col.colbutton);
      wrt2(copy(butt[i],butthigh[i]+1,40));
      if i=p then begin
        attrtxt(col.colbuttonarr);
        wrt2(#17);
        attrtxt(col.colbutton);
        end
      else
        wrt2(' ');
      gotoxy(wherex+abs,wherey);
      end;
    mon;
  end;

  procedure maus_bearbeiten;
  var xx,yy,i,_x : integer;
  begin
    maus_gettext(xx,yy);
    if (yy=y) and (xx>=x) then begin
      _x:=x;
      i:=1;
      while (i<=n) and (xx>=_x+length(butt[i])+2) do begin
        inc(_x,length(butt[i])+buttsp[i]+abs+2);
        inc(i);
        end;
      if (i<=n) and (xx>=_x) then
        if (t=mausleft) or (t=mauslmoved) then begin
          p:=i; t:=#0; end else
        if t=mausunleft then t:=hot[i];
      end;
  end;

begin
  spenter:=(firstchar(buttons)='*');
  if spenter then DeleteFirstChar(buttons);
  SetLength(buttons,Length(buttons)); { inc(byte(buttons[0])); }
  buttons[length(buttons)]:=',';
  n:=0;
  repeat
    p:=cPos(',',buttons);
    if p>0 then begin
      inc(n);
      if buttons[1]='˘' then begin
        i:=2; while (buttons[i]>='0') and (buttons[i]<='9') do inc(i);
        buttsp[n]:=ival(copy(buttons,2,i-2));
        buttons:=copy(buttons,i,255);
        dec(p,i-1);
        end
      else
        buttsp[n]:=0;
      butt[n]:=LeftStr(buttons,p-1);
      buttons:=copy(buttons,p+1,255);
      p:=cPos('^',butt[n]);
      delete(butt[n],p,1);
      butthigh[n]:=p;
      hot[n]:=butt[n,p];
    end;
  until p=0;
  if retkey='!' then begin
    display;
    readbutton:=0;
    end
  else begin
    hot[0]:=chr(n);
    p:=default;
    repeat
      mauszul:=(p>1); mauszur:=(p<n);
      display;
      repeat get(t,curoff) until t<>#0#0;
      stop:=false;
      if (t>=mausfirstkey) and (t<=mauslastkey) then
        maus_bearbeiten;
      if (t=keytab) or (not spenter and (t=' ')) or (t=keyrght) then
        p:=p mod n + 1
      else if (t=keystab) or (t=keyleft) then
        if p=1 then p:=n else dec(p)
      else if homeend and (t=keyhome) then p:=1
      else if homeend and (t=keyend) then p:=n
      else begin
        p1:=pos(UpperCase(t),UpperCase(hot));
        if p1>0 then begin
          p:=p1; display;
          t:=keycr; end
        else
          if (t<>keycr) and (t<>keyesc) and (t<>#0) and (retkey='*') then
            stop:=true;
        end;
      if spenter and (t=' ') then t:=keycr;
    until (t=keycr) or (t=keyesc) or stop;
    mauszul:=true; mauszur:=true;
    if stop then begin
      readbutton:=-p;
      retkey:=t;
      end
    else
      if t=keyesc then readbutton:=0
      else readbutton:=p;
    end;
end;


type  mprec     = record
                    mstr    : string[30];
                    hpos    : byte;
                    hkey    : char;
                    enabled : boolean;
                    chain   : byte;      { Untermenue-Nr. }
                    keep    : boolean;   { Menue nicht verlassen }
                    mpnr    : integer;   { Nummer des Menuepunkts }
                  end;
      menuarray = array[1..22] of mprec;
      map       = ^menuarray;
const mainmenu  : map = nil;

var   menu      : array[0..menus] of string;
      menulevel : byte;
      main_n    : integer;
      hmpos     : array[1..10] of byte;  { Hauptmenue-XPos }
      hidden    : array[1..maxhidden] of integer;
      anzhidden : integer;
      specials  : string;

procedure wrlogo;
begin
  writeln;
  writeln('CrossPoint-MenÅeditor    (c) ''96-99 Peter Mandrella, Freeware');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;


procedure error(txt:string);
begin
  wrlogo;
   writeln('Fehler: ', txt, #7);
  if ropen then CloseResource;
  halt;
end;


procedure readmenus;
var t  : text;
    s  : string;
    i  : integer;
begin
  if not fileexists('openxp.rsp') and not fileexists('openxp-d.res') then begin
    wrlogo;
    writeln('Fehler: ''openxp-d.res'' nicht gefunden.'#7);
    writeln;
    writeln('Starten Sie dieses Programm bitte in einem Verzeichnis, in dem');
    writeln('CrossPoint vollstÑndig installiert wurde.');
    writeln;
    halt;
    end;
  assign(t,'openxp.rsp');
  if existf(t) then begin
    reset(t);
    readln(t,s);
    close(t);
    end
  else
    s:='openxp-d.res';
  if not fileexists(s) then
    error(s+' fehlt!');
  OpenResource(s,10000);
  ropen:=true;
  if ival(getres(6))<11 then
    error('Es wird CrossPoint Version 3.11 oder hîher benîtigt!');
  for i:=0 to menus do begin
    s:=getres2(10,i);   { "[fehlt:...]" kann hier ignoriert werden. }
    menu[i]:=s;
    end;
  specials:=getres2(10,200);
  CloseResource;
end;


procedure Readconfig;
var t : text;
    s : string;
    p : byte;
begin
  assign(t,'xpoint.cfg');
  if existf(t) then begin
    reset(t);
    repeat
      readln(t,s);
      p:=cpos('=',s);
      if (s[1]<>'#') and (p>0) then
        if LowerCase(LeftStr(s,p-1))='auswahlcursor' then
          hcursor:=(UpperCase(mid(s,p+1))='J');
    until eof(t);
    close(t);
    end;
end;


procedure showscreen;
var
  i: integer;
{$IFDEF NCRT }
  x1, x2, y1, y2: integer;
{$ENDIF }
begin
  shadowcol:=0;
  cursor(curoff);
  attrtxt(7);
  clrscr;
  syssetbackintensity;
  attrtxt(col.colmenu[0]);
  {$IFDEF NCRT } { <- Evntl. neuer Token: VarScrSize ? }
  wrt2(sp(SysGetScreenLines));
  attrtxt(col.colback);
  for i:=1 to SysGetScreenLines do
    wrt(1,i,dup(SysGetScreenCols, #177));
  {$ELSE }
  wrt2(sp(80));
  attrtxt(col.colback);
  for i:=1 to 24 do
    wrt2(dup(80,'±'));
  {$ENDIF }
  attrtxt(col.colutility);
  forcecolor:=true;
  {$IFDEF NCRT }
  x1:= SysGetScreenCols-42;
  x2:= SysGetScreenCols-2;
  y1:= SysGetScreenLines-7;
  y2:= SysGetScreenLines-2;
  rahmen1(x1,x2-1,y1,y2-1,'');
  wrt(x1+2,y1,' CrossPoint-MenÅeditor ');
  clwin(x1+1,x2-2,y1+1,y2-2);
  forcecolor:=false;
  attrtxt(col.colutihigh);
  wrt(x1+3,y1+1,#27#24#25#26);
  wrt(x1+3,y1+2,'+ -');
  wrt(x1+3,y1+3,'Esc');
  attrtxt(col.colutility);
  wrt(x1+8,y1+1,'MenÅ(punkt) wÑhlen');
  wrt(x1+8,y1+2,'MenÅ(punkt) (de)aktivieren');
  wrt(x1+8,y1+3,'Ende');
  {$ELSE }
  rahmen1(38,78,18,23,'');
  wshadow(39,79,19,24);
  wrt(40,18,' CrossPoint-MenÅeditor ');
  clwin(39,77,19,22);
  forcecolor:=false;
  attrtxt(col.colutihigh);
  wrt(42,20,#27#24#25#26);
  wrt(42,21,'+ -');
  wrt(42,22,'Esc');
  attrtxt(col.colutility);
  wrt(50,20,'MenÅ(punkt) wÑhlen');
  wrt(50,21,'MenÅ(punkt) (de)aktivieren');
  wrt(50,22,'Ende');
  {$ENDIF }
end;


procedure msgbox(wdt,hgh:byte; txt:string; var x,y:byte);
begin
  x:=(80-wdt)div 2;
  y:=(25-hgh)div 2;
  attrtxt(col.colmbox);
  forcecolor:=true;
  wpushs(x,x+wdt,y,y+hgh,'');
  forcecolor:=false;
  if txt<>'' then wrt(x+2,y,' '+txt+' ');
end;


procedure closebox;
begin
  wpop;
end;

{ --- Menuesystem -------------------------------------------------------- }

function special(nr:integer):boolean;
var x,y : byte;
    t   : taste;
begin
  if pos('$'+hex(nr,3),UpperCase(specials))>0 then begin
    msgbox(60,6,'',x,y);
    wrt(x+3,y+2,'Dieser MenÅpunkt wird von XP automatisch aktiviert bzw.');
    wrt(x+3,y+3,'deaktiviert (s. XP-Handbuch).');
    wrt(x+3,y+5,'Taste drÅcken ...');
    errsound;
    get(t,curon);
    closebox;
    special:=true;
    end
  else
    special:=false;
end;


procedure click;
begin
  SysBeep(4000, 10);
end;


procedure addhidden(nr:integer);
var i : integer;
begin
  if anzhidden<maxhidden then begin
    i:=anzhidden+1;
    while (i>1) and (hidden[i-1]>=nr) do
      dec(i);
    if (i>anzhidden) or (hidden[i]<>nr) then begin
      if i<=anzhidden then
         System.Move(hidden[i],hidden[i+1],(anzhidden+1-i)*sizeof(hidden[1]));
      hidden[i]:=nr;
      inc(anzhidden);
      click;
      end;
   end;
end;


procedure delhidden(nr:integer);
var i : integer;
begin
  i:=1;
  while (i<=anzhidden) and (hidden[i]<>nr) do
    inc(i);
  if i<=anzhidden then begin
    if (i<anzhidden) then
       System.Move(hidden[i+1],hidden[i],(anzhidden-i)*sizeof(hidden[1]));
    dec(anzhidden);
    click;
    end;
end;


function ishidden(nr:integer):boolean;
var l,r,m : integer;
begin
  l:=1; r:=anzhidden;
  while (r-l>1) do begin
    m:=(l+r) div 2;
    if hidden[m]<nr then l:=m
    else r:=m;
    end;
  ishidden:=(r>0) and ((nr=hidden[l]) or (nr=hidden[r]));
end;


procedure splitmenu(nr:byte; ma:map; var n:integer);
var s       : string;
    p,p2,p3 : byte;
begin
  s:=menu[nr];
  n:=0;
  repeat
    p:=cPos(',',s);
    if p>0 then begin
      inc(n);
      with ma^[n] do begin
        s:=Mid(s,p+1);
        if LeftStr(s,2)<>'-,' then begin
          mpnr:=hexval(LeftStr(s,3));
          delete(s,1,3);
          end
        else
          mpnr:=0;
        enabled:=not ishidden(mpnr);
        if s[1]='!' then begin      { Menue nicht verlassen? }
          keep:=true;
          delete(s,1,1);
          end
        else
          keep:=false;
        p2:=cPos('^',s);
        p3:=cPos(',',s);
        if (p3=0) or ((p2>0) and (p2<p3)) then begin
          if p2>0 then delete(s,p2,1);
          if p3>0 then dec(p3);
          hpos:=p2;
          end
        else
          hpos:=0;
        p2:=p3;
        if p2=0 then mstr:=s
        else mstr:=LeftStr(s,p2-1);
        if hpos>0 then hkey:=UpCase(mstr[hpos])
        else hkey:=#255;
        if cPos('˘',mstr)>0 then begin
          p2:=cPos('˘',mstr);
          chain:=ival(copy(mstr,p2+1,40));
          mstr:=copy(mstr,1,p2-1);
          if (nr>0) and (pos('..',mstr)=0) then mstr:=mstr+'..';
          end
        else chain:=0;
        end;
      end;
  until p=0;
end;


procedure showmain(nr:shortint);
var i      : integer;
    s      : string[20];
begin
  if mainmenu=nil then begin
    new(mainmenu);
    splitmenu(0,mainmenu,main_n);
    end;
  gotoxy(2,1);
  for i:=1 to main_n do
    with mainmenu^[i] do begin
      hmpos[i]:=wherex+1;
      if enabled then begin
        if nr=i then attrtxt(col.colmenuinv[0])
        else attrtxt(col.colmenu[0]);
        s:=mstr;
        wrt2(' ');
        if hpos>1 then
          Wrt2(LeftStr(s,hpos-1));
        if i=nr then attrtxt(col.colmenuinvhi[0])
        else attrtxt(col.colmenuhigh[0]);
        wrt2(s[hpos]);
        if i=nr then attrtxt(col.colmenuinv[0])
        else attrtxt(col.colmenu[0]);
        Wrt2(copy(s,hpos+1,20) + ' ');
        end
      else begin
        if nr=i then attrtxt(col.colmenuseldis[0])
        else attrtxt(col.colmenudis[0]);
         Wrt2(' '+mstr+' ');
        end;
      end;
end;


{ nr       : Menuenummer                                    }
{ enterkey : erster Tastendruck                            }
{ x,y      : Koordinaten fuer Untermenue-Anzeige             }
{ Return   : xxy (Hex!) : Punkt y in Menue xx wurde gewaehlt }
{             0: Menue mit Esc oder sonstwie abgebrochen    }
{            -1: Untermenue nach links verlassen            }
{            -2: Untermenue nach rechts verlassen           }

function getmenu(nr:byte; enterkey:taste; x,y:byte):integer;
const EnableUpper : boolean = true;
var ma    : map;
    n,i   : integer;
    t     : taste;
    p,ml  : byte;
    pold  : byte;
    get2  : integer;
    xx,yy : byte;
    autolr: byte;
    dead  : boolean;   { alle disabled }
    has_checker : boolean;
    mausback : boolean;

  procedure display;
  var i,hp  : byte;
      s     : string[40];
      check : char;
  begin
    if nr=0 then showmain(p)
    else begin
      for i:=1 to n do begin
        s:=ma^[i].mstr;
        hp:=ma^[i].hpos;
        if (i<>p) or dead then
          if ma^[i].enabled then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenudis[menulevel])
        else
          if ma^[i].enabled then attrtxt(col.colmenuinv[menulevel])
          else attrtxt(col.colmenuseldis[menulevel]);
        check:=' ';
        if s='-' then
          wrt(x,y+i,'√'+dup(ml,'ƒ')+'¥')
        else if hp=0 then
          wrt(x+1,y+i,check+forms(s,ml-1))
        else if not ma^[i].enabled then
          wrt(x+1,y+i,' '+forms(s,ml-1))
        else begin
          wrt(x+1,y+i,check+LeftStr(s,hp-1));
          if i<>p then attrtxt(col.colmenuhigh[menulevel])
          else attrtxt(col.colmenuinvhi[menulevel]);
          wrt2(s[hp]);
          if i<>p then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenuinv[menulevel]);
          wrt2(forms(copy(s,hp+1,40),ml-hp-1));
          end;
        end;
      end;
  end;

  function nomp(p:byte):boolean;
  begin
    nomp:=(ma^[p].mstr='-'){ or ((nr=0) and not ma^[p].enabled)};
  end;

  procedure DoEnable;
  begin
    if not special(ma^[p].mpnr) and ishidden(ma^[p].mpnr) then begin
      if nr=0 then mainmenu^[p].enabled:=true;
      ma^[p].enabled:=true;
      DelHidden(ma^[p].mpnr);
      display;
      modi:=true;
      {$IFDEF usesyslog}
      XPInfoLog('Entry '''+ma^[p].mstr+''' enabled');
      {$ENDIF }
      end;
  end;

  procedure DoDisable;
  begin
    if not special(ma^[p].mpnr) and not ishidden(ma^[p].mpnr) then begin
      if nr=0 then mainmenu^[p].enabled:=false;
      ma^[p].enabled:=false;
      AddHidden(ma^[p].mpnr);
      display;
      modi:=true;
      {$IFDEF usesyslog}
      XPInfoLog('Entry '''+ma^[p].mstr+''' disabled');
      {$ENDIF }
      end;
  end;

begin
  if nr=0 then menulevel:=0;
  new(ma);
  splitmenu(nr,ma,n);
  has_checker:=false;
  p:=min(menupos[nr],n);
  i:=1;
  while nomp(p) and (i<=n) do begin
    p:=p mod n + 1; inc(i);
    end;
  dead:=i>n;
  autolr:=0;
  if nr>0 then begin
    ml:=0;
    for i:=1 to n do
      ml:=max(ml,length(ma^[i].mstr));
    inc(ml,2);
    x:=min(x,78-ml);
    attrtxt(col.colmenu[menulevel]);
    forcecolor:=true;
    wpushs(x,x+ml+1,y,y+n+1,'');
    forcecolor:=false;
    end
  else
    if (nr=0) and (enterkey<>keyf10) then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UpperCase(enterkey)) do inc(i);
      if i<=n then begin
        p:=i;
        autolr:=1;
        end;
      end;

  mausback:=false;
  pold:=99;
  repeat
    if p<>pold then display;
    pold:=p;
    case autolr of
      4 : begin {t:=mausleft;} autolr:=0; end;
      3 : begin t:=keyrght; autolr:=1; end;
      2 : begin t:=keyleft; autolr:=1; end;
      1 : begin t:=keycr; autolr:=0; end;
    else
      if hcursor then begin
        if nr=0 then gotoxy(hmpos[p]-1,1)
        else gotoxy(x+1,y+p);
        get(t,curon);
        end
      else
        get(t,curoff);
    end;
    if not dead then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UpperCase(t)) do inc(i);
      if (i<=n) and (ma^[i].enabled) then begin
        p:=i; t:=keycr;
        display;
        end
      else begin
        if t=keyhome then begin
          p:=1;
          if nomp(p)  then t:=keytab;
          end;
        if t=keyend then begin
          p:=n;
          if nomp(p) then t:=keystab;
          end;
        if ((nr=0) and (t=keyrght)) or ((nr>0) and (t=keydown)) or
           (t=keytab) or (not has_checker and (t=' ')) then
            repeat
              p:=(p mod n)+1
            until not nomp(p);
        if ((nr=0) and (t=keyleft)) or
           ((nr>0) and (t=keyup)) or (t=keystab) then
             repeat
               if p=1 then p:=n else dec(p)
             until not nomp(p);
        end;

      if nr=0 then begin
        if t=keyf10 then t:=keyesc;
        { In der Menuezeile oeffnet Cursor Down das Menue }
        if t=keydown then t:=keycr;
      end;

      if t='+' then DoEnable;
      if t='-' then DoDisable;

      get2:=0;
      if t=keycr then
        if ma^[p].enabled then
          if ma^[p].chain>0 then begin
            if nr=0 then begin
              xx:=hmpos[p]-1; yy:=2; end
            else begin
              xx:=x+2; yy:=y+1+p; end;
            menupos[nr]:=p;
            inc(menulevel);
            get2:=getmenu(ma^[p].chain,'',xx,yy);
            dec(menulevel);
            if EnableUpper then DoEnable
            else DoDisable;
            case get2 of
              0  : {if nr>0 then} t:='';
             -1  : if nr>0 then t:=keyleft
                   else begin
                     autolr:=2; t:=''; end;
             -2  : if nr>0 then t:=keyrght
                   else begin
                     autolr:=3; t:=''; end;
             -3  : begin autolr:=4; t:=''; end;
            end  { case }
          end
        else
          t:=''
      else  { not enabled }
        t:='';

      if (ma^[p].keep) and (get2>0) then
        t:='';

      end;   { not dead }

  until (t=keyesc) or ((nr>0) and ((t=keyleft) or (t=keyrght)));

  if nr>0 then wpop
  else showmain(0);
  menupos[nr]:=p;

  if nr>0 then begin
    i:=1;
    while (i<=n) and (not ma^[i].enabled or (ma^[i].mstr='-')) do
      inc(i);
    EnableUpper:=(i<=n);
    end;

  Dispose(ma);

  if t=keyesc then getmenu:=0
  else if t=keycr then getmenu:=get2
  else if t=keyleft then getmenu:=-1
  else if mausback then getmenu:=-3
  else getmenu:=-2;
end;


{ --- Daten laden/speichern ---------------------------------- }

procedure rdjn(var c:char; default:char);
var t : taste;
begin
  c:=default;
  repeat
    write(c,#8);
    get(t,curon);
    if (t='j') or (t='J') or (t='n') or (t='N') then
      c:=UpCase(t[1]);
  until (t=keycr) or (t=keyesc);
end;


procedure readmedata;
var f       : file of integer;
    version : integer;
    c       : char;
    i       : integer;
begin
  anzhidden:=0;
  assign(f,menufile);
  filemode:=$40;
  if existf(f) then begin
    reset(f);
    read(f,version);
    if version<>meversion then begin
      wrlogo;
      writeln('WARNUNG: XPME kann das Format der MenÅdatei (',menufile,') nicht');
      writeln('         erkennen. Die Datei wurde entweder mit einer neueren');
      writeln('         Version des MenÅeditors erstellt oder ist beschÑdigt.');
      writeln('         Wenn Sie jetzt fortfahren, wird diese Datei gelîscht,');
      writeln('         d.h. eventuelle Informationen Åber (de)aktivierte');
      writeln('         MenÅpunkte gehen verloren.');
      writeln;
      write('Fortfahren (J/N)? '#7);
      rdjn(c,'N');
      writeln;
      if c='N' then halt;
      end
    else begin
      read(f,anzhidden);
      anzhidden:=minmax(anzhidden,0,min(maxhidden,filesize(f)-2));
      for i:=1 to anzhidden do
        read(f,hidden[i]);
      c:='N';
      end;
    close(f);
     if c='J' then system.erase(f);
    end;
end;


procedure writemdata;
var f : file of integer;
    i : integer;
begin
  assign(f,menufile);
  filemode:=2;
  rewrite(f);
  i:=meversion;
  write(f,i);
  write(f,anzhidden);
  for i:=1 to anzhidden do
    write(f,hidden[i]);
  close(f);
  saved:=true;
end;


function askquit:boolean;
var x,y : byte;
    t   : taste;
begin
  msgbox(34,4,'',x,y);
  wrt(x+3,y+1,'énderungen sichern?');
  t:='';
 case readbutton(x+3,y+3,2,' ^Ja , ^Nein , ^ZurÅck ',1,true,t) of
    1 : begin
          writemdata;
          askquit:=true;
        end;
    2 : askquit:=true;
    else askquit:=false;
  end;
  closebox;
end;

procedure StartXPME;
begin
  readmenus;
  readconfig;
  readcol;
  readmedata;
  showscreen;
  repeat
    if getmenu(0,'',0,0)=0 then;
  until not modi or AskQuit;
  attrtxt(7);
  clrscr;
  wrlogo;
  if saved then begin
    writeln('énderungen wurden gesichert.'#10);
    {$IFDEF usesyslog}
    XPNoticeLog('Changes saved');
    {$ENDIF }
  end;
end;

procedure StartCommandLineXPME;
begin
  StartXPME;
  //more? halt?
end;

{
  $Log$
  Revision 1.41  2002/12/09 14:37:22  dodi
  - merged include files, updated comments

  Revision 1.40  2002/12/02 14:04:30  dodi
  made xpmenu internal tool

  Revision 1.39  2002/11/30 04:40:28  mk
  - made xpme to a unit and compilable

  Revision 1.38  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.37  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.36  2001/08/11 23:06:38  mk
  - changed Pos() to cPos() when possible

  Revision 1.35  2001/08/01 09:06:24  cl
  - renamed openxp.res to openxp.rsp

  Revision 1.34  2001/07/28 12:04:16  mk
  - removed crt unit as much as possible

  Revision 1.33  2001/04/15 19:33:34  ma
  - adjusted resource file names

  Revision 1.32  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.31  2000/11/18 00:04:43  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.30  2000/11/14 22:35:05  fe
  Replaced "exist()" by "fileexists()".

  Revision 1.29  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.28  2000/10/29 16:18:25  fe
  Mit VPC uebersetzbar gemacht.

  Revision 1.27  2000/10/17 10:06:00  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.26  2000/09/09 15:41:28  hd
  - Fix: GetScreen* -> SysGetScreen*

  Revision 1.25  2000/09/08 16:12:06  hd
  - Init-Reihenfolge

  Revision 1.24  2000/07/20 16:50:00  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.23  2000/07/13 10:23:48  mk
  - Zeiger auf Strings entfernt

  Revision 1.22  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.21  2000/07/04 09:59:04  mk
  - Sysutils eingefuegt

  Revision 1.20  2000/05/14 17:22:51  hd
  - Linux: Manuelle Init. der XPCurses

  Revision 1.19  2000/05/13 10:09:26  mk
  - Aufruf fuer SetBackIntensity angepasst

  Revision 1.18  2000/05/06 17:14:22  hd
  - Rahmen angepasst

  Revision 1.17  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.16  2000/05/06 11:25:13  hd
  - Kleinere Aenderungen fuer Linux:
    - Hintergrund nutzt gesamten Screen (nicht nur 80x25)
    - Textfenster unten rechts, je nach Groesse des Screen
    - Simple Log-Ausgabe nach syslog

  Revision 1.15  2000/05/02 14:38:35  hd
  Laeuft jetzt unter Linux. String-Konvertierung wird in XPCURSES.PAS
  vorgenommen, so dass alle StrDosToLinux-Aufrufe entfernt wurden.
  Die Konvertierungsroutine beruecksichtigt auch Ausgaben via Write
  und WriteLn.

  Revision 1.14  2000/04/29 16:13:29  hd
  Linux-Anpassung

  Revision 1.13  2000/04/15 10:58:32  mk
  - 1001x .DOC in .TXT geandert

  Revision 1.12  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/04/09 13:27:07  ml
  Diverse -nderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.10  2000/03/27 16:34:23  ml
  lauffShig unter linux

  Revision 1.9  2000/03/24 20:38:12  mk
  - xdelay entfernt

  Revision 1.8  2000/03/04 22:41:37  mk
  LocalScreen fuer xpme komplett implementiert

  Revision 1.7  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.3  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


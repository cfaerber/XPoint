{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

const colcfgfile = 'xpoint.col';

type  ColArr = array[0..3] of byte;
      ColRec = record
                  ColMenu       : ColArr; { Normaler Mentext       }
                  ColMenuHigh   : ColArr; { Direkt-Buchstaben       }
                  ColMenuInv    : ColArr; { Men-Balken             }
                  ColMenuInvHi  : ColArr; { Men-Balken/Buchstabe   }
                  ColMenuDis    : ColArr; { Men disabled           }
                  ColMenuSelDis : ColArr; { Men disabled/gew„hlt   }
                  ColKeys       : byte;   { Direkttasten            }
                  ColKeysHigh   : byte;   { Direkttasten-Buchstaben }
                  ColKeysAct    : byte;   { aktivierte Taste        }
                  ColKeysActHi  : byte;   { aktivierter Buchstabe   }
                  ColTLine      : byte;   { Trennlinie              }
                  ColBretter    : byte;   { User / Bretter          }
                  ColBretterInv : byte;   { User / Bretter, gew„hlt }
                  ColBretterHi  : byte;   { User / Bretter, markiert}
                  ColBretterTr  : byte;   { Trennzeile              }
                  ColMsgs       : byte;   { Msgs                    }
                  ColMsgsHigh   : byte;   { Msgs, markiert          }
                  ColMsgsInv    : byte;   { Msgs, gew„hlt           }
                  ColMsgsInfo   : byte;   { Msgs, 1. Zeile          }
                  ColMsgsUser   : byte;   { PM-archivierte Msgs     }
                  ColMsgsInvUser: byte;   { gew„hlt+hervorgehoben   }
                  ColMbox       : byte;   { Meldungs-Box, Text      }
                  ColMboxRahmen : byte;   { Meldungs-Box, Rahmen    }
                  ColMboxHigh   : byte;   { Meldungs-Box, hervorgeh.}
                  ColDialog     : byte;   { Dialoge, Feldnamen u.„. }
                  ColDiaRahmen  : byte;   { Dialogbox, Rahmen       }
                  ColDiaHigh    : byte;   { Dialogbox, hervorgeh.T. }
                  ColDiaInp     : byte;   { Dialogbox, Eingabefeld  }
                  ColDiaMarked  : byte;   { Dial., markierter Text  }
                  ColDiaArrows  : byte;   { Pfeile bei Scrollfeldern}
                  ColDiaSel     : byte;   { Masken-Auswahlliste     }
                  ColDiaSelBar  : byte;   {            "            }
                  ColDiaButtons : byte;   { Check/Radio-Buttons     }
                  ColSelbox     : byte;   { Auswahlbox              }
                  ColSelRahmen  : byte;   { Auswahlbox, Rahmen      }
                  ColSelHigh    : byte;   { Auswahlbox, hervorgeh.  }
                  ColSelBar     : byte;   { Auswahlbox, Balken      }
                  ColSel2box    : byte;   { Auswahlbox / dunkel     }
                  ColSel2Rahmen : byte;   { Auswahlbox, Rahmen      }
                  ColSel2High   : byte;   { Auswahlbox, hervorgeh.  }
                  ColSel2Bar    : byte;   { Auswahlbox, Balken      }
                  ColButton     : byte;   { Button                  }
                  ColButtonHigh : byte;   { Button - Hotkeys        }
                  ColButtonArr  : byte;   { aktiver Button: Pfeile  }
                  ColUtility    : byte;   { Kalender u.„.           }
                  ColUtiHigh    : byte;
                  ColUtiInv     : byte;
                  ColHelp       : byte;   { Hilfe normal            }
                  ColHelpHigh   : byte;   { hervorgehobener Text    }
                  ColHelpQVW    : byte;   { Querverweis             }
                  ColHelpSlQVW  : byte;   { gew„hlter Querverweis   }
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
                  ColEditMenu   : byte;   { TED: Men               }
                  ColEditMenuHi : byte;   { TED: Hotkey             }
                  ColEditMenuInv: byte;   { TED: Selbar             }
                  ColEditHiInv  : byte;   { TED: gew„hlter Hotkey   }
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
      s:=trim(copy(s,p+1,255))+' ';
      n:=0;
      repeat
        p:=cpos(' ',s);
        if p>0 then begin
          val(left(s,p-1),l,res);
          delete(s,1,p);
          while (s<>'') and (s[1]=' ') do delfirst(s);
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


{ Button-Abfrage -----------------------------------------------------}
{ x,y     : Position des linken Buttons                               }
{ abs     : Leerabstand zwischen Buttons                              }
{ buttons : '^Butt1,^Butt2...'                                        }
{ default : Startwert fr p                                           }
{ homeend : die Tasten Home/End sind zugelassen                       }
{ retkey  : '' -> Normale Abfrage. '*' -> bei jeder unbekannten Taste }
{           wird die Taste in 'retkey' und readbutton<0 zurckgegeben }
{           '!' -> nur Anzeige der Buttons, und direkt zurck         }
{ RETURN:  0 oder p bei normaler Abfrage, -p bei retkey='*' und Esc   }

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
      wrt2(left(butt[i],butthigh[i]-1));
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
  if spenter then delfirst(buttons);
  inc(byte(buttons[0]));
  buttons[length(buttons)]:=',';
  n:=0;
  repeat
    p:=pos(',',buttons);
    if p>0 then begin
      inc(n);
      if buttons[1]='ù' then begin
        i:=2; while (buttons[i]>='0') and (buttons[i]<='9') do inc(i);
        buttsp[n]:=ival(copy(buttons,2,i-2));
        buttons:=copy(buttons,i,255);
        dec(p,i-1);
        end
      else
        buttsp[n]:=0;
      butt[n]:=left(buttons,p-1);
      buttons:=copy(buttons,p+1,255);
      p:=pos('^',butt[n]);
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
{
  $Log$
  Revision 1.7  2000/07/11 21:39:23  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.6  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.5  2000/06/29 13:01:02  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.4  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.3  2000/03/04 10:42:25  mk
  Versionsinfos hinzugefuegt und weiter portiert

}

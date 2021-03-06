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

unit inout;

{$I xpdefine.inc }

{  ==================  Interface-Teil  ===================  }

interface

uses
  sysutils, //override date/time later
  xpglobal,
  typeform, 
  keys; //taste

var    lastkey   : taste = '';

       pm      : string[15] = 'Peter Mandrella';

       CapsLock   : boolean = false;
       NumLock    : boolean = false;
       ScrollLock : boolean = false;
       CapsEnable : boolean = true;
       NumEnable  : boolean = true;
       ScrollEnable:boolean = true;


const  lScrollLock = $10;    { Konstanten fuer mem[$40:$17] }
       lNumLock    = $20;
       lCapsLock   = $40;

       fndeflen = 40;
       maxalt   = 10;

       maxzaehler   = 5;
       TickFreq     = 18.206512451;


type   CurType   = (curnorm,curoff,cureinf,curnone);
       EditType  = (editread,editedit,editbreak,edittabelle);
       EndeEdTyp = (enlinks,enrechts,enoben,enunten,enreturn,enno,enabbr,
                    enctrly,enctrln,enctrld,enchome,encend,enpgdn);

       slcttyp  = record
                    el : string;                 { Auswahl-Position         }
                    zu : boolean;                { zugelassen ?             }
                    nu : longint;                { Benutzer                 }
                  end;
       slcta    = array[1..500] of slcttyp;
       pntslcta = ^slcta;
       testproc = procedure(var s:string; var ok:boolean);
       editproc = procedure(x,y:byte; var s:string; var p:shortint;
                            var en:endeedtyp);
                  { p=-1 -> nur anzeigen }
       nproc    = procedure;
       edits    = record
                    x,y,px,
                    len,art : shortint;
                    s       : string;
                    tproc   : testproc;
                    edproc  : editproc;
                  end;

var    fchar      : char     = '_';       { "Leerzeichen" bei ReadEd.      }
       rdedch     : taste    = '';        { ReadEdit Vorgabe f. 1. Zeichen }
       rdedactive : boolean  = false;     { ReadEdit aktiv                 }
       m2t        : boolean  = false;     { Zeitanzeige ueber multi2        }
       canf       : boolean  = true;      { Cursor bei Readedit an Anfang  }
       enlinksre  : boolean  = true;      { ReadEdit enlinks & enrechts    }
       rdedtrunc  : boolean  = true;      { Leerzeichem am Ende wegschneiden }
       esfx       : shortint = 8;         { X-Pos. fuer editsf              }
       esfy       : shortint = 9;         { Y-Pos. fuer editsf              }
       esfch      : char     = '>';       { Prompt fuer editsf              }
       curon      : curtype  = curnorm;   { Cursorform bei angesch. Cursor }
       lastcur    : curtype  = curoff;    { letzte Cursorform              }
       edm_str    : String   = 'Monat: '; { Prompt-Text bei edmonth        }
       hotkeys    : boolean  = true;      { Hotkeys aktiviert              }
       hotkey_f1  : boolean  = false;     { F1 aktiv (wenn hotkeys==false) }
       retonfn    : taste    = '';        { liefert Get bei FN-Taste zur.  }
       readendeny : boolean  = false;     { RdEd-Ende mit ^N/^Y m�gl.      }
       einfueg    : boolean  = false;     { Einfuege-Mode bei RdEd          }
       readblen   : byte     = 4;         { Laenge fuer readbescue           }
       key_pressed: boolean  = false;     { Taste wurde in Get gedrueckt?   }

       mausl      : char     = #13;       { linke Maustaste                }
       mausr      : char     = #27;       { rechte Maustaste               }
       mausst     : xpWord     = 3;         { Maske fuer Maustaste            }
       mauszuo    : boolean  = true;      { Fraigabe fuer Maus oben         }
       mauszuu    : boolean  = true;      { Freigabe fuer Maus unten        }
       mauszul    : boolean  = true;      { Freigabe fuer Maus links        }
       mauszur    : boolean  = true;      { Freigabe fuer Maus rechts       }
       mausfx     : shortint = 2;         { Maus-Faktor X                  }
       mausfy     : shortint = 1;         { Maus-Faktor Y                  }

       statposx   : shortint = 0;         { X-Pos. fuer Tast.-Stat-Anzeige  }
       statposy   : shortint = 0;         { Y-Pos. fuer Tast.-Stat-Anzeige  }
       scsavetime : integer  = 0;         { Screen-Saver Reload-Count      }
       scsavecnt  : integer  = 0;         { Screen-Saver Count             }
       dphback    : byte     = 7;         { Attribut fuer DispHard          }
       normattr   : byte     = 7;         { Screen-Attrib normtxt          }
       highattr   : byte     = 15;        { Screen-Attrib hightxt          }
       invattr    : byte     = $70;       { Screen-Attrib invtxt           }
       lowattr    : byte     = 0;         { Screen-Attrib lowtxt           }
       forcecolor : boolean  = false;     { Txt-Attribute blockieren       }

       iomaus     : boolean  = true;      { wird mit mouse.maus verknuepft  }
       AutoUp     : boolean  = false;     { Get: automatisches KeyUp       }
       AutoDown   : boolean  = false;     { Get: automatisches KeyDown     }
       AutoupEnable   : boolean = true;
       AutodownEnable : boolean = true;
       AutoBremse : boolean  = false;     { eine Zeile pro Tick            }

       Int15Delay : byte     = 0;         { 1=int15, 2=int28, 3=HLT, 4=int2F }


var
       chml : array[1..5] of string;

       datex,datey,                    { Koordinaten fuer Datum und Uhrzeit }
       timex,timey  : shortint;
       fndef        : array[1..20] of string[fndeflen];
       fnproc       : array[0..3,1..10] of nproc;
       altproc      : array[1..maxalt] of record
                                            schluessel : taste;
                                            funktion   : procedure;
                                            aktiv      : boolean;
                                          end;

       scsaveadr    : procedure;       { Screen-Saver Proc (muss alle Ak-  }
                                       { tionen selbst durchfuehren)        }
       lastattr     : byte;            { aktuelles Bildschirm-Attribut     }

       multi3       : procedure;       { Hintergrund-Prozess                }
       memerror     : nproc;           { ?!                                }
       editmsp      : integer;         { aktuelle editms-Zeile             }

       color,cga    : boolean;         { Grafik-detect                     }
       zaehler      : packed array[1..maxzaehler] of longint;
       zaehlproc    : packed array[1..maxzaehler] of procedure;


procedure Disp_DT;                              { Datum/Uhrzeit anzeigen  }
procedure SetSeconds(sec,flash:boolean);        { Sekundenanzeige ein/aus }
Procedure multi2;                               { vorgeg. Backgr.-Prozess }
Procedure initscs;                              { Screen-Saver init       }

{$IFNDEF NCRT }
Procedure window(l,o,r,u: Integer);              { Statt CRT.WINDOW         }
{$ENDIF }
Procedure Cursor(t:curtype);                 { Cursorschalter setzen    }
Procedure GetCur(var a,e,x,y:integer);       { Cursorbereich abfragen   }
Procedure SaveCursor;                        { Cursor retten            }
Procedure RestCursor;                        { Cursor wiederherstellen  }
Procedure Get(var z:taste; cur:curtype);     { Taste einlesen           }
Procedure testbrk(var brk:boolean);          { Test auf ESC             }
  //todo: retain old value of "brk" if no key pressed?
Procedure waitkey(x,y:byte);                 { Taste druecken            }
Procedure HighTxt;                           { Textfarbe hell           }
Procedure InvTxt;                            { Textfarbe invers         }
Procedure LowTxt;                            { Textfarbe schwarz        }
Procedure NormTxt;                           { Textfarbe normal         }
Procedure AttrTxt(attr:byte);                { Textfarbe nach Attr.     }
Procedure JN(VAR c:Char; default:Char);      { J/N-Abfrage (Esc = Def.) }
Procedure JNEsc(VAR c:Char; default:Char; var brk:boolean);
                                             { J/N-Abfrage mit Esc      }
{$IFNDEF NCRT }
Procedure DispHard(x,y: Integer; s:string);      { String ohne beruecksicht. }
                                             { des akt. Windows ausgeb. }
{$ENDIF }
Function  CopyChr(x,y:byte):char;            { Bildschirm-Inhalt ermitt.}
procedure DosOutput;                         { auf CON: umschalten      }
function  ticker:longint;                    { mem[Seg0040:$6c]         }

{     Haupt-String-Edit-Prozedur
      x,y : Koordinaten              txt : Prompt-Text
      s   : einzulesender String     ml  : max. Laenge
      li  : erlaubte Zeichen         px  : Startposition x
      art : Edittyp (edit-read, -edit, -break, -tabelle)
      enderded : EndeEdTyp (s.o.)                           }

Procedure ReadEdit(x,y: Integer; txt: atext; var s:string; ml:Byte;
                   li:string; var px : byte; art:edittype;
                   var enderded:endeedtyp);

{     String-Einlese-Prozeduren
      x,y : Koordinaten                txt : Prompt-Text
      s   : einzulesender String       ml  : max. Laenge
      li  : erlaubte Zeichen (chml)    brk : Abbruch        }

{ mit Esc }
Procedure bd(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint;
             VAR brk:Boolean);
{ ohne Esc }
Procedure ed(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint);
{ Return zum �bernehmen }
Procedure ld(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint;
             invers:boolean; VAR brk:Boolean);
{ ohne Esc; auf '' setzen }
Procedure rd(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint);
procedure bdchar(x,y:byte; txt:string; var c:char; li:string; var brk:boolean);

Procedure readb(x,y:byte; VAR b:Byte);       { Byte-Zahl einlesen       }
Procedure readbesc(x,y:Byte; VAR b:Byte; VAR brk:Boolean);
Procedure readbescue(x,y:byte; VAR b:byte; VAR brk:boolean);
Procedure readi(x,y:byte; VAR i:Integer);    { Integer-Zahl einlesen    }
Procedure readiesc(x,y:Byte; VAR i:Integer; VAR brk:Boolean);
Procedure readiescue(x,y:byte; var i:integer; var brk:boolean);
Procedure readw(x,y:byte; VAR w:Dword);       { Word-Zahl einlesen    }
Procedure readwesc(x,y:Byte; VAR w:dword; VAR brk:Boolean);
Procedure readwescue(x,y:byte; var w:dword; var brk:boolean);
Procedure readl(x,y:byte; VAR l:LongInt);    { Integer-Zahl einlesen    }
Procedure readlesc(x,y:Byte; VAR l:LongInt; VAR brk:Boolean);
Procedure readlescue(x,y:byte; var l:LongInt; var brk:boolean);
Procedure readr(x,y:byte; VAR r:Real);       { Real-Zahl einlesen       }
Procedure readresc(x,y:byte; VAR r:Real; VAR brk:Boolean);
Procedure readrescue(x,y:byte; VAR r:Real; VAR brk:Boolean);

{ Form-Editier-Funktionen
  art <> 0 : Tabelle (beenden durch o,u,l,r)     }

Procedure edform(cx,cy:byte; VAR dt:datetimest;
                 f1,f2: datetimest; VAR art:shortint);
Procedure eddate(x,y:byte; VAR d:datetimest; VAR art:shortint);
Procedure edmonth(x,y:byte; VAR m:datetimest; VAR defm,defj:xpWord;
                   var art:shortint);
Procedure edtime(x,y:byte; VAR t:datetimest; VAR art:shortint);

{ Feld-Editier-Funktionen }

procedure editsf(liste:pntslcta; n:xpWord; var brk:boolean);
procedure dummyproc(var s:string; var ok:boolean);
procedure dummyed(x,y:byte; var s:string; var p:shortint; var en:EndeEdTyp);
procedure editms(n:integer; var feld; eoben:boolean; var brk:boolean);

Procedure mausiniti;                 { Maus nach Bildschirmmitte             }
procedure dummyFN;
{$IFNDEF NCRT }
procedure mdelay(msec:xpWord);
{$ENDIF }

procedure InitInOutUnit;

{ ================= Implementation-Teil ==================  }

implementation

uses
{$IFDEF Win32 }
  windows,
{$ENDIF  }
{$IFDEF OS2 }
  xpos2,
  doscalls,
{$ENDIF }
{$ifdef NCRT }
  xpcurses,
{$endif }
{$IFDEF Dos32 }
  crt,
{$endif }
{$ifndef NCRT }
  winxp,
{$endif}
  mouse,
  xp0,
  maus2,
  debug;

const  maxsave     = 50;  { max. fuer savecursor }

var   __st : string[8] = '  :  :  ';    { fuer M2T }
      timeflash : boolean = false;
      getactive : boolean = false;

type   editsa      = array[1..500] of edits;   { nur fuer Type Cast }

var    ca,ce       : integer;
       sx,sy,sa,se : array[1..maxsave] of integer;
       wl,wr,wo,wu : array[1..maxsave] of byte;
{$IFNDEF NCRT }
       mwl,mwo,
       mwr,mwu     : byte;
{$ENDIF }
       cursp       : shortint;
       sec         : xpWord;
       mx,my       : integer;      { Maus-Koordinaten }
       st1         : byte;
       fnpactive   : array[0..3,1..10] of boolean;
       istack      : array[1..maxalt] of byte;
       istackp     : integer;
       autolast    : longint;   { Get: Tick des letzten AutoUp/Down }

{ Bild-Speicheradresse     }
function memadr(x,y:byte):xpWord;forward;

// time critical
function ticker:longint;
begin
{  DecodeTime(Now, h, m, s, millis);
  Ticker := system.round(((longint(h*60 + m)*60 + s) * TickFreq) +
    (millis / (1000 / TickFreq))); }
  // erst ab 01.01.2006 z�hlen (38718 = EncodeDate(2006,1,1));
  // um einen �berlauf herauszuz�gern
  // 8640000 = auf hundertstel sekunden umrechnen
  Result := LongInt(Trunc(((Now-38718) * 8640000)));
end;

{ !! Diese Funktion liefert mit and $70 nur CAPSLock zurueck,
  das kann nicht sinn der Sache sein. Muss geprueft werden }
function kbstat:byte;     { lokal }
begin
  kbstat := 0;
end;

{$IFNDEF NCRT }
Procedure window(l,o,r,u:Integer);
begin
  mwl:=l; mwr:=r;
  mwo:=o; mwu:=u;
(*  crt.window(l,o,r,min(u,25));
  if (l=1) and (o=1) and (r=screenwidth) and (u=screenlines) then
    crt.windmax:=ScreenWidth-1 {crt.windmax and $ff} + 256*ScreenLines
  else
    crt.windmax:=crt.windmax and $ff + 256*(u-1); *)
end;
{$ENDIF } { NCRT }

Procedure Cursor(t:curtype);
{$IFDEF Win32 }
var
  Info: TConsoleCursorInfo;
{$ENDIF }
begin
{$IFDEF Win32 }
  case t of
    curnorm: begin
               Info.bVisible := true;
               Info.dwSize := 15;
             end;
    cureinf: begin
               Info.bVisible := true;
               Info.dwSize := 100;
             end;
    curoff:  begin
               Info.bVisible := false;
               Info.dwSize := 50;
             end;
  end;
  SetConsoleCursorInfo(Outhandle, Info);
{$ELSE }
  {$IFDEF OS2 }
    case t of
      curnorm : SysSetCurType(-85, -100, true);
      cureinf : SysSetCurType(0, -100, true);
      curoff  : SysSetCurType(-100, -100, false);
    end;
  {$ELSE }
    case t of
      curnorm : Cursoron;
      cureinf : CursorBig;
      curnone,
      curoff  : CursorOff;
    end;
  {$ENDIF }
{$ENDIF }
  lastcur:=t;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

Procedure GetCur(var a,e,x,y:integer);
begin
  x :=wherex; y:=wherey;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure SaveCursor;
begin
  inc(cursp);
  if cursp>maxsave then cursp:=1;
  getcur(sa[cursp],se[cursp],sx[cursp],sy[cursp]);
  wo[cursp]:=mwo; wu[cursp]:=mwu;
  wl[cursp]:=mwl; wr[cursp]:=mwr;
end;


procedure RestCursor;
begin
  cursor(curoff);
  window(wl[cursp],wo[cursp],wr[cursp],wu[cursp]);
  gotoxy(sx[cursp],sy[cursp]);
  dec(cursp);
  if cursp<1 then cursp:=maxsave;
end;

procedure initscs;
begin
  scsavecnt:=scsavetime;
end;


procedure SetSeconds(sec,flash:boolean);          { Sekundenanzeige ein/aus }
begin
  if sec then __st:='  :  :  '
  else __st:='  :  ';
  timeflash:=flash;
end;


procedure disp_DT;
var
  h, m, s, s100: SmallWord;
begin
{$IFNDEF Unix }
  if m2t then
  begin
    DecodeTime(now, h, m, s, s100);
    __st[1]:=chr(h div 10+48);
    __st[2]:=chr(h mod 10+48);
    __st[4]:=chr(m div 10+48);
    __st[5]:=chr(m mod 10+48);
    if length(__st)>5 then begin
      __st[7]:=chr(s div 10+48);
      __st[8]:=chr(s mod 10+48);
      end
    else
      if timeflash then __st[3]:=iifc(odd(s),':',' ');
    disphard(timex+ScreenWidth-80,timey,' '+__st+' ');
  end;
{$ENDIF }
end;

Procedure multi2;
var h,m,s,s100 : smallword;
    i          : integer16;
    l          : longint;
begin
  DecodeTime(Now,h,m,s,s100);
  if s<>sec then begin
    disp_DT;
    sec:=s;
    if getactive and (scsavecnt<>0) then begin
      dec(scsavecnt);
      if (scsavecnt=0) and (@scsaveadr<>@dummyFN) then
        scsaveadr;
      end;
      for i:=1 to maxzaehler do
        if zaehler[i]>0 then
        begin
          dec(zaehler[i]);
          Move(Zaehlproc[i],l, 4);
          if (zaehler[i]=0) and (l <> 0) then
            Zaehlproc[i];
         end;
    end;
  multi3;
end;

(*
procedure showstatus(do_rest:boolean);
const stt : array[1..3] of string[8]  = (' CAPS ',' NUM ',' SCROLL ');
      stm : array[1..3] of string[16] = ('','','');

var   x   : boolean;

  procedure stput(pos,len,nr:byte);
  begin
    moff;
    Move(mem[base:memadr(statposx+pos,statposy)],stm[nr],len*2);
    SaveCursor;
    InvTxt;
    wrt(statposx+pos,statposy,stt[nr]);
    NormTxt;
    RestCursor;
    mon;
  end;

  procedure strest(pos,len,nr:byte);
  begin
    if do_rest then
      Move(stm[nr],mem[base:memadr(statposx+pos,statposy)],2*len)
    else
      Move(mem[base:memadr(statposx+pos,statposy)],stm[nr],2*len);
  end;

begin
  if (statposx=0) or (statposy=0) then exit;

  x:=(kbstat and $40)<>0;
  if CapsEnable and (CapsLock<>x) then begin
    CapsLock:=x;
    if CapsLock then stput(0,6,1) else strest(0,6,1);
    end;
  x:=(kbstat and $20)<>0;
  if NumEnable and (NumLock<>x) then begin
    NumLock:=x;
    if NumLock then stput(6,5,2) else strest(6,5,2);
    end;
  x:=(kbstat and $10)<>0;
  if ScrollEnable and (ScrollLock<>x) then begin
    ScrollLock:=x;
    if ScrollLock then stput(11,8,3) else strest(11,8,3);
    end;

  st1:=kbstat;
end; *)


procedure dummyFN;
begin
end;


Procedure Get(VAR z:taste; cur:curtype);
VAR
    i       : byte;
{$IFNDEF Win32} {Win32 uses WaitForMultipleObjects in ReadKey}
{$IFNDEF NCRT}  {NCurses uses wgetch}
    mox,moy : integer;
{$ENDIF }
{$ENDIF }


  procedure dofunc(state,nr:byte);
  var p  : procedure;
      la : byte;
      r1 : taste;
  begin
    la:=lastattr; normtxt;
    p:=fnproc[state,nr];
    if (@p<>@dummyFN) and
       (not fnpactive[state,nr]) then begin
      fnpactive[state,nr]:=true;
      savecursor;
      r1:=retonfn; retonfn:='';
      p;
      retonfn:=r1;
      restcursor;
      fnpactive[state,nr]:=false;
      z:=retonfn;
      if z='' then z:='!!';
      end;
    attrtxt(la);
  end;

  procedure doaltfunc(i:byte);
  var p  : procedure;
      r1 : taste;
  begin
    inc(istackp);
    istack[istackp]:=i;
    p:=altproc[istack[istackp]].funktion;
    altproc[istack[istackp]].aktiv:=true;
    savecursor;
    window(1,1,screenwidth,screenlines);
    r1:=retonfn; retonfn:='';
    p;
    retonfn:=r1;
    restcursor;
    altproc[istack[istackp]].aktiv:=false;
    dec(istackp);
    z:=retonfn;
    if z='' then z:='!!';
  end;

var
  s: String;
begin
  if autoup or autodown then begin
    if AutoBremse then
      repeat until autolast<>ticker;
    if autoup and autoupenable then z:=keyup
    else if autodown and autodownenable then z:=keydown
    else z:=#0#0;
{$IFNDEF DOS32}
    autoup:=false;
    autodown:=false;
{$ENDIF}
    autolast:=ticker;
    exit;
  end;

  repeat
    cursor(cur);
    initscs;

{$IFNDEF Win32} {Win32 uses WaitForMultipleObjects in ReadKey}
{$IFNDEF NCRT}  {NCurses uses wgetch}
    if true {UseMulti2 }then
      //????get_without_sys
    begin
      repeat
        st1:=kbstat;
        mox:=mausx; moy:=mausy;
        while not keypressed and
              not (maus and iomaus and ((maust and mausst)<>0)) and not
              (maus and iomaus and ((mox-mx>=8*mausfx) or (mox<mx) or (moy-my>=8*mausfy) or (moy<my)))
              and not (kbstat<>st1) do begin
          getactive:=true;
          multi2;
          getactive:=false;
          if maus and iomaus and ((mox<=8*mausfx-1) or (mox>=640-8*mausfx) or
                       (moy<=8*mausfy-1) or (moy>=200-8*mausfx)) then begin
            mausiniti;
            mox:=mx; moy:=my;
          end
          else begin
            mox:=mausx; moy:=mausy;
          end;
          mdelay(0);
        end;
        z:=#255;
        if maus and iomaus and (maust and mausst<>0) then begin
          while maust and mausst and 1=1 do z:=mausl;
          while maust and mausst and 2=2 do z:=mausr;
          exit;
          end;
        if maus and iomaus then begin
           if (mox-mx>=8*mausfx) or (mox<mx) or (moy-my>=8*mausfy) or (moy<my) then begin
            if mox-mx>=8*mausfx then begin
              inc(mx,8*mausfx);
              if mauszur then z:=keyrght;
              end else
            if mox<mx then begin
              dec(mx,8*mausfx);
              if mauszul then z:=keyleft;
              end else
            if moy-my>=8*mausfy then begin
              inc(my,8*mausfy);
              if mauszuu then z:=keydown;
              end else
            if moy<my then begin
              dec(my,8*mausfy);
              if mauszuo then z:=keyup;
              end;
            end
          end;
        if (kbstat<>st1) and (statposx<>0) then begin
          // showstatus(true);
          z:=#0#0;
          exit;
        end;
      until (z<>#255) or keypressed;   { KEYS.keypressed! }
      key_pressed:=true;
      if not keypressed then begin
        cursor(curoff);
        exit;
        end;
      end
    else
{$ENDIF NCRT}
{$ENDIF Win32}
    begin
      multi2;
      key_pressed:=true;
    end;

    z := ReadTaste;

    cursor(curoff);
    lastkey:=z;

    if hotkeys or ((z=keyf1) and hotkey_f1) then
      if (z>=keyf1)  and (z<=keyf10)  then dofunc(0,ord(z[2])-58) else
      if (z>=keysf1) and (z<=keyaf10) then
        dofunc((ord(z[2])-74) div 10,(ord(z[2])-84)mod 10+1)
      else
        for i:=1 to maxalt do
          if (@altproc[i].funktion<>@dummyFN) and (z=altproc[i].schluessel)
            and (not altproc[i].aktiv) then
              doaltfunc(i);
  until z<>'!!';

  if Length(z) = 1 then
    s := Format(iifs(z[1]>#32, '$%X ''%s''', '$%X'), [Integer(ord(z[1])), z[1]])
  else
    s := Format(iifs(z[2]>#32, '$%X $%X ''%s''', '$%X $%X'), [Integer(ord(z[1])), Integer(ord(z[2])), z[2]]);
  Debug.DebugLog('inout',
    Format('Get: '+iifs(length(z)=2,'#%d#%d',
                   iifs(z[1]>#32,'#%d (%2:s)','#%d')),
    [Integer(ord(z[1])),Integer(ord(z[2])),z[1]]), dlTrace);
end;

Procedure testbrk(var brk:boolean);
begin
  brk := ReadBreak;
end;


Procedure AttrTxt(attr:byte);
begin
  if forcecolor then exit;
{$IFDEF NCRT }
  SetTextAttr(attr);
  lastattr:= attr;
{$ELSE }
  textcolor(attr and $8f);
  textbackground((attr and $7f) shr 4);
  lastattr:=attr;
{$ENDIF }
end;


Procedure InvTxt;
begin
  AttrTxt(invattr);
end;


Procedure NormTxt;
begin
  AttrTxt(normattr);
end;


Procedure LowTxt;
begin
  AttrTxt(LowAttr);
end;


Procedure HighTxt;
begin
  AttrTxt(HighAttr);
end;

{$IFNDEF NCRT }
Procedure disphard(x,y: Integer; s:string);
var
    TempAttr: xpWord;
begin
  TempAttr := TextAttr;
  TextAttr := dphback;
  moff;
  FWrt(x, y, s);
  TextAttr := TempAttr;
  mon;
end;
{$ENDIF }

procedure jnread(var c:char);     { lokal }
var t : taste;
begin
  moff;
  Write(' (J/N) ? ',UpCase(c),#8);
  mon;
  repeat
    get(t,curon);
    c:=UpCase(t[1]);
  until c IN ['J','N',#13,#27];
  if (c<>#27) and (c<>#13) then begin
    moff; Wrt2(c); mon; end;
end;


Procedure JN(VAR c:Char; default:Char);
begin
  c:=default;
  jnread(c);
  if (c=#13) or (c=#27) then c:=default;
end;


Procedure JNEsc(VAR c:Char; default:Char; var brk:boolean);
begin
  c:=default;
  jnread(c);
  if c=#13 then c:=default;
  brk:=(c=#27);
end;


{ li = '>>...' : automatische Grossschreibung }

Procedure ReadEdit(x,y: Integer; txt: atext; VAR s:string; ml:Byte;
                   li:string; VAR px : byte; art:edittype;
                   VAR enderded:endeedtyp);

const trennz  = [' ','&','('..'/',':'..'?','['..'`','{'..#127];

VAR   p: integer;
      fnkn  : shortint;
      a       : taste;
      inss    : string;
      ste     : shortstring;
      mlm,mrm : boolean;
      r1      : taste;
      autogr  : boolean;

begin
  rdedactive:=true;
  mlm:=mauszul; mrm:=mauszur;
  r1:=retonfn; retonfn:=#1#1;
  enderded:=enno;
  if LeftStr(li,2)='>>' then begin
    autogr:=true;
    delete(li,1,2);
    end
  else autogr:=false;
  mwrt(x,y,txt);
  x:=x+length(txt);
  IF art=editread then s:='' ELSE s:=Copy(s,1,ml);
  if art<>edittabelle then
    s := TrimRight(s);
  p:=min(px,length(s));
  if not canf then
    IF art<>edittabelle THEN p:=p+length(s);
  REPEAT
    if readendeny then s:=forms(s,ml);
    if einfueg then
      curon:=cureinf
    else
      curon:=curnorm;
    mwrt(x,y,s+dup(ml-length(s),fchar));
    GotoXY(x+p,y);
    mauszul:=(p>0);
    mauszur:=(p<length(s)-1);
    if rdedch='' then
      Get(a,curon)
    else begin
      a:=rdedch; rdedch:='';
      end;
    IF (a=',') and (li=chml[3]) THEN a:='.';
    IF a=keybs   THEN begin
                   IF p>0 THEN begin
                     delete(s,p,1);
                     p:=pred(p);
                     end
                   ELSE
                     IF art=edittabelle THEN enderded:=enlinks
                   end;
    IF a=keyleft THEN begin
                   IF p>0 THEN p:=pred(p) ELSE
                   IF (art=edittabelle) and enlinksre THEN enderded:=enlinks
                   end ELSE
    IF a=keydel  THEN begin
                   if p<length(s) then delete(s,succ(p),1)
                   end ELSE
    if a=keyctt  then begin
                   if p<length(s) then begin
                     if s[succ(p)]=' ' then
                       while (s[succ(p)]=' ') and (p<length(s)) do
                         delete(s,p+1,1)
                     else
                     if s[succ(p)] in trennz then
                       delete(s,p+1,1)
                     else begin
                       while (not (s[succ(p)] in trennz)) and (p<length(s)) do
                         delete(s,p+1,1);
                       if p<length(s) then begin
                         if s[succ(p)]=' ' then
                           while (s[succ(p)]=' ') and (p<length(s)) do
                             delete(s,p+1,1)
                         else
                         if s[succ(p)] in trennz then
                           delete(s,p+1,1);
                         end;
                       end;
                     end;
                   end else
    IF a=keyrght THEN begin
                   IF p<length(s) THEN p:=succ(p) ELSE
                   if (art=edittabelle) and enlinksre then enderded:=enrechts;
                   IF (p=ml) AND (art=edittabelle) THEN enderded:=enrechts;
                   end ELSE
    IF a=keyclft THEN begin
                        if (p=0) then begin
                          if art=edittabelle then enderded:=enlinks;
                          end
                        else
                          repeat
                            dec(p);
                          until (p=0) or ((s[p+1]<>' ') and (s[p]=' '));
                      end ELSE
    IF a=keycrgt THEN begin
                        if (p=length(s)) then begin
                          if art=edittabelle then enderded:=enrechts;
                          end
                        else begin
                          repeat
                            inc(p);
                          until (p=length(s)) or ((s[p]=' ') and (s[p+1]<>' '));
                          IF (p=ml) AND (art=edittabelle) THEN enderded:=enrechts;
                          end;
                        end ELSE
    IF a=keyhome THEN p:=0 ELSE
    IF a=keyend  THEN begin
                        if (art=edittabelle) and (fchar=' ') then begin
                          p:=length(s);
                          while (p>0) and (s[p]=' ') do
                            dec(p);
                          end
                        else p:=length(s);
                      end ELSE
    IF a=keyesc  THEN begin
                   if art<>editread then enderded:=enabbr;
                   end ELSE
    IF a=keyup   THEN begin
                   IF art=edittabelle THEN enderded:=enoben;
                   end ELSE
    IF a=keydown THEN begin
                   IF art=edittabelle THEN enderded:=enunten;
                   end ELSE
    if (a=keypgdn) or (a=#10) then begin
                   if art=edittabelle then enderded:=enpgdn;
                   end else
    if a=keyins  then
                   einfueg:=not einfueg
                   else
    if ((a>=keyf1) and (a<=keyf10)) or
       ((a>=keysf1) and (a<=keysf10)) then begin
                   if a<=keyf10 then
                     fnkn:=ord(a[2])-58
                   else
                     fnkn:=ord(a[2])-73;
                   if fndef[fnkn]<>'' then begin
                     if LastChar(fndef[fnkn])=';' then
                       inss:=LeftStr(fndef[fnkn],length(fndef[fnkn])-1)
                     else
                       inss:=fndef[fnkn];
                     if einfueg then
                       s:=LeftStr(LeftStr(s,p)+inss+mid(s,succ(p)),ml)
                     else
                       s:=LeftStr(LeftStr(s,p)+
                               inss+mid(s,succ(p)+length(inss)),ml);
                     p:=min(p+length(inss),length(s));
                     if LastChar(fndef[fnkn])=';' then
                       a:=keycr;
                     end;
                 end else
    begin
      ste:=s;
      // das ist eine grosse Schweinerei, aber man muesste die ganze
      // Routine umschreiben, um das wieder gerade zubiegen
      ste[succ(p)]:=' ';
      if autogr then a:=UpperCase(a);
      IF (POS(a,li)>0) AND (p<ml) AND
         (NOT ((li=chml[2]) AND (p>0) AND (a='-'))) AND
         (NOT ((li=chml[2]) AND (cPos('.',ste)>0) AND (a='.'))) THEN begin
         p:=succ(p);
         if einfueg then begin
           ste:=s;
           insert(a,ste,p);
           s:=copy(ste,1,ml);
           end
         else
           if p>length(s) then
             s:=s+a
           else
             s[p]:=a[1];
         IF (p=ml) AND (art=edittabelle) THEN enderded:=enrechts;
         end;
       end;
    if (a=keyctn) and readendeny then enderded:=enctrln;
    if (a=keycty) then
      if readendeny then enderded:=enctrly
      else begin
        s:=''; p:=0;
        end;
    if (a=^D) and readendeny then enderded:=enctrld;
    if ((a=keychom) or (a=keypgup)) and readendeny then enderded:=enchome;
    if ((a=keycend) or (a=keypgdn)) and readendeny then enderded:=encend;
  UNTIL (a=keycr) OR (enderded<>enno);
  IF art=edittabelle THEN
    IF a=keycr THEN px:=0 ELSE px:=p;
  IF a=keycr THEN enderded:=enreturn;
  mwrt(x,y,s+sp(ml-length(s)));
  if (art<>edittabelle) and rdedtrunc then
    s := TrimRight(s);
  curon:=curnorm;
  retonfn:=r1;
  mauszul:=mlm; mauszur:=mrm;
  rdedactive:=false;
end;


Procedure ed(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint);

VAR dummy:endeedtyp;
    px   :byte;

begin
  px:=0;
  ReadEdit(x,y,txt,s,ml,chml[li],px,editedit,dummy);
end;


Procedure rd(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint);

VAR dummy:endeedtyp;
    px   :byte;

begin
  px:=0;
  ReadEdit(x,y,txt,s,ml,chml[li],px,editread,dummy);
end;


Procedure bd(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint;
             VAR brk:Boolean);

VAR px      : byte;
    abbruch : endeedtyp;

begin
  px:=0;
  ReadEdit(x,y,txt,s,ml,chml[li],px,editbreak,abbruch);
  IF abbruch=enabbr THEN brk:=True ELSE brk:=False;
end;


Procedure ld(x,y:byte; txt:string; VAR s:string; ml:Byte; li:shortint;
             invers:boolean; VAR brk:Boolean);

VAR
    a      : taste;
    la  : byte;

begin
  brk:=false;
  moff;
  wrt(x,y,txt);
  la:=lastattr;
  if invers then InvTxt; Wrt2(s);
  if invers then AttrTxt(la);
  Wrt2(dup(ml-length(s), fchar));
  mon;
  gotoxy(x+length(txt),y);
  Get(a,curon);
  IF a<>keycr THEN begin
    IF a=keyesc THEN brk:=True ELSE begin
      rdedch:=a;
      if pos(a,chml[li])>0 then s:='';
      bd(x,y,txt,s,ml,li,brk);
      end;
    end
  ELSE
    mwrt(x,y,txt+s+sp(ml-length(s)));
end;


Procedure readi(x,y:byte; VAR i:Integer);

VAR res: Integer;
    s  : string;

begin
  repeat
    rd(x,y,'',s,5,3);
    if s='' then
      res:=0
    else
      val(trim(s),i,res);
  until res=0;
end;


Procedure readiesc(x,y:byte; VAR i:Integer; VAR brk:boolean);

VAR res: Integer;
    s  : string;

begin
  repeat
    s:='';
    bd(x,y,'',s,5,3,brk);
    if s='' then
      res:=0
    else
      val(trim(s),i,res);
  until res=0;
end;


Procedure readiescue(x,y:byte; VAR i:Integer; VAR brk:boolean);

VAR res: Integer;
    s  : string;

begin
  repeat
    str(i,s);
    bd(x,y,'',s,5,3,brk); if brk then exit;
    val(trim(s),i,res);
  until res=0;
end;


Procedure readw(x,y:byte; VAR w:Dword);
begin
  readi(x,y,integer(w));
end;


Procedure readwesc(x,y:Byte; VAR w:dword; VAR brk:Boolean);
begin
  readiesc(x,y,integer(w),brk);
end;


Procedure readwescue(x,y:byte; var w:dword; var brk:boolean);
begin
  readiescue(x,y,integer(w),brk);
end;


Procedure readl(x,y:byte; VAR l:LongInt);

VAR res: Integer;
    s  : string;

begin
  repeat
    rd(x,y,'',s,9,3);
    if s='' then
      res:=0
    else
      val(trim(s),l,res);
  until res=0;
end;


Procedure readlesc(x,y:byte; VAR l:LongInt; VAR brk:boolean);

VAR res: Integer;
    s  : string;

begin
  repeat
    s:='';
    bd(x,y,'',s,5,3,brk);
    if s='' then
      res:=0
    else
      val(trim(s),l,res);
  until res=0;
end;


Procedure readlescue(x,y:byte; VAR l:LongInt; VAR brk:boolean);

VAR res: Integer;
    s  : string;

begin
  repeat
    str(l,s);
    bd(x,y,'',s,5,3,brk); if brk then exit;
    val(trim(s),l,res);
  until res=0;
end;


Procedure readb(x,y:byte; VAR b:byte);

var i : integer;

begin
  i:=b;
  repeat
    readi(x,y,i);
  until (i>=0) and (i<=255);
  b:=i;
end;


Procedure readbesc(x,y:Byte; VAR b:Byte; VAR brk:Boolean);

var i : integer;

begin
  i:=b;
  repeat
    readiesc(x,y,i,brk);
    if brk then exit;
  until (i>=0) and (i<=255);
  b:=i;
end;


Procedure readbescue(x,y:byte; VAR b:byte; VAR brk:boolean);

VAR res,i: Integer;
    s    : string;

begin
  repeat
    str(b,s);
    bd(x,y,'',s,readblen,3,brk); if brk then exit;
    val(trim(s),i,res);
  until (res=0) and (i<=255);
  b:=i;
end;


Procedure readr(x,y:byte; VAR r:Real);

VAR res:Integer;
    s  :string;

begin
  repeat
    rd(x,y,'',s,9,2);
    if s='' then
      res:=0
    else
      val(trim(s),r,res);
  until res=0;
end;


Procedure readresc(x,y:byte; VAR r:Real; VAR brk:boolean);

VAR res:Integer;
    s  :string;

begin
  repeat
    s:='';
    bd(x,y,'',s,9,2,brk);
    if s='' then
      res:=0
    else
      val(trim(s),r,res);
  until res=0;
end;


Procedure readrescue(x,y:byte; VAR r:Real; VAR brk:boolean);

VAR res:Integer;
    s  :string;

begin
  repeat
    s:=strsr(r,5);
    while LastChar(s)='0' do
      DeleteLastChar(s);
    if LastChar(s)='.' then DeleteLastChar(s);
    while FirstChar(s) =' ' do
      DeleteFirstChar(s);
    bd(x,y,'',s,9,2,brk);
    val(trim(s),r,res);
  until res=0;
end;


Procedure edform(cx,cy:byte; VAR dt:datetimest;
                 f1,f2: datetimest; VAR art:shortint);

VAR min1,min2,min3,max1,
    max2,max3               : String[4];
    endeed,ml,l,p,minpos,rl : Integer;
    a                       : taste;
    trz                     : Char;
    x                       : datetimest;

begin
  trz:=f1[3];
  if trz='/' then minpos:=3 else minpos:=0;
  rl:=length(f1)-minpos;
  ml:=length(f2);
  if trz=':' then begin
    dec(rl,3); dec(ml,3); end;
  l:=ml-6;
  min1:=Copy(f1,1,2); max1:=Copy(f2,1,2);
  min2:=Copy(f1,4,2); max2:=Copy(f2,4,2);
  min3:=Copy(f1,7,l); max3:=Copy(f2,7,l);
  p:=minpos; endeed:=0;
  REPEAT
    moff;
    GotoXY(cx,cy); Wrt2(copy(dt,succ(minpos),rl)); GotoXY(cx+p-minpos,cy);
    mon;
    Get(a,curon);
    IF a=keyesc THEN
      endeed:=-1
    ELSE IF a=keyup THEN begin
      IF art>0 THEN endeed:=1;
      end
    ELSE IF a=keydown THEN begin
      IF art>0 THEN endeed:=2;
      end
    ELSE IF (a=keyleft) OR (a=keybs) THEN
      IF p>minpos THEN
        IF f1[p]=trz THEN p:=p-2 ELSE p:=pred(p)
      ELSE begin
        IF art>0 THEN endeed:=3;
        end
    ELSE IF a=keyrght THEN
      IF p<ml THEN
        IF (f1[p+2]=trz) and (p<pred(ml)) THEN p:=p+2 ELSE p:=succ(p)
      ELSE begin
        IF art>0 THEN endeed:=4;
        end
    ELSE IF a=keycr THEN
      endeed:=5  { 2 ? }
    ELSE IF a=keyhome THEN
      p:=minpos
    ELSE IF a=keyend THEN
      p:=ml
    ELSE if (a=keypgdn) and (art>0) then
      endeed:=6
    ELSE IF a[1]=trz THEN begin
      IF p<3 THEN p:=3 ELSE IF (p<6) and (6<ml) THEN p:=6;
      end
    ELSE begin
      IF (Pos(a,'0123456789')>0) AND (p<ml) THEN begin
        x:=dt;
        x[succ(p)]:=a[1];
        IF (x[1]>=f1[1]) AND (x[1]<=f2[1]) AND (x[4]>=f1[4]) AND
           (x[4]<=f2[4]) AND (x[7]>=f1[7]) AND (x[7]<=f2[7]) THEN begin
          IF Copy(x,1,2)<min1 THEN begin x[1]:=min1[1]; x[2]:=min1[2]; end;
          IF Copy(x,4,2)<min2 THEN begin x[4]:=min2[1]; x[5]:=min2[2]; end;
          IF Copy(x,7,l)<min3 THEN x:=Copy(x,1,6)+min3;
          IF Copy(x,1,2)>max1 THEN x[2]:='0';
          IF Copy(x,4,2)>max2 THEN x[5]:='0';
          IF Copy(x,7,l)>max3 THEN x:=Copy(x,1,7)+dup(pred(l),'0');
          dt:=x;
          IF (f1[p+2]=trz) and (p<pred(ml)) THEN p:=p+2 ELSE p:=succ(p);
          end;
        end;
      IF p>ml THEN endeed:=4;
    end;
  UNTIL endeed<>0;
  mwrt(cx,cy,copy(dt,succ(minpos),rl));
  IF art<>0 THEN art:=endeed ELSE
    IF endeed=-1 THEN art:=-1 ELSE art:=0;
end;


Procedure eddate(x,y:byte; VAR d:datetimest; VAR art:shortint);

begin
  IF d='' THEN d:=date;
  edform(x,y,d,'01.01.1000','31.12.2199',art);
end;



Procedure edtime(x,y:byte; VAR t:datetimest; VAR art:shortint);

begin
  IF t='' THEN t:=time;
  edform(x,y,t,'00:00:00','24:59:59',art);
end;


Procedure edmonth(x,y:byte; VAR m:datetimest; VAR defm,defj:xpWord;
                  var art:shortint);

begin
  mwrt(x,y,edm_str);
  art:=0;
  m:='02/'+formi(defm,2)+'/'+formi(defj,4);
  edform(x+length(edm_str),y,m,'01/01/1000','31/12/2199',art);
  m:=Copy(m,4,7);
end;


procedure editsf(liste:pntslcta; n:xpWord; var brk:boolean);

var px,ml,i : byte;
    en      : endeedtyp;
    p       : xpWord;

  procedure clearout;
  var i,l : byte;
  begin
    for i:=esfy to esfy+n-1 do begin
      l:=liste^[i-esfy+1].nu;
      wrt(esfx,i,sp(l));
      end;
  end;

begin
  moff;
  clearout;
  for i:=1 to n do
    wrt(esfx,i+pred(esfy),iifs(esfch='*','',esfch+' ')+liste^[i].el);
  mon;
  px:=0; p:=1; brk:=false;
  repeat
    ml:=liste^[p].nu;
    readedit(esfx+iif(esfch='*',0,2),p+pred(esfy),'',liste^[p].el,
             ml,chml[1],px,edittabelle,en);
    case en of
      enlinks  : begin px:=60; dec(p); end;
      enrechts,
      enreturn : begin px:=0; inc(p); end;
      enoben   : dec(p);
      enunten  : inc(p);
      enabbr   : brk:=true;
      enpgdn   : p:=succ(n);
    end;
  until (p<1) or (p>n) or brk;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure dummyproc(var s:string; var ok:boolean);
begin
  ok:=true;
end;

procedure dummyed(x,y:byte; var s:string; var p:shortint; var en:endeedtyp);
begin
  en:=enabbr;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }


var
  m1 : shortint = -1;

procedure editms(n:integer; var feld; eoben:boolean; var brk:boolean);

var i,pl   : integer;
    en,en2 : endeedtyp;
    ok     : boolean;

{ art: 0 = links; 1 = fest/Feld; 2 = fest/insgesamt }

begin
  for i:=1 to n do
    with editsa(feld)[i] do
      px:=0;
  editmsp:=1; pl:=0; brk:=false;
  repeat
    moff;
    for i:=1 to n do
      with editsa(feld)[i] do
        if len>0 then wrt(x,y,forms(s,len))
        else if len=0 then edproc(x,y,s,m1,en2);
    mon;
    with editsa(feld)[editmsp] do begin
      case art of
        0 : px:=0;
        1 : px:=px;
        2 : px:=min(pl,length(s));
      end;
      if len=-1 then
        begin end          { ueberspringen }
      else if len=0 then
        edproc(x,y,s,px,en)
      else begin
        readedit(x,y,'',s,len,chml[1],byte(px),edittabelle,en);
        s := TrimRight(s);
      end;
      pl:=px;
      if en=enabbr then brk:=true else begin
        tproc(s,ok); if not ok then en:=enno;
        end;
      if (len=1) and (en=enrechts) and (lastkey<>keyrght) then en:=enno;
      if len>0 then mwrt(x,y,s)
      else edproc(x,y,s,m1,en2);
      case en of
        enlinks,
        enoben    : if editmsp>1 then dec(editmsp);
        enrechts,enunten,
        enreturn  : inc(editmsp);
        enpgdn    : editmsp:=succ(n);
      end;
    end;
    while (editmsp<=n) and (editmsp>1) and (editsa(feld)[editmsp].len=-1) do
      if (en=enlinks) or (en=enoben) then dec(editmsp)
      else inc(editmsp);
  until ((editmsp<1) and eoben) or (editmsp>n) or brk;
end;


Function chkn:longint;      { lokal }
begin
  chkn:=26690;
end;

procedure chalt;
begin
  Halt(0);
end;



procedure mausiniti;
begin
  if maus and iomaus then begin
    setmaus(320,96);
    mx:=320;
    my:=96;
    end;
end;


Function CopyChr(x,y:byte):char;
{$IFDEF NCRT }
begin
  CopyChr:= '?'; { <--- Notfalls doch wieder LocalScreen }
{$ELSE }
var
  c: Char;
  Attr: SmallWord;
begin
  GetScreenChar(x, y, c, Attr);
  CopyChr := c;
{$ENDIF}
end;

Function memadr(x,y:byte):xpWord;
begin
  memadr:=2*pred(x)+2*ScreenWidth*pred(y);
end;


procedure bdchar(x,y:byte; txt:string; var c:char; li:string; var brk:boolean);
var t : taste;
    p : byte;
begin
  mwrt(x,y,txt);
  inc(x,length(txt));
  repeat
    mwrt(x,y,c); gotoxy(x,y);
    get(t,curon);
    t:=UpperCase(t);
    if pos(t[1],li)>0 then c:=t[1];
    if (t=keyup) then begin
      p:=pos(c,li); inc(p);
      c:=li[iif(p>length(li),1,p)];
      end;
    if (t=keydown) then begin
      p:=pos(c,li); dec(p);
      c:=li[iif(p=0,length(li),p)];
      end;
  until (t=keycr) or (t=keyesc);
  brk:=(t=keyesc);
end;


procedure checkpm;        { lokal }
var check : longint;
  ii: Integer;
begin
  check:=0;
  for ii:=1 to length(pm) do
    inc(check,(ord(pm[ii]) xor $e5)*(20-ii));
  if check<>chkn then chalt;
end;

procedure waitkey(x,y:byte);
var t : taste;
begin
  mwrt(x,y,'Druecken Sie eine Taste ...');
  get(t,curon);
end;

procedure DosOutput;                         { auf CON: umschalten      }
begin
  close(output);
  assign(output,'');
  rewrite(output);
end;

{ msec = 0 -> laufende Timeslice freigeben }
{$IFNDEF NCRT }
procedure mdelay(msec:xpWord);   { genaues Delay }
var t      : longint;
    i,n    : xpWord;

  procedure idle;
  begin
  {$IFDEF Win32 }
    Sleep(1);
  {$ENDIF }
  {$IFDEF OS2 }
    DosSleep(1);
  {$ENDIF }
  end;

begin
  n:=system.round(msec/54.925401155);
  if n=0 then
    idle
  else
  begin
    t:=ticker;
    for i:=1 to n do begin
      multi2;
      while t=ticker do
        idle;
      if t<ticker then inc(t)
      else t:=ticker;
    end;
  end;
end;
{$ENDIF } { NCRT }

var
  SavedExitProc: pointer;

procedure ExitInOutUnit;
begin
  ExitProc:= SavedExitProc;
{$IFNDEF NCRT }
  cursor(curon);
{$ENDIF }
end;

procedure InitInOutUnit;
var
  ii, jj: Integer;
begin
  chml[1]:=range(#32,#126)+range(#128,#255);
  chml[3]:='1234567890 ';
  chml[2]:=chml[3]+'.,';
  getcur(ca,ce,sx[1],sy[1]);
  sa[1]:=ca; se[1]:=ce;
  sec:=99;
  fillchar(fndef,sizeof(fndef),0);
  fillchar(fnproc,sizeof(fnproc),0);
  checkpm;
  scsaveadr:=dummyFN;
  for ii:=1 to maxalt do begin
    altproc[ii].schluessel:='';
    altproc[ii].funktion:=dummyFN;
    altproc[ii].aktiv:=false;
    end;
  for jj:=0 to 3 do
    for ii:=1 to 10 do
      fnproc[jj,ii]:=dummyFN;
  fillchar(fnpactive,sizeof(fnpactive),false);
  istackp:=0;
  cursp:=0;
  multi3:=dummyFN;
  memerror:=dummyFN;
  fillchar(zaehler,sizeof(zaehler),0);
  fillchar(zaehlproc,sizeof(zaehlproc),0);
  mwl:=1; mwo:=1; mwr:=screenwidth; mwu:=screenlines;
  { Exit-Kette }
  SavedExitProc:= ExitProc;
  ExitProc:= @ExitInOutUnit;
end;

{
  $Log: inout.pas,v $
  Revision 1.108  2003/10/06 16:01:32  mk
  - some little code optimizations (mostly added const parameters and
    use of new file system RTL functions)

  Revision 1.107  2003/08/28 01:14:14  mk
  - removed old types s20, s40, s60 and s80

  Revision 1.106  2003/08/23 19:15:27  mk
  - compile fixes for dos32

  Revision 1.105  2003/05/10 17:30:00  mk
  - added call to multi2

  Revision 1.104  2003/04/12 13:02:12  mk
  - added more debug info for getkey

  Revision 1.103  2003/04/12 08:03:42  mk
  - removed ParWinTime, ParOs2, Usemulti2 and command line options /w and /os2

  Revision 1.102  2002/12/28 20:11:03  dodi
  - start keyboard input redesign

  Revision 1.101  2002/12/21 05:37:50  dodi
  - removed questionable references to Word type

  Revision 1.100  2002/12/16 14:58:35  mk
  - fixed compiler warnings

  Revision 1.99  2002/12/14 07:31:26  dodi
  - using new types

  Revision 1.98  2002/12/12 11:58:39  dodi
  - set $WRITEABLECONT OFF

  Revision 1.97  2002/12/04 16:56:58  dodi
  - updated uses, comments and todos

  Revision 1.96  2002/11/14 20:04:20  cl
  - Added button controls

  Revision 1.95  2002/10/06 17:52:53  mk
  - made compilable for DOS

  Revision 1.94  2002/09/09 08:42:32  mk
  - misc performance improvements

  Revision 1.93  2002/07/29 07:17:19  mk
  - fixed AnsiString[1] to FirstChar(AnsiString)

  Revision 1.92  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.91  2002/05/19 10:52:09  mk
  - do readpar before initializing the ncurses lib to
    allow displaying of the command line parameters
    with write and writeln

    Please test his change for side effects

  Revision 1.90  2002/05/12 17:57:43  ma
  - added clear text keypress debug info

  Revision 1.89  2002/02/21 17:14:41  mk
  - linux compile fix

  Revision 1.88  2002/02/21 13:52:30  mk
  - removed 21 hints and 28 warnings

  Revision 1.87  2001/12/24 16:49:53  mk
  - implemented Cursor for DOS32

  Revision 1.86  2001/10/24 09:25:17  ma
  - adjusted debug levels

  Revision 1.85  2001/10/20 17:26:39  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.84  2001/10/17 09:54:42  ml
  - cursorpos etc. byte -> integer to prevent range-errors
  - terminals can be larger than 255

  Revision 1.83  2001/10/11 15:27:01  mk
  - implemented direct screen writes for DOS32, no more LocalScreen

  Revision 1.82  2001/10/01 19:43:01  ma
  - compiles again (DOS32)
  - function Cursor is not yet implemented (DOS32)

  Revision 1.81  2001/09/26 23:19:15  mk
  - reimplemented testbrk

  Revision 1.80  2001/09/18 20:29:19  cl
  - fixed scrolling with pressed mouse button

  Revision 1.79  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.78  2001/09/16 17:56:01  ma
  - adjusted debug levels

  Revision 1.77  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.76  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.75  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.74  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.73  2001/08/11 23:06:26  mk
  - changed Pos() to cPos() when possible

  Revision 1.72  2001/07/31 16:18:39  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.71  2001/07/28 12:04:08  mk
  - removed crt unit as much as possible

  Revision 1.70  2001/04/10 10:03:23  ml
  - keyboard-translation completely rewritten (what a mess)
  - Ctrl-Up/Down now do the job

  Revision 1.69  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.68  2001/01/01 20:16:06  mk
  - changes for os2 and freepascal

  Revision 1.67  2000/12/26 16:39:34  mk
  - removed Window()

  Revision 1.66  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.65  2000/11/19 12:47:49  mk
  - fixed ticker

  Revision 1.64  2000/11/15 23:00:39  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.63  2000/11/15 17:45:47  hd
  - Unit DOS entfernt

  Revision 1.62  2000/10/25 17:32:12  fe
  Abhaengigkeitsprobleme (hoffentlich) beseitigt.

  Revision 1.61  2000/10/24 20:19:26  fe
  Zirkulaere Abhaenhigkeiten entfernt.

  Revision 1.60  2000/10/17 10:05:40  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.59  2000/09/30 16:39:00  mk
  - SysSetBackIntensity

  Revision 1.58  2000/09/10 15:11:52  hd
  - Fix: Farbe unter Linux

  Revision 1.57  2000/09/08 16:12:06  hd
  - Init-Reihenfolge

  Revision 1.56  2000/09/05 16:12:53  mo
  -  Rechenzeitfreigabe: wieder auf sleep(1) geaendert

  Revision 1.55  2000/08/26 12:47:11  mk
  - Sleep(1) in Sleep(0) geaendert, das reicht zur Rechenzeitfreigabe

  Revision 1.54  2000/08/17 12:11:13  mk
  MO: Anzeige der Uhr fuer Screen > 80 Zeichen angepasst

  Revision 1.53  2000/08/08 13:18:13  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.52  2000/08/08 00:00:40  mk
  - AnsiString-Bug beseitigt

  Revision 1.51  2000/08/03 15:26:31  mk
  - Zeitschleifenfreigabe unter OS/2 implementiert

  Revision 1.50  2000/07/30 08:49:52  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.49  2000/07/27 10:12:58  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.48  2000/07/21 21:17:43  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.47  2000/07/20 16:49:56  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.46  2000/07/09 09:09:54  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.45  2000/07/05 09:50:12  hd
  - AnsiString-Anpassung

  Revision 1.44  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.43  2000/07/04 12:04:16  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.42  2000/07/03 13:31:38  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.41  2000/07/02 14:24:48  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.40  2000/06/23 15:59:11  mk
  - 16 Bit Teile entfernt

  Revision 1.39  2000/06/22 19:53:26  mk
  - 16 Bit Teile ausgebaut

  Revision 1.38  2000/06/01 16:03:04  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.37  2000/05/10 10:30:22  hd
  - AttrTxt setzte die Farbe zweimal

  Revision 1.36  2000/05/07 15:56:32  hd
  Keine Uhr unter Linux

  Revision 1.35  2000/05/07 15:19:49  hd
  Interne Linux-Aenderungen

  Revision 1.34  2000/05/06 17:29:20  mk
  - DOS DPMI32 Portierung

  Revision 1.33  2000/05/06 15:57:03  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.32  2000/05/03 20:35:02  hd
  - disphard angepasst

  Revision 1.31  2000/05/02 15:48:40  hd
  Cursor unter Linux an-/ausschalten

  Revision 1.30  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.29  2000/04/29 16:45:06  mk
  - Verschiedene kleinere Aufraeumarbeiten

  Revision 1.28  2000/04/29 16:06:59  hd
  Linux-Anpassung

  Revision 1.27  2000/04/24 14:54:49  ml
  Linux-Anpassungen

  Revision 1.26  2000/04/23 07:58:52  mk
  - OS/2-Portierung

  Revision 1.25  2000/04/13 17:20:37  mk
  - Cursortypen setzen unter DOS und Win32 implementiert

  Revision 1.24  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.23  2000/04/04 21:01:21  mk
  - Bugfixes f�r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.22  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.21  2000/03/24 20:25:50  rb
  ASM-Routinen gesaeubert, Register fuer VP + FPC angegeben, Portierung FPC <-> VP

  Revision 1.20  2000/03/24 00:03:39  rb
  erste Anpassungen fuer die portierung mit VP

  Revision 1.19  2000/03/23 15:47:23  jg
  - Uhr im Vollbildlister aktiv
    (belegt jetzt 7 Byte (leerzeichen vorne und hinten)

  Revision 1.18  2000/03/20 11:25:15  mk
  - Sleep(1) in Idle-Routine bei Win32 eingefuegt

  Revision 1.17  2000/03/16 19:38:54  mk
  - ticker: globale Konstante TickFreq genutzt

  Revision 1.16  2000/03/16 10:14:24  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f�r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen �nderung wieder

  Revision 1.15  2000/03/15 14:00:52  mk
  - 32 Bit Ticker Bugfix

  Revision 1.14  2000/03/14 15:15:35  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.13  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.12  2000/03/08 22:36:33  mk
  - Bugfixes f�r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.11  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.9  2000/03/04 22:41:37  mk
  LocalScreen fuer xpme komplett implementiert

}
end.


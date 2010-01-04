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

{ Maskeneditor; V1.1 08/91, 05/92 PM }

{$I xpdefine.inc }

unit  maske;

interface

uses
  xpglobal,variants,
  keys,inout;

const digits : string = '-0123456789 ';
var   MaskSeekMenu : Byte = 0;
      //todo: make set of char = [' '..#$7E, #$80..#$FE];
const allchar = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXY'+
                'Z[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñ'+
                'óòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛';
      hexchar = '<0123456789abcdef';

      mtString   = 1;
      mtShortint = 2;
      mtByte     = 3;
      mtInteger  = 4;
      mtWord     = 5;
      mtLongint  = 6;

var   exit_mask    : boolean = false; { = true, sobald Maske verlassen wird   }
      cDel_pressed : boolean = false; { = true, wenn <Ctrl-Del> erkannt wird  }
                                               { und delete_on_cDel true ist  }

type  colrec   =  record              { 0 = keine spezielle Farbe }
                    ColBack,          { Hintergrund & Rahmen  }
                    ColFeldName,      { Feldnamen             }
                    ColDisabled,      { ausgeschalteter Name  }
                    ColFeldNorm,      { Feldinhalt Anzeige    }
                    ColFeldInput,     { Eingabefeld           }
                    ColFeldActive,    { aktives Eingabefeld   }
                    ColFeldMarked,    { markierter Feldinhalt }
                    ColArrows,        { Pfeile bei Scrollfeld }
                    ColHelpTxt,       { Hilfszeile            }
                    ColFnInfo,        { Info-Text fuer SekKey  }
                    ColFnFill,        { kein Info-Text...     }
                    ColSelBox,        { Auswahlbox            }
                    ColSelBar,        { Auswahlbalken A.-Box  }
                    ColButtons: byte; { Check/Radio-Buttons   }
                  end;

      customrec = record
                    acol : colrec;    { aktuelle Farben }
                    x,y  : Integer;   { aktuelle Pos. des Feldinhalts }
                    fpos : integer;   { akt. Feldnummer }
                    s    : string;    { var: Feldinhalt }
                    brk  : boolean;   { var }
                  end;

      testfunc    = function(var inhalt:string):boolean;
      testproc    = procedure(var inhalt:string);
      customsel   = procedure(var cr:customrec);
      quitfunc    = function(brk,modif:boolean):boolean;
      userdproc   = procedure;

      scrollfunc  = function(offset: integer):boolean;

      wrapmodes   = (dont_wrap,do_wrap,endonlast);


{-------------- allgemeine Funktionen -------------}

procedure openmask(l,r,o,u: Integer; pushit:boolean);   { neue Maske oeffnen }
procedure readmask(var brk:boolean);                { *** Einlesen ***  }
// function readmask(var brk:boolean): integer;                { *** Einlesen ***  }
procedure readHmask(mhelpnr: Integer; var brk:boolean);
// function readHmask(mhelpnr: Integer; var brk:boolean): integer; { .. mit Hilfsseiten }
function  mmodified:boolean;                        { Inhalt geaendert   }
procedure closemask;                                { Maske schliessen   }
procedure readstring(x,y: Integer; const text:string; var s:string; displ,maxl: Integer;
                     const chml:string; var brk:boolean);
procedure mbeep;
procedure DefaultColor(var col:colrec);             { col <- Default    }
procedure masklanguage(const _yesno:string);        { 'JN'              }

procedure mdummyp(var inhalt:string);               { Dummy fuer Test0   }
function  mdummyf(var inhalt:string):boolean;       { Dummy fuer Test1/2 }
function  qdummyf(brk,modif:boolean):boolean;       { Dummy fuer QuitFN  }

procedure maskShiftF2(p:testproc;helpnr:xpWord);

{--------------- Masken-Einstellungen -------------}
{ beziehen sich auf die jeweils aktuelle Maske und }
{ werden in amaskp^.stat abgelegt                  }

procedure maskcol(cols:colrec);              { Farben der akt. Maske setzen }
procedure maskrahmen(rtyp,l,r,o,u: Integer);     { Rahmentyp setzen }
procedure masksetstat(keepon_esc,autosel:boolean; selkey:taste);
procedure masksetfillchar(c:char);           { Fuellzeichen setzen }
procedure masksethelp(hx,hy,hl: Integer; center:boolean);   { Hilfszeile einst. }
procedure masksetfninfo(x,y: Integer; const text:string; fillc:char);
procedure masksetwrapmode(wm:wrapmodes);
procedure masksetautojump(aj: Integer);     { Sprungweite bei cr am unteren Rand }
procedure masksetqfunc(qfunc:quitfunc);
procedure masksetscrollfunc(scrollfn:scrollfunc);
procedure masksetarrowspace(aas:boolean);
procedure masksetmausarrows(ma:boolean);
procedure masksetautohigh(ah:boolean);  { Felder automatisch selektieren }
procedure maskdontclear;
procedure maskcheckbuttons;
procedure maskselcursor(cur:curtype);
procedure maskUpDownArrows(x1,y1,x2,y2: Integer; fill:char; col:byte);

{------------ Felder anlegen ------------}
{ werden an die aktuelle Maske angehaengt }

{ Integer-Typen: 2 = ShortInt, 3 = Byte, 4 = Integer, 5 = Word, 6 = LongInt }

procedure Maddtext(x,y:integer; const text:string; att:byte);   { Anzeigetext anfuegen }
procedure maddhline(x,y:integer);

procedure Maddstring(x,y:integer; const text:string; var s:string; displ,maxl:integer;
                     const chml:string);
procedure Maddint(x,y: integer; const text:string; var int; ityp,displ:integer;
                     imin,imax:longint);
procedure Maddreal(x,y: integer; const text:string; var r:real; displ,rnk : integer;
                     rmin,rmax : real);
procedure Maddbool(x,y:integer; text:string; var b:boolean);
procedure Maddform(x,y:integer; const text:string; var s:string; const form,chml:string);
procedure Madddate(x,y:integer; const text:string; var d:string; long,mbempty:boolean);
procedure Maddtime(x,y:integer; const text:string; var t:string; long:boolean);
procedure Maddcustomsel(x,y:integer; const text:string; var s:string; displ:integer;
                        cp:customsel);
procedure Maddbutton(x,y:integer; const text:string; var ret:integer; retval:integer; quit:boolean);

{----------------- Feld-Einstellungen ----------------}
{ beziehen ich auf das jeweils zuletzt angelegte Feld }

procedure MSetProcs(p0,p3:testProc);
procedure MSet0Proc(p0:testproc);        { bei Feldeintritt     }
procedure MSet1Func(p1:testfunc);        { bei jeder énderung   }
procedure MSetVFunc(p2:testfunc);        { vor Verlassen        }
procedure MSet3Proc(p3:testProc);        { bei Verlassen        }
procedure MSetUserDisp(ud:userdproc);    { bei Komplett-Neuanzeige }

procedure MDisable;                           { Feld deaktivieren }
procedure MDisabledNodisplay;                 { deaktiviert nicht anzeigen }
procedure MH(const text:string);              { Hilfszeile setzen }
procedure MHnr(helpnr: Integer);              { Hilfsseiten-Nr. setzen }
procedure MSelHnr(helpnr: Integer);           { Hilfsseite fuer <F2> }
procedure MSetSel(sx,sy,slen: Integer);       { Abmessungen der SelListe }
procedure MAppSel(force:boolean; const s:string); { SelBox aufbauen }
procedure Mappcustomsel(cp:customsel; nedit:boolean);
procedure Mnotrim;                            { kein autotrim }
procedure Malltrim;                           { rtrim/ltrim }
procedure Mspecialcol(attr:byte);             { spez. Farbe fuer Feldname }
procedure MSetAutoHigh(ah:boolean);           { automat. selektieren }

{----------------- Externe Funktionen --------------}
{ dienen zum Zugriff von externen (Test-)Funktionen }
{ auf Inhalte der momentan editierten Maske         }

procedure setfield(nr: Integer; const newcont:string);
function  getfield(nr: Integer):string;
function  fieldpos:integer;         { akt.FeldNr, auch waehrend Maskenaufbau! }
procedure setfieldpos(nr: Integer);
procedure setfieldenable(nr: Integer; eflag:boolean);   { Feld (de)aktivieren }
procedure setfieldnodisp(nr: Integer; dflag:boolean);   { Feld nicht anzeigen }
function  mask_helpnr: Integer;
function  readmask_active:boolean;
procedure set_chml(nr: Integer; chml:string);
procedure setfieldtext(nr: Integer; const newtxt:string);
function  mtextpos:pointer;
procedure settexttext(p:pointer; const newtxt:string);
procedure mclearsel(nr: Integer);
procedure mappendsel(nr: Integer; force:boolean; const s:string);

procedure mscroll(distance: Integer);
procedure mquit(brk: boolean); overload;
procedure mquit(returncode: integer); overload;

procedure InitMaskeUnit;

implementation  {---------------------------------------------------------}

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typinfo, xp0,
  typeform,
  maus2,winxp,montage, clip, osdepend;

const maxmask   = 10;                { max. gleichzeitig offene Masken }
      maxfields = 140;               { max. Felder pro Maske           }

var   insert_mode : boolean = true;
      help_page   : Integer = 0;        { Helpnr des Eingabefeldes }
      yesno       : string[2] = 'JN';

type
      { Achtung! Pointer in MaskStat muessen in OpenMask }
      {          gesondert behandelt werden!            }

      maskstat = record
                   col         : colrec;
                   rahmentyp   : byte;     { 0=keiner, 1/2/3, 4=wechselnd }
                   rl,rr,ro,ru : Integer;  { Rahmen-Koordinaten }
                   hpx,hpy,hpl : Integer;  { Position/Len Hilfstexte }
                   hcenter     : boolean;  { Hilfstexte zentrieren }
                   keeponesc   : boolean;  { Eingaben trotz Esc behalten }
                   autoselbox  : boolean;  { Auswahlbox automatisch oeffnen }
                   fillchar    : char;     { Fuellzeichen bei Eingabe }
                   selboxkey   : taste;    { '' -> keine SelBox; Def: F2 }
                   fnix,fniy   : Integer;  { Position der FNKey-Info }
                   fnkeyinfo   : string;
                   fnkeyfill   : char;
                   wrapmode    : wrapmodes;
                   autojump    : Integer;  { Zeilensprung bei verl.d.Fensters }
                   quitfn      : quitfunc;
                   scrollfn    : scrollfunc;
                   arrowspace  : boolean;  { Leerzeichen vor/hinter Feld }
                   mausarrows  : boolean;
                   fautohigh   : boolean;  { Felder automat. selektieren }
                   dontclear   : boolean;  { Fenster nicht loeschen }
                   checkbutts  : boolean;
                   Userdisp    : userdproc;  { bei Bild-Neuaufbau          }
                   selcursor   : boolean;
                 end;

      udarec   = record
                   x1,y1,x2,y2 : Integer;
                   fillc       : char;
                   color       : byte;
                 end;

      selnodep = ^selnode;
      selnode  = record                    { Knoten fuer Select-Liste }
                   next        : selnodep;
                   el          : string;
                 end;

      textnodep= ^textnode;
      textnode = record                    { Knoten fuer Anzeigetext-Liste }
                   next        : textnodep;
                   txt         : string;
                   xx,yy,attr  : Integer;
                 end;

      feldtyp = (  feldtyp_null = 0,
                   feldtyp_string = 1,
                   feldtyp_shortint,
                   feldtyp_byte,
                   feldtyp_integer,
                   feldtyp_word,
                   feldtyp_longint,
                   feldtyp_real,
                   feldtyp_bool,
                   feldtyp_date,
                   feldtyp_time,
                   feldtyp_button );

      feldrec  = record
                   enabled     : boolean;
                   disnodisp   : boolean;
                   txt         : string;   { Feld-Text }
                   typ         : feldtyp;     { Feldtyp }
                   variable    : Pointer;     { Adresse der Variablen }
                   xx,yy, len  : Integer;     { Position, Anzeigelaenge }
                   yy0,xx2     : Integer;     { Position des Inhalts }
                   maxlen      : Integer;     { maximale Laenge des Inhalts }
                   cont        : string;   { Feldinhalt }
                   allowed     : string;   { erlaubte Zeichen }
                   mask        : string[20];  { Masken-String }
                   autoup,
                   autodown,
                   topcase     : boolean;    { automatische Gross/Kleinschr.}
                   convcolon   : boolean;    { automatisch "," -> "." }
                   _min,_max   : longint;
                   _rmin,_rmax : real;
                   nk          : byte;       { Nachkommastellen bei Real   }
                   test0       : testproc;   { vor jedem Editieren         }
                   test1       : testfunc;   { bei jeder aendernden Eingabe }
                   test2       : testfunc;   { vor Verlassen des Feldes    }
                   test3       : testproc;   { bei Verlassen des Feldes    }
                   hpline      : string;
                   helpnr      : Integer;    { Hilfsseiten-Nr. }
                   selhelpnr   : Integer;    { Hilfsseite bei <F2> }
                   selliste    : selnodep;
                   hassel      : boolean;
                   slx,sly,sll : Integer;    { SListen-Position/Laenge }
                   slmin       : Integer;    { minimale Listenlaenge }
                   noslpos     : boolean;    { slx..sll noch nicht gesetzt }
                   forcesll    : boolean;
                   pempty      : boolean;    { Formatierter Str. darf leer sein }
                   custom      : customsel;  { eigene Select-Prozedur }
                   nonedit     : boolean;    { Feld nicht editierbar }
                   autotrim    : byte;       { 0=nein, 1=r, 2=r+l }
                   owncol      : boolean;    { spezielle Farbe fuer Feldname }
                   ownattr     : byte;
                   autohigh    : boolean;    { Feld autom. selektieren }
                   counter     : byte;       { 1/2 -> "+"/"-" bei Datum/Zeit }
                   checkbutt   : boolean;    { Check-Button }
                   hotkeypos   : integer;    { Position des Hotkeys von 1..Length(txt), 0=kein Hotkey }
                   hotkey      : string;     { Hotkey (string wg. UTF-8, etc.) }
                   btnquit     : boolean;    { Dialog bei Knopf verlassen }
                 end;
      feldp    = ^feldrec;

      masktyp  = record
                   stat        : maskstat;
                   li,re,ob,un : Integer;     { Arbeitsbereich }
                   dopush      : boolean;     { Inhalt sichern }
                   felder      : Integer;        { Anzahl Felder  }
                   fld         : array[1..maxfields] of feldp;
                   mtxt        : textnodep;
                   maxyy0      : Integer;        { groesster Y-Wert }
                   yp,a        : integer;     { akt. Feldnr./Offset }
                   modified    : boolean;     { Inhalt geaendert }
                   editing     : boolean;     { Editieren aktiv }
                   uda         : udarec;     { Pfeile bei scrollbaren Masken }

                   newfld      : boolean;
                   redisplay   : boolean;

                   done        : boolean;
                   return      : integer;
                 end;
      maskp    = ^masktyp;


var   mask    : array[0..maxmask] of maskp;
      masks   : byte;
      amask   : byte;       { aktuelle Maske, z.Zt. immer = masks! }
      amaskp  : maskp;      { mask[amask] }
      lastfld : feldp;      { aktuelles Feld waehrend des Maskenaufbaus }

      redispfields : boolean;
      redisptext   : boolean;

      ShiftF2Proc : testProc;
      ShiftF2Help : xpWord;


{ Feldtypen:   1=String, 2=Short, 3=Byte, 4=Integer, 5=Word, 6=Long,
               7=Real, 8=Datum (tt.mm.jj oder tt.mm.jjjj),
               9=Uhrzeit (hh:mm oder hh:mm:ss), 10=Boolean (J/N)  }

procedure maskShiftF2(p:testproc;helpnr:xpWord);
begin
    ShiftF2Proc:=p;
    ShiftF2help:=helpnr;
end;

procedure error(const txt:string);
begin
  writeln('MASK: ',txt);
  halt(1);
end;

procedure mbeep;
begin            
  SysBeep(600, 20);
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure mdummyp(var inhalt:string);
begin
end;

function mdummyf(var inhalt:string):boolean;
begin
  mdummyf:=true;
end;

function qdummyf(brk,modif:boolean):boolean;
begin
  qdummyf:=true;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }


{--------------------- Maske anlegen/bearbeiten ------------------}

procedure testfield(nr:integer); forward;

{ neue Maske oeffnen, falls noch Handles frei   }
{ der Maskenstatus wird von mask[0] uebernommen }

procedure openmask(l,r,o,u: Integer; pushit:boolean);
begin
  if masks=maxmask then error('Overflow');
  inc(masks);
  amask:=masks;
{  new(mask[amask]);}
  getmem(amaskp,sizeof(masktyp));
  system.fillchar(amaskp^,sizeof(masktyp),0);
  mask[amask] := amaskp;
  with amaskp^ do
  begin
    stat:=mask[0]^.stat;
    stat.fnkeyinfo := mask[0]^.stat.fnkeyinfo;
    uda:=mask[0]^.uda;
    li:=l; re:=r; ob:=o; un:=u;
    felder:=0; maxyy0:=0;
    dopush:=pushit;
    mtxt:=nil;
    editing:=false;
    yp:=1;
    end;
  lastfld:=nil;
end;


procedure mclearsel(nr: Integer);
var p1,p2 : selnodep;
begin
  testfield(nr);
  with amaskp^.fld[nr]^ do begin
    p1:=selliste;
    while p1<>nil do
    begin       { Selliste freigeben }
      p2:=p1^.next;
      freemem(p1,sizeof(selnode));
      p1:=p2;
    end;
    selliste:=nil;
  end;
end;


{ aktuelle (oberste) Maske schliessen }

procedure closemask;
var i     : integer;
    t1,t2 : textnodep;
begin
  if masks=0 then error('Underflow');
  with amaskp^ do begin
    for i:=1 to felder do begin
      with fld[i]^ do begin
        mclearsel(i);
      end;
      freemem(fld[i],sizeof(feldrec));
      end;
    t1:=mtxt;
    while t1<>nil do begin           { Textliste freigeben }
      t2:=t1^.next;
      freemem(t1,sizeof(textnode));
      t1:=t2;
      end;
    end;
  freemem(amaskp,sizeof(masktyp));
  amaskp:=nil;
  dec(masks);
  amask:=masks;
  amaskp:=mask[masks];
end;


{ neue Farben einstellen }

procedure maskcol(cols:colrec);
begin
  amaskp^.stat.col:=cols;
end;


{ neuen Rahmentyp einstellen
  0 = kein Rahmen
  1/2/3 = einfach/doppelt/spezial
  4 = automatisch aendern beim durchscrollen }

procedure maskrahmen(rtyp,l,r,o,u: Integer);
begin
  with amaskp^.stat do begin
    rahmentyp:=rtyp;
    rl:=l; rr:=r; ro:=o; ru:=u;
    end;
end;


{ diverse Status-Flags setzen                       }
{ keepon_esc:  Feldinhalt auch bei 'brk' uebernehmen }
{ autosel:     SelListen automatisch oeffnen   (nni) }
{ selkey:      Taste fuer SelListen                  }

procedure masksetstat(keepon_esc,autosel:boolean; selkey:taste);
begin
  with amaskp^.stat do begin
    keeponesc:=keepon_esc;
    autoselbox:=autosel;
    selboxkey:=selkey;
    end;
end;


{ Fuellzeichen fuer Rest der Zeile setzen }

procedure masksetfillchar(c:char);
begin
  amaskp^.stat.fillchar:=c;
end;


{ Hilfszeile einstellen      }
{ hx = 0 -> keine Hilfszeile }

procedure masksethelp(hx,hy,hl: Integer; center:boolean);
begin
  with amaskp^.stat do begin
    hpx:=hx;
    hpy:=hy;
    hpl:=hl;
    hcenter:=center;
    end;
end;


{ Info-Text fuer SelKey einstellen }

procedure masksetfninfo(x,y: Integer; const text:string; fillc:char);
begin
  with amaskp^.stat do begin
    fnix:=x; fniy:=y;
    fnkeyinfo:=text;
    fnkeyfill:=fillc;
    end;
end;


{ Wrap-Mode einstellen                          }
{ dont_wrap:  bei 1. und letztem Feld anhalten  }
{ do_wrap:    bei 1. und letztem Feld wrappen   }
{ endonlast:  bei 1. Feld anhalten, beim letzen }
{             Ctrl-Enter ausfuehren              }

procedure masksetwrapmode(wm:wrapmodes);
begin
  amaskp^.stat.wrapmode:=wm;
end;


{ AutoJump gibt an, um wieviele Zeilen die Maske }
{ automatisch weiterspringen soll, wenn sie nach }
{ unten mit Return verlassen wird.               }

procedure masksetautojump(aj: Integer);
begin
  amaskp^.stat.autojump:=aj;
end;


{ Masken-Fenster  wird zu Beginn nicht geloescht }

procedure maskdontclear;
begin
  amaskp^.stat.dontclear:=true;
end;


procedure maskcheckbuttons;
begin
  amaskp^.stat.checkbutts:=true;
end;

procedure maskselcursor(cur:curtype);
begin
  amaskp^.stat.selcursor:=(cur=curon);
end;


procedure MaskUpDownArrows(x1,y1,x2,y2: Integer; fill:char; col:byte);
begin
  amaskp^.uda.x1:=x1;
  amaskp^.uda.y1:=y1;
  amaskp^.uda.x2:=x2;
  amaskp^.uda.y2:=y2;
  amaskp^.uda.fillc:=fill;
  amaskp^.uda.color:=col;
end;


{ QFunc wird vor Beenden der Eingabe aufgerufen }
{ kann diese verhindern; uebergebene Parameter:  }
{ brk:    Beenden durch Esc-Taste               }
{ modif:  Feldinhalt wurde geaendert             }

procedure masksetqfunc(qfunc:quitfunc);
begin
  amaskp^.stat.quitfn:=qfunc;
end;

procedure masksetscrollfunc(scrollfn:scrollfunc);
begin
  amaskp^.stat.scrollfn := scrollfn;
end;

{ Anzeige eines Leezeichens vor/hinter den Eingabefeldern ein/ausschalten }

procedure masksetarrowspace(aas:boolean);
begin
  amaskp^.stat.arrowspace:=aas;
end;


{ Pfeil nach unten bei Auswahl-Liste }

procedure masksetmausarrows(ma:boolean);
begin
  amaskp^.stat.mausarrows:=ma;
end;


procedure masksetautohigh(ah:boolean);  { Felder automatisch selektieren }
begin
  amaskp^.stat.fautohigh:=ah;
end;


{----------------- Felder anfuegen -------------------}

{ reinen Anzeigetext anfuegen }
{ attr=0 -> ColFeldName      }

procedure Maddtext(x,y:integer; const text:string; att:byte);
var p : textnodep;
begin
  with amaskp^ do begin
    getmem(p,sizeof(textnode));
    system.fillchar(p^,sizeof(textnode),0);
    with p^ do begin
      xx:=x+li-1;
      yy:=y;
      txt:=text;
      if att=0 then attr:=stat.col.colfeldname
      else attr:=att;
      next:=mtxt;
      end;
    mtxt:=p;
    end;
end;

procedure maddhline(x,y:integer);
begin
  maddtext(0,y,#$C3+dup(amaskp^.re-amaskp^.li+1,#$C4)+#$B4,amaskp^.stat.col.colback);
end;

function mtextpos:pointer;
begin
  mtextpos:=amaskp^.mtxt;
end;


procedure setall(const text:string; x,y: Integer; addblank:boolean);
var
   newfld2 : feldp;
begin
  with amaskp^ do
    if felder=maxfields then
      error('no more fields')
    else begin
      inc(felder);
      getmem(newfld2,sizeof(feldrec));
      system.fillchar(newfld2^,sizeof(feldrec),0);
      fld[felder] := newfld2;
      lastfld:=fld[felder];
      with lastfld^ do
      begin
        autoup:=false; autodown:=false; topcase:=false;
        _min := 0; _max := 0; _rmin := 0; _rmax := 0;
        disnodisp:=false; hassel := false;
        convcolon:=false;
        hpline:=''; selliste:=nil;
        slx:=0; sly:=0; sll:=0;
        mask:=''; cont := '';
        nk:=0; helpnr := 0; selhelpnr := 0;
        maxlen := 0;
        pempty:=false;
        variable:= nil;
        custom:=nil; nonedit:=false;
        allowed:= ''; owncol:=false;
        counter:=0; noslpos := false;
        checkbutt:=false;
        enabled:=true;
        if text='' then addblank:=false;
        txt:=text+iifs(addblank,' ','');
        typ := feldtyp_null;
        len := 0;
        yy0:=y; ownattr := 0;
        maxyy0:=max(maxyy0,yy0);
        xx:=li+x-1; yy:=ob+y-1;
        xx2:=xx+length(text);
        if text<>'' then inc(xx2);
        test0:=mdummyp;
        test1:=mdummyf;
        test2:=mdummyf;
        test3:=mdummyp;
        noslpos:=true;
        forcesll:=true; slmin:=5;    { minimale SelListen-Laenge }
        autotrim:=1;
        autohigh:=stat.fautohigh;

        hotkeypos := CPos('^',txt);
        
        if (hotkeypos>0) and (hotkeypos<Length(txt)) then
        begin
          hotkey := txt[hotkeypos+1];
          txt := LeftStr(txt,hotkeypos-1)+Mid(txt,hotkeypos+1);
        end else
        begin
          hotkeypos := 0;
          hotkey := '';
        end;
        
        if (felder>1) and (fld[felder-1]^.helpnr>0) then
          helpnr:=fld[felder-1]^.helpnr+1;
        end;
      end;
end;


{ String anfuegen -----------------------------------------}
{ chml = ''  -> alle Zeichen erlaubt                      }
{ Das erste Zeichen von chml wird gesondert ausgewertet:  }
{ '>'  ->  automatische Umwandlung in Grossbuchstaben      }
{ '<'  ->  automatische Umwandlung in Kleinbuchstaben     }
{ '!'  ->  automatische Grossschreibung des 1. Buchstabens }

procedure Maddstring(x,y:integer; const text:string; var s:string; displ,maxl:integer;
                     const chml:string);
var p : Integer;
begin
  setall(text,x,y,true);
  with lastfld^ do 
  begin
    typ:=feldtyp_string;
    variable:=@s;          
    len:=displ; maxlen:=maxl;
    if maxlen=1 then autohigh:=false;
    repeat
      p:=cpos(#7,s);
      if p=0 then p:=cpos(#8,s);
      if p>0 then s[p]:=' ';
    until p=0;
    cont:=LeftStr(s,maxl);
    set_chml(amaskp^.felder,chml);
  end;
end;


{ Integer Anfuegen
  Typ 2 = ShortInt, 3 = Byte, 4 = Integer, 5 = Word, 6 = LongInt }

function feldtyp_is_int(ityp: feldtyp): boolean;
begin
  result := ityp in [feldtyp_shortint..feldtyp_longint];
end;

function feldtyp_is_num(ityp: feldtyp): boolean;
begin
  result := ityp in [feldtyp_shortint..feldtyp_longint,feldtyp_real];
end;

procedure Maddint(x,y:integer; const text:string; var int; ityp,displ:integer;
                  imin,imax:longint);
var l : longint;
    s : String;
begin
  if not feldtyp_is_int(feldtyp(ityp)) then
    error('illegal Int type');

  setall(text,x,y,true);
  with lastfld^ do begin
    typ:=feldtyp(ityp);
    variable:= @int;
    len:=displ{+1}; maxlen:=displ;
    case feldtyp(ityp) of
      feldtyp_shortint : l:=shortint(int);
      feldtyp_byte : l:=byte(int);
      feldtyp_integer : l:=integer16(int);
      feldtyp_word : l:=smallword(int);
    else
      l:=longint(int);
    end;
    _min:=imin; _max:=imax;
   { l:=min(max(l,imin),imax); }
    str(l:displ,s);
    cont:=s{+' '};
    allowed:=digits;
    end;
end;


{ Real anfuegen }

procedure Maddreal(x,y:integer; const text:string; var r:real; displ,rnk :integer;
                   rmin,rmax : real);
var s : String;
begin
  setall(text,x,y,true);
  with lastfld^ do begin
    typ:=feldtyp_real;
    variable:=@r;
    len:=displ{+1}; maxlen:=displ;
    _rmin:=rmin; _rmax:=rmax;
  { r:=minr(maxr(r,rmin),rmax); }
    str(r:displ:rnk,s);
    nk:=rnk;
    cont:=s;
    convcolon:=true;
    allowed:=digits+'.';
    end;
end;


{ Bool-Wert anfuegen }

procedure Maddbool(x,y:integer; text:string; var b:boolean);
begin
  if amaskp^.stat.checkbutts then begin
    text:=trimright(text);
    TrimLastChar(text, '?');
    end;
  setall(text,x,y,not amaskp^.stat.checkbutts);
  with lastfld^ do begin
    checkbutt:=amaskp^.stat.checkbutts;
    if checkbutt then begin
      xx2:=xx;
      xx:=xx2+7;
      end;
    typ:=feldtyp_bool;
    variable:=@b;
    len:=1; maxlen:=1;
    cont:=iifc(b,yesno[1],yesno[2]);
    autoup:=true;
    allowed:='>'+yesno;
    autohigh:=false;
    end;
end;


{ Formatierten String anfuegen                               }
{ Eingaben koennen ueberall erfolgen, wo im Format ' ' steht. }
{ Alle anderen Stellen werden aus dem Format uebernommen.    }
{ Wenn s='', dann wird s:=form gesetzt.                     }

procedure Maddform(x,y:integer; const text:string; var s:string; const form,chml:string);
begin
  if s='' then s:=form;
  MAddString(x,y,text,s,length(form),length(form),chml);
  with lastfld^ do begin
    mask:=form;
    autotrim:=0;
    autohigh:=false;
    end;
end;


{ Datum anfuegen               }
{ long -> langes Datumsformat }
{ mbempty -> may be empty     }

procedure Madddate(x,y:integer; const text:string; var d:string; long,mbempty:boolean);
begin
  Maddform(x,y,text,d,iifs(long,'  .  .    ','  .  .  '),' 0123456789');
  with lastfld^ do begin
    typ:=feldtyp_date;
    pempty:=mbempty;
    counter:=1;
    end;
end;


{ Uhrzeit anfuegen }

procedure Maddtime(x,y:integer; const text:string; var t:string; long:boolean);
begin
  Maddform(x,y,text,t,iifs(long,'  :  :  ','  :  '),'0123456789');
  lastfld^.typ:=feldtyp_time;
  lastfld^.counter:=2;
end;

{ Schaltfl‰che anf¸gen ----------------------------------------------- }

{ Wenn ein Knopf gedr¸ckt wird, passiert folgendes:                    }
{ - test0proc wird aufgeruden                                          }
{ - test1func wird aufgerufen, wenn diese true zur¸ckgibt, wird:       }
{   - der Wert retval in ret gespeichert                               }
{   - falls quit true ist, wird auﬂerdem:                              }
{     - das dialogfeld verlassen                                       }
{     - read(h)mask gibt in brk true zur¸ck, falls ret<0, sonst false  }

procedure Maddbutton(x,y:integer; const text:string; var ret:integer; retval:integer; quit:boolean);
begin
  setall(text,x,y,false);
  with lastfld^ do 
  begin
    typ := feldtyp_button;
    variable := @ret;
    maxlen := retval;     // na ja.
    autohigh := false;
    btnquit := quit;
  end;

end;

{ Feld mit beliebiger eigener Select-Routine anfuegen  }
{ s : Feldinhalt zu Beginn; wird von cp ueberschrieben }
{ displ : Anzeige-Laenge (wg. forms)                   }
{ Das Feld ist nicht mehr editierbar!                 }

procedure Maddcustomsel(x,y:integer; const text:string; var s:string; displ:integer;
                        cp:customsel);
begin
  Maddstring(x,y,text,s,displ,displ,'');
  with lastfld^ do begin
    custom:=cp;
    nonedit:=true;
    hassel:=true;
    end;
end;


function testlast:boolean;
begin
  if lastfld=nil then begin
    error('Operation on non-existing field');
    testlast:=false;
    end
  else
    testlast:=true;
end;


procedure MDisable;
begin
  if testlast then
    lastfld^.enabled:=false;
end;


procedure MDisabledNodisplay;                 { deaktiviert nicht anzeigen }
begin
  if testlast then begin
    lastfld^.enabled:=false;
    lastfld^.disnodisp:=true;
  end;
end;


procedure MSetProcs(p0,p3:testproc);
begin
  if testlast then
    with lastfld^ do begin
      test0:=p0;            { bei Feldeintritt }
      test3:=p3;            { bei Feldaustritt }
      end;
end;

procedure MSet0Proc(p0:testproc);
begin
  if testlast then
    lastfld^.test0:=p0;
end;

procedure MSet3Proc(p3:testproc);
begin
  if testlast then
    lastfld^.test3:=p3;
end;

procedure MSet1Func(p1:testfunc);
begin
  if testlast then
    lastfld^.test1:=p1;
end;


{ Valid-Funktion setzen }

procedure MSetVFunc(p2:testfunc);
begin
  if testlast then
    lastfld^.test2:=p2;
end;

procedure MSetUserDisp(ud:userdproc);    { bei Komplett-Neuanzeige }
begin
  amaskp^.stat.userdisp:=ud;
end;


{ Hilfszeile setzen }

procedure MH(const text:string);
begin
  if testlast then
    with lastfld^ do
      hpline:=text;
end;


procedure MHnr(helpnr: Integer);
begin
  if testlast then
    lastfld^.helpnr:=helpnr;
end;


procedure MSelHnr(helpnr: Integer);
begin
  if testlast then
    lastfld^.selhelpnr:=helpnr;
end;


{ Position und Laenge (gl) der SelListe einstellen }
{ Ist xp=0, so wird die Position weiterhin automa-}
{ tisch eingestellt, weobei die Liste mindestens  }
{ len Zeilen lang ist.                            }

procedure MSetSel(sx,sy,slen: Integer);
begin
  if testlast then
    with lastfld^ do begin
      slx:=sx;
      sly:=sy;
      if slx>0 then sll:=slen
      else slmin:=slen;
      noslpos:=(slx=0);
      end;
end;


{ Neue Zeilen an eine Select-Liste anhaengen            }
{ s kann mehrere durch "˘" getrennte Strings enthalten }
{ Ist force=true, so wird anhand der Listeneintraege    }
{ eine Valid-öberpruefung durchgefuehrt. Force wird bei  }
{ jedem Aufruf ueberschrieben; es ist also bei mehreren }
{ MAppSel und das letzte 'force' von Bedeutung.        }
{                                                      }
{ _mappsel:   interne Prozedur                         }
{ MappSel:    beim Maskenaufbau                        }
{ MAppendSel: nachtraeglich                             }


procedure _mappsel(feld:feldp; force:boolean; s:string);
var s1 : string[80];
    p  : byte;

  procedure app(var p:selnodep);
    procedure makenewel;
    begin
      getmem(p,sizeof(selnode));
      system.fillchar(p^,sizeof(selnode),0);
      p^.next:=nil;
      p^.el:=s1;
    end;
  begin
    if p<>nil then app(p^.next)
    else
      makenewel;     { getrennte Prozedur zur Stack-Entlastung }
  end;

begin
  with feld^ do begin
    while s<>'' do begin
      p:=cPos('˘',mid(s,2));
      if p=0 then p:=length(s)+1
      else inc(p);
      s1:=copy(s,1,p-1);
      if feldtyp_is_int(typ) then
        str(ival(s1):maxlen,s1)
      else if typ=feldtyp_real then
        str(rval(s1):maxlen:nk,s1);
      s:=Mid(s,p+1);
      app(selliste);
      end;
    forcesll:=force;
    hassel:=true;
    end;
end;


procedure mappendsel(nr: Integer; force:boolean; const s:string);
begin
  testfield(nr);
  _mappsel(amaskp^.fld[nr],force,s);
end;


procedure MAppSel(force:boolean; const s:string);
begin
  if testlast then
    _mappsel(lastfld,force,s);
end;


{ Eigene Select-Routine angeben }

procedure Mappcustomsel(cp:customsel; nedit:boolean);
begin
  if testlast then
    with lastfld^ do begin
      custom:=cp;
      nonedit:=nedit;
      hassel:=true;
      end;
end;


procedure Mnotrim;
begin
  if testlast then
    lastfld^.autotrim:=0;
end;

procedure Malltrim;
begin
  if testlast then
    lastfld^.autotrim:=2;
end;


{ spzeielle Farbe fuer Feldnamen einstellen }

procedure Mspecialcol(attr:byte);
begin
  if testlast then begin
    lastfld^.owncol:=true;
    lastfld^.ownattr:=attr;
    end;
end;


procedure MSetAutoHigh(ah:boolean);          { automat. selektieren }
begin
  if testlast then
    lastfld^.autohigh:=ah;
end;

{ mhelpnr=0 -> keine Hilfsseiten }

procedure readHmask(mhelpnr: Integer; var brk:boolean);
//function readHmask(mhelpnr: Integer; var brk:boolean): integer; { .. mit Hilfsseiten }
var ax,p,myp  : integer;
    afld      : feldp;
    helpsave  : Integer;          { zum Sichern von help_page }
    t,alt_t   : taste;
    newfld    : boolean;
    redisplay : boolean;
    replace   : boolean;
    doreplace : boolean;
    s         : string;
    valchar   : boolean;
    i,x       : integer;
    mzu,mzo   : boolean;
    mzl,mzr   : boolean;
    crec      : ^customrec;
    fwd       : boolean;
    maussel   : boolean;
    poutside  : boolean;
    { adiff     : integer; }
    moretext  : boolean;
    lasttexty : byte;
    gl        : byte;

  procedure ClearFields;
  var i : integer;
  begin
    with amaskp^ do begin
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if disnodisp and not enabled then begin
            attrtxt(stat.col.ColFeldName);
            wrt(xx,yy,dup(length(txt)+iif(typ=feldtyp_button,2,len+3),' '));
          end;
      mon;
      end;
    normtxt;
  end;

  procedure ShowFldNames;
  var i : integer;
  begin
    with amaskp^ do begin
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if enabled or (not disnodisp) and (typ<>feldtyp_button) then
          begin
            if enabled then
              if owncol then attrtxt(ownattr)
              else attrtxt(stat.col.ColFeldName)
            else attrtxt(stat.col.ColDisabled);

            if enabled and (hotkeypos > 0) then
            begin
              if hotkeypos>1 then
                FWrt(xx,yy-a,LeftStr(txt, hotkeypos));
              if hotkeypos<length(txt) then
                FWrt(xx+hotkeypos+1,yy-a,RightStr(txt, Length(txt)-hotkeypos));
              attrtxt(col.coldiahigh);
              FWrt(xx+hotkeypos,yy-a,txt[hotkeypos]);
            end else
              FWrt(xx,yy-a,txt);
          end;
      mon;
      end;
    normtxt;
  end;

  procedure ButtonDisplay(var fld:feldp;active:boolean);
  begin
    with fld^,amaskp^ do 
    begin
      if hotkeypos>0 then
      begin
        attrtxt(col.colbuttonhigh);
        FWrt(xx+1+hotkeypos-1,yy-a,txt[hotkeypos]);
      end;
          
      attrtxt(col.colbutton);

      if hotkeypos>1 then
        FWrt(xx+1        ,yy-a,LeftStr(txt,hotkeypos-1));
      FWrt(xx+1+hotkeypos,yy-a,Mid(txt,Max(1,hotkeypos+1)));
         
      if active then
      begin
        attrtxt(col.colbuttonarr);
        FWrt(xx,              yy-a,#16);
        FWrt(xx+Length(txt)+1,yy-a,#17);
      end else
      begin
        FWrt(xx,              yy-a,' ');
        FWrt(xx+Length(txt)+1,yy-a,' ');
      end;
    end;
  end;

  procedure FldDisplay;
  var i   : integer;
      l   : String;
      ua  : boolean;    { Scrollpfeil nach oben }
      da  : boolean;    { Scrollpfeil nach unten }
      s: String;
  begin
    with amaskp^ do begin
      attrtxt(stat.col.ColFeldInput);
      l:=iifs(stat.arrowspace,' ','');
      ua:=false; da:=false;
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if typ=feldtyp_button then
            ButtonDisplay(fld[i],i=yp)
          else
          if (yy0>=a+1) and (yy<=a+un) then begin
            if enabled or (not disnodisp) then begin
              if not enabled then { not enabled }
	      begin
                attrtxt(stat.col.ColBack);
                s := l+sp(len)+l;
                if stat.mausarrows and hassel then s := s + (' ')
                else if checkbutt then s := s + '  ';
                fwrt(xx2,yy-a,s);
                attrtxt(stat.col.ColFeldInput);
              end { not enabled }
              else if checkbutt then begin { enabled, checkbutt }
                attrtxt(stat.col.colbuttons);
                fwrt(xx2,yy-a,' ['+iifc(cont=yesno[1],'x',' ')+'] ');
              end { enabled, checkbutt }
              else begin { enabled, not checkbutt }
                attrtxt(stat.col.colfeldinput);
                s := l+forms(cont,len);
                fwrt(xx2,yy-a,s);
                if stat.arrowspace and enabled then
                begin
                  if length(cont)>len then
                  begin
                    attrtxt(stat.col.ColArrows);
                    FWrt(xx2+Length(s), yy-a, #16);
                  end else
                    FWrt(xx2+Length(s), yy-a, ' ');
                  if stat.mausarrows and hassel then
		  begin
                    attrtxt(stat.col.colback);
                    FWrt(xx2+Length(s)+1, yy-a, #25);
                  end;
                end; { stat.arrowspace and enabled }
              end; { enabled, not checkbutt }
            end; { enabled or (not dispnodisp) } 
          end { (yy0>=a+1) and (yy<=a+un) }
          else
            if yy0<a+1 then ua:=true
            else da:=true;
      attrtxt(uda.color);
      if uda.x1>0 then fwrt(uda.x1,uda.y1,iifc(ua,#30,uda.fillc));
      if uda.x2>0 then fwrt(uda.x2,uda.y2,iifc(da,#31,uda.fillc));
      mon;
      end;
    normtxt;
  end;

  procedure textdisplay;
  var p : textnodep;
  begin
    with amaskp^ do begin
      moretext:=false;
      lasttexty:=0;
      p:=mtxt;
      moff;
      while p<>nil do begin
        with p^ do begin
          lasttexty:=max(lasttexty,yy);
          if yy>a+un-ob+1 then
            moretext:=true
          else if yy>=a+1 then begin
            attrtxt(attr);
            fwrt(xx,yy+ob-1-a,txt);
            end;
          end;
        p:=p^.next;
        end;
      normtxt;
      mon;
      end;
  end;

  procedure setcur;
  begin
    with afld^ do
      gotoxy(xx2+p-iif(amaskp^.stat.arrowspace,0,1)+iif(checkbutt,1,0),
             yy-amaskp^.a);
  end;

  function valid(feld:feldp; entered:boolean):boolean;
  var l   : longint;
      res : integer;
      s   : string;
      r   : real;
      p   : selnodep;
      v   : boolean;

    function testdate:boolean;
    var t,m,j : Integer;
    begin
      with feld^ do
        if pempty and (cont=mask) then
          testdate:=true
        else begin
          if len=8 then
          begin
            { Y2K Fix }
            j :=ival(copy(cont,7,2));
            if j < 70 then inc(j, 2000) else inc(j, 1900);
          end else
            j:=ival(copy(cont,7,4));
          schalt(j);
          t:=ival(copy(cont,1,2));
          m:=ival(copy(cont,4,2));
          testdate:=(m>=1) and (m<=12) and
                    (t>=1) and (t<=monat[m].zahl);
          end;
    end;

    function testtime(len: Integer; const t:string):boolean;
    var h,m,s : Integer;
    begin
      h:=ival(copy(t,1,2));
      m:=ival(copy(t,4,2));
      if len>5 then s:=ival(copy(t,7,2))
      else s:=1;
      testtime:=(h<=23) and (m<=59) and (s<=59);
    end;

  begin
    v:=true;
    with feld^ do begin
      if typ=feldtyp_string then
        case autotrim of
          1 : cont:=trimright(cont);
          2 : cont:=trim(cont);
        end;
      if feldtyp_is_num(typ) then begin
        s:=trim(cont);
        if cPos(' ',s)>0 then s:=copy(s,1,cPos(' ',s)-1);
        if feldtyp_is_int(typ) then begin
          val(s,l,res);
          if not entered then begin
            v:=v and (l>=_min) and (l<=_max);
            end
          else
            if (l<_min) or (l>_max) then begin
              l:=max(l,_min);
              l:=min(l,_max);
              amaskp^.modified:=true;
              end;
          str(l:maxlen,cont);
          end
        else begin
          val(s,r,res);
          if not entered then begin
            v:=v and (r>=_rmin) and (r<=_rmax);
            end
          else
            if (r<_rmin) or (r>_rmax) then begin
              r:=maxr(r,_rmin);
              r:=minr(r,_rmax);
              amaskp^.modified:=true;
              end;
          str(r:maxlen:nk,cont);
          end;
        {cont:=cont+' ';}
        end;
      if (selliste<>nil) and forcesll then begin
        p:=selliste;
        while (p<>nil) and (UpperCase(cont)<>UpperCase(p^.el)) do
          p:=p^.next;
        if (p=nil) and (cont<>'') then begin    { Ñhnlichen Eintrag suchen }
          p:=selliste;
          while (p<>nil) and (LeftStr(UpperCase(p^.el),length(cont))<>UpperCase(cont)) do
            p:=p^.next;
          if p<>nil then begin
            cont:=p^.el;
            amaskp^.modified:=true;
            end;
          end;
        v:=v and (p<>nil);
        end;
      case typ of
        feldtyp_date : v:=v and testdate;
        feldtyp_time : v:=v and testtime(len,cont);
      end;
      valid:=v and test2(cont);
    end;
  end;

  procedure showfield;
  var
    x: Byte;
    s: String;
  begin
    with amaskp^ do
      with afld^ do begin
        moff;
        if checkbutt then
        begin
          attrtxt(stat.col.colbuttons);
          FWrt(xx2, yy-a, ' [' + iifc(cont=yesno[1],'x',' ') + '] ');
        end else
        if typ=feldtyp_button then
          ButtonDisplay(afld,true) else
        begin
          x := xx2;
          if stat.arrowspace then
          begin
            attrtxt(stat.col.colarrows);
            FWrt(x, yy-a, iifc(ax>0,#17,' ')); Inc(x);
          end;
          attrtxt(stat.col.ColFeldActive);
          if replace then attrtxt(stat.col.ColFeldMarked);
          s := copy(cont,ax+1,len);
          FWrt(x, yy-a, s); Inc(x, Length(s));
          attrtxt(stat.col.ColFeldActive);
          s := dup(len-length(cont)+ax,stat.fillchar);
          FWrt(x, yy-a, s); Inc(x, Length(s));
          if stat.arrowspace then begin
            attrtxt(stat.col.colarrows);
            FWrt(x, yy-a, (iifc(length(cont)-ax>len,#16,' '))); Inc(x);
          end;
          if stat.mausarrows and hassel then begin
            attrtxt(stat.col.colback);
            FWrt(x, yy-a, #25);
          end;
        end;
        mon;
        normtxt;
        end;
  end;

  procedure select(var brk,fwd:boolean);     { sll = gl ! }
  var p1     : selnodep;
      lines  : integer;
      width  : Integer;
      longest: Integer;
      i,p,sa : integer;
      t      : taste;
      na,x   : Integer;
      insmaus: boolean;   { Taste wurde 'inside' gedrÅckt }

    procedure display;
    var i : integer;
    begin
      with amaskp^,afld^ do begin
        p1:=selliste;
        for i:=1 to sa do       { sa *mu·* < lines sein!! }
          p1:=p1^.next;
        moff;
        for i:=1 to sll do begin
          if i=p then attrtxt(stat.col.ColSelBar);
          if p1=nil then fwrt(slx+1,sly+i,sp(width+2))
          else fwrt(slx+1,sly+i,' '+forms(mid(p1^.el,x),width+1));
          normtxt;
          if p1<>nil then p1:=p1^.next;
          end;
        fwrt(slx,sly+1,iifc(sa>0,#30,'≥'));
        fwrt(slx,sly+sll,iifc(sa+sll<lines,#31,'≥'));
        fwrt(slx+1,sly,iifc(x>0,#17,'ƒ'));
        fwrt(slx+width+2,sly,iifc(x+width<longest,#16,'ƒ'));
        mon;
        end;
    end;

    procedure oben;
    begin
      if p>1 then dec(p) else
      if sa>0 then dec(sa);
    end;

    procedure unten;
    begin
      if (sa+p<lines) then
        if p<afld^.sll then inc(p)
        else inc(sa);
    end;
    procedure maus_bearbeiten;
    var mx,my  : integer;
        inside : boolean;
    begin
      with afld^,amaskp^ do begin
        maus_gettext(mx,my);
        inside:=(mx>slx) and (mx<slx+width+3) and (my>sly) and (my<sly+sll+1);
        dec(my,sly);
        if (t=mausleft) or (t=mausright) then
          insmaus:=inside;
        if inside or insmaus then
          if (t=mausunleft) or (t=mausunright) then t:=^J;
        if inside then
          if (t=mausleft) or (t=mauslmoved) or (t=mausrmoved) or (t=mausright)
            then p:=minmax(my,1,min(sll,lines-sa));
        if not inside then begin
          if t=mausleft then t:=^J
          else if t=mausright then t:=keyesc;
          end;
        end;
    end;

  begin { procedure select }
    brk:=false;
    with amaskp^,afld^ do begin
      width:=4; lines:=0; x:= 0;      { Minimale Breite: 4 }
      p1:=selliste;
      p:=1; sa:=0; longest := 0;
      insmaus:=maussel;
      while p1<>nil do begin    { Breite und Zeilenzahl bestimmen }
        inc(lines);
        longest:=max(longest,length(p1^.el));
        width:=max(width,min(length(p1^.el), ScreenWidth-10));
        if UpperCase(cont)=UpperCase(p1^.el) then p:=lines;
        p1:=p1^.next;
        end;

      if noslpos then             { Position wird jeweils neu bestimmt }
        if maussel then begin
          slx:=min(xx2,ScreenWidth-5-width);
          sly:=max(3,yy-p-a);
          sll:=min(lines,ScreenLines-sly-1);
          end
        else
          if (yy-a<=ScreenLines-3-slmin) or (yy-a+lines+2<=ScreenLines-1) then begin
            slx:=min(xx2,ScreenWidth-5-width); sly:=yy-a+1; sll:=min(lines,ScreenLines-3-yy+a);
            end
          else begin
            i:=lines div 2;
            slx:=min(ScreenWidth-5-width,xx2+7);
            sly:=max(3,min(yy-a-i+1,ScreenLines-2-lines));
            sll:=min(ScreenLines-2-sly,lines);
            end;
      if p>sll then begin
        sa:=p-sll; p:=sll; end;

      na:=normattr; normattr:=stat.col.ColSelBox;
      normtxt;
      wpushs(slx,slx+width+3,sly,sly+sll+1,'');
      maus_pushinside(slx+1,slx+width+2,sly+1,sly+sll-1);
      repeat
        display;
        if stat.selcursor then begin
          gotoxy(slx+1,sly+p);
          get(t,curon);
          end
        else
          get(t,curoff);
        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten;
        if t=keyup then oben;
        if t=keydown then unten;
        if t=keyleft then if x>1 then dec(x,10);
        if t=keyrght then if x+width<longest then inc(x,10);
        if t=keyhome then begin
          p:=1; sa:=0;
          end;
        if t=keyend then begin
          p:=min(lines,sll);
          sa:=lines-p;
          end;
        if t=keypgup then
          for i:=1 to sll-1 do oben;
        if t=keypgdn then
          for i:=1 to sll-1 do unten;
        if t=stat.selboxkey then t:=keyesc;
      until (t=keycr) or (t=^J) or (t=keyesc);
      maus_popinside;
      normattr:=na;
      normtxt;
      if t<>keyesc then begin
        p1:=selliste;
        for i:=1 to p+sa-1 do p1:=p1^.next;
        if length(p1^.el)>maxlen then
          error('Eingabefeld zu kurz!');
        cont:=p1^.el;
        modified:=true;
        end;
      end;
    wpop;
    brk:=(t=keyesc);
    fwd:=(t=keycr);
    maussel:=false;
  end;

  { Achtung! vor jeder Modifikation an cont mu· xtest1() aufgerufen werden! }

  function xtest1(var s:string):boolean;
  var x1 : boolean;
  begin
    if afld^.nonedit then xtest1:=false
    else begin
      if afld^.topcase then
      begin
        s := LowerCase(s);
        if s<> '' then s[1] := UpCase(s[1]);
      end;
      x1:=afld^.test1(s);
      if x1 then amaskp^.modified:=true;
      xtest1:=x1;
    end;
  end;

  { offs Eingabefelder weiterspringen }

  procedure movefield(offs:integer);
  var newyp : integer;
      s     : string;
  begin
    with amaskp^ do begin
      p:=1; ax:=0;
      with afld^ do begin
        if autotrim<>0 then begin               { altes Feld verlassen ... }
          s:=cont;
          if autotrim=1 then
            s:= TrimRight(s)
          else
            s:=trim(s);
          if (s<>cont) and (xtest1(s)) then;
          cont:=s;
          end;
        if (offs=0) or valid(afld,true) then begin
          afld^.test3(afld^.cont);
          FldDisplay;

          newyp:=max(1,min(felder,yp+offs));    { neues Feld suchen ... }
          while (newyp<=felder) and not fld[newyp]^.enabled do begin
            if abs(offs)>1 then offs:=-sgn(offs);
            inc(newyp,sgn(offs));
            if newyp<1 then
              case stat.wrapmode of
                dont_wrap,endonlast : begin offs:=-offs; newyp:=1; end;
                do_wrap             : newyp:=felder;
              end
            else if newyp>felder then
              case stat.wrapmode of
                dont_wrap  : begin offs:=-offs; newyp:=felder; end;
                do_wrap    : newyp:=1;
                endonlast  : if t=keycr then t:=^J
                             else begin
                               offs:=-offs; newyp:=felder;
                               end;
              end;
            end;
          if newyp<=felder then yp:=newyp;

          while a+1>fld[yp]^.yy0 do begin       { a anpassen ... }
            dec(a); redisplay:=true;
            end;
          while a+(un-ob+1)<fld[yp]^.yy0 do begin
            if t=keycr then
              inc(a,min(stat.autojump,maxyy0-(a+un-ob+1)))
            else inc(a);
            redisplay:=true;
            end;
          end;
        end;
      newfld:=true;     { Kennzeichen: zur Zeit kein aktives Feld; }
                        {              Feld yp ist zu aktivieren   }
      end;
  end;

  procedure links;
  begin
    repeat
      if p>1 then dec(p)
      else if ax>0 then dec(ax);
    until (afld^.mask='') or (afld^.mask[p+ax]=' ');
    replace:=false;
  end;

  procedure rechts;
  begin
    with afld^ do begin
      if (p+ax<=length(cont)) and
         not ((p=len) and (len=maxlen) and (p+ax=length(cont))) then
        repeat
          if p<len then inc(p)
          else inc(ax);
        until (mask='') or ((mask[ax+p]=' ') or (ax+p>length(mask)));
      if (mask<>'') and (ax+p>length(mask)) then
        repeat
          if ax>0 then dec(ax)
          else dec(p)
        until mask[ax+p]=' ';
      end;
    replace:=false;
  end;

  procedure returnfields;
  var i : integer;
      l : longint;
      r : real;
      b : boolean;
  begin
    with amaskp^ do
      for i:=1 to felder do
        with fld[i]^ do
          if variable<>nil then begin
            if feldtyp_is_num(typ) then l:=ival(cont);
            case typ of
              feldtyp_string,feldtyp_date,feldtyp_time
                    : AnsiString(variable^):= cont; {Move(cont,variable^,length(cont)+1);}
              feldtyp_shortint,feldtyp_byte : Move(l,variable^,1);
              feldtyp_integer,feldtyp_word  : Move(l,variable^,2);
              feldtyp_longint               : Move(l,variable^,4);
              feldtyp_real : begin
                        r:=rval(cont);
                        Move(r,variable^,6);
                      end;
              feldtyp_bool : begin
                        b:=(cont=yesno[1]);
                        Move(b,variable^,1);
                      end;
            end;
    end;
  end;

  procedure fillfnhelp;
  begin
    with amaskp^.stat,afld^ do begin
      attrtxt(col.ColFnFill);
      mwrt(fnix,fniy,dup(length(fnkeyinfo),fnkeyfill));
      end;
  end;

  procedure exitmask;
  begin
    if not newfld then
      with amaskp^.stat,afld^ do begin
        test3(cont);
        replace:=false;
        showfield;
        if hpline<>'' then
        begin
          attrtxt(col.ColHelpTxt);
          wrt(hpx,hpy,sp(hpl));
        end;
        if (fnix<>0) and (selliste<>nil) then
          fillfnhelp;
        normtxt;
        end;
  end;

  procedure _count_;
  var dat : fdate;
      s   : datetimest;
      h,m : Integer;
  begin
    with afld^ do begin
      s:=cont;
      if cpos(' ',s)>0 then exit;
      if counter=1 then begin
        dat.t:=ival(LeftStr(s,2));
        dat.m:=ival(copy(s,4,2));
        if length(s)=8 then begin
          dat.j:=ival(RightStr(s,2));
          if dat.j>=50 then inc(dat.j,1900)
          else inc(dat.j,2000);
          end
        else
          dat.j:=ival(RightStr(s,4));
        if t='+' then incd(dat)
        else decd(dat);
        cont:=formi(dat.t,2)+'.'+formi(dat.m,2)+'.'+
               iifs(length(s)=8,formi(dat.j mod 100,2),formi(dat.j,4));
        end
      else begin
        h:=ival(LeftStr(s,2));
        m:=ival(RightStr(s,2));
        if t='+' then begin
          inc(m);
          if m>59 then begin
            m:=0; inc(h);
            if h>23 then h:=0;
            end;
          end
        else
          if m>0 then dec(m)
          else begin
            m:=59;
            if h>0 then dec(h)
            else h:=23;
            end;
        cont:=formi(h,2)+':'+formi(m,2);
        end;
      end;
  end;

  procedure testfndef;
  var fnkn : byte;
  begin
    if ((t>=keyf1) and (t<=keyf10)) or
       ((t>=keysf1) and (t<=keysf10)) then begin
      if t<=keyf10 then
        fnkn:=ord(t[2])-58
      else
        fnkn:=ord(t[2])-73;
      if fndef[fnkn]<>'' then
        if LastChar(fndef[fnkn])=';' then
          keyboard(LeftStr(fndef[fnkn],length(fndef[fnkn])-1)+#13)
        else
          keyboard(fndef[fnkn]);
      end;
  end;

  procedure button_bearbeiten(fld: feldp);
  begin
    fld.test0(fld.cont);
    if fld.test1(fld.cont) and fld.btnquit then
    begin
      amaskp^.return := fld.maxlen;
      if fld.maxlen<0 then t := keyESC
      else                 t := ^J;
    end;
  end;

  procedure maus_bearbeiten;
  var inside  : boolean;
      mx,my,i : integer;
      arrows  : integer;
      ok      : boolean;
      infield : boolean;
      s       : string;
  begin
    with amaskp^ do begin
      maus_gettext(mx,my);
      if ((t=mausleft) or (t=mausldouble)) and (mx=uda.x1) and (my=uda.y1) then
        t:=keypgup
      else if ((t=mausleft) or (t=mausldouble)) and (mx=uda.x2) and (my=uda.y2) then
        t:=keypgdn
      else begin
        inside:=(mx>=li-1) and (mx<=re+1) and (my>=ob-1) and (my<=un+1);
        if inside then begin
          arrows:=iif(stat.arrowspace,2,0);
          if (t=mausleft) or (t=mausldouble) or (t=mausright) then begin
            i:=1;
            ok:=false;
            while (i<=felder) and not ok do begin
              with fld[i]^ do
                ok:=(mx>=min(xx,xx2)-1) and
                    (mx<=max(xx2+len+arrows+iif(hassel,1,0),xx+length(txt))) and
                    (my=yy-a) and enabled;
              if not ok then inc(i);
              end;
            if ok then with fld[i]^ do begin
              movefield(i-yp);
              if yp=i then begin
                afld:=fld[yp];
                infield:=(mx>=xx2) and (mx<xx2+len+arrows+iif(checkbutt,2,0));
                if hassel and (mx>=xx2+len+arrows-1) and (mx<=xx2+len+arrows+1) then
                  _keyboard(stat.selboxkey)
                else begin
                  if infield then
                    if stat.arrowspace and (length(cont)-ax>len) and (mx=xx2+len+1)
                    then begin
                      ax:=length(cont)-len+1;
                      p:=length(cont)-ax+1;
                      replace:=false;
                      doreplace:=false;
                      end
                    else if t<>mausldouble then begin
                      p:=minmax(mx-xx2-iif(stat.arrowspace,0,1),1,
                                min(length(cont)+1,len));
                      replace:=false;
                      doreplace:=false;
                      end
                    else begin
                      p:=1; ax:=0;
                      end;
                  if checkbutt and ((t=mausleft) or (t=mausldouble)) then begin
                    if cont=yesno[1] then s:=yesno[2]
                    else s:=yesno[1];
                    if xtest1(s) then cont:=s;
                    end;
                  if t=mausright then
                    if infield and hassel then
                      maussel:=true;
                  end;
                end;
              end;
            end;
          if (t=mausunright) or (t=mausunleft) then
            poutside:=false;
          end
        else begin    { not inside }
          if (t=mausright) or (t=mausleft) then poutside:=true else
          if (t=mausunleft) and poutside then t:=^J else
          if (t=mausunright) and poutside then t:=keyesc;
          end;
        end;
      end;
  end;

begin { procedure readHmask(mhelpnr:word; var brk:boolean); }
  newfld:=true; redisplay:=true;
  mzu:=mauszuu; mzo:=mauszuo;
  mzl:=mauszul; mzr:=mauszur;
  mauszul:=true; mauszur:=true;
  insert_mode:=true;
  helpsave:=help_page;
  exit_mask:=false;

  with amaskp^,amaskp^.stat do begin
    if felder=0 then error('no fields!');
    editing:=true;
    a:=0; ax:=0; p:=1;
    modified:=false;
    doreplace:=true;
//  yp:=1;
    setfieldpos(yp);

    while not fld[yp]^.enabled do inc(yp);
    if dopush then
      if rahmentyp=0 then
        wpush(li,re+1,ob,un+1,'-')
      else
        wpush(rl,rr+1,ro,ru+1,'-');

    attrtxt(col.ColBack);
    forcecolor:=true;
    case rahmentyp of
      1   : rahmen1(rl,rr,ro,ru,'');
      2,4 : rahmen2(rl,rr,ro,ru,'');
      3   : rahmen3(rl,rr,ro,ru,'');
    end;
    if rahmentyp>0 then wshadow(rl+1,rr+1,ro+1,ru+1);
    forcecolor:=false;
    if rahmentyp>0 then clwin(rl+1,rr-1,ro+1,ru-1);

    maussel:=false;
    poutside:=false;
    repeat { main loop }

      if redisplay then
      begin
        attrtxt(col.ColBack);
        if rahmentyp=4 then
	begin
          moff;
          if a>0 then fwrt(rl,ro,'÷'+dup(rr-rl-1,'ƒ')+'∑')
          else fwrt(rl,ro,'…'+dup(rr-rl-1,'Õ')+'ª');
          if maxyy0-a>un-ob+1 then fwrt(rl,ru,'”'+dup(rr-rl-1,'ƒ')+'Ω')
          else fwrt(rl,ru,'»'+dup(rr-rl-1,'Õ')+'º');
          mon;
        end;
        if not dontclear then
	  clwin(li,re,ob,un);
        redisplay:=false;
        redispfields:=true;
        redisptext:=true;
      end;

      if redispfields then
      begin
        ClearFields;
        showfldnames;
        FldDisplay;
        if @userdisp<>nil then
	  userdisp;
        redispfields:=false;
      end;

      if redisptext then
      begin
        textdisplay;
        redisptext:=false;
      end;

      afld:=fld[yp];
      with afld^ do begin
        setcur;                          {----- Feldwechsel: neues Feld -----}
        help_page:=mhelpnr+helpnr;
        if newfld then begin
          replace:=autohigh and doreplace;
          doreplace:=true;
          if typ<>feldtyp_button then test0(cont);
          if autoup then UpString(cont)
          else if autodown then LoString(cont);
          showfield;
          newfld:=false;
          if hpx<>0 then begin           { Hilfszeile anzeigen }
            s:=hpline;
            if hcenter then s:=center(s,hpl)
            else s:=forms(s,hpl);
            attrtxt(col.ColHelpTxt);
            mwrt(hpx,hpy,s);
            normtxt;
            end;
          if fnix<>0 then begin          { SelKey-Info anzeigen }
            if (selliste<>nil) or (@custom<>nil) then begin
              attrtxt(col.ColFnInfo);
              mwrt(fnix,fniy,fnkeyinfo);
              end
            else
              fillfnhelp;
            normtxt;
            end;
          end                            {-----------------------------------}
        else
          showfield;
        setcur;
        mauszuo:=yp>1;
        mauszuu:=yp<felder;
        if maussel then
          t:=selboxkey
        else
          if typ=feldtyp_button then
            get(t,curoff)
          else
          if insert_mode then
            get(t,curon)
          else
            get(t,cureinf);

        alt_t := AltBaseKey(t); // always lowercase

        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten;

        if (t=^J) and not quitfn(false,modified) then t:='';
        if (t=keyesc) and not quitfn(true,modified) then t:='';

        if ((t=keyins) and not (kb_shift or kb_ctrl)) then insert_mode:=not insert_mode;

        if (t=keyup) or (t=keystab) or ((t=keyleft)and(typ in [feldtyp_button])) then
          if yp>1 then movefield(-1)
          else
            if wrapmode=do_wrap then t:=keycpgd;

        if (typ=feldtyp_button) and (Length(t)=1) and (t[1] in ['A'..'Z','a'..'z']) then
          alt_t := LowerCase(t);

        if (Length(alt_t)=1) and (alt_t[1] in ['a'..'z']) then
          for i := 1 to felder do
            if (fld[i].hotkeypos>0) and (LowerCase(fld[i].hotkey) = alt_t) then
            begin
              SetFieldPos(i);
              t := '';
              if fld[i].typ = feldtyp_button then button_bearbeiten(fld[i]);
              break; // found, don't continue search
            end;

        if t='' then Continue;

        if (typ=feldtyp_button) and ((t=' ') or (t=keycr)) then
          button_bearbeiten(afld)
        else
        if (t=keydown) or (t=keytab) or (t=keycr) or ((t=keyrght)and(typ in [feldtyp_button]))  then
          if yp<felder then movefield(+1)
          else if wrapmode=do_wrap then t:=keycpgu
          else if (wrapmode=endonlast) and (t=keycr) then t:=^J;
        if (t=keyleft)and not(typ in [feldtyp_button]) then links;
        if (t=keyrght)and not(typ in [feldtyp_button]) then rechts;
        if (t=keyclft) and (ax+p>1) then
          repeat
            links
          until (ax+p=1) or ((cont[ax+p]<>' ') and (cont[ax+p-1]=' '));
        if (t=keycrgt) and (ax+p<=length(cont)) then
          repeat
            rechts
          until (ax+p>length(cont)) or
                ((ax+p=length(cont)) and (length(cont)=maxlen)) or
                ((cont[ax+p]<>' ') and (cont[ax+p-1]=' '));
        if t=keyhome then begin
          p:=1; ax:=0;
          replace:=false;
          end;
        if t=keyend then begin
          if len=maxlen then p:=min(length(cont)+1,len)
          else begin
            ax:=max(0,length(cont)-len+1);
            p:=length(cont)-ax+1;
            end;
          replace:=false;
          end;
        if (t=keybs) and (p+ax>1) then
          if mask<>'' then links
          else
            if replace then
              t:=^Y
            else begin
              s:=cont;
              delete(s,p+ax-1,1);
              if xtest1(s) then cont:=s;
              links;
              end;
        if (t=keydel) and (p+ax<=length(cont)) and (mask='') then
          if replace then t:=^Y
          else begin
            s:=cont;
            delete(s,p+ax,1);
            if xtest1(s) then cont:=s;
            end;
        if ((t=^T) or (t=#127)) and (mask='') then begin
          i:=p+ax;
          if t=^T then begin
            while (i<=length(cont)) and (cont[i]<>' ') do inc(i);
            while (i<=length(cont)) and (cont[i]=' ') do inc(i);
            end
          else begin
            while (p+ax>1) and (cont[p+ax-1]<>' ') do links;
            while (p+ax>1) and (cont[p+ax-1]=' ') do links;
            end;
          s:=cont;
          delete(s,p+ax,i-(p+ax));
          if xtest1(s) then cont:=s;
          replace:=false;
          end;
        if typ=feldtyp_string then
          if (t=^G) and not (autodown or topcase) then begin
            UpString(cont); replace:=false; end
            else
          if (t=^K) and not (autoup or topcase) then begin
            cont:=LowerCase(cont); replace:=false; end
            else
          if (t=^X) and not (autoup or autodown) then begin
            cont:=TopAllStr(UpperCase(cont)); replace:=false; end
            else
                                                              { JG: Clipboard fuer Menues }
          if t=^A then replace:=true;

          if (t=^C) or (t=keycins) then string2clip(cont);    { Ctrl-C / Ctrl-Ins = kopieren }

          if ((t=^V) or ((t=keyins) and kb_shift)             { Ctrl-V / Shift-Ins }
             or ((maskseekmenu>=yp) and (t=keySF2)))
            { and may_insert_clip } then
          begin   { STRG+V / SHIFT+INS }
            s:= Clip2String;
            i := length(s);
            if replace then
            begin                                             { Ersetzen }
              if (t=keySF2) then begin
                help_page:=ShiftF2Help;
                ShiftF2proc(s);
                end
              else s:=clip2string;
              p:=1; ax:=0; replace:=false;
            end else
            begin
              if t=keySF2 then begin
                help_page:=ShiftF2Help;
                ShiftF2proc(s);
                truncstr(s,maxlen-length(cont));
                end
              else s :=clip2string(* maxlen-length(cont),1 *);
              s:=LeftStr(cont,p+ax-1)+s+mid(cont,p+ax);
            end;

            p:=p+i;                                           {Cursorposition aktualisieren}
            if p>=len then
            begin
              ax:=ax+p-len;
              p:=len;
              end;
            if xtest1(s) then cont:=s;
            end;

          if { (delete_on_cDel) and } (t=keycdel)                 { MY: <Ctrl-Del> in MenÅs erlauben }
          then begin                                          {     (auch wenn Feld gesperrt)    }
            cont:=mask;
            if xtest1(cont) then;    { leer ist immer zulÑssig! }
            p:=1; ax:=0;
            replace:=false;
            cDel_pressed:=true;
//          if leave_on_cDel then t:=keyesc;
          end;

        if (t=keypgup) then
          if yp>1 then begin
            i:=yp; x:=fld[yp]^.yy;
            while (i>1) and (x-fld[i]^.yy<un-ob) do dec(i);
            { adiff:=(un-ob+1)-(x-fld[i]^.yy); }
            movefield(i-yp);
            {if (a>0) and (adiff>0) then begin
              a:=max(0,a-adiff);
              redisplay:=true;
              end;}
            end
          else
            if a>0 then begin
              a:=0;
              redisplay:=true;
              end;
        if (t=keypgdn) and ((yp<felder) or moretext) then begin
          gl:=un-ob+1;
          i:=yp; x:=fld[yp]^.yy;
          while (i<felder) and (fld[i]^.yy-x<gl-1) do inc(i);
          if i<>yp then
            movefield(i-yp);
          if (lasttexty>a+gl) and (fld[i]^.yy-x<gl-1) then begin
            inc(a,min(lasttexty-a-gl,gl-1-(fld[i]^.yy-x)));
            redisplay:=true;
            end;
          end;
        if t=keycpgu then movefield(1-yp);
        if t=keycpgd then movefield(felder-yp);

        if t=selboxkey then begin
          if selhelpnr<>0 then
            help_page:=selhelpnr;
          if selliste<>nil then begin
            select(brk,fwd);
            if not brk then begin
              if xtest1(cont) then;   { Select-Strings sind immer ok! }
              movefield(iif(fwd,+1,0));
              end;
            end
          else if @custom<>nil then
          begin
            getmem(crec,sizeof(customrec));
            system.fillchar(crec^,sizeof(customrec),0);
            with crec^ do begin
              acol:=col;
              x:=xx2; y:=yy;
              fpos:=yp;
              s:=cont;
              brk:=false;
              custom(crec^);
              if not brk then begin
                cont:=s;
                if xtest1(cont) then;
                movefield(+1);
                end;
              end;
            freemem(crec,sizeof(customrec));
            end;
          help_page:=mhelpnr+helpnr;
          maussel:=false;
          end
        else
          testfndef;

        if (counter>0) and ((t='-') or (t='+')) then
          _count_
        else if (t>=' ') and (typ<>feldtyp_button) then
        begin
          if replace then
            s:=mask
          else
            s:=cont;
          if autoup then t[1]:=UpCase(t[1]);
          if autodown then t[1]:=LoCase(t[1]);
          if convcolon and (t[1]=',') then t[1]:='.';
          if checkbutt and (t=' ') then
          begin
            if cont=yesno[1] then
              s:=yesno[2]
            else
              s:=yesno[1];
            if xtest1(s) then cont:=s;
          end
          else
          begin
            if allowed='' then
              valchar:=(pos(t,allchar)>0)
            else
              valchar:=(pos(t,allowed)>0);
            if valchar then
            begin
              if (mask='') and (insert_mode or (p+ax>length(s))) then
              begin
                insert(t,s,p+ax);
                s:=copy(s,1,maxlen);
              end else
                s[p+ax]:=t[1];
              if xtest1(s) then begin
                cont:=s;
                rechts;
                end;
              if replace then replace:=false;
              end;
            end;
          end;

        if (t=^Y) and not nonedit then begin
          cont:=mask;
          if xtest1(cont) then;      { leer ist immer zulÑssig! }
          p:=1; ax:=0;
          replace:=false;
          end;

        if (t=^J) and not quitfn(false,modified) then t:='';
        if (t=keyesc) and not quitfn(true,modified) then t:='';

        if ((t=^J) or ((t=keyesc) and keeponesc)) then begin
          exit_mask:=true;
          myp:=yp;
          yp:=1;
          while (yp<=felder) and
                (not fld[yp]^.enabled or valid(fld[yp],false)) do inc(yp);
          i:=yp; yp:=myp;
          if i<=felder then begin
            t:='';
            movefield(i-yp);
            end;
          end;
        end;
    until (t=keyesc) or (t=^J); { main loop }
    exitmask;

    if dopush then wpop;
    mauszuu:=mzu; mauszuo:=mzo;
    mauszul:=mzl; mauszur:=mzr;

    brk:=(t=keyesc);
    if not brk or keeponesc then
      returnfields;
    editing:=false;
    end;
  help_page:=helpsave;
end;   { readHmask }


procedure readmask(var brk:boolean);
// function readmask(var brk:boolean): integer;
begin
  readhmask(0,brk);
  // result := readhmask(0,brk);
end;

{ mask_helpnr = mhelpnr + afld^.helpnr }
function mask_helpnr: Integer;
begin
  mask_helpnr:=help_page;
end;

function readmask_active:boolean;
begin
  readmask_active:=(masks>0) and (amaskp^.editing);
end;


procedure readstring(x,y: Integer; const text:string; var s:string; displ,maxl: Integer;
                     const chml:string; var brk:boolean);
begin
  openmask(x,x+length(text)+displ+2,y,y,false);
  maskrahmen(0,0,0,0,0);
  maddstring(1,1,text,s,displ,maxl,chml);
  readmask(brk);
  closemask;
end;


function mmodified:boolean;
begin
  mmodified:=amaskp^.modified;
end;


{--------------- Externer Zugriff auf interne Felder ------------------}

procedure testfield(nr:integer);
begin
  with amaskp^ do
    if (nr<1) or (nr>felder) then
      error('illegal fieldpos: '+strs(nr));
end;


{ Inhalt eines Feldes direkt aendern  }
{ Diese Prozedur ist fuer den Einsatz }
{ durch TEST-Prozeduren gedacht      }

procedure setfield(nr: Integer; const newcont:string);
begin
  testfield(nr);
  with amaskp^ do begin
    with fld[nr]^ do
      cont:=LeftStr(newcont,maxlen);
    redispfields:=true;
    modified:=true;
    end;
end;

procedure set_chml(nr: Integer; chml:string);
begin
  testfield(nr);
  with amaskp^.fld[nr]^ do begin
    if chml<>'' then begin
      autoup:=(chml[1]='>');
      autodown:=(chml[1]='<');
      topcase:=(chml[1]='!');
      if autoup or autodown or topcase then DeleteFirstChar(chml);
      end
    else begin
      autoup:=false; autodown:=false; topcase:=false;
      end;
    if chml<>'' then
      allowed:=chml;
    end;
end;

{ Inhalt eines Feldes direkt abfragen }
{ siehe oben                          }

function getfield(nr: Integer):string;
begin
  testfield(nr);
  getfield:=amaskp^.fld[nr]^.cont;
end;


{ Waehrend des Aufbaus einer Maske liefert fieldpos die Nummer des }
{ letzten (aktuellen) Feldes. Waehrend der Eingabe liefert es die  }
{ Nummer des aktiven Eingabefeldes, z.B. fuer F1-Hilfen.           }

function fieldpos:integer;
begin
  with amaskp^ do
    if not editing then fieldpos:=felder
    else fieldpos:=yp;
end;

procedure setfieldpos(nr: Integer);
var i: integer;
begin 
  if amaskp^.editing then
  begin
    amaskp^.newfld := true;    { Kennzeichen: zur Zeit kein aktives Feld; }
                               {              Feld yp ist zu aktivieren   }
    for i := nr to amaskp^.felder do 
      if amaskp^.fld[i].enabled then begin amaskp^.yp := i; exit; end;
    for i := nr-1 downto 1 do 
      if amaskp^.fld[i].enabled then begin amaskp^.yp := i; exit; end;
  end;
  
  amaskp^.yp := nr;  
end;

{ Feld aktivieren/deaktivieren }

procedure setfieldenable(nr: Integer; eflag:boolean);
begin
  testfield(nr);
  with amaskp^ do
    with fld[nr]^ do
      if enabled<>eflag then begin
        enabled:=eflag;
        redispfields:=true;
        end;
end;

procedure setfieldnodisp(nr: Integer; dflag:boolean);
begin
  testfield(nr);
  with amaskp^ do
    with fld[nr]^ do
      if disnodisp<>dflag then begin
        disnodisp:=dflag;
        redispfields:=true;
        end;
end;

{ Feldbezeichnung aendern }

procedure setfieldtext(nr: Integer; const newtxt:string);
begin
  testfield(nr);
  with amaskp^.fld[nr]^ do begin
    txt:=newtxt;
    redispfields:=true;
  end;
end;


{ Textfeld aendern }

procedure settexttext(p:pointer; const newtxt:string);
begin
  with textnodep(p)^ do begin
    txt:=newtxt;
    redisptext:=true;
    end;
end;


procedure DefaultColor(var col:colrec);
begin
  with col do
    if color then begin
      ColBack:=7;
      ColFeldName:=3;
      ColDisabled:=8;
      ColFeldNorm:=7;
      ColFeldInput:=7;
      ColFeldActive:=$17;
      ColFeldMarked:=$20;
      ColArrows:=10;
      ColHelpTxt:=14;
      ColFnInfo:=3;
      ColFnFill:=3;
      ColSelBox:=$30;
      ColSelBar:=3;
      end
    else begin
      ColBack:=7;
      ColFeldName:=15;
      ColDisabled:=7;
      ColFeldNorm:=7;
      ColFeldInput:=7;
      ColFeldActive:=1;
      ColFeldMarked:=$70;
      ColArrows:=15;
      ColHelpTxt:=15;
      ColFnInfo:=7;
      ColFnFill:=7;
      ColSelBox:=$70;
      ColSelBar:=7;
      end;
end;


procedure masklanguage(const _yesno:string);               { 'JN' }
begin
  yesno:=_yesno;
end;

procedure mscroll(distance: Integer);
begin
  with amaskp^ do begin
    if assigned(stat.scrollfn) then begin
      if stat.scrollfn(distance) then
      begin
        redisplay := true;
        newfld := true;
      end;
    end;

    redispfields := true;    
  end;  
end;

procedure mquit(brk: boolean);
begin
  amaskp^.done    := true;
  amaskp^.return  := iif(brk,-1,1);
end;

procedure mquit(returncode: integer);
begin
  amaskp^.done    := true;
  amaskp^.return  := returncode;
end;

{ Der Status der nullten Maske dient als Prototyp fuer alle }
{ weiteren Masken. Er kann daher zu Beginn - amask=0 -     }
{ ueber die maskset*-Funktionen eingestellt werden.         }

procedure InitMaskeUnit;
begin
  masks:=0; amask:=0;
  system.fillchar(mask[0]^,sizeof(masktyp),0);
  amaskp:=mask[0];
  with mask[0]^.Stat do
  begin
    rahmentyp:=0;
    rl := 0; rr := 0; ro := 0; ru := 0;
    hcenter := false;
    keeponesc:=false; autoselbox:=false;
    hpx:=0; hpy:=0; hpl:=0;
    fnix:=0; fniy:=0; fnkeyinfo:='';
    fnkeyfill := #0; // !!?
    selboxkey:=keyf2;
    fillchar:=' ';
    DefaultColor(col);
    wrapmode:=dont_wrap;
    autojump:=5;
    arrowspace := false;
    mausarrows := false;
    selcursor := false;
    dontclear := false; checkbutts := false;
    quitfn:=qdummyf;
    fautohigh:=true;
  end;
  with Mask[0]^do
    system.fillchar(uda,sizeof(uda),0);
end;

initialization
  Getmem(Mask[0],sizeof(masktyp));
finalization
  FreeMem(Mask[0]);
{
  $Log: maske.pas,v $
  Revision 1.48  2003/09/06 22:59:25  cl
  - added return value from read[h]mask to identify pressed button or value
    passed to MQuit (instead of boolean brk)

  Revision 1.47  2003/08/28 01:14:15  mk
  - removed old types s20, s40, s60 and s80

  Revision 1.46  2002/12/21 05:37:51  dodi
  - removed questionable references to Word type

  Revision 1.45  2002/12/14 07:31:27  dodi
  - using new types

  Revision 1.44  2002/12/12 11:58:40  dodi
  - set $WRITEABLECONT OFF

  Revision 1.43  2002/12/04 16:56:59  dodi
  - updated uses, comments and todos

  Revision 1.42  2002/11/14 20:04:20  cl
  - Added button controls

  Revision 1.41  2002/08/19 23:03:13  cl
  - Schaltflaechen in Masken (Teil I)

  Revision 1.40  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.39  2002/06/23 13:48:40  cl
  - Allow modification of fieldpos on creation of masks

  Revision 1.38  2002/04/14 22:10:12  cl
  - added:
      procedure masksetscrollfunc(scrollfn:scrollfunc);
      procedure setfieldpos(nr: Integer);
      procedure mscroll(distance: Integer);
      procedure mquit(brk: boolean);
    for better external control of masks

  Revision 1.37  2002/01/13 15:07:23  mk
  - Big 3.40 Update Part I

  Revision 1.36  2001/12/09 14:36:40  mk
  - implemented SysBeep and error sounds

  Revision 1.35  2001/10/17 14:00:24  mk
  - fixed range check error in maddstring
  - added some const parameters
  - changed some byte to integer, saves about 3kb code

  Revision 1.34  2001/09/27 23:04:03  mk
  - moved variable initialization to initialization-part to avoid crashes
    in finalization parts with rc and ihs

  Revision 1.33  2001/09/21 16:16:47  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.32  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.31  2001/09/08 16:29:29  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.30  2001/09/08 14:15:50  cl
  - fix for procedure MDisabledNodisplay;

  Revision 1.29  2001/08/11 23:06:27  mk
  - changed Pos() to cPos() when possible

  Revision 1.28  2001/07/31 13:10:31  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.27  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.26  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.25  2001/01/04 10:10:47  mk
  - changed some byte to integer

  Revision 1.24  2000/12/28 14:45:00  mk
  CL:- first things for UUCP over IP

  Revision 1.23  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.22  2000/10/17 10:05:42  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.21  2000/07/27 10:12:59  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.20  2000/07/22 10:15:36  hd
  - Variant-Typ by Maske

  Revision 1.19  2000/07/21 17:39:51  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.18  2000/07/21 13:14:09  hd
  - Fix: Strings in der Maske
  - Fix: Einige Datenbankzugriffe wegen AnsiString

  Revision 1.17  2000/07/20 16:49:56  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.16  2000/07/19 13:42:48  hd
  - new/dispose durch get-/freemem ersetzt

  Revision 1.15  2000/07/17 13:30:00  mk
  - AnsiString Updates

  Revision 1.14  2000/07/15 18:29:55  ml
  - Ansistring + NilStringzugriffBug

  Revision 1.13  2000/07/13 10:23:44  mk
  - Zeiger auf Strings entfernt

  Revision 1.12  2000/07/12 14:55:03  hd
  - Ansistring

  Revision 1.11  2000/07/03 16:20:02  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.10  2000/07/03 13:31:38  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.9  2000/06/22 19:53:26  mk
  - 16 Bit Teile ausgebaut

  Revision 1.8  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/13 12:48:32  mk
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
end.


{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

{$I XPDEFINE.INC }

unit  maske;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  sysutils,
  typinfo, xp0,
  typeform,keys,inout,maus2,winxp,montage, clip;

const digits : string{[12]} = '-0123456789 ';
      allchar = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXY'+
                'Z[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñ'+
                'óòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛';
      hexchar = '<0123456789abcdef';

      mtString   = 1;
      mtShortint = 2;
      mtByte     = 3;
      mtInteger  = 4;
      mtWord     = 5;
      mtLongint  = 6;

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
                    ColFnInfo,        { Info-Text fÅr SekKey  }
                    ColFnFill,        { kein Info-Text...     }
                    ColSelBox,        { Auswahlbox            }
                    ColSelBar,        { Auswahlbalken A.-Box  }
                    ColButtons: byte; { Check/Radio-Buttons   }
                  end;

      customrec = record
                    acol : colrec;    { aktuelle Farben }
                    x,y  : byte;      { aktuelle Pos. des Feldinhalts }
                    fpos : integer;   { akt. Feldnummer }
                    s    : string;    { var: Feldinhalt }
                    brk  : boolean;   { var }
                  end;

      testfunc    = function(var inhalt:string):boolean;
      testproc    = procedure(var inhalt:string);
      customsel   = procedure(var cr:customrec);
      quitfunc    = function(brk,modif:boolean):boolean;
      userdproc   = procedure;

      wrapmodes   = (dont_wrap,do_wrap,endonlast);


{-------------- allgemeine Funktionen -------------}

procedure openmask(l,r,o,u:byte; pushit:boolean);   { neue Maske îffnen }
procedure readmask(var brk:boolean);                { *** Einlesen ***  }
procedure readHmask(mhelpnr:word; var brk:boolean); { .. mit Hilfsseiten }
function  mmodified:boolean;                        { Inhalt geÑndert   }
procedure closemask;                                { Maske schlie·en   }
procedure readstring(x,y:byte; text:string; var s:string; displ,maxl:byte;
                     chml:string; var brk:boolean);
procedure mbeep;
procedure DefaultColor(var col:colrec);             { col <- Default    }
procedure masklanguage(_yesno:string);              { 'JN'              }

procedure mdummyp(var inhalt:string);               { Dummy fÅr Test0   }
function  mdummyf(var inhalt:string):boolean;       { Dummy fÅr Test1/2 }
function  qdummyf(brk,modif:boolean):boolean;       { Dummy fÅr QuitFN  }


{--------------- Masken-Einstellungen -------------}
{ beziehen sich auf die jeweils aktuelle Maske und }
{ werden in amaskp^.stat abgelegt                  }

procedure maskcol(cols:colrec);              { Farben der akt. Maske setzen }
procedure maskrahmen(rtyp,l,r,o,u:byte);     { Rahmentyp setzen }
procedure masksetstat(keepon_esc,autosel:boolean; selkey:taste);
procedure masksetfillchar(c:char);           { FÅllzeichen setzen }
procedure masksethelp(hx,hy,hl:byte; center:boolean);   { Hilfszeile einst. }
procedure masksetfninfo(x,y:byte; text:string; fillc:char);
procedure masksetwrapmode(wm:wrapmodes);
procedure masksetautojump(aj:byte);     { Sprungweite bei cr am unteren Rand }
procedure masksetqfunc(qfunc:quitfunc);
procedure masksetarrowspace(aas:boolean);
procedure masksetmausarrows(ma:boolean);
procedure masksetautohigh(ah:boolean);  { Felder automatisch selektieren }
procedure maskdontclear;
procedure maskcheckbuttons;
procedure maskselcursor(cur:curtype);
procedure maskUpDownArrows(x1,y1,x2,y2:byte; fill:char; col:byte);


{------------ Felder anlegen ------------}
{ werden an die aktuelle Maske angehÑngt }

{ Integer-Typen: 2 = ShortInt, 3 = Byte, 4 = Integer, 5 = Word, 6 = LongInt }

procedure Maddtext(x,y:integer; text:string; att:byte);   { Anzeigetext anfÅgen }
procedure Maddstring(x,y:integer; text:string; var s:string; displ,maxl:integer;
                     chml:string);
procedure Maddint(x,y: integer; text:string; var int; ityp,displ:integer;
                     imin,imax:longint);
procedure Maddreal(x,y: integer; text:string; var r:real; displ,rnk : integer;
                     rmin,rmax : real);
procedure Maddbool(x,y:integer; text:string; var b:boolean);
procedure Maddform(x,y:integer; text:string; var s:string; form,chml:string);
procedure Madddate(x,y:integer; text:string; var d:string; long,mbempty:boolean);
procedure Maddtime(x,y:integer; text:string; var t:string; long:boolean);
procedure Maddcustomsel(x,y:integer; text:string; var s:string; displ:integer;
                        cp:customsel);


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
procedure MH(text:string);                    { Hilfszeile setzen }
procedure MHnr(helpnr:word);                  { Hilfsseiten-Nr. setzen }
procedure MSelHnr(helpnr:word);               { Hilfsseite fÅr <F2> }
procedure MSetSel(sx,sy,slen:byte);           { Abmessungen der SelListe }
procedure MAppSel(force:boolean; s:string);   { SelBox aufbauen }
procedure Mappcustomsel(cp:customsel; nedit:boolean);
procedure Mnotrim;                            { kein autotrim }
procedure Malltrim;                           { rtrim/ltrim }
procedure Mspecialcol(attr:byte);             { spez. Farbe fÅr Feldname }
procedure MSetAutoHigh(ah:boolean);           { automat. selektieren }



{----------------- Externe Funktionen --------------}
{ dienen zum Zugriff von externen (Test-)Funktionen }
{ auf Inhalte der momentan editierten Maske         }

procedure setfield(nr:word; newcont:string);
function  getfield(nr:word):string;
function  fieldpos:integer;         { akt.FeldNr, auch wÑhrend Maskenaufbau! }
procedure setfieldenable(nr:word; eflag:boolean);   { Feld (de)aktivieren }
procedure setfieldnodisp(nr:word; dflag:boolean);   { Feld nicht anzeigen }
function  mask_helpnr:word;
function  readmask_active:boolean;
procedure set_chml(nr:word; chml:string);
procedure setfieldtext(nr:word; newtxt:string);
function  mtextpos:pointer;
procedure settexttext(p:pointer; newtxt:string);
procedure mclearsel(nr:word);
procedure mappendsel(nr:word; force:boolean; s:string);


procedure InitMaskeUnit;

implementation  {---------------------------------------------------------}

const maxmask   = 10;                { max. gleichzeitig offene Masken }
      maxfields = 140;               { max. Felder pro Maske           }

      insert_mode : boolean = true;
      help_page   : word = 0;        { Helpnr des Eingabefeldes }
      yesno       : string[2] = 'JN';

type
      { Achtung! Pointer in MaskStat mÅssen in OpenMask }
      {          gesondert behandelt werden!            }

      maskstat = record
                   col         : colrec;
                   rahmentyp   : byte;     { 0=keiner, 1/2/3, 4=wechselnd }
                   rl,rr,ro,ru : byte;     { Rahmen-Koordinaten }
                   hpx,hpy,hpl : byte;     { Position/Len Hilfstexte }
                   hcenter     : boolean;  { Hilfstexte zentrieren }
                   keeponesc   : boolean;  { Eingaben trotz Esc behalten }
                   autoselbox  : boolean;  { Auswahlbox automatisch îffnen }
                   fillchar    : char;     { FÅllzeichen bei Eingabe }
                   selboxkey   : taste;    { '' -> keine SelBox; Def: F2 }
                   fnix,fniy   : byte;     { Position der FNKey-Info }
                   fnkeyinfo   : string;
                   fnkeyfill   : char;
                   wrapmode    : wrapmodes;
                   autojump    : byte;    { Zeilensprung bei verl.d.Fensters }
                   quitfn      : quitfunc;
                   arrowspace  : boolean;  { Leerzeichen vor/hinter Feld }
                   mausarrows  : boolean;
                   fautohigh   : boolean;  { Felder automat. selektieren }
                   dontclear   : boolean;  { Fenster nicht lîschen }
                   checkbutts  : boolean;
                   Userdisp    : userdproc;  { bei Bild-Neuaufbau          }
                   selcursor   : boolean;
                 end;

      udarec   = record
                   x1,y1,x2,y2 : byte;
                   fillc       : char;
                   color       : byte;
                 end;

      selnodep = ^selnode;
      selnode  = record                    { Knoten fÅr Select-Liste }
                   next        : selnodep;
                   el          : string;
                 end;

      textnodep= ^textnode;
      textnode = record                    { Knoten fÅr Anzeigetext-Liste }
                   next        : textnodep;
                   txt         : string;
                   xx,yy,attr  : byte;
                 end;

      feldrec  = record
                   enabled     : boolean;
                   disnodisp   : boolean;
                   txt         : string;   { Feld-Text }
                   typ         : byte;     { Feldtyp }
                   variable    : variant;  { Adresse der Variablen }
                   xx,yy,len   : byte;     { Position, AnzeigelÑnge }
                   yy0,xx2     : byte;     { Position des Inhalts }
                   maxlen      : byte;     { maximale LÑnge des Inhalts }
                   cont        : string;   { Feldinhalt }
                   allowed     : string;   { erlaubte Zeichen }
                   mask        : string[20];  { Masken-String }
                   autoup,
                   autodown,
                   topcase     : boolean;    { automatische Gro·/Kleinschr.}
                   convcolon   : boolean;    { automatisch "," -> "." }
                   _min,_max   : longint;
                   _rmin,_rmax : real;
                   nk          : byte;       { Nachkommastellen bei Real   }
                   test0       : testproc;   { vor jedem Editieren         }
                   test1       : testfunc;   { bei jeder Ñndernden Eingabe }
                   test2       : testfunc;   { vor Verlassen des Feldes    }
                   test3       : testproc;   { bei Verlassen des Feldes    }
                   hpline      : string;
                   helpnr      : word;       { Hilfsseiten-Nr. }
                   selhelpnr   : word;       { Hilfsseite bei <F2> }
                   selliste    : selnodep;
                   hassel      : boolean;
                   slx,sly,sll : byte;       { SListen-Position/LÑnge }
                   slmin       : byte;       { minimale ListenlÑnge }
                   noslpos     : boolean;    { slx..sll noch nicht gesetzt }
                   forcesll    : boolean;
                   pempty      : boolean; { Formatierter Str. darf leer sein }
                   custom      : customsel;  { eigene Select-Prozedur }
                   nonedit     : boolean;    { Feld nicht editierbar }
                   autotrim    : byte;       { 0=nein, 1=r, 2=r+l }
                   owncol      : boolean;    { spezielle Farbe fÅr Feldname }
                   ownattr     : byte;
                   autohigh    : boolean;    { Feld autom. selektieren }
                   counter     : byte;       { 1/2 -> "+"/"-" bei Datum/Zeit }
                   checkbutt   : boolean;    { Check-Button }
                 end;
      feldp    = ^feldrec;

      masktyp  = record
                   stat        : maskstat;
                   li,re,ob,un : byte;        { Arbeitsbereich }
                   dopush      : boolean;     { Inhalt sichern }
                   felder      : byte;        { Anzahl Felder  }
                   fld         : array[1..maxfields] of feldp;
                   mtxt        : textnodep;
                   maxyy0      : byte;        { grî·ter Y-Wert }
                   yp,a        : integer;     { akt. Feldnr./Offset }
                   modified    : boolean;     { Inhalt geÑndert }
                   editing     : boolean;     { Editieren aktiv }
                   uda         : udarec;     { Pfeile bei scrollbaren Masken }
                 end;
      maskp    = ^masktyp;


var   mask    : array[0..maxmask] of maskp;
      masks   : byte;
      amask   : byte;       { aktuelle Maske, z.Zt. immer = masks! }
      amaskp  : maskp;      { mask[amask] }
      lastfld : feldp;      { aktuelles Feld wÑhrend des Maskenaufbaus }

      redispfields : boolean;
      redisptext   : boolean;


{ Feldtypen:   1=String, 2=Short, 3=Byte, 4=Integer, 5=Word, 6=Long,
               7=Real, 8=Datum (tt.mm.jj oder tt.mm.jjjj),
               9=Uhrzeit (hh:mm oder hh:mm:ss), 10=Boolean (J/N)  }


procedure error(txt:string);
begin
  writeln('MASK: ',txt);
  halt(1);
end;

procedure mbeep;
begin
{!!  sound(600);
  SysDelay(20);
  nosound; }
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

{ neue Maske îffnen, falls noch Handles frei   }
{ der Maskenstatus wird von mask[0] Åbernommen }

procedure openmask(l,r,o,u:byte; pushit:boolean);
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
    end;
  lastfld:=nil;
end;


procedure mclearsel(nr:word);
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


{ aktuelle (oberste) Maske schlie·en }

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
  4 = automatisch Ñndern beim durchscrollen }

procedure maskrahmen(rtyp,l,r,o,u:byte);
begin
  with amaskp^.stat do begin
    rahmentyp:=rtyp;
    rl:=l; rr:=r; ro:=o; ru:=u;
    end;
end;


{ diverse Status-Flags setzen                       }
{ keepon_esc:  Feldinhalt auch bei 'brk' Åbernehmen }
{ autosel:     SelListen automatisch îffnen   (nni) }
{ selkey:      Taste fÅr SelListen                  }

procedure masksetstat(keepon_esc,autosel:boolean; selkey:taste);
begin
  with amaskp^.stat do begin
    keeponesc:=keepon_esc;
    autoselbox:=autosel;
    selboxkey:=selkey;
    end;
end;


{ FÅllzeichen fÅr Rest der Zeile setzen }

procedure masksetfillchar(c:char);
begin
  amaskp^.stat.fillchar:=c;
end;


{ Hilfszeile einstellen      }
{ hx = 0 -> keine Hilfszeile }

procedure masksethelp(hx,hy,hl:byte; center:boolean);
begin
  with amaskp^.stat do begin
    hpx:=hx;
    hpy:=hy;
    hpl:=hl;
    hcenter:=center;
    end;
end;


{ Info-Text fÅr SelKey einstellen }

procedure masksetfninfo(x,y:byte; text:string; fillc:char);
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
{             Ctrl-Enter ausfÅhren              }

procedure masksetwrapmode(wm:wrapmodes);
begin
  amaskp^.stat.wrapmode:=wm;
end;


{ AutoJump gibt an, um wieviele Zeilen die Maske }
{ automatisch weiterspringen soll, wenn sie nach }
{ unten mit Return verlassen wird.               }

procedure masksetautojump(aj:byte);
begin
  amaskp^.stat.autojump:=aj;
end;


{ Masken-Fenster  wird zu Beginn nicht gelîscht }

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


procedure MaskUpDownArrows(x1,y1,x2,y2:byte; fill:char; col:byte);
begin
  amaskp^.uda.x1:=x1;
  amaskp^.uda.y1:=y1;
  amaskp^.uda.x2:=x2;
  amaskp^.uda.y2:=y2;
  amaskp^.uda.fillc:=fill;
  amaskp^.uda.color:=col;
end;


{ QFunc wird vor Beenden der Eingabe aufgerufen }
{ kann diese verhindern; Åbergebene Parameter:  }
{ brk:    Beenden durch Esc-Taste               }
{ modif:  Feldinhalt wurde geÑndert             }

procedure masksetqfunc(qfunc:quitfunc);
begin
  amaskp^.stat.quitfn:=qfunc;
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


{----------------- Felder anfÅgen -------------------}

{ reinen Anzeigetext anfÅgen }
{ attr=0 -> ColFeldName      }

procedure Maddtext(x,y:integer; text:string; att:byte);
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


function mtextpos:pointer;
begin
  mtextpos:=amaskp^.mtxt;
end;


procedure setall(var text:string; x,y:byte; addblank:boolean);
var
   newfld : feldp;
begin
  with amaskp^ do
    if felder=maxfields then
      error('no more fields')
    else begin
      inc(felder);
      getmem(newfld,sizeof(feldrec));
      system.fillchar(newfld^,sizeof(feldrec),0);
      fld[felder] := newfld;
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
        variable:=nil;
        custom:=nil; nonedit:=false;
        allowed:= ''; owncol:=false;
        counter:=0; noslpos := false;
        checkbutt:=false;
        enabled:=true;
        if text='' then addblank:=false;
        txt:=text+iifs(addblank,' ','');
        typ := 0;
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
        forcesll:=true; slmin:=5;    { minimale SelListen-LÑnge }
        autotrim:=1;
        autohigh:=stat.fautohigh;
        if (felder>1) and (fld[felder-1]^.helpnr>0) then
          helpnr:=fld[felder-1]^.helpnr+1;
        end;
      end;
end;


{ String anfÅgen -----------------------------------------}
{ chml = ''  -> alle Zeichen erlaubt                      }
{ Das erste Zeichen von chml wird gesondert ausgewertet:  }
{ '>'  ->  automatische Umwandlung in Gro·buchstaben      }
{ '<'  ->  automatische Umwandlung in Kleinbuchstaben     }
{ '!'  ->  automatische Gro·schreibung des 1. Buchstabens }

procedure Maddstring(x,y:integer; text:string; var s:string; displ,maxl:integer;
                     chml:string);
var p : byte;
begin
  setall(text,x,y,true);
  with lastfld^ do begin
    typ:=1;
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


{ Integer AnfÅgen
  Typ 2 = ShortInt, 3 = Byte, 4 = Integer, 5 = Word, 6 = LongInt }

procedure Maddint(x,y:integer; text:string; var int; ityp,displ:integer;
                  imin,imax:longint);
var l : longint;
    s : s40;
begin
  if (ityp<2) or (ityp>6) then
    error('illegal Int type');

  setall(text,x,y,true);
  with lastfld^ do begin
    typ:=ityp;
    variable:=@int;
    len:=displ{+1}; maxlen:=displ;
    case ityp of
      2 : l:=shortint(int);
      3 : l:=byte(int);
      4 : l:=integer16(int);
      5 : l:=smallword(int);
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


{ Real anfÅgen }

procedure Maddreal(x,y:integer; text:string; var r:real; displ,rnk :integer;
                   rmin,rmax : real);
var s : s40;
begin
  setall(text,x,y,true);
  with lastfld^ do begin
    typ:=7;
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


{ Bool-Wert anfÅgen }

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
    typ:=10;
    variable:=@b;
    len:=1; maxlen:=1;
    cont:=iifc(b,yesno[1],yesno[2]);
    autoup:=true;
    allowed:='>'+yesno;
    autohigh:=false;
    end;
end;


{ Formatierten String anfÅgen                               }
{ Eingaben kînnen Åberall erfolgen, wo im Format ' ' steht. }
{ Alle anderen Stellen werden aus dem Format Åbernommen.    }
{ Wenn s='', dann wird s:=form gesetzt.                     }

procedure Maddform(x,y:integer; text:string; var s:string; form,chml:string);
begin
  if s='' then s:=form;
  MAddString(x,y,text,s,length(form),length(form),chml);
  with lastfld^ do begin
    mask:=form;
    autotrim:=0;
    autohigh:=false;
    end;
end;


{ Datum anfÅgen               }
{ long -> langes Datumsformat }
{ mbempty -> may be empty     }

procedure Madddate(x,y:integer; text:string; var d:string; long,mbempty:boolean);
begin
  Maddform(x,y,text,d,iifs(long,'  .  .    ','  .  .  '),' 0123456789');
  with lastfld^ do begin
    typ:=8;
    pempty:=mbempty;
    counter:=1;
    end;
end;


{ Uhrzeit anfÅgen }

procedure Maddtime(x,y:integer; text:string; var t:string; long:boolean);
begin
  Maddform(x,y,text,t,iifs(long,'  :  :  ','  :  '),'0123456789');
  lastfld^.typ:=9;
  lastfld^.counter:=2;
end;


{ Feld mit beliebiger eigener Select-Routine anfÅgen  }
{ s : Feldinhalt zu Beginn; wird von cp Åberschrieben }
{ displ : Anzeige-LÑnge (wg. forms)                   }
{ Das Feld ist nicht mehr editierbar!                 }

procedure Maddcustomsel(x,y:integer; text:string; var s:string; displ:integer;
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


{ Valid-Fnuktion setzen }

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

procedure MH(text:string);
begin
  if testlast then
    with lastfld^ do
      hpline:=text;
end;


procedure MHnr(helpnr:word);
begin
  if testlast then
    lastfld^.helpnr:=helpnr;
end;


procedure MSelHnr(helpnr:word);
begin
  if testlast then
    lastfld^.selhelpnr:=helpnr;
end;


{ Position und LÑnge (gl) der SelListe einstellen }
{ Ist xp=0, so wird die Position weiterhin automa-}
{ tisch eingestellt, weobei die Liste mindestens  }
{ len Zeilen lang ist.                            }

procedure MSetSel(sx,sy,slen:byte);
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


{ Neue Zeilen an eine Select-Liste anhÑngen            }
{ s kann mehrere durch "˘" getrennte Strings enthalten }
{ Ist force=true, so wird anhand der ListeneintrÑge    }
{ eine Valid-öberprÅfung durchgefÅhrt. Force wird bei  }
{ jedem Aufruf Åberschrieben; es ist also bei mehreren }
{ MAppSel und das letzte 'force' von Bedeutung.        }
{                                                      }
{ _mappsel:   interne Prozedur                         }
{ MappSel:    beim Maskenaufbau                        }
{ MAppendSel: nachtrÑglich                             }


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
      if (typ>=2) and (typ<=6) then
        str(ival(s1):maxlen,s1)
      else if typ=7 then
        str(rval(s1):maxlen:nk,s1);
      s:=Mid(s,p+1);
      app(selliste);
      end;
    forcesll:=force;
    hassel:=true;
    end;
end;


procedure mappendsel(nr:word; force:boolean; s:string);
begin
  testfield(nr);
  _mappsel(amaskp^.fld[nr],force,s);
end;


procedure MAppSel(force:boolean; s:string);
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


{ spzeielle Farbe fÅr Feldnamen einstellen }

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


{$I maske.inc}     { - Hauptprogramm - }


procedure readmask(var brk:boolean);
begin
  readhmask(0,brk);
end;

{ mask_helpnr = mhelpnr + afld^.helpnr }
function mask_helpnr:word;
begin
  mask_helpnr:=help_page;
end;

function readmask_active:boolean;
begin
  readmask_active:=(masks>0) and (amaskp^.editing);
end;


procedure readstring(x,y:byte; text:string; var s:string; displ,maxl:byte;
                     chml:string; var brk:boolean);
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


{ Inhalt eines Feldes direkt Ñndern  }
{ Diese Prozedur ist fÅr den Einsatz }
{ durch TEST-Prozeduren gedacht      }

procedure setfield(nr:word; newcont:string);
begin
  testfield(nr);
  with amaskp^ do begin
    with fld[nr]^ do
      cont:=LeftStr(newcont,maxlen);
    redispfields:=true;
    modified:=true;
    end;
end;

procedure set_chml(nr:word; chml:string);
begin
  testfield(nr);
  with amaskp^.fld[nr]^ do begin
    if chml<>'' then begin
      autoup:=(chml[1]='>');
      autodown:=(chml[1]='<');
      topcase:=(chml[1]='!');
      if autoup or autodown or topcase then delete(chml,1,1);
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

function getfield(nr:word):string;
begin
  testfield(nr);
  getfield:=amaskp^.fld[nr]^.cont;
end;


{ WÑhrend des Aufbaus einer Maske liefert fieldpos die Nummer des }
{ letzten (aktuellen) Feldes. WÑhrend der Eingabe liefert es die  }
{ Nummer des aktiven Eingabefeldes, z.B. fÅr F1-Hilfen.           }

function fieldpos:integer;
begin
  with amaskp^ do
    if not editing then fieldpos:=felder
    else fieldpos:=yp;
end;


{ Feld aktivieren/deaktivieren }

procedure setfieldenable(nr:word; eflag:boolean);
begin
  testfield(nr);
  with amaskp^ do
    with fld[nr]^ do
      if enabled<>eflag then begin
        enabled:=eflag;
        redispfields:=true;
        end;
end;

procedure setfieldnodisp(nr:word; dflag:boolean);
begin
  testfield(nr);
  with amaskp^ do
    with fld[nr]^ do
      if disnodisp<>dflag then begin
        disnodisp:=dflag;
        redispfields:=true;
        end;
end;

{ Feldbezeichnung Ñndern }

procedure setfieldtext(nr:word; newtxt:string);
begin
  testfield(nr);
  with amaskp^.fld[nr]^ do begin
    txt:=newtxt;
    redispfields:=true;
  end;
end;


{ Textfeld Ñndern }

procedure settexttext(p:pointer; newtxt:string);
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


procedure masklanguage(_yesno:string);               { 'JN' }
begin
  yesno:=_yesno;
end;


{ Der Status der nullten Maske dient als Prototyp fÅr alle }
{ weiteren Masken. Er kann daher zu Beginn - amask=0 -     }
{ Åber die maskset*-Funktionen eingestellt werden.         }

procedure InitMaskeUnit;
begin
  masks:=0; amask:=0;
  getmem(mask[0],sizeof(masktyp));
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


{
  $Log$
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


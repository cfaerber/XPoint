{   $Id$

    OpenXP - generic routines
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

{$I xpdefine.inc }


unit xp1;

interface

uses
  classes,
  sysutils,
{$IFDEF Unix }
{$IFDEF Kylix}
  libc,
  xplinux,
{$ELSE}
  linux,
{$ENDIF}
{$ENDIF }
{$IFDEF FPC }
  {$IFDEF Win32 }
    Windows,
  {$ENDIF }
{$ENDIF }
  typeform,
  keys, //taste
  inout,datadef,help,lister,
  xp0,winxp,
  xpglobal;

const maxhidden  = 500;                 { max. versteckte Menuepunkte }

var   DisableDOS : boolean = false;
      shellkey   : boolean = false;
      ListMakros : byte    = 0;         { Flag fuer XPKEYS.XMakro     }
      Errorlevel : integer = 0;
      miscbase   : DB      = nil;       { wird bei Shell geschlossen }
      menurestart: boolean = false;     { fuer Config-Menue            }

type mprec     = record
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
{$IFDEF NCRT }
     scrptr    = TxpHandle;  { Handle }
{$ELSE }
     scrptr    = Pointer;
{$ENDIF }
     ahidden   = array[1..maxhidden] of SmallInt;

var printlines : longint;
    WaitKey    : taste;               { Taste, mit der wkey beendet wurde }
    llh        : boolean;             { "L"/"H" im Lister -> xp1o.listExt }
                                      { == Nachrichten-Lister             }
    rbx,rby    : Integer;             { Cursorposition fuer ReadButton     }
    hidden     : ^ahidden;            { Liste der unsichtbaren Menuepkte.  }
    anzhidden  : SmallInt;            { Anzahl der unsichtbaren Menuepkte. }


procedure XpIdle;

procedure showscreen(newmode:boolean);
procedure showusername;
procedure exitscreen(joke:shortint);
procedure showmain(nr:shortint);      { Hauptmenue anzeigen: nr=Position  }
function  mainkey(p:byte):taste;
procedure freemain;
procedure wait(cur:curtype);

procedure sichern(var sp:scrptr);
procedure holen(var sp:scrptr);

procedure hlp(nr:xpWord);             { setzt helpst[helpstp] }
procedure pushhp(nr:xpWord);
procedure pophp;
procedure freehelp;

procedure setenable(mnu,nr:byte; flag:boolean);
procedure setmenup(mnu:string; nr:byte; anew:string);
procedure setmenupos(mnu:string; newpos:byte);
procedure splitmenu(nr:byte; ma:map; var n:integer; nummern:boolean);

procedure SetExtraktMenu;
function  getmenu(nr:byte; enterkey:taste; x,y:integer):integer;
procedure setscreensize;
procedure lines(fnkey:byte);   { setzt gl usw. }
procedure xp_maus_aus;
procedure xp_maus_an(x,y: integer);
procedure SetMausEmu;

procedure blindon(total:boolean);
procedure blindoff;
procedure getpos(width,height: Integer; var x,y: Integer);
procedure openbox(width,height:Integer; const txt:string; var x,y: Integer; c1,c2: Integer);
procedure openboxat(width,height:Integer; const txt:string; x,y: Integer; c1,c2: Integer);
procedure msgbox(width,height: Integer; const txt:string; var x,y: Integer);
procedure diabox(width,height: Integer; const txt:string; var x,y: Integer);
procedure selbox(width,height: Integer; const txt:string; var x,y: Integer; hell:boolean);
function listbox(width,height: Integer; const txt:string): TLister;
procedure ListboxCol(List: TLister);
procedure utilbox(l,r,o,u: Integer; const txt:string);
procedure dialog(width,height: Integer; const txt:string; var x,y: Integer);
procedure enddialog;
procedure closebox;
procedure moment;
procedure message(const txt:string);
procedure rmessage(nr:xpWord);
procedure WaitIt(txt:atext; p:proc; sec:xpWord);
procedure WriteClipFile(fn:string);
procedure selcol;
procedure file_box(var name:string; changedir:boolean);
procedure XP_testbrk(var brk:boolean);

procedure errsound;
function  _errsound:boolean;
procedure signal;              { s. Config/Anzeige/Hilfen }
procedure fehler(const txt:string);
procedure rfehler(nr:xpWord);
procedure rfehler1(nr:xpWord; txt:string);
procedure hinweis(const txt:string);
function  mfehler(b:boolean; const txt:string):boolean;
function  fehlfunc(const txt:string):boolean;
procedure logerror(const txt:string);
procedure tfehler(const txt:string; sec:integer);
procedure trfehler(nr:xpWord; sec:integer);
procedure trfehler1(nr:xpWord; const txt:string; sec:integer);
procedure afehler(const txt:string; auto:boolean);
procedure arfehler(nr:xpWord; auto:boolean);
procedure interr(const txt:string);
function  ioerror(i:integer; const otxt:atext):atext;

procedure shell(const prog:string; space:xpWord; cls:shortint);  { externer Aufruf }

{ Execute an external program and add any files created in current dir to SL }
function ShellNTrackNewFiles(prog:string; space:xpWord; cls:shortint; SL: TStringList): Integer;

function  listfile(name,header:string; savescr,listmsg:boolean;
                   utf8:boolean;
                   cols:shortint):shortint; { Lister }
procedure RemoveEOF(const fn:string);
procedure editfile(const name: string; nachricht,reedit,senden:boolean;
                   keeplines:byte;ed_ukonv:boolean);
procedure dosshell;
procedure delete_tempfiles;
procedure set_checkdate;

procedure opendatabases;
procedure closedatabases;
procedure TempClose;
procedure TempOpen;
procedure FlushClose;
procedure xp_DB_Error;    { Aufruf bei <DB> internem Fehler }

function fmove(var f1,f2:file): boolean;
procedure iso_conv(var buf; bufsize: Integer);

function  aFile(nr:byte):string;

function  mbrett(typ:char; intnr:longint):string; { Xpoint.Db1/Bretter erz. }
function  mbrettd(typ:char; dbp:DB):string;       { Int_Nr auslesen }
function  ixdat(s:shortstring):longint;           { Z-Date -> Long  }
function  longdat(l:longint):string;              { Long -> Z-Date  }
function  ixdispdat(const dat:shortstring):longint;      { Datum -> Long   }
function  smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }

function  fdat(const dat:string):string;             { Z-Datum -> Datum   }
function  zdow(const dat:string):string;             { Z-Datum -> Mo/Di.. }
function  ftime(const dat:string):string;            { Z-Datum -> Uhrzeit }
function  Zdate:string;               { akt. Datum/Zeit im Z-Format }
function  fuser(const s:string):string;              { Spaces vor/hinter '@' }
function  aufnahme_string:string;

function  MsgidIndex(mid:string):longint;      { case-insensitive CRC32 }

function getb(const su, v:string; var b:byte):boolean;   { PARSER }
function getc(const su, v:string; var c:char):boolean;
function geti(const su, v:string; var i:integer):boolean;
function geti16(const su, v:string; var i:integer16):boolean; { wie geti, allerdings mit 16 Bit Integer }
function getw(const su, v:string; var w:smallword):boolean;
function getl(const su, v:string; var l:longint):boolean;
function getx(const su, v:string; var b:boolean):boolean;
function gets(const s,su, v:string; var ss:string):boolean;
function getr(const su, v:string; var r:double):boolean;

procedure exchange(var s:string; const repl,by:string);
function repfile(const prog,name:string):string;

function notempty(var s:string):boolean;

function IS_QPC(var betreff:string):boolean;
function IS_DES(var betreff:string):boolean;
function IS_PMC(var betreff:string):boolean;

procedure write_lastcall(const dat:String);

procedure InitPrinter;
procedure PrintPage;
procedure PrintLine(const s:string);
procedure ExitPrinter;

function  TempFree:Int64;                 { Platz auf Temp-Laufwerk }
function  TempS(bytes:longint):string;
function  TempExtS(bytes:longint;const startnamewith,ext:string):string;
procedure _era(const Filename: String);
// Deletes a file only if exists, uses _era to report errors
procedure SafeDeleteFile(const Filename: String);
procedure SafeMakeBak(const Filename, NewExt: String);
procedure _chdir(p:string);
function  testmem(size:longint; wait:boolean):boolean;

procedure cm_w(const s:string);                     { Command-Mode-Ausgabe }
procedure cm_wl(const s:string);                    { Writeln              }
procedure cm_wln;
procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
function  cm_key:char;

{$IFDEF Snapshot}
function compiletime:string;      { Erstelldatum von XP.EXE als String uebergeben }
{$ENDIF}

function  ComputeUserAddress(d: DB):string;

procedure InitXP1Unit;

implementation  {-------------------------------------------------------}

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
  crt,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
{$IFDEF NCRT }
  {$IFDEF Kylix}
    ncursix,
  {$ELSE}
    ncurses,
  {$ENDIF}
{$ENDIF }
  montage,fileio,win2,
  database,mouse,maus2,maske,printerx,clip,
  resource,crc,debug,  osdepend,
  xp1o,xp1o2,xp1help,xp1input,xpe,xpnt, xp3,
  direct,
  mime;

{ Diese Tabelle konvertiert NUR ™ š ae oe ue ss !    }
{ todo: remove!
  vollstaendige ISO-Konvertierung: siehe XP3 }

const isotab1   : array[$c0..$ff] of byte =
             ($c0,$c1,$c2,$c3,{ $8e,}$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
              $d0,$d1,$d2,$d3,$d4,$d5,$99,$d7,$d8,$d9,$da,$db,$9a,$dd,$de,$e1,
              $e0,$e1,$e2,$e3,$84,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
              $f0,$f1,$f2,$f3,$f4,$f5,$94,$f7,$f8,$f9,$fa,$fb,$81,$fd,$fe,$ff);

      maxwinst  = 20;

var   closed    : boolean = false;
      opendb    : boolean = false;
      mainmenu  : map = nil;            { Hauptmenue }
      menulast  : byte = 0;             { Hoehe des Menu-Stacks }
      winstp    : integer = 0;

var  menulevel : byte;                  { Menueebene }
     menustack : array[0..4] of byte;   { fuer Rekonstruktion im Config-Menue }
     hmpos     : array[1..10] of integer;  { Hauptmenue-XPos }
     main_n    : integer;               { MPs im Hauptmenue }
     mainrange : array[1..10,0..1] of byte;
     listhicol : byte;
     winstack  : array[1..maxwinst] of scrptr;   { fuer Blindensupport }
     mst       : boolean;


function  ixdat(s:shortstring):longint; assembler;  {&uses ebx, esi}
asm
         mov   esi,s
         inc   esi                      { Laenge ist z.Zt. immer 10 }
         call  @getbyte                 { Jahr }
         cmp   al,70
         jae   @neunzehn
         add   al,100
@neunzehn:mov   dh,al
         call  @getbyte                 { Monat }
         mov   cl,4
         shl   al,cl
         mov   dl,al
         mov   ecx,0
         call  @getbyte                 { Tag }
         shr   al,1
         rcr   ch,1
         add   dl,al
         call  @getbyte                 { Stunde }
         shl   al,1
         shl   al,1
         add   ch,al
         call  @getbyte                 { Minute }
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         add   ch,al
         shl   edx, 16
         mov   eax, edx
         mov   ax,cx
         jmp   @ende

@getbyte:mov   al, [esi]
         inc   esi
         sub   al,'0'
         mov   ah,10
         mul   ah
         add   al, [esi]
         sub   al,'0'
         inc   esi
         ret
@ende:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure iso_conv(var buf; bufsize: Integer); assembler;  {&uses ebx, edi}
{$IFDEF ANALYSE}
begin
  //no asm
end;
{$ELSE}
asm
         cld
         mov    edi, buf
         mov    ecx, bufsize
         mov    ebx, offset isotab1 - 0c0h
@isolp:  mov    al, [edi]
         cmp    al, 0c0h
         jb     @noconv
         xlatb
@noconv: stosb
         loop   @isolp
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }
{$ENDIF }


{ Hervorhebungsregeln fuer * und _ im Lister: }
{ 1 = vor  Startzeichen erlaubt }
{ 2 = nach Startzeichen erlaubt }
{ 4 = vor  Endzeichen erlaubt }
{ 8 = nach Endzeichen erlaubt }

const
  delimiters : array[0..255] of byte = (
            0                            ,{ ^@ }
            0                            ,{ ^A }
            0                            ,{ ^B }
            0                            ,{ ^C }
            0                            ,{ ^D }
            0                            ,{ ^E }
            0                            ,{ ^F }
            0                            ,{ ^G }
            0                            ,{ ^H }
            0                            ,{ ^I }
            0                            ,{ ^J }
            0                            ,{ ^K }
            0                            ,{ ^L }
            0                            ,{ ^M }
            0                            ,{ ^N }
            0                            ,{ ^O }
            0                            ,{ ^P }
            0                            ,{ ^Q }
            0                            ,{ ^R }
            0                            ,{ ^S }
            0                            ,{ ^T }
            0                            ,{ ^U }
            0                            ,{ ^V }
            0                            ,{ ^W }
            0                            ,{ ^X }
            0                            ,{ ^Y }
            0                            ,{ ^Z }
            0                            ,{ ^[ }
            0                            ,{ ^\ }
            0                            ,{ ^] }
            0                            ,{ ^^ }
            0                            ,{ ^_ }

            0  +  1 +         8          ,{ Space }
            0  +          4 + 8          ,{ ! }
            0  +  1 + 2 + 4 + 8          ,{ " }
            0                            ,{ # }
            0                            ,{ $ }
            0                            ,{ % }
            0                            ,{ & }
            0  +  1 + 2 + 4 + 8          ,{ ' }
            0  +  1                      ,{ ( }
            0  +              8          ,{ ) }
            0                            ,{ * }
            0                            ,{ + }
            0  +          4 + 8          ,{ , }
            0  +              8          ,{ - }
            0  +          4 + 8          ,{ . }
            0                            ,{ / }
            0  +      2 + 4              ,{ 0 }
            0  +      2 + 4              ,{ 1 }
            0  +      2 + 4              ,{ 2 }
            0  +      2 + 4              ,{ 3 }
            0  +      2 + 4              ,{ 4 }
            0  +      2 + 4              ,{ 5 }
            0  +      2 + 4              ,{ 6 }
            0  +      2 + 4              ,{ 7 }
            0  +      2 + 4              ,{ 8 }
            0  +      2 + 4              ,{ 9 }
            0  +          4 + 8          ,{ : }
            0  +          4 + 8          ,{ ; }
            0                            ,{ < }
            0                            ,{ = }
            0  +  1                      ,{ > }
            0  +          4 + 8          ,{ ? }
            0  +      2 + 4              ,{ @ }
            0  +      2 + 4              ,{ A }
            0  +      2 + 4              ,{ B }
            0  +      2 + 4              ,{ C }
            0  +      2 + 4              ,{ D }
            0  +      2 + 4              ,{ E }
            0  +      2 + 4              ,{ F }
            0  +      2 + 4              ,{ G }
            0  +      2 + 4              ,{ H }
            0  +      2 + 4              ,{ I }
            0  +      2 + 4              ,{ J }
            0  +      2 + 4              ,{ K }
            0  +      2 + 4              ,{ L }
            0  +      2 + 4              ,{ M }
            0  +      2 + 4              ,{ N }
            0  +      2 + 4              ,{ O }
            0  +      2 + 4              ,{ P }
            0  +      2 + 4              ,{ Q }
            0  +      2 + 4              ,{ R }
            0  +      2 + 4              ,{ S }
            0  +      2 + 4              ,{ T }
            0  +      2 + 4              ,{ U }
            0  +      2 + 4              ,{ V }
            0  +      2 + 4              ,{ W }
            0  +      2 + 4              ,{ X }
            0  +      2 + 4              ,{ Y }
            0  +      2 + 4              ,{ Z }
            0  +  1                      ,{ [ }
            0                            ,{ \ }
            0  +              8          ,{ ] }
            0                            ,{ ^ }
            0                            ,{ _ }
            0  +  1 + 2 + 4 + 8          ,{ ` }
            0  +      2 + 4              ,{ a }
            0  +      2 + 4              ,{ b }
            0  +      2 + 4              ,{ c }
            0  +      2 + 4              ,{ d }
            0  +      2 + 4              ,{ e }
            0  +      2 + 4              ,{ f }
            0  +      2 + 4              ,{ g }
            0  +      2 + 4              ,{ h }
            0  +      2 + 4              ,{ i }
            0  +      2 + 4              ,{ j }
            0  +      2 + 4              ,{ k }
            0  +      2 + 4              ,{ l }
            0  +      2 + 4              ,{ m }
            0  +      2 + 4              ,{ n }
            0  +      2 + 4              ,{ o }
            0  +      2 + 4              ,{ p }
            0  +      2 + 4              ,{ q }
            0  +      2 + 4              ,{ r }
            0  +      2 + 4              ,{ s }
            0  +      2 + 4              ,{ t }
            0  +      2 + 4              ,{ u }
            0  +      2 + 4              ,{ v }
            0  +      2 + 4              ,{ w }
            0  +      2 + 4              ,{ x }
            0  +      2 + 4              ,{ y }
            0  +      2 + 4              ,{ z }
            0  +  1                      ,(* { *)
            0                            ,{ | }
            0  +              8          ,{   }
            0                            ,{ ~ }
            0                            ,{ DEL }

            0  +      2 + 4              ,{ € }
            0  +      2 + 4              ,{ ue }
            0  +      2 + 4              ,{ ‚ }
            0  +      2 + 4              ,{ ƒ }
            0  +      2 + 4              ,{ ae }
            0  +      2 + 4              ,{ … }
            0  +      2 + 4              ,{ † }
            0  +      2 + 4              ,{ ‡ }
            0  +      2 + 4              ,{ ˆ }
            0  +      2 + 4              ,{ ‰ }
            0  +      2 + 4              ,{ Š }
            0  +      2 + 4              ,{ ‹ }
            0  +      2 + 4              ,{ Œ }
            0  +      2 + 4              ,{  }
            0  +      2 + 4              ,{ Ž }
            0  +      2 + 4              ,{  }
            0  +      2 + 4              ,{  }
            0  +      2 + 4              ,{ ‘ }
            0  +      2 + 4              ,{ ’ }
            0  +      2 + 4              ,{ “ }
            0  +      2 + 4              ,{ oe }
            0  +      2 + 4              ,{ • }
            0  +      2 + 4              ,{ – }
            0  +      2 + 4              ,{ — }
            0  +      2 + 4              ,{ ˜ }
            0  +      2 + 4              ,{ ™ }
            0  +      2 + 4              ,{ š }
            0  +          4              ,{ › }
            0  +          4              ,{ œ }
            0  +          4              ,{  }
            0  +          4              ,{ ž }
            0                            ,{ Ÿ }
            0  +      2 + 4              ,{   }
            0  +      2 + 4              ,{ ¡ }
            0  +      2 + 4              ,{ ¢ }
            0  +      2 + 4              ,{ £ }
            0  +      2 + 4              ,{ ¤ }
            0  +      2 + 4              ,{ ¥ }
            0  +          4              ,{ ¦ }
            0  +          4              ,{ § }
            0  +  1 +         8          ,{ ¨ }
            0                            ,{ © }
            0                            ,{ ª }
            0                            ,{ « }
            0                            ,{ ¬ }
            0  +  1 +         8          ,{ ­ }
            0  +  1                      ,{ ® }
            0  +              8          ,{ ¯ }
            0                            ,{ ° }
            0                            ,{ ± }
            0                            ,{ ² }
            0                            ,{ ³ }
            0                            ,{ ´ }
            0                            ,{ µ }
            0                            ,{ ¶ }
            0                            ,{ · }
            0                            ,{ ¸ }
            0                            ,{ ¹ }
            0                            ,{ º }
            0                            ,{ » }
            0                            ,{ ¼ }
            0                            ,{ ½ }
            0                            ,{ ¾ }
            0                            ,{ ¿ }
            0                            ,{ À }
            0                            ,{ Á }
            0                            ,{ Â }
            0                            ,{ Ã }
            0                            ,{ Ä }
            0                            ,{ Å }
            0                            ,{ Æ }
            0                            ,{ Ç }
            0                            ,{ È }
            0                            ,{ É }
            0                            ,{ Ê }
            0                            ,{ Ë }
            0                            ,{ Ì }
            0                            ,{ Í }
            0                            ,{ Î }
            0                            ,{ Ï }
            0                            ,{ Ð }
            0                            ,{ Ñ }
            0                            ,{ Ò }
            0                            ,{ Ó }
            0                            ,{ Ô }
            0                            ,{ Õ }
            0                            ,{ Ö }
            0                            ,{ × }
            0                            ,{ Ø }
            0                            ,{ Ù }
            0                            ,{ Ú }
            0                            ,{ Û }
            0                            ,{ Ü }
            0                            ,{ Ý }
            0                            ,{ Þ }
            0                            ,{ ß }
            0  +      2 + 4              ,{ à }
            0  +      2 + 4              ,{ ss }
            0  +      2 + 4              ,{ â }
            0  +      2 + 4              ,{ ã }
            0  +      2 + 4              ,{ ä }
            0  +      2 + 4              ,{ å }
            0  +      2 + 4              ,{ æ }
            0  +      2 + 4              ,{ ç }
            0  +      2 + 4              ,{ è }
            0  +      2 + 4              ,{ é }
            0  +      2 + 4              ,{ ê }
            0  +      2 + 4              ,{ ë }
            0                            ,{ ì }
            0                            ,{ í }
            0                            ,{ î }
            0                            ,{ ï }
            0                            ,{ ð }
            0                            ,{ ñ }
            0                            ,{ ò }
            0                            ,{ ó }
            0                            ,{ ô }
            0                            ,{ õ }
            0                            ,{ ö }
            0                            ,{ ÷ }
            0                            ,{ ø }
            0                            ,{ ù }
            0                            ,{ ú }
            0                            ,{ û }
            0  +          4              ,{ ü }
            0  +          4              ,{ ý }
            0                            ,{ þ }
            0  +  1 +         8          ){ #255 };

{ Variable in XP0.PAS: }
{ charbuf     : string[255];}                  {82 Zeichen}
{ attrbuf     : array [1..255] of smallword;}  {82 Attribute}

{ Attribute werden als Word erzeugt, fuer nicht Windows-Versionen }
{ mussen die Zugriffe auf Attrbuf evtl angepasst werden zu "attrbuf[ebx],dl" }

procedure MakeListDisplay(const s: shortstring); assembler; {&uses ebx, esi, edi}

asm
            mov edi,s
            cld
            xor ecx,ecx
            mov cl,[edi]
            inc edi
            push ecx

            xor ebx,ebx                    { s + color -> dispbuf }

            mov dh,0
            mov dl,textattr
            mov al,' '                     { Abgrenzung links }
            mov byte ptr charbuf[ebx],al
            mov word ptr attrbuf[ebx*2],dx
            inc ebx

@dcopylp:   mov al,[edi]
            mov byte ptr charbuf[ebx],al
            mov word ptr attrbuf[ebx*2],dx
            inc edi
            inc ebx
            loop @dcopylp

            mov al,' '                     { Abgrenzung rechts }
            mov byte ptr charbuf[ebx],al
            mov word ptr attrbuf[ebx*2],dx
            pop ecx

            cmp ListXhighlight,0           { keine Hervorhebungen? }
            je @nodh
            mov al,'*'
            call @testattr                 { sichert cx }
            mov al,'_'
            call @testattr
            mov   al,'/'
            call  @testattr

@nodh:      mov byte ptr charbuf[0],cl
            add ecx,ecx
            mov word ptr attrbuf[0],cx
            jmp @ende


{-----------------------}

@testattr:  pusha
            mov edx,ecx
            xor ebx,ebx

            {-----------}
@ta1:       push eax
            mov ecx,edx
            xor esi,esi

@talp1:     cmp al,byte ptr charbuf[esi]          { Startzeichen checken }
            jne @tanext1

             mov bl,byte ptr charbuf[esi-1]
             test byte ptr delimiters[ebx],1      { Byte vor Startzeichen ok? }
             jz @tanext1
             mov bl,byte ptr charbuf[esi+1]
             test byte ptr delimiters[ebx],2      { Byte vor Startzeichen ok? }
             jnz @tastart                          { Startzeichen gefunden }

@tanext1:   inc esi
            loop @talp1
            jmp @taende

            {-----------}

@tastart:   mov edi,esi                            { Di = Byte nach Startzeichen }
            dec ecx
            jz @taende                             { Mindestens 1 Zeichen abstand }
            dec ecx                                { min. ein Zeichen Abstand }
            jz @taende
            inc si                                 { dann Endzeichen Checken }

@talp2:     cmp al,byte ptr charbuf[esi]
            jne @tanext2

             mov bl,byte ptr charbuf[esi-1]
             test byte ptr delimiters[ebx],4      { Byte vor Endzeichen ok? }
             jz @tanext2
             mov bl,byte ptr charbuf[esi+1]
             test byte ptr delimiters[ebx],8      { Byte nach Endzeichen ok? }
             jnz @tafound2                        { Endzeichen gefunden }

@tanext2:   inc esi
            loop @talp2
            jmp @taende

            {------------}

@tafound2:  push ecx
            mov ecx,esi
            sub ecx,edi
            dec ecx                                { cx <- Anzahl hervorgeh. Zeichen }
            mov ah,listhicol

@tacopy1:   mov al,byte ptr charbuf[edi+1]        { hervorgehobenen Text eins nach }
            mov byte ptr charbuf[edi],al          { vorne kopieren; Farbe tauschen }
            mov byte ptr attrbuf[edi*2],ah
            inc edi
            loop @tacopy1

            pop ecx
            dec ecx                                { restliche Zeichen }
            jz @addspace

@tacopy2:   mov al,byte ptr charbuf[edi+2]         { Zeichen nach links schieben }
            mov byte ptr charbuf[edi],al
            mov ah,byte ptr attrbuf[edi*2+4]       { Attribute ebenso !!! }
            mov byte ptr attrbuf[edi*2],ah
            inc edi
            dec ecx
            jns  @tacopy2

@addspace:  mov word ptr charbuf[edi],'  '        { 2 Leerzeichen anhaengen }
            pop eax
            jmp @ta1                               { ... und das Ganze nochmal }

@taende:    pop eax
            mov ecx,edx
            popa
            ret
            // this is end of internal function testattr
{-------------------------}
@ende:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end; { of MakeListdisplay }
{$ENDIF }


procedure ListDisplay(x,y:xpWord; var s:string);
var
  s0: shortstring;
begin
  s0:= s;
  makelistdisplay(s0);
  Consolewrite(x,y,length(s0));
  s:= s0;                               { Falls var irgendeine Bedeutung hat }
end;

procedure ListDisplayUTF8(x,y:xpWord; var s: string);
begin
  FWrt(x,y,s);
end;

procedure interr(const txt:string);
begin
  moff;
  cm_wl(txt);
  runerror:=false;
  Debug.DebugLog('xp1','Internal error: '+txt,DLError);
  halt(1);
end;

procedure blindon(total:boolean);
var mf : boolean;
    mt : byte;
begin
  if blind and (winstp<maxwinst) then
  begin
    inc(winstp);
    if winstp=1 then begin
      mst:=m2t; m2t:=false;
      end;
    sichern(winstack[winstp]);
    mf:=forcecolor; forcecolor:=false; mt:=lastattr;
    attrtxt(7);
    moff;
    clwin(1,ScreenWidth,iif(total,1,2),screenlines);
    mon;
    attrtxt(mt);
    forcecolor:=mf;
    end;
end;


procedure blindoff;
begin
  if winstp>0 then begin
    moff;
    holen(winstack[winstp]);
    mon;
    dec(winstp);
    if winstp=0 then m2t:=mst;
    end;
end;


{ Online-Hilfe (s. auch xp1help.pas) }

procedure hlp(nr:xpWord);
begin
  helpst[helpstp]:=nr;
end;

procedure pushhp(nr:xpWord);
begin
  if helpstp>=maxhelpst then
    interr('PushHP: Overflow')
  else begin
    inc(helpstp);
    helpst[helpstp]:=nr;
    end;
end;

procedure pophp;
begin
  if helpstp=1 then
    interr('PopHP: Underflow')
  else
    dec(helpstp);
end;


procedure freehelp;  { wird von shell() benutzt }
begin
  if inithlp then begin
    releasehelp;
    inithlp:=false;
    end;
end;


{.$I xp1menu.inc}   { Menuefunktionen }

{ XP1: Menuefunktionen }


function MenuHidden(mpnr:integer):boolean;
var l,r,m : integer;
begin
  if ((mpnr=$001) or (mpnr=$011)) then
    MenuHidden:=false    { /XPoint/Registrierung }
  else
    case mpnr of
         0 : MenuHidden:=false;                      { zur Sicherheit ... }
{$ifdef DOS32}
      $017 : MenuHidden:=true;                       { X/Telnet }
{$endif}
      $069 : MenuHidden:=(SaveType=0);               { Config/Sichern }
      $0f5,$0f6 : MenuHidden:=true;                  { Netcall/Relogin, Netcall/Online }
      $11a : MenuHidden:=not (deutsch and ParDebug); { X/Statistik/Fragmente }
      $1ca : MenuHidden:=not languageopt;            { Config/Optionen/Sprache }
      $125 : MenuHidden:=true;                       // Config/Optionen/ISDN
      $1c7 : MenuHidden := true;
      else   if anzhidden=0 then
               MenuHidden:=false
             else begin
               l:=1; r:=anzhidden;
               while (r-l>1) do begin
                 m:=(l+r) div 2;
                 if hidden^[m]<mpnr then l:=m
                 else r:=m;
                 end;
               MenuHidden:=(mpnr=hidden^[l]) or (mpnr=hidden^[r]);
             end;
    end;
end;


procedure splitmenu(nr:byte; ma:map; var n:integer; nummern:boolean);
var s       : string;
    p,p2,p3 : Integer;
label again;
begin
  n:=0;
again:
  s:=menu[nr];
  repeat
    p:=cPos(',',s);
    if p>0 then begin
      inc(n);
      with ma^[n] do begin
        mstr:='';
        s:=Mid(s, p+1);
        if nummern and (LeftStr(s,2)<>'-,') then begin
          mpnr:=hexval(LeftStr(s,3));
          delete(s,1,3);
          enabled:=(menable[nr] and (xpWord(1) shl (mpnr and 15)))=0;
          end
        else begin
          mpnr:=0;
          enabled:=true;
          end;
        if FirstChar(s)='!' then begin      { Menue nicht verlassen? }
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
        if hpos>0 then hkey:=System.UpCase(mstr[hpos])
        else hkey:=#255;
        if cPos('ù',mstr)>0 then begin
          p2:=cPos('ù',mstr);
          chain:=ival(copy(mstr,p2+1,40));
          mstr:=copy(mstr,1,p2-1);
          if (nr>0) and (pos('..',mstr)=0) then mstr:=mstr+'..';
          end
        else chain:=0;
        if MenuHidden(mpnr) or    { versteckten Menuepunkt ueberspringen }
          ((mstr='-') and ((n=1) or (ma^[n-1].mstr='-')))   { doppelter Sep.? }
        then
          dec(n);
        end;
      end;
  until p=0;
  while (n>0) and (ma^[n].mstr='-') do    { Separatoren am Ende entfernen }
    dec(n);
  if nr=2 then begin
    nr:=menus;
    goto again;
    end;
end;


procedure showmain(nr:shortint);
var i      : integer;
    s      : string;
    p      : byte;
    x:   Integer;
begin
  if mainmenu=nil then begin
    getmem(mainmenu,sizeof(menuarray));
    splitmenu(0,mainmenu,main_n,true);
    p:=2;
    for i:=1 to main_n do begin
      mainrange[i,0]:=p;
      inc(p,length(mainmenu^[i].mstr)+2);
      mainrange[i,1]:=p-1;
      end;
    end;
  mainmenu^[3].enabled:=(aktdispmode<>20);
  setenable(0,3,aktdispmode<>20);
  x := 2;
  moff;
  for i:=1 to main_n do
    with mainmenu^[i] do begin
      hmpos[i]:= x+1;
      if enabled then begin
        if nr=i then
          attrtxt(col.colmenuinv[0])
        else
          attrtxt(col.colmenu[0]);
        s:=mstr;
        FWrt(x, 1, ' ' + s + ' ');
        if i=nr then
          attrtxt(col.colmenuinvhi[0])
        else
          attrtxt(col.colmenuhigh[0]);
        FWrt(x+hpos, 1, s[hpos]);
        x := x + Length(s) + 2;
      end
      else begin
        attrtxt(col.colmenudis[0]);
        FWrt(x, 1, ' ' + mstr + ' ');
        x := x + Length(mstr) + 2;
      end;
    end;
  mon;
end;


function mainkey(p:byte):taste;
var i : integer;
begin
  mainkey:=#0;
  for i:=1 to main_n do
    if (p>=mainrange[i,0]) and (p<=mainrange[i,1]) then
      with mainmenu^[i] do
        mainkey:=UpCase(mstr[hpos]);
end;


procedure freemain;
begin
  if Assigned(MainMenu) then Freemem(mainmenu, sizeof(menuarray));
  mainmenu:=nil;
end;


{ Menuepunkt suchen             }
{ mnu:  Menuename               }
{ nr :  Nummer des Menuepunkts  }
{ &n :  Menuenummer             }
{ &p :  Position im Menuestring }

procedure findnr(var mnu:string; nr:byte; var n,p:byte);
begin
  n:=0;
  mnu := LowerCase(mnu);
  while LowerCase(LeftStr(menu[n],length(mnu)))<>mnu do inc(n);
  p:=pos(','+LowerCase(typeform.hex(n,2)+typeform.hex(nr,1)),LowerCase(menu[n]))+1;
end;


{ Menuepunkt ein- uder ausschalten    }
{ mnu:  Name des Menues               }
{ nr :  Nummer des Menuepunkts        }

procedure setenable(mnu,nr:byte; flag:boolean);
begin
  if flag then menable[mnu]:=menable[mnu] and not (xpWord(1) shl nr)
  else menable[mnu]:=menable[mnu] or (xpWord(1) shl nr);
end;


{ Menuepunkt aendern             }
{ mnu: Name des Menues          }
{ nr : Position des Menuepunkts }
{ new: neuer Menuepunkt         }

{ ACHTUNG!! es muss auf dem Heap genuf Platz fuer menu[n]^ belegt sein!! }

procedure setmenup(mnu:string; nr:byte; anew:string);
var n,p,p2 : byte;
begin
  findnr(mnu,nr,n,p);
  p2:=cPos(',',Mid(menu[n],p));
  if p2=0 then p2:=length(menu[n])+1
  else inc(p2,p+1);
  menu[n]:=LeftStr(menu[n],p-1)+anew+Mid(menu[n],p2);
end;


{ neue Menue-Position setzen }

procedure setmenupos(mnu:string; newpos:byte);
var n,p : byte;
begin
  findnr(mnu,1,n,p);
  menupos[n]:=newpos;
end;


procedure miscschab;
var s       : string;
    useclip : boolean;
begin
  s:= '*' + extXps;
  useclip:=false;
  if readfilename(getres(103),s,false,useclip) then
  begin  { Schablone bearbeiten }
    if ExtractFileExt(s) = '' then
      s := ChangeFileExt(s, extXps);

    if FileUpperCase(ExtractFileExt(s)) <> extXps then
    begin
      rfehler(2);    { Dateierweiterung muss .XPS sein! }
      exit;
    end;
    EditFile(s,false,false,false,0,false);
  end;
end;


procedure SetExtraktMenu;
var n : byte;
begin
  n:=ival(getres2(104,2));
  setmenup('Extrakt',6,getres2(104,1)+
           copy(getres2(104,3),ExtraktTyp*n+1,n)+'ù13');
  freeres;
end;


{ Menuepunkt direkt ausfuehren und zurueck zum Menue }

procedure menu_keep(m:integer);
var m1 : byte;
    wp : boolean;
begin
  m1:=m mod 16;
  case m div 16 of
    8 : begin
          wp:=(exteditor<3) and (m1<14);
          if wp then begin
            attrtxt(col.coledithead);
            moff;
            wpush(1,80,1,2,'-');
            Fwrt(1,1,forms(' '+getres2(132,m1),80));
            mon;
            end;
          case m1 of           { Schablonen }
            1 : editfile(headerfile,false,false,false,1,false);
            2 : editfile(headerpriv,false,false,false,1,false);
            3 : editfile(quotemsk,false,false,false,1,false);
            4 : editfile(quotepriv,false,false,false,1,false);
            5 : editfile(quotepmpriv,false,false,false,1,false);
            6 : editfile(quotetomsk,false,false,false,1,false);
            7 : editfile(weitermsk,false,false,false,1,false);
            8 : editfile(erneutmsk,false,false,false,1,false);
           10 : sigedit(signatfile);
           11 : sigedit(privsignat);
           12 : editfile(EB_msk,false,false,false,1,false);
           13 : editfile(CancelMsk,false,false,false,1,false);
           14 : miscschab;
          end;
          if wp then wpop;
        end;
   13 : begin                { Extrakt Als... }
          ExtraktTyp:=m1-1;
          SetExtraktMenu;
        end;
  end;
end;


{ Menuesystem. -------------------------------------------- }
{ nr       : Menuenummer                                    }
{ enterkey : erster Tastendruck                            }
{ x,y      : Koordinaten fuer Untermenue-Anzeige             }
{ Return   : xxy (Hex!) : Punkt y in Menue xx wurde gewaehlt }
{             0: Menue mit Esc oder sonstwie abgebrochen    }
{            -1: Untermenue nach links verlassen            }
{            -2: Untermenue nach rechts verlassen           }

function getmenu(nr:byte; enterkey:taste; x,y:integer):integer;
var ma    : map;
    n,i   : integer;
    t     : taste;
    p,ml  : byte;
    pold  : byte;
    get2  : integer;
    xx,yy : integer;
    autolr: byte;
    dead  : boolean;   { alle disabled }
    has_checker : boolean;
    mausback : boolean;
    longmenu : boolean; {Menue mit mehr als 13 Menuepunkten (Zusatz)}

  procedure display;
  var i,hp  : byte;
      s     : string;
      check : char;

      begin
    if nr=0 then showmain(p)
    else begin
      moff;
      for i:=1 to n do begin
        s:=ma^[i].mstr;
        hp:=ma^[i].hpos;
        if (i<>p) or dead then
          if ma^[i].enabled then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenudis[menulevel])
        else
          if ma^[i].enabled then attrtxt(col.colmenuinv[menulevel])
          else attrtxt(col.colmenuseldis[menulevel]);
        check:=iifc(checker[nr]=i,'û',' ');
        if s='-' then
          Fwrt(x,y+i,'Ã'+dup(ml,'Ä')+'´')
        else if hp=0 then
          Fwrt(x+1,y+i,check+forms(s,ml-1))
        else if not ma^[i].enabled then
          Fwrt(x+1,y+i,' '+forms(s,ml-1))
        else
        begin
          FWrt(x+1,y+i, forms(check+s,ml));
          if i<>p then
            attrtxt(col.colmenuhigh[menulevel])
          else
            attrtxt(col.colmenuinvhi[menulevel]);
          FWrt(x+1+hp, y+i, s[hp]);
{          wrt(x+1,y+i,check+LeftStr(s,hp-1));
          if i<>p then
            attrtxt(col.colmenuhigh[menulevel])
          else
            attrtxt(col.colmenuinvhi[menulevel]);
          Wrt2(s[hp]);
          if i<>p then
            attrtxt(col.colmenu[menulevel])
          else
            attrtxt(col.colmenuinv[menulevel]);
          Wrt2(forms(copy(s,hp+1,40),ml-hp-1));  }
          end
        end;
      mon;
      end;
  end;

  function nomp(p:byte):boolean;
  begin
    nomp:=(ma^[p].mstr='-') or ((nr=0) and not ma^[p].enabled);
  end;

  function nr0pos(mx:byte):byte;
  var i : byte;
  begin
    i:=1;
    while (i<main_n) and (mx>mainrange[i,1]) do inc(i);
    nr0pos:=i;
  end;

  procedure maus_auswertung;
  var mx,my  : integer;
      _mx,_my : integer;
      inside : boolean;
  begin
    maus_gettext(_mx,_my);
    mx:=_mx-x; my:=_my-y;
    if nr>0 then
      inside:=(mx>=1) and (mx<=ml) and (my>=1) and (my<=n)
    else begin
      inside:=(_my=1) and (mx>=mainrange[1,0]) and (mx<=mainrange[main_n,1]);
      my:=nr0pos(mx);
      end;
    if inside and not nomp(my) then begin
      if t=mausunleft then begin
        p:=my; t:=keycr; display; end else
      if t=mausright then t:=keyesc else
      if (t=mausleft) or (t=mauslmoved) then begin
        p:=my;
        if nr=0 then begin display; t:=keycr; end;
        end;
      end
    else if not inside then
      if (t=mausleft) or
         ((nr>0) and (_my=1) and (t=mauslmoved) and (nr0pos(_mx)<>menupos[0]))
      then
        mausback:=true
      else
        if t=mausright then
          t:=keyesc;
  end;

  function Zusatz_II:byte;  { Benutzte Punkte in den ersten 10 Zusatzmenue-  }
  var i,n:byte;             { eintraegen zaehlen (ergibt die korrekte Grenze }
  begin                     { fuer Uebergang von Fkeys[0] zu Fkeys[4])       }
    n:=0;
    for i:=1 to 10 do
      if Fkeys[0][i].menue<>'' then inc(n);
    Zusatz_II:=n;
  end;

begin
  get2 := -1;
  longmenu := False;
  if nr=0 then begin
    menulevel:=0;
    if menurestart then enterkey:=mainmenu^[menustack[0]].hkey;
    end;
  getmem(ma,sizeof(menuarray));
  splitmenu(nr,ma,n,true);
  if n=0 then begin    { leeres Menue durch XPME }
    dispose(ma);
    getmenu:=0;
    exit;
    end;
  has_checker:=(checker[nr]>0);
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
    if menulevel=1 then blindon(false);
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
    mauszuo:=(p>1); mauszuu:=(p<n);
    hlp(10000+(ma^[p].mpnr shr 4)*100 + (ma^[p].mpnr and $f));
    if p<>pold then display;
    pold:=p;
    case autolr of
      4 : begin t:=mausleft; autolr:=0; end;
      3 : begin t:=keyrght; autolr:=1; end;
      2 : begin t:=keyleft; autolr:=1; end;
      1 : begin t:=keycr; autolr:=0; end;
    else
      if menurestart then
        if menulevel=menulast then begin
          menurestart:=false;
          p:=menustack[menulevel];
          t:='';
          end
        else
          t:=ma^[menustack[menulevel]].hkey
      else
        if auswahlcursor then begin
          if nr=0 then gotoxy(hmpos[p]-1,1)
          else gotoxy(x+1,y+p);
          get(t,curon);
          end
        else
          get(t,curoff);
    end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_auswertung;
    if t=keyaf4 then quit:=true;
    if not dead then begin
      i:=1;
      case t[1] of 
        #132: t[1] := #142; // Bildschirm ä in Tastatur ä umwandeln
        // ö und ü noch hinzufügen
      end;
      while (i<=n) and (ma^[i].hkey<>UpperCase(t)) do inc(i);
      if (i<=n) and (ma^[i].enabled) then begin
        p:=i; t:=keycr;
        hlp(10000+(ma^[p].mpnr shr 4)*100 + (ma^[p].mpnr and $f));
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
        if has_checker and (t=' ') then checker[nr]:=p;
        if ((nr=0) and (t=keyleft)) or
           ((nr>0) and (t=keyup)) or (t=keystab) then
             repeat
               if p=1 then p:=n else dec(p)
             until not nomp(p);
        end;

      if nr=0 then begin
        if t=keyf10 then t:=keyesc;
        if t=keydown then t:=keycr;
        end;

      get2:=0;
      longmenu := not ((nr<>2) or (p<Zusatz_II+4));
      if t=keycr then
        if ma^[p].enabled then
          if ma^[p].chain>0 then begin
            if nr=0 then begin
              xx:=hmpos[p]-1; yy:=2; end
            else begin
              xx:=x+2; yy:=y+1+p; end;
            menupos[nr]:=p;
            menustack[menulevel]:=p;
            inc(menulevel);
            get2:=getmenu(ma^[p].chain,'',xx,yy);
            dec(menulevel);
            case get2 of
              0  : if nr>0 then t:='';
             -1  : if nr>0 then t:=keyleft
                   else begin
                     autolr:=2; t:=''; end;
             -2  : if nr>0 then t:=keyrght
                   else begin
                     autolr:=3; t:=''; end;
             -3  : begin autolr:=4; t:=''; end;
            end  { case }
          end
        else begin   { kein Untermenue }
          { get2:=16*nr+p; - altes Menuesystem bis XP 3.1 }
          get2:=ma^[p].mpnr;
          menustack[menulevel]:=p;
          menulast:=menulevel;
          end
      else begin   { nicht enabled }
        errsound;
        t:='';
        end;

      if (ma^[p].keep) and (get2>0) then begin
        menu_keep(get2);        { direkt auswerten - Menue nicht verlassen }
        splitmenu(nr,ma,n,true);
        display;
        t:='';
        end;

      end;   { not dead }

  until (t=keyesc) or (t=keycr) or ((nr>0) and ((t=keyleft) or (t=keyrght)))
        or mausback or quit;

  if has_checker and (t=keycr) then checker[nr]:=p;
  if nr>0 then begin
    wpop;
    if menulevel=1 then blindoff;
    end
  else showmain(0);
  menupos[nr]:=p;
  freemem(ma,sizeof(menuarray));

  if t=keyesc then getmenu:=0
  else if t=keycr then
  begin
    if longmenu then get2:=get2+$1000-4;
    getmenu:=get2;
    end
  else if t=keyleft then getmenu:=-1
  else if mausback then getmenu:=-3
  else getmenu:=-2;
end;

{ ----- Externe Programme ------------------------------------------- }

procedure xp_maus_aus;
begin
  if _maus then begin
    maus_tasten_aus;
    mausaus;
    { mausinit; }
    maus_cursor:=false;
    end;
end;

procedure xp_maus_an(x,y: integer);
begin
  if _maus then begin
    if startup or MausShInit then
      mausinit;
    if (x+y>=0) then
      setmaus(x,y);
    mausan;
    maus_tasten_an;
    maus_cursor:=true;
    end;
end;

procedure SetMausEmu;
begin
  iomaus:=ParMaus and not _maus;
end;


procedure sichern(var sp:scrptr);
var
{$IFDEF NCRT }
  r: integer;
{$ELSE }
  scSize: Integer;
{$ENDIF }
begin
{$IFDEF NCRT }
  r:= getrahmen;
  setrahmen(0);
  wpull(1,screenwidth,1,screenlines,'',sp);
  setrahmen(r);
{$ELSE}
{$IFDEF Win32 }
  { Das Attribut belegt hier 2 Byte }
  scsize:=screenlines*screenwidth*4;
{$ELSE }
  scsize:=screenlines*2*screenwidth;
{$ENDIF }
  GetMem(sp, scsize);               { Bild sichern }
  moff;
  ReadScreenRect(1, screenwidth, 1, screenlines, sp^);
  mon;
{$ENDIF}
end;

procedure holen(var sp:scrptr);
begin
{$IFDEF NCRT}
  wrest(sp);
{$ELSE}
  moff;
  {$IFNDEF NCRT }
    WriteScreenRect(1, screenwidth, 1, screenlines, sp^);
  {$ENDIF }
  mon;
  disp_DT;
  freemem(sp);               { Bild wiederherstellen }
{$ENDIF}
end;


procedure InitPrinter;
begin
  checklst:=true;
  xlatger:=false;
  printlines:=0;
  OpenLst(DruckLPT);
  write(lst,PrintString(DruckInit));
end;

procedure PrintPage;
begin
  write(lst,PrintString(DruckFF));
  printlines:=0;
end;

procedure PrintLine(const s:string);
begin
  writeln(lst,sp(DruckLira),s);
  inc(printlines);
  if (DruckFormlen>0) and (printlines>=DruckFormlen) then
    PrintPage;
end;

procedure ExitPrinter;
begin
  write(lst,PrintString(DruckExit));
  write(lst, #26);
  CloseLst;
end;

{.$I xp1s.inc}    { Shell }

{ DOS-Shell }

function repfile(const prog,name:string):string;
var p : byte;
begin
  p:=pos('$FILE',UpperCase(prog));
  if p>0 then
    result:=LeftStr(prog,p-1)+name+copy(prog,p+5,127)
  else
    result:=prog+' '+name;
end;

var   trackpath : boolean = false;

{ call of external program. errorlevel returned in gloval var errorlevel.
  errorlevel is negative if call was not performed (program not found).
  prog may be a batch file or using pipes. program extension MAY be specified
  as well as program path.
  CAUTION: Automatically chdirs back to "ownpath"!
  cls:   0=nicht loeschen; 1=loeschen, 2=loeschen+Hinweis, 3=Mitte loeschen
         -1=loeschen/25 Zeilen, 4=loeschen/nicht sichern,
         5=nicht loeschen/nicht sichern }
procedure shell(const prog:string; space:xpWord; cls:shortint);

  { returned: errorlevel called program returned if call successful
    negative errorlevel if program not found }
  function Xec(command:string; const prompt:string):Integer;

{$ifdef UnixFS}
{$ifdef Unix}
  begin
    Debug.TempCloseLog(False);
    Debug.DebugLog('xp1s','saving terminal state', DLInform);
    def_prog_mode();           { save current tty modes }
    endwin();                  { restore original tty modes }
{$IFDEF Kylix}

    Result:=libc.system(PChar(command));
{$ELSE}
    Result:=linux.shell(command);
{$ENDIF}
    Debug.DebugLog('xp1s','reload terminal state', DLInform);
    refresh();                 { restore save modes, repaint screen }
    Debug.TempCloseLog(True);
    Debug.DebugLog('xp1s','called program "'+command+'": result '+
                   strs(Result),iif(Result=0,DLInform,DLError));
  end;
{$else}
  {$error Please implement this function for your OS}
{$endif}
{$else} // OS is Dos, Win or OS2
  var
    pp    : byte;
    parameters,commandsave : string;
    callviacli : boolean; // "call via command line interpreter" (usually via COMMAND /C)
  begin
    pp:=cPos(' ',command);
    if pp=0 then parameters:=''
    else begin
      parameters:=trim(Mid(command,pp+1));
      command:=LeftStr(command,pp-1);
    end;
    command:=FileUpperCase(command);

    callviacli:=(cPos('|',parameters)>0) or (cPos('>',parameters)>0) or (cPos('<',parameters)>0);
    commandsave:=command;
    if not callviacli then begin
      command:=FindExecutable(command);
      if (command='') // file not found, assume it is built-in in cli
         or(UpperCase(ExtractFileExt(command))= extBatch) // batch files have to be run via cli
         or(ExtractFileExt(command)='') // FindExecutable WILL find "copy" as built-in in cli
        then begin
        callviacli:=true; command:=commandsave;
      end;
    end;
    if trim(command)='' then begin result:=-100; exit end;
    if callviacli then begin
      parameters:=' /c '+command+' '+parameters;
      command:=getenv('comspec');
    end;

    Debug.TempCloseLog(False);
    Result:=SysExec(command, parameters);
    Debug.TempCloseLog(True);
    Debug.DebugLog('xp1s','called program "'+command+'" with parameters "'+parameters+'": result '+
                   strs(Result),iif(Result=0,DLInform,DLError));
  end;
  {$endif}

  procedure ShowPar;
  var
      x,y,p,p2 : Integer;
  begin
    savecursor;
    cursor(curoff);
    if length(prog)<=74 then
      message(prog)
    else begin
      msgbox(76,4,'',x,y);
      p:=blankposx(prog);
      p2:=71;
      while prog[p2]<>' ' do dec(p2);
      mwrt(x+3,y+1,LeftStr(prog,p2-1));
      mwrt(x+3+p,y+2,LeftStr(mid(prog,p2+1), 71-p));
      end;
    wkey(15,false);
    closebox;
    restcursor;
  end;

var
  sm2t     : boolean;
  maussave : mausstat;
  sp       : scrptr;

begin
  CloseAblage;
  if (ParDebFlags and 1<>0) or ShellShowpar then
    ShowPar;
  getmaus(maussave);
  xp_maus_aus;
  if (cls<>4) and (cls<>5) then begin
    sichern(sp);
    savecursor;
    end;
  TempClose;
  freehelp;

  { -> evtl. normaler Video-Mode }
  sm2t:=m2t;
  attrtxt(7);
  case abs(cls) of
    1,2,4 : begin
              clrscr;
              m2t:=false;
            end;
    3   : begin
            clwin(1,ScreenWidth,4,screenlines-2);
            gotoxy(1,5);
          end;
  end;
  {$ifdef Win32}
  // todo: adjust screen size with Win9x/ME only, not with WinNT/2000
  SysSetScreenSize(25,80);
  Window(1,1,80,25);
  {$endif}
  if (cls=2) or (cls=-1) then
  begin
    if shell25 and (screenlines>25) then
      SysSetScreenSize(25, 80);
    if cls=2 then writeln(getres(113));  { Mit EXIT geht''s zurueck zu CrossPoint. }
  end;
  cursor(curon);

  Errorlevel := Xec(prog,'[XP]');

  if shellkey or (ParDebFlags and 2<>0) or ShellWaitkey then
  begin
    if deutsch and (random<0.02) then write('Pressen Sie einen Schluessel ...')
    else write(getres(12));  { Taste druecken ... }
    m2t:=false;
    pushhp(51);
    clearkeybuf;
    wait(curon);
    pophp;
    m2t:=true;
    shellkey:=false;
  end;

  SysSetBackintensity;
  SetScreenSize;
  cursor(curoff);
  if (cls<>4) and (cls<>5) then holen(sp);
  m2t:=sm2t;
  Disp_DT;
  if (cls<>4) and (cls<>5) then restcursor;

  xp_maus_an(maussave.x,maussave.y);

  if (ErrorLevel<0) and (ErrorLevel<>-4) then
    fehler(ioerror(-ErrorLevel,getres(115)));   { Fehler bei Programm-Aufruf }

  if trackpath then getdir(0,shellpath);
  SetCurrentDir(OwnPath);
  TempOpen;
end;

{ Execute an external program and add any files created in current dir to SL }
function ShellNTrackNewFiles(prog:string; space:xpWord; cls:shortint; SL: TStringList): Integer;
var dir1,dir2: TDirectory; curdir,newfiles: string; i,j: Integer; fileexisted: boolean;
begin
  curdir:=GetCurrentDir;
  dir1:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  Shell(prog,space,cls);
  result:=errorlevel;
  newfiles:='';
  SetCurrentDir(curdir);
  dir2:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  for i:=0 to dir2.Count-1 do begin
    fileexisted:=false;
    for j:=0 to dir1.Count-1 do
      if dir2.Name[i]=dir1.Name[j] then fileexisted:=true;
    if not fileexisted then begin
      SL.Add(ExpandFilename(dir2.Name[i]));
      if newfiles<>'' then newfiles:=newfiles+', ';
      newfiles:=newfiles+ExpandFilename(dir2.Name[i]);
      end;
    end;
  dir1.destroy; dir2.destroy;
  Debug.DebugLog('xpnetcall','new files created by external program: '+newfiles,DLDebug);
  SetCurrentDir(OwnPath);
end;

function listheadercol:byte; { Headerzeilenfarbe entsprechend Hervorhebungsflag waehlen }
var nt : longint;
begin
  dbreadN(mbase,mb_netztyp,nt);
  listheadercol:=iif(nt and $1000 = 0,col.collistheader,col.collistheaderhigh);
end;

function listcolor(const s:string; line:longint):byte;
var p,p0,ml : byte;
    qn,pdiff: integer;
begin
  listhicol:=col.collisthigh;
  // highlight header lines
  if line<exthdlines then
    listcolor:=listheadercol
  else if s='' then
    listcolor:=0
  else if s[1]<=^c then
    listcolor:=iif((length(s)>1) and kludges,col.collistmarked,$ff)
  else begin
    p:=1;
    ml:=min(length(s),6);
    while (p<=ml) and ((s[p]=' ') or (s[p]=^I)) do
      inc(p);
    p0:=p;
    qn:=0;
    repeat
       while (s <> '') and (p<=length(s)) and (p-p0<6) and
       (
         (s[p]<>'>') and
         (not OtherQuoteChars or not (s[p] in QuoteCharSet))
       )
       do inc(p);
      pdiff:=p-p0;

      if (s <> '') and (p<=length(s)) and (s[p]='>') or
         (OtherQuoteChars and (p<=length(s)) and (s[p] in QuoteCharSet)) then
      begin
        inc(qn);
        p0:=p;
      end;
      inc(p);
    until (p>length(s)) or (pdiff=6);
    if qn<1 then
      listcolor:=0
    else begin
      listcolor:=col.collistquote[min(qn,iif(QuoteColors,9,1))];
      listhicol:=col.collistqhigh[min(qn,iif(QuoteColors,9,1))]
      end;
    end;
end;


{ 0=normal, -1=Minus, 1=Plus, 2=links, 3=rechts, 4=P/B/^P/^B (ListKey),
  5="0", 6=PgUp, 7=PgDn }

function listfile(name,header:string; savescr,listmsg:boolean;
                  utf8:boolean;
                  cols:shortint):shortint; { Lister }
var
    List   : TLister;
    p      : scrptr;
    oldm   : byte;
    msg    : boolean;
    lf     : string;
    pp     : byte;
    lt     : byte;
    lfirst : byte;     { Startzeile Lister }
    lofs   : xpWord;     { Ladeposition Datei }
    dphb   : byte;     { Uhr Hintergrundfarbe Backup }
    wrapb  : boolean;  { Backup no_ListWrapToggle }

    OldTCS,OldLCS: TMimeCharsets;

  procedure ShowMsgHead;
  var t : text;
      s : string;
      i : integer;
  begin
    assign(t,name); reset(t);
    attrtxt(listheadercol);
    if UTF8 then SetLogicalOutputCharset(csUTF8);
    for i:=1 to exthdlines do begin
      readln(t,s);
      mwrt(1,lfirst,' '+forms(s,79+ScreenWidth-80));
      inc(lfirst);
      inc(lofs,length(s)+2);
      end;
    if UTF8 then SetLogicalOutputCharset(csInternal);
    close(t);
    exthdlines:=0;
    lfirst:=min(lfirst,screenlines-5);
  end;

begin
  listexit:=0;
  wrapb:=no_ListWrapToggle;
  no_ListWrapToggle:=false;
  dphb := 0;
  if varlister<>'' then begin
    lf:=repfile(VarLister,name);
    pp:=pos('$TYPE',UpperCase(lf));
    if pp>0 then begin
      lt:=iif(listmsg,iif(listkommentar,2,1),0);
      lf:=LeftStr(lf,pp-1)+strs(lt)+mid(lf,pp+5);
      end;
    shell(lf,0,1);
    if errorlevel in [100..110] then ExtListKeys;
    end
  else begin
    if savescr then sichern(p);
    lfirst:=iif(listvollbild,1,4); lofs:=0;
    if listvollbild then begin                      { Bei Vollbild-lister : }
      if not listmsg or not listuhr then m2t:=false { Uhr nur im Message Lister... }
      else begin
        dphb:=dphback;
        if listmsg and ListFixedhead and (exthdlines>0) then   {   Wenn fester Header }
          dphback:=listheadercol                   {   dann Uhr aktiv mit Headerfarbe }
        else begin
          dphback:=col.colliststatus;              {   bei freiem Header }
          timey:=2;                                {   Uhr in Zeile 2 und Statuszeilenfarbe}
          end;
        end;
      end;
    if ListMsg and ListFixedHead then
      ShowMsgHead;

    if IsFileUTF8(name) then UTF8 := true;
    if UTF8 then
    begin
      OldTCS := GetConsoleOutputCharset;
      OldLCS := GetLogicalOutputCharset;
      SetConsoleOutputCharset(csUTF8);
      SetLogicalOutputCharset(csCP437);
{$IFDEF ANALYSE}  //prevent warning "not initialized"
    end else begin
      OldTCS := csCP437;
      OldLCS := csCP437;
{$ENDIF}
    end;

    List := TLister.CreateWithOptions(1,iif(_maus and listscroller,screenwidth-1,screenwidth),lfirst,
             iif(listvollbild,screenlines,screenlines-fnkeylines-1),
             iif(listvollbild,1,4),'/F1/MS/S/APGD/'+iifs(listendcr,'CR/','')+
             iifs(_maus and ListScroller,'VSC/','')+
             iifs(listmsg,'ROT/',''));
    if listwrap {or listkommentar} then
      list.Stat.WrapPos := iif(_maus and listscroller,78,80)+ScreenWidth-80;
//!!    if listmsg and ConvIso then List.OnConvert := ISO_conv;
    if not ListAutoscroll then List.Stat.Autoscroll := false;
    msg:=(_filesize(name)>1024*100);
    if msg then rmessage(130);    { 'Lade Datei ...' }
    List.UTF8Mode := utf8;
    List.ReadFromFile(name,lofs,true);
    if msg then closebox;
    List.HeaderText := header;
    List.OnKeypressed := listExt;
    llh:=listmsg;
    oldm:=ListMakros;
    if listmsg then ListMakros:=8;
    if cols<>0 then
    begin
      List.OnColor := listColor;
      if cols and 2<>0 then
      begin
        if utf8 then 
          List.OnDisplayLine := ListdisplayUTF8
        else
          List.OnDisplayLine := Listdisplay;
        xp1o.ListXHighlight:=ListHighlight;
        end;
      end;
    pushhp(39);
    if _maus and listscroller and listvollbild then begin
      attrtxt(col.colliststatus);
      mwrt(1,lfirst,sp(ScreenWidth));
      end;

    if listmsg then
    begin
      dbReadN(mbase,mb_halteflags,listhalten);
      dbReadN(mbase,mb_unversandt,listunvers);
      dbReadN(mbase,mb_flags,listflags);
      end
    else begin
      Listunvers:=0; Listhalten:=0; Listflags:=0;
      end;

    List.Show;
    Listunvers:=0; Listhalten:=0; Listflags:=0;
    pophp;
    ListMakros:=oldm;

    if UTF8 then
    begin
      SetConsoleOutputCharset(OldTCS);
      SetLogicalOutputCharset(OldLCS);
    end;

    if listvollbild and listuhr and ListMsg then begin
     dphback:=dphb;                        {Uhrfarbe reseten}
     if not Listfixedhead then timey:=1;   {Und evtl. Position}
     end;
   m2t:=true;
    if savescr then holen(p);
    List.Free;
  end;
  exthdlines:=0;
  llh:=false;
  if listexit<>4                         { Wenn nicht Editor gestartet wird... }
    then otherquotechars:=otherqcback;   { Status der Quotechars '|' und ':' reseten }
  listfile:=listexit;
  no_ListWrapToggle:=wrapb;
end;


procedure RemoveEOF(const fn:string);
var f : file;
    b : byte;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult<>0 then exit;    { Datei nicht gesichert }
  if filesize(f)>0 then begin
    seek(f,filesize(f)-1);
    blockread(f,b,1);
    if b=26 then begin
      seek(f,filesize(f)-1);
      truncate(f);
      end;
    end;
  close(f);
end;


{ reedit: Nachbearbeiten einer XP-erzeugten-Nachricht - }
{         TED-Softreturns zurueckwandeln                 }

procedure editfile(const name: string; nachricht,reedit,senden:boolean;
                   keeplines:byte;ed_ukonv:boolean);
var
    bak : string;
    ms  : boolean;
begin
  if ((exteditor=3) or ((exteditor=2) and nachricht)) and (VarEditor<>'')
     and (VarEditor[1]<>'*') then begin
    ms:=shell25; shell25:=edit25;
    shell(repfile(VarEditor,name),0,-1);
    shell25:=ms;
    removeeof(name);
    bak := EditorBakExt;
  end
  else begin
    if nachricht then pushhp(54);
    TED(name,reedit,keeplines,ed_ukonv,nachricht,senden);
    if nachricht then pophp;
    if nachricht and (FirstChar(VarEditor)='*') then begin
      DeleteFirstChar(VarEditor);
      shell(repfile(VarEditor,name),0,3);
      insert('*',VarEditor,1);
      end;
    bak:='BAK';
    end;
  if bak<>'' then
    SafeDeleteFile(ChangeFileExt(Name, '.'+bak)); { .BAK löschen }
end;


{ Achtung! ShellPath kann mit oder ohne '\' am Ende sein! }

procedure dosshell;

  {$IFNDEF DPMI}
  function environment:string;
  begin
    if envspace=0 then environment:=''
    else environment:=' /E:'+strs(envspace);
  end;
  {$ENDIF }

begin
  if DisableDos then
    fehler(getres(116))   { DOS-Shell hier nicht moeglich }
  else begin
    SetCurrentDir(ShellPath);
    if ioresult<>0 then SetCurrentDir(ownpath);
    trackpath:=true;
    {$IFDEF Unix }
      shell(getenv('SHELL'),0, 2);
    {$ELSE }
      shell(getenv('COMSPEC')+environment,640,2);
    {$ENDIF }
    trackpath:=false;
    end;
end;

procedure delete_tempfiles;
begin
  SafeDeleteFile(TempPath+swapfilename);
  SafeDeleteFile(TempPath+MsgTempFile);
  SafeDeleteFile(TempPath+'header.hdr');
end;


{ --- Bildschirmzeilen -------------------------------------}

{ Zeilenzahl einstellen; evtl. Videomodus zuruecksetzen }

procedure setscreensize;
var
  ma  : map;
  n,i : integer;
begin
  // Modus nochmal setzen
  SysSetScreenSize(ScreenLines, ScreenWidth);
  screenlines := SysGetScreenLines;
  screenwidth := SysGetScreenCols;
  {$IFDEF NCRT}xpcurses.{$ENDIF}window(1,1,screenwidth,screenlines);
  cursor(curoff);

  getmem(ma,sizeof(menuarray));
  splitmenu(ZeilenMenue,ma,n,true);
  for i:=1 to n do
    if screenlines=ival(ma^[i].mstr) then menupos[ZeilenMenue]:=i;
  freemem(ma,sizeof(menuarray));
end;


procedure showusername;
var d    : DB;
    user : string;
    realname : string;
    nt       : eNetz;

  procedure showtline;
  begin
    attrtxt(col.coltline);
{$IFDEF NCRT }
    HorizLine(3);
{$ELSE }
    wrt(1,3,dup(screenwidth,'ß'));
{$ENDIF }
  end;

begin
  if dispusername and not startup then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,UpperCase(DefaultBox));
    showtline;
    if dbFound then begin
      nt:=dbNetztyp(d);
      realname:=iifs(ntRealname(nt),dbReadStr(d,'realname'),'');
      user:=ComputeUserAddress(d);
      if realname <> '' then begin
        if (length(user)+length(realname)) <= screenwidth-7 then
          user := user + ' ('+realname+')'
        else if length(user) <= screenwidth-10 then
          user := user + ' ('+leftStr(realname,screenwidth-10-length(user))+'...)';
      end;
      mwrt(screenwidth-2-length(user),3,' '+user+' ');
      end;
    dbClose(d);
    end
  else
    showtline;
end;


procedure showscreen(newmode:boolean);
begin
  xp_maus_aus;
  attrtxt(7);
  setscreensize;
  lines(1);
  clrscr;
  SysSetbackIntensity;
  with col do begin
    attrtxt(colmenu[0]);
    Wrt2(sp(screenwidth));
    showusername;
    dispfunctionkeys(false);
    attrtxt(coltline);
{$IFDEF NCRT }
    HorizLine(screenlines-fnkeylines);
{$ELSE }
    mwrt(1,screenlines-fnkeylines,dup(screenwidth,'Ü'));
{$ENDIF }
    normtxt;
    end;
  showmain(0);
  dphback:=col.colmenu[0]; setseconds(false,true);
  timex:=74; timey:=1; m2t:=true;
  disp_DT;
  attrtxt(7);
  gotoxy(1,4);
  xp_maus_an(mausdefx,mausdefy);
end;


{ --- Videomode nach Shell- bzw. externem Aufruf neusetzen ----- }

procedure exitscreen(joke:shortint);
var i : integer;
begin
  moff;
  attrtxt(7);
  clrscr;
  if deutsch then
    case joke of
      1 : cm_wl('Vielen Dank. Sie haben ein einfaches Pointprogramm sehr gluecklich gemacht.');
      2 : cm_wl('Leider verloren.');
    end;
  if (res2anz(221)>0) and (getres2(221,1)<>'(dummy)') then begin
    writeln;
    for i:=1 to res2anz(221) do
      cm_wl(getres2(221,i));
    end;
  cm_wln;
end;


{ fnkeylines und gl anpassen }

procedure lines(fnkey:byte);
begin
  fnkeylines:=fnkey;
  gl:=screenlines-4-fnkeylines;
end;

{ --- Dialog- und sonstige Boxen ------------------------------- }

procedure getpos(width,height: Integer; var x,y: Integer);
begin
  x:=(screenwidth-width)div 2 +1;
  y:=(ScreenLines-height+1) div 2 +1;
end;

procedure openbox(width,height: Integer; const txt:string; var x,y: Integer; c1,c2: Integer);
begin
  getpos(width,height,x,y);
  openboxat(width,height,txt,x,y,c1,c2);
end;

procedure openboxat(width,height: Integer; const txt:string; x,y: Integer; c1,c2: Integer);
begin
  blindon(true);
  wpushs(x,x+width-1,y,y+height-1,'-');
  attrtxt(c1);
  forcecolor:=true;
  case getrahmen of
    1 : rahmen1(x,x+width-1,y,y+height-1,'');
    2 : rahmen2(x,x+width-1,y,y+height-1,'');
  end;
  forcecolor:=false;
  if txt<>'' then
    mwrt(x+2,y,' '+txt+' ');
  attrtxt(c2);
  clwin(x+1,x+width-2,y+1,y+height-2);
end;

procedure msgbox(width,height: Integer; const txt:string; var x,y: Integer);
begin
  openbox(min(width,screenwidth),height,txt,x,y,col.colmboxrahmen,col.colmbox);
end;


procedure diabox(width,height: Integer; const txt:string; var x,y: Integer);
begin
  openbox(width,height,txt,x,y,col.coldiarahmen,col.coldialog);
end;


procedure selbox(width,height: Integer; const txt:string; var x,y: Integer; hell:boolean);
begin
  openbox(width,height,txt,x,y,
          iif(hell,col.colselrahmen,col.colsel2rahmen),
          iif(hell,col.colselbox,col.colsel2box));
end;

procedure ListboxCol(List: TLister);
var lc : listcol;
begin
  with lc do
  begin
    coltext:=col.colselbox;
    colselbar:=col.colselbar;
    colmarkline:=col.colselhigh;
    colmarkbar:=col.colselbar and $f0 + col.colselhigh and $f;
    { colscroll:=col.colselscroll; }
    List.col := lc;
  end;
end;

function listbox(width,height: Integer; const txt:string): TLister;
var x,y : Integer;
begin
  selbox(width+2,height+2,txt,x,y,true);
  Result := TLister.CreateWithOptions(x+1,x+width,y+1,y+height,0,'/NS/SB/NLR/DM/');
  ListboxCol(result);
  Result.SetArrows(x,y+1,y+height,col.colselrahmen,col.colselrahmen,'³');
end;


procedure utilbox(l,r,o,u: INteger; const txt:string);
begin
  blindon(true);
  attrtxt(col.colutility);
  forcecolor:=true;
  wpushs(l,r,o,u,'');
  forcecolor:=false;
  if txt<>'' then
    mwrt(l+2,o,' '+txt+' ');
end;


procedure closebox;
begin
  wpop;
  blindoff;
end;


procedure WaitIt(txt:atext; p:proc; sec:xpWord);
begin
  message(txt);
  p;
  wkey(sec,false);
  closebox;
end;


procedure message(const txt:string);
var x,y : Integer;
begin
  msgbox(length(txt)+6,3,'',x,y);
  mwrt(x+3,y+1,LeftStr(txt,screenwidth-6));
end;

procedure rmessage(nr:xpWord);
begin
  message(getres(nr));
end;

procedure moment;
begin
  rmessage(105);   { 'Einen Moment bitte ...' }
end;


procedure dialog(width,height: Integer; const txt:string; var x,y: Integer);
begin
  diabox(width+2,height+2,txt,x,y);
  inc(x); inc(y);
  openmask(x,x+width-1,y,y+height-1,false);
  masksetfninfo(x+width-7,y+height,' [F2] ','Ä');
end;

procedure enddialog;
begin
  closemask;
  closebox;
end;


procedure WriteClipFile(fn:string);
begin
  if FileExists(fn) then begin
    FileToClip(fn);
    _era(fn);
    end;
end;


procedure errsound;
begin
  if not ParQuiet or soundflash then
  begin
    SysBeep(1000, 25);
    SysBeep(780, 25);
    if soundflash then
    begin
      mdelay(60);
    end;
  end;
end;

function _errsound:boolean;
begin
  errsound;
  _errsound:=true;
end;

procedure signal;              { s. Config/Anzeige/Hilfen }
begin
  if not ParQuiet and tonsignal then
  begin
    mdelay(60);
    SysBeep(1205, 60);
    SysBeep(1000, 60);
    SysBeep(800, 60);
  end;
end;

procedure _fehler(const txt:string; hinweis:boolean);
var x,y   : Integer;
    lcol  : byte;
    s: String;
begin
  Debug.DebugLog('_fehler', txt, DLError);
  s := LeftStr(txt, screenwidth-6);
  savecursor; lcol:=textattr;
  msgbox(length(s)+6,5,iifs(hinweis,_hinweis_,_fehler_),x,y);
  mwrt(x+3,y+2, s);
  errsound;
  wait(curoff);
  closebox;
  restcursor;
  attrtxt(lcol);
end;

procedure fehler(const txt:string);
begin
  _fehler(txt,false);
end;

procedure rfehler(nr:xpWord);
var s : string;
begin
  StripKey(KeyCr);
  s:=getres2(10000+100*(nr div 100),nr mod 100);
  freeres;
  pushhp(20000+nr);
  _fehler(s,false);
  pophp;
end;

procedure rfehler1(nr:xpWord; txt:string);
begin
  txt:=getreps2(10000+100*(nr div 100),nr mod 100,txt);
  freeres;
  pushhp(20000+nr);
  _fehler(txt,false);
  pophp;
end;

function mfehler(b:boolean; const txt:string):boolean;
begin
  if not b then _fehler(txt,false);
  mfehler:=not b;
end;


procedure hinweis(const txt:string);
begin
  _fehler(txt,true);
end;

function fehlfunc(const txt:string):boolean;
begin
  fehler(txt);
  fehlfunc:=true;
end;


procedure logerror(const txt:string);
var f : text;
begin
  assign(f,Logpath+ErrlogFile);
  append(f);
  if ioresult<>0 then rewrite(f);
  writeln(f,LeftStr(date,6),RightStr(date,2),' ',time,' ',txt);
  close(f);
  if ioresult<>0 then;   { Logpath koennte falsch gewesen sein }
end;

procedure tfehler(const txt:string; sec:integer);
var x,y : Integer;
begin
  Debug.DebugLog('_fehler', txt, DLError);
  msgbox(length(txt)+16,5,_fehler_,x,y);
  mwrt(x+3,y+2,LeftStr(txt,screenwidth-16)+'  '#4'  '+formi(sec div 60,2)+':'+
               formi(sec mod 60,2));
  GotoXY(WhereX-5, WhereY);
  errsound;
  logerror(txt);
  wkey(sec,true);
  closebox;
end;

procedure trfehler(nr:xpWord; sec:integer);
begin
  pushhp(20000+nr);
  tfehler(getres2(10000+100*(nr div 100),nr mod 100),sec);
  pophp;
  freeres;
end;

procedure trfehler1(nr:xpWord; const txt:string; sec:integer);
begin
  freeres;
  pushhp(20000+nr);
  tfehler(getreps2(10000+100*(nr div 100),nr mod 100,txt),sec);
  pophp;
end;

procedure afehler(const txt:string; auto:boolean);
begin
  if auto then
    tfehler(txt,20)
  else
    fehler(txt);
end;

procedure arfehler(nr:xpWord; auto:boolean);
begin
  if auto then
    trfehler(nr,20)
  else
    rfehler(nr);
end;


function ioerror(i:integer; const otxt:atext):atext;
var s : atext;
begin
  if ioresult<>0 then;
  if ResIsOpen then begin
    s:=getres2(12800,i);
    if LeftStr(s,5)='fehlt' then ioerror:=otxt
    else ioerror:=s;
    end
  else
    ioerror:=fileio.ioerror(i,otxt);
end;


procedure selcol;
begin
  normattr:=col.colselbox;
  invattr:=col.colselbar;
  highattr:=col.colselbox;
  normtxt;
end;

procedure file_box(var name:string; changedir:boolean);
begin
  if (cpos('*',name)>0) or (cpos('?',name)>0) then begin
    selcol;
    pushhp(89);
    name:=fsbox(ScreenLines div 2 - 5,name,'','',changedir,false,false);
    pophp;
    end;
end;


function mbrett(typ:char; intnr:longint):string;
begin
  mbrett:=typ+dbLongStr(intnr);
end;

function mbrettd(typ:char; dbp:DB):string;
begin
  mbrettd:=typ+dbLongStr(dbReadInt(dbp,'int_nr'));
end;


{ Internes Datumsformat:
  7.......0  7..43..0  76...210  7..43..0
  lod(Jahr)  mmmmtttt  thhhhhmm  mmmm0000  }


function longdat(l:longint):string;
var
  s: String;
  p: Integer;

  procedure WriteChar(i: Integer);
  begin
    Inc(p);
    s[p] := Char(i div 10 + Byte('0'));
    Inc(p);
    s[p] := Char(i mod 10 + Byte('0'));
  end;

begin
  SetLength(s, 10);
  p := 0;
  WriteChar((l shr 24) mod 100);
  WriteChar((l shr 20) and 15);
  WriteChar((l shr 15) and 31);
  WriteChar((l shr 10) and 31);
  WriteChar((l shr 4) and 63);
  Result := s;
{  Result  := formi((l shr 24) mod 100,2)+formi((l shr 20) and 15,2)+
           formi((l shr 15) and 31,2)+formi((l shr 10) and 31,2)+
           formi((l shr 4) and 63,2); }
end;

function ixdispdat(const dat:shortstring):longint;      { Datum -> Long   }
begin
  ixdispdat:=ixdat(RightStr(dat,2)+copy(dat,4,2)+LeftStr(dat,2)+'0000');
end;


function smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }
begin
  smdl:=(d1 shr 1) and $7fffffff < (d2 shr 1) and $7fffffff;
end;


function fdat(const dat:string):string;             { Z-Datum -> Datum  }
begin
  if Length(dat)<6 then begin
    Result := '';
    exit;
  end;

  SetLength(Result, 8);
  Result[1] := dat[5];
  Result[2] := dat[6];
  Result[3] := '.';
  Result[4] := dat[3];
  Result[5] := dat[4];
  Result[6] := '.';
  Result[7] := dat[1];
  Result[8] := dat[2];
//   fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+LeftStr(dat,2);
end;

function zdow(const dat:string):string;             { Z-Datum -> Mo/Di.. }
var j : xpWord;
    d : datetimest;
    n : integer;
begin
  j:=ival(LeftStr(dat,2))+1900;
  if j<1970 then inc(j,100);
  schalt(j);
  d:=fdat(dat);
  n:=_daylen_;
  zdow:=trim(copy(_days_,dow(copy(d,1,6)+strs(j))*n+1-n,n));
  { 'Montag    Dienstag  Mittwoch  DonnerstagFreitag   Samstag   Sonntag' }
end;


function ftime(const dat:string):string;            { Z-Datum -> Uhrzeit }
begin
  ftime:=copy(dat,7,2)+':'+copy(dat,9,2);
end;

{ Datum in Z-Format abfragen }

function Zdate:string;
begin
  result:= FormatDateTime('yymmddhhnn',Now);
end;


procedure wait(cur:curtype);
var t : taste;
begin
  repeat
    get(t,cur)
  until (t=mausleft) or (t=mausright) or (t=mausldouble) or
        (t<mausfirstkey) or (t>mauslastkey);
  if (t=mausleft) or (t=mausright) then
    repeat
      get(t,cur)
    until (t=mausunleft) or (t=mausunright);
end;


{ === Parser-Routinen ============================ }

{ parse a configuration line.
  search for conf key in s (has to be uppercase), return true if found,
  pos of '=' in p. multiple keys may be separated by '|'.
  example: scomp('TEST2=DUMMY','test1|test2',p) => true, p=6

  Optimized for performance. Called often upon program start. }

function scomp(const s, Keys: string; var p: integer):boolean;
var
  p0: Integer;
  ConfKey, UpperKeys: String;
begin
  UpperKeys := UpperCase(Keys);
  p := cpos('=', s);
  ConfKey := LeftStr(s, p-1);
  p0 := cpos('|', UpperKeys);
  if p0 = 0 then
  begin
    Result := UpperKeys = ConfKey
  end else
  begin
    repeat
      Result:= Trim(Copy(UpperKeys, 1, p0-1)) = ConfKey;
      Delete(UpperKeys, 1, p0);
      p0 := cpos('|', UpperKeys);
    until p0 = 0;
  end;
end;


function getb(const su:string; const v:string; var b:byte):boolean;
var
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),b,res);
    getb:=(res=0);
    end
  else getb:=false;
end;

function getc(const su, v:string; var c:char):boolean;
var 
  p: Integer;
begin
  if scomp(su,v,p) and (p + 1 <= Length(su)) then
  begin
    c:=su[p+1];
    Getc := true;
  end else
    Getc := false;
end;

function geti(const su, v:string; var i:integer):boolean;
var   
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),i,res);
    geti:=(res=0);
    end
  else geti:=false;
end;

function geti16(const su, v:string; var i:integer16):boolean;
var   
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),i,res);
    geti16:=(res=0);
    end
  else geti16:=false;
end;

function getw(const su, v:string; var w:smallword):boolean;
var   
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),w,res);
    getw:=(res=0);
    end
  else getw:=false;
end;

function getl(const su, v:string; var l:longint):boolean;
var   
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),l,res);
    getl:=(res=0);
    end
  else getl:=false;
end;

function getr(const su, v:string; var r:double):boolean;
var   
  res, p: Integer;
begin
  if scomp(su,v,p) then begin
    val(trim(Mid(su,p+1)),r,res);
    getr:=(res=0);
    end
  else getr:=false;
end;

function getx(const su, v:string; var b:boolean):boolean;
var ss : string;
    p  : Integer;
begin
  if scomp(su,v,p) then
  begin
    ss:=trim(copy(su,p+1,1));
    if ss='J' then begin
      b:=true; getx:=true;
      end
    else if ss='N' then begin
      b:=false; getx:=true;
      end
    else
      getx:=false;
    end
  else getx:=false;
end;

function gets(const s,su, v:string; var ss:string):boolean;
var 
  p: Integer;
begin
  if scomp(su,v,p) then
  begin
    ss := Mid(s, p+1);
    gets:=true;
  end else
    gets:=false;
end;


function fuser(const s:string):string;              { Spacec vor/hinter '@' }
var 
  p : Integer;
begin
  p:=cpos('@',s);
  if p=0 then fuser:=s
  { else fuser:=LeftStr(s,p-1)+' @ '+copy(s,p+1,80); }
  else fuser:=LeftStr(s,p-1)+' @ '+RightStr(s,length(s)-p+1);
end;

function aufnahme_string:string;
begin
  aufnahme_string:=getres2(108,minmax(useraufnahme,0,3));
end;


function IS_QPC(var betreff:string):boolean;
begin
  IS_QPC:=(LeftStr(betreff,length(QPC_ID))=QPC_ID);     { QPC: }
end;

function IS_DES(var betreff:string):boolean;
begin
  IS_DES:=(LeftStr(betreff,length(DES_ID))=DES_ID);     { DES: }
end;

function IS_PMC(var betreff:string):boolean;
begin
  IS_PMC:=(LeftStr(betreff,length(PMC_ID))=PMC_ID);     { *crypted* }
end;


{ Datum des letzten Netcalls merken }

procedure write_lastcall(const dat:string);
var t : text;
    s : shortstring;
begin
  assign(t,ownpath+NewDateFile);
  rewrite(t);
  writeln(t,dat);
  close(t);
  if readmode=rmNeues then begin
    s:= dat;
    readdate:=ixdat(s);
  end;
end;


function aFile(nr:byte):string;
begin
  aFile:=AblagenFile+strs(nr);
end;


{--- Allgemeine VFuncs fuer Eingabemasken -------------------------}

function notempty(var s:string):boolean;
begin
  result := trim(s) <> '';
  if not result then errsound;
end;


{-----------------------------------------------------------------}

procedure opendatabases;
begin
  if mbase=nil then begin
    dbOpen(mbase,ownpath+msgFile,1);
    dbOpen(ubase,ownpath+userFile,1);
    dbOpen(bbase,ownpath+brettFile,1);
    dbOpen(bezbase,ownpath+bezugFile,1);
    dbOpen(mimebase,ownpath+mimetFile,1);
    end;
  opendb:=true;
end;

procedure closedatabases;
begin
  if ioresult=0 then;
  if mbase<>nil then dbClose(mbase);
  if ubase<>nil then dbClose(ubase);
  if bbase<>nil then dbClose(bbase);
  if bezbase<>nil then dbClose(bezbase);
  if mimebase<>nil then dbClose(mimebase);
  opendb:=false;
end;

procedure TempClose;
begin
  if opendb and not closed then begin
    dbTempClose(mbase);
    dbTempClose(ubase);
    dbTempClose(bbase);
    dbTempClose(bezbase);
    dbTempClose(mimebase);
    if miscbase<>nil then
      dbTempClose(miscbase);
    closed:=true;
    end;
end;

procedure TempOpen;
begin
  if opendb and closed then begin
    dbTempOpen(mbase);
    dbTempOpen(ubase);
    dbTempOpen(bbase);
    dbTempOpen(bezbase);
    dbTempOpen(mimebase);
    if miscbase<>nil then
      dbTempOpen(miscbase);
    closed:=false;
    end;
end;

procedure FlushClose;
begin
  TempClose;
  TempOpen;
end;

{ alle restlichen Bytes ab fpos(f1) nach f2 kopieren }

function fmove(var f1,f2:file): boolean;
const 
  BufSize = 65536;
var x,y   : Integer;
    p     : pointer;
    box   : boolean;
    fpos,
    fsize : longint;
    rr: Integer;

  procedure show(n:longint);
  begin
    inc(fpos,n);
    if box then mwrt(x+3,y+2,dup(system.round(fpos * 50.0  / fsize),'²'));
  end;

begin
  result:=true;
  getmem(p, BufSize);
  fsize:=filesize(f1)-filepos(f1);
  if fsize>0 then begin
    box:=(fsize>1024*1024) and (ExtractFileExt(FileName(f1))<>'.$$$');
    if box then begin
      MsgBox(56,5,getreps(134,extractfilename(FileName(f1))),x,y);
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,dup(50,'°'));
      fpos:=0;
      end;
    while not eof(f1) and (inoutres=0) do begin
      blockread(f1,p^, BufSize,rr);
      show(rr div 2);
      blockwrite(f2,p^,rr);
      show(rr - rr div 2);
      end;
    if box then begin
      mdelay(300);
      closebox;
      end;
    if inoutres<>0 then begin
      fehler(ioerror(ioresult,getres(102)));  { Fehler beim Dateizugriff :-( }
      result:=false;
      end;
    end;
  Freemem(p);
end;


function TempFree:Int64;                 { Platz auf Temp-Laufwerk }
begin
  if temppath='' then
    TempFree:=diskfree(0)
  else
    TempFree:=diskfree(ord(temppath[1])-64);
end;


function TempS(bytes:longint):string;
begin
  if (temppath='') or (FirstChar(temppath)=FirstChar(ownpath)) or (TempFree+4096>bytes) then
    TempS:=TempFile(TempPath)
  else
    TempS:=TempFile(OwnPath);
end;

function TempExtS(bytes:longint;const startnamewith,ext:string):string;
begin
  if (temppath='') or (temppath[1]=ownpath[1]) or (TempFree+4096>bytes) then
    Result:=TempExtFile(TempPath,startnamewith,ext)
  else
    Result:=TempExtFile(OwnPath,startnamewith,ext);
end;

procedure _era(const Filename:string);
begin
  if (FileName <> '')  and not sysutils.DeleteFile(Filename) then
  begin
    {$IFDEF WIn32 }
      Debug.DebugLog('xp1','_era('''+Filename+'''): '+SysErrorMessage(GetLastError),dlError);
    {$ENDIF }
    trfehler1(4,'"'+Filename+'"',30);   { 'Kann "'+(fn)+'" nicht loeschen!?' }
  end;
end;

procedure SafeDeleteFile(const Filename: String);
begin
  if (FileName <> '') and FileExists(Filename) and not Sysutils.DeleteFile(Filename) then
  begin
    {$IFDEF WIn32 }
      Debug.DebugLog('xp1','SafeDeleteFile('''+Filename+'''): '+SysErrorMessage(GetLastError),dlError);
    {$ENDIF }
    trfehler1(4,'"'+Filename+'"',30);   { 'Kann "'+(fn)+'" nicht loeschen!?' }
  end;
end;

procedure SafeMakeBak(const Filename, NewExt: string);
begin
  if not MakeBak(Filename,NewExt) then
  begin
    {$IFDEF WIn32 }
      Debug.DebugLog('xp1','SafeMakeBak('''+Filename+''','''+NewExt+'''): '+SysErrorMessage(GetLastError),dlError);;
    {$ENDIF }
    trfehler1(24,'"'+Filename+'"',30);   { 'Kann "'+(fn)+'" nicht umbenennen.' }
  end;
end;

procedure _chdir(p:string);
begin
  p:=trim(p);
  if p<>'' then 
  begin
    TrimLastChar(p, DirSepa);
    chdir(p);
    if ioresult<>0 then
      trfehler1(5,UpperCase(p),30);   { ungueltiges Verzeichnis: }
    end;
end;

function testmem(size:longint; wait:boolean):boolean;
begin
  if memavail<=size+16 then begin
    if wait then trfehler(6,30)  { 'zu wenig freier Speicher' }
    else rfehler(6);
    testmem:=false;
    end
  else
    testmem:=true;
end;


procedure exchange(var s:string; const repl,by:string);
var p : Integer;
begin
  p:=pos(UpperCase(repl),UpperCase(s));
  if p>0 then s:=copy(s,1,p-1)+by+Mid(s,p+length(repl));
end;


procedure XpIdle;
begin
  mdelay(1);
end;

procedure set_checkdate;
{$IFDEF FPC }
var
  Handle: Integer;
begin
  Handle := FileOpen(NewDateFile, fmOpenReadWrite);
  FileSetDate(Handle, DateTimeToFileDate(Now));
  FileClose(Handle);
end;
{$ELSE }
begin
  FileSetDate(NewDateFile, DateTimeToFileDate(Now));
end;
{$ENDIF }


procedure XP_testbrk(var brk:boolean);
begin
  if not brk then begin
    testbrk(brk);
    if brk then begin
      pushhp(1504);
      if not ReadJN(getres(160),true) then brk:=false;   { 'Abbrechen' }
      pophp;
      end;
    end;
end;


procedure xp_DB_Error;    { Aufruf bei <DB> internem Fehler }
var i : integer;
begin
  if ioresult<>0 then;
  attrtxt(15);
  writeln;
  writeln;
  for i:=1 to res2anz(161) do   { Hinweise, was bei beschaedigter Datenbank }
    writeln(getres2(161,i));    { zu tun ist                               }
  writeln;
end;

{ rechten Teil der ID in LowerCase umwandeln und CRC32 bilden }

function MsgidIndex(mid:string):longint;
var p : integer;
begin
  p:=cposx('@',mid)+1;
  while p<=length(mid) do begin
    mid[p]:=system.upcase(mid[p]);
    inc(p);
    end;
  MsgidIndex:=CRC32Str(mid);
end;

const cm = false;

procedure cm_w(const s:string);
begin
  if cm then write(s)
  else write(s);
end;


var
  cm_lines : byte = 1;

procedure cm_wln;
{var   dummy : char; }
begin
  if cm then begin
    writeln;
    inc(cm_lines);
    if cm_lines=screenlines then begin
      if moremode then begin
        cm_w('<more>');
{        dummy:=}cm_key;
        cm_w(#13+'      '+#13);
      end;
      cm_lines:=1;
    end;
  end else
    writeln;
end;

procedure cm_wl(const s:string);
begin
  cm_w(s);
  cm_wln;
end;

function cm_key:char;
begin
  cm_key:=readkey;
end;

procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
var x,y : Integer;
    t   : taste;
begin
  x:=wherex; y:=wherey;
  brk:=false;
  repeat
    wrt(x,y,s);
    Wrt2(dup(maxlen-length(s), iifc(dot,'.',' ')) + dup(maxlen-length(s),#8));
    get(t,curon);
    if (t=keybs) and (s<>'') then DeleteLastChar(s)
    else if (t>=' ') and (length(s)<maxlen) then s:=s+t;
  until (t=keycr) or (t=keyesc) or (t=^X);
  brk:=(t<>keycr);
  s:=trim(s);
  writeln;
  cursor(curon);
end;

{ returns user identifier (email address with RFC nets) }
function ComputeUserAddress(d: DB):string;
var flags     : byte;
    netztyp   : eNetz;
    boxname   : string;
    username  : string;
    pointname : string;
    domain    : string;
    email     : string;
    aliaspt   : boolean;
begin
  netztyp := dbNetztyp(d);
  boxname := dbReadStr(d, 'boxname');
  username := dbReadStr(d, 'username');
  pointname := dbReadStr(d, 'pointname');
  dbRead (d, 'script', flags);
  aliaspt := (flags and 4 <> 0);
  domain := dbReadStr(d, 'domain');
  eMail := dbReadStr(d, 'email');
  case netztyp of
    nt_Maus    : result:=username + '@' + boxname;
    nt_ZConnect: result:=username + '@' +
                         iifs (aliaspt, pointname, boxname) + domain;
    nt_UUCP    : result:=iifs(email<>'', email, username + '@' +
                              iifs (aliaspt, boxname + ntServerDomain(boxname),
                                             pointname + domain));
  else
    if netztyp in netsRFC then begin
      if cpos('@',username)<>0 then
        // old versions stored email address in username db fields
        result:= username
      else
        result:= email;
      end else
      // Fido etc.
      result :=username + ' @ ' + boxname;
  end;
end;

{$IFDEF Snapshot}
function compiletime:string;      { Erstelldatum von XP.EXE als String uebergeben }
begin
  CompileTime := FormatDateTime('yyyy-mm-dd-hhnn', FileDateToDateTime(FileAge(OpenXPEXEPath)))
  {$IFDEF Delphi }
    + 'd'
  {$ENDIF }
  {$IFDEF Kylix }
    + 'k'
  {$ENDIF }
  {$IFDEF FPC }
    + 'f'
  {$ENDIF}
  ;
end;
{$ENDIF}

var
  SavedExitProc: pointer;

procedure ExitXP1Unit;
begin
  ExitProc:= SavedExitProc;
  if ioresult= 0 then ;
  dbReleaseCache;
  if not closed then closedatabases;
  SysSetBackIntensity;
  FreeMain;
end;

procedure InitXP1Unit;
begin
  SavedExitProc:= ExitProc;
  ExitProc:=@ExitXP1Unit;
end;


{
  $Log$
  Revision 1.180  2003/04/12 07:43:24  mk
  - scrptr is now a pointer instead of record

  Revision 1.179  2003/03/16 19:02:06  cl
  - initial support for langage files in encodings different from CP437

  Revision 1.178  2003/02/13 14:41:57  cl
  - implemented correct display of UTF8 in the lister
  - implemented Unicode line breaking in the lister

  Revision 1.177  2003/01/25 14:24:27  mk
  - GetLastError only for Win32

  Revision 1.176  2003/01/24 12:44:54  mk
  - added windows to uses

  Revision 1.175  2003/01/24 12:07:17  cl
  - added debug output for _era, SafeDeleteFile, SafeMakeBak

  Revision 1.174  2003/01/08 00:11:30  mk
  - fixed last checkin

  Revision 1.173  2003/01/07 11:18:26  mk
  - use cross platform FileSetDate

  Revision 1.172  2003/01/07 00:24:44  cl
  - added openboxat to allow non-centered boxes

  Revision 1.171  2003/01/01 16:19:44  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.170  2002/12/28 20:11:04  dodi
  - start keyboard input redesign

  Revision 1.169  2002/12/28 20:04:30  mk
  - made TXpHandle an integer
  - scrptr is now TXpHandle with ncrt

  Revision 1.168  2002/12/21 05:37:53  dodi
  - removed questionable references to Word type

  Revision 1.167  2002/12/14 07:31:29  dodi
  - using new types

  Revision 1.166  2002/12/12 11:58:42  dodi
  - set $WRITEABLECONT OFF

  Revision 1.165  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.164  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.163  2002/12/04 16:57:01  dodi
  - updated uses, comments and todos

  Revision 1.162  2002/11/14 20:05:48  cl
  - fixed range check error

  Revision 1.161  2002/09/09 09:06:34  mk
  - added const parameters

  Revision 1.160  2002/09/09 08:42:33  mk
  - misc performance improvements

  Revision 1.159  2002/08/03 15:27:16  mk
  - fixed compilation problem without define SNAPSHOT

  Revision 1.158  2002/07/31 19:26:21  ma
  - user=>email db field code synchronized with v3.8
    (does not need re-entering email address when upgrading from old
     versions now)

  Revision 1.157  2002/07/25 20:43:53  ma
  - updated copyright notices

  Revision 1.156  2002/06/15 14:28:40  mk
  - O- with delphi, avoid crashes with various functions

  Revision 1.155  2002/05/07 09:14:04  mk
  - last checkin fixed for fpc

  Revision 1.154  2002/05/07 07:58:37  mk
  - fixed set_checkdate again

  Revision 1.153  2002/05/03 17:04:12  mk
  - write _fehler and tfehler-messages to Debuglog

  Revision 1.152  2002/05/02 18:35:24  mk
  - better set_checkdate

  Revision 1.151  2002/05/01 16:34:15  mk
  - fixed last commit

  Revision 1.150  2002/04/30 08:54:03  mk
  - removed unnecessary ret

  Revision 1.149  2002/04/25 23:04:30  cl
  - fixed last commit

  Revision 1.148  2002/04/25 23:01:59  cl
  - BUGFIX: #216189 - Check letzter Programmstart
    replaced (deactivated) xp1.set_checkdate with portable version.

  Revision 1.147  2002/04/13 16:10:16  ma
  - fixed scomp: Scanning for multiple configuration keys did not work

  Revision 1.146  2002/04/09 08:51:33  mk
  - fixed potential ansistring-crash in TempS

  Revision 1.145  2002/04/07 14:48:29  mk
  - additional fix for xpme (ahidden is now SmalInt)

  Revision 1.144  2002/04/06 19:15:41  mk
  - fixed reading of xpmenu.dat from xpme

  Revision 1.143  2002/03/25 22:03:08  mk
  MY:- Anzeige der Stammbox-Adresse unterhalb der Menueleiste korrigiert
       und ueberarbeitet (bei aktivierter Option "C/A/D/Stammbox-Adresse
       anzeigen"):
       - Vollstaendige Adresse (statt nur Feld "Username") inkl. Domain
         wird angezeigt;
       - Alias-Points werden beruecksichtigt (RFC/UUCP und ZConnect);
       - Realname wird in Klammern angezeigt (falls es sich um einen
         Netztyp mit Realnames handelt) und ggf. automatisch gekuerzt, wenn
         die Gesamtlaenge von Adresse und Realname groesser als 76 Zeichen
         ist;
       - Bei einem Wechsel des Netztyps der Stammbox wird die Anzeige
         der Absenderadresse unterhalb der Menueleiste unmittelbar nach dem
         Wechsel aktualisiert.

  Revision 1.142  2002/03/03 21:55:16  mk
  - made readconfig about 20times faster

  Revision 1.141  2002/02/25 17:54:04  mk
  - little optimization for scomp

  Revision 1.140  2002/02/18 00:57:16  mk
  - fixed Arithmetic overflow in Show() from fmove

  Revision 1.139  2002/01/30 22:58:12  mk
  - test for empty FileName in SafeDeleFilename and _era

  Revision 1.138  2002/01/22 19:15:27  mk
  - after 3.40 merge fixes

  Revision 1.137  2002/01/21 23:30:12  cl
  - post-3.40 merge fixes

  Revision 1.136  2002/01/06 19:31:43  ma
  - getX now supports searching for multiple keys
    (provides backwards compatibility in case of changed key names)

  Revision 1.135  2002/01/05 16:01:08  mk
  - changed TSendUUData from record to class

  Revision 1.134  2002/01/03 19:11:41  cl
  - added config option for internal preliminal UTF-8 support

  Revision 1.133  2001/12/26 00:49:00  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)
  - added SafeMakeBat

  Revision 1.132  2001/12/09 14:36:40  mk
  - implemented SysBeep and error sounds

  Revision 1.131  2001/10/30 11:14:40  mk
  - JG: fixed Listdisplay, see <8Bj$$d0DkpB@ralle.post.rwth-aachen.de>

  Revision 1.130  2001/10/21 10:24:56  mk
  - fixed last commit to allow comile with win32

  Revision 1.129  2001/10/20 21:35:48  ml
  - saving/restoring terminal for calling external programs works now

  Revision 1.128  2001/10/17 20:56:23  mk
  - fixed AVs in exit parts of unit

  Revision 1.127  2001/10/17 10:07:38  ml
  - use integer for cursorpos to prevent range errors

  Revision 1.126  2001/10/11 15:27:02  mk
  - implemented direct screen writes for DOS32, no more LocalScreen

  Revision 1.125  2001/10/07 17:12:30  cl
  - added charset recoding for external editors
    and corresponding config option

  Revision 1.124  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.123  2001/09/21 16:16:47  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.122  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.121  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.120  2001/09/08 14:25:29  cl
  - added TempExtS (needed to create Temporary files for old PGP versions)

  Revision 1.119  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.118  2001/09/07 13:54:17  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.117  2001/08/28 08:16:03  mk
  - added some const parameters

  Revision 1.116  2001/08/28 08:04:02  mk
  - removed GetX-Workaround in Val for FPC
  - added const-parameters to scomp and GetX
  - some little optimizations in GetX-functions
  - GetS does not need MaxLength anymore

  Revision 1.115  2001/08/12 20:01:39  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.114  2001/08/11 23:06:28  mk
  - changed Pos() to cPos() when possible

  Revision 1.113  2001/08/11 21:20:50  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.112  2001/07/31 13:10:32  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.111  2001/07/28 14:34:15  ma
  - added some debug logs

  Revision 1.110  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.109  2001/07/27 18:10:11  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.108  2001/07/23 18:02:37  ma
  - fixed: Error messages were not displayed correctly

  Revision 1.107  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.106  2001/05/20 12:21:06  ma
  - added ShellTrackNewFiles

  Revision 1.105  2001/04/21 12:57:04  ma
  - fmove is a function now

  Revision 1.104  2001/03/22 18:25:09  ma
  - FmtDateTime: "mm" means "month", *not* "minute".

  Revision 1.103  2001/03/03 10:57:43  ml
  - compilable under linux

  Revision 1.102  2001/02/25 15:25:22  ma
  - added GPL headers
  - cosmetics

  Revision 1.101  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.100  2001/01/22 16:12:22  mk
  - added special handling for gets with maxlen = 0

  Revision 1.99  2001/01/05 18:35:03  ma
  - moved Exxec unit into XP1 unit

  Revision 1.98  2001/01/02 10:05:23  mk
  - implemented Header.References

  Revision 1.97  2000/12/25 14:02:40  mk
  - converted Lister to class TLister

  Revision 1.96  2000/12/03 22:23:08  mk
  - Improved Printing Support

  Revision 1.95  2000/12/03 12:38:20  mk
  - Header-Record is no an Object

  Revision 1.94  2000/11/25 10:31:47  mk
  - some fixes for new SendUUData

  Revision 1.93  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.92  2000/11/19 18:22:53  hd
  - Replaced initlization by InitxxxUnit to get control over init processes
}
end.



{   $Id$

    OpenXP - generic routines
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

{$I xpdefine.inc }

unit xp1;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
{$IFDEF linux }
{$IFDEF Kylix}
  libc,
  xplinux,
{$ELSE}
  linux,
{$ENDIF}  
{$ENDIF }
{$IFDEF Win32 }
  windows,
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
  crt,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
  typeform,montage,keys,fileio,inout,winxp,win2,
  datadef,database,mouse,maus2,help,maske,lister,printerx,clip,
  resource,xp0,crc,xpglobal,classes,debug,xpheader;

const maxhidden  = 500;                 { max. versteckte MenÅpunkte }

      DisableDOS : boolean = false;
      shellkey   : boolean = false;
      ListMakros : byte    = 0;         { Flag fÅr XPKEYS.XMakro     }
      Errorlevel : word    = 0;
      miscbase   : DB      = nil;       { wird bei Shell geschlossen }
      menurestart: boolean = false;     { fÅr Config-MenÅ            }

type mprec     = record
                   mstr    : string[30];
                   hpos    : byte;
                   hkey    : char;
                   enabled : boolean;
                   chain   : byte;      { UntermenÅ-Nr. }
                   keep    : boolean;   { MenÅ nicht verlassen }
                   mpnr    : integer;   { Nummer des MenÅpunkts }
                 end;
     menuarray = array[1..22] of mprec;
     map       = ^menuarray;
{$IFDEF NCRT }
     scrptr    = word;  { Handle }
{$ELSE }
     scrptr    = record
                   scsize  : word;
                   p       : pointer;
                 end;
{$ENDIF }
     ahidden   = array[1..maxhidden] of integer;

var printlines : longint;
    WaitKey    : taste;               { Taste, mit der wkey beendet wurde }
    llh        : boolean;             { "L"/"H" im Lister -> xp1o.listExt }
                                      { == Nachrichten-Lister             }
    rbx,rby    : Integer;             { Cursorposition fÅr ReadButton     }
    hidden     : ^ahidden;            { Liste der unsichtbaren MenÅpkte.  }
    anzhidden  : integer;             { Anzahl der unsichtbaren MenÅpkte. }


{$IFNDEF NCRT }
procedure sound(hz:word);
{$ENDIF }
procedure XpIdle;

procedure showscreen(newmode:boolean);
procedure showusername;
procedure exitscreen(joke:shortint);
procedure showmain(nr:shortint);      { HauptmenÅ anzeigen: nr=Position  }
function  mainkey(p:byte):taste;
procedure freemain;
procedure wait(cur:curtype);
procedure CondClearKeybuf;

procedure sichern(var sp:scrptr);
procedure holen(var sp:scrptr);

procedure hlp(nr:word);             { setzt helpst[helpstp] }
procedure pushhp(nr:word);
procedure pophp;
procedure freehelp;

function  allocsenduudatamem: senduuptr;
procedure clearsenduudata(sdata: senduuptr);
procedure freesenduudatamem(var sdata: senduuptr);

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
procedure rmessage(nr:word);
procedure WaitIt(txt:atext; p:proc; sec:word);
procedure WriteClipFile(fn:string);
procedure selcol;
procedure file_box(var name:string; changedir:boolean);
procedure XP_testbrk(var brk:boolean);

procedure errsound;
function  _errsound:boolean;
procedure signal;              { s. Config/Anzeige/Hilfen }
procedure fehler(const txt:string);
procedure rfehler(nr:word);
procedure rfehler1(nr:word; txt:string);
procedure hinweis(txt:string);
function  mfehler(b:boolean; txt:string):boolean;
function  fehlfunc(txt:string):boolean;
procedure logerror(txt:string);
procedure tfehler(txt:string; sec:integer);
procedure trfehler(nr:word; sec:integer);
procedure trfehler1(nr:word; const txt:string; sec:integer);
procedure afehler(txt:string; auto:boolean);
procedure arfehler(nr:word; auto:boolean);
procedure interr(const txt:string);
function  ioerror(i:integer; otxt:atext):atext;

procedure shell(const prog:string; space:word; cls:shortint);  { externer Aufruf }

{ Execute an external program and add any files created in current dir to SL }
function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;

function  listfile(name,header:string; savescr,listmsg:boolean;
                   cols:shortint):shortint; { Lister }
procedure RemoveEOF(fn:string);
procedure editfile(name:string; nachricht,reedit:boolean; keeplines:byte;
                   ed_ukonv:boolean);
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
procedure iso_conv(var buf; bufsize:word);

function  aFile(nr:byte):string;

function  mbrett(typ:char; intnr:longint):string; { Xpoint.Db1/Bretter erz. }
function  mbrettd(typ:char; dbp:DB):string;       { Int_Nr auslesen }
function  ixdat(s:shortstring):longint;           { Z-Date -> Long  }
function  longdat(l:longint):string;              { Long -> Z-Date  }
function  ixdispdat(dat:shortstring):longint;      { Datum -> Long   }
function  smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }

function  fdat(dat:string):string;             { Z-Datum -> Datum   }
function  zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
function  ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
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
function  TempExtS(bytes:longint;startnamewith,ext:string):string;
procedure _era(const Filename: String);
// Deletes a file only if exists, uses _era to report errors
procedure SaveDeleteFile(const Filename: String);
procedure _chdir(p:string);
function  testmem(size:longint; wait:boolean):boolean;

procedure cm_w(const s:string);                     { Command-Mode-Ausgabe }
procedure cm_wl(const s:string);                    { Writeln              }
procedure cm_wln;
procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
function  cm_key:char;

procedure InitXP1Unit;

implementation  {-------------------------------------------------------}

uses
  xp1o,xp1o2,xp1help,xp1input,xpe,xpnt,direct;

{ Diese Tabelle konvertiert NUR ôöÑîÅ· !    }
{ vollstÑndige ISO-Konvertierung: siehe XP3 }

const isotab1   : array[$c0..$ff] of byte =
             ($c0,$c1,$c2,$c3,{ $8e,}$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
              $d0,$d1,$d2,$d3,$d4,$d5,$99,$d7,$d8,$d9,$da,$db,$9a,$dd,$de,$e1,
              $e0,$e1,$e2,$e3,$84,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
              $f0,$f1,$f2,$f3,$f4,$f5,$94,$f7,$f8,$f9,$fa,$fb,$81,$fd,$fe,$ff);

      maxwinst  = 20;

      closed    : boolean = false;
      opendb    : boolean = false;
      mainmenu  : map = nil;            { HauptmenÅ }
      menulast  : byte = 0;             { Hîhe des Menu-Stacks }
      winstp    : integer = 0;

var  menulevel : byte;                  { MenÅebene }
     menustack : array[0..4] of byte;   { fÅr Rekonstruktion im Config-MenÅ }
     hmpos     : array[1..10] of integer;  { HauptmenÅ-XPos }
     main_n    : integer;               { MPs im HauptmenÅ }
     mainrange : array[1..10,0..1] of byte;
     listhicol : byte;
     winstack  : array[1..maxwinst] of scrptr;   { fÅr Blindensupport }
     mst       : boolean;


function  ixdat(s:shortstring):longint; assembler;  {&uses ebx, esi}
asm
         mov   esi,s
         inc   esi                      { LÑnge ist z.Zt. immer 10 }
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

procedure iso_conv(var buf; bufsize:word); assembler;  {&uses ebx, edi}
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

            0  +      2 + 4              ,{ Ä }
            0  +      2 + 4              ,{ Å }
            0  +      2 + 4              ,{ Ç }
            0  +      2 + 4              ,{ É }
            0  +      2 + 4              ,{ Ñ }
            0  +      2 + 4              ,{ Ö }
            0  +      2 + 4              ,{ Ü }
            0  +      2 + 4              ,{ á }
            0  +      2 + 4              ,{ à }
            0  +      2 + 4              ,{ â }
            0  +      2 + 4              ,{ ä }
            0  +      2 + 4              ,{ ã }
            0  +      2 + 4              ,{ å }
            0  +      2 + 4              ,{ ç }
            0  +      2 + 4              ,{ é }
            0  +      2 + 4              ,{ è }
            0  +      2 + 4              ,{ ê }
            0  +      2 + 4              ,{ ë }
            0  +      2 + 4              ,{ í }
            0  +      2 + 4              ,{ ì }
            0  +      2 + 4              ,{ î }
            0  +      2 + 4              ,{ ï }
            0  +      2 + 4              ,{ ñ }
            0  +      2 + 4              ,{ ó }
            0  +      2 + 4              ,{ ò }
            0  +      2 + 4              ,{ ô }
            0  +      2 + 4              ,{ ö }
            0  +          4              ,{ õ }
            0  +          4              ,{ ú }
            0  +          4              ,{ ù }
            0  +          4              ,{ û }
            0                            ,{ ü }
            0  +      2 + 4              ,{ † }
            0  +      2 + 4              ,{ ° }
            0  +      2 + 4              ,{ ¢ }
            0  +      2 + 4              ,{ £ }
            0  +      2 + 4              ,{ § }
            0  +      2 + 4              ,{ • }
            0  +          4              ,{ ¶ }
            0  +          4              ,{ ß }
            0  +  1 +         8          ,{ ® }
            0                            ,{ © }
            0                            ,{ ™ }
            0                            ,{ ´ }
            0                            ,{ ¨ }
            0  +  1 +         8          ,{ ≠ }
            0  +  1                      ,{ Æ }
            0  +              8          ,{ Ø }
            0                            ,{ ∞ }
            0                            ,{ ± }
            0                            ,{ ≤ }
            0                            ,{ ≥ }
            0                            ,{ ¥ }
            0                            ,{ µ }
            0                            ,{ ∂ }
            0                            ,{ ∑ }
            0                            ,{ ∏ }
            0                            ,{ π }
            0                            ,{ ∫ }
            0                            ,{ ª }
            0                            ,{ º }
            0                            ,{ Ω }
            0                            ,{ æ }
            0                            ,{ ø }
            0                            ,{ ¿ }
            0                            ,{ ¡ }
            0                            ,{ ¬ }
            0                            ,{ √ }
            0                            ,{ ƒ }
            0                            ,{ ≈ }
            0                            ,{ ∆ }
            0                            ,{ « }
            0                            ,{ » }
            0                            ,{ … }
            0                            ,{   }
            0                            ,{ À }
            0                            ,{ Ã }
            0                            ,{ Õ }
            0                            ,{ Œ }
            0                            ,{ œ }
            0                            ,{ – }
            0                            ,{ — }
            0                            ,{ “ }
            0                            ,{ ” }
            0                            ,{ ‘ }
            0                            ,{ ’ }
            0                            ,{ ÷ }
            0                            ,{ ◊ }
            0                            ,{ ÿ }
            0                            ,{ Ÿ }
            0                            ,{ ⁄ }
            0                            ,{ € }
            0                            ,{ ‹ }
            0                            ,{ › }
            0                            ,{ ﬁ }
            0                            ,{ ﬂ }
            0  +      2 + 4              ,{ ‡ }
            0  +      2 + 4              ,{ · }
            0  +      2 + 4              ,{ ‚ }
            0  +      2 + 4              ,{ „ }
            0  +      2 + 4              ,{ ‰ }
            0  +      2 + 4              ,{ Â }
            0  +      2 + 4              ,{ Ê }
            0  +      2 + 4              ,{ Á }
            0  +      2 + 4              ,{ Ë }
            0  +      2 + 4              ,{ È }
            0  +      2 + 4              ,{ Í }
            0  +      2 + 4              ,{ Î }
            0                            ,{ Ï }
            0                            ,{ Ì }
            0                            ,{ Ó }
            0                            ,{ Ô }
            0                            ,{  }
            0                            ,{ Ò }
            0                            ,{ Ú }
            0                            ,{ Û }
            0                            ,{ Ù }
            0                            ,{ ı }
            0                            ,{ ˆ }
            0                            ,{ ˜ }
            0                            ,{ ¯ }
            0                            ,{ ˘ }
            0                            ,{ ˙ }
            0                            ,{ ˚ }
            0  +          4              ,{ ¸ }
            0  +          4              ,{ ˝ }
            0                            ,{ ˛ }
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
            inc si                                 { dann Endzeichen Checken }

@talp2:     cmp al,byte ptr charbuf[esi]
            jne @tanext2

             mov bl,byte ptr charbuf[esi-1]
             test byte ptr delimiters[ebx],4     { Byte vor Endzeichen ok? }
             jz @tanext2
             mov bl,byte ptr charbuf[esi+1]
             test byte ptr delimiters[ebx],8     { Byte nach Endzeichen ok? }
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
            loop @tacopy2

@addspace:  mov word ptr charbuf[edi],'  '        { 2 Leerzeichen anhÑngen }
            pop eax
            jmp @ta1                               { ... und das Ganze nochmal }

@taende:    pop eax
            mov ecx,edx
            popa
            ret

{-------------------------}
@ende:
end; { of MakeListdisplay }


procedure ListDisplay(x,y:word; var s:string);
var
  s0: shortstring;
begin
  s0:= s;
  makelistdisplay(s0);
  Consolewrite(x,y,length(s0));
  s:= s0;                               { Falls var irgendeine Bedeutung hat }
end;

procedure interr(const txt:string);
begin
  moff;
  cm_wl(txt);
  runerror:=false;
  Debug.DebugLog('xp1','Internal error: '+txt,DLError);
  halt(1);
end;

{$IFNDEF NCRT }
procedure sound(hz:word);
begin
//  if not ParQuiet then
//    crt.sound(hz);
end;
{$ENDIF } { NCRT }


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

procedure hlp(nr:word);
begin
  helpst[helpstp]:=nr;
end;

procedure pushhp(nr:word);
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


{$I xp1menu.inc}   { MenÅfunktionen }


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
{$IFDEF NCRT}
var
  r: integer;
{$ENDIF}
begin
{$IFDEF NCRT }
  r:= getrahmen;
  setrahmen(0);
  wpull(1,screenwidth,1,screenlines,'',sp);
  setrahmen(r);
{$ELSE}
  with sp do
  begin
{$IFDEF Win32 }
    { Das Attribut belegt hier 2 Byte }
    scsize:=screenlines*screenwidth*4;
{$ELSE }
    scsize:=screenlines*2*screenwidth;
{$ENDIF }
    getmem(p,scsize);               { Bild sichern }
    moff;
    ReadScreenRect(1, screenwidth, 1, screenlines, p^);
    mon;
  end;
{$ENDIF}
end;

procedure holen(var sp:scrptr);
begin
{$IFDEF NCRT}
  wrest(sp);
{$ELSE}
  with sp do
  begin
    moff;
    {$IFNDEF NCRT }
      WriteScreenRect(1, screenwidth, 1, screenlines, p^);
    {$ENDIF }
    mon;
    disp_DT;
    freemem(p,scsize);               { Bild wiederherstellen }
  end;
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

{$I xp1s.inc}    { Shell }


procedure delete_tempfiles;
begin
  SaveDeleteFile(TempPath+swapfilename);
  SaveDeleteFile(TempPath+MsgTempFile);
  SaveDeleteFile(TempPath+'header.hdr');
end;


{ --- Bildschirmzeilen -------------------------------------}

{ Zeilenzahl einstellen; evtl. Videomodus zurÅcksetzen }

procedure setscreensize;
var
  ma  : map;
  n,i : integer;
begin
  // Modus nochmal setzen
  SysSetScreenSize(ScreenLines, ScreenWidth);
  screenlines := SysGetScreenLines;
  screenwidth := SysGetScreenCols;
  window(1,1,screenwidth,screenlines);
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

  procedure showtline;
  begin
    attrtxt(col.coltline);
{$IFDEF NCRT }
    HorizLine(3);
{$ELSE }
    wrt(1,3,dup(screenwidth,'ﬂ'));
{$ENDIF }
  end;

begin
  if dispusername and not startup then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,UpperCase(DefaultBox));
    showtline;
    if dbFound then begin
      user:= dbReadStr(d,'username');
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
    mwrt(1,screenlines-fnkeylines,dup(screenwidth,'‹'));
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
      1 : cm_wl('Vielen Dank. Sie haben ein einfaches Pointprogramm sehr glÅcklich gemacht.');
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
  blindon(true);
  getpos(width,height,x,y);
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
  Result.SetArrows(x,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
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


procedure WaitIt(txt:atext; p:proc; sec:word);
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

procedure rmessage(nr:word);
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
  masksetfninfo(x+width-7,y+height,' [F2] ','ƒ');
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
(* noch zu portieren !!
    sound(1000);
    delay(25);
    sound(780);
    delay(25);
    nosound;
    if soundflash then
    begin
      mdelay(60);
    end;
*)
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
{    mdelay(60);
    sound(1205);
    mdelay(60);
    sound(1000);
    mdelay(60);
    sound(800);
    mdelay(60);
    nosound; }
  end;
end;

procedure _fehler(const txt:string; hinweis:boolean);
var x,y   : Integer;
    lcol  : byte;
    s: String;
begin
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

procedure rfehler(nr:word);
var s : string;
begin
  if lastchar(forwardkeys)=#13 then forwardkeys:=copy(forwardkeys,1,length(forwardkeys)-1);
  s:=getres2(10000+100*(nr div 100),nr mod 100);
  freeres;
  pushhp(20000+nr);
  _fehler(s,false);
  pophp;
end;

procedure rfehler1(nr:word; txt:string);
begin
  txt:=getreps2(10000+100*(nr div 100),nr mod 100,txt);
  freeres;
  pushhp(20000+nr);
  _fehler(txt,false);
  pophp;
end;

function mfehler(b:boolean; txt:string):boolean;
begin
  if not b then _fehler(txt,false);
  mfehler:=not b;
end;


procedure hinweis(txt:string);
begin
  _fehler(txt,true);
end;

function fehlfunc(txt:string):boolean;
begin
  fehler(txt);
  fehlfunc:=true;
end;


procedure logerror(txt:string);
var f : text;
begin
  assign(f,Logpath+ErrlogFile);
  append(f);
  if ioresult<>0 then rewrite(f);
  writeln(f,LeftStr(date,6),RightStr(date,2),' ',time,' ',txt);
  close(f);
  if ioresult<>0 then;   { Logpath kînnte falsch gewesen sein }
end;

procedure tfehler(txt:string; sec:integer);
var x,y : Integer;
begin
  msgbox(length(txt)+16,5,_fehler_,x,y);
  mwrt(x+3,y+2,LeftStr(txt,screenwidth-16)+'  '#4'  '+formi(sec div 60,2)+':'+
               formi(sec mod 60,2));
  GotoXY(WhereX-5, WhereY);
  errsound;
  logerror(txt);
  wkey(sec,true);
  closebox;
end;

procedure trfehler(nr:word; sec:integer);
begin
  pushhp(20000+nr);
  tfehler(getres2(10000+100*(nr div 100),nr mod 100),sec);
  pophp;
  freeres;
end;

procedure trfehler1(nr:word; const txt:string; sec:integer);
begin
  freeres;
  pushhp(20000+nr);
  tfehler(getreps2(10000+100*(nr div 100),nr mod 100,txt),sec);
  pophp;
end;

procedure afehler(txt:string; auto:boolean);
begin
  if auto then
    tfehler(txt,20)
  else
    fehler(txt);
end;

procedure arfehler(nr:word; auto:boolean);
begin
  if auto then
    trfehler(nr,20)
  else
    rfehler(nr);
end;


function ioerror(i:integer; otxt:atext):atext;
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
begin
  longdat:=formi((l shr 24) mod 100,2)+formi((l shr 20) and 15,2)+
           formi((l shr 15) and 31,2)+formi((l shr 10) and 31,2)+
           formi((l shr 4) and 63,2);
end;

function ixdispdat(dat:shortstring):longint;      { Datum -> Long   }
begin
  ixdispdat:=ixdat(RightStr(dat,2)+copy(dat,4,2)+LeftStr(dat,2)+'0000');
end;


function smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }
begin
  smdl:=(d1 shr 1) and $7fffffff < (d2 shr 1) and $7fffffff;
end;


function fdat(dat:string):string;             { Z-Datum -> Datum  }
begin
  fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+LeftStr(dat,2);
end;

function zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
var j : word;
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


function ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
begin
  ftime:=copy(dat,7,2)+':'+copy(dat,9,2);
end;

{ Datum in Z-Format abfragen }

function Zdate:string;
begin
  result:= FormatDateTime('yymmddhhnn',Now);
end;


{ Tastaturpuffer lîschen, falls kein Makro aktiv }

procedure CondClearKeybuf;
begin
  if forwardkeys='' then ClearKeybuf;
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

{ p ist immer<>0! }
function scomp(const s1,s2 : string; p:byte):boolean;
var p0,n : Integer;
begin
  repeat dec(p) until (s1[p]<>' ') or (p=0);   { rtrim }
  p0:=1;
  while (s1[p0]=' ') and (p0<p) do inc(p0);    { ltrim }
  if p-p0+1<>length(s2) then
    scomp:=false
  else begin
    n:=1;
    while (p0<=p) and (s1[p0]=UpCase(s2[n])) do begin
      inc(n); inc(p0);
      end;
    scomp:=p0>p;
    end;
end;


function getb(const su:string; const v:string; var b:byte):boolean;
var   
  res, p: Integer;
begin
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
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
  p:=cpos('=',su);
  if scomp(su,v,p) then begin
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
  p:=cpos('=',su); 
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


{--- Allgemeine VFuncs fÅr Eingabemasken -------------------------}

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
var x,y   : Integer;
    p     : pointer;
    ps : word;
    box   : boolean;
    fpos,
    fsize : longint;
    rr: word;

  procedure show(n:longint);
  begin
    inc(fpos,n);
    if box then mwrt(x+3,y+2,dup(system.round(fpos*50 div fsize),'≤'));
  end;

begin
  ps:=65536;
  result:=true;
  getmem(p,ps);
  fsize:=filesize(f1)-filepos(f1);
  if fsize>0 then begin
    box:=(fsize>1024*1024) and (ExtractFileExt(FileName(f1))<>'.$$$');
    if box then begin
      MsgBox(56,5,getreps(134,extractfilename(FileName(f1))),x,y);
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,dup(50,'∞'));
      fpos:=0;
      end;
    while not eof(f1) and (inoutres=0) do begin
      blockread(f1,p^,ps,rr);
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
  freemem(p,ps);
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
  if (temppath='') or (temppath[1]=ownpath[1]) or (TempFree+4096>bytes) then
    TempS:=TempFile(TempPath)
  else
    TempS:=TempFile(OwnPath);
end;

function TempExtS(bytes:longint;startnamewith,ext:string):string;
begin
  if (temppath='') or (temppath[1]=ownpath[1]) or (TempFree+4096>bytes) then
    Result:=TempExtFile(TempPath,startnamewith,ext)
  else
    Result:=TempExtFile(OwnPath,startnamewith,ext);
end;

procedure _era(const Filename:string);
begin
  if not sysutils.DeleteFile(Filename) then
    trfehler1(4,'"'+Filename+'"',30);   { 'Kann "'+(fn)+'" nicht lîschen!?' }
end;

procedure SaveDeleteFile(const Filename: String);
begin
  if FileExists(Filename) then
    _era(Filename);
end;

procedure _chdir(p:string);
begin
  p:=trim(p);
  if p<>'' then 
  begin
    TrimLastChar(p, DirSepa);
    chdir(p);
    if ioresult<>0 then
      trfehler1(5,UpperCase(p),30);   { ungÅltiges Verzeichnis: }
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
begin
  { !!
  fillchar(dt,sizeof(dt),0);
  getdate(dt.year,dt.month,dt.day,dummy);
  gettime(dt.hour,dt.min,dt.sec,dummy);
  packtime(dt,pdt);
  if pdt shr 16 <> filetime(NewDateFile) shr 16 then
    fileio.setfiletime(NewDateFile,pdt); }
end;


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
  for i:=1 to res2anz(161) do   { Hinweise, was bei beschÑdigter Datenbank }
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

procedure cm_wln;
const lines : byte = 1;
{var   dummy : char; }
begin
  if cm then begin
    writeln;
    inc(lines);
    if lines=screenlines then begin
      if moremode then begin
        cm_w('<more>');
{        dummy:=}cm_key;
        cm_w(#13+'      '+#13);
        end;
      lines:=1;
      end;
    end
  else
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

function allocsenduudatamem: senduuptr;
const
  sdata: senduuptr = nil;
begin
  getmem(sdata, sizeof(senduudata));
  if sdata=nil then
    trfehler(6,30);  { 'zu wenig freier Speicher' }
  clearsenduudata(sdata);
  allocsenduudatamem:=sdata
end;

procedure clearsenduudata(sdata: senduuptr);
begin
  fillchar(sdata^, sizeof(senduudata), 0);
  with sdata^ do begin
    followup:=tstringlist.create;
    References := TStringlist.Create;
    OEM := TStringList.Create;
  end
end;

procedure freesenduudatamem(var sdata: senduuptr);
begin
  if not assigned(sdata) then
    exit;
  with sdata^ do begin
    followup.free;
    References.Free;
    OEM.Free;
  end;
  freemem(sdata);
  sdata:= nil
end;

var
  SavedExitProc: pointer;

procedure ExitXP1Unit;
begin
  ExitProc:= SavedExitProc;
  if ioresult= 0 then ;
  dbReleaseCache;
  if not closed then closedatabases;
  SysSetBackIntensity;
  FreeMem(MainMenu);
end;

procedure InitXP1Unit;
begin
  SavedExitProc:= ExitProc;
  ExitProc:=@ExitXP1Unit;
end;


{
  $Log$
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
  - added SaveDeleteFile
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



{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - allg. Routinen }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$F+}
  {$IFDEF DPMI}
    {$C permanent}
  {$ENDIF}
{$ENDIF }


unit xp1;

interface

uses
  xpglobal, crt,dos,dosx,typeform,montage,keys,fileio,inout,winxp,win2,video,
  datadef,database,mouse,maus2,help,maske,lister,printerx,clip,
  resource,xp0,xpcrc32;

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
     scrptr    = record
                   scsize  : word;
                   p       : pointer;
                 end;
     ahidden   = array[1..maxhidden] of integer;

Type TStartData = record
                    Length:        Word; { Must be 0x18,0x1E,0x20,0x32, or 0x3C }
                    Related:       Word; { 00 independent, 01 child }
                    FgBg:          Word; { 00 foreground, 01 background }
                    TraceOpt:      Word; { 00-02, 00 = no trace }
                    PgmTitle:      PChar; { max 62 chars or 0000:0000 }
                    PgmName:       PChar; { max 128 chars or 0000:0000 }
                    PgmInputs:     PChar; { max 144 chars or 0000:0000 }
                    TermQ:         PChar; { reserved, must be 00000000 }
                    Environment:   PChar; { max 486 bytes or 0000:0000 }
                    InheritOpt:    Word;  { 00 or 01 }
                    SessionType:   Word;  { 00 OS/2 session manager determines type (default)
                                            01 OS/2 full-screen
                                            02 OS/2 window
                                            03 PM
                                            04 VDM full-screen
                                            07 VDM window }
                    IconFile:      PChar; { max 128 chars or 0000:0000 }
                    PgmHandle:     LongInt; { reserved, must be 00000000 }
                    PgmControl:    Word;
                    InitXPos:      Word;
                    InitYPos:      Word;
                    InitXSize:     Word;
                    InitYSize:     Word;
                    Reserved:      Word; { 0x00 }
                    ObjectBuffer:  PChar; { reserved, must be 00000000 }
                    ObjectBuffLen: LongInt; { reserved, must be 00000000 }
  End;


var printlines : longint;
    WaitKey    : taste;               { Taste, mit der wkey beendet wurde }
    llh        : boolean;             { "L"/"H" im Lister -> xp1o.listExt }
                                      { == Nachrichten-Lister             }
    rbx,rby    : byte;                { Cursorposition fÅr ReadButton     }
    hidden     : ^ahidden;            { Liste der unsichtbaren MenÅpkte.  }
    anzhidden  : integer;             { Anzahl der unsichtbaren MenÅpkte. }


procedure showstack;                  { Stack/Heap-Anzeige im Debug-Mode }
procedure sound(hz:word);
procedure XpIdle;
function  plevelstr:string;           { Patchlevel }

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

procedure setenable(mnu,nr:byte; flag:boolean);
procedure setmenup(mnu:string; nr:byte; anew:string);
procedure setmenupos(mnu:string; newpos:byte);
procedure splitmenu(nr:byte; ma:map; var n:integer; nummern:boolean);

procedure SetExtraktMenu;
function  getmenu(nr:byte; enterkey:taste; x,y:byte):integer;
procedure setscreensize(newmode:boolean);
procedure lines(screen,fnkey:byte);   { setzt gl usw. }
procedure newscreenlines(m:integer);
procedure xp_maus_aus;
procedure xp_maus_an(x,y: integer16);
procedure SetMausEmu;
{$IFDEF BP }
procedure SetXPborder;
{$ENDIF }

procedure blindon(total:boolean);
procedure blindoff;
procedure getpos(width,height:byte; var x,y:byte);
procedure openbox(width,height:byte; var txt:string; var x,y:byte; c1,c2:byte);
procedure msgbox(width,height:byte; txt:string; var x,y:byte);
procedure diabox(width,height:byte; txt:string; var x,y:byte);
procedure selbox(width,height:byte; txt:string; var x,y:byte; hell:boolean);
procedure listbox(width,height:byte; txt:string);
procedure listboxcol;
procedure utilbox(l,r,o,u:byte; txt:string);
procedure dialog(width,height:byte; txt:string; var x,y:byte);
procedure enddialog;
procedure closebox;
procedure moment;
procedure message(txt:string);
procedure rmessage(nr:word);
procedure WaitIt(txt:atext; p:proc; sec:word);
procedure WriteClipFile(fn:pathstr);
procedure selcol;
procedure file_box(var name:pathstr; changedir:boolean);
procedure XP_testbrk(var brk:boolean);

procedure errsound;
function  _errsound:boolean;
procedure signal;              { s. Config/Anzeige/Hilfen }
procedure fehler(txt:string);
procedure rfehler(nr:word);
procedure rfehler1(nr:word; txt:string);
procedure hinweis(txt:string);
function  mfehler(b:boolean; txt:string):boolean;
function  fehlfunc(txt:string):boolean;
procedure logerror(txt:string);
procedure tfehler(txt:string; sec:integer);
procedure trfehler(nr:word; sec:integer);
procedure trfehler1(nr:word; txt:string; sec:integer);
procedure afehler(txt:string; auto:boolean);
procedure arfehler(nr:word; auto:boolean);
procedure interr(txt:string);
function  ioerror(i:integer; otxt:atext):atext;

procedure shell(prog:string; space:word; cls:shortint);  { externer Aufruf }

{$IFDEF BP }
Procedure Start_OS2(Programm,Parameter,Title:String);
{$ENDIF }

function  listfile(name,header:string; savescr,listmsg:boolean;
                   cols:shortint):shortint; { Lister }
procedure RemoveEOF(fn:pathstr);
procedure editfile(name:pathstr; nachricht,reedit:boolean; keeplines:byte;
                   ed_ukonv:boolean);
procedure dosshell;
procedure delete_tempfiles;
{$IFDEF BP }
procedure FlushSmartdrive(show:boolean);
{$ENDIF }
procedure set_checkdate;

procedure opendatabases;
procedure closedatabases;
procedure NewExit;                       { Exit-Prozedur          }
procedure TempClose;
procedure TempOpen;
procedure FlushClose;
procedure xp_DB_Error;    { Aufruf bei <DB> internem Fehler }

procedure fmove(var f1,f2:file);
procedure iso_conv(var buf; bufsize:word);

function  aFile(nr:byte):pathstr;

function  mbrett(typ:char; intnr:longint):string; { Xpoint.Db1/Bretter erz. }
function  mbrettd(typ:char; dbp:DB):string;       { Int_Nr auslesen }
function  ixdat(s:string):longint;              { Z-Date -> Long  }
function  longdat(l:longint):string;              { Long -> Z-Date  }
function  ixdispdat(dat:datetimest):longint;      { Datum -> Long   }
function  smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }

function  fdat(dat:string):string;             { Z-Datum -> Datum   }
function  zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
function  ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
function  Zdate:string;               { akt. Datum/Zeit im Z-Format }
function  fuser(s:string):string;              { Spaces vor/hinter '@' }
function  aufnahme_string:string;

function  MsgidIndex(mid:string):longint;      { case-insensitive CRC32 }

function getb(var su:string; v:string; var b:byte):boolean;   { PARSER }
function getc(var su:string; v:string; var c:char):boolean;
function geti(var su:string; v:string; var i:integer):boolean;
{ wie geti, allerdings mit 16 Bit Integer }
function geti16(var su:string; v:string; var i:integer16):boolean;
function getw(var su:string; v:string; var w:smallword):boolean;
function getl(var su:string; v:string; var l:longint):boolean;
function getx(var su:string; v:string; var b:boolean):boolean;
function gets(var s,su:string; v:string; var ss:string; maxlen:byte):boolean;
function getr(var su:string; v:string; var r:real):boolean;
procedure exchange(var s:string; repl,by:string);

function notempty(var s:string):boolean;

function IS_QPC(var betreff:string):boolean;
function IS_DES(var betreff:string):boolean;
function IS_PMC(var betreff:string):boolean;

procedure write_lastcall(dat:String);

procedure InitPrinter;
procedure PrintPage;
procedure PrintLine(s:string);
procedure ExitPrinter;

function  TempFree:longint;                 { Platz auf Temp-Laufwerk }
function  TempS(bytes:longint):pathstr;
procedure _era(fn:pathstr);
procedure ExErase(fn:pathstr);
procedure _chdir(p:pathstr);
function  testmem(size:longint; wait:boolean):boolean;

procedure cm_w(s:string);                     { Command-Mode-Ausgabe }
procedure cm_wl(s:string);                    { Writeln              }
procedure cm_wln;
procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
function  cm_key:char;


implementation  {-------------------------------------------------------}

uses
{$IFDEF BP }
  xpfonts,
{$ENDIF }
  xp1o,xp1o2,xp1help,xp1input,xp2,xpe,exxec,xpnt,strings;

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
     hmpos     : array[1..10] of byte;  { HauptmenÅ-XPos }
     main_n    : integer;               { MPs im HauptmenÅ }
     mainrange : array[1..10,0..1] of byte;
     listhicol : byte;
     startvideotype : byte;
     winstack  : array[1..maxwinst] of scrptr;   { fÅr Blindensupport }
     mst       : boolean;


{$IFDEF Ver32 }
function  ixdat(s:string):longint; assembler;  {&uses ebx, esi}
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
         xlat
@noconv: stosb
         loop   @isolp
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }


{$ELSE}

function  ixdat(s:string):longint; assembler;
asm
         les   si,s
         inc   si                       { LÑnge ist z.Zt. immer 10 }
         call  @getbyte                 { Jahr }
         cmp   al,70
         jae   @neunzehn
         add   al,100
@neunzehn:mov   dh,al
         call  @getbyte                 { Monat }
         mov   cl,4
         shl   al,cl
         mov   dl,al
         mov   cx,0
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
         mov   ax,cx
         jmp   @ende

@getbyte:mov   al,es:[si]
         inc   si
         sub   al,'0'
         mov   ah,10
         mul   ah
         add   al,es:[si]
         sub   al,'0'
         inc   si
         retn
@ende:
end;

procedure iso_conv(var buf; bufsize:word); assembler;
asm
         cld
         les   di,buf
         mov   cx,bufsize
         mov   bx,offset isotab1 - 0c0h
@isolp:  mov   al,es:[di]
         cmp   al,0c0h
         jb    @noconv
         xlat
@noconv: stosb
         loop  @isolp
end;

{$ENDIF}



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
            0  +  1                      ,{ { }
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

{$IFDEF BP}

var
  dispbuf: array[1..164] of byte;  {82 Zeichen und 82 Attribute}

procedure ListDisplay(x,y:word; var s:string); far; assembler;

asm
            les di,s
            cld
            xor cx,cx
            mov cl,es:[di]
            inc di
            push cx
            mov bx,offset dispbuf          { s + color -> dispbuf }
            mov ah,textattr
            mov al,' '                     { Abgrenzung links }
            mov [bx],ax
            add bx,2

@dcopylp:   mov al,es:[di]
            inc di
            mov [bx],ax
            add bx,2
            loop @dcopylp
            mov al,' '                     { Abgrenzung rechts }
            mov [bx],ax
            pop cx

            cmp ListXhighlight,0           { keine Hervorhebungen? }
            jz @nodh
            mov al,'*'
            call @testattr                 { sichert cx }
            mov al,'_'
            call @testattr
{            mov   al,'/'
             call  @testattr
}

@nodh:      mov ax,base                   { dispbuffer -> Bildschirm }
            mov es,ax
            mov ax,y
            dec ax
            mov si,zpz
            add si,si                     { si <- 160 }
            mul si
            mov di,x
            dec di
            add di,di
            add di,ax                     { es:di <- Bildschirmadresse }
            mov si,offset dispbuf[2]
            rep movsw

            jmp @ende


{-----------------------}

@testattr:  mov dx,cx
            xor bx,bx

            {-----------}
@ta1:       push ax
            mov cx,dx
            xor si,si

@talp1:     cmp al,byte ptr dispbuf[si]            { Startzeichen checken }
            jne @tanext1

             mov bl,byte ptr dispbuf[si-2]
             test byte ptr delimiters[bx],1        { Byte vor Startzeichen ok? }
             jz @tanext1
             mov bl,byte ptr dispbuf[si+2]
             test byte ptr delimiters[bx],2        { Byte vor Startzeichen ok? }
             jnz @tastart                          { Startzeichen gefunden }

@tanext1:   add si,2
            loop @talp1
            jmp @taende

            {-----------}

@tastart:   mov di,si                              { Di = Byte nach Startzeichen }
            dec cx
            jz @taende
            dec cx                                 { min. ein Zeichen Abstand }
            jz @taende
            add si,4                               { dann Endzeichen Checken }

@talp2:     cmp al,byte ptr dispbuf[si]
            jne @tanext2

             mov bl,byte ptr dispbuf[si-2]
             test byte ptr delimiters[bx],4        { Byte vor Endzeichen ok? }
             jz @tanext2
             mov bl,byte ptr dispbuf[si+2]
             test byte ptr delimiters[bx],8       { Byte nach Endzeichen ok? }
             jnz @tafound2                        { Endzeichen gefunden }

@tanext2:   add si,2
            loop @talp2
            jmp @taende

            {------------}

@tafound2:  push cx
            mov cx,si
            sub cx,di
            shr cx,1
            dec cx                                 { cx <- Anzahl hervorgeh. Zeichen }
            mov ah,listhicol

@tacopy1:   mov al,byte ptr dispbuf[di+2]          { hervorgehobenen Text eins nach }
            mov word ptr dispbuf[di],ax            { vorne kopieren; Farbe tauschen }
            add di,2
            loop @tacopy1

            pop cx
            dec cx                                 { restliche Zeichen }
            jz @addspace

@tacopy2:   mov ax,word ptr dispbuf[di+4]
            mov word ptr dispbuf[di],ax
            add di,2
            loop @tacopy2

@addspace:  mov byte ptr dispbuf[di],' '           { 2 Leerzeichen anhÑngen }
            mov byte ptr dispbuf[di+2],' '
            pop ax
            jmp @ta1                               { ... und das Ganze nochmal }


@taende:    pop ax
            mov cx,dx
            retn

{-------------------------}
@ende:
end; { of Listdisplay }



{$ELSE}

{ --- 32-Bit --- }

{ Variable in XP0.PAS: }
{ charbuf     : string[82];                  {82 Zeichen}
{ attrbuf     : array [1..82] of smallword;  {82 Attribute}

{ Attribute werden als Word erzeugt, fuer nicht Windows-Versionen }
{ mussen die Zugriffe auf Attrbuf evtl angepasst werden zu "attrbuf[ebx],dl" }

procedure MakeListDisplay(const s:string); assembler; {&uses ebx, esi, edi}

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
{            mov   al,'/'
             call  @testattr
}
@nodh:      mov byte ptr charbuf[0],cl
            add ecx,ecx
            mov word ptr attrbuf[0],cx
            jmp @ende


{-----------------------}

@testattr:  mov edx,ecx
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

@tacopy2:   mov al,byte ptr charbuf[edi+2]
            mov byte ptr charbuf[edi],al
            inc edi
            loop @tacopy2

@addspace:  mov word ptr charbuf[edi],'  '        { 2 Leerzeichen anhÑngen }
            pop eax
            jmp @ta1                               { ... und das Ganze nochmal }

@taende:    pop eax
            mov ecx,edx
            ret

{-------------------------}
@ende:
end; { of MakeListdisplay }


procedure ListDisplay(x,y:word; var s:string);

begin
  makelistdisplay(s);
  Consolewrite(x,y,length(s));
end;


{$ENDIF}




procedure interr(txt:string);
begin
  moff;
  cm_wl(txt);
  runerror:=false;
  halt(1);
end;

procedure sound(hz:word);
begin
{$IFNDEF VP }
  if not ParQuiet then
    crt.sound(hz);
{$ENDIF }
end;


procedure blindon(total:boolean);
var mf : boolean;
    mt : byte;
begin
  if blind and (winstp<maxwinst) and (memavail>160*50*2) then begin
    inc(winstp);
    if winstp=1 then begin
      mst:=m2t; m2t:=false;
      end;
    sichern(winstack[winstp]);
    mf:=forcecolor; forcecolor:=false; mt:=lastattr;
    attrtxt(7);
    moff;
    clwin(1,80,iif(total,1,2),screenlines);
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

procedure xp_maus_an(x,y: integer16);
begin
{$Q-}
  if _maus then begin
    if startup or MausShInit then
      mausinit;
    if (x+y>=0) then
      setmaus(x,y);
    setmauswindow(0,639,0,screenlines*8-1);
    mausan;
    maus_tasten_an;
    maus_cursor:=true;
    end;
{$IFDEF Debug }
  {$Q+}
{$ENDIF }
end;

procedure SetMausEmu;
begin
  iomaus:=ParMaus and not _maus;
end;


procedure sichern(var sp:scrptr);
begin
  with sp do
  begin
{$IFDEF Win32 }
    { Das Attribut belegt hier 2 Byte }
    scsize:=screenlines*screenwidth*4;
{$ELSE }
    scsize:=screenlines*2*screenwidth;
{$ENDIF }
    if maxavail<scsize+500 then interr('Speicher-öberlauf');
    getmem(p,scsize);               { Bild sichern }
    moff;
{$IFDEF BP }
    FastMove(mem[base:0],p^,scsize);
{$ELSE }
    ReadScreenRect(1, screenwidth, 1, screenlines, p^);
{$ENDIF }
    mon;
  end;
end;

procedure holen(var sp:scrptr);
begin
  with sp do
  begin
    moff;
{$IFDEF BP }
    FastMove(p^,mem[base:0],scsize);
{$ELSE }
  WriteScreenRect(1, screenwidth, 1, screenlines, p^);
{$ENDIF }
    mon;
    disp_DT;
    freemem(p,scsize);               { Bild wiederherstellen }
  end;
end;


procedure InitPrinter;
begin
  checklst:=true;
  printlines:=0;
  write(lst,PrintString(DruckInit));
end;

procedure PrintPage;
begin
  write(lst,PrintString(DruckFF));
  printlines:=0;
end;

procedure PrintLine(s:string);
begin
  writeln(lst,sp(DruckLira),s);
  inc(printlines);
  if (DruckFormlen>0) and (printlines>=DruckFormlen) then
    PrintPage;
end;

procedure ExitPrinter;
begin
  write(lst,PrintString(DruckExit));
end;


{$I xp1s.inc}    { Shell }


procedure delete_tempfiles;
begin
  if exist(TempPath+swapfilename) then
    _era(TempPath+swapfilename);
  if exist(TempPath+MsgTempFile) then
    _era(TempPath+MsgTempFile);
end;


{ --- Bildschirmzeilen -------------------------------------}

{$IFDEF BP }
procedure XPFont;
begin
  if not ParLCD then
    if ParFontfile[1]='*' then
      InternalFont
    else
      LoadFontfile(ParFontfile);
end;
{$ENDIF }

{$IFDEF BP }
procedure SetXPborder;
begin
  case videotype of
    1   : SetBorder16(col.colborder and $f);
    2,3 : SetBorder64(col.colborder and $3f);
  end;
end;
{$ENDIF }


{ Zeilenzahl einstellen; evtl. Videomodus zurÅcksetzen }

procedure setscreensize(newmode:boolean);
var ma  : map;
    n,i : integer;
begin
  if (videotype<2) or ParLCD then
    screenlines:=25
  else
    if ParFontfile<>'' then
    begin
  {$IFDEF BP }
      XPFont;
  {$ENDIF }
      screenlines:=GetScreenlines;
    end;
{$IFDEF Win32 }
  ScreenLines := GetScreenLines;
{$ENDIF }
  if (ParFontfile='') and not ParLCD then begin
    if newmode and (videotype>0) and ((screenlines>25) or (getvideomode<>3))
    then begin
      setvideomode(3);
      IoVideoInit;
      end;
     if (screenlines<>25) or (screenlines<>getscreenlines) then
      setscreenlines(screenlines);
    end;
  iosclines:=screenlines;
  crline:=screenlines;
  actscreenlines:=screenlines;
  screenwidth:=zpz;
  cursor(curoff);
  window(1,1,80,25);
  new(ma);
  splitmenu(ZeilenMenue,ma,n,true);
  for i:=1 to n do
    if screenlines=ival(ma^[i].mstr) then menupos[ZeilenMenue]:=i;
  dispose(ma);
  set_helppos;
end;


procedure showusername;
var d    : DB;
    user : string[40];

  procedure showtline;
  begin
    attrtxt(col.coltline);
    wrt(1,3,dup(screenwidth,'ﬂ'));
  end;

begin
  if dispusername and not startup then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,ustr(DefaultBox));
    showtline;
    if dbFound then begin
      dbRead(d,'username',user);
      mwrt(78-length(user),3,' '+user+' ');
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
  setscreensize(newmode);
  lines(screenlines,1);
  clrscr;
{$IFDEF BP }
  if (videotype>1) and not ParMono then
    setbackintensity(true);
  SetXPborder;
{$ENDIF }
  with col do begin
    attrtxt(colmenu[0]);
    write(sp(screenwidth));
    showusername;
    dispfunctionkeys(false);
    attrtxt(coltline);
    mwrt(1,screenlines-fnkeylines,dup(screenwidth,'‹'));
    normtxt;
    end;
  showmain(0);
  dphback:=col.colmenu[0]; setseconds(false,true);
  timex:=74; timey:=1; m2t:=true;
  disp_DT;
  attrtxt(7);
  gotoxy(1,4);
  xp_maus_an(mausdefx,mausdefy);
  if newmode then startvideotype:=videotype;
end;


{ --- Videomode nach Shell- bzw. externem Aufruf neusetzen ----- }

procedure resetvideo;
var m3 : boolean;
begin
  if startvideotype>0 then begin
    m3:=true;
    if getvideomode<>iif(color,3,7) then setvideomode(iif(color,3,7))
    else m3:=false;
    if (videotype>1) and not ParLCD then
{$IFDEF BP }
      if ParFontfile<>'' then
        XPFont
      else
{$ENDIF }
      if getscreenlines<>screenlines then begin
        if not m3 then setvideomode(3);
        setscreenlines(screenlines);
        setmauswindow(0,639,0,screenlines*8-1);
        end;
    end;
{$IFDEF BP }
  if (videotype>1) and not ParMono then setbackintensity(true);
  SetXPborder;
{$ENDIF }
end;


procedure exitscreen(joke:shortint);
var i : integer;
begin
  moff;
  attrtxt(7);
{$IFDEF BP }
  if col.colborder<>0 then
    setborder16(0);
{$ENDIF }
  clrscr;
  SetVideoMode(OrgVideomode);
{ screenlines:=25;
  setscreensize(false); }
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

procedure lines(screen,fnkey:byte);
begin
  screenlines:=screen; iosclines:=screen;
  fnkeylines:=fnkey;
  gl:=screenlines-4-fnkeylines;
end;


{ screenlines gemÑ· 25/26/...-MenÅ-Position neu setzen }

procedure newscreenlines(m:integer);
var ma : map;
    n  : integer;
begin
  new(ma);
  splitmenu(ZeilenMenue,ma,n,true);
  screenlines:=ival(ma^[m].mstr);
  dispose(ma);
  lines(screenlines,fnkeylines);
end;


{ --- Dialog- und sonstige Boxen ------------------------------- }

procedure getpos(width,height:byte; var x,y:byte);
begin
  x:=(screenwidth-width)div 2 +1;
  y:=(actscreenlines-height+1) div 2 +1;
end;


procedure openbox(width,height:byte; var txt:string; var x,y:byte; c1,c2:byte);
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


procedure msgbox(width,height:byte; txt:string; var x,y:byte);
begin
  openbox(min(width,screenwidth),height,txt,x,y,col.colmboxrahmen,col.colmbox);
end;


procedure diabox(width,height:byte; txt:string; var x,y:byte);
begin
  openbox(width,height,txt,x,y,col.coldiarahmen,col.coldialog);
end;


procedure selbox(width,height:byte; txt:string; var x,y:byte; hell:boolean);
begin
  openbox(width,height,txt,x,y,
          iif(hell,col.colselrahmen,col.colsel2rahmen),
          iif(hell,col.colselbox,col.colsel2box));
end;

procedure ListboxCol;
var lc : listcol;
begin
  with lc do begin
    coltext:=col.colselbox;
    colselbar:=col.colselbar;
    colmarkline:=col.colselhigh;
    colmarkbar:=col.colselbar and $f0 + col.colselhigh and $f;
    { colscroll:=col.colselscroll; }
    setlistcol(lc);
    end;
end;

procedure listbox(width,height:byte; txt:string);
var x,y : byte;
begin
  selbox(width+2,height+2,txt,x,y,true);
  openlist(x+1,x+width,y+1,y+height,0,'/NS/SB/NLR/DM/');
  ListboxCol;
  listarrows(x,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
end;


procedure utilbox(l,r,o,u:byte; txt:string);
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


procedure message(txt:string);
var x,y : byte;
begin
  msgbox(length(txt)+6,3,'',x,y);
  mwrt(x+3,y+1,left(txt,screenwidth-6));
end;

procedure rmessage(nr:word);
begin
  message(getres(nr));
end;

procedure moment;
begin
  rmessage(105);   { 'Einen Moment bitte ...' }
end;


procedure dialog(width,height:byte; txt:string; var x,y:byte);
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


procedure WriteClipFile(fn:pathstr);
begin
  if exist(fn) then begin
    FileToClip(fn);
    _era(fn);
    end;
end;


procedure errsound;
begin
  if not ParQuiet or soundflash then
  begin
{$IFDEF BP }
    if soundflash then SetBorder16(3);
    sound(1000);
    delay(25);
    sound(780);
    delay(25);
    nosound;
    if soundflash then
    begin
      mdelay(60);
      {$IFDEF BP }
      SetXPborder;
      {$ENDIF }
    end;
{$ENDIF }
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
{$IFDEF VP }
    PlaySound(1205, 60);
    PlaySound(1000, 60);
    PlaySound(800, 60);
{$ELSE }
    mdelay(60);
    sound(1205);
    mdelay(60);
    sound(1000);
    mdelay(60);
    sound(800);
    mdelay(60);
    nosound;
{$ENDIF }
  end;
end;

procedure _fehler(var txt:string; hinweis:boolean);
var x,y   : byte;
    w1,w2 : word;
    lcol  : byte;
begin
  truncstr(txt,screenwidth-4);
  savecursor; lcol:=textattr;
  w1:=windmin; w2:=windmax;
  window(1,1,80,25);
  msgbox(length(txt)+6,5,iifs(hinweis,_hinweis_,_fehler_),x,y);
  mwrt(x+3,y+2,left(txt,screenwidth-6));
  errsound;
  wait(curoff);
  closebox;
  windmin:=w1; windmax:=w2;
  restcursor;
  attrtxt(lcol);
end;

procedure fehler(txt:string);
begin
  _fehler(txt,false);
end;

procedure rfehler(nr:word);
var s : string[80];
begin
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
  writeln(f,left(date,6),right(date,2),' ',time,' ',txt);
  close(f);
  if ioresult<>0 then;   { Logpath kînnte falsch gewesen sein }
end;

procedure tfehler(txt:string; sec:integer);
var x,y : byte;
begin
  msgbox(length(txt)+16,5,_fehler_,x,y);
  mwrt(x+3,y+2,left(txt,screenwidth-16)+'  '#4'  '+formi(sec div 60,2)+':'+
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

procedure trfehler1(nr:word; txt:string; sec:integer);
begin
  txt:=getreps2(10000+100*(nr div 100),nr mod 100,txt);
  freeres;
  pushhp(20000+nr);
  tfehler(txt,sec);
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
    if left(s,5)='fehlt' then ioerror:=otxt
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

procedure file_box(var name:pathstr; changedir:boolean);
begin
  if (pos('*',name)>0) or (pos('?',name)>0) then begin
    selcol;
    pushhp(89);
    name:=fsbox(actscreenlines div 2 - 5,name,'','',changedir,false,false);
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

function ixdispdat(dat:datetimest):longint;      { Datum -> Long   }
begin
  ixdispdat:=ixdat(right(dat,2)+copy(dat,4,2)+left(dat,2)+'0000');
end;


function smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }
begin
  smdl:=(d1 shr 1) and $7fffffff < (d2 shr 1) and $7fffffff;
end;


function fdat(dat:string):string;             { Z-Datum -> Datum  }
begin
  fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+left(dat,2);
end;

function zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
var j : word;
    d : datetimest;
    n : integer;
begin
  j:=ival(left(dat,2))+1900;
  if j<1970 then inc(j,100);
  schalt(j);
  d:=fdat(dat);
  n:=_daylen_;
  zdow:=trim(copy(_days_^,dow(copy(d,1,6)+strs(j))*n+1-n,n));
  { 'Montag    Dienstag  Mittwoch  DonnerstagFreitag   Samstag   Sonntag' }
end;


function ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
begin
  ftime:=copy(dat,7,2)+':'+copy(dat,9,2);
end;

{ Datum in Z-Format abfragen }

function Zdate:string;
var t,m,j,dow,h,mm,s,s100 : rtlword;
begin
  getdate(j,m,t,dow);
  gettime(h,mm,s,s100);
  while h>23 do dec(h,24);
  Zdate:=formi(j mod 100,2)+formi(m,2)+formi(t,2)+formi(h,2)+formi(mm,2);
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
function scomp(var s1,s2 : string; p:byte):boolean;
var p0,n : byte;
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


function getb(var su:string; v:string; var b:byte):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),b,res);
    getb:=(res=0);
    end
  else getb:=false;
end;

function getc(var su:string; v:string; var c:char):boolean;
var p : byte;
begin
  { MK 09.02.00 Getc war nicht initialisiert }
  p:=pos('=',su);
  if scomp(su,v,p) and (p + 1 <= Length(su)) then
  begin
    c:=su[p+1];
    Getc := true;
  end else
    Getc := false;
end;

function geti(var su:string; v:string; var i:integer):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),i,res);
    geti:=(res=0);
    end
  else geti:=false;
end;

function geti16(var su:string; v:string; var i:integer16):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),i,res);
    geti16:=(res=0);
    end
  else geti16:=false;
end;

function getw(var su:string; v:string; var w:smallword):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),w,res);
    getw:=(res=0);
    end
  else getw:=false;
end;

function getl(var su:string; v:string; var l:longint):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),l,res);
    getl:=(res=0);
    end
  else getl:=false;
end;

function getr(var su:string; v:string; var r:real):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),r,res);
    getr:=(res=0);
    end
  else getr:=false;
end;

function getx(var su:string; v:string; var b:boolean):boolean;
var ss : string[1];
    p  : byte;
begin
  p:=pos('=',su);
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

function gets(var s,su:string; v:string; var ss:string; maxlen:byte):boolean;
var
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then
  begin
    ss:=copy(s,p+1,maxlen);
    gets:=true;
  end else
    gets:=false;
end;


function fuser(s:string):string;              { Spacec vor/hinter '@' }
var p : byte;
begin
  p:=pos('@',s);
  if p=0 then fuser:=s
  else fuser:=left(s,p-1)+' @ '+copy(s,p+1,80);
end;

function aufnahme_string:string;
begin
  aufnahme_string:=getres2(108,minmax(useraufnahme,0,3));
end;


function IS_QPC(var betreff:string):boolean;
begin
  IS_QPC:=(left(betreff,length(QPC_ID))=QPC_ID);     { QPC: }
end;

function IS_DES(var betreff:string):boolean;
begin
  IS_DES:=(left(betreff,length(DES_ID))=DES_ID);     { DES: }
end;

function IS_PMC(var betreff:string):boolean;
begin
  IS_PMC:=(left(betreff,length(PMC_ID))=PMC_ID);     { *crypted* }
end;


{ Datum des letzten Netcalls merken }

procedure write_lastcall(dat:String);
var t : text;
begin
  assign(t,ownpath+NewDateFile);
  rewrite(t);
  writeln(t,dat);
  close(t);
  if readmode=rmNeues then readdate:=ixdat(dat);
end;


function aFile(nr:byte):pathstr;
begin
  aFile:=AblagenFile+strs(nr);
end;


{--- Allgemeine VFuncs fÅr Eingabemasken -------------------------}

function notempty(var s:string):boolean;
begin
  if trim(s)='' then errsound;
  notempty:=(trim(s)<>'');
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
{$IFDEF BP }
  FlushSmartdrive(false);
{$ENDIF }
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
{$IFDEF BP }
    FlushSmartdrive(false);
{$ENDIF }
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


{$S-}
procedure newexit;               { Exit-Prozedur }
begin
  if ioresult= 0 then ;
  dbReleaseCache;
  if not closed then closedatabases;
  if lockopen then begin
    unlockfile(lockfile);
    close(lockfile);
    erase(lockfile);
    if ioresult<>0 then ;
  end;
{$IFDEF BP }
  if videotype>1 then setbackintensity(false);
  setcbreak(orgcbreak);
  exitproc:=oldexit;
{$ENDIF}
end;
{$S+}

procedure showstack;
{$IFDEF BP }
const lastsptr : word = 0;
      lastavail: longint = 0;
var b : byte;
{$ENDIF }
begin
{$IFDEF BP }
  if (sptr<>lastsptr) or (memavail<>lastavail) then begin
    b:=dphback; dphback:=col.colkeys;
    {$IFDEF DPMI}
      disphard(70,screenlines,hex(sptr,4)+'/'+hex(memavail,6));
    {$ELSE}
      disphard(71,screenlines,hex(sptr,4)+'/'+hex(memavail,5));
    {$ENDIF}
    dphback:=b;
    lastsptr:=sptr;
    lastavail:=memavail;
    end;
{$ENDIF }
end;


{ alle restlichen Bytes ab fpos(f1) nach f2 kopieren }

procedure fmove(var f1,f2:file);
var x,y   : byte;
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
  ps:=min(maxavail-5000,60000);
  getmem(p,ps);
  fsize:=filesize(f1)-filepos(f1);
  if fsize>0 then begin
    box:=(fsize>1024*1024) and (windmin=0) and (GetFileExt(FileName(f1))<>'$$$');
    if box then begin
      MsgBox(56,5,getreps(134,getfilename(FileName(f1))),x,y);
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
    if inoutres<>0 then
      fehler(ioerror(ioresult,getres(102)));  { Fehler beim Dateizugriff :-( }
    end;
  freemem(p,ps);
end;


function TempFree:longint;                 { Platz auf Temp-Laufwerk }
begin
  if temppath='' then
    TempFree:=diskfree(0)
  else
    TempFree:=diskfree(ord(temppath[1])-64);
end;


function TempS(bytes:longint):pathstr;
begin
  if (temppath='') or (temppath[1]=ownpath[1]) or (TempFree+4096>bytes) then
    TempS:=TempFile(TempPath)
  else
    TempS:=TempFile(OwnPath);
end;


procedure _era(fn:pathstr);
var f : file;
begin
  assign(f,fn);
  erase(f);
  if ioresult<>0 then
    trfehler1(4,ustr(fn),30);   { 'Kann '+ustr(fn)+' nicht lîschen!?' }
end;

procedure ExErase(fn:pathstr);
begin
  if exist(fn) then _era(fn);
end;

procedure _chdir(p:pathstr);
begin
  p:=trim(p);
  if p<>'' then begin
    if (length(p)>1) and (right(p,1)='\') then
      dellast(p);
    chdir(p);
    if ioresult<>0 then
      trfehler1(5,ustr(p),30);   { ungÅltiges Verzeichnis: }
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


procedure exchange(var s:string; repl,by:string);
var p : byte;
begin
  p:=pos(ustr(repl),ustr(s));
  if p>0 then s:=copy(s,1,p-1)+by+copy(s,p+length(repl),255);
end;


procedure XpIdle;
begin
  mdelay(1);
end;

{$IFDEF BP }
procedure FlushSmartdrive(show:boolean);   { Schreibcache leeren }
begin
  if not ParNoSmart and (SmartCache(ord(getdrive)-65)=2) then begin
    if show then rmessage(131);   { 'Leere Smartdrive-Schreibcache...' }
    SmartResetCache;
    if show then closebox;
    end;
end;
{$ENDIF }


procedure set_checkdate;
var dt    : datetime;
    dummy : rtlword;
    pdt   : longint;
begin
  fillchar(dt,sizeof(dt),0);
  getdate(dt.year,dt.month,dt.day,dummy);
  gettime(dt.hour,dt.min,dt.sec,dummy);
  packtime(dt,pdt);
  if pdt shr 16 <> filetime(NewDateFile) shr 16 then
    setfiletime(NewDateFile,pdt);
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


function plevelstr:string;           { Patchlevel }
begin
  if lastchar(patchlevel)='0' then
    plevelstr:=''
  else
    plevelstr:=' pl'+lastchar(patchlevel);
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
  MsgidIndex:=CRC32(mid);
end;


{$I xp1cm.inc}

end.
{
  $Log$
  Revision 1.30  2000/04/15 21:44:45  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.29  2000/04/13 13:54:45  mk
  - 32 Bit: Fehlerhafte Prozentanzeigen behoben
  - 32 Bit VP: Shift-Tab funktioniert jetzt

  Revision 1.28  2000/04/13 12:48:34  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.27  2000/04/11 16:38:42  jg
  - Config/Optionen/Editor
  - Hilfe der Editoroptionen jetzt kontextsensitiv

  Revision 1.26  2000/04/09 19:47:22  mk
  - Benutze Register fuer ListDisplay und VP angegeben

  Revision 1.25  2000/04/09 06:51:56  jg
  - XP/32 Listdisplay (Hervorhebungsroutine fuer Lister) portiert.
  - XP/16 Listdisplay etwas umgebaut und optimiert (Tabelle in DS)

  Revision 1.24  2000/04/04 21:01:23  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.23  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.22  2000/03/25 11:46:09  jg
  - Lister: Uhr wird jetzt auch bei freiem Nachrichtenkopf eingeblendet
  - Config/Optionen/Lister: Schalter ListUhr zum (de)aktivieren der Uhr

  Revision 1.21  2000/03/25 09:03:56  mk
  - xdelay jetzt komplett entfernt

  Revision 1.20  2000/03/24 15:41:01  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.19  2000/03/22 10:19:21  mk
  - Bug in ListDisplay behoben

  Revision 1.18  2000/03/20 11:58:04  mk
  - Assembler-Routinen komplett in Inline-ASM umgeschrieben

  Revision 1.17  2000/03/16 19:25:10  mk
  - fileio.lock/unlock nach Win32 portiert
  - Bug in unlockfile behoben

  Revision 1.16  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.15  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.14  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.13  2000/03/08 22:13:31  rb
  nicht mehr benîtigte Routinen fÅr OS/2 Programmaufruf entfernt

  Revision 1.12  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.11  2000/03/02 21:19:51  jg
  - Uhr beim verlassen des Nachrichtenheaders eleganter deaktiviert

  Revision 1.10  2000/02/27 08:24:57  jg
  -Strings.StrPCopy wird wieder benutzt...

  Revision 1.9  2000/02/26 18:14:46  jg
  - StrPCopy in Xp1s.inc integriert
  - Suche aus Archivviewer wieder zugelassen
    (zwecks Headereintregsuche im "O" Fenster)

  Revision 1.8  2000/02/24 23:50:11  rb
  Aufruf externer Viewer bei OS/2 einigermassen sauber implementiert

  Revision 1.7  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

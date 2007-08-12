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
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
{$IFDEF Unix }
  unix,xpunix,
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
  resource,xp0,crc,xpglobal,classes,debug,osdepend;

const maxhidden  = 500;                 { max. versteckte MenÅpunkte }

      DisableDOS : boolean = false;
      shellkey   : boolean = false;
      ListMakros : byte    = 0;         { Flag fÅr XPKEYS.XMakro     }
      Errorlevel : integer = 0;
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
     scrptr    = Pointer; 
{$ENDIF }
     ahidden   = array[1..maxhidden] of SmallInt;

var printlines : longint;
    WaitKey    : taste;               { Taste, mit der wkey beendet wurde }
    llh        : boolean;             { "L"/"H" im Lister -> xp1o.listExt }
                                      { == Nachrichten-Lister             }
    rbx,rby    : Integer;             { Cursorposition fÅr ReadButton     }
    hidden     : ^ahidden;            { Liste der unsichtbaren MenÅpkte.  }
    anzhidden  : SmallInt;            { Anzahl der unsichtbaren MenÅpkte. }


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

procedure setenable(mnu,nr:byte; flag:boolean);
procedure setmenup(mnu:string; nr:byte; const anew:string);
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
procedure WriteClipFile(const fn:string);
procedure selcol;
procedure file_box(var name:string; changedir:boolean);
procedure XP_testbrk(var brk:boolean);

procedure errsound;
function  _errsound:boolean;
procedure signal;              { s. Config/Anzeige/Hilfen }
procedure fehler(const txt:string);
procedure rfehler(nr:word);
procedure rfehler1(nr:word; txt:string);
procedure hinweis(const txt:string);
function  mfehler(b:boolean; const txt:string):boolean;
function  fehlfunc(const txt:string):boolean;
procedure logerror(const txt:string);
procedure tfehler(const txt:string; sec:integer);
procedure trfehler(nr:word; sec:integer);
procedure trfehler1(nr:word; const txt:string; sec:integer);
procedure afehler(const txt:string; auto:boolean);
procedure arfehler(nr:word; auto:boolean);
procedure interr(const txt:string);
function  ioerror(i:integer; const otxt:atext):atext;

procedure shell(const prog:string; space:word; cls:shortint);  { externer Aufruf }

{ Execute an external program and add any files created in current dir to SL }
function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;

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

function  aFile(nr:byte):string;

function  mbrett(typ:char; intnr:longint):string; { Xpoint.Db1/Bretter erz. }
function  mbrettd(typ:char; dbp:DB):string;       { Int_Nr auslesen }
function  ixdat(const s:string):longint;           { Z-Date -> Long  }
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
procedure PrintLine(s:string);
procedure ExitPrinter;

function  TempFree:Int64;                 { Platz auf Temp-Laufwerk }
function  TempS(bytes:longint):string;
function  TempExtS(bytes:longint;const startnamewith,ext:string):string;
procedure _era(const Filename: String);
// Deletes a file only if exists, uses _era to report errors
procedure SafeDeleteFile(const Filename: String);
procedure SafeMakeBak(const Filename, NewExt: String);
procedure _chdir(const p:string);
function  testmem(size:longint; wait:boolean):boolean;

procedure cm_w(const s:string);                     { Command-Mode-Ausgabe }
procedure cm_wl(const s:string);                    { Writeln              }
procedure cm_wln;
procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
function  cm_key:char;

function  ComputeUserAddress(d: DB):string;

procedure InitXP1Unit;

implementation  {-------------------------------------------------------}

uses
  xp1o,xp1o2,xp1help,xp1input,xpe,xpnt,
{$IFDEF Unix }
  {$IFDEF Kylix}
    ncursix,
  {$ELSE}
    ncurses,
  {$ENDIF}
{$ENDIF }
  mime,
  direct,
  xpunicode,
  xpunicode_lbr,
  xpcharset;

const
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


// 0506032053 -> 1768018768
// jjmmtthhmm
function ixdat(const s: string): longint;       
  var jahr   : integer;
      monat  : integer;
      tag    : integer;
      stunde : integer;
      minute : integer;
begin
  if Length(s) < 10 then begin
    result := 0;
    exit;
  end;

  jahr   := ((Ord(s[1])-48) * 10 + (Ord(s[2])-48)) and $007F;
  if jahr < 70 then 
    jahr := jahr + 100;
  monat  := ((Ord(s[3])-48) * 10 + (Ord(s [4])-48)) and $FF;
  tag    := ((Ord(s[5])-48) * 10 + (Ord(s [6])-48)) and $FF;
  stunde := ((Ord(s[7])-48) * 10 + (Ord(s [8])-48)) and $1F;
  minute := ((Ord(s[9])-48) * 10 + (Ord(s[10])-48)) and $3F;

  result:=((jahr   shl 24) or
           (monat  shl 20) or
           (tag    shl 15) or
           (stunde shl 10) or
           (minute shl  4)   );          
end;


procedure ListDisplay(x,y, StartC, Columns: Integer; var s: string);
var Pos:     integer;   // current position in bytes
    NewPos:  integer;   // save for current pos
    PosC:    integer;   // position in columns
    OutPos:  integer;   // current position of data already written in bytes
    OutPosC: integer;   // position in columns

    C, LastC: TUnicodeChar;     // current and last character
    W: Integer;                 // width of current character (columns)
    B, LastB : TUnicodeLineBreakType; // lb class of current and last character

    HiChar : TUnicodeChar;      // character that started highlighting
    HiStart: Integer;           // position of starting char in bytes
    HiStartC: Integer;          // ...in columns
    HiDelimCount : Integer;     // count of occurrence of HiChar within hilighting sequence
    HiWordCCount : Integer;     // count of word characters within hilighting sequence

    URL: boolean;               // URL has been found
    URLStart, URLEnd: integer;  // URL start and end position in bytes
    URLStartC: integer;

    DefaultAttr: SmallWord;     // saved text attribute

    i, j, k : Integer;
    us: String;

  procedure _(Attr: SmallWord; ToPos, ToPosC: integer);
  begin
    if ToPos <= OutPos then exit;
    TextAttr := Attr;
    FWrt(x+OutPosC-StartC,y,Copy(s,OutPos,ToPos-OutPos));
    OutPosC := ToPosC;
    OutPOs := ToPos;
  end;

begin
{$IFDEF Debug }
{
  Debug.DebugLog('xp1','ListDisplay, x:'+IntToStr(x)
                 +',y:'+IntToStr(y)+',StartC:'+IntToStr(StartC)
                 +', Columns:'+IntToStr(Columns)
                 +', s:<'+s+'>',DLTrace);
}
{$ENDIF }
  if not ListXHighlight then begin
    FWrt(x,y,UTF8FormS(s,StartC,Columns));
    exit;
  end;

  DefaultAttr := TextAttr;
  
  Pos := 1; PosC := 1;
  OutPos := 1; OutPosC := 1;

  URLStartC := 0;
  HiChar := 0;

  // put a space at the start
  LastC := 32;
  LastB := UNICODE_BREAK_SP;

  // and at the end
  s := s+' ';

  URL := FindUrl(s, URLStart, URLEnd);

  while (Pos <= Length(s)) do
  begin
    NewPos := Pos;
    C := UTF8GetCharNext(s, NewPos);
    { HJT 19.02.2006 Q&D. Im nicht UTF-8 Modus (Enable-UTF8=F, XPOINT.CFG)    )
    { keine Sequenz laenger eins zulassen. Eigentlich muessten  W und B (s.u.)}
    { auch angepasst werden.                                                  }
    { Und: Der Lister ist scheinbar GAR NICHT auf nicht UTF-8 Dateien /       }
    { Nachrichten vorbereitet mit Zeichen > $7f                               }
    if not Enable_UTF8 then begin
       if NewPos - Pos > 1 then begin
          {$IFDEF Debug }
          us:='';
          for k:=pos to NewPos - 1 do begin
            us:=us+Hex(Ord(s[k]),2)+' ';
          end;
          Debug.DebugLog('xp1','ListDisplay, Setting wg NOT Enable_UTF8'
                         +' NewPos from: '+IntToStr(NewPos)
                         +' To: '+IntToStr(Pos+1)
                         +', Seq: '+us
                         ,DLTrace);
          {$ENDIF }
          NewPos := Pos + 1;
       end;
    end;

    W := UnicodeCharacterWidth(C);
    B := UnicodeCharacterLineBreakType(C);

    if PosC <= StartC then begin OutPos := Pos; OutPosC := PosC; end;
{$IFDEF Debug }
{
    Debug.DebugLog('xp1','ListDisplay, NewPos:'+IntToStr(NewPos)
                         +',OutPosC:'+IntToStr(OutPosC)
                         +',UTF8GetCharNext:'+IntToStr(C)
                         +',UnicodeCharacterWidth:'+IntToStr(W)
                         +',UnicodeCharacterLineBreakType:'+IntToStr(integer(B)),DLTrace);
}
{$ENDIF }
    if URL and (Pos >= URLStart) and (URLStartC <= 0) then
    begin
      URLStartC := PosC;
      HiChar := 0; // URLs break the highlighting
    end else

    if URL and (Pos >= URLStart) and (Pos < UrlEnd) then
    begin
    end else

    if URL and (Pos >= URLEnd) and (URLStartC > 0) then
    begin
      _(DefaultAttr,URLStart, URLStartC);
      _(ListHiCol,  Pos,      PosC);
      URL := false;
    end else

    if (HiChar<>0) and (C=HiChar) then
    begin
      // Count the delimiters seen
      Inc(HiDelimCount);
    end else

    if (HiChar<>0) and (LastC=HiChar) and not (B in [ UNICODE_BREAK_AL,
      UNICODE_BREAK_ID,UNICODE_BREAK_GL, UNICODE_BREAK_UNKNOWN,
      UNICODE_BREAK_CM ]) and (HiWordCCount >= 1) then
    begin
      // replace continuations by ' '
      if(HiDelimCount > 1) and (HiChar <> Ord('_')) then
        for i := HiStart+1 to Pos-1 do
          if Ord(s[i]) = HiChar then
            s[i] := ' ';
      _(DefaultAttr, HiStart, HiStartC);
      Inc(OutPos); // ignore starting delimiter
      Dec(PosC,2);
      _(ListHiCol, Pos-1, PosC);
      Inc(OutPos); // ignore ending delimiter
      HiChar := 0;
    end else

    if (HiChar<>0) and (B in [ UNICODE_BREAK_AL, UNICODE_BREAK_ID,
      UNICODE_BREAK_NU ]) then
    begin
      // count "word" chars
      Inc(HiWordCCount);
    end else

    if (HiChar<>0) and not (B in [ UNICODE_BREAK_AL, UNICODE_BREAK_ID,
      UNICODE_BREAK_GL, UNICODE_BREAK_UNKNOWN, UNICODE_BREAK_CM,
      UNICODE_BREAK_B2, UNICODE_BREAK_BA, UNICODE_BREAK_NU,
      UNICODE_BREAK_BB, UNICODE_BREAK_HY, UNICODE_BREAK_SP ]) then
    begin
      // not a valid highlighting sequence, ignore
      HiChar := 0;
    end else

    // check for c < 256 to avoid range check error with fpc
    if (HiChar = 0) and (C <= 255) and (C in [Ord('*'),Ord('_') { HJT 12.08.07  kein Hervorheben bei Pfadangaben ,Ord('/') } ]) and not
      (LastB in [ UNICODE_BREAK_AL, UNICODE_BREAK_ID,UNICODE_BREAK_GL,
      UNICODE_BREAK_UNKNOWN, UNICODE_BREAK_CM, UNICODE_BREAK_NU ]) then
    begin
      HiStart := Pos;
      HiStartC := PosC;
      HiDelimCount := 0;
      HiWordCCount := 0;
      HiChar := C;
    end;

    LastC := C;
    LastB := B;

    Pos := NewPos;

    { HJT 11.11.2005  no width of -1                            }
    { xpunicode.pas, UnicodeCharacterWidth                      }
    { - Other C0/C1 control characters and DEL will lead to a	}
    {   return value of -1.					                    }	
    { if Pos <= Length(s) then Inc(PosC, W); }
    if Pos <= Length(s) then Inc(PosC, iif(W<=0,1,W));
  end;

  // the last char is the space inserted above and should not be output
  _(DefaultAttr,Length(s),PosC);

  TextAttr := DefaultAttr;
  // fill end of line
  if(OutPosC <= Columns) then
    FWrt(Max(1,x+OutPosC-StartC),y,sp(Min(Columns,Columns-OutPosC+StartC)));

  Debug.DebugLog('xp1','ListDisplay, End',DLTrace);
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
  {$IFDEF Unix }
    OpenLst(PrinterName);
  {$ELSE }
    OpenLST(DruckLPT);
  {$ENDIF }
  write(lst,PrintString(DruckInit));
end;

procedure PrintPage;
begin
  write(lst,PrintString(DruckFF));
  printlines:=0;
end;

procedure PrintLine(s:string);
begin
  {$IFDEF Unix }
    s := IBMToISO(s);
  {$ENDIF }
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

const trackpath : boolean = false;

{ call of external program. errorlevel returned in gloval var errorlevel.
  errorlevel is negative if call was not performed (program not found).
  prog may be a batch file or using pipes. program extension MAY be specified
  as well as program path.
  CAUTION: Automatically chdirs back to "ownpath"!
  cls:   0=nicht loeschen; 1=loeschen, 2=loeschen+Hinweis, 3=Mitte loeschen
         -1=loeschen/25 Zeilen, 4=loeschen/nicht sichern,
         5=nicht loeschen/nicht sichern }
procedure shell(const prog:string; space:word; cls:shortint);

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
    Result:=unix.shell(command);
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
    Debug.DebugLog('xp1s','Xec, command:<'+command+'>, prompt:>'+prompt+'>', DLInform);
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
    if trim(command)='' then
    begin
      result:=-100;
      exit
    end;
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
  Debug.DebugLog('xp1s','shell, prog:<'+prog+'>, cls:<'+IntToStr(cls)+'>', DLInform);
  Debug.DebugLog('xp1s','shell, screenlines:'+IntToStr(screenlines)+',ScreenWidth:'+IntToStr(ScreenWidth), DLInform); { HJT 01.10.2005 }
  if (ParDebFlags and 1<>0) or ShellShowpar then
    ShowPar;
  getmaus(maussave);
  xp_maus_aus;
  if (cls<>4) and (cls<>5) then begin
    Debug.DebugLog('xp1s','shell, calling sichern(sp)', DLDebug);
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
            Debug.DebugLog('xp1s','shell, calling clwin(1,ScreenWidth,4,screenlines-2)', DLInform);
            clwin(1,ScreenWidth,4,screenlines-2);
            gotoxy(1,5);
          end;
  end;
  {$ifdef Win32}
  if not SysIsNT then begin  { HJT 10.09.05 }
    Debug.DebugLog('xp1s','shell, calling SysSetScreenSize(25,80)', DLInform);
    SysSetScreenSize(25,80);
    Debug.DebugLog('xp1s','shell, calling Window(1,1,80,25)', DLInform);
    Window(1,1,80,25);
  end;
  {$endif}
  if (cls=2) or (cls=-1) then
  begin
    if shell25 and (screenlines>25) then
      SysSetScreenSize(25, 80);
    if cls=2 then writeln(getres(113));  { Mit EXIT geht''s zurÅck zu CrossPoint. }
  end;
  cursor(curon);

  Debug.DebugLog('xp1s','shell, calling Xec('+prog+'>', DLInform);
  
  Errorlevel := Xec(prog,'[XP]');

  Debug.DebugLog('xp1s','shell, Xec returns Errowlevel:'+IntToStr(Errorlevel), DLInform);
  
  if shellkey or (ParDebFlags and 2<>0) or ShellWaitkey then
  begin
    if deutsch and (random<0.02) then write('Pressen Sie einen SchlÅssel ...')
    else write(getres(12));  { Taste drÅcken ... }
    m2t:=false;
    pushhp(51);
    clearkeybuf;
    wait(curon);
    pophp;
    m2t:=true;
    shellkey:=false;
  end;

  SysSetBackintensity;
  Debug.DebugLog('xp1s','shell, calling SetScreenSize, ScreenLines:'
                        + IntToStr(ScreenLines)
                        + ', ScreenWidth:'+IntToStr(ScreenWidth), 
                        DLInform);
  SetScreenSize;

  Debug.DebugLog('xp1s','shell, after calling SetScreenSize, ScreenLines:'
                        + IntToStr(ScreenLines)
                        + ', ScreenWidth:'+IntToStr(ScreenWidth), 
                        DLInform);

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
function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;
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
    lofs   : word;     { Ladeposition Datei }
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
      { HJT 05.02.2006: UTF8FormS bei UTF-8, ansonsten bleiben eventuell }
      { bei Headerzeilen am Ende nicht 'ausgeblankte' Zeichen }
      if UTF8 then begin
         mwrt(1,lfirst,' '+UTF8FormS(s,79+ScreenWidth-80));
      end else begin
         mwrt(1,lfirst,' '+forms(s,79+ScreenWidth-80));
      end;      
      inc(lfirst);
      inc(lofs,length(s)+2);
      end;
    if UTF8 then SetLogicalOutputCharset(csCP437);
    close(t);
    exthdlines:=0;
    lfirst:=min(lfirst,screenlines-5);
  end;

begin   // listfile
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
    begin
      ShowMsgHead;
      if UTF8 then 
      begin
        OldTCS := GetConsoleOutputCharset;
        OldLCS := GetLogicalOutputCharset;
        SetConsoleOutputCharset(csUTF8);
        SetLogicalOutputCharset(csCP437);
      end;
    end;

    Debug.Debuglog('xp1','listfile, TLister.CreateWithOptions',dlTrace);
      
    List := TLister.CreateWithOptions(1,iif(_maus and listscroller,screenwidth-1,screenwidth),lfirst,
             iif(listvollbild,screenlines,screenlines-fnkeylines-1),
             iif(listvollbild,1,4),'/F1/MS/S/APGD/'+iifs(listendcr,'CR/','')+
             iifs(_maus and ListScroller,'VSC/','')+
             { HJT 05.02.2006 wenn UTF-8 gewuenscht wird, die Options }
             { auch entsprechend versorgen                            }
             iifs(UTF8,'UTF8/','')+
             iifs(listmsg,'ROT/SIG/',''));
    if listwrap {or listkommentar} then
      list.Stat.WrapPos := iif(_maus and listscroller,78,80)+ScreenWidth-80;
//!!    if listmsg and ConvIso then List.OnConvert := ISO_conv;
    if not ListAutoscroll then List.Stat.Autoscroll := false;
    msg:=(_filesize(name)>1024*100);
    if msg then rmessage(130);    { 'Lade Datei ...' }
    List.ReadFromFile(name,lofs,listwrap);
    if msg then closebox;
    List.HeaderText := header;
    Debug.Debuglog('xp1s','listfile, List.OnKeypressed := listExt',dlTrace);
    List.OnKeypressed := listExt;
    List.UTF8Mode := utf8;
    llh:=listmsg;
    oldm:=ListMakros;
    if listmsg then ListMakros:=8;
    if cols<>0 then
    begin
      List.OnColor := listColor;
      if cols and 2<>0 then
      begin
        Debug.Debuglog('xp1s','listfile, List.OnDisplayLine := Listdisplay',dlTrace);
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

    if ListMsg and ListFixedHead then
    begin
      if UTF8 then 
      begin
        SetConsoleOutputCharset(OldTCS);
        SetLogicalOutputCharset(OldLCS);
      end;
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

  Debug.Debuglog('xp1s','listfile, At End',dlTrace);
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
{         TED-Softreturns zurÅckwandeln                 }

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
    SafeDeleteFile(ChangeFileExt(Name, '.'+bak)); { .BAK lˆschen }
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
    fehler(getres(116))   { DOS-Shell hier nicht mîglich }
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
    nt       : byte;

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
      nt:=dbReadInt(d,'netztyp');
      realname:=iifs(ntRealname(nt),dbReadStr(d,'realname'),'');
      user:=ComputeUserAddress(d);
      if (length(user)+length(realname)) <= screenwidth-7 then
        user:=user + iifs(realname<>'',' ('+realname+')','')
      else if length(user) <= screenwidth-10 then
        user:=user + iifs(realname<>'',' ('+leftStr(realname,screenwidth-10-length(user))+'...)','');
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


procedure WriteClipFile(const fn:string);
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
  if ioresult<>0 then;   { Logpath kînnte falsch gewesen sein }
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

procedure afehler(const txt:string; auto:boolean);
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
  fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+LeftStr(dat,2);
end;

function zdow(const dat:string):string;             { Z-Datum -> Mo/Di.. }
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


function ftime(const dat:string):string;            { Z-Datum -> Uhrzeit }
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
  Debug.DebugLog('xp1',Format('Writeing %s %s as Lastcall-Date in %s', [fdat(Dat), ftime(dat), OwnPath+NewDateFile]),DLDebug);
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
    if Box then
    begin
      inc(fpos,n);
      mwrt(x+3,y+2,dup(system.round(fpos * 50.0  / fsize),'≤'));
    end;
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
      mwrt(x+3,y+2,dup(50,'∞'));
      fpos := 0;
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

procedure _chdir(const p:string);
begin
 if p<>'' then 
    if not SetCurrentDir(ExcludeTrailingPathDelimiter(Trim(p))) then
      trfehler1(5,UpperCase(p),30);   { ungueltiges Verzeichnis: }
end;

function testmem(size:longint; wait:boolean):boolean;
begin
(*  if memavail<=size+16 then begin
    if wait then trfehler(6,30)  { 'zu wenig freier Speicher' }
    else rfehler(6);
    testmem:=false;
    end
  else *)
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

{ returns user identifier (email address with RFC nets) }
function ComputeUserAddress(d: DB):string;
var flags     : byte;
    netztyp   : byte;
    boxname   : string;
    username  : string;
    pointname : string;
    domain    : string;
    email     : string;
    aliaspt   : boolean;
begin
  dbRead(d, 'netztyp', netztyp);
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

end.



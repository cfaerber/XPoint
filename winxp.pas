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

{$I xpdefine.inc}

unit winxp;

{  ==========================  Interface-Teil  ==========================  }

interface

uses
  {$IFDEF Win32 }
    windows,
    {$IFDEF Delphi }
      messages,
    {$ENDIF }
  {$ENDIF }
  sysutils,
  inout,
  mime,
  xpglobal;

const
{$IFDEF NCRT }
      maxpull = 50; { sichern/holen verwenden auch diese Funktionen }
{$ELSE }
      maxpull = 30;
{$ENDIF }
      maxpush = 20;

      shadowcol: byte = 8;

{$IFNDEF NCRT}
const
{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }
  Blink         = 128;
{$ENDIF }

type  selproc = procedure(var sel:slcttyp);

type  TxpHandle = byte; //0..maxpull
      TWord = integer;  //todo: adjust or remove, as appropriate

var   wpstack  : array[1..maxpush] of TxpHandle;
      wpp      : byte;
      warrows  : boolean;     { Pfeile bei wslct anzeigen }
      warrcol  : byte;        { Farbe fuer Pfeile          }
      selp     : selproc;

procedure clwin(l,r,o,u:TWord);

procedure rahmen1(li,re,ob,un: Integer; const txt:string);    { Rahmen � zeichen       }
procedure rahmen2(li,re,ob,un: Integer; const txt:string);    { Rahmen � zeichnen      }
procedure rahmen3(li,re,ob,un: Integer; const txt:string);    { Special-Rahmen         }
procedure rahmen1d(li,re,ob,m,un: Integer; const txt:string); { Doppelrahmen � zeichen }
procedure rahmen2d(li,re,ob,m,un: Integer; const txt:string); { Doppelrahmen � zeichnen}
procedure explode(l,r,o,u,typ,attr1,attr2: Integer; msec:TWord; const txt:string);
procedure wshadow(li,re,ob,un: Integer);                { 8-Schatten }

procedure setrahmen(n:shortint);                 { Rahmenart fuer wpull+ setzen }
function  getrahmen:shortint;
procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
Procedure wpull(x1,x2,y1,y2: Integer; const text:string; var handle: TxpHandle);
procedure wrest(handle: TxpHandle);
procedure wslct(anz:integer; ta:pntslcta; handle:TxpHandle; pos:TWord;
  abs1:boolean; var n:TWord; var brk:boolean);
procedure seldummy(var sel:slcttyp);
procedure wpush(x1,x2,y1,y2: integer; text:string);
procedure wpushs(x1,x2,y1,y2: integer; text:string);
procedure wpop;

{$IFNDEF NCRT }
{ Schreiben eines Strings mit Update der Cursor-Posititon }
{ Diese Routine aktualisiert wenn noetig den LocalScreen }
{ Die Koordinaten beginnen bei 1,1 }
procedure Wrt(const x,y: Integer; const s:string);
{ Schreiben eines Strings, wie Write, CursorPosition
  wird aktualisiert }
{ Die Koordinaten beginnen bei 1,1 }
procedure Wrt2(const s:string);
{ Schreiben eines Strings ohne Update der Cursor-Position
  Der LocalScreen wird wenn noetig aktualisiert }
{ Die Koordinaten beginnen bei 1,1 }
procedure FWrt(const x,y: Integer; const s:string);
procedure Clreol;
procedure GotoXY(x, y: Integer);
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure ClrScr;
{$ENDIF }

{ Schreiben eines Strings ohne Update der Cursor-Position
  Der Textbackground (nicht die Farbe!) wird nicht veraendert }
procedure SDisp(const x,y:TWord; const s:string);

procedure consolewrite(x,y:TWord; num: Integer);


{ Routinen fuer 32 Bit Versionen, die den Zugriff auf den Bildschirm
  managen }

{ Liest ein Zeichen direkt von der Konsole aus
  x und y beginnen mit 1 }
procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);

{ Diese Routinen kopieren rechteckige Bildschirmbereiche aus
  der Console heraus und wieder hinein. Der Buffer muss dabei
  die dreifache Groesse (Win32) der Zeichenzahl besitzen. Die Koordinaten
  beginnen bei 1/1.

  Unter Win32 enthaelt der Buffer ein Byte Zeichen und zwei Byte
  fuer das Attribut. Unter anderen Betriebssystemen darf das
  anders gemacht werden. }
procedure ReadScreenRect(const l, r, o, u: Integer; var Buffer);
procedure WriteScreenRect(const l, r, o, u: Integer; var Buffer);


{ Fuellt eine Bildschirmzeile mit konstantem Zeichen und Attribut
  Die Koordinaten beginnen bei 1/1.
  Die Routine ist bis jetzt unter Win32 mit API und fuer den
  Rest mit FWrt implementiert }
procedure FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer);

{$IFDEF Win32 }
var { Enthaelt das Fensterhandle fuer die Console }
    OutHandle     : THandle;
{$ENDIF }

{$IFNDEF NCRT }
var
  WhereX, WhereY: Integer;
{$IFNDEF DOS32 }
  TextAttr: Byte;   { Current text attribute }
{$ENDIF }
{$ENDIF }

{
  Sets and reads the charset the conole actually is in. (Note: The 
  charset actually used may be different from the one requested)
}
procedure SetConsoleOutputCharset(NewCharset:TMimeCharsets);
function  GetConsoleOutputCharset:TMimeCharsets;

{ 
  Sets and reads the charset (most) functions that operate on the
  console screen use.
  If different from the actual console charset, there will be automatic
  conversion.
}
procedure SetLogicalOutputCharset(NewCharset:TMimeCharsets);
function  GetLogicalOutputCharset:TMimeCharsets;

{$IFDEF Win32}
function IsWindowsNT: boolean;
{$ENDIF}

procedure InitWinXPUnit;

{ ========================= Implementation-Teil =========================  }

implementation

uses
  {$IFDEF DOS32}
    crt, {for GotoXY}
  {$ENDIF}
  {$IFDEF unix}
    xplinux,
    xpcurses,
  {$ENDIF }
  osdepend,
  keys,
  maus2,
  typeform,
  xp0,
{$IFDEF DOS32}
  Go32,
{$ENDIF}
{$IFDEF FPC }
  Objects, (* For PWordArray *)
{$ENDIF}  
  unicode,
  utftools;

const rchar : array[1..3,1..6] of char =
              ('�Ŀ���','�ͻ�ȼ','�͸�Ծ');
{$ifdef NCRT }
      { LSSize - Gibt die maximale Groesse des LocalScreen-Buffers
        an. (Zeilen * Spalten * (sizeof(Char) + sizeof(Attribut))) }
      CharSize =        1;              { Groesse eines Zeichens }
      AttrSize =        1;              { Groesse eines Attributs }
      LSSize =          $7fff;          { Sollte fuer 160 x 100 reichen }
{$else }
      LSSize = $1fff;
{$endif }
      shad  : byte = 0;  { Zusatz-Fensterbreite/hoehe }

type  { Achtung: hier muss der komplette Bildschirm mit Attributen reinpassen }
  memarr     = array[0..$1fff] of byte;

  { Speicher den kompletten Bildschirm lokal zwischen, damit beim Auslesen
    des Fensterinhaltes nicht auf API-Funktionen zurueckgegriffen werden muss.
    Jede �nderung am Bildschirm _muss_ gleichzeitig hier gemacht werden }
  TLocalScreen = array[0..LSSize] of char;

{$IFDEF LocalScreen }
var
  LocalScreen: ^TLocalScreen;
{$ENDIF }

var pullw   : array[1..maxpull] of record
                                     l,r,o,u,wi : integer;
                                     ashad      : integer;
{$IFDEF NCRT }
                                     win        : TWinDesc;
{$ELSE }
                                     savemem    : ^memarr;
                                     free       : boolean;
                                     MemSize    : LongInt;
{$ENDIF }
                                   end;
    rahmen  : shortint;


{$IFNDEF NCRT }
procedure qrahmen(l,r,o,u: Integer; typ,attr:byte; clr:boolean);
var
  i: integer;
  SaveAttr: Byte;
begin
  SaveAttr := TextAttr; TextAttr := Attr;
  Fwrt(l, o, rchar[typ,1] + Dup(r-l-1, rchar[typ, 2]) + rchar[typ,3]);
  Fwrt(l, u, rchar[typ,5] + Dup(r-l-1, rchar[typ, 2]) + rchar[typ,6]);

  { Wird benutzt, wenn Fenster im Rahmen gefuellt werden soll }
  for i := o+1 to u -1 do
  begin
    FWrt(l, i, rchar[typ, 4]);
    FWrt(r, i, rchar[typ, 4]);
    if clr then
      FillScreenLine(l+1, i-1, ' ', r-l-2);
  end;
  TextAttr := SaveAttr;
end;
{$ENDIF NCRT }

{$IFDEF NCRT }
procedure wshadow(li,re,ob,un: Integer);
begin
  { Vorlaeufig kein Schatten unter Linux }
end;
{$ELSE }
procedure wshadow(li,re,ob,un: Integer);
var
  i: Integer;
  c: Char;
  Attr: SmallWord;
  save: byte;
begin
  moff;
  save := textattr;
  textattr := shadowcol;
  for i := ob to un do
  begin
    GetScreenChar(re, i, c, Attr);
    fwrt(re, i, c);
  end;
  for i := li to re do
  begin
    GetScreenChar(i, un, c, Attr);
    fwrt(i, un, c);
  end;
  textattr := save;
  mon;
end;
{$ENDIF }

procedure clwin(l,r,o,u:TWord);
var
  i: Integer;
begin
  for i := o to u do
    FillScreenLine(l, i, ' ', r-l+1);
end;

{$IFDEF DOS32}
type TCoord= record x,y: integer end;
{$ENDIF}

{$IFDEF Win32}
function Win32_Wrt(WritePos:TCoord; s:string): Integer; forward;
{$DEFINE CS_IMPLEMENTATION}
{$ENDIF}

{$IFNDEF CS_IMPLEMENTATION}
function Wrt_Convert(const s: string):string; forward;
{$ENDIF}

{$IFNDEF NCRT }
{$R-,Q-}
procedure Wrt(const x,y: Integer; const s:string);
{$IFDEF Win32Console }
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: DWord;
  Len: Integer;
{$ENDIF }
begin
  {$IFDEF Win32Console }
    WritePos.X := x-1; WritePos.Y := y-1;
    Len := Win32_Wrt(WritePos,s);
    FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);
    WhereX := x + len; WhereY := y;
    WritePos.X := WhereX - 1;
    if WritePos.X >= ScreenWidth then WritePos.X := 0;
    SetConsoleCursorPosition(OutHandle, WritePos);
  {$ELSE }
    FWrt(x, y, s);
    GotoXY(x+Length(s), y);
  {$ENDIF }
end; { Wrt }

procedure Wrt2(const s:string);
{$IFDEF Win32Console }
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: DWord;
  Len: Integer;
{$ENDIF }
begin
  {$IFDEF Win32Console }
    WritePos.X := WhereX-1; WritePos.Y := WhereY-1;
    Len := Win32_Wrt(WritePos,s);
    FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);
    WhereX := WhereX + Len;
    WritePos.X := WhereX;
    if WritePos.X >= ScreenWidth then WritePos.X := 0;
    SetConsoleCursorPosition(OutHandle, WritePos);
  {$ELSE }
    FWrt(WhereX, WhereY, s);
    GotoXY(WhereX+Length(s), WhereY);
  {$ENDIF }
end;
{$ENDIF }


{$IFNDEF NCRT }
procedure FWrt(const x,y: Integer; const s:string);
var
  {$IFDEF Win32Console }
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: DWord;
    Len: Integer;
  {$ELSE}
   {$IFDEF DOS32 }
    s2: String;
   {$ENDIF }
  {$ENDIF }
  i, Count: Integer;
begin
  {$IFDEF Win32Console }
    { Kompletten String an einem Stueck auf die Console ausgeben }
    WritePos.X := x-1; WritePos.Y := y-1;
    Len := Win32_Wrt(WritePos,s);
    FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);
  {$ELSE }
    {$IFDEF DOS32 }
      s2 := Wrt_Convert(s);
      Count := ((X-1)+(y-1)*screenwidth)*2;
      for i := 0 to Length(s2)-1 do
        memw[$B800:Count+i*2]:=(textattr shl 8) or byte(s2[i+1]);
    {$ELSE }
      GotoXY(x, y);
      Write(s);
    {$ENDIF }
  {$ENDIF Win32 }

  {$IFDEF Localscreen }
  { LocalScreen uebernimmt die �nderungen }
    if s <> '' then
      begin
        Count := ((x-1)+(y-1)*ScreenWidth)*2;
        FillChar(LocalScreen^[Count], Length(s)*2, TextAttr);
        for i := 1 to Length(s) do
        begin
          LocalScreen^[Count] := s[i];
          Inc(Count, 2);
        end;
       end;
  {$ENDIF LocalScreen }
  end;
{$ENDIF NCRT }

{$IFDEF Win32Console }
  procedure consolewrite(x,y:TWord; num: Integer);  { 80  Chars in xp0.charpuf (String) }
  var                                           { Attribute in xp0.attrbuf (Array of smallword)}
    WritePos: TCoord;                           { generiert in XP1.MakeListdisplay }
    OutRes: ULong;                            { Auf Konsole ausgeben....}
  begin
    WritePos.X := x-1; WritePos.Y := y-1;
    Num := Win32_Wrt(WritePos,Copy(charbuf,1,num));
    WriteConsoleOutputAttribute(OutHandle, @attrbuf[2], num, WritePos, OutRes);  end;
{$ELSE }
  procedure consolewrite(x,y:TWord; num: Integer);  { Num = Chars in xp0.charpuf (String) }
  var
    i, j: Integer;                        
  begin
    i := 1;                             
    while i < num do
    begin
      j := i;
      { Solange suchen, bis im String unterschiedliche Attribute auftauchen }
      while((AttrBuf[i+1] = AttrBuf[j+2]) and (j<num)) do inc(j);

      TextAttr := AttrBuf[i+1];
      FWrt(x+i-1, y, Copy(CharBuf, i, j-i+1));
      i := j; inc(i);
    end;
  end;
{$ENDIF Win32 }

{$IFNDEF NCRT}
procedure Clreol;
begin
  FillScreenLine(WhereX, WhereY, ' ', ScreenWidth-WhereX);
end;

procedure GotoXY(x, y: Integer);
var
  CurInfo: TCoord;
begin
  WhereX := X;
  WhereY := Y;
{$IFDEF Win32}
  CurInfo.X := X - 1;
  CurInfo.Y := Y - 1;

  SetConsoleCursorPosition(OutHandle, CurInfo);
{$ENDIF}
{$IFDEF DOS32}
  CRT.GotoXY(X,Y);
{$ENDIF}
end;

Procedure TextColor(Color: Byte);
{
  Switch foregroundcolor
}
Begin
  TextAttr:=(Color and $8f) or (TextAttr and $70);
End;



Procedure TextBackground(Color: Byte);
{
  Switch backgroundcolor
}
Begin
  TextAttr:=((Color shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
End;

procedure ClrScr;
var
  i: Integer;
begin
  for i := 1 to ScreenLines do
    FillScreenLine(1, i, ' ', ScreenWidth);
  GotoXY(1, 1);
end;

{$ENDIF}

procedure SDisp(const x,y:TWord; const s:string);
{$IFDEF Win32Console }
  var
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: ULong;
    i,Len: Integer;
    a: PWordArray;
  begin
    { Kompletten String an einem Stueck auf die Console ausgeben }
    WritePos.X := x-1; WritePos.Y := y-1;
    Len := Win32_Wrt(WritePos,s);
    GetMem(a,SizeOf(a[0])*Len);
    ReadConsoleOutputAttribute(OutHandle, a, Len, WritePos, OutRes);
    for i := 0 to Len-1 do
      a^[i] := (a^[i] and $FFF0) or (TextAttr and $F);
    WriteConsoleOutputAttribute(OutHandle, @a, Len, WritePos, OutRes);
    FreeMem(a);
{$ELSE Win32 }
  begin
    FWrt(x, y, s);
{$ENDIF Win32 }
end;

procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);
{$IFDEF Win32Console }
var
  ReadPos: TCoord;                       { Upper-left cell to Read from }
  OutRes: ULong;
  aChr: Char;
  aAttr: SmallWord;
begin
  ReadPos.X := x-1; ReadPos.Y := y-1;
  ReadConsoleOutputCharacter(OutHandle, @aChr, 1, ReadPos, OutRes);
  ReadConsoleOutputAttribute(OutHandle, @aAttr, 1, ReadPos, OutRes);
  c := aChr; Attr := aAttr;
{$ELSE }
{$IFDEF DOS32}
  var
    w: SmallWord;
{$ENDIF}
  begin
    {$IFDEF LocalScreen }
      c := Char(LocalScreen^[((x-1)+(y-1)*ScreenWidth)*2]);
      Attr := SmallWord(Byte(LocalScreen^[((x-1)+(y-1)*ScreenWidth)*2+1]));
    {$ENDIF }
    {$IFDEF DOS32 }
      w :=  MemW[$B800:((x-1)+(y-1)*ScreenWidth)*2];
      c := Char(w);
      Attr := w and $00ff;
    {$ENDIF }
{$ENDIF }
end;

procedure FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer);
{$IFDEF Win32Console }
  var
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: ULong;
  begin
    WritePos.x := x-1; WritePos.y := y-1;
    FillConsoleOutputCharacter(OutHandle, Chr, Count, WritePos, OutRes);
    FillConsoleOutputAttribute(OutHandle, TextAttr, Count, WritePos, OutRes)
  end;
{$ELSE }
  begin
    FWrt(x, y, Dup(Count, Chr));
  end;
{$ENDIF }

procedure ReadScreenRect(const l, r, o, u: Integer; var Buffer);
{$IFDEF Win32Console }
var
  BSize, Coord: TCoord;
  SourceRect: TSmallRect;
begin
  BSize.X := r-l+1; BSize.Y := u-o+1;
  Coord.X := 0; Coord.Y := 0;
  with SourceRect do
  begin
    Left := l-1; Right := r-1;
    Top := o-1; Bottom := u-1;
  end;
   ReadConsoleOutput(OutHandle, @Buffer, BSize, Coord, SourceRect);
{$ELSE }
var
  x, y, Offset: Integer;
begin
  Offset := 0;
  for y := o-1 to u-1 do
    for x := l-1 to r-1 do
    begin
      {$IFDEF LocalScreen }
        TLocalScreen(Buffer)[Offset] := LocalScreen^[(x+y*ScreenWidth)*2];
        TLocalScreen(Buffer)[Offset+1] := LocalScreen^[(x+y*ScreenWidth)*2+1];
      {$ENDIF }
      {$IFDEF DOS32 }
        TLocalScreen(Buffer)[Offset] := Char(Mem[$B800:(x+y*ScreenWidth)*2]);
        TLocalScreen(Buffer)[Offset+1] := Char(Mem[$B800:(x+y*ScreenWidth)*2+1]);
      {$ENDIF }
      Inc(Offset, 2);
    end;
{$ENDIF }
end;

procedure WriteScreenRect(const l, r, o, u: Integer; var Buffer);
{$IFDEF Win32Console }
var
  BSize, Coord: TCoord;
  DestRect: TSmallRect;
begin
  BSize.X := r-l+1; BSize.Y := u-o+1;
  Coord.X := 0; Coord.Y := 0;
  with DestRect do
  begin
    Left := l-1; Right := r-1;
    Top := o-1; Bottom := u-1;
  end;
  WriteConsoleOutput(OutHandle, @Buffer, BSize, Coord, DestRect);
{$ELSE }
  var
    x, y, i, j, Offset: Integer;
    s: String;
  begin
    Offset := 0;
    for y := o to u do
    begin
      {$IFDEF LocalScreen }
        { LocalScreen zeilenweise aktualisieren }
        Move(TLocalScreen(Buffer)[Offset],LocalScreen^[((y-1)*ScreenWidth+l-1)*2], (r-l+1)*2);
      {$ENDIF }
      x := l;
      while x <= r do
      begin
        j := x;
        { Solange suchen, bis im String unterschiedliche Attribute auftauchen }
        while (TLocalScreen(Buffer)[Offset+1] = TLocalScreen(Buffer)[Offset+3+(j-x)*2])
          and (j<r) do inc(j);

        s := '';
        for i := x to j do
        begin
          s := s + Char(TLocalScreen(Buffer)[Offset]);
          Inc(Offset, 2);
        end;
        TextAttr := SmallWord(Byte(TLocalScreen(Buffer)[Offset-1]));
        FWrt(x, y, s);
        x := j; inc(x);
      end;
    end;
{$ENDIF }
end;

{$IFDEF Debug }
  {$R+,Q+}
{$ENDIF }

{ attr1 = Rahmen/Background; attr2 = Kopf }
procedure explode(l,r,o,u,typ,attr1,attr2: Integer; msec:TWord; const txt:string);
var la           : byte;
    ls,rs,os,us,
    i,nx,ny,del  : byte;
begin
  if odd(r-l) then begin
    ls:=(r+l)div 2-1; rs:=ls+3; nx:=(r-l-3)div 2;
    end
  else begin
    ls:=(r+l)div 2-1; rs:=ls+2; nx:=(r-l-2)div 2;
    end;
  if odd(u-o) then begin
    os:=(u+o)div 2-1; us:=os+3; ny:=(u-o-3)div 2;
    end
  else begin
    os:=(u+o)div 2-1; us:=os+2; ny:=(u-o-2)div 2;
    end;
  del:=msec div max(nx,ny);
  if nx>ny then
    for i:=0 to nx do begin
      moff;
      qrahmen(ls-i,rs+i,os-i*ny div nx,us+i*ny div nx,typ,attr1,true);
      mon;
      Sysdelay(del);
  end
  else
    for i:=0 to ny do begin
      moff;
      qrahmen(ls-i*nx div ny,rs+i*nx div ny,os-i,us+i,typ,attr1,true);
      mon;
      Sysdelay(del);
    end;
  if txt<>'' then begin
    moff;
    la:=lastattr;
    attrtxt(attr1);
    wrt((r+l+1)div 2-length(txt)div 2-2,o,' ');
    attrtxt(attr2); Wrt2(' ' + txt + ' ');
    attrtxt(attr1); Wrt2(' ');
    attrtxt(la);
    mon;
    end;
end;

procedure rahmen1(li,re,ob,un: Integer; const txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,1,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then
  begin
    wrt((re+li+1) div 2 - length(txt) div 2 - 2,ob,' ');
    invtxt; Wrt2(' '+ txt + ' '); normtxt; Wrt2(' ');
  end;
  mon;
end;


procedure rahmen2(li,re,ob,un: Integer; const txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,2,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; Wrt2(' '+ txt + ' '); normtxt; Wrt2(' ');
    end;
  mon;
end;


procedure rahmen3(li,re,ob,un: Integer; const txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,3,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; Wrt2(' '+ txt + ' '); normtxt; Wrt2(' ');
    end;
  mon;
end;


Procedure rahmen1d(li,re,ob,m,un: Integer; const txt:string);
begin
  rahmen1(li,re,ob,un,txt);
  mwrt(li,m,hbar(re-li+1));
end;


Procedure rahmen2d(li,re,ob,m,un: Integer; const txt:string);
begin
  rahmen2(li,re,ob,un,txt);
  mwrt(li,m,'�'+dup(re-li-1,'�')+'�');
end;

procedure setrahmen(n:shortint);
begin
  rahmen:=n;
end;

function getrahmen:shortint;
begin
  getrahmen:=rahmen;
end;


Procedure wpull(x1,x2,y1,y2: Integer; const text:string; var handle: TxpHandle);
{$IFDEF NCRT }
var
  i: TxpHandle;
begin
  handle := 0;
  for I := 1 to maxpull do
    if (pullw[i].win.wHnd = nil) then
      with pullw[i] do
      begin
        handle:= i;
        if (rahmen > 0) then
          MakeWindow(win, x1, y1, x2, y2, text, true)
        else
          MakeWindow(win, x1, y1, x2, y2, text, false);
        l:=x1; r:=x2; o:=y1; u:=y2;
        ashad:=shad;
        wi:=(r-l+1+shad)*2;
        break;
      end;
end;
{$ELSE }
var
  i : TxpHandle;
begin
  if (x2-x1<1) or (y2-y1<1) then
  begin
    writeln('WPULL error');
    halt(1);
  end;
  savecursor;
  cursor(curoff);
  i:=1;
  while not pullw[i].free do
    inc(i);
  handle:=i;
  with pullw[i] do begin
    free:=false;
    l:=x1; r:=x2;
    o:=y1; u:=iif(y2+shad>screenlines,screenlines-shad,y2);
    ashad:=shad;
    wi:=(r-l+1+shad)*2;
    moff;
    MemSize := wi*(u-o+ashad+1)*2;

    getmem(savemem, MemSize);

    ReadScreenRect(l, r+ashad, o, u+ashad, SaveMem^);

    mon;
    if rahmen=1 then rahmen1(l,r,o,u,text);
    if rahmen=2 then rahmen2(l,r,o,u,text);
    if rahmen>0 then clwin(l+1,r-1,o+1,u-1);
    if rahmen<0 then explode(l,r,o,u,abs(rahmen),normattr,invattr,100,text);
    end;
  restcursor;
end;
{$ENDIF } { NCRT }

Procedure wrest(handle: TxpHandle);
{$IFDEF NCRT }
begin
  RestoreWindow(pullw[handle].win);
  pullw[handle].win.wHnd:= nil;
end;
{$ELSE }
begin
  with pullw[handle] do
  begin
    moff;
    WriteScreenRect(l, r+ashad, o, u+ashad, SaveMem^);
    mon;
    freemem(savemem, MemSize);
    free:=true;
  end;
end;
{$ENDIF }

procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
var i,j : TWord;
    xch : boolean;
    sa  : slcttyp;
    l   : pntslcta;
begin
  l:=pntslcta(pa);
  j:=anz-1;
  repeat
    xch:=false;
    for i:=1 to j do
      if UpperCase(l^[i].el)>UpperCase(l^[i+1].el) then begin
        sa:=l^[i];
        l^[i]:=l^[i+1];
        l^[i+1]:=sa;
        xch:=true;
        end;
  until not xch;
end;


procedure wslct(anz:integer; ta:pntslcta; handle:TxpHandle; pos:TWord;
  abs1:boolean; var n:TWord; var brk:boolean);

var z          : taste;
    i,po,pon   : integer;
    wsize      : TWord;
    pa,pan     : integer;
    ende       : boolean;
    ox         : integer;

Procedure dispage;
var i:integer;
begin
  moff;
  with pullw[handle] do begin
    for i:=1 to wsize do
      if i+pa<=anz then
        with ta^[i+pa] do begin
          if zu then normtxt else hightxt;
          wrt(l+2,ox+i,el);
          normtxt;
          end
      else
        wrt(l+2,ox+i,sp(r-l-3));
    if warrows then begin
      attrtxt(warrcol);
      wrt(l,o+1,iifc(pa>0,#30,#179));
      wrt(l,u-1,iifc(pa+wsize<anz,#31,#179));
      normtxt;
      end;
    end;
  mon;
end;

Procedure godown;
begin
  if (pan+pon<anz) then begin
    inc(pon);
    if pon>wsize then begin
      dec(pon); inc(pan);
      end;
    end;
end;

Procedure goup;
begin
  if pon+pan>1 then begin
    dec(pon);
    if pon=0 then begin
      dec(pan); pon:=1;
      end;
    end;
end;


begin    { of wslct }
  if anz=0 then begin
    brk:=true;
    exit;
    end;
  pos:=min(pos,anz);
  savecursor;
  ende:=false;
  with pullw[handle] do begin
    for i:=1 to anz do
      ta^[i].el:=forms(ta^[i].el,r-l-3);
    ox:=iif(abs1,o+1,o);
    wsize:=u-ox-1;
    if pos<=anz then begin
      pa:=0; po:=pos;
      end
    else begin
      pa:=pos-1; po:=1;
      end;
    if po>wsize then begin
      inc(pa,po-wsize);
      po:=wsize;
      end;
    dispage;
    mausiniti;
    repeat
      mauszuo:=(pa+po>1);
      mauszuu:=(pa+po<anz);
      invtxt;
      mwrt(l+2,ox+po,ta^[pa+po].el);
      selp(ta^[pa+po]);
      get(z,curoff);
      pan:=pa; pon:=po;
      if (z=keydown) or (z=keytab) or (z[1]='2') then
        godown
      else if (z=keyup) or (z=keystab) or (z[1]='8') then
        goup
      else if z=keyesc then begin
        brk:=true;
        ende:=true;
        end
      else if (z=keyhome) or (z[1]='7') then begin
        pon:=1;
        if not ta^[pan+pon].zu then godown;
        end
      else if (z=keyend) or (z[1]='1') then begin
        pon:=min(wsize,anz-pan);
        if not ta^[pan+pon].zu then goup;
        end
      else if (z=keypgup) or (z[1]='9') then begin
        if pan=0 then pon:=1
        else pan:=max(0,pan-wsize);
        end
      else if (z=keypgdn) or (z[1]='3') then begin
        if pan+wsize>=anz then pon:=anz-pan
        else pan:=min(anz-pon,pan+wsize);
        end
      else if (z=keycpgu) or (z=keychom) then begin
        pan:=0; pon:=1;
        end
      else if (z=keycpgd) or (z=keycend) then begin
        pan:=max(0,anz-wsize);
        pon:=anz-pan;
        end
      else if z=keycr then begin
        brk:=false;
        ende:=true;
        end;
      if pa<>pan then begin
        pa:=pan;
        po:=pon;
        dispage;
        end
      else begin
        normtxt;
        wrt(l+2,ox+po,ta^[pa+po].el);
        po:=pon;
        end;
    until ende;
    n:=po+pa;
    end;
  restcursor;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure seldummy(var sel:slcttyp);
begin
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure wpush(x1,x2,y1,y2: Integer; text:string);
var r   : Integer;
    tx1 : char;
begin
  if wpp=maxpush then writeln('WPUSH error')
  else begin
    r:=rahmen;
    if (text='*') or (text='-') then begin
      setrahmen(0);
      tx1:=text[1];
      text:='';
      end
    else
      tx1:=' ';
    inc(wpp);
    wpull(x1,x2,y1,y2,text,wpstack[wpp]);
    if tx1='*' then clwin(x1,x2,y1,y2);
    setrahmen(r);
    end;
end;


procedure wpushs(x1,x2,y1,y2: Integer; text:string);
begin
  shad:=1;
  wpush(x1,x2,y1,y2,text);
{  rahmen1(x1,x2,y1,y2,text);}
{  clwin(x1+1,x2-1,y1+1,y2-1); }
  wshadow(x1+1,x2+1,y1+1,y2+1);
  shad:=0;
end;

procedure wpop;
begin
  if wpp=0 then
{$IFDEF Debug }
  writeln('WPOP error')
{$ENDIF }
  else begin
    wrest(wpstack[wpp]);
    dec(wpp);
    Disp_DT;
    end;
end;

//
// Win32: can switch charsets on Windows NT/2k/XP, not on 95/98/ME
//
{$IFDEF Win32 }
{$DEFINE CS_IMPLEMENTATION }
{$DEFINE CS_IMPLEMENTATION_USES_CODEPAGES }
var IsUnicode: Boolean;
    OutputCP,TrueOutputCP: Integer;
    OutputCharset: TMIMECharsets;

var SourceToUTF8: TUTF8Encoder;
    UTF8ToDest:  TUTF8Decoder;    
var ConvertersOK: Boolean;    
{$ENDIF}

{$IFDEF DOS32}
{$DEFINE CS_IMPLEMENTATION_USES_CODEPAGES }
{$ENDIF}

{$IFDEF CS_IMPLEMENTATION_USES_CODEPAGES }
function GetCPfromCharset(cs:TMimeCharsets):Integer;
begin
  case cs of
    csUTF8:       result := 65001;
    csCP437:      result := 437;
    csCP850:      result := 850;
    csCP857:      result := 857;
    csCP858:      result := 858;
    csCP866:      result := 866;
    csCP1250:     result := 1250;
    csCP1251:     result := 1251;
    csCP1252:     result := 1252;
    csCP1255:     result := 1255;
    csISO8859_1:  result := 28591;
    csISO8859_2:  result := 28592;
    csISO8859_3:  result := 28593;
    csISO8859_4:  result := 28594;
    csISO8859_5:  result := 28595;
    csISO8859_6:  result := 28596;
    csISO8859_7:  result := 28597;
    csISO8859_8:  result := 28598;
    csUTF7:       result := 65000;
    csASCII:      result := 437;
    else          result := 0;
  end;
end;

function GetCharsetfromCP(cp:Integer):TMimeCharsets;
begin
  case cp of
    65001: result := csUTF8;
    437:   result := csCP437;
    850:   result := csCP850;
    857:   result := csCP857;
    858:   result := csCP858;
    866:   result := csCP866;
    1250:  result := csCP1250;
    1251:  result := csCP1251;
    1252:  result := csCP1252;
    1255:  result := csCP1255;
    28591: result := csISO8859_1;
    28592: result := csISO8859_2;
    28593: result := csISO8859_3;
    28594: result := csISO8859_4;
    28595: result := csISO8859_5;
    28596: result := csISO8859_6;
    28597: result := csISO8859_7;
    28598: result := csISO8859_8;
    28599: result := csISO8859_9;
    28605: result := csISO8859_15;
    65000: result := csUTF7;
    else   result := csUNKNOWN;
  end;
end;
{$ENDIF CS_IMPLEMENTATION_USES_CODEPAGES }

{$IFDEF Win32 }
procedure MakeConverters;
var TrueOutputCharset: TMIMECharsets;
begin
  if ConvertersOK then exit; 
  ConvertersOK := true;

  SourceToUTF8.Free;  SourceToUTF8:=nil;
  UTF8ToDest.Free;    UTF8ToDest:=nil;

{ 
  NB: The Unicode functions only work on NT/2k/XP, not on 95/98/ME.
  That's not a big problem as only NT/2k/XP allow switching charsets
  for the console (i.e. 95/98/ME _always_ uses the OEM codepage).
}
  if IsUnicode then 
  begin
    OutputCP := GetCPfromCharset(OutputCharset);
    if (OutputCP = 0) or not IsValidCodePage(OutputCP) then
    begin
      OutputCP := 65001 {UTF-8};
      TrueOutputCP := 65001;
      SourceToUTF8:= CreateUTF8Encoder(OutputCharset);
    end;
  end else
  begin
    TrueOutputCharset := GetCharsetfromCP(TrueOutputCP);
    if (TrueOutputCharset = OutputCharset) or
       (csUNKNOWN in [TrueOutputCharset,OutputCharset]) then exit;
    if OutputCharset<>csUTF8 then
      SourceToUTF8:= CreateUTF8Encoder(OutputCharset);
    UTF8ToDest  := CreateUTF8Decoder(GetCharsetfromCP(TrueOutputCP));
  end;  

end;

function Win32_Wrt(WritePos:TCoord; s:string): Integer;
var
  OutRes: ULong;
  s2: String;
  dwFlags: DWORD;
  
begin
{$IFDEF Win32Console }
  if Length(s)<=0 then begin result := 0; exit; end;
  MakeConverters;
{ 
  NB: The Unicode functions only work on NT/2k/XP, not on 95/98/ME.
  That's not a big problem as only NT/2k/XP allow switching charsets
  for the console (i.e. 95/98/ME _always_ uses the OEM codepage).
}
  if Assigned(SourceToUTF8) then s := SourceToUTF8.Encode(s);
  if Assigned(UTF8ToDest)   then s := UTF8ToDest.Decode(s);
  
  if IsUnicode then
  begin
    if(OutputCP = 50220) or
      (OutputCP = 50221) or
      (OutputCP = 50222) or
      (OutputCP = 50225) or
      (OutputCP = 50227) or
      (OutputCP = 50229) or
      (OutputCP = 52936) or
      (OutputCP = 54936) or
      ((OutputCP >= 57002) and (OutputCP <= 57011)) or
      (OutputCP = 65000) or
      (OutputCP = 65001) then 
      dwFlags := 0 
    else
      dwFlags := MB_PRECOMPOSED + MB_USEGLYPHCHARS;
  
    OutRes := MultiByteToWideChar(OutputCP,dwFlags,@(s[1]),
      Length(s),nil,0);
    if OutRes = 0 then begin Result := 0; Exit; end;
    SetLength(s2,OutRes*2);
    OutRes := MultiByteToWideChar(OutputCP,dwFlags,@(s[1]),
      Length(s),@(s2[1]),Length(s2) div 2);
    WriteConsoleOutputCharacterW(OutHandle, @(s2[1]), OutRes, WritePos, OutRes);
  end else 
  begin
    WriteConsoleOutputCharacterA(OutHandle, @(s[1]), Length(s), WritePos, OutRes);
  end;

  Result := OutRes;
{$ENDIF }
end;

procedure SetConsoleOutputCharset(NewCharset:TMimeCharsets);
var NewCP: IntegeR;
begin
{$IFDEF Win32Console }
  if not IsWindowsNT then exit;

  NewCP := GetCPfromCharset(NewCharset);
  IsUnicode := (NewCP = 65000) or (NewCP = 65001) or (NewCP = 1200);
  if not IsUnicode then NewCP := GetOEMCP;
  SetConsoleOutputCP(NewCP);
  TrueOutputCP := GetConsoleOutputCP;
  convertersOK := false;
{$ENDIF }
end;

function  GetConsoleOutputCharset:TMimeCharsets;
begin
{$IFDEF Win32Console }
  result := GetCharsetfromCP(TrueOutputCP);
{$ELSE}
{$ENDIF}
end;

procedure SetLogicalOutputCharset(NewCharset:TMimeCharsets);
begin
  OutputCharset := NewCharset;
  convertersOK := false;
end;

function  GetLogicalOutputCharset:TMimeCharsets;
begin
  result := OutputCharset;
end;

procedure InitCharsetSystem;
begin
  IsUnicode := false;
  ConvertersOK := false;  

  if IsWindowsNT then
  begin
    SetConsoleCP(437);
    SetConsoleOutputCP(437);
  end;

  TrueOutputCP := GetConsoleOutputCP;
  OutputCharset := csCP437;
end;

procedure ExitCharsetSystem;
begin
  SourceToUTF8.Free;
  UTF8ToDest.Free;
end;

function IsWindowsNT: Boolean;
begin
  result := Longint(Windows.GetVersion)>=0;
end;
{$ENDIF}

//
// Default implementation: Internal conversion, assume IBMPC (CP437)
//
{$IFNDEF CS_IMPLEMENTATION}
var OutputCharset: TMimeCharsets;
    TrueOutputCharset: TMimeCharsets;

var SourceToUTF8: TUTF8Encoder;
    UTF8ToDest:  TUTF8Decoder;    
var ConvertersOK: Boolean;    

procedure MakeConverters;
begin
  if ConvertersOK then exit;
  ConvertersOK := true;

  SourceToUTF8.Free;  SourceToUTF8:=nil;
  UTF8ToDest.Free;    UTF8ToDest:=nil;

  if OutputCharset=TrueOutputCharset then exit;

  if OutputCharset<>csUTF8 then
    SourceToUTF8:= CreateUTF8Encoder(OutputCharset);

  if TrueOutputCharset<>csUTF8 then
    UTF8ToDest  := CreateUTF8Decoder(TrueOutputCharset);
end;

function  GetConsoleOutputCharset:TMimeCharsets;
begin
  result:=TrueOutputCharset;
end;

function  GetLogicalOutputCharset:TMimeCharsets;
begin
  result:=OutputCharset;
end;
    
procedure SetLogicalOutputCharset(NewCharset:TMimeCharsets);
begin
  OutputCharset:=NewCharset;
  ConvertersOK := false;
end;

procedure SetConsoleOutputCharset(NewCharset:TMimeCharsets);
begin
  { No operation, charset is fixed }
end;

function Wrt_Convert(const s: string):string; 
begin
  MakeConverters;
  result := s;
  if Assigned(SourceToUTF8) then result := SourceToUTF8.Encode(result);
  if Assigned(UTF8ToDest)   then result := UTF8ToDest.Decode(result);
end;

procedure InitCharsetSystem;
  {$IFDEF DOS32}
  var r: TRealRegs;
  {$ENDIF}
begin
  OutputCharset:=csCP437;
  {$IFDEF DOS32} 
  { INT 21 - DOS 3.3+ - GET GLOBAL CODE PAGE TABLE		}
  {	    AX = 6601h						}
  Fillbyte(r,sizeof(r),0);
  r.ax := $6601;
  RealIntr($21,r);
  
  if(r.flags and carryflag)=0 then
    TrueOutputCharset := GetCharsetFromCP(r.bx)
  else
    TrueOutputCharset := csUNKNOWN;
  {$ELSE}
  TrueOutputCharset:=csCP437;
  {$ENDIF}
  SourceToUTF8:=nil;
  UTF8ToDest:=nil;
  ConvertersOK := false;
end;

procedure ExitCharsetSystem;
begin
  SourceToUTF8.Free;
  UTF8ToDest.Free;
end;
{$ENDIF}

var
  SavedExitProc: pointer;

procedure ExitWinXPUnit;
begin
  ExitProc:= SavedExitProc;
  {$IFDEF Localscreen }
  FreeMem(LocalScreen);
  {$ENDIF }
  ExitCharsetSystem;
end;

{$IFDEF Win32GUI }
const
  AppName = 'OpenXP/32 GUI';

function WindowProc(Window: HWnd; AMessage: UINT; WParam : WPARAM;
                    LParam: LPARAM): LRESULT; stdcall; export;

  var
     dc : hdc;
     ps : Tpaintstruct;
     r : Trect;
     s: String;
     i,y: Integer;
begin
  WindowProc := 0;

  case AMessage of
    wm_paint:
    begin
      SetLength(s, ScreenWidth);
      dc:=BeginPaint(Window,ps);
      GetClientRect(Window, r);
      for y := 0 to 20 do
      begin
        for i := 0 to ScreenWidth - 1do
          s[1+i] := LocalScreen^[y*ScreenWidth*2+i*2];
        TextOut(dc, 0, y*14, PChar(s), ScreenWidth);

      end;
      EndPaint(Window,ps);
      Exit;
    end;
    wm_Destroy:
      begin
         PostQuitMessage(0);
         Exit;
      end;
  end;

  WindowProc := DefWindowProc(Window, AMessage, WParam, LParam);
end;

 { Register the Window Class }
function WinRegister: Boolean;
var
  WindowClass: WndClass;
begin
  WindowClass.Style := cs_hRedraw or cs_vRedraw;
  WindowClass.lpfnWndProc := TFNWndProc(@WindowProc);
  WindowClass.cbClsExtra := 0;
  WindowClass.cbWndExtra := 0;
  WindowClass.hInstance := system.MainInstance;
  WindowClass.hIcon := LoadIcon(0, idi_Application);
  WindowClass.hCursor := LoadCursor(0, idc_Arrow);
  WindowClass.hbrBackground := GetStockObject(BLACK_BRUSH);
  WindowClass.lpszMenuName := nil;
  WindowClass.lpszClassName := AppName;

  Result := RegisterClass(WindowClass) <> 0;
end;

 { Create the Window Class }
function WinCreate: HWnd;
var
  hWindow: HWnd;
begin
  hWindow := CreateWindow(AppName, 'OpenXP/32',
              ws_OverlappedWindow, cw_UseDefault, cw_UseDefault,
              cw_UseDefault, cw_UseDefault, 0, 0, system.MainInstance, nil);

  if hWindow <> 0 then begin
    ShowWindow(hWindow, CmdShow);
    UpdateWindow(hWindow);
  end;

  Result := hWindow;
end;
{$ENDIF }

procedure InitWinXPUnit;
{$IFNDEF NCRT }
var
  i: byte;
{$IFDEF Win32Gui }
  hWindow: HWnd;
  aMessage: TMsg;
{$ENDIF }
begin
  for i:=1 to maxpull do
    pullw[i].free:=true;
{$ELSE }
begin
  FillChar(pullw, sizeof(pullw), 0);
{$ENDIF }
  rahmen:=1;
  fnproc[3,10]:=DummyFN;
  wpp:=0;
  warrows:=false;
  warrcol:=7;
  selp:=seldummy;

{$IFDEF LocalScreen }
  GetMem(LocalScreen, SizeOf(LocalScreen^));
{$ENDIF }

{$IFDEF Win32}
{$IFDEF Win32Console }
  { Consolenhandle holen }
  OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  InitCharsetSystem;
{$ELSE }
  if not WinRegister then begin
    MessageBox(0, 'Register failed', nil, mb_Ok);
    Exit;
  end;
  hWindow := WinCreate;
  if longint(hWindow) = 0 then begin
    MessageBox(0, 'WinCreate failed', nil, mb_Ok);
    Exit;
  end;
{$ENDIF }
{$ELSE }
  InitCharsetSystem;
{$ENDIF }

  SavedExitProc:= ExitProc;
  ExitProc:= @ExitWinXPUnit;
end;

{
  $Log$
  Revision 1.89  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.88  2002/12/05 19:36:21  dodi
  - removed ambiguous word type

  Revision 1.87  2002/07/25 20:43:53  ma
  - updated copyright notices

  Revision 1.86  2002/07/24 00:09:46  cl
  - Fixed illegal codepage with SetConsoleOutputCP

  Revision 1.85  2002/07/13 12:21:11  ma
  - fix wpull: window was not saved if y+shad>screenlines

  Revision 1.84  2002/03/02 18:23:51  cl
  - Correct charset handling for DOS32

  Revision 1.83  2002/02/22 18:29:59  cl
  - added windows-1250

  Revision 1.82  2002/02/21 13:52:31  mk
  - removed 21 hints and 28 warnings

  Revision 1.81  2002/02/20 22:28:24  cl
  - added all known charsets to known codepages

  Revision 1.80  2002/01/30 22:08:49  mk
  - parameter validation for SetConsoleCursorPosition

  Revision 1.79  2002/01/12 23:05:13  mk
  - fixed DOS32 compile

  Revision 1.78  2002/01/12 21:57:13  mk
  - fixed FPC compilation

  Revision 1.77  2002/01/12 14:42:13  cl
  - Kylix 2 compile fixes

  Revision 1.76  2002/01/12 14:13:17  cl
  - Kylix 2 compile fix

  Revision 1.75  2002/01/12 11:10:12  mk
  - Win32 GUI Part I

  Revision 1.74  2002/01/03 23:49:48  mk
  - fixed range check error (followed by a crash) in win32_wrt, when outres = 0

  Revision 1.73  2002/01/03 21:41:16  cl
  - fix for Windows NT/2k/XP full screen mode

  Revision 1.72  2002/01/02 15:33:51  cl
  - UUZ can now (optionally) not recode any charsets.
  - new box configuration option: UUZRecodeCharset
  - extract_msg can not handle all charsets and extract in UTF8 mode.

  Revision 1.71  2002/01/01 19:34:37  cl
  - Basic support for console charset switching + initial Win32 implementation

  Revision 1.70  2001/12/30 19:22:05  cl
  - Linux FPC compile fix (prototype for wshadow did not match NCRT implementation).

  Revision 1.69  2001/12/05 18:24:45  mk
  - fixed last commit

  Revision 1.68  2001/12/05 17:42:56  mk
  - optimized Win32 screen draw speed, part I

  Revision 1.67  2001/10/21 13:33:14  mk
  - compile fix for windows

  Revision 1.66  2001/10/21 12:49:57  ml
  - removed some warnings

  Revision 1.65  2001/10/21 12:33:54  ml
  - killed local constant
  - fix for range-error

  Revision 1.64  2001/10/11 15:27:01  mk
  - implemented direct screen writes for DOS32, no more LocalScreen

  Revision 1.63  2001/10/01 19:30:09  ma
  - compiles again (DOS32)

  Revision 1.62  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.61  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.60  2001/08/10 20:57:57  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.59  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.58  2001/07/28 12:39:56  mk
  - removed unused unit strings

  Revision 1.57  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.56  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.55  2001/04/22 21:34:38  mk
  - FPC compatibility fix

  Revision 1.54  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.53  2001/02/22 11:48:13  ma
  - fixed crashes occurring with ScreenWidth>128

  Revision 1.52  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.51  2000/11/16 14:04:48  hd
  - Unit DOS entfernt

  Revision 1.50  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.49  2000/09/29 11:27:43  fe
  Ungenutzte, lokale Variablen entfernt.

  Revision 1.48  2000/09/28 03:21:41  mk
  - spezielle Anpassungen fuer Debug-Modus

  Revision 1.47  2000/07/27 13:41:49  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.46  2000/07/27 10:12:59  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.45  2000/07/11 21:39:19  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.44  2000/07/09 09:09:54  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.43  2000/07/07 14:38:35  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.42  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.41  2000/07/02 15:13:51  mk
  - Anpassungen an neue FPC-Version

  Revision 1.40  2000/07/02 14:24:51  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.39  2000/06/24 14:10:26  mk
  - 32 Bit Teile entfernt

  Revision 1.38  2000/06/23 15:59:14  mk
  - 16 Bit Teile entfernt

  Revision 1.37  2000/06/22 19:53:29  mk
  - 16 Bit Teile ausgebaut

  Revision 1.36  2000/05/10 11:01:14  hd
  - maxpull erhoeht

  Revision 1.35  2000/05/07 18:17:36  hd
  - Wrt, Wrt2, FWrt und qrahmen sind jetzt Bestandteil von XPCURSES.PAS
  - Kleiner Fix im Window-Handling

  Revision 1.34  2000/05/07 15:56:32  hd
  Keine Uhr unter Linux

  Revision 1.33  2000/05/07 13:58:07  mk
  - Localscreen laeuft jetzt komplett

  Revision 1.32  2000/05/07 10:40:40  hd
  - Fix: FWrt: FPC behandelt Integer nicht als Integer :-/

  Revision 1.31  2000/05/07 09:24:02  hd
  FWrt-Korrektur

  Revision 1.30  2000/05/06 17:29:21  mk
  - DOS DPMI32 Portierung

  Revision 1.29  2000/05/06 15:57:03  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.28  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.27  2000/05/02 11:29:13  mk
  - Anpassungen 32 Bit und Localscreen

  Revision 1.26  2000/05/01 18:58:55  hd
  Einige Anpassungen an xpcurses

  Revision 1.25  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.24  2000/04/23 16:18:41  mk
  - Source wieder unter Linux compilierbar

  Revision 1.23  2000/04/23 07:58:52  mk
  - OS/2-Portierung

  Revision 1.22  2000/04/17 14:32:04  ml
  xpme wieder unter linux kompilierbar

  Revision 1.21  2000/04/13 12:48:33  mk
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


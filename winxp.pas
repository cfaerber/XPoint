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

{$I xpdefine.inc}

unit winxp;

{  ==========================  Interface-Teil  ==========================  }

interface

uses
  sysutils,
  {$IFDEF Win32 } windows, {$ENDIF }
  {$IFDEF DOS32} crt, {for GotoXY} {$ENDIF}
  {$IFDEF unix} xplinux,xpcurses, {$ENDIF }
  {$IFDEF VP } vpsyslow, {$ENDIF }
  OsDepend, keys,inout,maus2,typeform, xpglobal;

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

var   wpstack  : array[1..maxpush] of word;
      wpp      : byte;
      warrows  : boolean;     { Pfeile bei wslct anzeigen }
      warrcol  : byte;        { Farbe fÅr Pfeile          }
      selp     : selproc;

procedure clwin(l,r,o,u:word);

procedure rahmen1(li,re,ob,un: Integer; const txt:string);    { Rahmen ≥ zeichen       }
procedure rahmen2(li,re,ob,un: Integer; const txt:string);    { Rahmen ∫ zeichnen      }
procedure rahmen3(li,re,ob,un: Integer; const txt:string);    { Special-Rahmen         }
procedure rahmen1d(li,re,ob,m,un: Integer; const txt:string); { Doppelrahmen ≥ zeichen }
procedure rahmen2d(li,re,ob,m,un: Integer; const txt:string); { Doppelrahmen ∫ zeichnen}
procedure explode(l,r,o,u,typ,attr1,attr2: Integer; msec:word; const txt:string);
procedure wshadow(li,re,ob,un: Integer);                { 8-Schatten }

procedure setrahmen(n:shortint);                 { Rahmenart fÅr wpull+ setzen }
function  getrahmen:shortint;
procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
procedure wpull(x1,x2,y1,y2: Integer; const text:string; var handle:word);
procedure wrest(handle:word);
procedure wslct(anz:integer; ta:pntslcta; handle,pos:word; abs1:boolean;
                var n:word; var brk:boolean);
procedure seldummy(var sel:slcttyp);
procedure wpush(x1,x2,y1,y2: integer; text:string);
procedure wpushs(x1,x2,y1,y2: integer; text:string);
procedure wpop;

{$IFNDEF NCRT }
{ Schreiben eines Strings mit Update der Cursor-Posititon }
{ Diese Routine aktualisiert wenn nîtig den LocalScreen }
{ Die Koordinaten beginnen bei 1,1 }
procedure Wrt(const x,y:word; const s:string);
{ Schreiben eines Strings, wie Write, CursorPosition
  wird aktualisiert }
{ Die Koordinaten beginnen bei 1,1 }
procedure Wrt2(const s:string);
{ Schreiben eines Strings ohne Update der Cursor-Position
  Der LocalScreen wird wenn nîtig aktualisiert }
{ Die Koordinaten beginnen bei 1,1 }
procedure FWrt(const x,y:word; const s:string);
procedure Clreol;
procedure GotoXY(x, y: Integer);
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure ClrScr;
{$ENDIF }

{ Schreiben eines Strings ohne Update der Cursor-Position
  Der Textbackground (nicht die Farbe!) wird nicht verÑndert }
procedure SDisp(const x,y:word; const s:string);

procedure consolewrite(x,y:word; num:dword);


{ Routinen fÅr 32 Bit Versionen, die den Zugriff auf den Bildschirm
  managen }

{ Liest ein Zeichen direkt von der Konsole aus
  x und y beginnen mit 1 }
procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);

{ Diese Routinen kopieren rechteckige Bildschirmbereiche aus
  der Console heraus und wieder hinein. Der Buffer mu· dabei
  die dreifache Grî·e (Win32) der Zeichenzahl besitzen. Die Koordinaten
  beginnen bei 1/1.

  Unter Win32 enthÑlt der Buffer ein Byte Zeichen und zwei Byte
  fÅr das Attribut. Unter anderen Betriebssystemen darf das
  anders gemacht werden. }
procedure ReadScreenRect(const l, r, o, u: Integer; var Buffer);
procedure WriteScreenRect(const l, r, o, u: Integer; var Buffer);


{ FÅllt eine Bildschirmzeile mit konstantem Zeichen und Attribut
  Die Koordinaten beginnen bei 1/1.
  Die Routine ist bis jetzt unter Win32 mit API und fÅr den
  Rest mit FWrt implementiert }
procedure FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer);

{$IFDEF Win32 }
var { EnthÑlt das Fensterhandle fÅr die Console }
    OutHandle     : THandle;
{$ENDIF }

{$IFNDEF NCRT }
var
  WhereX, WhereY: Integer;
{$IFNDEF DOS32 }
  TextAttr: Byte;   { Current text attribute }
{$ENDIF }
{$ENDIF }

procedure InitWinXPUnit;

{ ========================= Implementation-Teil =========================  }

implementation

uses xp0;

const rchar : array[1..3,1..6] of char =
              ('⁄ƒø≥¿Ÿ','…Õª∫»º','’Õ∏≥‘æ');
{$ifdef NCRT }
      { LSSize - Gibt die maximale Groesse des LocalScreen-Buffers
        an. (Zeilen * Spalten * (sizeof(Char) + sizeof(Attribut))) }
      CharSize =        1;              { Groesse eines Zeichens }
      AttrSize =        1;              { Groesse eines Attributs }
      LSSize =          $7fff;          { Sollte fuer 160 x 100 reichen }
{$else }
      LSSize = $1fff;
{$endif }
      shad  : byte = 0;  { Zusatz-Fensterbreite/hîhe }

type  { Achtung: hier mu· der komplette Bildschirm mit Attributen reinpassen }
  memarr     = array[0..$1fff] of byte;

  { Speicher den kompletten Bildschirm lokal zwischen, damit beim Auslesen
    des Fensterinhaltes nicht auf API-Funktionen zurÅckgegriffen werden mu·.
    Jede énderung am Bildschirm _mu·_ gleichzeitig hier gemacht werden }
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

  { Wird benutzt, wenn Fenster im Rahmen gefÅllt werden soll }
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
procedure wshadow(li,re,ob,un:word);
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

procedure clwin(l,r,o,u:word);
var
  i: Integer;
begin
  for i := o to u do
    FillScreenLine(l, i, ' ', r-l+1);
end;

{$IFNDEF NCRT }
procedure Wrt(const x,y:word; const s:string);
begin
  FWrt(x, y, s);
  GotoXY(x+Length(s), y);
end; { Wrt }
{$ENDIF }

{$IFDEF DOS32}
type TCoord= record x,y: integer end;
{$ENDIF}

{$IFNDEF NCRT }
procedure FWrt(const x,y:word; const s:string);
var
  {$IFDEF Win32 }
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: DWord;
  {$ENDIF }
  i, Count: Integer;
begin
  {$R-}
  {$IFDEF Win32 }
    { Kompletten String an einem StÅck auf die Console ausgeben }
    WritePos.X := x-1; WritePos.Y := y-1;
    WriteConsoleOutputCharacter(OutHandle, @s[1], Length(s), WritePos, OutRes);
    FillConsoleOutputAttribute(OutHandle, Textattr, Length(s), WritePos, OutRes);
  {$ELSE }
    {$IFDEF DOS32 }
      Count := ((X-1)+(y-1)*screenwidth)*2;
      for i := 0 to Length(s)-1 do
        memw[$B800:Count+i*2]:=(textattr shl 8) or byte(s[i+1]);
    {$ELSE }
      GotoXY(x, y);
      Write(s);
    {$ENDIF }
  {$ENDIF Win32 }

  {$IFDEF Debug }
    {$R+}
  {$ENDIF }

  {$IFDEF Localscreen }
  { LocalScreen Åbernimmt die énderungen }
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

{$IFDEF Win32 }
  procedure consolewrite(x,y:word; num:dword);  { 80  Chars in xp0.charpuf (String) }
  var                                           { Attribute in xp0.attrbuf (Array of smallword)}
    WritePos: TCoord;                           { generiert in XP1.MakeListdisplay }
    OutRes: ULong;                            { Auf Konsole ausgeben....}
  begin
    WritePos.X := x-1; WritePos.Y := y-1;
    WriteConsoleOutputCharacter(OutHandle, @charbuf[1], num, WritePos, OutRes);
    WriteConsoleOutputAttribute(OutHandle, @attrbuf[2], num, WritePos, OutRes);  end;
{$ELSE }
  procedure consolewrite(x,y:word; num:dword);  { Num = Chars in xp0.charpuf (String) }
  var
    i, j: Integer;
  begin
    i := 1;
    while i < num do
    begin
      j := i;
      { Solange suchen, bis im String unterschiedliche Attribute auftauchen }
      while((AttrBuf[i+1] = AttrBuf[j+2]) and (j<num)) do inc(j);

      {$IFDEF VP }
         SysWrtCharStrAtt(@CharBuf[i], j-i+1, x+i-2, y-1, byte(AttrBuf[i+1]));
      {$ELSE VP }
         TextAttr := AttrBuf[i+1];
         FWrt(x+i-1, y, Copy(CharBuf, i, j-i+1));
      {$ENDIF VP }
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
  FillChar(Curinfo, SizeOf(Curinfo), 0);
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

procedure SDisp(const x,y:word; const s:string);
{$IFDEF Win32 }
  var
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: ULong;
  begin
    { Kompletten String an einem StÅck auf die Console ausgeben }
    WritePos.X := x-1; WritePos.Y := y-1;
    WriteConsoleOutputCharacter(OutHandle, @s[1], Length(s), WritePos, OutRes);
    { !! Hier mÅsste noch die Textfarbe verÑndert werden,
      nicht aber der Texthintergrund }
{$ELSE Win32 }
  begin
    FWrt(x, y, s);
{$ENDIF Win32 }
end;

procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);
{$IFDEF Win32 }
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
  var
    w: SmallWord;
  begin
    {$IFDEF VP }
      c := SysReadCharAt(x-1, y-1);
      Attr := SmallWord(SysReadAttributesAt(x-1, y-1));
    {$ENDIF }
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
{$IFDEF Win32 }
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
{$IFDEF Win32 }
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
      {$IFDEF VP }
        TLocalScreen(Buffer)[Offset] := SysReadCharAt(x, y);
        TLocalScreen(Buffer)[Offset+1] := Char(SysReadAttributesAt(x, y));
      {$ELSE }
        {$IFDEF LocalScreen }
          TLocalScreen(Buffer)[Offset] := LocalScreen^[(x+y*ScreenWidth)*2];
          TLocalScreen(Buffer)[Offset+1] := LocalScreen^[(x+y*ScreenWidth)*2+1];
        {$ENDIF }
        {$IFDEF DOS32 }
          TLocalScreen(Buffer)[Offset] := Char(Mem[$B800:(x+y*ScreenWidth)*2]);
          TLocalScreen(Buffer)[Offset+1] := Char(Mem[$B800:(x+y*ScreenWidth)*2+1]);
        {$ENDIF }
      {$ENDIF }
      Inc(Offset, 2);
    end;
{$ENDIF }
end;

procedure WriteScreenRect(const l, r, o, u: Integer; var Buffer);
{$IFDEF Win32 }
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
        {$IFDEF VP }
           SysWrtCharStrAtt(@s[1], Length(s), x-1, y-1, Byte(TLocalScreen(Buffer)[Offset-1]));
        {$ELSE VP }
           TextAttr := SmallWord(Byte(TLocalScreen(Buffer)[Offset-1]));
           FWrt(x, y, s);
        {$ENDIF VP }
        x := j; inc(x);
      end;
    end;
{$ENDIF }
end;

{$IFNDEF NCRT }
procedure Wrt2(const s:string);
begin
  FWrt(WhereX, WhereY, s);
  GotoXY(WhereX+Length(s), WhereY);
end;
{$ENDIF }

{ attr1 = Rahmen/Background; attr2 = Kopf }
procedure explode(l,r,o,u,typ,attr1,attr2: Integer; msec:word; const txt:string);
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
  mwrt(li,m,'Ã'+dup(re-li-1,'Õ')+'π');
end;

procedure setrahmen(n:shortint);
begin
  rahmen:=n;
end;

function getrahmen:shortint;
begin
  getrahmen:=rahmen;
end;


Procedure wpull(x1,x2,y1,y2: Integer; const text:string; var handle:word);
{$IFDEF NCRT }
const
  i: word = 1;
begin
  while (pullw[i].win.wHnd <> nil) do
    Inc(i);
  handle:= i;
  with pullw[i] do begin
    if (rahmen > 0) then
      MakeWindow(win, x1, y1, x2, y2, text, true)
    else
      MakeWindow(win, x1, y1, x2, y2, text, false);
    l:=x1; r:=x2; o:=y1; u:=y2;
    ashad:=shad;
    wi:=(r-l+1+shad)*2;
  end;
end;
{$ELSE }
var
  i : byte;
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
    l:=x1; r:=x2; o:=y1; u:=y2;
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

Procedure wrest(handle:word);
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
var i,j : word;
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


Procedure wslct(anz:integer; ta:pntslcta; handle,pos:word; abs1:boolean;
                var n:word; var brk:boolean);

var z          : taste;
    i,po,pon   : integer;
    wsize      : word;
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


var
  SavedExitProc: pointer;

procedure ExitWinXPUnit;
begin
  ExitProc:= SavedExitProc;
  {$IFDEF Localscreen }
  FreeMem(LocalScreen);
  {$ENDIF }
end;

procedure InitWinXPUnit;
var
  i: byte;
begin
{$IFDEF NCRT }
  FillChar(pullw, sizeof(pullw), 0);
{$ELSE }
  for i:=1 to maxpull do
    pullw[i].free:=true;
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
{$IFDEF Win32 }
  { Consolenhandle holen }
  OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
{$ENDIF }
  SavedExitProc:= ExitProc;
  ExitProc:= @ExitWinXPUnit;
end;

{
  $Log$
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


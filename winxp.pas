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

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$F+}
{$ENDIF }

unit winxp;

{  ==========================  Interface-Teil  ==========================  }

INTERFACE

uses
{$IFDEF Win32 }
  windows, strings,
{$ENDIF }
  xpglobal, crt,dos,keys,inout,maus2,typeform;

const maxpull = 30;
      maxpush = 20;

      crline  : byte = 25;      { zeile fÅr Alt-F10-Copyright }
      shadowcol: byte = 8;

type  selproc = procedure(var sel:slcttyp);

var   wpstack  : array[1..maxpush] of word;
      wpp      : byte;
      warrows  : boolean;     { Pfeile bei wslct anzeigen }
      warrcol  : byte;        { Farbe fÅr Pfeile          }
      selp     : selproc;


procedure normwin;
procedure clwin(l,r,o,u:word);

procedure rahmen1(li,re,ob,un:byte; txt:string);    { Rahmen ≥ zeichen       }
procedure rahmen2(li,re,ob,un:byte; txt:string);    { Rahmen ∫ zeichnen      }
procedure rahmen3(li,re,ob,un:byte; txt:string);    { Special-Rahmen         }
procedure rahmen1d(li,re,ob,m,un:byte; txt:string); { Doppelrahmen ≥ zeichen }
procedure rahmen2d(li,re,ob,m,un:byte; txt:string); { Doppelrahmen ∫ zeichnen}
procedure explode(l,r,o,u,typ,attr1,attr2:byte; msec:word; txt:string);
procedure wshadow(li,re,ob,un:word);                { 8-Schatten }

procedure setrahmen(n:shortint);                 { Rahmenart fÅr wpull setzen }
function  getrahmen:shortint;
procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
procedure wpull(x1,x2,y1,y2:byte; text:string; var handle:word);
procedure wrest(handle:word);
procedure wslct(anz:integer; ta:pntslcta; handle,pos:word; abs1:boolean;
                var n:word; var brk:boolean);
procedure seldummy(var sel:slcttyp);
procedure wpush(x1,x2,y1,y2:byte; text:string);
procedure wpushs(x1,x2,y1,y2:byte; text:string);
procedure wpop;
procedure w_copyrght;

{ Schreiben eines Strings mit Update der Cursor-Posititon }
{ Diese Routine aktualisiert wenn nîtig den LocalScreen }
procedure Wrt(const x,y:word; const s:string);
{ Schreiben eines Strings, wie Write, CursorPosition
  wird aktualisiert }
procedure Wrt2(const s:string);
{ Schreiben eines Strings ohne Update der Cursor-Position
  Der LocalScreen wird wenn nîtig aktualisiert }
procedure FWrt(const x,y:word; const s:string);

{$IFDEF Win32 }
{ Schreiben eines Strings ohne Update der Cursor-Position
  Der Textbackground (nicht die Farbe!) wird nicht verÑndert }
procedure SDisp(const x,y:word; const s:string);
{$ENDIF }

{$IFDEF Ver32 }
{ Routinen fÅr 32 Bit Versionen, die den Zugriff auf den Bildschirm
  managen }

{ Liest ein Zeichen direkt von der Konsole aus
  x und y beginnen mit 1 }
procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);

{ Liest direkt von der Console eine Zeile (nicht Åber Zeilenende) aus
  und speichert das Ergebnis in Buffer. Buffer enthÑlt ein array
  aus Zeichen + Attribut mit je einem Byte. Count gibt die Zeichenzahl an
  Achtung: X und Y beginnen hier bei Koodinate 0 ! }
procedure GetScreenLine(const x, y: Integer; var Buffer; const Count: Integer);

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

{$ENDIF }


{ ========================= Implementation-Teil =========================  }

IMPLEMENTATION

const rchar : array[1..3,1..6] of char =
              ('⁄ƒø≥¿Ÿ','…Õª∫»º','’Õ∏≥‘æ');

      shad  : byte = 0;  { Zusatz-Fensterbreite/hîhe }

type  { Achtung: hier mu· der komplette Bildschirm mit Attributen reinpassen }
  memarr     = array[0..$1fff] of byte;

{$IFDEF Ver32 }
  { Speicher den kompletten Bildschirm lokal zwischen, damit beim Auslesen
    des Fensterinhaltes nicht auf API-Funktionen zurÅckgegriffen werden mu·.
    Jede énderung am Bildschirm _mu·_ gleichzeitig hier gemacht werden }
  TLocalScreen = array[0..$1fff] of char;
var
  LocalScreen: ^TLocalScreen;
{$ENDIF }

var pullw   : array[1..maxpull] of record
                                     l,r,o,u,wi : byte;
                                     ashad      : byte;
                                     savemem    : ^memarr;
                                     free       : boolean;
                                   end;
    rahmen  : shortint;

{$IFDEF Win32 }
    { EnthÑlt das Fensterhandle fÅr die Console }
    OutHandle     : THandle;
{$ENDIF }

{$IFDEF BP }
procedure qrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean); assembler;
asm
         cld
         mov   al,typ
         mov   ah,6
         mul   ah
         mov   bx,offset rchar - 6
         add   bx,ax                   { Offset der Zeichentabbelle [typ] }

         mov   es,base
         mov   al,byte ptr o
         dec   al
         mul   byte ptr zpz
         shl   ax,1                    { di  <-  (o-1) * zpz * 2 }
         mov   di,l
         dec   di
         shl   di,1
         add   di,ax                   { di  <-  di + (l-1) * 2  }
         mov   ax,r
         sub   ax,l
         dec   ax
         mov   dx,ax

         mov   ah,attr
         push  di
         mov   al,[bx+0]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+2]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         mov   si,u
         sub   si,o
         dec   si
         jz    @r2                      { Leerer Mittelteil }

@qrlp1:   push  di
         mov   al,[bx+3]
         stosw
         mov   al,' '
         mov   cx,dx
         cmp   clr,1
         jz    @docl
         add   di,cx
         add   di,cx
         jmp   @nocl
@docl:   rep   stosw
@nocl:   mov   al,[bx+3]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         sub   si,1
         jnz   @qrlp1

@r2:      mov   al,[bx+4]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+5]
         stosw
end;
{$ELSE }
procedure qrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean);
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
{$ENDIF }

{$IFDEF BP }
procedure wshadow(li,re,ob,un:word); assembler;
asm
         call  moff
         mov   ax,un                   { Adresse untere linke Ecke berechnen }
         dec   ax
         mov   bx,zpz
         mul   bx
         shl   ax,1
         mov   di,li
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   es,base
         mov   cx,re
         cmp   cx,bx
         jbe   @c1ok
         mov   cx,bx
@c1ok:    sub   cx,li
         inc   cx
         mov   al,shadowcol

@usloop: stosb                         { unteren Schatten zeichnen }
         inc   di
         loop  @usloop

         mov   ax,ob                   { Adresse obere rechte Ecke berechnen }
         dec   ax
         mul   bx
         shl   ax,1
         mov   di,re
         cmp   di,bx                   { Schattenspalte > 80? }
         ja    @nors
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   cx,un
         sub   cx,ob
         mov   al,shadowcol
         dec   bx

@rsloop: stosb                         { rechten Schatten zeichnen }
         add   di,bx
         add   di,bx
         inc   di
         loop  @rsloop

@nors:   call  mon
end;
{$ELSE }
procedure wshadow(li,re,ob,un:word);
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
{$IFDEF Win32 }
    GetScreenChar(re, i, c, Attr);
{$ELSE }
    c := Char(LocalScreen^[((re-1)+(i-1)*zpz)*2]);
{$ENDIF }
    fwrt(re, i, c);
  end;
  for i := li to re do
  begin
{$IFDEF Win32 }
    GetScreenChar(i, un, c, Attr);
{$ELSE }
    c := LocalScreen^[((i-1)+(un-1)*zpz)*2];
{$ENDIF }
    fwrt(i, un, c);
  end;
  textattr := save;
  mon;
end;
{$ENDIF }

{$IFDEF BP }
procedure clwin(l,r,o,u:word); assembler;
asm
         call   moff
         mov    si,zpz
         shl    si,1
         mov    cx,si
         mov    ax,o
         dec    ax
         mul    cx
         mov    dx,l
         dec    dx
         mov    bx,dx
         shl    dx,1
         add    dx,ax
         mov    di,dx                  { dx, di = Startadresse des Fensters }
         mov    cx,r
         sub    cx,bx
         mov    bx,u
         sub    bx,o
         inc    bx                     { bl = Fensterhîhe }
         mov    bh,cl                  { bh,cx = Fensterbreite }
         mov    es,base
         mov    al,' '
         mov    ah,textattr
@wclo:    or     bl,bl
         jz     @wcende                 { Fenster ist gelîscht }
         cld
         rep    stosw                  { Fensterbereich lîschen mit del }
         mov    cl,bh                  { Fensterbreite holen }
         add    dx,si                  { NÑchste Fensterzeile }
         mov    di,dx
         dec    bl
         jmp    @wclo
@wcende: call   mon
end;
{$ELSE }
procedure clwin(l,r,o,u:word);
var
  i: Integer;
begin
  for i := o to u do
    FillScreenLine(l, i, ' ', r-l+1);
end;
{$ENDIF }

procedure Wrt(const x,y:word; const s:string);
begin
{$IFDEF BP }
  gotoxy(x,y);
  write(s);
{$ELSE }
  FWrt(x, y, s);
  GotoXY(x+Length(s), y);
{$ENDIF }
end;

{$IFDEF BP }
procedure FWrt(const x,y:word; const s:string); assembler;
asm
         push ds
         cld
         mov    es,base
         mov    ax, y
         dec    ax
         mul    zpz
         shl    ax,1
         mov    di,ax
         add    di,x
         add    di,x
         sub    di,2
         mov    ah,textattr
         lds    si,s
         mov    ch,0
         lodsb
         mov    cl,al
         jcxz   @nowrt
@lp:     lodsb
         stosw
         loop   @lp
@nowrt:  pop ds
end;
{$ELSE }
procedure FWrt(const x,y:word; const s:string);
var
{$IFNDEF Win32 }
  i, Count: Integer;
{$ELSE }
  WritePos: TCoord;                       { Upper-left cell to write from }
  Buffer: array[1..255] of smallword;
  OutRes: LongInt;
{$ENDIF }
begin
{$IFDEF Win32 }
  { Kompletten String an einem StÅck auf die Console ausgeben }
  WritePos.X := x-1; WritePos.Y := y-1;
  WriteConsoleOutputCharacter(OutHandle, @s[1], Length(s), WritePos, @OutRes);

  { Die Attribute mÅssen extra geschrieben werden, sie sind fÅr alle
    Zeichen gleich. Da der Buffer ein Word ist, mu· die doppelte LÑnge
    gefÅllt werden }
  FillChar(Buffer, Length(s)*2, Textattr);
  WriteConsoleOutputAttribute(OutHandle, SmallWord(Addr(Buffer)^), Length(s), WritePos, @OutRes);
{$ELSE }
  GotoXY(x, y);
  Write(s);
{$ENDIF }

{$IFNDEF Win32 }
  { LocalScreen Åbernimmt die énderungen }
  if s <> '' then
  begin
    Count := ((x-1)+(y-1)*zpz)*2;
    FillChar(LocalScreen^[Count], Length(s)*2, TextAttr);
    for i := 1 to Length(s) do
    begin
      LocalScreen^[Count] := s[i];
      Inc(Count, 2);
    end;
   end;
{$ENDIF }
end;
{$ENDIF }

{$IFDEF Win32 }
procedure SDisp(x,y:word; const s:string);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: LongInt;
begin
  { Kompletten String an einem StÅck auf die Console ausgeben }
  WritePos.X := x-1; WritePos.Y := y-1;
  WriteConsoleOutputCharacter(OutHandle, @s[1], Length(s), WritePos, @OutRes);
  { !! Hier mÅsste noch die Textfarbe verÑndert werden,
    nicht aber der Texthintergrund }
end;
{$ENDIF }

{$IFDEF Ver32 }
procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: SmallWord);
{$IFDEF Win32 }
var
  ReadPos: TCoord;                       { Upper-left cell to Read from }
  OutRes: LongInt;
  aChr: Char;
  aAttr: SmallWord;
begin
  ReadPos.X := x-1; ReadPos.Y := y-1;
  ReadConsoleOutputCharacter(OutHandle, @aChr, 1, ReadPos, @OutRes);
  ReadConsoleOutputAttribute(OutHandle, @aAttr, 1, ReadPos, @OutRes);
  c := aChr; Attr := aAttr;
{$ELSE }
begin
  { !!Hier LocalScreen abfragen oder direkt aus dem Bildschirm lesen }
{$ENDIF }
end;

procedure GetScreenLine(const x, y: Integer; var Buffer; const Count: Integer);
{$IFDEF Win32 }
var
  ReadPos: TCoord;                       { Upper-left cell to Read from }
  OutRes: LongInt;
  CharBuffer: array[0..1023] of Char;
  AttrBuffer: array[0..1023] of SmallWord;
  i: Integer;
begin
  ReadPos.X := x; ReadPos.Y := y;
  ReadConsoleOutputCharacter(OutHandle, @Char(Addr(CharBuffer)^), Count, ReadPos, @OutRes);
  ReadConsoleOutputAttribute(OutHandle, @SmallWord(Addr(AttrBuffer)^), Count, ReadPos, @OutRes);
  for i := 0 to Count - 1 do
  begin
    TLocalScreen(Buffer)[i*2] := CharBuffer[i];
    TLocalScreen(Buffer)[I*2+1] := Char(Lo(AttrBuffer[i]));
  end;
{$ELSE }
begin
{$ENDIF }
end;

procedure FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer);
{$IFDEF Win32 }
  var
    WritePos: TCoord;                       { Upper-left cell to write from }
    OutRes: LongInt;
  begin
    WritePos.x := x-1; WritePos.y := y-1;
    FillConsoleOutputCharacter(OutHandle, Chr, Count, WritePos, @OutRes);
    FillConsoleOutputAttribute(OutHandle, TextAttr, Count, WritePos, @OutRes)
  end;
{$ELSE }
  var
    i: Integer;
  begin
    GotoXY(x, y);
    Write(Dup(Count, Chr));
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
  ReadConsoleOutput(OutHandle, PChar_Info(@Buffer), BSize, Coord, @SourceRect);
{$ELSE }
begin
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
  WriteConsoleOutput(OutHandle, Char_Info(Buffer), BSize, Coord, @DestRect);
{$ELSE }
begin
{$ENDIF }
end;

{$ENDIF Ver32 }

procedure Wrt2(const s:string);
{$IFNDEF Win32 }
var
  i, Count: Integer;
{$ENDIF }
begin
{$IFDEF Ver32 }
  {$IFDEF Win32 }
    FWrt(WhereX, WhereY, s);
    GotoXY(WhereX+Length(s), WhereY);
  {$ELSE }
  { LocalScreen Åbernimmt die énderungen }
  Write(s);
  Count := ((Wherex-1)+(Wherey-1)*zpz)*2;
  if s <> '' then
    for i := 0 to Length(s)-1 do
    begin
      LocalScreen^[Count+i*2] := s[i+1];
      LocalScreen^[Count+i*2+1] := Char(TextAttr);
    end;
  {$ENDIF }
{$ELSE }
  Write(s);
{$ENDIF }
end;
{ attr1 = Rahmen/Background; attr2 = Kopf }

procedure explode(l,r,o,u,typ,attr1,attr2:byte; msec:word; txt:string);
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
   delay(del);
      end
  else
    for i:=0 to ny do begin
      moff;
      qrahmen(ls-i*nx div ny,rs+i*nx div ny,os-i,us+i,typ,attr1,true);
      mon;
   delay(del);
      end;
  if txt<>'' then begin
    moff;
    la:=lastattr;
    attrtxt(attr1);
    wrt((r+l+1)div 2-length(txt)div 2-2,o,' ');
    attrtxt(attr2); write(' ',txt,' ');
    attrtxt(attr1); write(' ');
    attrtxt(la);
    mon;
    end;
end;

procedure normwin;
begin
  window(1,1,80,25);
end;


procedure rahmen1(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,1,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then
  begin
    wrt((re+li+1)div 2 - length(txt) div 2 - 2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
  end;
  mon;
end;


procedure rahmen2(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,2,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
    end;
  mon;
end;


procedure rahmen3(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,3,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
    end;
  mon;
end;


Procedure rahmen1d(li,re,ob,m,un:byte; txt:string);
begin
  rahmen1(li,re,ob,un,txt);
  mwrt(li,m,hbar(re-li+1));
end;


Procedure rahmen2d(li,re,ob,m,un:byte; txt:string);
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


Procedure wpull(x1,x2,y1,y2:byte; text:string; var handle:word);
var i : byte;
{$IFNDEF Win32 }
  j: Integer;
{$ENDIF }
begin
  if (x2-x1<1) or (y2-y1<1) then begin
    writeln('WPULL error');
    halt(1);
    end;
  savecursor;
  cursor(curoff);
  normwin;
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
{$IFDEF Win32 }
    getmem(savemem,wi*(u-o+ashad+1)*2);
    ReadScreenRect(l, r+ashad, o, u+ashad, SaveMem^);
{$ELSE }
    getmem(savemem,wi*(u-o+ashad+1));
    for j:=o-1 to u-1+ashad do
{$IFDEF BP }
      Fastmove(mem[base:j*zpz*2+(l-1)*2],savemem^[(1+j-o)*wi],wi);
{$ELSE }
      (*  GetScreenLine(l-1, j, savemem^[(1+j-o)*wi], wi div 2); *)
      Fastmove(LocalScreen^[j*zpz*2+(l-1)*2],savemem^[(1+j-o)*wi],wi);
{$ENDIF}
{$ENDIF}
    mon;
    if rahmen=1 then rahmen1(l,r,o,u,text);
    if rahmen=2 then rahmen2(l,r,o,u,text);
    if rahmen>0 then clwin(l+1,r-1,o+1,u-1);
    if rahmen<0 then explode(l,r,o,u,abs(rahmen),normattr,invattr,100,text);
    end;
  restcursor;
end;


Procedure wrest(handle:word);
{$IFNDEF Win32 }
var
  i : byte;
  j, Offset: integer;
{$ENDIF }
begin
  normwin;
  with pullw[handle] do begin
    moff;

{$IFDEF Win32 }
    WriteScreenRect(l, r+ashad, o, u+ashad, SaveMem^);
{$ELSE }
    for i:=o-1 to u-1+ashad do
{$IFDEF BP }
      Fastmove(savemem^[(i-o+1)*wi],mem[base:i*zpz*2+(l-1)*2],wi);
{$ELSE }
      begin
        { in den lokalen Screen kopieren }
        Fastmove(savemem^[(i-o+1)*wi],LocalScreen^[i*zpz*2+(l-1)*2],wi);
        { Offset nur einmal berechnen, beschleunigt das ganze etwas }
        Offset := (i-o+1)*wi;
        for j := 0 to wi div 2 - 1 do
        begin
          TextAttr := savemem^[Offset+1];
          FWrt(l+j, i+1, Char(Savemem^[Offset]));
          Inc(Offset, 2);
        end;
      end;
{$ENDIF }
{$ENDIF }
    mon;
{$IFDEF Win32 }
    freemem(savemem,wi*(u-o+ashad+1)*2);
{$ELSE }
    freemem(savemem,wi*(u-o+ashad+1));
{$ENDIF }
    free:=true;
    end;
end;


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
      if UStr(l^[i].el)>UStr(l^[i+1].el) then begin
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
{$IFNDEF ver32}
  cursor(curoff);
{$ENDIF}
  normwin;
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


procedure w_copyrght;
var z     : taste;
    sd,st : boolean;
{$IFDEF BP }
    buf   : array[0..159] of byte;
{$ENDIF }
begin
  moff;
{$IFDEF BP }
  Fastmove(mem[base:(crline-1)*160],buf,160);
{$ENDIF}
  mon;
  disphard(1,crline,dup(16,'∞')+' (c) by '+pm+' ∞∞∞∞∞∞ Tel. 02632/48651 '+
           dup(16,'∞'));
  sd:=m2d; st:=m2t;
  if datey=25 then m2d:=false;
  if timey=25 then m2t:=false;
  get(z,curoff);
  m2d:=sd; m2t:=st;
  Disp_DT;
  moff;
{$IFDEF BP }
  Fastmove(buf,mem[base:(crline-1)*160],160);
{$ENDIF}
  mon;
end;


procedure wpush(x1,x2,y1,y2:byte; text:string);
var r   : byte;
    tx1 : char;
begin
  if wpp=maxpush then writeln('WPUSH error')
  else begin
    r:=rahmen;
    if (text='*') or (text='-') then begin
      setrahmen(0); text:='';
      tx1:=text[1];
      end
    else
      tx1:=' ';
    inc(wpp);
    wpull(x1,x2,y1,y2,text,wpstack[wpp]);
    if tx1='*' then clwin(x1,x2,y1,y2);
    setrahmen(r);
    end;
end;


procedure wpushs(x1,x2,y1,y2:byte; text:string);
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
  i: byte;
begin
  for i:=1 to maxpull do
    pullw[i].free:=true;
  rahmen:=1;
  fnproc[3,10]:=w_copyrght;
  wpp:=0;
  warrows:=false;
  warrcol:=7;
  selp:=seldummy;
{$IFDEF Ver32 }
  GetMem(LocalScreen, SizeOf(LocalScreen^));
  {$IFDEF Win32 }
    OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ENDIF }
{$ENDIF }
end.
{
  $Log$
  Revision 1.14  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.13  2000/03/20 11:26:21  mk
  - SDisp-Routine teilweise nach Win32 portiert

  Revision 1.12  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.11  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.10  2000/03/08 01:33:15  mk
  - Kopieren von rechteckigen Bildschirmbereichen hinzugefuegt

  Revision 1.8  2000/03/04 22:41:37  mk
  LocalScreen fuer xpme komplett implementiert

  Revision 1.7  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}

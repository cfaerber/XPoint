{ $Id$

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de> 
  see CVS log below for authors

  This file is part of OpenXP/32 and XPLib.

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.
  
  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  As a special exception, the authors give permission for additional uses of
  the text contained in its release of this library. 

  The exception is that, if you link this library with other files to produce
  an executable, this does not by itself cause the resulting executable to be
  covered by the GNU General Public License. Your use of that executable is in
  no way restricted on account of linking this library code into it. 

  This exception does not however invalidate any other reasons why the
  executable file might be covered by the GNU General Public License. 

  This exception applies only to the code released by the authors within this
  library. If you copy code from other Free Software Foundation releases into
  a copy of this library, as the General Public License permits, the exception
  does not apply to the code that you add in this way. To avoid misleading
  anyone as to the status of such modified files, you must delete this
  exception notice from them. 

  If you write modifications of your own for this library, it is your choice
  whether to permit this exception to apply to your modifications.  If you do
  not wish that, delete this exception notice. 
}

{$I xpdefine.inc }

{ @abstract(Unicode Line Break Properties (UTR#14).)
  This unit implements the Unicode Line Break Properties and Algorithm 
  described in Unicode Technical Report #14.
}
unit xpunicode_lbr;

{ ---------------------------} interface { --------------------------- }

uses
  Classes,
  xpunicode;

{ The Line Break Type character property. }
type TUnicodeLineBreakType = (
  // UTF#14 Table
  UNICODE_BREAK_OP, UNICODE_BREAK_CL, UNICODE_BREAK_QU, UNICODE_BREAK_GL,
  UNICODE_BREAK_NS, UNICODE_BREAK_EX, UNICODE_BREAK_SY, UNICODE_BREAK_IS,
  UNICODE_BREAK_PR, UNICODE_BREAK_PO, UNICODE_BREAK_NU, UNICODE_BREAK_AL,
  UNICODE_BREAK_ID, UNICODE_BREAK_IN, UNICODE_BREAK_HY, UNICODE_BREAK_BA,
  UNICODE_BREAK_BB, UNICODE_BREAK_B2, UNICODE_BREAK_ZW, UNICODE_BREAK_CM,
  // Not in Table
  UNICODE_BREAK_CR, UNICODE_BREAK_LF, UNICODE_BREAK_SP, UNICODE_BREAK_BK,
  UNICODE_BREAK_UNKNOWN );

{ Returns the Line Break Type property @link(TUnicodeLineBreakType) 
  of an @link(TUnicodeChar). }  
function UnicodeCharacterLineBreakType(AUnicodeChar: TUnicodeChar): TUnicodeLineBreakType;

{ Determines if a line break is allowed between two Unicode characters. }
function UnicodeLineBreakAllowed(BeforeType, AfterType: TUnicodeLineBreakType; SPInbetween: boolean): boolean;

{ @abstract(Line Breaker)

  This class implements the line breaking algorithm. It supports input
  in both UTF-8 and Single Byte Character Sets (SBCSs).
  
  PLANNED: It also supports soft line breaks, i.e. overriding hard line 
  breaks (CR/LF/CRLF) with a preceeding space (U+0020).  
}  
type TUnicodeLineBreaker = class
  private
    FCodePage: P8BitTable;
    FMaxWidth: integer;
    FTabWidth: integer;
//  FSoftBreaks: boolean;

    Line: string;
    LineWidth: integer;

    CRSeen:      boolean;
    FFSeen:      boolean;

    ShyPos:      integer;
    ShyBefore:   string;
    ShyAfter:    string;

    Start : boolean;

  private
    FSinkObjIsTStream: boolean;
    FSinkObjIsTStrings: boolean;
    
    FSinkObj: TObject;

    procedure FSetSinkObj(AnObject: TObject);

  private
    procedure FAddData(stream_data:TStream;const string_data:string);
    procedure FAddLine(const Line: string);

  public
    { Constructs a @link(TUnicodeLineBreaker). }
    constructor Create;

    { Destroys the @link(TUnicodeLineBreaker). }
    destructor Destroy; override;

    { Sets the breaker to UTF-8 mode. (This is the default.) }
    procedure SetUTF8;
    { Sets the breaker to SBCS mode. }
    procedure SetCodePage(const CodePage: T8BitTable);

    { Maximum width for output in columns (defaults to unlimited). }
    property MaxWidth: integer read FMaxWidth write FMaxWidth;
    { Tabulator width in columns (defaults to 8). }
    property TabWidth: integer read FTabWidth write FTabWidth;
//  property SoftBreaks: boolean read FSoftBreaks write FSoftBreaks;

  public  
    { Reads text from a simple @link(string). No implicit line break
      will be added. }
    procedure AddData(const data:string); overload;
    { Reads text from a @link(TStrings) object. An implicit line break 
      is added to the end of each @link(string). }    
    procedure AddData(data:TStrings); overload;
    { Reads text from a @link(TStream). No implicit line breaks will 
      be added. }
    procedure AddData(data:TStream); overload;

    { Adds a line break if the input data processed so far did not
      end with one. }
    procedure FlushData;

  public
    { Object that reveices the output of the line breaker. 
      This can be a @link(TStream) or @link(TStrings). 
      A @link(TStream) will receive line breaks as CRLF (U+000D, 
      U+000A), a @link(TStrings) will receive no line breaks (they are 
      implicit between the elements). }
    property Sink: TObject read FSinkObj write FSetSinkObj;
  end;

{ ------------------------} implementation { ------------------------- }

uses xpstreams, 
  strutils,
  sysutils;

{$IFDEF UnixFS}
{$I unicode/linebreak.inc }
{$ELSE}
{$I unicode\linebreak.inc }
{$ENDIF}

function UnicodeCharacterLineBreakType(AUnicodeChar: TUnicodeChar): TUnicodeLineBreakType;
var spos,epos,mpos: integer;
begin
  spos := Low(UnicodeLineBreakTypes);
  epos := High(UnicodeLineBreakTypes);
  repeat
    mpos := (spos + epos) div 2; // spos <= mpos <= epos
    with UnicodeLineBreakTypes[mpos] do
      if start > AUnicodeChar then epos := mpos - 1 else
      if stop  < AUnicodeChar then spos := mpos + 1 else
      begin result := line_break_type; exit; end;
  until spos > epos;
  result := UNICODE_BREAK_UNKNOWN;
end;

const UnicodeLineBreakAllowedTable: 
  packed array[boolean  (* space/no spaces *)] of 
  packed array[UNICODE_BREAK_OP..UNICODE_BREAK_CM (* before *)] of
  packed array[UNICODE_BREAK_OP..UNICODE_BREAK_CM (* after  *)] of
  boolean = (
         {  OP     CL     QU     GL     NS     EX     SY     IS     PR     PO     NU     AL     ID     IN     HY     BA     BB     B2     ZW     CM   }
  { OP }(( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ),
  { CL } ( true,  false, false, false, false, false, false, false, true,  false, true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { QU } ( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ),
  { GL } ( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ),
  { NS } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { EX } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { SY } ( true,  false, false, false, false, false, false, false, true,  true,  false, true,  true,  true,  false, false, true,  true,  false, false ),
  { IS } ( true,  false, false, false, false, false, false, false, true,  true,  false, true,  true,  true,  false, false, true,  true,  false, false ),
  { PR } ( false, false, false, false, false, false, false, false, true,  true,  false, false, false, true,  false, false, true,  true,  false, false ),
  { PO } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { NU } ( true,  false, false, false, false, false, false, false, true,  false, false, false, true,  false, false, false, true,  true,  false, false ),
  { AL } ( true,  false, false, false, false, false, false, false, true,  true,  false, false, true,  false, false, false, true,  true,  false, false ),
  { ID } ( true,  false, false, false, false, false, false, false, true,  false, true,  true,  true,  false, false, false, true,  true,  false, false ),
  { IN } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  false, false, false, true,  true,  false, false ),
  { HY } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { BA } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  true,  false, false ),
  { BB } ( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ),
  { B2 } ( true,  false, false, false, false, false, false, false, true,  true,  true,  true,  true,  true,  false, false, true,  false, false, false ),
  { ZW } ( true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, false ),
  { CM } ( true,  false, false, false, false, false, false, false, true,  true,  false, false, true,  false, false, false, true,  true,  false, false )),

  { OP }(( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true  ),
  { CL } ( true,  false, true,  true,  false, false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { QU } ( false, false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { GL } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { NS } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { EX } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { SY } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { IS } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { PR } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { PO } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { NU } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { AL } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { ID } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { IN } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { HY } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { BA } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { BB } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { B2 } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  false, false, true  ),
  { ZW } ( true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  ),
  { CM } ( true,  false, true,  true,  true,  false, false, false, true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  false, true  )));

function UnicodeLineBreakAllowed(BeforeType, AfterType: TUnicodeLineBreakType; SPInbetween: boolean): boolean;
begin
  if (BeforeType in [UNICODE_BREAK_OP..UNICODE_BREAK_CM]) and
     (AfterType  in [UNICODE_BREAK_OP..UNICODE_BREAK_CM]) then
    result := UnicodeLineBreakAllowedTable[SPInbetween,BeforeType,AfterType]
  else
    result := false;
end;

procedure TUnicodeLineBreaker.SetUTF8;
begin
  FCodePage := nil;
end;

procedure TUnicodeLineBreaker.SetCodePage(const CodePage: T8BitTable);
begin
  FCodePage := @CodePage;
end;

constructor TUnicodeLineBreaker.Create;
begin
  Start := true;

  Line := '';
  LineWidth := 0;

  CRSeen   := false;
  FFSeen   := false;

  TabWidth := 8;
  MaxWidth := MAXINT;  

  FCodePage:= nil;
end;

{ Destroys the @link(TUnicodeLineBreaker). }
destructor TUnicodeLineBreaker.Destroy;
begin
  FlushData;
end;

procedure TUnicodeLineBreaker.FSetSinkObj(AnObject: TObject);
begin
  FSinkObj := AnObject;

  FSinkObjIsTStream  := AnObject is TStream;
  FSinkObjIsTStrings := AnObject is TStrings;
end;

procedure TUnicodeLineBreaker.FAddLine(const Line: string);
begin
  if FSinkObjIsTStream then
    writeln_s(TStream(FSinkObj),Line)
  else
  if FSinkObjIsTStrings then
    TStrings(FSinkObj).Add(Line);
end;

procedure TUnicodeLineBreaker.AddData(const data:string);
begin
  FAddData(nil,data);
end;

procedure TUnicodeLineBreaker.AddData(data:TStrings);
var i: integer;
begin
  for i := 0 to data.Count-1 do
    FAddData(nil,data[i]+#10);
end;

procedure TUnicodeLineBreaker.AddData(data:TStream);
begin
  FAddData(data,'');
end;

procedure TUnicodeLineBreaker.FlushData;
begin
  if Length(Line)>0 then
    FAddData(nil,#10);
end;

procedure TUnicodeLineBreaker.FAddData(stream_data:TStream;const string_data:string);
var
  s: string;

  Buffer: string;  // data
  BufPos: integer; // position of next byte to read

  st_eof: boolean;
  sd_used: boolean;

  c: TUnicodeChar;
  w,i: integer;
  t: TUnicodeLineBreakType;

  function FUTF8Mode: Boolean;
  begin
    result := not assigned(FCodePage);
  end;

  function GetChar(const AString: string; Position: integer): TUnicodeChar;
  begin
    if FUTF8Mode then
      result := xpunicode.UTF8GetChar(AString,Position)
    else
      result := FCodePage^[AString[Position]];
  end;

  function PrevChar(const AString: string; var Position: integer): boolean;
  begin
    if FUTF8Mode then
      result := xpunicode.UTF8PrevChar(AString,Position)
    else begin
      result := Position > 1;
      dec(Position);
    end;
  end;

  function delete_at_end(c: TUnicodeChar): boolean;
  begin
    case c of
    $2000, { EN QUAD }
    $2001, { EM QUAD }
    $2002, { EN QUAD }
    $2003, { EM QUAD }
    $2004, { THREE-PER-EM SPACE }
    $2005, { FOUR-PER-EM SPACE }
    $2006, { SIX-PER-EM SPACE }
    $2008, { PUNCTUATION SPACE }
    $2009, { THIN SPACE }
    $200A, { HAIR SPACE }
    $1680: { OGHAM SPACE MARK }
      result := true;
    else 
      result := false;
    end;
  end;

  function replace_by_hypen(c: TUnicodeChar): boolean;
  begin
    case c of
    $2027, { HYPHENATION POINT }
    $007C: { VERTICAL LINE }
      result := true;
    else 
      result := false;
    end;
  end;

  procedure rb;
  const BufLen = 8192;
  var Len,Rd: integer;

    procedure mk_room;
    begin
      if BufPos >= 1 then begin Delete(Buffer,1,BufPos-1); BufPos := 1; end;
    end;
  
  begin
    if assigned(stream_data) then
    begin
      if ((Length(Buffer)-BufPos+1) <= 5) and (not st_eof) then 
      begin
        mk_room;        
        Len := Length(Buffer); if Len >= BufLen then exit;
        SetLength(Buffer,BufLen);
        Rd := stream_data.Read(Buffer[Len+1],Length(Buffer)-Len);
        if Rd<=0 then st_eof := true;
        SetLength(Buffer,Len+Rd);
      end;
    end else
    begin
      if not sd_used then 
      begin
        mk_room;
        Buffer := Buffer + string_data;
        sd_used := true;
      end;
    end;  
  end;

  function rc(var c: TUnicodeChar; var data: string; var width: integer): boolean;
  var op : integer;
  begin
    rb;
    if BufPos > Length(Buffer) then begin result := false; exit; end;
    if Start then begin
      if (not FUTF8Mode) and (LeftStr(Buffer,3) = #$EF#$BB#$BF) then
      begin
        inc(BufPos,3);
        FCodePage := nil;
      end;
      Start := false;
    end;
    
    if FUTF8Mode then
    begin
      op := BufPos;
      c := UTF8GetCharNext(Buffer,BufPos);
      width := UnicodeCharacterWidth(c);
      data := Copy(Buffer,op,BufPos-op);      
    end else
    begin
      c := FCodePage^[Buffer[BufPos]];
      width := UnicodeCharacterWidth(c);
      data := Buffer[BufPos];
      Inc(BufPos);
    end;

    if width < 0 then width := 0;
    result := true;
  end;

  procedure putline(breakat: integer; shy, wrap: boolean);
  var i: integer;
      c: TUnicodeChar;
      n: string;
  begin
    if (breakat > 0) and (breakat < Length(Line)) then
    begin
      if Shy then 
        n := ShyAfter+Copy(Line,breakat+1,MaxInt)
      else
        n := Copy(Line,breakat+1,MaxInt);
      SetLength(Line,BreakAt);
      if shy then Line := Line + ShyBefore;
    end else
      n := '';
  
    i := Length(Line)+1;
    while PrevChar(Line,i) do
    begin
      c := GetChar(Line,i);
      
      if delete_at_end(c) then
      begin
        SetLength(Line,i-1);
        continue;
      end else

      if wrap and replace_by_hypen(c) then
      begin
        SetLength(Line,i);
        Line[i] := '-';
      end;

      break;
    end;

    FAddLine(Line);
    
    Line := n;

    if FUTF8Mode then
      LineWidth := UTF8StringWidth(n)
    else
      LineWidth := Length(n);

    ShyPos := 0;
    ShyBefore := '';
    ShyAfter := '';
  end;

  procedure putchar;
  var pc: TUnicodeChar;
      pt: TUnicodeLineBreakType;
      pi: Integer;
      
      lt: TUnicodeLineBreakType;
      li: Integer;      

      sp: boolean;
  begin
  
    // This character does not fit into current line
    if (LineWidth + w > MaxWidth) and (t<>UNICODE_BREAK_SP) then 
    begin
      pi := Length(Line)+1;
      lt := t;
      li := pi;
      sp := false;

      repeat
        // no more characters => emergency break
        if not PrevChar(Line,pi) then
        begin
          putline(0,false,false);
          break;
        end;

        // handle soft hyphens
        if pi<=ShyPos then
        begin
          putline(ShyPos,true,false);
          break;
        end;

        pc := GetChar(Line,pi);
        pt := UnicodeCharacterLineBreakType(pc);

        if (pt = UNICODE_BREAK_SP) then
        begin
          if pi <= 1 then
          begin
            putline(0,false,false);
            break;
          end;
          sp := true;
          continue;
        end else

        if UnicodeLineBreakAllowed(pt,lt,SP) then
        begin
          putline(li-1,false,true);
          break;  
        end else
        begin
          lt := pt;
          li := pi;
          sp := false;
        end;
      until false;
    end;
    
    Line := Line + s;
    LineWidth := LineWidth + w;
  end;

begin
  bufpos := 1;
  sd_used := false;
  st_eof := false;
    
    while rc(c,s,w) do begin
      case c of
      // TAB: (tabwidth: 8)
        9:  begin
              i := 8 - (LineWidth mod 8);
              SetLength(Line,Length(Line)+i);
              Inc(LineWidth,i);
              repeat
                Dec(i);
                Line[Length(Line)-i] := ' ';
              until i<=0;
            end;
            
      // FF: handled specially: force line break before and after -- but 
      //     only if there is not already one
        $000C:
            begin
              if Line <> '' then 
                putline(0,false,false);
              putchar;
              putline(0,false,false);
              FFSeen := true;
              continue;
            end;

      // SHY: remember (if a hypen fits into the current line)
        $00AD:
            if (LineWidth+1) <= MaxWidth then 
            begin
              ShyPos := Length(Line);
              ShyBefore := s;
              ShyAfter := '';
            end;

      // MONGOLIAN TODO SOFT HYPHEN: remember
        $1806: begin
              ShyPos := Length(Line);
              ShyBefore := '';
              ShyAfter := s;
            end;
            
      else 
        begin
          t := UnicodeCharacterLineBreakType(c);
          case t of 
            UNICODE_BREAK_LF: if not (CRSeen or FFSeen) then putline(0,false,false);
            UNICODE_BREAK_CR: begin if not (FFSeen) then putline(0,false,false); CRSeen := true; continue; end;
            UNICODE_BREAK_BK: begin putchar; putline(0,false,false); continue; end;
            else putchar;
          end; // case UnicodeLineBreakTypes(c) 
        end;
      end; // case c
      CRSeen := false;
      FFSeen := false;
      Start := false;
    end; // while
end;


//
// $Log$
// Revision 1.3  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
// Revision 1.2  2003/08/23 23:02:38  mk
// - removed hints and warnings
//
// Revision 1.1  2003/08/05 23:34:34  cl
// - xpunicode_linebreak was too long unit name for FPC/1.0.6 on Linux
//
// Revision 1.4  2003/04/27 22:24:33  cl
// - TUnicodeLineBreaker.FlushData added
// - TUnicodeLineBreaker.Destroy calls FlushData
//
// Revision 1.3  2003/03/16 18:55:27  cl
// - started PasDoc documentation
//
// Revision 1.2  2003/02/15 21:47:39  cl
// - FPC/Linux compile fixes (FPC has problems with mixed-case filenames)
//
// Revision 1.1  2003/02/13 14:27:11  cl
// - Unicode support library:
//   . character width
//   . character line breaking properties/line breaking library
//   . UTF8 functions
//
end.

{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC}

unit lister;

interface

uses
  xpglobal,
  {$IFDEF NCRT }
  xpcurses,
  {$ELSE }
  crt,
  {$ENDIF }
  typeform,
  sysutils, classes,
  fileio, inout, maus2, keys, winxp, resource;

const
  ListHelpStr: string[8] = 'Hilfe';
  Listunvers: byte = 0;
  Listhalten: byte = 0;
  Listflags: longint = 0;
  ListerBufferCount = 16383;            { LÑnge des Eingangspuffers }

const
  mcursor: boolean = false;             { Auswahlcursor fÅr Blinde }

type
  listcol = packed record
    coltext,                            { normaler Text           }
    colselbar,                          { Balken bei Auswahlliste }
    colmarkline,                        { markierte Zeile         }
    colmarkbar,                         { Balken auf mark. Zeile  }
    colfound,                           { Suchergebnis            }
    colstatus,                          { Statuszeile             }
    colscroll,                          { Scroller                }
    colhigh,                            { *hervorgehoben*         }
    colqhigh: byte;                     { Quote / *hervorgehoben* }
  end;

  TLister = class;

  TListerConvertEvent = procedure(var buf; Size: word) of object; { fÅr Zeichensatzkonvert. }
  TListerTestMarkEvent = function(var s: string; block: boolean): boolean;
  TListerEnterEvent = procedure(const s: string);
  TListerKeyPressedEvent = procedure(Self: TLister; var t: taste);
  TListerShowLinesEvent = procedure(s: string);
  TListerDisplayLineEvent = procedure(x, y: word; var s: string);
  TListerColorEvent = function(var s: string; line: longint): byte;

  { mîgliche Optionen fÅr den Lister                             }
  {                                                              }
  { SB  =  SelBar                  M   =  markable               }
  { F1  =  "F1-Hilfe"              S   =  Suchen mîglich         }
  { NS  =  NoStatus                NA  =  ^A nicht mîglich       }
  { CR  =  mit Enter beendbar      MS  =  SelBar umschaltbar     }
  { NLR =  kein l/r-Scrolling      APGD=  immer komplettes PgDn  }
  {                                DM  =  direkte Mausauswahl    }
  { VSC =  vertikaler Scrollbar    ROT =  Taste ^R aktivieren    }

  liststat = packed record
    statline: boolean;
    wrapmode: boolean;
    markable: boolean;                  { markieren mîglich   }
    endoncr: boolean;                   { Ende mit <cr>       }
    helpinfo: boolean;                  { F1=Hilfe            }
    wrappos: byte;
    noshift: boolean;                   { kein links/rechts-Scrolling }
    markswitch: boolean;                { SelBar umschaltbar  }
    maysearch: boolean;                 { Suchen mîglich      }
    noctrla: boolean;                   { ^A nicht mîglich    }
    AllPgDn: boolean;                   { immer komplettes PgDn }
    directmaus: boolean;                { Enter bei Maus-Auswahl }
    vscroll: boolean;                   { vertikaler Scrollbar   }
    scrollx: byte;
    rot13enable: boolean;               { ^R mîglich }
    autoscroll: boolean;
  end;

  listarr = record                      { Pfeile }
    usearrows: boolean;
    x, y1, y2: byte;
    arrowattr: byte;
    backattr: byte;
    backchr: char;
  end;

  ListerCharArray = array[0..ListerBufferCount] of Char;

  TLister = class
  protected
    FOnConvert: TListerConvertEvent;
    FOnTestMark: TListerTestMarkEvent;
    FOnEnter: TListerEnterEvent;
    FOnKeypressed: TListerKeyPressedEvent;
    FOnShowLines: TListerShowLinesEvent;
    FOnDisplayLine: TListerDisplayLineEvent;
    FOnColor: TListerColorEvent;

    FStartPos: Integer;                 // first position of select bar
    FHeaderText: string;                // text of header, max 40 chars
    FSelBar: boolean;                   // display select bar
    FSelLine: Integer;                  // actual selected line
    FSelCount: Integer;                 // Number of selected lines
    FLinePos: integer;

    arrows: listarr;
    l, o, w,
    Height: Integer;                    // Height including status line
    Lines: TStringList;
    markpos: integer;
    // test if line Index is marked
    function Marked(Index: Integer): boolean;
    procedure SetHeaderText(s: String);
    function make_list(var buf: ListerCharArray; BufLen: Integer; wrap: byte): Integer;
  public
    col: listcol;
    stat: liststat;
    constructor Create;
    constructor CreateWithOptions(_l, _r, _o, _u: byte; statpos: shortint; options: string);
    destructor Destroy; override;
    // should modified to uses properties
    procedure SetSize(_l, _r, _o, _u: byte);
    // append one line
    procedure AddLine(Line: String);
    // read file from Ofs into Lines of Lister
    procedure ReadFromFile(const Filename: string; ofs: Integer);
    // Show Lister, Result is true if ESC was pressed
    function Show: Boolean;
    procedure SetArrows(x, y1, y2, acol, bcol: byte; backchr: char);
    function FirstMarked: string;
    function NextMarked: string;
    function GetSelection: string;
    function FirstLine: string;
    function NextLine: string;
    function PrevLine: string;
    property StartPos: Integer read FStartPos write FStartPos;
    property HeaderText: String read FHeaderText write SetHeaderText;
    property SelCount: Integer read FSelCount;
    property SelLine: Integer read FSelLine;
    property SelBar: Boolean read FSelBar;
    property LinePos: Integer read FLinePos;
    property OnConvert: TListerConvertEvent read FOnConvert write FOnConvert;
    property OnTestMark: TListerTestMarkEvent read FOnTestMark write FOnTestMark;
    property OnEnter: TListerEnterEvent read FOnEnter write FOnEnter;
    property OnKeyPressed: TListerKeyPressedEvent read FOnKeyPressed write FOnKeyPressed;
    property OnShowLines: TListerShowLinesEvent read FOnShowLines write FOnShowLines;
    property OnDisplayLine: TListerDisplayLineEvent read FOnDisplayline write FOnDisplayLine;
    property OnColor: TListerColorEvent read FOnColor write FOnColor;
  end;

var
  ListColors: ListCol;
  LastLister: TLister; // points to the last opened lister (for historical reasons)

implementation { ------------------------------------------------ }

uses
  GPLTools;

// Zerlegen des Buffers in einzelne Zeilen

function TLister.make_list(var buf: ListerCharArray; BufLen: Integer; wrap: byte):
  Integer;
var
  i, j: Integer;
  s: string;
  MaxLen: Integer;                      // Maximale Leselaenge
  TempIJ: Integer;                      // Temporaer i + j in der Hauptschleife
begin
  Result := 0;
  if BufLen = 0 then exit;
  if wrap = 0 then wrap := 255;
  Wrap := Min(Wrap, 255); // Begrenzung der ZeilenlÑnge auf 255 Zeichen
  j := 0;
  while j < BufLen do
  begin
    i := 0;

    // vorher auswerten, um Rechenzeit in der Hauptschleife zu sparen
    MaxLen := Min(Wrap + j, BufLen);
    TempIJ := j;

    // solange, bis entweder Zeilenende, Wrap-Bereich Åberschritten,
    // LÑnge des Buffers erreicht oder maximale ZeilenÑnge erreicht
    while (buf[TempIJ] <> #13) and (buf[TempIJ] <> #10) and (TempIJ < MaxLen) do
      inc(TempIJ);
    i := TempIJ - j;                    // i = lÑnge der neuen Zeile

    // Spezialbehandlung, wenn Zeile umgebrochen werden mu·
    if i = wrap then
    begin
      i := wrap;
      while (i > 20) and (Buf[i + j] <> ' ') do
        dec(i);
      // wenn keine Umbruchstelle gefunden wurde, Wrap-LÑnge Åbernehmen
      // ansonsten Leerzeichen Åberspringen
      if i = 20 then
        i := wrap
      else
        inc(i);
    end;

    // wenn kein Zeilenende gefunden wurde, aber Puffer alle ist
    if i + j = BufLen then
    begin
      Result := i;
      Move(Buf[j], Buf[0], i);
      Exit;
    end
    else
    begin
      SetLength(s, i);
      if i > 0 then
        Move(Buf[j], S[1], i);
      AddLine(s);
    end;
    // CRLF Åberlesen
    if Buf[i + j] = #13 then inc(i);
    if Buf[i + j] = #10 then inc(i);
    inc(j, i);
  end;
end;

constructor TLister.Create;
begin
  LastLister := Self;
  col := ListColors;
  fillchar(stat, sizeof(stat), 0);
  with stat do
  begin
    {: txt:='';
       wrapmode:=false; markable:=false;
       endoncr:=false;
       wrappos:=0;       :}
    statline := true;
    helpinfo := true;
  end;
  FOnConvert := nil;
  FOnTestMark := nil;
  FOnEnter := nil;
  FOnKeypressed := nil;
  FOnShowLines := nil;
  FOnDisplayLine := nil;
  FOnColor := nil;

  Lines := TStringList.Create;
end;

constructor TLister.CreateWithOptions(_l, _r, _o, _u: byte; statpos: shortint; options: string);
begin
  Create;
  SetSize(_l, _r, _o, _u);
  options := UpperCase(options);
  Fselbar := pos('/SB/', options) > 0;
  stat.markable := pos('/M/', options) > 0;
  stat.endoncr := pos('/CR/', options) > 0;
  stat.helpinfo := pos('/F1/', options) > 0;
  stat.statline := (statpos > 0) and (pos('/NS/', options) = 0);
  stat.noshift := pos('/NLR/', options) > 0;
  stat.markswitch := pos('/MS/', options) > 0;
  stat.maysearch := pos('/S/', options) > 0;
  stat.noctrla := pos('/NA', options) > 0;
  stat.allpgdn := pos('/APGD/', options) > 0;
  stat.directmaus := pos('/DM/', options) > 0;
  stat.vscroll := pos('/VSC:', options) > 0;
  stat.rot13enable := pos('/ROT/', options) > 0;
  if stat.vscroll then
    stat.scrollx := ival(copy(options, pos('/VSC:', options) + 5, 3));
  stat.autoscroll := true;
  startpos := 0;
end;

destructor TLister.Destroy;
begin
  Lines.Free;
  inherited destroy;
end;

function TLister.Marked(Index: Integer): boolean;
begin
  Result := Lines.Objects[Index] <> Pointer(0);
end;

procedure TLister.SetSize(_l, _r, _o, _u: byte);
begin
  l := _l; o := _o;
  w := _r - _l + 1; Height := _u - _o + 1;
end;

procedure TLister.AddLine(Line: String);
var
  p: integer;
begin
  if Line = #13 then exit;             // ignore one CR
  p := pos(#9, Line);
  while p > 0 do
  begin
    delete(Line, p, 1);
    insert(sp(8 - (p - 1) mod 8), Line, p);
    p := pos(#9, Line);
  end;
  // add one line, not marked
  Lines.AddObject(Line, pointer(0));
end;

procedure TLister.ReadFromFile(const Filename: string; ofs: Integer);
var
  f: file;
  s: string;
  p: ^ListerCharArray;
  ps: word;
  rp, TempRP: Integer;
  rr: word;
  fm: byte;
begin
  FHeaderText := fitpath(FileUpperCase(FileName), 40);
  assign(f, FileName);
  fm := filemode; filemode := 0;
  reset(f, 1);
  filemode := fm;
  ps := ListerBufferCount;
  getmem(p, ps);
  rp := 0; rr := 0;
  if ioresult = 0 then
  begin
    seek(f, ofs);
    repeat
      blockread(f, p^[rp], ps - rp, rr);
      if Assigned(FOnConvert) and (rr > 0) then FOnConvert(p^[rp], rr);
      TempRP := rp;
      rp := make_list(p^, rr + rp, stat.wrappos);
    until eof(f);
    close(f);
    if rp > 0 then
    begin { den Rest der letzten Zeile noch anhÑngen.. }
      SetLength(s, rp);
      Move(p^[0], s[1], rp);
      AddLine(s);
    end;
    // Sonderbehandlung fÅr die letzte Leerzeile
//    if p^[rr + TempRP - 1] = #10 then AddLine('');
  end;
  freemem(p, ps);
end;

function TLister.Show: Boolean;
const
  suchstr: string = '';
  suchcase: boolean = false;            { true -> Case-sensitiv }
var
  DispLines: Integer; // Screenlines to Display (List.Height - Statusline)
  y: integer;
  xa: byte;
  t: taste;
  i: longint;
  s: string;
  FirstLine: integer;                   // number of first line in display
  f7p, f8p: longint;
  suchline: longint;                    { Zeilennr.           }
  spos, slen: integer;                  { Such-Position/LÑnge }

  mzo, mzu: boolean;
  mzl, mzr: boolean;
  mb: boolean;                          { Merker fÅr Inout.AutoBremse }
  vstart,
    vstop: integer;                     { Scrollbutton-Position }
  _unit: longint;
  scrolling: boolean;
  scrolladd: integer;
  scrollpos: integer;
  mausdown: boolean;                    { Maus innerhalb des Fensters gedrÅckt }

  procedure showstat;
  begin
    if stat.statline then
    begin
      moff;
      attrtxt(col.colstatus);
      gotoxy(l, o);
(*      Write(' SelLine: ', SelLine: 3, ' xa: ', xa: 3, ' FirstLine: ',
        FirstLine: 3, ' lines.count: ', lines.count: 5, ' SelCount: ', SelCount: 3, DispLines: 3);*)

      Write(FirstLine+1:5,lines.count-1:6);
      if xa=1 then
        Wrt2('        ')
      else
        Wrt2(RightStr('     +'+strs(xa-1),5)+'   ');
{    if (a=0) and more then write(#31)
      else if (a+gl>=lines.count) and (a>0) then write(#30)
      else write(' '); }
      Wrt2(iifs(listhalten=0,' ',iifs(listhalten=1,'+','-')));
      if (listunvers=0) and (listflags=0) then Wrt2('  ')
      else begin
        if listunvers and 16 = 0
          then Wrt2(iifs(listunvers and 1 = 0,' ','!'))
          else Wrt2(iifs(listunvers and 1 = 0,'*',''));
        if listflags and 3=1 then Wrt2('S')
        else if listflags and 3=2 then Wrt2('s')
        else write (iifs(listunvers and 8 = 8,'w',iifs(listunvers and 4=4,'c',' ')));
        end;
      if SelCount>0 then Wrt2('  ['+forms(strs(SelCount)+']',7))
      else if stat.helpinfo then Wrt2(' F1-' + ListHelpStr);
      mon;
    end;
    disp_DT;
  end;

  procedure display;
  var
    i: integer;
    s: string;
    b: byte;
  begin
    i := 0;
    moff;
    while (i < DispLines) and (FirstLine + i < Lines.Count) do
    begin
      s := Lines[FirstLine + i];
      if selbar and (FirstLine + i = FSelLine) then
        if marked(FirstLine + i) then
          attrtxt(col.colmarkbar)
        else
          attrtxt(col.colselbar)
      else
        if marked(FirstLine + i) then
        attrtxt(col.colmarkline)
      else
        if Assigned(FOnColor) then
        begin
          b := FOnColor(s, FirstLine + i);
          if b = 0 then
            b := col.coltext
          else
            if b = $FF then
            b := (col.coltext and $F0) + (col.coltext shr 4);
          attrtxt(b);
        end
        else
          attrtxt(col.coltext);

      if xa = 1 then
        s := forms(s, w)
      else
        s := forms(Mid(s, xa), w);
      if Assigned(FOnDisplayLine) then
        FOnDisplayLine(l, y + i, s)
      else
        FWrt(l, y + i, s);
      if (FirstLine + i = suchline) and (slen > 0) and (spos >= xa) and (spos
        <= xa + w - slen) then
      begin
        attrtxt(col.colfound);
        wrt(l + spos - xa, y + i, copy(s, spos - xa + 1, slen));
      end;
      inc(i);
    end;
    attrtxt(col.coltext);

    // clear rest of screen if not enough lines to display
    if i < DispLines then clwin(l, l + w - 1, y + i, y + DispLines - 1);
    mon;

    if stat.vscroll then
    begin
      attrtxt(col.colscroll);
      maus_showVscroller(true, false, stat.scrollx, y, y + DispLines - 1,
        lines.count + 1, FirstLine + 1, DispLines,
        vstart, vstop, _unit);
    end;
    with arrows do
      if usearrows then
      begin
        if FirstLine = 0 then
        begin
          attrtxt(backattr);
          mwrt(x, y1, backchr);
        end
        else
        begin
          attrtxt(arrowattr);
          mwrt(x, y1, #30);
        end;
        if FirstLine + DispLines >= lines.count then
        begin
          attrtxt(backattr);
          mwrt(x, y2, backchr);
        end
        else
        begin
          attrtxt(arrowattr);
          mwrt(x, y2, #31);
        end;
      end;
  end;

  procedure clearmark;
  var
    i: Integer;
  begin
    for i := 0 to Lines.Count - 1 do
      Lines.Objects[i] := Pointer(0);
    FSelCount := 0;
  end;

  procedure setmark;
  var
    n: Integer;
    s: string;
  begin
    Clearmark;
    for n := f7p to f8p do
    begin
      s := Lines[n];
      if Assigned(FOnTestMark) then
        if FOnTestMark(s, true) then
          Lines.Objects[n] := Pointer(1)
      else
        Lines.Objects[n] := Pointer(1)
    end;
    FSelCount := f8p - f7p;
  end;

  procedure suchen(rep: boolean);
  var
    found, brk: boolean;
    pp: byte;
    i: longint;
    sw: byte;
    nftxt: atext;
    mi: byte;
  begin
     attrtxt(col.colstatus);
     sw:=min(40,w-11);
     nftxt:=sp(w);
     mwrt(l,y+DispLines-1,nftxt);
     if not rep then
     begin
       mi:=invattr; invattr:=$70;
       rdedtrunc:=false;
       ld(l,y+DispLines-1, GetRes2(11,23),suchstr,sw,1,true,brk);
       rdedtrunc:=true;
       invattr:=mi;
       SuchLine := 0;
       Slen := 0;
     end
     else
     begin
       brk:=false;
       mwrt(l,y+DispLines-1,GetRes2(11,24));
     end;

     if brk or (suchstr='') then
     begin
       slen:=0; spos:=1;
       display;
     end
     else
     begin
       found:=false;
       while not found and (SuchLine<Lines.Count) do
       begin
         if suchcase then
           pp:=pos(suchstr,mid(Lines[SuchLine],spos))
         else
           pp:=pos(UpperCase(suchstr),UpperCase(Mid(Lines[SuchLine],spos)));
         if pp=0 then
         begin
           Inc(SuchLine);
           spos:=1;
         end else
         begin
           inc(spos,pp);
           slen:=length(suchstr);
           found:=true;
         end;
       end;
       if not found then
       begin
         attrtxt(col.colstatus);
         mwrt(l,y+DispLines-1,center(GetRes2(11,25),w-1));
         slen:=0;
       end
       else
       begin
          if FirstLine + DispLines - 1 < SuchLine then Inc(FirstLine);
//         if SuchLine > FirstLine + DispLines then FirstLine := SuchLine;
         while spos<xa do dec(xa,10);
         while spos+slen>xa+w-1 do inc(xa,10);
       end;
    end;
    FreeRes;
  end;

  procedure listrot13;
  var
    i: Integer;
    s: string;
  begin
    for i := 0 to Lines.Count - 1 do
      Lines[i] := DecodeRot13String(Lines[i]);
  end;

  (*  procedure Maus_bearbeiten;
    const plm : boolean = true;
    var xx,yy,i : integer;
        inside  : boolean;
        nope    : boolean;
        oldmark : boolean;

      procedure back;
      begin
        if FirstLine^.prev<>nil then begin
          FirstLine:=FirstLine^.prev; pl:=pl^.prev;
          dec(a);
          end
        else
          nope:=true;
      end;

      procedure forth;
      begin
        if pl^.next<>nil then begin
          inc(a);
          FirstLine:=FirstLine^.next; pl:=pl^.next;
          end
        else
          nope:=true;
      end;

      procedure scroll;
      var _start,_stop  : integer;
          i,dummy       : longint;
          up,down,_down : boolean;
          ma            : word;
      begin
        _down:=(yy>scrollpos);
        yy:=minmax(yy,y+scrolladd,y+gl-1-(vstop-vstart-scrolladd));
        ma:=a;
        while yy<scrollpos do begin
          for i:=1 to _unit do back;
          dec(scrollpos);
          end;
        while yy>scrollpos do begin
          for i:=1 to _unit do forth;
          inc(scrollpos);
          end;
        repeat
          maus_showVscroller(false,false,0,y,y+gl-1,alist^.lines+1,a+1,gl,
                             _start,_stop,dummy);
          nope:=false;
          up:=(yy<_start+scrolladd) or ((yy-scrolladd=y) and (FirstLine^.prev<>nil));
          down:=(yy>_start+scrolladd);
          if up then back
          else if down then forth;
        until not (up or down) or nope;
        if _down and (a=ma) then    { Korrektur am Textende }
          while a+gl<alist^.lines do begin
            FirstLine:=FirstLine^.next; pl:=pl^.next;
            inc(a);
            end;
      end;

    begin
      maus_gettext(xx,yy);
      with alist^ do
        if scrolling then begin
          if t=mausunleft then
            scrolling:=false
          else if t=mauslmoved then
            Scroll;
          end
        else begin
          inside:=(xx>=l) and (xx<l+w) and (yy>=y) and (yy<y+gl);
          if t=mausmoved then begin
            if stat.autoscroll and (lines>gl) and
               (not stat.vscroll or (stat.scrollx<>xx)) then
              if yy<=y then AutoUp:=true
              else if yy>=y+gl-1 then AutoDown:=true;
            end
          else if t=mausunright then
            t:=keyesc
          else if (t=mausleft) or (t=mausldouble) or (t=mauslmoved) then begin
            if inside and (stat.markswitch or selbar) then begin
              mausdown:=true;
              if not selbar then begin
                selbar:=true; stat.markable:=true;
                end;
              pl:=FirstLine;
              p:=1;
              for i:=1 to yy-y do
                if pl^.next<>nil then begin
                  pl:=pl^.next; inc(p);
                  end;
              if stat.markable and testmark(pl^.cont,false) then begin
                oldmark:=pl^.marked;
                if t=mauslmoved then
                  pl^.marked:=plm
                else begin
                  pl^.marked:=not pl^.marked;
                  plm:=pl^.marked;
                  end;
                if oldmark and not pl^.marked then dec(markanz) else
                if not oldmark and pl^.marked then inc(markanz);
                end;
              end
            else if ((t=mausleft) or (t=mausldouble)) and
                    (xx=stat.scrollx) and (yy>=y) and (yy<=y+gl) then
              if yy<vstart then
                t:=keypgup
              else if yy>vstop then
                t:=keypgdn
              else begin
                scrolling:=true;
                scrolladd:=yy-vstart;
                scrollpos:=yy;
                end;
            end
          else if (t=mausunleft) and inside then begin
            if stat.directmaus and mausdown then
              t:=keycr;
              mausdown:=false;
              end;
          end;
    end; *)

begin
  startpos := minmax(startpos, 0, Lines.Count - 1);
  DispLines := Height - iif(stat.statline, 1, 0);
  if startpos > DispLines then
  begin
    FirstLine := startpos; FSelLine := StartPos;
  end
  else
  begin
    FirstLine := 0; FSelLine := Startpos;
  end;
  xa := 1;
  y := o + iif(stat.statline, 1, 0);
  if stat.statline then
  begin
    attrtxt(col.colstatus);
    mwrt(l, o, sp(w));
    mwrt(l + w - length(HeaderText), o, HeaderText);
  end;
  attrtxt(col.coltext);
  clwin(l, l + w - 1, y, y + DispLines - 1);

  suchline := 0; slen := 0;
  f7p := 1; f8p := 0;
  mzo := mauszuo; mzu := mauszuu;
  mzl := mauszul; mzr := mauszur;
  mausdown := false;
  maus_pushinside(l, l + w - 2, y + 1, y + DispLines - 2);
  mb := InOut.AutoBremse; AutoBremse := true;
  scrolling := false;
  repeat
    display;
    showstat;
    if Assigned(FOnShowLines) then FOnShowLines(GetSelection);
    (*      mauszuo:=(pl<>nil) and (pl^.prev<>nil);
          mauszuu:=(pl<>nil) and (pl^.next<>nil); *)
    mauszul := false; mauszur := false;
    if (FirstLine = 0) or (_mausy > y) then AutoUp := false;
    if (FirstLine + DispLines > lines.count - 1) or (_mausy < y + DispLines -
      1) then AutoDown := false;
    if mcursor and selbar then
    begin
      gotoxy(l, y + FSelLine - FirstLine);
      get(t, curon);
    end
    else
      get(t, curoff);
    mauszuo := mzo; mauszuu := mzu;
    mauszul := mzl; mauszur := mzr;

    (*      if (t>=mausfirstkey) and (t<=mauslastkey) then
             Maus_bearbeiten; *)
    if Assigned(FonKeyPressed) then FOnKeyPressed(Self, t);

    if Lines.Count > 0 then
    begin                             { Liste nicht leer }
      s := Lines[FSelLine];
      if stat.markable and (t = ' ') and Assigned(FOnTestMark) and FOnTestMark(s, false) then
      begin
        if Marked(FSelLine) then
        begin
          Lines.Objects[FSelLine] := Pointer(0);
          Dec(FSelCount)
        end else
        begin
          Lines.Objects[FSelLine] := Pointer(1);
          Inc(FSelCount);
        end;
        t := keydown;
      end;

      if (t = ' ') and not stat.markable and not selbar then
        t := keypgdn;

      if stat.maysearch and ((UpperCase(t) = 'S') or (t = '/') or (t = '\'))
        then
      begin
        suchcase := (t = 'S') or (t = '\');
        suchen(false);
      end;
      if stat.maysearch and (t = keytab) then
        suchen(true);

      // key up
      if t = keyup then
        if SelBar then
        begin
          if FSelLine > 0 then Dec(FSelLine);
          FirstLine := Min(FirstLine, FSelLine);
        end
        else
          if FirstLine > 0 then
            Dec(FirstLine);

      // key down
      if t = keydown then
        if selbar then
        begin
          if FSelLine < Lines.Count - 1 then Inc(FSelLine);
          if FirstLine + DispLines - 1 < FSelLine then Inc(FirstLine);
        end
        else
          if FirstLine + DispLines < Lines.Count then
            Inc(FirstLine);

      // goto first line of text
      if (t = keyhome) or (t = keycpgu) then
      begin
        FirstLine := 0;
        FSelLine := 0;
        slen := 0;
      end;

      // goto last line of text
      if (t = keyend) or (t = keycpgd) then
      begin
        FirstLine := Max(Lines.Count - DispLines, 0);
        FSelLine := Lines.Count - 1;
      end;

      if t = keypgup then
      begin
        FirstLine := Max(0, FirstLine - DispLines);
        FSelLine := Max(FirstLine, FSelLine - DispLines);
      end;

      if t = keypgdn then
      begin
        FirstLine := Min(FirstLine + DispLines, Max(Lines.Count - DispLines, 0));
        FSelLine := Min(FSelLine + DispLines, Max(Lines.Count - 1, 0));
      end;

      if t = keychom then
        FirstLine := 0;

      if (t = keycend) and selbar then
      begin
        FirstLine := 0;
        while (FirstLine < lines.count) and (FirstLine < DispLines) do
        begin
          inc(Firstline);
        end;
      end;

      if not stat.noshift then
      begin
        if ((t = keyrght) or (t = keycrgt)) and (xa < 180) then inc(xa, 10);
        if ((t = keyleft) or (t = keyclft)) and (xa > 1) then dec(xa, 10);
        { if t=keyclft then xa:=1;
          if t=keycrgt then xa:=181; }
      end;

      if t = ^E then
      begin
        clearmark;
        slen := 0;
      end;
      if stat.markable then
      begin
        if t = keyf7 then
        begin
          f7p := FSelLine;
          setmark;
        end;
        if t = keyf8 then
        begin
          f8p := FSelLine;
          setmark;
        end;
      end;
      if (stat.markable or stat.markswitch) and (t = ^A)
        and not stat.noctrla then
      begin
        f7p := 0; f8p := lines.count - 1;
        setmark;
      end;

      if (UpperCase(t) = 'M') and stat.markswitch then
      begin
        Fselbar := not FSelbar;
        stat.markable := Selbar;
        // move Selbar between first and last line in Screen
        FSelLine := MinMax(FSelLine, FirstLine, FirstLine + DispLines);
      end;

      if stat.rot13enable and (t = ^R) then
        ListRot13;

      if (t = keycr) and not stat.endoncr and Assigned(FOnEnter) then
      begin
        FOnEnter(Lines[FSelLine]);
        t := '';
      end;
    end;
  until (t = keyesc) or
    ((t = keycr) and ((selbar) or stat.endoncr));
  maus_popinside;
  AutoBremse := mb;
  Result := (t = keyesc);
  if Result then
    FSelLine := - 1;
end;

procedure TLister.SetHeaderText(s: string);
begin
  FHeaderText := LeftStr(s, 40);
end;

procedure TLister.SetArrows(x, y1, y2, acol, bcol: byte; backchr: char);
begin
  arrows.x := x;
  arrows.y1 := y1; arrows.y2 := y2;
  arrows.arrowattr := acol;
  arrows.backattr := bcol;
  arrows.backchr := backchr;
  arrows.usearrows := true;
end;

function TLister.FirstMarked: string;
begin
  MarkPos := 0;
  if SelCount = 0 then
    Result := GetSelection
  else
  begin
    while (MarkPos < Lines.Count) and (not Marked(MarkPos)) do
      Inc(MarkPos);
    if MarkPos = Lines.Count then
      Result := #0
    else
      Result := Lines[MarkPos];
    Inc(MarkPos);
  end;
  FLinePos := MarkPos;
end;

function TLister.NextMarked: string;
begin
  while (MarkPos < Lines.Count) and (not Marked(MarkPos)) do
    Inc(MarkPos);
  if MarkPos = Lines.Count then
    Result := #0
  else
    Result := Lines[MarkPos];
  Inc(MarkPos);
  FLinePos := MarkPos;
end;

function TLister.GetSelection: string;
begin
  if FSelLine <> -1 then
    Result := Lines[FSelLine]
  else
    Result := '';
end;

function TLister.FirstLine: string;
begin
  if lines.count = 0 then
  begin
    Result := #0;
    FLinePos := -1;
  end else
  begin
    FLinePos := 1;
    Result := Lines[0];
  end;
end;

function TLister.NextLine: string;
begin
  if linepos = -1 then
    Result := #0
  else
  begin
    Result := Lines[LinePos];
    Inc(FLinepos);
    if FLinepos = Lines.Count then
    begin
      FLinePos := -1;
      Result := #0;
    end;
  end;
end;

function TLister.PrevLine: string;
begin
  if linepos >= Lines.Count then
    Result := #0
  else
  begin
    Result := Lines[LinePos];
    Dec(FLinePos);
  end;
end;

initialization
  with ListColors do
    if color then
    begin
      coltext := 7;
      colselbar := $30;
      colmarkline := green;
      colmarkbar := $30 + green;
      colfound := $71;
      colstatus := 3;
    end
    else
    begin
      coltext := 7;
      colselbar := $70;
      colmarkline := $F;
      colmarkbar := $70;
      colfound := 1;
      colstatus := $F;
    end;
finalization
end.
{
  $Log$
  Revision 1.47  2000/12/26 16:40:27  mk
  - readded search function

  Revision 1.46  2000/12/26 12:10:00  mk
  - fixed a litte bug in pgend

  Revision 1.45  2000/12/26 09:44:48  mk
  - fixed some more bugs

  Revision 1.44  2000/12/25 22:50:45  mk
  - MarkPos in FirstMarked should be 0

  Revision 1.43  2000/12/25 20:40:24  mk
  - fixed FirstMarked

  Revision 1.42  2000/12/25 20:30:20  mk
  - test if Event functions are not nil

  Revision 1.41  2000/12/25 14:02:40  mk
  - converted Lister to class TLister

  Revision 1.40  2000/12/22 10:09:04  mk
  - compatiblity update for fpc

  Revision 1.39  2000/12/22 10:04:33  mk
  - nearly complete rewrite

}

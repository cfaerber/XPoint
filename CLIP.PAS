{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

(***********************************************************)
(*                                                         *)
(*                       UNIT clip                         *)
(*                                                         *)
(*           Schnittstelle zum Windows-Clipboard           *)
(*                     + Smartdrive                        *)
(*                                         PM 11/92, 05/93 *)
(***********************************************************)

{$B-,V-,R-,O+}

unit clip;

interface

uses dos;

const     cf_Text      = 1;            { Clipboard-Datenformate }
          cf_Bitmap    = 2;
          cf_Oemtext   = 7;
          cf_Dsptext   = $81;
          cf_DspBitmap = $82;

function  WinVersion:word;                      { Windows >= 3.0      }
procedure Idle;                                 { Rechenzeit freigeben}

function ClipAvailable:boolean;                 { Clipboard verfgbar }
function ClipOpen:boolean;                      { Clipboard ”ffnen    }
function ClipClose:boolean;                     { Clipboard schlieáen }
function ClipEmpty:boolean;                     { Clipboard l”schen   }
function ClipCompact(desired:longint):longint;  { freien Platz ermitteln }
function ClipWrite(format:word; size:longint; var data):boolean;
function ClipGetDatasize(format:word):longint;
function ClipRead(format:word; var data):boolean;   { Daten lesen }

procedure FileToClip(fn:pathstr);
procedure ClipToFile(fn:pathstr);

procedure ClipTest;

function  SmartInstalled:boolean;
function  SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
function  SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
procedure SmartResetCache;
procedure SmartFlushCache;


implementation  { ---------------------------------------------------- }

const Multiplex = $2f;
      maxfile   = 65520;

type  ca  = array[0..65530] of char;
      cap = ^ca;

{$L clip.obj}

function WinVersion:word;       external;     { Windows-Version abfragen }
function ClipAvailable:boolean; external;     { wird Clipboard untersttzt? }
function ClipOpen:boolean;      external;     { Clipboard ”ffnen }
function ClipClose:boolean;     external;     { Clipboard schlieáen }
function ClipEmpty:boolean;     external;     { Clipboard l”schen }
function ClipCompact(desired:longint):longint; external;  { Platz ermitteln }
function ClipWrite2(format:word; size:longint; var data):boolean; external;
function ClipGetDatasize(format:word):longint; external;
function ClipRead(format:word; var data):boolean; external;   { Daten lesen }

procedure Idle; external;

function ClipWrite(format:word; size:longint; var data):boolean;  { Schreiben }
begin
  if ClipCompact(size)>=size then
    ClipWrite:=ClipWrite2(format,size,data)
  else
    ClipWrite:=false;
end;



{$I-}

procedure FileToClip(fn:pathstr);
var f  : file;
    p  : pointer;
    bs : word;
    rr : word;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult=0 then
    if ClipAvailable and ClipOpen then begin
      if maxavail>maxfile then bs:=maxfile
      else bs:=maxavail;
      getmem(p,bs);
      blockread(f,p^,maxfile,rr);
      close(f);
      if ClipEmpty then;
      if ClipWrite(cf_Oemtext,rr,p^) then;
      if ClipClose then;
      freemem(p,bs);
      end;
end;

procedure ClipToFile(fn:pathstr);
var f  : file;
    p  : cap;
    bs : longint;
    s  : string[40];
    bp : longint;
begin
  assign(f,fn);
  rewrite(f,1);
  if ioresult=0 then begin
    if ClipAvailable and ClipOpen then begin
      bs:=ClipGetDatasize(cf_OemText);
      if bs>=maxfile then begin
        s:='Clipboard-Inhalt ist zu umfangreich'#13#10;
        blockwrite(f,s[1],length(s));
        end
      else
        if bs>0 then begin
          getmem(p,bs);
          if ClipRead(cf_Oemtext,p^) then begin
            bp:=bs;
            while (bp>0) and (p^[bp-1]=#0) do dec(bp);
            blockwrite(f,p^,bp);
            end;
          if ClipClose then;
          freemem(p,bs);
          end;
      end;
    close(f);
    end;
end;

{$I+}


procedure ClipTest;
var s : string;

  procedure TestRead(ft:string; format:word);
  var l : longint;
      p : ^ca;
      i : integer;
  begin
    l:=ClipGetDatasize(format);
    if l>0 then begin
      writeln(ft,': ',l,' Bytes');
      if l<65530 then begin
        getmem(p,l);
        if ClipRead(format,p^) then
          for i:=0 to l-1 do
            write(p^[i]);
        freemem(p,l);
        end;
      writeln;
      end;
  end;

begin
  if not ClipAvailable then
    writeln('kein Clipboard vorhanden!')
  else
    repeat
      write('(l)esen, (s)chreiben, (d)atei-lesen, d(a)tei-schreiben, (e)nde >');
      readln(s);
      if s='l' then
        if ClipOpen then begin
          TestRead('Text',cf_Text);
          TestRead('Oemtext',cf_Oemtext);
          if ClipClose then;
          end
        else else
      if s='s' then begin
        write('Text> '); readln(s);
        if ClipOpen then begin
          if ClipEmpty then;
          if {ClipWrite(cf_Text,length(s),s[1]) and}
             ClipWrite(cf_Oemtext,length(s),s[1]) then;
          if ClipClose then;
          end
        end else
      if s='d' then begin
        write('Datei> '); readln(s);
        ClipToFile(s);
        end else
      if s='a' then begin
        write('Datei> '); readln(s);
        FileToClip(s);
        end;
    until s='e';
end;


{ Smartdrive vorhanden? }

function SmartInstalled:boolean;       
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=0;                { installation check }
    intr($2f,regs);
    SmartInstalled:=(ax=$BABE);
    end;
end;


{ Cache-Status abfragen }

function SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=3;
    bp:=drive;
    dl:=0;                { get status }
    intr($2f,regs);
    if (ax<>$BABE) or (dl=$ff) then
      SmartCache:=0
    else if dl and $40=0 then SmartCache:=2
    else if dl and $80=0 then SmartCache:=1
    else SmartCache:=0;
    end;
end;


{ Cache-Status setzen }

function SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
var regs : registers;
  procedure sfunc(nr:byte);
  begin
    with regs do begin
      ax:=$4a10;
      bx:=3;
      bp:=drive;
      dl:=nr;
      intr($2f,regs);
      SmartSetcache:=(ax=$BABE) and (dl<>$ff);
      end;
  end;
begin
  case b of
    0 : sfunc(2);          { turn off read cache }
    1 : begin
          sfunc(1);        { turn on read cache }
          sfunc(4);        { turn off write cache }
        end;
    2 : begin
          sfunc(1);        { turn on read cache }
          sfunc(3);        { turn on write cache }
        end;
  end;
end;


{ Schreib-Cache leeren }

procedure SmartResetCache;
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=1;
    intr($2f,regs);
    end;
end;


{ Read-Cache-Inhalt verwerfen, Schreibcache leeren }

procedure SmartFlushCache;
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=2;
    intr($2f,regs);
    end;
end;


end.


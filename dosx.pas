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
(*                       UNIT dosx                         *)
(*                                                         *)
(*        Erweiterungen von DOS; BIOS-Schnittstelle        *)
(*                                                         *)
(***********************************************************)

UNIT dosx;

{$I XPDEFINE.INC}


{  ==================  Interface-Teil  ===================  }

INTERFACE

uses xpglobal, crt,dos, typeform;

var  readfail,writefail : boolean;


procedure setmaxhandles(count:word; buf:pointer);

function  GetDrive:char;
procedure SetDrive(drive:char);
function  dospath(d:byte):pathstr;
procedure GoDir(path:pathstr);
function  GetMaxDrive:char;
{function  GetLastdrive:char;}
function  GetMediaType(drive:char):byte;    { aus BPB von Diskette   }
function  SetupRam(n:byte):byte;
function  GetDeviceType(drive:char):byte;   { Åber IOCTL von Treiber }
function  laufwerke:byte;                   { Åber int $11 }
function  DriveType(drive:char):byte;       { 0=nix, 1=Disk, 2=RAM, 3=Subst }
                                            { 4=Device, 5=Netz              }
function  alldrives:string;

procedure readsec(drive,head:byte; track:integer; sec:shortint; var buffer);
procedure writesec(drive,head:shortint; track:integer; sec:shortint; var buffer);

function  diskready(drive:byte):boolean;
function  GetVolLabel(drive:char):string;
procedure SetVolLabel(drive:char; vlabel:string);

function  InputRedirected:boolean;
function  OutputRedirected:boolean;
function  ConfigFILES:byte;                  { FILES= .. }
function  FreeFILES(maxfiles:byte):word;     { freie Files; max. 255 }
function  IsDevice(fn:pathstr):boolean;

procedure XIntr(intno:byte; var regs:registers);   { DPMI-kompatibler Intr }
function  DPMIallocDOSmem(paras:word; var segment:word):word;
procedure DPMIfreeDOSmem(selector:word);


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

{$IFNDEF DPMI}
  const Seg0040 = $40;
{$ENDIF}

const DPMI   = $31;

type fcbtype = record
                 drive   : byte;
                 name    : array[1..8] of char;
                 ext     : array[1..3] of char;
                 fpos    : word;
                 recsize : word;
                 fsize   : longint;
                 fdate   : word;
                 ftime   : word;
                 reserv  : array[1..8] of byte;
                 currec  : byte;
                 relrec  : longint;
               end;
     extfcb =  record
                 flag    : byte;                  { mu· $ff sein! }
                 reserv  : array[1..5] of byte;
                 attrib  : byte;
                 fcb     : fcbtype;
               end;

function GetDrive:char;
var regs : registers;
begin
  with regs do begin
    ax:=$1900;
    msdos(regs);
    getdrive:=chr(al+65);
    end;
end;


procedure SetDrive(drive:char);
var regs : registers;
begin
  with regs do begin
    ah:=$e;
    dl:=ord(UpCase(drive))-65;
    msdos(regs);
    end;
end;


{ 0=aktuell, 1=A, .. }

function dospath(d:byte):pathstr;
var s : string;
begin
  getdir(d,s);
  dospath:=s;
end;


procedure GoDir(path:pathstr);
begin
  if path='' then exit;
  SetDrive(path[1]);
  if (length(path)>3) and (path[length(path)]='\') then
    dec(byte(path[0]));
  chdir(path);
end;


function GetMaxDrive:char;
var regs : registers;
begin
  with regs do begin
    ah:=$19;            
    msdos(regs);        { aktuelles LW abfragen; 0=A, 1=B usw. }
    ah:=$e; dl:=al;
    msdos(regs);        { aktuelles LW setzen; liefert lastdrive in al }
    GetMaxDrive:=chr(al+64);
    end;
end;

{
function getlastdrive:char;
var drive : char;
    regs  : registers;
    p     : ^byte;
begin
  with regs do begin
    ah:=$52;
    msdos(regs);
    p:=ptr(memw[es:bx+2],memw[es:bx]);
    repeat
      drive:=chr(p^+65);
      inc(longint(p),$19);
      FastMove(p^,p,4);
    until ofs(p^)=$ffff;
    getlastdrive:=drive;
    end;
end;
}

function GetMediaType(drive:char):byte;
{$IFNDEF VER32 }
var regs : registers;
{$ENDIF}
begin
{$IFNDEF VER32 } { !! MK 12/99 }
  with regs do begin
    ah:=$1c;
    dl:=ord(UpCase(drive))-64;
    msdos(regs);
    GetMediaType:=mem[ds:bx];
    end;
{$ENDIF}
end;


function SetupRam(n:byte):byte;
begin
{$IFDEF Ver32} { !! MK 12/99 }
{$ELSE}
  if n<=63 then begin
    inline($fa);
    port[$70]:=n;
    SetupRam:=port[$71];
    end
  else
    SetupRam:=0;
{$ENDIF}
end;


function GetDeviceType(drive:char):byte;
{$IFNDEF WIN32} { !! MK 12/99 }
var dp   : array[-1..40] of byte;
    i    : integer;
    regs : registers;
{$ENDIF}
begin
{$IFDEF WIN32} { !! MK 12/99 }
  GetDeviceType := 5; { immer Festplatte }
{$ELSE}
  with regs do begin
    ax:=$440d;
    bx:=ord(UpCase(drive))-64;
    cx:=$860;
    ds:=seg(dp);
    dx:=ofs(dp);
    fillchar(dp,sizeof(dp),0);
    msdos(regs);
    if (flags and FCarry)<>0 then
      GetDeviceType:=0            { nicht installiert }
    else
      case dp[0] of
        0 : GetDeviceType:=1;     { 360 K             }
        1 : GetDeviceType:=2;     { 1,2 MB            }
        2 : GetDeviceType:=3;     { 720 K             }
        7 : GetDeviceType:=4;     { 1,44 MB           }
        5 : GetDeviceType:=5;     { Festplatte        }
      end;
    end;
{$ENDIF}
end;


function laufwerke:byte;
var regs : registers;
begin
  intr($11,regs);
  if not odd(regs.ax) then laufwerke:=0
  else laufwerke:=(regs.ax shr 6) and 3 + 1;
end;


procedure readsec(drive,head:byte; track:integer; sec:shortint; var buffer);
{$IFDEF WIN32} { !! MK 12/99 }
begin
{$ELSE}
var regs : registers;
begin
  with regs do begin
    ah:=2;
    al:=1;
    es:=seg(buffer);
    bx:=ofs(buffer);
    dl:=drive;
    dh:=head;
    ch:=track and $ff;
    cl:=sec+(track shr 8) shl 6;
    intr($13,regs);
    readfail:=(flags and fcarry)<>0;
    end;
{$ENDIF}
end;


procedure writesec(drive,head:shortint; track:integer; sec:shortint; var buffer);
{$IFDEF WIN32} { !! MK 12/99 }
begin
{$ELSE}
var regs : registers;
begin
  with regs do begin
    ah:=3;
    al:=1;
    es:=seg(buffer);
    bx:=ofs(buffer);
    dl:=drive;
    dh:=head;
    ch:=track and $ff;
    cl:=sec+(track shr 8) shl 6;
    intr($13,regs);
    writefail:=(flags and fcarry)<>0;
    end;
{$ENDIF}
end;


{ Test, ob Diskette eingelegt und Klappe geschlossen }
{ drive: 0=LW A:, 1=LW B:                            }

function diskready(drive:byte):boolean;
{$IFDEF Ver32 } { MK 12/99 }
begin
{$ELSE}

  function ticker:longint;
  begin
    ticker:=meml[Seg0040:$6c];
  end;

  procedure SetMotorTicker(b:byte);
  begin
    mem[Seg0040:$40]:=b;
  end;

  procedure SetMotorStat(b:byte);
  begin
    mem[Seg0040:$3f]:=b;
  end;

  procedure sendfdc(b:byte);
  var t : longint;
  begin
    t:=ticker;
    repeat until (port[$3f4]>=$80) or (abs(ticker-t)>4);  { ~1/5 s Timeout }
    port[$3f5]:=b;
  end;

  function getfdc:byte;
  var t : longint;
  begin
    t:=ticker;
    repeat until (port[$3f4]>=$80) or (abs(ticker-t)>4);  { ~1/5 s Timeout }
    if port[$3f4]>=$80 then getfdc:=port[$3f5]
    else getfdc:=$ff;
  end;

var i,b : byte;

begin
  if drive>1 then writeln('ungÅltiges Laufwerk!')
  else begin
    SetMotorTicker($30);
    SetMotorStat(1 shl drive);  { Motor-on-Flag setzen          }
    port[$3f2]:=$18+drive;      { Controller resetten; Motor an }
    delay(50);
    port[$3f2]:=$1c+drive;
    delay(1000);              { die Verzîgerungszeit muss fÅr besonders  }
                              { lahme Laufwerke evtl. noch erhîht werden }
                              { 1000 ms sollten aber Minimum sein!       }

    sendfdc($4a);             { Read Sector ID }
    sendfdc(drive);
    i:=1;
    repeat
      b:=getfdc;
      inc(i);
    until (b=255) or (i>7);
    port[$3f2]:=$18+drive;    { Controller resetten }
    delay(50);
    port[$3f2]:=$1c+drive;
    diskready:=(i>7);
  end;
{$ENDIF}
end;


function GetVolLabel(drive:char):string;
var sr : searchrec;
begin
{$IFNDEF WIN32}
  findfirst(drive+':\*.*',VolumeID,sr);
  if doserror=0 then GetVolLabel:=sr.name
  else GetVolLabel:='';
{$ELSE }
  GetVolLabel := '';
{$ENDIF}
end;


procedure setfcbname(var fcb:fcbtype; name:string);
{$IFDEF WIN32}
begin
end;
{$ELSE}
var p : byte;
begin
  p:=pos('.',name);
  if p=0 then begin
    p:=length(name)+1;
    name:=name+'.';
    end;
  fillchar(fcb.name,11,' ');
  FastMove(name[1],fcb.name,p-1);
  FastMove(name[p+1],fcb.ext,length(name)-p);
end;
{$ENDIF}

procedure SetVolLabel(drive:char; vlabel:string);
{$IFDEF WIN32}
begin
end;
{$ELSE}
var fcb  : extfcb;
    vl   : pathstr;
    regs : registers;
    f    : file;
begin
  vl:=GetVolLabel(drive);
  fcb.flag:=$ff;
  fcb.attrib:=VolumeID;
  if vl<>'' then begin
    setfcbname(fcb.fcb,vl);
    fcb.fcb.drive:=ord(UpCase(drive))-64;
    regs.ah:=$13;                { Delete File }
    regs.ds:=seg(fcb);
    regs.dx:=ofs(fcb);
    msdos(regs);
    end;
  if vlabel<>'' then begin
    fcb.fcb.drive:=ord(UpCase(drive))-64;
    setfcbname(fcb.fcb,vlabel);
    with regs do begin
      ah:=$16;                  { Create File }
      ds:=seg(fcb);
      dx:=ofs(fcb);
      msdos(regs);
      ah:=$10;                  { Close File }
      ds:=seg(fcb);
      dx:=ofs(fcb);
      msdos(regs);
      end;
    end;
end;
{$ENDIF}


function InputRedirected:boolean;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var regs : registers;
begin
  with regs do begin
    ax:=$4400;
    bx:=textrec(input).handle;
    intr($21,regs);
    InputRedirected:=(flags and fcarry=0) and (dx and 128=0);
    end;
end;
{$ENDIF}

function OutputRedirected:boolean;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var regs : registers;
begin
  with regs do begin
    ax:=$4400;
    bx:=textrec(output).handle;
    intr($21,regs);
    OutputRedirected:=(flags and fcarry=0) and (dx and 128=0);
    end;
end;
{$ENDIF}

{ Buf sollte im Datensegment liegen }
{ benîtigt DOS ab Version 3.0       }
{ pro Handle wird 1 Byte benîtigt   }

procedure setmaxhandles(count:word; buf:pointer);
{$IFDEF Ver32 }
begin
end;
{$ELSE}
var oldbuf : pointer;
    oldcnt : word;
begin
  oldcnt:=memw[prefixseg:$32];
  oldbuf:=pointer(meml[prefixseg:$34]);
  if oldcnt<count then
    FastMove(oldbuf^,buf^,oldcnt)
  else
    FastMove(oldbuf^,buf^,count);
  meml[prefixseg:$34]:=longint(buf);
  memw[prefixseg:$32]:=count;
end;
{$ENDIF}

{ 0=nix, 1=Disk, 2=RAM, 3=Subst, 4=Device, 5=Netz }

function DriveType(drive:char):byte;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var regs : registers;
begin
  if (drive='B') and (laufwerke=1) then
    drivetype:=0
  else
    with regs do begin
      ax:=$4409;
      bl:=ord(drive)-64;
      msdos(regs);
      if flags and fcarry<>0 then
        drivetype:=0
      else
        if dx and $8000<>0 then drivetype:=3 else
        if dx and $1000<>0 then drivetype:=5 else
        if dx and $8ff=$800 then drivetype:=2 else
        if dx and $4000<>0 then drivetype:=4 else
        drivetype:=1;
      end;
end;
{$ENDIF}

function alldrives:string;
var b : byte;
    s : string;
    c : char;
begin
  b:=0;
  for c:='A' to GetMaxdrive do
    if drivetype(c)>0 then begin
      inc(b);
      s[b]:=c;
      end;
  s[0]:=chr(b);
  alldrives:=s;
end;


function ConfigFILES:byte;                  { FILES= .. - DOS >= 2.0! }
{$IFDEF WIN32}
begin
end;
{$ELSE}
type wa   = array[0..2] of word;
var  regs : registers;
     wp   : ^wa;
     n    : word;
begin
  with regs do begin
    ah:=$52;             { Get List of Lists }
    msdos(regs);
    wp:=ptr(es,bx+4);
    wp:=ptr(wp^[1],wp^[0]);
    n:=0;
    while ofs(wp^)<>$ffff do begin
      inc(n,wp^[2]);
      wp:=ptr(wp^[1],wp^[0]);
      end;
    if n>255 then n:=255;
    ConfigFILES:=n;
    end;
end;
{$ENDIF}

function FreeFILES(maxfiles:byte):word;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var f  : array[1..255] of ^file;
    i  : integer;
    fm : byte;
begin
  {$I-}
  i:=0;
  fm:=filemode;
  filemode:=$40;
  repeat
    inc(i);
    new(f[i]);
    assign(f[i]^,'nul');
    reset(f[i]^,1);
  until (i=maxfiles) or (inoutres<>0);
  if ioresult<>0 then begin
    dispose(f[i]); dec(i); end;
  FreeFILES:=i;
  while i>0 do begin
    close(f[i]^);
    dispose(f[i]);
    dec(i);
    end;
  filemode:=fm;
end;
{$ENDIF}

procedure XIntr(intno:byte; var regs:registers);   { DPMI-kompatibler Intr }
var dpmistruc : record
                  edi,esi,ebp,reserved : longint;
                  ebx,edx,ecx,eax      : longint;
                  flags,es,ds,fs,gs    : word;
                  ip,cs,sp,ss          : word;
                end;
    regs2     : registers;
begin
  {$IFNDEF DPMI}
    intr(intno,regs);
  {$ELSE}
    with dpmistruc do begin       { Register-Translation-Block aufbauen }
      edi:=regs.di; esi:=regs.si;
      ebp:=regs.bp; reserved:=0;
      ebx:=regs.bx; edx:=regs.dx;
      ecx:=regs.cx; eax:=regs.ax;
      flags:=$200;
      es:=regs.es; ds:=regs.ds;
      fs:=regs.es; gs:=regs.es; cs:=regs.es;
      sp:=0; ss:=0;      { neuen Real-Mode-Stack anlegen }
      end;
    with regs2 do begin           { Protected-Mode-Int aufrufen }
      ax:=$300;
      bx:=intno;
      cx:=0;
      es:=seg(dpmistruc);
      di:=ofs(dpmistruc);
      intr(DPMI,regs2);
      end;
    with dpmistruc do begin       { Real-Mode-Register zurÅckkopieren }
      regs.ax:=eax and $ffff; regs.bx:=ebx and $ffff;
      regs.cx:=ecx and $ffff; regs.dx:=edx and $ffff;
      regs.bp:=ebp and $ffff;
      regs.si:=esi and $ffff; regs.di:=edi and $ffff;
      regs.ds:=ds; regs.es:=es; regs.flags:=flags;
      end;
  {$ENDIF}
end;

function DPMIallocDOSmem(paras:word; var segment:word):word;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var regs : registers;
begin
  with regs do begin
    ax:=$100;
    bx:=paras;
    intr(DPMI,regs);
    if flags and fcarry<>0 then begin
      segment:=0;
      DPMIallocDOSmem:=0;
      end
    else begin
      segment:=regs.ax;
      DPMIallocDOSmem:=dx;
      end;
    end;
end;
{$ENDIF}

procedure DPMIfreeDOSmem(selector:word);
var regs : registers;
begin
  regs.ax:=$101;
  regs.dx:=selector;
  intr(DPMI,regs);
end;


function IsDevice(fn:pathstr):boolean;
{$IFDEF WIN32}
begin
end;
{$ELSE}
var f    : file;
    regs : registers;
begin
  assign(f,fn);
  reset(f);
  if ioresult<>0 then
    IsDevice:=false
  else begin
    with regs do begin
      ax:=$4400;        { IOCTL Get device data }
      bx:=filerec(f).handle;
      msdos(regs);
      IsDevice:=(flags and fcarry=0) and (dx and 128<>0);
      end;
    close(f);
    end;
end;
{$ENDIF}

begin
  readfail:=false;
  writefail:=false;
end.



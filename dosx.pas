{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

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

function  GetDrive:char;
function  dospath(d:byte):pathstr;
procedure GoDir(path:pathstr);
function  DriveType(drive:char):byte;       { 0=nix, 1=Disk, 2=RAM, 3=Subst }
                                            { 4=Device, 5=Netz              }
function  alldrives:string;

function  OutputRedirected:boolean;
{$IFDEF BP }
function  ConfigFILES:byte;                  { FILES= .. }
function  FreeFILES(maxfiles:byte):word;     { freie Files; max. 255 }
{$ENDIF }
function  IsDevice(fn:pathstr):boolean;

{$IFDEF BP }
procedure XIntr(intno:byte; var regs:registers);   { DPMI-kompatibler Intr }
function  DPMIallocDOSmem(paras:word; var segment:word):word;
procedure DPMIfreeDOSmem(selector:word);
{$ENDIF }


{ ================= Implementation-Teil ==================  }

implementation

{$IFDEF Ver32 }
uses sysutils;
{$ENDIF }


{$IFDEF BP }
const DPMI   = $31;
{$ENDIF }

function GetDrive:char;
{$IFDEF BP }
var regs : registers;
begin
  with regs do begin
    ax:=$1900;
    msdos(regs);
    getdrive:=chr(al+65);
    end;
{$ELSE  }
var
  s: String;
begin
  s := GetCurrentDir;
  GetDrive := s[1];
{$ENDIF }
end;

{ 0=aktuell, 1=A, .. }

function dospath(d:byte):pathstr;
var s : string;
begin
  getdir(d,s);
  dospath:=s;
end;

procedure SetDrive(drive:char);
{$IFDEF BP }
var regs : registers;
begin
  with regs do begin
    ah:=$e;
    dl:=ord(UpCase(drive))-65;
    msdos(regs);
    end;
{$ELSE }
begin
  SetCurrentDir(Drive + ':');
{$ENDIF }
end;

procedure GoDir(path:pathstr);
begin
  if path='' then exit;
  SetDrive(path[1]);
  if (length(path)>3) and (path[length(path)]='\') then
    dec(byte(path[0]));
  chdir(path);
end;


function OutputRedirected:boolean;
{$IFDEF ver32}
begin
  OutputRedirected := false;
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

{ 0=nix, 1=Disk, 2=RAM, 3=Subst, 4=Device, 5=Netz }


function DriveType(drive:char):byte;
{$IFDEF Ver32  }
begin
  { !! Hier sollte speziell fÅr das Betriebssystem eine saubere
    Implementation gemacht werden }
  if Drive <= #26 then DriveType := 1 else DriveType := 0;
end;
{$ELSE }

  function laufwerke:byte;
  var regs : registers;
  begin
    intr($11,regs);
    if not odd(regs.ax) then laufwerke:=0
    else laufwerke:=(regs.ax shr 6) and 3 + 1;
  end;

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
{$ENDIF }

function alldrives:string;

  function GetMaxDrive:char;
  {$ifdef ver32 }
  begin
    GetMaxDrive:='C'; { muss noch portiert werden }
  end;
  {$else}
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
  {$endif}

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

{$IFDEF BP }
function ConfigFILES:byte;                  { FILES= .. - DOS >= 2.0! }
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
{$ENDIF }

{$IFDEF BP }
function FreeFILES(maxfiles:byte):word;
var f  : array[1..255] of ^file;
    i  : integer;
    fm : byte;
begin
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

{$IFDEF BP }
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
{$ENDIF }

{$IFDEF BP }
function DPMIallocDOSmem(paras:word; var segment:word):word;
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

{$IFDEF BP }
procedure DPMIfreeDOSmem(selector:word);
var regs : registers;
begin
  regs.ax:=$101;
  regs.dx:=selector;
  intr(DPMI,regs);
end;
{$ENDIF }


function IsDevice(fn:pathstr):boolean;
{$IFDEF BP }
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
{$ELSE }
begin
  { COMs sind Devices, der Rest nicht }
  IsDevice := Pos('COM', fn) = 1;
{$ENDIF }
end;


end.
{
  $Log$
  Revision 1.9  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.8  2000/03/14 15:15:35  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}

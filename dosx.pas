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

unit dosx;

{$I XPDEFINE.INC}


{  ==================  Interface-Teil  ===================  }

interface

{$ifdef Linux}
  {$i linux/dosxh.inc}
{$else}

uses
  xpglobal,
  dos;

function  GetDrive:char;
function  alldrives:string;
function  DriveType(drive:char):byte;       { 0=nix, 1=Disk, 2=RAM, 3=Subst }
                                            { 4=Device, 5=Netz              }
{$endif} { Linux }

function  dospath(d:byte):pathstr;

function  OutputRedirected:boolean;
function  IsDevice(fn:pathstr):boolean;


{ ================= Implementation-Teil ==================  }

implementation

{$ifdef Linux}
  {$i linux/dosx.inc}
{$else}

uses
{$ifdef vp }
  vpsyslow,
{$endif}
{$IFDEF Win32 }
  windows,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
  sysutils;

function GetDrive:char;
var
  s: String;
begin
  s := GetCurrentDir;
  GetDrive := s[1];
end;

{ 0=aktuell, 1=A, .. }

function dospath(d:byte):pathstr;
var s : string;
begin
  getdir(d,s);
  dospath:=s;
end;

function OutputRedirected:boolean;
begin
  {$IFDEF OS2 }
    { VP 2.0 ist fehlerhaft, Funktion geht nur unter OS/2 }
    {$IFDEF VP }
       OutputRedirected := SysFileIsDevice(SysFileStdOut) = 0;
    {$ELSE }
       OutputRedirected := false;
    {$ENDIF }
  {$ELSE }
    OutputRedirected := false;
  {$ENDIF }
end;

{ Buf sollte im Datensegment liegen }
{ benîtigt DOS ab Version 3.0       }
{ pro Handle wird 1 Byte benîtigt   }

{ 0=nix, 1=Disk, 2=RAM, 3=Subst, 4=Device, 5=Netz, 6=CD-ROM }

function DriveType(drive:char):byte;
{$IFDEF WIn32 }
const
  DriveStr: String = '?:\'+#0;
{$ENDIF }
{$ifdef vp }
  var
    dt:TDriveType;
{$endif}
begin
  {$ifdef vp }
  dt:=SysGetDriveType(drive);
  case dt of
    dtFloppy,
    dtHDFAT,
    dtHDHPFS,
    dtHDNTFS,
    dtHDExt2     : DriveType:=1;
    dtTVFS       : DriveType:=3;
    dtNovellNet,
    dtLAN        : DriveType:=5;
    dtCDRom      : DriveType:=6;
    else           DriveType:=0;
  end;
  {$else}
    {$IFDEF Win32 }
      DriveStr[1] := Drive;
      case GetDriveType(@DriveStr[1]) of
        DRIVE_REMOVABLE,
        DRIVE_FIXED:     DriveType := 1;
        DRIVE_RAMDISK:   DriveType := 2;
        DRIVE_REMOTE:    DriveType := 5;
        DRIVE_CDROM:     DriveType := 6;
      else
        DriveType := 0;
      end;
    {$ELSE }
      DriveType := SysGetDriveType(drive);
    {$ENDIF }
  {$endif}
end;

function alldrives:string;
var
  s : string;
  Drives: longint; { Bitmaske mit vorhandenen Laufwerken }
  i: integer;
begin
  s := '';
  {$IFNDEF DOS32 }
  {$IFDEF Vp }
    Drives:=SysGetValidDrives;
  {$ELSE }
    {$IFDEF Win32 }
      Drives := GetLogicalDrives;
    {$ELSE }
      Drives := 1 shl 27 - 1; {!!}
    {$ENDIF }
  {$ENDIF }
    for i := 0 to 25 do
      if (Drives and (1 shl i)) > 0 then
        s := s + Chr(i + 65);
  {$ELSE }
    for i:=Ord('A') to Ord(SysGetMaxdrive) do
      if drivetype(Char(i))>0 then
        s := s + Chr(i);
  {$ENDIF }

  alldrives:=s;
end;

function IsDevice(fn:pathstr):boolean;
begin
  { COMs sind Devices, der Rest nicht }
  IsDevice := Pos('COM', fn) = 1;
end;

{$endif} { Linux }

end.
{
  $Log$
  Revision 1.26  2000/10/03 15:45:07  mk
  - DOS32-Implementation von DriveType und AllDrives

  Revision 1.25  2000/07/16 16:59:28  mk
  - AnsiString Updates

  Revision 1.24  2000/07/15 20:02:58  mk
  - AnsiString updates, noch nicht komplett

  Revision 1.23  2000/07/04 21:23:07  mk
  - erste AnsiString-Anpassungen

  Revision 1.22  2000/07/03 16:26:28  mk
  - unnoetige Compilerdirectiven entfernt bzw. vereinfacht

  Revision 1.21  2000/07/03 15:23:26  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.20  2000/06/29 12:55:37  hd
  - Linux-Teil isoliert

  Revision 1.19  2000/06/23 15:59:09  mk
  - 16 Bit Teile entfernt

  Revision 1.18  2000/06/22 19:53:24  mk
  - 16 Bit Teile ausgebaut

  Revision 1.17  2000/05/09 13:10:14  hd
  - UnixFS: get/setdrive entfernt
  - GoDir: dec(path[0]) durch delete(path, length(path), 1) ersetzt
  - UnixFS: AllDrives entfernt
  - UnixFS: DriveType entfernt
  - UnixFS: Uses optimiert

  Revision 1.16  2000/04/30 17:55:58  hd
  Keine crt-Unit, keine nCrt-Unit! :-)

  Revision 1.15  2000/04/30 15:37:30  mk
  - crt-Unit aus uses entfernt

  Revision 1.14  2000/04/29 16:19:45  hd
  Linux-Anpassung

  Revision 1.13  2000/04/23 07:58:52  mk
  - OS/2-Portierung

  Revision 1.12  2000/04/13 12:48:30  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/03/25 00:29:22  mk
  - GetDriveType und AllDrives jetzt sauber portiert

  Revision 1.10  2000/03/24 23:11:16  rb
  VP Portierung

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

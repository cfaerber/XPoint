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

{$I xpdefine.inc }

{$ifdef FPC} {$mode objfpc} {$endif}

{ File I/O and file name processing functions }
unit fileio;

interface

{$ifdef unix}
uses sysutils,
{$IFDEF Kylix}
  libc,
{$ELSE}
  Linux,
{$ENDIF }
  xplinux,xpglobal,typeform;
{$else }
uses sysutils,xpglobal,typeform
  {$ifdef vp} ,vpusrlow {$endif}
  {$ifdef Win32} ,xpwin32, windows {$endif}
  {$ifdef Dos32} ,xpdos32 {$endif};
{$endif}

const
  FMRead       = $00;     { Konstanten fuer Filemode }
  FMWrite      = $01;
  FMRW         = $02;
  FMDenyNone   = $40;
  FMDenyRead   = $30;
  FMDenyWrite  = $20;
  FMDenyBoth   = $10;
  FMCompatible = $00;

  fmsharedenynone = 0;


const
  { Neue AnyFile-Konstante, da $3F oft nicht laeuft }
  ffAnyFile = $20;

{$IFNDEF FPC }
const
  fsFromBeginning = 0;
  fsFromCurrent   = 1;
  fsFromEnd       = 2;
{$ENDIF}

type
  { Zugriffsrechte beim Erstellen einer Datei }
  TCreateMode = (
        cmUser,                         { Nur User RW }
        cmUserE,                        { mit Ausfuehrung }
        cmGrpR,                         { User RW, Group R }
        cmGrpRE,                        { + Ausfuehrung }
        cmGrpRW,                        { Beide RW }
        cmGrpRWE,                       { + Ausfuehrung }
        cmAllR,                         { User RW, alle anderen R }
        cmAllRE,                        { + Ausfuehrung }
        cmAllRW,                        { Alle RW }
        cmAllRWE);                      { Alle alles }

{ Funktionen zum Erstellen der Datei unter Beruecksichtigung
  der Zugriffsrechte. }

{$IFDEF FPC }
procedure XPRewrite(var F: file; l: longint; cm: TCreateMode);
procedure XPRewrite(var F: text; cm: TCreateMode);
{$ENDIF }
procedure XPRewrite(var F: file; cm: TCreateMode);

{ Works like Dos.FSplit }
procedure FSplit(const path: string; var dir, name, ext: string);

{ Append directory separator; resolve DRIVE: if necessary }
function  AddDirSepa(const p: string): string;

{$IFNDEF Delphi }
{ Append directory separator }
function IncludeTrailingPathDelimiter(const S: string): string;
{$ENDIF }

{ Checks whether file exists - note f has to be a file type, NOT a string.
  Use SysUtils.FileExists to check using a filename. }
function  existf(var f):boolean;

{ Searches for file in current directory, program's directory and path.
  Path has to be correct if specified. Characters after a space
  in fn are ignored. Reports empty string if file not found. }
function  FindFile(fn: string): string;

{ Searches for file in current directory, program's directory and path.
  Characters after a space in fn are ignored. File MAY be specified
  without extension; returned name WILL contain extension. Path has to
  be correct if specified. Returns empty string if not found. }
function  FindExecutable(fn: string): string;

{ Searches for file in current directory, program's directory and path
  if no path is given; if path is specified, checks if executable exists
  in this path. Characters after a space in fn are ignored. File MAY be
  specified without extension. }
function  ExecutableExists(fn: string): boolean;

{ Checks whether file name is valid (= a file of this name could be created
  or exists already) }
Function  ValidFileName(const name:string):boolean;

function  isEmptyDir(const path: string): boolean;

{ Checks whether this path exists }
function  IsPath(const Fname:string):boolean;

{ Generate name for a temp file, returns empty string if not successful }
function  TempFile(const path:string):string;

{ Generate a name for a temp file beginning with startnamewith (max. 4 chars).
  Specify ext with a trailing "." (in Dos). Returns empty string if
  not successful }
function TempExtFile(path,startnamewith,ext:string):string;

{ Return file size or zero if file does not exist }
function  _filesize(const fn:string):longint;

{ Return file time or zero if file does not exist }
function  filetime(fn:string):longint;

{ Set file time, report true on success }
function  setfiletime(fn:string; newtime:longint): boolean;

{ Copy file, report true on success; an existing dest file will be
  overwritten! }
function  copyfile(srcfn, destfn:string):boolean;

{ Erase file(s). Wildcards are allowed }
procedure erase_mask(const s: string);

{ Move file to backup file with extension newext. Report true on success }
function MakeBak(const n,newext:string): boolean;

{ Create empty file, return true on success }
function MakeFile(const fn: string): Boolean;

{ Create multiple directories (separated by PathSepa).
  Returns empty string on success; if no success, returns
  directory which creation has failed }
function CreateMultipleDirectories(path:string): String;

{ Sets filemode readonly (used when opening files) }
procedure fm_ro;

{ Sets filemode r/w (used when opening files) }
procedure fm_rw;

{ Open only THIS file with filemode fm }
procedure resetfm(var f:file; fm:byte);

{ Add path dir to filename fn if no path specified in fn }
procedure AddDir(var fn: string; dir: string);

{ Returns sum of size of all files in dir }
function  DirectorySize(const dir: string): longint;

{ Returns sum of size of all files that match mask }
function  FileMaskSize(const mask: string): longint;

{ Return file name (without extension) }
function  GetBareFileName(const p: string):string;

{ Returns clear text error messages for IOError values; otxt if unknown }
function  ioerror(i: integer; otxt: string):string;

{ raises an exception if there was an IOError }
procedure IOExcept(e: ExceptClass);

{ to avoid collisons with DOS device names (AUX, COMn, LPTn, CON,...) }
function IsDOSDevice(s:string):boolean;

{$ifndef Unix}
{ Returns concatenation of all valid drive letters }
function  alldrives: string;
{$endif}

implementation  { ------------------------------------------------------- }

uses debug;

{$ifdef unix}
const
  STAT_IRWUSR           = STAT_IRUSR or STAT_IWUSR;
  STAT_IRWGRP           = STAT_IRGRP or STAT_IWGRP;
  STAT_IRWOTH           = STAT_IROTH or STAT_IWOTH;
{$endif}

function SetAccessMode(const fn: string; const cm: TCreateMode): boolean;

{$ifdef Unix}
var
  fm: longint;
begin
  case cm of
    cmUser   : fm:= STAT_IRWUSR;
    cmUserE  : fm:= STAT_IRWXU;
    cmGrpR   : fm:= STAT_IRWUSR or STAT_IRGRP;
    cmGrpRE  : fm:= STAT_IRWXU or STAT_IRGRP or STAT_IXGRP;
    cmGrpRW  : fm:= STAT_IRWUSR or STAT_IRWGRP;
    cmGrpRWE : fm:= STAT_IRWXU or STAT_IRWXG;
    cmAllR   : fm:= STAT_IRWUSR or STAT_IRGRP or STAT_IROTH;
    cmAllRE  : fm:= STAT_IRWXU or STAT_IRGRP or STAT_IXGRP or STAT_IROTH or STAT_IXOTH;
    cmAllRW  : fm:= STAT_IRWUSR or STAT_IRWGRP or STAT_IRWOTH;
    cmAllRWE : fm:= STAT_IRWXU or STAT_IRWXG or STAT_IRWXO;
    else
      fm:= STAT_IRWUSR or STAT_IRWGRP;
  end; { case }
  { Mode setzen }
{$IFDEF Kylix}
  result:= chmod(PChar(fn), fm) <> 0;
{$ELSE}
  result:= chmod(fn, fm);
{$ENDIF}
end;
{$else}
begin result:=true end;
{$endif}

{$IFDEF FPC }
procedure XPRewrite(var F: file; l: longint; cm: TCreateMode);
var
  fn : string;
begin
  System.Rewrite(F,l);
  if ioresult=0 then begin
    fn:= FileName(F);
    System.Close(F);
    { Mode setzen }
    SetAccessMode(fn, cm);
  end; { if }
  { Nochmaliges oeffnen, denn bei IOResult=0 ist die Datei zu,
    ansonsten waere IOResult geloescht. }
  System.Rewrite(F,l);
end;

procedure XPRewrite(var F: text; cm: TCreateMode);
var
  fn : string;
begin
  System.Rewrite(F);
  if ioresult=0 then begin
    fn:= FileName(F);
    System.Close(F);
    { Mode setzen }
    SetAccessMode(fn, cm);
  end; { if }
  System.Rewrite(F);
end;
{$ENDIF }

procedure XPRewrite(var F: file; cm: TCreateMode);
var
  fn : string;
begin
  System.Rewrite(F);
  if ioresult=0 then begin
    fn:= FileName(F);
    System.Close(F);
    { Mode setzen }
    SetAccessMode(fn, cm);
  end; { if }
  System.Rewrite(F);
end;

function isEmptyDir(const path: string): boolean;
var
  sr: TSearchRec;
  rc: integer;
begin
  result:= true;
  rc:= Sysutils.FindFirst(AddDirSepa(path)+WildCard,ffAnyFile,sr);
  while rc = 0 do begin
    if (sr.name <> '.') and (sr.name <> '..') then begin
      result:= false;
      break;
    end;
    rc:= SysUtils.FindNext(sr);
  end;
  sysutils.findclose(sr);
end;

procedure FSplit(const path: string; var dir, name, ext: string);
begin
  dir:= ExtractFilePath(path);
  ext:= ExtractFileExt(path);
  name:= ExtractFileName(path);
  if ext<>'' then
    Delete(name,pos(ext,name),length(ext));
end;

function existf(var f):Boolean;
var
  fr: ^tfilerec;
begin
  fr:= @f;
  result:= FileExists(fr^.name);
end;

function ioerror(i:integer; otxt:string):string;
begin
  case i of
      2 : ioerror:='Datei nicht gefunden';
      3 : ioerror:='ungÅltiges Verzeichnis';
      4 : ioerror:='zu viele Dateien geîffnet (bitte FILES erhîhen!)';
      5 : ioerror:='Zugriff verweigert';
      7 : ioerror:='Speicherverwaltung zerstîrt';
      8 : ioerror:='ungenÅgend Speicher';
     10 : ioerror:='ungÅltiges Environment';
     11 : ioerror:='ungÅltiges Aufruf-Format';
     15 : ioerror:='ungÅltige Laufwerksbezeichnung';
     16 : ioerror:='Verzeichnis kann nicht gelîscht werden';
     18 : ioerror:='Fehler bei Dateisuche';
    101 : ioerror:='Diskette/Platte voll';
    102 : ioerror:='Dateiname fehlt';
    103 : ioerror:='Datei nicht geîffnet';
    104 : ioerror:='Datei nicht zum Lesen geîffnet';
    105 : ioerror:='Datei nicht zum Schreiben geîffnet';
    150 : ioerror:='Diskette ist schreibgeschÅtzt';
    152 : ioerror:='keine Diskette eingelegt';
154,156 : ioerror:='Lesefehler (Diskette/Platte defekt)';
157,158 : ioerror:='Diskette ist nicht korrekt formatiert';
    159 : ioerror:='Drucker ist nicht betriebsbereit';
    162 : ioerror:='Hardware-Fehler';
    209 : ioerror:='Fehler in .OVR-Datei';
  else
    ioerror:=otxt;
  end;
end;

procedure IoExcept(e:ExceptClass);
var r: Integer;
begin
  r:=IOResult;
  if r<>0 then
    raise E.Create(ioerror(r,'Unbekannter E/A-Fehler #'+StrS(r)));
end;

procedure fm_ro;      { Filemode ReadOnly }
begin
  filemode:=fmRead;
end;

procedure fm_rw;      { Filemode Read/Write }
begin
  filemode:=fmRW;
end;


procedure resetfm(var f:file; fm:byte);
var fm0 : byte;
begin
  fm0:=filemode;
  filemode:=fm;
  reset(f,1);
  filemode:=fm0;
end;

function  AddDirSepa(const p: string): string;
{$ifndef Unix}
var
  cwd: string;
{$endif}
begin
  if p='' then
    AddDirSepa:= ''
  else if LastChar(p)<>DirSepa then
{$ifdef Unix}
      result:= p + DirSepa
{$else}
    begin
      if (length(p)=2) and (p[2]=':') then begin     { Nur C: ? }
        getdir(Ord(UpCase(p[1]))-64,cwd);               { -> Akt. Verz. ermitteln }
        result:= AddDirSepa(cwd);
      end else
        result:= p+DirSepa;
    end
{$endif}
  else
    result:= p;
end;

{$IFNDEF Delphi }
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  if LastChar(s) <> DirSepa then
    Result := s + DirSepa
  else
    Result := s;
end;
{$ENDIF }

function FindFile(fn: string): string;
begin
  result:='';
  fn:=trim(fn);
  // strip parameters
  fn:=Copy(fn,1,iif(cPos(' ',fn)<>0,cPos(' ',fn)-1,Length(fn)));

  if ExtractFilePath(fn)<>'' then
    if FileExists(fn)then
      result:=fn
    else // path not correct
      exit
  else
    result:=FileSearch(fn,'.'+PathSepa+
                          ExtractFilePath(ParamStr(0))+PathSepa+
                          GetEnv('PATH'));
  {$ifndef UnixFS} result:=UpperCase(result); {$endif}
end;

function FindExecutable(fn: string): string;
begin
  fn:=trim(fn);
  // strip parameters
  fn:=Copy(fn,1,iif(cPos(' ',fn)<>0,cPos(' ',fn)-1,Length(fn)));
  {$ifndef UnixFS}
  if UpperCase(fn)='COPY' then
    begin result:=fn; exit end; // built-in in cli
  if ExtractFileExt(fn)='' then begin
    result:=FindFile(fn+'.exe');
    if result='' then result:=FindFile(fn+'.com');
    if result='' then result:=FindFile(fn +'.bat' { extBatch} );
    end
  else result:=FindFile(fn);
  {$else}
  result:=FindFile(fn);
  {$endif}
end;

function ExecutableExists(fn: string): boolean;
begin
  result:=FindExecutable(fn)<>'';
end;

function ValidFileName(const name:string):boolean;
var handle: longint;
begin
  result:=false;
  if (name='') or multipos('*?&',name) then
    exit
  else if FileExists(name) then
    result:=true
  else begin
    handle:=FileCreate(name);
    if handle>0 then
    begin
      FileClose(handle);
      result:=SysUtils.DeleteFile(name);
    end;
  end;
end;

function IsPath(const fname:string):boolean;
var
  curdir: string;
begin
  if fname='' then begin result:=true; exit end;
  curdir:= GetCurrentDir;
  result:= SetCurrentDir(fname);
  SetCurrentDir(curdir);
end;

function copyfile(srcfn, destfn:string):boolean;
const bufs= 65536;
var buf:pointer;
    f1,f2,bytesread:longint;
begin
  result:=false;
  if not FileExists(srcfn)then exit;
  getmem(buf,bufs);
  f2:=FileCreate(destfn);
  if f2<=0 then exit;
  f1:=FileOpen(srcfn,fmOpenRead);
  if f1<=0 then begin FileClose(f2); exit end;
  repeat
    bytesread:=FileRead(f1,buf^,bufs);
    if bytesread>0 then bytesread:=FileWrite(f2,buf^,bytesread);
  until bytesread<>bufs;
  FileClose(f1); FileClose(f2);
  freemem(buf,bufs);
  result:=bytesread<>-1;
end;

procedure erase_mask(const s: string);
var
  sr : TSearchrec;
begin
  if SysUtils.FindFirst(s, faAnyfile, sr) = 0 then repeat
    sysutils.DeleteFile(AddDirSepa(ExtractFilePath(s))+sr.name);
  until SysUtils.FindNext(sr) <> 0;
  sysutils.FindClose(sr);
end;

function MakeBak(const n,newext:string): boolean;

  {$IFDEF FPC }
  function booltostr(b: boolean; x: boolean): string;
  begin
    if b then
      result := 'true'
    else
      result := 'false';
  end;
  {$ENDIF }
  
var
  bakname : string;
begin
  result:=false;
  if not FileExists(n) then exit;
  BakName := ChangeFileExt(n, '.' + FileUpperCase(NewExt));
  if FileExists(BakName) then
  begin
    {$IFNDEF UnixFS }
    if FileSetAttr(BakName,faArchive) <> 0 then exit;
    {$ENDIF }
    if not SysUtils.DeleteFile(Bakname)then exit;
  end;
  {$IFNDEF UnixFS }
  if FileSetAttr(n, faArchive) <> 0 then exit;
  {$ENDIF }
  Result := RenameFile(n, bakname);
end;

function CreateMultipleDirectories(path:string): String;
var
  p : Integer;
  adir : string;
begin
  result:='';
  path:=trim(path);
  if path='' then exit;

  while path<>'' do begin
    p:=iif(Pos(PathSepa,path)>0,Pos(PathSepa,path),Length(path)+1);
    adir:=Copy(path,1,p-1);
    CreateDir(adir);
    if not IsPath(adir)then begin result:=adir; exit end;
    Delete(path,1,p);
  end;
end;

function TempFile(const path: string): string;
var
  n: string;
begin
  result:='';
  if not IsPath(path)then exit;
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not FileExists(AddDirSepa(path)+n);
  result:=AddDirSepa(path)+n;
end;

function TempExtFile(path,startnamewith,ext:string):string;
begin
  result:='';
  if not IsPath(path)then exit;
  repeat
    result:=path+startnamewith+formi(random(10000),4)+ext
  until not FileExists(result);
end;

function _filesize(const fn:string):longint;
var sr : TSearchrec;
begin
  if SysUtils.FindFirst(fn,faAnyFile,sr) = 0 then
    Result := sr.Size
  else
    Result := 0;
  sysutils.Findclose(sr);
end;

function MakeFile(const fn: string): Boolean;
var handle: longint;
begin
  handle:=FileCreate(fn);
  result:=handle>0;
  if result then FileClose(handle);
end;

function filetime(fn:string):longint;
var sr : Tsearchrec;
begin
  if SysUtils.FindFirst(fn,faAnyFile,sr) = 0 then
    filetime:=sr.time
  else
    filetime:=0;
  sysutils.findclose(sr);
end;

function setfiletime(fn:string; newtime:longint): boolean;
{$IFNDEF Kylix}
var
  fh: longint;
begin
  fh:= FileOpen(fn, fmOpenWrite);
  if fh > 0 then
  begin
    result:= (FileSetDate(fh, newtime) = 0);
    FileClose(fh);
  end else
    result:= false;
{$ELSE}
begin
  result:= (FileSetDate(fn, newtime) = 0);
{$ENDIF}
end;

function GetBareFileName(const p:string):string;
var d,
    n,
    e : string;
begin
  fsplit(p,d,n,e);
  GetBareFileName:=n;
end;

procedure AddDir(var fn: string; dir: string);
var s: string;
begin
{$IFDEF UnixFS }
  fn:=ResolvePathName(fn);
{$ENDIF }
  s:= ExtractFilePath(fn);
  if s='' then
    fn:= AddDirSepa(dir)+fn;
end;

{$ifndef Unix }
function alldrives:string;
var
  s : string;
  Drives: longint; { Bitmaske mit vorhandenen Laufwerken }
  i: integer;
begin
  s := '';
  {$IFNDEF DOS32 }
    {$IFDEF Win32 }
      Drives := GetLogicalDrives;
    {$ELSE }
      Drives := 1 shl 27 - 1; {!!}
    {$ENDIF }
    for i := 0 to 25 do
      if (Drives and (1 shl i)) > 0 then
        s := s + Chr(i + 65);
  {$ELSE }
    for i:=Ord('A') to Ord(SysGetMaxdrive) do
      if SysGetDrivetype(Char(i))>0 then
        s := s + Chr(i);
  {$ENDIF }

  alldrives:=s;
end;
{$endif }

function FileMaskSize(const mask: string): longint;
var
  sr: tsearchrec;
  rc: integer;
begin
  result:= 0;
  rc:=SysUtils.FindFirst(mask,faAnyFile,sr);
  while rc=0 do begin
    inc(result,sr.size);
    rc:= SysUtils.FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

function DirectorySize(const dir: string): longint;
begin
  result:= FileMaskSize(AddDirSepa(dir)+WildCard);
end;

{ to avoid collisons with DOS device names (AUX, COMn, LPTn, CON,...) }
{ NB: This is also implemented on non-DOS systems as it would be very }
{ unfortunate if we were to create files that a DOS-XP could not      }
{ handle }
function IsDOSDevice(s:string):boolean;
begin
  s:=UpperCase(s);

  result :=
    ( LastChar(s)='$') or
    ( s = 'CON' ) or
    ( s = 'AUX' ) or
    ( s = 'NUL' ) or
    ( s = 'PRN' ) or
    ( LeftStr(s,3)='COM' ) or
    ( LeftStr(s,3)='LPT' );
end;

{
  $Log$
  Revision 1.113  2002/03/23 09:48:31  mk
  - remove debuginfo from MakeBak
  - MakeBak returns true on success, instead of false

  Revision 1.112  2002/03/20 14:44:23  mk
  - added debug infos for makebak

  Revision 1.111  2002/02/21 13:52:30  mk
  - removed 21 hints and 28 warnings

  Revision 1.110  2002/01/21 22:45:48  cl
  - fixes after 3.40 merge

  Revision 1.109  2001/10/21 12:49:57  ml
  - removed some warnings

  Revision 1.108  2001/10/20 17:26:39  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.107  2001/10/15 09:04:21  ml
  - compilable with Kylix ;-)

  Revision 1.106  2001/10/07 17:09:11  cl
  - removed import of xp0 (causes rc.exe/ihs.exe compiled with FPC 1.0.0
    to crash).

  Revision 1.105  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.104  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.103  2001/09/07 17:27:24  mk
  - Kylix compatiblity update

  Revision 1.102  2001/09/07 13:54:17  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.101  2001/08/28 13:01:39  mk
  - removed RenameDir
  - renamed RenameDir to RenameFile

  Revision 1.100  2001/08/11 23:06:26  mk
  - changed Pos() to cPos() when possible

  Revision 1.99  2001/08/01 01:00:30  mk
  - added IncludeTrailingPathDelimiter and fixed compile problems

  Revision 1.98  2001/07/31 13:10:31  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.97  2001/07/28 12:33:33  mk
  - GetEnv is now in OS dependend and not in dos unit

  Revision 1.96  2001/07/21 13:29:51  mk
  - Added RenameDir
  - minior fix for CreateMultipleDirectories

  Revision 1.95  2001/03/16 17:04:53  cl
  - IoExcept (checks for IoError and raises exception)


  Revision 1.94  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.93  2001/03/13 00:29:30  cl
  - fixed erase_mask

  Revision 1.92  2001/03/02 22:07:20  cl
  - VPascal: define fsFromXXX constants for FileSeek

  Revision 1.91  2001/02/25 11:34:12  ma
  - removed non-GPL code

  Revision 1.90  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.89  2001/01/21 16:56:25  mk
  - fix for VP compatiblity in FileCreate/FileOpen results

  Revision 1.88  2001/01/14 10:58:04  ma
  - fixed FindFile/FindExecutable/ExecutableExists
  - paths have to be correct if specified in these functions now

  Revision 1.87  2001/01/06 16:58:26  ma
  - well. VP doesn't like exit(X).

  Revision 1.86  2001/01/06 16:13:30  ma
  - general cleanup
  - using SysUtil functions instead of System functions
  - new functions: FindFile, FindExecutable
  - renamed mklongdir to CreateMultipleDirectories and changed function
}
end.


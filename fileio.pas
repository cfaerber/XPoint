{ $Id$

   File I/O and file name processing functions
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I XPDEFINE.INC }

{$ifdef FPC} {$mode objfpc} {$endif}

{ File I/O and file name processing functions }
unit fileio;

interface

{$ifdef unix}
uses sysutils,linux,xplinux,xpglobal,typeform;
{$else }
uses sysutils,xpglobal,dos,typeform
  {$ifdef vp} ,vpusrlow {$endif}
  {$ifdef Win32} ,windows {$endif}
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

{ Checks whether file exists - note f has to be a file type, NOT a string.
  Use SysUtils.FileExists to check using a filename. }
function  existf(var f):boolean;

{ Searches for file in current directory, program's directory and path.
  Path is ignored if specified but not correct. Characters after a space
  in fn are ignored. Reports empty string if file not found. }
function  FindFile(fn: string): string;

{ Searches for file in current directory, program's directory and path.
  Characters after a space in fn are ignored. File MAY be specified
  without extension; returned name WILL contain extension. Path is
  ignored if specified but not correct. Returns empty string if not found. }
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

{ Generate a name for a temp file beginning with ld (max. 4 chars).
  Specify ext with a trailing "." (in Dos). Returns empty string if
  not successful }
function  TempExtFile(path,ld,ext:string):string;

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
function MakeBak(n,newext:string): boolean;

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

{ Replacement for Dos.GetEnv }
function  GetEnv(const name: string): string;

{ Returns clear text error messages for IOError values; otxt if unknown }
function  ioerror(i: integer; otxt: string):string;

{$ifndef Unix}
{ Returns concatenation of all valid drive letters }
function  alldrives: string;
{$endif}


implementation  { ------------------------------------------------------- }

uses xp0;

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
  end; { case }
  { Mode setzen }
  result:= chmod(fn, fm);
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

function FindFile(fn: string): string;
begin
  result:='';
  fn:=trim(fn);
  fn:=Copy(fn,1,iif(Pos(' ',fn)<>0,Pos(' ',fn)-1,Length(fn)));

  if ExtractFilePath(fn)<>'' then
    if FileExists(fn)then begin
      result:=fn; exit
      end
    else // path not correct, get rid of it
      fn:=ExtractFileName(fn);

  // fn is without path now

  result:=FileSearch(fn,'.'+PathSepa+
                        ExtractFilePath(ParamStr(0))+PathSepa+
                        GetEnv('PATH'));
  {$ifndef UnixFS} result:=UpperCase(result); {$endif}
end;

function FindExecutable(fn: string): string;
begin
  fn:=trim(fn);
  fn:=Copy(fn,1,iif(Pos(' ',fn)<>0,Pos(' ',fn)-1,Length(fn)));
  {$ifndef UnixFS}
  if UpperCase(fn)='COPY' then exit(fn); // built-in in cli
  if ExtractFileExt(fn)='' then begin
    result:=FindFile(fn+'.exe');
    if result='' then result:=FindFile(fn+'.com');
    if result='' then result:=FindFile(fn+'.bat');
    end
  else result:=FindFile(fn);
  {$else}
  result:=FindFile(fn);
  {$endif}
end;

function ExecutableExists(fn: string): boolean;
var found: string;
begin
  found:=FindExecutable(fn);
  // caution: FindExecutable ignores specified path if it's not correct,
  // therefore...
  if ExtractFilePath(fn)<>'' then
    {$ifdef UnixFS}
    result:=fn=found
    {$else}
    result:=UpperCase(fn)=UpperCase(found)
    {$endif}
  else result:=found<>'';
end;

function ValidFileName(const name:string):boolean;
var handle: longint;
begin
  if (name='') or multipos('*?&',name) then
    exit(false)
  else if FileExists(name) then
    exit(true)
  else begin
    handle:=FileCreate(name);
    if handle<>-1 then begin
      FileClose(handle);
      exit(SysUtils.DeleteFile(name));
    end else exit(false);
  end;
end;

function IsPath(const fname:string):boolean;
var
  curdir: string;
begin
  if fname='' then exit(true);
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
    sysutils.DeleteFile(ExtractFileDir(s) + DirSepa +sr.name);
  until SysUtils.FindNext(sr) <> 0;
  sysutils.FindClose(sr);
end;

function MakeBak(n,newext:string): boolean;
var bakname : string;
begin
  result:=false;
  if not FileExists(n) then exit;
  BakName := ChangeFileExt(n, '.' + NewExt);
  if FileExists(BakName) then
  begin
    if FileSetAttr(BakName,faArchive)=-1 then exit;
    if not SysUtils.DeleteFile(Bakname)then exit;
  end;
  if FileSetAttr(n, faArchive)=-1 then exit;
  if not RenameFile(n, bakname)then exit;
  result:=true;
end;

function CreateMultipleDirectories(path:string): String;
var
  p : byte;
  adir : string;
begin
  result:='';
  path:=trim(path);
  if path='' then exit;

  while path<>'' do begin
    p:=iif(Pos(PathSepa,path)>0,Pos(PathSepa,path),Length(path)+1);
    adir:=Copy(path,1,p-1);
    CreateDir(adir);
    if not IsPath(adir)then exit(adir);
    Delete(path,1,p);
  end;
end;

function TempFile(const path: string): string;
var
  n: string;
begin
  if not IsPath(path)then exit('');
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not FileExists(AddDirSepa(path)+n);
  result:=AddDirSepa(path)+n;
end;

function TempExtFile(path,ld,ext:string):string;
var n : string[MaxLenFilename];
begin
  if not IsPath(path)then exit('');
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not FileExists(path+n);
  result:=path+n;
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
  result:=handle<>-1;
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
var
  fh: longint;
begin
  fh:= FileOpen(fn, fmOpenWrite);
  if fh >= 0 then begin
    result:= (FileSetDate(fh, newtime) = 0);
    FileClose(fh);
  end else
    result:= false;
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

function GetEnv(const name: string): string;
begin
  {$ifdef Linux} result:= Linux.GetEnv(name);
  {$else} result:=Dos.GetEnv(name); {$endif}
end;

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

end.

{
  $Log$
  Revision 1.86  2001/01/06 16:13:30  ma
  - general cleanup
  - using SysUtil functions instead of System functions
  - new functions: FindFile, FindExecutable
  - renamed mklongdir to CreateMultipleDirectories and changed function

  Revision 1.85  2000/11/19 22:29:36  hd
  - fix: missing uses-statement

  Revision 1.84  2000/11/19 17:51:56  ma
  - GetEnv works again under Win32. Proper replacement of
    Dos.GetEnv will be used at request if Dos is *really*
    that bad.
  - renamed existBin to ExecutableExists

  Revision 1.83  2000/11/18 21:15:56  mk
  - removed GetCBreak, SetCBreak - this routines are not necessary anymore

  Revision 1.82  2000/11/18 16:11:57  hd
  - Grml, on ist kein g¸ltiger BEzeichner

  Revision 1.81  2000/11/18 16:09:56  hd
  - Get-/SetCBreak
    - Diese Routinen muessen fuer andere OS als Linux implementiert werden!!

  Revision 1.80  2000/11/16 22:35:29  hd
  - DOS Unit entfernt
}

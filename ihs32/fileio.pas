{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ File-I/O und Dateinamenbearbeitung }

{$I XPDEFINE.INC }

{$IFDEF FPC }
  {$mode objfpc}
{$ENDIF }

unit fileio;

interface

{$ifdef unix}
uses
  sysutils,
  linux,
  xplinux,
  xpglobal,
  debug,
  typeform;
{$else }
uses
{$ifdef vp }
  vpusrlow,
{$endif}
{$IFDEF Win32 }
  windows,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
  sysutils,
  xpglobal,
  debug,dos,
  typeform;
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

{ Zugriffsrechte beim Erstellen einer Datei }
type TCreateMode = (
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

{ DOS-Routinen }
procedure FSplit(const path: string; var dir, name, ext: string);

function  AddDirSepa(const p: string): string;      { Verz.-Trenner anhaengen }
Function  existf(var f):boolean;                { Datei vorhanden ?       }
function  ExecutableExists(fn: string): boolean;  { ausfuehrbare Datei vorhanden? (PATH), fn (auch) ohne Extension }
Function  ValidFileName(const name:string):boolean;  { gueltiger Dateiname ?    }
function  isEmptyDir(const path: string): boolean;
function  IsPath(const Fname:string):boolean;         { Pfad vorhanden ?        }
function  TempFile(const path:string):string;       { TMP-Namen erzeugen      }
function  TempExtFile(path,ld,ext:string):string; { Ext-Namen erzeugen }
function  _filesize(const fn:string):longint;        { Dateigroesse in Bytes     }
function  filetime(fn:string):longint;         { Datei-Timestamp         }
function  setfiletime(fn:string; newtime:longint): boolean;  { Dateidatum setzen  }
function  copyfile(srcfn, destfn:string):boolean; { Datei kopieren }
procedure erase_mask(const s: string);          { Datei(en) loeschen       }
Procedure MakeBak(n,newext:string);             { sik anlegen             }
procedure MakeFile(fn:string);                 { Leerdatei erzeugen      }
procedure mklongdir(path:string; var res:integer);  { mehrere Verz. anl. }

procedure fm_ro;                                { Filemode ReadOnly       }
procedure fm_rw;                                { Filemode Read/Write     }
procedure resetfm(var f:file; fm:byte);         { mit spez. Filemode oeffn.}

procedure adddir(var fn:string; dir:string);
function  DirectorySize(const dir: string): longint;
function  FileMaskSize(const mask: string): longint;
function  GetBareFileName(const p:string):string;
function  GetEnv(const name: string): string;
function  ioerror(i:integer; otxt:string):string; { Fehler-Texte            }

{$IFNDEF Unix }
function  alldrives:string;
{$ENDIF }

implementation  { ------------------------------------------------------- }

uses
  StringTools, xp0;

const
{$ifdef unix}
  STAT_IRWUSR           = STAT_IRUSR or STAT_IWUSR;
  STAT_IRWGRP           = STAT_IRGRP or STAT_IWGRP;
  STAT_IRWOTH           = STAT_IROTH or STAT_IWOTH;

  PathSepaChar          = ':'; { Trennzeichen in der Environment-Var PATH }
{$else}
  PathSepaChar          = ';'; { Trennzeichen in der Environment-Var PATH }
{$endif}

function SetAccessMode(const fn: string; const cm: TCreateMode): boolean;
{$ifdef Unix}
var
  fm: longint;
{$endif}
begin
{$ifdef Unix}
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
{$else}
  result:= true;
{$endif}
end;

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

{ Liefert True zurueck, falls ein Verzeichnis leer ist }
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
{
var
  fm : byte;
begin
  fm:=filemode;
  filemode:=FMDenyNone;
  reset(file(f));
  existf:=(ioresult=0);
  close(file(f));
  filemode:=fm;
  if ioresult = 0 then ;
end;
}

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

{ Haengt einen fehlenden Verzeichnisseparator an.
  Loest dabei C: auf (nur Nicht-Unix }
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

{ Sucht die Datei 'fn' in folgender Reihenfolge:
  - Aktuelles Verzeichnis
  - Startverzeichnis der aktuellen Programmdatei
  - Environment-Var PATH sysutils
}
function ExecutableExists(fn: string): boolean;
var
  i      : integer;
  fname,path,SearchingFor   : string;
begin
  result:= false;
  {$IFNDEF UnixFS}
  if(Pos('.COM',UpperCase(fn))=0)and(Pos('.EXE',UpperCase(fn))=0)then fn:=fn+'.exe';
  {$ENDIF}
  fname:= ExtractFileName(fn);
  if FileExists(fn) then
    result:= true
  else if FileExists(AddDirSepa(ExtractFilePath(ParamStr(0)))+fname) then
    result:= true
  else begin
    path:=getenv('PATH');
    Debug.DebugLog('FILEIO','Searching in path: '+path,4);
    i:= pos(PathSepaChar, path);
    while i>0 do begin
      SearchingFor:=AddDirSepa(Copy(path,1,i-1))+fname;
      Debug.DebugLog('FILEIO','Searching: '+SearchingFor,4);
      if FileExists(SearchingFor)then begin
        result:= true;
        path:= '';
        break;
      end;
      Delete(path,1,i); i:=pos(PathSepaChar, path);
    end;
    if Length(path)>0 then
      result:= FileExists(AddDirSepa(path)+fname);
  end;
end;

function ValidFileName(const name:string):boolean;
var
  f: file;
begin
  if (name='') or multipos('*?&',name) then
    ValidFileName:=false
  else if FileExists(name) then
    ValidFileName:=true
  else begin
    assign(f,name);
    rewrite(f);
    result:= (ioresult=0);
    close(f);
    erase(f);
    if ioresult<>0 then ;       { Wert zuruecksetzen }
  end;
end;

function IsPath(const fname:string):boolean;         { Pfad vorhanden ? }
var
  curdir: string;
begin
  curdir:= GetCurrentDir;
  IsPath:= SetCurrentDir(fname);
  SetCurrentDir(curdir);
end;

function copyfile(srcfn, destfn:string):boolean;  { Datei kopieren }
{ keine Ueberpruefung, ob srcfn existiert oder destfn bereits existiert }
var bufs,rr:word;
    buf:pointer;
    f1,f2:file;
begin
  bufs:=65536;
  getmem(buf,bufs);
  assign(f1,srcfn);
  assign(f2,destfn);
  reset(f1,1);
  rewrite(f2,1);
  while not eof(f1) and (inoutres=0) do begin
    blockread(f1,buf^,bufs,rr);
    blockwrite(f2,buf^,rr);
  end;
  close(f2);
  close(f1);
  copyfile:=(inoutres=0);
  if ioresult<>0 then ;
  freemem(buf,bufs);
end;

procedure erase_mask(const s: string);                 { Datei(en) loeschen }
var
  sr : TSearchrec;
begin
  if SysUtils.FindFirst(s, faAnyfile, sr) = 0 then repeat
    sysutils.DeleteFile(ExtractFileDir(s) + DirSepa +sr.name);
  until SysUtils.FindNext(sr) <> 0;
  sysutils.FindClose(sr);
end;


Procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
begin
  if not FileExists(n) then exit;
  BakName := ChangeFileExt(n, '.' + NewExt);
  if FileExists(BakName) then
  begin
    Assign(f, Bakname);
    FileSetAttr(BakName,faArchive);
    erase(f);
  end;
  Assign(f,n);
  FileSetAttr(n, faArchive);
  RenameFile(n, bakname);
  if ioresult<>0 then;
end;

{ res:  0 = Pfad bereits vorhanden }
{       1 = Pfad angelegt          }
{     < 0 = IO-Fehler              }

procedure mklongdir(path:string; var res:integer);
const testfile = 'test0000.$$$';
var p : byte;
begin
  path:=trim(path);
  if path='' then begin
    res:=0;
    exit;
  end;
  if RightStr(path,1)<>DirSepa then path:=path+DirSepa;
  if validfilename(path+testfile) then
    res:=0
  else
    if pos(DirSepa,path)<=1 then begin
      mkdir(path);
      res:=-ioresult;
    end
    else begin
      p:=iif(path[1]=DirSepa,2,1);
      res:=0;
      while (p<=length(path)) do begin
        while (p<=length(path)) and (path[p]<>DirSepa) do inc(p);
        if not IsPath(LeftStr(path,p)) then begin
          mkdir(LeftStr(path,p-1));
          if inoutres<>0 then begin
            res:=-ioresult;
            exit;
          end;
        end
        else
          res:=1;
        inc(p);
      end;
    end;
end;

function TempFile(const path: string): string;       { TMP-Namen erzeugen }
var
  n: string;
begin
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not FileExists(AddDirSepa(path)+n);
  TempFile:=AddDirSepa(path)+n;
end;

function TempExtFile(path,ld,ext:string):string;  { Ext-Namen erzeugen }
{ ld max. 4 Zeichen, ext mit Punkt '.bat' }
var n : string[MaxLenFilename];
begin
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not Fileexists(path+n);
  TempExtFile:=path+n;
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

procedure MakeFile(fn:string);
var
  t : text;
begin
  assign(t,fn);
  rewrite(t);
  if ioresult=5 then
    FileSetAttr(fn,0)
  else
    close(t);
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

function setfiletime(fn:string; newtime:longint): boolean;  { Dateidatum setzen }
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
{
var f : file;
begin
  assign(f,fn);
  reset(f,1);
  setftime(f,newtime);
  close(f);
  if ioresult<>0 then;
end;
}
function GetBareFileName(const p:string):string;
var d,
    n,
    e : string;
begin
  fsplit(p,d,n,e);
  GetBareFileName:=n;
end;


{ Verzeichnis einfuegen, falls noch nicht vorhanden }

procedure adddir(var fn: string; dir: string);
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
  {$IFDEF Linux}result:= Linux.GetEnv(name);{$ELSE}result:=Dos.GetEnv(name){$ENDIF}
end;

{ Dir muﬂ WildCards enthalten }
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
  Revision 1.1.2.2  2003/01/25 08:30:51  mw
  MW: - Log-Kosmetik

  Revision 1.1.2.1  2003/01/25 08:00:01  mw
  MW: - IHS32 angefÅgt, da der 16Bit Hilfecompiler nicht funktioniert.
}

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
  typeform;
{$else }
uses
  sysutils,
{$ifdef vp }
  vpusrlow,
{$endif}
{$IFDEF Win32 }
  windows,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
  xpglobal,
  typeform;
{$endif} { Unix }

const
  FMRead       = $00;     { Konstanten fÅr Filemode }
  FMWrite      = $01;
  FMRW         = $02;
  FMDenyNone   = $40;
  FMDenyRead   = $30;
  FMDenyWrite  = $20;
  FMDenyBoth   = $10;
  FMCompatible = $00;

  fmsharedenynone = 0;


const
  { Neue AnyFile-Konstante, da $3F oft nicht lÑuft }
  ffAnyFile = $20;

{ Zugriffsrechte beim Erstellen einer Datei }
type TCreateMode = (
        cmUser,                         { Nur User RW }
        cmUserE,                        { mit Ausf¸hrung }
        cmGrpR,                         { User RW, Group R }
        cmGrpRE,                        { + Ausf¸hrung }
        cmGrpRW,                        { Beide RW }
        cmGrpRWE,                       { + Ausf¸hrung }
        cmAllR,                         { User RW, alle anderen R }
        cmAllRE,                        { + Ausf¸hrung }
        cmAllRW,                        { Alle RW }
        cmAllRWE);                      { Alle alles }

{ Funktionen zum Erstellen der Datei unter Ber¸cksichtigung
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
function  existBin(const fn: string): boolean;       { Datei vorhanden (PATH)  }
Function  ValidFileName(const name:string):boolean;  { gÅltiger Dateiname ?    }
function  isEmptyDir(const path: string): boolean;
function  IsPath(const Fname:string):boolean;         { Pfad vorhanden ?        }
function  TempFile(const path:string):string;       { TMP-Namen erzeugen      }
function  TempExtFile(path,ld,ext:string):string; { Ext-Namen erzeugen }
function  _filesize(const fn:string):longint;        { Dateigrî·e in Bytes     }
function  filetime(fn:string):longint;         { Datei-Timestamp         }
function  setfiletime(fn:string; newtime:longint): boolean;  { Dateidatum setzen  }
function  copyfile(srcfn, destfn:string):boolean; { Datei kopieren }
procedure erase_mask(s:string);                 { Datei(en) lîschen       }
Procedure MakeBak(n,newext:string);             { sik anlegen             }
procedure MakeFile(fn:string);                 { Leerdatei erzeugen      }
procedure mklongdir(path:string; var res:integer);  { mehrere Verz. anl. }

procedure fm_ro;                                { Filemode ReadOnly       }
procedure fm_rw;                                { Filemode Read/Write     }
procedure resetfm(var f:file; fm:byte);         { mit spez. Filemode îffn.}

procedure adddir(var fn:string; dir:string);
function  GetBareFileName(const p:string):string;
function getenv(const EVar: String): String;

function  ioerror(i:integer; otxt:string):string; { Fehler-Texte            }

{$IFNDEF Unix }
function  alldrives:string;
{$ENDIF }

implementation  { ------------------------------------------------------- }

uses
  xp0;

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
  { Nochmaliges ˆffnen, denn bei IOResult=0 ist die Datei zu,
    ansonsten wSre IOResult gelˆscht. }
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

{ Liefert True zur¸ck, falls ein Verzeichnis leer ist }
function isEmptyDir(const path: string): boolean;
var
  sr: TSearchRec;
  rc: integer;
begin
  result:= true;
  rc:= findfirst(AddDirSepa(path)+WildCard,ffAnyFile,sr);
  while rc = 0 do begin
    if (sr.name <> '.') and (sr.name <> '..') then begin
      result:= false;
      break;
    end;
    rc:= findnext(sr);
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
  fr: ^filerec;
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
  - Aktuelle Verzeichnis
  - Startverzeichnis der aktuellen Programmdatei
  - Environment-Var PATH sysutils
}
function existBin(const fn: string): boolean;
var
  i      : integer;
  fname,
  path   : string;
begin
  result:= false;
  fname:= ExtractFileName(fn);
  if FileExists(fn) then
    result:= true
  else if FileExists(AddDirSepa(ExtractFilePath(ParamStr(0)))+fname) then
    result:= true
  else
{$IFDEF FPC }
  begin
    path:= getenv('PATH');
    i:= pos(PathSepaChar, path);
    while i>0 do begin
      if FileExists(AddDirSepa(Copy(path,1,i-1))+fname) then begin
        result:= true;
        break;
      end;
      Delete(path,1,i);
      i:= pos(PathSepaChar, path);
    end;
    if Length(path)>0 then
      result:= FileExists(AddDirSepa(path)+fname);
  end;
{$ENDIF }
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
{ keine öberprÅfung, ob srcfn existiert oder destfn bereits existiert }
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

procedure erase_mask(s:string);                 { Datei(en) lîschen }
var
  sr : TSearchrec;
begin
  if findfirst(s, faAnyfile, sr) = 0 then repeat
    sysutils.DeleteFile(ExtractFileDir(s) + DirSepa +sr.name);
  until findnext(sr) <> 0;
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
  if Findfirst(fn,faAnyFile,sr) = 0 then
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
  if findfirst(fn,faAnyFile,sr) = 0 then
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


{ Verzeichnis einfÅgen, falls noch nicht vorhanden }

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

function getenv(const EVar: String): String;
begin
  // !!
end;

end.
{
  $Log$
  Revision 1.73  2000/11/15 23:00:39  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.72  2000/11/15 20:30:58  hd
  - GetFTime
  - PackTime
  - UnpackTime

  Revision 1.71  2000/11/15 17:59:30  hd
  - Fix: BackSlash unter UnixFS gibt es nicht

  Revision 1.70  2000/11/15 17:57:02  hd
  - FSplit implementiert
  - DOS-Unit entfernt

  Revision 1.69  2000/11/15 15:51:18  hd
  - Neu: isEmptyDir, da FileExists('/path/*.*') immer false liefert

  Revision 1.68  2000/11/15 12:24:37  mk
  - vp compatiblity update

  Revision 1.67  2000/11/14 20:49:22  ma
  - geht jetzt auch unter Dos/Win wieder...

  Revision 1.66  2000/11/14 20:13:58  hd
  - Laeuft unter Linux wieder
  - existBin optimiert
  - existf umgestellt (Datei wird nicht mehr geoffnet)

  Revision 1.65  2000/11/14 15:51:26  mk
  - replaced Exist() with FileExists()

  Revision 1.63  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.62  2000/11/09 18:12:22  mk
  - ispath corrections

  Revision 1.61  2000/11/09 15:27:02  mk
  - Compilierfaehigkeit wiederhergestellt

  Revision 1.60  2000/11/09 10:50:06  hd
  - Neu: XPRewrite(var F: File|Text [; l: longint]; cm: TCreateMode);
  - Fix: Invalid Types dirstr<->string

  Revision 1.59  2000/11/04 23:12:56  mk
  - fixed false reporting in IsPath

  Revision 1.58  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.57  2000/10/19 20:52:20  mk
  - removed Unit dosx.pas

  Revision 1.56  2000/10/19 12:57:43  mk
  - deleted unused function erase_all

  Revision 1.55  2000/10/17 20:36:13  mk
  - falschen Kommentar zu Disksize/Diskfree entfernt

  Revision 1.54  2000/10/17 17:38:18  mk
  - fixed typo

  Revision 1.53  2000/10/17 12:53:18  mk
  - einige Funktionen auf Sysutils umgestellt

  Revision 1.52  2000/10/17 10:05:39  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.51  2000/07/05 18:57:53  mk
  - AnsiString Updates

  Revision 1.50  2000/07/05 17:10:53  mk
  - AnsiString Updates

  Revision 1.49  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.48  2000/07/04 21:23:07  mk
  - erste AnsiString-Anpassungen

  Revision 1.47  2000/07/04 12:04:16  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.46  2000/07/01 13:18:28  hd
  - Uses-Anweisung in ifdef

  Revision 1.45  2000/06/30 19:11:31  mk
  - wieder compilierbar

  Revision 1.44  2000/06/30 13:48:10  hd
  - Linux extrahiert
  - fileio.inc fuer systemunabhaengige Routinen
  - Linux: IsPath, era, erase_mask vereinfacht

  Revision 1.43  2000/06/23 15:59:10  mk
  - 16 Bit Teile entfernt

  Revision 1.42  2000/06/22 19:53:25  mk
  - 16 Bit Teile ausgebaut

  Revision 1.41  2000/06/20 18:22:27  hd
  - Kleine Aenderungen

  Revision 1.40  2000/06/17 13:14:02  hd
  - Laesst sich jetzt auch wieder unter Linux compilieren :-)

  Revision 1.39  2000/06/16 19:56:24  mk
  - jetzt geht es auch unter nicht Linux wieder zu compilieren, bitte die Aenderungen pruefen!

  Revision 1.38  2000/06/16 14:50:43  hd
  - Neue Funktion: existBin: Sucht eine Datei auch in PATH
  - Neue Funktion: AddDirSepa: Haengt Slash/Backslash an, wenn er fehlt

  Revision 1.37  2000/06/05 16:16:20  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.36  2000/05/29 15:12:57  oh
  -delete_all() abgesichert

  Revision 1.35  2000/05/26 20:13:03  mk
  - Lock war unter DOS32 Bit undefiniert

  Revision 1.34  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.33  2000/05/17 18:45:33  mk
  - Wieder unter allen Platformen compilierbar

  Revision 1.32  2000/05/17 16:11:03  ml
  Zeilenanzahl aendern nun auch in Win32

  Revision 1.31  2000/05/14 12:21:42  hd
  - Anpassungen an UnixFS

  Revision 1.30  2000/05/09 15:51:23  hd
  - UnixFS: WriteBatch angepasst
  - $I- wieder entfernt (war FPC Workaround)
  - WildForm wird unter UnixFS nicht benutzt (8+3 ist nun wirklich Stone-Age)

  Revision 1.29  2000/05/08 19:02:30  hd
  - Bedingte Compilierung optimiert

  Revision 1.28  2000/05/06 17:11:53  hd
  - $I- eingefuegt

  Revision 1.27  2000/05/05 00:10:49  oh
  -PGP-Aufrufe ueber Batch-Datei

  Revision 1.26  2000/05/03 07:33:56  mk
  - unbenutze Variablen/Units rausgeworfen

  Revision 1.25  2000/04/29 16:45:06  mk
  - Verschiedene kleinere Aufraeumarbeiten

  Revision 1.24  2000/04/28 16:23:53  hd
  Linux-Anpassungen (UnixFS):
   - Backslash -> Slash
   - *.* -> *
   - Neuer Exe-Typ: ET_ELF
   - ACHTUNG: Flags fehlen noch.

  Revision 1.23  2000/04/18 11:23:47  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.22  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.21  2000/04/15 14:18:21  mk
  - Fix fuer FindFirst mit Diretories

  Revision 1.20  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.19  2000/03/28 08:38:28  mk
  - Debugcode in Testshare entfernt

  Revision 1.18  2000/03/26 09:41:12  mk
  - erweiterte Share-Erkennung

  Revision 1.17  2000/03/25 23:20:30  mk
  - LockFile geht unter Win9x nicht, wohl aber unter NT. Ausgeklammert

  Revision 1.16  2000/03/25 18:46:59  ml
  uuz lauff‰hig unter linux

  Revision 1.15  2000/03/24 23:11:17  rb
  VP Portierung

  Revision 1.14  2000/03/24 20:25:50  rb
  ASM-Routinen gesÑubert, Register fÅr VP + FPC angegeben, Portierung FPC <-> VP

  Revision 1.13  2000/03/24 04:16:21  oh
  - Function GetBareFileName() (Dateiname ohne EXT) fuer PGP 6.5.x

  Revision 1.12  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.11  2000/03/16 19:25:10  mk
  - fileio.lock/unlock nach Win32 portiert
  - Bug in unlockfile behoben

  Revision 1.10  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.9  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.8  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.7  2000/03/03 20:26:40  rb
  Aufruf externer MIME-Viewer (Win, OS/2) wieder geÑndert

  Revision 1.6  2000/02/23 23:49:47  rb
  'Dummy' kommentiert, Bugfix beim Aufruf von ext. Win+OS/2 Viewern

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}

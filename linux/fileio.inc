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

uses
  xp0;

const
  PathSepaChar          = ':'; { Trennzeichen in der Environment-Var PATH }
  STAT_IRWUSR           = STAT_IRUSR or STAT_IWUSR;
  STAT_IRWGRP           = STAT_IRGRP or STAT_IWGRP;
  STAT_IRWOTH           = STAT_IROTH or STAT_IWOTH;

function SetAccessMode(const fn: string; const cm: TCreateMode): boolean;
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
    ansonsten w‰re IOResult gelˆscht. }
  System.Rewrite(F,l);        
end;

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


{ Haengt einen fehlenden Verzeichnisseparator an.
  Loest dabei C: auf (nur Nicht-Unix }
function  AddDirSepa(p: string): string;
begin
  if p='' then
    AddDirSepa:= ''
  else begin
    if LastChar(p)<>DirSepa then
      AddDirSepa:= p+DirSepa
    else
      AddDirSepa:= p;
  end;
end;

{ Sucht die Datei 'fn' in folgender Reihenfolge:
  - Aktuelle Verzeichnis
  - Startverzeichnis der aktuellen Programmdatei
  - Environment-Var PATH
}
function  existBin(fn: string): boolean;
var
  envpath: string;                      { Opps, bug in brain. PATH kann > 256 sein }
  filename, path: string;
  i, j, k: integer;
begin
  filename:= ExtractFilename(fn);           { Evtl. Pfad ignorieren }
  if exist(fn) then begin               { -> Aktuelles Verzeichnis }
    existBin:= true;
    exit;
  end;
  path:= ProgPath;                      { -> Startverzeichnis }
  if path<>'' then begin
    if exist(AddDirSepa(path)+filename) then begin
      existBin:= true;
      exit;
    end;
  end;
  envpath:= strpas(linux.getenv('PATH'));
  j:= CountChar(PathSepaChar,envpath);
  for i:= 1 to j do begin
    k:= CPos(PathSepaChar, envpath);
    path:= copy(envpath,1,k-1);
    delete(envpath,1,k);
    if path<>'' then
      if exist(AddDirSepa(path)+filename) then begin
        existBin:= true;
        exit;
      end;
  end;
  if envpath<>'' then begin             { Noch was ueber ? }
    if exist(AddDirSepa(envpath)+filename) then
      existBin:= true
    else
      existBin:= false;
  end else
    existBin:= false;
end;

function ValidFileName(name:string):boolean;
var f : file;
begin
  if (name='') or multipos('*?&',name) then
    ValidFileName:=false
  else begin
    assign(f, ResolvePathName(name));           { ~/ aufloesen }
    if existf(f) then ValidFileName:=true
    else begin
      rewrite(f);
      close(f);
      erase(f);
      ValidFileName:=(ioresult=0);
    end;
  end;
end;


function IsPath(fname:string):boolean;         { Pfad vorhanden ? }
var
  curdir: string;
begin
  curdir:= GetCurrentDir;
  IsPath:= SetCurrentDir(fname);
  SetCurrentDir(curdir);
end;

function dospath(d:byte):pathstr;
var s : string;
begin
  getdir(0, s);
  dospath:=s;
end;


function copyfile(srcfn, destfn:string):boolean;  { Datei kopieren }
{ keine öberprÅfung, ob srcfn existiert oder destfn bereits existiert }
var bufs,rr:word;
    buf:pointer;
    f1,f2:file;
begin
  bufs:=65536;
  getmem(buf,bufs);
  assign(f1,ResolvePathName(srcfn));
  assign(f2,ResolvePathName(destfn));
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

procedure era(s:string);
begin
  DeleteFile(ResolvePathName(s));
end;


procedure erase_mask(s:string);                 { Datei(en) lîschen }
var sr : searchrec;
begin
  findfirst(ResolvePathName(s),ffAnyFile,sr);
  while doserror=0 do begin
    DeleteFile(getfiledir(s)+sr.name);
    findnext(sr);
  end;
  FindClose(sr);
end;

procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
    dir     : dirstr;
    name    : namestr;
    ext     : extstr;
begin
  n:= ResolvePathName(n);
  assign(f,n);
  if not existrf(f) then exit;
  fsplit(n,dir,name,ext);
  bakname:=dir+name+'.'+newext;
  assign(f,bakname);
  if existrf(f) then begin
    setfattr(f,archive);
    erase(f);
  end;
  assign(f,n);
  setfattr(f,archive);
  rename(f,bakname);
  if ioresult<>0 then;
end;

procedure WriteBatch(s:string);
var
  f:text;
  io:integer;
begin
  assign(f, TempBatchFN);
  rewrite(f);
  io:=ioresult;
  if (io=0) then begin
    writeln(f,'#!',getenv('SHELL'));
    writeln(f,'#');
    writeln(f,'# This script was generated by ',xp_xp,'.');
    writeln(f,'# Feel free to delete it!');
    writeln(f,'#');
    writeln(f,s);
    close(f);
    SetAccess(TempBatchFN, taUserRWX);          { Ausfuehrbar machen }
  end;
  io:=ioresult;
end;

{ res:  0 = Pfad bereits vorhanden }
{       1 = Pfad angelegt          }
{     < 0 = IO-Fehler              }

procedure mklongdir(path:string; var res:integer);
const testfile = 'test0000.$$$';
var p : byte;
begin
  path:=ResolvePathName(trim(path));
  if path='' then begin
    res:=0;
    exit;
  end;
  if rightstr(path,1)<>DirSepa then path:=path+DirSepa;
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
        if not IsPath(leftstr(path,p)) then begin
          mkdir(leftstr(path,p-1));
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

function TempFile(path:string):string;       { TMP-Namen erzeugen }
var n : string[12];
begin
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not exist(path+n);
  TempFile:=ResolvePathName(path+n);
end;

function TempExtFile(path,ld,ext:string):string;  { Ext-Namen erzeugen }
{ ld max. 4 Zeichen, ext mit Punkt '.bat' }
var n : string[MaxLenFilename];
begin
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not exist(path+n);
  TempExtFile:=ResolvePathName(path+n);
end;


function _filesize(fn:string):longint;
var sr : searchrec;
begin
  findfirst(ResolvePathName(fn),ffAnyFile,sr);
  if doserror<>0 then
    _filesize:=0
  else
    _filesize:=sr.size;
  findclose(sr);
end;

procedure MakeFile(fn:string);
var t : text;
begin
  assign(t,ResolvePathName(fn));
  rewrite(t);
  if ioresult=5 then
    setfattr(t,0)
  else
    close(t);
end;

function filetime(fn:string):longint;
var sr : searchrec;
begin
  findfirst(ResolvePathName(fn),ffAnyFile,sr);
  if doserror=0 then
    filetime:=sr.time
  else
    filetime:=0;
  findclose(sr);
end;

procedure setfiletime(fn:string; newtime:longint);  { Dateidatum setzen }
var f : file;
begin
  assign(f,ResolvePathName(fn));
  reset(f,1);
  setftime(f,newtime);
  close(f);
  if ioresult<>0 then;
end;

function GetFileDir(p:string):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(ResolvePathName(p),d,n,e);
  GetFileDir:=d;
end;

function Gestring(p:string):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(ResolvePathName(p),d,n,e);
  Gestring:=n+e;
end;

function GetBareFileName(p:string):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(ResolvePathName(p),d,n,e);
  GetBareFileName:=n;
end;

function GetFileExt(p:string):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(ResolvePathName(p),d,n,e);
  GetFileExt:=mid(e,2);
end;

function _rename(n1,n2:string):boolean;
var f : file;
begin
  assign(f,ResolvePathName(n1));
  rename(f,ResolvePathName(n2));
  _rename:=(ioresult=0);
end;

{ Extension anhÑngen, falls noch nicht vorhanden }

procedure addext(var fn:string; ext:string);
var s : string;
    l : integer;
begin
  fn:=ResolvePathName(fn);
  s:= ExtractFileExt(fn);
  l:= Length(s);
  if l>0 then
    Delete(fn, Length(fn)-l, l);
  fn:= fn + ext;
end;

{ Verzeichnis einfÅgen, falls noch nicht vorhanden }

procedure adddir(var fn: string; dir: string);
var s: string;
begin
  fn:=ResolvePathName(fn);
  s:= ExtractFilePath(fn);
  if s='' then
    fn:= AddDirSepa(dir)+fn;
end;

procedure WildForm(var s: string);
begin
end;

function exetype(fn:string):TExeType;
var f       : file;
    magic   : array[0..1] of char;
    magic2  : array[0..2] of char;
    hdadr   : longint;
    version : byte;
begin
  fn:= ResolvePathName(fn);
  assign(f,fn);
  resetfm(f,FMDenyWrite);
  blockread(f,magic,2);
  seek(f,60);
  blockread(f,hdadr,4);
  if (ioresult<>0) then
    exetype:=ET_Unknown
  else if (magic<>'MZ') then
    begin
      seek(f, 1);                    { ELF }
      blockread(f,magic2,3);         { IOResult braucht nicht abgefragt }
      if (magic2='ELF') then         { zu werden, da bereits ein hoehrer }
        exetype:=ET_ELF              { Offset verwandt wurde }
      { Fuer andere Suchen }
      else
        exetype:=ET_Unknown;
    end
  else if odd(hdadr) then
    exetype:=ET_DOS
  else
  begin { Fix fÅr LZEXE gepackte Dateien }
    if (hdadr > 0) and (hdadr < FileSize(f)-54) then
    begin
      seek(f,hdadr);
      blockread(f,magic,2);
      if ioresult<>0 then
        exetype:=ET_DOS
      else if magic='PE' then
        exetype:=ET_Win32
      else if magic='LX' then
        exetype:=ET_OS2_32
      else if magic<>'NE' then
        exetype:=ET_DOS
      else begin
        seek(f,hdadr+54);
        blockread(f,version,1);
        if version=2 then exetype:=ET_Win16
        else exetype:=ET_OS2_16;
      end;
    end else
      exetype := ET_DOS;
  end;
  close(f);
  if ioresult<>0 then;
end;

function IsDevice(fn:pathstr):boolean;
begin
  { COMs sind Devices, der Rest nicht }

  { Noch nicht implementiert !!!! }
  IsDevice := false;
end;

{
        $Log$
        Revision 1.8  2000/11/09 10:50:06  hd
        - Neu: XPRewrite(var F: File|Text [; l: longint]; cm: TCreateMode);
        - Fix: Invalid Types dirstr<->string

        Revision 1.7  2000/10/19 20:52:26  mk
        - removed Unit dosx.pas

        Revision 1.6  2000/10/19 12:57:31  mk
        - deleted unused function erase_all

        Revision 1.5  2000/10/18 12:21:34  hd
        - Unter Linux wieder compilierbar

        Revision 1.4  2000/10/17 20:36:26  mk
        - falschen Kommentar zu Disksize/Diskfree entfernt

        Revision 1.3  2000/10/17 12:53:20  mk
        - einige Funktionen auf Sysutils umgestellt

        Revision 1.2  2000/07/05 09:09:29  hd
        - Anpassungen AnsiString
        - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
          erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

        Revision 1.1  2000/06/30 13:48:11  hd
        - Linux extrahiert
        - fileio.inc fuer systemunabhaengige Routinen
        - Linux: IsPath, era, erase_mask vereinfacht

}

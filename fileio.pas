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

{ File-I/O, Locking und Dateinamenbearbeitung }

{$I XPDEFINE.INC }

unit fileio;

interface

uses
  xpglobal, dos, typeform, lfn;

const FMRead       = $00;     { Konstanten fÅr Filemode }
      FMWrite      = $01;
      FMRW         = $02;
      FMDenyNone   = $40;
      FMDenyRead   = $30;
      FMDenyWrite  = $20;
      FMDenyBoth   = $10;
      FMCompatible = $00;

const
  { Neue AnyFile-Konstante, da $3F oft nicht lÑuft }
  ffAnyFile = $20;

type  TExeType = (ET_Unknown, ET_DOS, ET_Win16, ET_Win32,
                  ET_OS2_16, ET_OS2_32, ET_ELF);


function  AddDirSepa(p: pathstr): pathstr; { Verz.-Trenner anhaengen }
Function  exist(const n:string):boolean;         { Datei vorhanden ?       }
Function  existf(var f):boolean;                { Datei vorhanden ?       }
Function  existrf(var f):boolean;               { D.v. (auch hidden etc.) }
function  existBin(const fn: pathstr): boolean;	{ Datei vorhanden (PATH)  }
Function  ValidFileName(const name:PathStr):boolean;  { gÅltiger Dateiname ?    }
Function  IsPath(name:PathStr):boolean;         { Pfad vorhanden ?        }
function  TempFile(const path:pathstr):pathstr;       { TMP-Namen erzeugen      }
function  TempExtFile(const path,ld,ext:pathstr):pathstr; { Ext-Namen erzeugen }
function  _filesize(const fn:pathstr):longint;        { Dateigrî·e in Bytes     }
function  filetime(const fn:pathstr):longint;         { Datei-Timestamp         }
procedure setfiletime(const fn:pathstr; newtime:longint);  { Dateidatum setzen  }
function  copyfile(const srcfn, destfn:pathstr):boolean; { Datei kopieren }
Procedure era(const s:string);                        { Datei lîschen           }
procedure erase_mask(const s:string);                 { Datei(en) lîschen       }
Procedure erase_all(path:pathstr);              { Lîschen mit Subdirs     }
function  _rename(const n1,n2:pathstr):boolean;       { Lîschen mit $I-         }
Procedure MakeBak(const n,newext:string);             { sik anlegen             }
procedure MakeFile(const fn:pathstr);                 { Leerdatei erzeugen      }
procedure mklongdir(path:pathstr; var res:integer);  { mehrere Verz. anl. }
function  diskfree(drive:byte):longint;         { 2-GB-Problem umgehen    }
function  exetype(const fn:pathstr):TExeType;

procedure fm_ro;                                { Filemode ReadOnly       }
procedure fm_rw;                                { Filemode Read/Write     }
procedure resetfm(var f:file; fm:byte);         { mit spez. Filemode îffn.}
function  FileLock(var datei:file; from,size:longint):boolean;
procedure FileUnLock(var datei:file; from,size:longint);

procedure addext(var fn:pathstr; ext:extstr);
procedure adddir(var fn:pathstr; dir:dirstr);
function  GetFileDir(const p:pathstr):dirstr;
function  GetFileName(const p:pathstr):string;
function  GetBareFileName(const p:pathstr):string;    { Filename ohne .ext }
function  GetFileExt(const p:pathstr):string;         { Extension *ohne* "." }
procedure WildForm(var s: pathstr);              { * zu ??? erweitern }

function  ioerror(i:integer; otxt:atext):atext; { Fehler-Texte            }
procedure WriteBatch(const s:string);                 { Batchfile erstellen     }

implementation  { ------------------------------------------------------- }

uses
  xp0;

const
  PathSepaChar          = ';';

var
  ShareDa : boolean;

{ Haengt einen fehlenden Verzeichnisseparator an.
  Loest dabei C: auf (nur Nicht-Unix }
function  AddDirSepa(p: pathstr): pathstr;
var
  cwd: pathstr;
begin
  if p='' then
    AddDirSepa:= ''
  else begin
    if LastChar(p)<>DirSepa then
    begin
      if (length(p)=2) and (p[2]=':') then
      begin	{ Nur C: ? }
        p:= UStr(p);
        getdir(Ord(p[1])-64,cwd);		{ -> Akt. Verz. ermitteln }
	AddDirSepa:= AddDirSepa(cwd);
      end else
        AddDirSepa:= p+DirSepa;
    end
    else
      AddDirSepa:= p;
  end;
end;

{ Sucht die Datei 'fn' in folgender Reihenfolge:
  - Aktuelle Verzeichnis
  - Startverzeichnis der aktuellen Programmdatei
  - Environment-Var PATH
}
function  existBin(const fn: pathstr): boolean;
var
  envpath: string;			{ Opps, bug in brain. PATH kann > 256 sein }
  filename, path: PathStr;
  i, j, k: integer;
begin
  filename:= GetFileName(fn);		{ Evtl. Pfad ignorieren }
  if exist(fn) then begin		{ -> Aktuelles Verzeichnis }
    existBin:= true;
    exit;
  end;
  path:= ProgPath;			{ -> Startverzeichnis }
  if path<>'' then begin
    if exist(AddDirSepa(path)+filename) then begin
      existBin:= true;
      exit;
    end;
  end;
  envpath:= dos.getenv('PATH');
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
  if envpath<>'' then begin		{ Noch was ueber ? }
    if exist(AddDirSepa(envpath)+filename) then
      existBin:= true
    else
      existBin:= false;
  end else
    existBin:= false;
end;

function exist(const n:string):boolean;
var
  sr : searchrec;
  ex : boolean;
begin
  FindFirst(n,anyfile-volumeid-directory,sr);
  ex:=(doserror=0);
  while not ex and (doserror=0) do
  begin
    FindNext(sr);
    ex:=(doserror=0);
  end;
  FindClose(sr);
  exist:=ex;
end;

Function existf(var f):Boolean;
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


Function existrf(var f):Boolean;
var a : rtlword;
    e : boolean;
begin
  getfattr(f,a);
  setfattr(f,archive);
  e:=existf(f);
  setfattr(f,a);
  a:=ioresult;
  existrf:=e;
end;

Function ValidFileName(const name:PathStr):boolean;
var f : file;
begin
  if (name='') or multipos('*?/',name) then  { Fehler in DR-DOS 5.0 umgehen }
    ValidFileName:=false
  else begin
    assign(f,name);
    if existf(f) then ValidFileName:=true
    else begin
      rewrite(f);
      close(f);
      erase(f);
      ValidFileName:=(ioresult=0);
    end;
  end;
end;


Function IsPath(name:PathStr):boolean;         { Pfad vorhanden ? }
var sr : searchrec;
begin
  name:=trim(name);
  if multipos('?*',name) or (trim(name)='') then
    IsPath:=false
  else begin
    if (name='\') or (name[length(name)]=':') or (right(name,2)=':\')
    then begin
      findfirst(name+'*.*',ffAnyFile,sr);
      if doserror=0 then
        IsPath:=true
      else
        IsPath:=validfilename(name+'1$2$3.xx');
      findclose(sr);
    end
    else
    begin
      if name[length(name)]='\' then
        dellast(name);
      findfirst(name,Directory,sr);
      IsPath:=(doserror=0) and (sr.attr and directory<>0);
      findclose(sr);
    end;
  end;
end;

function copyfile(const srcfn, destfn:pathstr):boolean;  { Datei kopieren }
{ keine öberprÅfung, ob srcfn existiert oder destfn bereits existiert }
var bufs,rr:word;
    buf:pointer;
    f1,f2:file;
begin
  bufs := GetMaxMem(buf, 1024, 32768);
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

Procedure era(const s:string);
var f : file;
begin
  assign(f,s);
  erase(f);
end;


procedure erase_mask(const s:string);                 { Datei(en) lîschen }
var sr : searchrec;
begin
  findfirst(s,ffAnyfile,sr);
  while doserror=0 do begin
    era(getfiledir(s)+sr.name);
    findnext(sr);
  end;
  FindClose(sr);
end;

{ path: Pfad mit '\' bzw. '/' am Ende! }

procedure erase_all(path:pathstr);
var sr : searchrec;
    f  : file;
    er : integer;
begin
  { Auf keinen Fall das XP-Verzeichnis lîschen! }
  Findfirst(path+'xp.ovr',anyfile-VolumeID,sr);
  FindClose(sr);
  er:=doserror;
  { xp.ovr gefunden, dann wahrscheinlich im XP-Verzeichnis! }
  if (er=0) then exit;
  { Oops, XPVerzeichnis erwischt! }
  if (ownpath=path) then exit;
  { Oops, Rootverzeichnis erwischt! }
  if ((path='\') or (path='/')) then exit;

  findfirst(path+WildCard,anyfile-VolumeID,sr);
  while (doserror=0) do begin
    with sr do
      if (name[1]<>'.') then
        if attr and Directory<>0 then
          erase_all(path+name+DirSepa)
        else begin
          assign(f,path+name);
          if attr and (ReadOnly+Hidden+Sysfile)<>0 then setfattr(f,0);
          erase(f);
        end;
    findnext(sr);
  end;
  FindClose(sr);
  if cpos(DirSepa,path)<length(path) then begin
    dellast(path);
    rmdir(path);
  end;
end;

Procedure MakeBak(const n,newext:string);
var bakname : string;
    f       : file;
    dir     : dirstr;
    name    : string;
    ext     : extstr;
begin
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

function ioerror(i:integer; otxt:atext):atext;
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


procedure WriteBatch(const s:string);
var
  f:text;
  io:integer;
begin
  assign(f, TempBatchFN);
  rewrite(f);
  io:=ioresult;
  if (io=0) then begin
    writeln(f,'@echo off');
    writeln(f,s);
    close(f);
  end;
  io:=ioresult; { Muss das doppelt sein? (hd) }
  io:=ioresult;
end;

{ res:  0 = Pfad bereits vorhanden }
{       1 = Pfad angelegt          }
{     < 0 = IO-Fehler              }

procedure mklongdir(path:pathstr; var res:integer);
const testfile = 'test0000.$$$';
var p : byte;
begin
  path:=trim(path);
  if path='' then begin
    res:=0;
    exit;
  end;
  if right(path,1)<>DirSepa then path:=path+DirSepa;
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
        if not IsPath(left(path,p)) then begin
          mkdir(left(path,p-1));
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

function TempFile(const path:pathstr):pathstr;       { TMP-Namen erzeugen }
var n : string[12];
begin
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not exist(path+n);
  TempFile:=path+n;
end;

function TempExtFile(const path,ld,ext:pathstr):pathstr;  { Ext-Namen erzeugen }
{ ld max. 4 Zeichen, ext mit Punkt '.bat' }
var n : string[MaxLenFilename];
begin
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not exist(path+n);
  TempExtFile:=path+n;
end;


function _filesize(const fn:pathstr):longint;
var sr : searchrec;
begin
  findfirst(fn,ffAnyFile,sr);
  if doserror<>0 then
    _filesize:=0
  else
    _filesize:=sr.size;
  FindClose(sr);
end;

procedure MakeFile(const fn:pathstr);
var t : text;
begin
  assign(t,fn);
  rewrite(t);
  if ioresult=5 then
    setfattr(t,0)
  else
    close(t);
end;

function filetime(const fn:pathstr):longint;
var sr : searchrec;
begin
  findfirst(fn,ffAnyFile,sr);
  if doserror=0 then
    filetime:=sr.time
  else
    filetime:=0;
  FindClose(sr);
end;

procedure setfiletime(const fn:pathstr; newtime:longint);  { Dateidatum setzen }
var f : file;
begin
  assign(f,fn);
  reset(f,1);
  setftime(f,newtime);
  close(f);
  if ioresult<>0 then;
end;

function GetFileDir(const p:pathstr):dirstr;
var d : dirstr;
    n : string;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileDir:=d;
end;

function GetFileName(const p:pathstr):string;
var d : dirstr;
    n : string;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileName:=n+e;
end;

function GetBareFileName(const p:pathstr):string;
var d : dirstr;
    n : string;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetBareFileName:=n;
end;

function GetFileExt(const p:pathstr):string;
var d : dirstr;
    n : string;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileExt:=mid(e,2);
end;

function _rename(const n1,n2:pathstr):boolean;
var f : file;
begin
  assign(f,n1);
  rename(f,n2);
  _rename:=(ioresult=0);
end;

{ Extension anhÑngen, falls noch nicht vorhanden }

procedure addext(var fn:pathstr; ext:extstr);
var dir  : dirstr;
    name : string;
    _ext : extstr;
begin
  fsplit(fn,dir,name,_ext);
  if _ext='' then fn:=dir+name+'.'+ext;
end;

{ Verzeichnis einfÅgen, falls noch nicht vorhanden }

procedure adddir(var fn: pathstr; dir:dirstr);
var _dir : dirstr;
    name : string;
    ext  : extstr;
begin
  fsplit(fn,_dir,name,ext);
  if _dir='' then begin
    if dir[length(dir)]<>DirSepa then dir:=dir+DirSepa;
    insert(dir,fn,1);
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

function FileLock(var datei:file; from,size:longint):boolean;
var
  regs : registers;
begin
  if Shareda then
    with regs do
    begin
      ax:=$5c00;
      bx:=filerec(datei).handle;
      cx:=from shr 16; dx:=from and $ffff;
      si:=size shr 16; di:=size and $ffff;
      msdos(regs);
      FileLock:=flags and fcarry = 0;
    end else
      FileLock:=true;
end;

procedure FileUnLock(var datei:file; from,size:longint);
var
  regs : registers;
begin
  if shareda then
  with regs do
  begin
    ax:=$5c01;
    bx:=filerec(datei).handle;
    cx:=from shr 16; dx:=from and $ffff;
    si:=size shr 16; di:=size and $ffff;
    msdos(regs);
  end;
end;

procedure TestShare;
var
  regs : registers;
begin
  { Installcheck fÅr Share }
  with regs do
  begin
    fillchar(regs, sizeof(regs), 0);
    ah:=$10;
    intr($2f, regs);
    ShareDa := al = $ff;
  end;
end;

procedure resetfm(var f:file; fm:byte);
var fm0 : byte;
begin
  fm0:=filemode;
  filemode:=fm;
  reset(f,1);
  filemode:=fm0;
end;

procedure WildForm(var s: pathstr);
var dir : dirstr;
    name: string;
    ext : extstr;
    p   : byte;
begin
  fsplit(s,dir,name,ext);
  p:=cpos('*',name);
   if p>0 then name:=left(name,p-1)+typeform.dup(9-p,'?');
  p:=cpos('*',ext);
   if p>0 then ext:=left(ext,p-1)+typeform.dup(5-p,'?');
  s:=dir+name+ext;
end;

{ Zwei diskfree/disksize-Probleme umgehen:                   }
{                                                            }
{ - bei 2..4 GB liefern diskfree und disksize negative Werte }
{ - bei bestimmten Cluster/Sektorgrî·en-Kombinationen        }
{   liefern diskfree und disksize falsche Werte              }
{ Unter FPC gibt es eine gleichlautende Procedure in der Unit DOS }
function diskfree(drive:byte):longint;
var l,ll : longint;
    regs : registers;
begin
  regs.ah := $36;
  regs.dl := drive;
  msdos(regs);
  if regs.ax=$ffff then
    l:=0
  else begin
    l:=longint(regs.ax)*regs.bx;   { Secs/Cluster * Free Clusters }
    if regs.cx>=512 then
      ll:=(l div 2)*(regs.cx div 512)
    else
      ll:=(l div 1024)*regs.cx;
    if ll>=2097152 then l:=maxlongint
    else l:=l*regs.cx;
  end;
  diskfree:=l;
end;

function exetype(const fn:pathstr):TExeType;
var f       : file;
    magic   : array[0..1] of char;
    magic2  : array[0..2] of char;
    hdadr   : longint;
    version : byte;
begin
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
  begin { MK 01/00 Fix fÅr LZEXE gepackte Dateien }
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

begin
  TestShare;
end.
{
  $Log$
  Revision 1.41.2.7  2000/12/12 11:30:26  mk
  - FindClose hinzugefuegt

  Revision 1.41.2.6  2000/11/18 22:11:26  mk
  - einige Dirname, extname, pathname in string geaendert

  Revision 1.41.2.5  2000/11/10 11:30:41  mk
  - fixed Bug #116292: Mehrfachstart von XP abfangen

  Revision 1.41.2.4  2000/10/19 12:55:20  mk
  - const-Parameter sparen 600 Byte in der EXE-Datei ;-)

  Revision 1.41.2.3  2000/08/28 23:14:59  mk
  - Unit LFN als letze Unit in Uses eingetragen, um FindFirst/FindNext usw. LFN-faehig zu machen; das muss bei den anderen Units noch nachgeholt werden

  Revision 1.41.2.2  2000/08/22 17:45:37  mk

  - Test auf Share entfernt

  Revision 1.41.2.1  2000/07/01 11:17:26  mk
  - 32 Bit Teile entfernt

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

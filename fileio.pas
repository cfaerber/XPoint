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
{$IFDEF Ver32 }
  sysutils,
{$ENDIF }
{$ifdef Linux }
  xplinux,
{$endif }
{$ifdef vp }
  vpusrlow,
{$else}
 {$IFDEF Win32 }
  windows,
 {$ENDIF }
{$endif}
  xpglobal,
  dos, typeform;

{$ifdef vp }
const FMRead       = fmOpenRead;     { Konstanten fÅr Filemode }
      FMWrite      = fmOpenWrite;
      FMRW         = fmOpenReadWrite;
      FMDenyNone   = fmShareDenyNone;
      FMDenyRead   = fmShareDenyRead;
      FMDenyWrite  = fmShareDenyWrite;
      FMDenyBoth   = fmShareExclusive;
      FMCompatible = fmShareCompat;
{$else}
const FMRead       = $00;     { Konstanten fÅr Filemode }
      FMWrite      = $01;
      FMRW         = $02;
      FMDenyNone   = $40;
      FMDenyRead   = $30;
      FMDenyWrite  = $20;
      FMDenyBoth   = $10;
      FMCompatible = $00;
{$endif}

const
  { Neue AnyFile-Konstante, da $3F oft nicht lÑuft }
  ffAnyFile = $20;

type  TExeType = (ET_Unknown, ET_DOS, ET_Win16, ET_Win32,
                  ET_OS2_16, ET_OS2_32, ET_ELF);


Function  exist(n:string):boolean;              { Datei vorhanden ?       }
Function  existf(var f):boolean;                { Datei vorhanden ?       }
Function  existrf(var f):boolean;               { D.v. (auch hidden etc.) }
Function  ValidFileName(name:PathStr):boolean;  { gÅltiger Dateiname ?    }
Function  IsPath(name:PathStr):boolean;         { Pfad vorhanden ?        }
function  TempFile(path:pathstr):pathstr;       { TMP-Namen erzeugen      }
function  TempExtFile(path,ld,ext:pathstr):pathstr; { Ext-Namen erzeugen }
function  _filesize(fn:pathstr):longint;        { Dateigrî·e in Bytes     }
function  filetime(fn:pathstr):longint;         { Datei-Timestamp         }
procedure setfiletime(fn:pathstr; newtime:longint);  { Dateidatum setzen  }
function  copyfile(srcfn, destfn:pathstr):boolean; { Datei kopieren }
Procedure era(s:string);                        { Datei lîschen           }
procedure erase_mask(s:string);                 { Datei(en) lîschen       }
Procedure erase_all(path:pathstr);              { Lîschen mit Subdirs     }
function  _rename(n1,n2:pathstr):boolean;       { Lîschen mit $I-         }
Procedure MakeBak(n,newext:string);             { sik anlegen             }
procedure MakeFile(fn:pathstr);                 { Leerdatei erzeugen      }
procedure mklongdir(path:pathstr; var res:integer);  { mehrere Verz. anl. }

{$IFDEF BP }
function  diskfree(drive:byte):longint;         { 2-GB-Problem umgehen    }
{$ENDIF }
function  exetype(fn:pathstr):TExeType;

procedure fm_ro;                                { Filemode ReadOnly       }
procedure fm_rw;                                { Filemode Read/Write     }
procedure resetfm(var f:file; fm:byte);         { mit spez. Filemode îffn.}
function  lock(var datei:file; from,size:longint):boolean;
procedure unlock(var datei:file; from,size:longint);
function  lockfile(var datei:file):boolean;
procedure unlockfile(var datei:file);

procedure addext(var fn:pathstr; ext:extstr);
procedure adddir(var fn:pathstr; dir:dirstr);
function  GetFileDir(p:pathstr):dirstr;
function  GetFileName(p:pathstr):string;
function  GetBareFileName(p:pathstr):string;    { Filename ohne .ext }
function  GetFileExt(p:pathstr):string;         { Extension *ohne* "." }
procedure WildForm(var s: pathstr);              { * zu ??? erweitern }

function  ioerror(i:integer; otxt:atext):atext; { Fehler-Texte            }
procedure WriteBatch(s:string);                 { Batchfile erstellen     }

implementation  { ------------------------------------------------------- }

uses
{$ifdef linux}
  linux,
{$endif}
  xp0;

{$IFDEF BP }
var
  ShareDa : boolean;
{$ENDIF }

function exist(n:string):boolean;
{$IFDEF Ver32  }
begin
{$IFDEF UnixFS}
  Exist:= FileExists(ResolvePathName(n));
{$ELSE }
  Exist := FileExists(n);
{$ENDIF }
end;
{$ELSE }
var sr : searchrec;
    ex : boolean;
begin
  findfirst(n,anyfile-volumeid-directory,sr);
  ex:=(doserror=0);
  while not ex and (doserror=0) do begin
    findnext(sr);
    ex:=(doserror=0);
  end;
  exist:=ex;
end;
{$ENDIF }

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

Function ValidFileName(name:PathStr):boolean;
var f : file;
begin
{$IFDEF UnixFS }
  if (name='') or multipos('*?&',name) then
{$ELSE }
  if (name='') or multipos('*?/',name) then  { Fehler in DR-DOS 5.0 umgehen }
{$ENDIF }
    ValidFileName:=false
  else begin
{$IFDEF UnixFS}
    assign(f, ResolvePathName(name));		{ ~/ aufloesen }
{$ELSE}
    assign(f,name);
{$ENDIF}
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
{$IFDEF UnixFS }
    name:= ResolvePathName(name);
    { Laufwerksbuchstaben gibt es nicht, aber immer das
      Wurzelverzeichnis }
    if (name=DirSepa) then
      IsPath:= true
    else if (name[length(name)]=DirSepa) then
      dellast(name);
    findfirst(name,Directory,sr);
    IsPath:=(doserror=0) and (sr.attr and directory<>0);
{$ELSE }
    if (name='\') or (name[length(name)]=':') or (right(name,2)=':\')
    then begin
      findfirst(name+'*.*',ffAnyFile,sr);
      if doserror=0 then
        IsPath:=true
      else
        IsPath:=validfilename(name+'1$2$3.xx');
    end
    else begin
      if name[length(name)]='\' then
        dellast(name);
      findfirst(name,Directory,sr);
      IsPath:=(doserror=0) and (sr.attr and directory<>0);
    end;
{$ENDIF }
{$ifdef ver32 }
    findclose(sr);
{$endif}
  end;
end;

function copyfile(srcfn, destfn:pathstr):boolean;  { Datei kopieren }
{ keine öberprÅfung, ob srcfn existiert oder destfn bereits existiert }
var bufs,rr:word;
    buf:pointer;
    f1,f2:file;
begin
  bufs:=min(maxavail,65520);
  getmem(buf,bufs);
{$IFDEF UnixFS}
  assign(f1,ResolvePathName(srcfn));
  assign(f2,ResolvePathName(destfn));
{$ELSE }
  assign(f1,srcfn);
  assign(f2,destfn);
{$ENDIF }
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

Procedure era(s:string);
var f : file;
begin
{$IFDEF UnixFS}
  assign(f,ResolvePathName(s));
{$ELSE}
  assign(f,s);
{$ENDIF}
  erase(f);
end;


procedure erase_mask(s:string);                 { Datei(en) lîschen }
var sr : searchrec;
begin
{$IFDEF UnixFS}
  findfirst(ResolvePathName(s),ffAnyFile,sr);
{$ELSE}
  findfirst(s,ffAnyfile,sr);
{$ENDIF}
  while doserror=0 do begin
    era(getfiledir(s)+sr.name);
    findnext(sr);
  end;
  {$IFDEF Ver32 }
  FindClose(sr);
  {$ENDIF}
end;

{ path: Pfad mit '\' bzw. '/' am Ende! }

procedure erase_all(path:pathstr);
var sr : searchrec;
    f  : file;
begin
{$IFDEF UnixFS}
  path:= ResolvePathName(path);
{$ENDIF}
  findfirst(path+WildCard,anyfile-VolumeID,sr);
  while doserror=0 do begin
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
  {$IFDEF Ver32}
  FindClose(sr);
  {$ENDIF}
  if cpos(DirSepa,path)<length(path) then begin
    dellast(path);
    rmdir(path);
  end;
end;

Procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
    dir     : dirstr;
    name    : namestr;
    ext     : extstr;
begin
{$IFDEF UnixFS}
  n:= ResolvePathName(n);
{$ENDIF}
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


procedure WriteBatch(s:string);
var
  f:text;
  io:integer;
begin
  assign(f, TempBatchFN);
  rewrite(f);
  io:=ioresult;
  if (io=0) then begin
{$IFDEF UnixFS }
    writeln(f,'#!',getenv('SHELL'));
    writeln(f,'#');
    writeln(f,'# This script was generated by ',xp_xp,'.');
    writeln(f,'# Feel free to delete it!');
    writeln(f,'#');
{$ELSE }
    writeln(f,'@echo off');
{$ENDIF }
    writeln(f,s);
    close(f);
{$IFDEF UnixFS }
    SetAccess(TempBatchFN, taUserRWX);		{ Ausfuehrbar machen }
{$ENDIF }
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
{$IFDEF UnixFS}
  path:=ResolvePathName(trim(path));
{$ELSE}
  path:=trim(path);
{$ENDIF}
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

function TempFile(path:pathstr):pathstr;       { TMP-Namen erzeugen }
var n : string[12];
begin
  repeat
    n:=formi(random(10000),4)+'.tmp'
  until not exist(path+n);
{$IFDEF UnixFS}
  TempFile:=ResolvePathName(path+n);
{$ELSE}
  TempFile:=path+n;
{$ENDIF}
end;

function TempExtFile(path,ld,ext:pathstr):pathstr;  { Ext-Namen erzeugen }
{ ld max. 4 Zeichen, ext mit Punkt '.bat' }
var n : string[MaxLenFilename];
begin
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not exist(path+n);
{$IFDEF UnixFS}
  TempExtFile:=ResolvePathName(path+n);
{$ELSE }
  TempExtFile:=path+n;
{$ENDIF }
end;


function _filesize(fn:pathstr):longint;
var sr : searchrec;
begin
{$IFDEF UnixFS}
  findfirst(ResolvePathName(fn),ffAnyFile,sr);
{$ELSE}
  findfirst(fn,ffAnyFile,sr);
{$ENDIF}
  if doserror<>0 then
    _filesize:=0
  else
    _filesize:=sr.size;
  {$ifdef ver32 }
  findclose(sr);
  {$endif}
end;

procedure MakeFile(fn:pathstr);
var t : text;
begin
{$IFDEF UnixFS}
  assign(t,ResolvePathName(fn));
{$ELSE}
  assign(t,fn);
{$ENDIF}
  rewrite(t);
  if ioresult=5 then
    setfattr(t,0)
  else
    close(t);
end;

function filetime(fn:pathstr):longint;
var sr : searchrec;
begin
{$IFDEF UnixFS}
  findfirst(ResolvePathName(fn),ffAnyFile,sr);
{$ELSE}
  findfirst(fn,ffAnyFile,sr);
{$ENDIF}
  if doserror=0 then
    filetime:=sr.time
  else
    filetime:=0;
  {$ifdef ver32 }
  findclose(sr);
  {$endif}
end;

procedure setfiletime(fn:pathstr; newtime:longint);  { Dateidatum setzen }
var f : file;
begin
{$IFDEF UnixFS}
  assign(f,ResolvePathName(fn));
{$ELSE}
  assign(f,fn);
{$ENDIF}
  reset(f,1);
  setftime(f,newtime);
  close(f);
  if ioresult<>0 then;
end;

function GetFileDir(p:pathstr):dirstr;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
{$IFDEF UnixFS}
  fsplit(ResolvePathName(p),d,n,e);
{$ELSE}
  fsplit(p,d,n,e);
{$ENDIF}
  GetFileDir:=d;
end;

function GetFileName(p:pathstr):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
{$IFDEF UnixFS}
  fsplit(ResolvePathName(p),d,n,e);
{$ELSE}
  fsplit(p,d,n,e);
{$ENDIF}
  GetFileName:=n+e;
end;

function GetBareFileName(p:pathstr):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
{$IFDEF UnixFS}
  fsplit(ResolvePathName(p),d,n,e);
{$ELSE}
  fsplit(p,d,n,e);
{$ENDIF}
  GetBareFileName:=n;
end;

function GetFileExt(p:pathstr):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
{$IFDEF UnixFS}
  fsplit(ResolvePathName(p),d,n,e);
{$ELSE}
  fsplit(p,d,n,e);
{$ENDIF}
  GetFileExt:=mid(e,2);
end;

function _rename(n1,n2:pathstr):boolean;
var f : file;
begin
{$IFDEF UnixFS}
  assign(f,ResolvePathName(n1));
  rename(f,ResolvePathName(n2));
{$ELSE}
  assign(f,n1);
  rename(f,n2);
{$ENDIF}
  _rename:=(ioresult=0);
end;

{ Extension anhÑngen, falls noch nicht vorhanden }

procedure addext(var fn:pathstr; ext:extstr);
var dir  : dirstr;
    name : namestr;
    _ext : extstr;
begin
{$IFDEF UnixFS}
  fn:=ResolvePathName(fn);
{$ENDIF}
  fsplit(fn,dir,name,_ext);
  if _ext='' then fn:=dir+name+'.'+ext;
end;

{ Verzeichnis einfÅgen, falls noch nicht vorhanden }

procedure adddir(var fn: pathstr; dir:dirstr);
var _dir : dirstr;
    name : namestr;
    ext  : extstr;
begin
{$IFDEF UnixFS}
  fn:=ResolvePathName(fn);
{$ENDIF}
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

{$IFDEF FPC }
  { Wir wissen, was Hi/Lo bei Longint zurÅckliefert }
  {$WARNINGS OFF }
{$ENDIF }

function lock(var datei:file; from,size:longint):boolean;
{$IFDEF ver32 }
begin
  {$ifdef vp }
  lock:=SysLockFile(datei,from,size)=0;
  {$else}
  {$ifdef win32}
  (* Aus unbekannten GrÅnden funktioniert das ganze unter Windows 95
     nicht, wohl aber unter Windows NT
    Lock := Windows.LockFile(FileRec(Datei).Handle,
    Lo(From), Hi(From), Lo(Size), Hi(Size)) *)
  Lock := true;
  {$endif}
  {$ifdef UnixFS}                     { Filelocking f¸r Linux }
  lock := flock (datei, LOCK_SH);
  {$endif}
{$ENDIF}
{$ELSE }
var regs : registers;
begin
  if Shareda then with regs do begin
    ax:=$5c00;
    bx:=filerec(datei).handle;
    cx:=from shr 16; dx:=from and $ffff;
    si:=size shr 16; di:=size and $ffff;
    msdos(regs);
    lock:=flags and fcarry = 0;
  end
  else
    lock:=true;
{$ENDIF }
end;

procedure unlock(var datei:file; from,size:longint);
{$IFDEF ver32 }
begin
 {$ifdef vp }
  if SysUnLockFile(datei,from,size)=0 then ;
 {$else}
  {$ifdef win32}
  Windows.UnLockFile(FileRec(Datei).Handle,
    Lo(From), Hi(From), Lo(Size), Hi(Size));
  {$endif}
  {$ifdef UnixFS}                 { ML 25.03.2000    Filelocking f¸r Linux }
  flock(Datei, LOCK_UN);
  {$endif}
 {$endif}
{$ELSE }
var regs : registers;
begin
  if shareda then with regs do begin
    ax:=$5c01;
    bx:=filerec(datei).handle;
    cx:=from shr 16; dx:=from and $ffff;
    si:=size shr 16; di:=size and $ffff;
    msdos(regs);
  end;
{$ENDIF }
end;

{$IFDEF FPC }
  { Wir wissen, was Hi/Lo bei Longint zurÅckliefert }
  {$WARNINGS ON }
{$ENDIF }

function lockfile(var datei:file):boolean;
begin
  lockfile:=lock(datei,0,maxlongint);
end;

procedure unlockfile(var datei:file);
begin
  unlock(datei,0,maxlongint);
end;


procedure TestShare;
{$IFDEF Ver32 }
begin
end;
{$ELSE}
var regs : registers;
begin
  fillchar(regs,sizeof(regs),0);
  with regs do begin
    ax:=$5c00;
    di:=1;
    msdos(regs);
    if flags and fcarry=0 then begin
      ax:=$5c01;
      msdos(regs);
    end;
    ShareDa:=(ax<>1);
  end;
  { Weiterer Installcheck fÅr Share, um Probleme mit einem Plain
    DR-DOS zu umgehen }
  with regs do
  begin
    ax:=$1000;
    intr($2f, regs);
    if al <> $ff then ShareDa := false;
  end;
end;
{$ENDIF}

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
    name: namestr;
    ext : extstr;
    p   : byte;
begin
{$IFNDEF UnixFS }
  fsplit(s,dir,name,ext);
  p:=cpos('*',name);
   if p>0 then name:=left(name,p-1)+typeform.dup(9-p,'?');
  p:=cpos('*',ext);
   if p>0 then ext:=left(ext,p-1)+typeform.dup(5-p,'?');
  s:=dir+name+ext;
{$ENDIF }
end;

{ Zwei diskfree/disksize-Probleme umgehen:                   }
{                                                            }
{ - bei 2..4 GB liefern diskfree und disksize negative Werte }
{ - bei bestimmten Cluster/Sektorgrî·en-Kombinationen        }
{   liefern diskfree und disksize falsche Werte              }
{ Unter FPC gibt es eine gleichlautende Procedure in der Unit DOS }
{$IFDEF BP }
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
{$ENDIF }

function exetype(fn:pathstr):TExeType;
var f       : file;
    magic   : array[0..1] of char;
    magic2  : array[0..2] of char;
    hdadr   : longint;
    version : byte;
begin
{$IFDEF UnixFS}
  fn:= ResolvePathName(fn);
{$ENDIF}
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

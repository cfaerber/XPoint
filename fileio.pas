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
{$IFNDEF Delphi }
  dos,
{$ELSE }
  sysutils,
{$ENDIF }
  xpglobal, typeform;

const FMRead       = $00;     { Konstanten fÅr Filemode }
      FMWrite      = $01;
      FMRW         = $02;
      FMDenyNone   = $40;
      FMDenyRead   = $30;
      FMDenyWrite  = $20;
      FMDenyBoth   = $10;
      FMCompatible = $00;

type  TExeType = (ET_Unknown, ET_DOS, ET_Win16, ET_Win32,
                  ET_OS2_16, ET_OS2_32);


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
procedure setfileattr(fn:pathstr; attr:word);   { Dateiattribute setzen   }
function  copyfile(srcfn, destfn:pathstr):boolean; { Datei kopieren }
Procedure era(s:string);                        { Datei lîschen           }
procedure erase_mask(s:string);                 { Datei(en) lîschen       }
Procedure erase_all(path:pathstr);              { Lîschen mit Subdirs     }
function  _rename(n1,n2:pathstr):boolean;       { Lîschen mit $I-         }
procedure move_mask(source,dest:pathstr; var res:integer);
Function  ReadOnlyHidden(name:PathStr):boolean; { Datei Read Only ?       }
Procedure MakeBak(n,newext:string);             { sik anlegen             }
procedure MakeFile(fn:pathstr);                 { Leerdatei erzeugen      }
procedure mklongdir(path:pathstr; var res:integer);  { mehrere Verz. anl. }
function  textfilesize(var t:text):longint;     { Grî·e v. offener Textdatei }
function  diskfree(drive:byte):longint;         { 2-GB-Problem umgehen    }
function  disksize(drive:byte):longint;
function  exetype(fn:pathstr):TExeType;

procedure fm_ro;                                { Filemode ReadOnly       }
procedure fm_rw;                                { Filemode Read/Write     }
procedure fm_all;                               { Deny none               }
procedure resetfm(var f:file; fm:byte);         { mit spez. Filemode îffn.}
function  ShareLoaded:boolean;                  { Locking verfÅgbar       }
function  lock(var datei:file; from,size:longint):boolean;
procedure unlock(var datei:file; from,size:longint);
function  lockfile(var datei:file):boolean;
procedure unlockfile(var datei:file);

procedure addext(var fn:pathstr; ext:extstr);
procedure adddir(var fn:pathstr; dir:dirstr);
function  GetFileDir(p:pathstr):dirstr;
function  GetFileName(p:pathstr):string;
function  GetFileExt(p:pathstr):string;         { Extension *ohne* "." }
procedure WildForm(var s: pathstr);              { * zu ??? erweitern }

function  ioerror(i:integer; otxt:atext):atext; { Fehler-Texte            }


implementation  { ------------------------------------------------------- }

var ShareDa : boolean;


Function exist(n:string):boolean;
var sr : searchrec;
    ex : boolean;
begin
  findfirst(n,anyfile-volumeid-directory,sr);
  ex:=(doserror=0);
  while not ex and (doserror=0) do begin
    findnext(sr);
    ex:=(doserror=0);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
  exist:=ex;
end;

Function existf(var f):Boolean;
var
  fm : byte;
begin
  fm:=filemode;
  filemode:=$40;
  reset(file(f));
  existf:=(ioresult=0);
  close(file(f));
  filemode:=fm;
  if ioresult = 0 then ;
end;


Function existrf(var f):Boolean;
var a : smallword;
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
      findfirst(name+'*.*',AnyFile,sr);
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

Procedure era(s:string);
var f : file;
begin
  assign(f,s);
  erase(f);
end;


procedure erase_mask(s:string);                 { Datei(en) lîschen }
var sr : searchrec;
begin
  findfirst(s,0,sr);
  while doserror=0 do begin
    era(getfiledir(s)+sr.name);
    findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
end;

{ path: Pfad mit '\' am Ende! }

procedure erase_all(path:pathstr);
var sr : searchrec;
    f  : file;
begin
  findfirst(path+'*.*',anyfile-VolumeID,sr);
  while doserror=0 do begin
    with sr do
      if (name[1]<>'.') then
        if attr and Directory<>0 then
          erase_all(path+name+'\')
        else begin
          assign(f,path+name);
          if attr and (ReadOnly+Hidden+Sysfile)<>0 then setfattr(f,0);
          erase(f);
          end;
    findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
  if pos('\',path)<length(path) then begin
    dellast(path);
    rmdir(path);
    end;
end;

Function ReadOnlyHidden(name:PathStr):boolean;
var f    : file;
    attr : smallword;
begin
  assign(f,name);
  if not existf(f) then ReadOnlyHidden:=false
  else begin
    getfattr(f,attr);
    ReadOnlyHidden:=(attr and (ReadOnly or Hidden))<>0;
    end;
end;

Procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
    dir     : dirstr;
    name    : namestr;
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


{ res:  0 = Pfad bereits vorhanden }
{       1 = Pfad angelegt          }
{     < 0 = IO-Fehler              }

procedure mklongdir(path:pathstr; var res:integer);
const testfile = 'test0000.$$$';
var p : byte;
begin
  path:=trim(path);
  if path='' then begin
    res:=0; exit; end;
  if right(path,1)<>'\' then path:=path+'\';
  if validfilename(path+testfile) then
    res:=0
  else
    if pos('\',path)<=1 then begin
      mkdir(path);
      res:=-ioresult;
      end
    else begin
      p:=iif(path[1]='\',2,1);
      res:=0;
      while (p<=length(path)) do begin
        while (p<=length(path)) and (path[p]<>'\') do inc(p);
        if not IsPath(left(path,p)) then begin
          mkdir(left(path,p-1));
          if inoutres<>0 then begin
            res:=-ioresult; exit;
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
  TempFile:=path+n;
end;

function TempExtFile(path,ld,ext:pathstr):pathstr;  { Ext-Namen erzeugen }
{ ld max. 4 Zeichen, ext mit Punkt '.bat' }
var n : string[12];
begin
  repeat
    n:=ld+formi(random(10000),4)+ext
  until not exist(path+n);
  TempExtFile:=path+n;
end;


function _filesize(fn:pathstr):longint;
var sr : searchrec;
begin
  findfirst(fn,0,sr);
  if doserror<>0 then
    _filesize:=0
  else
    _filesize:=sr.size;
end;

procedure MakeFile(fn:pathstr);
var t : text;
begin
  assign(t,fn);
  rewrite(t);
  if ioresult=5 then
    setfattr(t,0)
  else
    close(t);
end;

function filetime(fn:pathstr):longint;
var sr : searchrec;
begin
  findfirst(fn,AnyFile,sr);
  if doserror=0 then
    filetime:=sr.time
  else
    filetime:=0;
end;

procedure setfiletime(fn:pathstr; newtime:longint);  { Dateidatum setzen }
var f : file;
begin
  assign(f,fn);
  reset(f,1);
  setftime(f,newtime);
  close(f);
  if ioresult<>0 then;
end;

procedure setfileattr(fn:pathstr; attr:word);   { Dateiattribute setzen }
var f : file;
begin
  assign(f,fn);
  setfattr(f,attr);
  if ioresult<>0 then;
end;

function GetFileDir(p:pathstr):dirstr;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileDir:=d;
end;

function GetFileName(p:pathstr):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileName:=n+e;
end;

function GetFileExt(p:pathstr):string;
var d : dirstr;
    n : namestr;
    e : extstr;
begin
  fsplit(p,d,n,e);
  GetFileExt:=mid(e,2);
end;

function _rename(n1,n2:pathstr):boolean;
var f : file;
begin
  assign(f,n1);
  rename(f,n2);
  _rename:=(ioresult=0);
end;


procedure move_mask(source,dest:pathstr; var res:integer);
var sr : searchrec;
begin
  res:=0;
  if lastchar(dest)<>'\' then
    dest:=dest+'\';
  findfirst(source,0,sr);
  while doserror=0 do begin
    if not _rename(getfiledir(source)+sr.name,dest+sr.name) then
      inc(res);
    findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
end;

{ Extension anhÑngen, falls noch nicht vorhanden }

procedure addext(var fn:pathstr; ext:extstr);
var dir  : dirstr;
    name : namestr;
    _ext : extstr;
begin
  fsplit(fn,dir,name,_ext);
  if _ext='' then fn:=dir+name+'.'+ext;
end;

{ Verzeichnis einfÅgen, falls noch nicht vorhanden }

procedure adddir(var fn: pathstr; dir:dirstr);
var _dir : dirstr;
    name : namestr;
    ext  : extstr;
begin
  fsplit(fn,_dir,name,ext);
  if _dir='' then begin
    if dir[length(dir)]<>'\' then dir:=dir+'\';
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

procedure fm_all;     { Filemode Read/Write }
begin
  filemode:=fmDenyNone+fmRW;
end;


function ShareLoaded:boolean;
begin
  ShareLoaded:=shareda;
end;


function lock(var datei:file; from,size:longint):boolean;
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
end;

procedure unlock(var datei:file; from,size:longint);
var regs : registers;
begin
  if shareda then with regs do begin
    ax:=$5c01;
    bx:=filerec(datei).handle;
    cx:=from shr 16; dx:=from and $ffff;
    si:=size shr 16; di:=size and $ffff;
    msdos(regs);
    end;
end;

function lockfile(var datei:file):boolean;
begin
  lockfile:=lock(datei,0,maxlongint);
end;

procedure unlockfile(var datei:file);
begin
  unlock(datei,9,maxlongint);
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


{ t mu· eine geîffnete Textdatei sein }

function textfilesize(var t:text):longint;
var regs  : registers;
    fplow : word;           { alter Filepointer }
    fphigh: word;
begin
  with regs do begin
    ax:=$4201;              { File Pointer ermitteln }
    bx:=textrec(t).handle;
    cx:=0; dx:=0;
    msdos(regs);
    fphigh:=dx; fplow:=ax;
    ax:=$4202;              { Dateigrî·e ermitteln }
    cx:=0; dx:=0;
    msdos(regs);
    textfilesize:=$10000*dx+ax;
    ax:=$4200;              { alte Position wiederherstellen }
    cx:=fphigh; dx:=fplow;
    msdos(regs);
  end;
end;

procedure WildForm(var s: pathstr);
var dir : dirstr;
    name: namestr;
    ext : extstr;
    p   : byte;
begin
  fsplit(s,dir,name,ext);
  p:=cpos('*',name);
  if p>0 then name:=left(name,p-1)+dup(9-p,'?');
  p:=cpos('*',ext);
  if p>0 then ext:=left(ext,p-1)+dup(5-p,'?');
  s:=dir+name+ext;
end;

{ Zwei diskfree/disksize-Probleme umgehen:                   }
{                                                            }
{ - bei 2..4 GB liefern diskfree und disksize negative Werte }
{ - bei bestimmten Cluster/Sektorgrî·en-Kombinationen        }
{   liefern diskfree und disksize falsche Werte              }

function diskspace(drive:byte; size:boolean):longint;
var l,ll : longint;
    regs : registers;
begin
  regs.ah := $36;
  regs.dl := drive;
  msdos(regs);
  if regs.ax=$ffff then
    l:=0
  else begin
    if size then
      l:=longint(regs.ax)*regs.dx    { Secs/Cluster * Clusters/Disk }
    else
      l:=longint(regs.ax)*regs.bx;   { Secs/Cluster * Free Clusters }
    if regs.cx>=512 then
      ll:=(l div 2)*(regs.cx div 512)
    else
      ll:=(l div 1024)*regs.cx;
    if ll>=2097152 then l:=maxlongint
    else l:=l*regs.cx;
    end;
  diskspace:=l;
end;


function diskfree(drive:byte):longint;
begin
  diskfree:=diskspace(drive,false);
end;


function disksize(drive:byte):longint;
begin
  disksize:=diskspace(drive,true);
end;


function exetype(fn:pathstr):TExeType;
var f       : file;
    magic   : array[0..1] of char;
    hdadr   : longint;
    version : byte;
begin
  assign(f,fn);
  resetfm(f,FMDenyWrite);
  blockread(f,magic,2);
  seek(f,60);
  blockread(f,hdadr,4);
  if (ioresult<>0) or (magic<>'MZ') then
    exetype:=ET_Unknown
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
  Revision 1.7  2000/03/03 20:26:40  rb
  Aufruf externer MIME-Viewer (Win, OS/2) wieder geÑndert

  Revision 1.6  2000/02/23 23:49:47  rb
  'Dummy' kommentiert, Bugfix beim Aufruf von ext. Win+OS/2 Viewern

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
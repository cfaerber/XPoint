{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp2b;

interface

uses crt, xpcfg,
     dos,dosx,typeform,fileio,keys,inout,winxp,mouse,datadef,database,
     databaso,maske,video,help,printerx,lister,win2,maus2,crc,clip,
     resource,montage, xpglobal, xp0,xp1,xp10,xp1o2,xp1input,
     xp1help,xp5,xpdatum,xpeasy, lfn;

procedure testlock;
procedure DelTmpfiles(fn:string);
procedure ReadDefaultViewers;

implementation

uses
  xp1o,xpe,xp3,xp9bp,xp9,xpnt,xpfido,xpkeys,xpreg;

procedure testlock;
var i : integer;
begin
  if ParNolock then exit;
  assign(lockfile, 'LOCKFILE');
  filemode:=FMRW + FMDenyWrite;
  rewrite(lockfile);
  if (ioresult<>0) or not fileio.lockfile(lockfile) then
  begin
    writeln;
    for i:=1 to res2anz(244) do
      writeln(getres2(244,i));
    mdelay(1000);
    close(lockfile);
    if ioresult<>0 then;
    runerror:=false;
    halt(1);
  end;
  lockopen:=true;
  { MK 09.01.00: Bugfix fÅr Mime-Lîschen-Problem von Heiko.Schoenfeld@gmx.de }
  FileMode := FMRW;
end;

procedure DelTmpfiles(fn:string);
var sr : searchrec;
begin
  findfirst(fn,ffAnyFile,sr);
  while doserror=0 do begin
    _era(sr.name);
    findnext(sr);
  end;
end;

procedure ReadDefaultViewers;

  procedure SeekViewer(mimetyp:string; var viewer:pviewer);
  var prog : string[ViewprogLen];
  begin
    dbSeek(mimebase,mtiTyp,ustr(mimetyp));
    if not dbEOF(mimebase) and not dbBOF(mimebase) and
       stricmp(dbReadStr(mimebase,'typ'),mimetyp) then begin
      dbReadN(mimebase,mimeb_programm,prog);
      getmem(viewer,length(prog)+1);   { auch bei prog=''! }
      viewer^:=prog;
      end
    else
      viewer:=nil;
  end;

begin
  SeekViewer('*/*',DefaultViewer);
  SeekViewer('text/*',DefTextViewer);
  SeekViewer('text/plain',PTextViewer);
end;

end.

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

procedure check_date;
procedure ShowDateZaehler;
Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche }

implementation

uses
  xpovl,xp1o,xpe,xp3,xp9bp,xp9,xpnt,xpfido,xpkeys,xpreg;

var
  zaehlx,zaehly : byte;

procedure testlock;
const
  LockString: String = 'Isn''t this a beautiful lockfile?';
var
  i : integer;
  LockDenied: Boolean;
begin
  if ParNolock then exit;
  LockDenied := false;
  assign(lockfile, 'LOCKFILE');
  filemode:=FMWrite + FMDenyBoth;
  rewrite(lockfile, 1);
  if IOResult <> 0 then
    LockDenied := true
  else
  begin
    BlockWrite(lockfile, LockString[1], Length(LockString));
    if IOResult <> 0 then
      LockDenied := true
    else
    begin
      Close(lockfile);
      Reset(lockfile, 1);
      if IOResult <> 0 then
        LockDenied := true
      else
        if (not FileLock(LockFile, 0, FileSize(Lockfile))) or
        (IOResult <> 0) then LockDenied := true;
    end;
  end;
  if LockDenied then
  begin
    writeln;
    for i:=1 to res2anz(244) do
      writeln(getres2(244,i));
    mdelay(1000);
    close(lockfile);
    runerror:=false;
    halt(1);
  end;
  lockopen:=true;
  FileMode := FMRW; { Filemode restaurieren! }
end;

procedure DelTmpfiles(fn:string);
var sr : searchrec;
begin
  findfirst(fn,ffAnyFile,sr);
  while doserror=0 do begin
    _era(sr.name);
    findnext(sr);
  end;
  Findclose(sr);
end;

procedure ReadDefaultViewers;

  procedure SeekViewer(mimetyp:string; var viewer:pviewer);
  var prog : string[ViewprogLen];
  begin
    dbSeek(mimebase,mtiTyp,ustr(mimetyp));
    if not dbEOF(mimebase) and not dbBOF(mimebase) and
       stricmp(dbReadStr(mimebase,'typ'),mimetyp) then
    begin
      dbReadN(mimebase,mimeb_programm,prog);
      if viewer<>nil then freemem(viewer,length(viewer^)+1);
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

procedure check_date;      { Test, ob Systemdatum verstellt wurde }
const maxdays = 14;
var dt   : DateTime;
    days : longint;
    dow  : rtlword;
    ddiff: longint;
    wdt  : byte;
    x,y  : byte;
    brk  : boolean;
    dat  : datetimest;
    t,m,j: word;
    m3s  : procedure;
begin
  fillchar(dt,sizeof(dt),0);
  getdate(dt.year,dt.month,dt.day,dow);
  days:=longint(dt.year)*365+dt.month*30+dt.day;
  unpacktime(filetime(NewDateFile),dt);                  { Abstand in Tagen }
  ddiff:=days - (longint(dt.year)*365+dt.month*30+dt.day);
  if (ddiff<0) or (ddiff>maxdays) then begin
    wdt:=4+max(max(length(getres2(225,1)),length(getres2(225,2))),
                   length(getres2(225,3))+10);
    dialog(wdt,5,'',x,y);
    if ddiff>0 then
      { 'Seit dem letzten Programmstart sind mehr als %s Tage vergangen.' }
      maddtext(3,2,getreps2(225,1,strs(maxdays)),0)
    else
      { 'Das Systemdatum liegt vor dem Datum des letzten Programmstarts.' }
      maddtext(3,2,getreps2(225,2,strs(maxdays)),0);
    dat:=left(date,6)+right(date,2);
    madddate(3,4,getres2(225,3),dat,false,false);   { 'Bitte best„tigen Sie das Datum: ' }
      mhnr(92);
    zaehler[1]:=30; zaehlx:=x+wdt-6; zaehly:=y-1;
     m3s:=multi3;
    multi3:=ShowDateZaehler; hotkeys:=false;
    readmask(brk);
    multi3:=m3s; hotkeys:=true;
    if not brk and mmodified then begin
      t:=ival(left(dat,2));
      m:=ival(copy(dat,4,2));
      j:=ival(right(dat,2));
      if j<80 then inc(j,2000) else inc(j,1900);
      setdate(j,m,t);
      end;
    enddialog;
    end;
end;

Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche }
Var i : byte;                { Anhand der Feldtauscheinstellungen bestimmen }
Begin
  UsrFeldPos1:=1;
  UsrFeldPos2:=2;
  i:=1;
  While UsrFeldtausch[i]<>'A' do
  begin
    Case UsrFeldtausch[i] of
                      { Spezial             Normal }
      'F' : Begin inc(UsrFeldPos1,5);   inc(UsrFeldPos2,4);  end; { Flags }
      'G' : Begin inc(UsrFeldPos1,3);                        end; { Gruppen }
      'H' : Begin inc(UsrFeldPos1,7);                        end; { Haltezeit }
      'B' : Begin inc(UsrFeldPos1,10);                       end; { Box }
      'K' : Begin inc(UsrFeldPos1,31);  inc(UsrFeldPos2,31); end; { Kommentar }
      end;
    inc(i);
    end;
   if UsrfeldPos2=33 Then UsrFeldpos2:=32;
end;

procedure ShowDateZaehler;
const lastdz : integer = -1;
begin
  if zaehler[1]<>lastdz then begin
    savecursor;
    lastdz:=zaehler[1];
    attrtxt(col.coldiarahmen);
    wrt(zaehlx,zaehly,' '+strsn(lastdz,2)+' ');
    restcursor;
    if lastdz=0 then keyboard(KeyEsc);
    end;
end;

end.

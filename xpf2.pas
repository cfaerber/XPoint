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

{ Fido-Modul, Teil 2 }

{$I xpdefine.inc}

unit xpf2;

interface

uses
  sysutils,
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,archive,montage,
  xp0,xp1,xp1o,xp3,xp3o;


procedure TestTICfiles(var logfile:string);   { TIC-Files verarbeiten }


implementation   { -------------------------------------------------- }

uses xpheader, xpnt,xp3o2;

(*
function UNIX2Zdate(secs:longint):string;
const tage : array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
      tagsec = 86400;  { 24*60*60 }
var y,m,d,dow : word;
    h,min   : word;

  procedure setfeb(y:word);
  begin
    if schaltj(y) then tage[2]:=29
    else tage[2]:=28;
  end;

begin
  y:=1970;
  while secs>=iif(schaltj(y),366,365)*tagsec do begin
    dec(secs,iif(schaltj(y),366,365)*tagsec);
    inc(y);
    end;
  setfeb(y); m:=1;
  while (secs>=tagsec*tage[m]) do begin
    dec(secs,tagsec*tage[m]);
    inc(m);
    end;
  d:=secs div tagsec + 1; secs:=secs mod tagsec;
  h:=secs div 3600;       secs:=secs mod 3600;
  min:=secs div 60;       secs:=secs mod 60;
  UNIX2Zdate:=formi(y mod 100,2)+formi(m,2)+formi(d,2)+formi(h,2)+formi(min,2);
end; *)


{ TIC-Files verarbeiten; BoxPar^ muá korrekte geladen sein! }

procedure TestTICfiles(var logfile:string);
var t    : text;
    s    : string;
    name1: string;
    at   : shortint;
    ar   : ArchRec;
    sr   : tsearchrec;
    f    : file;
    tmp  : string;
    count: longint;

label ende;

  function IsTIC(name:string):boolean;
  begin
    UpString(name);
    IsTIC:={((LeftStr(name,2)='TK') and} (UpperCase(RightStr(name,4))='.TIC');
  end;

  { True -> passende Datei ist vorhanden }

  function ProcessTICfile(fn:string):boolean;
  var t2  : text;
      hdp : Theader;
      s   : string;
      feld: string;
      p   : byte;
  begin
    ProcessTICfile:=false;
    hdp := THeader.Create;
    assign(t2,fn);
    reset(t2);
    while not eof(t2) do begin
      readln(t2,s);
      p:=blankpos(s);
      if p>0 then with hdp,boxpar^ do begin
        feld:=LowerCase(LeftStr(s,p-1));
        s:=trim(mid(s,p));
        if feld='area' then empfaenger:=MagicBrett+'FILES/'+s else
        if feld='origin' then absender:='FileScan@'+s else
        if feld='file' then betreff:=ExpandFilename(ExtractFilePath(fn)+s) else
        if feld='desc' then summary:=s else
        if (feld='path') and (blankpos(s)>0) then
          pfad:=LeftStr(s,blankpos(s)-1)+'!'+pfad;
        { if feld='date' then datum:=UNIX2Zdate(hexval(s)); }
        end;
      end;
    close(t2);
    with hdp do begin
      DeleteLastChar(pfad);
      if (empfaenger<>'') and (betreff<>'') and FileExists(betreff) then begin
        netztyp:=nt_Fido;
        inc(attrib,AttrFile);
        if absender='' then absender:='???';
        if pfad='' then pfad:=boxpar^.boxname;
        {if datum='' then} datum:=zdate;
        inc(count);
        msgid:=datum+'.'+strs(count)+'.Tick@'+boxpar^.boxname;
        fido_to:=summary;
        WriteHeader(hdp,f);
        ProcessTICfile:=true;
        end;
      end;
    Hdp.Free;
  end;

begin
  assign(t,logfile);
  reset(t);
  tmp:=TempS(16384);
  assign(f,tmp);         { Ausgabepuffer }
  rewrite(f,1);
  count:=0;
  while not eof(t) do begin        { gepackte TIC-Files auswerten }
    readln(t,s);
    if (Copy(s,1,1)='*') and (pos('  rcvd ',LowerCase(s))>0) then begin
      s:=trim(mid(s,18));
      s:=LeftStr(s,cpos(';',s)-1);  { Pfad\Dateiname isolieren }
      UpString(s);
      if (hexval(LeftStr(extractfilename(s),8))<>0) or (LeftStr(extractfilename(s),4)='TO__')
      then begin   { m”gliches TIC-Paket? }
        at:=ArcType(s);
        if at<>0 then begin
          OpenArchive(s,at,ar);
          while not ar.ende and not IsTIC(ar.name) do
            ArcNext(ar);
          name1:=ar.name;
          CloseArchive(ar);
          if not ar.ende then begin           { .TIC-Files enthalten }
            if not IsPath(FilePath+'TICK') then begin
              mkdir(FilePath+'TICK');
              if ioresult<>0 then begin
                rfehler1(2123,ExpandFilename(FilePath+'TICK'));   { 'Kann Verzeichnis %s nicht anlegen!' }
                goto ende;
                end;
              end;
            if UniExtract(s,FilePath+'TICK\','*.*') and
               FileExists(FilePath+'TICK\'+name1) then begin
              DeleteFile(s);
              while FindFirst(FilePath+'TICK\*.TIC',faAnyFile,sr)=0 do repeat       { .TIC-Files verarbeiten }
                if ProcessTICfile(FilePath+'TICK\'+sr.name) then;
                DeleteFile(FilePath+'TICK\'+sr.name);
              until findnext(sr)<>0;
              FindClose(sr);
            end;
          end;   { of TIC-File vorhanden }
        end;   { at>0 }
      end;
    end;   { rcvd }
  end;
ende:
  close(t);

  { ungepackte TIC-Files }
  while findfirst(FilePath+'*.TIC',faAnyFile,sr)=0 do repeat
    if ProcessTICfile(FilePath+sr.name) then
      DeleteFile(FilePath+sr.name);
  until findnext(sr)<>0;
  FindClose(sr);

  close(f);
  if _filesize(tmp)>0 then
    if PufferEinlesen(tmp,BoxPar^.boxname,true,false,false,0) then;
  _era(tmp);
end;


end.

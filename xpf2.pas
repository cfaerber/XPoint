{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Fido-Modul, Teil 2 }

{$I XPDEFINE.INC}

unit xpf2;

interface

uses  {$IFDEF virtualpascal}sysutils,{$endif}
      xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
      dos,typeform,fileio,archive,montage,
      xp0,xp1,xp1o,xp3,xp3o;


procedure TestTICfiles(var logfile:string);   { TIC-Files verarbeiten }


implementation   { -------------------------------------------------- }

uses xpnt,xp3o2;

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
    s    : string[80];
    at   : shortint;
    ar   : ArchRec;
    sr   : searchrec;
    f    : file;
    tmp  : pathstr;
    count: longint;
    name1: string[14];

label ende;

  function IsTIC(name:pathstr):boolean;
  begin
    UpString(name);
    IsTIC:={((left(name,2)='TK') and} (right(name,4)='.TIC');
  end;

  { True -> passende Datei ist vorhanden }

  function ProcessTICfile(fn:pathstr):boolean;
  var t2  : text;
      hdp : headerp;
      s   : string;
      feld: string[20];
      p   : byte;
  begin
    ProcessTICfile:=false;
    new(hdp);
    fillchar(hdp^,sizeof(hdp^),0);
    assign(t2,fn);
    reset(t2);
    while not eof(t2) do begin
      readln(t2,s);
      p:=blankpos(s);
      if p>0 then with hdp^,boxpar^ do begin
        feld:=lstr(left(s,p-1));
        s:=trim(mid(s,p));
        if feld='area' then empfaenger:=MagicBrett+'FILES/'+s else
        if feld='origin' then absender:='FileScan@'+s else
        if feld='file' then betreff:=FExpand(GetFileDir(fn)+s) else
        if feld='desc' then summary:=s else
        if (feld='path') and (blankpos(s)>0) then
          pfad:=left(s,blankpos(s)-1)+'!'+pfad;
        { if feld='date' then datum:=UNIX2Zdate(hexval(s)); }
        end;
      end;
    close(t2);
    with hdp^ do begin
      dellastHuge(pfad);
      if (empfaenger<>'') and (betreff<>'') and exist(betreff) then begin
        netztyp:=nt_Fido;
        inc(attrib,AttrFile);
        if absender='' then absender:='???';
        if pfad='' then pfad:=boxpar^.boxname;
        {if datum='' then} datum:=zdate;
        inc(count);
        msgid:=datum+'.'+strs(count)+'.Tick@'+boxpar^.boxname;
        fido_to:=summary;
        WriteHeader(hdp^,f,nil);
        ProcessTICfile:=true;
        end;
      end;
    dispose(hdp);
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
    if (s[1]='*') and (pos('  rcvd ',lstr(s))>0) then begin
      s:=trim(mid(s,18));
      s:=left(s,cpos(';',s)-1);  { Pfad\Dateiname isolieren }
      UpString(s);
      if (hexval(left(getfilename(s),8))<>0) or (left(getfilename(s),4)='TO__')
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
                rfehler1(2123,FExpand(FilePath+'TICK'));   { 'Kann Verzeichnis %s nicht anlegen!' }
                goto ende;
                end;
              end;
            if UniExtract(s,FilePath+'TICK\','*.*') and
               exist(FilePath+'TICK\'+name1) then begin
              _era(s);
              Dos.FindFirst(FilePath+'TICK\*.TIC',ffAnyFile,sr);
              while doserror=0 do begin       { .TIC-Files verarbeiten }
                if ProcessTICfile(FilePath+'TICK\'+sr.name) then;
                _era(FilePath+'TICK\'+sr.name);
                Dos.findnext(sr);
              end;
              FindClose(sr);
            end;
          end;   { of TIC-File vorhanden }
        end;   { at>0 }
      end;
    end;   { rcvd }
  end;
ende:
  close(t);

  Dos.findfirst(FilePath+'*.TIC',ffAnyFile,sr);    { ungepackte TIC-Files }
  while doserror=0 do begin
    if ProcessTICfile(FilePath+sr.name) then
      _era(FilePath+sr.name);
    Dos.findnext(sr);
  end;
  FindClose(sr);

  close(f);
  if _filesize(tmp)>0 then
    if PufferEinlesen(tmp,BoxPar^.boxname,true,false,false,0) then;
  _era(tmp);
end;


end.

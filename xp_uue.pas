{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - UUDecode }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp_uue;


interface

uses  crt,dos,typeform,fileio,inout,database,maus2,resource,
      xp0,xp1,xp1o,xp1o2,xp1input, xpglobal;


procedure uudecode;            { aktuelle Nachricht decodieren }

function uudecfile(infile,outpath:pathstr; wait:boolean):boolean;


implementation

uses xp3,xp3ex;

const obufsize = 10000;
      ibufsize = 15000;
      maxbuf   : word = obufsize-4;

type  buffer = array[0..50000] of byte;
      bufptr = ^buffer;

var   s        : string[100];
      f1,f2    : ^file;
      bufp     : word;
      outbuf,
      inbuf    : bufptr;
      ibufp,
      ibufend  : word;
      ln,bytes : longint;
      EOFinput : boolean;
      IO_Error : boolean;


{$IFDEF ver32}
procedure decode;  begin end;
procedure getstring; begin end;
{$ELSE}
procedure decode; near; external;
procedure getstring; near; external;
{$L xp_uue.obj}
{$ENDIF}


procedure ReadInputLine;
const ibufmin   = 255;
var   bytesread : word;
begin
  if ibufend<ibufp then begin
    blockread(f1^,inbuf^,ibufsize-5,bytesread);
    if bytesread=0 then begin
      ibufend:=0; EOFinput:=true;
      end
    else ibufend:=bytesread-1;
    ibufp:=0;
    end
  else if ibufend<ibufp+ibufmin then begin
    Move(inbuf^[ibufp],inbuf^[0],ibufend-ibufp+1);
    dec(ibufend,ibufp); ibufp:=0;
    blockread(f1^,inbuf^[ibufend+1],ibufsize-ibufend-5,bytesread);
    inc(ibufend,bytesread);
    end;
  if ibufend<ibufp then EOFinput:=true
  else getstring;        { String aus inbuf^[ibufp] --> s lesen }
end;


procedure flushbuf;
begin
  if IO_error then exit;
  blockwrite(f2^,outbuf^,bufp);
  if inoutres<>0 then begin
    fehler(ioerror(ioresult,getres2(10000,10)));
    IO_error:=true;
    end;
  bufp:=0;
end;


{ f1 -> mit Assign zugewiesene Eingabedatei }
{ fn <- Dateiname aus der 'begin'-Zeile     }

function openinfile(var f1:file; var fn:pathstr):boolean;
var found: boolean;
    p    : byte;
begin
  reset(f1,1); ibufp:=1; ibufend:=0; EOFinput:=false;
  repeat
    ReadInputLine;
    found:=(left(s,5)='begin');
  until EOFinput or found;
  p:=posn(' ',s,7);
  if p=0 then fn:=''
  else fn:=UStr(trim(mid(s,p)));
  openinfile:=found and (fn<>'');
  if EOFinput then exit;
end;

procedure opencontfile(var f1:file);
begin
  reset(f1,1); ibufp:=1; ibufend:=0; EOFinput:=false;
end;


procedure uudecIt(var f1:file; outfile:pathstr; wait:boolean;
                  filenr,filetotal:integer; overwrite:boolean);
const x : byte = 0;
      y : byte = 0;

var ende : boolean;
    size : longint;
    start: boolean;
    l    : longint;
    warn : boolean;

  procedure showze;
  begin
    moff;
    gotoxy(x+23,y+2); write(filenr:3);
    gotoxy(x+35,y+2); write(ln:5);
    gotoxy(x+42,y+2); write(bytes:8);
    mon;
  end;

begin
  if filenr=1 then begin
    msgbox(59,5,'uudecode',x,y);
    mwrt(x+3,y+2,getres(2400));   { 'decodiere Nachricht     / Zeile       /         Bytes' }
    end;
  attrtxt(col.colmboxhigh);
  new(f2);
  assign(f2^,outfile);
  if overwrite then
    rewrite(f2^,1)
  else begin
    reset(f2^,1);               { an vorhandene Teile anhÑngen }
    seek(f2^,filesize(f2^));
    end;
  ln:=0; bufp:=0;
  start:=true;
  warn:=false;
  repeat
    if start and (filenr>1) then
      repeat
        ReadInputline
      until ((s<>'') and (s[1]='M')) or EOFinput
    else
      ReadInputLine;
    ende:=(left(s,3)='end');
    if length(s)>1 then
      if (s[1]<='_') and (s[1]>='!') and not ende then begin
        if length(s)<((ord(s[1])-32) div 3 * 4)+1 then begin
          if not warn and (filenr=filetotal) then begin
            hinweis(getres(2405));  { 'Warnung: Fehlerhafte Zeile; decodierte Datei ist evtl. fehlerhaft' }
            warn:=true;
            end;
          s:=forms(s,((ord(s[1])-32) div 3 * 4)+1);
          end;
        decode;
        end
      else
        if filenr<filetotal then ende:=true;
    if ln mod 10 = 0 then showze;
    start:=false;
  until EOFinput or IO_error or ende or ((filenr<filetotal) and (s=''));
  if bufp>0 then flushbuf;
  if not EOFinput then begin           { 'size' auswerten }
    ReadInputLine;
    if left(s,4)='size' then begin
      size:=ival(mid(s,5));
      if (size>0) and (size<filesize(f2^)) then begin
        seek(f2^,size);
        truncate(f2^);
        end;
      end
    else
      {:  sum -r/size 49243/2336 section (from "begin" to "end")  }
      {:  sum -r/size 34783/1676 entire input file                }
      while (left(s,12)='sum -r/size ') and not EOFinput do begin
        if pos('entire',s)>0 then begin
          s:=trim(mid(s,13));
          l:=ival(GetToken(s,'/'));   { Summe Åberlesen }
          l:=ival(GetToken(s,' '));
          if (l<filesize(f2^)) and (l>filesize(f2^)-20) then begin
            seek(f2^,l);
            truncate(f2^);
            end;
          end;
        ReadInputline;
        end;
    end;
  close(f2^);
  dispose(f2);
  showze;
  if filenr=filetotal then begin
    if wait then
      wkey(1,false);
    closebox;
    end;
end;


procedure uudecode;
const maxuumark= 100;       { max. decodierbare, markierte Einzelteile }
      fldDatum = 1;
      fldIntnr = 2;
      fldSection = 3;
type  uumarkrec = record
                    recno   : longint;
                    sortfld : array[1..3] of longint;
                  end;

var tmp,fn   : pathstr;
    p        : byte;
    ende     : boolean;
    useclip  : boolean;
    brk      : boolean;
    decmark  : boolean;
    marklist : array[1..maxuumark] of uumarkrec;
    mlanz    : integer;
    i        : integer;
    ok       : boolean;
    msize    : longint;
    hdp      : headerp;
    hds      : longint;
    o        : boolean;

  procedure SortFor(fld:integer);
  var i,j : integer;
      w   : uumarkrec;
  begin
    for i:=mlanz downto 2 do
      for j:=1 to i-1 do
        if marklist[j].sortfld[fld]>marklist[j+1].sortfld[fld] then begin
          w:=marklist[j]; marklist[j]:=marklist[j+1]; marklist[j+1]:=w;
          end;
    ok:=true;                               { Test auf doppeltes Datum }
    for i:=1 to mlanz-1 do
      if marklist[i].sortfld[fld]=marklist[i+1].sortfld[fld] then
        ok:=false;
  end;

  procedure GetMsize;                 { Nachrichten-Maximalgrî·e berechnen }
  var i : integer;
  begin
    msize:=0;
    for i:=1 to mlanz do begin
      dbGo(mbase,marklist[i].recno);
      msize:=max(msize,dbReadInt(mbase,'groesse'));
      end;
    tmp:=TempS(msize);
  end;

  procedure ReadSections;             { Section-Nummern einlesen }
  const tbs = 4096;
  var i  : integer;
      t  : text;
      s  : string[80];
      tb : pointer;
      p  : byte;
  begin
    ok:=true;
    getmem(tb,tbs);
    i:=1;
    while (i<=mlanz) and ok do begin
      dbGo(mbase,marklist[i].recno);
      extract_msg(0,'',tmp,false,0);
      assign(t,tmp); settextbuf(t,tb^,tbs);
      reset(t); s:='';
      while not eof(t) and (left(s,8)<>'section ') do
        readln(t,s);
      close(t);
      if left(s,8)='section ' then begin
        s:=trim(mid(s,9));
        p:=blankpos(s);
        if p>0 then marklist[i].sortfld[fldSection]:=ival(left(s,p-1));
        end;
      if marklist[i].sortfld[fldSection]=0 then ok:=false;
      inc(i);
      end;
    freemem(tb,tbs);
  end;

begin
  if not testmem(30000,false) then exit;
  if markanz=0 then
    decmark:=false
  else begin
    decmark:=ReadJNesc(GetReps(2403,strs(markanz)),true,brk);  { '%s markierte Nachrichten decodieren' }
    if brk then exit;
    if decmark and (markanz>maxuumark) then begin
      rfehler1(2404,strs(maxuumark));   { 'Es kînnen maximal %s markierte Einzelteile decodiert werden.' }
      exit;
      end;
    end;
  if decmark then begin
    mlanz:=markanz;
    moment;
    new(hdp);
    sortmark;
    for i:=1 to mlanz do begin              { Liste der Nachrichten nach }
      marklist[i].recno:=marked^[i-1].recno;  { Datum sortieren            }
      dbGo(mbase,marklist[i].recno);
      ReadHeader(hdp^,hds,ok);              { Sekunden dazuaddieren      }
      marklist[i].sortfld[fldDatum]:=(ixdat(hdp^.datum) and $ffffff) shl 7 +
                                     ival(copy(hdp^.zdatum,13,2));
      marklist[i].sortfld[fldIntnr]:=dbReadInt(mbase,'int_nr');
      marklist[i].sortfld[fldSection]:=0;
      end;
    unsortmark;
    dispose(hdp);
    GetMsize;
    ReadSections;
    if ok then SortFor(fldSection);
    if not ok then SortFor(fldDatum);
    closebox;
    if not ok then begin
      hinweis(getres(2404));   { 'Warnung: Reihenfolge kann nicht zuverlÑssig ermittelt werden.' }
      SortFor(fldIntnr);
      end;
    end
  else begin
    mlanz:=1;
    marklist[1].recno:=dbRecno(mbase);     { Datum ist egal }
    GetMsize;
    end;

  dbGo(mbase,marklist[1].recno);
  extract_msg(0,'',tmp,false,0);
  new(f1);
  assign(f1^,tmp);
  getmem(inbuf,ibufsize);
  getmem(outbuf,obufsize);
  if not openinfile(f1^,fn) then
    rfehler(2402)    { 'Nachricht enthÑlt keine UUcodierte Datei' }
  else begin
    pushhp(75);
    useclip:=false;
    if ReadFilename(getres(2402),fn,true,useclip) then begin   { 'Zieldatei' }
      if not multipos(':\',fn) then fn:=ExtractPath+fn;
      if exist(fn) then o:=overwrite(fn,true,brk)
      else o:=true;
      if not exist(fn) or not brk then begin
        bytes:=0;
        IO_error:=false;
        uudecIt(f1^,fn,true,1,mlanz,o);
        if IO_error and (mlanz>1) then
          closebox
        else begin
          i:=2;
          while (i<=mlanz) and not IO_error do begin
            close(f1^);
            dbGo(mbase,marklist[i].recno);
            extract_msg(0,'',tmp,false,0);
            OpenContFile(f1^);
            uudecIt(f1^,fn,true,i,mlanz,false);
            inc(i);
            end;
          if i<mlanz then closebox;   { IO_error }
          end;
        end;
      end;
    pophp;
    end;
  close(f1^);
  dispose(f1);
  _era(tmp);
  freemem(outbuf,obufsize);
  freemem(inbuf,ibufsize);
end;


function uudecfile(infile,outpath:pathstr; wait:boolean):boolean;
var fn : pathstr;
begin
  uudecfile:=false;
  new(f1);
  assign(f1^,infile);
  getmem(inbuf,ibufsize);
  getmem(outbuf,obufsize);
  if not openinfile(f1^,fn) then
    trfehler(2403,20)    { 'Kann Datei nicht decodieren.' }
  else begin
    uudecit(f1^,outpath+fn,wait,1,1,true);
    uudecfile:=true;
    end;
  close(f1^);
  dispose(f1);
  freemem(outbuf,obufsize);
  freemem(inbuf,ibufsize);
end;

end.


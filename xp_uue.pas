{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - UUDecode }

{$I XPDEFINE.INC}

unit xp_uue;


interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
      dos,typeform,fileio,inout,database,maus2,resource,
      xp0,xp1,xp1o,xp1o2,xp1input, xpglobal;


procedure uudecode;            { aktuelle Nachricht decodieren }

function uudecfile(infile,outpath:string; wait:boolean):boolean;


implementation

uses xp3,xp3ex;

const obufsize = 12288; { Auf 32 Byte Grî·en angepasst }
      ibufsize = 16384;
      maxbuf   : word = obufsize-4;

type  buffer = array[0..50000] of byte;
      bufptr = ^buffer;

var   s        : string;
      shorts   : ShortString;
      f1,f2    : ^file;
      bufp     : word;
      outbuf,
      inbuf    : bufptr;
      ibufp,
      ibufend  : word;
      ln,bytes : longint;
      EOFinput : boolean;
      IO_Error : boolean;


procedure flushbuf;                   {JG:08.02.00 Verschoben: wird von Decode aufgerufen...}
begin
  if IO_error then exit;
  blockwrite(f2^,outbuf^,bufp);
  if inoutres<>0 then begin
    fehler(ioerror(ioresult,getres2(10000,10)));
    IO_error:=true;
    end;
  bufp:=0;
end;

{ !! Ungetestet und unoptimiert }
procedure decode; assembler;  {&uses ebx, esi, edi}
asm
          mov esi, offset ShortS       { Adresse des zu dekod. Strings }
          mov ebx, 2              { Offset innerhalb von s }

          mov cl,1                { SchleifenzÑhler }
          mov ch,[esi+1]          { 1. Byte : LÑngeninformation }
          sub ch,' '
          and ch,3fh

@mloop0:  mov edi,outbuf           { Adresse des Ausgabepuffers }
          add edi,bufp

@mainloop:
          cmp cl,ch               { i<=n ? }
          jle @lp1
          inc ln
          xor eax, eax
          mov al,ch
          add dword ptr bytes, eax
          jmp @ende

@lp1:     mov dx,[esi+ebx]          { 4 Bytes dekodieren }
          sub dx,2020h
          mov ax,[esi+ebx+2]
          sub ax,2020h
          add ebx,4
          shl al,1
          shl al,1
          shl al,1
          rcl dh,1
          shl al,1
          rcl dh,1
          shl al,1
          rcl dh,1
          rcl dl,1
          shl al,1
          rcl dh,1
          rcl dl,1
          and ah,3fh
          add al,ah
          mov [edi],dx
          inc edi
          inc edi
          mov [edi],al
          inc edi

          mov eax,bufp
          inc cl
          inc eax
          cmp cl,ch
          ja @zende
          inc cl
          inc eax
          cmp cl,ch
          ja @zende
          inc cl
          inc eax
@zende:   mov bufp,eax
          cmp eax,maxbuf
          ja @flush
          jmp @mainloop

@flush:   pushad
          call dword ptr flushbuf            {setzt bufp auf 0   JG:FAR }
          popad
          jmp @mloop0
@ende:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }


procedure getstring; assembler; {&uses ebx, esi}
asm
          mov esi,inbuf
          mov ebx,ibufp
          mov edi, offset ShortS
          inc edi
          mov edx,ibufend
          mov ah,0

@getloop: cmp ebx,edx               { ibufp > ibufend ? }
          ja  @getende
          mov al,[esi+ebx]
          cmp al,' '              { Zeilenende ? }
          jb  @getok
          mov [edi],al
          inc edi
          inc ebx
          inc ah
          cmp ah,100              { max. StringlÑnge }
          jb  @getloop
@getok:   mov al,2                { max. ein CR/LF Åberlesen }
@getok2:  cmp ebx,edx               { ibufp > ibufend ? }
          ja @getende
          cmp byte ptr [esi+ebx],' '
          jae @getende
          inc ebx                  { ZeilenvorschÅbe Åberlesen }
          dec al
          jnz @getok2

@getende: mov ibufp,ebx
          mov edi, offset ShortS
          mov [edi],ah             { StringlÑnge setzen (s[0]) }
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure ReadInputLine;
const
  ibufmin   = 255;
var
  bytesread : word;
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
  else
  begin
    getstring;        { String aus inbuf^[ibufp] --> s lesen }
    s := shorts;
  end;
end;



{ f1 -> mit Assign zugewiesene Eingabedatei }
{ fn <- Dateiname aus der 'begin'-Zeile     }

function openinfile(var f1:file; var fn:String; goon:boolean):boolean;
var found: boolean;
    p    : byte;
begin
  if not goon then begin
    reset(f1,1); ibufp:=1; ibufend:=0; EOFinput:=false;
  end;
  repeat
    ReadInputLine;
    found:=(left(s,5)='begin');
  until EOFinput or found;
  p:=posn(' ',s,7);
  if p=0 then fn:=''
  else fn:=FileUpperCase(trim(mid(s,p)));
  openinfile:=found and (fn<>'');
  if EOFinput then exit;
end;

procedure opencontfile(var f1:file);
begin
  reset(f1,1); ibufp:=1; ibufend:=0; EOFinput:=false;
end;


procedure uudecIt(var f1:file; outfile:string; wait:boolean;
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
        begin
          shorts := s;
          decode;
        end;
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

var tmp,fn   : string;
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
    Filenr   : byte;

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
      s  : string;
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
    hdp := AllocHeaderMem;
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
    FreeHeaderMem(hdp);
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
  if not openinfile(f1^,fn,false) then
    rfehler(2402)    { 'Nachricht enthÑlt keine UUcodierte Datei' }
  else begin
    Filenr:=0;
    repeat

    inc(Filenr);
    pushhp(75);
    useclip:=false;
    if ReadFilename(getres(2402)+iifs(filenr>1,' '+strs(filenr),''),fn,true,useclip)
    then begin                                            { 'Zieldatei' }
      if not multipos(_MPMask,fn) then fn:=ExtractPath+fn;
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

    until (mlanz>1) or not openinfile(f1^,fn,true);
  end;
  close(f1^);
  dispose(f1);
  _era(tmp);
  freemem(outbuf,obufsize);
  freemem(inbuf,ibufsize);
end;


function uudecfile(infile,outpath:string; wait:boolean):boolean;
var fn : string;
begin
  uudecfile:=false;
  new(f1);
  assign(f1^,infile);
  getmem(inbuf,ibufsize);
  getmem(outbuf,obufsize);
  if not openinfile(f1^,fn, false) then
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
{
  $Log$
  Revision 1.20  2000/08/22 23:25:17  mk
  MK+MO:- Ansistring-Bugfix

  Revision 1.19  2000/07/21 17:39:57  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.18  2000/07/16 16:52:29  mk
  JG:  UUE-Decoding auch bei mehreren Files in einer Nachricht moeglich

  Revision 1.17  2000/07/06 08:58:46  hd
  - AnsiString

  Revision 1.16  2000/07/04 12:04:28  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.15  2000/07/03 13:31:43  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.14  2000/06/23 15:59:25  mk
  - 16 Bit Teile entfernt

  Revision 1.13  2000/05/22 17:05:40  hd
  - FUStr statt UStr
  - multipos angepasst

  Revision 1.12  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.11  2000/04/04 21:01:24  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.10  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.9  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.8  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.7  2000/03/14 15:15:41  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}

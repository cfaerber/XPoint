{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Archiv-Routinen              }
{ fr ARC, ZOO, LZH, ZIP, ARJ, }
{ PAK, DWC, HYP, SQZ und tar   }
{ PM 11/91, 01/93, 07/93       }
{ RAR, UC2 08/94               }

{$I XPDEFINE.INC }

unit archive;

interface

uses
  xpglobal, sysutils, dos, typeform, montage;

const  ArcTypes   = 12;
       ArcUnknown = 0;
       ArcARC     = 1;
       ArcLZH     = 2;
       ArcZOO     = 3;
       ArcZIP     = 4;
       ArcARJ     = 5;
       ArcPAK     = 6;
       ArcDWC     = 7;
       ArcHYP     = 8;
       ArcSQZ     = 9;
       ArcTAR     = 10;
       ArcRAR     = 11;
       ArcUC2     = 12;

       ArcName : array[1..ArcTypes] of string[3] =
                   ('ARC','LZH','ZOO','ZIP','ARJ','PAK','DWC','HYP','SQZ',
                    'tar','RAR','UC2');

type   arcpath = string[79];
       ArchRec = record
                   arctyp   : shortint;
                   sfx      : boolean;
                   f        : file;
                   opened   : boolean;
                   ende     : boolean;       { keine weiteren Dateien }
                   adr      : longint;       { Adresse der n„chsten Datei }
                   method   : string[10];
                   datum    : word;          { im DOS-Format }
                   uhrzeit  : word;          { im DOS-Format }
                   orgsize  : longint;       { Gr”áe der Original-Datei }
                   compsize : longint;       { komprimierte Gr”áe       }
                   path     : arcpath;       { Pfad ohne Dateiname }
                   name     : string[12];
                   attrib   : word;          { DOS-Attribute }
                 end;


function  ArcType(fn:arcpath):shortint;
function  ArcRestricted(atyp:shortint):boolean;
procedure OpenArchive(fn:arcpath; atyp:shortint; var ar:ArchRec);  { 0=detect }
procedure ArcNext(var ar:ArchRec);
procedure CloseArchive(var ar:ArchRec);
function  UnSFX(name:arcpath; typ:shortint):boolean;
function  ArchiveOk(fn:arcpath):boolean;


implementation   { ------------------------------------------------- }


type archd = record
               id       : byte;   { $1a }
               method   : byte;
               name     : array[0..12] of char;
               compsize : longint;
               dosdate  : smallword;
               dostime  : smallword;
               crc16    : smallword;
               orgsize  : longint;
             end;

     lzhhd = record
               hdsize   : byte;
               checksum : byte;
               method   : array[0..4] of char;
               compsize : longint;
               orgsize  : longint;
               dostime  : smallword;
               dosdate  : smallword;
               attrib   : smallword;
               namelen  : byte;
               path     : shortstring;
             end;

     ziphd = record
               id       : longint;   { $04034b50 }
               extver   : smallword;
               flags    : smallword;
               method   : smallword;
               dostime  : smallword;
               dosdate  : smallword;
               crc32    : longint;
               compsize : longint;
               orgsize  : longint;
               namelen  : smallword;
               eflen    : smallword;
               path     : shortstring;
             end;

     zoorec  = record
                 id       : longint;        { $fdc4a7dc }
                 dtype    : byte;  { ?? }
                 method   : byte;
                 next     : longint;
                 header   : longint;
                 dosdate  : smallword;
                 dostime  : smallword;
                 crc16    : smallword;
                 orgsize  : longint;
                 compsize : longint;
                 version  : byte;
                 xver     : byte;
                 deleted  : boolean;
                 commadr  : longint;
                 commlen  : smallword;
                 name     : shortstring;
               end;

     arjrec  = record
                 HeaderID : smallword;    { $ea60 }
                 hdsize   : smallword;    { ab hdsize1 }
                 hdsize1  : byte;
                 version  : byte;
                 extver   : byte;
                 hostos   : byte;
                 flags    : byte;
                 method   : byte;
                 ftype    : byte;
                 res      : byte;
                 dostime  : smallword;
                 dosdate  : smallword;
                 compsize : longint;
                 orgsize  : longint;
                 orgcrc   : longint;
                 nameadr  : smallword;
                 attrib   : smallword;
                 hostdata : smallword;
                 name     : shortstring;   { evtl. 4 Bytes Ext-FilePos }
               end;

     dwchd   = record
                 hdsize   : smallword;
                 recsize  : smallword;
                 unknown  : array[1..16] of byte;
                 entries  : longint;
                 id       : array[0..2] of char;
               end;

     dwcrec  = record
                 name     : string[12];
                 orgsize  : longint;
                 secsf70  : longint;
                 compsize : longint;
                 arcofs   : longint;
                 method   : byte;
                 unknown  : array[1..3] of byte;
               end;

     hyperhd = record
                 id       : byte;     { #26 }
                 method   : array[1..2] of char;   { HP oder ST }
                 version  : byte;
                 compsize : longint;
                 orgsize  : longint;
                 dostime  : smallword;
                 dosdate  : smallword;
                 chksum   : longint;
                 attrib   : byte;
                 name     : shortstring;
              end;

     sqzrec = record
                case hdtype : byte of   { 0=Ende, 1=Comment, 2=PW, 18..=File }
                  1 : (ComSize  : smallword;     { Comment }
                       ComComp  : smallword;
                       ComMeth  : byte;
                       ComCRC   : longint);
                  2 : (BlkSize  : smallword);    { Password u.a. }
                 18 : (HdChksum : byte;
                       Method   : byte;     { + Flags }
                       compsize : longint;
                       orgsize  : longint;
                       dostime  : smallword;
                       dosdate  : smallword;
                       attrib   : byte;
                       crc32    : longint;
                       name     : shortstring);
              end;

     tarrec = record
                name    : array[0..99] of char;
                mode    : array[0..7] of char;
                uid,gid : array[0..7] of char;
                size    : array[0..11] of char;
                mtime   : array[0..11] of char;
                chksum  : array[0..11] of char;
                linkflg : char;
                linkname: array[0..99] of char;
                magic   : array[0..7] of char;
                uname   : array[0..31] of char;
                gname   : array[0..31] of char;
                devmjr  : array[0..7] of char;
                devmnr  : array[0..7] of char;
              end;

     rarhd  = record
                crc       : smallword;
                hdtype    : byte;
                flags     : smallword;
                hdsize    : smallword;
                compsize  : integer32;
                orgsize   : integer32;
                OS        : byte;
                filecrc   : longint;
                dostime   : smallword;
                dosdate   : smallword;
                extver    : byte;
                method    : char;
                namelen   : integer16;
                attrib    : longint;
                name      : shortstring;
              end;


var  dwcnum : longint;    { Anzahl DirEintr„ge }
     dwcsize: word;       { Gr”áe der Eintr„ge }


function monthlen(j,m:word):word;
begin
  case m of
    1 : monthlen:=31;
    2 : if schaltj(j) then monthlen:=29
        else monthlen:=28;
    3 : monthlen:=31;
    4 : monthlen:=30;
    5 : monthlen:=31;
    6 : monthlen:=30;
    7 : monthlen:=31;
  else  if odd(m) then monthlen:=30
        else monthlen:=31;
  end;
end;


{ Sekunden seit 1970 in DOS-Timestamp umwandeln }

procedure GetDateFrom70(secs:longint; var datum,uhrzeit:word);
const tagsec = 24*60*60;
var dt   : DateTime;
    l    : longint;
begin
  with dt do begin
    year:=1970;
    while (secs>=iif(schaltj(year),366,365)*tagsec) and (year<=2099) do begin
      dec(secs,iif(schaltj(year),366,365)*tagsec);
      inc(year);
      end;
    if year>2099 then
      secs:=0
    else begin
      month:=1;
      while (secs>=tagsec*monthlen(year,month)) do begin
        dec(secs,tagsec*monthlen(year,month));
        inc(month);
        end;
      day:=secs div tagsec + 1; secs:=secs mod tagsec;
      hour:=secs div 3600;      secs:=secs mod 3600;
      min:=secs div 60;         sec:=secs mod 60;
      end;
    end;
  packtime(dt,l);
  uhrzeit:=l and $ffff;
  datum:=l shr 16;
end;

{ negativer Wert: SFX }

function ArcType(fn:arcpath):shortint;
var f    : file;
    idr  : record
             case integer of
               0 : (l : longint);
               1 : (w,w2 : smallword);
               2 : (b : byte;
                    zipID : longint);
               3 : (txt : array[0..19] of char;
                    id : longint);
               4 : (hb : byte;
                    hw : word);
               5 : (buf: array[0..$7f] of char);    { Anfang tar-Record }
             end;
    rr   : word;
    fs   : record
             case integer of
               0 : (b    : byte;
                    ofs  : word;   { L„nge MOD 512 }
                    secs : word);  { L„nge DIV 512 + 1 }
               1 : (s:shortstring);
           end;
    typ  : longint;
    sfx  : boolean;
    sadr : longint;
    fm   : word;

label ende;

  function TestLZH:boolean;
  var lbuf  : array[0..255] of byte;
      rr    : word;
{      chk,i : byte; }
      meth  : string;
  begin                                { Funktion arc Type }
    setlength(meth,5);
    seek(f,sadr);
    blockread(f,lbuf,256,rr);
{    chk:=0;         MK 06.02.00 Programmteil abgesch., da im Original
                                 das Ergebnis von chk garnicht benutzt wird
    for i:=2 to lbuf[0]-1 do
      chk:=(chk+lbuf[i]) mod $100;
    meth:='     '; }
    Move(lbuf[2],meth[1],5);

{    TestLZH:=((chk=lbuf[1]) and (meth[1]='-') and (meth[5]='-')) or
             (meth='-lh1-'); }
    TestLZH:=(copy(meth,1,3)='-lh') or (copy(meth,1,3)='-lz');
  end;

  function TestARC:boolean;
  var lbuf  : array[0..255] of byte;
      rr    : word;
      isarc : boolean;
      b     : byte;
  begin
    seek(f,sadr);
    blockread(f,lbuf,256,rr);
    isarc:=(lbuf[0]=$1a) and (lbuf[1]<20);
    b:=2;
    while (b<=14) and (lbuf[b]<>0) do begin
      isarc:=isarc and (lbuf[b]>32) and not (chr(lbuf[b]) in ['?','*']);
      inc(b);
      end;
    TestARC:=isarc;
  end;

  function TestDWC:boolean;
  var dwh : DWChd;
  begin
    if filesize(f)<$1b then
      testDWC:=false
    else begin
      seek(f,filesize(f)-$1b);
      blockread(f,dwh,$1b);
      TestDWC:=(dwh.hdsize=$1b) and ((dwh.id='DWC') or (dwh.id='VAP'));
      end;
  end;

  function TestTAR:boolean;
    function isnum(ofs:byte):boolean;
    begin
      with idr do
        isnum:=(buf[ofs] in ['0'..'9']) and (buf[ofs+1]=' ') and (buf[ofs+2]=#0);
    end;
  begin
    TestTAR:=(idr.buf[$63]=#0) and IsNum($69) and IsNum($71) and IsNum($79);
  end;

begin
  assign(f,fn);
  fm:=filemode;
  filemode:=0;
  reset(f,1);
  filemode:=fm;
  if ioresult<>0 then
    ArcType:=0
  else begin
    if filesize(f)<$20 then
      ArcType:=0
    else begin
      fillchar(idr,sizeof(idr),0);
      blockread(f,idr,sizeof(idr),rr);
      if idr.w=$5a4d then begin         { EXE -> Test auf SFX }
        seek(f,2);
        blockread(f,fs.ofs,256,rr);
        sadr:=longint(fs.secs-1)*512+fs.ofs;
        fs.s[0]:=chr(min(255,rr));
        if sadr+$10>=filesize(f) then begin
          ArcType:=0;
          goto ende;
          end;
        seek(f,sadr);
        blockread(f,idr,sizeof(idr),rr);
        sfx:=true;
        end
      else begin
        sfx:=false;
        sadr:=0;
        end;
      with idr do
        if TestDWC then
          typ:=ArcDWC
        else if (l=$04034b50) or (ZipID=$04034b50) then
          typ:=ArcZIP
        else if id=longint($fdc4a7dc) then       { es ist ein Longint-Typ, deshalb konvertieren MK12/99}
          typ:=ArcZOO
        else if l=$21726152 then
          typ:=ArcRAR
        else if TestTAR then
          typ:=ArcTAR
        else if l=$51534c48 then
          typ:=ArcSQZ
        else if (w=$ea60) or (sfx and (w2=$ea60)) then
          typ:=ArcARJ
        else if TestLZH then
          typ:=ArcLZH
        else if TestARC then
          if not sfx and (w shr 8<=9) then
            typ:=ArcARC
          else
            typ:=ArcPAK
        else if (hb=26) and ((hw=$5048) or (hw=$5453)) then
          typ:=ArcHYP
        else if l=$1a324355 then
          typ:=ArcUC2
        else
          typ:=ArcUnknown;
      if sfx then
        ArcType:=-typ
      else ArcType:=typ;
      end;
    ende:
      close(f);
    end;
end;


{ true -> der Typ wird zwar erkannt; Auslesen des Archivs ist }
{         aber nicht m”glich.                                 }

function ArcRestricted(atyp:shortint):boolean;
begin
  arcrestricted:=(atyp=ArcUC2);
end;


{ ARJ-extended-Header berlesen }

procedure ArjSkipExt(var ar:ArchRec);
var w : smallword;
begin
  with ar do
    repeat
      if adr>=filesize(f)-1 then begin
        ende:=true;
        exit;
        end;
      seek(f,adr);
      blockread(f,w,2);
      inc(adr,w+2);
    until w=0;
end;


procedure OpenArchive(fn:arcpath; atyp:shortint; var ar:ArchRec);  { 0=detect }
var zoohd : record
              txt     : array[0..19] of char;
              id      : longint;
              firsthd : longint;
              xx      : longint;
              version : byte;
              xver    : byte;
            end;
    arjhd : record
              id     : smallword;  { $EA60 }
              hdsize : smallword;  { + 8   }
            end;
    fs    : record
              ofs  : word;   { L„nge MOD 512 }
              secs : word;   { L„nge DIV 512 + 1 }
            end;
    sfxofs: longint;
    dwh   : dwchd;
    fm    : word;
    l     : longint;

begin
  with ar do begin
    opened:=false; ende:=false;
    assign(f,fn);
    fm:=filemode;
    filemode:=0;
    reset(f,1);
    filemode:=fm;
    if ioresult<>0 then ende:=true
    else
      if filesize(f)<16 then begin
        close(f);
        ende:=true;
        end;

    if not ende then begin
      opened:=true;
      if atyp<>0 then arctyp:=atyp
      else begin
        arctyp:=ArcType(fn);
        if arctyp=ArcUnknown then begin
          close(f);
          opened:=false;
          ende:=true;
          end;
        end;
      if not ende then begin
        sfx:=(ArcTyp<0);
        ArcTyp:=Abs(ArcTyp);
        if sfx then begin
          seek(f,2);
          blockread(f,fs,4);
          sfxofs:=longint(fs.secs-1)*512+fs.ofs;
          seek(f,sfxofs);
          end
        else
          sfxofs:=0;
        case ArcTyp of
          ArcARC,
          ArcPAK : adr:=0;
          ArcLZH : adr:=0;
          ArcZOO : begin
                     blockread(f,zoohd,sizeof(zoohd));
                     adr:=zoohd.firsthd;
                   end;
          ArcZIP : begin
                     adr:=0;
                     seek(f,sfxofs);
                     blockread(f,l,4);
                     if l<>$04034b50 then inc(sfxofs);
                   end;
          ArcARJ : begin
                     adr:=0;
                     blockread(f,arjhd,sizeof(arjhd));
                     if sfx and (arjhd.id<>$ea60) and (arjhd.hdsize=$ea60)
                     then begin
                       inc(sfxofs,2);
                       blockread(f,arjhd.hdsize,2);
                       end;
                     inc(adr,arjhd.hdsize+8);  { 4 HD + 4 CRC }
                   end;
          ArcDWC : begin
                     seek(f,filesize(f)-$1b);
                     blockread(f,dwh,$1b);
                     dwcnum:=dwh.entries;
                     dwcsize:=dwh.recsize;
                     adr:=filesize(f)-$1b-dwcnum*dwcsize;
                     sfxofs:=0;
                   end;
          ArcHYP : adr:=0;
          ArcSQZ : adr:=8;
          ArcTAR : adr:=0;
          ArcRAR : adr:=7;
          ArcUC2 : adr:=4;
        end;
        inc(adr,sfxofs);
        if ArcTyp=ArcARJ then ArjSkipExt(ar);
        ArcNext(ar);
        end;
      end;
    end;
end;


procedure ArcNext(var ar:ArchRec);
var buffer : array[0..511] of byte;
    ARC    : ArcHD absolute buffer;
    LZH    : LzhHD absolute buffer;
    ZIP    : ZipHD absolute buffer;
    ZOO    : ZooRec absolute buffer;
    ARJ    : ArjRec absolute buffer;
    DWC    : DwcRec absolute buffer;
    HYP    : HyperHD absolute buffer;
    SQZ    : SqzRec absolute buffer;
    TAR    : TarRec absolute buffer;
    RAR    : RarHD absolute buffer;
    rr     : word;
    oldadr : longint;

label again;

  procedure zname(var fname);
  type ba  = shortstring;
  var  b,p : byte;
       s   : string;
  begin
    with ar do begin
       b:=0;
      while ba(fname)[b]<>#0 do inc(b);
      SetLength(s, b);
      Move(fname,s[1],b); {s[0]:=chr(b);}
      p:=pos('/',s);
      if p=0 then p:=pos('\',s);
      if p=0 then begin
        p:=pos(':',s);
        if p=0 then begin
          path:=''; name:=copy(s,1,12);
          end
        else begin
          path:=copy(s,1,p); name:=copy(s,p+1,12);
          end;
        end
      else begin
        b:=length(s);
        while (s[b]<>'/') and (s[b]<>'\') do dec(b);
        name:=copy(s,b+1,12);
        path:=copy(s,1,b);
        for b:=1 to length(path) do
          if path[b]='/' then path[b]:='\';
        end;
      end;
  end;

  function TarVAL(s:string):longint;
  var l   : longint;
 {      res : integer;           MK 12/99 }
  begin
    while (s<>'') and (LastChar(s)<'0') do SetLength(s, Length(s)-1); {dec(byte(s[0]));}
    s := TrimLeft(s);
    l:=0;
    while s<>'' do begin      { oktal-Value berechnen }
      l:=8*l+ord(s[1])-48;
      delete(s,1,1);
      end;
    TarVAL:=l;
  end;

  procedure CalcUnixTime(s:string; var zeit,datum:word);
  const  tagsec = 24*60*60;
  var dt   : DateTime;
      secs : longint;
      l    : longint;
      tage : word;

    function monthlen(j,m:word):word;
    begin
      case m of
        1 : monthlen:=31;
        2 : if schaltj(j) then monthlen:=29
            else monthlen:=28;
        3 : monthlen:=31;
        4 : monthlen:=30;
        5 : monthlen:=31;
        6 : monthlen:=30;
        7 : monthlen:=31;
      else  if odd(m) then monthlen:=30
            else monthlen:=31;
      end;
    end;

  begin
    secs:=OctVal(s);
    with dt do begin
      year:=1970;
      if schaltj(year) then tage:=366
      else tage:=365;
      while (secs>=tage*tagsec) and (year<=2099) do begin
        dec(secs,tage*tagsec);
        inc(year);
        if schaltj(year) then tage:=366
        else tage:=365;
        end;
      if year>2099 then
        secs:=0
      else begin
        month:=1;
        while (secs>=tagsec*monthlen(year,month)) do begin
          dec(secs,tagsec*monthlen(year,month));
          inc(month);
          end;
        day:=secs div tagsec + 1; secs:=secs mod tagsec;
        hour:=secs div 3600;      secs:=secs mod 3600;
        min:=secs div 60;         sec:=secs mod 60;
        end;
      end;
    packtime(dt,l);
    zeit := l and $ffff;
    datum := l shr 16;
  end;

begin
  with ar do
  again:
    if ende or (adr>=filesize(f)) then ende:=true
    else begin
      oldadr:=adr;
      seek(f,adr);
      blockread(f,buffer,sizeof(buffer),rr);
      case arctyp of
        ArcARC,
        ArcPAK : if ARC.method=0 then ende:=true
                 else begin
                   Zname(ARC.name);
                   inc(adr,ARC.compsize+sizeof(archd));
                   case ARC.method of
                     1,2  : method:='stored';
                     3    : method:='packed';
                     4    : method:='squeezed';
                     5..8 : method:='crunched';
                     9    : method:='squashed';
                     10   : method:='crushed';
                     11   : method:='distilled';
                   else
                     method:='unknown';
                   end;
                   datum:=ARC.dosdate;
                   uhrzeit:=ARC.dostime;
                   orgsize:=ARC.orgsize;
                   compsize:=ARC.compsize;
                   attrib:=0;
                   end;
        ArcLZH : if LZH.hdsize=0 then ende:=true
                 else begin
                   inc(adr,LZH.compsize+LZH.hdsize+2);
                   LZH.path[LZH.namelen]:=#0;
                   ZName(LZH.path);
                   Move(LZH.method,method[1],5);
                   method[0]:=#5;
                   insert(' ',method,1);
                   datum:=LZH.dosdate;
                   uhrzeit:=LZH.dostime;
                   orgsize:=LZH.orgsize;
                   compsize:=LZH.compsize;
                   attrib:=LZH.attrib;
                   end;
        ArcZIP : if (ZIP.namelen=0) or (ZIP.path='') or (ZIP.compsize<0)
                 then ende:=true
                 else begin
                   inc(adr,ZIP.compsize+30+ZIP.namelen+ZIP.eflen);
                   ZIP.path[min(ZIP.namelen,255)]:=#0;
                   Zname(ZIP.path);
                   case ZIP.method of
                     0    : method:='stored';
                     1    : method:='shrunk';
                     2..5 : method:='reduced';
                     6    : method:='imploded';
                     8    : method:='deflated';
                   else
                     method:='unknown';
                   end;
                   datum:=ZIP.dosdate;
                   uhrzeit:=ZIP.dostime;
                   orgsize:=ZIP.orgsize;
                   compsize:=ZIP.compsize;
                   attrib:=0;
                   end;
        ArcZOO : if ZOO.next=0 then ende:=true
                 else begin
                   adr:=ZOO.next;
                   Zname(ZOO.name[1]);
                   datum:=ZOO.dosdate;
                   uhrzeit:=ZOO.dostime;
                   orgsize:=ZOO.orgsize;
                   compsize:=ZOO.compsize;
                   if ZOO.method=0 then method:='stored'
                   else method:='crunched';
                   attrib:=0;
                   end;
        ArcARJ : if ARJ.hdsize=0 then ende:=true
                 else begin
                   inc(adr,ARJ.hdsize+8);
                   ArjSkipExt(ar);
                   inc(adr,ARJ.compsize);
                   if ARJ.flags and 8<>0 then
                     Move(ARJ.name[4],ARJ.name[0],251);
                   ZName(ARJ.name);
                   datum:=ARJ.dosdate;
                   uhrzeit:=ARJ.dostime;
                   orgsize:=ARJ.orgsize;
                   compsize:=ARJ.compsize;
                   attrib:=ARJ.attrib;
                   method:='  m'+chr(ARJ.method+48);
                   end;
        ArcDWC : if dwcnum=0 then ende:=true
                 else begin
                   inc(adr,dwcsize);
                   ZName(DWC.name);
                   GetDateFrom70(DWC.secsf70,datum,uhrzeit);
                   orgsize:=DWC.orgsize;
                   compsize:=DWC.compsize;
                   attrib:=0;
                   case DWC.method of
                     1 : method:='crunched';
                     2 : method:='stored';
                   else
                     method:='unknown';
                   end;
                   dec(dwcnum);
                   end;
        ArcHYP  : if HYP.name='' then ende:=true
                  else begin
                    name:=HYP.name;
                    datum:=HYP.dosdate;
                    uhrzeit:=HYP.dostime;
                    orgsize:=HYP.orgsize;
                    compsize:=HYP.compsize;
                    attrib:=HYP.attrib;
                    if HYP.method='ST' then
                      method:='stored'
                    else
                      method:='packed';
                    inc(adr,22+length(name)+compsize);
                  end;
        ArcSQZ  : if SQZ.hdtype=0 then ende:=true
                  else if SQZ.hdtype<18 then begin
                    if SQZ.hdtype=1 then inc(adr,SQZ.ComComp+10)
                    else inc(adr,SQZ.blksize+2);
                    goto again;
                    end
                  else begin
                    buffer[SQZ.hdtype+2]:=0;   { \0 hinter Dateiname }
                    ZName(SQZ.name);
                    method:='  m'+chr(SQZ.method+48);
                    compsize:=SQZ.compsize;
                    orgsize:=SQZ.orgsize;
                    uhrzeit:=SQZ.dostime;
                    datum:=SQZ.dosdate;
                    attrib:=SQZ.attrib;
                    inc(adr,SQZ.hdtype+2+compsize);
                    end;
        ArcTAR  : if TAR.name[0]=#0 then ende:=true
                  else begin
                    zname(TAR.name);
                    method:='stored';
                    orgsize:=TarVal(TAR.size);
                    compsize:=orgsize;
                    CalcUnixTime(TAR.mtime,uhrzeit,datum);
                    attrib:=0;
                    inc(adr,512+((compsize+511)div 512)*512);
                    end;
        ArcRAR  : begin
                    while not eof(f) and (RAR.hdtype<>$74) do begin
                      inc(adr,RAR.hdsize);
                      if RAR.flags and $8000<>0 then inc(adr,RAR.compsize);
                      seek(f,adr);
                      if not eof(f) then
                        blockread(f,buffer,sizeof(buffer),rr);
                      end;
                    if eof(f) then ende:=true
                    else begin
                      if RAR.namelen>255 then RAR.namelen:=255;
                      RAR.name[RAR.namelen]:=#0;
                      ZName(RAR.name);
                      compsize:=RAR.compsize;
                      orgsize:=RAR.orgsize;
                      method:='  m'+RAR.method;
                      datum:=RAR.dosdate;
                      uhrzeit:=RAR.dostime;
                      inc(adr,RAR.hdsize+RAR.compsize);
                      end;
                  end;
        ArcUC2  : ende:=true;
      end;
      if adr<=oldadr then ende:=true;
    end;
end;


procedure CloseArchive(var ar:ArchRec);
begin
  if ar.opened then close(ar.f);
end;


function UnSFX(name:arcpath; typ:shortint):boolean;
const maxbuf = 60000;
var f1,f2 : file;
    rr    : word;
    p     : pointer;
    ps    : word;
    fsrec : record
              ofs  : word;   { L„nge MOD 512 }
              secs : word;   { L„nge DIV 512 + 1 }
            end;
    arcofs : word;
    pkch   : array[0..1] of char;

  function SetLocalZipHeaders: Boolean; { MK 01/2000 Proz->Funk }
  var s   : string;
      b,p : byte;
      n,i : word;
      adr : longint;
      rr  : word;
      rec : record
              compsize : longint;
              orgsize  : longint;
              namelen  : word;
              elen     : word;
              commlen  : word;
              disk     : word;
              intattr  : word;
              extattr  : longint;
              lheader  : longint;
            end;
  begin
    SetLocalZipHeaders := true;
    b:=min(255,filesize(f2));
    seek(f2,filesize(f2)-b);
    blockread(f2,s[1],b);
    SetLength(s, b); {s[0]:=chr(b);}
    p:=pos('PK'#5#6,s);  { End of CentDir }
    if p=0 then begin
      SetLocalZipHeaders:=false;
      exit;
      end;
    Move(s[p+10],n,2);
    Move(s[p+16],adr,4);
    dec(adr,arcofs);
    seek(f2,filesize(f2)-b+p+15);
    blockwrite(f2,adr,4);
    for i:=1 to n do begin
      seek(f2,adr+20);
      blockread(f2,rec,sizeof(rec),rr);
      dec(rec.lheader,arcofs);
      seek(f2,adr+42);
      blockwrite(f2,rec.lheader,4);
      inc(adr,46+rec.namelen+rec.elen+rec.commlen);
      end;
  end;

begin
  UnSFX:=false;
  if typ>=0 then exit;
  assign(f1,name);
  reset(f1,1);
  seek(f1,2);
  blockread(f1,fsrec,4);
  arcofs:=longint(fsrec.secs-1)*512+fsrec.ofs;
  seek(f1,arcofs);
  if typ=-ArcZIP then begin
    blockread(f1,pkch,2);
    if pkch<>'PK' then inc(arcofs);   { altes PKZIP-SFX-Format }
    seek(f1,arcofs);
    end;
  ps := maxbuf;
  getmem(p,ps);
  assign(f2,copy(name,1,length(name)-3)+ArcName[abs(typ)]);
  rewrite(f2,1);
  repeat
    blockread(f1,p^,ps,rr);
    blockwrite(f2,p^,rr);
  until eof(f1);
  freemem(p,ps);
  close(f1);
  UnSFX:=true;
  if typ=-ArcZIP then if SetLocalZipHeaders = false then UnSFX := false;
  close(f2);
end;


function ArchiveOk(fn:arcpath):boolean;
var ar : archrec;
begin
  OpenArchive(fn,0,ar);
  with ar do
    if ende then
      ArchiveOk:=false
    else begin
      while not ende do ArcNext(ar);
      ArchiveOk:=(adr<=filesize(f));
      CloseArchive(ar);
      end;
end;


end.
{
  $Log$
  Revision 1.18  2000/08/09 13:19:09  mk
  MO: weitere Anpassungen fuer ARJ, ZIP, LZH

  Revision 1.17  2000/08/09 08:56:55  mk
  MO:- AnsiString-Fixes fuer Rar-Archive

  Revision 1.16  2000/08/08 13:18:12  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.15  2000/07/05 09:27:08  hd
  - AnsiString-Anpassung

  Revision 1.14  2000/07/04 17:33:22  mk
  - stapelweise ungenutze Routinen entfernt

  Revision 1.13  2000/07/04 10:21:36  mk
  - doppelte Routinen rausgenommen

  Revision 1.12  2000/07/02 14:24:43  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.11  2000/06/22 19:53:23  mk
  - 16 Bit Teile ausgebaut

  Revision 1.10  2000/05/02 17:48:07  mk
  - Unit crt komplett rausgenommen, da unnoetig

  Revision 1.9  2000/05/02 17:17:21  hd
  uses crt fuer Linux entfernt, da unnoetig.

  Revision 1.8  2000/03/14 15:15:34  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:35  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

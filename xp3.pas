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

{ CrossPoint - Verarbeitung von Pointdaten }

{$I XPDEFINE.INC}

unit xp3;

interface

uses
  sysutils,
  typeform,fileio,inout,datadef,database,montage,resource, xpheader,
  xp0,xp1,xp1input,xp_des,xp_pgp,xpdatum,xpglobal,classes,fidoglob;

const XreadF_error : boolean  = false;
      XReadIsoDecode : boolean = false;
      ReadHeadEmpf : shortint = 0;


function  msgmarked:boolean;                 { Nachricht markiert? }
procedure MsgAddmark;
procedure MsgUnmark;
procedure SortMark;
procedure UnsortMark;

function  ubmarked(rec:longint):boolean;     { User/Brett markiert? }
procedure UBAddMark(rec:longint);
procedure UBUnmark(rec:longint);

procedure XreadF(ofs:longint; var f:file);
procedure Xread(fn:string; append:boolean);
procedure XmemRead(ofs:word; var size:word; var data);
procedure Xwrite(fn:string);

procedure Cut_QPC_DES(var betr:string);
function  ReCount(var betr:string):integer;
procedure ReplyText(var betr:string; rehochn:boolean);

procedure BriefSchablone(pm:boolean; schab,fn:string; empf:string;
                         var realname:string);
procedure ReadHeader(var hd:theader; var hds:longint; hderr:boolean);  { Fehler-> hds=1 ! }
procedure QPC(decode:boolean; var data; size:word; passwd:pointer;
              var passpos:smallword);
procedure Iso1ToIBM(var data; size:word);
procedure IBMToIso1(var data; size:word);
function  TxtSeek(adr:pointer; size:word; igcase,umlaut:boolean):boolean;

function  newdate:longint;    { Datum des letzten Puffer-Einlesens }

procedure AddNewUser(const UserName, PollBox: string);
function  EQ_betreff(var betr:string):boolean;
function  grQuoteMsk:string;
function  isbox(const box:string):boolean;
procedure ReplaceVertreterbox(var box:string; pm:boolean);

procedure wrkilled;
procedure brettslash(var s:string);
procedure getablsizes;
function  QuoteSchab(pm:boolean):string;
procedure ClearPGPflags(hdp:theader);

function  MakeFidoAdr(var frec:fidoadr; usepoint:boolean):string;
function  IsNodeAddress(adr:string):boolean;
procedure SetDefZoneNet;   { Fido-Defaultzone/Net setzen }

function  vert_name(s:string):string;
function  vert_long(s:string):string;
function  systemname(adr:string):string;
function  pfadbox(zconnect:boolean; var pfad:String):string;
function  file_box(d:DB; dname:string):string;
function  box_file(box:string):string;
function  brettok(trenn:boolean):boolean;

function  extmimetyp(typ:string):string;
function  compmimetyp(typ:string):string;

var
  TxtSeekKey: ShortString;

implementation  {-----------------------------------------------------}

{ Brettcodes:   '$'  ==  intern/lokal, z.B. Netzanruf
                '1'  ==  /PM-Bretter
                'A'  ==  Netzbretter
                'U'  ==  Userbretter (nur in der MBase) }

uses  xp3o,xp3ex,xpnt, xpmakeheader;


procedure QPC(decode:boolean; var data; size:word; passwd:pointer;
              var passpos:smallword); assembler; {&uses ebx, esi, edi}
asm
         mov   edi,passpos
         xor   ebx, ebx
         mov   bx,[edi]
         mov   edi,data
         mov   edx,size
         mov   esi,passwd
         mov   ch,[esi]                 { Pa·wort-LÑnge }
         mov   cl,4                     { zum Nibble-Tauschen }
         mov   ah,decode
         cld

@QPClp:  mov   al,[edi]                { Original-Byte holen }
         or    ah,ah                   { decodieren ? }
         jnz   @code1
         rol   al,cl                   { Nibbles vertauschen }
@code1:  xor   al,[esi+ebx]              { Byte codieren }
         inc   bl
         cmp   bl,ch                   { am PW-Ende angekommen? }
         jbe   @pwok
         mov   bl,1                    { PW-Index auf 1 zurÅcksetzen }
@pwok:   or    ah,ah                   { codieren? }
         jz    @code2
         rol   al,cl                   { Nibbles vertauschen }
@code2:  stosb
         dec   edx                     { nÑchstes Byte }
         jnz   @QPClp

         mov   edi,passpos              { neuen PW-Index speichern }
         mov   [edi],bx
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

function TxtSeek(adr:pointer; size:word; igcase,umlaut:boolean):
         boolean;assembler;
asm
         push ebp
         cld
         mov   esi,adr
         mov   ecx, size
         or    ecx, ecx
         jz    @nfound
         mov   dh,umlaut
         cmp   dh,0                   { Bei Umlautsensitiver Suche zwingend ignore Case. }
         jne   @icase
         cmp   igcase,0               { ignore case? }
         jz    @case

@icase:  push  ecx
         push  esi

@cloop:  lodsb                        {  den kompletten Puffer in }
         cmp   al,'Ñ'
         jnz   @no_ae
         mov   al,'é'
         jmp   @xl
@no_ae:  cmp   al,'î'
         jnz   @no_oe
         mov   al,'ô'
         jmp   @xl
@no_oe:  cmp   al,'Å'
         jnz   @no_ue
         mov   al,'ö'
         jmp   @xl

@no_ue:  cmp   al,'Ç'
         je    @is_eac
         cmp   al,'ê'
         jne   @no_eac
@is_eac: mov   al,'E'
         jmp   @xl

@no_eac: cmp   al,'a'                 {  UpperCase umwandeln }
         jb    @noc
         cmp   al,'z'
         ja    @noc
         sub   al,32
@xl:     mov   [esi-1],al
@noc:    loop  @cloop
         pop   esi
         pop   ecx

@case:   mov   edi,offset TxtSeekKey
         sub   cl,[edi]
         sbb   ch,0
         jc    @nfound                 { key >= LÑnge }
         inc   ecx

@sblp1:  xor   ebx,ebx                 { Suchpuffer- u. String-Offset }
         xor   ebp,ebp
         mov   dl,[edi]              { Key-LÑnge }
@sblp2:  mov   al,[esi+ebx]
@acctst: cmp   al,[edi+ebp+1]
         jnz   @testul
@ulgood: inc   ebx                    { Hier gehts weiter nach Erfolgreichem Umlautvergleich }
         inc   ebp
         dec   dl
         jz    @found
         jmp   @sblp2

@testul:                                { WILDCARDS }
         mov ah,[edi+ebp+1]
         cmp ah,'?'                     { ? = Ein Zeichen beliebig }
         je @ulgood                     {--------------------------}


         cmp ah,'*'                     { * = mehrere Zeichen beliebig }
         jne @ultst                     { ---------------------------- }

@1:      inc ebp
         dec dl                         { Naechstes Suchkey-Zeichen laden }
         jz @found                      { Kommt keines mehr, Suche erfolgreich }

@2:      mov al,[esi+ebx]               { im Text Nach Anfang des rests suchen }
         cmp al,[edi+ebp+1]
         je @3
         cmp al,' '                     { Abbruch bei Wortende }
         jbe @nextb
         inc ebx
         jmp @2

@3:      inc ebx                        {Weitervergleichen bis naechster * oder Suchkeyende }
         inc ebp
         dec dl
         jz @found                      { Bei Suchkeyende ist Suche erfolgreich }
         mov al,[edi+ebp+1]
         cmp al,[esi+ebx]               { weiter im Text solange er passt }
         je @3
         cmp al,'?'                     { ...oder "?" - Wildcard greift }
         je @3
         cmp al,'*'                     { bei neuem * - Wildcard diesen ueberspringen }
         je @1
         jmp @2                         { Textanfang erneut suchen }

                                        {--------------}
@ultst:  cmp dh,0                       { UMLAUTSUCHE }
         je @nextb                      { Aber nur wenn erwuenscht... }

         mov ah,'E'

         cmp al,'é'                     { Wenn "é" im Puffer ist, }
         jne @@1
         mov al,'A'
@ultest: cmp ax,[edi+ebp+1]            { Dann auf "AE" Testen. }
         jne @nextb
         inc ebp                        { Wenn gefunden: Zeiger im Suchbegriff }
         dec dl                         { und Restsuchlange um ein Zeichen weiterschalten }
         jmp @ulgood                    { und oben weitermachen. }

@@1:     cmp al,'ô'
         jne @@2
         mov al,'O'                     { "OE"... }
         jmp @ultest

@@2:     cmp al,'ö'
         jne @@3
         mov al,'U'                     { "UE"... }
         jmp @ultest

@@3:     cmp al,'·'
         jne @@4
         mov ax,'SS'                    { und "SS"... }
         jmp @ultest
@@4:                                    {--------------}

@nextb:  inc   esi                       { Weitersuchen... }
         dec   ecx
         jne @sblp1
@nfound: xor   eax,eax
         jmp   @ende
@found:  mov   eax,1
@ende:   pop ebp
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure Iso1ToIBM(var data; size:word); assembler;
asm
          mov    ecx,size
          jecxz  @noconv1
          mov    edi,data
          mov    ebx,offset ISO2IBMtab - 128
          cld
@isolp1:  mov    al,[edi]
          or     al,al
          jns    @ii1
          xlatb
@ii1:     stosb
          loop   @isolp1
@noconv1:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure IBMToIso1(var data; size:word); assembler;
asm
          mov    ecx,size
          jecxz  @noconv2
          mov    edi,data
          mov    ebx,offset IBM2ISOtab
          cld
@isolp2:  mov    al,[edi]
          xlatb
          stosb
          loop   @isolp2
@noconv2:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }

{ Datum des letzten Puffer-Einlesens ermitteln }

function newdate:longint;
var t : text;
    s : string;
begin
  assign(t,ownpath+newdatefile);
  if not existf(t) then
    newdate:=ixdat(Zdate)
  else begin
    reset(t);
    readln(t,s);
    close(t);
    newdate:=ixdat(LeftStr(trim(s),10));
    end;
end;

{ in dieser Prozedur kein ReadN/WriteN verwenden, wegen }
{ XP2.NewFieldMessageID !                               }

procedure ReadHeader(var hd:theader; var hds:longint; hderr:boolean);
var ok     : boolean;
    puffer : file;
    ablg   : byte;
    flags  : byte;
    errstr : string[30];
    empfnr : smallword;
    nopuffer: boolean;
begin
  ok:=true;
  dbReadN(mbase,mb_ablage,ablg);
  hds:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');
  if (hds<0) or (hds>iif(ntZconnect(ablg),1000000,8000)) then begin
    ok:=false;
    errstr:=getres(300);  { '; falsche Grî·enangaben' }
    end
  else begin
    assign(puffer,aFile(ablg));
    reset(puffer,1);
    nopuffer:=(ioresult<>0);
    if nopuffer then begin
      ok:=false;
      errstr:=getres(301);  { '; Ablage fehlt' }
      end
    else begin
      seek(puffer,dbReadInt(mbase,'adresse'));
      if eof(puffer) then begin
        ok:=false;
        errstr:=getres(302)  { '; Adre·index fehlerhaft' }
        end;
      end;
    if ReadHeadEmpf=0 then
      empfnr:=dbReadInt(mbase,'netztyp') shr 24
    else begin
      empfnr:=ReadHeadEmpf; ReadHeadEmpf:=0;
      end;
    if ok then makeheader(ntZCablage(ablg),puffer,empfnr,hds,hd,
                          ok,true, true);
    if not nopuffer then
      close(puffer);
    errstr:='';
    end;
  if not ok then begin
    hds:=1;
    if hderr then begin
      fehler(getres(303)+strs(ablg)+errstr+')');  { 'Nachricht ist beschÑdigt  (Ablage ' }
      aufbau:=true;
      end;
    flags:=2;
    dbWriteN(mbase,mb_halteflags,flags);
    end;
  if LeftStr(hd.empfaenger,TO_len)=TO_ID then   { /TO: }
    hd.empfaenger:=Mid(hd.empfaenger,9);
  ReadEmpflist:=false; 
  ReadKopList:=false;
end;


function buferr(nr:byte):string;
begin
  buferr:=getreps(304,strs(nr))+#13#10;   { 'Ablage Nr. %s ist fehlerhaft!' }
end;


procedure XreadF(ofs:longint; var f:file);
var p        : pointer;
    bufs,rr  : word;
    puffer   : file;
    ablage   : byte;
    size     : longint;
    adr      : longint;
    berr     : string[40];
    iso      : boolean;
    hdp      : theader;
    hds      : longint;
    minus    : longint;

label ende;
begin
  bufs:=65536;
  dbReadN(mbase,mb_ablage,ablage);
  assign(puffer,aFile(ablage));
  reset(puffer,1);
  if ioresult<>0 then begin
    fehler(getreps(305,strs(ablage)));   { 'Ablage %s fehlt!' }
    XReadIsoDecode:=false;
    exit;
    end;
  getmem(p,bufs);
  dbReadN(mbase,mb_adresse,adr);
  minus:=0;
  if dbReadInt(mbase,'netztyp') and $8000<>0 then begin  { KOM vorhanden }
    hdp := THeader.Create;
    ReadHeader(hdp,hds,false);
    if (hdp.komlen>0) and (ofs=hds+hdp.komlen) then
      minus:=hdp.komlen;
    Hdp.Free;
    end;
  if adr+ofs-minus+dbReadInt(mbase,'groesse')>filesize(puffer) then begin
    berr:=buferr(ablage);
    blockwrite(f,berr[1],length(berr));
    end
  else begin
    seek(puffer,adr+ofs);
    dbReadN(mbase,mb_msgsize,size);
    dec(size,ofs);
    iso:=XReadIsoDecode and (dbReadInt(mbase,'typ')=ord('T')) and
         (dbReadInt(mbase,'netztyp') and $2000<>0);
    while size>0 do begin
      blockread(puffer,p^,min(bufs,size),rr);
      if inoutres<>0 then begin
        tfehler('XReadF: '+ioerror(ioresult,getreps(306,strs(ablage))),30);  { 'Fehler beim Lesen aus Ablage %s' }
        goto ende;
        end;
      if iso then Iso1ToIBM(p^,rr);
      blockwrite(f,p^,rr);
      if inoutres<>0 then begin
        tfehler('XReadF: '+ioerror(ioresult,getres(307)),30);  { 'Fehler beim Schreiben in Datei' }
        XreadF_error:=true;
        goto ende;
        end;
      dec(size,rr);
      if (size>0) and eof(puffer) then
        size:=0;
      end;
    end;
ende:
  close(puffer);
  if ioresult = 0 then ;
  freemem(p,bufs);
  XReadIsoDecode:=false;
end;


procedure Xread(fn:string; append:boolean);
var f : file;
begin
  assign(f,fn);
  if FileExists(fn) and append then begin
    reset(f,1);
    seek(f,filesize(f));
    end
  else
    rewrite(f,1);
  XreadF(0,f);
  close(f);
end;


procedure XmemRead(ofs:word; var size:word; var data);
var puffer   : file;
    ablage   : byte;
begin
  if ofs<dbReadInt(mbase,'msgsize') then begin
    dbReadN(mbase,mb_ablage,ablage);
    assign(puffer,aFile(ablage));
    reset(puffer,1);
    seek(puffer,dbReadInt(mbase,'adresse')+ofs);
    blockread(puffer,data,size,size);
    if ioresult = 0 then ;
    close(puffer);
    end
  else
    fillchar(data,size,0);
end;


procedure Xwrite(fn:string);
var f,puffer : file;
    ablage   : byte;
    oldsize  : longint;
    oldadr   : longint;
    adr,size : longint;
    p        : pointer;
    bs,rr    : word;
begin
  bs:=65536;
  getmem(p,bs);
  dbReadN(mbase,mb_ablage,ablage);
  dbReadN(mbase,mb_msgsize,oldsize);
  dbReadN(mbase,mb_adresse,oldadr);
  assign(puffer,aFile(ablage));
  reset(puffer,1);
  if ioresult<>0 then
    rewrite(puffer,1)
  else
    if (oldsize>0) and (oldadr+oldsize=filesize(puffer)) then begin
      seek(puffer,oldadr);    { letzte Nachricht -> an gleicher Stelle ablegen }
      truncate(puffer);
      end
    else
      seek(puffer,filesize(puffer));
  adr:=filepos(puffer);
  dbWriteN(mbase,mb_adresse,adr);
  assign(f,fn);
  reset(f,1);
  size:=filesize(f);
  dbWriteN(mbase,mb_msgsize,size);
  while size>0 do begin
    blockread(f,p^,bs,rr);
    blockwrite(puffer,p^,rr);
    dec(size,rr);
    end;
  close(puffer);
  close(f);
  freemem(p,bs);
end;


{ R-}
procedure seekmark(rec:longint; var found:boolean; var x:integer);
var l,r,m : integer;
begin
  if not marksorted then begin
    l:=-1; r:=markanz;
    found:=false;
    while not found and (l+1<r) do begin
      m:=(l+r) div 2;
      if marked^[m].recno=rec then begin
        found:=true; l:=m; end
      else
        if marked^[m].recno<rec then r:=m
        else l:=m;
      end;
    x:=l;
    end
  else begin
    found:=false;
    x:=0;
    while (x<markanz) and not found do
      if marked^[x].recno=rec then found:=true
      else inc(x);
    end;
end;


function msgmarked:boolean;
var found : boolean;
    x     : integer;
begin
  if markanz=0 then
    msgmarked:=false
  else begin
    seekmark(dbRecno(mbase),found,x);
    msgmarked:=found;
    end;
end;


procedure MsgAddmark;
var found : boolean;
    x     : integer;
    dat   : longint;
    intnr : longint;
begin
  dbReadN(mbase,mb_empfdatum,dat);
  dbRead(mbase,'int_nr',intnr);
  seekmark(dbRecno(mbase),found,x);
  if not found and (markanz<maxmark) then begin
    if marksorted then
      x:=markanz
    else begin
      inc(x);
        if x<markanz then { Longint, da sonst bei 2731 Nachrichten RTE 215 }
          { ACHTUNG: Hier kein Move wegen Åberlappenden Speicherbereichen! }
          { SizeOf(MarkRec) ist 12, die MarkAnz kann bis 5000 sein. Um
            einen Integer-Ueberlauf nach Multiplikation zu verhindern muss
            mit Word gerechnet werden, so das mehr als 32kb verschoben werden
            koennen. Das tritt bei 2731 Nachrichten auf (65536 div 12 div 2) }
          Move(marked^[x],marked^[x+1],word(sizeof(markrec))*word(markanz-x));
      end;
    inc(markanz);
    marked^[x].recno:=dbRecno(mbase);
    marked^[x].datum:=dat;
    marked^[x].intnr:=intnr;
  end;
end;


procedure MsgUnmark;
var found : boolean;
    x     : integer;
begin
  seekmark(dbRecno(mbase),found,x);
  if found then begin
    dec(markanz);
    if (x<markanz) then
      Move(marked^[x+1],marked^[x],word(sizeof(markrec))*word((markanz-x)));
    end;
end;


procedure SortMark;

  procedure sort(l,r:integer);
  var i,j : integer;
      x,y : longint;
      w   : markrec;
  begin
    i:=l; j:=r;
    x:=marked^[(l+r) div 2].datum;
    y:=marked^[(l+r) div 2].intnr;
    repeat
      while smdl(marked^[i].datum,x) or
            ((marked^[i].datum=x) and (marked^[i].intnr<y)) do inc(i);
      while smdl(x,marked^[j].datum) or
            ((marked^[j].datum=x) and (marked^[j].intnr>y)) do dec(j);
      if i<=j then begin
        w:=marked^[i]; marked^[i]:=marked^[j]; marked^[j]:=w;
        inc(i); dec(j);
        end;
    until i>j;
    if l<j then sort(l,j);
    if r>i then sort(i,r);
  end;

begin
  if markanz>0 then
    sort(0,markanz-1);
  marksorted:=true;
end;


procedure UnsortMark;

  procedure sort(l,r:integer);
  var i,j : integer;
      x   : longint;
      w   : markrec;
  begin
    i:=l; j:=r;
    x:=marked^[(l+r) div 2].recno;
    repeat
      while marked^[i].recno>x do inc(i);
      while marked^[j].recno<x do dec(j);
      if i<=j then begin
        w:=marked^[i]; marked^[i]:=marked^[j]; marked^[j]:=w;
        inc(i); dec(j);
        end;
    until i>j;
    if l<j then sort(l,j);
    if r>i then sort(i,r);
  end;

begin
  if markanz>0 then
    sort(0,markanz-1);
  marksorted:=false;
end;
{ R+}


procedure seekbmark(_marked:bmarkp; _markanz:integer; rec:longint;
                    var found:boolean; var x:integer);
var l,r,m : integer;
begin
  l:=-1; r:=_markanz;
  found:=false;
  while not found and (l+1<r) do begin
    m:=(l+r) div 2;
    if _marked^[m]=rec then begin
      found:=true; l:=m; end
    else
      if _marked^[m]<rec then r:=m
      else l:=m;
    end;
  x:=l;
end;


function UBmarked(rec:longint):boolean;
var found : boolean;
    x     : integer;
begin
  if bmarkanz=0 then
    UBmarked:=false
  else begin
    seekbmark(bmarked,bmarkanz,rec,found,x);
    UBmarked:=found;
    end;
end;


procedure UBAddmark(rec:longint);
var found : boolean;
    x     : integer;
begin
  seekbmark(bmarked,bmarkanz,rec,found,x);
  if not found and (bmarkanz<maxbmark) then begin
    inc(x);
    if x<bmarkanz then
      Move(bmarked^[x],bmarked^[x+1],4*(bmarkanz-x));
    inc(bmarkanz);
    bmarked^[x]:=rec;
    end;
end;


procedure UBUnmark(rec:longint);
var found : boolean;
    x     : integer;
begin
  seekbmark(bmarked,bmarkanz,rec,found,x);
  if found then begin
    dec(bmarkanz);
    if (x<bmarkanz) then
      Move(bmarked^[x+1],bmarked^[x],4*(bmarkanz-x));
    end;
end;


procedure Cut_QPC_DES(var betr:string);
begin
  if IS_QPC(betr) then
    betr:=mid(betr,length(QPC_ID)+1);
  if IS_DES(betr) then
    betr:=mid(betr,length(DES_ID)+1);
end;


{ zÑhlt die Replys am linken Ende von betr; betr wird dabei geÑndert! }

function ReCount(var betr:string):integer;
const reanz = 8;
      retyp : array[1..reanz] of string[10] =
                ('A:','RE:','RE :','A.AUF','A. AUF','1.A.AUF:','1. A. AUF:',
                 'AW:');
var xch   : boolean;
    cnt   : integer;
    recnt : longint;
    p,i   : byte;
begin
  cnt:=0;
  Cut_QPC_DES(betr);
  p:=pos('(war:',LowerCase(betr)); if p>0 then betr:=LeftStr(betr,p-1);
  p:=pos('(was:',LowerCase(betr)); if p>0 then betr:=LeftStr(betr,p-1);
  repeat
    xch:=false;
    betr:=trim(betr);
    for i:=1 to ReAnz do
      if UpperCase(LeftStr(betr,length(retyp[i])))=retyp[i] then begin
        inc(cnt); betr:=trim(copy(betr,length(retyp[i])+1,BetreffLen));
        xch:=true;
        end;
    if UpperCase(LeftStr(betr,3))='RE^' then begin
      p:=4;
      while (p<=length(betr)) and (betr[p]>='0') and (betr[p]<='9') do
        inc(p);
      recnt:=ival(copy(betr,4,p-4));
      betr:=copy(betr,p+1,BetreffLen); xch:=true;
      if recnt<100 then inc(cnt,recnt);
      end;
  until not xch;
  ReCount:=cnt;
end;


procedure ReplyText(var betr:string; rehochn:boolean);
var cnt : integer;
begin
  cnt:=ReCount(betr);
  if (cnt=0) or (cnt>99) or not rehochn then betr:=LeftStr('Re: '+betr,betrefflen)
  else betr:=LeftStr('Re^'+strs(cnt+1)+': '+betr,betrefflen);
end;

// Add a new user to the database
procedure AddNewUser(const UserName, PollBox: string);
var
  b: byte;
begin
  dbAppend(ubase);
  dbWriteNStr(ubase,ub_username,UserName);
  dbWriteNStr(ubase,ub_pollbox,pollbox);
  dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
  dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
  b := 1 + iif(newuseribm,0,8);
  dbWriteN(ubase,ub_userflags, b);
end;


{ EQ-betreff = LeftStr(betreff)='*crypted*' and hexval(RightStr(betreff))=orgGroesse }
{        oder  (betr = dbreadStr(mbase,betreff)) oder                         }

function EQ_betreff(var betr:string):boolean;
var pmcrypted : boolean;
    betreff   : string;
    b2        : string;
begin
  Betreff := dbReadNStr(mbase,mb_betreff);
  betreff:=trimright(betreff);
  b2:=trimright(LeftStr(betr,40));
  pmcrypted:=(LeftStr(betr,length(PMC_ID))=PMC_ID);
  EQ_betreff:=(pmcrypted and (hexval(RightStr(betr,6))=dbReadInt(mbase,'groesse'))) or
              (b2=betreff) or (b2=LeftStr(QPC_ID+betreff,40)) or
              (b2=LeftStr(DES_ID+betreff,40));
end;


{ bestimmt den Namen der passenden Quote-Schablone zu aktuellen        }
{ Nachricht. Entweder QuoteMsk, oder eine gruppenspezifische Schablone }

function grQuoteMsk:string;
var d     : DB;
    grnr  : longint;
    brett : string;
    n     : integer;
    qm    : string;
begin
  grQuoteMsk:=QuoteMsk;
  Brett := dbReadNStr(mbase,mb_brett);
  dbSeek(bbase,biIntnr,copy(brett,2,4));
  if dbFound then begin
    dbReadN(bbase,bb_gruppe,grnr);
    dbOpen(d,'gruppen',1);
    dbSeek(d,giIntnr,dbLongStr(grnr));
    if dbFound then begin
      n:=dbGetFeldNr(d,'quotemsk');
      if n>0 then begin
        qm:= dbReadNStr(d,n);
        if trim(qm)<>'' then grQuoteMsk:=trim(qm)+extXps;
        end;
      end;
    dbClose(d);
    end;
end;


procedure BriefSchablone(pm:boolean; schab,fn:string; empf:string;
                         var realname:string);
var t1,t2 : text;
    s     : string;
begin
  if FileExists(schab) then begin
    assign(t1,schab); reset(t1);
    assign(t2,fn); rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s);
      if pm then rpsUser(s,empf,realname);
      rpsdate(s);
      writeln(t2,s);
      end;
    close(t1);
    close(t2);
    end;
end;


function isbox(const box:string):boolean;
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  isbox:=dbFound;
  dbClose(d);
end;


{ Killed-Flag fÅr Ablage der aktuellen Nachricht setzen   }
{ diese Ablage wird bei der nÑchsten Reorg auf jeden Fall }
{ reorganisiert                                           }

procedure wrkilled;
type ba = array[0..ablagen-1] of boolean;
var  f  : file;
     rr : word;
     b  : ba;
     abl : byte;
begin
  if not dbEOF(mbase) and not dbBOF(mbase) then begin
    dbReadN(mbase,mb_ablage,abl);
    if abl<ablagen then begin
      fillchar(b,sizeof(b),false);
      assign(f,killedDat);
      if existf(f) then begin
        reset(f,1); blockread(f,b,ablagen,rr); seek(f,0);
        end
      else
        rewrite(f,1);
      if not b[abl] then begin
        b[abl]:=true;
        blockwrite(f,b,ablagen);
        end;
      close(f);
      end;
    end;
end;


procedure brettslash(var s:string);
begin
  if FirstChar(s)<>'/' then s:='/'+s;
end;


procedure getablsizes;
var i : byte;
begin
  for i:=0 to ablagen-1 do
    ablsize[i]:=_filesize(AblagenFile+strs(i));
end;


function QuoteSchab(pm:boolean):string;
begin
  if pm then
    if FirstChar(dbReadStrN(mbase,mb_brett))='A' then
      QuoteSchab:=QuotePriv
    else
      QuoteSchab:=QuotePMpriv
  else
    QuoteSchab:=QuoteMsk;
end;


function vert_name(s:string):string;
begin
  if FirstChar(s)<>vert_char then
    vert_name:=s
  else begin
    if cpos('@',s)>0 then truncstr(s,cpos('@',s)-1);
    vert_name:=mid(s,2);
    end;
end;

function vert_long(s:string):string;
begin
  if (FirstChar(s)='[') and (LastChar(s)=']') then
    vert_long:=vert_char+s+'@V'
  else
    vert_long:=s;
end;


{ Systemname aus einer Adresse ausfiltern }

function systemname(adr:string):string;
var p : byte;
begin
  p:=cpos('@',adr);
  if p=0 then systemname:=DefaultBox
  else begin
    adr:=mid(adr,p+1);
    p:=cpos('.',adr);
    if p=0 then systemname:=adr
    else systemname:=LeftStr(adr,p-1);
    end;
end;


function pfadbox(zconnect:boolean; var pfad:String):string;
var p : byte;
begin
  if zconnect then begin
    p:=1;
    while (p<=length(pfad)) and (pfad[p]<>'!') and (pfad[p]<>'.') and
          (pfad[p]<>';') and   { ";" wg. ProNet }
          (pfad[p]<>'@') do    { "@" wg. FidoNet-Domains }
      inc(p);
    pfadbox:=trim(LeftStr(pfad,p-1));
    end
  else begin
    p:=length(pfad);
    while (p>0) and (pfad[p]<>'!') do dec(p);
    if p=0 then pfadbox:=trim(pfad)
    else pfadbox:=trim(mid(pfad,p+1));
    end;
end;


function file_box(d:DB; dname:string):string;
var open : boolean;
begin
  open:=(d<>nil);
  if not open then
    dbOpen(d,BoxenFile,1);
  dbSeek(d,boiDatei,dname);
  if dbFound then file_box:=dbReadStr(d,'boxname')
  else file_box:=dname;
  if not open then
    dbClose(d);
end;


function box_file(box:string):string;
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then box_file:=dbReadStr(d,'dateiname')
  else box_file:=box;
  dbClose(d);
end;


{ trifft aktuelles Brett auf den Lesemode zu? }

function brettok(trenn:boolean):boolean;   { s. auch XP4D.INC.Write_Disp_Line }
begin
  if dbEOF(bbase) or dbBOF(bbase) then
    brettok:=false
  else if trennall and trenn and (LeftStr(dbReadStrN(bbase,bb_brettname),3)='$/T') then
    brettok:=true
  else
    case readmode of
      0 : brettok:=true;
      1 : brettok:=(dbReadInt(bbase,'flags') and 2<>0);
    else
      brettok:=(not smdl(dbReadInt(bbase,'LDatum'),readdate));
    end;
end;


function MakeFidoAdr(var frec:fidoadr; usepoint:boolean):string;
begin
  with frec do
    MakeFidoadr:=strs(zone)+':'+strs(net)+'/'+strs(node)+
                 iifs(ispoint and usepoint,'.'+strs(point),'');
end;

function IsNodeAddress(adr:string):boolean;
var p : byte;
begin
  p:=cpos(':',adr);
  if p=0 then p:=cPos('/',adr);
  if p=0 then p:=cPos('.',adr);
  IsNodeAddress := ((p>0) and (ival(LeftStr(adr,p-1))>0)) or
                   (ival(adr)>0) or (adr=',') or
                   ((p=1) and (ival(mid(adr,p+1))>0));
end;

procedure SetDefZoneNet;   { Fido-Defaultzone/Net setzen }
var fa : FidoAdr;
begin
  Splitfido(DefFidoBox,fa,2);
  DefaultZone:=fa.zone;
  DefaultNet:=fa.net;
  DefaultNode:=fa.node;
end;


procedure ReplaceVertreterbox(var box:string; pm:boolean);
var d    : DB;
    wbox : string;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then begin              { Test auf Vertreterbox }
    if pm then
      wbox:= dbReadStr(d,'PVertreter')
    else
      wbox:= dbReadStr(d,'AVertreter');
    if IsBox(wbox) then box:=wbox;
    end;
  dbClose(d);
end;


procedure ClearPGPflags(hdp:theader);
begin
  hdp.pgpflags:=hdp.pgpflags and (not fPGP_haskey);
end;


function extmimetyp(typ:string):string;
begin
  if firstchar(typ)='/' then
    extmimetyp:='application'+typ
  else
    extmimetyp:=typ;
end;


function compmimetyp(typ:string):string;
begin
  if LeftStr(typ,12)='application/' then
    compmimetyp:=LowerCase(mid(typ,12))
  else
    compmimetyp:=LowerCase(typ);
end;

initialization
  EmpfList := TStringList.Create;
finalization
  EmpfList.Free;
{
  $Log$
  Revision 1.67  2001/09/08 16:29:32  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.66  2001/09/07 13:54:19  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.65  2001/09/07 10:56:00  mk
  - added GetServerFilename

  Revision 1.64  2001/09/07 08:28:02  mk
  - added new procedure: AddNewBezug, collects three pieces of code

  Revision 1.63  2001/09/06 19:31:19  mk
  - removed some hints und warnings

  Revision 1.62  2001/08/31 14:44:37  mk
  - changed TxtSeek for Delphi/Kylix compatiblity

  Revision 1.61  2001/08/12 11:50:37  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.60  2001/08/11 23:06:30  mk
  - changed Pos() to cPos() when possible

  Revision 1.59  2001/07/31 13:10:33  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.58  2001/07/28 12:04:11  mk
  - removed crt unit as much as possible

  Revision 1.57  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.56  2001/01/14 10:13:33  mk
  - MakeHeader() integreated in new unit

  Revision 1.55  2001/01/11 13:21:35  mk
  - fixed chararr-bugs and removed some unnecessary defines

  Revision 1.54  2001/01/04 16:54:21  mk
  - const-Parameter in isbox() verwenden

  Revision 1.53  2001/01/02 10:05:24  mk
  - implemented Header.References

  Revision 1.52  2000/12/27 22:36:35  mo
  -new class TfidoNodeList

  Revision 1.51  2000/12/05 14:58:09  mk
  - AddNewUser

  Revision 1.50  2000/12/03 12:38:21  mk
  - Header-Record is no an Object

  Revision 1.49  2000/11/30 14:38:09  mk
  - fixed NewUserIBM when adding new uesers

  Revision 1.48  2000/11/14 15:51:29  mk
  - replaced Exist() with FileExists()

  Revision 1.47  2000/11/14 11:14:32  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.46  2000/11/09 18:15:11  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.45  2000/10/17 10:05:49  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.44  2000/08/20 10:43:46  mk
  - Clearheader war nicht noetig, entfernt

  Revision 1.43  2000/08/08 00:02:55  mk
  - TxtSeek auf Shortstring umgestellt

  Revision 1.42  2000/07/26 09:29:37  mk
  - Fehler beim Anzeigen von Nachrichten mit KOM-Header beseitigt

  Revision 1.41  2000/07/23 21:20:47  mk
  - Bugfix fuer neue makeheader-definition

  Revision 1.40  2000/07/22 14:05:26  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.39  2000/07/21 20:56:23  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.38  2000/07/21 17:39:52  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.37  2000/07/21 13:23:45  mk
  - Umstellung auf TStringList

  Revision 1.36  2000/07/20 17:03:21  mk
  - HugeString -> String

  Revision 1.35  2000/07/20 16:49:58  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.34  2000/07/09 08:35:15  mk
  - AnsiStrings Updates

  Revision 1.33  2000/07/05 15:46:47  hd
  - AnsiString

  Revision 1.32  2000/07/04 12:04:22  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.31  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.30  2000/07/03 13:31:40  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.29  2000/07/02 14:24:53  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.28  2000/07/02 14:11:24  mk
  JG: - Volltextsuche mit Wildcards implementiert

  Revision 1.27  2000/06/29 13:00:55  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.26  2000/06/23 15:59:19  mk
  - 16 Bit Teile entfernt

  Revision 1.25  2000/06/05 16:16:22  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.24  2000/05/26 00:01:10  mk
  - Assembler-Fixes (32 Bit)

  Revision 1.23  2000/05/04 10:26:03  mk
  - UUZ teils auf HugeString umgestellt

  Revision 1.22  2000/05/03 00:21:21  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.21  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.20  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.19  2000/04/15 09:58:00  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.18  2000/04/04 21:01:23  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.17  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.16  2000/03/19 21:31:51  mk
  - Fix f¸r 32 Bit TxtSeek

  Revision 1.15  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.14  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.13  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.12  2000/02/20 20:46:17  jg
  Sourcefiles wieder lesbar gemacht (CRCRLF gegen CRLF getauscht)
  Todo aktualisiert

  Revision 1.11  2000/02/20 17:22:10  ml
  Kommentare in MsgAddMark hinzugefuegt

  Revision 1.10  2000/02/20 14:48:21  jg
  -Bugfix: Nachrichten entmarkieren, wenn viele
   Nachrichten markiert waren (Msgunmark)

  Revision 1.9  2000/02/19 18:00:24  jg
  Bugfix zu Rev 1.9+: Suchoptionen werden nicht mehr reseted
  Umlautunabhaengige Suche kennt jetzt "Ç"
  Mailadressen mit "!" und "=" werden ebenfalls erkannt

  Revision 1.8  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.7  2000/02/18 18:39:04  jg
  Speichermannagementbugs in Clip.pas entschaerft
  Prozedur Cliptest in Clip.Pas ausgeklammert
  ROT13 aus Editor,Lister und XP3 entfernt und nach Typeform verlegt
  Lister.asm in Lister.pas integriert

  Revision 1.6  2000/02/18 09:13:27  mk
  JG: * Volltextsuche jettz Sprachabhaengig gestaltet
      * XP3.ASM in XP3.PAS aufgenommen

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


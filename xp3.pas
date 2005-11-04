{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{$I xpdefine.inc}

unit xp3;

interface

uses
  sysutils,classes,
  datadef,xpheader,fidoglob,addresses,
  xpglobal;

var   XreadF_error : boolean  = false;
      XReadIsoDecode : boolean = false;
      ReadHeadEmpf : shortint = 0;

function  msgmarked:boolean;                 { Nachricht markiert? }
procedure MsgAddmark;
procedure MsgUnmark;

function  ubmarked(rec:longint):boolean;     { User/Brett markiert? }
procedure UBAddMark(rec:longint);
procedure UBUnmark(rec:longint);

procedure XreadF(ofs:longint; var f:file);
procedure XreadS(ofs:longint; s:TStream);
procedure Xread(const fn:string; append:boolean);
procedure XmemRead(ofs:xpWord; var size: Integer; var data);
procedure Xwrite(const fn:string);

procedure Cut_QPC_DES(var betr:string);
function  ReCount(var betr:string):integer;
procedure ReplyText(var betr:string; rehochn:boolean);

procedure BriefSchablone(pm:boolean;const schab,fn:string;const empf:string;
                         const realname:string); overload;
procedure BriefSchablone(Addr: TAddress; const ein, aus: string); overload;                         
procedure ReadHeader(var hd:theader; var hds:longint; hderr:boolean);  { Fehler-> hds=1 ! }
procedure QPC(decode:boolean; var data; size: Integer; passwd:pointer;
              var passpos:smallword);
procedure Iso1ToIBM(var data; size: Integer);
procedure IBMToIso1(var data; size: Integer);
function  TxtSeek(adr:pointer; Size: Integer; igcase,umlaut:boolean):boolean;

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

// function  MakeFidoAdr(const frec:fidoadr; usepoint:boolean):string;
function  IsNodeAddress(const adr:string):boolean;
procedure SetDefZoneNet;   { Fido-Defaultzone/Net setzen }

function  vert_name(s:string):string;
function  vert_long(const s:string):string;
function  systemname(adr:string):string;
function  pfadbox(zconnect:boolean; var pfad:String):string;
function  file_box(const dname:string):string;
function  box_file(const box:string):string;
function  brettok(trenn:boolean):boolean;
function Addr2DB(const addr: string): string;

function  extmimetyp(const typ:string):string;
function  compmimetyp(const typ:string):string;
procedure CloseAblage;

var
  TxtSeekKey: ShortString;

implementation  {-----------------------------------------------------}

{ Brettcodes:   '$'  ==  intern/lokal, z.B. Netzanruf
                '1'  ==  /PM-Bretter
                'A'  ==  Netzbretter
                'U'  ==  Userbretter (nur in der MBase) }

uses
  typeform,fileio,database,resource,xpconst,
  xp0,xp1,xp3o, xp3ex, xpnt, xpmakeheader, debug;

var
  GlobalAblageOpen : Boolean = false;
  GlobalPuffer: file;
  GlobalLastPuffer: Byte;

procedure CloseAblage;
begin
  if IOResult = 0 then;
  if GlobalAblageOpen then
  begin
    Close(GlobalPuffer);
    if IOResult = 0 then ;
    GlobalAblageOpen := false;
  end;
end;

procedure OpenAblage(Ablage: Byte);
var
  OldFileMode: Byte;
begin
  if (GlobalLastPuffer <> Ablage) or not GlobalAblageOpen then
  begin
    CloseAblage;
    OldFileMode := FileMode;
    FileMode := fmOpenRead + fmShareDenyWrite;
    Assign(GlobalPuffer,aFile(Ablage));
    Reset(GlobalPuffer, 1);
    GlobalLastPuffer := Ablage;
    FileMode := OldFileMode;
    GlobalAblageOpen := true;
    if IOResult = 0 then ;
  end;
end;

procedure QPC(decode:boolean; var data; size: Integer; passwd:pointer;
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

function TxtSeek(adr:pointer; size: Integer; igcase,umlaut:boolean):
         boolean; assembler;
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

@2:      mov al,[esi+ebx]               { im Text Nach Anfang des Rests suchen }
         cmp al,[edi+ebp+1]
         je @ulgood
         cmp al,' '                     { Abbruch bei Wortende }
         jb @nextb
         inc ebx
         jmp @2

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

procedure Iso1ToIBM(var data; size: Integer); assembler;
{$IFDEF ANALYSE}
begin
  //no asm
end;
{$ELSE}
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
{$ENDIF }

procedure IBMToIso1(var data; size: Integer); assembler;
{$IFDEF ANALYSE}
begin
  //no asm
end;
{$ELSE}
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
    ablg   : byte;
    flags  : byte;
    errstr : string[30];
    empfnr : smallword;
    nopuffer: boolean;
begin
  ok:=true;
  dbRead(mbase,'ablage',ablg);  { KEIN 'dbReadN' VERWENDEN !!! }
  hds:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');
  //if (hds<0) or (hds>iif(ntZconnect(ablg),1000000,8000)) then begin ???
  if (hds<0) or (hds>iif(ntZCablage(ablg),1000000,8000)) then begin
    ok:=false;
    errstr:=getres(300);  { '; falsche Grî·enangaben' }
    end
  else begin
    OpenAblage(Ablg);
    nopuffer:=(ioresult<>0);
    if nopuffer then begin
      ok:=false;
      errstr:=getres(301);  { '; Ablage fehlt' }
      end
    else begin
      seek(Globalpuffer,dbReadInt(mbase,'adresse'));
      if IOResult <> 0 { eof(Globalpuffer) }then begin
        ok:=false;
        errstr:=getres(302)  { '; Adre·index fehlerhaft' }
        end;
      end;
    if ReadHeadEmpf=0 then
      empfnr:=dbReadInt(mbase,'netztyp') shr 24
    else begin
      empfnr:=ReadHeadEmpf; ReadHeadEmpf:=0;
      end;
    if ok then makeheader(ntZCablage(ablg),Globalpuffer,empfnr,hds,hd,
                          ok,true, true);
    errstr:='';
    end;
  if not ok then begin
    hds:=1;
    if hderr then begin
      fehler(getres(303)+strs(ablg)+errstr+')');  { 'Nachricht ist beschÑdigt  (Ablage ' }
      aufbau:=true;
      end;
    flags:=2;
    dbWrite(mbase,'halteflags',flags);  { KEIN 'dbWriteN' VERWENDEN !!! }
    end;
  if LeftStr(hd.FirstEmpfaenger,TO_len)=TO_ID then   { /TO: }
    hd.Firstempfaenger:=Mid(hd.Firstempfaenger,9);
end;


function buferr(nr:byte):string;
begin
  buferr:=getreps(304,strs(nr))+#13#10;   { 'Ablage Nr. %s ist fehlerhaft!' }
end;

procedure XreadF(ofs:longint; var f:file);
var p        : pointer;
    bufs,rr  : Integer;
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
  OpenAblage(ablage);
  if IOResult <>0 then begin
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
  if adr+ofs-minus+dbReadInt(mbase,'groesse')>filesize(Globalpuffer) then begin
    berr:=buferr(ablage);
    blockwrite(f,berr[1],length(berr));
    end
  else begin
    seek(Globalpuffer,adr+ofs);
    dbReadN(mbase,mb_msgsize,size);
    dec(size,ofs);
    iso:=XReadIsoDecode and (dbReadInt(mbase,'typ')=ord('T')) and
         (dbReadInt(mbase,'netztyp') and $2000<>0);
    while size>0 do begin
      blockread(Globalpuffer,p^,min(bufs,size),rr);
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
      if (size>0) and eof(Globalpuffer) then
        size:=0;
      end;
    end;
ende:
  if ioresult = 0 then ;
  freemem(p,bufs);
  XReadIsoDecode:=false;
end;

procedure XreadS(ofs:longint; s:TStream);
var p        : pointer;
    bufs,rr  : Integer;
    ablage   : byte;
    size     : longint;
    adr      : longint;
    berr     : string[40];
    hdp      : theader;
    hds      : longint;
    minus    : longint;

begin
  bufs:=65536;
  p:=nil;
  dbReadN(mbase,mb_ablage,ablage);
 try
  OpenAblage(ablage);
  if ioresult<>0 then begin
    fehler(getreps(305,strs(ablage)));   { 'Ablage %s fehlt!' }
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
  if adr+ofs-minus+dbReadInt(mbase,'groesse')>filesize(Globalpuffer) then begin
    berr:=buferr(ablage);
    s.Write(berr[1],length(berr));
    end
  else begin
    seek(Globalpuffer,adr+ofs);
    dbReadN(mbase,mb_msgsize,size);
    dec(size,ofs);
    while size>0 do begin
      blockread(Globalpuffer,p^,min(bufs,size),rr);
      s.Write(PChar(p)^,rr);
      dec(size,rr);
      if (size>0) and eof(Globalpuffer) then
        size:=0;
      end;
    end;
 finally
  if ioresult = 0 then ;
  if assigned(p) then freemem(p,bufs);
 end;
end;


procedure Xread(const fn:string; append:boolean);
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

procedure XmemRead(ofs:xpWord; var size: Integer; var data);
var
  ablage   : byte;
begin
  if ofs<dbReadInt(mbase,'msgsize') then
  begin
    dbReadN(mbase,mb_ablage,ablage);
    OpenAblage(Ablage);
    seek(GlobalPuffer,dbReadInt(mbase,'adresse')+ofs);
    blockread(GlobalPuffer,data,size,size);
    if ioresult = 0 then ;
  end
  else
    fillchar(data,size,0);
end;

{procedure XmemRead(ofs:xpWord; var size: Integer; var data);
var
  ablage   : byte;
  f: Integer;
begin
  if ofs<dbReadIntN(mbase, mb_msgsize) then begin
    dbReadN(mbase,mb_ablage,ablage);
    f := FileOpen(aFile(ablage), fmOpenRead);
    FileSeek(f, dbReadInt(mbase,'adresse')+ofs, 0);
    size := FileRead(f, data, size);
    FileClose(f);
    end
  else
    fillchar(data,size,0);
end;          }


procedure Xwrite(const fn:string);
var f,puffer : file;
    ablage   : byte;
    oldsize  : longint;
    oldadr   : longint;
    adr,size : longint;
    p        : pointer;
    bs,rr    : Integer;
begin
  CloseAblage;
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


procedure seekmark(rec:longint; var found:boolean; var x:integer);
var
  l,r,m : integer;
begin
  found:=false;
  if not Marked.Sorted then
  begin
    l:=-1; r:= Marked.Count;
    while not found and (l+1<r) do begin
      m:=(l+r) div 2;
      if marked[m].recno=rec then begin
        found:=true; l:=m; end
      else
        if marked[m].recno<rec then r:=m
        else l:=m;
      end;
    x:=l;
    end
  else begin
    x:=0;
    while (x< Marked.Count) and not found do
      if marked[x].recno=rec then found:=true
      else inc(x);
    end;
end;


function msgmarked:boolean;
var found : boolean;
    x     : integer;
begin
  if Marked.Count =0 then
    msgmarked:=false
  else
  begin
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
  if not found then
  begin
    if Marked.Sorted then
      Marked.AddWithOptions(dbRecno(mbase), dat, intnr)
    else
      Marked.InsertWithOptions(x, dbRecno(mbase), dat, intnr)
   end;
end;


procedure MsgUnmark;
var found : boolean;
    x     : integer;
begin
  seekmark(dbRecno(mbase),found,x);
  if found then
    Marked.Delete(x);
end;

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


{ Zaehlt die "Re:"s am Anfang von betr;
  loescht fuehrende "Re:"s und angehaengte alte Betreffs. }

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


procedure BriefSchablone(pm:boolean;const schab,fn:string;const empf:string;
                         const realname:string);
var t1,t2 : text;
    s     : string;
begin
  if FileExists(OwnPath + schab) then begin
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

procedure BriefSchablone(Addr: TAddress; const ein, aus: string);
begin
  if addr is TDomainEmailAddress then
    BriefSchablone(true,ein,aus,addr.ZCAddress,
      TDomainEmailAddress(addr).RealName)
  else
    BriefSchablone(addr is TEmailAddress,ein,aus,
      TEmailAddress(addr).ZCAddress,'');
end;

function isbox(const box:string):boolean;
begin
  dbSeek(Boxbase,boiName,UpperCase(box));
  isbox:=dbFound;
end;


{ Killed-Flag fÅr Ablage der aktuellen Nachricht setzen   }
{ diese Ablage wird bei der nÑchsten Reorg auf jeden Fall }
{ reorganisiert                                           }

procedure wrkilled;
type ba = array[0..ablagen-1] of boolean;
var  f  : file;
     rr : Integer;
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
    if IsMailAddr(s) then
      truncstr(s,cpos('@',s)-1);
    vert_name:=mid(s,2);
    end;
end;

function vert_long(const s:string):string;
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


function file_box(const dname:string):string;
begin
  dbSeek(Boxbase,boiDatei,UpperCase(dname));
  if dbFound then
    file_box:=dbReadStr(Boxbase, 'boxname')
  else begin
    Debug.DebugLog('xp3','file_box: assigned server name not found (' + dname + ')!', DLWarning);
    file_box:=dname;
  end;
end;


function box_file(const box:string):string;
begin
  dbSeek(boxbase,boiName,UpperCase(box));
  if dbFound then
    box_file:=dbReadStr(boxbase,'dateiname')
  else begin
    Debug.DebugLog('xp3','box_file: assigned file name not found (' + box + ')!', DLWarning);
    box_file:=box;
    end;
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


function IsNodeAddress(const adr:string):boolean;
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
var dd : String;
    pp : Integer;
begin
  FTNParse(DefFidoBox,dd,DefaultZone,DefaultNet,DefaultNode,pp,2);
(*
  Splitfido(DefFidoBox,fa,2);
  DefaultZone:=fa.zone;
  DefaultNet:=fa.net;
  DefaultNode:=fa.node;
*)
end;


procedure ReplaceVertreterbox(var box:string; pm:boolean);
var
  wbox : string;
begin
  dbSeek(Boxbase,boiName,UpperCase(box));
  if dbFound then begin              { Test auf Vertreterbox }
    if pm then
      wbox:= dbReadStr(Boxbase,'PVertreter')
    else
      wbox:= dbReadStr(Boxbase,'AVertreter');
    if IsBox(wbox) then box:=wbox;
  end;
end;


procedure ClearPGPflags(hdp:theader);
begin
  hdp.pgpflags:=hdp.pgpflags and (not fPGP_haskey);
end;

function Addr2DB(const addr: string): string;
var i: integer;
    q: boolean;
begin
  result := (addr);
  q := FirstChar(result)='"';

  for i := 2 to Length(result) do 
    if (result[i-1]<>'\')and(result[i]='"') then
      q := not q else
    if (not q)and(result[i-1]=' ')and(result[i]='(') then begin
      SetLength(result,i-2);
      break;
    end;
end;

function extmimetyp(const typ:string):string;
begin
  if firstchar(typ)='/' then
    extmimetyp:='application'+typ
  else
    extmimetyp:=typ;
end;


function compmimetyp(const typ:string):string;
begin
  if LeftStr(typ,12)='application/' then
    compmimetyp:=LowerCase(mid(typ,12))
  else
    compmimetyp:=LowerCase(typ);
end;

{
  $Log: xp3.pas,v $
  Revision 1.95  2004/01/17 16:33:43  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

  Revision 1.94  2003/10/18 17:14:44  mk
  - persistent open database boxenfile (DB: boxbase)

  Revision 1.93  2003/05/11 11:12:16  mk
  - use IsMailAddr when possible

  Revision 1.92  2003/01/28 10:42:25  cl
  - Added statistical SPAM filter

  Revision 1.91  2003/01/13 22:05:19  cl
  - send window rewrite - Fido adaptions
  - new address handling - Fido adaptions and cleanups

  Revision 1.90  2002/12/21 05:37:55  dodi
  - removed questionable references to Word type

  Revision 1.89  2002/12/14 07:31:31  dodi
  - using new types

  Revision 1.88  2002/12/12 11:58:44  dodi
  - set $WRITEABLECONT OFF

  Revision 1.87  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.86  2002/09/09 09:06:35  mk
  - added const parameters

  Revision 1.85  2002/09/09 08:42:33  mk
  - misc performance improvements

  Revision 1.84  2002/07/26 08:19:23  mk
  - MarkedList is now a dynamically created list, instead of a fixed array,
    removes limit of 5000 selected messages

  Revision 1.83  2002/07/25 20:43:54  ma
  - updated copyright notices

  Revision 1.82  2002/06/23 15:03:06  cl
  - Adapted Nachricht/Direkt to new address handling.

  Revision 1.81  2002/04/14 22:22:13  cl
  - added Addr2DB (converts ZConnect Address to DB address)

  Revision 1.80  2002/04/13 16:29:13  ma
  - clarified comments

  Revision 1.79  2002/03/24 14:18:20  mk
  - removed lonly label "ende"

  Revision 1.78  2002/03/09 15:31:06  ma
  MY+HG+JG:- Bugfix: OpenXP kann jetzt wieder eine Nachrichten-
         Datenbank von XP v3.12 korrekt und ohne Stillstand des
         Rechners konvertieren. Bug existent seit dem 12.08.2001.

  Revision 1.77  2002/01/13 15:15:50  mk
  - new "empfaenger"-handling

  Revision 1.76  2002/01/13 15:07:27  mk
  - Big 3.40 Update Part I

  Revision 1.75  2002/01/03 19:15:00  cl
  - added XreadS (similar to XReadF, but reads message from database into TStream)

  Revision 1.74  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.73  2001/12/22 22:15:05  mk
  - search templates always in our main path

  Revision 1.72  2001/11/18 12:31:22  mk
  - fixed some file case problems with fido file lists

  Revision 1.71  2001/10/23 22:59:17  ma
  - fixed: file_box failed with unix file systems
    (tried to search for lowercase but db index is uppercase)

  Revision 1.70  2001/10/20 17:26:40  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.69  2001/10/14 20:42:37  ma
  - added debug info (for tracing 8charN/U/Z Linux bug :-)

  Revision 1.68  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.67  2001/09/08 16:29:32  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.66  2001/09/07 13:54:19  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase
}

end.


{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Verarbeitung von Pointdaten }

{$I XPDEFINE.INC}
{$O+,F+}

unit  xp3;

interface

uses
  xpglobal, crt, dos,typeform,fileio,inout,datadef,database,montage,
  resource,xp0,xp1,xp1input,xp_des,xp_pgp,xpdatum;

const XreadF_error : boolean  = false;
      XReadIsoDecode : boolean = false;
      ReadHeadEmpf : shortint = 0;
      ReadHeadDisk : shortint = 0;           { Diskussion-In }
      reflist      : refnodep = nil;         { Reference-Liste, rÅckwÑrts! }
      empflist     : empfnodep= nil;         { EmpfÑngerliste }
      ReadEmpflist : boolean  = false;       { EMP-Liste auslesen }
      ReadKopList  : boolean  = false;       { KOP-Liste auslesen }
      ReadOEMList  : boolean  = false;       { OEM-Liste auslesen }
      NoPM2AMconv  : boolean  = false;


function  msgmarked:boolean;                 { Nachricht markiert? }
procedure MsgAddmark;
procedure MsgUnmark;
procedure SortMark;
procedure UnsortMark;

function  ubmarked(rec:longint):boolean;     { User/Brett markiert? }
procedure UBAddMark(rec:longint);
procedure UBUnmark(rec:longint);

procedure XreadF(ofs:longint; var f:file);
procedure Xread(fn:pathstr; append:boolean);
procedure XmemRead(ofs:word; var size:word; var data);
procedure Xwrite(fn:pathstr);

procedure Cut_QPC_DES(var betr:string);
function  ReCount(var betr:string):integer;
procedure ReplyText(var betr:string; rehochn:boolean);
procedure DisposeReflist(var list:refnodep);
procedure AddToReflist(ref:string);
procedure AddToEmpflist(empf:string);
procedure DisposeEmpflist(var list:empfnodep);

procedure BriefSchablone(pm:boolean; schab,fn:pathstr; empf:string;
                         var realname:string);
procedure makeheader(ZConnect:boolean; var f:file; empfnr,disknr:smallword;
                     var size:longint; var hd:header; var ok:boolean;
                     PM2AMconv:boolean);
procedure ReadHeader(var hd:header; var hds:longint; hderr:boolean);  { Fehler-> hds=1 ! }
{ procedure Rot13(var data; size:word); }                             {jetzt in Typeform.pas }
procedure QPC(decode:boolean; var data; size:word; passwd:pointer;
              var passpos:smallword);
procedure Iso1ToIBM(var data; size:word);
procedure IBMToIso1(var data; size:word);
function  TxtSeek(adr:pointer; size:word; var key:string; igcase,umlaut:boolean):boolean;

function  newdate:longint;    { Datum des letzten Puffer-Einlesens }

procedure makeuser(absender,pollbox:string);
function  EQ_betreff(var betr:string):boolean;
function  grQuoteMsk:pathstr;
function  isbox(const box:string):boolean;
procedure ReplaceVertreterbox(var box:string; pm:boolean);

procedure wrkilled;
procedure brettslash(var s:string);
procedure getablsizes;
function  QuoteSchab(pm:boolean):string;
procedure ClearPGPflags(hdp:headerp);

procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);
function  MakeFidoAdr(var frec:fidoadr; usepoint:boolean):string;
function  IsNodeAddress(adr:string):boolean;
procedure SetDefZoneNet;   { Fido-Defaultzone/Net setzen }

function  vert_name(s:string):string;
function  vert_long(s:string):string;
function  systemname(adr:string):string;
function  pfadbox(zconnect:boolean; var pfad:Hugestring):string;
function  file_box(d:DB; dname:string):string;
function  box_file(box:string):string;
function  brettok(trenn:boolean):boolean;

function  extmimetyp(typ:string):string;
function  compmimetyp(typ:string):string;
procedure CloseAblage;


implementation  {-----------------------------------------------------}

{ Brettcodes:   '$'  ==  intern/lokal, z.B. Netzanruf
                '1'  ==  /PM-Bretter
                'A'  ==  Netzbretter
                'U'  ==  Userbretter (nur in der MBase) }

uses  xp3o,xp3ex,xpnt,xpovl;

const
  GlobalAblageOpen: Boolean = false;
var
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
    FileMode := 0 + fmDenyNone;
    Assign(GlobalPuffer,aFile(Ablage));
    Reset(GlobalPuffer, 1);
    GlobalLastPuffer := Ablage;
    FileMode := OldFileMode;
    GlobalAblageOpen := true;
    if IOResult = 0 then ;
  end;
end;

procedure QPC(decode:boolean; var data; size:word; passwd:pointer;
              var passpos:smallword); assembler;

{ decode:  TRUE -> dekodieren, FALSE -> codierem                  }
{ data:    Zeiger auf Datenblock                                  }
{ size:    Anzahl zu codierender Bytes                            }
{ passwd:  Zeiger auf Pa·wort (Pascal-String, max. 255 Zeichen)   }
{ passpos: aktueller Index im Pa·wort; Startwert 1                }

asm
         push ds
         les   di,passpos
         mov   bx,es:[di]
         les   di,data
         mov   dx,size
         lds   si,passwd
         mov   ch,[si]                 { Pa·wort-LÑnge }
         mov   cl,4                    { zum Nibble-Tauschen }
         mov   ah,decode
         cld

@QPClp:  mov   al,es:[di]              { Original-Byte holen }
         or    ah,ah                   { decodieren ? }
         jnz   @code1
         rol   al,cl                   { Nibbles vertauschen }
@code1:  xor   al,[si+bx]              { Byte codieren }
         inc   bl
         cmp   bl,ch                   { am PW-Ende angekommen? }
         jbe   @pwok
         mov   bl,1                    { PW-Index auf 1 zurÅcksetzen }
@pwok:   or    ah,ah                   { codieren? }
         jz    @code2
         rol   al,cl                   { Nibbles vertauschen }
@code2:  stosb
         dec   dx                      { nÑchstes Byte }
         jnz   @QPClp

         les   di,passpos              { neuen PW-Index speichern }
         mov   es:[di],bx
         pop ds
end;




function TxtSeek(adr:pointer; size:word; var key:string;igcase,umlaut:boolean):
         boolean; assembler;

{ Bei "Umlautsensitiver" Suche (ae=Ñ) muss der Key-String im AE/OE... Format und Upcase sein }

asm
         push ds
         push bp
         cld
         lds   si,adr
         mov   cx,size
         cmp   cx, 0
         jz    @nfound
         mov   dh,umlaut
         cmp   dh,0                   { Bei Umlautsensitiver Suche zwingend ignore Case. }
         jne   @icase
         cmp   igcase,0               { ignore case? }
         jz    @case

@icase:  push  cx
         push  si

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
@xl:     mov   [si-1],al
@noc:    loop  @cloop
         pop   si
         pop   cx


@case:   les   di,key
         sub   cl,es:[di]
         sbb   ch,0
         jc    @nfound                 { key >= LÑnge }
         inc   cx

@sblp1:  xor   bx,bx                   { Suchpuffer- u. String-Offset }
         xor   bp,bp
         mov   dl,es:[di]              { Key-LÑnge }
@sblp2:  mov   al,[si+bx]
@acctst: cmp   al,es:[di+bp+1]
         jnz   @testul
@ulgood: inc   bx                      { Hier gehts weiter nach Erfolgreichem Umlautvergleich }
         inc   bp
         dec   dl
         jz    @found
         jmp   @sblp2

                                        {--------------}
@testul:                                { WILDCARDS }
         mov ah,es:[di+bp+1]
         cmp ah,'?'                     { ? = Ein Zeichen beliebig }
         je @ulgood                     {--------------------------}


         cmp ah,'*'                     { * = mehrere Zeichen beliebig }
         jne @ultst                     { ---------------------------- }
                                           
@1:      inc bp
         dec dl                         { Naechstes Suchkey-Zeichen laden }
         jz @found                      { Kommt keines mehr, Suche erfolgreich }  

@2:      mov al,[si+bx]                 { im Text Nach Anfang des rests suchen }
         cmp al,es:[di+bp+1]
         je @3 
         cmp al,' '                     { Abbruch bei Wortende }        
         jbe @nextb
         inc bx
         jmp @2
        
@3:      inc bx                         {Weitervergleichen bis naechster * oder Suchkeyende }
         inc bp  
         dec dl
         jz @found                      { Bei Suchkeyende ist Suche erfolgreich }
         mov al,es:[di+bp+1]                 
         cmp al,[si+bx]                 { weiter im Text solange er passt }
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
@ultest: cmp ax,es:[di+bp+1]            { Dann auf "AE" Testen. }
         jne @nextb
         inc bp                         { Wenn gefunden: Zeiger im Suchbegriff }
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


@nextb:  inc   si                       { Weitersuchen... }
         dec cx
         jne @sblp1                     { 286er Loop geht nur +/- 127 Byte...}
@nfound: xor   ax,ax
         jmp   @ende
@found:  mov   ax,1
@ende:   pop bp
         pop ds
end;



procedure Iso1ToIBM(var data; size:word); assembler;
asm
          mov    cx,size
          jcxz   @noconv1
          les    di,data
          mov    bx,offset ISO2IBMtab - 128
          cld
@isolp1:  mov    al,es:[di]
          or     al,al
          jns    @ii1
          xlat
@ii1:     stosb
          loop   @isolp1
@noconv1:
end;



procedure IBMToIso1(var data; size:word); assembler;
asm
          mov    cx,size
          jcxz   @noconv2
          les    di,data
          mov    bx,offset IBM2ISOtab
          cld
@isolp2:  mov    al,es:[di]
          xlat
          stosb
          loop   @isolp2
@noconv2:
end;


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
    newdate:=ixdat(left(trim(s),10));
    end;
end;


procedure DisposeReflist(var list:refnodep);
var p : refnodep;
begin
  while list<>nil do begin
    p:=list^.next;
    dispose(list);
    list:=p;
    end;
end;

procedure AddToReflist(ref:string);
var p : refnodep;
begin
  if ref<>'' then begin
    new(p);
    p^.next:=reflist;
    p^.ref:=ref;
    reflist:=p;
    end;
end;

procedure AddToEmpflist(empf:string);
var p : empfnodep;
begin
  p:=@empflist;
  while p^.next<>nil do p:=p^.next;
  new(p^.next);
  p^.next^.next:=nil;
  p^.next^.empf:=empf;
end;

procedure DisposeEmpflist(var list:empfnodep);
var p : empfnodep;
begin
  while list<>nil do begin
    p:=list^.next;
    dispose(list);
    list:=p;
    end;
end;


{$define allrefs}
{$define convbrettempf}
{$define pgp}
{$I xpmakehd.inc}           { MakeHeader() }

{ in dieser Prozedur kein ReadN/WriteN verwenden, wegen }
{ XP2.NewFieldMessageID !                               }

procedure ReadHeader(var hd:header; var hds:longint; hderr:boolean);
var ok     : boolean;
    ablg   : byte;
    flags  : byte;
    errstr : string[30];
    empfnr : smallword;
    nopuffer: boolean;
begin
  ok:=true;
  dbReadN(mbase,mb_ablage,ablg);
  hds:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');
  if (hds<0) or (hds>iif(ntZconnect(ablg),1000000,8000)) then
  begin
    ok:=false;
    errstr:=getres(300);  { '; falsche Grî·enangaben' }
  end
  else
  begin
    OpenAblage(Ablg);
    nopuffer:=(ioresult<>0);
    if nopuffer then begin
      ok:=false;
      errstr:=getres(301);  { '; Ablage fehlt' }
    end
    else
    begin
      seek(GlobalPuffer,dbReadInt(mbase,'adresse'));
      if IOResult <> 0 {eof(puffer) }then
      begin
        ok:=false;
        errstr:=getres(302)  { '; Adre·index fehlerhaft' }
      end;
    end;
    if ReadHeadEmpf=0 then
      empfnr:=dbReadInt(mbase,'netztyp') shr 24
    else begin
      empfnr:=ReadHeadEmpf; ReadHeadEmpf:=0;
      end;
    if ok then makeheader(ntZCablage(ablg),GlobalPuffer,empfnr,ReadHeadDisk,hds,hd,
                          ok,not NoPM2AMconv);
    errstr:='';
  end;
  if not ok then
  begin
    hds:=1;
    if hderr then begin
      fehler(getres(303)+strs(ablg)+errstr+')');  { 'Nachricht ist beschÑdigt  (Ablage ' }
      aufbau:=true;
      end;
    flags:=2;
    dbWriteN(mbase,mb_halteflags,flags);
  end;
  if left(hd.empfaenger,TO_len)=TO_ID then   { /TO: }
    hd.empfaenger:=copy(hd.empfaenger,9,255);
  ReadEmpflist:=false; ReadHeadDisk:=0;
  ReadKopList:=false;
  ReadOEMList:=false;
  NoPM2AMconv:=false;
end;


function buferr(nr:byte):string;
begin
  buferr:=getreps(304,strs(nr))+#13#10;   { 'Ablage Nr. %s ist fehlerhaft!' }
end;


procedure XreadF(ofs:longint; var f:file);
var p        : pointer;
    bufs,rr  : word;
    ablage   : byte;
    size     : longint;
    adr      : longint;
    berr     : string[40];
    iso      : boolean;
    hdp      : headerp;
    hds      : longint;
    minus    : longint;

label ende;
begin
  dbReadN(mbase,mb_ablage,ablage);
  OpenAblage(ablage);
  if ioresult<>0 then begin
    fehler(getreps(305,strs(ablage)));   { 'Ablage %s fehlt!' }
    XReadIsoDecode:=false;
    exit;
    end;
  Bufs := GetMaxMem(p, 2048, 16384);
  dbReadN(mbase,mb_adresse,adr);
  minus:=0;
  if dbReadInt(mbase,'netztyp') and $8000<>0 then begin  { KOM vorhanden }
    new(hdp);
    ReadHeader(hdp^,hds,false);
    if (hdp^.komlen>0) and (ofs=hds+hdp^.komlen) then
      minus:=hdp^.komlen;
    dispose(hdp);
    end;
  if adr+ofs-minus+dbReadInt(mbase,'groesse')>filesize(GlobalPuffer) then
  begin
    berr:=buferr(ablage);
    blockwrite(f,berr[1],length(berr));
    end
  else begin
    seek(GlobalPuffer,adr+ofs);
    dbReadN(mbase,mb_msgsize,size);
    dec(size,ofs);
    iso:=XReadIsoDecode and (dbReadInt(mbase,'typ')=ord('T')) and
         (dbReadInt(mbase,'netztyp') and $2000<>0);
    while size>0 do begin
      blockread(GlobalPuffer,p^,min(bufs,size),rr);
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
      if (size>0) and eof(GlobalPuffer) then
        size:=0;
      end;
    end;
ende:
  if ioresult = 0 then ;
  freemem(p,bufs);
  XReadIsoDecode:=false;
end;


procedure Xread(fn:pathstr; append:boolean);
var f : file;
begin
  assign(f,fn);
  if exist(fn) and append then begin
    reset(f,1);
    seek(f,filesize(f));
    end
  else
    rewrite(f,1);
  XreadF(0,f);
  close(f);
end;


procedure XmemRead(ofs:word; var size:word; var data);
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


procedure Xwrite(fn:pathstr);
var f,puffer : file;
    ablage   : byte;
    oldsize  : longint;
    oldadr   : longint;
    adr,size : longint;
    p        : pointer;
    bs,rr    : word;
begin
  CloseAblage;
  bs := GetMaxMem(p, 1024, 32768);
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
          { ACHTUNG: Hier kein FastMove wegen Åberlappenden Speicherbereichen! }
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
     { ACHTUNG: Hier kein FastMove wegen Åberlappenden Speicherbereichen! }
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
     { ACHTUNG: Hier kein FastMove wegen Åberlappenden Speicherbereichen! }
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
      { ACHTUNG: Hier kein FastMove wegen Åberlappenden Speicherbereichen! }
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
  p:=pos('(war:',lstr(betr)); if p>0 then betr:=left(betr,p-1);
  p:=pos('(was:',lstr(betr)); if p>0 then betr:=left(betr,p-1);
  repeat
    xch:=false;
    betr:=trim(betr);
    for i:=1 to ReAnz do
      if ustr(left(betr,length(retyp[i])))=retyp[i] then begin
        inc(cnt); betr:=trim(copy(betr,length(retyp[i])+1,BetreffLen));
        xch:=true;
        end;
    if ustr(left(betr,3))='RE^' then begin
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
  if (cnt=0) or (cnt>99) or not rehochn then betr:=left('Re: '+betr,betrefflen)
  else betr:=left('Re^'+strs(cnt+1)+': '+betr,betrefflen);
end;


procedure makeuser(absender,pollbox:string);
var b : byte;
begin
  dbAppend(ubase);
  dbWriteN(ubase,ub_username,absender);
  dbWriteN(ubase,ub_pollbox,pollbox);
  dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
  dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
  b:=1 + iif(newuseribm,0,8);
  dbWriteN(ubase,ub_userflags,b);  { aufnehmen }
end;


{ EQ-betreff = left(betreff)='*crypted*' and hexval(right(betreff))=orgGroesse
{        oder  (betr = dbread(mbase,betreff)) oder                         }

function EQ_betreff(var betr:string):boolean;
var pmcrypted : boolean;
    betreff   : string[betrefflen];
    b2        : string[40];
begin
  dbReadN(mbase,mb_betreff,betreff);
  betreff:=rtrim(betreff);
  b2:=rtrim(left(betr,40));
  pmcrypted:=(left(betr,length(PMC_ID))=PMC_ID);
  EQ_betreff:=(pmcrypted and (hexval(right(betr,6))=dbReadInt(mbase,'groesse'))) or
              (b2=betreff) or (b2=left(QPC_ID+betreff,40)) or
              (b2=left(DES_ID+betreff,40));
end;


{ bestimmt den Namen der passenden Quote-Schablone zu aktuellen        }
{ Nachricht. Entweder QuoteMsk, oder eine gruppenspezifische Schablone }

function grQuoteMsk:pathstr;
var d     : DB;
    grnr  : longint;
    brett : string[5];
    n     : integer;
    qm    : string[8];
begin
  grQuoteMsk:=QuoteMsk;
  dbReadN(mbase,mb_brett,brett);
  dbSeek(bbase,biIntnr,copy(brett,2,4));
  if dbFound then begin
    dbReadN(bbase,bb_gruppe,grnr);
    dbOpen(d,'gruppen',1);
    dbSeek(d,giIntnr,dbLongStr(grnr));
    if dbFound then begin
      n:=dbGetFeldNr(d,'quotemsk');
      if n>0 then begin
        dbReadN(d,n,qm);
        if trim(qm)<>'' then grQuoteMsk:=trim(qm)+'.xps';
        end;
      end;
    dbClose(d);
    end;
end;


procedure BriefSchablone(pm:boolean; schab,fn:pathstr; empf:string;
                         var realname:string);
var t1,t2 : text;
    s     : string;
begin
  if exist(schab) then begin
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
  dbSeek(d,boiName,ustr(box));
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
  if not dbEOF(mbase) and not dbBOF(mbase) then
  begin
    CloseAblage;
    dbReadN(mbase,mb_ablage,abl);
    if abl<ablagen then begin
      fillchar(b,sizeof(b),false);
      assign(f,killedDat);
      if existf(f) then
      begin
        reset(f,1); blockread(f,b,ablagen,rr); seek(f,0);
      end else
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
  if left(s,1)<>'/' then s:='/'+s;
end;


procedure getablsizes;
var i : byte;
begin
  CloseAblage;
  for i:=0 to ablagen-1 do
    ablsize[i]:=_filesize(AblagenFile+strs(i));
end;


function QuoteSchab(pm:boolean):string;
begin
  if pm then
    if left(dbReadStrN(mbase,mb_brett),1)='A' then
      QuoteSchab:=QuotePriv
    else
      QuoteSchab:=QuotePMpriv
  else
    QuoteSchab:=QuoteMsk;
end;


function vert_name(s:string):string;
begin
  if left(s,1)<>vert_char then
    vert_name:=s
  else begin
    if cpos('@',s)>0 then truncstr(s,cpos('@',s)-1);
    vert_name:=mid(s,2);
    end;
end;

function vert_long(s:string):string;
begin
  if (left(s,1)='[') and (right(s,1)=']') then
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
    else systemname:=left(adr,p-1);
    end;
end;


function pfadbox(zconnect:boolean; var pfad:Hugestring):string;
var p : byte;
begin
  if zconnect then begin
    p:=1;
    while (p<=length(pfad)) and (pfad[p]<>'!') and (pfad[p]<>'.') and
          (pfad[p]<>';') and   { ";" wg. ProNet }
          (pfad[p]<>'@') do    { "@" wg. FidoNet-Domains }
      inc(p);
    pfadbox:=trim(left(pfad,p-1));
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
  dbSeek(d,boiName,ustr(box));
  if dbFound then box_file:=dbReadStr(d,'dateiname')
  else box_file:=box;
  dbClose(d);
end;


{ trifft aktuelles Brett auf den Lesemode zu? }

function brettok(trenn:boolean):boolean;   { s. auch XP4D.INC.Write_Disp_Line }
begin
  if dbEOF(bbase) or dbBOF(bbase) then
    brettok:=false
  else if trennall and trenn and (left(dbReadStrN(bbase,bb_brettname),3)='$/T') then
    brettok:=true
  else
    case readmode of
      0 : brettok:=true;
      1 : brettok:=(dbReadInt(bbase,'flags') and 2<>0);
    else
      brettok:=(not smdl(dbReadInt(bbase,'LDatum'),readdate));
    end;
end;


procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);
var p1,p2,p3 : byte;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos('@',adr);
    if p1>0 then begin
      username:=trim(left(adr,p1-1));
      delete(adr,1,p1);
      end;
    adr:=trim(adr);
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if p3=0 then p3:=cpos(',',adr);
    if p1+p2=0 then begin
      zone:=DefaultZone;
      net:=DefaultNet;
      if p3>0 then begin
        if p3>1 then
          node:=ival(left(adr,p3-1))
        else
          node:=DefaultNode;
        point:=minmax(ival(mid(adr,p3+1)),0,65535);
        ispoint:=(point>0);
        end
      else
        node:=minmax(ival(adr),0,65535);
      end
    else
      if (p2<>0) and (p1<p2) and ((p3=0) or (p3>p2)) then begin
        if p1=0 then
          zone:=DefaultZone
        else
          zone:=minmax(ival(left(adr,p1-1)),0,65535);
        net:=minmax(ival(copy(adr,p1+1,p2-p1-1)),0,65535);
        ispoint:=(p3>0);
        if ispoint then begin
          point:=minmax(ival(mid(adr,p3+1)),0,65535);
          if point=0 then ispoint:=false;
          end
        else
          p3:=length(adr)+1;
        node:=minmax(ival(copy(adr,p2+1,p3-p2-1)),0,65535);
        end;
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
  if p=0 then p:=cpos('/',adr);
  if p=0 then p:=cpos('.',adr);
  IsNodeAddress := ((p>0) and (ival(left(adr,p-1))>0)) or
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
    wbox : string[BoxNameLen];
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  if dbFound then begin              { Test auf Vertreterbox }
    if pm then
      dbRead(d,'PVertreter',wbox)
    else
      dbRead(d,'AVertreter',wbox);
    if IsBox(wbox) then box:=wbox;
    end;
  dbClose(d);
end;


procedure ClearPGPflags(hdp:headerp);
begin
  hdp^.pgpflags:=hdp^.pgpflags and (not fPGP_haskey);
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
  if left(typ,12)='application/' then
    compmimetyp:=lstr(mid(typ,12))
  else
    compmimetyp:=lstr(typ);
end;


end.
{
  $Log$
  Revision 1.25.2.13  2001/08/12 11:20:29  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.25.2.12  2001/08/11 22:17:57  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.25.2.11  2001/08/05 11:45:34  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.25.2.10  2001/04/28 15:47:33  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.25.2.9  2001/01/16 15:22:14  mk
  - kleiner Bug in txtseek() beseitigt

  Revision 1.25.2.8  2001/01/10 17:39:04  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.25.2.7  2001/01/04 16:53:07  mk
  - const-Parameter in isbox() verwenden

  Revision 1.25.2.6  2000/11/30 14:15:25  mk
  - fixed NewUserIBM when adding new uesers

  Revision 1.25.2.5  2000/11/30 14:10:35  mk
  - fixed NewUserIBM when adding new uesers

  Revision 1.25.2.4  2000/08/20 10:42:51  mk
  - kleine Performanceverbesserungen

  Revision 1.25.2.3  2000/07/26 09:32:35  mk
  - Fehler beim Anzeigen von Nachrichten mit KOM-Header beseitigt

  Revision 1.25.2.2  2000/07/02 13:44:14  mk
  JG: - Volltextsuche mit Wildcards implementiert

  Revision 1.25.2.1  2000/06/24 14:16:32  mk
  - 32 Bit Teile entfernt, Fixes

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

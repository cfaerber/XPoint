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

{ Nodelist }

{$I xpdefine.inc}

unit xpfido;

interface

uses  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$IFDEF Kylix}
  libc,
{$ENDIF}
{$ENDIF }
  sysutils,typeform,fileio,inout,keys,winxp,maus2,
  maske,lister, archive,stack,montage,resource,datadef,database,
  xp0,xp1,xp1o,xp1input,fidoglob;

const nfComp   = $0001;
      nfHST    = $0002;
      nfV32    = $0004;
      nfV32b   = $0008;
      nfPEP    = $0010;
      nfZYXEL  = $0020;
      nfHST16  = $0040;
      nfCM     = $0080;
      nfISDN   = $0100;
      nfTerbo  = $0200;
      nfVFC    = $0400;
      nfV34    = $0800;

      rfWaZOO  = $0001;
      rfUpWaz  = $0002;
      rfBark   = $0004;
      rfUpBark = $0008;

      NodeChar = '0123456789:/.';
      crashID  = '�crash�';

type  nodeinfo = record
                   found    : boolean;
                   ispoint  : boolean;
                   status   : string;
                   boxname  : string;
                   standort : string; { 03.02.2000 MH: 40 -> 65 }{ unbedenklich }
                   sysop    : string;
                   telefon  : string;
                   baud     : word;
                   fflags   : string; { MH: 40 -> 80 } { unbedenklich }
                   flags    : word;
                   request  : word;
                   datei    : byte;     { Nummer der Nodeliste }
                 end;

procedure MakeNodelistIndex;
procedure OpenNodeindex(const fn:string);
procedure CloseNodeindex;
procedure GetNodeinfo(const adr:string; var ni:nodeinfo; pointtyp:integer);
function  IsFidoNode(const adr:string):boolean;
function  FidoIsISDN(const fa:FidoAdr):boolean;
{ returns node name if node supports BinkP; node name may not be a valid IP }
{ address, there seems to be no standard :-( }
function  FidoIsBinkP(var fa:FidoAdr):string;
procedure KeepNodeindexOpen;
procedure KeepNodeindexClosed;
procedure GetNodeuserInfo(var fa:FidoAdr; var ni:NodeInfo);

function  FidoRequest(node,files:string):string;
{ procedure FidoTransfer; }
function  FidoSeekfile:string;
procedure ReadFidolist;
procedure DelFidolist;

function  TestNodelist:boolean;
function  testDefbox:boolean;

function  FidoFilename(const fa:FidoAdr):string;
function  CrashFile(adr:string):string;
procedure GetReqFiles(adr:string; var files:string);
function  FidoPhone(var fa:FidoAdr; var nl_phone:string):string;
function  FidoAppendRequestfile(var fa:FidoAdr):string;
procedure ShrinkPointToNode(var fa:FidoAdr; var ni:NodeInfo);
function  FindFidoAddress(const fn:string; var fa:FidoAdr):boolean;

procedure NodelistBrowser;

procedure SetCrash(adr:string; insert:boolean);
procedure SetRequest(const adr,files:string);  { '' -> Request loeschen }

procedure NodelistIndex;
procedure NodelistSeek;
procedure SetShrinkNodelist;
procedure ShrinkNodelist(indizieren:boolean);

function  ReqTestNode(var s:string):boolean;
procedure FileSelProc(var cr:customrec);
function  fstestmark(const s:string; block:boolean):boolean;
procedure NodeSelProc(var cr:customrec);


implementation

uses  xpnt,xp2,xp3,xp4e,
{$IFDEF Kylix}
  xplinux,
{$ELSE}
{$IFDEF Linux}
  linux, // for stat & fsstat
{$ENDIF}
{$ENDIF}
xpfidonl;


{ --- Nodelisten ----------------------------------------------------- }

const bersize   = 200;     { Max. Netze pro Bereich }
      maxber    = 300;
      maxnodes  = 3000;    { max Nodes / Net }
      maxpoints = 700;     { max Points / Node }
      nodekenn  = 'IDX'^Z;
      MaxNamelen= 30;      { max. Namenslaenge in Userindex }
      blocksize = 1024;    { Blockgroesse in Userindex }

type  noderec = packed record
                  node : smallword;
                  adr  : longint;
                end;
      nodea   = array[0..maxnodes-1] of noderec;
      pointrec= packed record
                  point : smallword;
                  adr   : longint;
                end;
      pointa  = array[0..maxpoints-1] of pointrec;
      berrec  = packed record             { Netzindex - Bereich }
                  fromnet  : smallword;
                  fromzone : smallword;
                  anz      : smallword;
                  adr      : longint;
                end;
      netrec  = packed record case integer of
                  0 : (net  : smallword;
                       zone : smallword;
                       anz  : smallword;
                       fnr  : byte;   { Datei-Nr. }
                       flags: byte;   { 1=Pointliste }
                       adr  : longint);
                  1 : (sortl : longint);
                end;
      netrecl = array[1..bersize] of netrec;

      userrec = packed record
                  name : string[MaxNamelen];
                  adr  : array[0..3] of smallword;  { Zone:Net/Node.Point }
                  fnr  : byte;                 { Nodelisten-Dateinr. }
                  fadr : longint;
                end;
      unodep  = ^usernode;
      usernode= packed record
                  left,right : unodep;
                  user       : userrec;
                end;

      idxheader = packed record
                    kennung : array[0..3] of char;
                    beradr  : longint;    { Adresse Bereichsindex }
                    bernum  : smallword;  { Anzahl Bereiche       }
                    adrnetx : longint;    { Adresse Netzindex     }
                    fill    : array[0..49] of byte;
                  end;

      udxheader = packed record
                    kennung : array[0..3] of char;
                    anzahl  : longint;
                    blocks  : longint;
                    version : smallword;
                  end;

type  bereichlst = array[1..maxber] of berrec;

const nodelistopen : boolean = false;

var   NX_adrnetx   : longint;
      bereiche     : word;
      berliste     : ^bereichlst;
      nodef        : file;
      nodelf       : file;
      FreqLst      : string;
      DelFilelist  : boolean;   { lokal NodeSelProc }
      UserBlocks   : longint;


procedure MakeNodelistIndex;
const tbuf     = 8192;
      nbuffers = 32;
var x,y        : Integer;
    nf         : text;
    idf,tf     : file;
    p          : byte;
    s          : string;
    k          : string;
    ss         : string;
    zone,net   : word;
    node,nodes : word;
    l          : longint;
    liste,ltyp : byte;
    np         : ^nodea;
    tb         : pointer;
    newnet     : boolean;
    new_net    : word;
    new_zone   : word;
    fpos       : longint;
    res        : integer;
    nets       : word;
    _netr      : netrec;
    nbuffer    : array[1..nbuffers] of netrec;
    ll         : byte;
    bufnets    : word;
    ixh        : idxheader;
    fa         : FidoAdr;
    points     : integer;
    pp         : ^pointa;

    uroot      : unodep;
    chunksize  : longint;   { User pro Spur }
    chunks     : longint;   { Spuren }
    users      : longint;   { User im akt. Chunk }
    gusers     : longint;   { User gesamt }
    uf         : array[0..1] of ^file;

  procedure Display;
  begin
    attrtxt(col.colmboxhigh);
    moff;
    Wrt(x+31,y+2, Format('%d:%d', [zone, net])); Wrt2(sp(x+40-wherex));
    Wrt(x+49,y+2, Format('%6d', [gusers]));
    mon;
  end;

  procedure wrmsg(txt:string);
  begin
    attrtxt(col.colmbox);
    mwrt(x+3,y+2,forms(txt,55));
  end;

  procedure SortTempindex(l,r:longint);
  var i,j : longint;
      x   : longint;
      w,z : netrec;

    function nrx(ll:longint):longint;
    var nr : netrec;
    begin
      seek(tf,ll*sizeof(netrec));
      blockread(tf,nr,sizeof(netrec));
      nrx:=nr.sortl;
    end;

  begin
    i:=l; j:=r;
    x:=nrx((l+r) div 2);
    repeat
      while nrx(i)<x do inc(i);
      while nrx(j)>x do dec(j);
      if i<=j then begin
        seek(tf,i*sizeof(netrec)); blockread(tf,w,sizeof(netrec));
        seek(tf,j*sizeof(netrec)); blockread(tf,z,sizeof(netrec));
        seek(tf,i*sizeof(netrec)); blockwrite(tf,z,sizeof(netrec));
        seek(tf,j*sizeof(netrec)); blockwrite(tf,w,sizeof(netrec));
        inc(i); dec(j);
        end;
    until i>j;
    if l<j then sorttempindex(l,j);
    if r>i then sorttempindex(i,r);
  end;

  procedure WriteBerindex;
  var
      r    : berrec;
      na   : ^netrecl;
      rr   : Integer;
      add  : word;
      bpos : longint;
  begin
    new(na);
    seek(tf,0);
    bpos:=0;
    repeat
      inc(ixh.bernum);
      if eof(tf) then     { keine Daten -> keine Listen, leerer Index }
        fillchar(r,sizeof(r),0)
      else begin
        blockread(tf,na^,sizeof(na^),rr);
        rr:=rr div sizeof(netrec);
        add:=0;
        if rr=bersize then begin
          while na^[bersize-add].sortl=na^[bersize-add-1].sortl do
            inc(add);
          inc(add);    { letzes Netz in diesem Bereich koennte = 1. Netz im }
                       { naechsten sein..                                   }
          seek(tf,filepos(tf)-add*sizeof(netrec));
          end;
        r.fromnet:=na^[1].net;
        r.fromzone:=na^[1].zone;
        r.anz:=rr-add;
        r.adr:=bpos;
        end;
      blockwrite(idf,r,sizeof(r));
      inc(bpos,r.anz*sizeof(netrec));
    until eof(tf);
    dispose(na);
  end;

  procedure CopyNetindex;
  const bs = 2048;
  var p  : pointer;
      rr : Integer;
  begin
    getmem(p,bs);
    seek(tf,0);
    repeat
      blockread(tf,p^,bs,rr);
      blockwrite(idf,p^,rr);
    until eof(tf);
    freemem(p,bs);
  end;

  { --- User-Indizierung --------------------------------------------- }

  procedure WriteU;
  const ubufsize = 100;
  type  ubufa    = array[0..ubufsize-1] of UserRec;
  var   ubuf     : ^ubufa;
        ubufs    : word;
        lusers   : longint;

    procedure FlushUbufs;
    begin
      blockwrite(uf[0]^,ubuf^,ubufs*sizeof(userrec));
      ubufs:=0;
    end;

    procedure WriteDelU(var node:unodep);
    begin
      if node<>nil then with node^ do begin
        WriteDelU(left);
        ubuf^[ubufs]:=user;
        inc(ubufs);
        if ubufs=ubufsize then FlushUbufs;
        WriteDelU(right);
        dispose(node);
        end;
    end;

  begin
    lusers:=users;
    blockwrite(uf[0]^,lusers,4);
    new(ubuf); ubufs:=0;
    WriteDelU(uroot);
    uroot:=nil;
    if ubufs>0 then FlushUBufs;
    dispose(ubuf);
    users:=0;
    inc(chunks);
  end;

  procedure AppUser(zone,net,_node,point:word; fpos:longint);
  var name : string;
      i    : integer;

    procedure AppU(var node:unodep);
      procedure setname;
      begin
        node^.user.name:=name;
      end;
      function smallname:boolean;
      begin
        smallname:=(name<node^.user.name);
      end;
    begin
      if node=nil then begin
        new(node);
        fillchar(node^,sizeof(node^),0);
        setname;
        with node^.user do begin
          adr[0]:=zone; adr[1]:=net;
          adr[2]:=_node; adr[3]:=point;
          fnr:=liste;
          fadr:=fpos;
          end;
        end
      else
        if SmallName then
          AppU(node^.left)
        else
          AppU(node^.right);
    end;

  begin
    for i:=1 to 3 do begin
      if p>0 then delete(s,1,p);
      p:=cpos(',',s);
      end;
    if p>0 then begin
      name:=LeftStr(s,p-1);
      p:=length(name);
      while (p>1) and (name[p]<>'_') do dec(p);
      if p>1 then
        name:=mid(name,p+1)+' '+LeftStr(name,p-1);
      for i:=1 to length(name) do
        if name[i]='_' then name[i]:=' ';
      name:= UpperCase(name); { UpString(name);}
      AppU(uroot);
      inc(users); inc(gusers);
      if users=chunksize then
        WriteU;
      end;
  end;

  procedure SortChunks;
  var ufpos  : byte;
      i,cc   : longint;
      durchl : integer;   { Anzahl Durchlaeufe }
      nn     : integer;   { akt. Durcklauf }

    procedure MergeChunks(f0,f1:byte);
    const ubufmax = 1000;
    type  ubufa   = array[0..ubufmax-1] of userrec;
    var bufsize   : word;
        bufanz    : word;
        buf       : array[1..3] of ^ubufa;
        bp        : array[1..3] of word;
        anz,banz  : array[1..2] of longint;
        ranz      : array[1..2] of longint;   { schon gelesene User }
        pos       : array[1..2] of longint;
        rn        : byte;
        total     : longint;

      procedure ReadBuf(nr:byte);
      begin
        banz[nr]:=min(anz[nr]-ranz[nr],bufanz);
        if banz[nr]>0 then begin
          seek(uf[f0]^,pos[nr]);
          blockread(uf[f0]^,buf[nr]^,banz[nr]*sizeof(userrec));
          pos[nr]:=filepos(uf[f0]^);
          inc(ranz[nr],banz[nr]);
          end;
        bp[nr]:=0;
      end;

      procedure FlushOutbuf;
      begin
        blockwrite(uf[f1]^,buf[3]^,bp[3]*sizeof(userrec));
        bp[3]:=0;
      end;

      procedure WrUser(nr:byte);
      begin
        buf[3]^[bp[3]]:=buf[nr]^[bp[nr]];
        inc(bp[nr]);
        if bp[nr]=banz[nr] then ReadBuf(nr);
        inc(bp[3]);
        if bp[3]=bufanz then FlushOutbuf;
      end;

    begin
      bufsize:=ubufmax*sizeof(userrec);
      bufanz:=bufsize div sizeof(userrec);
      bufsize:=bufanz*sizeof(userrec);     { Groesse abrunden }
      getmem(buf[1],bufsize);
      getmem(buf[2],bufsize);
      getmem(buf[3],bufsize);
      bp[1]:=0; bp[2]:=0; bp[3]:=0;
      blockread(uf[f0]^,anz[1],4);
      pos[1]:=filepos(uf[f0]^);
      pos[2]:=pos[1]+anz[1]*sizeof(userrec)+4;
      seek(uf[f0]^,pos[2]-4);
      blockread(uf[f0]^,anz[2],4);      { <- Lesefehler! }
      total:=anz[1]+anz[2];
      blockwrite(uf[f1]^,total,4);
      ranz[1]:=0; ranz[2]:=0;
      ReadBuf(1); ReadBuf(2);
      while (bp[1]<banz[1]) and (bp[2]<banz[2]) do
        if buf[1]^[bp[1]].name<buf[2]^[bp[2]].name then
          WrUser(1)
        else
          WrUser(2);
      if bp[1]<banz[1] then rn:=1
      else rn:=2;
      while bp[rn]<banz[rn] do
        WrUser(rn);
      if bp[3]>0 then
        FlushOutbuf;
      seek(uf[f0]^,pos[2]);
      freemem(buf[3],bufsize);
      freemem(buf[2],bufsize);
      freemem(buf[1],bufsize);
    end;

  begin
    spush(chunks,sizeof(chunks));
    durchl:=0;
    while chunks>1 do begin    { zaehlen }
      i:=1; cc:=chunks;
      while i<chunks do begin
        inc(i,2); dec(cc); end;
      chunks:=cc;
      inc(durchl);
      end;
    spop(chunks);

    ufpos:=0; nn:=0;
    while chunks>1 do begin    { sortieren }
      inc(nn);
      attrtxt(col.colmboxhigh);
      mwrt(x+30,y+2,strs(nn)+'/'+strs(durchl));
      seek(uf[0]^,0); seek(uf[1]^,0);
      truncate(uf[1-ufpos]^);
      i:=1; cc:=chunks;
      while i<chunks do begin
        MergeChunks(ufpos,1-ufpos);
        inc(i,2);
        dec(cc);
        end;
      if i=chunks then
        fmove(uf[ufpos]^,uf[1-ufpos]^);    { einzelnen Chunk kopieren }
      chunks:=cc;
      ufpos:=1-ufpos;
      { chunksize:=chunksize*2; }
      end;

    close(uf[1-ufpos]^);
    erase(uf[1-ufpos]^);
    if ufpos=1 then
      Move(uf[1]^,uf[0]^,sizeof(file));
  end;

  procedure MakeUserIndex(xx:byte);     { Userindex komprimieren, user-Name -fidoAdresse -AdresseNlEintrag }
  const ubufanz   = 100;                { werden komprimiert in user.idx abgespeichert }
  type  block     = array[0..blocksize-1] of byte;
        ubufa     = array[0..ubufanz-1] of userrec;
  var   bbuf      : ^block;
        ubuf      : ^ubufa;
        uihd      : udxheader;
        bufp,bufanz:word;
        lname     : string[MaxNamelen];
        user      : UserRec;
        cuser     : array[0..50] of byte;       { komprimierter User-Record }
        cuserp    : byte;
        outp      : word;                       { Position in bbuf }
        w         : word;
        b,adrf    : byte;
        nn        : longint;
        ok        : boolean;

    procedure ReadUbuf;
    var 
      rr: Integer;
    begin
      blockread(uf[0]^,ubuf^,sizeof(ubufa),rr);
      bufanz:=rr div sizeof(userrec);
      bufp:=0;
    end;

    procedure FlushOut;
    begin
      attrtxt(col.colmboxhigh);
      mwrt(xx,y+2,strsn(nn*100 div uihd.anzahl,3));
      bbuf^[outp]:=$ff;
      blockwrite(uf[1]^,bbuf^,blocksize);
      fillchar(bbuf^,blocksize,0);
      outp:=0;
      lname:='';
    end;

  begin         { procedure MakeUserIndex(xx:byte);      Userindex komprimieren  }
    fillchar(uihd,sizeof(uihd),0);
    uihd.kennung:=nodekenn;
    seek(uf[0]^,0);
    blockread(uf[0]^,uihd.anzahl,4);
    new(bbuf);
    fillchar(bbuf^,blocksize,0);
    seek(uf[1]^,sizeof(uihd));
    blockwrite(uf[1]^,bbuf^,blocksize-sizeof(uihd));
    new(ubuf);
    ReadUbuf;
    lname:=''; outp:=0;
    nn:=0;
    while bufp<bufanz do begin
      inc(nn);
      user:=ubuf^[bufp];      { user.name :string[30]; adr: array[0..3] of smalword; fnr:byte        ; fadr :longint}
      inc(bufp);              { username               zone/net/node               Nodelistennummer    adresse in der nodelist }
      if bufp=bufanz then ReadUbuf;
      with user do
        repeat
          cuserp:=0;                                            { Laender der abzuspeichernden Daten }
          { R-}
          b:=0; w:=min(length(name),length(lname));             { user.name, lname = name des letzten Eintrages}
          while (b<w) and (name[b+1]=lname[b+1]) do inc(b);     { aktueller username im letzten usernamen enthalten oder gleich? }
          cuser[cuserp]:=b; inc(cuserp,2);                      { curser[0]= Anzahl der gleichen chars,curse[1] wird uebersprungen inhalt adrf }
          w:=length(name)-b;                                    { Laenge des Namens (Anzahl der ungleichen Zeichen )}
          cuser[cuserp]:=w; inc(cuserp);        { Name }        { curser[2]=Laenge Namen, Zeiger+1}
          if w>0 then                                           { curser[3]=Anzahl derungleiche Zeichen }
            Move(name[b+1],cuser[cuserp],w);                    { Name nach curser[3] und}
          inc(cuserp,w);                                        { Zeiger um Laenge Namen erhoehen }
          lname:=name;                                          { aktuellen Namen in lname merken }
          adrf:=0;                                              { adrf:byte, bitfeld zum merken div. Eigenschaften }
          if (adr[0]>0) and (adr[0]<16) then    { Zone }        { passt user zone in 4 Bit }
            inc(adrf,adr[0]*16)                                 { ja, adrf=user.ddr[0] geshiftet um 4 }
          else begin
            cuser[cuserp]:=lo(adr[0]);                          { nein, user zone in zwei bytes sichern }
            cuser[cuserp+1]:=hi(adr[0]);                        { swap}
            inc(cuserp,2);                                      { zeiger+2 }
            end;
          cuser[cuserp]:=lo(adr[1]); inc(cuserp);   { Net }     { net speichern }
          if adr[1]<256 then                                    { passt net in ein Byte }
            inc(adrf,1)                                         { ja, lsb in adef setzen}
          else begin
            cuser[cuserp]:=hi(adr[1]); inc(cuserp);             { nein, dann  net in zwei bytes speicher }
            end;
          cuser[cuserp]:=lo(adr[2]); inc(cuserp);   { Node }    { das gleiche Spiel mit der node Adresse }
          if adr[2]<256 then                                    { passt in ein byte }
            inc(adrf,2)                                         { ja, bit 1 setzen }
          else begin
            cuser[cuserp]:=hi(adr[2]); inc(cuserp);             { nein, dann noch zweites byte speichern }
            end;
          if adr[3]=0 then                                      { Point ? }
            inc(adrf,4)                                         { nein, bit 2 setzen }
          else begin
            cuser[cuserp]:=lo(adr[3]); inc(cuserp);             { nein, point# speichern }
            if adr[3]<256 then                                  { passt point in ein byte }
              inc(adrf,8)                                       { bit 3 setzen }
            else begin
              cuser[cuserp]:=hi(adr[3]); inc(cuserp);
              end;
            end;
          cuser[1]:=adrf;       { Adress-Flag  bit 0  - net  in einem byte gespeichert }
                                {             bit 1  - node in einem byte gespeichert }
                                {             bit 2  - is node (keine point# gespeicheret }
                                {             bit 3  - point in einem byte gespeichert }
                                {             bit 4..7 -zonen#, wenn zone# <16         }

          if fnr=0 then         { nummer der nodeliste, 0 = keine Nodeliste bei Dos/16 oder }
            inc(cuser[2],$40)   { bit 6 setzen          0 = ertser Eintrag in der Tlist }
          else begin
            cuser[cuserp]:=fnr;                 { nodelisten# speichern  }
            inc(cuserp);                        { zeiger + 1 }
            end;
          if fadr<$1000000 then begin           { passt Adress in 3 byte }
            inc(cuser[2],$80);                  { ja, msb curser[2] setzen }
            Move(fadr,cuser[cuserp],3);         { ja, adresse nach cuser schieben }
            inc(cuserp,3);                      { Zeiger erhoehen }
            end
          else begin
            Move(fadr,cuser[cuserp],4);         { nein, 4-byte Adresse sichern }
            inc(cuserp,4);
            end;
          { R+}
          ok:=(outp+cuserp+1)<=blocksize;       { blockgroesse erreicht ? }
          if not ok then
            FlushOut                            { ja, block wegschreiben }
          else begin
            Move(cuser,bbuf^[outp],cuserp);     { cuser[], in den Block schreiben }
            inc(outp,cuserp);                   { bytezaehler erhoehen}
            end;
        until ok;
      end;

    if outp>0 then
      FlushOut;
    seek(uf[1]^,0);
    uihd.blocks:=filesize(uf[1]^) div blocksize - 1;
    blockwrite(uf[1]^,uihd,sizeof(uihd));       { Header schreiben }
    dispose(ubuf);
    dispose(bbuf);
  end;

  procedure SortNodes(l,r:integer);
  var i,j : integer;
      x   : word;
      w   : noderec;
  begin
    i:=l; j:=r;
    x:=np^[(l+r) div 2].node;
    repeat
      while np^[i].node<x do inc(i);
      while np^[j].node>x do dec(j);
      if i<=j then begin
        w:=np^[i]; np^[i]:=np^[j]; np^[j]:=w;
        inc(i); dec(j);
        end;
    until i>j;
    if l<j then sortnodes(l,j);
    if r>i then sortnodes(i,r);
  end;

  procedure writenodes;
  begin
    blockwrite(idf,np^,nodes*sizeof(noderec));
  end;

  procedure writepoints;
  begin
    blockwrite(idf,points,2);
    blockwrite(idf,pp^,points*sizeof(pointrec));
    points:=0;
  end;

  procedure flushnbuffers;
  begin
    blockwrite(tf,nbuffer,bufnets*sizeof(netrec));
    bufnets:=0;
  end;

  procedure AppPoint(pnode:word);
  begin
    if points<maxpoints then begin
      pp^[points].point:=node;
      pp^[points].adr:=fpos;
      inc(points);
      AppUser(zone,net,pnode,node,fpos);
      end;
  end;

begin
  getmem(tb,tbuf);
  msgbox(59,5,getres2(2101,1),x,y) ;   { 'Nodeindex anlegen' }
  mwrt(x+3,y+2,getres2(2101,2));       { 'Datei' }
  mwrt(x+25,y+2,getres2(2101,3));      { 'Netz'  }
  mwrt(x+43,y+2,getres2(2101,4));      { 'User'  }
  new(np); new(pp);
  assign(idf,NodeindexF);
  rewrite(idf,1);
  seek(idf,64);
  assign(tf,'nodes.$$$'); rewrite(tf,1);
  nets:=0; bufnets:=0;
  uroot:=nil; chunks:=0; users:=0; gusers:=0;
  chunksize:= 2566;
  new(uf[0]);
  assign(uf[0]^,'users1.$$$'); rewrite(uf[0]^,1);

  for liste:=0 to NodeList.Count - 1 do
  begin
    zone:=TNodeListItem(Nodelist.Items[liste]).zone;
    if zone=0 then zone:=DefaultZone;
    net:=0; node:=0;
    assign(nf,FidoDir+NodeList.GetFilename(liste));
    ltyp:=TNodeListItem(Nodelist.Items[liste]).fformat;
    case ltyp of
      nlPoints24,
      nl4DPointlist,
      nlFDpointlist : zone:=TNodeListItem(Nodelist.Items[liste]).zone;
      nlNode        : begin
                         zone:=TNodeListItem(Nodelist.Items[liste]).zone;
                         net :=TNodeListItem(Nodelist.Items[liste]).net;
                      end;
    end;

    if existf(nf) then begin
      settextbuf(nf,tb^,tbuf);
      reset(nf);
      attrtxt(col.colmboxhigh);
      mwrt(x+10,y+2,forms(ExtractFileName(filename(nf)),12));
      fpos:=0;
      nodes:=0;
      points:=0;
      repeat
        newnet:=false;
        Display;

        repeat
          readln(nf,s);
          ll:=length(s);
          p:=cpos(',',s);
          if ( FirstChar(s)<>';') and (p>0) then begin
            if (p=1) or (ltyp=nlNode) then begin
              if ltyp=nlFDpointlist then k:='Point'
              else k:='';
              newnet:=false; end
            else begin
              k:=TopStr(copy(s,1,p-1));
              if (ltyp=nlFDpointlist) and (k='Boss') then begin
                ss:=mid(s,p+1);
                p:=cposx(',',ss);
                SplitFido(LeftStr(ss,p-1),fa,zone);
                if fa.zone<>zone then begin
                  k:='Zone'; newnet:=true; end
                else if fa.net<>net then begin
                  k:='Host'; newnet:=true; end
                else if fa.node<>node then begin
                  k:='Node'; newnet:=false; end
                else begin
                  k:=''; newnet:=false; end;
                node:=fa.node;
                new_zone:=fa.zone; new_net:=fa.net;
                end
              else
                newnet:=(k='Host') or (k='Region') or (k='Zone');
              end;

            if (ltyp<>nlFDpointlist) or (k='Point') then begin
              delete(s,1,p);
              p:=cposx(',',s);
              val(LeftStr(s,p-1),l,res);
              node:=minmax(l,0,65535);
              end;

            if node<>0 then case ltyp of

              nlNodelist:
                if not newnet and (nodes<maxnodes) then begin
                  np^[nodes].node:=node;
                  np^[nodes].adr:=fpos;
                  inc(nodes);
                  AppUser(zone,net,node,0,fpos);
                  end;

              nl4DPointlist:
                if k='Point' then
                  if nodes>0 then AppPoint(np^[nodes-1].node)
                  else   { nodes=0 kann vorkommen, wenn falsches Listenformat }
                else
                  if not newnet and (nodes<maxnodes) then begin
                    if points>0 then WritePoints;
                    np^[nodes].node:=node;
                    np^[nodes].adr:=filepos(idf);
                    inc(nodes);
                    end;

              nlFDpointlist:
                if (k='Node') and (nodes<maxnodes) then begin
                  if points>0 then WritePoints;
                  np^[nodes].node:=node;
                  np^[nodes].adr:=filepos(idf);
                  inc(nodes);
                  end
                else
                  if (k='Point') or (k='Pvt') or (k='Down') or (k='Hold') then
                    if nodes>0 then  { sicher ist sicher ... }
                      AppPoint(np^[nodes-1].node);

              nlNode:
                if not newnet then
                  AppPoint(TNodeListItem(Nodelist.Items[liste]).node);

              nlPoints24:
                if not newnet then
                  if nodes>0 then AppPoint(np^[nodes-1].node)
                  else
                else begin
                  if points>0 then
                    WritePoints
                  else
                    if nodes>0 then
                      dec(nodes);   { Node ohne Points !? }
                  if k='Region' then
                    k:='Host'
                  else if (k='Host') and (nodes<maxnodes) then begin
                    newnet:=false;
                    delete(s,1,p);
                    p:=cpos(',',s);
                    splitfido(LeftStr(s,p-1),fa,zone);
                    np^[nodes].node:=fa.node;
                    np^[nodes].adr:=filepos(idf);
                    net:=fa.net;
                    Display;
                    inc(nodes);
                    end;
                  end;

              end;  { case }
            end;  { s[1]<>';' }
          inc(fpos,ll+2);
        until newnet or eof(nf);

        if points>0 then begin
          if nodes=0 then begin       { ntNode }
            np^[0].node:=TNodeListItem(Nodelist.Items[liste]).node;
            inc(nodes);
            end;
          np^[nodes-1].adr:=filepos(idf);
          WritePoints;
          end;

        if nodes>0 then begin
          inc(nets);
          _netr.net:=net;
          _netr.zone:=zone;
          _netr.anz:=nodes;
          _netr.fnr:=liste;
          _netr.flags:=iif(ltyp=nlNodelist,0,1);
          _netr.adr:=filepos(idf);
          inc(bufnets);
          nbuffer[bufnets]:=_netr;
          if bufnets=nbuffers then
            flushnbuffers;
          if nodes>1 then
            SortNodes(0,nodes-1);
          WriteNodes;
          end;
        if ltyp=nlFDpointlist then begin
          zone:=new_zone;
          net:=new_net;
          end;
        if not eof(nf) then begin
          if ltyp<>nlFDpointlist then begin
            if (k<>'Host') and (k<>'Region') then
              zone:=node;
            net:=node;
            np^[0].node:=0;
            np^[0].adr:=fpos-ll-2;
            end
          else begin
            np^[0].node:=node;
            np^[0].adr:=filepos(idf);
            end;

          nodes:=1;
          node:=0;
          if ltyp=nlNodelist then
            AppUser(zone,net,node,0,fpos-ll-2);
          end;
      until eof(nf);
      close(nf);
      end;
    end;

  if bufnets>0 then
    flushnbuffers;
  WriteU;

  fillchar(ixh,sizeof(ixh),0);
  ixh.kennung:=NodeKenn;
  ixh.beradr:=filepos(idf);
  ixh.bernum:=0;  {(nets-1) div bersize +1;}

  attrtxt(col.colmbox);
  wrmsg(getres2(2101,5));   { 'Netzindex sortieren ...' }

  if nets>0 then
    SortTempIndex(0,nets-1);
  seek(idf,ixh.beradr);
  WriteBerindex;
  ixh.adrnetx:=ixh.beradr+ixh.bernum*sizeof(berrec);
  CopyNetindex;

  close(tf);
  erase(tf);
  dispose(pp); dispose(np);

  wrmsg(getres2(2101,6));    { 'Userindex sortieren ...' }
  new(uf[1]); assign(uf[1]^,'users2.$$$');
  rewrite(uf[1]^,1);
  SortChunks;    { schliesst+loescht uf[1]^ }
  wrmsg(getres2(2101,7));    { 'Userindex packen ...      %' }
  assign(uf[1]^,UserIndexF);
  rewrite(uf[1]^,1);
  MakeUserindex(x+length(getres2(2101,7))-2);
  close(uf[0]^); erase(uf[0]^); dispose(uf[0]);
  close(uf[1]^); dispose(uf[1]);

  seek(idf,0);
  blockwrite(idf,ixh,sizeof(ixh));
  close(idf);

  freeres;
  closebox;
  freemem(tb,tbuf);
end;


procedure OpenNodeindex(const fn:string);
var hd  : idxheader;
    uhd : udxheader;
    rr  : Integer;
    f   : file;
  procedure NXerror;
  begin
    rfehler(2101);    { 'fehlerhafter Nodelisten-Index' }
    close(nodef);
    erase(nodef);
  end;
begin
  assign(nodef,fn);
  reset(nodef,1);
  fillchar(hd,sizeof(hd),0);
  blockread(nodef,hd,sizeof(hd),rr);
  if (hd.kennung<>nodekenn) or (hd.beradr>=filesize(nodef)) then begin
    NXerror; exit; end;
  nx_adrnetx:=hd.adrnetx;
  bereiche:=hd.bernum;
  getmem(berliste,bereiche*sizeof(berrec));
  seek(nodef,hd.beradr);
  blockread(nodef,berliste^,bereiche*sizeof(berrec));
  close(nodef);
  assign(f,UserIndexF);
  reset(f,1);
  fillchar(uhd,sizeof(uhd),0);
  blockread(f,uhd,sizeof(uhd),rr);
  if (uhd.kennung<>nodekenn) then begin
    close(f);
    NXerror; exit; end;
  UserBlocks:=uhd.blocks;
  close(f);
  Nodelist.Open:=true;
end;


procedure CloseNodeindex;
begin
  freemem(berliste,bereiche*sizeof(berrec));
end;


procedure KeepNodeindexOpen;
begin
  if Nodelist.Open and not nodelistopen then begin
    { new(nodelf);
    assign(nodelf^,nodefile);
    reset(nodelf^,1); }
    nodelistopen:=true;
    reset(nodef,1);
    end;
end;

procedure KeepNodeindexClosed;
begin
  if nodelistopen then begin
    close(nodef);
 {   close(nodelf^);
    dispose(nodelf); }
    nodelistopen:=false;
    end;
end;


function FormFidoPhone(telefon:string):string;
var p : byte;
begin
  if pos('unpublished',LowerCase(telefon))>0 then
    FormFidoPhone:=telefon
  else if LeftStr(telefon,length(vorwahl))=vorwahl then begin
    delete(telefon,1,length(vorwahl));
    TrimFirstChar(telefon, '-');
    FormFidoPhone:=telefon;
    end
  else begin
    p:=cpos('-',vorwahl);
    if LeftStr(telefon,p)=LeftStr(vorwahl,p) then
      FormFidoPhone:=NatVorwahl+mid(telefon,p+1)
    else
      FormFidoPhone:=intVorwahl+telefon;
    end;
end;

{.$I xpf1.inc}   { Nodeliste auslesen/abfragen }

{ XPFIDO - Nodeliste auslesen/abfragen }

procedure ReadNData(nfile:byte; adr:longint; var ni:NodeInfo);
var s  : string;
    rr : Integer;

  procedure SetInfo;
  var p : byte;
      x : string[10];
    function getstr:string;
    var i: byte;
    begin
      p:=cpos(',',s);
      if p=0 then
        getstr:=''
      else
      begin
        for i:=p-1 downto 1 do
          if s[i]='_' then s[i]:=' ';

        getstr:=copy(s,1,p-1);
        delete(s,1,p);
      end;
    end;
  begin
    with ni do begin
      if LastChar(s)<>',' then s:=s+',';
     { for p:=1 to length(s^) do       nach GetStr verschoben, damit im
        if s^[p]='_' then s^[p]:=' ';  String FFlags '_' erhalten bleibt }
      status:=getstr;
      p:=cpos(',',s);
      if p>0 then begin
        if status='' then
          status:=iifs(ispoint,'Point','Node');
        delete(s,1,p);    { Nodenummer }
        boxname:=getstr;
        standort:=getstr;
        sysop:=getstr;
        telefon:=getstr;
        baud:=minmax(ival(getstr),110,65535);
        fflags:=s;
        DeleteLastChar(fflags);    { Komma entfernen }
        repeat
          x:=getstr;
          if x='V32B' then flags:=flags or nfV32b else
          if x='V32' then flags:=flags or nfV32 else
          if (x='HST') or (x='H14') then flags:=flags or nfHST else
          if x='PEP' then flags:=flags or nfPEP else
          if x='ZYX' then flags:=flags or nfZYXEL else
          if x='H16' then flags:=flags or nfHST16 else
          if pos('ISDN',x)>0 then flags:=flags or nfISDN else
          { MK 01/00 Zeile eingefuegt, erkennt jetzt ISDN-Boxen richtig }
          if (x='X75') then flags:=flags or nfISDN else
          if (x='VFC') then flags:=flags or nfVFC else
          if (x='V32T') then flags:=flags or nfTerbo else
          if (x='V34') then flags:=flags or nfV34 else
          if x='CM' then flags:=flags or nfCM else
          if x='XA' then request:=rfWaZOO+rfUpWaz+rfBark+rfUpBark else
          if x='XB' then request:=rfBark+rfUpBark+rfWaZOO else
          if x='XC' then request:=rfBark+rfWaZOO+rfUpWaz else
          if x='XP' then request:=rfBark+rfUpBark else
          if x='XR' then request:=rfBark+rfWaZOO else
          if x='XW' then request:=rfWaZOO else
          if x='XX' then request:=rfWaZOO+rfUpWaz else
          if x='MN' then flags:=flags and (not nfComp);
        until x='';
        end;
      end;
  end;

begin           
  ni.found:=false;                                             
  if nfile>NodeList.Count-1 then exit;
  assign(nodelf,FidoDir+NodeList.GetFilename(nfile));    //es kann ein nodelisten index von
  resetfm(nodelf,fmRead);
  if ioresult=0 then
  begin
    { reset(nodelf^,1);  !?!? }
    seek(nodelf,adr);
    SetLength(s,255);
    blockread(nodelf,s[1],255,rr);
    SetLength(s,rr);
    SetLength(s,cpos(#13,s)-1);
    SetInfo;
    ni.found:=true;
    close(nodelf);
    end;
end;


{ Pointtyp: 0=nur Node, 1=Point/Node, 2=bei nicht gef. Point wiederholen }

procedure GetNodeinfo(const adr:string; var ni:nodeinfo; pointtyp:integer);
var fa     : fidoadr;
    i,netp : integer;
    bp     : ^netrecl;
    banz   : word;
    nanz   : word;
    nadr,l : longint;
    nfile  : byte;
    np     : ^nodea;
    found  : boolean;
    _adr   : longint;
    points : integer16;
    pp     : ^pointa;

label again;

begin
  fillchar(ni,sizeof(ni),0);
  if not Nodelist.Open then exit;
  splitfido(adr,fa,2);
  if pointtyp=0 then fa.ispoint:=false;
  if not nodelistopen then begin
    reset(nodef,1);
    if ioresult<>0 then exit;
    end;
again:
  i:=bereiche;
  while (i>0) and ((berliste^[i].fromzone>fa.zone) or
                   ((berliste^[i].fromzone=fa.zone) and
                    (berliste^[i].fromnet>fa.net))) do
    dec(i);
  fillchar(ni,sizeof(ni),0);
  if i>0 then begin
    new(bp);
    seek(nodef,NX_adrnetx+berliste^[i].adr);
    banz:=berliste^[i].anz;                     //Bereichsliste, array[1..maxber] of berrec
    if banz>bersize then
      writeln(getres(2121),#7);   { 'Fehler in Nodelisten-Index!' }
    //alle netze einer Zone
    blockread(nodef,bp^,banz*sizeof(netrec));   // bp netrecL   {net,zone,ant, dateiNo(fnr), flag (1=Pointliste), adr, sortl }
    l:=$10000*fa.zone+fa.net;
    {$R-}
    netp:=1;
    while (netp<=banz) and (bp^[netp].sortl<l) do
      inc(netp);
    repeat
      found:=(netp<=banz) and (bp^[netp].sortl=l);
      if found then begin
        nanz:=bp^[netp].anz;
        nadr:=bp^[netp].adr;
        nfile:=bp^[netp].fnr;                           //der index fuer die nodeliste
        ni.datei:=nfile;
        end;
      if found and (fa.ispoint=odd(bp^[netp].flags)) then begin
        getmem(np,nanz*sizeof(noderec));
        seek(nodef,nadr);
        blockread(nodef,np^,nanz*sizeof(noderec));
        i:=0;
        while (i<nanz) and (np^[i].node<fa.node) do      //np bis zur Node# abklappern
          inc(i);                                        //enthaelz nun den passenden Satz
{$IFDEF Debug }
  {$R+}
{$ENDIF }
        if (i<nanz) and (np^[i].node=fa.node) then
          _adr:=np^[i].adr                               //adresse zur Node#'12478'
        else
          _adr:=-1;
        freemem(np,nanz*sizeof(noderec));
        if (_adr>=0) and fa.ispoint then begin           //node gefunden aber point
          seek(nodef,_adr);
          blockread(nodef,points,2);
          getmem(pp,points*sizeof(pointrec));
          blockread(nodef,pp^,points*sizeof(pointrec));
          i:=0;
          while (i<points) and (pp^[i].point<fa.point) do
            inc(i);
          if (i<points) and (pp^[i].point=fa.point) then
            _adr:=pp^[i].adr
          else
            _adr:=-1;
          freemem(pp,points*sizeof(pointrec));
          end;
        if _adr>=0 then begin
          ni.ispoint:=fa.ispoint;
          ReadNData(nfile,_adr,ni);                     //Nodedaten nach ni auslesen
          end;
        end;
      inc(netp);
    until not found or ni.found;
    dispose(bp);
    end;
  if (pointtyp=2) and not ni.found and fa.ispoint then begin
    fa.ispoint:=false;
    goto again;
    end;
  ni.ispoint:=fa.ispoint;
  if not nodelistopen then
    close(nodef);
end;


function IsFidoNode(const adr:string):boolean;
var ni : NodeInfo;
begin
  GetNodeInfo(adr,ni,1);
  IsFidoNode:=ni.found;
end;


procedure GetNodeuserInfo(var fa:FidoAdr; var ni:NodeInfo);
type ubufa  = array[0..blocksize-1] of byte;
var  f     : file;
     name  : string;
     vname : string;
     s     : string;
     p,x,y : Integer;
     buf   : ^ubufa;
     bufp  : word;
     l,r,m : longint;
     user  : UserRec;
     last  : boolean;
     anz   : longint;
     brk   : boolean;
     height: word;
     List: TLister;

label ende;

  function getbyte:byte;
  begin
    getbyte:=buf^[bufp];
    inc(bufp);
  end;

  procedure GetNextUser;
  var flags,b : byte;
      not0    : boolean;        { User nicht aus Nodeliste 0 }
      adr3    : boolean;        { 3-Byte-Dateioffset }
  begin
    with user do begin
      name[0]:=chr(getbyte);    { n Zeichen uebernehmen }
      flags:=getbyte;
      b:=getbyte;
      not0:=(b and $40)=0;
      adr3:=(b and $80)<>0;
      b:=b and $3f;
      if b>0 then begin            { restlichen Namen kopieren }
        if b+length(name)>MaxNameLen then begin
          rfehler(2120);   { 'Fehler im Nodelisten-Userindex' }
          b:=MaxNameLen-length(name);
          end;
        Move(buf^[bufp],name[length(name)+1],b);
        inc(byte(name[0]),b);
        inc(bufp,b);
        end;
      adr[0]:=flags shr 4;         { Adresse ermitteln }
      if adr[0]=0 then begin
        adr[0]:=getbyte;        { getrennte Auswertung, wegen umgekehrter }
        inc(adr[0],256*getbyte);    { Auswertung durch Compiler! }
        end;
      adr[1]:=getbyte;
      if not odd(flags) then inc(adr[1],256*getbyte);
      adr[2]:=getbyte;
      if flags and 2=0 then inc(adr[2],256*getbyte);
      if flags and 4<>0 then
        adr[3]:=0
      else begin
        adr[3]:=getbyte;
        if flags and 8=0 then inc(adr[3],256*getbyte);
        end;
      if not0 then fnr:=getbyte    { Datei-Nummer }
      else fnr:=0;
      b:=iif(adr3,3,4);            { Datei-Offset }
      fadr:=0;
      Move(buf^[bufp],fadr,b);
      inc(bufp,b);
      last:=buf^[bufp]=$ff;
      end;
  end;

  procedure GetFirstUser;
  begin
    bufp:=0;
    user.name:='';
    GetNextUser;
  end;

  procedure ReadBlock(nr:longint);
  begin
    seek(f,nr*BlockSize);
    blockread(f,buf^,BlockSize);
    GetFirstUser;
    vname:=LeftStr(user.name,length(name));
  end;

begin
  fillchar(ni,sizeof(ni),0);
  if UserBlocks=0 then exit;    { keien Nodelisten -> leerer Index }
  with fa do begin
    p:=cpos(',',username);         { Name formatieren }
    if p>0 then
      name:=LeftStr(username,p-1)+' '+trim(mid(username,p+1))
    else begin
      p:=length(username);
      while (p>1) and (username[p]<>' ') do dec(p);
      if p>1 then name:=mid(username,p+1)+' '+LeftStr(username,p-1)
      else name:=username;
      end;
    end;
  name:=trim(name);
  if name='' then exit;
  UpString(name);
  assign(f,UserIndexF);
  reset(f,1);
  new(buf);
  l:=1; r:=UserBlocks;
  repeat
    m:=(l+r)div 2;
    ReadBlock(m);
    if vname<name then l:=m
    else r:=m;
  until (r-l<2) or (vname=name);
  if (name<>vname) and (l<m) then ReadBlock(l)
  else l:=m;
  if name<vname then goto ende;
  while (name=vname) and (l>1) do begin
    dec(l);
    ReadBlock(l);
    end;
  repeat
    while not last and (user.name<name) do
      GetNextUser;
    if (user.name<name) and (l<userblocks) then begin
      inc(l);
      ReadBlock(l);
      end;
  until last or (LeftStr(user.name,length(name))>=name);
  if (LeftStr(user.name,length(name))=name) then
  begin
    List := TLister.CreateWithOptions(2,ScreenWidth-2,10,11,0,'/NS/SB/NLR/DM/APGD/');
    anz:=0;
    repeat
      with user do
      begin
        ReadNdata(fnr,fadr,ni);
        List.AddLine(' '+forms(TopAllStr(user.name),22)+'  '+
              forms(strs(adr[0])+':'+strs(adr[1])+'/'+strs(adr[2])+
              iifs(adr[3]=0,'','.'+strs(adr[3])),15)+' '+
              forms(iifs(adr[3]=0,ni.boxname+', '+ni.standort,ni.standort),32));
        inc(anz);
        if not last then
          GetNextUser
        else begin
          inc(l);
          if l<=userblocks then
            ReadBlock(l);
          end;
        end;
    until (LeftStr(user.name,length(name))<>name) or (l>userblocks);
    if anz>0 then begin
      if anz=1 then
        s:=List.FirstLine
      else begin
        selbox(76,min(anz+2,screenlines-6),'',x,y,true);
        height:=min(anz+2,screenlines-6)-2;
        List.SetSize(x+1,x+74,y+1,y+height);
        List.SetArrows(x,y+1,y+height,col.colselbox,col.colselbox,'�');
        listboxcol(list);
        pushhp(80);
        brk := List.Show;
        pophp;
        closebox;
        if brk then s:=''
        else s:=List.GetSelection;
        end;
      if s<>'' then begin
        SplitFido(trim(copy(s,26,15)),fa,DefaultZone);
        GetNodeinfo(trim(copy(s,26,15)),ni,1);
        end
      else
        ni.found:=false;
      end;
    List.Free;
  end;

ende:
  close(f);
  dispose(buf);
end;


procedure NodelistSeek;
const active : boolean = false;
var x,y,b: Integer;
    brk  : boolean;
    ni   : ^NodeInfo;
    adr  : string;
    fa   : fidoadr;
    first: boolean;
    NlItem :TNodeListItem;
begin
  if active or not TestNodelist or DisableAltN then exit;
  new(ni);
  active:=true;
  diabox(77,11,getres2(2100,1),x,y);    { Node-Infos abfragen }
  if NodeList.GetMainNodelist>0 then begin
    NlItem:=nodelist.Items[NodeList.GetMainNodelist];
    attrtxt(col.coldiarahmen);
    //mwrt(x+70,y,' '+formi(PNodeListItem(nodelist[MainNodelist]^).number,3)+' ');
    mwrt(x+70,y,' '+formi(NlItem.number,3)+' ');
    attrtxt(col.coldialog);
    end;
  mwrt(x+3, y+2, GetRes2( 2100, 2));     { Box   }
  mwrt(x+3, y+3, GetRes2( 2100, 4));     { Sysop }
  mwrt(x+3, y+4, GetRes2( 2100, 5));     { TelNr }
  mwrt(x+3, y+5, GetRes2( 2100, 6));     { Flags }
  mwrt(x+3, y+7, GetRes2( 2100, 3));     { ~���į [                    ] }
  mwrt(x+35, y+7, GetRes2( 2100, 10));   { eMAiL }
  mwrt(x+35, y+8, GetRes2( 2100, 9));    { Status }
  mwrt(x+55, y+8, GetRes2( 2100, 8));    { Datei }
  adr:='';
  first:=true;
  TempOpen;
  repeat
    if first and (aktdispmode in [10..19]) then begin
      dbGo(mbase,AktDisprec);
      if not dbEOF(mbase) and not dbBOF(mbase) and (mbNetztyp=nt_Fido) then begin
        splitfido(dbReadStrN(mbase,mb_absender),fa,DefaultZone);
        adr:=MakeFidoAdr(fa,false);
        brk:=false;
        end;
      end;
    if not first or (adr='') then begin
      pushhp(750);
      ReadString(x+3, y+8, Forms(GetRes2( 2100, 7), 6), adr, 20, 20, '', brk);
      pophp;                   { AKA�į }
      end;
    first:=false;
    if not brk then begin
      if not isNodeAddress(adr) then begin
        fa.username:=adr;
        getNodeUserInfo(fa,ni^);
        end
      else begin
        splitfido(adr,fa,DefaultZone);
        adr:=MakeFidoAdr(fa,true);
        GetNodeinfo(adr,ni^,1);
        end;
      attrtxt(col.coldialog);
      moff;
    clwin(x+10, x+75, y+2, y+5); { Oberer Block }
     clwin(x+43, x+51, y+8, y+8); { Status      }
      clwin(x+62, x+73, y+8, y+8); { Datei      }
       clwin(x+11, x+30, y+7, y+7); { AKA [ ]   }
        clwin(x+43, x+75, y+7, y+7); { eMAiL    }
      if ni^.found then with ni^ do begin
        attrtxt(col.coldiahigh);
        wrt(x+10, y+2, LeftStr(BoxName + ', ' + Standort,65));
        wrt(x+10, y+3, Sysop);
        wrt(x+10, y+4, Telefon);
        wrt(x+10, y+5, copy(MailString(FFlags, True),1,65)); { eMail loeschen }
        wrt(x+11, y+7, MakeFidoAdr(fa, True));
        b := cpos('@', FFlags);
        if b = 0 then FFlags := '';
        wrt(x+43, y+7, MailString(FFlags, False)); { eMail extrahieren }
        wrt(x+43, y+8, Status);
        wrt(x+62, y+8, NodeList.GetFilename(datei));
        end;
      mon;
      end;
  until brk;
  freeres;
  closebox;
  dispose(ni);
  active:=false;
end;


{ --- Nodelist-Browser ---------------------------------------------- }

var  rdispx,rdispy : byte;

procedure ShowRQ(s:string);
var ni  : NodeInfo;
    add : byte;
begin
  GetNodeinfo(copy(s,39,17),ni,1);
  add:=max(length(getres2(2131,30)),length(getres2(2131,32)));
  attrtxt(col.colselbox);
  if ni.ispoint then begin
    mwrt(rdispx,rdispy,getres2(2131,33));    { 'Sysop' }
    mwrt(rdispx+54-length(getres2(2131,34)),rdispy-1,getres2(2131,34));
    end                                      { 'Pointliste' }
  else begin
    mwrt(rdispx,rdispy,getres2(2131,30));    { 'Sysop' }
    mwrt(rdispx+54-length(getres2(2131,31)),rdispy-1,getres2(2131,31));
    end;                                     { 'Nodeliste' }
  mwrt(rdispx,rdispy+1,getres2(2131,32));    { 'Flags' }
  mwrt(rdispx,rdispy-1,getres2(2131,35));    { 'Status' }
  attrtxt(col.colselhigh);
  if ni.found then begin
    mwrt(rdispx+add+2,rdispy,forms(iifs(ni.ispoint,ni.boxname,ni.sysop),33));
    mwrt(rdispx+55,rdispy-1,forms(NodeList.GetFilename(ni.datei),12));
    mwrt(rdispx+add+2,rdispy+1,forms(ni.fflags,65));
    mwrt(rdispx+7,rdispy-1,forms(ni.status,12));
    end
  else begin
    mwrt(rdispx+add+2,rdispy,sp(33));
    mwrt(rdispx+55,rdispy-1,sp(12));
    mwrt(rdispx+add+2,rdispy+1,sp(65));
    mwrt(rdispx+7,rdispy-1,sp(12));
    end;
end;


procedure NodelistBrowser;

const orflags  = 10;
      andflags = 5;

type NodeBrec = record
                  nodeadr : string;
                  sysop   : string;
                  boxname : string;
                  standort: string;
                  telefon : string;
                  flags   : string;
                  fnl,snl,
                  pl      : boolean;
                end;
     NodeBRP  = ^NodeBrec;

const NB_data : NodeBRP = nil;
      bufsize = 2048;

type
  tAndFlags = array[1..andflags] of String;
var   x,y,h   : Integer;
      brk     : boolean;
      flag    : array[1..orflags] of tAndFlags;
      k       : string;
      sphone  : string;    { in Nodelistenformat konvertierte Tel.Nr. }
      adr     : string;
      flaganz : array[1..orflags] of byte;
      flags   : byte;     { Anzahl or-Flags }
      i       : integer;
      t       : text;
      s,ss    : string;
      buf     : pointer;
      found,n : longint;
      p       : byte;
      nn      : word;
      azone,                   { aktuelle Nodeadresse beim Suchen }
      anet,
      anode,
      apoint  : word;
      lastnet : word;
      skip    : boolean;
      ni      : nodeinfo;
      d       : DB;
      List: TLister;

label again, NewStart;

  procedure ParseFlags;
  var s,s2 : string; 
      p    : Integer;
  begin
    flags:=0;
    s:=nb_data^.flags;
    while (s<>'') and (flags<orflags) do begin
      inc(flags);
      flaganz[flags]:=0;
      p:=blankposx(s);
      s2:=LeftStr(s,p-1);
      while s2<>'' do begin
        if flaganz[flags]<andflags then begin
          inc(flaganz[flags]);
          flag[flags,flaganz[flags]]:=','+LeftStr(s2,cposx(',',s2)-1)+',';
          end;
        delete(s2,1,cposx(',',s2));
        end;
      s:=trim(mid(s,p+1));
      end;
  end;

  procedure GetAddress(format:integer; var skip:boolean);
  var p  : Integer;
      fa : FidoAdr;
  begin
    skip:=false;
    case format of
      nlNodelist,
      nl4Dpointlist: if k='ZONE' then begin
                       azone:=nn; anet:=nn; anode:=nn;
                       skip:=(format=nl4Dpointlist);
                       end
                     else if (k='HOST') or (k='REGION') then begin
                       anet:=nn; anode:=0;
                       skip:=(format=nl4Dpointlist);
                       end
                     else if k='POINT' then
                       apoint:=nn
                     else begin
                       anode:=nn;
                       skip:=(format=nl4Dpointlist);
                       end;

      nlPoints24   : if k='HOST' then begin
                       ss:=LeftStr(ss,cposx(',',ss)-1);  { Nodeadresse isolieren }
                       p:=cpos('/',ss);
                       if p>0 then begin
                         anet:=ival(LeftStr(ss,p-1));
                         anode:=ival(mid(ss,p+1));
                         end;
                       skip:=true;
                       end
                     else if (k='') or (k='PVT') then
                       apoint:=nn;

      nlFDpointlist: if k='BOSS' then begin
                       ss:=LeftStr(ss,cposx(',',ss)-1);
                       splitfido(ss,fa,azone);
                       azone:=fa.zone; anet:=fa.net; anode:=fa.node;
                       skip:=true;
                       end
                     else if (k='') or (k='PVT') then
                       apoint:=nn;

      nlNode      : if (k='') or (k='PVT') then apoint:=nn;

    end;  { case }
  end;

  function ntest(ts:string):boolean;
  var p : byte;
  begin
    UpString(ts);
    p:=cposx(',',ss);
    ntest:=(ts='') or (pos(ts,LeftStr(ss,p-1))>0);
    delete(ss,1,p);
  end;

  function gets:string;
  var p : byte;
  begin
    p:=cposx(',',s);
    gets:=LeftStr(s,p-1);
    delete(s,1,p);
  end;

  function testphone:boolean;
  begin
    testphone:=(LeftStr(GetToken(ss,','),length(sphone))=sphone);
  end;

  function node_str:string;
  begin
    if apoint=0 then
      node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)
    else
      node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)+'.'+strs(apoint);
  end;

  function testaddress(var s:string):boolean;
  begin
    if s='' then
      testaddress:=true
    else
      testaddress:=(pos(s,node_str)>0);
  end;

  function testflags:boolean;
  var i,j   : integer;
      _s    : string;
  begin
    _s:=','+ss+',';
    if flags=0 then
      testflags:=true
    else begin
      i:=0;
      repeat
        inc(i);
        j:=1;
        while (j<=flaganz[i]) and (pos(flag[i,j],_s)>0) do
          inc(j);
      until (i>flags) or (j>flaganz[i]);
      testflags:=(i<=flags) and (j>flaganz[i]);
      end;
  end;

begin
  if not TestNodelist then exit;
  if not assigned(NB_Data) then begin
    new(NB_data);
    fillchar(NB_data^,sizeof(NB_Data^),0);
    NB_data^.fnl:=true; NB_data^.snl:=true;
    end;

again:
  dialog(ival(getres2(2131,0)),17,getres2(2131,1),x,y); { 'Nodelisten durchsuchen' }
  with NB_Data^ do begin
    maddstring(3,2,getres2(2131,2),sysop,30,30,'');     { 'Sysop' }
      mhnr(950);
      mnotrim;
    maddstring(3,3,getres2(2131,3),standort,30,30,'');  { 'Standort' }
      mnotrim;
    maddstring(3,4,getres2(2131,4),boxname,30,30,'');   { 'Boxname' }
      mnotrim;
    maddstring(3,6,getres2(2131,5),nodeadr,20,20,'0123456789:/.'); { 'Nodeadresse' }
    maddstring(3,7,getres2(2131,6),telefon,20,20,'0123456789-');   { 'Telefon' }
    maddstring(3,9,getres2(2131,8),flags,30,65,'>');  {MH: 40>65}{ unbedenklich } { 'Flags' }
    maddbool (3,11,getres2(2131,9),fnl);   { 'FidoNet-Nodeliste durchsuchen' }
    maddbool (3,12,getres2(2131,10),snl);  { 'sonstige Nodelisten durchsuchen' }
      mhnr(956);
    maddbool (3,13,getres2(2131,11),pl);   { 'Pointlisten durchsuchen' }
      mhnr(956);
    readmask(brk);
    closemask;
    end;
  if brk then begin
    closebox;
    freeres;
    exit;
    end;

  sphone:=NB_Data^.telefon;    { Telefon in Nodelistenformat konvertieren }
  if (IntVorwahl<>'') and (LeftStr(sphone,length(IntVorwahl))=IntVorwahl) then
    delete(sphone,1,length(IntVorwahl))
  else if (NatVorwahl<>'') and (LeftStr(sphone,length(NatVorwahl))=NatVorwahl) then
    sphone:=LeftStr(Vorwahl,cpos('-',vorwahl))+mid(sphone,length(NatVorwahl)+1);
  ParseFlags;

  getmem(buf,bufsize);
  attrtxt(col.coldialog);
  mwrt(x+2,y+14,getres2(2131,20));   { 'Datei:' }
  mwrt(x+2,y+15,getres2(2131,21));   { 'Netz:'  }
  mwrt(x+28,y+14,getres2(2131,22));  { 'Eintraege:' }
  mwrt(x+28,y+15,getres2(2131,23));  { 'passend:' }
  List := TLister.CreateWithOptions(2,ScreenWidth-2,10,11,0,'/NS/SB/NLR/DM/');  { Koordinaten beliebig }
  found:=0; n:=0;
  for i:=0 to NodeList.Count - 1 do
    with TNodeListItem(Nodelist.Items[i]),NB_Data^ do
      if ((fnl and (listfile='NODELIST.###') and (format=nlNodelist)) or
          (snl and (listfile<>'NODELIST.###') and (format=nlNodelist)) or
          (pl and (fformat<>nlNodelist))) and
         FileExists(FidoDir+NodeList.GetFilename(i))
      then begin
        attrtxt(col.coldiahigh);
        mwrt(x+10,y+14,forms(NodeList.GetFilename(i),12));
        assign(t,FidoDir+NodeList.GetFilename(i));
        settextbuf(t,buf^,bufsize);
        reset(t);
        if zone<>0 then azone:=zone   { Start-Nodeadresse setzen }
        else azone:=DefaultZone;
        apoint:=0;
        if fformat=nlNode then begin
          anet:=net; anode:=node;
          end
        else begin
          anet:=0; anode:=0;
          end;
        lastnet:=65535;
        while not eof(t) and not brk do begin
          readln(t,s);
          if (s<>'') and (s[1]<>';') and (cpos(',',s)>0) then begin
            inc(n);
            ss:=UpperCase(s);
            while cpos('_',ss)>0 do ss[cpos('_',ss)]:=' ';
            k:=LeftStr(ss,cpos(',',ss)-1);
            delete(ss,1,cpos(',',ss));
            if k<>'BOSS' then begin
              p:=cposx(',',ss);
              nn:=minmax(ival(LeftStr(ss,p-1)),0,65535);
              delete(ss,1,p);
              end;
            GetAddress(fformat,skip);     { akt. Adresse ermitteln }
            if anet<>lastnet then
              mwrt(x+10,y+15,forms(strs(azone)+':'+strs(anet),15));
            lastnet:=anet;
            if n mod 100=0 then
              mwrt(x+38,y+14,strsn(n,7));
            if not skip and
               ntest(boxname) and
               ntest(standort) and
               ntest(sysop) and
               testphone and
               testaddress(nodeadr) and
               testflags
            then begin
              inc(found);
              mwrt(x+38,y+15,strsn(found,7));
              gets; gets;  { Nodetyp und -nummer ueberlesen }
              while cpos('_',s)>0 do s[cpos('_',s)]:=' ';
              ni.boxname:=gets; ni.standort:=gets;
              ni.sysop:=gets; ni.telefon:=gets;
              with ni do
                List.AddLine(' '+forms(iifs(apoint<>0,sysop,boxname)+', '+standort,35)+
                      '  '+forms(node_str,17)+' '+FormFidoPhone(telefon));
              end;
            end;
          testbrk(brk);
          end;   { not eof }
        close(t);
        end;
  freemem(buf,bufsize);
  closebox;

  if found=0 then
  begin
    List.Free;
    if not brk then
    begin
      rfehler(2126);        { 'Es wurden keine passenden Eintraege gefunden.' }
      goto again;
    end;
  end;

  if found>0 then begin
    xp1.signal;
    h:=min(found+6,screenlines-6);
    selbox(76,h,'',x,y,true);
    dec(h,5);
    rdispx:=x+2; rdispy:=y+h+2;
    attrtxt(col.colselrahmen);
    mwrt(x,rdispy-2,hbar(76));
    List.SetSize(x+1,x+74,y+1,y+h-1);
    listboxcol(list);
    List.SetArrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'�');
    List.OnShowLines := ShowRQ;
    List.OnKeyPressed := listext;
    listmakros:=0;
    repeat
NewStart:                  { nach Break im AltN-Screen hier einspringen }
      pushhp(958);
      brk := List.Show;
      pophp;
      n:=0;
      if not brk then begin
        GetNodeInfo(copy(List.GetSelection,39,18),ni,2);
        if not ni.found then
          errsound
        else begin
          adr:=ni.sysop + ' @ ' +trim(copy(List.GetSelection,39,18));
          if ni.ispoint then s:=adr
          else s:=ni.boxname + ', ' +adr;
          pushhp(959);
          n:=ReadIt(max(length(s)-4,length(getres(2132)))+11,s,
             getres(2132),    { ' ^Nachricht , ^Request , ^Info , ^Zurueck ' }
             1,brk);
          pophp;
          brk:=false;
          end;
        end;
    until brk or (n=1) or (n=2) or (n=3);

      if n<>3 then begin  { Break aus AltN-Screen hier nicht zulassen }
        List.Free;
        closebox;
      end;

    if not brk then
      case n of
        1 : begin
              _keyboard(adr+keycr);
              dbOpen(d,BoxenFile,0);
              if dbRecCount(d)>1 then _keyboard(keyup+DefFidoBox+keycr);
              dbClose(d);
              msgdirect;
            end;
        2 : begin
              _keyboard(keycr);
              AutoCrash:=FidoRequest(trim(mid(adr,cpos('@',adr)+1)),'');
            end;

        3 : begin   { AltN-Screen aufrufen und Aka uebergeben }
              _keyboard(trim(copy(List.GetSelection,39,18))+keycr);
                NodeListSeek;          { AltN-Screen starten }
              goto newstart;           { Zurueck zur Liste    }
            end;

      end;
    end;

  freeres;
end;


procedure NodelistIndex;
begin
  if not TestNodelist then exit;
  CloseNodeindex;
  Nodelist.Open:=false;
  MakeNodelistIndex;
  OpenNodeindex(NodeIndexF);
end;

procedure SetShrinkNodelist;
var x,y   : Integer;
    brk   : boolean;
    s,s2  : string;
    ss    : string;
    p     : byte;
    l     : longint;
    res   : integer;
begin
  if not TestNodelist then exit;
  if NodeList.GetMainNodelist<0 then begin     //bestimmt den index der nodeliste
    rfehler(2125);    { 'Es ist keine Haupt-Fido-Nodeliste (NODELIST.###) eingebunden.' }
    exit;
    end;
  dialog(57,3,getres2(2104,1),x,y);   { 'Nodelist einschraenken' }
  s:=ShrinkNodes;
  maddstring(3,2,getres2(2104,2),s,35,100,'0123456789 :');  { 'Zonen/Regionen ' }
  readmask(brk);
  enddialog;
  if not brk then begin
    if s='' then begin
      ShrinkNodes:='';
      SaveConfig2;
      message(getres2(2104,3));  { 'Einschraenkungen geloescht - ggf. Nodeliste neu einlesen.' }
      wait(curoff);
      closebox;
      freeres;
      exit;
      end;
    s2:=s;
    s:=s+' ';
    repeat
      p:=cpos(' ',s);
      ss:=LeftStr(s,p-1);
      s:=trimleft(mid(s,p));
      p:=cpos(':',ss);
      if p=0 then
        val(ss,l,res)
      else begin
        val(LeftStr(ss,p-1),l,res);
        if res=0 then val(mid(ss,p+1),l,res);
        end;
    until (res<>0) or (s='');
    if res<>0 then
      fehler(getres2(2104,4))   { 'ungueltige Eingabe' }
    else begin
      ShrinkNodes:=s2;
      SaveConfig2;
      ShrinkNodelist(true);
      end;
    end;
  freeres;
end;


procedure ShrinkNodelist(indizieren:boolean);
var i : integer;
begin
  i:=NodeList.GetMainNodelist;
  if (i>0) and Nodelist.Open and (trim(ShrinkNodes)<>'') then
  begin
    xp1.shell(OwnPath + 'OPENXP NDIFF.EXE -s '+FidoDir+NodeList.GetFilename(i)+' '+ShrinkNodes,250,3);
    if errorlevel<>0 then
      rfehler(2114)   { 'Fehler beim Bearbeiten der Nodelist' }
    else
      if indizieren then
        MakeNodelistIndex;
  end;
end;


{ --- File Request --------------------------------------------------- }

function ReqTestNode(var s:string):boolean;  { s. auch XP7.getCrashBox }
var fa : FidoAdr;
    ni : NodeInfo;
begin
  if not IsNodeAddress(s) then begin
    fa.username:=s;
    GetNodeuserInfo(fa,ni);
    if ni.found then s:=MakeFidoAdr(fa,true);
    end
  else begin
    splitfido(s,fa,DefaultZone);
    if fa.node+fa.net=0 then begin
      errsound;
      ReqTestNode:=false;
      ni.found:=false;
      end
    else begin
      s:=MakeFidoAdr(fa,true);
      getNodeinfo(s,ni,2);
      end;
    end;
  if not ni.found then begin
    if multipos(':/',s) then
      rfehler(2115);   { 'unbekannte Adresse' }
    ReqTestNode:=false;
    end
  else begin
    fa.ispoint:=ni.ispoint;
    s:=MakeFidoAdr(fa,true);
    ReqtestNode:=true;
    end;
end;

procedure NodeSelProc(var cr:customrec);
var t   : text;
    s   : string;
    p   : byte;
    node: string;
    ni  : nodeinfo;
    anz : longint;
    fn  : string;
    sr  : tsearchrec;
    rc  : integer;
    List: TLister;

begin
  List := listbox(73,15,getres(iif(delfilelist,2105,2106)));
  assign(t,FileLists);          { 'Fileliste loeschen','File Request' }
  reset(t);
  anz:=0;
  KeepNodeindexOpen;
  while not eof(t) do begin
    readln(t,s); s:=trim(s);
    p:=cPos('=',s);
    if (s<>'') and (s[1]<>'#') and (s[1]<>';') and (p>0) then begin
      fn:=mid(s,p+1);
      rc:= findfirst(FidoDir+fn,faAnyFile,sr);
      if (rc=0) and (sr.size>0) then begin
        node:=LeftStr(s,p-1);
        GetNodeinfo(node,ni,1);
        inc(anz);
        if cpos('.',fn)>0 then
          fn:=LeftStr(fn,cpos('.',fn)-1);
          List.AddLine(' '+forms(node,14)+' '+
              forms(iifs(ni.found,ni.boxname+', '+ni.standort,'???'),32)+
              '  '+FormatDateTime('mm/yy', FileDateToDateTime(sr.time))+'  '+forms(fn,9)+strsn(sr.size div 1024,5)+'k ');
      end; // if rc...
      FindClose(sr);
    end; // if
  end; // while
  KeepNodeindexClosed;
  close(t);
  if anz>0 then begin
    cr.brk := List.Show;
    if not cr.brk then begin
      cr.s:=trim(LeftStr(List.GetSelection,15));
      if not delfilelist and (copy(List.GetSelection,17,4)<>'??? ') then
        keyboard(keycr+keyf2);
      end;
    end
  else
    cr.brk:=true;
  List.Free;
  closebox;
end;

procedure ShrinkPointToNode(var fa:FidoAdr; var ni:NodeInfo);
var ni2 : NodeInfo;
begin
  if fa.ispoint then begin
    getNodeinfo(MakeFidoadr(fa,false),ni2,1);
    if ni2.found and ((ni2.telefon=ni.telefon) or (pos('unpublished',LowerCase(ni.telefon))>0))
    then begin
      fa.ispoint:=false;
      ni:=ni2;
      end;
    end;
end;


procedure GetFAddress(request:boolean; txt:string; var fa:FidoAdr;
                      var ni:NodeInfo; var brk:boolean; var x,y: Integer);
var node    : string;
    xx,yy,i : Integer;
    t       : taste;
begin
  dialog(38,3,txt,x,y);
  if fa.net+fa.node=0 then
    node:=DefFidoBox
  else
    node:=MakeFidoAdr(fa,true);
  maddstring(3,2,getres(2107),node,20,25,''); mhnr(730);  { 'Node/Name ' }
  if request and FileExists(FileLists) and FileExists(FidoDir+'*' + extFl) then 
  begin
    mappcustomsel(NodeSelproc,false); mselhnr(85);
    DelFilelist:=false;
    end;
  msetvfunc(ReqTestNode);
  readmask(brk);
  enddialog;
  if not brk then begin
    if IsNodeaddress(node) then begin
      SplitFido(node,fa,DefaultZone);
      GetNodeinfo(node,ni,1);
      end
    else begin
      fa.username:=node;
      getNodeUserInfo(fa,ni);
      end;
    if not ni.found then begin      { duerfte eigentlich nicht passieren.. }
      rfehler(2116);   { 'unbekannte Nodeadresse' }
      brk:=true;
      end
    else begin
      ShrinkPointToNode(fa,ni);
      node:=MakeFidoAdr(fa,true);
      if request and (ni.request and rfWaZOO=0) then
        brk:=not ReadJN(getres(2108),true);  { 'Laut Nodelist kein Request bei dieser Box moeglich - trotzdem versuchen' }
      if not brk and (ni.flags and nfCM=0) and (pos('Kratzenberg',ni.sysop)>0)
      then begin
        msgbox(ival(getres2(2126,0)),res2anz(2126)+4,_hinweis_,xx,yy);
        for i:=1 to res2anz(2126)-1 do
          mwrt(xx+3,yy+1+i,getres2(2126,i));
        mwrt(xx+3,yy+res2anz(2126)+2,getres(12));
        errsound;
        get(t,curon);
        closebox;
        end;
      end;
    end;
  if not brk then begin
    dialog(65,iif(request,10,8),txt,x,y);
    maddtext(3,2,getres2(2109,1),0);   { 'Box' }
    maddtext(3,3,getres2(2109,2),0);   { 'Nummer' }
    maddtext(3,4,getres2(2109,3),0);   { 'Flags' }
    maddtext(3,5,getres2(2109,4),0);   { 'Status' }
    freeres;
    attrtxt(col.coldiahigh);
    maddtext(12,2,iifs(ni.ispoint,ni.sysop,leftStr(ni.boxname+', '+ni.standort,
                                            48-length(node)))+' ('+node+')',
                col.coldiahigh);
    maddtext(12,3,ni.telefon,col.coldiahigh);

    {modem:=''; }
    with ni do begin
    { if flags and nfHST<>0 then modem:='HST';
      if flags and nfHST16<>0 then modem:=modem+' / HST-16.8';
      if flags and nfV32b<>0 then modem:=modem+' / V.32bis'
      else if flags and nfV32<>0 then modem:=modem+' / V.32';
      if flags and nfPEP<>0 then modem:=modem+' / PEP';
      if flags and nfZYXEL<>0 then modem:=modem+' / ZyXEL-16.8';
      if flags and nfISDN<>0 then modem:=modem+' / ISDN';
      if flags and nfTerbo<>0 then modem:=modem+' / V32 terbo';
      if flags and nfVFC<>0 then modem:=modem+' / V.Fast Class';
      if flags and nfV34<>0 then modem:=modem+' / V.34';

      if FirstChar(modem)=' ' then delete(modem,1,3);
      if modem='' then modem:=strs(baud);
      maddtext(12,4,modem,col.coldiahigh); }


      { Fix: Flagzeile zu lang: gek�rzt und E-Mail rausschneiden }
      maddtext(12, 4, copy(MailString(FFlags, True), 1, 50), col.coldiahigh);
      maddtext(12, 5, status, col.coldiahigh);

      end;
    end;
end;

function TestNodelist:boolean;
begin
  if not Nodelist.Open then
    rfehler(2102);   { 'keine Nodelist aktiv' }
  TestNodelist:=Nodelist.Open;
end;

function testDefbox:boolean;
begin
  if DefFidoBox='' then
    rfehler(2118);   { 'keine Fido-Stammbox gewaehlt (Edit/Boxen)' }
  testDefbox:=(DefFidobox<>'');
end;

function FidoFilename(const fa:FidoAdr):string;
begin
  FidoFilename:=FileUpperCase(hex(fa.net,4)+hex(fa.node,4));
end;

function FidoAppendRequestfile(var fa:FidoAdr):string;
var files : string;
    _file : string[30];
    t     : text;
    p,p2  : byte;
    ff    : string[12];
begin
  files:='';
  GetReqFiles(MakeFidoAdr(fa,true),files);
  TrimFirstChar(files, '>');
  ff:=FidoFilename(fa)+'.REQ';
  if files<>'' then begin
    assign(t,ff);
    if existf(t) then append(t)
    else rewrite(t);
    files:=files+' ';
    repeat
      p:=cpos(' ',files);
      if p>0 then begin
        _file:=trim(LeftStr(files,p));
        files:=trimleft(mid(files,p));
        p2:=cpos('/',_file);
        if p2=0 then writeln(t,_file)
        else writeln(t,LeftStr(_file,p2-1)+' !'+mid(_file,p2+1));
        end;
    until p=0;
    close(t);
    end;
  if FileExists(ff) then FidoAppendRequestfile:=ff
  else FidoAppendRequestfile:='';
end;

function CrashFile(adr:string):string;
var fa : FidoAdr;
begin
  splitfido(adr,fa,DefaultZone);
  CrashFile:=FileUpperCase(FidoFilename(fa)+'.cp');
end;

procedure GetReqFiles(adr:string; var files:string);
var

    t     : text;
    s     : string;
begin
  assign(t,ReqDat);
  if existf(t) then begin
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if s=adr then
        repeat
          readln(t,s);
          TrimFirstChar(s, '>');
          if s<>CrashID then files:=files+' '+s;
        until s=''
      else
        repeat readln(t,s) until s='';
      end;
    close(t);
    files:=trim(files);
    end;
end;


function fstestmark(const s:string; block:boolean):boolean;
begin
  if (LeftStr(s,2)>'  ') and ((s<#176) or (s>#223)) then
    fstestmark:=true
  else begin
    if not block then errsound;
    fstestmark:=false;
    end;
end;

procedure FileSelProc(var cr:customrec);
var s   : string;
    p   : scrptr;
  List: TLister;
begin
  List := TLister.CreateWithOptions(1,ScreenWidth,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/APGD/');
  rmessage(2110);   { 'Lade Fileliste ...' }
  List.ReadFromFile(FreqLst,0);
  closebox;
  List.OnTestMark := fstestmark;
  sichern(p);
  repeat
    cr.brk := List.Show;
  until not cr.brk or (List.SelCount=0) or ReadJN(getres(2123),true);   { 'Auswahl verwerfen' }
  holen(p);
  if not cr.brk then begin
    s:= List.FirstMarked;
    while s<>#0 do begin
      s:=trim(s);
      if s[1]>='�' then s:=trim(mid(s,2));
      if blankpos(s)>0 then
        s:=LeftStr(s,blankpos(s)-1);
      cr.s:=cr.s+' '+s;
      s:= List.NextMarked;
      end;
    cr.s:=trim(cr.s);
    end;
  List.Free;
end;

function GetFilelist(var fa:fidoadr):string;
var t     : text;
    s     : string[80];
    p     : byte;
    found : boolean;
    node  : string[20];
begin
  GetFilelist:='';
  assign(t,FileLists);
  if existf(t) then begin
    node:=MakeFidoadr(fa,true);
    reset(t);
    found:=false;
    while not found and not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      p:=cpos('=',s);
      if (s<>'') and (s[1]<>'#') and (s[1]<>';') and (p>0) then
        if LeftStr(s,p-1)=node then begin
          found:=true;
          delete(s,1,p);
          if cpos(' ',s)>0 then s:=LeftStr(s,cpos(' ',s)-1);
          getFilelist:=FidoDir+s;
          end;
      end;
    close(t);
    end;
end;

function FidoRequest(node,files:string):string;
var brk   : boolean;
    x,y   : Integer;
    fa    : FidoAdr;
    ni    : NodeInfo;
    atonce: boolean;
    doreq : boolean;
begin
  fidorequest:='';
  if not TestNodelist or not TestDefbox then exit;
  if node='' then fillchar(fa,sizeof(fa),0)
  else SplitFido(node,FA,DefaultZone);
  getFAddress(true,getres(2111),fa,ni,brk,x,y);   { 'File-Request' }
  if brk then exit;
  node:=MakeFidoAdr(fa,true);
  getReqFiles(node,files);
  TrimFirstChar(files, '>');
  atonce:=false;
  maddstring(3,7,getres2(2112,1),files,50,254,'>'); mhnr(735);  { 'Dateien ' }
  freqlst:=GetFilelist(fa);
  if Fileexists(freqlst) then
    MappCustomsel(FileSelProc,false);
  maddbool(4,9,getres2(2112,2),atonce);   { ' sofort starten' }
  readmask(brk);
  enddialog;
  doreq:=(files<>'');
  if not brk then begin
    if not doreq then begin
      message(getres2(2112,3));  { 'Request-Anforderung loeschen ...' }
      SetRequest(node,'');
      end
    else begin
      message(getres2(2112,4));   { 'Request-Anforderung speichern ...' }
      SetRequest(node,files);
      end;
    if not atonce then mdelay(500);
    closebox;
    if atonce and doreq then
      fidorequest:=makeFidoAdr(fa,true);
    end;
end;


procedure SetCrash(adr:string; insert:boolean);
var t1,t2   : text;
    s,files : string;
    ni      : NodeInfo;
    fa      : FidoAdr;
begin
  SplitFido(adr,fa,DefaultZone);
  if fa.ispoint then begin     { evtl aus Pointadresse Nodeadresse machen }
    GetNodeinfo(adr,ni,1);
    ShrinkPointToNode(fa,ni);
    if not fa.ispoint then
      adr:=MakeFidoAdr(fa,false);
    end;

  files:='';
  assign(t2,TempFile(''));
  rewrite(t2);
  if Fileexists(ReqDat) then begin
    assign(t1,ReqDat);
    reset(t1);
    while not eof(t1) do begin
      readln(t1,s); s:=trim(s);
      if s=adr then
        repeat
          readln(t1,s); s:=trim(s);
          if (s<>'') and (s<>CrashID) then files:=s;
        until s=''
      else begin
        writeln(t2,s);
        repeat
          readln(t1,s); s:=trim(s);
          writeln(t2,s);
        until s='';
        end;
      end;
    close(t1);
    erase(t1);   { alte ReqDat loeschen }
    end;
  if insert or (files<>'') then begin
    writeln(t2,adr);
    if insert then
      writeln(t2,CrashID);
    if files<>'' then writeln(t2,files);
    writeln(t2);
    end;
  close(t2);
  rename(t2,ReqDat);
end;


procedure SetRequest(const adr,files:string);   { '' -> Request loeschen }
var t1,t2   : text;
    s       : string;
    crash   : boolean;
begin
  crash:=false;
  assign(t2,TempFile(''));
  rewrite(t2);
  if FileExists(ReqDat) then begin
    assign(t1,ReqDat);
    reset(t1);
    while not eof(t1) do begin
      readln(t1,s); s:=trim(s);
      if s=adr then
        repeat
          readln(t1,s); s:=trim(s);
          if s=CrashID then crash:=true;
        until s=''
      else begin
        writeln(t2,s);
        repeat
          readln(t1,s); s:=trim(s);
          writeln(t2,s);
        until s='';
        end;
      end;
    close(t1);
    erase(t1);   { alte ReqDat loeschen }
    end;
  if crash or (files<>'') then begin
    writeln(t2,adr);
    if crash then
      writeln(t2,CrashID);
    if files<>'' then writeln(t2,files);
    writeln(t2);
    end;
  close(t2);
  rename(t2,ReqDat);
end;


{ nl_phone: Nummer im 'Originalzustand', max. 30 Zeichen }

function FidoPhone(var fa:FidoAdr; var nl_phone:string):string;
var ni : NodeInfo;
begin
  GetNodeinfo(MakeFidoAdr(fa,true),ni,2);
  if not ni.found then begin
    nl_phone:='0-0';
    FidoPhone:='???';
    end
  else begin
    nl_phone:=ni.telefon;
    FidoPhone:=FormFidoPhone(ni.telefon);
    end;
end;


{ --- File-Listen --------------------------------------------------- }

procedure ReadFidolist;
var fn     : string;
    brk    : boolean;
    x,y    : Integer;
    node   : string;
    fi,fi2 : string;
    ni     : nodeinfo;
    p      : byte;
    ar     : archrec;
    copied : boolean;
    fa     : FidoAdr;
    arc    : integer;
    useclip: boolean;

label ende;

  function overwrite(fn:string):boolean;
  begin
    overwrite:=ReadJN(LeftStr(fn,40)+getres(2113),true);  { ' bereits vorhanden - ueberschreiben' }
  end;

  function filetest(docopy:boolean; size:Int64; path:string; fi:string):boolean;
  var
    p       : Integer;
{$IFDEF Linux}
{$IFDEF Kylix}
    fs : TStatFs;
  begin
    statfs(PChar(path),fs);
    if ((int64(fs.f_bavail)*int64(fs.f_bsize))<=size)
{$ELSE}
    fs : statfs;
  begin
    fsstat(path,fs);
    if ((int64(fs.bavail)*int64(fs.bsize))<=size)
{$ENDIF}

{$ELSE}
    driveNr : Integer;
  begin
     driveNr := ord(FirstChar(Path))-64;

    if (diskfree(driveNr)<=size)
{$ENDIF}
                                 and fehlfunc(getres(2114)) then  { 'zu wenig Platz' }
      filetest:=false
    else if docopy and FileExists(path+fi) and not overwrite(path+fi) then
      filetest:=false
    else if ExtractFileExt(fi) = extFl then
      filetest:=true
    else 
    begin
      fi := ChangeFileExt(fi, extFl);
      filetest:=(not FileExists(path+fi) or overwrite(path+fi));
    end;
  end;

  procedure UpdateReqdat;
  var t1,t2 : text;
      s     : string[80];
      p     : byte;
  begin
    rmessage(2115);   { 'Eintrag in Filelisten-Datei..' }
    assign(t2,FidoDir+'reqdat.$$$');
    rewrite(t2);
    writeln(t2,'# ',getres(2116));   { Verzeichnis der Request-Filelisten }
    writeln(t2);
    assign(t1,FileLists);
    if existf(t1) then begin
      reset(t1);
      while not eof(t1) do begin
        readln(t1,s);
        s:=trim(s);
        if (s<>'') and (s[1]<>'#') and (s[1]<>';') then begin
          p:=cpos('=',s);
          if (p>0) and (LeftStr(s,p-1)<>node) and (mid(s,p+1)<>fi2) and
             FileExists(FidoDir+mid(s,p+1)) then   { falsche Eintraege killen }
            writeln(t2,s);
          end;
        end;
      close(t1);
      erase(t1);
      end;
    writeln(t2,node,'=',fi2);
    close(t2);
    rename(t2,FileLists);
    mdelay(200);
    closebox;
  end;

begin
  fn:=FilePath+WildCard;
  useclip:=false;
  if not ReadFilename(getres2(2117,1),fn,true,useclip) or  { 'Fileliste einlesen' }
     (not FileExists(fn) and fehlfunc(getres2(2117,2))) then  { 'Datei nicht vorhanden' }
    goto ende;
  fn:=FileUpperCase(ExpandFileName(fn));
  fi:=ExtractFilename(fn);
  p:=cpos('.',fi);
  if p=0 then fn:=fn+'.';
  if (p>0) and (ival(LeftStr(fi,p-1))>0) then 
  begin
    fillchar(fa,sizeof(fa),0);
    if not Nodelist.Open then
      node:=''
    else begin
      node:=strs(DefaultZone)+':'+strs(ival(LeftStr(fi,4)))+'/'+strs(ival(copy(fi,5,4)));
      getNodeinfo(node,ni,1);
      if not ni.found then
        if FindFidoAddress(fn,fa) then
          node:=MakeFidoAdr(fa,false)
        else
          if ival(FirstChar(fi))>0 then
            node:=FirstChar(fi)+':'+strs(ival(copy(fi,2,3)))+'/'+strs(ival(copy(fi,5,4)))
          else
            node:='';
      end;
    end
  else
    if FindFidoAddress(fn,fa) then
      node:=MakeFidoAdr(fa,false)
    else
      node:='';
  dialog(40,5,getres2(2117,1),x,y);
  maddtext(3,2,getres2(2117,3),0);  { 'Datei' }
  maddtext(10,2,fi,col.coldiahigh);
  maddstring(3,4,getres2(2117,4),node,15,15,nodechar);  { 'Node  ' }
  readmask(brk);
  enddialog;
  if brk or (node='') then goto ende;
  splitfido(node,fa,DefaultZone);
  node:=MakeFidoAdr(fa,true);

  arc:=ArcType(fn);
  if arc<>0 then begin          { gepackte Fileliste }
    OpenArchive(fn,arc,ar);
    if stricmp(ar.name,'FILE_ID.DIZ') then
      ArcNext(ar);
    closearchive(ar);
    if (ar.name='') and fehlfunc(getres2(2117,5)) then goto ende;  { 'Fehler in Archivdatei' }
    if not FileTest(true,ar.orgsize,FidoPath,ar.name) then     goto ende;
    SafeDeleteFile(FidoPath+ar.name);
    if not UniExtract(fn,FidoPath,ar.name) then                goto ende;
    fi:=ar.name;
    copied:=true;
    end
  else if ExtractFilePath(fn)=FidoPath then begin   { ungepackt, in FIDO\ }
    if RightStr(fn,3)<> extFl then
      if not FileTest(false,0,FidoPath,fi) then goto ende;
    copied:=false;
    end
  else begin                                    { ungepackt, woanders }
    if not FileTest(true,_filesize(fn),FidoPath,fi) then goto ende;
    message(getres2(2117,6));  { 'Kopiere Fileliste..' }
    if not filecopy(fn,FidoPath+fi) then begin
      closebox;
      fehler(getres2(2117,7));  { 'Fehler beim Kopieren' }
      goto ende;
      end;
    closebox;
    copied:=true;
    end;
                          
  if ExtractFileExt(fi) <> extFl then 
  begin
    fi2:= ChangeFileExt(fi, extFl);
    SafeDeleteFile(FidoPath+fi2);
    if not RenameFile(FidoPath+fi,FidoPath+fi2) and
       fehlfunc(getres2(2117,8)) then   { 'Fehler beim Umbenennen' }
      goto ende;
    end
  else
    fi2:=fi;
  UpdateReqdat;
  truncstr(fidolastseek,1);
  SaveConfig2;
  if copied and ReadJN(reps(getres2(2117,9),fn),true) then begin  { '%s loeschen' }
    message(reps(getres2(2117,10),fn));   { 'Loesche %s ...' }
    _era(fn);
    mdelay(300);
    closebox;
    end;
ende:
  freeres;
end;

procedure DelFidolist;
var cr    : CustomRec;
    t1,t2 : text;
    s     : string[80];
    p     : byte;
    nn    : longint;
    comment: boolean;
begin
  if not FileExists(FileLists) or not FileExists(FidoDir+'*' +extFl) then
    rfehler(2119)   { 'keine Filelisten vorhanden' }
  else begin
    cr.s:='';
    DelFilelist:=true;
    pushhp(85);
    NodeSelProc(cr);
    pophp;
    if cr.brk then exit;
    if ReadJN(reps(getres(2118),cr.s),false) then begin   { 'Fileliste fuer %s loeschen' }
      rmessage(2119);   { 'Loesche Fileliste ...' }
      nn:=0;
      assign(t1,FileLists); reset(t1);
      assign(t2,FidoDir+'request.$$$'); rewrite(t2);
      while not eof(t1) do begin
        readln(t1,s);
        s:=trim(s);
        p:=cpos('=',s);
        comment:=(s='') or (s[1]='#') or (s[1]=';') or (p=0);
        if comment or (LeftStr(s,p-1)<>cr.s) then begin
          writeln(t2,s);
          if not comment then inc(nn);
          end
        else
          SafeDeleteFile(FidoDir+mid(s,p+1));
        end;
      close(t1); close(t2);
      erase(t1);
      if nn>0 then
        rename(t2,FileLists)
      else
        erase(t2);    { keine Listen uebrig }
      mdelay(200);
      closebox;
      end;
    end;
end;



{ File Suche in Fido Requstlisten mit bis zu fuenf Suchbegriffen  }
function FidoSeekfile:string;
  const
    seekfile         = 'fileseek.dat';
    maxbuf           = 20;
    SearchStr_maxIdx = 4;
    iCase : boolean = true;                 { Gross-Kleinschreibung  }
    wCase : boolean = true;                 { Suche nur ganze Worte }
    _sep             = '/&';                { der Seperator 2-Beyte }

  var
    seek, oldseek    : string;
    sNodInf,sZeile   : string;
    sFlistName       : string;
    sa               : array[0..maxbuf] of string;
    searchStr        : array[0..SearchStr_maxIdx] of string;
    x,y              : Integer;
    brk              : boolean;
    pFileListCfg     : text;
    pOutput          : text;
    pFileListe       : text;
    anz_FileFound, p : longint;
    tb               : pointer;
    scp              : scrptr;
    tbs              : word;
    ni               : ^nodeinfo;
    files            : string;
    len              : byte;
    anz_searchStr    : integer;    { Anzahl der besetzten Suchstrings }
    List: TLister;
    label
       ende;

 {true, wenn der Substring in dem zu durchsuchenden String vollstaendig enthalten ist }
 { _pos= die Startposition des Vorhandenen SubStrings!!! }
  function fInStr( var SubStr: string; var s : string; var _pos: integer) :boolean;
  begin
    fInStr:=false;                      { Funktionswert false initialisieren }
    if  _pos > 1  then                  { String nicht zu Beginn des Suchstringes und }
      if s[ _pos-1] <>' '  then exit;   { Zeichen vor dem Stringbegin <> space}

    if _pos+length(Substr) <= length(s) then  { Suchbegriff nicht am Ende des Stringes }
      if (s[_pos+length(Substr)] <> ' ') then exit;

    { Alle Tests bestanden }
    fInStr:=true;
  end;
  procedure pTestWriteln(const s:string);
  begin
    writeln(pOutput,s);
    if IOResult<>0 then begin
      fehler(ioerror(ioresult,getres2(2120,1)));   { 'Schreibfehler' }
      brk:=true;
     end;
  end;

  procedure pTabExpand(var ss:string);
   const TAB = #9;
   var p : byte;
  begin
    p:=cpos(TAB,ss);
    while p>0 do begin
      delete(ss,p,1);
      insert(sp(8-(p-1) mod 8),ss,p);
      p:=cpos(TAB,sNodInf);
      end;
  end;

  {teste den Suchstring auf Korrektheit}
  procedure pTestSeekStr( var fidolastseek: string );
  begin
    if ( pos(_sep,fidolastseek) = 1 ) then
    begin
      fidolastseek:=copy(fidolastseek,3,500 );
    end;
  end;

  {zerlege Suchstring }
  procedure pInitSearchStr;
  var
    _pos    : integer;
    tempStr : string[50];
  begin
    anz_searchStr:=0;
    tempStr:=seek;
    while ( tempStr <> '') and ( anz_searchStr <= SearchStr_maxIdx) Do
    begin
      searchStr[anz_searchStr]:=tempStr;   { (Rest)-String -> Suchbegiff }
      inc(anz_searchStr);                  { noch ein Suchbegriff }
      _pos:=pos(_sep, tempStr);
      if _pos=0 then exit;                 { kein Seperator mehr gefunden -> raus}
      searchStr[anz_searchStr-1]:=copy(searchStr[anz_searchStr-1],1,_pos-1); {String abschneiden}
      tempStr:=copy(tempStr,_pos+length(_sep),500); {String abschneiden}
    end;
  end;

  {sind die Suchbegriffe im Dateibeschreibungsblock vorhanden }
  procedure pTestBlock( BlockLength :integer );
    var
      n, nn, n2, _pos  : integer;
      test : boolean;
      sZeile           : string;
      sSub             : string;
  begin
    test := false;
    for n2:=0 to anz_searchStr - 1 Do     { nach alle Begriffen durchsuchen }
    begin
      n:=0;
      test:=false;
      if  icase  then  sSub:=UpperCase(searchStr[n2])
      else             sSub:=searchStr[n2];
      { alle Zeilen auf ein Suchbegriff durchsuchen durchtesten }
      while ( n < BlockLength ) and (test=false)  do
      begin
        if  icase then sZeile:=UpperCase(sa[n])
        else           sZeile:=sa[n];
        _pos:=pos( sSub, sZeile );
        if _pos  > 0 then   { SubSting in der Beschreibung vorhanden}
        begin
          if wCase = false then  test:=true         { ignore, Ganzes Wort}
          else test:=fInStr( sSub, sZeile, _pos );  { noch auf ganzes Wort testen }
        end;
        inc(n);                                     { naechste Zeile }
      end; { while ( n < apos)   do } { alle Zeilen durchtesten }
      {Begriff im Bolck nicht gefunden, dann raus }
      if test =false then exit;
    end;    { for n2:=0 to anz_searchStr - 1 Do }

    if test = true then
    begin
      { schreibe 1. Zeile und die node Nummer }
      pTestWriteln(forms(sa[0],80)+LeftStr(sNodInf,p-1));
      nn:=1;
      while ( nn < BlockLength) and ( brk = false )   do
      begin
        pTestWriteln(sa[nn] ); {Zeile des Blockes speichern }
        inc(nn);
      end;
      inc(anz_FileFound);      {Erhoehe Anzahl der gefunden  }
      mwrt(x+13,y+3,strsn(anz_FileFound,5));
    end;
    if KeyPressed then
       brk:= ReadKey = #27;
  end;      { function testBlock :boolean;}

  procedure pBearbeiteFileListe;
  var
     apos           : integer;
     beginSafe      : boolean;
  begin
    attrtxt(col.colmboxhigh);
    mwrt(x+13,y+2,forms(mid(sNodInf,p+1),12));  { Dateiname anzeigen }
    getNodeinfo(LeftStr(sNodInf,p-1),ni^,1);
    settextbuf(pFileListe,tb^,tbs);
    reset(pFileListe);
    begin                                  { write header}
      sNodInf:=LeftStr(sNodInf,p-1)+' ';      { s:= 2:244/1278 }
      if ni^.found then sNodInf:=sNodInf+'('+ni^.boxname+', '+ni^.standort+', '+sFlistName  +')'
      else sNodInf:=sNodInf+'(??, '+sFlistName  +')';         { s:= 2:244/1278 (C-Box, Frankfurt, 02441278.FL}
      writeln(pOutput,' ',' '+sNodInf);
      writeln(pOutput,' ',' '+typeform.dup(length(sNodInf),'�'));
      writeln(pOutput);
    end;
    apos:=0;                                    { Zeilenzaehler ungueltig setzen}
    beginSafe:=false;
    while not eof(pFileListe) and (not brk) do               {'FIDO\22441278.fl'}
    begin
      readln(pFileListe,sZeile);               {lese Zeile aus der Fileliste }
      pTabExpand(sZeile);
      { ist gelesene Zeile eine headerzeile }
      if ( FirstChar(sZeile) >' ') and( sZeile <> '') and (( FirstChar(sZeile) <#176)or( FirstChar( sZeile) >#223)) then
      begin
        beginSafe:=true;
        if( apos > 0 ) then { wurde ein Block bereits zwischengespeichert?}
        begin
          { Block testen und ggf. speichern }
          pTestBlock( apos );
          apos:=0;  { setze ZeilenZaehler auf Start }
        end;
      end; { if (ss[1]>' ') and ( (ss[1]<#176)or( ss[1]>#223) ) then }
      if ( apos <= maxbuf) and  ( beginSafe ) then { im gueltigen Bereich}
      begin
        sa[apos]:=sZeile;  { speicher String in Array }
        inc(apos);
      end;
    end; { while not eof(pFileListe^) do begin}
    close(pFileListe);
  end;


begin       { FidoSeekfile:string;************************ }
  FidoSeekfile:='';
  anz_FileFound:=0;                     { Anzahl der gefunden Dateien =0 }
  if not FileExists(FileLists) or not FileExists(FidoDir+'*' + extFl) then
  begin
    fehler(getres2(2120,2));            { 'keine Filelisten vorhanden' }
    goto ende;
  end;
  oldseek:=fidolastseek;                { icase+letzten Suchstring  }
  fidolastseek:=mid(fidolastseek,3);    { icase extrahieren }
  iCase:=(FirstChar(oldseek)='J');         { icase     }
  wCase:=(copy(oldseek,2,1)='J');       { Wcase     }
  {Anzeige initilisieren }
  dialog(57,6,getres2(2120,3),x,y);     { 'Dateien suchen' }
  if LastChar(fidolastseek)=#27 then
     fidolastseek:=LeftStr(fidolastseek, length(fidolastseek)-1);
  maddstring(3,2,getres2(2120,4),fidolastseek,40,40,'');   { 'Suchbegriff ' }
  maddbool(3,4,getres2(2120, 5),iCase); { 'Schreibweise ignorieren' }
  maddbool(3,5,getres2(2120,11),wCase); { 'Nur ganze Woerter suchen }
  readmask(brk);                        {  Dialog anzeigen/ausfuehren }
  enddialog;
  pTestSeekStr( fidolastseek);           {teste den String auf Korrektheit}
  fidolastseek:=iifc(icase,'J','N')+iifc(Wcase,'J','N')+fidolastseek; {Ja or Nein + Suchbegriff }

  if brk or (fidolastseek='') then goto ende;

  if icase then seek:=UpperCase(mid(fidolastseek,3)) {seek, enthaelt nun den suchstring}
  else          seek:=mid(fidolastseek,3);
  pInitSearchStr;                       { zerlege Suchstring}

  if fidolastseek <> oldseek  then
  begin
    msgbox(30,6,getres2(2120,6),x,y);   { 'Suchen ..' }
    mwrt(x+3,y+2,getres2(2120,7));      { 'Datei' }
    mwrt(x+3,y+3,getres2(2120,8));      { 'gefunden' }
    new(ni);
    tbs:=16384;
    getmem(tb,tbs);
    assign(pOutput,seekfile);
    rewrite(pOutput);
    len:=length(getres2(2120,9))+3;
    writeln(pOutput,' ',' '+typeform.dup(len+length(seek),'�'));
    writeln(pOutput,' ',' '+getres2(2120,9),' "',mid(fidolastseek,3),'"');   { 'Dateisuche nach' .. }
    writeln(pOutput,' ',' '+typeform.dup(len+length(seek),'�'));
    writeln(pOutput);
    assign(pFileListCfg,FileLists);
    reset(pFileListCfg);
    while ( not eof(pFileListCfg)) and (not brk) do       {noch Eintraege in der cfg.datei}
    begin
      readln(pFileListCfg,sNodInf); sNodInf:=trim(sNodInf);
      p:=cpos('=',sNodInf);
      if (sNodInf<>'') and (FirstChar(sNodInf)<>'#') and (FirstChar(sNodInf) <>';') and (p>0) then
      begin
        sFlistName:= UpperCase(mid(sNodInf,p+1));        { Name der Fileliste z.B: 22441278.fl}
        assign(pFileListe,FidoDir+sFlistName  );    { pFileListe='FIDO\22441278.fl'}
        if existf(pFileListe) then
          pBearbeiteFileListe;
      end;
    end;
    saveconfig2;
    xp1.signal;
    close(pFileListCfg);
    close(pOutput);
    if brk then
    begin
      erase(pOutput);
      fidolastseek:=fidolastseek+#27;
    end;
    if IoResult<>0 then;
    closebox;
    freemem(tb,tbs);
    dispose(ni);
  end;               { fidolastseek<>oldseek }
  if not brk then    { gefundene Dateien Listen und ggf. requesten }
  begin
    List := TLister.CreateWithOptions(1,ScreenWidth,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/NLR/APGD/');
    List.ReadFromFile(seekfile,0);
    List.OnTestMark := fstestmark;
    List.OnKeypressed := listext;                { 'D' + 'W' }
    llh:=false; listmakros:=0;
    sichern(scp);
    pushhp(83);
    brk := List.Show;
    pophp;
    holen(scp);
    files:='';
    sNodInf:= mid(List.FirstMarked,81);;
    sZeile:=List.FirstMarked;
    while (sZeile<>#0) do
    begin
      if mid(sZeile,81)<>sNodInf then  { Request bei zwei Boxen! }
      begin
        fehler(getres2(2120,10));      { 'kein gleichzeitiger Request bei mehreren Boxen moeglich' }
        sZeile:=#0;                    { Schleife abbrechen }
      end
      else
      begin
       files:=files+' '+trim(LeftStr(sZeile,12));
       sZeile:=List.NextMarked;
      end;
    end;
    files:=trim(files);
    if files<>'' then
    begin
      keyboard(keycr);
      FidoSeekfile:=FidoRequest(sNodInf,files);
    end;
    List.Free;
  end;
ende:
  freeres;
end;

function FidoIsISDN(const fa:FidoAdr):boolean;
var ni : NodeInfo;
begin
  GetNodeInfo(MakeFidoAdr(fa,true),ni,2);
  result:=(pos('ISDN',ni.fflags)>0) or
          (pos('X75',ni.fflags)>0);
end;

function FidoIsBinkP(var fa:FidoAdr):string;
var ni : NodeInfo;
begin
  GetNodeInfo(MakeFidoAdr(fa,true),ni,2);
  if pos('IBN',ni.fflags)>0 then
    result:=ni.boxname
  else
    result:='';
end;


{ In Textfile nach erster brauchbarer Nodeadresse suchen }

function FindFidoAddress(const fn:string; var fa:FidoAdr):boolean;
var t    : text;
    s    : string;
    n    : byte;
    found: boolean;
    p,p2 : integer;
begin
  FindFidoAddress := false;
  assign(t,fn);
  if not existf(t) then exit;
  reset(t);
  n:=0;
  found:=false;
  while (n<100) and not found and not eof(t) do begin
    readln(t,s);
    p:=cpos(':',s);
    while (p>1) and not found do begin
      p2:=p+1;
      while (p2<length(s)) and isnum(s[p2]) do   { Ende der Netznummer suchen }
        inc(p2);
      if s[p2]='/' then begin
        found:=true;
        while (p>1) and isnum(s[p-1]) do    { Anfang der Zonennummer suchen }
          dec(p);
        inc(p2);
        while (p2<length(s)) and isnum(s[p2+1]) do
          inc(p2);                          { Ende der Nodenummer suchen }
        s:=copy(s,p,p2-p+1);
        end
      else begin
        s:=mid(s,p2);
        p:=cpos(':',s);
        end;
      end;
    inc(n);
    end;
  close(t);
  if found then SplitFido(s,fa,DefaultZone);
  FindFidoAddress:=found;
end;

{
  $Log$
  Revision 1.71  2002/12/09 14:37:21  dodi
  - merged include files, updated comments

  Revision 1.70  2002/07/25 20:43:56  ma
  - updated copyright notices

  Revision 1.69  2002/07/16 17:46:25  ma
  - fixed: Nodelist lookup by point address did not work with certain
    addresses and FD style pointlists

  Revision 1.68  2002/04/07 16:57:53  mk
  - added some const parameters

  Revision 1.67  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.66  2001/11/22 10:39:58  mk
  - NDIFF is intern, removed test for ndiff.exe

  Revision 1.65  2001/11/18 12:31:22  mk
  - fixed some file case problems with fido file lists

  Revision 1.64  2001/10/20 17:26:42  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.63  2001/10/12 22:55:25  mk
  - fixed some Write()

  Revision 1.62  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.61  2001/09/20 18:29:52  cl
  - changed var to const for TLister.OnMarkTest

  Revision 1.60  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.59  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.58  2001/09/08 16:29:39  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.57  2001/09/07 23:24:55  ml
  - Kylix compatibility stage II

  Revision 1.56  2001/09/07 13:54:24  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.55  2001/08/11 23:06:37  mk
  - changed Pos() to cPos() when possible

  Revision 1.54  2001/07/31 16:18:41  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.53  2001/07/28 12:04:15  mk
  - removed crt unit as much as possible

  Revision 1.52  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.51  2001/06/05 16:44:49  ma
  - Fido crash netcalls should be working again
  - cleaned up a bit

  Revision 1.50  2001/06/04 16:12:52  ma
  - fixed: file name case

  Revision 1.49  2001/04/28 15:20:56  ml
  - final fix for Diskfree (patch for sysutils needed)

  Revision 1.48  2001/04/27 10:00:40  ml
  - bugfix Diskfree in linux (while Reading Nodelist - temporarily Root-Diskfree till fpc-developer fixes his routines)

  Revision 1.47  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.46  2001/02/28 14:25:47  mk
  - removed some tainted comments

  Revision 1.45  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.44  2001/01/15 23:01:39  mk
  - diskfree with in64

  Revision 1.43  2001/01/07 12:34:36  mo
  - einig  �nderungen an TNodeList

  Revision 1.42  2001/01/06 21:13:36  mo
  - �nderung an TnodeListItem

  Revision 1.41  2001/01/06 17:18:08  mk
  - fixed some TNodeListItem-Bugs

  Revision 1.40  2000/12/28 23:12:03  mo
  - class TNodeList erg�nzt

  Revision 1.39  2000/12/27 22:36:31  mo
  -new class TfidoNodeList

  Revision 1.38  2000/12/25 14:02:44  mk
  - converted Lister to class TLister

  Revision 1.37  2000/12/09 16:00:29  mo
  - string[1] nach FirstChar

  Revision 1.36  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.35  2000/11/15 23:00:43  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.34  2000/11/14 15:51:36  mk
  - replaced Exist() with FileExists()

  Revision 1.33  2000/11/14 11:14:34  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.32  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.31  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.30  2000/08/21 06:51:45  mo
  -mainnodelist() liefert richtigen Wert

  Revision 1.29  2000/08/19 08:56:24  mk
  MO:- Source ausfuehrlich kommentiert

  Revision 1.28  2000/08/17 22:13:00  mk
  - MO: - fidolastseek[Length(fidolastseek)] durch Lastchar ersetzt

  Revision 1.27  2000/08/14 14:45:14  mk
  MO: Umfangreiche Aenderung fuer Null basierende Stringlisten

  Revision 1.26  2000/08/08 18:07:51  mk
  - Test auf Speicher beim Nodelistenindex anlegen rausgenommen

  Revision 1.25  2000/08/01 08:40:41  mk
  - einige String-Parameter auf const geaendert

  Revision 1.24  2000/07/27 10:13:05  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.23  2000/07/21 21:17:48  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.22  2000/07/13 10:23:47  mk
  - Zeiger auf Strings entfernt

  Revision 1.21  2000/07/05 13:55:02  hd
  - AnsiString

  Revision 1.20  2000/07/04 12:04:29  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.19  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.18  2000/07/03 13:31:44  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.17  2000/07/02 14:24:54  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.16  2000/06/29 13:01:00  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.15  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.14  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.13  2000/04/18 11:23:51  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.12  2000/04/13 12:48:40  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.10  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.9  2000/03/04 23:34:53  mk
  MH: Flagzeile kuerzen, falls zu lang

  Revision 1.8  2000/03/03 18:14:46  mk
  MO: - fileseek fidofilelist, Quelltext renoviert und Suche nach ganzen Woertern eingef�hrt

  Revision 1.7  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


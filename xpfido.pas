{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Nodelist }

{$I XPDEFINE.INC}

unit xpfido;

interface

uses  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  sysutils,dos,dosx,typeform,fileio,inout,keys,winxp,maus2,
  maske,lister, archive,stack,montage,resource,datadef,database,
  xp0,xp1,xp1o,xp1input;

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
      crashID  = 'ÆcrashØ';

      nlNodelist = 1;     { normale Nodelist }
      nlPoints24 = 2;     { Pointliste im P24-Format }
      nlNode     = 3;     { Pointlist fÅr einen Node }
      nl4DPointlist = 4;  { 4D-Pointliste }
      nlFDpointlist = 5;  { FrontDoor-Pointliste }

type  nodeinfo = record
                   found    : boolean;
                   ispoint  : boolean;
                   status   : string;
                   boxname  : string;
                   standort : string; { 03.02.2000 MH: 40 -> 65 }
                   sysop    : string;
                   telefon  : string;
                   baud     : word;
                   fflags   : string; { MH: 40 -> 80 }
                   flags    : word;
                   request  : word;
                   datei    : byte;     { Nummer der Nodeliste }
                 end;

procedure MakeNodelistIndex;
procedure OpenNodeindex(fn:string);
procedure CloseNodeindex;
procedure GetNodeinfo(adr:string; var ni:nodeinfo; pointtyp:integer);
function  IsFidoNode(adr:string):boolean;
function  FidoIsISDN(var fa:FidoAdr):boolean;
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

function  FidoFilename(var fa:FidoAdr):string;
function  CrashFile(adr:string):string;
procedure GetReqFiles(adr:string; var files:string);
function  FidoPhone(var fa:FidoAdr; var nl_phone:string):string;
function  FidoAppendRequestfile(var fa:FidoAdr):string;
procedure ShrinkPointToNode(var fa:FidoAdr; var ni:NodeInfo);
function  FindFidoAddress(fn:string; var fa:FidoAdr):boolean;

procedure NodelistBrowser;

procedure SetCrash(adr:string; insert:boolean);
procedure SetRequest(adr,files:string);  { '' -> Request lîschen }

function  MainNodelist:integer;
procedure NodelistIndex;
procedure NodelistSeek;
procedure SetShrinkNodelist;
procedure ShrinkNodelist(indizieren:boolean);

function  ReqTestNode(var s:string):boolean;
procedure FileSelProc(var cr:customrec);
function  fstestmark(var s:string; block:boolean):boolean;
procedure NodeSelProc(var cr:customrec);


implementation

uses  xpnt,xp2,xp3,xp4e,xpfidonl;


{ --- Nodelisten ----------------------------------------------------- }

const bersize   = 200;     { Max. Netze pro Bereich }
      maxber    = 300;
      maxnodes  = 3000;    { max Nodes / Net }
      maxpoints = 700;     { max Points / Node }
      nodekenn  = 'IDX'^Z;
      MaxNamelen= 30;      { max. NamenslÑnge in Userindex }
      blocksize = 1024;    { Blockgrî·e in Userindex }

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
      filep      = ^file;

const nodelistopen : boolean = false;

var   NX_adrnetx   : longint;
      bereiche     : word;
      berliste     : ^bereichlst;
      nodef        : file;
      nodelf       : filep;
      FreqLst      : string;
      DelFilelist  : boolean;   { lokal NodeSelProc }
      UserBlocks   : longint;


procedure MakeNodelistIndex;
const tbuf     = 8192;
      nbuffers = 32;
var x,y        : byte;
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
    gotoxy(x+31,y+2); write(zone,':',net); write(sp(x+40-wherex));
    gotoxy(x+49,y+2); write(gusers:6);
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
      rr   : word;
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
          inc(add);    { letzes Netz in diesem Bereich kînnte = 1. Netz im }
                       { nÑchsten sein..                                   }
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
      rr : word;
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
      name:=left(s,p-1);
      p:=length(name);
      while (p>1) and (name[p]<>'_') do dec(p);
      if p>1 then
        name:=mid(name,p+1)+' '+left(name,p-1);
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
      durchl : integer;   { Anzahl DurchlÑufe }
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
      bufsize:=bufanz*sizeof(userrec);     { Grî·e abrunden }
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
    while chunks>1 do begin    { zÑhlen }
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

  procedure MakeUserIndex(xx:byte);     { Userindex komprimieren }
  const ubufanz   = 100;
  type  block     = array[0..blocksize-1] of byte;
        ubufa     = array[0..ubufanz-1] of userrec;
  var   bbuf      : ^block;
        ubuf      : ^ubufa;
        uihd      : udxheader;
        bufp,bufanz:word;
        lname     : string[MaxNamelen];
        user      : UserRec;
        cuser     : array[0..50] of byte;  { komprimierter User-Record }
        cuserp    : byte;
        outp      : word;  { Position in bbuf }
        w         : word;
        b,adrf    : byte;
        nn        : longint;
        ok        : boolean;

    procedure ReadUbuf;
    var rr : word;
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

  begin
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
      user:=ubuf^[bufp];
      inc(bufp);
      if bufp=bufanz then ReadUbuf;
      with user do
        repeat
          cuserp:=0;
          { R-}
          b:=0; w:=min(length(name),length(lname));
          while (b<w) and (name[b+1]=lname[b+1]) do inc(b);
          cuser[cuserp]:=b; inc(cuserp,2);
          w:=length(name)-b;
          cuser[cuserp]:=w; inc(cuserp);        { Name }
          if w>0 then
            Move(name[b+1],cuser[cuserp],w);
          inc(cuserp,w);
          lname:=name;
          adrf:=0;
          if (adr[0]>0) and (adr[0]<16) then    { Zone }
            inc(adrf,adr[0]*16)
          else begin
            cuser[cuserp]:=lo(adr[0]);
            cuser[cuserp+1]:=hi(adr[0]);
            inc(cuserp,2);
            end;
          cuser[cuserp]:=lo(adr[1]); inc(cuserp);   { Net }
          if adr[1]<256 then
            inc(adrf,1)
          else begin
            cuser[cuserp]:=hi(adr[1]); inc(cuserp);
            end;
          cuser[cuserp]:=lo(adr[2]); inc(cuserp);   { Node }
          if adr[2]<256 then
            inc(adrf,2)
          else begin
            cuser[cuserp]:=hi(adr[2]); inc(cuserp);
            end;
          if adr[3]=0 then                          { Point }
            inc(adrf,4)
          else begin
            cuser[cuserp]:=lo(adr[3]); inc(cuserp);
            if adr[3]<256 then
              inc(adrf,8)
            else begin
              cuser[cuserp]:=hi(adr[3]); inc(cuserp);
              end;
            end;
          cuser[1]:=adrf;                           { Adre·-Flag }
          if fnr=0 then
            inc(cuser[2],$40)
          else begin
            cuser[cuserp]:=fnr;
            inc(cuserp);
            end;
          if fadr<$1000000 then begin
            inc(cuser[2],$80);
            Move(fadr,cuser[cuserp],3);
            inc(cuserp,3);
            end
          else begin
            Move(fadr,cuser[cuserp],4);
            inc(cuserp,4);
            end;
          { R+}
          ok:=(outp+cuserp+1)<=blocksize;
          if not ok then
            FlushOut
          else begin
            Move(cuser,bbuf^[outp],cuserp);
            inc(outp,cuserp);
            end;
        until ok;
      end;

    if outp>0 then
      FlushOut;
    seek(uf[1]^,0);
    uihd.blocks:=filesize(uf[1]^) div blocksize - 1;
    blockwrite(uf[1]^,uihd,sizeof(uihd));   { Header schreiben }
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
  chunksize:=(memavail-10000) div (sizeof(usernode)+8);
  new(uf[0]);
  assign(uf[0]^,'users1.$$$'); rewrite(uf[0]^,1);

  for liste:=0 to NodeList.Count - 1 do
  begin
    zone:=PNodeListItem(Nodelist[liste])^.zone;
    if zone=0 then zone:=DefaultZone;
    net:=0; node:=0;
    assign(nf,FidoDir+NLfilename(liste));
    ltyp:=PNodeListItem(Nodelist[liste])^.format;
    case ltyp of
      nlPoints24,
      nl4DPointlist,
      nlFDpointlist : zone:=PNodeListItem(Nodelist[liste])^.zone;
      nlNode        : begin
                         zone:=PNodeListItem(Nodelist[liste])^.zone;
                         net :=PNodeListItem(Nodelist[liste])^.net;
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
          if (s[1]<>';') and (p>0) then begin
            if (p=1) or (ltyp=nlNode) then begin
              if ltyp=nlFDpointlist then k:='Point'
              else k:='';
              newnet:=false; end
            else begin
              k:=TopStr(copy(s,1,p-1));
              if (ltyp=nlFDpointlist) and (k='Boss') then begin
                ss:=mid(s,p+1);
                p:=cposx(',',ss);
                SplitFido(left(ss,p-1),fa,zone);
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
              val(left(s,p-1),l,res);
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
                  AppPoint(PNodeListItem(Nodelist[liste])^.node);

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
                    splitfido(left(s,p-1),fa,zone);
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
            np^[0].node:=PNodeListItem(Nodelist[liste])^.node;
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
          if (k='Host') or (k='Region') then begin
            if ltyp<>nlFDpointlist then begin
              net:=node;
              np^[0].node:=0;
              end
            else
              np^[0].node:=node;
            np^[0].adr:=fpos-ll-2;
            nodes:=1;
            end
          else begin  { zone }
            if ltyp<>nlFDpointlist then begin
              zone:=node;
              net:=node;
              end;
            nodes:=1;
            if ltyp=nlFDpointlist then
              np^[0].node:=node
            else
              np^[0].node:=0;
            np^[0].adr:=fpos-ll-2;
            end;
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
  SortChunks;    { schlie·t+lîscht uf[1]^ }
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


procedure OpenNodeindex(fn:string);
var hd  : idxheader;
    uhd : udxheader;
    rr  : word;
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
  nodeopen:=true;
end;


procedure CloseNodeindex;
begin
  freemem(berliste,bereiche*sizeof(berrec));
end;


procedure KeepNodeindexOpen;
begin
  if nodeopen and not nodelistopen then begin
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
  else if left(telefon,length(vorwahl))=vorwahl then begin
    delete(telefon,1,length(vorwahl));
    if left(telefon,1)='-' then delfirst(telefon);
    FormFidoPhone:=telefon;
    end
  else begin
    p:=cpos('-',vorwahl);
    if left(telefon,p)=left(vorwahl,p) then
      FormFidoPhone:=NatVorwahl+mid(telefon,p+1)
    else
      FormFidoPhone:=intVorwahl+telefon;
    end;
end;


{$I xpf1.inc}   { Nodeliste auslesen/abfragen }


procedure NodelistIndex;
begin
  if not TestNodelist then exit;
  CloseNodeindex;
  nodeopen:=false;
  MakeNodelistIndex;
  OpenNodeindex(NodeIndexF);
end;


function MainNodelist:integer;
begin
  Result := NodeList.Count - 1;
  while (Result>0) and (PNodeListItem(nodelist[Result])^.listfile<>'NODELIST.###') do
    dec(Result);
end;


procedure SetShrinkNodelist;
var x,y   : byte;
    brk   : boolean;
    s,s2  : string;
    ss    : string;
    p     : byte;
    l     : longint;
    res   : integer;
begin
  if not TestNodelist then exit;
  if MainNodelist=0 then begin
    rfehler(2125);    { 'Es ist keine Haupt-Fido-Nodeliste (NODELIST.###) eingebunden.' }
    exit;
    end;
  dialog(57,3,getres2(2104,1),x,y);   { 'Nodelist einschrÑnken' }
  s:=ShrinkNodes;
  maddstring(3,2,getres2(2104,2),s,35,100,'0123456789 :');  { 'Zonen/Regionen ' }
  readmask(brk);
  enddialog;
  if not brk then begin
    if s='' then begin
      ShrinkNodes:='';
      SaveConfig2;
      message(getres2(2104,3));  { 'EinschrÑnkungen gelîscht - ggf. Nodeliste neu einlesen.' }
      wait(curoff);
      closebox;
      freeres;
      exit;
      end;
    s2:=s;
    s:=s+' ';
    repeat
      p:=cpos(' ',s);
      ss:=left(s,p-1);
      s:=trimleft(mid(s,p));
      p:=cpos(':',ss);
      if p=0 then
        val(ss,l,res)
      else begin
        val(left(ss,p-1),l,res);
        if res=0 then val(mid(ss,p+1),l,res);
        end;
    until (res<>0) or (s='');
    if res<>0 then
      fehler(getres2(2104,4))   { 'ungÅltige Eingabe' }
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
  i:=MainNodelist;
  if (i>0) and nodeopen and (trim(ShrinkNodes)<>'') then
    if not exist('NDIFF.EXE') then
      rfehler(103)   { 'NDIFF.EXE fehlt!' }
    else begin
      shell('NDIFF.EXE -s '+FidoDir+NLfilename(i)+' '+ShrinkNodes,250,3);
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
    s   : string[80];
    p   : byte;
    node: string[20];
    ni  : nodeinfo;
    anz : longint;
    fn  : string[12];
    sr  : searchrec;

  function fdate:string;
  var
    dt : datetime;
  begin
    unpacktime(sr.time,dt);
    fdate:=formi(dt.month,2)+'/'+formi(dt.year mod 100,2);
  end;

begin
  listbox(73,15,getres(iif(delfilelist,2105,2106)));
  assign(t,FileLists);          { 'Fileliste lîschen','File Request' }
  reset(t);
  anz:=0;
  KeepNodeindexOpen;
  while not eof(t) do begin
    readln(t,s); s:=trim(s);
    p:=pos('=',s);
    if (s<>'') and (s[1]<>'#') and (s[1]<>';') and (p>0) then begin
      fn:=mid(s,p+1);
      findfirst(FidoDir+fn,ffAnyFile,sr);
      if (doserror=0) and (sr.size>0) then begin
        node:=left(s,p-1);
        GetNodeinfo(node,ni,1);
        inc(anz);
        if cpos('.',fn)>0 then
          fn:=left(fn,cpos('.',fn)-1);
        app_l(' '+forms(node,14)+' '+
              forms(iifs(ni.found,ni.boxname+', '+ni.standort,'???'),32)+
              '  '+fdate+'  '+forms(fn,9)+strsn(sr.size div 1024,5)+'k ');
        end;
      end;
    end;
  KeepNodeindexClosed;
  close(t);
  if anz>0 then begin
    list(cr.brk);
    if not cr.brk then begin
      cr.s:=trim(left(get_selection,15));
      if not delfilelist and (copy(get_selection,17,4)<>'??? ') then
        keyboard(keycr+keyf2);
      end;
    end
  else
    cr.brk:=true;
  closelist;
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
                      var ni:NodeInfo; var brk:boolean; var x,y:byte);
var node    : string;
{    modem   : string[53]; }
    xx,yy,i : byte;
    t       : taste;
begin
  dialog(38,3,txt,x,y);
  if fa.net+fa.node=0 then
    node:=DefFidoBox
  else
    node:=MakeFidoAdr(fa,true);
  maddstring(3,2,getres(2107),node,20,25,''); mhnr(730);  { 'Node/Name ' }
  if request and exist(FileLists) and exist(FidoDir+'*.FL') then begin
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
    if not ni.found then begin      { dÅrfte eigentlich nicht passieren.. }
      rfehler(2116);   { 'unbekannte Nodeadresse' }
      brk:=true;
      end
    else begin
      ShrinkPointToNode(fa,ni);
      node:=MakeFidoAdr(fa,true);
      if request and (ni.request and rfWaZOO=0) then
        brk:=not ReadJN(getres(2108),true);  { 'Laut Nodelist kein Request bei dieser Box mîglich - trotzdem versuchen' }
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
    maddtext(12,2,iifs(ni.ispoint,ni.sysop,left(ni.boxname+', '+ni.standort,
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

      if left(modem,1)=' ' then delete(modem,1,3);
      if modem='' then modem:=strs(baud);
      maddtext(12,4,modem,col.coldiahigh); }


      { Fix: Flagzeile zu lang: gek¸rzt und E-Mail rausschneiden }
      maddtext(12, 4, copy(MailString(FFlags, True), 1, 50), col.coldiahigh);
      maddtext(12, 5, status, col.coldiahigh);

      end;
    end;
end;

function TestNodelist:boolean;
begin
  if not NodeOpen then
    rfehler(2102);   { 'keine Nodelist aktiv' }
  TestNodelist:=NodeOpen;
end;

function testDefbox:boolean;
begin
  if DefFidoBox='' then
    rfehler(2118);   { 'keine Fido-Stammbox gewÑhlt (Edit/Boxen)' }
  testDefbox:=(DefFidobox<>'');
end;

function FidoFilename(var fa:FidoAdr):string;
begin
  FidoFilename:=hex(fa.net,4)+hex(fa.node,4);
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
  if left(files,1)='>' then delfirst(files);
  ff:=FidoFilename(fa)+'.REQ';
  if files<>'' then begin
    assign(t,ff);
    if existf(t) then append(t)
    else rewrite(t);
    files:=files+' ';
    repeat
      p:=cpos(' ',files);
      if p>0 then begin
        _file:=trim(left(files,p));
        files:=trimleft(mid(files,p));
        p2:=cpos('/',_file);
        if p2=0 then writeln(t,_file)
        else writeln(t,left(_file,p2-1)+' !'+mid(_file,p2+1));
        end;
    until p=0;
    close(t);
    end;
  if exist(ff) then FidoAppendRequestfile:=ff
  else FidoAppendRequestfile:='';
end;

function CrashFile(adr:string):string;
var fa : FidoAdr;
begin
  splitfido(adr,fa,DefaultZone);
  CrashFile:=FidoFilename(fa)+'.CP';
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
          if left(s,1)='>' then delfirst(s);
          if s<>CrashID then files:=files+' '+s;
        until s=''
      else
        repeat readln(t,s) until s='';
      end;
    close(t);
    files:=trim(files);
    end;
end;


function fstestmark(var s:string; block:boolean):boolean;
begin
  if (left(s,2)>'  ') and ((s<#176) or (s>#223)) then
    fstestmark:=true
  else begin
    if not block then errsound;
    fstestmark:=false;
    end;
end;

procedure FileSelProc(var cr:customrec);
var s   : string;
    p   : scrptr;
begin
  OpenList(1,ScreenWidth,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/APGD/');
  rmessage(2110);   { 'Lade Fileliste ...' }
  list_readfile(FreqLst,0);
  closebox;
  listVmark(fstestmark);
  sichern(p);
  repeat
    list(cr.brk);
  until not cr.brk or (list_markanz=0) or ReadJN(getres(2123),true);   { 'Auswahl verwerfen' }
  holen(p);
  if not cr.brk then begin
    s:=first_marked;
    while s<>#0 do begin
      s:=trim(s);
      if s[1]>='≥' then s:=trim(mid(s,2));
      if blankpos(s)>0 then
        s:=left(s,blankpos(s)-1);
      cr.s:=cr.s+' '+s;
      s:=next_marked;
      end;
    cr.s:=trim(cr.s);
    end;
  closelist;
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
        if left(s,p-1)=node then begin
          found:=true;
          delete(s,1,p);
          if cpos(' ',s)>0 then s:=left(s,cpos(' ',s)-1);
          getFilelist:=FidoDir+s;
          end;
      end;
    close(t);
    end;
end;

function FidoRequest(node,files:string):string;
var brk   : boolean;
    x,y   : byte;
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
  if left(files,1)='>' then delfirst(files);
  atonce:=false;
  maddstring(3,7,getres2(2112,1),files,50,254,'>'); mhnr(735);  { 'Dateien ' }
  freqlst:=GetFilelist(fa);
  if exist(freqlst) then
    MappCustomsel(FileSelProc,false);
  maddbool(4,9,getres2(2112,2),atonce);   { ' sofort starten' }
  readmask(brk);
  enddialog;
  doreq:=(files<>'');
  if not brk then begin
    if not doreq then begin
      message(getres2(2112,3));  { 'Request-Anforderung lîschen ...' }
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
  if exist(ReqDat) then begin
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
    erase(t1);   { alte ReqDat lîschen }
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


procedure SetRequest(adr,files:string);   { '' -> Request lîschen }
var t1,t2   : text;
    s       : string;
    crash   : boolean;
begin
  crash:=false;
  assign(t2,TempFile(''));
  rewrite(t2);
  if exist(ReqDat) then begin
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
    erase(t1);   { alte ReqDat lîschen }
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
    x,y    : byte;
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
    overwrite:=ReadJN(left(fn,40)+getres(2113),true);  { ' bereits vorhanden - Åberschreiben' }
  end;

  function filetest(docopy:boolean; size:longint; path:string; fi:string):boolean;
  var p : byte;
  begin
    if (diskfree(ord(path[1])-64)<=size) and fehlfunc(getres(2114)) then  { 'zu wenig Platz' }
      filetest:=false
    else if docopy and exist(path+fi) and not overwrite(path+fi) then
      filetest:=false
    else if right(fi,3)='.FL' then
      filetest:=true
    else begin
      p:=cpos('.',fi);
      if p=0 then fi:=fi+'.FL'
      else fi:=left(fi,p)+'FL';
      filetest:=(not exist(path+fi) or overwrite(path+fi));
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
          if (p>0) and (left(s,p-1)<>node) and (mid(s,p+1)<>fi2) and
             exist(FidoDir+mid(s,p+1)) then   { falsche EintrÑge killen }
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
     (not exist(fn) and fehlfunc(getres2(2117,2))) then  { 'Datei nicht vorhanden' }
    goto ende;
  fn:=UpperCase(FExpand(fn));
  fi:=ExtractFilename(fn);
  p:=cpos('.',fi);
  if p=0 then fn:=fn+'.';
  if (p>0) and (ival(left(fi,p-1))>0) then begin
    fillchar(fa,sizeof(fa),0);
    if not nodeopen then
      node:=''
    else begin
      node:=strs(DefaultZone)+':'+strs(ival(left(fi,4)))+'/'+strs(ival(copy(fi,5,4)));
      getNodeinfo(node,ni,1);
      if not ni.found then
        if FindFidoAddress(fn,fa) then
          node:=MakeFidoAdr(fa,false)
        else
          if ival(left(fi,1))>0 then
            node:=left(fi,1)+':'+strs(ival(copy(fi,2,3)))+'/'+strs(ival(copy(fi,5,4)))
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
    if exist(FidoPath+ar.name) then
      _era(FidoPath+ar.name);
    if not UniExtract(fn,FidoPath,ar.name) then                goto ende;
    fi:=ar.name;
    copied:=true;
    end
  else if GetFiledir(fn)=FidoPath then begin   { ungepackt, in FIDO\ }
    if right(fn,3)<>'.FL' then
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

  if right(fi,3)<>'.FL' then begin
    p:=cpos('.',fi);
    if p=0 then fi2:=fi+'.FL'
    else fi2:=left(fi,p)+'FL';
    if exist(FidoPath+fi2) then
      _era(FidoPath+fi2);
    if not _rename(FidoPath+fi,FidoPath+fi2) and
       fehlfunc(getres2(2117,8)) then   { 'Fehler beim Umbenennen' }
      goto ende;
    end
  else
    fi2:=fi;
  UpdateReqdat;
  truncstr(fidolastseek,1);
  SaveConfig2;
  if copied and ReadJN(reps(getres2(2117,9),fn),true) then begin  { '%s lîschen' }
    message(reps(getres2(2117,10),fn));   { 'Lîsche %s ...' }
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
  if not exist(FileLists) or not exist(FidoDir+'*.FL') then
    rfehler(2119)   { 'keine Filelisten vorhanden' }
  else begin
    cr.s:='';
    DelFilelist:=true;
    pushhp(85);
    NodeSelProc(cr);
    pophp;
    if cr.brk then exit;
    if ReadJN(reps(getres(2118),cr.s),false) then begin   { 'Fileliste fÅr %s lîschen' }
      rmessage(2119);   { 'Lîsche Fileliste ...' }
      nn:=0;
      assign(t1,FileLists); reset(t1);
      assign(t2,FidoDir+'request.$$$'); rewrite(t2);
      while not eof(t1) do begin
        readln(t1,s);
        s:=trim(s);
        p:=cpos('=',s);
        comment:=(s='') or (s[1]='#') or (s[1]=';') or (p=0);
        if comment or (left(s,p-1)<>cr.s) then begin
          writeln(t2,s);
          if not comment then inc(nn);
          end
        else
          if exist(FidoDir+mid(s,p+1)) then
            _era(FidoDir+mid(s,p+1));
        end;
      close(t1); close(t2);
      erase(t1);
      if nn>0 then
        rename(t2,FileLists)
      else
        erase(t2);    { keine Listen Åbrig }
      mdelay(200);
      closebox;
      end;
    end;
end;



{ MO 23.01.0000 File Suche in Fido Requstlisten mit bis zu fÅnf
  Suchbegriffen  }
function FidoSeekfile:string;
  const
    seekfile         = 'fileseek.dat';
    maxbuf           = 20;
    SearchStr_maxIdx = 4;
    iCase : boolean = true;                 { Gro·-Kleinschreibung  }
    wCase : boolean = true;                 { Suche nur ganze Worte }
    _sep             = '/&';                { der Seperator 2-Beyte }

  var
    seek, oldseek    : string;
    sNodInf,sZeile   : string;
    sFlistName       : string;
    sa               : array[0..maxbuf] of string;
    searchStr        : array[0..SearchStr_maxIdx] of string;
    x,y              : byte;
    brk              : boolean;
    pFileListCfg     : ^text;
    pOutput          : ^text;
    pFileListe       : ^text;
    anz_FileFound, p : longint;
    tb               : pointer;
    scp              : scrptr;
    tbs              : word;
    ni               : ^nodeinfo;
    files            : string;
    len              : byte;
    anz_searchStr    : integer;    { Anzahl der besetzten Suchstrings }
    label
       ende;

 {true, wenn der Substring in dem zu durchsuchenden String vollstÑndig enthalten ist }
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
  procedure pTestWriteln(s:string);
  begin
    writeln(pOutput^,s);
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
        inc(n);                                     { nÑchste Zeile }
      end; { while ( n < apos)   do } { alle Zeilen durchtesten }
      {Begriff im Bolck nicht gefunden, dann raus }
      if test =false then exit;
    end;    { for n2:=0 to anz_searchStr - 1 Do }

    if test = true then
    begin
      { schreibe 1. Zeile und die node Nummer }
      pTestWriteln(forms(sa[0],80)+left(sNodInf,p-1));
      nn:=1;
      while ( nn < BlockLength) and ( brk = false )   do
      begin
        pTestWriteln(sa[nn] ); {Zeile des Blockes speichern }
        inc(nn);
      end;
      inc(anz_FileFound);      {Erhîhe Anzahl der gefunden  }
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
    getNodeinfo(left(sNodInf,p-1),ni^,1);
    settextbuf(pFileListe^,tb^,tbs);
    reset(pFileListe^);
    begin                                  { write header}
      sNodInf:=left(sNodInf,p-1)+' ';      { s:= 2:244/1278 }
      if ni^.found then sNodInf:=sNodInf+'('+ni^.boxname+', '+ni^.standort+', '+sFlistName  +')'
      else sNodInf:=sNodInf+'(??, '+sFlistName  +')';         { s:= 2:244/1278 (C-Box, Frankfurt, 02441278.FL}
      writeln(pOutput^,' ',' '+sNodInf);
      writeln(pOutput^,' ',' '+dup(length(sNodInf),'ƒ'));
      writeln(pOutput^);
    end;
    apos:=0;                                    { ZeilenzÑhler ungÅltig setzen}
    beginSafe:=false;
    while not eof(pFileListe^) and (not brk) do               {'FIDO\22441278.fl'}
    begin
      readln(pFileListe^,sZeile);               {lese Zeile aus der Fileliste }
      pTabExpand(sZeile);
      { ist gelesene Zeile eine headerzeile }
      if ( FirstChar(sZeile) >' ') and( sZeile <> '') and (( FirstChar(sZeile) <#176)or( FirstChar( sZeile) >#223)) then
      begin
        beginSafe:=true;
        if( apos > 0 ) then { wurde ein Block bereits zwischengespeichert?}
        begin
          { Block testen und ggf. speichern }
          pTestBlock( apos );
          apos:=0;  { setze ZeilenZÑhler auf Start }
        end;
      end; { if (ss[1]>' ') and ( (ss[1]<#176)or( ss[1]>#223) ) then }
      if ( apos <= maxbuf) and  ( beginSafe ) then { im gÅltigen Bereich}
      begin
        sa[apos]:=sZeile;  { speicher String in Array }
        inc(apos);
      end;
    end; { while not eof(pFileListe^) do begin {'FIDO\22441278.fl'}
    close(pFileListe^);
  end;


begin       { FidoSeekfile:string;************************ }

  FidoSeekfile:='';
  anz_FileFound:=0;                     { Anzahl der gefunden Dateien =0 }
  if not exist(FileLists) or not exist(FidoDir+'*.FL') then
  begin
    fehler(getres2(2120,2));            { 'keine Filelisten vorhanden' }
    goto ende;
  end;
  oldseek:=fidolastseek;                { icase+letzten Suchstring  }
  fidolastseek:=mid(fidolastseek,3);    { icase extrahieren }
  iCase:=(left(oldseek,1)='J');         { icase     }
  wCase:=(copy(oldseek,2,1)='J');       { Wcase     }
  {Anzeige initilisieren }
  dialog(57,6,getres2(2120,3),x,y);     { 'Dateien suchen' }
  if fidolastseek[length(fidolastseek)] = #27 then
     fidolastseek:=left(fidolastseek, length(fidolastseek)-1);
  maddstring(3,2,getres2(2120,4),fidolastseek,40,40,'');   { 'Suchbegriff ' }
  maddbool(3,4,getres2(2120, 5),iCase); { 'Schreibweise ignorieren' }
  maddbool(3,5,getres2(2120,11),wCase); { 'Nur ganze Wîrter suchen }
  readmask(brk);                        {  Dialog anzeigen/ausfÅhren }
  enddialog;
  pTestSeekStr( fidolastseek);           {teste den String auf Korrektheit}
  fidolastseek:=iifc(icase,'J','N')+iifc(Wcase,'J','N')+fidolastseek; {Ja or Nein + Suchbegriff }

  if brk or (fidolastseek='') then goto ende;

  if icase then seek:=UpperCase(mid(fidolastseek,3)) {seek, enthÑlt nun den suchstring}
  else          seek:=mid(fidolastseek,3);
  pInitSearchStr;                       { zerlege Suchstring}

  if fidolastseek <> oldseek  then
  begin
    msgbox(30,6,getres2(2120,6),x,y);   { 'Suchen ..' }
    mwrt(x+3,y+2,getres2(2120,7));      { 'Datei' }
    mwrt(x+3,y+3,getres2(2120,8));      { 'gefunden' }
    new(pFileListCfg);
    new(pOutput);
    new(pFileListe);
    new(ni);
    tbs:=16384;
    getmem(tb,tbs);
    assign(pOutput^,seekfile);
    rewrite(pOutput^);
    len:=length(getres2(2120,9))+3;
    writeln(pOutput^,' ',' '+dup(len+length(seek),'Õ'));
    writeln(pOutput^,' ',' '+getres2(2120,9),' "',mid(fidolastseek,3),'"');   { 'Dateisuche nach' .. }
    writeln(pOutput^,' ',' '+dup(len+length(seek),'Õ'));
    writeln(pOutput^);
    assign(pFileListCfg^,FileLists);
    reset(pFileListCfg^);
    while ( not eof(pFileListCfg^)) and (not brk) do       {noch EintrÑge in der cfg.datei}
    begin
      readln(pFileListCfg^,sNodInf); sNodInf:=trim(sNodInf);
      p:=cpos('=',sNodInf);
      if (sNodInf<>'') and (FirstChar(sNodInf)<>'#') and (FirstChar(sNodInf) <>';') and (p>0) then
      begin
        sFlistName:= UpperCase(mid(sNodInf,p+1));        { Name der Fileliste z.B: 22441278.fl}
        assign(pFileListe^,FidoDir+sFlistName  );    { pFileListe='FIDO\22441278.fl'}
        if existf(pFileListe^) then
          pBearbeiteFileListe;
      end;
    end;
    saveconfig2;
    signal;
{I-}
    close(pFileListCfg^);
    close(pOutput^);
    if brk then
    begin
      erase(pOutput^);
      fidolastseek:=fidolastseek+#27;
    end;
    if IoResult<>0 then;
{I+}
    closebox;
    freemem(tb,tbs);
    dispose(ni);
    dispose(pFileListe);
    dispose(pOutput);
    dispose(pFileListCfg);
  end;               { fidolastseek<>oldseek }
  if not brk then    { gefundene Dateien Listen und ggf. requesten }
  begin
    OpenList(1,ScreenWidth,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/NLR/APGD/');
    list_readfile(seekfile,0);
    listVmark(fstestmark);
    listTp(listext);                { 'D' + 'W' }
    llh:=false; listmakros:=0;
    sichern(scp);
    pushhp(83);
    list(brk);
    pophp;
    holen(scp);
    files:='';
    sNodInf:= mid(first_marked,81);;
    sZeile:=first_marked;
    while (sZeile<>#0) do
    begin
      if mid(sZeile,81)<>sNodInf then  { Request bei zwei Boxen! }
      begin
        fehler(getres2(2120,10));      { 'kein gleichzeitiger Request bei mehreren Boxen mîglich' }
        sZeile:=#0;                    { Schleife abbrechen }
      end
      else
      begin
       files:=files+' '+trim(left(sZeile,12));
       sZeile:=next_marked;
      end;
    end;
    files:=trim(files);
    if files<>'' then
    begin
      keyboard(keycr);
      FidoSeekfile:=FidoRequest(sNodInf,files);
    end;
    closelist;
  end;
ende:
  freeres;
end;

function FidoIsISDN(var fa:FidoAdr):boolean;
var ni : NodeInfo;
begin
  GetNodeInfo(MakeFidoAdr(fa,true),ni,2);
  FidoIsISDN:=(pos('ISDN',ni.fflags)>0) or
              (pos('X75',ni.fflags)>0);
end;


{ In Textfile nach erster brauchbarer Nodeadresse suchen }

function FindFidoAddress(fn:string; var fa:FidoAdr):boolean;
var t    : text;
    s    : string;
    n    : byte;
    found: boolean;
    p,p2 : integer;
begin
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


end.
{
  $Log$
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
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
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
  MO: - fileseek fidofilelist, Quelltext renoviert und Suche nach ganzen Woertern eingef¸hrt

  Revision 1.7  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

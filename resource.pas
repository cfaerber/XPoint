{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Schnittstelle zum Resourcen-Compiler RC }
{ PM 12/92                                }

{$I XPDEFINE.INC }

unit  resource;


interface

uses
  xpglobal,
  sysutils,
  typeform,fileio;

procedure OpenResource(fn:string; preloadmem:longint);
procedure CloseResource;
function  ResEmspages:word;
function  ResIsOpen:boolean;

function  GetRes(nr:word):string;
function  GetRepS(nr:word; txt:String):string;
function  GetRes2(nr1,nr2:word):string;
function  GetReps2(nr1,nr2:word; txt:string):string;
function  Res2Anz(nr:word):word;
function  IsRes(nr:word):boolean;
procedure FreeRes;                        { Cluster freigeben }
function  reps(s1,s2:string):string;

procedure InitResourceUnit;

implementation  { --------------------------------------------------- }

const maxblocks = 4;
      maxindex  = 4096;   { max. Strings pro Block }
      flPreload = 1;

type
      barr   = packed array[0..65300] of byte;
      barrp  = ^barr;
      rblock = packed record case integer of
                 0 : (anzahl   : smallword; { Anzahl Strings in diesem Block  }
                      fileadr  : longint;   { Startadresse in RES-Datei       }
                      contsize : smallword; { Gr”áe des Inhalts (Texte)       }
                      lastnr   : smallword; { letzte Res.-Nr. in diesem Block }
                      flags    : smallword; { 1 = preload                     }
                      emshandle: smallword;
                      loaded   : boolean;
                      emspages : byte);
                 2 : (dummy2   : smallword;
                      rptr     : barrp);
               end;
      tindex = packed array[0..maxindex-1,0..1] of smallword;

const f      : ^file = nil;
      clnr   : word  = $ffff;    { geladener Cluster }

var   block  : packed array[1..maxblocks] of rblock;
      blocks : word;
      index  : packed array[1..maxblocks] of ^tindex;

      clsize : word;         { Cluster-Gr”áe }
      clindex: ^tindex;      { Cluster-Index  }
      clcont : barrp;        { Cluster-Inhalt }
      clcsize: word;         { Gr”áe des Inhalts }
      clbnr  : integer;


procedure error(txt:string);
begin
  writeln('<RES> Error: ',txt);
  halt(1);
end;

{ preloadmem: soviel Bytes Heap soll mindestens freibleiben }

procedure OpenResource(fn:string; preloadmem:longint);
var i  : integer;
    fm : byte;
begin
  if f<>nil then
    error('Resource file already open');
  new(f);
  assign(f^,fn);
  fm:=filemode; filemode:=0;   { nur lesen }
  reset(f^,1);
  filemode:=fm;
  if inoutres<>0 then
    error(ioerror(ioresult,'can''t open '+UpperCase(fn)));
  seek(f^,128);
  blockread(f^,blocks,2);
  seek(f^,128+16);
  blockread(f^,block,sizeof(block));
  for i:=1 to blocks do begin          { Indextabellen laden }
    getmem(index[i],block[i].anzahl*4);
    seek(f^,block[i].fileadr);
    blockread(f^,index[i]^,block[i].anzahl*4);
    if block[i].flags and flPreload<>0 then
    begin
        if memavail-block[i].contsize>preloadmem then begin
          getmem(block[i].rptr,block[i].contsize);
          block[i].loaded:=true;
          end;
      if block[i].loaded then
        blockread(f^,block[i].rptr^,block[i].contsize)   { preload }
      else
        inc(block[i].fileadr,block[i].anzahl*4);
      end;
    end;
end;


procedure CloseResource;
var i : integer;
begin
  if f=nil then
    error('no resource file open');
  close(f^);
  dispose(f);
  for i:=1 to blocks do with block[i] do begin
      if loaded then
        freemem(rptr,contsize);
    freemem(index[i],anzahl*4);
    end;
  freeres;
  f:=nil;
end;

function ResIsOpen:boolean;
begin
  ResIsOpen:=(f<>nil);
end;


function ResEmspages:word;
var w,i : word;
begin
  w:=0;
  if f<>nil then
    for i:=1 to blocks do
      inc(w,block[i].emspages);
  ResEmspages:=w;
end;


function getnr(nr:word; var bnr,inr:word):boolean;
var l,r,m : word;
begin
  getnr:=false;
  bnr:=1;
  while (bnr<=blocks) and (nr>block[bnr].lastnr) do
    inc(bnr);
  if (bnr<=blocks) and (block[bnr].anzahl>0) then begin
    getnr:=true;
    l:=0; r:=block[bnr].anzahl-1;
    while (r-l>1) and (index[bnr]^[l,0] and $7fff<>nr) do begin
      m:=(l+r)div 2;
      if index[bnr]^[m,0] and $7fff<=nr then l:=m
      else r:=m;
      end;
    if index[bnr]^[l,0] and $7fff=nr then
      inr:=l
    else if index[bnr]^[r,0] and $7fff=nr then
      inr:=r
    else
      getnr:=false;
    end;
end;


function rsize(bnr,inr:word):word;
begin
  with block[bnr] do
    if inr<anzahl-1 then
      rsize:=index[bnr]^[inr+1,1]-index[bnr]^[inr,1]
    else
      rsize:=contsize-index[bnr]^[inr,1];
end;


function GetRes(nr:word):string;
var bnr,inr : word;
    s       : shortstring;
begin
  if not getnr(nr,bnr,inr) then
    GetRes:='fehlt: ['+strs(nr)+'] '
  else
    with block[bnr] do begin
      SetLength(s, rsize(bnr,inr)); {s[0]:=chr(rsize(bnr,inr));}
      if loaded then begin
        Move(rptr^[index[bnr]^[inr,1]],s[1],length(s));
        end
      else begin
        seek(f^,fileadr+index[bnr]^[inr,1]);
        blockread(f^,s[1],length(s));
        end;
      GetRes:=s;
      end;
end;


procedure FreeRes;                        { Gruppe freigeben }
begin
  if clnr<>$ffff then begin
    if not block[clbnr].loaded then
      freemem(clcont,clcsize);
    freemem(clindex,clsize*4);
    clnr:=$ffff;
    end;
end;


function GetRes2(nr1,nr2:word):string;
var bnr,inr  : word;
    size,ofs : word;
    p        : barrp;
    l,r,m,i  : word;
    s        : shortstring;
label ende;
  function fehlt:string;
  begin
    fehlt:='fehlt: ['+strs(Nr1)+'.'+strs(nr2)+'] ';
  end;
begin
  if not getnr(nr1,bnr,inr) then
    GetRes2:=fehlt
  else
    if index[bnr]^[inr,0] and $8000=0 then
      error('['+strs(nr1)+']: no split page')
    else
      with block[bnr] do begin
        if (inr<>clnr) or (bnr <> clbnr) then
        begin
          FreeRes;
          size:=rsize(bnr,inr);
          ofs:=index[bnr]^[inr,1];
          if loaded then begin
            Move(rptr^[ofs],clsize,2);
            clcsize:=size-2-clsize*4;
            getmem(clindex,clsize*4);
            Move(rptr^[ofs+2],clindex^,clsize*4);
            clcont:=@rptr^[ofs+2+clsize*4];
            end
          else begin
            seek(f^,fileadr+ofs);
            getmem(p,size);
            blockread(f^,p^,size);                 { Cluster komplett laden }
            Move(p^[0],clsize,2);                  { -> Anzahl Elemente     }
            clcsize:=size-2-clsize*4;
            getmem(clindex,clsize*4);
            Move(p^[2],clindex^,clsize*4);         { -> Clusterindex        }
            getmem(clcont,clcsize);
            Move(p^[2+clsize*4],clcont^,clcsize);  { -> Clusterinhalt }
            freemem(p,size);
            end;
          end;
        l:=0; r:=clsize-1;
        while (r-l>1) and (clindex^[l,0]<>nr2) do begin
          m:=(l+r)div 2;
          if clindex^[m,0]<=nr2 then l:=m
          else r:=m;
          end;
        if clindex^[l,0]=nr2 then i:=l
        else if clindex^[r,0]=nr2 then i:=r
        else begin
          s:=fehlt; goto ende;
          end;
        if i<clsize-1 then
          size:=clindex^[i+1,1]-clindex^[i,1]
        else
          size:=clcsize-clindex^[i,1];
        SetLength(s, size); { s[0]:=chr(size); }
        Move(clcont^[clindex^[i,1]],s[1],size);
      ende:
        GetRes2:=s;
        clbnr:=bnr; clnr:=inr;
        end;
end;


function Res2Anz(nr:word):word;
var bnr,inr : word;
begin
  if getnr(nr,bnr,inr) then
    with block[bnr] do begin
      if loaded then begin
        Move(rptr^[index[bnr]^[inr,1]],nr,2);
        end
      else begin
        seek(f^,fileadr+index[bnr]^[inr,1]);
        blockread(f^,nr,2);
        end;
      Res2Anz:=nr;
      end
  else
    Res2Anz:=0;
end;

function IsRes(nr:word):boolean;
var bnr,inr : word;
begin
  IsRes:=getnr(nr,bnr,inr);
end;


function reps(s1,s2:string):string;
var p : byte;
begin
  p:=pos('%s',s1);
  if p>0 then reps:=LeftStr(s1,p-1)+s2+mid(s1,p+2)
  else reps:=s1;
end;

function GetRepS(nr:word; txt:String):string;
begin
  GetReps:=reps(getres(nr),txt);
end;

function GetReps2(nr1,nr2:word; txt:string):string;
begin
  GetReps2:=reps(getres2(nr1,nr2),txt);
end;

var
  SavedExitProc: pointer;

procedure ExitResourceUnit;
begin
  ExitProc:= SavedExitProc;
  if f<>nil then
    closeresource;
end;

procedure InitResourceUnit;
begin
  f:= nil;
  SavedExitProc:= ExitProc;
  ExitProc:= @ExitResourceUnit;
end;

end.
{
  $Log$
  Revision 1.17  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.16  2000/10/17 10:05:43  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.15  2000/08/24 09:15:39  mk
  MO:- Bug in Resourcencaching behoben

  Revision 1.14  2000/07/09 09:09:54  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.13  2000/07/07 14:38:35  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.12  2000/07/05 09:27:09  hd
  - AnsiString-Anpassung

  Revision 1.11  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.10  2000/07/02 14:24:49  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.9  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.8  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.7  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

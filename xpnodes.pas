{ Dieser Quelltext ist Public Domain. }

{ Die folgende Unit erm”glicht den Zugriff auf den Nodelistenindex }
{ von CrossPoint. Es sind alle Daten aus allen eingebundenen Node- }
{ und Pointlisten verfgbar.                        v3.1  PM 09/95 }

{ fr CrossPoint ab Version 3.1    }
{ Heapspeicherbedarf: ca. 10 KByte }
{ $Id$ }

{$I XPDEFINE.INC}

unit xpnodes;

interface

uses
  typeform, sysutils, dos;


const PointNurNode = 0;      { Nur den Nodeteil der Adresse bercksichti-  }
                             { gen, auch falls es eine Pointadresse ist    }

      PointExakt   = 1;      { exakt nach der angegebenen Adresse suchen   }

      PointSpezial = 2;      { Erst nach der angegebenen Pointadresse su-  }
                             { chen. Wenn nicht gefunden, Suche nach Node- }
                             { Adresse wiederholen.                        }


type  nodeinfo = record               { Datenstruktur fr Nodelist-Infos }
                   found    : boolean;          { Adresse wurde gefunden }
                   ispoint  : boolean;          { es war ein Point ...   }
                   boxname  : string[40];
                   standort : string[40];
                   sysop    : string[40];
                   telefon  : string[30];
                   baud     : word;
                   fflags   : string[40];       { Alles hinter der Baudrate }
                 end;


{ Nodelistenindex-Datei ”ffnen - muá einmal vor der Benutzung von   }
{ GetNodeInfo aufgerufen werden. XP_Verzeichnis kann leerbleiben,   }
{ wenn es das aktuelle Verzeichnis ist.                             }
{ Ergebnis FALSE -> FIDO\NODELST.CFG wurde nicht gefunden, ein-     }
{ getragene Node- oder Pointliste fehlt oder Nodelistenindex fehlt. }

function OpenNodelistIndex(XP_Verzeichnis:string):boolean;


{ Adr = FidoNet-Node oder Pointadresse, z.B. '2:243/97' }
{ ni  = Record fr Ergebnisdaten                        }
{ pointtyp: s.o.                                        }

procedure GetNodeinfo(adr:string; var ni:nodeinfo; pointtyp:shortint);


{ Nodeindex wieder schlieáen }

procedure CloseNodelistindex;


implementation  { ------------------------------------------------------ }

const MaxNodelists = 100;
      FidoDir      = 'FIDO\';
      NodelistCfg  = fidodir+'nodelst.cfg';
      NodeindexF   = fidodir+'nodelist.idx';

      bersize   = 200;     { Max. Netze pro Bereich }
      maxber    = 300;
      maxnodes  = 3000;    { max Nodes / Net }
      maxpoints = 700;     { max Points / Node }
      nodekenn  = 'IDX'^Z;

      nodeopen  : boolean = false;
      xpdir     : string[80] = '';


type  NL_Rec  = record
                  listfile   : string[12];    { Nodelisten-Datei      }
                  number     : integer;       { akt. Nummer           }
                  updatefile : string[12];    { Diff/Update-Datei     }
                  updatearc  : string[12];    { gepackte Update-Datei }
                  processor  : string;       { externer Bearbeiter   }
                  DoDiff     : boolean;
                  DelUpdate  : boolean;       { Diff l”schen }
                  format     : byte;     { 1=NL, 2=P24, 3=PVT, 4=4D, 5=FD }
                  zone,net,node : word;
                  sort       : longint;       { Tempor„rfeld }
                end;
      NL_array= array[1..maxNodelists] of NL_Rec;
      NL_ap   = ^NL_array;

      FidoAdr = record
                  username   : string[36];
                  zone,net   : word;
                  node,point : word;
                  ispoint    : boolean;
                end;

      noderec = record
                  node : word;
                  adr  : longint;
                end;
      nodea   = array[0..maxnodes-1] of noderec;
      pointrec= record
                  point : word;
                  adr   : longint;
                end;
      pointa  = array[0..maxpoints-1] of pointrec;
      berrec  = record             { Netzindex - Bereich }
                  fromnet  : word;
                  fromzone : word;
                  anz      : word;
                  adr      : longint;
                end;
      netrec  = record case integer of
                  0 : (net  : word;
                       zone : word;
                       anz  : word;
                       fnr  : byte;   { Datei-Nr. }
                       flags: byte;   { 1=Pointliste }
                       adr  : longint);
                  1 : (sortl : longint);
                end;
      netrecl = array[1..bersize] of netrec;
      bereichlst = array[1..maxber] of berrec;

      idxheader = record
                    kennung : array[0..3] of char;
                    beradr  : longint;    { Adresse Bereichsindex }
                    bernum  : word;       { Anzahl Bereiche       }
                    adrnetx : longint;    { Adresse Netzindex     }
                    fill    : array[0..49] of byte;
                  end;

      udxheader = record
                    kennung : array[0..3] of char;
                    anzahl  : longint;
                    blocks  : longint;
                    version : word;
                  end;

var   Nodelist    : NL_ap;          { benutzerdefinierte Node/Pointlisten }
      NL_Anz      : byte;           { Anzahl " }
      nodef       : file;
      NX_adrnetx  : longint;
      bereiche    : word;
      berliste    : ^bereichlst;

{ ------------------------------------------------------------------- }

procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);
var p1,p2,p3 : byte;
    res      : integer;
    l        : longint;
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
      net:=2;
      if p3>0 then begin
        if p3>1 then
          node:=ival(left(adr,p3-1))
        else
          node:=0;
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


function NLfilename(n:integer):string;
var p : byte;
begin
  if (n<1) or (n>NL_anz) then
    NLfilename:=''
  else
    with Nodelist^[n] do begin
      p:=pos('###',listfile);
      if p=0 then
        NLfilename:=listfile
      else
        NLfilename:=left(listfile,p-1)+formi(number,3)+mid(listfile,p+3);
      end;
end;


function OpenNodeindex(fn:string):boolean;
var hd  : idxheader;
    uhd : udxheader;
    rr  : word;
    f   : file;
begin
  assign(nodef,fn);
  reset(nodef,1);
  fillchar(hd,sizeof(hd),0);
  blockread(nodef,hd,sizeof(hd),rr);
  if (hd.kennung<>nodekenn) or (hd.beradr>=filesize(nodef)) then begin
    OpenNodeindex:=false;
    close(nodef);
    end
  else begin
    nx_adrnetx:=hd.adrnetx;
    bereiche:=hd.bernum;
    getmem(berliste,bereiche*sizeof(berrec));
    seek(nodef,hd.beradr);
    blockread(nodef,berliste^,bereiche*sizeof(berrec));
    OpenNodeindex:=true;
    end;
end;


Function exist(n:string):boolean;
var sr : searchrec;
    ex : boolean;
begin
  Dos.findfirst(n,anyfile-volumeid-directory,sr);
  ex:=(doserror=0);
  while not ex and (doserror=0) do begin
    Dos.findnext(sr);
    ex:=(doserror=0);
  end;
  FindClose(sr);
  exist:=ex;
end;


function OpenNodelistIndex(XP_Verzeichnis:string):boolean;
var t  : text;
    s  : string;
    ss : string[20];
    p  : byte;
    ul : NL_ap;
    fa : FidoAdr;
    ok : boolean;
label abbruch;
begin
  if nodeopen then exit;
  xpdir:=XP_Verzeichnis;
  if (xpdir<>'') and (xpdir[length(xpdir)]<>'\') then
    xpdir:=xpdir+'\';
  NL_anz:=0;
  new(nodelist);
  assign(t,xpdir+NodelistCfg);
  reset(t);
  ok:=(ioresult=0);
  if ok then begin
    while not eof(t) do begin
      inc(NL_anz);
      with nodelist^[NL_anz] do begin
        repeat
          readln(t,s);
          p:=cpos('=',s);
          if p>0 then begin
            ss:=lstr(left(s,p-1));
            s:=mid(s,p+1);
            if ss='listfile'       then listfile:=s else
            if ss='number'         then number:=minmax(ival(s),0,999) else
            if ss='updatefile'     then updatefile:=s else
            if ss='delupdate'      then delupdate:=(ustr(s)='J') else
            if ss='updatearchive'  then updatearc:=s else
            if ss='process-by'     then processor:=s;
            if ss='dodiff'         then dodiff:=(ustr(s)='J') else
            if ss='format'         then format:=minmax(ival(s),0,6) else
            if ss='zone'           then zone:=minmax(ival(s),0,32767) else
            if ss='address'        then begin
              SplitFido(s,fa,2);
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              end;
            end;
        until eof(t) or (s='');
        if (format<1) or (format>5) then
          dec(NL_anz);
        end;  { with }
      end;  { while }
    close(t);
    end;

  if not exist(xpdir+NodeindexF) then ok:=false;
  ok:=ok and OpenNodeindex(xpdir+nodeindexf);
  if not ok then dispose(nodelist);
  nodeopen:=ok;
  opennodelistindex:=ok;
end;


procedure closenodelistindex;
begin
  if nodeopen then begin
    close(nodef);
    freemem(berliste,bereiche*sizeof(berrec));
    dispose(nodelist);
    nodeopen:=false;
    end;
end;


{ XPFIDO - Nodeliste auslesen/abfragen }


procedure ReadNData(nfile:byte; adr:longint; var ni:NodeInfo);
var s      : string;
    nodelf : ^file;
    rr     : word;
    mfm    : byte;

label ende;

  procedure SetInfo;
  var p : byte;
      x : string[10];
    function getstr:string;
    begin
      p:=cpos(',',s^);
      if p=0 then getstr:=''
      else begin
        getstr:=copy(s^,1,p-1);
        delete(s^,1,p);
        end;
    end;
  begin
    with ni do begin
      if s^[length(s^)]<>',' then s^:=s^+',';
      for p:=1 to length(s^) do
        if s^[p]='_' then s^[p]:=' ';
      p:=cpos(',',s^);
      delete(s^,1,p);
      p:=cpos(',',s^);
      if p>0 then begin
        delete(s^,1,p);
        boxname:=getstr;
        standort:=getstr;
        sysop:=getstr;
        telefon:=getstr;
        baud:=ival(getstr);
        fflags:=s^;
        dellast(fflags);    { Komma entfernen }
        end;
      end;
  end;

begin
  ni.found:=false;
  if nfile>NL_anz then exit;
  new(s);
  new(nodelf);
  assign(nodelf^,xpdir+FidoDir+NLfilename(nfile));
  mfm:=filemode; filemode:=0;
  reset(nodelf^,1);
  filemode:=mfm;
  if ioresult=0 then begin
    seek(nodelf^,adr);
    blockread(nodelf^,s^[1],255,rr);
    s^[0]:=chr(rr);
    s^[0]:=chr(cpos(#13,s^)-1);
    SetInfo;
    ni.found:=true;
    close(nodelf^);
    end;
  dispose(nodelf);
  dispose(s);
end;


procedure CloseNodeindex;
begin
  freemem(berliste,bereiche*sizeof(berrec));
end;



{ Pointtyp: 0=nur Node, 1=Point/Node, 2=bei nicht gef. Point wiederholen }

procedure GetNodeinfo(adr:string; var ni:nodeinfo; pointtyp:shortint);
var fa     : fidoadr;
    i,netp : integer;
    bp     : ^netrecl;
    banz   : word;
    nanz   : word;
    nadr,l : longint;
    nfile  : byte;
    np     : ^nodea;
    s      : string;
    found  : boolean;
    _adr   : longint;
    points : integer;
    pp     : ^pointa;

label again;

begin
  fillchar(ni,sizeof(ni),0);
  if not nodeopen then exit;
  splitfido(adr,fa,2);
  if pointtyp=0 then fa.ispoint:=false;
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
    banz:=berliste^[i].anz;
    if banz>bersize then
      writeln('Fehler in Nodelisten-Index!'#7);
    blockread(nodef,bp^,banz*sizeof(netrec));
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
        nfile:=bp^[netp].fnr;
        end;
      if found and (fa.ispoint=odd(bp^[netp].flags)) then begin
        getmem(np,nanz*sizeof(noderec));
        seek(nodef,nadr);
        blockread(nodef,np^,nanz*sizeof(noderec));
        i:=0;
        while (i<nanz) and (np^[i].node<fa.node) do
          inc(i);
{$IFDEF Debug }
  {$R+}
{$ENDIF }
        if (i<nanz) and (np^[i].node=fa.node) then
          _adr:=np^[i].adr
        else
          _adr:=-1;
        freemem(np,nanz*sizeof(noderec));
        if (_adr>=0) and fa.ispoint then begin
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
        if _adr>=0 then
          ReadNData(nfile,_adr,ni);
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
end;


end.
{
  $Log$
  Revision 1.10  2000/07/13 10:23:48  mk
  - Zeiger auf Strings entfernt

  Revision 1.9  2000/07/04 10:21:36  mk
  - doppelte Routinen rausgenommen

  Revision 1.8  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.7  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.6  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.5  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}

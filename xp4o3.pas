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

{$I XPDEFINE.INC}

unit xp4o3;

interface

uses dos,typeform,fileio,datadef,database,inout,keys,resource,
     sysutils,xp0,xp1,xp1input, xpglobal;

function  __getfilename(nr,nn:byte):string;
function  go_pm:boolean;
procedure Smenu(var t:taste);
function  readmsg_getfilename:pathstr;
function  GetWABreplyEmpfaenger(var realname:string):string;
procedure ReadXpostEmpfaenger(pm:boolean; var empf:adrstr; var brk:boolean);


implementation  { --------------------------------------------------- }

uses xp3,xp3ex,xp4,xp6,xpnt,xpkeys,xpcc;


function __getfilename(nr,nn:byte):string;
var fn   : pathstr;
    i    : integer;
    betr : string[betrefflen];

  procedure setfn;
  var tf : pathstr;
      d  : dirstr;
      n  : namestr;
      e  : extstr;
  begin
    tf:=TempS(dbReadInt(mbase,'msgsize')+5000);
    fsplit(tf,d,n,e);
    if fn='' then
      fn:=d+msgtempfile
      else fn:=d+fn;
    if cpos('.',fn)=0 then fn:=fn+'.';
  end;

begin
  fn:='';
  with fkeys[nr]^[nn] do begin
    if bname then begin
      dbReadN(mbase,mb_betreff,betr);
      UpString(betr);
      i:=1;
      while (i<=length(betr)) and
            (betr[i] in ['A'..'Z','_','-','é','ô','ö','#','@','$','!','0'..'9','\']) do
        inc(i);
      fn:=left(betr,min(i-1,iif(cpos('.',fn)>0,12,8)));
      end;
    setfn;
    extract_msg(ntyp,iifs(ntyp=3,QuoteMsk,''),fn,false,1);
    end;
  __getfilename:=fn;
end;



function go_pm:boolean;
var brett : string[BrettLen];
    ok    : boolean;
begin
  dbSeek(bbase,biBrett,'1');
  if not dbBOF(bbase) and not dbEOF(bbase) then begin
    ok:=false;
    repeat
      dbRead(bbase,'brettname',brett);
      if brett[1]='1' then begin
        dbSkip(bbase,-1);    { Index auf Brettindex-Feld }
        ok:=true;
        end;
    until dbBOF(bbase) or (brett[1]<>'1');
    if dbBOF(bbase) then dbGoTop(bbase)
    else dbNext(bbase);
    go_pm:=ok;
    end
  else
    go_pm:=false;
end;


procedure Smenu(var t:taste);
const sm_pos : byte = 1;        { Position im Spezial-MenÅ }
var   n,x,y  : shortint;
      s      : string;
begin
  x:=iif(mauskey,60,31);
  y:=iif(mauskey,4,9+(screenlines-25)div 2);
  case aktdispmode of
    11 : s:=getres2(23,2);
    12 : s:=getres2(23,2);
  else   s:=getres2(23,2)+getres2(23,3);
  end;
  freeres;
  n:=minisel(x,y,'',s,sm_pos);
{
  n:=MiniSel(x,y,'',
             '^EditUser    ^U,^Rot13       ^R,^Hex-Dump    ^H,^Datum       ^D,'+
             '^Ausdruck     R'+iifs(aktdispmode=11,'',',^Markieren,'+
             'Wieder^vlg.   V,^Textdatei   ^F,B^inÑrdatei  ^I,^UserSuche   @U'+
             iifs(aktdispmode<>12,',^BetrSuche   @B','')),
             sm_pos);
}
  if n<>0 then
    sm_pos:=abs(n);
  if n>0 then
    case n of
      1 : t:=k2_cU;
      2 : t:=k2_cR;
      3 : t:=k2_cH;
      4 : t:=k2_cD;
      5 : t:=k2_R;
      6 : t:=' ';
      7 : t:=k2_V;
      8 : t:=k2_cF;
      9 : t:=k2_cI;
     10 : t:=k2_cQ;
     11 : t:=keyaltu;
     12 : t:=keyaltb;
    end;
end;


function readmsg_getfilename:pathstr;
var fn  : pathstr;
    hdp : headerp;
    hds : longint;
begin
  new(hdp);
  ReadHeader(hdp^,hds,false);
  fn:=hdp^.betreff;
  dispose(hdp);
  if not multipos('\:',fn) then fn:=FilePath+fn;
  readmsg_getfilename:=fn;
end;


function GetWABreplyEmpfaenger(var realname:string):string;
const maxadr = 10;
var hdp : headerp;
    hds : longint;
    abs : string[AdrLen];
    s   : string;
    nr  : shortint;
    wabok: boolean;
    anz : integer;
    adra: array[1..maxadr] of ^adrstr;
    resn: array[1..maxadr] of integer;
    i   : integer;
{    size: word; }

  procedure appadr(adr:string; nr:integer);
  begin
    if anz<maxadr then begin
      inc(anz);
      new(adra[anz]);
      adra[anz]^:=adr;
      resn[anz]:=nr;
      end;
   end;

begin
  new(hdp);
  ReadHeader(hdp^,hds,false);
  { 03.02.2000 robo }
  if (hdp^.PmReplyTo<>'') and not askreplyto then
  { /robo }
    abs:=hdp^.PmReplyTo
  else begin
    wabok:=(pos('.',mid(hdp^.wab,cpos('@',hdp^.wab)))<>0);
    { 03.02.2000 robo }
    if (hds=1) or ((hdp^.wab='') and (hdp^.oem='') and (hdp^.PmReplyTo='')) or
                  ((hdp^.wab='') and (hdp^.oem=hdp^.vertreter) and (hdp^.PmReplyTo='')) or
                  (not wabok and (hdp^.oem='') and (hdp^.PmReplyTo=''))
    { /robo }
    then begin
      dbReadN(mbase,mb_absender,abs);
      realname:=hdp^.realname;
      end
    else begin
      anz:=0;
      { 03.02.2000 robo }
      if hdp^.PmReplyTo<>'' then
        appadr(hdp^.PmReplyTo,7);                    { 'Reply-To-EmpfÑnger :' }
      { /robo }
      if hdp^.wab<>'' then appadr(hdp^.absender,1)   { 'Original-Absender  :' }
      else appadr(hdp^.absender,5);                  { 'Absender           :' }
      if wabok then
        appadr(hdp^.wab,2);                          { 'Weiterleit-Absender:' }
      if hdp^.oem<>'' then
        appadr(hdp^.oem,3);                          { 'Original-EmpfÑnger :' }
    (*
      dbSeek(ubase,uiName,UpperCase(hdp^.absender));
      if dbFound and (dbXsize(ubase,'adresse')>0) then begin
        size:=0;
        dbReadX(ubase,'adresse',size,s);
        appadr(s,6);                                 { 'Vertreter          :' }
        end;
    *)
      s:=getres2(476,resn[1])+' '+left(adra[1]^,50);
      for i:=2 to anz do
        s:=s+','+getres2(476,resn[i])+' '+left(adra[i]^,50);
      nr:=minisel(0,0,getres2(476,4),s,1);           { 'EmpfÑnger wÑhlen ...' }
      freeres;
      if (nr>=1) and (nr<=anz) then
        abs:=adra[nr]^
      else
        abs:='';
      for i:=1 to anz do
        dispose(adra[i]);
      end;
    end;
  dispose(hdp);
  GetWABreplyEmpfaenger:=abs;
end;


procedure ReadXpostEmpfaenger(pm:boolean; var empf:adrstr; var brk:boolean);
var i,n    : integer;
    s0,                            { Server der 1. Kopie }
    server : string[BoxNameLen];
    d      : DB;
    ok     : boolean;
    e      : AdrStr;
    p      : empfnodep;

  procedure TestServer;   { Crossposting bei diesem Server erlaubt? }
  begin
    dbSeek(d,boiName,UpperCase(server));
    if not dbFound then
      ok:=false
    else begin
      ok:=(pm and ntCrossPM(dbReadInt(d,'netztyp'))) or
          (not pm and ntCrossAM(dbReadInt(d,'netztyp')));
      if not ok then rfehler1(480,left(e,40));   { '%s: Crossposting ist hier nicht erlaubt.' }
      brk:=(lastkey=keyesc);
      end;
  end;

begin
  dbOpen(d,BoxenFile,1);
  s0:='';
  ok := true; { !! MK 12/99 }
  brk:=false; n:=0;
  for i:=0 to bmarkanz-1 do begin
    if pm then begin
      dbGo(ubase,bmarked^[i]);
      dbReadN(ubase,ub_username,e);
      if dbReadInt(ubase,'userflags') and 4<>0 then begin
        rfehler1(483,vert_name(e));   { 'Crossposting an Verteiler ist nicht mîglich.' }
        ok:=false;
        end
      else begin
        dbReadN(ubase,ub_pollbox,server);
        TestServer;
        end;
      end
    else begin
      dbGo(bbase,bmarked^[i]);
      dbReadN(bbase,bb_brettname,e);
      dbReadN(bbase,bb_pollbox,server);
      if (e[1]<>'A') or (server='') or (dbReadInt(bbase,'flags')and 8<>0) then
      begin
        rfehler1(482,copy(e,2,40));     { '%s: Schreibzugriff gesperrt!' }
        ok:=false;
        end
      else begin
        delfirst(e);
        TestServer;
        end;
      end;
    if ok then
      if s0='' then s0:=UpperCase(server)
      else if s0<>UpperCase(server) then begin
        rfehler(481);   { 'Crosspostings sind nur innerhalb eines Servers mîglich!' }
        ok:=false;
        brk:=true;
        end;
    if brk then break;    { for-Schleife verlassen }
    if ok and ((pm and (n<maxcc)) or (not pm and (n<MaxXposts))) then begin
      AddToEmpflist(e);
      inc(n);
      end;
    end;
  dbClose(d);
  if empflist=nil then brk:=true;
  if not brk then begin
    empf:=iifs(pm,'','A')+empflist^.empf;
    p:=empflist^.next;
    dispose(empflist); empflist:=nil;
    sendempflist:=p;
    xp6.forcebox:=s0;
    end
  else
    DisposeEmpflist(empflist);
end;


end.
{
  $Log$
  Revision 1.8  2000/07/04 12:04:25  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.7  2000/06/29 13:00:57  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.6  2000/05/04 10:42:59  mk
  - Unbenutze Units aus uses entnommen

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

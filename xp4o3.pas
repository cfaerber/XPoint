{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp4o3;

interface

uses dos,typeform,fileio,datadef,database,inout,keys,resource,
     xp0,xp1,xp1input, xpglobal, lfn;

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
      dbSeek(ubase,uiName,ustr(hdp^.absender));
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
    server : string[BoxNameLen];
    d      : DB;
    ok     : boolean;
    e,s    : AdrStr;
    p      : empfnodep;
    t      : text;

begin
  dbOpen(d,BoxenFile,1);
  ok := true;
  brk:=false; n:=0;
  for i:=0 to bmarkanz-1 do begin
    if pm then begin
      dbGo(ubase,bmarked^[i]);
      dbReadN(ubase,ub_username,e);
      dbReadN(ubase,ub_pollbox,server);
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
      else delfirst(e);
      end;
    if ok and ((pm and (n<maxcc)) or (not pm and (n<MaxXposts))) then begin

      if firstchar(e)=vert_char then
      begin
        e:=copy(e,2,cpos('@',e)-2);                       { nach Verteilernamen suchen }
        assign(t,CCfile);
        reset(t);
        if ioresult=0 then
        begin
          repeat
            readln(t,s)
          until eof(t) or (ustr(s)=ustr(e));
          if not eof(t) then                                   { wenn gefunden... }
          begin
            repeat
              readln(t,s);                                    { auslesen und anhaengen }
              if (trim(s)<>'') and not ((firstchar(s)='[') and (lastchar(s)=']'))
              then begin
                AddToEmpflist(s);
                inc(n);
                end;
            until eof(t) or ((firstchar(s)='[') and (lastchar(s)=']'));
            end;
          close(t);
          end;
        end

      else begin
        AddToEmpflist(e);
        inc(n);
        end;
      end;
    end; {For...}

  dbClose(d);
  if empflist=nil then brk:=true;
  if not brk then begin
    empf:=iifs(pm,'','A')+empflist^.empf;
    p:=empflist^.next;
    dispose(empflist); empflist:=nil;
    sendempflist:=p;
    xp6.forcebox:='';
    end
  else
    DisposeEmpflist(empflist);
end;


end.
{
  $Log$
  Revision 1.6.2.2  2000/10/15 09:28:07  mk
  - LFN fixes

  Revision 1.6.2.1  2000/07/25 16:45:07  jg
  - Crosspostings mit Shift+B: Beschraenkungen bei Verteilern, verschiedenen
    Serverboxen, und anderen Netzen als Zconnect und RFC aufgehoben.

  Revision 1.6  2000/05/04 10:42:59  mk
  - Unbenutze Units aus uses entnommen

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

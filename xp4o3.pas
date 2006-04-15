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

{$I xpdefine.inc}

unit xp4o3;

interface

uses typeform,fileio,datadef,inout,keys,resource,
     sysutils,xp0,xp1,xp1input, xpglobal;

function  __getfilename(nr,nn:byte):string;
function  go_pm:boolean;
procedure Smenu(var t:taste);
function  readmsg_getfilename:string;
function  GetWABreplyEmpfaenger(var realname:string):string;
procedure ReadXpostEmpfaenger(pm:boolean; var empf:adrstr; var brk:boolean);


implementation  { --------------------------------------------------- }

uses xpheader, xpmakeheader, xp3,xp3ex,xp4,xpsendmessage,xpnt,xpkeys,xpcc,
  database;


function __getfilename(nr,nn:byte):string;
var fn   : string;
    i    : integer;
    betr : string;

  procedure setfn;
  var tf : string;
      d, n, e: String;
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
  with fkeys[nr][nn] do begin
    if bname then begin
      betr:= UpperCase(dbReadNStr(mbase,mb_betreff));
{$ifdef FPC}
{$ifdef UnixFS}
{$hint Anpassung an Schreibweise des Dateinamen erforderlich }
{$endif}
{$endif}
      i:=1;
      while (i<=length(betr)) and
            (betr[i] in ['A'..'Z','_','-','é','ô','ö','#','@','$','!','0'..'9','\']) do
        inc(i);
      fn:=LeftStr(betr,min(i-1,iif(cpos('.',fn)>0,12,8)));
      end;
    setfn;
    extract_msg(ntyp,iifs(ntyp=3,QuoteMsk,''),fn,false,1);
    end;
  __getfilename:=fn;
end;



function go_pm:boolean;
var brett : string;
    ok    : boolean;
begin
  dbSeek(bbase,biBrett,'1');
  if not dbBOF(bbase) and not dbEOF(bbase) then begin
    ok:=false;
    repeat
      brett:= dbReadStrN(bbase,bb_brettname);
      if (length(brett)>0) and (brett[1]='1') then begin
        dbSkip(bbase,-1);    { Index auf Brettindex-Feld }
        ok:=true;
        end;
    until dbBOF(bbase) or ((length(brett)>0) and (brett[1]<>'1'));
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


function readmsg_getfilename:string;
var fn  : string;
    hdp : THeader;
    hds : longint;
begin
  hdp:= THeader.Create;
  ReadHeader(hdp,hds,false);
  fn:=hdp.betreff;
  hdp.Free;
  if not multipos(_MPMask,fn) then fn:=FilePath+fn;
  readmsg_getfilename:=fn;
end;


function GetWABreplyEmpfaenger(var realname:string):string;
const maxadr = 10;
var hdp : THeader;
    hds : longint;
    abs : string;
    s   : string;
    nr  : shortint;
    wabok: boolean;
    anz : integer;
    adra: array[1..maxadr] of string;
    resn: array[1..maxadr] of integer;
    i   : integer;
{    size: word; }

  procedure appadr(adr:string; nr:integer);
  begin
    if anz<maxadr then begin
      inc(anz);
      {new(adra[anz]);}
      adra[anz]:=adr;
      resn[anz]:=nr;
      end;
   end;

begin
  hdp:= THeader.Create;
  ReadHeader(hdp,hds,false);
  if (hdp.replyto <> '') and not askreplyto then
    abs:=hdp.replyto
  else begin
    wabok:=(cPos('.',mid(hdp.wab,cpos('@',hdp.wab)))<>0);
    if (hds=1) or ((hdp.wab='') and (hdp.oem.Count = 0) and (hdp.replyto <> '')) or
                  ((hdp.wab='') and (hdp.oem.count > 0) and (hdp.oem[0]=hdp.vertreter) and (hdp.replyto <> '')) or
                  (not wabok and (hdp.oem.count = 0) and (hdp.replyto = ''))
    then begin
      abs:= dbReadNStr(mbase,mb_absender);
      realname:=hdp.realname;
      end
    else begin
      anz:=0;
      if hdp.replyto <> '' then
        appadr(hdp.replyto,7);                   {'Reply-To-Empfaenger :' }
      if hdp.wab<>'' then appadr(hdp.absender,1)    { 'Original-Absender  :' }
      else appadr(hdp.absender,5);                  { 'Absender           :' }
      if wabok then
        appadr(hdp.wab,2);                          { 'Weiterleit-Absender:' }
      if hdp.oem.Count > 0 then
        appadr(hdp.oem[0],3);                          { 'Original-Empfaenger :' }
    (*
      dbSeek(ubase,uiName,UpperCase(hdp.absender));
      if dbFound and (dbXsize(ubase,'adresse')>0) then begin
        size:=0;
        s:= dbReadXStr(ubase,'adresse',size);
        appadr(s,6);                                 { 'Vertreter          :' }
        end;
    *)
      s:=getres2(476,resn[1])+' '+LeftStr(adra[1],50);
      for i:=2 to anz do
        s:=s+','+getres2(476,resn[i])+' '+LeftStr(adra[i],50);
      nr:=minisel(0,0,getres2(476,4),s,1);           { 'Empfaenger waehlen ...' }
      freeres;
      if (nr>=1) and (nr<=anz) then
        abs:=adra[nr]
      else
        abs:='';
      end;
    end;
  Hdp.Free;
  GetWABreplyEmpfaenger:=abs;
end;


procedure ReadXpostEmpfaenger(pm:boolean; var empf:adrstr; var brk:boolean);
var i,n    : integer;
    server : string;
    d      : DB;
    ok     : boolean;
    e,s    : string;
    t:     Text;

begin
  dbOpen(d,BoxenFile,1);
  ok := true;
  brk:=false; n:=0;
  for i:=0 to bmarkanz-1 do begin
    if pm then begin
      dbGo(ubase,bmarked^[i]);
      e:= dbReadNStr(ubase,ub_username);
      server := dbReadNStr(ubase,ub_pollbox);
    end
    else begin
      dbGo(bbase,bmarked^[i]);
      e:= dbReadNStr(bbase,bb_brettname);
      server:= dbReadNStr(bbase,bb_pollbox);
      if (FirstChar(e)<>'A') or (server='') or (dbReadInt(bbase,'flags')and 8<>0) then
      begin
        rfehler1(482,copy(e,2,40));     { '%s: Schreibzugriff gesperrt!' }
        ok:=false;
        end
      else DeleteFirstChar(e);
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
          until eof(t) or (UpperCase(s)=UpperCase(e));
          if not eof(t) then                                   { wenn gefunden... }
          begin
            repeat
              readln(t,s);                                    { auslesen und anhaengen }
              if (trim(s)<>'') and not ((firstchar(s)='[') and (lastchar(s)=']'))
              then begin
                SendEmpfList.Add(s);
                inc(n);
              end;
            until eof(t) or ((firstchar(s)='[') and (lastchar(s)=']'));
          end;
          close(t);
        end;
      end else
      begin
        SendEmpfList.Add(e);
        inc(n);
      end;
    end;
  end; {for }
  dbClose(d);

  if SendEmpfList.Count = 0 then brk:=true;

  if not brk then
  begin
    empf:=iifs(pm,'','A')+SendEmpflist[0];
    SendEmpfList.Delete(0);
    xpsendmessage.forcebox:='';
  end else
    SendEmpfList.Clear;
end;

end.

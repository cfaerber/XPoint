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

{ Overlay-Teile zu XP3 }

{$I xpdefine.inc}

unit xp3o2;

interface

uses sysutils,classes,typeform,database,resource,xp0,xpsendmessage,
     xpheader,xpglobal;

procedure WriteHeader(var hd:theader; var f:file);
procedure SetBrettindex;
procedure SetBrettindexEnde;
procedure makebrett(s:string; var n:longint; const box:string; netztyp:byte;
                    order_ende:boolean);
procedure get_bezug(pm:boolean; var repto:string; var reptoanz:integer;
                    var betreff:string; sdata: TSendUUData;
                    indirectquote:boolean);
procedure SetUngelesen;
function  UserNetztyp(adr:string):byte;


implementation  { ---------------------------------------------------- }

uses xp3,xp3o,xp4,xp4e,xpnt,xpmakeheader,mime;

procedure WriteHeader(var hd:theader; var f:file);
var s:TMemoryStream;
begin
  s:=TMemoryStream.Create;
  hd.WriteToStream(s);
  blockwrite(f,PChar(s.Memory)^,s.Size);
  s.Free;
end;

{ aufrufen nach jedem dbAppend(bbase): }

procedure SetBrettindex;
var bi  : shortint;
    nr  : longint;
    nr1,nr2 : longint;
    rec : longint;
label again;

  procedure neu;
  begin
    ReorgBrettindex;
    dbSetIndex(bbase,biBrett);
    dbGo(bbase,rec);
  end;

begin
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biBrett);
  rec:=dbRecno(bbase);
again:
  dbSkip(bbase,1);
  if dbEOF(bbase) then begin
    dbGoEnd(bbase);     { springt auf neuen Datensatz.. }
    dbSkip(bbase,-1);
    if dbBOF(bbase) then
      nr:=10000
    else begin
      dbReadN(bbase,bb_index,nr1);
      if nr1=0 then nr:=10000
      else begin
        dbSetIndex(bbase,biIndex);
        dbSkip(bbase,1);
        if dbEOF(bbase) then nr:=nr1+100
        else begin
          nr:=(dbReadInt(bbase,'index')+nr1)div 2;
          if nr=nr1 then begin
            neu;
            goto again;
            end;
          end;
        end;
      end;
    end
  else begin
    dbReadN(bbase,bb_index,nr2);
    dbSetIndex(bbase,biIndex);
    dbSkip(bbase,-1);
    dbReadN(bbase,bb_index,nr1);
    if nr1=0 then
      if nr2>100 then
        nr:=nr2-100
      else begin
        neu;
        goto again;
        end
    else begin
      nr:=(nr1+nr2) div 2;
      if nr=nr1 then begin
        neu;
        goto again;
        end;
      end;
    end;
  dbSetIndex(bbase,bi);
  dbGo(bbase,rec);
  dbWriteN(bbase,bb_index,nr);
end;


procedure SetBrettindexEnde;
var mi  : byte;
    rec : longint;
    nr  : longint;
begin
  rec:=dbRecno(bbase);
  mi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biIndex);
  dbGoEnd(bbase);
  if dbEOF(bbase) then
    nr:=10000
  else
    nr:=dbReadInt(bbase,'index')+100;
  dbSetIndex(bbase,mi);
  dbGo(bbase,rec);
  dbWriteN(bbase,bb_index,nr);
end;


procedure makebrett(s:string; var n:longint; const box:string; netztyp:byte;
                    order_ende:boolean);
var komm  : string;
    p     : byte;
begin
  s:=trim(s);
  if s[2]=' ' then s:=trim(copy(s,3,80));
  if s<>'' then 
  begin
    if s[1]<>'/' then s:='/'+s;
    s:='A'+s;
    komm:='';
    p:=cpos(' ',s);
    if p=0 then p:=cpos(#9,s);  { TAB }
    if p>0 then begin
      komm:=LeftStr(trim(Mid(s,p)),30);
      s:=LeftStr(s,p-1);
      if komm='No' then komm:='';
      end;
    { UpString(s); }
    dbSeek(bbase,biBrett,UpperCase(s));
    if not dbFound then 
    begin
      inc(n);
      AddNewBrett(s, iifs(BrettKomm, komm, ''), box, 
        StdHalteZeit, NetzGruppe, iif(netztyp in netsRFC,16,0)); 
      if order_ende and NewbrettEnde then
        SetBrettindexEnde
      else
        SetBrettindex;
      end
    else 
      if komm<>'' then
        dbWriteNStr(bbase,bb_kommentar,komm);
  end;
end;


procedure get_bezug(pm:boolean; var repto:string; var reptoanz:integer;
                    var betreff:string; sdata: TSendUUData;
                    indirectquote:boolean);
var hd : THeader;
    hds : longint;
    p   : integer;
begin
  hd := THeader.Create;
  ReadHeader(hd,hds,false);
  betreff:=hd.betreff;
  if betreff='' then betreff:=getres(343);    { '<kein Betreff>' }
  with hd do
  begin
    xpsendmessage._bezug:=msgid;
    xpsendmessage._orgref:=org_msgid;
    xpsendmessage._beznet:=netztyp;
    xpsendmessage._pmReply:=pm and (cpos('@', FirstEmpfaenger)=0);
    if netztyp=nt_Maus then
    begin
      xpsendmessage._ReplyPath:=pfad;
      if cpos('@',hd.FirstEmpfaenger)=0 then
        sData.ReplyGroup:=FirstEmpfaenger;
    end;
    p:=cpos('@',absender);
    if p=0 then p:=length(absender)+1;
    if netztyp in (netsRFC + [nt_ZConnect]) then
      if hd.fido_to<>'' then xp0.fidoto:=realname
      else xp0.fidoto:=''
    else begin
      if indirectquote and (hd.fido_to<>'') then
        xp0.fidoto:=hd.fido_to
      else
        xp0.fidoto:=LeftStr(absender,minmax(p-1,0,35));
      if (netztyp=nt_Fido) and (cpos('#',xp0.fidoto)>0) then
        xp0.fidoto:=realname;
      end;
    reptoanz:=0;

    if pm then
    begin
      repto := ReplyTo;
      reptoanz := 0;
    end
    else
      if (Followup.count=0) or ((Empfaenger.Count = 0) and (followup.count > 0) and (FirstEmpfaenger=followup[0])) then
        repto:=''
      else
      begin
        if Followup.count > 0 then
          repto:='A'+followup[0]
        else repto := '';
           reptoanz:=followup.count;
        end;
    if not pm then
      sData.References.Assign(hd.References);
    sData.keywords:=keywords;
    sData.distribute:=distribution;
    end;
  hd.Free;
end;


procedure SetUngelesen;     { akt. Nachricht auf 'ungelesen' }
var rec : longint;
    b   : byte;
begin
  rec:=dbRecno(mbase);
  b:=0;
  dbWriteN(mbase,mb_gelesen,b);
  RereadBrettdatum(dbReadStrN(mbase,mb_brett));
  dbGo(mbase,rec);
  setbrettgelesen(dbReadStrN(mbase,mb_brett));
  dbGo(mbase,rec);
end;


function UserNetztyp(adr:string):byte;
begin
  dbSeek(ubase,uiName,UpperCase(adr));
  if not dbFound then
    UserNetztyp:=0
  else
    UserNetztyp:=ntBoxNetztyp(dbReadStrN(ubase,ub_pollbox));
end;

end.

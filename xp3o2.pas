{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

uses sysutils,classes, typeform,datadef,database,resource,xp0,xpsendmessage,xpheader,
  xpglobal,xp1;

procedure WriteHeader(var hd:theader; var f:file);
procedure SetBrettindex;
procedure SetBrettindexEnde;
procedure makebrett(s:string; var n:longint; const box:string; netztyp:byte;
                    order_ende:boolean);
procedure get_bezug(pm:boolean; var repto:string; var reptoanz:integer;
                    var betreff:string; sdata:SendUUptr;
                    indirectquote:boolean);
procedure SetUngelesen;
function  UserNetztyp(adr:string):byte;


implementation  { ---------------------------------------------------- }

uses xp3,xp3o,xp4, xp4e, xpnt,xpdatum,xp_pgp, xpmakeheader, mime;

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
                    var betreff:string; sdata:SendUUptr;
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
    xpsendmessage._pmReply:=pm and (cpos('@',empfaenger)=0);
    if netztyp=nt_Maus then begin
      xpsendmessage._ReplyPath:=pfad;
      if cpos('@',hd.empfaenger)=0 then
        sData^.ReplyGroup:=empfaenger;
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
      if (followup.count=0) or
         ((empfanz=1) and (followup.count > 0) and (empfaenger=followup[0])) then repto:=''
         else
         begin
           if Followup.count > 0 then repto:='A'+followup[0]
             else repto := '';
           reptoanz:=followup.count;
         end;
    if not pm then
      sData^.References.Assign(hd.References);
    sData^.keywords:=keywords;
    sData^.distribute:=distribution;
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

{
  $Log$
  Revision 1.51  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.50  2001/09/08 14:29:08  cl
  - THeader can now write itsself to streams

  Revision 1.49  2001/09/07 09:17:56  mk
  - added AddNewBrett procedure

  Revision 1.48  2001/08/27 09:13:42  ma
  - changes in net type handling (1)

  Revision 1.47  2001/08/23 11:15:02  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.46  2001/08/12 20:01:39  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.45  2001/08/12 11:50:38  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.44  2001/08/11 21:20:51  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.43  2001/07/29 12:58:16  ma
  - fixed setting of NNTP area db flags

  Revision 1.42  2001/07/27 18:10:12  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.41  2001/07/20 14:41:47  ma
  - fixed: X-No-Archive did not work with NNTP

  Revision 1.40  2001/06/12 21:22:27  my
  - added more meaningful description for "ungelesen-fix" of 01/05/23

  Revision 1.39  2001/05/23 10:30:49  mk
  JG:- ungelesen-fix
  (Amended description by my: Unread flag of /Netcall ("/Netzanruf")
  message area should now always be set correctly => "Update date
  entries after netcall" under C/O/C should not be necessary anymore.)

  Revision 1.38  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.37  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.36  2001/01/14 10:13:34  mk
  - MakeHeader() integreated in new unit

  Revision 1.35  2001/01/05 09:33:09  mk
  - removed THeader.Ref

  Revision 1.34  2001/01/02 10:05:25  mk
  - implemented Header.References

  Revision 1.33  2000/12/30 15:59:27  mk
  - fixed another Bug from Frank Ellert in Get_bezug

  Revision 1.32  2000/12/11 11:00:48  mk
  - fixed some of Frank Ellerts header changes

  Revision 1.31  2000/12/03 12:38:22  mk
  - Header-Record is no an Object

  Revision 1.30  2000/11/25 18:28:31  fe
  Fixed some bugs.

  Revision 1.29  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.28  2000/11/24 09:40:11  mk
  - fixed Franks suboptimal changes :(

  Revision 1.27  2000/11/23 22:33:22  fe
  Fixed some ugly bugs with followup and replyto.

  Revision 1.26  2000/11/19 22:26:44  mk
  - fixed crash in followup[0] with empty stringlist

  Revision 1.25  2000/11/18 00:04:44  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.24  2000/11/09 18:15:12  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.23  2000/10/17 10:05:50  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.22  2000/10/10 13:58:58  mk
  RB:- Ersetzt-Nachrichten in Autoversand

  Revision 1.21  2000/09/06 21:31:01  fe
  /home/fe/foo

  Revision 1.20  2000/07/22 14:05:27  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.19  2000/07/21 21:17:45  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.18  2000/07/21 20:56:24  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.17  2000/07/21 17:39:53  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.16  2000/07/21 13:23:46  mk
  - Umstellung auf TStringList

  Revision 1.15  2000/07/20 16:49:59  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.14  2000/07/09 08:35:15  mk
  - AnsiStrings Updates

  Revision 1.13  2000/07/05 10:59:52  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.12  2000/07/04 12:04:23  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.11  2000/07/03 13:31:40  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.10  2000/06/29 13:00:55  mk
  - 16 Bit Teile entfernt
  - OS/2 Version laeuft wieder
  - Jochens 'B' Fixes uebernommen
  - Umfangreiche Umbauten fuer Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.9  2000/06/10 20:15:11  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.8  2000/05/10 07:47:15  mk
  RB: X-* -> U-X-*

  Revision 1.7  2000/04/24 08:04:21  mk
  - X-No-Archive und X-Homepage mit jetzt mit U-

  Revision 1.6  2000/04/13 12:48:36  mk
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
end.


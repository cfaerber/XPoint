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

  procedure ReadGruppe(edit:boolean; var name:string; var hzeit:integer16;
                       var limit:longint; var umlaut:byte; var hd,qt,sig:string;
                       var flags:byte; var brk:boolean);
  const fname = '1234567890$_-';

    function retypes(nr:byte):string;
    begin
      retypes:=getres2(901,15+nr);
    end;

  var x,y,i : Integer;
      ums   : string;
      ss    : string;
      retyp : string;  { Re^n / Re / Default / nein }
  begin
    dialog(ival(getres2(901,0)),9,getres2(901,iif(edit,1,2)),x,y);    { 'Brettgruppe bearbeiten','neue Brettgruppe anlegen' }

    if not odd(flags) then begin
      maddstring(3,2,getres2(901,3),name,30,30,''); mhnr(201);   { 'Name    ' }
      msetvfunc(notempty);
      end
    else begin
      maddtext(3,2,getres2(901,4),col.coldialog);      { 'Name' }
      maddtext(12,2,name,col.coldiahigh);
      end;
    maddint   (3,4,getres2(901,5),limit,6,8,0,99999999); mhnr(202);   { 'Limit   ' }
    maddtext  (length(getres2(901,5))+14,4,getres(13),col.coldialog);
    maddint   (3,6,getres2(901,6),hzeit,4,4,0,9999);   { 'Halten: ' }
    maddtext  (length(getres2(901,6))+11,6,getres2(901,7),col.coldialog);   { 'Tage' }
    ums:=umtyp[umlaut];
    maddstring(3,7,getres2(901,8),ums,5,5,'');         { 'Sonderz.' }
    for i:=0 to 1 do
      mappsel(true,umtyp[i]);
    retyp:=retypes((flags and 6) shr 1);
    maddstring(3,8,getres2(901,20),retyp,7,7,'');      { 'Replies ' }
    for i:=0 to 3 do
      mappsel(true,retypes(i));
    ss:=range('A','Z')+range('a','z')+fname;
    maddstring(26,6,getres2(901,9),hd,8,8,ss); mhnr(206);   { 'Kopf    ' }
    mappcustomsel(SelSchab,false);
    maddstring(26,7,getres2(901,10),qt,8,8,ss); mhnr(206);  { 'Quote   ' }
    mappcustomsel(SelSchab,false);
    maddstring(26,8,getres2(901,11),sig,8,8,ss); mhnr(206); { 'Signatur' }
    mappcustomsel(SelSchab,false);
    readmask(brk);

    if not brk then begin
      for i:=0 to 5 do
        if UpperCase(ums)=UpperCase(umtyp[i]) then umlaut:=i;
      flags:=flags and (not 6);
      LoString(retyp);
      if retyp=LowerCase(retypes(1)) then inc(flags,2)        { re^n: }
      else if retyp=LowerCase(retypes(2)) then inc(flags,4)   { re:   }
      else if retyp=LowerCase(retypes(3)) then inc(flags,6);  { nein  }
      end;
    enddialog;
    freeres;
  end;

  procedure NeueGruppe;
  var name   : string;
      hd,sig : string;
      qt     : string;
      hzeit  : integer16;
      limit  : longint;
      umlaut : byte;
      flags  : byte;
      brk    : boolean;
  begin
    name:=''; hzeit:=stdhaltezeit; limit:=MaxNetMsgs;
    hd:='header'; sig:='signatur'; qt:='qbrett';
    umlaut:=0;   { IBM-Umlaute, keine Konvertierung }
    flags:=0;    { keine Standard-Gruppe; Re^n: Default }
    readgruppe(false,name,hzeit,limit,umlaut,hd,qt,sig,flags,brk);
    if not brk then begin
      dbSeek(d,giName,UpperCase(name));
      if dbFound then
        rfehler(910)   { 'Eine Gruppe mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWriteStr(d,'Name',name);
        dbWrite(d,'haltezeit',hzeit);
        dbWrite(d,'MsgLimit',limit);
        dbWrite(d,'umlaute',umlaut);
        dbWriteStr(d,'kopf',hd);
        dbWriteStr(d,'signatur',sig);
        dbWriteStr(d,'quotemsk',qt);
        dbWrite(d,'flags',flags);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure EditGruppeAllgemein;
  var name   : string;
      hd,sig : string;
      qt     : string;
      hzeit  : integer16;
      limit  : longint;
      flags  : byte;
      umlaut : byte;
      brk    : boolean;
  begin
    dbGo(d,drec[p]);
    name:= dbReadStr(d,'Name');
    dbRead(d,'haltezeit',hzeit);
    dbRead(d,'MsgLimit',limit);
    dbRead(d,'flags',flags);
    dbRead(d,'umlaute',umlaut);
    hd:= dbReadStr(d,'kopf');
    sig:= dbReadStr(d,'signatur');
    qt:= dbReadStr(d,'quotemsk');
    readgruppe(true,name,hzeit,limit,umlaut,hd,qt,sig,flags,brk);
    if not brk then begin
      dbWriteStr(d,'Name',name);
      dbWrite(d,'haltezeit',hzeit);
      dbWrite(d,'MsgLimit',limit);
      dbWrite(d,'Umlaute',umlaut);
      dbWriteStr(d,'kopf',hd);
      dbWriteStr(d,'signatur',sig);
      dbWriteStr(d,'quotemsk',qt);
      dbWrite(d,'flags',flags);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  procedure EditGruppeFido;
  var x,y  : Integer;
      brk  : boolean;
      orig : string;
      addr : string;
  begin
    dbGo(d,drec[p]);
    orig:= dbReadStr(d,'origin');
    addr:= dbReadStr(d,'adresse');
    dialog(46,5,getres2(902,1),x,y);    { 'Fido-Einstellungen' }
    maddstring(3,2,getres2(902,2),orig,32,48,range(' ',#126)); mhnr(690);   { 'Origin ' }
    maddstring(3,4,getres2(902,3),addr,15,15,'');   { 'Adresse' }
    mset3proc(setfidoadr);
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWriteStr(d,'origin',orig);
      dbWriteStr(d,'adresse',addr);
      dbFlushClose(d);
      end;
  end;

  procedure EditGruppeRFC;
  var x,y  : Integer;
      brk  : boolean;
      AMRealname,AMMail,AMReplyTo,AMFQDN : string;
      PMRealname,PMMail,PMReplyTo,PMFQDN : string;
  begin
    dbGo(d,drec[p]);
    AMRealname:= dbReadStr(d,'amrealname');
    AMMail:= dbReadStr(d,'ammail');
    AMReplyTo:= dbReadStr(d,'amreplyto');
    AMFQDN:= dbReadStr(d,'amfqdn');
    PMRealname:= dbReadStr(d,'pmrealname');
    PMMail:= dbReadStr(d,'pmmail');
    PMReplyTo:= dbReadStr(d,'pmreplyto');
    PMFQDN:= dbReadStr(d,'pmfqdn');
    dialog(49,15,getres2(902,10),x,y);    { 'RFC-Einstellungen' }
    maddtext(3,2,getres2(902,11),col.coldiahigh);     { 'oeffentliche Nachrichten' }
    maddstring(3,4,getres2(902,13),AMRealname,32,40,''); mhnr(695);   { 'Realname' }
    maddstring(3,5,getres2(902,14),AMMail,32,80,range(' ',#126));     { 'E-Mail  ' }
    maddstring(3,6,getres2(902,15),AMReplyTo,32,80,range(' ',#126));  { 'Reply-To' }
    maddstring(3,7,getres2(902,16),AMFQDN,32,60,range(' ',#126));     { 'FQDN    ' }
    maddtext(3,9,getres2(902,12),col.coldiahigh);     { 'private Nachrichten' }
    maddstring(3,11,getres2(902,13),PMRealname,32,40,''); mhnr(695);  { 'Realname' }
    maddstring(3,12,getres2(902,14),PMMail,32,80,range(' ',#126));    { 'E-Mail  ' }
    maddstring(3,13,getres2(902,15),PMReplyTo,32,80,range(' ',#126)); { 'Reply-To' }
    maddstring(3,14,getres2(902,16),PMFQDN,32,60,range(' ',#126));    { 'FQDN    ' }
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWriteStr(d,'amrealname',AMRealname);
      dbWriteStr(d,'ammail',AMMail);
      dbWriteStr(d,'amreplyto',AMReplyTo);
      dbWriteStr(d,'amfqdn',AMFQDN);
      dbWriteStr(d,'pmrealname',PMRealname);
      dbWriteStr(d,'pmmail',PMMail);
      dbWriteStr(d,'pmreplyto',PMReplyTo);
      dbWriteStr(d,'pmfqdn',PMFQDN);
      dbFlushClose(d);
      end;
  end;

  procedure DelGruppe;
  var grnr  : longint;
      flags : byte;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'flags',flags);
    if odd(flags) then
      rfehler(911)       { 'Gruppe kann nicht gel"scht werden!' }
    else begin
      dbRead(d,'INT_NR',grnr);
      dbSetindex(bbase,biGruppe);
      dbSeek(bbase,biGruppe,dbLongStr(grnr));
      if dbFound then
        rfehler(912)     { 'Es sind noch Bretter in dieser Gruppe vorhanden.' }
      else begin
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
  end;

  procedure addhzeit(add:integer);
  var hzeit : integer16;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'haltezeit',hzeit);
    hzeit:=max(0,min(hzeit+add,9999));
    dbWrite(d,'haltezeit',hzeit);
    displine(p);
  end;


  procedure EditGruppe;
  const edb_pos : shortint = 1;
  var n   : shortint;
      nts : string;
  begin
    pushhp(207);
    nts:=getres2(901,21); { ' ^Allgemein,^RFC,^Fido ' }
    n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts,edb_pos);
    freeres;
    if n<>0 then edb_pos:=abs(n);
    if n>0 then
      case n of
        1: EditGruppeAllgemein;
        2: EditGruppeRFC;
        3: EditGruppeFido;
      end;
    pophp;
  end;

{
  $Log$
  Revision 1.2  2001/07/23 16:05:22  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.1  2001/06/04 17:40:41  ma
  - renamed, was xp9
  - implemented role feature

}

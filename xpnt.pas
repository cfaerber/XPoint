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

{ Netztypen   PM 04/92 }
{ s. auch NETZTYP.DOC  }

{$I xpdefine.inc }

unit xpnt;

interface

uses   sysutils,xp0,typeform,datadef,database,crc;

const  nt_Netcall   = 0;         { Puffer-Formate       }
       nt_ZConnect  = 2;         { XRef: XP3, XP6       }
       nt_Magic     = 3;
       nt_Pronet    = 4;         { ProNET / TopNET      }
       nt_Quick     = 10;        { QuickMail            }
       nt_GS        = 11;        { GS-Mailbox           }
       nt_Maus      = 20;
       nt_Fido      = 30;
       nt_QWK       = 31;

       nt_UUCP      = 40;
       nt_Client    = 41;
       nt_NNTP      = 50;
       nt_POP3      = 51;
       nt_IMAP      = 52;

       netsRFC = [nt_NNTP,nt_POP3,nt_IMAP,nt_UUCP,nt_Client];
       netsSupportingPGP = [nt_ZConnect,nt_Fido,nt_Maus,nt_UUCP,nt_client,nt_NNTP,nt_POP3,nt_IMAP];

var ntused : array[0..99] of integer;

function ntZConnect(nt:byte):boolean;         { Ablagentyp ermitteln  }
function ntConv(nt:byte):boolean;             { konvertiertes Format  }
function ntZCablage(ablg:byte):boolean;       { ZConnect-Ablage       }
function ntName(nt:byte):string;              { s. auch XP4O.MsgInfo() }
function mbNetztyp:byte;
function ntZonly:boolean;                     { nur Z-Netz/alt }
function ntXPctl(nt:byte):boolean;            { XP-Control-Messages }

function ntBinary(nt:byte):boolean;           { Bin„rmails erlaubt    }
function ntBinaryBox(box:string):boolean;     { dito                  }
function ntMIME(nt:byte):boolean;
function ntBinEncode(nt:byte):boolean;        { Bin„rmails werden uucodiert }
function ntMessageID(nt:byte):byte;           { Message-ID-Format     }
function ntDomainReply(nt:byte):boolean;      { Replys auf eig. Nachr. erkennbar }
function ntZDatum(nt:byte):boolean;           { langes Datumsformat   }
function ntDomainType(nt:byte):byte;          { Domain fr Absender + MsgID }
function ntAutoZer(nt:byte):boolean;          { .ZER-Pflicht          }
function ntAutoDomain(box:string; ownbox:boolean):string;   { .Domain }
function ntServerDomain(box:string):string;   { Domain des Servers    }
function ntDefaultDomain(nt:byte):string;     { Domain fr neue Boxen }
function ntGrossUser(nt:byte):boolean;        { User-Groáschreibung   }
function ntGrossBrett(nt:byte):boolean;       { Bretter-Groáschreibung }
function ntKleinBrett(nt:byte):boolean;       { Bretter-Kleinschreibung }
function ntKomkette(nt:byte):boolean;         { Kommentar-Verkettung  }
function ntRfcCompatibleID(nt:byte):boolean;  { RFC-Msgid             }
function ntMIDCompatible(n1,n2:byte):boolean; { austauschbare MsgIDs  }
function ntOrigID(nt:byte):boolean;           { X-XP-ORGMID -> X-XP-ORGREF }
function ntAdrCompatible(n1,n2:byte):boolean; { umleitbare PM-Adresse }
function ntEmpfBest(nt:byte):boolean;         { EB-Flag im Header     }
function ntMsg0(nt:byte):boolean;             { Nachricht darf leer sein }
function ntNameSpace(nt:byte):boolean;        { Leerzeichen in Usernamen }
function ntBrettEmpf(nt:byte):boolean;        { Fido-To }
function ntBrettEmpfUsed:boolean;             { Netztypen mit Fido-To vorh. }
function ntEditbrettEmpf(nt:byte):boolean;    { dito, aber editierbar }
function ntRealname(nt:byte):boolean;         { Realnames m”glich }
function ntRealUmlaut(nt:byte):boolean;       { Umlaute im Realname }
function ntHeaderUmlaut(nt:byte):boolean;     { Umlaute in Keywords etc. }
function ntCancel(nt:byte):boolean;           { Cancel-Messages m”glich }
function ntCancelPM(nt:byte):boolean;         { Cancel auch bei PM m”glich }
function ntErsetzen(nt:byte):boolean;         { Supersedes/Ersetzt m”glich }
function ntBetreffLen(nt:byte):byte;          { max. Betreffl„nge }
function ntPmReply(nt:byte):boolean;          { attrPmReply erzeugen }
function ntFollowup(nt:byte):boolean;         { Followup-To m”glich }
function ntCrossAM(nt:byte):boolean;          { AM-Crosspostings m”glich }
function ntCrossPM(nt:byte):boolean;          { PM-Crosspostings m”glich }
function ntOrigWeiter(nt:byte):boolean;       { Weiterleiten mit WAB  }
function ntBoxnameLen(nt:byte):byte;          { max. L„nge von Servernamen }
function ntPMTeleData(nt:byte):boolean;       { PMs: Telefon + Postanschrift }
function ntAMTeleData(nt:byte):boolean;       { AMs: Telefon + Postanschrift }
function ntSec(nt:byte):boolean;              { sekundengenaue Uhrzeit }
function ntOptIso(nt:byte):boolean;           { wahlweise ISO-Zeichensatz }
function ntIBM(nt:byte):boolean;              { IBM-Zeichensatz verwenden }
function ntPGP(nt:byte):boolean;              { PGP-Keys im Headaer }
function ntBrettebene(nt:byte):boolean;       { Netztyp mit Brettebene }
function ntBCC(nt:byte):boolean;              { BCC-Option vorhanden }
function ntFilename(nt:byte):boolean;         { Dateiname im Header }
function ntBoxNetztyp(box:string):byte;       { Netztyp der Box       }
function ntRelogin(nt:byte):byte;             { Relogin-Netcall m”glich }
function ntOnline(nt:byte):boolean;           { Online-Anruf m”glich  }
function ntNetcall(nt:byte):boolean;          { Netcall m”glich }
function ntOnePW(nt:byte):boolean;            { Point-PW = Online-PW  }
function ntDownarcPath(nt:byte):boolean;      { Entpacker muá im Pfad liegen }
function ntExtProt(nt:byte):boolean;          { externes š.-Protokoll }
function ntGrossPW(nt:byte):boolean;          { Paáwort muá groágeschr. werden }

function ntMAF(nt:byte):boolean;              { MAF statt MAPS        }
function ntQuickMaps(nt:byte):boolean;        { Maps-Bestellung an Sysop }
function ntNude(nt:byte):boolean;             { Mausnet-"CMD"-Maps    }
function ntAreamgr(nt:byte):boolean;          { Fido-Areafix/Areamgr  }
function ntProMaf(nt:byte):boolean;           { Pronet-System         }
function ntNoMaps(nt:byte):boolean;           { kein Maps-Service     }
function ntMapsOthers(nt:byte):boolean;       { Maps/Sonstige         }
function ntMapsBrettliste(nt:byte):boolean;   { Maps/Liste_anfordern  }

function ntReplyToAll (nt :integer) :boolean;    { Reply-To-All erlaubt }

function formmsgid(msgid:string):string;
function grosschar(b:boolean):string;

function ntValidAddress(nt:byte;const addr:string):boolean;
function ntNormalizeAddress(nt:byte;var addr:string):boolean;

implementation  { ---------------------------------------------------- }

uses Debug;

{ X-XP-NTP:  Netztyp - optional, Default 2 (nt_ZConnect)
  X-XP-ARC:  archivierte PM - optional
  X-XP-BOX:  Absendebox - falls Absendebox nicht im Absender steht
  X-XP-PNT:  Absendepoint (MagicNET)
  X-XP-BST:  Maus PM-Bearbeitungsstatus
  X-XP-ATT:  Nachrichten-Attribute, Fido & Maus, Hex(4)
  X-XP-FTO:  Fido-Empf„nger bei Echomail
  X-XP-MRP:  Maus-Reply-Path (Pfad der Bezugsnachricht) }


function ntZConnect(nt:byte):boolean;
begin
  ntZConnect:=(nt>1);
end;

function mbNetztyp:byte;
begin
  mbNetztyp:=(dbReadInt(mbase,'netztyp') and $ff);
end;


function ntBinary(nt:byte):boolean;
begin
  ntBinary:=(nt in [nt_Netcall,nt_ZCONNECT,nt_Quick,nt_GS,nt_Maus,
                    nt_UUCP, nt_POP3, nt_IMAP, nt_NNTP]) or
            (fidobin and (nt=nt_Fido));
end;

function ntMIME(nt:byte):boolean;
begin
  ntMIME  :=(nt in [nt_UUCP, nt_POP3, nt_IMAP, nt_NNTP])  or
            (zc_mime and (nt in [nt_ZConnect]));
end;

function ntBinEncode(nt:byte):boolean;        { Bin„rmails werden uucodiert }
begin
  ntBinEncode:=(nt=nt_Maus) or (nt=nt_Fido);
end;


function ntBinaryBox(box:string):boolean;
begin
  ntBinaryBox:=ntBinary(ntBoxNetztyp(box));
end;


{ 0=keine, 1=Netcall, 2=ZConnect, 3=Maus, 4=Fido, 5=UseNet, 6=ProNet }

function ntMessageID(nt:byte):byte;
begin
  case nt of
    nt_Netcall  : ntMessageID:=1;     { @BOX }
    nt_ZConnect : ntMessageID:=2;     { @POINT.BOX.zer.sub.org }
    nt_Magic    : ntMessageID:=2;     { [_point]@system.seven.sub.org }
    nt_Pronet   : ntMessageID:=6;     { X/HHMMSSssDDMMYYYY_KKK@BOX;NR.pro }
    nt_Quick    : ntMessageID:=1;     { @POINT }
    nt_GS       : ntMessageID:=1;     { 0815@POINT }
    nt_Maus     : ntMessageID:=3;     { 0815@BOX }
    nt_Fido     : ntMessageID:=4;     { net:zone/node.point[@domain] xxxxxxxx }
    nt_UUCP,
    nt_NNTP,
    nt_POP3     : ntMessageID:=5;     { @point.do.main }
  else  { QWK }
    ntMessageID:=1;
  end;
end;


{ Replys auf eigene Nachrichten werden anhand der BEZ-Domain erkannt: }

function ntDomainReply(nt:byte):boolean;
begin
  ntDomainReply:=(ntMessageID(nt) in [2,5,50,51]);
end;


function ntConv(nt:byte):boolean;
begin
  ntConv:=(nt>2);
end;


function ntZCablage(ablg:byte):boolean;
begin
  ntZCablage:=(ablg>9);
end;


function ntBoxNetztyp(box:string):byte;
var d  : DB;
    nt : byte;
begin
  if box='' then box:=DefaultBox;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then
    dbRead(d,'netztyp',nt)
  else
    nt:=0;
  dbClose(d);
  ntBoxNetztyp:=nt;
end;


function ntZDatum(nt:byte):boolean;
begin
  ntZDatum:=(nt>=2);
end;


function formmsgid(msgid:string):string;
var p : byte;
begin
  if msgid='' then
    formmsgid:=''
  else begin
    p:=cposx('@',msgid)+1;
    while p<=length(msgid) do begin
      msgid[p]:=system.upcase(msgid[p]);
      inc(p);
      end;
    formmsgid:=dbLongStr(CRC32Str(msgid))+LeftStr(msgid,15);
    end;
end;


function ntMAF(nt:byte):boolean;
begin
  ntMAF:=(nt=nt_Magic);
end;

function ntProMaf(nt:byte):boolean;
begin
  ntProMaf:=(nt=nt_Pronet);
end;

function ntQuickMaps(nt:byte):boolean;
begin
  ntQuickMaps:=(nt=nt_Quick);
end;

function ntNude(nt:byte):boolean;
begin
  ntNude:=(nt=nt_Maus);
end;

function ntAreamgr(nt:byte):boolean;
begin
  ntAreamgr:=(nt=nt_Fido);
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }
function ntNoMaps(nt:byte):boolean;
begin
  ntNoMaps:= nt in [nt_POP3];
end;
{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

function ntMapsOthers(nt:byte):boolean;       { Maps/Sonstige         }
begin
  ntMapsOthers:=not (nt in [nt_Quick,nt_Pronet,nt_QWK,nt_NNTP,nt_POP3]);
end;

function ntMapsBrettliste(nt:byte):boolean;   { Maps/Liste_anfordern  }
begin
  ntMapsBrettliste:=(nt<>nt_QWK);
end;

function ntDomainType(nt:byte):byte;
begin
  case nt of
    0,1      : ntDomainType:=0;   { @BOX.ZER [@POINT.ZER] }
    2        : ntDomainType:=5;   { @BOX.domain [@POINT.domain] }
    3        : ntDomainType:=1;   { @POINT oder @BOX }
    4        : ntDomainType:=7;   { @BOX;POINT }
    10,11    : ntDomainType:=2;   { @POINT }
    20,31,90 : ntDomainType:=3;   { @BOX }
    30       : ntDomainType:=4;   { @Net:Zone/Node.Point = @Box.Point }
    40       : ntDomainType:=6;   { @point.domain }
    50,51,52 : ntDomainType:=8;   { }
  else
    ntDomainType:=0;
  end;
end;


function ntAutoZer(nt:byte):boolean;
begin
  ntAutoZer:=(nt<=1);
end;

function ntAutoDomain(box:string; ownbox:boolean):string;
var d  : DB;
    nt : shortint;
begin
  ntAutoDomain:='';
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then begin
    dbRead(d,'netztyp',nt);
    if ntAutoZer(nt) then
      ntAutoDomain:='.ZER'
    else if (nt=nt_ZConnect) and not ownbox then
      ntAutoDomain:='.invalid'
    else
      if ntDomainType(nt) in [5,6] then
        ntAutoDomain:=dbReadStr(d,'domain');
    end;
  dbClose(d);
end;

function ntServerDomain(box:string):string;   { Domain des Servers    }
var d : DB;
begin
  ntServerDomain:='';
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then
    if dbReadInt(d,'netztyp')<>nt_UUCP then
      ntServerDomain:=ntAutoDomain(box,true)
    else
      ntServerDomain:=dbReadStr(d,'boxdomain');
  dbClose(d);
end;


function ntDefaultDomain(nt:byte):string;     { Domain fr neue Boxen }
begin
  case nt of
    nt_Pronet   : ntDefaultDomain:='.pro';
    nt_ZConnect : ntDefaultDomain:='.invalid';
    nt_Fido     : ntDefaultDomain:='';
  else
    ntDefaultDomain:='';
  end;
end;


function ntGrossUser(nt:byte):boolean;
begin
  ntGrossUser:=(not smallnames and (nt=nt_Netcall)) or
               (nt in [nt_Magic,nt_Quick,nt_GS,nt_Pronet]);
end;


function ntGrossBrett(nt:byte):boolean;
begin
  ntGrossBrett:=(nt=nt_Netcall) or (nt=nt_Fido) or (nt=nt_Pronet);
end;


function ntKleinBrett(nt:byte):boolean;       { Bretter-Kleinschreibung }
begin
  ntKleinBrett:=(nt in [nt_UUCP, nt_NNTP]);
end;


function grosschar(b:boolean):string;
begin
  if b then grosschar:='>'
  else grosschar:='';
end;


function ntName(nt:byte):string;
begin
  case nt of
    nt_Netcall  : ntName:='Z-Netz alt';
    nt_ZConnect : ntName:='ZConnect';
    nt_Magic    : ntName:='MagicNET';
    nt_Pronet   : ntName:='ProNET';
    nt_Quick    : ntName:='QuickMail';
    nt_GS       : ntName:='GS-Mailbox';
    nt_Maus     : ntName:='MausTausch';
    nt_Fido     : ntName:='Fido';
    nt_QWK      : ntName:='QWK';
    nt_UUCP     : ntName:='RFC/UUCP';
    nt_Client   : ntName:='RFC/Client';
    nt_NNTP     : ntName:='NNTP';
    nt_POP3     : ntName:='POP3/SMTP';
  else
    Debug.DebugLog('xpnt','Unknown net type: '+IntToStr(nt),DLWarning);
    ntName:='? '+IntToStr(nt);
  end;
end;


{ 0=kein Relogin, 1=Relogin nur mit Script, 2=Relogin immer m”glich }

function ntRelogin(nt:byte):byte;
begin
  case nt of
    nt_Fido,
    nt_QWK,
    nt_NNTP,
    nt_POP3     : ntRelogin:=0;
    nt_GS,
    nt_UUCP     : ntRelogin:=1;
    else          ntRelogin:=2;
  end;
end;

function ntOnline(nt:byte):boolean;   { false -> Script erforderlich }
begin
  {ntOnline:=(nt<>nt_Fido) and (nt<>nt_GS) and (nt<>nt_UUCP) and (nt<>nt_QWK);}
  ntOnline:=not (nt in [nt_Fido, nt_GS, nt_UUCP, nt_QWK, nt_NNTP, nt_POP3]);
end;

function ntNetcall(nt:byte):boolean;          { Netcall m”glich }
begin
  ntNetcall:=(nt<>nt_QWK);
end;


function ntOnePW(nt:byte):boolean;
begin
  ntOnePW:=(nt=nt_Maus);
end;

function ntKomkette(nt:byte):boolean;
begin
  ntKomkette:=
    (nt in [nt_Maus,nt_Fido,nt_ZConnect,nt_UUCP,nt_QWK,nt_Pronet,nt_NNTP, nt_POP3])
    or ((nt=nt_Magic) and MaggiVerkettung);
end;


function ntRfcCompatibleID(nt:byte):boolean;
begin
  ntRfcCompatibleID:=nt in [nt_ZConnect,nt_Magic,nt_UUCP,nt_NNTP,nt_POP3];
end;

function ntMIDCompatible(n1,n2:byte):boolean;  { austauschbare MsgIDs  }
begin
  ntMIDcompatible:=(n1=n2) or (ntRfcCompatibleID(n1) and ntRfcCompatibleID(n2));
end;

function ntOrigID(nt:byte):boolean;           { X-XP-ORGMID -> X-XP-ORGREF }
begin
  ntOrigID:=(nt=nt_Maus) or (nt=nt_Fido);
end;

function ntAdrCompatible(n1,n2:byte):boolean;  { umleitbare PM-Adresse }
begin
  ntAdrCompatible:= (n1 in ([nt_Maus, nt_ZConnect]+netsRFC)) and
                    (n2 in ([nt_Maus, nt_ZConnect]+netsRFC));
{  ntAdrCompatible:=(n1=n2) or
                   (((n1=nt_ZConnect) or (n1=nt_UUCP)) and
                     (n2 in [nt_ZConnect,nt_UUCP,nt_Maus]));}
end;

function ntEmpfBest(nt:byte):boolean;
begin
  ntEmpfBest:= nt in [nt_Fido,nt_ZConnect,nt_Magic,nt_UUCP,nt_NNTP,nt_POP3];
end;


function ntMsg0(nt:byte):boolean;             { Nachricht darf leer sein }
begin
  ntMsg0:=nt in ([nt_ZConnect,nt_Fido]+netsRFC);
end;


function ntNameSpace(nt:byte):boolean;        { Leerzeichen in Usernamen }
begin
  ntNameSpace:=(nt in [nt_Fido,nt_Magic,nt_Maus,nt_Quick,nt_GS,nt_Pronet,nt_QWK]);
end;


function ntDownarcPath(nt:byte):boolean;      { Entpacker muá im Pfad liegen }
begin
  ntDownarcPath:=(nt=nt_Fido) or (nt=nt_ZConnect);
end;

function ntBrettEmpf(nt:byte):boolean;        { Fido-To }
begin
  ntBrettEmpf:=(nt=nt_Fido) or (nt=nt_QWK) or (nt=nt_Magic) or (nt=nt_Pronet);
end;

function ntBrettEmpfUsed:boolean;             { Netztypen mit Fido-To vorh. }
begin
  ntBrettEmpfUsed:= ntUsed[nt_Fido] + ntUsed[nt_QWK] + ntUsed[nt_Magic]+
                    ntUsed[nt_Pronet] > 0;
end;

function ntEditbrettEmpf(nt:byte):boolean;    { dito, aber editierbar;  }
begin                                         { Empf„nger in mbase.name }
  ntEditBrettEmpf:=(nt=nt_Fido) or (nt=nt_QWK);
end;


function ntRealname(nt:byte):boolean;         { Realnames m”glich }
begin
  ntRealname:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3];
end;


function ntRealUmlaut(nt:byte):boolean;       { Umlaute im Realname }
begin
  ntRealUmlaut:=nt in [nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3];
end;


function ntHeaderUmlaut(nt:byte):boolean;     { Umlaute in Keywords etc. }
begin
  ntHeaderUmlaut:=nt in [nt_ZCONNECT,nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3];
end;

function ntCancel(nt:byte):boolean;           { Cancel-Messages m”glich }
begin
  ntCancel:=nt in [nt_UUCP,nt_Maus,nt_ZConnect,NT_NNTP];
end;

function ntCancelPM(nt:byte):boolean;         { Cancel auch bei PM m”glich }
begin
  ntCancelPM:=(nt=nt_Maus);
end;

function ntErsetzen(nt:byte):boolean;         { Supersedes/Ersetzt m”glich }
begin
  ntErsetzen:=nt in [nt_UUCP,nt_ZConnect,nt_NNTP,nt_POP3];
end;

function ntBetreffLen(nt:byte):byte;          { max. Betreffl„nge }
begin
  case nt of
    nt_Netcall : ntBetreffLen:=40;
  { nt_Maus    : ntBetreffLen:=30; }
    nt_Magic   : ntBetreffLen:=60;
  { nt_QWK     : ntBetreffLen:=25; }
    nt_Pronet  : ntBetreffLen:=40;
  else
    ntBetreffLen:=BetreffLen;
  end;
end;


function ntPmReply(nt:byte):boolean;          { attrPmReply erzeugen }
begin
  ntPmReply:=nt in [nt_Maus,nt_UUCP,nt_NNTP,nt_POP3];
end;


function ntFollowup(nt:byte):boolean;         { Followup-To m”glich }
begin
  ntFollowup:=nt in [nt_ZConnect,nt_UUCP,nt_NNTP];
end;


function ntCrossAM(nt:byte):boolean;          { AM-Crosspostings m”glich }
begin
  ntCrossAM:=(nt in [nt_UUCP,nt_NNTP]) or ((nt=nt_ZConnect) and zc_xposts);
end;

function ntCrossPM(nt:byte):boolean;          { PM-Crosspostings m”glich }
begin
  ntCrossPM:=nt in [nt_ZConnect,nt_UUCP,nt_POP3];
end;


function ntExtProt(nt:byte):boolean;          { externes š.-Protokoll }
begin
  ntExtProt:=not (nt in [nt_Fido,nt_UUCP,nt_QWK,nt_NNTP,nt_POP3]);
end;


function ntOrigWeiter(nt:byte):boolean;       { Weiterleiten mit WAB  }
begin
  ntOrigWeiter:=nt in [nt_ZConnect,nt_UUCP,nt_Maus,nt_NNTP,nt_POP3];
end;


function ntBoxnameLen(nt:byte):byte;
begin
  case nt of
    nt_Zconnect: ntBoxnameLen:=20;
    nt_Magic   : ntBoxnameLen:=10;
    nt_Pronet  : ntBoxnameLen:=9;
    nt_Quick   : ntBoxnameLen:=15;
    nt_GS      : ntBoxnameLen:=10;
    nt_Maus    : ntBoxnameLen:=8;
    nt_Fido    : ntBoxnameLen:=15;
    nt_QWK     : ntBoxnameLen:=15;
    nt_UUCP,
    nt_NNTP,
    nt_POP3    : ntBoxnameLen:=20;
  else
    ntBoxnameLen:=8;   { Netcall }
  end;
end;


function ntPMTeleData(nt:byte):boolean;        { Telefon + Postanschrift }
begin
  ntPMTeleData:=nt in [nt_ZConnect,nt_UUCP,nt_NNTP,nt_POP3];
end;

function ntAMTeleData(nt:byte):boolean;        { Telefon + Postanschrift }
begin
  ntAMTeleData:=((nt=nt_ZConnect) and not adrpmonly) or (nt=nt_POP3);
end;


function ntSec(nt:byte):boolean;              { sekundengenaue Uhrzeit }
begin
  ntSec:=(nt in [nt_ZCONNECT,nt_UUCP,nt_Magic,nt_Pronet,nt_NNTP,nt_POP3]);
end;


function ntZonly:boolean;                     { nur Z-Netz/alt }
var i : integer;
begin
  i:=99;
  while (i>0) and (ntUsed[i]=0) do dec(i);
  ntZonly:=(i=0);
end;

// Network types where the IBM charset is default but can be changed
// to ISO-8859-*
function ntOptIso(nt:byte):boolean;           { wahlweise ISO-Zeichensatz }
begin
  ntOptIso:=(nt=nt_ZConnect);
end;

// Network types where the IBM charset is default, otherwise we use
// ISO-8859-1 or UTF-8
function ntIBM(nt:byte):boolean;      
begin
  Result:=nt in [
    nt_Netcall,
    nt_ZConnect,
    nt_Magic,
    nt_Pronet,
    nt_Quick,
    nt_GS,
    nt_Maus,
    nt_Fido,
    nt_QWK ];
end;

function ntPGP(nt:byte):boolean;              { PGP-Keys im Header }
begin
  ntPGP:=(nt=nt_ZCONNECT) or
         ((nt in [nt_UUCP,nt_NNTP,nt_POP3]) and PGP_UUCP) or
         ((nt=nt_Fido) and PGP_Fido);
end;


function ntGrossPW(nt:byte):boolean;       { Paáwort muá groágeschr. werden }
begin
  ntGrossPW:=(nt in [nt_Netcall,nt_Magic,nt_Pronet,nt_Quick,nt_GS]);
end;


{ XP-Control-Messages fr SUPPORT.CFG }

function ntXPctl(nt:byte):boolean;
begin
  ntXPctl:=(nt in [nt_ZConnect,nt_UUCP,nt_Fido,nt_NNTP,nt_POP3]);
end;


function ntBrettebene(nt:byte):boolean;       { Netztyp mit Brettebene }
begin
  ntBrettebene := (nt in [nt_Fido,nt_Maus,nt_QWK,nt_Magic,nt_Pronet]);
end;


function ntBCC(nt:byte):boolean;              { BCC-Option vorhanden }
begin
  ntBCC := (nt in [nt_ZConnect,nt_UUCP,nt_POP3]);
end;


function ntFilename(nt:byte):boolean;         { Dateiname im Header }
begin
  ntFilename := (nt in [nt_ZConnect,nt_UUCP]);
end;

function ntReplyToAll (nt :integer) :boolean;    { Reply-To-All allowed? }
begin
  // only LSB contains net type
  ntReplyToAll := ((nt and $FF) in [nt_ZConnect, nt_UUCP, nt_POP3, nt_NNTP]);
end;

function ntValidAddress(nt:byte;const addr:string):boolean;
begin
//  if nt in netsRFC then result:=RFC2822ValidAdress(addr) else
  Debug.DebugLog('xpnt','Unimplemented function called (ntValidAddress)',dlWarning);
  result:=false;
end;

function ntNormalizeAddress(nt:byte;var addr:string):boolean;
begin
  Debug.DebugLog('xpnt','Unimplemented function called (ntNormalizeAddress)',dlWarning);
  result := false;
end;

begin
  fillchar(ntused,sizeof(ntused),0);
{
  $Log$
  Revision 1.38  2001/10/21 13:09:05  ml
  - removed some more warnings (only 130 yet...)

  Revision 1.37  2001/10/18 10:57:52  mk
  - fixed bug in ntReplyToAll introduced with last commit from ML

  Revision 1.36  2001/10/17 22:11:25  ml
  - removed some range-check Errors

  Revision 1.35  2001/09/25 21:07:45  cl
  - added UI for non-RFC network charset selection

  Revision 1.34  2001/09/17 16:17:07  cl
  - Fixed ntAddressCompatible

  Revision 1.33  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.32  2001/09/08 14:39:57  cl
  - added ZC_MIME configuration option

  Revision 1.31  2001/09/06 22:01:14  mk
  - client mode updates

  Revision 1.30  2001/08/27 09:13:43  ma
  - changes in net type handling (1)

  Revision 1.29  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.28  2001/08/10 20:58:01  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.27  2001/07/29 13:58:23  ma
  - removed nt_UUCP_U, some fixes

  Revision 1.26  2001/07/27 18:10:15  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.25  2001/07/22 21:05:19  mk
  - fixed double RFC/UUCP in Edit/Boxen/Neu
  - renamed ntnr to NetTypes and removed eNetztypen

  Revision 1.24  2001/07/21 16:02:12  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.23  2001/05/05 08:05:42  mk
  - allow binaries in POP3, IMAP and NNTP

  Revision 1.22  2001/04/17 20:21:29  ma
  - removed "## XP ##" checking

  Revision 1.21  2001/04/05 14:58:12  ml
  -Fix: absender doesnt end with @ anymore

  Revision 1.20  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.19  2001/01/05 09:33:11  mk
  - removed THeader.Ref

  Revision 1.18  2000/10/17 10:06:00  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.17  2000/09/28 16:43:17  fe
  ZC-Defaultdomain von '.do.main' nach '.invalid' geaendert.  (RFC 2606)

  Revision 1.16  2000/08/15 16:51:03  mk
  - Updates fuer neue Boxentypen NNTP, POP3/SMTP und IMAP

  Revision 1.15  2000/07/25 18:02:19  hd
  - NNTP-Unterstuetzung (Anfang)

  Revision 1.14  2000/07/25 12:54:02  hd
  - Kleine Anpassung an NNTP

  Revision 1.13  2000/07/23 13:24:12  hd
  - Vorlaeufige Struktur (Masken) fuer Box-Typ 'NNTP'

  Revision 1.12  2000/07/22 15:28:49  hd
  - Neue Netztypen: nt_NNTP und nt_POP3

  Revision 1.11  2000/07/11 21:39:23  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.10  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.9  2000/06/19 20:22:48  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.8  2000/06/10 20:15:12  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.7  2000/05/04 10:33:01  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.6  2000/02/21 22:48:02  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


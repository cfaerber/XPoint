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

{ Netztypen   PM 04/92 }
{ s. auch NETZTYP.DOC  }

{$I xpdefine.inc }

unit xpnt;

interface

uses   sysutils,xp0,typeform,datadef,database,crc;

{$IFDEF ANALYSE}
type
  eNetz = (
    nt_Netcall,   //= 0;         { Puffer-Formate       }
    nt_1,
    nt_ZConnect,  //= 2;         { XRef: XP3, XP6       }
    nt_Magic,     //= 3;
    nt_Pronet,    //= 4;         { ProNET / TopNET      }
    nt_Quick,     //= 10;        { QuickMail            }
    nt_GS,        //= 11;        { GS-Mailbox           }
    nt_Maus,      //= 20;
    nt_Fido,      //= 30;
    nt_QWK,       //= 31;

    nt_UUCP,      //= 40;
    nt_Client,    //= 41;
    nt_NNTP,      //= 50;
    nt_POP3,      //= 51;
    nt_IMAP,      //= 52;
    nt_m1,        //= -1
    nt_90,        //= 90
    nt_99,        //= 99;
    nt_None       //=$FF/-1
  );
{$ELSE}
const  nt_Netcall   = 0;         { Puffer-Formate       }
        nt_1        = 1;
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
        nt_90       = 90;
        nt_99       = 99;
        nt_None     = $FF;  //or -1
type
  eNetz = 0..99;
{$ENDIF}

type
  eMsgFlags = (
    mf_RueckVerkettet,
    mf_Attachment,
    mf_pm_reply,
    mf_wab, //?
    mf_Reply, //mf_1000 - Antwort auf eigene Msg
    mf_ISO1,  //standard charset?
    mf_PGPsig,
    mf_Kom  //if komlen>0 then //inc(mnt,$8000);
  );
  sMsgFlags = set of eMsgFlags;

  RNetzMsg = packed record
  case integer of
  0:  (i: LongInt);
  1:  (netztyp:     eNetz;
      flags:        sMsgFlags;
      CPanz, CPpos: byte; //crosspostings
      );
  end;

type
  eDomainType = (
  //DoDi: some dummies, for now
    dt0, dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8
  );

const
       midNone      = 0;
       midMausNet   = 1;
       midNetcall   = 2;
       midProNet    = 6;
       midFido      = 10;
       midZConnect  = 20;
       midRFC       = 30;

       netsRFC = [nt_NNTP,nt_POP3,nt_IMAP,nt_UUCP,nt_Client];
       netsFTN = [nt_Fido,nt_QWK];

       netsSupportingPGP = [nt_ZConnect,nt_Fido,nt_Maus,nt_UUCP,nt_client,nt_NNTP,nt_POP3,nt_IMAP];

type
  TAddressType = (
    addrDomain,	// user@domain.tld
	  addrFido    // user@1:2/3.4
   );
type   TNetClass = (
         ncNone,
         ncZConnect,
         ncFTN,
         ncRFC,
         ncMaus );

       TNetClassSet = set of TNetClass;

type   TAttachMode = (
         attachNone,        // no attachments permitted
         attachZConnect,    // one text part followed by one binary part (ZConnect KOM+TYP:BIN)
         attachFTN,         // one text part followed by several binary parts (FTN FileAttach)
         attachMIME );      // random combination of parts (MIME)

var ntused : array[eNetz] of integer; //[eNetz]?

function ntZConnect(nt:eNetz):boolean;         { Ablagentyp ermitteln  }
function ntConv(nt:eNetz):boolean;             { konvertiertes Format  }
function ntZCablage(ablg:byte):boolean;       { ZConnect-Ablage       }
function ntName(nt:eNetz):string;              { s. auch XP4O.MsgInfo() }
function mbNetztyp:eNetz;
function ntZonly:boolean;                     { nur Z-Netz/alt }
function ntXPctl(nt:eNetz):boolean;            { XP-Control-Messages }
function ntClass(nt:eNetz):TNetClass;         { Netz-Uebertyp }

function ntBinary(nt:eNetz):boolean;           { Bin„rmails erlaubt    }
function ntBinaryBox(box:string):boolean;     { dito                  }
function ntMIME(nt:eNetz):boolean;             { MIME möglich          }
function ntBinEncode(nt:eNetz):boolean;        { Bin„rmails werden uucodiert }
function ntMessageID(nt:eNetz):byte;           { Message-ID-Format     }
function ntDomainReply(nt:eNetz):boolean;      { Replys auf eig. Nachr. erkennbar }
function ntZDatum(nt:eNetz):boolean;           { langes Datumsformat   }
function ntDomainType(nt:eNetz):byte;          { Domain fr Absender + MsgID }
function ntAutoZer(nt:eNetz):boolean;          { .ZER-Pflicht          }
function ntAutoDomain(box:string; ownbox:boolean):string;   { .Domain }
function ntServerDomain(box:string):string;   { Domain des Servers    }
function ntDefaultDomain(nt:eNetz):string;     { Domain fuer neue Boxen }
function ntGrossUser(nt:eNetz):boolean;        { User-Groáschreibung   }
function ntGrossBrett(nt:eNetz):boolean;       { Bretter-Groáschreibung }
function ntKleinBrett(nt:eNetz):boolean;       { Bretter-Kleinschreibung }
function ntKomkette(nt:eNetz):boolean;         { Kommentar-Verkettung  }
function ntRfcCompatibleID(nt:eNetz):boolean;  { RFC-Msgid             }
function ntMIDCompatible(n1,n2:eNetz):boolean; { austauschbare MsgIDs  }
function ntOrigID(nt:eNetz):boolean;           { X-XP-ORGMID -> X-XP-ORGREF }
function ntAdrCompatible(n1,n2:eNetz):boolean; { umleitbare PM-Adresse }
function ntMsg0(nt:eNetz):boolean;             { Nachricht darf leer sein }
function ntNameSpace(nt:eNetz):boolean;        { Leerzeichen in Usernamen }
function ntBrettEmpf(nt:eNetz):boolean;        { Fido-To }
function ntBrettEmpfUsed:boolean;             { Netztypen mit Fido-To vorh. }
function ntEditbrettEmpf(nt:eNetz):boolean;    { dito, aber editierbar }
function ntRealname(nt:eNetz):boolean;         { Realnames m”glich }
function ntRealUmlaut(nt:eNetz):boolean;       { Umlaute im Realname }
function ntHeaderUmlaut(nt:eNetz):boolean;     { Umlaute in Keywords etc. }
function ntCancel(nt:eNetz):boolean;           { Cancel-Messages m”glich }
function ntCancelPM(nt:eNetz):boolean;         { Cancel auch bei PM m”glich }
function ntErsetzen(nt:eNetz):boolean;         { Supersedes/Ersetzt m”glich }
function ntBetreffLen(nt:eNetz):Integer;      { max. Betreffl„nge }
function ntPmReply(nt:eNetz):boolean;          { attrPmReply erzeugen }
function ntFollowup(nt:eNetz):boolean;         { Followup-To m”glich }
function ntCrossAM(nt:eNetz):boolean;          { AM-Crosspostings m”glich }
function ntCrossPM(nt:eNetz):boolean;          { PM-Crosspostings m”glich }
function ntOrigWeiter(nt:eNetz):boolean;       { Weiterleiten mit WAB  }
function ntBoxnameLen(nt:eNetz):byte;          { max. L„nge von Servernamen }
function ntPMTeleData(nt:eNetz):boolean;       { PMs: Telefon + Postanschrift }
function ntAMTeleData(nt:eNetz):boolean;       { AMs: Telefon + Postanschrift }
function ntSec(nt:eNetz):boolean;              { sekundengenaue Uhrzeit }
function ntOptIso(nt:eNetz):boolean;           { wahlweise ISO-Zeichensatz }
function ntIBM(nt:eNetz):boolean;              { IBM-Zeichensatz verwenden }
function ntPGP(nt:eNetz):boolean;              { PGP-Keys im Headaer }
function ntBrettebene(nt:eNetz):boolean;       { Netztyp mit Brettebene }
function ntBCC(nt:eNetz):boolean;              { BCC-Option vorhanden }
function ntFilename(nt:eNetz):boolean;         { Dateiname im Header }
function ntBoxNetztyp(box:string):eNetz;       { Netztyp der Box       }
function ntRelogin(nt:eNetz):byte;             { Relogin-Netcall m”glich }
function ntOnline(nt:eNetz):boolean;           { Online-Anruf m”glich  }
function ntNetcall(nt:eNetz):boolean;          { Netcall m”glich }
function ntOnePW(nt:eNetz):boolean;            { Point-PW = Online-PW  }
function ntDownarcPath(nt:eNetz):boolean;      { Entpacker muá im Pfad liegen }
function ntExtProt(nt:eNetz):boolean;          { externes š.-Protokoll }
function ntGrossPW(nt:eNetz):boolean;          { Paáwort muá groágeschr. werden }

function ntMAF(nt:eNetz):boolean;              { MAF statt MAPS        }
function ntQuickMaps(nt:eNetz):boolean;        { Maps-Bestellung an Sysop }
function ntNude(nt:eNetz):boolean;             { Mausnet-"CMD"-Maps    }
function ntAreamgr(nt:eNetz):boolean;          { Fido-Areafix/Areamgr  }
function ntProMaf(nt:eNetz):boolean;           { Pronet-System         }
function ntNoMaps(nt:eNetz):boolean;           { kein Maps-Service     }
function ntMapsOthers(nt:eNetz):boolean;       { Maps/Sonstige         }
function ntMapsBrettliste(nt:eNetz):boolean;   { Maps/Liste_anfordern  }

function ntEnvelopes(nt: eNetz):boolean;   { Trennung Envelope/Header }
function ntReplyToAll (nt :eNetz):boolean;    { Reply-To-All erlaubt }

function FormMsgId(MsgID: String): String;
function grosschar(b:boolean):string;

(* todo: Entwicklungsleichen?
function ntValidAddress(nt:byte;const addr:string):boolean;
function ntNormalizeAddress(nt:byte;var addr:string):boolean;
*)

function  dbNetztyp(d: DB):eNetz;

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

function ntZConnect(nt:eNetz):boolean;
begin
  ntZConnect:= nt>nt_1;
end;

function mbNetztyp:eNetz;
begin
  mbNetztyp:=eNetz(dbReadInt(mbase,'netztyp'));
end;

function  dbNetztyp(d: DB):eNetz;
begin
  Result := eNetz(dbReadInt(d,'netztyp'));
end;


function ntBinary(nt:eNetz):boolean;
begin
  ntBinary:=(nt in [nt_Netcall,nt_ZCONNECT,nt_Quick,nt_GS,nt_Maus,
                    nt_UUCP, nt_POP3, nt_IMAP, nt_NNTP, nt_Client]) or
            (fidobin and (nt=nt_Fido));
end;

function ntMIME(nt:eNetz):boolean;
begin
  ntMIME  :=(nt in [nt_UUCP, nt_POP3, nt_IMAP, nt_NNTP, nt_Client])  or
            (zc_mime and (nt in [nt_ZConnect]));
end;

function ntBinEncode(nt:eNetz):boolean;        { Binaermails werden uucodiert }
begin
  ntBinEncode:= nt in [nt_Maus, nt_Fido];
end;


function ntBinaryBox(box:string):boolean;
begin
  ntBinaryBox:=ntBinary(ntBoxNetztyp(box));
end;

(*                                                      -- see above --
       midNone      = 0;
       midMausNet   = 1;
       midNetcall   = 2;
       midProNet    = 6;
       midFido      = 10;
       midZConnect  = 20;
       midRFC       = 30;
*)
function ntMessageID(nt:eNetz):byte;
begin
  case nt of
    nt_Netcall  : ntMessageID:=midNetcall;      { @BOX }
    nt_ZConnect : ntMessageID:=midZConnect;     { @POINT.BOX.zer.sub.org }
    nt_Magic    : ntMessageID:=midZConnect;     { [_point]@system.seven.sub.org }
    nt_Pronet   : ntMessageID:=midProNet;       { X/HHMMSSssDDMMYYYY_KKK@BOX;NR.pro }
    nt_Quick    : ntMessageID:=midNetcall;      { @POINT }
    nt_GS       : ntMessageID:=midNetcall;      { 0815@POINT }
    nt_Maus     : ntMessageID:=midMausNet;      { 0815@BOX }
    nt_Fido     : ntMessageID:=midFido;         { net:zone/node.point[@domain] xxxxxxxx }
    nt_UUCP,
    nt_NNTP,
    nt_POP3,
    nt_Client   : ntMessageID:=midRFC;          { @point.do.main }
  else  { QWK }
    ntMessageID := midNetcall;
  end;
end;


{ Replys auf eigene Nachrichten werden anhand der BEZ-Domain erkannt: }

function ntDomainReply(nt:eNetz):boolean;
begin
  ntDomainReply:=(ntMessageID(nt) in [midZConnect,midRFC]);
end;


function ntConv(nt:eNetz):boolean;
begin
  ntConv:= nt>nt_ZConnect;
end;


function ntZCablage(ablg:byte):boolean;
begin
  ntZCablage:=(ablg>9);
end;

function ntClass(nt:eNetz):TNetClass;         { Netz-Uebertyp }
begin
  case nt of
    nt_Netcall: Result := ncZConnect;
    nt_ZConnect:Result := ncZConnect;
    nt_Maus:    Result := ncMaus;
    nt_Fido,
    nt_QWK:     Result := ncFTN;
    nt_UUCP,
    nt_Client,
    nt_NNTP,
    nt_POP3,
    nt_IMAP:    Result := ncRFC;
    else        Result := ncNone;
  end;
end;

function ntBoxNetztyp(box:string):eNetz;
var d  : DB;
begin
  if box='' then box:=DefaultBox;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then
    dbRead(d,'netztyp',Result)
  else
    Result:=nt_Netcall;
  dbClose(d);
end;


function ntZDatum(nt:eNetz):boolean;
begin
  ntZDatum:= nt>=nt_ZConnect;
end;


function FormMsgId(MsgID: String): String;
var
  p: Integer;
begin
  if msgid='' then
    Result :=''
  else
  begin
    p := cPosX('@',msgid)+1;
    while p<=length(msgid) do
    begin
      msgid[p] := UpCase(msgid[p]);
      inc(p)
    end;
    Result := dbLongStr(CRC32Str(msgid))+LeftStr(msgid,15);
  end;
end;


function ntMAF(nt:eNetz):boolean;
begin
  ntMAF:=(nt=nt_Magic);
end;

function ntProMaf(nt:eNetz):boolean;
begin
  ntProMaf:=(nt=nt_Pronet);
end;

function ntQuickMaps(nt:eNetz):boolean;
begin
  ntQuickMaps:=(nt=nt_Quick);
end;

function ntNude(nt:eNetz):boolean;
begin
  ntNude:=(nt=nt_Maus);
end;

function ntAreamgr(nt:eNetz):boolean;
begin
  ntAreamgr:=(nt=nt_Fido);
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }
function ntNoMaps(nt:eNetz):boolean;
begin
  ntNoMaps:= nt in [nt_POP3];
end;
{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

function ntMapsOthers(nt:eNetz):boolean;       { Maps/Sonstige         }
begin
  ntMapsOthers:=not (nt in [nt_Quick,nt_Pronet,nt_QWK,nt_NNTP,nt_POP3]);
end;

function ntMapsBrettliste(nt:eNetz):boolean;   { Maps/Liste_anfordern  }
begin
  ntMapsBrettliste:=(nt<>nt_QWK);
end;

function ntDomainType(nt:eNetz):byte;
begin
  case nt of
    nt_Netcall, nt_1 {???}  : ntDomainType:=0;   { @BOX.ZER [@POINT.ZER] }
    nt_ZConnect             : ntDomainType:=5;   { @BOX.domain [@POINT.domain] }
    nt_Magic                : ntDomainType:=1;   { @POINT oder @BOX }
    nt_Pronet               : ntDomainType:=7;   { @BOX;POINT }
    nt_Quick, nt_GS         : ntDomainType:=2;   { @POINT }
    nt_Maus, nt_QWK, nt_90  : ntDomainType:=3;   { @BOX }
    nt_Fido                 : ntDomainType:=4;   { @Net:Zone/Node.Point = @Box.Point }
    nt_UUCP                 : ntDomainType:=6;   { @point.domain }
  else // (POP3, NNTP, IMAP, Client)
    ntDomainType:=8;   { eMail-Adresse ('email') }
  end;
end;


function ntAutoZer(nt:eNetz):boolean;
begin
  ntAutoZer:=(nt<=nt_1);
end;

function ntAutoDomain(box:string; ownbox:boolean):string;
var d  : DB;
    nt : eNetz; //shortint;
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
      if ntDomainType(nt) in [5,6,8] then
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
    if dbNetztyp(d)<>nt_UUCP then
      ntServerDomain:=ntAutoDomain(box,true)
    else
      ntServerDomain:=dbReadStr(d,'boxdomain');
  dbClose(d);
end;


function ntDefaultDomain(nt:eNetz):string;     { Domain fuer neue Boxen }
begin
  case nt of
    nt_Pronet   : ntDefaultDomain:='.pro';
    nt_ZConnect : ntDefaultDomain:='.invalid';
    nt_Fido     : ntDefaultDomain:='fidonet';
  else
    ntDefaultDomain:='';
  end;
end;


function ntGrossUser(nt:eNetz):boolean;
begin
  ntGrossUser:=(not smallnames and (nt=nt_Netcall)) or
               (nt in [nt_Magic,nt_Quick,nt_GS,nt_Pronet]);
end;


function ntGrossBrett(nt:eNetz):boolean;
begin
  ntGrossBrett:=(nt=nt_Netcall) or (nt=nt_Fido) or (nt=nt_Pronet);
end;


function ntKleinBrett(nt:eNetz):boolean;       { Bretter-Kleinschreibung }
begin
  ntKleinBrett:=(nt in [nt_UUCP, nt_NNTP, nt_Client]);
end;


function grosschar(b:boolean):string;
begin
  if b then grosschar:='>'
  else grosschar:='';
end;


function ntName(nt:eNetz):string;
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
    Debug.DebugLog('xpnt','Unknown net type: '+IntToStr(ord(nt)),DLWarning);
    ntName:='? '+IntToStr(ord(nt));
  end;
end;


{ 0=kein Relogin, 1=Relogin nur mit Script, 2=Relogin immer m”glich }

function ntRelogin(nt:eNetz):byte;
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

function ntOnline(nt:eNetz):boolean;   { false -> Script erforderlich }
begin
  ntOnline:=not (nt in [nt_Fido, nt_GS, nt_UUCP, nt_QWK, nt_NNTP, nt_POP3, nt_Client]);
end;

function ntNetcall(nt:eNetz):boolean;          { Netcall m”glich }
begin
  ntNetcall:=(nt<>nt_QWK);
end;


function ntOnePW(nt:eNetz):boolean;
begin
  ntOnePW:=(nt=nt_Maus);
end;

function ntKomkette(nt:eNetz):boolean;
begin
  ntKomkette:=
    (nt in [nt_Maus,nt_Fido,nt_ZConnect,nt_UUCP,nt_QWK,nt_Pronet,nt_NNTP, nt_POP3, nt_Client])
    or ((nt=nt_Magic) and MaggiVerkettung);
end;


function ntRfcCompatibleID(nt:eNetz):boolean;
begin
  ntRfcCompatibleID:=nt in [nt_ZConnect,nt_Magic,nt_UUCP,nt_NNTP,nt_POP3, nt_Client];
end;

function ntMIDCompatible(n1,n2:eNetz):boolean;  { austauschbare MsgIDs  }
begin
  ntMIDcompatible:=(n1=n2) or (ntRfcCompatibleID(n1) and ntRfcCompatibleID(n2));
end;

function ntOrigID(nt:eNetz):boolean;           { X-XP-ORGMID -> X-XP-ORGREF }
begin
  ntOrigID:= nt in [nt_Maus, nt_Fido];
end;

function ntAdrCompatible(n1,n2:eNetz):boolean;  { umleitbare PM-Adresse }
begin
  ntAdrCompatible:= (n1 in ([nt_Maus, nt_ZConnect]+netsRFC)) and
                    (n2 in ([nt_Maus, nt_ZConnect]+netsRFC));
end;

function ntMsg0(nt:eNetz):boolean;             { Nachricht darf leer sein }
begin
  ntMsg0:=nt in ([nt_ZConnect,nt_Fido]+netsRFC);
end;


function ntNameSpace(nt:eNetz):boolean;        { Leerzeichen in Usernamen }
begin
  ntNameSpace:=(nt in [nt_Fido,nt_Magic,nt_Maus,nt_Quick,nt_GS,nt_Pronet,nt_QWK]);
end;


function ntDownarcPath(nt:eNetz):boolean;      { Entpacker muá im Pfad liegen }
begin
  ntDownarcPath:= nt in [nt_Fido, nt_ZConnect];
end;

function ntBrettEmpf(nt:eNetz):boolean;        { Fido-To }
begin
  ntBrettEmpf:= nt in [nt_Fido, nt_QWK, nt_Magic, nt_Pronet];
end;

function ntBrettEmpfUsed:boolean;             { Netztypen mit Fido-To vorh. }
begin
  ntBrettEmpfUsed:= ntUsed[nt_Fido] + ntUsed[nt_QWK] + ntUsed[nt_Magic]+
                    ntUsed[nt_Pronet] > 0;
end;

function ntEditbrettEmpf(nt:eNetz):boolean;    { dito, aber editierbar;  }
begin                                         { Empf„nger in mbase.name }
  ntEditBrettEmpf:= nt in [nt_Fido, nt_QWK];
end;


function ntRealname(nt:eNetz):boolean;         { Realnames m”glich }
begin
  ntRealname:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3, nt_Client];
end;


function ntRealUmlaut(nt:eNetz):boolean;       { Umlaute im Realname }
begin
  ntRealUmlaut:=nt in [nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3, nt_Client];
end;


function ntHeaderUmlaut(nt:eNetz):boolean;     { Umlaute in Keywords etc. }
begin
  ntHeaderUmlaut:=nt in [nt_ZCONNECT,nt_Magic,nt_Pronet,nt_UUCP,nt_NNTP,nt_POP3, nt_Client];
end;

function ntCancel(nt:eNetz):boolean;           { Cancel-Messages m”glich }
begin
  ntCancel:=nt in [nt_UUCP,nt_Maus,nt_ZConnect,NT_NNTP, nt_Client];
end;

function ntCancelPM(nt:eNetz):boolean;         { Cancel auch bei PM m”glich }
begin
  ntCancelPM:=(nt=nt_Maus);
end;

function ntErsetzen(nt:eNetz):boolean;         { Supersedes/Ersetzt m”glich }
begin
  ntErsetzen:=nt in [nt_UUCP,nt_ZConnect,nt_NNTP,nt_POP3, nt_Client];
end;

function ntBetreffLen(nt:eNetz):Integer;      { max. Betreffl„nge }
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


function ntPmReply(nt:eNetz):boolean;          { attrPmReply erzeugen }
begin
  ntPmReply:=nt in [nt_Maus, nt_NNTP,nt_POP3,nt_IMAP,nt_UUCP,nt_Client];
end;


function ntFollowup(nt:eNetz):boolean;         { Followup-To m”glich }
begin
  ntFollowup:=nt in [nt_ZConnect,nt_UUCP,nt_NNTP];
end;


function ntCrossAM(nt:eNetz):boolean;          { AM-Crosspostings m”glich }
begin
  ntCrossAM:=(nt in [nt_UUCP,nt_NNTP, nt_Client]) or ((nt=nt_ZConnect) and zc_xposts);
end;

function ntCrossPM(nt:eNetz):boolean;          { PM-Crosspostings m”glich }
begin
  ntCrossPM:=nt in [nt_ZConnect,nt_UUCP,nt_POP3];
end;


function ntExtProt(nt:eNetz):boolean;          { externes š.-Protokoll }
begin
  ntExtProt:=not (nt in [nt_Fido,nt_UUCP,nt_QWK,nt_NNTP,nt_POP3]);
end;


function ntOrigWeiter(nt:eNetz):boolean;       { Weiterleiten mit WAB  }
begin
  ntOrigWeiter:=nt in [nt_ZConnect,nt_UUCP,nt_Maus,nt_NNTP,nt_POP3, nt_Client];
end;


function ntBoxnameLen(nt:eNetz):byte;
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
    nt_POP3,
    nt_Client  : ntBoxnameLen:=20;
  else
    ntBoxnameLen:=8;   { Netcall }
  end;
end;


function ntPMTeleData(nt:eNetz):boolean;        { Telefon + Postanschrift }
begin
  ntPMTeleData:=nt in [nt_ZConnect,nt_UUCP,nt_NNTP,nt_POP3, nt_Client];
end;

function ntAMTeleData(nt:eNetz):boolean;        { Telefon + Postanschrift }
begin
  ntAMTeleData:=((nt=nt_ZConnect) and not adrpmonly) or (nt=nt_POP3);
end;


function ntSec(nt:eNetz):boolean;              { sekundengenaue Uhrzeit }
begin
  ntSec:=(nt in [nt_ZCONNECT,nt_UUCP,nt_Magic,nt_Pronet,nt_NNTP,nt_POP3, nt_Client]);
end;


function ntZonly:boolean;                     { nur Z-Netz/alt }
var i : eNetz;
begin
  i:=nt_99;
  while (i>nt_Netcall) and (ntUsed[i]=0) do dec(i);
  ntZonly:= (i=nt_Netcall);
end;

// Network types where the IBM charset is default but can be changed
// to ISO-8859-*
function ntOptIso(nt:eNetz):boolean;           { wahlweise ISO-Zeichensatz }
begin
  ntOptIso:=(nt=nt_ZConnect);
end;

// Network types where the IBM charset is default, otherwise we use
// ISO-8859-1 or UTF-8
function ntIBM(nt:eNetz):boolean;
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

function ntPGP(nt:eNetz):boolean;              { PGP-Keys im Header }
begin
  ntPGP:=(nt=nt_ZCONNECT) or
         ((nt in [nt_UUCP,nt_NNTP,nt_POP3, nt_Client]) and PGP_UUCP) or
         ((nt=nt_Fido) and PGP_Fido);
end;


function ntGrossPW(nt:eNetz):boolean;       { Paáwort muá groágeschr. werden }
begin
  ntGrossPW:=(nt in [nt_Netcall,nt_Magic,nt_Pronet,nt_Quick,nt_GS]);
end;


{ XP-Control-Messages fr SUPPORT.CFG }

function ntXPctl(nt:eNetz):boolean;
begin
  ntXPctl:=(nt in [nt_ZConnect,nt_UUCP,nt_Fido,nt_NNTP,nt_POP3, nt_Client]);
end;


function ntBrettebene(nt:eNetz):boolean;       { Netztyp mit Brettebene }
begin
  ntBrettebene := (nt in [nt_Fido,nt_Maus,nt_QWK,nt_Magic,nt_Pronet]);
end;


function ntBCC(nt:eNetz):boolean;              { BCC-Option vorhanden }
begin
  ntBCC := (nt in ([nt_ZConnect]+netsRFC));
end;


function ntFilename(nt:eNetz):boolean;         { Dateiname im Header }
begin
  ntFilename := (nt in [nt_ZConnect,nt_UUCP, nt_Client]);
end;

function ntReplyToAll (nt :eNetz):boolean;    { Reply-To-All allowed? }
begin
  // only LSB contains net type
  ntReplyToAll := nt in [nt_ZConnect, nt_UUCP, nt_POP3, nt_NNTP, nt_CLient];
end;

function ntEnvelopes(nt: eNetz):boolean;   { Trennung Envelope/Header }
begin
  result := nt in (netsRFC + [nt_Maus]);
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
  Revision 1.51  2002/12/13 14:31:15  dodi
  - introduced new types

  Revision 1.50  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.49  2002/07/28 11:31:46  cl
  - BUGFIX: [ 587626 ] 3.9: EBs verschandeln Subject
  - BUGFIX: [ 587388 ] 3.9: EBs gehen nicht immer

  Revision 1.48  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.47  2002/06/12 09:14:53  mk
  - removed some length limits including AdressLength (for RFC nets only)

  Revision 1.46  2002/05/08 09:10:48  ma
  - added Fido default domain

  Revision 1.45  2002/04/22 10:04:22  mk
  - fixed crashes with delphi in non debug mode (asm registers had to be preserved)

  Revision 1.44  2002/04/14 22:29:46  cl
  - added types TNetClass, TAttachMode, TAddressType
          consts mid* (message id type), netsFTN

  Revision 1.43  2002/03/03 15:51:31  cl
  - added ntEnvelope

  Revision 1.42  2002/02/13 18:19:53  mk
  - improvements for THeader and ClrUVS

  Revision 1.41  2001/12/24 23:07:04  mk
  - updates for nt_Client

  Revision 1.40  2001/12/23 12:00:32  mk
  - added some nt_Client

  Revision 1.39  2001/12/02 12:11:21  cl
  - got two range check errors

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


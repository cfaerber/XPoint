{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Netztypen   PM 04/92 }
{ s. auch NETZTYP.DOC  }

{$I XPDEFINE.INC }
{$F+,O+}

unit xpnt;

interface

uses   xp0,typeform,datadef,database,xpcrc32;

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
       nt_Turbo     = 90;        { TurboBox             }

       ltNetcall    = 0;         { Login/Transfer-Typen }
       ltZConnect   = 2;         { XRef: XP7            }
       ltMagic      = 3;
       ltQuick      = 5;
       ltGS         = 8;
       ltMaus       = 6;
       ltFido       = 7;
       ltTurbo      = 9;
       ltUUCP       = 10;
       ltQWK        = 11;


var ntused : array[0..99] of integer;

function ntZConnect(nt:byte):boolean;         { Ablagentyp ermitteln  }
function ntConv(nt:byte):boolean;             { konvertiertes Format  }
function ntZCablage(ablg:byte):boolean;       { ZConnect-Ablage       }
function ntName(nt:byte):string;              { s. auch XP4O.MsgInfo() }
function mbNetztyp:byte;
function ntZonly:boolean;                     { nur Z-Netz/alt }
function ntXPctl(nt:byte):boolean;            { XP-Control-Messages }

function ntBinary(nt:byte):boolean;           { BinÑrmails erlaubt    }
function ntBinaryBox(box:string):boolean;     { dito                  }
function ntBinEncode(nt:byte):boolean;        { BinÑrmails werden uucodiert }
function ntMessageID(nt:byte):byte;           { Message-ID-Format     }
function ntDomainReply(nt:byte):boolean;      { Replys auf eig. Nachr. erkennbar }
function ntZDatum(nt:byte):boolean;           { langes Datumsformat   }
function ntDomainType(nt:byte):byte;          { Domain fÅr Absender + MsgID }
function ntAutoZer(nt:byte):boolean;          { .ZER-Pflicht          }
function ntAutoDomain(box:string; ownbox:boolean):string;   { .Domain }
function ntServerDomain(box:string):string;   { Domain des Servers    }
function ntDefaultDomain(nt:byte):string;     { Domain fÅr neue Boxen }
function ntGrossUser(nt:byte):boolean;        { User-Gro·schreibung   }
function ntGrossBrett(nt:byte):boolean;       { Bretter-Gro·schreibung }
function ntKleinBrett(nt:byte):boolean;       { Bretter-Kleinschreibung }
function ntKomkette(nt:byte):boolean;         { Kommentar-Verkettung  }
function ntUserIBMchar(nt:byte):boolean;      { Default/User: IBM=J   }
function ntRfcCompatibleID(nt:byte):boolean;  { RFC-Msgid             }
function ntMIDCompatible(n1,n2:byte):boolean; { austauschbare MsgIDs  }
function ntOrigID(nt:byte):boolean;           { X-XP-ORGMID -> X-XP-ORGREF }
function ntAdrCompatible(n1,n2:byte):boolean; { umleitbare PM-Adresse }
function ntEmpfBest(nt:byte):boolean;         { EB-Flag im Header     }
function ntMsg0(nt:byte):boolean;             { Nachricht darf leer sein }
function ntNameSpace(nt:byte):boolean;        { Leerzeichen in Usernamen }
function ntDataQuestBetreff(nt:byte):boolean; { Betreff bei DB-Abfrage   }
function ntBrettEmpf(nt:byte):boolean;        { Fido-To }
function ntBrettEmpfUsed:boolean;             { Netztypen mit Fido-To vorh. }
function ntEditbrettEmpf(nt:byte):boolean;    { dito, aber editierbar }
function ntRealname(nt:byte):boolean;         { Realnames mîglich }
function ntRealUmlaut(nt:byte):boolean;       { Umlaute im Realname }
function ntHeaderUmlaut(nt:byte):boolean;     { Umlaute in Keywords etc. }
function ntCancel(nt:byte):boolean;           { Cancel-Messages mîglich }
function ntCancelPM(nt:byte):boolean;         { Cancel auch bei PM mîglich }
function ntBetreffLen(nt:byte):byte;          { max. BetrefflÑnge }
function ntPmReply(nt:byte):boolean;          { attrPmReply erzeugen }
function ntFollowup(nt:byte):boolean;         { Followup-To mîglich }
function ntCrossAM(nt:byte):boolean;          { AM-Crosspostings mîglich }
function ntCrossPM(nt:byte):boolean;          { PM-Crosspostings mîglich }
function ntOrigWeiter(nt:byte):boolean;       { Weiterleiten mit WAB  }
function ntBoxnameLen(nt:byte):byte;          { max. LÑnge von Servernamen }
function ntPMTeleData(nt:byte):boolean;       { PMs: Telefon + Postanschrift }
function ntAMTeleData(nt:byte):boolean;       { AMs: Telefon + Postanschrift }
function ntMaxRef(nt:byte):byte;              { max. References }
function ntSec(nt:byte):boolean;              { sekundengenaue Uhrzeit }
function ntOptIso(nt:byte):boolean;           { wahlweise ISO-Zeichensatz }
function ntForceMailer(nt:byte):boolean;      { '... (unregistriert)' anzeigen }
function ntPGP(nt:byte):boolean;              { PGP-Keys im Headaer }
function ntBrettebene(nt:byte):boolean;       { Netztyp mit Brettebene }
function ntBCC(nt:byte):boolean;              { BCC-Option vorhanden }
function ntFilename(nt:byte):boolean;         { Dateiname im Header }

function ntBoxNetztyp(box:string):byte;       { Netztyp der Box       }
function ntTransferType(nt:byte):shortint;    { Art des Netcalls      }
function ntRelogin(nt:byte):byte;             { Relogin-Netcall mîglich }
function ntOnline(nt:byte):boolean;           { Online-Anruf mîglich  }
function ntNetcall(nt:byte):boolean;          { Netcall mîglich }
function ntOnePW(nt:byte):boolean;            { Point-PW = Online-PW  }
function ntPackPuf(nt:byte):boolean;          { gepackte öbertragung mîgl. }
function ntDownarcPath(nt:byte):boolean;      { Entpacker mu· im Pfad liegen }
function ntExtProt(nt:byte):boolean;          { externes ö.-Protokoll }
function ntISDN(nt:byte):boolean;             { ISDN/CAPI mîglich }
function ltMultiPuffer(lt:byte):boolean;      { Puffer zusammenkopieren }
function ntGrossPW(nt:byte):boolean;          { Pa·wort mu· gro·geschr. werden }

function ntMAF(nt:byte):boolean;              { MAF statt MAPS        }
function ntQuickMaps(nt:byte):boolean;        { Maps-Bestellung an Sysop }
function ntNude(nt:byte):boolean;             { Mausnet-"CMD"-Maps    }
function ntAreamgr(nt:byte):boolean;          { Fido-Areafix/Areamgr  }
function ntProMaf(nt:byte):boolean;           { Pronet-System         }
function ntNoMaps(nt:byte):boolean;           { kein Maps-Service     }
function ntMapsOthers(nt:byte):boolean;       { Maps/Sonstige         }
function ntMapsBrettliste(nt:byte):boolean;   { Maps/Liste_anfordern  }

function ltVarBuffers(lt:byte):boolean;       { variable Puffernamen  }

function formmsgid(msgid:string):string;
function grosschar(b:boolean):string;

implementation  { ---------------------------------------------------- }


{ X-XP-NTP:  Netztyp - optional, Default 2 (nt_ZConnect)
  X-XP-ARC:  archivierte PM - optional
  X-XP-BOX:  Absendebox - falls Absendebox nicht im Absender steht
  X-XP-PNT:  Absendepoint (MagicNET)
  X-XP-BST:  Maus PM-Bearbeitungsstatus
  X-XP-ATT:  Nachrichten-Attribute, Fido & Maus, Hex(4)
  X-XP-FTO:  Fido-EmpfÑnger bei Echomail
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
                    nt_UUCP,nt_Turbo]) or
            (fidobin and (nt=nt_Fido));
end;

function ntBinEncode(nt:byte):boolean;        { BinÑrmails werden uucodiert }
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
    nt_UUCP     : ntMessageID:=5;     { @point.do.main }
  else  { Turbo, QWK }
    ntMessageID:=1;
  end;
end;


{ Replys auf eigene Nachrichten werden anhand der BEZ-Domain erkannt: }

function ntDomainReply(nt:byte):boolean;  
begin
  ntDomainReply:=(ntMessageID(nt) in [2,5]);
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
  dbSeek(d,boiName,ustr(box));
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
    formmsgid:=dbLongStr(CRC32(msgid))+left(msgid,15);
    end;
end;


function ntTransferType(nt:byte):shortint;
begin
  case nt of
    0,1 : ntTransferType:=ltNetcall;
    2   : ntTransferType:=ltZConnect;
    3,4 : ntTransferType:=ltMagic;
    10  : ntTransferType:=ltQuick;
    11  : ntTransferType:=ltGS;
    20  : ntTransferType:=ltMaus;
    30  : ntTransferType:=ltFido;
    31  : ntTransferType:=ltQWK;
    40  : ntTransferType:=ltUUCP;
    90  : ntTransferType:=ltTurbo;
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

function ntNoMaps(nt:byte):boolean;
begin
  ntNoMaps:=false;
end;

function ntMapsOthers(nt:byte):boolean;       { Maps/Sonstige         }
begin
  ntMapsOthers:=(nt<>nt_Quick) and (nt<>nt_Turbo) and (nt<>nt_Pronet) and
                (nt<>nt_QWK);
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
  dbSeek(d,boiName,ustr(box));
  if dbFound then begin
    dbRead(d,'netztyp',nt);
    if ntAutoZer(nt) then
      ntAutoDomain:='.ZER'
    else if (nt=nt_ZConnect) and not ownbox then
      ntAutoDomain:='.do.main'
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
  dbSeek(d,boiName,ustr(box));
  if dbFound then
    if dbReadInt(d,'netztyp')<>nt_UUCP then
      ntServerDomain:=ntAutoDomain(box,true)
    else
      ntServerDomain:=dbReadStr(d,'boxdomain');
  dbClose(d);
end;


function ntDefaultDomain(nt:byte):string;     { Domain fÅr neue Boxen }
begin
  case nt of
    nt_Pronet   : ntDefaultDomain:='.pro';
    nt_ZConnect : ntDefaultDomain:='.do.main';
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
  ntKleinBrett:=(nt=nt_UUCP);
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
    nt_Turbo    : ntName:='Turbo-Box';
    nt_UUCP     : ntName:='RFC/UUCP';
  else
    ntName:='???';
  end;
end;


{ 0=kein Relogin, 1=Relogin nur mit Script, 2=Relogin immer mîglich }

function ntRelogin(nt:byte):byte;
begin
  case nt of
    nt_Fido,nt_QWK : ntRelogin:=0;
    nt_GS,nt_Turbo,
    nt_UUCP        : ntRelogin:=1;
    else             ntRelogin:=2;
  end;
end;

function ntOnline(nt:byte):boolean;   { false -> Script erforderlich }
begin
  ntOnline:=(nt<>nt_Fido) and (nt<>nt_GS) and (nt<>nt_UUCP) and (nt<>nt_QWK);
end;

function ntNetcall(nt:byte):boolean;          { Netcall mîglich }
begin
  ntNetcall:=(nt<>nt_QWK);
end;


function ntOnePW(nt:byte):boolean;
begin
  ntOnePW:=(nt=nt_Maus) or (nt=nt_Turbo);
end;

function ntKomkette(nt:byte):boolean;
begin
  ntKomkette:=
    (nt in [nt_Maus,nt_Fido,nt_ZConnect,nt_UUCP,nt_QWK,nt_Pronet,nt_Turbo])
    or ((nt=nt_Magic) and MaggiVerkettung);
end;


function ltVarBuffers(lt:byte):boolean;       { variable Puffernamen }
begin
  ltVarBuffers:=(lt=ltFido);   { evtl. ltUsenet }
end;


function ntUserIBMchar(nt:byte):boolean;      { Default/User: IBM=J   }
begin
  ntUserIBMchar:=newuseribm or
                 ((nt<>nt_Fido) and (nt<>nt_QWK) and (nt<>nt_Turbo) and
                  (nt<>nt_UUCP));
end;


function ntRfcCompatibleID(nt:byte):boolean;
begin
  ntRfcCompatibleID:=(nt=nt_ZConnect) or (nt=nt_Magic) or (nt=nt_UUCP);
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
  ntAdrCompatible:=(n1=n2) or
                   (((n1=nt_ZConnect) or (n1=nt_UUCP)) and
                     (n2 in [nt_ZConnect,nt_UUCP,nt_Maus]));
end;

function ntEmpfBest(nt:byte):boolean;
begin
  ntEmpfBest:=(nt=nt_Fido) or (nt=nt_ZConnect) or (nt=nt_Magic) or
              (nt=nt_UUCP);
end;


function ntMsg0(nt:byte):boolean;             { Nachricht darf leer sein }
begin
  ntMsg0:=(nt=nt_ZConnect) or (nt=nt_Fido) or (nt=nt_UUCP);
end;


function ntNameSpace(nt:byte):boolean;        { Leerzeichen in Usernamen }
begin
  ntNameSpace:=(nt in [nt_Fido,nt_Magic,nt_Maus,nt_Quick,nt_GS,nt_Pronet,nt_QWK]);
end;


function ntPackPuf(nt:byte):boolean;          { gepackte öbertragung mîgl. }
begin
  ntPackPuf:=(nt<>nt_Turbo);
end;

function ntDownarcPath(nt:byte):boolean;      { Entpacker mu· im Pfad liegen }
begin
  ntDownarcPath:=(nt=nt_Fido) or (nt=nt_ZConnect);
end;


function ntDataQuestBetreff(nt:byte):boolean; { Betreff bei DB-Abfrage   }
begin
  ntDataQuestBetreff:=false;  { (nt=nt_Turbo); }
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
begin                                         { EmpfÑnger in mbase.name }
  ntEditBrettEmpf:=(nt=nt_Fido) or (nt=nt_QWK);
end;


function ntRealname(nt:byte):boolean;         { Realnames mîglich }
begin
  ntRealname:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP,nt_Turbo];
end;


function ntRealUmlaut(nt:byte):boolean;       { Umlaute im Realname }
begin
  ntRealUmlaut:=nt in [nt_Magic,nt_Pronet,nt_UUCP];
end;


function ntHeaderUmlaut(nt:byte):boolean;     { Umlaute in Keywords etc. }
begin
  ntHeaderUmlaut:=nt in [nt_ZCONNECT,nt_Magic,nt_Pronet,nt_UUCP];
end;


function ntCancel(nt:byte):boolean;           { Cancel-Messages mîglich }
begin
  ntCancel:=(nt=nt_UUCP) or (nt=nt_Maus);
end;

function ntCancelPM(nt:byte):boolean;         { Cancel auch bei PM mîglich }
begin
  ntCancelPM:=(nt=nt_Maus);
end;


function ntBetreffLen(nt:byte):byte;          { max. BetrefflÑnge }
begin
  case nt of
    nt_Netcall : ntBetreffLen:=40;
  { nt_Maus    : ntBetreffLen:=30; }
    nt_Turbo   : ntBetreffLen:=40;
    nt_Magic   : ntBetreffLen:=60;
  { nt_QWK     : ntBetreffLen:=25; }
    nt_Pronet  : ntBetreffLen:=40;
  else
    ntBetreffLen:=BetreffLen;
  end;
end;


function ntPmReply(nt:byte):boolean;          { attrPmReply erzeugen }
begin
  ntPmReply:=(nt=nt_Maus) or (nt=nt_UUCP);
end;


function ntFollowup(nt:byte):boolean;         { Followup-To mîglich }
begin
  ntFollowup:=(nt=nt_ZConnect) or (nt=nt_UUCP);
end;


function ntCrossAM(nt:byte):boolean;          { AM-Crosspostings mîglich }
begin
  ntCrossAM:=(nt=nt_UUCP) or ((nt=nt_ZConnect) and zc_xposts);
end;

function ntCrossPM(nt:byte):boolean;          { PM-Crosspostings mîglich }
begin
  ntCrossPM:=(nt=nt_ZConnect) or (nt=nt_UUCP);
end;


function ntExtProt(nt:byte):boolean;          { externes ö.-Protokoll }
begin
  ntExtProt:=not (nt in [nt_Fido,nt_UUCP,nt_Turbo,nt_QWK]);
end;

function ntISDN(nt:byte):boolean;             { ISDN/CAPI mîglich }
begin
  ntISDN:=true; { MK 28.01.2000: CAPI-Support eingeschaltet }
end;


function ntOrigWeiter(nt:byte):boolean;       { Weiterleiten mit WAB  }
begin
  ntOrigWeiter:=(nt=nt_ZConnect) or (nt=nt_UUCP) or (nt=nt_Maus);
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
    nt_UUCP    : ntBoxnameLen:=20;
  else
    ntBoxnameLen:=8;   { Netcall, Turbo }
  end;
end;


function ntPMTeleData(nt:byte):boolean;        { Telefon + Postanschrift }
begin
  ntPMTeleData:=(nt=nt_ZConnect) or (nt=nt_UUCP);
end;

function ntAMTeleData(nt:byte):boolean;        { Telefon + Postanschrift }
begin
  ntAMTeleData:=(nt=nt_ZConnect) and not adrpmonly;
end;


function ntMaxRef(nt:byte):byte;              { max. References }
begin
  case nt of
    nt_UUCP     : ntMaxRef:=5;
    nt_ZConnect : ntMaxRef:=3;
  else            ntMaxRef:=1;
  end;
end;


function ntSec(nt:byte):boolean;              { sekundengenaue Uhrzeit }
begin
  ntSec:=(nt in [nt_ZCONNECT,nt_UUCP,nt_Magic,nt_Pronet]);
end;


function ltMultiPuffer(lt:byte):boolean;      { Puffer zusammenkopieren }
begin
  ltMultiPuffer:=(lt in [ltZConnect,ltGS]);
end;

function ntZonly:boolean;                     { nur Z-Netz/alt }
var i : integer;
begin
  i:=99;
  while (i>0) and (ntUsed[i]=0) do dec(i);
  ntZonly:=(i=0);
end;

function ntOptIso(nt:byte):boolean;           { wahlweise ISO-Zeichensatz }
begin
  ntOptIso:=(nt=nt_ZConnect);
end;


function ntForceMailer(nt:byte):boolean;      { '... (unregistriert)' anzeigen }
begin
  ntForceMailer:=(nt in [nt_ZConnect,nt_UUCP]);
end;


function ntPGP(nt:byte):boolean;              { PGP-Keys im Header }
begin
  ntPGP:=(nt=nt_ZCONNECT) or
         ((nt=nt_UUCP) and PGP_UUCP) or
         ((nt=nt_Fido) and PGP_Fido);
end;


function ntGrossPW(nt:byte):boolean;       { Pa·wort mu· gro·geschr. werden }
begin
  ntGrossPW:=(nt in [nt_Netcall,nt_Magic,nt_Pronet,nt_Quick,nt_GS,nt_Turbo]);
end;


{ XP-Control-Messages fÅr SUPPORT.CFG }

function ntXPctl(nt:byte):boolean;
begin
  ntXPctl:=(nt in [nt_ZConnect,nt_UUCP,nt_Fido]);
end;


function ntBrettebene(nt:byte):boolean;       { Netztyp mit Brettebene }
begin
  ntBrettebene := (nt in [nt_Fido,nt_Maus,nt_QWK,nt_Magic,nt_Pronet,nt_Turbo]);
end;


function ntBCC(nt:byte):boolean;              { BCC-Option vorhanden }
begin
  ntBCC := (nt in [nt_ZConnect,nt_UUCP]);
end;


function ntFilename(nt:byte):boolean;         { Dateiname im Header }
begin
  ntFilename := (nt in [nt_ZConnect,nt_UUCP]);
end;


begin
  fillchar(ntused,sizeof(ntused),0);
end.

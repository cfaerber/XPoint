{ $Id$

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

{ Nachrichten versenden, weiterleiten, unversandt-bearbeiten }

{$I xpdefine.inc }

unit xpsendmessage;

interface

uses
  sysutils,
  typeform,fileio,inout,keys,datadef,database,maske,crc,lister, osdepend,
  winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,xp2c,xp_des,xpe, xpheader,
  xpglobal,xpsendmessage_attach,xpsendmessage_attach_analyze,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
Classes,fidoglob;

const sendIntern = 1;     { force Intern              }
      sendShow   = 2;     { ausfuehrliche Sendeanzeige }
      sendDelay  = 4;     { 0,5 s Warten              }
      sendQuote  = 8;     { akt. Nachricht quoten     }
      SendWAB    = 16;    { ABS->WAB, OAB->ABS        }
      SendReedit = 32;    { TED: Softbreaks umwandeln }
      SendHalt   = 64;    { Nachricht auf 'halten'    }
      SendMark   = 128;   { Nachricht markieren       }
      SendPGPkey = 256;   { PGP-Key-Header erzeugen   }
      SendPGPreq = 512;   { PGP-Key anfordern         }
      SendPGPsig = 1024;  { Nachricht signieren       }
      SendNokop  = 2048;  { STAT: NOKOP               }
      SendIQuote = 4096;  { indirekter Quote          }
      SendMPart  = 8192;  { Multipart zerlegen        }

      pgdown    : boolean = false;
      _sendmaps : boolean = false;
      forcebox  : string = '';
      forceabs  : string = '';       { 'SYSOP' fuer ProNet-System }
      _bezug    : string = '';
      _orgref   : string = '';
      _replypath: string = '';        { Box, ueber die die Bezugsnachr. kam }
      sendfilename   : string = '';
      sendfiledate   : string = '';
      force_quotemsk : string = '';
      CrosspostBox   : string = '';
      _beznet   : shortint = -1;         { Netztyp der Bezugsnachricht }
      _pmReply  : boolean = false;
      IsEbest   : boolean = false;
      NoCrash   : boolean = false;
      FileAttach: boolean = false;
      EditAttach: boolean = true;
      msgprio   : byte    = 0;           { ZConnect-Prio }
      rfcprio   : byte    = 0;           { RFC-Priority  }   { 6.2.2000 MH: } { unbedenklich }
      ControlMsg: boolean = false;
      newbrettgr: longint = 0;           { Gruppe fuer neues Brett }
      flCrash   : boolean = false;
      flQTo     : boolean = false;       { Maus: Wildwestverkettung }
      flNokop   : boolean = false;
      qmpdata   : pointer = nil;

      OldMsgSize: longint = 0;{ s. XP3.XWrite }
      OldMsgPos : longint = 0;

var
  SendEmpfList: TStringList;

var
      InternBox : string;  { Boxname bei /Netzanruf }
      msgMarkEmpf: byte;   { fuer sendMark }


function DoSend(pm:boolean; datei:string; is_temp,is_file:boolean;
                empfaenger,betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData:SendUUptr;
                const signat:string; sendFlags:word):boolean;
procedure send_file(pm,binary:boolean);
function SendPMmessage(betreff,fn:string; is_temp:boolean; var box:string):boolean;

function umlauttest(var s:string):boolean;
function test_senddate(var s:string):boolean;
procedure firstslash(var s:string);
function testreplyto(var s:string):boolean;

function pgpo_sigtest(var s:string):boolean;
function pgpo_keytest(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses mime, mime_analyze, rfc2822, StringTools, utftools, xp_pgp, xp1o, xp3,
  xp3ex, xp3o, xp3o2, xp4e, xp9bp, xpcc, xpconfigedit, xpfido, xpmakeheader,
  xpnt, xpsendmessage_internal, xpstreams;

procedure ukonv(typ:byte; var data; var bytes:word); assembler; {&uses ebx, esi, edi}
asm
         xor   edx, edx
         mov   edi,bytes
         mov   ecx,[edi]
         cmp   ecx, 0
         jne   @weiter
         jmp   @ende
@weiter: mov   edi, data
         lea   esi,[edi+1500]
         cld
         mov   bl,typ
         cmp   bl,2                    { ISO? }
         jz    @isolp

@uklp:   mov   al,[esi]              { IBM -> ASCII }
         cmp   al,'Ñ'
         jnz   @noae
         mov   ax,'ea'
         jmp   @conv
@noae:   cmp   al,'î'
         jnz   @nooe
         mov   ax,'eo'
         jmp   @conv
@nooe:   cmp   al,'Å'
         jnz   @noue
         mov   ax,'eu'
         jmp   @conv
@noue:   cmp   al,'é'
         jnz   @no_ae
         mov   ax,'eA'
         jmp   @conv
@no_ae:  cmp   al,'ô'
         jnz   @no_oe
         mov   ax,'eO'
         jmp   @conv
@no_oe:  cmp   al,'ö'
         jnz   @no_ue
         mov   ax,'eU'
         jmp   @conv
@no_ue:  cmp   al,'·'
         jnz   @noconv
         mov   ax,'ss'
@conv:   stosw
         inc   edx
         cmp   edx,1500
         jz    @ende                    { Konvertierpuffer voll :-( }
         inc   esi
         loop  @uklp1
         jmp   @ende
@uklp1:  jmp   @uklp
@noconv: stosb
         inc   esi
         loop  @uklp1
         jmp   @ende

@isolp:  mov   al,[esi]
         inc   esi
         stosb
         loop  @isolp

@ende:   mov   edi,bytes
         add   [edi],edx
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

function  ContainsUmlaut(var s:string):boolean;
var
  i: Integer;
begin
  ContainsUmlaut := false;
  for i := 1 to Length(s) do
    if s[i] > #127 then
    begin
      Containsumlaut := true;
      break;
    end;
end;

procedure ukstring(var s:string);
const du : string[14] = 'aeoeueAeOeUess';
var p,i : byte;
begin
  case umlaute of
    1 : for i:=1 to 7 do
          repeat
            p:=pos(um[i],s);
            if p>0 then begin
              delete(s,p,1);
              insert(copy(du,i*2-1,2),s,p);
              end;
          until p=0;
   { 2 : for i:=1 to length(s) do
           s[i]:=chr(isotab[ord(s[i])]); }
  end;
end;


function umlauttest(var s:string):boolean;
var i : integer;
{    p : byte; }
begin
  umlauttest:=true;
  case umlaute of
    1 : for i:=1 to 7 do
          if pos(um[i],s)>0 then ukonvstr(s,betrefflen);

  { 2 : for i:=1 to 7 do begin
          p:=pos(um[i],s);
          if p>0 then s[p]:=iso[i];
          end; }
  end;
end;

function test_senddate(var s:string):boolean;
begin
  if smdl(ixdispdat(s),min_send) then begin
    rfehler(601);    { 'Rueckdatieren nicht moeglich.' }
    s:=fdat(longdat(min_send));
    test_senddate:=false;
    end
  else
    test_senddate:=true;
end;

procedure firstslash(var s:string);
begin
  if (s<>'') and (s[1]<>'/') then
    s:='/'+s;
end;

function testreplyto(var s:string):boolean;
var p : byte;
    d : DB;
begin
  p:=cpos('@',s);
  if (s<>'') and ((p=0) or (cpos('.',mid(s,p))=0)) then
  begin
      dbOpen(d,PseudoFile,1);           { Wenns keine gueltige Adresse ist...}
      dbSeek(d,piKurzname,UpperCase(s));
      if dbFound then
      begin
        s:= dbReadStr(d,'Langname');
        dbclose(d);                     { ists ein Kurzname ? }
        testreplyto:=true;
        if cpos(' ',s)<>0 then           { jetzt der Langname jetzt gueltig ? }
          begin
            rfehler(908);               { 'ungueltige Adresse' }
            testreplyto:=false;
            end;
        end
      else begin
        rfehler(908);                   { 'ungueltige Adresse' }
        dbclose(d);
        testreplyto:=false;
        end;
      end
  else
    testreplyto:=true;
end;


function pgpo_sigtest(var s:string):boolean;
begin
  if (s=_jn_[1]) and (getfield(3)=_jn_[1]) then
    setfield(3,_jn_[2]);
  pgpo_sigtest:=true;
end;

function pgpo_keytest(var s:string):boolean;
begin
  if (s=_jn_[1]) and (getfield(1)=_jn_[1]) then
    setfield(1,_jn_[2]);
  pgpo_keytest:=true;
end;

{$IFDEF Snapshot}
function compiletime:string;      { Erstelldatum von XP.EXE als String uebergeben }
begin
  CompileTime := FormatDateTime('ddmmyyhhnn', FileDateToDateTime(FileAge(ParamStr(0))));
end;
{$ENDIF}

{ --- Datei verschicken ---------------------------------------------------- }
{ Datei:  Pfadname der Datei. Wenn nicht vorhanden, wird eine leere angelegt }
{ is_temp: Datei lˆschen                                                     }
{ is_file: Es ist ein Datei-Attachment                                       }
{ empfaenger: der Empfaenger (User oder x/Brett)                             }
{ Edit :   Nachricht zunaechst Editieren und dann erst senden                }
{ Binary:  Binaerdatei                                                       }
{ sendwin: vor dem Senden Sende-Fenster abfragen                             }
{ datei, header und signat sind nur aus Stack-Platz-Gruenden VARs!           }
{ header wird veraendert!!                                                   }

function DoSend(pm:boolean; datei:string; is_temp,is_file:boolean;
                empfaenger,betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData:SendUUptr;
                const signat:string; sendFlags:word):boolean;

var f,f2     : file;
    edis     : byte;
    x,y      : Integer;
    brk      : boolean;
    typ      : string;  { Kopf fuer Betreff/Sende-Box          }
    wbox     : string;
    ch       : string;  { '*'-Zeichen fuer abweichende Adresse }
    box      : string;  { Empfaenger-Pollbox                   }
    adresse  : string;
    newbox   : string;  { Zwischensp. fuer geaenderte Pollbox  }
    boxfile  : string;
    username : string;  { eigener Username                    }
    pointname: string;
    _brett   : string;
    mapsname : string;
    senddate : string;  { mit 'D' zeitversetzt absenden       }
    shortmid : string;
    realname : string;
    domain   : string;
    fqdn     : string;  { 16.01.00: HS}
    fidoname : string;  { Origin-Systemname                   }
    OrigBox  : string;    { Box aus Pfad  }
    AltAdr   : string;  { Gruppen / Fido-Absender }
    sendbutt : string;
    kopkey   : string;   { (K)opien }
    fidokey  : string;   { (A)n     }
    pgpkey   : string;
    oldbetr  : string;
    d        : DB;
    fs,l     : longint;
    t        : taste;
    n,p      : shortint;
    fn,fn2,
    fn3      : string;
    b        : byte;
    si0      : integer;
    hdp      : THeader;

    size     : integer;
    empfneu  : boolean;
    cancode  : shortint;    { -1=Rot13, 0=kein PW, 1=QPC, 2=DES, 9=PGP }
    docode   : shortint;    { gewaehlte Codierung                 }
    pmc_code : boolean;
    senden   : shortint;    { 0=Nein, 1=Ja, 2=Intern              }
    halten   : integer16;   { Haltezeit fuer neuen User           }
    sendedat : longint;     { Empfangsdatum                       }
    passwd   : string;      { Passwort des empfangenden Users     }
    passpos  : smallword;   { PW-Position fuer QPC                }
    newbin   : boolean;     { Typ nach Codierung                  }
    intern,                 { interne Nachricht                   }
    lokalPM  : boolean;     { lokale PM                           }
    maxsize  : longint;     { ab hier muss gesplittet werden      }
    grnr     : longint;     { Brettgruppen-Nr.                    }
    addsize  : longint;     { Header + Signatur                   }
{    hdsize   : word; }
    oversize : longint;     { Nachrichtenlimit ueberschritten     }
    parken   : boolean;     { Nachricht nach />>Unversandt        }
    SendDefault : shortint;
    verteiler: boolean;
    _verteiler: boolean;    { bleibt true bei allen Einzelnachrichten }
    netztyp  : byte;        { Netztyp                             }
    aliaspt  : boolean;     { Alias-Point (USER@BOX)              }
    nomids   : boolean;     { keine Message-ID's erzeugen         }
    nobox    : boolean;     { Absender-Name im PP ohne Boxname    }
    echomail : boolean;
    fadd     : shortint;
    oldnt    : byte;        { alter Netztyp bei Pollbox-Wechsel   }
    fidoam   : boolean;
    old_cca  : integer;     { vor (K)opien            }
    FidoBin  : boolean;     { File Attach }
    cc_count : integer;
    betrlen  : byte;        { max. Betrefflaenge }
    bboxwid  : byte;

    spezial  : boolean;
    flOhnesig: boolean;
    flLoesch : boolean;
    sdnope   : boolean;     { sData = nil }
    orgftime : longint;
    sigfile  : string;
    sigtemp  : boolean;
    flPGPkey : boolean;     { eigenen Key mitschicken }
    flPGPsig : boolean;     { Nachricht signieren }
    flPGPreq : boolean;     { Key-Request }

    msgCPanz : shortint;    { EMP's in aktueller Msg. }
    msgCPpos : shortint;    { gerade bearbeiteter EMP }
    ii       : integer;
    m1adr    : longint;     { Pufferadresse der ersten Kopie }
    m1msgsize: longint;     { Gesamtgroesse der ersten Kopie }
    showempfs: shortint;    { fuer Betreffbox }
    fo       : string;
    flags    : longint;

    parts    : TList;       { Nachrichten-Teile }
    partsex  : Boolean;	    { bereits extrahiert? }
    pa       : TSendAttach_Part;

    s        : String;
    s1,s2,s3,s4,s5 : TStream;
    Boundary : String;
    i        : Integer;

  label xexit,xexit1,xexit2,fromstart,ReadAgain;

{$I xpsendmessage_subs.inc}
{$I xpsendmessage_subs_mime.inc}

  function uucpbrett(s:string; edis:byte):string;
  var i : integer;
  begin
    if (edis=1) or (not netztyp in netsRFC) or not NewsgroupDisp then
      uucpbrett:=mid(s,edis)
    else begin
      delete(s,1,2);
      for i:=1 to length(s) do if s[i]='/' then s[i]:='.';
      uucpbrett:=s;
      end;
  end;

  procedure AddMessagePart(datei:string;temp,is_orig:boolean);
  var pa     : TSendAttach_Part;
  begin
    pa := TSendAttach_Part.Create;

    pa.FileName    := datei;
    pa.IsTemp	   := temp;
    pa.IsFile      := false;

    if is_orig and assigned(sData^.OrgHdp) then
    begin
      pa.FileCharset := sData^.OrgHdp.Charset;
      pa.FileEOL     := MimeEolCRLF;
      pa.ContentDisposition.AsString := iifs(sData^.OrgHdp.Mime.Disposition<>'',sData^.OrgHdp.Mime.Disposition,'inline');
      pa.ContentDescription := sData^.OrgHdp.Mime.Description;
      pa.ContentType.AsString := iifs(sData^.OrgHdp.Mime.CType<>'',sData^.OrgHdp.Mime.CType,'text/plain');
      pa.ContentEncoding := sData^.OrgHdp.Mime.Encoding;
    end else
    begin
      pa.FileCharset := 'IBM437';
      pa.FileEOL     := MimeEolCRLF;
      pa.ContentDisposition.DispoType := MimeDispositionInline;
      pa.ContentEncoding := MimeEncoding7Bit;
      pa.ContentType.AsString := 'text/plain';
    end;

    SendAttach_Analyze(pa,not is_orig,iifs(flOhneSig,'',sigfile),netztyp,docode,flPGPSig);

    parts.Insert(0,pa);
  end;

  procedure AddFilePart(datei:string;temp:boolean);
  var pa     : TSendAttach_Part;
  begin
    pa := TSendAttach_Part.Create;

    pa.FileName    := datei;
    pa.IsTemp	   := temp;
    pa.IsFile      := true;

    SendAttach_Analyze(pa,true,'',netztyp,docode,flPGPSig);

    parts.Insert(0,pa);
  end;

  procedure EditNachricht(pushpgdn:boolean);
  var p      : byte;
      edpush : boolean;
  begin
    MIMEDecompose;

    edpush:=not editvollbild and ((exteditor=1) or (VarEditor='') or (VarEditor[1]='*'));
    if edpush then begin
      attrtxt(col.coledithead);
      moff;
      // Wegen der Fensterbehandlung wpush auf den gesamten Bereich anwenden
      wpush(1,ScreenWidth,1,ScreenLines,'-');          { 'Nachricht an  ' / 'Nachricht in  ' }
      p:=cpos('@',empfaenger);
      wrt(1,1,' ');
      if verteiler then Wrt2(forms(getres2(611,40)+vert_name(empfaenger),79+screenwidth-80))
      else
        if pm then Wrt2(forms(getres2(611,40)+LeftStr(empfaenger,p-1)+'@'+
                       mid(empfaenger,p+1),70+screenwidth-80)+sp(9))
        else Wrt2(forms(getres2(611,41)+copy(empfaenger,edis,55)+
                 iifs(ntBrettEmpf(netztyp) and (fidoto<>''),
                      getres2(611,43)+fidoto,''),70+screenwidth-80)+sp(9));
      wrt(1,2,' '+forms(getres2(611,42)+betreff,79 + screenwidth-80));   { 'Betreff:      ' }
      mon;
      end;
    if pushpgdn then pushkey(keycpgd);
    if exteditor<3 then EditSetBetreff(betreff,betrlen);

    if (parts.count<=0) or not TSendAttach_Part(parts[0]).IsMessage then
      addMessagePart(TempS($FFFF),true,false);

    SendAttach_EditText(TSendAttach_Part(parts[0]),true,umlaute=1,iifs(flOhneSig,'',SigFile),netztyp,docode,flPGPSig);

    if exteditor<3 then betreff:=EditGetbetreff;
    if edpush then begin
      moff; wpop; mon;
      end;
    if pushpgdn and keypressed then begin
      get(t,curoff);
      if t<>keycpgd then _keyboard(t);
     end;
    otherquotechars:=otherqcback; {evtl. mit 'Q' im Lister umgeschaltene Quotechars reseten }
  end;

  procedure TestXpostings(all:boolean);  { Crossposting-Informationen zusammenstellen }
  var i,first : integer;

    procedure GetInf(n:integer; var adr:string);
    var
      p : Integer;
      size: Integer;
      temp :string[90];
    begin
      with ccm^[n] do begin
        ccpm:=(cpos('@',adr)>0);
        if ccpm then begin      // persînlich mail
          dbSeek(ubase,uiName,UpperCase(adr));
          if dbFound then
          begin

            if dbreadint(ubase,'adrbuch')=0 then      { CCs-Empfaenger ins Adressbuch aufnehmen }
              dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
            Server := dbReadNStr(ubase,ub_pollbox);
            if (dbReadInt(ubase,'userflags') and 2<>0) and
               (dbReadInt(ubase,'codierer')<>0) then
              encode:=true;

          size := 0;
          if dbXsize (ubase, 'adresse') <> 0 then
          begin
            dbReadX (ubase, 'adresse', size, temp);
            dbSeek (ubase, uiName, UpperCase(temp));
            if dbFound then
            begin
{              if dbreadint(ubase,'adrbuch')=0 then      { CC-Empfaenger ins Adressbuch aufnehmen }
{                dbwriteN(ubase,ub_adrbuch,NeuUserGruppe);}
              Server := dbReadNStr(ubase,ub_pollbox);
              if (dbReadInt(ubase,'userflags') and 2<>0) and
                 (dbReadInt(ubase,'codierer')<>0) then
                encode:=true;
            end;
          end else
          begin
{              if dbreadint(ubase,'adrbuch')=0 then      { CC-Empfaenger ins Adressbuch aufnehmen }
{                dbwriteN(ubase,ub_adrbuch,NeuUserGruppe);}
              Server := dbReadNStr(ubase,ub_pollbox);
            if (dbReadInt(ubase,'userflags') and 2<>0) and
               (dbReadInt(ubase,'codierer')<>0) then
              encode:=true;
          end;
        end
        else begin      // if ccpm then begin
          p:=cpos(':',adr);
          if (adr[1]='+') and (p>0) then begin    { nicht eingetragenes Brett }
            server:=copy(adr,2,p-2);
            nobrett:=true;
            end
          else begin
            if adr[1]='/' then dbSeek(bbase,biBrett,'A'+UpperCase(adr))
            else dbSeek(bbase,biBrett,UpperCase(adr));
            if dbFound then Server := dbReadNStr(bbase,bb_pollbox)
            else if CrosspostBox<>'' then begin
              adr:='+'+CrosspostBox+':'+adr;
              server:=UpperCase(CrosspostBox);
              nobrett:=true;
              end;
            end;
          end;
        server:= UpperCase(server);
        end;
      end;
    end;        // procedure GetInf(n:integer; var adr:string);

  { alle Kopien mit gleichem Server wie 'empfaenger' nach oben }
  { wandern lassen                                             }

  procedure CollectFirstServer;
  var s1    : string[BoxNameLen];
      s     : AdrStr;
      cmr   : ccmore;
      p1,p2 : integer;
  begin
    s1:=ccm^[0].server;
    p1:=1;
    while (p1<=cc_anz) and (ccm^[p1].server=s1) do inc(p1);    //Anzah der mail fÅr Server 1
    p2:=p1;
    while p1<=cc_anz do begin
      while (p1<=cc_anz) and (ccm^[p1].server<>s1) do inc(p1);
      if p1<=cc_anz then begin
        cmr:=ccm^[p1];                  // rette alten Wert
        Move(ccm^[p2],ccm^[p2+1],(p1-p2)*sizeof(cmr));
        ccm^[p2]:=cmr;
        s:=cc^[p1];
        Move(cc^[p2],cc^[p2+1],(p1-p2)*sizeof(cc^[1]));
        cc^[p2]:=s;
        inc(p1);inc(p2);
        end;
      end;
    first:=p2;
  end;

  { ccm^/cc^ nach Server/CCPM sortieren (PMs vor AMs) }

  procedure SortForServer_PM;
  var i   : integer;
      xch : boolean;
      cmr : ccmore;
      s   : AdrStr;
    function IndexStr(i:integer):string;
    begin
      with ccm^[i] do
        { !! Char(Byte(x)) ist eine grosse Schweinerei, evtl. mal aendern }
        IndexStr:=char(byte(encode))+forms(server,BoxNameLen)+char(byte(ccpm));
    end;
  begin
    repeat
      xch:=false;
      for i:=cc_anz downto first+1 do
        if IndexStr(i)<IndexStr(i-1) then begin
          cmr:=ccm^[i];
          s:=cc^[i]; cc^[i]:=cc^[i-1]; cc^[i-1]:=s;
          xch:=true;
          end;
      inc(first);
    until not xch;
  end;

  procedure ReadServerNTs;              // Netztyp des Servers ermitteln
  var d  : DB;
      i  : integer;
      nt : byte;
      s  : string[BoxNameLen];
  begin
    dbOpen(d,BoxenFile,1);
    i:=iif(verteiler,1,0);
    while i<=cc_anz do begin
      s:=ccm^[i].server;
      dbSeek(d,boiName,s);
      if dbFound then dbRead(d,'netztyp',nt)
      else nt:=0;
      while (i<=cc_anz) and (ccm^[i].server=s) do begin
        ccm^[i].ccnt:=nt;
        inc(i);
        end;
      end;
    dbClose(d);
  end;

  procedure FindXposts;
  var i,j,k   : integer;
      errflag : boolean;
  begin
    errflag:=false;
    i:=iif(verteiler,1,0);
    while i<=cc_anz do begin
      while (i<=cc_anz) and
            ((ccm^[i].ccpm and not ntCrossPM(ccm^[i].ccnt)) or
             (not ccm^[i].ccpm and not ntCrossAM(ccm^[i].ccnt)) or
             ccm^[i].encode) do
        inc(i);
      if i<=cc_anz then begin
        j:=i;
        while (j<=cc_anz) and (ccm^[j].{ccnt}server=ccm^[i].{ccnt}server) and
              (ccm^[j].ccpm=ccm^[i].ccpm) and not ccm^[j].encode do
          inc(j);
        if j-i>1 then begin
          ccm^[i].cpanz:=min(j-i,iif(ccm^[i].ccpm,maxcc,MaxXposts));
          if not ccm^[i].ccpm and (j-i>MaxXposts) then begin
            if not errflag then
              rfehler1(632,strs(MaxXposts));   { 'Es sind maximal %s Brettempfaenger pro Server moeglich.' }
            errflag:=true;
            if j<=cc_anz then begin
              Move(cc^[j],cc^[i+MaxXposts],(cc_anz-j)*sizeof(cc^[1]));
              Move(ccm^[j],ccm^[i+MaxXposts],(cc_anz-j)*sizeof(ccm^[1]));
              end;
            dec(cc_anz,j-i-MaxXposts);
            for k:=cc_anz+1 to maxcc do cc^[k]:='';
            j:=i+MaxXposts;
            end;
          end;
        i:=j;
        end;
      end;
   end;
  begin
    if all then begin
    if cc_anz>10 then rmessage(620);    { 'Teste auf Crosspostings ...' }
    fillchar(ccm^,sizeof(ccm^),0);
    if not verteiler then GetInf(0,empfaenger);   { 1. Server einlesen, }
    for i:=1 to cc_anz do                         {    PM-Flags setzen  }
      GetInf(i,cc^[i]);
    first:=1;
    if not verteiler then CollectFirstServer;     { 2. nach Server sortieren }
    SortForServer_PM;
    ReadServerNTs;                                { 3. Netztypen einlesen }
    FindXposts;                                   { 4. Crosspostflags setzen }
    if cc_anz>10 then closebox;
    end
  else begin                       { Nach Pollbox-Wechsel }
    for i:=0 to cc_anz do
      ccm^[i].cpanz:=0;
    SortForServer_PM;
    FindXposts;
    end;
  end;


  procedure ReadEmpflist;
  var
    i: Integer;
  begin
    // !! Assign moeglich, wenn beides StringListe
    for i := 0 to SendEmpfList.Count - 1 do
      if cc_anz<maxcc then
      begin
        inc(cc_anz);
        cc^[cc_anz]:=Sendempflist[i];
      end;
    SortCCs(cc,cc_anz);
    TestXpostings(true);
  end;


  procedure RemovePhantomServers;   { .. werden jetzt nicht mehr gebraucht }
  var i : integer;
  begin
    for i:=1 to cc_anz do
      if ccm^[i].nobrett then
        delete(cc^[i],1,cpos(':',cc^[i]));
  end;

  function ohnebox(i:integer):string;
  begin
    if ccm^[i].nobrett then
      ohnebox:='A'+mid(cc^[i],cpos(':',cc^[i])+1)
    else
      ohnebox:='A'+cc^[i];
  end;


  function IncompatibleNTs:boolean;
  var i  : integer;
      nt : byte;
  begin
    IncompatibleNTs:=false;
    nt:=ccm^[iif(verteiler,1,0)].ccnt;
    for i:=1 to cc_anz do
      if not ntAdrCompatible(nt,ccm^[i].ccnt) then
        IncompatibleNTs:=true;
  end;


  procedure KorrPhantomServers(var oldbox,newbox:string; newnt:byte);
  var i    : integer;
      modi : boolean;
  begin
    modi:=false;
    for i:=0 to cc_anz do
      if ccm^[i].nobrett and (ccm^[i].server=UpperCase(oldbox)) then begin
        ccm^[i].server:=UpperCase(newbox);
        cc^[i]:='+'+newbox+mid(cc^[i],cpos(':',cc^[i]));
        modi:=true;
        end
      else if ccm^[i].ccnt=newnt then begin
        ccm^[i].server:=UpperCase(newbox);
        modi:=true;
        end;
    if modi then TestXpostings(false);
  end;

  Procedure changeempf;                         {Empfaenger der Mail aendern}
  var kb_s: boolean;
  begin
    kb_s:=kb_shift;
    pm:=cpos('@',empfaenger)>0;
    if pm then adresse:=empfaenger
      else adresse:=uucpbrett(empfaenger,2);
    if pm and (adresse[1]=vert_char)
      then adresse:=copy(adresse,2,length(adresse)-3);
    attrtxt(col.coldiarahmen);
    mwrt(x+70,y+12,' [F2] ');
    if not pm and (Netztyp=nt_fido) then y:=y-2;   {Zeile fuer Fidoempf beachten}
    openmask(x+13,x+13+51+2,y+2,y+2,false);
    maskrahmen(0,0,0,0,0);
    maddstring(1,1,'',adresse,52,adrlen,'');
    mappcustomsel(scr_auto_empfsel,false);
    testmailstring_nt:=255;   { Adressschreibweisen aller Netztypen erlauben }
    msetvfunc(testmailstring);
    sel_verteiler:=true;
    readmask(brk);
    closemask;
    attrtxt(col.coldiahigh);
    mwrt(x+13,y+2,' '+forms(adresse,53)+'   ');
    if (adresse<>'') and (cc_testempf(adresse)) then 
    begin
      if (FirstChar(adresse)='[') and (LastChar(adresse)=']')
        then adresse:=vert_char+adresse+'@V'                 { Verteiler: Namen anpassen }
      else if not kb_s then
      begin
        cc_anz:=0;                                         { Kein Verteiler: CCs loeschen }
        fillchar(cc^,sizeof(cc^),0);
        end;
      if cpos('@',adresse)=0 then adresse:='A'+adresse;
      empfaenger:=adresse;
      end;
    pm:=cpos('@',empfaenger)>0;
    sel_verteiler:=false;
    end;

{ ausgelagert, weil Prozedurrumpf zu gross: }

  procedure DisplaySendbox;
  var
    ToStr: String;
    ToPos: Integer;
  begin
    diabox(78,13+fadd,typ,x,y);
    moff;
    wrt(x+3,y+2,getres2(611,10)+ch);   { 'Empfaenger ' }

    ToStr := getres2(611,11); { '^An' }
    ToPos := cpos('^', ToStr);
    fidokey := copy(ToStr, ToPos+1, 1); { (A)n }
    Delete(ToStr, ToPos, 1);

    if echomail then begin
      wrt(x+3,y+4, ToStr);    { 'An' }
      inc(y,2);
      end;
    wrt(x+3,y+4,getres2(611,12));      { 'Betreff' }
    wrt(x+3,y+6,getres2(611,13));      { 'Server'  }
    wrt(x+3,y+8,getres2(611,14));      { 'Groesse' }
    wrt(x+42,y+6,getres2(611,15));     { 'Code:'   }
    showcode; 
    attrtxt(col.coldialog);
    wrt(x+43,y+8,mid(getres2(611,16),2));    { 'opien:' }
    showcc; 
    attrtxt(col.coldiahigh);
    kopkey:=FirstChar(getres2(611,16));
    wrt(x+42,y+8,kopkey); 
    if empfaenger[1]=vert_char then
      wrt(x+14,y+2-fadd,vert_name(copy(empfaenger,edis,52)))
    else
      wrt(x+14,y+2-fadd,LeftStr(uucpbrett(empfaenger,edis),52));

    pgpkey:=getres2(611,50);
    if pgpkey='^' then pgpkey:=chr(ord(lastchar(getres2(611,50)))-64);

    if echomail then
    begin
      wrt(x+2+ToPos,y+2,fidokey);            { 'A' }
      wrt(x+14,y+2,fidoto);
     end;
    showbetreff;
    showbox;
    showsize;
    mon;
  end;


  procedure WriteHeaderHdr;
  var f:text;
  begin
    assign(f,temppath+FileUpperCase('header.hdr'));
    rewrite(f);
    writeln(f,'TYP: ',typ);
    writeln(f,'BOX: ',box);
    if cpos('@',empfaenger)=0 then
      writeln(f,'EMPF: ',copy(empfaenger,2,99))
    else
      writeln(f,'EMPF: ',copy(empfaenger,1,99));
    writeln(f,'FIDOTO: ',fidoto);
    writeln(f,'BETREFF: ',betreff);
    close(f);
  end;

begin      //-------- of DoSend ---------
  DoSend:=false;
  parken:=false;
  _verteiler:=false;
  flOhnesig:=false; flLoesch:=false;
  assign(f,datei);
  parts := TList.Create;
  partsex := false;
  s1:=nil;{s2:=nil;}s3:=nil;s4:=nil;s5:=nil;

  sdNope:=(sdata=nil);
  if sdNope then sdata:=allocsenduudatamem;

  if sendFlags and sendQuote<>0 then
  begin
    ExtractSetMpdata(qmpdata);
    extract_msg(3,iifs(force_quotemsk<>'',force_quotemsk,QuoteSchab(pm)),
                datei,false,1);
    sdata^.quotestr:=qchar;
    get_xref;
    partsex:=true;
    AddMessagePart(datei,true,true);
  end else
  if sendFlags and sendMPart<>0 then
  begin
    PartsEx := not FileExists(datei);
      // don't extract multipart parts if file does not exist anyway...
  end else
  begin
    if FileExists(datei) then
    begin
      if not is_file then
        AddMessagePart(datei,is_temp,true)
      else
        if netztyp=nt_Fido then AddFilePart(datei,is_temp);
    end;
    partsex:=true;
    OrigBox:='';
  end;

  if not pm and betreffbox and (FirstChar(empfaenger)<>'A') then
  begin
    rfehler(606);   { 'Schreiben in dieses Brett nicht moeglich!' }
    SendEmpfList.Clear; { clear list of CC recipients }
    goto xexit1;
  end;

  hdp := THeader.Create;

  MakeSignature(signat,sigfile,sigtemp);

  cc_anz:=0; cc_count:=0;
  new(cc);new(ccm);                             //mo bookmark
  fillchar(cc^,sizeof(cc^),0);
  SendDefault:=1;
  verteiler:=false;
  if SendEmpflist<>nil then ReadEmpflist;
  flPGPkey:=(sendflags and SendPGPkey<>0);
  flPGPsig:=(sendflags and SendPGPsig<>0) or PGP_signall;
  flPGPreq:=(sendflags and SendPGPreq<>0);
  flNokop:=(sendflags and SendNokop<>0) or DefaultNokop;
  fo:='';

{ Einsprung hier startet ganze Versand-Prozedur von vorne (mit den bestehenden Daten) }
fromstart:

  passwd:='';          { Betreffbox true = Betreff nochmal eintippen           }
  empfneu:=false;      { Edit       true = Editor Starten                      }
  docode:=0;           { Sendbox    true = Sendefenster zeigen                 }
  fidoname:='';        { forcebox ''-um Box entsprechend Empfaenger zu waehlen }
  ch:=' ';             {          Ansonsten steht hier die zu benutzende Box   }

  { -- Empfaenger -- }

  if pm then begin
    fidoto:='';
    dbSeek(ubase,uiName,UpperCase(empfaenger));
    if dbFound then begin                                 {Empfaenger Bekannt}
      verteiler:=(dbReadInt(ubase,'userflags') and 4<>0);
      if verteiler then _verteiler:=true;
      Box := dbReadStrN(ubase,ub_pollbox);
      if verteiler then begin  { Verteiler }
        cancode:=0;
        read_verteiler(vert_name(empfaenger),cc,cc_anz);
        TestXpostings(true);
        if box='' then box:=ccm^[1].server
        else forcebox:=box;
        ch:='';
        end
      else begin
        if dbReadInt(ubase,'userflags') and 16<>0 then
          flEB:=true;
        size:=0;
        if (dbXsize(ubase,'adresse')=0) or sdata.RTAHasSetVertreter then adresse:=''
        else adresse:= dbReadXStr(ubase,'adresse',size);
        _brett:=mbrettd('U',ubase);
        if adresse<>'' then begin
          umlaute:=iif(dbReadInt(ubase,'userflags') and 8=0,0,1);
          empfaenger:=adresse;
          ch:='*';
          dbSeek(ubase,uiName,UpperCase(empfaenger));
          end;
        if dbFound then begin
          Box := dbReadStrN(ubase,ub_pollbox);   { leider doppelt noetig :-/ }
          _brett:=mbrettd('U',ubase);
          dbReadN(ubase,ub_codierer,cancode);
          if (not cancode in [8,9]) and (dbXsize(ubase,'passwort')=0) then
            cancode:=0
          else begin
            if cancode<>0 then
              if dbReadInt(ubase,'userflags') and 2<>0 then
                docode:=cancode;
            si0:=0;
            passwd:= dbReadXStr(ubase,'passwort',si0);
            end;
          umlaute:=iif(dbReadInt(ubase,'userflags') and 8=0,0,1);
          end
        else begin
          cancode:=0;
          empfneu:=true;
          end;
        end;
      end
    else begin                                                 { Empfaenger unbekannt }
    if newuseribm then umlaute:=0 { NewUserIBM beruecksichtigen }
     else umlaute:=1;
      empfneu:=true;
      verteiler:=false;
      if fileserver(empfaenger) or _sendmaps then begin
        box:=mid(empfaenger,cpos('@',empfaenger)+1);
        if cpos('.',box)>0 then box:=LeftStr(box,cpos('.',box)-1);
        if not isbox(box) then box:=DefaultBox;
        end
      else
        if forcebox='' then begin         { keine EB .. }
          ch:=' ';             { Antwort auf Brettnachricht an User, der  }
          cancode:=0;          { noch nicht in der ubase steht            }
          if dbBOF(mbase) or dbEOF(mbase) then
            box:=DefaultBox         { /Nachricht/Direkt }
          else begin
            _brett := dbReadStrN(mbase,mb_brett);
            if _brett[1]='1' then begin    { PM-Reply an nicht eingetr. User }
              if origbox='' then get_origbox;
              if (OrigBox='') or not IsBox(OrigBox) then
              begin
                box := getBrettUserPollBox (_brett);
                if box = '' then
                  box:=DefaultBox
              end
              else
                box:=OrigBox;
              end
            else
              if _brett[1]='U' then
                box:=DefaultBox
              else begin
                dbSeek(bbase,biIntnr,copy(_brett,2,4));
                if dbBOF(bbase) or dbEOF(bbase) then box:=''
                else Box := dbReadStrN(bbase,bb_pollbox);
                if box='' then box:=DefaultBox;  { duerfte nicht vorkommen }
                end;
            ReplaceVertreterbox(box,true);
            end;
          end;
      end;
    if forcebox<>'' then box:=forcebox;
    edis:=1;
    intern:=false;
    fidoname:='';
    AltAdr:='';
    end

  else begin   { not pm }
    ch:='';
    verteiler:=false;
    dbSeek(bbase,biBrett,UpperCase(empfaenger));
    if not dbFound then begin
      empfneu:=true;
      if empfaenger[1]='$' then box:=InternBox  { autom. Nachricht an neues Brett }
      else box:=iifs(forcebox<>'',forcebox,DefaultBox);
      grnr:=iif(newbrettgr<>0,newbrettgr,IntGruppe);
      end
    else begin
      Box := dbReadStrN(bbase,bb_pollbox);    { Nachricht an vorhandenes Brett  }
      if (box='') and (empfaenger[1]='$') then
        box:=InternBox;               { /Netzanruf, /Statistik ... }
      dbReadN(bbase,bb_gruppe,grnr);
      _brett:=mbrettd(empfaenger[1],bbase);
      if dbReadInt(bbase,'flags') and 32<>0 then
        FidoName := dbReadNStr(bbase,bb_adresse);    { Brett-Origin }
    end;

    dbOpen(d,gruppenfile,1);          { max. BrettMsg-Groesse ermitteln   }
    dbSeek(d,giIntnr,dbLongStr(grnr));
    if not dbFound then maxsize:=0    { duerfte nicht vorkommen }
    else dbRead(d,'MsgLimit',maxsize);
    if box='' then grnr:=IntGruppe;
    intern:=(grnr=IntGruppe) or (box='');
    if box='' then box:=DefaultBox;
    if forcebox<>'' then box:=forcebox;
    if binary or not dbFound then umlaute:=0
    else dbRead(d,'umlaute',umlaute);
    if (fidoname='') and dbFound then
      FidoName := dbReadStr(d,'Origin');
    if dbFound then
      altadr:= dbReadStr(d,'Adresse')
    else
      altadr:='';
    dbClose(d);

    edis:=2;
    if not binary then cancode:=-1;  { Rot13 moeglich }
  end;   { of not pm }

  { -- Boxdaten -- }

  dbOpen(d,BoxenFile,1);           { Pollbox + MAPS-Name ueberpruefen }
  if box<>'' then begin            { nicht intern.. }
    dbSeek(d,boiName,UpperCase(box));
    if not dbFound then begin
      dbClose(d);
      rfehler1(607,box);  { 'Unbekannte Serverbox: %s  -  Bitte ueberpruefen!' }
      goto xexit;                  { --> unbekannte Pollbox }
    end;
    Box := dbReadStr(d,'boxname');       { Schreibweise korrigieren }
  end else                         { interne Msgs -> Default-Username }
    dbSeek(d,boiName,UpperCase(DefaultBox));
  LoadBoxData;

  if pm then
    SetLocalPM;
  dbClose(d);

  flMLoc:=(netztyp=nt_Maus) and stricmp(sData^.distribute,'lokal');
  flMnet:=(netztyp=nt_Maus) and stricmp(sData^.distribute,'mausnet');
  FidoBin:=binary and pm and
           ((netztyp=nt_Fido) or
            ((netztyp=nt_UUCP) and (LeftStr(empfaenger,length(uuserver))=uuserver)));
  if FidoBin then begin
    if length(datei)>BetreffLen then begin
      rfehler(608);   { 'zu langer Datei-Pfad' }
      goto xexit;
    end;
    betreffbox:=false;
    binary:=false;
    FileAttach:=true;
    if pos('@'+box,empfaenger)=0 then flCrash:=true;
    edit:=EditAttach;
    if edit then begin
      betreff:=datei;
      datei:=TempS(20000);
      MakeFile(datei);
    end;
  end else if binary and not ntBinary(netztyp) then begin
    rfehler(609);   { 'In diesem Netz sind leider keine Binaernachrichten moeglich :-(' }
    goto xexit;
  end;

  if (netztyp<>nt_Fido) then
    AltAdr:='';
{
  else
    if (altadr<>'') and (cpos('.',altadr)=0) then
      AltAdr:=AltAdr+'.'+pointname;
}

  if pm and not ntEmpfBest(netztyp) then begin
    flEB:=flEB or (LeftStr(betreff,length(EmpfBkennung))=EmpfBkennung);
    SetEBkennung;
  end;
  if not fileattach then
    ukstring(betreff);
  typ:=getres2(611,iif(pm,1,iif(grnr=IntGruppe,2,3)));  { 'private Nachricht' / 'interne Nachricht' / 'oeffentliche Nachricht' }

  betreff:=LeftStr(betreff,betrlen);
  if betreffbox then begin         { Betreff editieren }
    if sendFlags and sendQuote<>0 then typ:=typ+getres2(611,4) else   { ' (Quote)' }
    if binary then typ:=typ+getres2(611,5);   { ' (Binaer)' }
    fidoam:=ntEditBrettempf(netztyp) and not pm;
    bboxwid:=min(betrlen,54);
    showempfs:=min(cc_anz,15);
    diabox(bboxwid+19,iif(fidoam,9,7)+showempfs,typ,x,y);
    mwrt(x+3,y+2,getres2(611,6)+iifs (ch='*', '*', ''));   { 'EmpfÑnger  ' }
    attrtxt(col.coldiahigh);
    moff;
    if FirstChar(empfaenger)=vert_char then
      Wrt2(copy(vert_name(empfaenger),edis,bboxwid))
    else
      Wrt2(LeftStr(uucpbrett(empfaenger,edis),bboxwid));
    for ii:=1 to min(showempfs,14) do
      if ccm^[ii].ccpm then
        wrt(x+3+length(getres2(611,6)),y+2+ii,LeftStr(cc^[ii],bboxwid))
      else
        wrt(x+3+length(getres2(611,6)),y+2+ii,LeftStr(uucpbrett(ohnebox(ii),2),bboxwid));
    if showempfs=15 then
      wrt(x+3+length(getres2(611,6)),y+17,'(...)');
    mon;
    openmask(x+3,x+bboxwid+10,y+showempfs+4,y+showempfs+iif(fidoam,6,4),false);
    oldbetr:=LeftStr(betreff,20);
    maddstring(1,1,getres2(611,7),betreff,bboxwid,betrlen,'');   { 'Betreff   ' }
    msetvfunc(umlauttest); mhnr(86);
    if fidoam then begin
      maddstring(1,3,getres2(611,8),fidoto,35,35,'');  { 'An        ' }
      mhnr(90);
    end;
    readmask(brk);
    closemask;
    closebox;
    betreff:=trim(betreff);
    if brk then goto xexit;            { --> Abbruch bei Betreffmaske }
    if betreff='' then begin
      if not pm then rfehler(635);  { 'Nachricht muss einen Betreff haben' }
      if (pm and not ReadJNesc(getres(618),false,brk)) or   { 'Nachricht ohne Betreff absenden' }
         not pm then goto xexit;
    end;
    if (_bezug<>'') and ntKomkette(netztyp) and
                    (UpperCase(LeftStr(betreff,20))<>UpperCase(oldbetr)) then begin
      pushhp(1501);
      if not
ReadJNesc(getres(617),(LeftStr(betreff,5)=LeftStr(oldbetr,5)) or   { 'Betreff geaendert - Verkettung beibehalten' }
             ((cpos('(',oldbetr)=0) and (cpos('(',betreff)>0)),brk) then
      begin
        _bezug:='';
        _orgref:='';
        sdata^.References.Clear;
      end else
        { betreff:=LeftStr(betreff+' ('+getres(619)+': '+oldbetr,betrlen-1)+')'} ;
      pophp;
      if brk then goto xexit;
    end;
    if pm and not ntEmpfBest(netztyp) then begin
      flEB:=(LeftStr(betreff,length(EmpfBkennung))=EmpfBkennung);
      SetEBkennung;
    end;
  end;

  orgftime:=filetime(datei);
  if edit then begin
    WriteHeaderHdr;
    EditNachricht(pgdown);              //Editor aufrufen
  end;
  if not getsize then goto xexit;        { --> Nachrichten-Groesse 0 }
  calc_hdsize;

  echomail:=ntEditBrettempf(netztyp) and not pm;

  if sendbox then
  repeat
    echomail:=ntEditBrettempf(netztyp) and not pm;
    fadd:=iif(echomail,2,0);
    DisplaySendbox;                         { SendBox aufbauen }
    senden:=-1;
//  n:=1;                                { SendBox-Abfrage }
    pushhp(68);
    spezial:=false;
    repeat
      if pm then intern:=false
      else intern:=(grnr=IntGruppe);
      ShowFlags;
      if spezial then begin
        spezial:=false;
        attrtxt(col.coldialog);
        mwrt(x+1,y+11,sp(76)); { 05.02.2000 MH: 67 -> 76 f. Zurueck } { unbedenklich }
      end;
    ReadAgain:
      n:=1;
      ShowLine(spezial);
      if spezial then begin
        case netztyp of    { '^Parken,^Datum, ^EB ,o^hneSig,l^oeschen,' }
          nt_Fido     : sendbutt:=getres2(611,20);  { 'C^rash,P^GP'     }
          nt_Maus     : sendbutt:=getres2(611,21);  { '^MausNet,^Lokal' }
          nt_ZConnect : sendbutt:=getres2(611,22);  { 'P^rio,P^GP'      }
        else
            if netztyp in netsRFC then
              sendbutt:=getres2(611,23)   { 'Z^usatz,P^GP'    }
            else
              sendbutt:=getres2(611,24);  { '^Zurueck'        }
        end;
        repeat
          t:='*';
          n:=readbutton(x+3,y+11,1,sendbutt,
                        abs(n),true,t);
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        case netztyp of
          nt_Fido : if n=7 then n:=11        { PGP         }
                    else if n=8 then n:=0;   { MH: Zurueck }
          nt_Maus : if n=8 then n:=0         { Zurueck     }
                    else if n>5 then inc(n);
          nt_ZConnect :  if n=6 then n:=9    { Prio        }
                    else if n=7 then n:=10   { Zusatz      }
                    else if n=8 then n:=11   { PGP         }
                    else if n=9 then n:=0;   { MH: Zurueck }
          else
            if netztyp in netsRFC then begin
              if n=6 then n:=12        { MH: RFC-Prio}
                else if n=7 then n:=10 { Zusatz      }
                else if n=8 then n:=11 { MH: PGP-Sig }
                else if n=9 then n:=0; { Zurueck     }
            end else
              if n=6 then n:=0;        { Zurueck     }
        end;
        if n=0 then n:=-1;
        if n>0 then
          inc(n,10)
        else begin
          p:=pos(upcase(t[1]),getres2(611,25));   { JNI˘BOCT }
          if p>0 then n:=p;
        end;
      end else begin
        repeat
          t:='*';
          n:=readbutton(x+3,y+11,1,getres2(611,28)+
               iifs((not CanEdit) or (sendflags and sendWAB<>0),'',getres2(611,29)),
                        abs(n),true,t);
           { ' ^Ja ,^Nein,^Intern,^Spezial,˘2^Betreff,B^ox,^Code' ',^Text' }
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        if n=4 then begin
          spezial:=true;
          attrtxt(col.coldialog);
          mwrt(x+1,y+11,sp(76)); { 05.02.2000 MH: 68 -> 76 f. Zurueck } { unbedenklich }
          goto ReadAgain;
          end;

        if { (n=5) or } (t='/') then    { Empfaenger aendern? }
        begin
          Changeempf;
          betreffbox:=false; edit:=false; sendbox:=true;
          SendDefault:=senden;
          forcebox:='';
          pophp;
          closebox;
          goto fromstart;
          end
        else
        case n of     { Ansonsten eins zurueckzaehlen fuer alte Keys }
          1..4: ;               { Ja, Nein, Intern, Spezial     }
          5..7: ;               { Betreff, Box, Code            }
          8:    n:=23;          { Anh‰nge                       }
          9:    ;               { Text                          }
        end;

        if n<0 then begin
          p:=pos(UpCase(t[1]),getres2(611,30));   { PDEHôRMLG }
          case p of
            1..5 : n:=p+10;
            6    : if netztyp=nt_Fido then n:=16 else
                   if netztyp=nt_ZConnect then n:=19 else
                   if netztyp in netsRFC then n:=22;
            7    : if netztyp=nt_Maus then n:=17;
            8    : if netztyp=nt_Maus then n:=18;
            9    : if netztyp in (netsRFC + [nt_ZConnect]) then n:=20;
            10   : if netztyp in (netsRFC + [nt_ZConnect,nt_Fido,nt_Maus]) then
                     n:=21;  { PGP }
            else   if ntBCC(netztyp) and (t=^K) then
                     flNokop:=not flNokop;
          end;
          end;
        end;
      case n of
        0   : if SaveUVS and not binary then senden:=3   { Abbruch }
              else if sdata^.uv_edit then senden:=1
              else senden:=0;
        1   : if not pm or (cc_anz>0) or not EmpfError then
                if (OverSize=0) or (msgprio>0) or
                   (_errsound and ReadJN(getreps2(612,1,strs(OverSize)),false))
                then senden:=1;
                  { 'Nachrichtenlimit um %s Bytes ueberschritten! Trotzdem absenden' }
        2   : senden:=0;   { Nein   }
        3   : senden:=2;   { Intern }
        5   : if FileAttach then
                rfehler(610)   { 'Betreff kann nicht geaendert werden' }
              else begin
                { neuer Betreff }
                readstring(x+13,y+4,'',betreff,min(betrlen,52),betrlen,'',brk);
                betreff:=trim(betreff);
                if umlauttest(betreff) then;
                showbetreff;
//              n:=1;
              end;
        6   : if intern then
                rfehler(611)   { 'nicht moeglich - interne Nachricht' }
              else if IncompatibleNTs then
                rfehler(629)   { 'nicht moeglich - unterschiedliche Netztypen' }
              else begin                        { neue Pollbox }
                newbox:=UniSel(1,false,box);
                if newbox<>'' then
                  if not pm and (cc_anz=0) and ntBrettebene(netztyp) and
                     ntBrettebene(ntBoxNetztyp(newbox)) and
                     not stricmp(BoxBrettebene(box),BoxBrettebene(newbox)) then
                    rfehler(637)   { 'Serveraenderung nicht moeglich - abweichende Brettebene!' }
                  else begin
                    dbOpen(d,BoxenFile,1);
                    dbSeek(d,boiName,UpperCase(newbox));
                    if binary and not ntBinary(dbReadInt(d,'netztyp')) then
                      rfehler(609)  { 'In diesem Netz sind leider keine Binaernachrichten moeglich :-(' }
                     else if (((not pm) and (netztyp<>dbReadInt(d,'netztyp'))) or
                     not ntAdrCompatible(netztyp,dbReadInt(d,'netztyp'))) then
                     rfehler(629)   { 'nicht mîglich - unterschiedliche Netztypen' }
                 else begin
                      KorrPhantomServers(box,newbox,dbReadInt(d,'netztyp'));
                      box:=newbox;
                      oldnt:=netztyp;
                      sData^.replyto := '';
                      LoadBoxData;
                      if (netztyp=nt_Fido)<>(oldnt=nt_Fido) then
                        senden:=5;
                      if pm then SetLocalPM;
                      showsize;
                      if cc_anz>0 then forcebox:=box;
                      showbox;
                      if netztyp<>nt_Fido then
                        flCrash:=false;
                      end;
                  dbClose(d);
                  end;
//              n:=1;
              end;
        7   : if cancode<>0 then
              begin                                { Codierung aendern }
                if docode<>0 then
                  docode:=0
                else
                if parts.Count=1 then
                  docode:=cancode
                else if (cancode in [8,9]) and ntMIME(netztyp)
                then
                  docode:=8;                       // use PGP/MIME instead of PGP for multiparts
                showcode;
//              n:=1;
              end;
        9   : if not binary and (sendflags and sendWAB=0) then begin
                editnachricht(false);              { zurueck zum Editor }
                if not getsize then begin
                  closebox; goto xexit; end;    { -> Nachrichtengroesse 0 }
                showbetreff;
                showsize;
//              n:=1;
              end;
       11   : if binary then rfehler(612)   { 'Bei Binaernachrichten nicht moeglich.' }
              else
                {if cc_anz>0 then rfehler(613) }  { 'Bei mehreren Kopien nicht moeglich.' }
                {else} senden:=3;   { Parken }
       12   : if cc_anz>0 then    { Datum }
                rfehler(613)
              else begin
                if DateSend then senden:=4;     { zeitversetzt absenden }
//              n:=1;
                end;
       13   : if not pm then
                rfehler(614)   { 'Empfangsbestaetigung nur bei PMs moeglich' }
              else begin
                flEB:=not flEB;
                if not ntEmpfbest(netztyp) then
                  SetEBkennung;
                showbetreff;
                end;
       14   : begin
                flOhnesig:=not flOhnesig;

                // If the user explicitly says that s/he wants a signature
                // then create a part that can hold it.

                if (not flOhneSig) and ( (Parts.Count<1) or
                    not TSendAttach_Part(Parts[0]).IsMessage ) then
                  AddMessagePart(TempS($FFFF),true,false);

                calc_hdsize;
                showsize;
              end;
       15   : flLoesch:=not flLoesch;
       16   : if pm then
                if flCrash or (not flCrash and FidoAdrOK(true)) then
                  flCrash:=not flCrash
                else
              else
                rfehler(615);   { 'nur bei PMs moeglich' }
       17   : begin
                flMnet:=not flMnet;
                flMloc:=false;
                sData^.distribute:=iifs(flMnet,'MausNet','');
                calc_hdsize; showsize;
              end;
       18   : begin
                flMloc:=not flMloc;
                flMnet:=false;
                sData^.distribute:=iifs(flMloc,'lokal','');
                calc_hdsize; showsize;
              end;
       19   : if pm then begin
                inc(msgprio,10);
                if msgprio>20 then msgprio:=0;
                showflags;
                end
              else
                rfehler(615);
       20   : EditSdata;
       21   : SendPgpOptions;
       22   : begin                    { RFC-Priority }
               if not(netztyp in netsRFC)then rfehler(622);
                getprio;
               showflags;
              end;
       23   : begin
                MIMEDecompose;
                SendAttach(Parts,Umlaute=1,iifs(flOhneSig,'',sigfile),netztyp,
                  iif(docode in [8,9],cancode,docode),flPGPSig);

                // if the user deleted the message part, switch off signatures
                if (Parts.Count<1) or not TSendAttach_Part(Parts[0]).IsMessage then
                  flOhneSig := true;

                KorrCode;
              end;

      else    if n<0 then begin
//              n:=abs(n);
                if UpperCase(t)=kopkey then begin
                  old_cca:=cc_anz;
                  sel_verteiler:=true;           { im Kopien-Dialog sind Verteiler erlaubt }
                  edit_cc(cc,cc_anz,brk);
                  sel_verteiler:=false;
                  if (old_cca=0) and (cc_anz>0) then forcebox:='';
                  if cc_anz>0 then TestXpostings(true);
                  showcc;
                  showbox;   { evtl. in Klammern }
                  end;
                if echomail and (UpperCase(t)=fidokey) then begin
                  readstring(x+13,y+2,'',fidoto,35,35,'',brk);
                  attrtxt(col.coldiahigh);
                  mwrt(x+13,y+2,' '+forms(fidoto,35)+' ');
                  end;
                end;
      end;
    until senden>=0;
    pophp;
    closebox;

    case senden of
      0 : goto xexit;              { Abbruch }
      2 : intern:=true;            { nicht in Puffer + kein unversandt }
      3 : begin                    { Nachricht nach />>Unversandt }
            ParkMsg;               { ## Originalempfaenger einfuegen }
            pm:=false;
            Internbox:={default}box;
            empfaenger:=UnvBrett;
            parken:=true;
            betreffbox:=false; edit:=false; sendbox:=false;
            sendFlags:=sendFlags or sendIntern;
            cc_anz:=0;
            flcrash:=false;   { !! }
            goto fromstart;
          end;
      4 : begin
            DateSendIt;
            goto xexit;
          end;
    end;
    if sendFlags and sendIntern<>0 then intern:=true;

  until (senden<>5) and ((netztyp<>nt_Pronet) or SizeOk) and
        ((senden<>1) or intern or (grnr=LocGruppe) or pm or (fs+addsize<1024)
         or (fs+addsize>50000) or binary or QuoteOK)
  else begin
    senden:=SendDefault;    { not sendbox }
    case senden of
      2 : intern:=true;
    end;
    end;

  if pm then fidoto:=''
  else
    case netztyp of
      nt_Fido,
      nt_QWK      : fidoto:=fidoto;
    else
      if netztyp in (netsRFC + [nt_Magic, nt_Pronet, nt_ZConnect]) then begin
        if (fidoto=brettalle) or (blankpos(fidoto)=0) then
          fidoto:='';
      end else fidoto:='';
    end;

  RemovePhantomServers;

  if not verteiler then begin
    newbin:=binary or (docode=1) or (docode=2);
    pmc_code:=(docode>=3) and (docode<=2+maxpmc);
    SendMbox;
    DoSend:=true;

    if empfneu then
      if pm then
      begin
        AddNewUser(Empfaenger, Box);
        _brett:=mbrettd('U',ubase);
      end
      else
      begin
        dbAppend(bbase);                        { neues Brett anlegen }
        dbWriteNStr(bbase,bb_brettname,empfaenger);
        wbox:=iifs(empfaenger[1]='$','',box);
        intern:=intern or (wbox='');
        dbWriteNStr(bbase,bb_pollbox,wbox);
        halten:=stdhaltezeit;
        dbWriteN(bbase,bb_haltezeit,halten);
        dbWriteN(bbase,bb_gruppe,grnr);
        b:=iif(netztyp in netsRFC,16,0);
        dbWriteN(bbase,bb_flags,b);
        SetBrettindex;
        _brett:=mbrettd(empfaenger[1],bbase);
        end
    else
      if pm then begin
        dbSeek(ubase,uiName,UpperCase(empfaenger));
        dbReadN(ubase,ub_adrbuch,b);
        if b=0 then begin
          b:=1;
          dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
          end;
        end;

    if (cc_count>0) and (_bezug<>'') then begin
      flQto:=not pm;
      _pmReply:=pm;
      end;

    { --- 1. Schritt: Body erzeugen ----------------------------------- }

    betreff:=LeftStr(betreff,betrlen);
    Hdp.Clear;

    hdp.netztyp:=netztyp;
    if ntMIME(netztyp) then
      hdp.MIME.mversion := '1.0';

    if (not partsex) and assigned(sdata^.orghdp) then
    begin
      // just pass-through
      s1 := TFileStream.Create(datei,fmOpenRead);

      hdp.typ           := sdata^.orghdp.typ;
      hdp.mime.ctype    := sdata^.orghdp.mime.ctype;
      hdp.mime.encoding := sdata^.orghdp.mime.encoding;
      hdp.mime.disposition := sdata^.orghdp.mime.disposition;
      hdp.mime.cid      := sdata^.orghdp.mime.cid;
      hdp.charset       := sdata^.orghdp.charset;
      hdp.x_charset     := sdata^.orghdp.x_charset;
    end
    else case parts.count of
      0: assert(false);
      1: with TSendAttach_Part(parts[0]) do begin
//         if ntMIME(netztyp) and not ntConv(netztyp) then
//           hdp.typ := 'M' else // ZConnect with MIME
           if(TSendAttach_Part(parts[0]).FileEOL=MimeEOLNone) or
             (TSendAttach_Part(parts[0]).Analyzed.IsBinary) then
             hdp.typ := 'B'  // UUZ will encode
           else
             hdp.typ := 'T'; // UUZ will encode

           if ntMIME(netztyp) then
           begin
             hdp.mime.ctype    := TSendAttach_Part(parts[0]).ContentType.AsString;
             hdp.mime.encoding := TSendAttach_Part(parts[0]).ContentEncoding;
             hdp.mime.disposition := TSendAttach_Part(parts[0]).ContentDisposition.AsString;
             hdp.mime.description := TSendAttach_Part(parts[0]).ContentDescription;
           end; // ntMIME

           hdp.datei := TSendAttach_Part(parts[0]).FileNameO;
           hdp.summary := TSendAttach_Part(parts[0]).ContentDescription;
           if ntMime(netztyp) and not IsNAN(FileModify) then
             hdp.ddatum := DateTimeToZCDateTime(TSendAttach_Part(parts[0]).FileModify);

           if ContentType.NeedCharset then
           begin
             FileCharset:=MimeCharsetCanonicalName(FileCharset);
             ContentCharset:=MimeCharsetCanonicalName(ContentCharset);
           end;

{$IFDEF 0}
           if ntBinEncode(netztyp) and (hdp.typ='B') then
           begin
             hdp.typ='T';
             hdp.charset:='IBM437';
             hdp.x_charset:='';

             s1 := TMemoryStream.Create;

             writeln_s(s1,'Diese Nachricht beinhaltet eine uu-codierte BinÑrdatei. Verwenden Sie');
             writeln_s(s1,'das Programm uudecode zum Decodieren.');
             writeln_s(s1,'');
             writeln_s(s1,'This message contains an uu-encoded binary file. Use uudecode to obtain');
             writeln_s(s1,'the original file.');

             UUEncodeWriteContent(s1,TSendAttach_Part(parts[0]));

             if not flOhnesig and (sigfile<>'') then
             try
               s2 := TFileStream.Create(sigfile,fmOpenRead);
               try
                 CopyStream(s2,s1);
               finally
                 s2.Free;
               end;
             except
             end;

             if netztyp in [nt_Fido] then
               writeln_s(s1,fido_origin);

           end else
{$ENDIF}           
           // Determine whether we can use the file unchanged:
           if (netztyp=nt_Fido) or (FileEOL in [MimeEOLLF,MIMEEOLCR]) or
              ((IsMessage) and not flOhnesig and (sigfile<>'')) or
              (ContentType.NeedCharset and not MIMESaveCharsetAsCP437(FileCharset)) then
           begin
             s1 := TMemoryStream.Create;
             MIMEWriteContent(s1,TSendAttach_Part(parts[0]),hdp.typ='M',
               iifs((IsMessage) and not flOhnesig and (sigfile<>''),sigfile,''),
               fido_origin);
             if ContentType.NeedCharset then begin
               hdp.charset:=MimeCharsetToZC(FileCharset);
               hdp.x_charset:=ContentCharset;
             end;
           end else
           begin
             s1 := TFileStream.Create(FileName,fmOpenRead);
             if ContentType.NeedCharset then begin
               hdp.charset:=MimeCharsetToZC(FileCharset);
               hdp.x_charset:=ContentCharset;
             end;
           end;

         end; // with
      else
      begin
        Boundary:=MimeCreateMultipartBoundary(username); // does not contain chars that must be quoted
        hdp.boundary:=boundary;
        hdp.typ:='M';
        hdp.MIME.ctype := 'multipart/mixed; boundary="'+Boundary+'"';
        hdp.MIME.encoding := MimeEncoding7Bit;

        // if we use PGP/MIME, then only 7bit transparent encodings
        // are allowed.
        if flPGPSig or (docode in [8,9]) then
          for i:=0 to parts.Count-1 do
            with TSendAttach_Part(parts[i]) do
              if not Analyzed.EncodingSafeForSigned[ContentEncoding] then
                ContentEncoding := MimeEncodingQuotedPrintable;

        s1 := TMemoryStream.Create;

        for i:=0 to parts.Count-1 do
        begin
          with TSendAttach_Part(parts[i]) do
          begin
            if ContentType.NeedCharset then
            begin
              FileCharset:=MimeCharsetCanonicalName(FileCharset);
              ContentCharset:=MimeCharsetCanonicalName(ContentCharset);
            end;
            writeln_s(s1,'--'+boundary);
            MIMEWriteContentWithHeaders( s1,TSendAttach_Part(parts[i]),
              iifs((i=0) and (IsMessage) and
                not flOhnesig and (sigfile<>''),SigFile,''));
            writeln_s(s1,'');
            if (hdp.mime.Encoding=MimeEncoding7bit) and (ContentEncoding=MimeEncoding8bit) then
              hdp.mime.Encoding:=MimeEncoding8bit else
            if (hdp.mime.Encoding in [MimeEncoding7bit,MimeEncoding8bit]) and (ContentEncoding=MimeEncodingBinary) then
              hdp.mime.Encoding:=MimeEncodingBinary;
          end; // with
        end; // for
        writeln_s(s1,'--'+boundary+'--');
      end;
    end;

    { --- 2. Schritt: Headerdaten erzeugen ---------------------------- }

    if ntZConnect(netztyp) then begin
      if pm then
        hdp.empfaenger:=empfaenger            { PM }
      else if empfaenger[1]<>'1' then
        hdp.empfaenger:=mid(empfaenger,2)     { normale AM }
      else begin
        hdp.empfaenger:=mid(empfaenger,3);    { interne PM-Brett-Nachricht }
        p:=cpos('/',hdp.empfaenger);
        if p=0 then hdp.empfaenger:=hdp.empfaenger+'@'+box
        else hdp.empfaenger[p]:='@';
        end;
      if pm then hdp.archive:=true;
      end
    else
      hdp.empfaenger:=iifs(pm,TO_ID+empfaenger,mid(empfaenger,2));
    hdp.betreff:=betreff;
    case ntDomainType(netztyp) of    { s. auch XP3O.CancelMessage! }
      0 : hdp.absender:=username+'@'+iifs(aliaspt,pointname,box)+'.ZER';
      1 : begin
            hdp.absender:=username+'@'+iifs(aliaspt,box,pointname);
            if not aliaspt then hdp.real_box:=box;
          end;
      2 : hdp.absender:=username+'@'+pointname;
      3 : hdp.absender:=username+'@'+box;
      4 : hdp.absender:=username+'@'+FidoAbsAdr;
      5 : hdp.absender:=username+'@'+iifs(aliaspt,pointname,box)+domain;
      6 : begin
            hdp.absender:=iifs(sdata^.SenderMail='',
                               username+'@'+iifs(aliaspt,box+ntServerDomain(box),pointname+domain),
                               sdata^.SenderMail);
            hdp.real_box:=box;
          end;
      7 : begin
            hdp.absender:=username+'@'+box+';'+pointname;
            hdp.real_box:=box;
          end;
      8 : hdp.absender:=iifs(sdata^.SenderMail='',username,sdata^.SenderMail);
    end;
    hdp.realname:=realname;
    if (sendFlags and sendWAB<>0) and ntAdrCompatible(sData^.onetztyp,netztyp)
    then begin
      hdp.wab:=hdp.absender; hdp.war:=hdp.realname;
      hdp.absender:=sData^.oab; hdp.realname:=sData^.oar;
      { sData^.oab:=''; }
      end;

    if netztyp=nt_Magic then
      hdp.hd_point:=pointname;
    hdp.replyto := sData^.Replyto;
    if (not pm) and (sData^.followup.count>0) then
      hdp.followup.assign(sData^.followup);
    hdp.Keywords:=sData^.keywords;
    hdp.Summary:=sData^.summary;
    if  ntAdrCompatible(sData^.onetztyp,netztyp)
    then begin
      if sendFlags and sendWAB=0 then begin
        hdp.oab:=sData^.oab; hdp.oar:=sData^.oar;
        end;
      hdp.oem.Assign(sData^.oem);
      end;
    if UpperCase(sData^.ReplyGroup)<>UpperCase(mid(empfaenger,2)) then
      hdp.ReplyGroup:=sData^.ReplyGroup;
    if not pm then
      hdp.distribution:=sData^.distribute;
    hdp.quotestring:=sData^.quotestr;
    sendedat:=ixdat(zdate);
    hdp.datum:=iifs(ReplaceEtime,LeftStr(zdate,6)+'0000',zdate);
    case netztyp of
      nt_Magic  : hdp.pfad:=box;
      nt_Quick,
      nt_GS     : hdp.pfad:=pointname;
      nt_Pronet : hdp.pfad:=box {+';'+pointname};
      nt_UUCP   : hdp.pfad:=iifs(aliaspt,username,pointname+domain+'!'+username);
    else
      hdp.pfad:='';
    end;
    dbAppend(mbase);            { neue mbase.INT_NR fuer MessageID }
    hdp.msgid:=MessageID;
    sData^.msgid:=hdp.msgid;

    if (_beznet>=0) and ntMIDCompatible(_beznet,netztyp) then
      if sData^.References.IndexOf(_bezug)=-1 then
        sData^.References.Add(_bezug);

    if (_beznet>=0) then  // bugfix fÅr VP
      if ntOrigID(netztyp) and ntMIDCompatible(_Beznet,netztyp) then
        hdp.org_xref:=_orgref;

    hdp.replypath:=_replypath;
//  hdp.typ:=iifs(binary,'B','T');
    if (netztyp<>nt_Fido) or pm then
      hdp.programm:=LeftStr(xp_xp,iif(cpos('/',xp_xp)<>0,cpos('/',xp_xp)-1,length(xp_xp)))
                     +'/'+trim(verstr)
                     +' ('+Without(Without(Trim(pformstr),'('),')')+betastr
                     {$IFDEF Snapshot} + ' @ ' + compiletime {$ENDIF}
                     +')';
    hdp.organisation:=orga;
    if sdata^.ersetzt<>''then hdp.ersetzt:=sdata^.ersetzt;
    if (pm and ntPMTeleData(netztyp)) or (not pm and ntAMTeleData(netztyp))
    then begin
      hdp.postanschrift:=postadresse;
      hdp.telefon:=telefonnr;
      hdp.homepage:=wwwHomepage;
      end
    else if (netztyp in netsRFC) and not adrpmonly then
      hdp.homepage:=wwwHomepage;
    hdp.priority:=rfcprio;      { 6.2.2000 MH: X-Priority: } { unbedenklich }
    hdp.xnoarchive:=noarchive;  {!MH: X-NoArchive: Yes }     { unbedenklich }
    hdp.datei:=sendfilename;
    hdp.ddatum:=sendfiledate;
    if FidoTo<>'' then
      hdp.fido_to:=fidoto
    else
      if not pm and (netztyp in [nt_Fido,nt_QWK]) then hdp.fido_to:=brettalle;
    hdp.attrib:=iif(pm and flEB,attrReqEB,0);
    if IsEbest then with hdp do
    begin
      attrib := attrib and (not attrReqEB) + attrIsEB;
      if netztyp in netsRFC { !!and BoxPar.EB_Daemon }then
      begin
        if ReplyTo='' then ReplyTo:=absender;
        absender:='MAILER-DAEMON'+mid(absender,cpos('@',absender));
        if (realname<>'') and (length(realname)<=31) then begin
          realname:=realname+'''';
          if not (realname[length(realname)-1] in ['s','z','·']) then
            realname:=realname+'s';
          realname:=realname+' Mailer'
          end;
        end;
      end;
    if FileAttach then { Fido file attachment }
      inc(hdp.attrib,attrFile);
    if netztyp=nt_Maus then
      if flQTo then inc(hdp.attrib,AttrQuoteTo);
    if ntPmReply(netztyp) then
      if _pmReply then inc(hdp.attrib,AttrPmReply);
    if ControlMsg then inc(hdp.attrib,AttrControl);
    if ((hdp.typ='B') and (netztyp in netsRFC) and multipartbin) or
       ((hdp.typ='B') and (netztyp=nt_Maus) and mausmpbin) then
      inc(hdp.attrib,AttrMPbin);
    if flPGPkey then
      inc(hdp.pgpflags,fPGP_haskey);
    if flPGPreq then
      inc(hdp.pgpflags,fPGP_request);
    if UsePGP and not flPGPkey and ntPGP(netztyp) then begin
      if not FileExists(PGPkeyfile) then UpdateKeyfile;
      if FileExists(PGPkeyfile) then
        inc(hdp.pgpflags,fPGP_avail);
      end;
    hdp.prio:=msgprio;
    hdp.nokop:=flNokop;
//    if umlaute=0 then
//      if netztyp=nt_Fido then
//        hdp.x_charset:='IBMPC 2'   { s. FSC-0054, grmpf }
//      else if netztyp in netsRFC then
//        if FileContainsUmlaut then hdp.x_charset:='ISO-8859-1';
//    if iso then
//      hdp.charset:='ISO1';
    if assigned(sData^.orghdp) then
      with sData^.orghdp do begin
        { hdp.zdatum:=zdatum; hdp.orgdate:=true;  !! Unversandt/* !! }
        hdp.organisation:=organisation;
        hdp.ReplyTo := ReplyTo;
        hdp.datei:=datei; hdp.ddatum:=ddatum;
        end;
    if _sendmaps then
      hdp.replyto := '';
    SetXpointCtl;
    if cc_anz=0 then     { Anzahl der Crossposting-EMPS ermitteln }
      msgCPanz:=0
    else
      msgCPanz:=ccm^[0].cpanz;
    msgCPpos:=0;

    for ii:=1 to msgCPanz-1 do
      EmpfList.Add(cc^[ii]);
    hdp.References.Assign(sData^.References);

    hdp.groesse:=s1.Size;
    s1.Seek(0,soFromBeginning);

    fn2:=TempS(s1.Size+4000);
    s2 := TFileStream.Create(fn2,fmCreate);

    hdp.WriteToStream(s2);      // Header erzeugen
    CopyStream(s1,s2);  // Body anh‰ngen

    s2.Free; {s2:=nil;}

    { --- 3. Schritt: Nachricht in Datenbank ablegen ------------------ }

    repeat                                   { einzelne Crosspostings in }
      if ntZConnect(netztyp) then begin      { mbase ablegen             }
        b:=10;
        dbWriteN(mbase,mb_ablage,b);
        end;                                 { ansonsten bleibt's bei 0 }
      l:=netztyp;
      if hdp.GetLastReference <> '' then inc(l,$100); // rÅckwÑrts-verkettet
      if FileAttach then inc(l,$200);
      if hdp.pm_reply then inc(l,$400);
      if (hdp.wab<>'') or (hdp.oem.Count > 0) then inc(l,$800);
      if (hdp.typ='T')and(hdp.charset='ISO1')then inc(l,$2000);
      if flPGPsig then inc(l,$4000);
      if msgCPanz>0 then begin
        inc(l,longint(msgCPanz) shl 16);
        inc(l,longint(succ(msgCPpos)) shl 24);        { Empfaengernummer }
        end;
      dbWriteN(mbase,mb_netztyp,l);
      shortmid:=FormMsgid(hdp.msgid);
      dbWriteNStr(mbase,mb_msgid,shortmid);
      dbWriteNStr(mbase,mb_brett,_brett);
      dbWriteNStr(mbase,mb_betreff,hdp.betreff);
      dbWriteNStr(mbase,mb_absender,hdp.absender);
      dbWriteNStr(mbase,mb_mimetyp,LowerCase(Trim(LeftStr(hdp.mime.ctype,CPosX(';',hdp.mime.ctype)-1))));
      l:=ixdat(hdp.datum);
      dbWriteN(mbase,mb_origdatum,l);
      dbWriteN(mbase,mb_empfdatum,sendedat);
      dbWriteN(mbase,mb_groesse,hdp.groesse);
      dbWriteN(mbase,mb_typ,hdp.typ[1]);
      if ntEditBrettempf(netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.fido_to)
      else if ntRealname(netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.realname);
      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      if sendFlags and sendHalt<>0 then b:=1
      else if flLoesch then b:=2
      else if not (HaltOwn and (sendbox or _verteiler))
        or (pm and not HaltOwnPM) then b:=0; { Eigene Nachrichten Halten gilt nicht fuer Mails }
      dbWriteN(mbase,mb_halteflags,b);
      if intern then b:=0
      else b:=1;
      if flCrash and MayCrash then inc(b,16);    { !! Crash-Flag }
      dbWriteN(mbase,mb_unversandt,b);

      dbreadN(mbase,mb_flags,flags);                 { Farb - Flags setzen... }
      flags := flags or 256; // this mail is from yourself, needed for replaceown

      flags:=flags and not 56;
      if netztyp=nt_Zconnect then                    { Zconnect-Prioritaet: }
        if msgprio=10 then flags:=flags or 16        { Direkt = Hoch }
        else if msgprio=20 then flags:=flags or 8;   { Eilmail = Hoechste }

      case rfcprio of                                { RFC - Prioritaet }
        1 : flags:=flags or 8;                       { hoechste }
        2 : flags:=flags or 16;                      { hoch }
        4 : flags:=flags or 24;                      { niedrig }
        5 : flags:=flags or 32;                      { niedrigste }
        end;

      dbwriteN(mbase,mb_flags,flags);

      if msgCPpos=0 then begin
        if OldMsgsize<>0 then begin
          dbWriteN(mbase,mb_msgsize,oldmsgsize);
          dbWriteN(mbase,mb_adresse,oldmsgpos);
          oldmsgsize:=0;  { zur Sicherheit.. }
          end;
        Xwrite(fn2);
        dbReadN(mbase,mb_adresse,m1adr);
        dbReadN(mbase,mb_msgsize,m1msgsize);
        _era(fn2);
        end
      else begin
        dbWriteN(mbase,mb_adresse,m1adr);
        dbWriteN(mbase,mb_msgsize,m1msgsize);
        end;

      if (sendFlags and sendMark<>0) and (msgCPpos+1=msgMarkEmpf) then
        msgaddmark;
      AddBezug(hdp,iif(msgCPanz=0,0,iif(msgCPpos=0,1,2)));
      if cc_anz=0 then dbFlushClose(mbase);
      if not pm and (msgCPpos=0) then begin    { Brettdatum neu setzen }
        dbSeek(bbase,biBrett,UpperCase(empfaenger));
        if not dbFound then
          tfehler('neue Msg: Brett weg??',30)
        else
          if not smdl(sendedat,dbReadInt(bbase,'ldatum')) then
            { nur, wenn keine Wiedervorlage vorhanden! }
            dbWriteN(bbase,bb_ldatum,sendedat);
        end;
      inc(msgCPpos);
      while (msgCPpos<msgCPanz) and ccm^[msgCPpos].nobrett do
        inc(msgCPpos);
      if msgCPpos<msgCPanz then begin
        repeat
          if ccm^[msgCPpos].ccpm then
          begin
            dbSeek(ubase,uiName,UpperCase(cc^[msgCPpos]));
            if dbFound then
            begin
              _brett:=mbrettd('U',ubase);
              if dbreadint(ubase,'adrbuch')=0 then      { CC-Empfaenger ins Adressbuch aufnehmen }
                dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
            end;
          end
          else begin
            dbSeek(bbase,biBrett,'A'+UpperCase(cc^[msgCPpos]));
            if dbFound then begin
              _brett:=mbrettd('A',bbase);
              dbWriteN(bbase,bb_ldatum,sendedat);    { Brettdatum neu setzen }
              end;
            end;
          if not dbFound then inc(msgCPpos);
        until dbFound or (msgCPpos>=msgCPanz);
        if msgCPpos<msgCPanz then
          dbAppend(mbase);
        end;
      SendFlags:=SendFlags and not SendHalt;
    until msgCPpos>=msgCPanz;

    { --- 3. Schritt: Nachricht ggf. fuer Pollpaket kodieren --------- }

    if not intern then begin
      s1.Seek(0,soFromBeginning);

      if (hdp.typ<>'M') and 
        ( (docode in [1..5,9]) or 
          (flPGPSig and ((cancode=9) or ((cancode<>8) and not PGP_MIME))) ) then
      begin
        case docode of
          1: EncryptMessage(false,s1); // QPC
          2: EncryptMessage(true, s1); // DES
          3..5: pmEncryptMessage(s1);  // PMC-1..3
          8,9: XP_PGP.PGP_EncodeStream(s1,hdp,passwd,true,flPGPSig,fo);
          else if flPGPSig then XP_PGP.PGP_EncodeStream(s1,hdp,'',false,true,fo);
        end;
        end else
      begin
        if (hdp.typ<>'M') and (flPGPSig or (docode in [8,9])) then
        begin
          // encode the single part NOW for MIME
          assert(parts.count=1);

          s1.Free; s1:=TMemoryStream.Create;
          with TSendAttach_Part(parts[0]) do
            MIMEWriteContent(s1,TSendAttach_Part(parts[0]),true,
              iifs((IsMessage) and not flOhnesig and (sigfile<>''),sigfile,''),
              fido_origin);
          hdp.typ:='M';
        end;

        if flPGPSig then
          XP_PGP.PGP_MimeSignStream(s1,hdp);

        if (docode in [8,9]) then
          XP_PGP.PGP_MimeEncodeStream(s1,hdp,passwd);
      end;

    { --- 4. Schritt: Nachricht ins Pollpaket schreiben -------------- }

      for ii:=1 to msgCPanz-1 do
        Empflist.Add(cc^[ii]);

      if not flCrash or not MayCrash then
        fn2 := boxfile+ExtBoxfile
      else begin
        fn2 := CrashFile(hdp.empfaenger);
        SetCrashInfo;
      end;

      if FileExists(fn2) {grrr} then begin
        s2 := TFileStream.Create(fn2,fmOpenReadWrite);
        s2.Seek(0,soFromEnd);
      end else
        s2 := TFileStream.Create(fn2,fmCreate);

      hdp.groesse := s1.Size;
      hdp.WriteToStream(s2);
      EmpfList.Clear;

      s1.Seek(0,soFromBeginning);
      CopyStream(s1,s2);
      s2.Free; {s2:=nil;}

      if uvs_active and (aktdispmode=11) and (cc_count=0) and
         (msgCPanz<=1) then
        MsgAddmark;

    end; // not intern

    s1.Free; s1:=nil;
    closebox;    { "Nachricht abschicken/speichern" }

    if not intern and
      not noCrash and flCrash and MayCrash and FidoAdrOK(false) and
         ReadJN(getres(615),true) then    { 'Crash sofort absenden' }
        AutoCrash:=CrashAdr;  { Empfaenger, evtl. ohne Point }

  { --- CCs ----------------------------------------------------------- }

    if msgCPanz>1 then begin    { cc-Epfaenger bis auf einen ueberspringen }
      Move(cc^[msgCPanz],cc^[1],(maxcc-msgCPanz+1)*sizeof(cc^[1]));
      Move(ccm^[msgCPanz-1],ccm^[0],(maxcc-msgCPanz+2)*sizeof(ccm^[1]));
      dec(cc_anz,msgCPanz-1); inc(cc_count,msgCPanz-1);
      end;

//  if not binary then _era(fn);
  end;   { not verteiler }

  if cc_anz>0 then begin           { weitere CC-Empfaenger bearbeiten }
    empfaenger:=cc^[1];
    Move(cc^[2],cc^[1],(maxcc-1)*sizeof(cc^[1]));
    Move(ccm^[1],ccm^[0],maxcc*sizeof(ccm^[1]));
    dec(cc_anz); inc(cc_count);
    pm:=cpos('@',empfaenger)>0;
    if not pm then empfaenger:='A'+empfaenger;
    betreffbox:=false; edit:=false; sendbox:=false;
    SendDefault:=senden;
    _verteiler:=true;
    goto fromstart;
    end;

  { --- Aufr‰umarbeiten zum Schluss ----------------------------------- }

//  if FidoBin and FileExists(datei) and EditAttach then begin
//    _era(datei);
//    datei:=betreff;
//  end;

  aufbau:=true; xaufbau:=true;
  { es muss jetzt der korrekte Satz in mbase aktuell sein! }
xexit:
  freeres;
  dispose(cc);
  Hdp.Free;
  if sigtemp then _era(sigfile);
xexit1:
  if sdNope then freesenduudatamem(sdata);
xexit2:
  forcebox:=''; forceabs:='';
  sendfilename:=''; sendfiledate:='';
  _bezug:=''; _orgref:=''; _beznet:=-1; _replypath:='';
  fidoto:=BrettAlle;
  flCrash:=false;
  flEB:=false; IsEbest:=false;
  flQto:=false;
  flMloc:=false; flMnet:=false;
  _pmReply:=false;
  NoCrash:=false;
  FileAttach:=false; EditAttach:=true;
  msgprio:=0;
  rfcprio:=0;
  ControlMsg:=false;
  NewbrettGr:=0;
  oldmsgpos:=0; oldmsgsize:=0;

//  Dispose(cc); Dispose(ccm);
  for ii:=0 to parts.count-1 do
    TObject(parts[ii]).Free;
  parts.Free;
end; {------ of DoSend -------}


procedure send_file(pm,binary:boolean);
const xp_support = 'A/T-NETZ/SUPPORT/XPOINT';
var
    empf,repto : string;
    betr,dummy : string;
    hf         : string;
    reptoanz   : integer;
    fn         : string;
    useclip    : boolean;
    sData      : SendUUptr;

  function FileOK:boolean;
  var f : file;
  begin
    fileok:=true;
    assign(f,fn);
    reset(f);
    if ioresult>0 then fileok:=false
    else close(f);
  end;

begin
  betr:='';
  case aktdispmode of
   -1..0 : empf := dbReadNStr(bbase,bb_brettname); { B^inaer / Text^File an Brett }
    1..4 : empf := dbReadNStr(ubase,ub_username);  { B^inaer / Text^File an User }
  10..19 : begin
             empf := dbReadNStr(mbase,mb_absender);  { ^I/^F an Absender der Msg }
             betr := dbReadNStr(mbase,mb_betreff);
             ReplyText(betr,false);
           end;
  end;
  fn:=sendpath+Wildcard;
  useclip:=true;
  if readfilename(getres(iif(binary,613,614)),fn,true,useclip)   { 'Binaerdatei' / 'Textdatei' versenden }
  then begin
    if not multipos(_MPMask,fn) then fn:=sendpath+fn else
    fn:=ExpandFileName(fn);
    if not FileExists(fn) then rfehler(616)    { 'Datei nicht vorhanden' }
    else if not FileOK then fehler(getres(102)) { Fehler beim Dateizugriff }
    else
    begin
      {fsplit(fn,dir,name,ext);}
      if betr='' then betr:=ExtractFileName(fn)
      else betr:=LeftStr(ExtractFilename(fn)+' ('+betr,39)+')';
      sdata:=allocsenduudatamem;
      if aktdispmode in [10..19] then begin
        get_bezug(pm,repto,reptoanz,dummy,Pointer(sData),false);
        if repto<>'' then empf:=repto;
        end;
      hf:='';
      sendfilename:=UpperCase(ExtractFilename(fn));
      sendfiledate:=zcfiletime(fn);
      if DoSend(pm,fn,useclip,true,empf,betr,false,binary,true,true,false,sData,hf,0) then;
      freesenduudatamem(sData);
      end;
    end;
end;


function SendPMmessage(betreff,fn:string; is_temp:boolean; var box:string):boolean;
var d    : DB;
    empf : string;
    s    : string;
    l    : longint;
begin
  SendPMmessage:=false;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then empf:='1/'+dbReadStr(d,'username')+iifs(userboxname,'/'+box,'')
  else empf:='';
  dbClose(d);
  if empf<>'' then begin
    InternBox:=box; forcebox:=box;
    s:='';
    if DoSend(false,fn,is_temp,false,empf,betreff,
              false,false,false,false,false,nil,s,sendIntern+sendShow)
    then begin
      l:=dbReadInt(mbase,'unversandt') or 64;    { interne Nachricht }
      dbWriteN(mbase,mb_unversandt,l);     { -> keine Mausstatus-Abfrage }
      SetUngelesen;
      SendPMmessage:=true;
      end;
    end;
end;

initialization
  SendEmpfList := TStringList.Create;
finalization
  SendEmpfList.Free;

{
  $Log$
  Revision 1.21  2001/10/01 10:26:19  ma
  - (hopefully) fixed Fido binary mails

  Revision 1.20  2001/09/25 21:07:45  cl
  - added UI for non-RFC network charset selection

  Revision 1.19  2001/09/24 22:04:51  mk
  - fixed last commit

  Revision 1.18  2001/09/21 16:16:49  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.17  2001/09/19 18:02:36  cl
  - various small fixes

  Revision 1.16  2001/09/16 23:01:20  cl
  - BUGFIX: Fido tearline now added

  Revision 1.15  2001/09/16 18:01:40  ma
  - removed bin_msg (seems to be some relic, caused error messages with fido)

  Revision 1.14  2001/09/14 18:26:35  cl
  - fixed iso flag
  - no ZConnect-MIME for single-part messages

  Revision 1.13  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.12  2001/09/09 17:40:47  cl
  - moved common code between alle en-/decoding streams to a base class
  - all en-/decoding streams can now destruct the other stream
  - much more elegant way to connect en-/decoding streams to each other

  Revision 1.11  2001/09/08 23:30:58  cl
  - fixed last fix

  Revision 1.10  2001/09/08 21:07:16  cl
  - MIME typ now always written to database in lower case and without paramters

  Revision 1.9  2001/09/08 18:46:43  cl
  - small bug/compiler warning fixes

  Revision 1.8  2001/09/08 16:29:40  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.7  2001/09/08 14:42:09  cl
  - added Multipart-MIME support
  - added PGP/MIME support
  - adaptions/fixes for MIME support
  - adaptions/fixes for PGP/MIME support

  Revision 1.6  2001/09/07 13:54:25  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.5  2001/09/07 09:17:56  mk
  - added AddNewBrett procedure

  Revision 1.4  2001/08/29 19:50:47  ma
  - changes in net type handling (2)
  - shortened CVS logs

  Revision 1.3  2001/08/29 17:08:12  mk
  - Fix: first character of PM recipient in temp file HEADER.HDR is not
    truncated anymore (whatever HEADER.HDR might be needed for)

  Revision 1.2  2001/08/23 11:15:04  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.1  2001/08/12 19:57:21  cl
  - rename xp6*.* => xpsendmessage*.*
  - fixed crash in DoSend w/ hdp.oem.free

  Revision 1.125  2001/08/12 11:50:41  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.124  2001/08/11 23:06:34  mk
  - changed Pos() to cPos() when possible

  Revision 1.123  2001/08/11 21:20:51  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.122  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.121  2001/07/31 16:18:40  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.120  2001/07/29 12:57:27  ma
  - removed Developer and ntAllowed variables
  - fixed setting of NNTP area db flags

  Revision 1.119  2001/07/28 12:04:13  mk
  - removed crt unit as much as possible

  Revision 1.118  2001/07/27 18:10:13  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.117  2001/07/23 16:05:20  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.116  2001/06/26 23:43:47  mk
  JG:- fixed ancient 'forcebox' bug: it was possible to e.g. create a mail to
       an RFC recipient and then select a Fido server in the send window.

  Revision 1.115  2001/06/06 18:44:01  mk
  JG:- Fix (DoSend): clear list of CC recipients after rfehler(606)
         ("Internal newsgroup - writing not allowed!"). Ancient bug
         that could lead to "ghost CC recipients", but that did not
         occur anymore - obviously due to some RTA code somewhere.

  Revision 1.114  2001/06/04 17:31:37  ma
  - implemented role feature

  Revision 1.113  2001/05/27 09:31:19  ma
  - enabled PGP and some other things for NNTP/POP3/IMAP

  Revision 1.112  2001/05/19 16:17:51  ma
  - removed XP_ID (shareware notice)
  - changed program id:
    "OpenXP/32 vVERSION (PLATFORM)"
}
end.


{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ MH: PGP-Sig auch in RFC, ZurÅck Button f. alle Netztypen }

{ Nachrichten versenden, weiterleiten, unversandt-bearbeiten }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp6;

interface

uses
  crt, dos, typeform,fileio,inout,keys,datadef,database,maske,crc,lister,
  winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,xp2c,xp_des,xpe,
  xpglobal, lfn;

const sendIntern = 1;     { force Intern              }
      sendShow   = 2;     { ausfÅhrliche Sendeanzeige }
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

      pgdown    : boolean = false;
      _sendmaps : boolean = false;
      forcebox  : string[BoxNameLen] = '';
      forceabs  : string[10] = '';       { 'SYSOP' fÅr ProNet-System }
      _bezug    : string[120] = '';
      _orgref   : string[120] = '';
      _ref6list : refnodep = nil;
      _beznet   : shortint = -1;         { Netztyp der Bezugsnachricht }
      _replypath: string[8] = '';        { Box, Åber die die Bezugsnachr. kam }
      _pmReply  : boolean = false;
      IsEbest   : boolean = false;
      NoCrash   : boolean = false;
      FileAttach: boolean = false;
      EditAttach: boolean = true;
      msgprio   : byte    = 0;           { ZConnect-Prio }
      rfcprio   : byte    = 0;           { RFC-Priority  }   { 6.2.2000 MH: }
      ControlMsg: boolean = false;
      newbrettgr: longint = 0;           { Gruppe fÅr neues Brett }
      flCrash   : boolean = false;
      flQTo     : boolean = false;       { Maus: Wildwestverkettung }
      flNokop   : boolean = false;
      qmpdata   : pointer = nil;

      OldMsgSize: longint = 0;{ s. XP3.XWrite }
      OldMsgPos : longint = 0;

      sendfilename   : string = '';
      sendfiledate   : string[14] = '';
      force_quotemsk : string[12] = '';
      sendempflist   : empfnodep = nil;
      CrosspostBox   : string[BoxNameLen] = '';

type  SendUUdata = record
                     AmReplyTo  : string[AdrLen];
                     PmReplyTo  : string[AdrLen];
                     keywords   : string[60];
                     summary    : string[200];
                     distribute : string[40];
                     ReplyGroup : string[40];     { Maus-QuoteTo }
                     oab,oem,wab: string[AdrLen];
                     oar,war    : string[40];
                     onetztyp   : byte;
                     orghdp     : headerp;
                     quotestr   : string[20];
                     UV_edit    : boolean;        { <Esc> -> "J" }
                     empfrealname : string[40];
                     msgid, ersetzt    : string[MidLen];
                     RTAHasSetVertreter :boolean;
                   end;
      SendUUptr   = ^SendUUdata;

var   InternBox : string[BoxNameLen];  { Boxname bei /Netzanruf }
      msgMarkEmpf: byte;   { fÅr sendMark }


function DoSend(pm:boolean; var datei:pathstr; empfaenger,betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData:SendUUptr;
                var header,signat:string; sendFlags:word):boolean;
procedure send_file(pm,binary:boolean);
function  SendPMmessage(betreff,fn:string; var box:string):boolean;

function umlauttest(var s:string):boolean;
function test_senddate(var s:string):boolean;
procedure firstslash(var s:string);
function testreplyto(var s:string):boolean;

function pgpo_sigtest(var s:string):boolean;
function pgpo_keytest(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses xp1o,xp3,xp3o,xp3o2,xp3ex,xp4e,xp9,xp9bp,xpcc,xpnt,xpfido,
     xp_pgp,xp6l,xms,xpovl;

procedure ukonv(typ:byte; var data; var bytes:word); assembler;
asm
         mov   dx,0
         les   di,bytes
         mov   cx,es:[di]
         jcxz  @ende
         les   di, data
         lea   si,[di+1500]
         cld
         mov   bl,typ
         cmp   bl,2                    { ISO? }
         jz    @isolp

@uklp:   mov   al,es:[si]              { IBM -> ASCII }
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
         inc   dx
         cmp   dx,1500
         jz    @ende                    { Konvertierpuffer voll :-( }
         inc   si
         loop  @uklp
         jmp   @ende
@noconv: stosb
         inc   si
         loop  @uklp
         jmp   @ende

@isolp:  mov   al,es:[si]
         inc   si
         stosb
         loop  @isolp

@ende:    les   di,bytes
         add   es:[di],dx
end;

function  testbin(var bdata; rr:word):boolean; assembler;
asm
         push ds
         mov   cx,rr
         lds   si,bdata
         cld
@tbloop:  lodsb
         cmp   al,9
         jb    @is_bin                  { BinÑrzeichen 0..8 }
         cmp   al,127
         jae   @is_bin                  { "binÑr"zeichen 127..255 }
         cmp   al,32
         jae   @no_bin                  { ASCII-Zeichen 32..126 }
         cmp   al,13
         jbe   @no_bin                  { erlaubte Zeichen 9,10,12,13 }
@is_bin: mov   ax,1                     { TRUE: BinÑrzeichen gefunden }
         jmp   @tbend
@no_bin: loop  @tbloop
         mov   ax,cx                    { FALSE: nix gefunden }
@tbend:  pop ds
end;

function  ContainsUmlaut(var s:string):boolean; assembler;
asm
         push ds
         cld
         lds   si,s
         lodsb
         mov   cl,al
         mov   ch,0
         jcxz  @cu_ende
@cu_loop: lodsb
         or    al,al
         js    @cu_found
         loop  @cu_loop
         jmp   @cu_ende
@cu_found: mov  cx,1
@cu_ende: mov   ax,cx
         pop ds
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
    rfehler(601);    { 'RÅckdatieren nicht mîglich.' }
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
      dbSeek(d,piKurzname,ustr(s));
      if dbFound then
      begin
        dbRead(d,'Langname',s);
        dbclose(d);                     { ists ein Kurzname ? }
        testreplyto:=true;
        if cpos(' ',s)<>0 then           { jetzt der Langname jetzt gueltig ? }
          begin
            rfehler(908);               { 'ungÅltige Adresse' }
            testreplyto:=false;
            end;
        end
      else begin
        rfehler(908);                   { 'ungÅltige Adresse' }
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
var                                          { Format: 1105001824 }
 d:datetime;
begin
  unpacktime(filetime(paramstr(0)),d);
  compiletime:=(formi(d.day,2)+formi(d.month,2)+right(formi(d.year,2),2)
    +formi(d.hour,2)+formi(d.min,2));
end;
{$ENDIF}

{ --- Datei verschicken ---------------------------------------------------- }
{ Datei:  Pfadname der Datei. Wenn nicht vorhanden, wird eine leere angelegt }
{ empfaenger: der Empfaenger (User oder x/Brett)                             }
{ Edit :   Nachricht zunÑchst Editieren und dann erst senden                 }
{ Binary:  BinÑrdatei                                                        }
{ sendwin: vor dem Senden Sende-Fenster abfragen                             }
{ datei, header und signat sind nur aus Stack-Platz-GrÅnden VARs!            }
{ header wird verÑndert!!                                                    }

function DoSend(pm:boolean; var datei:pathstr; empfaenger,betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData:SendUUptr;
                var header,signat:string; sendFlags:word):boolean;

var f,f2     : ^file;
    edis     : byte;
    x,y      : byte;
    brk      : boolean;
    typ      : string[50];   { Kopf fÅr Betreff/Sende-Box          }
    wbox     : string[BoxNameLen];
    ch       : string[1];    { '*'-Zeichen fÅr abweichende Adresse }
    box      : string[BoxNameLen]; { EmpfÑnger-Pollbox             }
    d        : DB;
    fs,l     : longint;
    t        : taste;
    n,p      : shortint;
    fn,fn2,
    fn3      : ^pathstr;
    b        : byte;
    si0      : smallword;
    hdp      : headerp;

    adresse  : string[AdrLen];
    size     : smallword;
    empfneu  : boolean;
    cancode  : shortint;    { -1=Rot13, 0=kein PW, 1=QPC, 2=DES, 9=PGP }
    docode   : shortint;    { gewÑhlte Codierung                  }
    pmc_code : boolean;
    senden   : shortint;    { 0=Nein, 1=Ja, 2=Intern              }
    newbox   : string[20];  { Zwischensp. fÅr geÑnderte Pollbox   }
    halten   : integer16;   { Haltezeit fÅr neuen User            }
    boxfile  : string[12];
    username : string[30];  { eigener Username                    }
    pointname: string[25];
    email    : string[80];
    sendedat : longint;     { Empfangsdatum                       }
    XP_ID    : string[40];
    XID      : string[40];  { CrossPoint-ID                       }
    passwd   : ^string;     { Pa·wort des empfangenden Users      }
    passpos  : smallword;   { PW-Position fÅr QPC                 }
    newbin   : boolean;     { Typ nach Codierung                  }
    intern,                 { interne Nachricht                   }
    lokalPM  : boolean;     { lokale PM                           }
    maxsize  : longint;     { ab hier mu· gesplittet werden       }
    grnr     : longint;     { Brettgruppen-Nr.                    }
    _brett   : string[5];
    addsize  : longint;     { Header + Signatur                   }
{    hdsize   : word; }
    mapsname : string[20];
    oversize : longint;     { Nachrichtenlimit Åberschritten      }
    parken   : boolean;     { Nachricht nach /ØUnversandt         }
    senddate : datetimest;  { mit 'D' zeitversetzt absenden       }
    bin_msg  : boolean;     { BinÑr-Versandmeldung                }
    SendDefault : byte;
    verteiler: boolean;
    _verteiler: boolean;    { bleibt true bei allen Einzelnachrichten }
    netztyp  : byte;        { Netztyp                             }
    shortmid : string[19];

    realname : string[40];
    aliaspt  : boolean;     { Alias-Point (USER@BOX)              }
    domain   : string[60];
    fqdn     : string[60];  { 16.01.00: HS}
    nomids   : boolean;     { keine Message-ID's erzeugen         }
    nobox    : boolean;     { Absender-Name im PP ohne Boxname    }
    fidoname : string[60];  { Origin-Systemname                   }
    echomail : boolean;
    fadd     : shortint;
    oldnt    : byte;        { alter Netztyp bei Pollbox-Wechsel   }
    fidoam   : boolean;
    OrigBox  : string[BoxNameLen];    { Box aus Pfad  }
    AltAdr   : string[20];  { Gruppen / Fido-Absender }
    old_cca  : integer;     { vor (K)opien            }
    sendbutt : string[80];
    FidoBin  : boolean;     { File Attach }
    cc_count : integer;
    betrlen  : byte;        { max. BetrefflÑnge }
    bboxwid  : byte;

    spezial  : boolean;
    flOhnesig: boolean;
    flLoesch : boolean;
    kopkey   : string[1];   { (K)opien }
    fidokey  : string[1];   { (A)n     }
    pgpkey   : string[1];
    sdnope   : boolean;     { sData = nil }
    oldbetr  : string[20];
    orgftime : longint;
    sigfile  : pathstr;
    sigtemp  : boolean;
    iso      : boolean;
    flPGPkey : boolean;     { eigenen Key mitschicken }
    flPGPsig : boolean;     { Nachricht signieren }
    flPGPreq : boolean;     { Key-Request }

    msgCPanz : shortint;    { EMP's in aktueller Msg. }
    msgCPpos : shortint;    { gerade bearbeiteter EMP }
    ii       : integer;
    m1adr    : longint;     { Pufferadresse der ersten Kopie }
    m1msgsize: longint;     { Gesamtgrî·e der ersten Kopie   }
    showempfs: shortint;    { fÅr Betreffbox }
    fo       : ^string;
    flags    : longint;
    force_absender : string;

label xexit,xexit1,xexit2,fromstart,ReadAgain;

{$I xp6s.inc}

function uucpbrett(s:string; edis:byte):string;
var i : integer;
begin
  if (edis=1) or (netztyp<>nt_UUCP) or not NewsgroupDisp then
    uucpbrett:=mid(s,edis)
  else begin
    delete(s,1,2);
    for i:=1 to length(s) do if s[i]='/' then s[i]:='.';
    uucpbrett:=s;
    end;
end;

procedure EditNachricht(pushpgdn:boolean);
var p      : byte;
    edpush : boolean;

    cc_hand,  cc_size : word;
    ccm_hand,ccm_size : word;
  (*  ma_hand,  ma_size : word; *)
    xmsstored         : boolean;

  procedure store_arrays;     { Arrays ins XMS sichern und Speicher freigeben }
  var i : longint;
      n : byte; {*}

    function xms_ok:boolean;    {*} { Debugcode evtl. spaeter entfernen oder ausklammern.}
    begin
      xms_ok:=true;
      inc(n);
      if xmsresult=0 then exit
      else begin
        xms_ok:=false;
        hinweis('');
        hinweis('XMS Fehler: '+strs(n)+','+strs(xmsresult)+' '+strs(xmsavail)+'/'+
          strs(((sizeof(cc^)+sizeof(ccm^)+sizeof(marked^)) div 1024) +3) +'K');
        if n=2 then XmsFree(ccm_hand);
        XmsFree(cc_hand);
        end;
    end;

  begin
    XmsStored:=false;
    if xmstest and (xmsavail > ((sizeof(cc^)+sizeof(ccm^) (*+sizeof(marked^)*) ) div 1024) +3)
    then begin
      n:=0;
      cc_size:=sizeof(cc^);
      cc_hand:=XmsAlloc(sizeof(cc^) div 1024 +1);
      if xmsresult=0 then XmsWrite(cc_hand,cc^,0,sizeof(cc^));
 {*}  if not xms_ok then exit;
      ccm_size:=sizeof(ccm^);
      ccm_hand:=XmsAlloc(sizeof(ccm^) div 1024 +1);
      if xmsresult=0 then XmsWrite(ccm_hand,ccm^,0,sizeof(ccm^));
 {*}  if not xms_ok then exit;
 (*     ma_size:=sizeof(marked^);
      ma_hand:=XmsAlloc(sizeof(marked^) div 1024 +1);
      if xmsresult=0 then XmsWrite(ma_hand,marked^,0,sizeof(marked^));
 {*}  if not xms_ok then exit; *)
      XmsStored:=true;
      dispose(ccm); dispose(cc); (* dispose(marked); *)
      end;
  end;

  procedure get_arrays;       { Arrays neu Anlegen und aus XMS einlesen }
  begin
    if XmsStored then begin
      (* new(marked); *) new(cc); new(ccm);
      XmsRead(cc_hand,cc^,0,cc_size);
      XmsRead(ccm_hand,ccm^,0,ccm_size);
      (* XmsRead(ma_hand,marked^,0,ma_size);
      XmsFree(ma_hand); *)
      XmsFree(ccm_hand);
      XmsFree(cc_hand);
      end;
  end;

begin
  edpush:=not editvollbild and
     ((exteditor=1) or (VarEditor='') or (VarEditor[1]='*'));
  if edpush then begin
    attrtxt(col.coledithead);
    moff;
{$IFDEF NCRT }
    { Wegen der Fensterbehandlung wpush auf den gesamten Bereich anwenden }
    wpush(1,GetScreenCols,1,GetScreenLines,'-');
{$ELSE }
    wpush(1,80,1,2,'-');         { 'Nachricht an  ' / 'Nachricht in  ' }
{$ENDIF }
    p:=cpos('@',empfaenger);
    wrt(1,1,' ');
    if verteiler then Wrt2(forms(getres2(611,40)+vert_name(empfaenger),79))
    else
      if pm then Wrt2(forms(getres2(611,40)+left(empfaenger,p-1)+' @ '+
                       mid(empfaenger,p+1),70)+sp(9))
      else Wrt2(forms(getres2(611,41)+copy(empfaenger,edis,55)+
                 iifs(ntBrettEmpf(netztyp) and (fidoto<>''),
                      getres2(611,43)+fidoto,''),70)+sp(9));
    wrt(1,2,' '+forms(getres2(611,42)+betreff,79));   { 'Betreff:      ' }
    mon;
    end;
  if pushpgdn then pushkey(keycpgd);
  if exteditor<3 then EditSetBetreff(betreff,betrlen);
  store_arrays;
  editfile(datei,true,
           (sendFlags and SendReedit<>0) or (filetime(datei)<>orgftime),
           iif(editvollbild,0,2),umlaute=1);
  get_arrays;
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
  var p : byte;
      size :word;
      temp :string[90];
  begin
    with ccm^[n] do begin
      ccpm:=(cpos('@',adr)>0);
      if ccpm then begin
        dbSeek(ubase,uiName,ustr(adr));
        if dbFound then begin
          size := 0;
          if dbXsize (ubase, 'adresse') <> 0 then
          begin
            dbReadX (ubase, 'adresse', size, temp);
            dbSeek (ubase, uiName, ustr (temp));
            if dbFound then
            begin
{              if dbreadint(ubase,'adrbuch')=0 then   }   { CC-Empfaenger ins Adressbuch aufnehmen }
{                dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);}
              dbReadN(ubase,ub_pollbox,server);
              if (dbReadInt(ubase,'userflags') and 2<>0) and
                 (dbReadInt(ubase,'codierer')<>0) then
                encode:=true;
            end;
          end else
          begin
{            if dbreadint(ubase,'adrbuch')=0 then    }  { CC-Empfaenger ins Adressbuch aufnehmen }
{              dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);}
            dbReadN(ubase,ub_pollbox,server);
            if (dbReadInt(ubase,'userflags') and 2<>0) and
               (dbReadInt(ubase,'codierer')<>0) then
              encode:=true;
          end;
        end;
      end
      else begin
        p:=cpos(':',adr);
        if (adr[1]='+') and (p>0) then begin    { nicht eingetragenes Brett }
          server:=copy(adr,2,p-2);
          nobrett:=true;
          end
        else begin
          if adr[1]='/' then dbSeek(bbase,biBrett,'A'+ustr(adr))
          else dbSeek(bbase,biBrett,ustr(adr));
          if dbFound then dbReadN(bbase,bb_pollbox,server)
          else if CrosspostBox<>'' then begin
            adr:='+'+CrosspostBox+':'+adr;
            server:=ustr(CrosspostBox);
            nobrett:=true;
            end;
          end;
        end;
      UpString(server);
      end;
  end;

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
    while (p1<=cc_anz) and (ccm^[p1].server=s1) do inc(p1);
    p2:=p1;
    while p1<=cc_anz do begin
      while (p1<=cc_anz) and (ccm^[p1].server<>s1) do inc(p1);
      if p1<=cc_anz then begin
        cmr:=ccm^[p1];
        Move(ccm^[p2],ccm^[p2+1],(p1-p2)*sizeof(cmr));
        ccm^[p2]:=cmr;
        s:=cc^[p1];
        Move(cc^[p2],cc^[p2+1],(p1-p2)*sizeof(cc^[1]));
        cc^[p2]:=s;
        inc(p1); inc(p2);
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
        { !! Char(Byte(x)) ist eine gro·e Schweinerei, evtl. mal Ñndern }
        IndexStr:=char(byte(encode))+forms(server,BoxNameLen)+char(byte(ccpm));
    end;
  begin
    repeat
      xch:=false;
      for i:=cc_anz downto first+1 do
        if IndexStr(i)<IndexStr(i-1) then begin
          cmr:=ccm^[i]; ccm^[i]:=ccm^[i-1]; ccm^[i-1]:=cmr;
          s:=cc^[i]; cc^[i]:=cc^[i-1]; cc^[i-1]:=s;
          xch:=true;
          end;
      inc(first);
    until not xch;
  end;

  procedure ReadServerNTs;
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
              rfehler1(632,strs(MaxXposts));   { 'Es sind maximal %s BrettempfÑnger pro Server mîglich.' }
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
var p : empfnodep;
begin
  while sendempflist<>nil do begin
    if cc_anz<maxcc then begin
      inc(cc_anz);
      cc^[cc_anz]:=sendempflist^.empf;
      end;
    p:=sendempflist^.next;
    dispose(sendempflist);
    sendempflist:=p;
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
    if ccm^[i].nobrett and (ccm^[i].server=ustr(oldbox)) then begin
      ccm^[i].server:=ustr(newbox);
      cc^[i]:='+'+newbox+mid(cc^[i],cpos(':',cc^[i]));
      modi:=true;
      end
    else if ccm^[i].ccnt=newnt then begin
      ccm^[i].server:=ustr(newbox);
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
    mwrt(x+70,y+14,' [F2] ');
    if not pm and (Netztyp=nt_fido) then y:=y-2;   {Zeile fuer Fidoempf beachten}
    openmask(x+13,x+13+51+2,y+4,y+4,false);
    maskrahmen(0,0,0,0,0);
    maddstring(1,1,'',adresse,52,adrlen,'');
    mappcustomsel(scr_auto_empfsel,false);
    testmailstring_nt:=255;   { Adressschreibweisen aller Netztypen erlauben }
    msetvfunc(testmailstring);
    sel_verteiler:=true;
    readmask(brk);
    closemask;
    attrtxt(col.coldiahigh);
    mwrt(x+13,y+4,' '+forms(adresse,53)+'   ');
    if (adresse<>'') and (cc_testempf(adresse)) then begin
      if (adresse[1]='[') and (adresse[length(adresse)]=']')
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


  procedure set_name(var absender:string);
  begin
    case ntDomainType(netztyp) of    { s. auch XP4O.CancelMassage! }
      0 : absender:=username+'@'+iifs(aliaspt,pointname,box)+'.ZER';
      1 : absender:=username+'@'+iifs(aliaspt,box,pointname);
      2 : absender:=username+'@'+pointname;
      3 : absender:=username+'@'+box;
      4 : absender:=username+'@'+FidoAbsAdr;
      5 : absender:=username+'@'+iifs(aliaspt,pointname,box)+domain;
      6 : if email<>'' then absender:=email else
            if email<>'' then absender:=email else
              absender:=username+'@'+
               iifs(aliaspt,box+ntServerDomain(box),pointname+domain);
      7 : absender:=username+'@'+box+';'+pointname;
      end;
    if realname <>'' then absender:=absender+'  ('+realname+')';
  end;

  function Samebox:boolean;
  var i : integer;
  ss    : string[BoxNameLen];
  begin
    ss:=ccm^[iif(verteiler,1,0)].server;
    Samebox:=true;
    for i:=1 to cc_anz do
      if ccm^[i].server<>ss then Samebox:=false;
  end;

  Procedure changeabs;                         {Absender der Mail aendern}
  var yy:byte;
      s : string;
  label again;

    procedure Set_Box_Selection;
    var s1: string[adrlen];
        orgbox: string[boxnamelen];
        orgnt : byte;
    begin
      orgbox:=box;
      orgNT:=netztyp;
      dbOpen(d,BoxenFile,1);
      repeat
        dbread(d,'boxname',box);
        loadboxdata;
        if ntAdrCompatible(orgnt,netztyp)
        then begin
          set_name(s1);
          mappsel(false,s1);
          end;
        dbnext(d);
      until dbeof(d);
      box:=orgbox;
      dbSeek(d,boiName,ustr(orgbox));
      loadBoxData;
      dbclose(d);
    end;

  begin
    if not samebox then
    begin
      errsound;
      exit;
      end;
    yy:=y;
    if not pm and (Netztyp=nt_fido) then yy:=yy-2;   {Zeile fuer Fidoempf beachten}
  again:
    s:=force_absender;
    openmask(x+13,x+13+51+2,yy+2,yy+2,false);
    maskrahmen(0,0,0,0,0);
    maddstring(1,1,'',force_absender,52,adrlen,'');
    set_box_selection;
    readmask(brk);
    closemask;
    if brk then force_absender:=s
    else begin
      if force_absender='' then goto again;
      testmailstring_nt:=netztyp;
      if (netztyp<>nt_fido) and (netztyp<>nt_maus)
        then s:=(left(force_absender,cposx(' ',force_absender)-1))
        else s:=trim(force_absender);
      if cpos('@',s)=0 then begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,ustr(s));
        if dbFound
         then dbRead(d,'Langname',s)
         else s:='@\';
        dbclose(d);
        end;
      if not testmailstring(s) then goto again;
      force_absender:=s+'  '+mid(force_absender,cposx('(',force_absender));
      if (cpos('(',force_Absender)=0) and
        ((netztyp<>nt_fido) and (netztyp<>nt_maus))
          then force_absender:=s+'  ('+realname+')';
      end;
    attrtxt(col.coldiahigh);
    mwrt(x+13,yy+2,' '+forms(force_absender,53)+'   ');
   end;


{ ausgelagert, weil Prozedurrumpf zu gro·: }

procedure DisplaySendbox;
var
  ToStr: String;
  ToPos: Integer;
begin
  echomail:=ntEditBrettempf(netztyp) and not pm;
  fadd:=iif(echomail,2,0);
  diabox(78,15+fadd,typ,x,y);
  moff;
  wrt(x+3,y+2,getres2(441,6)+ch);    { 'Absender ' }
  wrt(x+3,y+4,getres2(611,10)+ch);   { 'EmpfÑnger ' }

  ToStr := getres2(611,11); { '^An' }
  ToPos := cpos('^', ToStr);
  fidokey := copy(ToStr, ToPos+1, 1); { (A)n }
  Delete(ToStr, ToPos, 1);

  if echomail then begin
    wrt(x+3,y+6, ToStr);    { 'An' }
    inc(y,2);
    end;
  wrt(x+3,y+6,getres2(611,12));      { 'Betreff' }
  wrt(x+3,y+8,getres2(611,13));      { 'Server'  }
  wrt(x+3,y+10,getres2(611,14));      { 'Grî·e'   }
  wrt(x+42,y+8,getres2(611,15));     { 'Code:'   }
  showcode;
  attrtxt(col.coldialog);
  wrt(x+43,y+10,mid(getres2(611,16),2));    { 'opien:' }
  showcc;
  attrtxt(col.coldiahigh);
  kopkey:=left(getres2(611,16),1);
  wrt(x+42,y+10,kopkey);
  if empfaenger[1]=vert_char then
    wrt(x+14,y+4-fadd,vert_name(copy(empfaenger,edis,52)))
  else
    wrt(x+14,y+4-fadd,left(uucpbrett(empfaenger,edis),52));

  pgpkey:=getres2(611,50);
  if pgpkey='^' then pgpkey:=chr(ord(lastchar(getres2(611,50)))-64);

  if echomail then
  begin
    wrt(x+2+ToPos,y+4,fidokey);            { 'A' }
    wrt(x+14,y+4,fidoto);
  end;
  showbetreff;
  showbox;
  showsize;
  showabsender;
  mon;
  senden:=-1;
  n:=1;                                { SendBox-Abfrage }
  pushhp(68);
  spezial:=false;
end;


procedure WriteHeaderHdr;
var f:text;
begin
  assign(f,temppath+'HEADER.HDR');
  rewrite(f);
  writeln(f,'TYP: ',typ);
  writeln(f,'BOX: ',box);
{  writeln(f,'NETZTYP: ',netztyp); }
  writeln(f,'EMPF: ',copy(empfaenger,2,99));
  writeln(f,'FIDOTO: ',fidoto);
  writeln(f,'BETREFF: ',betreff);
  close(f);
end;

procedure DoSendInit1;
begin
  DoSend:=false;
  parken:=false;
  _verteiler:=false;
  box := '';
  flOhnesig:=false; flLoesch:=false;
end;

procedure DoSendInit2;
begin
  new(f); new(f2);
  new(fn); new(fn2); new(fn3);
  assign(f^,datei);

  sdNope:=(sdata=nil);
  if sdNope then
  begin
    new(sData);
    fillchar(sData^,sizeof(sdata^),0);
  end;

  if sendFlags and sendQuote<>0 then
  begin
    ExtractSetMpdata(qmpdata);
    extract_msg(3,iifs(force_quotemsk<>'',force_quotemsk,QuoteSchab(pm)),
                datei,false,1);
    sdata^.quotestr:=qchar;
    get_xref;
  end else
  begin
    if not exist(datei) then
    begin       { leere Datei anlegen }
      rewrite(f^); close(f^);
    end;
    OrigBox:='';
  end;
end;

procedure DoSendInit3;
begin
  new(passwd);
  new(hdp);

  MakeSignature(signat,sigfile,sigtemp);

  cc_anz:=0; cc_count:=0;
  new(cc); new(ccm);
  fillchar(cc^,sizeof(cc^),0);
  fillchar(ccm^,sizeof(ccm^),0);
  SendDefault:=1;
  verteiler:=false;
  if SendEmpflist<>nil then ReadEmpflist;
  flPGPkey:=(sendflags and SendPGPkey<>0);
  flPGPsig:=(sendflags and SendPGPsig<>0) or PGP_signall;
  flPGPreq:=(sendflags and SendPGPreq<>0);
  flNokop:=(sendflags and SendNokop<>0) or DefaultNokop;
  new(fo); fo^:='';
end;

procedure DoSendInit4;
begin
  passwd^:='';         { Betreffbox true = Betreff nochmal eintippen           }
  empfneu:=false;      { Edit       true = Editor Starten                      }
  docode:=0;           { Sendbox    true = Sendefenster zeigen                 }
  fidoname:='';        { forcebox ''-um Box entsprechend Empfaenger zu waehlen }
  ch:=' ';             {          Ansonsten steht hier die zu benutzende Box   }
  if pm then begin
    fidoto:='';
    dbSeek(ubase,uiName,ustr(empfaenger));
    if dbFound then begin                                 {Empfaenger Bekannt}
      verteiler:=(dbReadInt(ubase,'userflags') and 4<>0);
      if verteiler then _verteiler:=true;
      dbReadN(ubase,ub_pollbox,box);
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
        if (dbXsize(ubase,'adresse')=0) or sdata^.RTAHasSetVertreter then adresse:=''
        else dbReadX(ubase,'adresse',size,adresse);
        _brett:=mbrettd('U',ubase);
        if adresse<>'' then begin
          umlaute:=iif(dbReadInt(ubase,'userflags') and 8=0,0,1);
          empfaenger:=adresse;
          ch:='*';
          dbSeek(ubase,uiName,ustr(empfaenger));
          end;
        if dbFound then begin
          dbReadN(ubase,ub_pollbox,box);   { leider doppelt nîtig :-/ }
          _brett:=mbrettd('U',ubase);
          dbReadN(ubase,ub_codierer,cancode);
          if (cancode<>9) and (dbXsize(ubase,'passwort')=0) then
            cancode:=0
          else begin
            if cancode<>0 then
              if dbReadInt(ubase,'userflags') and 2<>0 then
                docode:=cancode;
            si0:=0;
            dbReadX(ubase,'passwort',si0,passwd^);
            end;
          umlaute:=iif(dbReadInt(ubase,'userflags') and 8=0,0,1);
          end
        else begin
          cancode:=0;
          empfneu:=true;
          end;
        end;
      end
    else begin                                                 { EmpfÑnger unbekannt }
    { 14.02.2000 MH: IBM=0, ASCII=1, ISO=2 }
    if newuseribm then umlaute:=0 { MH: NewUserIBM berÅcksichtigen }
     else umlaute:=1;
      empfneu:=true;
      verteiler:=false;
      if fileserver(empfaenger) or _sendmaps then begin
        box:=mid(empfaenger,cpos('@',empfaenger)+1);
        if cpos('.',box)>0 then box:=left(box,cpos('.',box)-1);
        if not isbox(box) then box:=DefaultBox;
        end
      else
        if forcebox='' then begin         { keine EB .. }
          ch:=' ';             { Antwort auf Brettnachricht an User, der  }
          cancode:=0;          { noch nicht in der ubase steht            }
          if dbBOF(mbase) or dbEOF(mbase) then
            box:=DefaultBox         { /Nachricht/Direkt }
          else begin
            dbReadN(mbase,mb_brett,_brett);
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
                else dbReadN(bbase,bb_pollbox,box);
                if box='' then box:=DefaultBox;  { dÅrfte nicht vorkommen }
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
    dbSeek(bbase,biBrett,ustr(empfaenger));
    if not dbFound then begin
      empfneu:=true;
      if empfaenger[1]='$' then box:=InternBox  { autom. Nachricht an neues Brett }
      else box:=iifs(forcebox<>'',forcebox,DefaultBox);
      grnr:=iif(newbrettgr<>0,newbrettgr,IntGruppe);
      end
    else begin
      dbReadN(bbase,bb_pollbox,box);    { Nachricht an vorhandenes Brett  }
      if (box='') and (empfaenger[1]='$') then
        box:=InternBox;               { /Netzanruf, /Statistik ... }
      dbReadN(bbase,bb_gruppe,grnr);
      _brett:=mbrettd(empfaenger[1],bbase);
      if dbReadInt(bbase,'flags') and 32<>0 then
        dbReadN(bbase,bb_adresse,fidoname);    { Brett-Origin }
    end;
    dbOpen(d,gruppenfile,1);          { max. BrettMsg-Grî·e ermitteln   }
    dbSeek(d,giIntnr,dbLongStr(grnr));
    if not dbFound then maxsize:=0    { dÅrfte nicht vorkommen }
    else dbRead(d,'MsgLimit',maxsize);
    if box='' then grnr:=IntGruppe;
    intern:=(grnr=IntGruppe) or (box='');
    if box='' then box:=DefaultBox;
    if forcebox<>'' then box:=forcebox;
    if binary or not dbFound then umlaute:=0
    else dbRead(d,'umlaute',umlaute);
    if (fidoname='') and dbFound then
      dbRead(d,'Origin',fidoname);
    if dbFound then
      dbRead(d,'Adresse',AltAdr)
    else
      altadr:='';
    dbClose(d);
    edis:=2;
    if not binary then cancode:=-1;  { Rot13 mîglich }
  end;   { of not pm }
end;

begin      {-------- of DoSend ---------}
  force_absender:='';
  DoSendInit1;
  {$IFDEF BP }
  if memavail<20000 then
  begin
    rfehler(605);   { 'zu wenig freier Speicher zum Absenden der Nachricht' }
    goto xexit2;
  end;
  {$ENDIF }

  DoSendInit2;

  if not pm and betreffbox and (left(empfaenger,1)<>'A') then
  begin
    rfehler(606);   { 'Schreiben in dieses Brett nicht mîglich!' }
    disposeempflist(sendempflist);
    goto xexit1;
  end;

  DoSendInit3;

{ Einsprung hier startet ganze Versand-Prozedur von vorne (mit den bestehenden Daten) }
fromstart:

  DoSendInit4;


  dbOpen(d,BoxenFile,1);           { Pollbox + MAPS-Name ÅberprÅfen }
  if box<>'' then begin            { nicht intern.. }
    dbSeek(d,boiName,ustr(box));
    if not dbFound then begin
      dbClose(d);
      rfehler1(607,box);  { 'Unbekannte Serverbox: %s  -  Bitte ÅberprÅfen!' }
      goto xexit;                  { --> unbekannte Pollbox }
    end;
    dbRead(d,'boxname',box);       { Schreibweise korrigieren }
  end else                         { interne Msgs -> Default-Username }
    dbSeek(d,boiName,ustr(DefaultBox));
  LoadBoxData;
  if pm and not XP_ID_PMs then XpID:=false;
  if not pm and not XP_ID_AMs then XpID:=false;
  if pm then
    SetLocalPM;
  dbClose(d);
  flMLoc:=(netztyp=nt_Maus) and stricmp(sData^.distribute,'lokal');
  flMnet:=(netztyp=nt_Maus) and stricmp(sData^.distribute,'mausnet');
  FidoBin:=binary and pm and
           ((netztyp=nt_Fido) or
            ((netztyp=nt_UUCP) and (left(empfaenger,length(uuserver))=uuserver)));
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
    rfehler(609);   { 'In diesem Netz sind leider keine BinÑrnachrichten mîglich :-(' }
    goto xexit;
  end;
  if not ((registriert.non_uucp and (netztyp<>nt_UUCP)) or
          (registriert.uucp and (netztyp=nt_UUCP)) or
          binary or TestXPointID)
     and (pm or not ntForceMailer(netztyp)) then
    XpID:=true;
  if pm and (ustr(left(empfaenger,length(mapsname)))=mapsname) then
    XpID:=false;
  if SendFlags and SendWAB<>0 then XpID:=false;
  { Bei Nachrichten, die mit N/W/O weitergeleitet wurden, darf keine
    XpID gesetzt werden, auch nicht in der unregistrierten Version }
  Set_XP_ID;

  if (netztyp<>nt_Fido) then
    AltAdr:='';
{
  else
    if (altadr<>'') and (cpos('.',altadr)=0) then
      AltAdr:=AltAdr+'.'+pointname;
}

  if pm and not ntEmpfBest(netztyp) then begin
    flEB:=flEB or (left(betreff,length(EmpfBkennung))=EmpfBkennung);
    SetEBkennung;
  end;
  if not fileattach then
    ukstring(betreff);
  typ:=getres2(611,iif(pm,1,iif(grnr=IntGruppe,2,3)));  { 'private Nachricht' / 'interne Nachricht' / 'îffentliche Nachricht' }

  betreff:=left(betreff,betrlen);
  if betreffbox then begin         { Betreff editieren }
    if sendFlags and sendQuote<>0 then typ:=typ+getres2(611,4) else   { ' (Quote)' }
    if binary then typ:=typ+getres2(611,5);   { ' (BinÑr)' }
    fidoam:=ntEditBrettempf(netztyp) and not pm;
    bboxwid:=min(betrlen,54);
    showempfs:=min(cc_anz,15);
    diabox(bboxwid+19,iif(fidoam,9,7)+showempfs,typ,x,y);
    mwrt(x+3,y+2,getres2(611,6)+iifs (ch='*', '*', ''));   { 'EmpfÑnger  ' }
    attrtxt(col.coldiahigh);
    moff;
    if empfaenger[1]=vert_char then
      Wrt2(copy(vert_name(empfaenger),edis,bboxwid))
    else
      Wrt2(left(uucpbrett(empfaenger,edis),bboxwid));
    for ii:=1 to min(showempfs,14) do
      if ccm^[ii].ccpm then
        wrt(x+3+length(getres2(611,6)),y+2+ii,left(cc^[ii],bboxwid))
      else
        wrt(x+3+length(getres2(611,6)),y+2+ii,left(uucpbrett(ohnebox(ii),2),bboxwid));
    if showempfs=15 then
      wrt(x+3+length(getres2(611,6)),y+17,'(...)');
    mon;
    openmask(x+3,x+bboxwid+10,y+showempfs+4,y+showempfs+iif(fidoam,6,4),false);
    oldbetr:=left(betreff,20);
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
      if not pm then rfehler(635);  { 'Nachricht mu· einen Betreff haben' }
      if (pm and not ReadJNesc(getres(618),false,brk)) or   { 'Nachricht ohne Betreff absenden' }
         not pm then goto xexit;
    end;
    if (_bezug<>'') and ntKomkette(netztyp) and
                    (ustr(left(betreff,20))<>ustr(oldbetr)) then begin
      pushhp(1501);
      if not ReadJNesc(getres(617),(left(betreff,5)=left(oldbetr,5)) or   { 'Betreff geÑndert - Verkettung beibehalten' }
             ((cpos('(',oldbetr)=0) and (cpos('(',betreff)>0)),brk) then
      begin
        _bezug:='';
        _orgref:='';
        DisposeReflist(_ref6list);
      end else
        { betreff:=left(betreff+' ('+getres(619)+': '+oldbetr,betrlen-1)+')'} ;
      pophp;
      if brk then goto xexit;
    end;
    if pm and not ntEmpfBest(netztyp) then begin
      flEB:=(left(betreff,length(EmpfBkennung))=EmpfBkennung);
      SetEBkennung;
    end;
  end;

  orgftime:=filetime(datei);
  if edit then begin
    WriteHeaderHdr;
    EditNachricht(pgdown);
  end;
  if not getsize then goto xexit;        { --> Nachrichten-Grî·e 0 }
  calc_hdsize;

  echomail:=ntEditBrettempf(netztyp) and not pm;

  if sendbox then
  repeat
    set_name(force_absender);
    DisplaySendbox;                         { SendBox aufbauen }
    repeat
      if pm then intern:=false
      else intern:=(grnr=IntGruppe);
      ShowFlags;
      if spezial then begin
        spezial:=false;
        attrtxt(col.coldialog);
        mwrt(x+1,y+13,sp(76)); { 05.02.2000 MH: 67 -> 76 f. ZurÅck }
      end;
    ReadAgain:
      n:=1;
      ShowLine(spezial);
      if spezial then begin
        case netztyp of     { '^Parken,^Datum, ^EB ,o^hneSig,l^îschen,' }
          nt_Fido     : sendbutt:=getres2(611,20);  { 'C^rash,P^GP'     }
          nt_Maus     : sendbutt:=getres2(611,21);  { '^MausNet,^Lokal' }
          nt_ZConnect : sendbutt:=getres2(611,22);  { 'P^rio,P^GP'      }
          nt_UUCP     : sendbutt:=getres2(611,23);  { 'Z^usatz,P^GP'    }
          else          sendbutt:=getres2(611,24);  { '^ZurÅck'         }
        end;
        repeat
          t:='*';
          n:=readbutton(x+3,y+13,1,sendbutt,
                        abs(n),true,t);
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        case netztyp of
          nt_Fido : if n=7 then n:=11        { PGP         }
                    else if n=8 then n:=0;   { MH: ZurÅck  }
          nt_Maus : if n=8 then n:=0         { ZurÅck      }
                    else if n>5 then inc(n);
          nt_ZConnect :  if n=6 then n:=9    { Prio        }
                    else if n=7 then n:=10   { Zusatz      }
                    else if n=8 then n:=11   { PGP         }
                    else if n=9 then n:=0;   { MH: ZurÅck  }
          nt_UUCP : if n=6 then n:=12        { MH: RFC-Prio}
                      else if n=7 then n:=10 { Zusatz      }
                      else if n=8 then n:=11 { MH: PGP-Sig }
                      else if n=9 then n:=0; { ZurÅck      }
          else      if n=6 then n:=0;        { ZurÅck      }
        end; { 05.02.2000 MH: ZurÅck-Button in allen Netztypen }
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
          n:=readbutton(x+3,y+13,1,getres2(611,28)+
               iifs(binary or (sendflags and sendWAB<>0),'',getres2(611,29)),
                        abs(n),true,t);
           { ' ^Ja ,^Nein,^Intern,^Spezial,˘2^Betreff,B^ox,^Code' ',^Text' }
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        if n=4 then begin
          spezial:=true;
          attrtxt(col.coldialog);
          mwrt(x+1,y+13,sp(76)); { 05.02.2000 MH: 68 -> 76 f. ZurÅck }
          goto ReadAgain;
          end;

        if (n=5) or (t='/') then    { Empfaenger aendern? }
        begin
          Changeempf;
          betreffbox:=false; edit:=false; sendbox:=true;
          SendDefault:=senden; forcebox:='';
          pophp;
          closebox;
          goto fromstart;
          end
        else if n>5 then dec(n); { Ansonsten eins zurueckzaehlen fuer alte Keys}

        if n<0 then begin
          p:=pos(UpCase(t[1]),getres2(611,30));   { PDEHôRMLG }
          case p of
            1..5 : n:=p+10;
            6    : if netztyp=nt_Fido then n:=16 else
                   if netztyp=nt_ZConnect then n:=19 else
                   if netztyp=nt_uucp then n:=22;
            7    : if netztyp=nt_Maus then n:=17;
            8    : if netztyp=nt_Maus then n:=18;
            9    : if netztyp in [nt_ZConnect,nt_UUCP] then n:=20;
            10   : if netztyp in [nt_ZConnect,nt_Fido,nt_Maus,nt_UUCP] then
                     n:=21;  { PGP }
            else   if ntBCC(netztyp) and (t=^K) then
                     flNokop:=not flNokop;
          end;
          end;
        end;
      case n of
        0   : if SaveUVS and not binary then senden:=3   { Abbruch }
              else if sdata^.uv_edit then senden:=iif(developer,0,1)
              else senden:=0;
        1   : if not pm or (cc_anz>0) or not EmpfError then
                if (OverSize=0) or (msgprio>0) or
                   (_errsound and ReadJN(getreps2(612,1,strs(OverSize)),false))
                then senden:=1;
                  { 'Nachrichtenlimit um %s Bytes Åberschritten! Trotzdem absenden' }
        2   : senden:=0;   { Nein   }
        3   : senden:=2;   { Intern }
        5   : if FileAttach then
                rfehler(610)   { 'Betreff kann nicht geÑndert werden' }
              else begin
                { neuer Betreff }
                readstring(x+13,y+6,'',betreff,min(betrlen,52),betrlen,'',brk);
                betreff:=trim(betreff);
                if umlauttest(betreff) then;
                showbetreff;
                n:=1;
              end;
        6   : if intern then
                rfehler(611)   { 'nicht mîglich - interne Nachricht' }
              else if IncompatibleNTs then
                rfehler(629)   { 'nicht mîglich - unterschiedliche Netztypen' }
              else begin                        { neue Pollbox }
                newbox:=UniSel(1,false,box);
                if newbox<>'' then
                  if not pm and (cc_anz=0) and ntBrettebene(netztyp) and
                     ntBrettebene(ntBoxNetztyp(newbox)) and
                     not stricmp(BoxBrettebene(box),BoxBrettebene(newbox)) then
                    rfehler(637)   { 'ServerÑnderung nicht mîglich - abweichende Brettebene!' }
                  else begin
                    dbOpen(d,BoxenFile,1);
                    dbSeek(d,boiName,ustr(newbox));
                    if binary and not ntBinary(dbReadInt(d,'netztyp')) then
                      rfehler(609)  { 'In diesem Netz sind leider keine BinÑrnachrichten mîglich :-(' }
                    else if (((not pm) and (netztyp<>dbReadInt(d,'netztyp'))) or
                     not ntAdrCompatible(netztyp,dbReadInt(d,'netztyp'))) then
                      rfehler(629)   { 'nicht mîglich - unterschiedliche Netztypen' }
                    else begin
                      KorrPhantomServers(box,newbox,dbReadInt(d,'netztyp'));
                      box:=newbox;
                      oldnt:=netztyp;
                      sData^.pmreplyto:='';
                      LoadBoxData;
                      if (netztyp=nt_Fido)<>(oldnt=nt_Fido) then
                        senden:=5;
                      if pm then SetLocalPM;
                      showsize;
                      if cc_anz>0 then forcebox:=box;
                      showbox;
                      if netztyp<>nt_Fido then
                        flCrash:=false;
                      set_name(force_absender);
                      showabsender;
                      end;
                  dbClose(d);
                  end;
                n:=1;
              end;
        7   : if cancode<>0 then begin          { Codierung Ñndern }
                if docode=0 then docode:=cancode
                else docode:=0;
                showcode;
                n:=1;
              end;
        8   : if not binary and (sendflags and sendWAB=0) then begin
                editnachricht(false);              { zurÅck zum Editor }
                if not getsize then begin
                  closebox; goto xexit; end;    { -> Nachrichtengrî·e 0 }
                showbetreff;
                showsize;
                n:=1;
              end;
       11   : if binary then rfehler(612)   { 'Bei BinÑrnachrichten nicht mîglich.' }
              else
                {if cc_anz>0 then rfehler(613)   { 'Bei mehreren Kopien nicht mîglich.' }
                {else} senden:=3;   { Parken }
       12   : if cc_anz>0 then    { Datum }
                rfehler(613)
              else begin
                if DateSend then senden:=4;     { zeitversetzt absenden }
                n:=1;
                end;
       13   : if not pm then
                rfehler(614)   { 'EmpfangsbestÑtigung nur bei PMs mîglich' }
              else begin
                flEB:=not flEB;
                if not ntEmpfbest(netztyp) then
                  SetEBkennung;
                showbetreff;
                end;
       14   : begin
                flOhnesig:=not flOhnesig;
                calc_hdsize;
                showsize;
              end;
       15   : flLoesch:=not flLoesch;
       16   : if pm then
                if flCrash or (not flCrash and FidoAdrOK(true)) then
                  flCrash:=not flCrash
                else
              else
                rfehler(615);   { 'nur bei PMs mîglich' }
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
       22   : begin                    { 06.02.2000 MH: RFC-Priority }
               if netztyp<>nt_uucp then rfehler(622);
                getprio;
               showflags;
              end

      else    if n<0 then begin
                n:=abs(n);

                if t=keyaltA then changeabs; { Absender aendern }

                if ustr(t)=kopkey then begin
                  old_cca:=cc_anz;
                  sel_verteiler:=true;           { im Kopien-Dialog sind Verteiler erlaubt }
                  edit_cc(cc,cc_anz,brk);
                  sel_verteiler:=false;
                  if (old_cca=0) and (cc_anz>0) then forcebox:='';
                  if cc_anz>0 then TestXpostings(true);
                  showcc;
                  showbox;   { evtl. in Klammern }
                  end;
                if echomail and (ustr(t)=fidokey) then begin
                  readstring(x+13,y+4,'',fidoto,35,35,'',brk);
                  attrtxt(col.coldiahigh);
                  mwrt(x+13,y+4,' '+forms(fidoto,35)+' ');
                  end;
                end;
      end;
    until senden>=0;
    pophp;
    closebox;

    case senden of
      0 : goto xexit;              { Abbruch }
      2 : intern:=true;            { nicht in Puffer + kein unversandt }
      3 : begin                    { Nachricht nach /ØUnversandt }
            ParkMsg;               { ## OriginalempfÑnger einfÅgen }
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

  if not samebox then force_absender:='';

  if pm then fidoto:=''
  else
    case netztyp of
      nt_Fido,
      nt_QWK      : fidoto:=fidoto;
      nt_Magic,
      nt_Pronet,
      nt_UUCP,
      nt_ZConnect : if (fidoto=brettalle) or (blankpos(fidoto)=0) then
                      fidoto:='';
    else
      fidoto:='';
    end;

  RemovePhantomServers;

  if not verteiler then begin
    newbin:=binary or (docode=1) or (docode=2);
    pmc_code:=(docode>=3) and (docode<=2+maxpmc);
    SendMbox;
    DoSend:=true;

    if empfneu then
      if pm then begin
        dbAppend(ubase);                        { neuen User anlegen }
        dbWriteN(ubase,ub_username,empfaenger);
        dbWriteN(ubase,ub_pollbox,box);
        halten:=stduhaltezeit;
        dbWriteN(ubase,ub_haltezeit,halten);
        b:=1+iif(newuseribm,0,8);;
        dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
        dbWriteN(ubase,ub_userflags,b);      { aufnehmen }
        dbFlushClose(ubase);
        _brett:=mbrettd('U',ubase);
        end
      else begin
        dbAppend(bbase);                        { neues Brett anlegen }
        dbWriteN(bbase,bb_brettname,empfaenger);
        wbox:=iifs(empfaenger[1]='$','',box);
        intern:=intern or (wbox='');
        dbWriteN(bbase,bb_pollbox,wbox);
        halten:=stdhaltezeit;
        dbWriteN(bbase,bb_haltezeit,halten);
        dbWriteN(bbase,bb_gruppe,grnr);
        b:=iif(netztyp=nt_UUCP,16,0);
        dbWriteN(bbase,bb_flags,b);
        SetBrettindex;
        _brett:=mbrettd(empfaenger[1],bbase);
        end
    else
      if pm then begin
        dbSeek(ubase,uiName,ustr(empfaenger));
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

    { --- 1. Schritt: Nachrichten-Inhalt erzeugen ---------------------- }

    betreff:=left(betreff,betrlen);
    if binary then
      fn^:=datei
    else
      fn^:=TempS(system.round((_filesize(datei)+addsize+2000)*1.5));
    assign(f2^,datei);
    iso:=not binary and ntOptISO(netztyp) and zc_iso and (grnr<>IntGruppe);
    if not binary then begin
      assign(f2^,fn^);
      rewrite(f2^,1);
      if header<>'' then begin           { Header }
        assign(f^,header);
        AppendFile(docode,0,iso);
        end;
      assign(f^,datei);                   { Text }
      AppendFile(docode,0,iso);
      if not flOhnesig and (sigfile<>'') then begin       { Signatur }
        assign(f^,sigfile);
        AppendFile(docode,0,iso);
        end;
      fo^:=fido_origin(false);
      if fo^<>'' then
        wrs(fo^)
      else
        if XpID then                       { ID }
          blockwrite(f2^,XID[1],length(XID));
      close(f2^);
      end;

    { --- 2. Schritt: Nachricht in mbase/MPUFFER ablegen --------------- }

    bin_msg:=binary and (maxbinsave>0) and (fs>maxbinsave*1024);
    if not bin_msg then
      assign(f^,fn^)
    else begin
      assign(f2^,TempPath+'binmsg');
      rewrite(f2^,1);
      wrs('');
      wrs(getres2(612,2));   { 'BinÑrdatei verschickt' }
      wrs('');
      wrs(getreps2(612,3,ustr(datei)));   { 'Dateiname: %s' }
      wrs(getreps2(612,4,strs(fs)));      { 'Grî·e    : %s Bytes' }
      close(f2^);
      assign(f^,TempPath+'binmsg');
      end;
    fillchar(hdp^,sizeof(hdp^),0);
    hdp^.netztyp:=netztyp;
    if ntZConnect(netztyp) then begin
      if pm then
        hdp^.empfaenger:=empfaenger            { PM }
      else if empfaenger[1]<>'1' then
        hdp^.empfaenger:=mid(empfaenger,2)     { normale AM }
      else begin
        hdp^.empfaenger:=mid(empfaenger,3);    { interne PM-Brett-Nachricht }
        p:=cpos('/',hdp^.empfaenger);
        if p=0 then hdp^.empfaenger:=hdp^.empfaenger+'@'+box
        else hdp^.empfaenger[p]:='@';
        end;
      if pm then hdp^.archive:=true;
      end
    else
      hdp^.empfaenger:=iifs(pm,TO_ID+empfaenger,mid(empfaenger,2));
    hdp^.betreff:=betreff;
    case ntDomainType(netztyp) of    { s. auch XP3O.CancelMessage! }
      0 : hdp^.absender:=username+'@'+iifs(aliaspt,pointname,box)+'.ZER';
      1 : begin
            hdp^.absender:=username+'@'+iifs(aliaspt,box,pointname);
            if not aliaspt then hdp^.real_box:=box;
          end;
      2 : hdp^.absender:=username+'@'+pointname;
      3 : hdp^.absender:=username+'@'+box;
      4 : hdp^.absender:=username+'@'+FidoAbsAdr;
      5 : hdp^.absender:=username+'@'+iifs(aliaspt,pointname,box)+domain;
      6 : begin
            if email<>'' then hdp^.absender:=email else
              hdp^.absender:=username+'@'+
               iifs(aliaspt,box+ntServerDomain(box),pointname+domain);
            hdp^.real_box:=box;
          end;
      7 : begin
            hdp^.absender:=username+'@'+box+';'+pointname;
            hdp^.real_box:=box;
          end;
    end;
    if (force_absender='') or (hdp^.absender=force_absender)
      then hdp^.realname:=realname
    else begin
       hdp^.absender:=(left(force_absender,cposx(' ',force_absender)-1));
       n:=cpos('(',force_absender);
       if n>0 then begin
         hdp^.realname:=mid(force_absender,n+1);
         n:=cposx(')',hdp^.realname);
         hdp^.realname:=left(hdp^.realname,n-1);
         end;
       end;

    if (sendFlags and sendWAB<>0) and ntAdrCompatible(sData^.onetztyp,netztyp)
    then begin
      hdp^.wab:=hdp^.absender; hdp^.war:=hdp^.realname;
      hdp^.absender:=sData^.oab; hdp^.realname:=sData^.oar;
      { sData^.oab:=''; }
      end;

    if netztyp=nt_Magic then
      hdp^.hd_point:=pointname;
    hdp^.PmReplyTo:=sData^.PmReplyTo;
    if not pm then
      hdp^.AmReplyTo:=sData^.AmReplyTo;
    hdp^.Keywords:=sData^.keywords;
    hdp^.Summary:=sData^.summary;
    if  ntAdrCompatible(sData^.onetztyp,netztyp)
    then begin
      if sendFlags and sendWAB=0 then begin
        hdp^.oab:=sData^.oab; hdp^.oar:=sData^.oar;
        end;
      hdp^.oem:=sData^.oem;
      end;
    if ustr(sData^.ReplyGroup)<>ustr(mid(empfaenger,2)) then
      hdp^.ReplyGroup:=sData^.ReplyGroup;
    if not pm then
      hdp^.distribution:=sData^.distribute;
    hdp^.quotestring:=sData^.quotestr;
    sendedat:=ixdat(zdate);
    hdp^.datum:=iifs(ReplaceEtime,left(zdate,6)+'0000',zdate);
    case netztyp of
      nt_Magic  : hdp^.pfad:=box;
      nt_Quick,
      nt_GS     : hdp^.pfad:=pointname;
      nt_Pronet : hdp^.pfad:=box {+';'+pointname};
      nt_UUCP   : hdp^.pfad:=iifs(aliaspt,username,pointname+domain+'!'+username);
    else
      hdp^.pfad:='';
    end;
    dbAppend(mbase);            { neue mbase.INT_NR fÅr MessageID }
    hdp^.msgid:=MessageID;
    sData^.msgid:=hdp^.msgid;
    if (_beznet>=0) and ntMIDCompatible(_beznet,netztyp) then
      hdp^.ref:=_bezug;
    if ntOrigID(netztyp) and ntMIDCompatible(_Beznet,netztyp) then
      hdp^.org_xref:=_orgref;
    hdp^.replypath:=_replypath;
    hdp^.typ:=iifs(binary,'B','T');
(*    if (netztyp<>nt_Fido) or pm {or not XP_ID_AMs} then *)
      hdp^.programm:=xp_xp+' '+verstr+Trim(betastr)
                     {$IFDEF Snapshot} + '@' + compiletime {$ENDIF}
                     +iifs(registriert.r2,' '+KomOrgReg+'R/'+
                            registriert.tc+strs(registriert.nr),'');
    hdp^.organisation:=orga^;
    if sdata^.ersetzt<>''then hdp^.ersetzt:=sdata^.ersetzt;
    if (pm and ntPMTeleData(netztyp)) or (not pm and ntAMTeleData(netztyp))
    then begin
      hdp^.postanschrift:=postadresse^;
      hdp^.telefon:=telefonnr^;
      hdp^.homepage:=wwwHomepage^;
      end
    else if (netztyp=nt_UUCP) and not adrpmonly then
      hdp^.homepage:=wwwHomepage^;
    hdp^.priority:=rfcprio;      { 6.2.2000 MH: X-Priority: }
    hdp^.xnoarchive:=noarchive;  {!MH: X-NoArchive: Yes }
    hdp^.datei:=sendfilename;
    hdp^.ddatum:=sendfiledate;
    if FidoTo<>'' then
      hdp^.fido_to:=fidoto
    else
      if not pm and (netztyp in [nt_Fido,nt_QWK]) then hdp^.fido_to:=brettalle;
    hdp^.attrib:=iif(pm and flEB,attrReqEB,0);
    if IsEbest then with hdp^ do begin
      attrib := attrib and (not attrReqEB) + attrIsEB;
      if (netztyp=nt_UUCP) and boxpar^.EB_Daemon then begin
        if pmReplyTo='' then pmReplyTo:=absender;
        absender:='MAILER-DAEMON'+mid(absender,cpos('@',absender));
        if (realname<>'') and (length(realname)<=31) then begin
          realname:=realname+'''';
          if not (realname[length(realname)-1] in ['s','z','·']) then
            realname:=realname+'s';
          realname:=realname+' Mailer'
          end;
        end;
      end;
    if FileAttach then inc(hdp^.attrib,attrFile);
    if netztyp=nt_Maus then
      if flQTo then inc(hdp^.attrib,AttrQuoteTo);
    if ntPmReply(netztyp) then
      if _pmReply then inc(hdp^.attrib,AttrPmReply);
    if ControlMsg then inc(hdp^.attrib,AttrControl);
    if (binary and (netztyp=nt_UUCP) and multipartbin) or
       (binary and (netztyp=nt_Maus) and mausmpbin) then
      inc(hdp^.attrib,AttrMPbin);
    if flPGPkey then
      inc(hdp^.pgpflags,fPGP_haskey);
    if flPGPreq then
      inc(hdp^.pgpflags,fPGP_request);
    if UsePGP and not flPGPkey and ntPGP(netztyp) then begin
      if not exist(PGPkeyfile) then UpdateKeyfile;
      if exist(PGPkeyfile) then
        inc(hdp^.pgpflags,fPGP_avail);
      end;
    hdp^.prio:=msgprio;
    hdp^.nokop:=flNokop;
    if umlaute=0 then
      case netztyp of
        nt_UUCP   : if FileContainsUmlaut then
                      hdp^.x_charset:='ISO-8859-1';
        nt_Fido   : hdp^.x_charset:='IBMPC 2';   { s. FSC-0054, grmpf }
      end;
    if iso then
      hdp^.charset:='ISO1';
    if assigned(sData^.orghdp) then
      with sData^.orghdp^ do begin
        { hdp^.zdatum:=zdatum; hdp^.orgdate:=true;  !! Unversandt/* !! }
        hdp^.organisation:=organisation;
        hdp^.pmreplyto:=pmreplyto;
        hdp^.datei:=datei; hdp^.ddatum:=ddatum;
        end;
    if _sendmaps then
      hdp^.pmreplyto:='';
    SetXpointCtl;
    if cc_anz=0 then     { Anzahl der Crossposting-EMPS ermitteln }
      msgCPanz:=0
    else
      msgCPanz:=ccm^[0].cpanz;
    msgCPpos:=0;

    fm_ro;
    reset(f^,1);
    fm_rw;
    hdp^.groesse:=filesize(f^);
    fn2^:=TempS(hdp^.groesse+4000);
    assign(f2^,fn2^);
    rewrite(f2^,1);
    for ii:=1 to msgCPanz-1 do
      AddToEmpflist(cc^[ii]);
    WriteHeader(hdp^,f2^,_ref6list);
{    hdsize:=filepos(f2^); }
    fmove(f^,f2^);
    close(f^);
    close(f2^);

    repeat                                   { einzelne Crosspostings in }
      if ntZConnect(netztyp) then begin      { mbase ablegen             }
        b:=10;
        dbWriteN(mbase,mb_ablage,b);
        end;                                 { ansonsten bleibt's bei 0 }
      l:=netztyp;
      if hdp^.ref<>'' then inc(l,$100);
      if FileAttach then inc(l,$200);
      if hdp^.pm_reply then inc(l,$400);
      if (hdp^.wab<>'') or (hdp^.oem<>'') then inc(l,$800);
      if iso then inc(l,$2000);
      if flPGPsig then inc(l,$4000);
      if msgCPanz>0 then begin
        inc(l,longint(msgCPanz) shl 16);
        inc(l,longint(succ(msgCPpos)) shl 24);        { EmpfÑngernummer }
        end;
      dbWriteN(mbase,mb_netztyp,l);
      shortmid:=FormMsgid(hdp^.msgid);
      dbWriteN(mbase,mb_msgid,shortmid);
      dbWriteN(mbase,mb_brett,_brett);
      dbWriteN(mbase,mb_betreff,hdp^.betreff);
      dbWriteN(mbase,mb_absender,hdp^.absender);
      l:=ixdat(hdp^.datum);
      dbWriteN(mbase,mb_origdatum,l);
      dbWriteN(mbase,mb_empfdatum,sendedat);
      dbWriteN(mbase,mb_groesse,hdp^.groesse);
      dbWriteN(mbase,mb_typ,hdp^.typ[1]);
      if ntEditBrettempf(netztyp) then
        dbWriteN(mbase,mb_name,hdp^.fido_to)
      else if ntRealname(netztyp) then
        dbWriteN(mbase,mb_name,hdp^.realname);
      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      if sendFlags and sendHalt<>0 then b:=1
      else if flLoesch then b:=2
      else if not (HaltOwn and (sendbox or _verteiler))
        or (pm and not HaltOwnPM) then b:=0; { Eigene Nachrichten Halten gilt nicht fuer Mails }
      dbWriteN(mbase,mb_halteflags,b);
      if intern then b:=0
      else b:=1;
      if bin_msg then inc(b,2);                  { 2 = BinÑr-Meldung }
      if flCrash and MayCrash then inc(b,16);    { !! Crash-Flag }
      dbWriteN(mbase,mb_unversandt,b);

      dbreadN(mbase,mb_flags,flags);                 { Farb - Flags setzen... }

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

      flags := flags or 256; { diese Mail ist von uns erstellt worden!! }
      dbwriteN(mbase,mb_flags,flags);

      if msgCPpos=0 then begin
        if OldMsgsize<>0 then begin
          dbWriteN(mbase,mb_msgsize,oldmsgsize);
          dbWriteN(mbase,mb_adresse,oldmsgpos);
          oldmsgsize:=0;  { zur Sicherheit.. }
          end;
        Xwrite(fn2^);
        dbReadN(mbase,mb_adresse,m1adr);
        dbReadN(mbase,mb_msgsize,m1msgsize);
        _era(fn2^);
        if bin_msg then
          _era(TempPath+'binmsg');
        end
      else begin
        dbWriteN(mbase,mb_adresse,m1adr);
        dbWriteN(mbase,mb_msgsize,m1msgsize);
        end;

      if (sendFlags and sendMark<>0) and (msgCPpos+1=msgMarkEmpf) then
        msgaddmark;
      AddBezug(hdp^,iif(msgCPanz=0,0,iif(msgCPpos=0,1,2)));
      if cc_anz=0 then dbFlushClose(mbase);
      if not pm and (msgCPpos=0) then begin    { Brettdatum neu setzen }
        dbSeek(bbase,biBrett,ustr(empfaenger));
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
            dbSeek(ubase,uiName,ustr(cc^[msgCPpos]));
            if dbFound then
            begin
              _brett:=mbrettd('U',ubase);
              if dbreadint(ubase,'adrbuch')=0 then      { CC-Empfaenger ins Adressbuch aufnehmen }
                dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
            end;
          end
          else begin
            dbSeek(bbase,biBrett,'A'+ustr(cc^[msgCPpos]));
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

  { if not pm then dbFlushClose(bbase); }


    { --- 3. Schritt: Nachricht in PP ---------------------------------- }

    if not intern then begin
      if (docode=1) or (docode=2) then begin
        SetCryptFlag;
        assign(f^,fn^);
        fm_ro;
        reset(f^,1);
        fm_rw;
        fn2^:=TempS(filesize(f^)+2000);
        assign(f2^,fn2^);
        rewrite(f2^,1);
        passpos:=1;
        case docode of
          1 : encode_file(false,f^,f2^);
          2 : begin
                DES_PW(passwd^);
                encode_file(true,f^,f2^);
              end;
        end;
        close(f^); close(f2^);
        assign(f^,fn2^);
        end
      else
        assign(f^,fn^);

      fm_ro;
      reset(f^,1);
      fm_rw;
      fn3^:=TempS(filesize(f^)+4000);
      assign(f2^,fn3^);
      rewrite(f2^,1);
      hdp^.archive:=false;
      hdp^.empfaenger:=iifs(pm,empfaenger,mid(empfaenger,2));
      b:=cpos('@',hdp^.absender);
      if not ntZConnect(netztyp) then begin
        if nobox and (b>0) then
          TruncStr(hdp^.absender,b-1);
        b:=cpos('@',hdp^.empfaenger);
        if (b>0) and (ustr(mid(hdp^.empfaenger,b+1))=box+'.ZER') then
          hdp^.empfaenger:=left(hdp^.empfaenger,b-1);
        end;
      case docode of
        1 : begin
              hdp^.betreff:=left(QPC_ID+hdp^.betreff,BetreffLen);
              inc(hdp^.attrib,AttrQPC);
            end;
        2 : hdp^.betreff:=left(DES_ID+hdp^.betreff,BetreffLen);
      end;
      hdp^.typ:=iifs(newbin,'B','T');
      hdp^.groesse:=filesize(f^);
      for ii:=1 to msgCPanz-1 do
        AddToEmpflist(cc^[ii]);
      WriteHeader(hdp^,f2^,_ref6list);
      fmove(f^,f2^);
      close(f^); close(f2^);
      if (docode=1) or (docode=2) then
        _era(fn2^);
      if pmc_code then pmCryptFile(hdp^,fn3^) else
      if (docode=9) or flPGPsig then begin
        for ii:=1 to msgCPanz-1 do
          AddToEmpflist(cc^[ii]);
        xp_pgp.PGP_EncodeFile(f^,hdp^,fn3^,passwd^,docode=9,flPGPsig,fo^);
        DisposeEmpflist(empflist);
        end;

      if not flCrash or not MayCrash then
        assign(f2^,boxfile+'.PP')           { ..und ab damit ins Pollpaket }
      else begin
        assign(f2^,CrashFile(hdp^.empfaenger));
        SetCrashInfo;
        end;
      reset(f2^,1);
      if ioresult<>0 then rewrite(f2^,1)
      else seek(f2^,filesize(f2^));
      assign(f^,fn3^);
      fm_ro;
      reset(f^,1);
      fm_rw;
      fmove(f^,f2^);
      close(f^); close(f2^);
      _era(fn3^);

      if uvs_active and (aktdispmode=11) and (cc_count=0) and
         (msgCPanz<=1) then
        MsgAddmark;

      closebox;
      if not noCrash and flCrash and MayCrash and FidoAdrOK(false) and
         ReadJN(getres(615),true) then    { 'Crash sofort absenden' }
        AutoCrash:=CrashAdr;  { EmpfÑnger, evtl. ohne Point }
      end
    else
      closebox;    { "Nachricht abschicken/speichern" }

    if msgCPanz>1 then begin    { cc-EpfÑnger bis auf einen Åberspringen }
      Move(cc^[msgCPanz],cc^[1],(maxcc-msgCPanz+1)*sizeof(cc^[1]));
      Move(ccm^[msgCPanz-1],ccm^[0],(maxcc-msgCPanz+2)*sizeof(ccm^[1]));
      dec(cc_anz,msgCPanz-1); inc(cc_count,msgCPanz-1);
      end;

    if not binary then _era(fn^);
  end;   { not verteiler }

  if cc_anz>0 then begin           { weitere CC-EmpfÑnger bearbeiten }
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

  if FidoBin and exist(datei) and EditAttach then begin
    _era(datei);
    datei:=betreff;
    end;

  aufbau:=true; xaufbau:=true;
  { es mu· jetzt der korrekte Satz in mbase aktuell sein! }
xexit:
  freeres;
  dispose(ccm);
  dispose(cc);
  dispose(passwd);
  dispose(hdp);
  if sigtemp then _era(sigfile);
  dispose(fo);
xexit1:
  dispose(f); dispose(f2);
  dispose(fn); dispose(fn2); dispose(fn3);
  if sdNope then dispose(sdata);
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
  DisposeReflist(_ref6list);
  NewbrettGr:=0;
  oldmsgpos:=0; oldmsgsize:=0;
end; {------ of DoSend -------}


procedure send_file(pm,binary:boolean);
const xp_support = 'A/T-NETZ/SUPPORT/XPOINT';
var empf,repto : string[AdrLen];
    reptoanz   : integer;
    betr,dummy : string[BetreffLen];
    fn         : pathstr;
    hf         : string;
    dir        : dirstr;
    name       : namestr;
    ext        : extstr;
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
   -1..0 : dbReadN(bbase,bb_brettname,empf); { B^inÑr / Text^File an Brett }
    1..4 : dbReadN(ubase,ub_username,empf);  { B^inÑr / Text^File an User }
  10..19 : begin
             dbReadN(mbase,mb_absender,empf);  { ^I/^F an Absender der Msg }
             dbReadN(mbase,mb_betreff,betr);
             ReplyText(betr,false);
           end;
  end;
  empf:=left(empf,79);
  fn:=sendpath+'*.*';
  useclip:=true;
  if readfilename(getres(iif(binary,613,614)),fn,true,useclip)   { 'BinÑrdatei' / 'Textdatei' versenden }
  then
    if binary and (left(empf,length(xp_support))=xp_support) and
       ((left(getfilename(fn),4)='PDZM') or
        (left(getfilename(fn),3)='ZPR')) and not developer then
      fehler('Bitte Åberlassen Sie das Versenden dieses Programms dem Programmautor!')
    else begin
      if not multipos('\:',fn) then fn:=sendpath+fn else
      fn:=FExpand(fn);
      if not exist(fn) then rfehler(616)    { 'Datei nicht vorhanden' }
      else if not FileOK then fehler(getres(102)) { Fehler beim Dateizugriff }
      else (* if _filesize(fn)+MaxHdsize>TempFree then
        rfehler(617)   { 'zu wenig Platz auf TemporÑr-Laufwerk' }
      else *) begin
        fsplit(fn,dir,name,ext);
        if betr='' then betr:=name+ext
        else betr:=left(name+ext+' ('+betr,39)+')';
        new(sData);
        fillchar(sData^,sizeof(sData^),0);
        if aktdispmode in [10..19] then begin
          get_bezug(pm,repto,reptoanz,dummy,sData,false);
          if repto<>'' then empf:=repto;
          end;
        hf:='';
        sendfilename:=ustr(name+ext);
        sendfiledate:=zcfiletime(fn);
        if DoSend(pm,fn,empf,betr,false,binary,true,true,false,sData,hf,hf,0) then;
        dispose(sData);
        end;
      if useclip then _era(fn);
      end;
end;


function SendPMmessage(betreff,fn:string; var box:string):boolean;
var d    : DB;
    empf : string[80];
    s    : string[10];
    l    : longint;
begin
  SendPMmessage:=false;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  if dbFound then empf:='1/'+dbReadStr(d,'username')+iifs(userboxname,'/'+box,'')
  else empf:='';
  dbClose(d);
  if empf<>'' then begin
    InternBox:=box; forcebox:=box;
    s:='';
    if DoSend(false,fn,empf,betreff,
              false,false,false,false,false,nil,s,s,sendIntern+sendShow)
    then begin
      l:=dbReadInt(mbase,'unversandt') or 64;    { interne Nachricht }
      dbWriteN(mbase,mb_unversandt,l);     { -> keine Mausstatus-Abfrage }
      SetUngelesen;
      SendPMmessage:=true;
      end;
    end;
end;

end.
{
  $Log$
  Revision 1.39.2.37  2001/08/23 11:04:04  mk
  - little code optimization (const parameter, MWrt)

  Revision 1.39.2.36  2001/08/12 11:20:35  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.39.2.35  2001/08/11 22:18:00  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.39.2.34  2001/08/05 11:45:35  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.39.2.33  2001/07/23 18:40:09  mk
  - removed nested comments

  Revision 1.39.2.32  2001/07/08 21:32:28  my
  JG:- Fix: <F2> selection of Fido AKAs now works when changing
       the sender with <Alt-A> in the send window

  Revision 1.39.2.31  2001/06/26 20:14:37  my
  JG:- fixed 'change sender' bug: when changing F-TO the input field
       was 2 lines too high.
  JG:- fixed 'change sender' bug: when a public message was cc'd to a user,
       the mail to the user had a wrong sender (first name of realname).
  JG:- fixed ancient 'forcebox' bug: it was possible to e.g. create a mail to
       an RFC recipient and then select a Fido server in the send window.

  Revision 1.39.2.30  2001/06/13 02:10:09  my
  JG/MY:- New Server type "RFC/Client" (formerly "Client Mode"):
          - All vital client settings from Edit/Point, Edit/Names and
            Edit/RFC/UUCP are summarized under one item Edit/Client now.
            Superfluous RFC/UUCP settings have been removed (well, more
            hidden in fact ;)).
          - introduced simplified entry "eMail address" (rather than composing
            it of removed entries user name, point name and domain).
          - new FQDN festures: "@" is replaced with ".", and "_" with "-"
            automatically. <F2> selection now shows the result of the
            proposed FQDN rather than a fixed string. Special T-Online FQDN
            support (".dialin.").
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
          - new unit XP9SEL as unit XP9 exceeded 64K size.
  JG/MY:- Server type RFC/UUCP:
          - introduced simplified entry "eMail address". If empty, the entries
            user name, point name and domain are automatically filled with the
            appropriate values taken from this eMail address.
          - re-designed Edit/Point to the "old" stage (removed Client Mode specific
            stuff). Kept new BSMTP options "SMTP/UUCP" and "SMTP/Client".
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
        - Removed superfluous code in connection with the changes above, updated
          and cleaned up resource and help files (still a lot to do for the English
          part).

  Revision 1.39.2.29  2001/06/05 20:33:50  my
  JG:- Fix (DoSend): clear list of CC recipients after rfehler(606)
       ("Internal newsgroup - writing not allowed!"). Ancient bug
       that could lead to "ghost CC recipients", but that did not
       occur anymore - obviously due to some RTA code somewhere.

  Revision 1.39.2.28  2001/04/28 15:47:35  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.39.2.27  2001/03/27 12:27:28  mk
  - ops, fixed last commit

  Revision 1.39.2.26  2001/03/27 08:25:22  mk
  - snapshot datum aus der xp.ovr ermitteln

  Revision 1.39.2.25  2001/01/04 09:25:05  mk
  - eigene Mails in der Datenbank markieren

  Revision 1.39.2.24  2000/12/31 11:54:12  mk
  JG:- eigene PMs halten fix

  Revision 1.39.2.23  2000/12/24 16:51:57  mk
  - header.hdr grosss geschrieben

  Revision 1.39.2.22  2000/12/20 18:47:00  mk
  - Schalter Binaernachrichten fuer ZC entfernt

  Revision 1.39.2.21  2000/12/19 21:24:05  mk
  - Binaernachrichten als Attachments auch fuer ZConnect

  Revision 1.39.2.20  2000/12/17 00:14:46  mk
  - optische Korrekturen an der Speicherstatistik

  Revision 1.39.2.19  2000/12/03 13:03:51  mk
  - Variable box initialisieren

  Revision 1.39.2.18  2000/11/30 14:10:36  mk
  - fixed NewUserIBM when adding new uesers

  Revision 1.39.2.17  2000/11/19 11:12:25  mk
  - fixed Bug #112083: Vertreteradressen blieben bei Boxwechsel erhalten

  Revision 1.39.2.16  2000/10/22 21:50:02  mk
  - .PP gross geschrieben

  Revision 1.39.2.15  2000/10/16 10:19:10  mk
  - Fixes fuer LFN

  Revision 1.39.2.14  2000/10/15 09:28:08  mk
  - LFN fixes

  Revision 1.39.2.13  2000/10/10 22:49:45  mk
  - Unit xp2 gesplittet, um Codegroessengrenzen zu umgehen

  Revision 1.39.2.12  2000/10/10 13:04:55  mk
  RB:- Supersedes in Autoversand

  Revision 1.39.2.11  2000/10/05 23:13:20  mk
  - zu grossen Prozedurrumpf verkleinert

  Revision 1.39.2.10  2000/10/05 20:29:49  fe
  RFC/UUCP: Hostname masquerading / UUCP-Alias-Points repariert:
  Statt "User@Server.domain" jetzt "User@Server.Serverdomain".

  Revision 1.39.2.9  2000/09/30 14:17:26  my
  JG:- Fix: Aufnahme von Usern ins Adreﬂbuch, wenn diese schon
       in der Datenbank sind, auch bei CCs
       (bitte noch f¸r 3.70 nachholen)

  Revision 1.39.2.8  2000/09/12 12:41:59  fe
  1. Kleine Anpassung an Gatebau '97: Fido-To wird nicht mehr in der
     proprietaeren X-XP-FTO-Zeile, sondern in der Standard-Zeile F-TO
     untergebracht.  (X-XP-FTO wird aber weiterhin verarbeitet.)

  2. Kleine Anpassung an Gatebau '97: Fido-To wird auch aus und in
     RFC-Nachrichten konvertiert.  (X-Comment-To)

  3. Auch bei RFC wird bei oeffentlichen Antworten auf Nachrichten mit
     Fido-To eine Fido-To-Zeile erzeugt.  (Kleine Verbesserung fuer Leute,
     die mit RFC-Technik in Fido-Foren schreiben.)

  Revision 1.39.2.7  2000/08/26 07:56:18  jg
  - Config/Optionen/Nachrichten... "Eigene PMs halten" eingebaut

  Revision 1.39.2.6  2000/08/22 14:02:33  mk
  - SendenDefault in Shortint geaendert

  Revision 1.39.2.5  2000/08/16 06:14:30  jg
  - Bugfix: Crashs im Zusammenhang mit dem Editorstart behoben
    (der Speicher von Marked wird nich mehr freigegeben)

  Revision 1.39.2.4  2000/07/31 08:16:15  jg
  - OOPS.. Version mit unbrauchbarem Debugcode hochgeladen...

  Revision 1.39.2.3  2000/07/30 12:51:07  jg
  - Maximale Anzahl Crossposting-Empfaenger auf 126 gesetzt
  - Darstellungsbug beim Crossposting an Fido Bretter behoben
  - 80K mehr Speicher im Editor (3 grosse Arrays im XMS zwischengelagert)

  Revision 1.39.2.2  2000/07/01 09:22:58  mk
  - Mailerstringanpassungen

  Revision 1.39.2.1  2000/06/24 14:16:34  mk
  - 32 Bit Teile entfernt, Fixes

  Revision 1.39  2000/06/19 20:21:46  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.38  2000/06/13 16:57:56  jg
  - Empfaenger-aendern im Sendefenster:
    Bugfix:  Verteiler funktioieren jetzt wieder
    Feature: Bei gedrueckter Shifttaste bleiben die Kopien-Eintraege erhalten

  Revision 1.37  2000/06/10 20:15:11  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.36  2000/06/05 16:41:04  mk
  - Zugriff auf uninitialisiertes sdata verhindert

  Revision 1.35  2000/06/01 21:18:40  mk
  - Resource 611,11 enthaelt jetzt Hotkey

  Revision 1.34  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.33  2000/05/21 07:21:17  jg
  - Empfaenger aendern: [F2] wird jetzt reingemalt

  Revision 1.32  2000/05/17 18:15:51  sv
  - Auch in nicht registrierten Versionen wird keine XP-ID an mit N/W/O
    weitergeleiteten Nachrichten angehaengt

  Revision 1.31  2000/05/14 07:58:13  mk
  - ContainsUmlaut gefixt

  Revision 1.30  2000/05/13 09:14:41  jg
  - Ueberpruefung der Adresseingaben jetzt auch Fido und Maus kompatibel

  Revision 1.29  2000/05/11 18:21:53  jg
  - Compiledatum im Mailer-String von Snapshotversionen ($IFDEF Snapshot)

  Revision 1.28  2000/05/07 10:41:27  hd
  - Linux: Variable Fensterbreite

  Revision 1.27  2000/05/06 17:29:23  mk
  - DOS DPMI32 Portierung

  Revision 1.26  2000/05/05 18:08:50  jg
  - Sendefenster: Verteiler im "Kopien an" Dialog erlaubt
  - Empfaenger aendern Loescht alte "Kopien an" Eintraege

  Revision 1.25  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.24  2000/05/01 17:26:33  jg
  - Verteiler als Empfaenger bei Nachricht/Direkt;  Nachricht/Weiterleiten
    Und Sendefenster-Empfaengeraendern erlaubt

  Revision 1.23  2000/04/29 19:11:51  jg
  - Ueberpruefung der Usernameneingabe bei Nachricht/Direkt, Verteilern
    und "Kopien an" + "Empfaenger aendern" im Sendefenster

  Revision 1.22  2000/04/28 22:30:10  jg
  - Diverse Verbesserungen beim Versenden mit Priority
  - Farbige Hervorhebung auch fuer Zconnect Eil- und Direktmail

  Revision 1.21  2000/04/27 09:45:40  jg
  - C/O/N "Eigene Nachrichten Halten" gilt nicht mehr fuer PMs

  Revision 1.20  2000/04/18 16:17:33  jg
  - Schoenheitsfix: Empfaengeraendern beim Senden mit Lister im Hintergrund
  - Neue Selectroutine scr_auto_select (Sichert Screen und stellt Hauptmenue dar)
  - Ein paar erledigte Sachen aus !Todo.tst geloescht.

  Revision 1.19  2000/04/17 17:24:09  jg
  - Sendefenster: Empfaengeraendern jetzt als richtiger Menuepunkt ("Emp.")
  - xp1input.readbutton: alten Minibug bei Leerzeichen vor Buttons beseitigt.

  Revision 1.18  2000/04/15 21:44:47  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.17  2000/04/15 09:58:00  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.16  2000/04/11 19:34:01  oh
  - [tempdir]\header.hdr fuer Mailnachbearbeitung

  Revision 1.15  2000/04/09 08:01:26  jg
  - Umlaute in Betreffs, werden jetzt (falls verboten) automatisch konvertiert

  Revision 1.14  2000/04/04 21:01:24  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.13  2000/04/01 07:41:38  jg
  - "Q" im Lister schaltet otherquotechars (benutzen von | und :) um.
    neue Einstellung wird dann auch beim Quoten verwendet
  - Hilfe aktualisiert, und Englische Hilfe fuer
    Config/Optionen/Allgemeines auf Stand gebracht.

  - Externe-Viewer (Windows): "START" als Allroundviewer
    funktioniert jetzt auch mit der Loeschbatch-Variante
  - Text fuer MIME-Auswahl in englische Resource eingebaut

  Revision 1.12  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.11  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.10  2000/03/14 15:15:40  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.8  2000/03/07 20:36:03  jg
  - Bugfix: Versand von bereits r/w geoeffneten Dateien fehlermeldung
    statt 0-byte Mails bei Binaerfiles bzw. RTE 200 bei Textfiles
  - DoSend etwas kommentiert

  Revision 1.7  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.6  2000/02/18 21:54:46  jg
  Kurvnamen fÅr UUCP + ZConnect Vertreteradressen

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

{ $Id$

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

{ Nachrichten versenden, weiterleiten, unversandt-bearbeiten }

{$I xpdefine.inc }

unit xpsendmessage;

interface

uses
  sysutils,
  typeform,fileio,inout,keys,datadef,database,maske,crc,lister, osdepend,
  winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,xp2c,xp_des,xpe, xpheader,
  xpglobal,xpsendmessage_attach,xpsendmessage_attach_analyze,xpmime,
  addresslist,xpnt,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
Classes,fidoglob;

//      const sendIntern = 1;     { force Intern              }
//            sendShow   = 2;     { ausfuehrliche Sendeanzeige }
//            sendDelay  = 4;     { 0,5 s Warten              }
//      //    sendQuote  = 8;     { akt. Nachricht quoten     }
//            SendWAB    = 16;    { ABS->WAB, OAB->ABS        }
//            SendReedit = 32;    { TED: Softbreaks umwandeln }
//            SendHalt   = 64;    { Nachricht auf 'halten'    }
//            SendMark   = 128;   { Nachricht markieren       }
//            SendPGPkey = 256;   { PGP-Key-Header erzeugen   }
//            SendPGPreq = 512;   { PGP-Key anfordern         }
//            SendPGPsig = 1024;  { Nachricht signieren       }
//            SendNokop  = 2048;  { STAT: NOKOP               }
//      //    SendIQuote = 4096;  { indirekter Quote          }
//      //    SendMPart  = 8192;  { Multipart zerlegen        }

const
//      pgdown    : boolean = false;
//    _sendmaps : boolean = false;
//    forcebox  : string = '';
//    forceabs  : string = '';       { 'SYSOP' fuer ProNet-System }
//    _bezug    : string = '';
//    _orgref   : string = '';
//    _replypath: string = '';        { Box, ueber die die Bezugsnachr. kam }
//    sendfilename   : string = '';
//    sendfiledate   : string = '';
//    force_quotemsk : string = '';
//    CrosspostBox   : string = '';
//    _beznet   : shortint = -1;         { Netztyp der Bezugsnachricht }
//    _pmReply  : boolean = false;
//    IsEbest   : boolean = false;
      NoCrash   : boolean = false;
//    FileAttach: boolean = false;
//    EditAttach: boolean = true;
//    msgprio   : byte    = 0;           { ZConnect-Prio }
//    rfcprio   : byte    = 0;           { RFC-Priority  }   { 6.2.2000 MH: } { unbedenklich }
//    ControlMsg: boolean = false;
      newbrettgr: longint = 0;           { Gruppe fuer neues Brett }
//    flCrash   : boolean = false;
//    flQTo     : boolean = false;       { Maus: Wildwestverkettung }
//    flNokop   : boolean = false;

      OldMsgSize: longint = 0;{ s. XP3.XWrite }
      OldMsgPos : longint = 0;

var
  SendEmpfList: TStringList;
  qMimePart: TMimePart;

var
      InternBox : string;  { Boxname bei /Netzanruf }
      msgMarkEmpf: byte;   { fuer sendMark }

type
{ ------------------------- } TSendUUData { -------------------------- }
  = class

  private
    FHas     : array [TNetClass,boolean] of boolean;
    FCharsets: array [TNetClass] of TSTringList;
    
    function GetHasPM: boolean;
    function GetHasAM: boolean;
    function GetHasNetz(nc: TNetClass): boolean;
    function GetHasNetzPM(nc: TNetClass; pm: Boolean): boolean;

  private
    FSigData:       string;     { Fertige Signatur                     }
    FSigDataOK:     boolean;    { FSigData ist fertig                  }
    FSigTemplate:   string;     { Vorlage für Signatur (Dateiname)     }

    FHeadData:      string;     { Fertige Signatur                     }
    FHeadDataOK:    boolean;    { FSigData ist fertig                  }
    FHeadTemplate:  string;     { Vorlage für Signatur (Dateiname)     }

    FRealname    : string;      { Roles: Realname                      }
    FMail        : string;      { Roles: E-Mail-Adresse                }
    FReplyTo     : string;      { Roles: Antwort-An                    }
    FFQDN        : string;      { Roles: FQDN                          }

//  FQuoteBrett  : string;      { Brett, aus dem zitiert wurde für Va-
//                                riablen in Signatur/Kopf }

    FUserDataOK:     boolean;        { false: FSigTemplate neu suchen   }
    FUserDataForced: boolean;        { true: FSigTemplate nicht ändern  }

    FForceBox    : string;

    procedure MakeUserdata;
    
    function GetSigTemplate: string;
    procedure SetSigTemplate(NewTmplFile: string);
    function GetSigData: string;

    function GetHeadTemplate: string;
    procedure SetHeadTemplate(NewTmplFile: string);
    function GetHeadData: string;

    function GetReplyTo: string;
    procedure SetReplyTo(const NewValue: string);
    function GetSenderRealname: string;
    procedure SetSenderRealname(const NewValue: string);
    function GetSenderMail: string;
    procedure SetSenderMail(const NewValue: string);
    function GetFQDN: string;
    procedure SetFQDN(const NewValue: string);

    function GetAutoUserData:boolean;
    procedure SetAutoUserData(x: boolean);

    function GetDefaultBox: string;
    procedure SetForceBox(const NewBox: string);

  public
    property Has_AM: boolean read GetHasAM;
    property Has_PM: boolean read GetHasPM;

    property Has_Netz[nc: TNetClass]: Boolean read GetHasNetz;
    property Has[nc:TNetClass; pm: Boolean]: Boolean read GetHasNetzPM;

    property HeadTemplate: string read GetHeadTemplate write SetHeadTemplate;
    property HeadData: string read GetHeadData;
    property SigTemplate: string read GetSigTemplate write SetSigTemplate;
    property SigData: string read GetSigData;

    property ReplyTo: string read GetReplyTo write SetReplyTo;
    property SenderRealname: string read GetSenderRealname write SetSenderRealname;
    property SenderMail: string read GetSenderMail write SetSenderMail;
    property FQDN: string read GetFQDN write SetFQDN;

    property AutoUserData: boolean read GetAutoUserData write SetAutoUserData;

    property DefaultBox: string read GetDefaultBox;
    property ForceBox: string read FForceBox write SetForceBox;

  public 
    EmpfList   : TAddressList; // includes Newsgroups
    
    can_crash: boolean;     { FTN Crash mails allowed (what did you think?!) }
    Boxen    : TStringList;

    maxsize  : longint;     { ab hier muss gesplittet werden      }
    attachMode: TAttachMode;{ erlaubter Attachment-Modus          }

  private
    procedure AddURI_(const URI: string; WithBody: Boolean);

  public
    procedure AddURI (const URI: string);                    
    procedure AddURIWithBody(const URI: string);                    

  private
  { -- Whether to save that charset as-is in the local database ------ }
    class function MIMESaveCharsetAsCP437(const s:string): Boolean;

  { -- MIME Functions ------------------------------------------------ }
    procedure MIMEWriteContent(s1:TStream;pa:TSendAttach_Part;DoEncode:Boolean;const Signature,FidoOrigin:string);
    procedure MIMEWriteContentWithHeaders(s:TStream;pa:TSendAttach_Part;const Signature: string);

  { == Variables from DoSend ========================================= }
  public
    Subject  : string;
    
  public
    DoCode   : integer;
    CanCode  : integer;

  { -- Boxdaten ------------------------------------------------------ }
//  username : string;      { eigener Username                         }
//  aliaspt  : boolean;     { Alias-Point (USER@BOX)              }
    
  { -- Flags --------------------------------------------------------- }
    flOhneSig     : boolean; { Keine Signatur anhängen                 }

    flIntern      : boolean; { force Intern                            }
    flShow        : boolean; { ausfuehrliche Sendeanzeige              }
    flDelay       : boolean; { 0,5 s Warten                            }
    flReedit      : boolean; { TED: Softbreaks umwandeln               }

    flWAB         : boolean; { ABS->WAB, OAB->ABS                      }
    
    flPGPSig      : boolean; { PGP: Signatur erzeugen                  }
    flPGPKey      : boolean; { PGP: eigenen Key mitschicken            }
    flPGPReq      : boolean; { PGP: Key-Request                        }
    
    flControlMsg  : boolean; { ist Kontrollnachricht                   }
    flEB          : boolean; { ist Empfangsbestaetigung                }
//  flNokop       : boolean; { keine Kopien                            }
    flPMReply     : boolean; { keine öffentlichen Antworten            }
    
    flCrash       : boolean; { Fido: Crash-Nachricht                   }
    flCrashAtOnce : boolean; { Fido: keine Rückfrage, sofort versenden }
    flFileAttach  : boolean; { Fido: Datei mitsenden                   }
    
    flQTo         : boolean; { ist Followup in andere Gruppe           }
                             { auch: Maus: Wildwestverkettung          }

    flUngelesen   : boolean; { Nachricht auf 'ungelesen'               }
    flHalt        : boolean; { Nachricht auf 'halten'                  }
    flMark        : boolean; { Nachricht markieren                     }
    flLoesch      : boolean; { Nachricht auf 'gelöscht'                }

    SentOK        : boolean; { Nachricht erfolgreich versendet         }

    procedure AddText(const fn:string; temp:boolean);
    procedure AddFile(const fn:string; temp:boolean; const ctype:string);

    procedure SetMessageContent(const fn:string; temp:boolean; orig_hdp: THeader);

  public
    function EditBetreff(dlg_title: string): boolean;
    procedure EditText;
    procedure EditAttach;
    function SendWindow(dlg_title: string): boolean;
    procedure CreateMessages;           { create & send messages }

    function DoIt(dlg_title: string;EditBetreff, EditText, SendWindow: Boolean): Boolean;
    
  private
    FSignature: String;

  public
    property Signature: string read FSignature write FSignature;    

  { -- Multipart support --------------------------------------------- }
  public
    Parts    : TList;       { Nachrichten-Teile }
    
    PartsEx  : Boolean;	    { Nachrichten-Teile aus Originaldatei extrahiert? }
    PartFile : String;      { Originaldatei }
    PartFTmp : Boolean;     { Originaldatei temporär? }

  { -- Primary Address ----------------------------------------------- }
    
  private
    function GetEmpf1Address:  string; procedure SetEmpf1Address (NewValue: string);
    function GetEmpf1RealName: string; procedure SetEmpf1RealName(NewValue: string);

  public
    property Empf1Address  : string read GetEmpf1Address  write SetEmpf1Address;
    property Empf1RealName : string read GetEmpf1RealName write SetEmpf1RealName;

  public  
    followup   : TStringlist;
    References : TStringList;
    keywords   : string;
    summary    : string;
    distribute : string;
    oab, wab: string;
    OEM: TStringList;
    oar,war    : string;
    onetztyp   : byte;
    orghdp     : THeader;
    orgbox     : string;

    org_mid    : string;        { X-XP-ORGMID }
    org_ref    : string;        { X-XP-ORGREF }

    ReplyGroup : string;        { Maus-QuoteTo }
    Replypath  : string;        { X-XP-MRP }

    FidoTo     : string;

    msgprio   : byte    ;           { ZConnect-Prio }
    rfcprio   : byte    ;           { RFC-Priority  }

    sendfilename   : string;
    sendfiledate   : string;
    
    quotestr   : string;
    UV_edit    : boolean;        { <Esc> -> "J" }
    msgid,
    ersetzt    : string;
    msgidtyp   : byte;    

    RTAHasSetVertreter: Boolean;
    boundary   : string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
   
  public
    procedure MergeCharsets(nt:byte;NewCharsets:TStringList);    
    procedure MergeMsgType(nt:byte;pm:boolean);

  private
    function EditEmpfaengerList(
      const DialogueTitle: String;          // Dialog-Titel
      EditRecipients:   Boolean;            // Empfänger bearbeiten
      EditSubject:      Boolean;            // Betreff bearbeiten
      ShowSubject:      Boolean;            // Betreff überhaupt anzeigen
      PMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für PMs
      AMAllowedNets:    TNetClassSet        // Erlaubte Netztypen für AMs
    ):boolean;

    procedure ComposeEditableList;          // helper function
    procedure UnComposeEditableList;        // helper function

    procedure CheckEmpfaengerList(
      List:             TAddressList;       // Addresse to check
      Prompt:           Boolean;            // Ask user for new addresses
      AutoAdd:          Boolean             // Add addresses to DB
    );

    procedure ClearParts;

    procedure AddFilePart(datei:string;temp:boolean);
    procedure EditNachricht(pushpgdn:boolean);
    procedure MIMEDecompose;
    procedure AddMessagePart(datei:string;temp,is_orig:boolean);
  end;

{ -------------------------------------------------------------------- }  

(*
function DoSend(pm:boolean; __datei:string; is_temp,is_file:boolean;
                __empfaddr,__betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData: TSendUUData;
                signat:string; __sendFlags:word):boolean;
*)
                
function umlauttest(var s:string):boolean;
function test_senddate(var s:string):boolean;
procedure firstslash(var s:string);
function testreplyto(var s:string):boolean;

function pgpo_sigtest(var s:string):boolean;
function pgpo_keytest(var s:string):boolean;

implementation  { --------------------------------------------------- }

uses mime, mime_analyze, rfc2822, StringTools, utftools, xp_pgp, xp1o, xp3,
  xp3ex, xp3o, xp3o2, xp4e, xp9bp, xpcc, xpconfigedit, xpfido, xpmakeheader,
  xpsendmessage_internal, xpstreams, addresses, 
  xpserver;

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
         cmp   al,'„'
         jnz   @noae
         mov   ax,'ea'
         jmp   @conv
@noae:   cmp   al,'”'
         jnz   @nooe
         mov   ax,'eo'
         jmp   @conv
@nooe:   cmp   al,''
         jnz   @noue
         mov   ax,'eu'
         jmp   @conv
@noue:   cmp   al,'Ž'
         jnz   @no_ae
         mov   ax,'eA'
         jmp   @conv
@no_ae:  cmp   al,'™'
         jnz   @no_oe
         mov   ax,'eO'
         jmp   @conv
@no_oe:  cmp   al,'š'
         jnz   @no_ue
         mov   ax,'eU'
         jmp   @conv
@no_ue:  cmp   al,'á'
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

{ --- Datei verschicken ---------------------------------------------------- }
{ Datei:  Pfadname der Datei. Wenn nicht vorhanden, wird eine leere angelegt }
{ is_temp: Datei löschen                                                     }
{ is_file: Es ist ein Datei-Attachment                                       }
{ empfaenger: der Empfaenger (User oder x/Brett)                             }
{ Edit :   Nachricht zunaechst Editieren und dann erst senden                }
{ Binary:  Binaerdatei                                                       }
{ sendwin: vor dem Senden Sende-Fenster abfragen                             }
{ datei, header und signat sind nur aus Stack-Platz-Gruenden VARs!           }
{ header wird veraendert!!                                                   }
(*
function DoSend(pm:boolean; __datei:string; is_temp,is_file:boolean;
                __empfaddr,__betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData: TSendUUData;
                signat:string; __sendFlags:word):boolean;
var sDataNew: boolean;
                
begin
  try
    sDataNew := not assigned(sData);
    if sDataNew then sData := TSendUUData.Create;
    
    if not pm and betreffbox and (__empfaddr<>'') and (FirstChar(__empfaddr)<>'A') then begin
      rfehler(606);   { 'Schreiben in dieses Brett nicht moeglich!' }
      exit;
    end;

    if __empfaddr<>'' then sData.EmpfList.AddNewXP(pm,__empfaddr,'');
    sData.flIntern := LeftStr(__empfaddr,3)='$/'#$AF;

    sData.Subject := __betreff;
    sData.Signature := Signat;
//  sData.sendFlags := __sendFlags;

    sData.flIntern := (__SendFlags and sendIntern)<>0;
    sData.flShow   := (__SendFlags and sendShow  )<>0;
    sData.flDelay  := (__SendFlags and sendDelay )<>0;
//  sData.flQuote  := (__SendFlags and sendQuote )<>0;
    sData.flWAB    := (__SendFlags and sendWAB   )<>0;
    sData.flReedit := (__SendFlags and sendReedit)<>0;
    sData.flHalt   := (__SendFlags and sendHalt  )<>0;
    sData.flMark   := (__SendFlags and sendMark  )<>0;
    sData.flPGPkey := (__SendFlags and sendPGPkey)<>0;
    sData.flPGPreq := (__SendFlags and sendPGPreq)<>0;
    sData.flPGPsig := (__SendFlags and sendPGPsig)<>0;
    sData.flNokop  := (__SendFlags and sendNokop )<>0;
//  sData.flIQuote := (__SendFlags and sendIQuote)<>0;
//  sData.flMPart  := (__SendFlags and sendMPart )<>0;

  finally
    if sDataNew then sData.Free;
  end;
end;
*)

{----------------------------------------------------------------------}
{-- TSendUUData: Send Window ------------------------------------------}
{----------------------------------------------------------------------}

function TSendUUData.DoIt(dlg_title: string;EditBetreff, EditText, SendWindow: Boolean): Boolean;
begin
  result := true;

  if EditBetreff and result then
    result := Self.EditBetreff(dlg_title);
  result := result and (EmpfList.Count>0);

  if EditText and result then 
    self.EditText;

  if SendWindow and result then 
    result := result and self.SendWindow(dlg_title);

  if result and not SentOK then
    CreateMessages;

  result := result and SentOK;
end;

function TSendUUData.EditBetreff(dlg_title: string): boolean;
begin
  if dlg_title='' then dlg_title := GetRes2(610,10);

  result := EditEmpfaengerList(dlg_title,true,true,true,
    [ncZConnect,ncFTN,ncRFC,ncMaus],
    [ncZConnect,ncFTN,ncRFC,ncMaus] );
  CheckEmpfaengerList(EmpfList,false,false);
end;

procedure TSendUUData.EditText;
begin
  EditNachricht(true);
end;

procedure TSendUUData.EditAttach;
begin
  MIMEDecompose;
  SendAttach(Parts,Umlaute=1,SigData,nt_UUCP,
    iif(docode in [8,9],cancode,docode),flPGPSig);

  // if the user deleted the message part, switch off signatures
  if (Parts.Count<1) or not TSendAttach_Part(Parts[0]).IsMessage then
    flOhneSig := true;
end;

{----------------------------------------------------------------------}
{-- TSendUUData: Message creation -------------------------------------}
{----------------------------------------------------------------------}

procedure TSendUUData.AddText(const fn:string; temp:boolean);
begin
  AddMessagePart(fn,temp,false);
end;

procedure TSendUUData.AddFile(const fn:string; temp:boolean; const ctype:string);
begin
  MimeDecompose;
  AddMessagePart(fn,temp,true);
end;

procedure TSendUUData.SetMessageContent(const fn:string; temp:boolean; orig_hdp: THeader);
begin
  ClearParts;
  orgHdp.Free;
  orgHdp := orig_hdp;

  if UpperCase(LeftStr(OrgHdp.MIME.CType,10)) = 'MULTIPART/' then
  begin
    PartFile := fn;
    PartFTmp := temp;
    PartsEx := not FileExists(fn);
  end else
  begin
    AddMessagePart(fn,temp,true);
  end;
end;

{$IFDEF __undefined__}

var f,f2     : file;
    edis     : byte;
    x,y,y2   : Integer;
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
    email    : string;
    _brett   : string;
    mapsname : string;
    senddate : string;  { mit 'D' zeitversetzt absenden       }
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
//  cancode  : Byte;        { 0=kein PW, 1=QPC, 2=DES, 9=PGP, 10=Rot13 }
//  docode   : Byte;        { gewaehlte Codierung                 }
//  pmc_code : boolean;
    senden   : shortint;    { 0=Nein, 1=Ja, 2=Intern              }
    halten   : integer16;   { Haltezeit fuer neuen User           }
    passwd   : string;      { Passwort des empfangenden Users     }
    passpos  : smallword;   { PW-Position fuer QPC                }
    newbin   : boolean;     { Typ nach Codierung                  }
    SDNope   : boolean;
    dbshown  : boolean;
//    intern,                 { interne Nachricht                   }
   
//    lokalPM  : boolean;     { lokale PM                           }

    grnr     : longint;     { Brettgruppen-Nr.                    }
    addsize  : longint;     { Header + Signatur                   }
{   hdsize   : word; }
    oversize : longint;     { Nachrichtenlimit ueberschritten     }
    parken   : boolean;     { Nachricht nach />>Unversandt        }
    SendDefault : shortint;
    verteiler: boolean;
    _verteiler: boolean;    { bleibt true bei allen Einzelnachrichten }
//  netztyp  : byte;        { Netztyp                             }
    nomids   : boolean;     { keine Message-ID's erzeugen         }
    nobox    : boolean;     { Absender-Name im PP ohne Boxname    }
    echomail : boolean;
    fadd     : shortint;
    oldnt    : byte;        { alter Netztyp bei Pollbox-Wechsel   }
    fidoam   : boolean;
//  old_cca  : integer;     { vor (K)opien            }
//  FidoBin  : boolean;     { File Attach }
//  cc_count : integer;
    betrlen  : Integer;     { max. Betrefflaenge }
    bboxwid  : byte;

    spezial  : boolean;
//  flOhnesig: boolean;
//  flLoesch : boolean;
//  sdnope   : boolean;     { sData = nil }
    orgftime : longint;
//  sigfile  : string;
//  sigtemp  : boolean;

    sigfile  : string;      { Signatur                             }
    sigtmp   : boolean;     { Signatur: Bearbeitet (eine Signatur ausgesucht) }
    siglast  : string;      { Signatur: Letzte Quelldatei          }
    sigok    : boolean;


//  msgCPanz : shortint;    { EMP's in aktueller Msg. }
//  msgCPpos : shortint;    { gerade bearbeiteter EMP }
    ii       : integer;
    m1adr    : longint;     { Pufferadresse der ersten Kopie }
    m1msgsize: longint;     { Gesamtgroesse der ersten Kopie }
    showempfs: shortint;    { fuer Betreffbox }
    fo       : string;

    pa       : TSendAttach_Part;

    s        : String;
    s1,s2,s3,s4,s5 : TStream;
    Boundary : String;
    i        : Integer;
    firststart :boolean;

  label fromstart,ReadAgain;

{$I xpsendmessage_subs.inc}
{$I xpsendmessage_subs_mime.inc}
(*
  function RFCBrett(s:string; edis:byte):string;
  var i : integer;
  begin
    if (edis=1) or ((not (netztyp in netsRFC)) and not Newsgroupdispall) or not NewsgroupDisp then
      rfcbrett:=mid(s,edis)
    else begin
      delete(s,1,2);
      for i:=1 to length(s) do if s[i]='/' then s[i]:='.';
      RFCBrett:=s;
    end;
  end;
*)

  procedure DisplaySendbox;
  var
    ToStr: String;
    ToPos: Integer;

    LastGroup: array [atNewsgroup..atBCC] of Integer;
    Addresses: array [atNewsgroup..atBCC] of TStringList;
    Count:     array [atNewsgroup..atBCC] of Integer;
    Ia:        TAddressListType; 
    
    showempfs : integer;
        sl,ii : integer;
            s : string;

    procedure _(w:TAddressListType;const s: string;delim:boolean);
    var MaxFirstLen: integer;
    begin
      MaxFirstLen := 78-sl-4-iif(delim,0,1);
      if Addresses[w].Count>0 then 
        Dec(MaxFirstLen,Length(Addresses[w].Strings[Addresses[w].Count-1]));
    
      if (Addresses[w].Count>0) and
          (Length(s) <= MaxFirstLen) then
        Addresses[w].Strings[Addresses[w].Count-1] := 
          Addresses[w].Strings[Addresses[w].Count-1] + s
      else 
      if (Addresses[w].Count>0) and 
          (Length(s) = MaxFirstLen+1) and
          (LastChar(s)=' ') then
        Addresses[w].Strings[Addresses[w].Count-1] := 
          Addresses[w].Strings[Addresses[w].Count-1] + LeftStr(s,Length(s)-1)
      else    
        if FirstChar(s)<>' ' then
          Addresses[w].Add(s)
        else
          Addresses[w].Add(Mid(s,2));
    end;
    
  begin
    if dbshown then closebox;
    dbshown := true;

    s := GetRes2(611,10); sl := Length(s);

    for ia := Low(Addresses) to High(Addresses) do
      LastGroup[ia] := -1;
    for ia := Low(Addresses) to High(Addresses) do
      Addresses[ia] := TStringList.Create;    

    with sData do for ii := 0 to sData.EmpfList.Count-1 do
    begin
      if not (EmpfList[ii].AddressType in [atNewsgroup,atTo,atCC,atBCC]) then 
        continue;

      if Addresses[EmpfList[ii].AddressType].Count>0 then
        _(EmpfList[ii].AddressType,', ',true);
      
      if (LastGroup[EmpfList[ii].AddressType]<>EmpfList[ii].Group) and
         (LastGroup[EmpfList[ii].AddressType]>=0) then
        _(EmpfList[ii].AddressType,';',true);

      if LastGroup[EmpfList[ii].AddressType]<>EmpfList[ii].Group then
        if EmpfList[ii].Group>=0 then
          _(EmpfList[ii].AddressType,
            RFCQuotePhrase(EmpfList.GroupNames[EmpfList[ii].Group],false)+': ',false);

      LastGroup[EmpfList[ii].AddressType]:=EmpfList[ii].Group;

      if (EmpfList[ii].Address is TDomainEMailAddress) and
         (TDomainEMailAddress(EmpfList[ii].Address).Realname<>'') then
      begin
        _(EmpfList[ii].AddressType,RFCQuotePhrase(TDomainEMailAddress(EmpfList[ii].Address).Realname,false)+' ',false);
        _(EmpfList[ii].AddressType,' <'+TDomainEMailAddress(EmpfList[ii].Address).AddrSpec+'>',false);
      end else
        _(EmpfList[ii].AddressType,EmpfList[ii].DisplayString,false);
    end;

    for ia := Low(Addresses) to High(Addresses) do
      Count[ia] := Addresses[ia].Count;

    ShowEmpfs := Count[atNewsgroup]+Count[atTo]+Count[atCC]+Count[atBCC];

    if ShowEmpfs > ScreenLines-(FAdd+13+2) then
    begin
      ShowEmpfs := ScreenLines-(FAdd+13+2);
      Count[atNewsgroup] := Min(Count[atNewsgroup],Max(0,Max(Showempfs-Count[atTo]-Count[atCC]-Count[atBCC],ShowEmpfs div 2)));
      if Count[atNewsgroup]+Count[atTo]+Count[atCC]+Count[atBCC] > ShowEmpfs then
      begin
        Count[atTo] := Min(Count[atTo],Max(0,Max(Showempfs-Count[atNewsgroup]-Count[atCC]-Count[atBCC],ShowEmpfs div 3)));
        if Count[atNewsgroup]+Count[atTo]+Count[atCC]+Count[atBCC] > ShowEmpfs then
        begin
          Count[atCC] := Min(Count[atCC],Max(0,Max(Showempfs-Count[atNewsgroup]-Count[atTo]-Count[atBCC],ShowEmpfs div 4)));
          if Count[atNewsgroup]+Count[atTo]+Count[atCC]+Count[atBCC] > ShowEmpfs then
          begin
            Count[atBCC] := Min(Count[atBCC],Showempfs-Count[atNewsgroup]-Count[atTo]-Count[atCC]);
          end;
        end;
      end;
      ShowEmpfs := Count[atNewsgroup]+Count[atTo]+Count[atCC]+Count[atBCC];
    end;
    
    diabox(78,13+fadd+Max(ShowEmpfs,1)-1,typ,x,y);
    moff;

    if (Count[atNewsgroup]>0) then wrt(x+3,y+2,s );
    if (Count[atTo]>0) or (ShowEmpfs=0) then wrt(x+2+Count[atNewsgroup],y+2,s );
    if (Count[atCC]>0)         then wrt(x+3,y+2+Count[atTo]+Count[atNewsgroup],s );
    if (Count[atBCC]>0)        then wrt(x+3,y+2+Count[atTo]+Count[atNewsgroup]+Count[atCC],s );

    attrtxt(col.coldiahigh);    
    for ii:=0 to Count[atNewsgroup]-1 do
      Wrt(x+4+sl,y+2+ii,LeftStr(Addresses[atNewsgroup].Strings[ii],78-sl));
    for ii:=0 to Count[atTo]-1 do
      Wrt(x+4+sl,y+2+ii+Count[atNewsgroup],LeftStr(Addresses[atTo].Strings[ii],78-sl));
    for ii:=0 to Count[atCC]-1 do
      Wrt(x+4+sl,y+2+ii+Count[atTo]+Count[atNewsgroup],LeftStr(Addresses[atCC].Strings[ii],78-sl));
    for ii:=0 to Count[atBCC]-1 do
      Wrt(x+4+sl,y+2+ii+Count[atTo]+Count[atNewsgroup]+Count[atCC],LeftStr(Addresses[atBCC].Strings[ii],78-sl));
      
    if Count[atNewsgroup]<Addresses[atNewsgroup].Count then 
      wrt(x+4+sl+BBoxWid-7,y+2+Count[atNewsgroup]-1,' (...)');
    if Count[atTo]<Addresses[atTo].Count then 
      wrt(x+4+sl+BBoxWid-7,y+2+Count[atNewsgroup]+Count[atTo]-1,' (...)');
    if Count[atcc]<Addresses[atcc].Count then 
      wrt(x+4+sl+BBoxWid-7,y+2+Count[atNewsgroup]+Count[atTo]+Count[atcc]-1,' (...)');
    if Count[atbcc]<Addresses[atbcc].Count then 
      wrt(x+4+sl+BBoxWid-7,y+2+Count[atNewsgroup]+Count[atTo]+Count[atcc]+Count[atbcc]-1,' (...)');

    if ShowEmpfs=0 then
      Wrt(x+3+sl,y+2,GetRes(602));     

    y2 := y;    
    inc(y,showempfs-1);
    
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
    wrt(x+51,y+8,strs(sData.EmpfList.Count));
    attrtxt(col.coldiahigh);
    kopkey:=FirstChar(getres2(611,16));
    wrt(x+42,y+8,kopkey); 

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

    for ia := Low(Addresses) to High(Addresses) do
      Addresses[ia].Free;
  end;

  procedure WriteHeaderHdr;
  var f:text;
      i:integer;
  begin
    assign(f,temppath+FileUpperCase('header.hdr'));
    rewrite(f);
    writeln(f,'TYP: ',typ);
    writeln(f,'BOX: ',box);
    for i:=0 to sData.EmpfList.Count-1 do writeln(f,'EMPF: ',sData.EmpfList[i].ZCAddress);
//  for i:=0 to sData.CCList  .Count-1 do writeln(f,'EMPF: ',sData.  CCList[i].ZCAddress);
//  for i:=0 to sData.BCCList .Count-1 do writeln(f,'EMPF: ',sData. BCCList[i].ZCAddress);
    writeln(f,'FIDOTO: ',fidoto);
    writeln(f,'BETREFF: ',sdata.Subject);
    close(f);
  end;


procedure editbetreff;                             { Betreff editieren }
var        ii : integer;
      oldbetr : string;
            s : string;
           sl : integer;

      empfcnt : integer;
        cccnt : integer;
       bcccnt : integer;

begin
  if sData.flQuote then typ:=typ+getres2(611,4) else   { ' (Quote)' }
  if binary then typ:=typ+getres2(611,5);   { ' (Bin„r)' }
  fidoam:=ntEditBrettempf(nt_uucp) and not pm;
  bboxwid:=60;

  EmpfCnt := SData.EmpfList.Count;
//  CCCnt := SData.  CCList.Count;
// BCCCnt := SData. BCCList.Count;
   
  showempfs := EmpfCnt; //+CCCnt+BCCCnt;

  if showempfs > 15 then begin showempfs := 15; 
                                              (* EmpfCnt := Min(showempfs-CCCnt-BCCCnt,  showempfs div 2);
  if EmpfCnt+CCCnt+BCCCnt > showempfs then begin CCCnt   := Min(showempfs-EmpfCnt-BCCCnt,showempfs div 3);
  if EmpfCnt+CCCnt+BCCCnt > showempfs then       BCCCnt  := showempfs-EmpfCnt-CCCnt; end; *) end;

  diabox(bboxwid+19,iif(fidoam,9,7)+showempfs,typ,x,y);
  moff;
(*
  s:= GetRes2(611,6); sl:= Length(s); {'Empf„nger  '}
  if EmpfCnt>0 then wrt(x+3,y+2,              s              );
  if   CCCnt>0 then wrt(x+3,y+2+EmpfCnt,      getres2(611,16)); {'CC         '}
  if  BCCCnt>0 then wrt(x+3,y+2+EmpfCnt+CCCnt,getres2(611,16)); {'BCC        '}

  attrtxt(col.coldiahigh);
  
  for ii:=0 to Min(EmpfCnt,sData.EmpfList.Count)-1 do
    Wrt(x+3+sl,y+2+ii,LeftStr(SData.EmpfList[i].DisplayString,BBoxWid));
  for ii:=0 to Min(CCCnt,sData.CCList.Count)-1 do
    Wrt(x+3+sl,y+2+ii+EmpfCnt,LeftStr(SData.CCList[i].DisplayString,BBoxWid));
  for ii:=0 to Min(BCCCnt,sData.BCCList.Count)-1 do
    Wrt(x+3+sl,y+2+ii+EmpfCnt+CCCnt,LeftStr(SData.BCCList[i].DisplayString,BBoxWid));

  if EmpfCnt<sData.EmpfList.Count then 
    wrt(x+3+sl+BBoxWid-6,y+2+EmpfCnt-1,' (...)');
  if   CCCnt<sData.  CCList.Count then 
    wrt(x+3+sl+BBoxWid-6,y+2+EmpfCnt+CCCnt+1,' (...)');
  if  BCCCnt<sData. BCCList.Count then 
    wrt(x+3+sl+BBoxWid-6,y+2+EmpfCnt+CCCnt+BCCCnt-1,' (...)');
*)   
  mon;
  openmask(x+2,x+bboxwid+10,y+showempfs+4,y+showempfs+iif(fidoam,6,4),false);
  oldbetr:=sdata.Subject;
  maddstring(1,1,getres2(611,7),sdata.Subject,bboxwid,MAXINT,'');   { 'Betreff   ' }
  msetvfunc(umlauttest); mhnr(86);
  if fidoam then begin
    maddstring(1,3,getres2(611,8),fidoto,35,35,'');  { 'An        ' }
    mhnr(90);
  end;
  readmask(brk);
  closemask;
  closebox;
  sdata.Subject:=trim(sdata.Subject);
  if brk then exit;                  { --> Abbruch bei Betreffmaske }
  if sdata.Subject='' then begin
    brk:=true;
    if not pm then rfehler(635);  { 'Nachricht muá einen Betreff haben' }
    if (pm and not ReadJNesc(getres(618),false,brk)) or   { 'Nachricht ohne Betreff absenden' }
       not pm then exit;
    brk:=false;
  end;
  if (_bezug<>'') and // ntKomkette(netztyp) and
                  (uppercase(sdata.Subject)<>uppercase(oldbetr)) then begin
    pushhp(1501);
    if not ReadJNesc(getres(617),(leftstr(sdata.Subject,5)=leftstr(oldbetr,5)) 
       or ((cpos('(',oldbetr)=0) and (cpos('(',sdata.Subject)>0)),brk) then
          { 'Betreff ge„ndert - Verkettung beibehalten' }
    begin
      _bezug:='';
      _orgref:='';
      sData.References.Clear;
    end else 
      if RFCAppendOldSubject (* and (netztyp in netsRFC) *) then
        sdata.Subject:=sdata.Subject+' ('+getres(619)+': '+oldbetr+')';
    pophp;
    if brk then exit;
  end;
end;

procedure EditRecipients;
begin
  EditEmpfaengerList(GetRes2(611,19),true,
        sData.EmpfList,
        nil,
        false,
        false,
        sdata.Subject,
        [],[],sData);

  if not sData.flIntern then
  begin
    CheckEmpfaengerList(sData.EmpfList, false, false, sData);
    MakeSignature;
  end;    
end;


{$I xpsendmessage_create.inc}

{ --- Datei verschicken ---------------------------------------------------- }
{ Datei:  Pfadname der Datei. Wenn nicht vorhanden, wird eine leere angelegt }
{ is_temp: Datei löschen                                                     }
{ is_file: Es ist ein Datei-Attachment                                       }
{ empfaenger: der Empfaenger (User oder x/Brett)                             }
{ Edit :   Nachricht zunaechst Editieren und dann erst senden                }
{ Binary:  Binaerdatei                                                       }
{ sendwin: vor dem Senden Sende-Fenster abfragen                             }
{ datei, header und signat sind nur aus Stack-Platz-Gruenden VARs!           }
{ header wird veraendert!!                                                   

function DoSend(pm:boolean; __datei:string; is_temp,is_file:boolean;
                __empfaddr,__betreff:string;
                edit,binary,sendbox,betreffbox,XpID:boolean; sData: TSendUUData;
                signat:string; __sendFlags:word):boolean;
}

begin      //-------- of DoSend ---------
 try
  DoSend:=false;
  parken:=false;
  _verteiler:=false;
  dbshown := false;

  sigfile  := '';
  sigtmp   := false;
  siglast  := '';
  sigok    := false;
  
  s1:=nil;{s2:=nil;}s3:=nil;s4:=nil;s5:=nil;
  
 try 
// assign(f,sdata.datei);

  SDNope := not assigned(SData);
  if not assigned(sData) then sData := TSendUUData.Create;
  
//sdata.netztyp:=sData.onetztyp;

  sdata.datei := __datei;
  sdata.SendFlags := __sendFlags;

 try // xexit1;
  if not pm and betreffbox and (__empfaddr<>'') and (FirstChar(__empfaddr)<>'A') then
  begin
    rfehler(606);   { 'Schreiben in dieses Brett nicht moeglich!' }
    SendEmpfList.Clear; { clear list of CC recipients }
    exit;
  end;

  if __empfaddr<>'' then sData.EmpfList.AddNewXP(pm,__empfaddr,'');
  sData.flIntern := LeftStr(__empfaddr,3)='$/'#$AF;

  if not sData.flIntern then
    CheckEmpfaengerList(sData.EmpfList, false, false, sData);

  if sData.EmpfList.Count<=0 then 
    exit;

  if (not   sData.flIntern) and (sdata.datei='') then
  begin
    sdata.datei := TempS(2000);
    BriefSchablone(sData.EmpfList[0].Address,HeaderPriv,sdata.datei);
  end;

  MakeSignature;

  if sdata.flQuote then
  begin
    ExtractSetMimePart(qMimePart);
    extract_msg(3,iifs(force_quotemsk<>'',force_quotemsk,QuoteSchab(pm)),
                sdata.datei,false,1);
    sData.quotestr:=qchar;
    get_xref;
    sdata.partsex:=true;
    sdata.AddMessagePart(sdata.datei,true,true);
  end else
  if sdata.flMPart then
  begin
    sdata.PartsEx := not FileExists(sdata.datei);
      // don't extract multipart parts if file does not exist anyway...
  end else
  begin
    if FileExists(sdata.datei) then
    begin
      if not is_file then
        sdata.AddMessagePart(sdata.datei,is_temp,true)
      else
        { if netztyp=nt_Fido then } sdata.AddFilePart(sdata.datei,is_temp);
    end;
    sdata.partsex:=true;
    OrigBox:='';
  end;

  hdp := THeader.Create;

  SendDefault:=1;
  verteiler:=false;
 with sdata do  flPGPkey:=(sendflags and SendPGPkey<>0);
 with sdata do  flPGPsig:=(sendflags and SendPGPsig<>0) or PGP_signall;
 with sdata do  flPGPreq:=(sendflags and SendPGPreq<>0);
  flNokop:=(sdata.sendflags and SendNokop<>0) or DefaultNokop;

  fo:='';
  
 try //xexit
{ Einsprung hier startet ganze Versand-Prozedur von vorne (mit den bestehenden Daten) }
fromstart:

  passwd:='';          { Betreffbox true = Betreff nochmal eintippen           }
  empfneu:=false;      { Edit       true = Editor Starten                      }
  sdata.docode:=0;           { Sendbox    true = Sendefenster zeigen                 }
  fidoname:='';        { forcebox ''-um Box entsprechend Empfaenger zu waehlen }
  ch:=' ';             {          Ansonsten steht hier die zu benutzende Box   }

  { -- Empfaenger -- }

// betreff:=LeftStr(betreff,betrlen);

  if betreffbox then begin
    editbetreff;
    if brk then exit;
  end;

  orgftime:=filetime(sdata.datei);
  if edit then begin
    WriteHeaderHdr;
    sdata.EditNachricht(pgdown);              //Editor aufrufen
  end;
  if not getsize then exit;        { --> Nachrichten-Groesse 0 }
  calc_hdsize;

//  echomail:=ntEditBrettempf(Server.netztyp) and not pm;

  if sendbox then
  repeat
//    echomail:=ntEditBrettempf(netztyp) and not pm;
    fadd:=iif(echomail,2,0);
    DisplaySendbox;                         { SendBox aufbauen }
    senden:=-1;
//  n:=1;                                { SendBox-Abfrage }
    pushhp(68);
    spezial:=false;
    repeat
//      if pm then intern:=false
//      else intern:=(grnr=IntGruppe);
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

        case nt_UUCP of    { '^Parken,^Datum, ^EB ,o^hneSig,l^oeschen,' }
          nt_Fido     : sendbutt:=getres2(611,20);  { 'C^rash,P^GP'     }
          nt_Maus     : sendbutt:=getres2(611,21);  { '^MausNet,^Lokal' }
          nt_ZConnect : sendbutt:=getres2(611,22);  { 'P^rio,P^GP'      }
        else
            if nt_UUCP in netsRFC then
              sendbutt:=getres2(611,23)   { 'Z^usatz,P^GP'    }
            else
              sendbutt:=getres2(611,24);  { '^Zurueck'        }
        end;
        
        repeat
          t:='*';
          n:=readbutton(x+3,y+11,1,sendbutt,
                        abs(n),true,t);
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        case nt_UUCP of
          nt_Fido : if n=7 then n:=11        { PGP         }
                    else if n=8 then n:=0;   { MH: Zurueck }
          nt_Maus : if n=8 then n:=0         { Zurueck     }
                    else if n>5 then inc(n);
          nt_ZConnect :  if n=6 then n:=9    { Prio        }
                    else if n=7 then n:=10   { Zusatz      }
                    else if n=8 then n:=11   { PGP         }
                    else if n=9 then n:=0;   { MH: Zurueck }
          else
            if nt_UUCP in netsRFC then begin
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
          p:=pos(upcase(t[1]),getres2(611,25));   { JNIùBOCT }
          if p>0 then n:=p;
        end;
      end else begin
        repeat
          t:='*';
          n:=readbutton(x+3,y+11,1,getres2(611,28)+
               iifs((not CanEdit) or (sdata.sendflags and sendWAB<>0),'',getres2(611,29)),
                        abs(n),true,t);
           { ' ^Ja ,^Nein,^Intern,^Spezial,ù2^Betreff,B^ox,^Code' ',^Text' }
        until (n>=0) or ((t<>mausmoved) and (t<>mauslmoved));
        if n=4 then begin
          spezial:=true;
          attrtxt(col.coldialog);
          mwrt(x+1,y+11,sp(76)); { 05.02.2000 MH: 68 -> 76 f. Zurueck } { unbedenklich }
          goto ReadAgain;
          end;

        if { (n=5) or } (t='/') then    { Empfaenger aendern? }
        begin
          EditRecipients;
          DisplaySendbox;              
          goto fromstart;
        end
        else
        case n of     { Ansonsten eins zurueckzaehlen fuer alte Keys }
          1..4: ;               { Ja, Nein, Intern, Spezial     }
          5..7: ;               { Betreff, Box, Code            }
          8:    n:=23;          { Anhänge                       }
          9:    ;               { Text                          }
        end;

        if n<0 then begin
          p:=pos(UpCase(t[1]),getres2(611,30));   { PDEH™RMLG }
          case p of
            1..5 : n:=p+10;
            6    : if nt_UUCP=nt_Fido then n:=16 else
                   if nt_UUCP=nt_ZConnect then n:=19 else
                   if nt_UUCP in netsRFC then n:=22;
            7    : if nt_UUCP=nt_Maus then n:=17;
            8    : if nt_UUCP=nt_Maus then n:=18;
            9    : if nt_UUCP in (netsRFC + [nt_ZConnect]) then n:=20;
            10   : if nt_UUCP in (netsRFC + [nt_ZConnect,nt_Fido,nt_Maus]) then
                     n:=21;  { PGP }
            else   if ntBCC(nt_UUCP) and (t=^K) then
                     flNokop:=not flNokop;
          end;
          end;
        end;
      case n of
        0   : if SaveUVS and not binary then senden:=3   { Abbruch }
              else if sData.uv_edit then senden:=1
              else senden:=0;
        1   : if sData.EmpfList.Count>0 then
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
                readstring(x+13,y+4,'',sdata.Subject,min(betrlen,52),60,'',brk);
                sdata.Subject:=trim(sdata.Subject);
                if umlauttest(sdata.Subject) then;
                showbetreff;
//              n:=1;
              end;
              
        6   : if   sData.flIntern then
                rfehler(611)   { 'nicht moeglich - interne Nachricht' }
//            else if IncompatibleNTs then
//              rfehler(629)   { 'nicht moeglich - unterschiedliche Netztypen' }
              else begin                        { neue Pollbox }
                newbox:=UniSel(1,false,box);
                if newbox<>'' then
                  if not pm and (cc_anz=0) and ntBrettebene(nt_UUCP) and
                     ntBrettebene(ntBoxNetztyp(newbox)) and
                     not stricmp(BoxBrettebene(box),BoxBrettebene(newbox)) then
                    rfehler(637)   { 'Serveraenderung nicht moeglich - abweichende Brettebene!' }
                  else begin
                    dbOpen(d,BoxenFile,1);
                    dbSeek(d,boiName,UpperCase(newbox));
                    if binary and not ntBinary(dbReadInt(d,'netztyp')) then
                      rfehler(609)  { 'In diesem Netz sind leider keine Binaernachrichten moeglich :-(' }
                     else if (((not pm) and (nt_UUCP<>dbReadInt(d,'netztyp'))) or
                     not ntAdrCompatible(nt_UUCP,dbReadInt(d,'netztyp'))) then
                     rfehler(629)   { 'nicht m”glich - unterschiedliche Netztypen' }
                 else begin
//                    KorrPhantomServers(box,newbox,dbReadInt(d,'netztyp'));
                      box:=newbox;
                      oldnt:=nt_UUCP;
                      sData.replyto := '';
//                      LoadBoxData(d);
                      if (nt_UUCP=nt_Fido)<>(oldnt=nt_Fido) then
                        senden:=5;
                      if pm then SetLocalPM;
                      showsize;
                      if cc_anz>0 then forcebox:=box;
                      showbox;
                      if nt_UUCP<>nt_Fido then
                        flCrash:=false;
                      end;
                  dbClose(d);
                  end;
//              n:=1;
              end;
        7   : with sdata do if cancode<>0 then
              begin                                { Codierung aendern }
                if docode<>0 then
                  docode:=0
                else
                if     sdata.parts.Count=1 then
                  docode:=cancode
                else if (cancode in [8,9]) and ntMIME(nt_UUCP)
                then
                  docode:=8;                       // use PGP/MIME instead of PGP for multiparts
                showcode;
//              n:=1;
              end;
        9   : if not binary and (sdata.sendflags and sendWAB=0) then begin
                sdata.editnachricht(false);              { zurueck zum Editor }
                if not getsize then begin
                  closebox; exit; end;    { -> Nachrichtengroesse 0 }
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
       13   : if not sData.has_pm then
                rfehler(614)   { 'Empfangsbestaetigung nur bei PMs moeglich' }
              else
                flEB:=not flEB;
       14   : with sdata do begin
                flOhnesig:=not flOhnesig;

                // If the user explicitly says that s/he wants a signature
                // then create a part that can hold it.

                if (not flOhneSig) and ( (    sdata.Parts.Count<1) or
                    not TSendAttach_Part(    sdata.Parts[0]).IsMessage ) then
                  AddMessagePart(TempS($FFFF),true,false);

                calc_hdsize;
                showsize;
              end;
       15   : with sdata do flLoesch:=not flLoesch;
       16   : if sData.can_crash then
                flCrash:=not flCrash
              else
                rfehler(615);   { 'nur bei PMs moeglich' }
       17   : begin
                flMnet:=not flMnet;
                flMloc:=false;
                sData.distribute:=iifs(flMnet,'MausNet','');
                calc_hdsize; showsize;
              end;
       18   : begin
                flMloc:=not flMloc;
                flMnet:=false;
                sData.distribute:=iifs(flMloc,'lokal','');
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
               if not(nt_UUCP in netsRFC)then rfehler(622);
                getprio;
               showflags;
              end;
       23   : begin
                MIMEDecompose;
                SendAttach(    sdata.Parts,Umlaute=1,iifs(sdata.flOhneSig,'',sigfile),nt_UUCP,
                  iif(sdata.docode in [8,9],sdata.cancode,sdata.docode),sdata.flPGPSig);

                // if the user deleted the message part, switch off signatures
                if (    sdata.Parts.Count<1) or not TSendAttach_Part(    sdata.Parts[0]).IsMessage then
                  sdata.flOhneSig := true;

                KorrCode;
              end;

      else  if n<0 then begin 
              EditRecipients;
              DisplaySendbox;              
            end;
      end;
    until senden>=0;
    pophp;
    closebox;

    case senden of
      0 : exit;              { Abbruch }
      2 :   sData.flIntern:=true;            { nicht in Puffer + kein unversandt }
      3 : begin                    { Nachricht nach />>Unversandt }
            ParkMsg;               { ## Originalempfaenger einfuegen }
            pm:=false;
            Internbox:={default}box;
            sData.EmpfList.Clear;
            sData.EmpfList.AddNewXP(false,UnvBrett,'');
              sData.flIntern:=true;
            parken:=true;
            betreffbox:=false; edit:=false; sendbox:=false;
            sdata.flIntern := true;
            cc_anz:=0;
            flcrash:=false;   { !! }
            goto fromstart;
          end;
      4 : begin
            DateSendIt;
            exit;
          end;
    end;

  until (senden<>5) and ((nt_UUCP<>nt_Pronet) or SizeOk) and
        ((senden<>1) or   sData.flIntern or (grnr=LocGruppe) or pm or (fs+addsize<1024)
         or (fs+addsize>50000) or binary or QuoteOK)
  else begin
    senden:=;    { not sendbox }
    case senden of  SendDefault
      2 :   sData.flIntern:=true;
    end;
    end;

  if pm then fidoto:=''
  else
    case nt_UUCP of
      nt_Fido,
      nt_QWK      : fidoto:=fidoto;
    else
      if nt_UUCP in (netsRFC + [nt_Magic, nt_Pronet, nt_ZConnect]) then begin
        if (fidoto=brettalle) or (blankpos(fidoto)=0) then
          fidoto:='';
      end else fidoto:='';
    end;

    CheckEmpfaengerList(sData.EmpfList,false,true,sData);
    CreateMessages;
(*
    if not intern and
      not noCrash and flCrash and MayCrash and FidoAdrOK(false) and
         ReadJN(getres(615),true) then    { 'Crash sofort absenden' }
        AutoCrash:=CrashAdr;  { Empfaenger, evtl. ohne Point }
*)

  { --- Aufräumarbeiten zum Schluss ----------------------------------- }

  aufbau:=true; xaufbau:=true;
  { es muss jetzt der korrekte Satz in mbase aktuell sein! }
 finally // xexit:
  freeres;
//dispose(cc); dispose(ccm);
  Hdp.Free;
  if sigtmp then _era(sigfile);
 end;
 
 finally //xexit1
  if sdNope then sData.Free;

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
  sendEmpfList.Clear;
 end;

 finally
  for ii:=0 to     sdata.parts.count-1 do
    TObject(    sdata.parts[ii]).Free;
      sdata.parts.Free;
 end;

 except 
   on E:Exception do
   begin
     fehler(E.Message);
   {$IFDEF Debug }
      raise; // raise error to write debug output with line info
   {$ENDIF }
   end;
 end;

end; {------ of DoSend -------}
{$ENDIF}

{ -------------------------------------------------------------------- }
{ -- TSendUUData ----------------------------------------------------- }
{ -------------------------------------------------------------------- }

{ TSendUUData is a class that represents a message being sent. It      }
{ replaces the DoSend procedure.                                       }

constructor TSendUUData.Create;
var n: TNetClass;
begin
  EmpfList := TAddressList.Create;
  Followup := TStringlist.Create;
  References := TStringList.Create;
  OEM := TStringList.Create;

  parts := TList.Create;

  Boxen        := TStringList.Create;
  Boxen.Sorted     := true;
  Boxen.Duplicates := dupIgnore;

  for n := Low(n) to high(n) do 
    FCharsets[n] := TStringList.Create;

  Clear;
end;

destructor TSendUUData.Destroy;
var n: TNetClass;
begin
  EmpfList.Free;
  FollowUp.Free;
  REferences.Free;
  OEM.Free;

  Boxen.Free;

  ClearParts;
  Parts.Free;
    
  for n := Low(n) to high(n) do 
    FCharsets[n].Free;

  inherited;
end;

procedure TSendUUData.Clear;
var n: TNetClass;
    b: boolean;
begin
  EmpfList.Clear;

  for n := Low(n) to high(n) do 
  begin
    FCharsets[n].Clear;
    for b := low(b) to high(b) do
      FHas[n,b] := false;
  end;

  can_crash:= false;

  Boxen    .Clear;
    
  maxsize   := MAXINT;
  attachMode:= attachMIME;

  ClearParts;
  partsex := false;
 
  Replyto := '';
  followup.Clear;
  References.Clear;
  keywords := '';
  summary := '';
  distribute := '';
  ReplyGroup := '';     { Maus-QuoteTo }
  oab := '';
  wab := '';
  OEM.Clear;
  oar := '';
  war := '';
  onetztyp := 0;
  orghdp := nil;
  quotestr := '';
  UV_edit:= false; { <Esc> -> "J" }
//empfrealname := '';
  msgid := '';
  ersetzt := '';
  SenderRealname := '';
  SenderMail := '';
  FQDN := ''; { overriding standards in DoSend if set }
  RTAHasSetVertreter := false;
  boundary := '';

  SentOK   := false;
  
  flIntern := false;
  flShow   := false;
  flDelay  := false;
//flQuote  := false;
  flWAB    := false;
  flReedit := false;
  flHalt   := false;
  flMark   := false;
  flPGPkey := false;
  flPGPreq := false;
  flPGPsig := false;
//flNokop  := false;
//flIQuote := false;
//flMPart  := false;

  flControlMsg := false;
  flUngelesen := false;

  flOhneSig := true;

  FUserDataForced := false;
end;

{ -------------------------------------------------------------------- }

function TSendUUData.GetEmpf1Address:  string; 
begin
  if EmpfList.Count<=0 then
    result := ''
  else
  if EmpfList[0].Address is TEmailAddress then
    result := EmpfList[0].XPAddress
  else
    result := EmpfList[0].ZCAddress;
end;

procedure TSendUUData.SetEmpf1Address (NewValue: string);
begin
  if EmpfList.Count<=0 then
    EmpfList.AddNew.ZCAddress := NewValue
  else
  if EmpfList[0].Address is TDomainEmailAddress then
    TDomainEmailAddress(EmpfList[0].Address).AddrSpec := NewValue
  else
    EmpfList[0].ZCAddress := NewValue;
end;

function TSendUUData.GetEmpf1RealName: string; 
begin
  if (EmpfList.Count>0) and (EmpfList[0].Address is TDomainEmailAddress) then
    result := (EmpfList[0].Address as TDomainEMailAddress).Realname
  else 
    result := '';
end;

procedure TSendUUData.SetEmpf1RealName(NewValue: string);
begin
  if (EmpfList.Count>0) and (EmpfList[0].Address is TDomainEmailAddresS) then
    (EmpfList[0].Address as TDomainEMailAddress).Realname := NewValue;
end;

(*
function TSendUUData.GetSendFlags: word;
begin
  Result := 0;
  if flIntern then Result := Result or sendIntern;
  if flShow   then Result := Result or sendShow  ;
  if flDelay  then Result := Result or sendDelay ;
//if flQuote  then Result := Result or sendQuote ;
  if flWAB    then Result := Result or sendWAB   ;
  if flReedit then Result := Result or sendReedit;
  if flHalt   then Result := Result or sendHalt  ;
  if flMark   then Result := Result or sendMark  ;
  if flPGPkey then Result := Result or sendPGPkey;
  if flPGPreq then Result := Result or sendPGPreq;
  if flPGPsig then Result := Result or sendPGPsig;
  if flNokop  then Result := Result or sendNokop ;
//if flIQuote then Result := Result or sendIQuote;
//if flMPart  then Result := Result or sendMPart ;
end; 
*)

(*
procedure TSendUUData.SetSendFlags(flags:word);
begin
  flIntern := (flags and sendIntern)<>0;
  flShow   := (flags and sendShow  )<>0;
  flDelay  := (flags and sendDelay )<>0;
//flQuote  := (flags and sendQuote )<>0;
  flWAB    := (flags and sendWAB   )<>0;
  flReedit := (flags and sendReedit)<>0;
  flHalt   := (flags and sendHalt  )<>0;
  flMark   := (flags and sendMark  )<>0;
  flPGPkey := (flags and sendPGPkey)<>0;
  flPGPreq := (flags and sendPGPreq)<>0;
  flPGPsig := (flags and sendPGPsig)<>0;
  flNokop  := (flags and sendNokop )<>0;
//flIQuote := (flags and sendIQuote)<>0;
//flMPart  := (flags and sendMPart )<>0;
end;
*)

procedure TSendUUData.MergeCharsets(nt:byte;NewCharsets:TStringList);
var i,i2: integer;
    n: TStringList;
    List: TStringList;    

  procedure Normalise(List:TStringList);
  var i: integer;
  begin
    for i:=List.Count-1 downto 0 do
      List[i] := MimeCharsetCanonicalName(List[i]);
  end;
    
begin
  List := FCharsets[ntClass(nt)];

  if List.Count<=0 then
  begin
    List.Assign(NewCharsets);
    Normalise(List);
  end else
  begin
    N := TStringList.Create;
   try
    N.Assign(NewCharsets);
    Normalise(N);
    N.Sorted := true;

    for i:= List.Count-1 downto 0 do
      if not N.Find(List[i],i2) then
        List.Delete(i);
   finally
    N.Free;
   end; 
  end;

  if List.Count<=0 then
    List.Add('US-ASCII');
end;

procedure TSendUUData.MergeMsgType(nt:byte;pm:boolean);
begin
  FHas[ntClass(nt),pm] := true;
  FHas[ncNone,pm] := true;
end;

{ -------------------------------------------------------------------- }

{ Note: This define is temporary until we have migrated all            }
{ subprocedures/subfunctions of DoSend to TUUSendData                  }
                                                                                 
{$DEFINE IN_TSENDUUDATA}
{$INCLUDE xpsendmessage_mime.inc}
{$INCLUDE xpsendmessage_subs.inc}
{$INCLUDE xpsendmessage_window.inc}  
{$INCLUDE xpsendmessage_create.inc}
{$INCLUDE xpsendmessage_uri.inc}

procedure TSendUUData.AddMessagePart(datei:string;temp,is_orig:boolean);
var pa     : TSendAttach_Part;
begin
  pa := TSendAttach_Part.Create;

  pa.FileName    := datei;
  pa.IsTemp	 := temp;
  pa.IsFile      := false;

  if is_orig and assigned(OrgHdp) then
  begin
    pa.FileCharset := OrgHdp.Charset;
    pa.FileEOL     := MimeEolCRLF;
    pa.ContentDisposition.AsString := iifs(OrgHdp.Mime.Disposition<>'',OrgHdp.Mime.Disposition,'inline');
    pa.ContentDescription := OrgHdp.Mime.Description;
    pa.ContentType.AsString := iifs(OrgHdp.Mime.CType<>'',OrgHdp.Mime.CType,'text/plain');
    pa.ContentEncoding := OrgHdp.Mime.Encoding;
  end else
  begin
    pa.FileCharset := 'IBM437';
    pa.FileEOL     := MimeEolCRLF;
    pa.ContentDisposition.DispoType := MimeDispositionInline;
    pa.ContentEncoding := MimeEncoding7Bit;
    pa.ContentType.AsString := 'text/plain';
  end;

  SendAttach_Analyze(pa,not is_orig,SigData,nt_uucp,docode,flPGPSig);

  parts.Insert(0,pa);
end;

procedure TSendUUData.AddFilePart(datei:string;temp:boolean);
var pa     : TSendAttach_Part;
begin
  pa := TSendAttach_Part.Create;

  pa.FileName    := datei;
  pa.IsTemp	   := temp;
  pa.IsFile      := true;

  SendAttach_Analyze(pa,true,'',nt_uucp,docode,flPGPSig);

  parts.Insert(0,pa);
end;

procedure TSendUUData.EditNachricht(pushpgdn:boolean);
var p      : byte;
    s0,
    s1,
    s2,
    s3,
    s4     : string;
    edpush : boolean;
    t      : Taste;
    str    : TStream;
begin
  MIMEDecompose;
  edpush:=not editvollbild and ((exteditor=1) or (VarEditor='') or (VarEditor[1]='*'));
  
  if edpush then 
  begin
    attrtxt(col.coledithead);
    moff;
    // Wegen der Fensterbehandlung wpush auf den gesamten Bereich anwenden
    wpush(1,ScreenWidth,1,ScreenLines,'-');          { 'Nachricht an  ' / 'Nachricht in  ' }
    p:=cpos('@',Empf1Address);

    if(FirstChar(Empf1Address)=#4)or(p>0)then
      s0:=GetRes2(611,40)  { 'Nachricht an ' }
    else
      s0:=GetRes2(611,41); { 'Nachricht in ' }
      
    if(FirstChar(Empf1Address)=#4)then
      s1:=vert_name(Empf1Address)
    else 
      s1:=iifs(Empf1RealName<>'',Empf1RealName,Empf1Address);

    s2:=GetRes2(611,45); { '...' }

    if (EmpfList.Count)>=2 then
      s3:=GetRes2(611,44)  { 'u. a.' }
    else
      s3:='';

    if fidoto<>'' then
      s4:=GetRes2(611,43)  { ' an ' } + fidoto
    else
      s4:='';

    if Length(s0)+Length(s1)+Length(s3)+Length(s4)>screenwidth-1 then
    begin
      SetLength(s4,max(10,screenwidth-1-Length(s0)-Length(s1)-Length(s3)-Length(s2)));
      s4 := s4 + s2;
    end;

    if Length(s0)+Length(s1)+Length(s3)+Length(s4)>screenwidth-1 then
    begin
      SetLength(s1,screenwidth-1-Length(s0)-Length(s4)-Length(s3)-Length(s2));
      s1 := s1 + s2;
    end;

    Wrt(1,1,FormS(' '+s0+s1+s3+s4,screenwidth));
    Wrt(1,2,' '+forms(getres2(611,42)+Subject,79 + screenwidth-80));   { 'Betreff:      ' }
    mon;
      
  end;
  if pushpgdn then pushkey(keycpgd);
  if exteditor<3 then EditSetBetreff(Subject,80);

  if (parts.count<=0) or not TSendAttach_Part(parts[0]).IsMessage then
  begin
    s0 := TempS($FFFF);
    str := TFileStream.Create(s0,fmCreate);
    writeln_s(str,HeadData); str.Free;    
    addMessagePart(s0,true,false);
  end;

  SendAttach_EditText(TSendAttach_Part(parts[0]),true,umlaute=1,SigData,nt_uucp,docode,flPGPSig);

  if exteditor<3 then Subject:=EditGetbetreff;
  if edpush then begin
    moff; wpop; mon;
    end;
  if pushpgdn and keypressed then begin
    get(t,curoff);
    if t<>keycpgd then _keyboard(t);
   end;
  otherquotechars:=otherqcback; {evtl. mit 'Q' im Lister umgeschaltene Quotechars reseten }
end;
  
{ -------------------------------------------------------------------- }

function TSendUUData.GetHasPM: boolean;
begin result := FHas[ncNone,true]; end;

function TSendUUData.GetHasAM: boolean;
begin result := FHas[ncNone,false]; end;

function TSendUUData.GetHasNetz(nc: TNetClass): boolean;
begin result := FHas[nc,false] or FHas[nc,true]; end;

function TSendUUData.GetHasNetzPM(nc: TNetClass; pm: Boolean): boolean;
begin result := FHas[nc,pm]; end;

{ -------------------------------------------------------------------- }
                                                                        
initialization
  SendEmpfList := TStringList.Create;
  qMimePart := nil;
finalization
  SendEmpfList.Free;

{
  $Log$
  Revision 1.65  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.64  2002/08/28 18:48:54  mk
  JG:- fixed: Alt-B in editor sometimes doesn't work
    see <8Vf7E22S6pB.3.219@jochen.gehring.dialin.t-online.de>

  Revision 1.63  2002/08/09 22:24:07  cl
  - Fixed [ 587426 ] /»Unversandt funktioniert nicht

  Revision 1.62  2002/08/09 22:17:39  cl
  - Fixed #588187 3.9: keine UseNet Postings

  Revision 1.61  2002/07/28 11:31:46  cl
  - BUGFIX: [ 587626 ] 3.9: EBs verschandeln Subject
  - BUGFIX: [ 587388 ] 3.9: EBs gehen nicht immer

  Revision 1.60  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.59  2002/07/20 15:35:51  cl
  - BUGFIX: Betrefffeld beim Editieren zu kurz.

  Revision 1.58  2002/07/09 13:37:20  mk
  - merged forcebox-fixes from OpenXP/16 (sv+my), most obsolte due to new adress handling

  Revision 1.57  2002/06/23 15:30:41  cl
  - Fixed List Index Error

  Revision 1.56  2002/06/23 15:03:06  cl
  - Adapted Nachricht/Direkt to new address handling.

  Revision 1.55  2002/06/15 08:55:34  mk
  - fixed range check error: betreflen is now integer instead of byte

  Revision 1.54  2002/06/13 17:09:23  mk
  - reraise exceptions in DoSend to give debug info with line information

  Revision 1.53  2002/06/12 09:14:53  mk
  - removed some length limits including AdressLength (for RFC nets only)

  Revision 1.52  2002/05/26 12:26:12  ma
  - using "email" db field instead of "user" db field for email now
    email may be longer than 30 chars now
    EMAIL ADDRESS HAS TO BE RE-ENTERED IN SERVER SETTINGS

  Revision 1.51  2002/05/20 15:23:10  cl
  - BUGFIX: subject truncated (preliminiary fix)

  Revision 1.50  2002/05/09 15:18:06  cl
  - fixed internal messages

  Revision 1.49  2002/04/14 22:33:10  cl
  - New address handling, supports To, CC, and BCC
  - Nearly complete rewrite of DoSend's message creation
  - Added TAddress and TAddressList
  - Moved many local variables from DoSend into TSendUUData fields

  Revision 1.48  2002/04/13 19:01:13  ms
  Changed the RFCAppendOldSubject to that it works for all RFC net types now.

  Revision 1.47  2002/04/07 10:08:33  mk
  - fixed crash when sending messages with 0 byte length

  Revision 1.46  2002/03/03 11:25:24  mk
  - fixed cc bug

  Revision 1.45  2002/02/21 13:52:34  mk
  - removed 21 hints and 28 warnings

  Revision 1.44  2002/02/18 16:59:41  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.43  2002/02/13 18:19:53  mk
  - improvements for THeader and ClrUVS

  Revision 1.42  2002/02/07 20:21:12  cl
  - fixed sending of binary files

  Revision 1.41  2002/02/06 09:45:02  mk
  MA:- fixed new empfaenger handling

  Revision 1.40  2002/02/01 10:31:55  mk
  - fixed some bugs with new empfaenger handling
  - made DomainList to StringList

  Revision 1.39  2002/01/21 23:30:13  cl
  - post-3.40 merge fixes

  Revision 1.38  2002/01/21 22:45:48  cl
  - fixes after 3.40 merge

  Revision 1.37  2002/01/13 15:15:54  mk
  - new "empfaenger"-handling

  Revision 1.36  2002/01/13 15:07:32  mk
  - Big 3.40 Update Part I

  Revision 1.35  2002/01/06 19:41:18  ma
  - changed variable name

  Revision 1.34  2002/01/06 16:33:25  ma
  - ported "append old subject" feature from OpenXP/16 (JG+MY)

  Revision 1.33  2002/01/05 16:01:10  mk
  - changed TSendUUData from record to class

  Revision 1.32  2001/12/25 20:28:15  cl
  - fixed RangeCheckError

  Revision 1.31  2001/12/23 12:02:45  mk
  - fixes GetInf, now crossposts are working again

  Revision 1.30  2001/12/08 09:23:03  mk
  - create list of MIME parts dynamically

  Revision 1.29  2001/11/06 12:48:43  ml
  - fix for 2 range-check-errors

  Revision 1.28  2001/10/28 15:40:38  ma
  - Fido mailer header uses standard format

  Revision 1.27  2001/10/26 11:37:36  ma
  - use YEAR-MONTH-DAY
}
end.


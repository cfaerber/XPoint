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
  sysutils,Classes,
  typeform,fileio,inout,keys,datadef,database,maske,crc,lister, osdepend,
  winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,xp2c,xp_des,xpe, xpheader,
  xpglobal,xpsendmessage_attach,xpsendmessage_attach_analyze,xpmime,
  addresslist,xpnt,
  xprope,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  fidoglob;

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

var
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
//  followup   : TStringlist;
    References : TStringList;
    keywords   : string;
    summary    : string;
    distribute : string;
    oab, wab: string;
    OEM: TStringList;
    oar,war    : string;
    onetztyp   : eNetz;

  { -- Original Message(s) ------------------------------------------- }
  private
    FParentHdp : THeader;
    FOrgHdp    : THeader;
    function GetParentHdp: THeader;
    function GetOrgHdp: THeader;
    
  public
    { Contains information about the message for which a followup or reply
      is being sent. }
    property ParentHdp  : THeader read GetParentHdp write FParentHdp;

    { Contains information about the original message, which is being
      replaced, superseded, forwared, etc. }    
    property OrgHdp     : THeader read GetOrgHdp    write FOrgHdp;
    
  public
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
    procedure MergeCharsets(nt:eNetz;NewCharsets:TStringList);
    procedure MergeMsgType(nt:eNetz;pm:boolean);

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
    procedure AddMessagePart(const datei:string;temp,is_orig:boolean);
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
  xp3ex, xp3o2, xp4e, xpcc, xpfido, xpmakeheader,
  xpsendmessage_internal, xpstreams, addresses, 
  xpserver, xp4;

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
    PartsEx := true;
    AddMessagePart(fn,temp,true);
  end;
end;

{ -------------------------------------------------------------------- }
{ -- TSendUUData ----------------------------------------------------- }
{ -------------------------------------------------------------------- }

{ TSendUUData is a class that represents a message being sent. It      }
{ replaces the DoSend procedure.                                       }

constructor TSendUUData.Create;
var n: TNetClass;
begin
  EmpfList := TAddressList.Create;
//Followup := TStringlist.Create;
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
//FollowUp.Free;
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
//followup.Clear;
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
  onetztyp := nt_Netcall;
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

procedure TSendUUData.MergeCharsets(nt:eNetz;NewCharsets:TStringList);
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

procedure TSendUUData.MergeMsgType(nt:eNetz;pm:boolean);
begin
  FHas[ntClass(nt),pm] := true;
  FHas[ncNone,pm] := true;
end;

{ -------------------------------------------------------------------- }

{ Note: This define is temporary until we have migrated all            }
{ subprocedures/subfunctions of DoSend to TUUSendData                  }
                                                                                 
{$INCLUDE xpsendmessage_mime.inc}
{$INCLUDE xpsendmessage_subs.inc}
{$INCLUDE xpsendmessage_window.inc}  
{$INCLUDE xpsendmessage_create.inc}
{$INCLUDE xpsendmessage_uri.inc}

procedure TSendUUData.AddMessagePart(const datei:string;temp,is_orig:boolean);
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

function TSendUUData.GetParentHdp: THeader;
begin
  result := FParentHdp;
  if not assigned(result) then result := FOrgHdp;
end;
    
function TSendUUData.GetOrgHdp: THeader;
begin
  result := FOrgHdp;
  if not assigned(result) then result := FParentHdp;
end;
    

{ -------------------------------------------------------------------- }
                                                                        
initialization
  SendEmpfList := TStringList.Create;
  qMimePart := nil;
finalization
  SendEmpfList.Free;

{
  $Log$
  Revision 1.73  2003/05/01 10:06:18  mk
  - added const parameter to AddMessagePart

  Revision 1.72  2003/04/28 20:18:57  cl
  - CRLF at the end of a text file is now uniformly handled as the start of
    an additional line.

  Revision 1.71  2003/01/13 22:48:51  cl
  - enabled TRopeStream

  Revision 1.70  2003/01/13 22:14:28  cl
  - send window rewrite IIa - cleanups

  Revision 1.69  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.68  2002/12/21 05:38:02  dodi
  - removed questionable references to Word type

  Revision 1.67  2002/12/14 07:31:39  dodi
  - using new types

  Revision 1.66  2002/12/12 11:58:51  dodi
  - set $WRITEABLECONT OFF

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


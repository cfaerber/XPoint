{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the Lesser GNU General Public License (LGPL) as
   published by the Free Software Foundation; either version 2,
   or (at your option) any later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LGPL
   for more details.

   You should have received a copy of the LGPL along with this
   software; see the file lgpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on December, 03th 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
   Copyright (c) 2000 by the OpenXP Team.

}
{$I xpdefine.inc }

{ Contains class THeader }

{ Headerdefinitionen, die auch von den Tools genutzt werden }

unit xpheader;

interface

uses Classes,Mime,addresslist,xpnt;

type
  mimedata = record
    mversion:    string;                  { MIME-Version              }
    ctype:       string;
    encoding:    TMimeEncoding;
    disposition: string;
    description: string;
    cid:         string;
  end;

  THeader = class
  private
    function GetFirstEmpfaenger: String;
    procedure SetFistEmpfaenger(const Value: String);
    function GetTypChar: Char;

  public // TODO: should be private when makeheader is made a method!
    FTo,FCC,FBCC: string;   
    FReplyTo,FMailFollowupTo: string;
    FMailReplyTo: string;
  private
//  function GetTo: string;         procedure SetTo(s:string);
//  function GetCC: string;         procedure SetCC(s:string);
//  function GetBCC: string;        procedure SetBCC(s:string);
//  function GetReplyto: string;    procedure SetReplyTo(s:string);
//  function GetMailFollowupTo: string; procedure SetMailFollowupTo(s:string);
    function GetNewsgroups: string; procedure SetNewsgroups(s:string);
    function GetFollowupTo: string; procedure SetFollowupTo(s:string);

  public
    netztyp: eNetz; //byte;             { --- intern ----------------- }
    archive: boolean;                   { archivierte PM               }
    attrib: word;                       { Attribut-Bits                }
    filterattr: word;                   { Filter-Attributbits          }

  { -- Envelope-Empfänger/ZConnect-Header ---------------------------- }
    Empfaenger:   TStringList;          { EMP:                         }
    Kopien:       TStringList;          { KOP: = bereits versendet     }
    
    DiskussionIn: TStringList;          { Diskussion-In:               }
    AntwortAn:    TStringList;          { Antwort-An:                  }

  { -- Informative Empfänger (RFC) ----------------------------------- }
    property UTo: string read FTo  write FTo;
    property CC:  string read FCC  write FCC;
    property BCC: string read FBCC write FBCC;
    
    property UReplyTo: string read FReplyTo write FReplyTo;
    property UMailReplyTo: string read FMailReplyTo write FMailReplyTo;
    property UMailFollowupTo: string read FMailFollowupTo write FMailFollowupTo;
    
    property Newsgroups: string read GetNewsgroups write SetNewsgroups;
    property FollowupTo: string read GetFollowupTo write SetFollowupto;

  { -- ReconstructEnvelope ------------------------------------------- }
    procedure MakeEnvelopeFromRFCHeaders;
    procedure MakeRFCHeadersFromEnvelope;

  public
    betreff: string;
    absender: string;
    datum: string;                      { Netcall-Format               }
    zdatum: string;                     { ZConnect-Format; nur auslesen }
    orgdate: boolean;                   { Ausnahme: zdatum schreiben   }
    pfad: string;                       { Netcall-Format               }
    msgid: string;                      { ohne <>                      }
    ersetzt: string;                    { ohne <>                      }
    typ: string;                        { T / B                        }

    crypt: record
      method: string;
      typ: string;
      charset: string;
      komlen: Integer;
    end;

    charset: string;
    groesse: longint;
    realname: string;
    programm: string;                   { Mailer-Name }
    organisation: string;
    postanschrift: string;
    telefon: string;
    homepage: string;
    komlen: longint;                    { --- ZCONNECT --- Kommentar-Laenge }
    datei: string;                      { Dateiname                  }
    ddatum: string;                     { Dateidatum, jjjjmmtthhmmss }
    prio: byte;                         { 10=direkt, 20=Eilmail      }
    error: string;                      { ERR-Header              }
    oem: TStringList;
    oab, wab: string;
    oar, war: string;                   { Realnames }
    real_box: string; { --- Maggi --- falls Adresse = User@Point }
    hd_point: string;                   { eigener Pointname }
    pm_bstat: string;                   { --- Maus --- Bearbeitungs-Status }
    org_msgid: string;
    org_xref: string;
    ReplyPath: string;
    ReplyGroup: string;                 { Kommentar-zu-Gruppe          }
    fido_to: string;                    { --- Fido ------------------- }
    x_charset: string;                  { --- RFC -------------------- }
    keywords: string;
    summary: string;
    expiredate: string;                 { Expires / LDA }
    priority: byte;                     { Priority: 1, 3, 5 }
    distribution: string;
    pm_reply: boolean;                  { Followup-To: poster }
    quotestring: string;
    empfbestto: string;
    pgpflags: word;                     { PGP-Attribut-Flags           }
    pgp_uid: string;                    { alternative Adresse          }
    vertreter: string;
    XPointCtl: longint;
    boundary: string;                   { MIME-Multipart-Boundary      }
    gate: string;
    xnoarchive: boolean;
    Cust1, Cust2: string;
    control: string;
    uline: TStringList;
    xline: TStringList;                 // X-Zeilen, die 'uebrig' sind
    zline: TStringList;
    fline: TStringList;
    nokop: boolean;
    References: TStringList;            // references:
    MIME: mimedata;
    gateway: string;
    sender: string;
    lines: longint;                     { "Lines:" }
    XPMode: string;

    ListID, ListPost,
    ListSubscribe,
    ListUnSubscribe,
    ListHelp,
    ListOwner,
    ListArchive: string;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function GetLastReference: String;
    // get managled Message ID
    function BinaryMsgId: string;

  { -- Flags --------------------------------------------------------- }    


  { -- Write/Read ---------------------------------------------------- }
    procedure WriteToStream(stream:TStream);
    procedure WriteZ38(stream:TStream);
    procedure WriteZConnect(stream:TStream);
//  procedure WriteRFC(stream:TStream);

//  procedure ReadZ38(stream:TStream);
    procedure ReadZConnect(stream:TStream);
//  procedure ReadRFC(stream:TStream);

    property FirstEmpfaenger: String read GetFirstEmpfaenger write SetFistEmpfaenger;
    property TypChar: Char read GetTypChar;
  end;

implementation

uses
  SysUtils,Typeform,xp0,xpdatum,xp_pgp,xpmakeheader,xpstreams;

constructor THeader.Create;
begin
  inherited Create;
  Empfaenger := TStringList.Create;
  Kopien := TStringList.Create;
// CC := TStringList.Create;
// BCC := TStringList.Create;
  ULine := TStringList.Create;
  XLIne := TStringList.Create;
  fLine := TStringList.Create;
  zLIne := TStringList.Create;
  DiskussionIn := TStringList.Create;
  AntwortAn    := TStringList.Create;
//MailCopies := TStringList.Create;
//MailCopies.Duplicates := dupIgnore;
  References := TStringList.Create;
  References.Duplicates := dupIgnore;
// XEmpf := TStringList.Create;
  OEM := TStringList.Create;
// XOEM := TStringList.Create;
  Clear;
end;

procedure THeader.Clear;
begin
  netztyp := nt_Netcall;
  archive := false;
  attrib := 0;
  filterattr := 0;
  Empfaenger.Clear;
  Kopien.Clear;
//  CC.Clear;
//  BCC.Clear;
  FTo := '';
  FCC := '';
  FBCC:= '';

  FReplyTo := '';
  FMailReplyTo := '';
  FMailFollowupTo := '';
  
  Nokop := false;

  betreff := '';
  absender := '';
  datum := '';
  zdatum := '';
  orgdate := false;
  pfad := '';
  msgid := '';
  ersetzt:= '';                    { ohne <>                      }
  typ:= '';                        { T / B                        }
  with crypt do
  begin
    method:='';
    typ:= '';                   { '' / T / B                   }
    charset:= '';
    komlen := 0;
  end;
  charset:='';
  groesse := 0;
  realname:= '';
  programm:= '';                   { Mailer-Name }
  organisation:= '';
  postanschrift:= '';
  telefon:= '';
  homepage:= '';
  DiskussionIn.Clear;;
  AntwortAn.Clear;
  komlen := 0;
  datei:= '';                      { Dateiname                  }
  ddatum:= '';                     { Dateidatum, jjjjmmtthhmmss }
  prio := 0;
  error:= '';                      { ERR-Header              }
  oem.Clear;
  oab:= '';
  wab:= '';
  oar:= '';
  war:= '';                   { Realnames }
  real_box:= ''; { --- Maggi --- falls Adresse = User@Point }
  hd_point:= '';                   { eigener Pointname }
  pm_bstat:= '';                   { --- Maus --- Bearbeitungs-Status }
  org_msgid:= '';
  org_xref:= '';
  ReplyPath:= '';
  ReplyGroup:= '';                 { Kommentar-zu-Gruppe          }
  fido_to:= '';                    { --- Fido ------------------- }
  x_charset:= '';                  { --- Fremdformate ----------- }
  keywords:= '';
  summary:= '';
  expiredate:= '';                 { Expires / LDA }
  priority := 0;
  distribution:= '';
  pm_reply := false;
  quotestring:= '';
  empfbestto:= '';
  pgpflags := 0;
  pgp_uid:= '';                    { alternative Adresse          }
  vertreter:= '';
  XPointCtl := 0;
  nokop:= false;
  boundary:= '';                   { MIME-Multipart-Boundary      }
  gate:= '';
  xnoarchive:= false;;
  Cust1 := '';
  Cust2:= '';
  control:= '';
  uline.clear;
  xline.clear;                    // X-Zeilen, die 'uebrig' sind
  zline.clear;
  fline.clear;
  References.Clear;
// xempf.clear;
//mailcopies.clear;
// xoem.clear;
  gateway:= '';
  sender:= '';
  lines := 0;
  with mime do
  begin
    mversion := '';
    encoding := MimeEncodingUnknown;
    CType :='';
    Disposition :='';
    CID := '';
    Description := '';
  end;

  ListID := '';
  ListPost := '';
  ListSubscribe := '';
  ListUnSubscribe := '';
  ListHelp := '';
  ListOwner := '';
  ListArchive := '';
end;

destructor THeader.Destroy;
begin
  Empfaenger.Free;
//  CC.Free;
//  BCC.Free;
  Kopien.Free;
  ULine.Free;
  XLine.Free;
  fLine.Free;
  zLine.Free;
  DiskussionIn.Free;
  AntwortAn.free;
  References.Free;
// XEmpf.Free;
  OEM.Free;
// XOEM.Free;
  inherited destroy;
end;

function THeader.GetLastReference: String;
begin
  if References.Count>0 then
    Result := References[References.Count-1]
  else
    Result := '';
end;

procedure THeader.WriteZConnect(stream:TStream);
//procedure WriteHeader(var hd:theader; var f:file);

  procedure WriteStichworte(keywords:string);
  var p  : byte;
      stw: string[60];
  begin
    while keywords<>'' do begin
      p:=cpos(',',keywords);
      if p=0 then p:=length(keywords)+1;
      stw:=trim(LeftStr(keywords,p-1));
      if stw<>'' then writeln_s(stream,'Stichwort: '+stw);
      delete(keywords,1,p);
      end;
  end;

  function PMEmpfAnz: Integer;
  var
    i: Integer;
  begin
    Result := 0;  
    for i := 0 to Empfaenger.Count - 1 do
      if IsMailAddr(Empfaenger[i]) then
        Inc(Result);
  end;

//  procedure WriteZheader;
  var
    i: Integer;
    s: String;
    gb : boolean;
    mtype : TMimeContentType;
    mdisp : TMimeDisposition;
    mtype_iscomposed : boolean;
  begin
//  with hd do begin
      if not orgdate then
        if replaceetime then
          zdatum:=iifs(ival(LeftStr(datum,2))<70,'20','19')+datum+'00W+0'
        else
                  ZtoZCdatum(datum,zdatum);
      gb:=ntGrossBrett(netztyp) or (netztyp=nt_ZConnect);
      if gb and (not IsMailAddr(FirstEmpfaenger)) and (LeftStr(FirstEmpfaenger,2)<>'/¯') then
        FirstEmpfaenger := UpperCase(Firstempfaenger);
      if nokop and (pmempfanz>1) then
        writeln_s(stream,'STAT: NOKOP');

      for i := 0 to Empfaenger.Count - 1 do
      begin
        s := Empfaenger[i];
        if gb and (not IsMailAddr(s)) then
          s := UpperCase(s);
        writeln_s(stream,'EMP: '+ s);
      end;

{      if gb and (cpos('@',AmReplyTo)=0) then
        UpString(AmReplyTo);}
      for i := 0 to OEM.Count - 1 do
        writeln_s(stream,'OEM: '+ OEM[i]);
      for i := 0 to Kopien.Count - 1 do
        writeln_s(stream,'KOP: '+ Kopien[i]);
      writeln_s(stream,'ABS: '+absender+iifs(realname='','',' ('+realname+')'));
      if oab<>'' then writeln_s(stream,'OAB: '+oab+iifs(oar='','',' ('+oar+')'));
      if wab<>'' then writeln_s(stream,'WAB: '+wab+iifs(war='','',' ('+war+')'));

      if fto<>'' then writeln_s(stream,'U-TO: '+fto);
      if fcc<>'' then writeln_s(stream,'U-CC: '+fcc);

      for i:=0 to AntwortAn.Count-1 do
        writeln_s(stream,'ANTWORT-AN: '+AntwortAn[i]);
      for i:=0 to DiskussionIn.Count-1 do
        writeln_s(stream,'DISKUSSION-IN: '+DiskussionIn[i]);

      if FReplyTo       <>'' then writeln_s(stream,'U-REPLY-TO: '        +FReplyTo);
      if FMailReplyTo   <>'' then writeln_s(stream,'U-MAIL-REPLY-TO: '   +FMailReplyTo);
      if FMailFollowupTo<>'' then writeln_s(stream,'U-MAIL-FOLLOWUP-TO: '+FMailFollowupTo);
      
      writeln_s(stream,'BET: '+betreff);
      writeln_s(stream,'EDA: '+zdatum);
      writeln_s(stream,'MID: '+msgid);

      for i := 0 to References.Count - 1 do
        writeln_s(stream,'BEZ: '+ References[i]);

      if ersetzt<>'' then writeln_s(stream,'ERSETZT: '+ersetzt);

      if (attrib and attrControl<>0) and (netztyp=nt_ZConnect) then
      begin
        writeln_s(stream,'STAT: CTL');
        writeln_s(stream,'CONTROL: '+betreff);
      end;
      writeln_s(stream,'ROT: '+pfad);

      if typ='B'       then writeln_s(stream,'TYP: BIN') else
      if (typ='M')and(netztyp in [nt_ZConnect]) then writeln_s(stream,'TYP: MIME');
      if datei<>''     then writeln_s(stream,'FILE: ' +LowerCase(datei));
      if ddatum<>''    then writeln_s(stream,'DDA: '  +ddatum+'W+0');
      if error<>''     then writeln_s(stream,'ERR: '  +error);
      if programm<>''  then writeln_s(stream,'MAILER: '+programm);
      if prio<>0       then writeln_s(stream,'PRIO: '  +strs(prio));
      if organisation<>'' then writeln_s(stream,'ORG: '+organisation);
      if attrib and attrReqEB<>0 then
//      if wab <> ''     then writeln_s(stream,'EB: ' + wab) else
//      if ReplyTo <> '' then writeln_s(stream,'EB: ' + replyto)
//      else
          writeln_s(stream,'EB:');
      if attrib and attrIsEB<>0  then writeln_s(stream,'STAT: EB');
      if pm_reply                then writeln_s(stream,'STAT: PM-REPLY');

      charset:=MimeCharsetToZC(charset);
      if (charset<>'') and (charset<>'US-ASCII') and (charset<>'IBM437') then writeln_s(stream,'CHARSET: '+charset);
      
      if postanschrift<>''       then writeln_s(stream,'POST: '+postanschrift);
      if telefon<>''   then writeln_s(stream,'TELEFON: '+telefon);
      if homepage<>''  then writeln_s(stream,'U-X-Homepage: '+homepage);
      if priority<>0   then writeln_s(stream,'U-X-Priority: '+strs(priority));
      if xnoarchive then
        writeln_s(stream,'U-X-No-Archive: Yes');
      if keywords<>''  then WriteStichworte(keywords);
      if summary<>''   then writeln_s(stream,'Zusammenfassung: '+summary);
      if distribution<>'' then writeln_s(stream,'U-Distribution: '+distribution);
      if ersetzt<>''   then writeln_s(stream,'ERSETZT: '+ersetzt);

      if pgpflags<>0 then begin
        if pgpflags and fPGP_avail<>0    then writeln_s(stream,'PGP-Key-Avail:');
        if pgpflags and fPGP_signed<>0   then writeln_s(stream,'SIGNED: PGP');
        if pgpflags and fPGP_clearsig<>0 then writeln_s(stream,'SIGNED: PGPCLEAR');
        if pgpflags and fPGP_please<>0   then writeln_s(stream,'PGP: PLEASE');
        if pgpflags and fPGP_request<>0  then writeln_s(stream,'PGP: REQUEST');
        if pgpflags and fPGP_haskey<>0   then WritePGPkey_header(stream);
        if pgpflags and fPGP_sigok<>0    then writeln_s(stream,'X-XP-PGP: SigOk');
        if pgpflags and fPGP_sigerr<>0   then writeln_s(stream,'X-XP-PGP: SigError');
      end;

      if attrib and AttrQPC<>0   then writeln_s(stream,'CRYPT: QPC') else
      if attrib and AttrPmcrypt<>0 then writeln_s(stream,'CRYPT: PMCRYPT2') else
      if pgpflags and fPGP_encoded<>0  then writeln_s(stream,'CRYPT: PGP') else
      if crypt.method<>'' then writeln_s(stream,'CRYPT: '+crypt.method);

      if crypt.typ='B' then writeln_s(stream,'Crypt-Content-TYP: BIN') else
      if crypt.typ='M' then writeln_s(stream,'Crypt-Content-TYP: MIME');
      crypt.charset:=MimeCharsettoZC(crypt.charset);
      if (crypt.charset<>'') and (crypt.charset<>'US-ASCII') and (crypt.charset<>'IBM437') then 
        writeln_s(stream,'Crypt-Content-KOM: '+strs(crypt.komlen));

      if ntConv(netztyp) then begin
        writeln_s(stream,'X_C:');
        writeln_s(stream,'X-XP-NTP: '+strs(ord(netztyp)));
        if x_charset<>'' then writeln_s(stream,'X-XP-Charset: '+x_charset);
        if real_box<>''  then writeln_s(stream,'X-XP-BOX: '+real_box);
        if hd_point<>''  then writeln_s(stream,'X-XP-PNT: '+hd_point);
        if pm_bstat<>''  then writeln_s(stream,'X-XP-BST: '+pm_bstat);
        if attrib<>0     then writeln_s(stream,'X-XP-ATT: '+hex(attrib,4));
        if ReplyPath<>'' then writeln_s(stream,'X-XP-MRP: '+replypath);
        if ReplyGroup<>''then writeln_s(stream,'X-XP-RGR: '+replygroup);
        if org_msgid<>''  then writeln_s(stream,'X-XP-ORGMID: '+org_msgid);
        if org_xref<>''  then writeln_s(stream,'X-XP-ORGREF: '+org_xref);
        end;
      if fido_to<>''   then writeln_s(stream,'F-TO: '+fido_to);
      if boundary<>''  then writeln_s(stream,'X-XP-Boundary: '+boundary);

      if (Boundary<>'') or (Mime.CType<>'') then
      begin
        mtype := TMimeContentType.Create(iifs(Mime.CType<>'',Mime.Ctype,'multipart/mixed'));
        mtype_iscomposed := mtype.IsComposed;

        if boundary <>'' then mtype.boundary := boundary;
        if x_charset<>'' then mtype.charset := x_charset;
        writeln_s(stream,'U-Content-Type: '+mtype.AsString);
        if ((typ='M') and (netztyp in [nt_Zconnect])) or mtype_iscomposed then
          writeln_s(stream,'MIME-Type: '+mtype.AsString);
        mtype.Free;
      end else
        mtype_iscomposed:=false;

      if (datei<>'') or (Mime.Disposition<>'') then
      begin
        mdisp := TMimeDisposition.Create(Mime.Disposition);
        if Mime.Disposition='' then mdisp.Verb := iifs(typ='B','attachment','inline');
        if length(mdisp.ParamValues['filename'])>0 then
          with mdisp.Params['filename'] do begin
            Value:=datei;
            Charset:='IBM437';
          end;
        writeln_s(stream,'U-Content-Disposition: '+mdisp.AsString);
        mdisp.Free;
      end;

      if (typ='M') or ntConv(netztyp) or mtype_iscomposed then
      if Mime.Encoding<>MimeEncodingUnknown then 
      begin
        case Mime.Encoding of
          MimeEncodingBinary: s:='binary';
          MimeEncoding7Bit:   s:='7bit';
          MimeEncoding8Bit:   s:='8bit';
          MimeEncodingQuotedPrintable: s:='quoted-printable';
          MimeEncodingBase64: s:='base64';
        end;

        if (typ='M') or ntConv(netztyp) or mtype_iscomposed then
          writeln_s(stream,'U-Content-Transfer-Encoding: '+s);
        if (typ='M') or mtype_iscomposed then
          writeln_s(stream,'MIME-Encoding: '+s);
      end;

      if archive then writeln_s(stream,'X-XP-ARC:');
      if xpointctl<>0  then writeln_s(stream,'X-XP-CTL: '+strs(XpointCtl));
      writeln_s(stream,'LEN: '+strs(groesse));
      if komlen>0 then writeln_s(stream,'KOM: '+strs(komlen));

      if ListID <> '' then writeln_s(stream,'U-List-ID: '+ListID); 
      if ListPost <> '' then writeln_s(stream,'U-List-Post: '+ListPost); 
      if ListSubscribe <> '' then writeln_s(stream,'U-List-Subscribe: '+ListSubscribe); 
      if ListUnSubscribe <> '' then writeln_s(stream,'U-List-UnSubscribe: '+ListUnSubscribe); 
      if ListHelp <> '' then writeln_s(stream,'U-List-Help: '+ListHelp); 
      if ListOwner <> '' then writeln_s(stream,'U-List-Owner: '+ListOwner); 
      if ListArchive <> '' then writeln_s(stream,'U-List-Archive: '+ListArchive); 

      for i := 1 to ULine.Count -1 do
        writeln_s(stream,Uline[i]);
      for i := 1 to xLine.Count -1 do
        writeln_s(stream,xline[i]);

      writeln_s(stream,'');
//  end;
  end;

procedure THeader.WriteZ38(stream:TStream);

  // replace domains with ".ZER" for ZConnect
  // don't ask me why, this was in xpsendmessage.pmEncryt...

  // NB: This only has an effecht if WriteZ38 is
  // called directly, otherwise, WriteToStream will
  // call WriteZConnect anyway!

  function AddZer(const s:string):string;
  var  p,p2: byte;
  begin
    Result:=s;
    p:=cpos('@',Result);
    p2:=cPos('.',mid(Result,p+1));
    if p2>0 then result:=LeftStr(Result,p+p2)+'ZER';
  end;

begin
  if netztyp=nt_ZConnect then
    writeln_s(stream,AddZer(Firstempfaenger))
  else
    writeln_s(stream,Firstempfaenger);

  writeln_s(stream,LeftStr(betreff,40));

  if netztyp=nt_ZConnect then
    writeln_s(stream,AddZer(absender))
  else
    writeln_s(stream,absender);

  writeln_s(stream,datum);
  writeln_s(stream,pfad);
  writeln_s(stream,msgid);
  writeln_s(stream,typ);
  writeln_s(stream,strs(groesse));
end;

procedure THeader.WriteToStream(stream:TStream);
begin
  if ntZConnect(netztyp) then
    WriteZConnect(stream)
  else
    WriteZ38(stream);
end;

procedure THeader.ReadZConnect(stream:TStream);
begin
  xpmakeheader.makeheader(self,stream);
end;

function THeader.GetFirstEmpfaenger: String;
begin
  if Empfaenger.Count > 0 then
    Result := Empfaenger[0]
  else
    Result := '';
end;

function THeader.GetTypChar: Char;
begin
  result := iifc(typ='B','B','T');
end;

procedure THeader.SetFistEmpfaenger(const Value: String);
begin
  if Empfaenger.Count = 0 then
    Empfaenger.Add(Value)
  else
    Empfaenger[0] := Value;
end;

function THeader.BinaryMsgId: string;
begin
  Result := FormMsgId(MsgId);
end;

  function ZCBrettToRFC(const source:string):string;
  var i: integer;
  begin
    if FirstChar(source)='/' then
      Result := Mid(source,2)
    else
      Result := source;
    for i := 1 to Length(Result) do
      if Result[i]='/' then
        Result[i]:='.';
  end;

  function RFCBrettToZC(const source:string):string;
  var i: integer;
  begin
    Result := '/'+Source;
    for i := 2 to Length(Result) do
      if Result[i]='.' then
        Result[i]:='/';
  end;

function THeader.GetNewsgroups: string;
var i: integer;
begin
  { -- Construct Newsgroups out of EMP/KOP --------------------------- }
  result := '';
  for i:=0 to EMPfaenger.Count-1 do
    if not IsMailAddr(EMPfaenger[i]) then
      result:=result+ZCBrettToRFC(EMPfaenger[i])+',';
  for i:=0 to KOPien.Count-1 do
    if not IsMailAddr(KOPien[i]) then
      result:=result+ZCBrettToRFC(KOPien[i])+',';
  SetLength(result,Length(result)-1);
end;

function THeader.GetFollowupTo: string;
var i: integer;
begin
  if pm_reply then begin
    result := 'poster';
    exit;
  end;

  { -- Construct Newsgroups out of EMP/KOP --------------------------- }
  result := '';
  for i:=0 to DiskussionIn.Count-1 do
    if not IsMailAddr(EMPfaenger[i]) then
      result:=result+ZCBrettToRFC(DiskussionIn[i])+',';
  SetLength(result,Length(result)-1);
end;


procedure THeader.SetNewsgroups(s:string);
var n:   TSTringList;
    i,j: integer;
begin
  n := TStringList.Create;
 try

  { -- Build list of Newsgroups -------------------------------------- } 
  n.Sorted := true;

  while Length(s)>0 do
  begin
    i:=RightPos(',',s);
    n.Add(RFCBrettToZC(Mid(s,i+1)));
    SetLength(s,max(0,i-1));
  end;

  { -- Walk KOP ------------------------------------------------------ } 
  for i:=KOPien.Count-1 downto 0 do
    if not IsMailAddr(KOPien[i]) then      // ignore mail addreses
      if N.Find(KOPien[i],j) then          
        N.Delete(j)                     // no need to add
      else
        Kopien.Delete(i);               // no longer in Newsgroups
      
  { -- Walk EMP ------------------------------------------------------ }
  for i:=EMPfaenger.Count-1 downto 0 do
    if not IsMailAddr(EMPfaenger[i]) then  
      EMPfaenger.Delete(i);             // delete all Newsgroups

  { -- Add new Newsgroups to EMP ------------------------------------- }
  EMPfaenger.AddStrings(N);

 finally
  n.Free;
 end;
end;

procedure THeader.SetFollowupTo(s:string);
var n:   TSTringList;
    i,j: integer;
begin
  n := TStringList.Create;
 try
  pm_reply := (LowerCase(s) = 'poster');

  { -- Build list of Newsgroups -------------------------------------- } 
  n.Sorted := true;

  if not pm_reply then while Length(s)>0 do
  begin
    i:=RightPos(',',s);
    n.Add(RFCBrettToZC(Mid(s,i+1)));
    SetLength(s,max(0,i-1));
  end;

  { -- Walk DiskussionIn --------------------------------------------- } 
  for i:=DiskussionIn.Count-1 downto 0 do
    if not IsMailAddr(DiskussionIn[i]) then      // ignore mail addreses
      if N.Find(DiskussionIn[i],j) then          
        N.Delete(j)                           // no need to add
      else
        DiskussionIn.Delete(i);               // no longer in Newsgroups
      
  { -- Add new Newsgroups to EMP ------------------------------------- }
  DiskussionIn.AddStrings(N);

 finally
  n.Free;
 end;
end;

procedure THeader.MakeEnvelopeFromRFCHeaders;
var n:   TStringList;
    i,j: integer;
begin
  if (FTo<>'')or(FCC<>'')or(FBCC<>'') then
  begin
    n := TStringList.Create;
    try
      { -- Build list of Recipients ---------------------------------- } 
      n.Sorted := true;
      RFCReadAddressList(FTo, n,nil);
      RFCReadAddressList(FCC, n,nil);
      RFCReadAddressList(FBCC,n,nil);
      { -- Walk KOP -------------------------------------------------- } 
      for i:=KOPien.Count-1 downto 0 do
        if not IsMailAddr(KOPien[i]) then      // ignore mail addreses
          if N.Find(KOPien[i],j) then          
            N.Delete(j)                     // no need to add
        else
          Kopien.Delete(i);               // no longer in Recipients
      { -- Walk EMP -------------------------------------------------- }
      for i:=EMPfaenger.Count-1 downto 0 do
        if IsMailAddr(EMPfaenger[i]) then  
          EMPfaenger.Delete(i);             // delete all Mail Addresses
      { -- Add new Recipients to EMP --------------------------------- }
        EMPfaenger.AddStrings(N);
    finally
      n.Free;
    end;
  end;

  { -- Same with Antwort-An ------------------------------------------ }
  if (FMailReplyTo<>'')or(FReplyTo<>'')or(FMailFollowupTo<>'') then 
  begin
    AntwortAn.Clear;
    if FMailReplyTo<>'' then  
      RFCReadAddressList(FMailReplyto,AntwortAn,nil) 
    else
      RFCReadAddressList(FReplyto,    AntwortAn,nil);
  { -- Same with Diskussion-In --------------------------------------- }
    for i:=DiskussionIn.Count-1 downto 0 do
      if IsMailAddr(DiskussionIn[i]) then  
        DiskussionIn.Delete(i);             // delete all Mail Addresses
    RFCReadAddressList(FMailFollowupTo,DiskussionIn,nil);
  end;
end;

procedure THeader.MakeRFCHeadersFromEnvelope;
var i: integer;
begin
  if (Length(FTo)<=0) and (Length(FCC)<=0) and (Length(FBCC)<=0) then
  begin
    FTo := '';
    FBCC := '';

  { -- construct To out of EMP if STAT: BCC is not set --------------- }
  { -- construct BCC out of EMP if STAT: BCC is set ------------------ }
    if nokop then
    begin
      for i:=0 to EMPfaenger.Count-1 do
        if IsMailAddr(EMPfaenger[i]) then
          FBCC:=FBCC+EMPfaenger[i]+',';
      for i:=0 to KOPien.Count-1 do
        if IsMailAddr(KOPien[i]) then
          FBCC:=FBCC+KOPien[i]+',';
      SetLength(FBCC,Length(FBCC)-1);
    end else
    begin
      for i:=0 to Empfaenger.Count-1 do
        if IsMailAddr(Empfaenger[i]) then
          FTo:=FTo+Empfaenger[i]+',';
      for i:=0 to Kopien.Count-1 do
        if IsMailAddr(Kopien[i]) then
          FTo:=FTo+Kopien[i]+',';
      SetLength(FTo,Length(FTo)-1);
    end;
  end;

  { -- construct Reply-To out of ANTWORT-AN -------------------------- }
  if FReplyto = '' then begin
    for i:=0 to AntwortAn.Count-1 do
      if IsMailAddr(AntwortAn[i]) then
        FReplyto:=FReplyto+AntwortAn[i]+',';
    SetLength(FReplyto,Length(FReplyto)-1);
  end;

  { -- construct Mail-Followup-To out of DISKUSSION-IN --------------- }
  if FMailReplyTo = '' then begin
    for i:=0 to DiskussionIn.Count-1 do
      if IsMailAddr(DiskussionIn[i]) then
        FMailReplyTo:=FMailReplyTo+DiskussionIn[i]+',';
    SetLength(FMailReplyTo,Length(FMailReplyTo)-1);
  end;
end;


{
  $Log$
  Revision 1.40  2003/08/26 22:33:05  cl
  - added interface for THeader to read from TSTream objects

  Revision 1.39  2003/08/24 23:33:27  cl
  - Sendefenster: Priorität setzen (RFC), Keine Signatur (ohneSig),
    Nachricht löschen (nach Versand), Empfangsbestätigungen,
    X-No-Archive setzen
  - updated on-line help

  CLOSES:
    task #76791 Sendefenster: Empfangsbestätigungen
    task #76793 Sendefenster: ohne Sig
    task #76794 Sendefenster: Priorität
    task #76796 Sendefenster: Löschen

  Revision 1.38  2003/05/11 11:12:19  mk
  - use IsMailAddr when possible

  Revision 1.37  2003/04/25 21:11:19  mk
  - added Headeronly and MessageID request
    toggle with "m" in message view

  Revision 1.36  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.35  2002/12/14 07:31:37  dodi
  - using new types

  Revision 1.34  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.33  2002/09/13 12:09:28  cl
  - fixed last commit (too much changes comitted)

  Revision 1.32  2002/09/13 11:57:10  cl
  - added List-* fields

  Revision 1.31  2002/06/23 15:03:06  cl
  - Adapted Nachricht/Direkt to new address handling.

  Revision 1.30  2002/05/20 21:53:17  cl
  - Newsgroup property is converted to/from RFC format

  Revision 1.29  2002/05/20 15:20:17  cl
  - new address handling fixes

  Revision 1.28  2002/05/12 17:58:59  ma
  - fixed: Reply-To handling was broken if real name specified in Reply-To

  Revision 1.27  2002/04/14 22:33:10  cl
  - New address handling, supports To, CC, and BCC
  - Nearly complete rewrite of DoSend's message creation
  - Added TAddress and TAddressList
  - Moved many local variables from DoSend into TSendUUData fields

  Revision 1.26  2002/03/03 18:02:24  cl
  - FPC fix

  Revision 1.25  2002/03/03 15:52:36  cl
  - start for envelope/informative recipient handling

  Revision 1.24  2002/02/18 16:59:41  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.23  2002/02/13 18:19:53  mk
  - improvements for THeader and ClrUVS

  Revision 1.22  2002/02/06 21:26:20  mk
  MA:- do not clear empfaenger list after writeing the header

  Revision 1.21  2002/01/13 15:15:54  mk
  - new "empfaenger"-handling

  Revision 1.20  2002/01/05 16:01:10  mk
  - changed TSendUUData from record to class

  Revision 1.19  2001/09/25 21:12:06  cl
  - CRYPT-CONTENT-CHARSET is correctly converted to ZConnect charset names

  Revision 1.18  2001/09/15 00:08:31  cl
  - fixed initialization of THeader.Charset in THeader.Clear

  Revision 1.17  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.16  2001/09/08 23:30:26  cl
  - 'CHARSET:' only written if not in ['IBM437','US-ASCII'] (backwards compatibility fix)

  Revision 1.15  2001/09/08 20:59:50  cl
  - ZC header X-Charset/X-XP-Charset renamed to X-XP-Charset uniformly (X-Charset
    is still recognized for backwards compatibility).

  Revision 1.14  2001/09/08 16:29:39  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.13  2001/09/08 14:37:13  cl
  - cleaned up MIME-related fields in THeader
  - THeader can now write itsself to streams
  - Moved Stream functions to xpstreams.pas

  Revision 1.12  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.11  2001/08/11 21:20:52  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.10  2001/07/27 18:10:15  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.9  2001/06/04 17:31:37  ma
  - implemented role feature

  Revision 1.8  2001/04/17 22:20:18  ma
  - fixed: duplicated references were possible

  Revision 1.7  2001/04/10 17:38:01  mk
  - Stringlist Code cleanup

  Revision 1.6  2001/01/05 09:33:10  mk
  - removed THeader.Ref

  Revision 1.5  2001/01/02 15:47:33  mk
  - clear mimedata in THeader.Clear

  Revision 1.4  2001/01/02 10:05:27  mk
  - implemented Header.References

  Revision 1.3  2000/12/30 17:47:41  mk
  - renamed AddRef to References

  Revision 1.2  2000/12/30 17:36:54  mk
  *** empty log message ***

  Revision 1.1  2000/12/03 12:38:26  mk
  - Header-Record is no an Object

  Revision 1.9  2000/11/25 10:31:48  mk
  - some fixes for new SendUUData

  Revision 1.8  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.7  2000/11/17 19:35:45  fe
  Followup-To support updated to ZC 3.1.
  Mail-Copies-To support added.

  Revision 1.6  2000/11/09 18:15:12  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.5  2000/11/05 20:14:13  fe
  Added LDA/Expires.

  Revision 1.4  2000/09/21 16:22:21  mk
  - ZFido wieder compilierbar

  Revision 1.3  2000/07/21 13:23:48  mk
  - Umstellung auf TStringList

  Revision 1.2  2000/07/09 13:21:56  mk
  - UUZ nutzt jetzt xpheader.inc

  Revision 1.1  2000/07/09 09:09:56  mk
  - Newexit in Initialization/Finalization umgewandelt

}
end.

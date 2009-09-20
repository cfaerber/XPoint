{ $Id: uuzng_maus.pas,v 1.5 2003/11/10 10:31:52 cl Exp $

  UUZNG - OpenXP Message Converter

  This file is part of OpenXP.

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de>
  see CVS log below for authors

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I xpdefine.inc }

unit uuzng_maus;

interface

uses uuzng, classes, xpmessage, xpcharset;

type
  TMausTauschSpoolIn = class(TNetcallFileSpoolIn)
  private
    FTauschVersion, FBoxVersion, FBoxType: string;
    FMausName, FMausUser: string;
  public
    procedure CopyTo(Dest: TNetcallSpoolOut); override;
  end;

  TMausTauschSpoolOut = class(TNetcallFileSpoolOut)
  private
    FIsQuark: boolean;
    FIdCount: integer;
  public
    constructor Create(const FileName: string);
    procedure Put(msg: TXPMessage); override;
  private
    function OpenFile: TStream; override;
    procedure CloseFile; override;
// TODO: OpenFile/CloseFile zum Packen Ã¼berladen
    
  public    
    property IsQuark: boolean read FIsQuark write FIsQuark;
  end;

implementation

uses sysutils,
  addresses,
  typeform,
  mime,
  rfc2822,
  fidoglob,
  xp1,
  xprope,
  xpcharset_streams,
  xpstreams,
  xpstreams_codec;

const
  Domain = 'maus.de';

type
  TMausTauschAddColonStream = class(TCODECStream)
  private
    FLastWasCRLF, FLastWasCR: boolean;
  public
    constructor Create;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

procedure TMausTauschSpoolIn.CopyTo(Dest: TNetcallSpoolOut);
var InPos, Start, Stop: Int64;
    Size: Int64;
    Msg: TXPMessage;
    C: char;
    S: string;
    i,j: integer;
    inHead: boolean;
    inMIME: boolean;

    DefaultCS : string;
    u_ae,u_oe,u_ue,
    ug_ae,ug_oe,ug_ue,
    u_ss : char;

    rfc : TStream;
    FirstLine : boolean;

  procedure m(const s: string);
  begin
    if not assigned(rfc) then rfc := TRopeStream.Create;
    writeln_s(rfc,s);    
  end;

  function u(const s: string): string;
  var i: integer;

    function um(const gross,klein:string): string;
    begin
      result := iifs(
        ((i < length(s)) and (s[i+1] in ['A'..'Z'])) or
        ((i > 1) and (s[i-1] in ['A'..'Z'])), gross, klein);
    end;

  begin
    SetLength(result,length(s));
    result := '';
    for i := 1 to length(s) do
      if s[i] = u_ae then result := result + 'ae' else
      if s[i] = u_oe then result := result + 'oe' else
      if s[i] = u_ue then result := result + 'ue' else
      if s[i] = u_ss then result := result + 'ss' else
      if s[i] = ug_ae then result := result + um('AE','Ae') else
      if s[i] = ug_oe then result := result + um('OE','Oe') else
      if s[i] = ug_ue then result := result + um('UE','Ue') else
      result := result + s[i];
  end;

  function a(const s: string): string;
  var i,j: integer;
     l,r,d: string;
     z,n,f,p: integer;
  begin
    i := CPosX('@',s);
    l := u(Trim(LeftStr(s,Max(0,i-1))));
    r := u(Trim(Mid(s,i+1)));

    j := CPos(' ',r);
    if FTNParse(Trim(Mid(r,j+1)),d,z,n,f,p) then
      result := l + '@' +
        FTNForm(iifs(d<>'',d,LeftStr(s,Max(0,j-1))),z,n,f,p)
    else
    begin
      j := CPos('.',r);
      if j <= 0 then r := r + '.' + Domain;
      result := RFCNormalizeAddress(l + '@' + r, '');
    end;
  end;

  function g(s: string): string;
  var i: integer;
  begin
    s := u(s);
    SetLength(result,Length(s));
    result := '/';
    for i := 1 to length(s) do
      if s[i] in ['A'..'Z','a'..'z','0'..'9'] then
        result := result + s[i] else
      if s[i] in ['+','&'] then
        result := result + '+' else
      if s[i] in ['-','_'] then
        result := result + s[i] else
      if result[Length(result)] <> '/' then
        result := result + '/';
  end;

  procedure o;
  var rp : TRFCHeaderParser;
      nw : TStream;
   spezial : boolean;
  begin
   try
    Spezial := CPos('@',Msg.Head.Maus_MsgId) <= 0; // kein '@' => Spezial

    if assigned(rfc) then
    begin
      writeln_s(rfc,'');
      rfc.Seek(0,soFromBeginning);
      rp := TRFCHeaderParser.Create(rfc);

      while rp.NextLine do
      begin
        if rp.NameUC = 'CONTENT-TYPE' then Msg.Head.Mime.ContentType.AsString := rp.Content else
        if rp.NameUC = 'CONTENT-TRANSFER-ENCODING' then Msg.Head.Mime.Encoding := MimeGetEncodingFromName(rp.Content) else
        if rp.NameUC = 'CONTENT-DISPOSITION' then Msg.Head.Mime.Disposition.AsString := rp.Content else
        if rp.NameUC = 'CONTENT-DESCRIPTION' then Msg.Head.Mime.Description := rp.Content else
        if rp.NameUC = 'CONTENT-ID' then Msg.Head.Mime.CID := rp.Content else
        Msg.Head.ULine.Add('U-' + rp.Name + ': ' + rp.Content);
      end;
    end;

    if Msg.Head.Mime.Encoding in [MimeEncodingQuotedPrintable, MimeEncodingBase64] then
    begin
      nw := TRopeStream.Create;
      ConnectStream(nw,MimeCreateDecoder(Msg.Head.Mime.Encoding));
      CopyStream(Msg.Body,nw);
      UnConnectStream(nw);
      nw.Seek(0,soFromBeginning);
      Msg.Body := nw;
    end;

    if (Msg.Head.Mime.ContentType.Charset <> '') and
      not(MimeGetCharsetFromName(Msg.Head.Mime.ContentType.Charset) in [csASCII,csCP437]) then
      Msg.Head.Charset := MimeCharsetToZc(Msg.Head.Mime.ContentType.Charset);

    if Msg.Head.Empfaenger.Count <= 0 then
      Msg.Head.Empfaenger.Add('/¯Mausinfo');

    if (not spezial) and (Msg.Head.msgid = '') and (Msg.Head.References.Count <=0) then
    begin
      Msg.Head.MsgId := Msg.Head.Maus_MsgId;
      Msg.Head.References.Clear;
      if Msg.HEad.Maus_Reference <> '' then
        Msg.Head.References.Add(Msg.Head.Maus_Reference);
      Msg.Head.Maus_MsgId := '';
      Msg.Head.Maus_Reference := '';
    end;

    if Spezial then
    begin
      // Nothing important (e.g. empty #REN block)
      if Msg.Body.Size <= 2 then
        exit;
      if Msg.Head.absender = '' then Msg.Head.Absender :=
        Coalesce(FMausUser,'<Maus>') + '@' +
        iifs(FMausName<>'',FMausName+Domain,'invalid') + ' ('+
        Coalesce(FBoxType,'Maus') + iifs(FMausName<>'',' '+FMausName,'') + ')';
      if Msg.Head.Betreff = '' then
        Msg.Head.Betreff := Msg.Head.Maus_MSgId;
      if Msg.Head.zdatum = '' then begin
        Msg.Head.ZDatum := ZDate;
        Msg.Head.Orgdate := true;
      end;
    end;

    Dest.Put(Msg);
   finally
    FreeAndNil(Msg);
    FreeAndNil(rfc);
   end;
  end;

begin
  Msg := nil;
  Size := InStream.Size;

  u_ae := #0; ug_ae := #0; u_oe := #0; ug_oe := #0;
  u_ue := #0; ug_ue := #0; u_ss := #0;

  rfc := nil;

  DefaultCs := 'CP437';

  InMime := false;
  InHead := false;

  while InStream.Position < Size do
  begin
    s := readln_s(InStream); // slow
    if s = '' then Continue;

    c := FirstChar(s);
    s := Mid(s,2);

    if (c = '#') then
    begin
      if assigned(Msg) then o;
      if s = '' then Continue;
      inHead := UpperCase(s) = 'HEAD';
    end;

    if (not assigned(Msg)) and not inHead then
    begin
      Msg := TXPMessage.Create;
      Msg.Body := TRopeStream.Create;
      inMIME := false;
      FirstLine := true;
    end;

    if inHead then
    case c of
      ':' : begin
          c := FirstChar(s);
          s := Mid(s,2);
          case c of
            'C' : if Length(s) > 6 then begin
                u_ae := s[1];
                u_oe := s[2];
                u_ue := s[3];
                ug_ae := s[4];
                ug_oe := s[5];
                ug_ue := s[6];
                u_ss := s[7];
              end;
            'Z' : DefaultCS := Trim(s);

            'T' : FTauschVersion := Trim(s);
            'V' : FBoxVersion := Trim(s);
            'B' : FBoxType := Trim(s);
            'I' : FMausName := Trim(s);
            'U' : FMausUser := Trim(s);
          end;
        end;
    end else
    case c of
      ':' : if inMime then if FirstChar(s)= '-' then inMime := false
              else m(s) else begin
                write_s(Msg.Body,iifs(firstline,'',#13#10)+s);
                firstline := false;
              end;
      '>' : Msg.Head.xline.Add('X-XP-MAUS-KOM: '+s);
      '#' : Msg.Head.maus_msgid := s;
      '-' : Msg.Head.maus_reference := s;
      'I','i' : Msg.Head.msgid := s;
      'R','r' : Msg.Head.References.Add(s);
      'E','e' : begin
                   Msg.Head.ZDatum := LeftStr(s+'00000000000000',16); // oh oh
                   Msg.Head.Datum := Copy(s+'00000000000000',2,14);
                   Msg.Head.Orgdate := true;
                end;
      'V','v' : Msg.Head.absender := a(s);
      'A','a' : Msg.Head.Empfaenger.add(a(s));
      'G','g' : Msg.Head.Empfaenger.add(g(s));
//      'B','b' :
      'W','w' : Msg.Head.betreff := s;
      'O','o' : Msg.Head.organisation := s;
      'N','n' : Msg.Head.realname := s;
      'D','d' : Msg.Head.distribution := s;
      'K','k' : Msg.Head.Kopien.Add(a(s));
      'F','f' : Msg.Head.DiskussionIn.Add(g(s));
      'S','s' : Msg.Head.sender := a(s);
      'T','t' : Msg.Head.AntwortAn.Add(a(s));
      'M','m' : begin
                  inMime := true;
                  i := CPosX(';',s);
                  Msg.Head.MIME.Version := Trim(LeftStr(s,i-1));
                  m(Mid(s,i+1));
                end;
    end; // case
  end; // while
end;

constructor TMausTauschSpoolOut.Create(const FileName: string);
begin
  inherited Create(FileName);
  FIsQuark := true;
  FIdCount := 0;
end;

procedure TMausTauschSpoolOut.Put(msg: TXPMessage);
var em: array[boolean] of TStringList;

  procedure o(Header: Char; const Data: String);
  begin
    if Data <> '' then writeln_s(OutStream,Header+Data);
  end;

  procedure m(const MimeHeader, Data: string);
  begin
    if Data <> '' then writeln_s(OutStream,':'+MimeHeader+': '+Data);
  end;

  function g(const grp: string): string;
  var i: integer;
  begin
    if FirstChar(grp) = '/' then result := Mid(grp,2)
    else 			 result := grp;
    for i := 1 to Length(result ) do
      if result[i] = '/' then
        result[i] := '.';
  end;
  
  function a(const adr: string): string;
  var ado: TAddress;
  begin
    ado := TAddress.CreateZC(adr);
    try
      result := ado.AddrSpec;
    finally
      ado.free
    end;
  end;

  procedure w(pm: boolean; const an: string; kop, grp: TStrings);
  var i: integer;
      s: TStream;
     isCmd: boolean;
  begin
    isCmd := UpperCase(Msg.Head.Maus_MsgId) = 'CMD';

    if isCmd then
      writeln_s(OutStream,'#CMD')
    else
    begin
      inc(FidCount);
      o('#',StrS(FIDCount));

      if (Msg.Head.Maus_MsgId<>'') or
         (Msg.Head.Maus_Reference<>'') then
      begin
        o('-',Msg.Head.Maus_Reference);
        if Msg.Head.References.Count > 0 then
          o('I',Msg.Head.References[0]);
      end else
        if Msg.Head.References.Count > 0 then
          o('-',Msg.Head.References[0]);

      o('E', LeftStr(Msg.Head.ZDatum,12));

      o('V', Msg.Head.Absender);
      o('N', Msg.Head.Realname);

      o('W', Msg.Head.Betreff);
      o('O', Msg.Head.Organisation);
      o('D', Msg.Head.Distribution);

      if pm then
      begin
        if an<>'' then
          o('A',a(an))
        else
        begin
          o('A', a(kop[0]));
          for i := 1 to kop.count -1 do
            o('K', a(kop[i]));
        end
      end else
      begin
        if kop.Count > 0 then
          o('A', kop[0]);
        for i := 0 to grp.count - 1 do
          o('G', g(grp[i]));
      end;

      for i := 0 to Msg.Head.DiskussionIn.Count-1 do
        if not IsMailAddr(Msg.Head.DiskussionIn[i]) then
          o('F',g(Msg.Head.DiskussionIn[i]));

      for i := 0 to Msg.Head.AntwortAn.Count-1 do
        o('T', a(Msg.Head.AntwortAn[i]));
    end; // if not isCmd;

    s := TNullCodecStream.Create;
    (s as TNullCodecStream).OtherStream := OutStream;
    (s as TNullCodecStream).DestroyOtherStream := false;

    if not isCmd then
    begin
      if(Msg.Head.X_Charset <> '') and
        (Msg.Head.X_Charset <> Msg.Head.Charset) and
        IsKnownCharset(Msg.Head.X_Charset) and
        IsKnownCharset(Msg.Head.Charset) and
        Msg.Head.Mime.ContentType.NeedCharset and
        not isCmd
      then
      begin
        ConnectStream(s,TCharsetEncoderStream.Create(Msg.Head.Charset,Msg.Head.X_Charset));
        Msg.Head.Charset := Msg.Head.X_Charset;
        Msg.Head.X_Charset := '';
        Msg.Head.Mime.ContentType.Charset := Msg.Head.Charset;
      end;

      if (LowerCase(Msg.Head.Mime.ContentType.Verb) <> 'text/plain') or
        (not (MimeGetCharsetFromName(Msg.Head.Mime.ContentType.Charset) in [csASCII, csCP437])) or
        (Msg.Head.Mime.Disposition.ParamNames.Count > 0) or
        (Msg.Head.Mime.CID <> '') or
        (Msg.Head.Mime.Description <> '')
      then
      begin
        o('M','1.0; Content-Type: '+Msg.Head.Mime.ContentType.AsString);
        m('Content-Disposition', Msg.Head.Mime.Disposition.AsString);
        m('Content-ID', Msg.Head.Mime.CID);
        m('Content-Description', Msg.Head.Mime.Description);
        writeln_s(OutStream,':-');
      end;
    end; // not isCmd

    ConnectStream(s,TMausTauschAddColonStream.Create);

    CopyStream(Msg.Body,s);
    writeln_s(OutStream,''); // body does not end with CRLF in ZConnect    
    s.Free;   
  end;

var pm: boolean;
    i: integer;
    an: string;

begin
  em[false] := TStringList.Create;
  em[true ] := TStringList.Create;

  for i := 0 to Msg.Head.Empfaenger.Count - 1 do
    em[IsMailAddr(Msg.Head.Empfaenger[i])].Add(Msg.Head.Empfaenger[i]);

  if em[true].Count > 11 then
    for i := 0 to em[true].Count-1 do
      w(true, em[true][i], nil, em[false])
  else
  if em[true].Count > 0 then
    w(false, '', em[true], em[false]);

  if em[false].Count > 0 then
    w(false, '', em[true], em[false]);

  FreeAndNil(em[false]);
  FreeAndNil(em[true]);
end;

function TMausTauschSpoolOut.OpenFile: TStream;
begin
  result := TFileStream.Create(Self.FileName, fmCreate);
  writeln_s(result, '#CMD');
  writeln_s(result, ':ON'); // send new public mails
  writeln_s(result, ':PN'); // send new private mails
  writeln_s(result, ':TS'); // enable extended format
  writeln_s(result, ':OE'); // don't send back our messages
  writeln_s(result, ':REN'); // send group rename information
end;

procedure TMausTauschSpoolOut.CloseFile;
begin
  writeln_s(Self.OutStream, '#');
  Self.OutStream.Free;
end;

constructor TMausTauschAddColonStream.Create;
begin
  FLastWasCRLF := true;
  FLastWasCR := false;
end;

function TMausTauschAddColonStream.Write(const Buffer; Count: Longint): Longint;
var Position: Longint;
    Written: Longint;
begin
  Position := 0;
  Written := 0;
  Result := 0;

  if Count < 1 then exit;

  if FLastWasCRLF then
    write_s(OtherStream, ':') 
  else

  if FLastWasCR and (PChar(@Buffer)^ = #10) then
  begin
    write_s(OtherStream, #10':');
    Written := 1;
  end;

  for Position := 0 to Count-2 { last skipped } do
  begin
    if ((PChar(@Buffer)+Position)^ = #13) and 
      ((PChar(@Buffer)+Position+1)^ = #10) then
    begin
      OtherStream.WriteBuffer((PChar(@Buffer)+Written)^, Position - Written + 2);
      write_s(OtherStream,':');
      Written := Position + 2;
    end;
  end;

  if Written < Count then 
    OtherStream.WriteBuffer((PChar(@Buffer)+Written)^, Count - Written);
  
  FLastWasCRLF := (Count>=2) 
    and ((PChar(@Buffer)+Count-2)^ = #13) 
    and ((PChar(@Buffer)+Count-1)^ = #10);
  FLastWasCR := 
    ((PChar(@Buffer)+Count-2) = #13);

  Result := Count;
end;

end.

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

{ beliebig grossen Netcall- oder ZConnect-Header auswerten }
{ wird in XP3, MAGGI und XPCHECK included                 }

{$I xpdefine.inc }

unit xpmakeheader;

interface

uses
  xpglobal, classes, xpheader;

procedure makeheader(ZConnect:boolean; var f:file; NrOfFirstRecipient: integer;
                     var size:longint; hd:Theader; var ok:boolean;
                     PM2AMconv:boolean; ConvBrettEmpf: Boolean); overload;

procedure makeheader(outHeader: THeader; inStream: TStream); overload;

implementation

uses
  xpdatum, xpnt, Xp0, SysUtils, Typeform, mime, xpmime, debug, rfc2822,
  xpstreams;

{ Achtung! hd.empfaenger entaelt u.U. eine /TO:-Kennung }

const
  bufsize = 4096;


var line : string;

procedure _makeheader(ZConnect:boolean; var fx:file; inStream:TStream;
                     NrOfFirstRecipient:integer;
                     var size:longint; hd:Theader; var ok:boolean;
                     PM2AMconv:boolean; ConvBrettEmpf: Boolean);
var i,res : integer;
    o: integer; { Offset im Lesepuffer }
    s       : string;
    dummy   : string;
    id      : string;
    id0     : string;
    p    : integer;
    buf     : PCharArray;
    bufanz  : Integer;   { gelesene Bytes im Puffer }
    tc      : char;   { 1. Trennzeichen hinter ':' }
    idummy  : Integer;

  procedure _getline(var s:string);
  var l,p:integer;
  label again;
  begin
    SetLength(s,0);
   
  again:
    if o>=BufAnz then
      if eof(fx) then begin
        ok := false;
        exit;
      end else
      begin
        blockread(fx,buf^,bufsize,bufanz);
//      Inc(size,o);
        o:=0;
      end;

    l := o + BufferScan(Buf^[o], BufAnz-o, #10);    

    if l>o then 
    begin
      p := Length(s);
      SetLength(s,p+l-o);
      Move(Buf^[o],s[p+1],l-o);
    end;
    
    o := l+1;

    if (l<BufAnz) and (Length(s)>0) and (s[Length(s)]=#13) then 
    begin
      SetLength(s,Length(s)-1);
      Inc(Size, Length(s)+2);
      exit;
    end;
  
    goto again;
  end;

  procedure getline(var s:string);
  begin
    if assigned(inStream) then
      try
        s := readln_s(inStream)
      except
        ok := false;
        s := '';
      end
    else
      _getline(s);
  end;

  procedure LRead(var s:string);
  begin
    s:=line;
  end;

  procedure GetName(var name,realname:string);
  var p :integer;
  begin
    p:=pos(' (',line);
    if (p=0) or (p<cpos('@',line)) then begin
      realname:='';
      name:=line;
    end else begin
      realname:=trim(copy(line,p+2,length(line)-p-2));
      name:=LeftStr(line,p-1);
    end;
  end;

  procedure GetEmpf;
  var
    p: Integer;
    s: String;
  begin
    s := line;
    if IsMailAddr(s) or (FirstChar(s)<>'/') then
    begin
      p:=pos(' (',s);
      if p>0 then SetLength(s, p-1);
    end;
    if (hd.empfaenger.count+1=NrOfFirstRecipient) or
       ((NrOfFirstRecipient=0) and (hd.empfaenger.count=0)) then
     hd.Empfaenger.Insert(0,s)
    else
     hd.Empfaenger.Add(s);
  end;

  procedure GetTyp(var typ,charset:string);
  begin
    if line<>'' then begin
      if (UpperCase(line)='MIME') then typ:='M' else
      if (UpperCase(line)<>'TRANSPARENT') then typ:='B';
      if (typ<>'T') and (typ<>'') then charset:='';
      end;
  end;

  procedure GetStat;
  var p : integer;
  begin
    if line='' then exit;
    UpString(line);
    line:=trim(line)+' ';
    repeat
      p:=cpos(' ',line);
      if LeftStr(line,p-1)='EB' then hd.attrib:=hd.attrib or attrIsEB else
      if LeftStr(line,p-1)='PM-REPLY' then hd.pm_reply:=true else
      if LeftStr(line,p-1)='NOKOP' then hd.nokop:=true else
      if LeftStr(line,p-1)='CTL' then hd.attrib:=hd.attrib or attrControl;
      line:=trimleft(mid(line,p+1));
    until line='';
  end;

  procedure CheckBetreff;
  var p : integer;
  begin
    repeat
      p:=cpos(#7,hd.betreff);
      if p>0 then hd.betreff[p]:=' ';
    until p=0;
    repeat
      p:=cpos(#8,hd.betreff);
      if p>0 then hd.betreff[p]:=' ';
    until p=0;
  end;

  procedure GetStichwort;
  var s : string;
  begin
    s := line;
    if hd.keywords<>'' then hd.keywords:=hd.keywords+',';
    hd.keywords:=hd.keywords+s;
  end;

  procedure GetCrypt;
  var s : string;
  begin
    s := Uppercase(line);
    if s='QPC' then
      inc(hd.attrib,attrQPC)
    else if(s='PMCRYPT2')or(s='PM-CRYPT') then
      inc(hd.attrib,attrPMCrypt) 
    else if s='PGP' then
      inc(hd.pgpflags,fPGP_encoded);
    hd.crypt.method:=s;
  end;

  procedure GetSigned;
  var
    s : string;
  begin
    s := UpperCase(line);
    if s='PGP' then
      inc(hd.pgpflags,fPGP_signed)
    else
      if s='PGPCLEAR' then
        inc(hd.pgpflags,fPGP_clearsig);
  end;

  procedure GetPGP;
  var
    s : string;
  begin
    s := UpperCase(line);
    if s='PLEASE' then inc(hd.attrib,fPGP_please) else
    if s='REQUEST' then inc(hd.pgpflags,fPGP_request);
  end;

  procedure GetPGPid;
  var s : string;
      p : integer;
  begin
    s := line;
    p:=cpos('<',s);
    if (p>0) and (lastchar(s)='>') then begin
      delete(s,1,p);
      DeleteLastChar(s);
      if IsMailAddr(s) then hd.pgp_uid:=s;
      end;
  end;

  procedure GetXPpgp;
  var s : string;
  begin
    s := LowerCase(Line);
    if s='sigok'    then hd.pgpflags := hd.pgpflags or fPGP_sigok else
    if s='sigerror' then hd.pgpflags := hd.pgpflags or fPGP_sigerr;
  end;

  procedure GetQStr;
  begin
    with hd do begin
      QuoteString := Line;
      if FirstChar(quotestring)='"' then DeleteFirstChar(quotestring);
      if LastChar(quotestring)='"' then DeleteLastChar(quotestring);
      end;
  end;

  procedure GetCharset(var charset:string);
  begin
    if charset='' then
      Charset := LowerCase(Line)
  end;

  procedure ParseContentType(var hd:Theader);
  begin
    Hd.MIME.ContentType.AsString := line; 
    if Hd.MIME.ContentType.IsComposed then hd.typ := 'M';
  end;

  procedure ParseDisposition(var hd:Theader);
  begin
    hd.Mime.Disposition.AsString := line;
  end;

  procedure ParseEncoding(var hd:THeader);
  var ll: string;
  begin
    ll:=LowerCase(line);
    if ll='8bit' then hd.mime.encoding:=MimeEncoding8bit else
    if ll='7bit' then hd.mime.encoding:=MimeEncoding7bit else
    if ll='quoted-printable' then hd.mime.encoding:=MimeEncodingQuotedPrintable else
    if ll='base64' then hd.mime.encoding:=MimeEncodingBase64 else
    if ll='binary' then hd.mime.encoding:=MimeEncodingBinary else
    hd.mime.encoding:=MimeEncodingUnknown;
  end;

  procedure CheckEmpfs;          { /Brett@Box.domain -> /Brett }

    function check(const s:string): String;
    begin
      if (cpos('@',s)>0) and (FirstChar(s)='/') then
        Result := LeftStr(s, cpos('@',s)-1)
      else
        Result := s;
    end;

  var
    i: Integer;
  begin
    with hd do
      if (netztyp=nt_ZConnect) and not archive and PM2AMconv then
        for i := 0 to Empfaenger.Count - 1 do
          Empfaenger[i] := Check(Empfaenger[i]);
  end;

begin
  ok:=true;
  hd.Clear;
  getmem(buf,bufsize);
  size:=0; o:=0; BufAnz:=0;

  with hd do
    if ZConnect then
    begin
      netztyp:=nt_ZConnect;
      archive:=false;
      typ:='T';
      repeat
        getline(line);
        if length(line)>2 then
        begin
          if line[1]<' ' then DeleteFirstChar(line);    { gegen LF's o.ae. }
          p:=cpos(':',line);
          if p<2 then 
            ok:=false // Die ID muss mindestens ein Zeichen sein
          else begin
            id:=LeftStr(line,p-1);
            id0:=id;
            if length(line)>p then
              tc:=line[p+1] { Fix: Exception bei leeren Headern }
            else
              tc := '_';
            for i:=1 to length(id) do         { UpperCase }
              if (id[i]>='a') and (id[i]<='z') then
                dec(byte(id[i]),32);
            i:=p;                             { ltrim }
            while (i<length(line)) and ((line[i+1]=' ') or (line[i+1]=#9)) do
              inc(i);
            delete(line,1,i);
            line:= TrimRight(line);

            if LeftStr(id,2)='U-' then                      { RFC }
            begin
            if id = 'U-TO'           then FTo := Line else
            if id = 'U-CC'           then FCC := Line else
            if id = 'U-KEYWORDS'     then Keywords := Line else
            if id = 'U-SUMMARY'      then Summary := line else
            if id = 'U-DISTRIBUTION' then Distribution:= line else
            if id = 'U-X-NEWSREADER' then Programm:= line else
            if id = 'U-X-MAILER'     then Programm := line else
            if id = 'U-ENCRYPTED'    then GetCrypt else
            if id = 'U-X-HOMEPAGE'   then homepage := Line else
            if id = 'U-CONTENT-TYPE' then ParseContentType(hd) else
            if id = 'U-CONTENT-TRANSFER-ENCODING' then ParseEncoding(hd) else
            if id = 'U-CONTENT-DISPOSITION' then ParseDisposition(hd) else
            if id = 'U-CONTENT-ID'   then Mime.CID := line else

            if id = 'U-REPLY-TO'     then FReplyTo := line else
            if id = 'U-MAIL-REPLY-TO' then FMailReplyTo := line else
            if id = 'U-MAIL-FOLLOWUP-TO' then FMailFollowupTo := line else

            if id = 'U-LIST-ID' then ListID := RFCRemoveComments(line) else 
            if id = 'U-LIST-POST' then ListPost := RFCRemoveComments(line) else 
            if id = 'U-LIST-SUBSCRIBE' then ListSubscribe := RFCRemoveComments(line) else 
            if id = 'U-LIST-UNSUBSCRIBE' then ListUnSubscribe := RFCRemoveComments(line) else 
            if id = 'U-LIST-HELP' then ListHelp := RFCRemoveComments(line) else 
            if id = 'U-LIST-OWNER' then ListOwner := RFCRemoveComments(line) else 
            if id = 'U-LIST-ARCHIVE' then ListArchive := RFCRemoveComments(line) else 

            { X-No-Archive Konvertierung }
            if id = 'U-X-NO-ARCHIVE' then begin
              if LowerCase(line)='yes' then xnoarchive:=true;
            end else

            if id = 'U-X-PRIORITY' then begin
              i:=ival(line);
              if i<>0 then priority:=minmax(i,1,5)
            end else

            { suboptimal, eigentlich sollten alle Mail-Copies-To-
              Zeilen eingelesen werden, damit es auch hinter
              Gateways, die MCT nicht umwandeln, funktioniert     }
//          if (id='U-MAIL-COPIES-TO') then UMailCopiesTo := line;

            { Mime-Version wegschmeissen - wird neu erzeugt}
            if id = 'U-MIME-VERSION' then
              mime.version:=line
            else
              ULine.Add(mid(id0,3)+': '+line);
            end // 'U-'

            else

            if id = 'MIME-TYPE'      then ParseContentType(hd) else
            if id = 'MIME-ENCODING'  then ParseEncoding(hd) else
            if id = 'MIME-VERSION'   then mime.version := line else
            if id = 'MIME-ID'        then mime.Cid     := line else

            if id = 'EMP' then GetEmpf else             { ZConnect 3.0 }
            if id = 'ABS' then GetName(absender,realname) else
            if id = 'BET' then betreff := Line else
            if id = 'ROT' then pfad:=line else
            if id = 'O-ROT' then if pfad='' then pfad:=line
                                 else pfad:=pfad+'!'+line else
            if id = 'MID' then MsgID := line else
            if id = 'EDA' then begin
                                 zdatum := line;
                                 ZCtoZdatum(zdatum,datum);
                               {  if tc=' ' then xpmail:=true; }
                               end else
            if id = 'LEN'    then val(line,groesse,res) else
            if id = 'KOM'    then val(line,komlen,res) else
            if id = 'KOP'    then hd.Kopien.Add(line) else
            if id = 'BEZ'    then begin
                                  if (Line<>'') then
                                  begin
                                    idummy := References.IndexOf(Line);
                                    if idummy <> -1 then
                                    begin
                                      Debug.DebugLog('xpmakeheader.pas','makeheader,'
                                                     +'deleting first of dublicate BEZ:<'
                                                     +References[idummy]+'>'
                                                     +', first index: '+IntToStr(idummy)
                                                     , DLDebug);
                                      References.Delete(idummy);
                                    end;
                                    References.Add(Line);
                                  end
                             end else
            if id = 'MAILER' then programm := line else
            if id = 'ORG'    then organisation := line else
            if id = 'OEM'    then hd.Oem.Add(Line) else
            if id = 'OAB'    then GetName(oab,oar) else
            if id = 'WAB'    then GetName(wab,war) else
            if id = 'FILE'   then datei := line else
            if id = 'DDA'    then ddatum := line else
            if id = 'TYP'    then GetTyp(hd.typ,hd.charset) else
            if id = 'PRIO'   then prio:=byte(minmax(ival(line),0,20)) else
            if id = 'EB'     then begin
                                    GetName(empfbestto,dummy);
                                    attrib:=attrib or attrReqEB;
                                  end else
            if id = 'STAT'   then GetStat else
            if id = 'CHARSET'then GetCharset(hd.charset) else
            if id = 'ERR'    then error := line else
            if id = 'ANTWORT-AN' then AntwortAn.Add(Line) else
            if id = 'DISKUSSION-IN' then DiskussionIn.add(line) else
            if id = 'STICHWORT' then GetStichwort else
            if id = 'ZUSAMMENFASSUNG' then Summary  := Line  else
            if id = 'QUOTE-STRING' then GetQStr else
            if id = 'POST'   then postanschrift  := Line  else
            if id = 'TELEFON' then telefon := Line  else
            if id = 'HOMEPAGE' then homepage := Line  else
{            if id = 'X_C'    then xpmail:=true else }
            if id = 'VER'    then GetName(vertreter,dummy) else
            if id = 'CONTROL' then control := Line  else
            if id = 'ERSETZT' then ersetzt := Line else
            if id = 'LDA' then expiredate := line else
            if id = 'F-TO' then fido_to := line else
            if leftstr(id,2) = 'F-' then
              fline.add(rightstr(id,length(id)-2)+': '+line) else


            if FirstChar(id)='X' then begin               { XP }
              if id = 'X-CHARSET' then LRead(x_charset) else
              if id = 'X-XP-CHARSET' then LRead(x_charset) else
              if id = 'X-XP-NTP' then begin //netztyp:=minmax(ival(line),0,99)
                i := ival(line);
                if i < 0 then
                  netztyp := low(eNetz) //really?
                else if i > ord(high(eNetz)) then
                  netztyp := high(eNetz)
                else
                  netztyp := eNetz(i);
              end else {XP}
              if id = 'X-XP-BOX' then LRead(real_box) else
              if id = 'X-XP-PNT' then LRead(hd_point) else
              if id = 'X-XP-BST' then LRead(pm_bstat) else
              if id = 'X-XP-ATT' then attrib:=hexval(LeftStr(line,4)) else
              if id = 'X-XP-FTO' then LRead(fido_to) else
              if id = 'X-XP-MRP' then LRead(ReplyPath) else
              if id = 'X-XP-RGR' then LRead(ReplyGroup) else
              if id = 'X-XP-MAUS-MID' then LRead(maus_msgid) else
              if id = 'X-XP-MAUS-BEZ' then LRead(maus_reference) else
              if id = 'X-XP-BOUNDARY' then begin if Mime.ContentType.Boundary='' then Mime.ContentType.Boundary := line; end else
              if id = 'X-XP-CTL' then XpointCtl:=ival(line) else
              if id = 'X-XP-ARC' then archive:=true else
              if id = 'X-XP-MODE' then LRead(XPMode) else

              if tc=' ' then
                if id = 'X-XP_F'   then filterattr:=minmax(ival(line),0,65535);

           end else

            if pos('CRYPT',id)>0 then begin
              if id = 'CRYPT'       then GetCrypt else
              if id = 'CRYPT-CONTENT-TYP' then GetTyp(crypt.typ,crypt.charset) else
              if id = 'CRYPT-CONTENT-CHARSET' then GetCharset(crypt.charset) else
              if id = 'CRYPT-CONTENT-KOM' then val(line,crypt.komlen,res);
              end else
              if id = 'SIGNED'         then GetSigned else
              if id = 'U-X-SIGNED'     then GetSigned else
              if pos('PGP',id)>0 then begin
                if id = 'PGP'            then GetPGP else
                if id = 'U-X-PGP'        then GetPGP else
                if id = 'PGP-ID'         then GetPGPid else
                if id = 'PGP-KEY-AVAIL'      then inc(pgpflags,fPGP_avail) else
                if id = 'U-X-PGP-KEY-AVAIL'  then inc(pgpflags,fPGP_avail) else
                if id = 'PGP-PUBLIC-KEY'     then inc(pgpflags,fPGP_haskey) else
                if id = 'U-X-PGP-PUBLIC-KEY' then inc(pgpflags,fPGP_haskey) else
                if id = 'PGP-KEY-COMPROMISE' then inc(pgpflags,fPGP_comprom) else
                if id = 'X-XP-PGP'           then GetXPpgp;
                end
              else

            if id = 'ABR' then LRead(realname) else { ZConnect 1.9 }
            if id = 'BIN' then typ:='B' else
            if id = 'MAL' then LRead(programm) 

            else
              // unbearbeitete X-Lines fuer UUZ merken
              if Copy(id, 1, 2) = 'X-' then
                XLine.Add(mid(id0,3)+': '+line) else
              zline.add(id+': '+line);

            { Customizable Headerlines }
            if id = UpperCase(mheadercustom[1]) then Cust1 := line
            else
              if id = UpperCase(mheadercustom[2]) then Cust2 := line;

            line:='*';
          end;
        end
        else    { line='' }
          if not ok and ((not assigned(inStream)) and eof(fx)) then
            ok:=(groesse=0);          { letzte Msg hat Laenge 0 }
      until (line='') or not ok;
      { "DISKUSSION-IN: foo@bar.example.org" <-> "Followup-To: poster" }
(*      
      if (mailcopies.count>0) and (followup.count=0) and
        (lowercase(mailcopies[0])<>'nobody') and
        (lowercase(mailcopies[0])<>'never') then
        pm_reply:=true;
*)        
      if ok and (attrib and attrQPC<>0) and (UpperCase(LeftStr(betreff,4))<>'QPC:') then
        betreff:='QPC:'+betreff;
      end
    else begin
      Debug.DebugLog('xpmakeheader','buffer type not zconnect?!',dlWarning);
      GetLine(s);
      Empfaenger.Add(TrimRight(s));
      getline(betreff);
      Betreff := TrimLeft(Betreff);
      getline(absender);
      getline(datum);
      getline(pfad);
      getline(msgid);
      getline(typ);
      getline(s);
      val(trim(s),groesse,res);
    end;
  freemem(buf,bufsize);
  CheckBetreff;
  if ConvBrettEmpf then
    CheckEmpfs;                      { /Brett@Box.domain -> /Brett }
//inc(size,o);
  if res<>0 then ok:=false;
end;

procedure makeheader(ZConnect:boolean; var f:file; NrOfFirstRecipient: integer;
                     var size:longint; hd:Theader; var ok:boolean;
                     PM2AMconv:boolean; ConvBrettEmpf: Boolean);
begin
  _makeheader(ZConnect, f, nil, NrOfFirstRecipient, size, hd, ok,
    PM2AMconv, ConvBrettEmpf);
end;

procedure makeheader(outHeader: THeader; inStream: TStream); overload;
var dummy1: file;
    dummy2: longint;
        ok: boolean;
begin
  _makeheader(true, dummy1, inStream, 0, dummy2, outHeader, ok, false, false);
end;

{
  $Log: xpmakeheader.pas,v $
  Revision 1.40  2003/10/21 21:25:04  cl
  - Changed THeader.MIME to use TMimeContentType and TMimeDisposition objects
  - Changed MausTausch headers for Maus-internal IDs: MID/BEZ => maus_*, org_* => MID/BEZ,

  Revision 1.39  2003/08/26 22:33:05  cl
  - added interface for THeader to read from TSTream objects

  Revision 1.38  2003/08/25 22:45:04  mk
  - fixed #589632: 3.8: Anzeige selbstdefinierte Kopfzeilen

  Revision 1.37  2003/07/12 18:36:13  cl
  - BUXFIX: byte count problem in makeheader::getline with rare messages
    ("Fehlerhafter Puffer nicht eingelesen")

  Revision 1.36  2003/05/11 11:12:19  mk
  - use IsMailAddr when possible

  Revision 1.35  2003/04/25 21:11:19  mk
  - added Headeronly and MessageID request
    toggle with "m" in message view

  Revision 1.34  2003/01/11 19:53:13  cl
  - fixed reading of long headers

  Revision 1.33  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.32  2002/12/14 07:31:38  dodi
  - using new types

  Revision 1.31  2002/11/14 20:11:42  cl
  - some bugfixes after optimisation

  Revision 1.30  2002/10/01 15:38:48  cl
  - BUGFIX <m3elba8iwo.fsf@stell.crashmail.de>: "Doppelte Headerzeilen"
    (Duplicate header fields)

  Revision 1.29  2002/09/13 11:56:11  cl
  - fixed range check error

  Revision 1.28  2002/09/09 08:42:34  mk
  - misc performance improvements

  Revision 1.27  2002/07/25 20:43:56  ma
  - updated copyright notices

  Revision 1.26  2002/05/21 15:35:13  ma
  - ensure realname's blank if not given (just in case...)

  Revision 1.25  2002/04/14 22:33:10  cl
  - New address handling, supports To, CC, and BCC
  - Nearly complete rewrite of DoSend's message creation
  - Added TAddress and TAddressList
  - Moved many local variables from DoSend into TSendUUData fields

  Revision 1.24  2002/02/18 16:59:41  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.23  2002/02/08 16:39:22  ma
  - partially fixed handling of crosspostings
    (showed up in "Nix" most times)

  Revision 1.22  2002/01/13 15:15:54  mk
  - new "empfaenger"-handling

  Revision 1.21  2001/12/26 01:08:36  cl
  - BUGFIX: "Brett mit unvers. Nachr. nicht mehr vorhanden!"

  Revision 1.20  2001/12/23 23:26:00  mk
  - fixed multible EmfpList and problems with CCs (outgoing, witz uuz -smtp)

  Revision 1.19  2001/10/23 18:55:47  ma
  - added small debug log, there have been many errors connected with
    this in the past

  Revision 1.18  2001/10/20 17:26:43  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.17  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.16  2001/09/08 20:59:50  cl
  - ZC header X-Charset/X-XP-Charset renamed to X-XP-Charset uniformly (X-Charset
    is still recognized for backwards compatibility).

  Revision 1.15  2001/09/08 18:46:43  cl
  - small bug/compiler warning fixes

  Revision 1.14  2001/09/08 16:29:40  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.13  2001/09/08 14:37:48  cl
  - cleaned up MIME-related fields in THeader

  Revision 1.12  2001/09/06 19:31:20  mk
  - removed some hints und warnings

  Revision 1.11  2001/08/11 21:20:52  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.10  2001/07/31 16:18:42  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.9  2001/07/31 13:10:35  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.8  2001/07/27 18:10:15  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.7  2001/04/18 11:02:13  ma
  - using StrgList.IndexOf instead of Find, Find only works on sorted
    Strglists.

  Revision 1.6  2001/04/18 10:31:12  ma
  - this should prevent doubled references in every case BUT there
    seems to be an error in TStrglist.Find :-(

  Revision 1.5  2001/04/17 00:20:36  ma
  - fixed: "BEZ:" was read as one reference ""

  Revision 1.4  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.3  2001/03/01 14:04:19  mk
  - removed some tinted code/comments

  Revision 1.2  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.1  2001/01/14 10:13:36  mk
  - MakeHeader() integreated in new unit

}
end.


{  $Id$

   OpenXP/32: TSendUUData - Message Creation
   
   (C) Copyright 1991-2001 Peter Mandrella
   (C) Copyright 2001-2002 by OpenXP/32 team <http://www.openxp.de>

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

// Local procedure in xpsendmessage.DoSend

procedure TSendUUData.CreateMessages;
var hdp:    THeader;
var Count:  Integer;
    MidType:byte;
var FidoOriginSav: string;

  procedure CreateSingleMessage(
//  Box    : String;
//  Netztyp: Byte;
    Server : TXPServer;
    FileStream: TStream;
    EMP,KOP: TStringList;
    PM,BCC : Boolean;
    EncryptionMethod: TSendEncryptionMethod;
    SigningMethod: TSendSigningMethod;
    const EncrPwd: string );

  var
    NoServer : boolean;

    {-- CreateSingleMessage ------------------------------------------ }

    {-- CreateSingleMessage: Fido ------------------------------------ }

    function FidoOrigin: string;
    var fn,s1: String;
        t    : text;
        buf  : array[0..511] of byte;
        num  : integer;
    begin
      Result := '';
      if NoServer or not Server.IsFTN then exit;
      if pm then exit;

      if FidoOriginSav<>'' then begin
        result := FidoOriginSav;     
        exit;
      end;

      if XP_Tearline then
        Result := #13#10+xp_origin + ' ' + verstr + betastr
      else
        Result := #13#10+'---';

      if LeftStr(LowerCase(Server.FidoName),8)='shuffle:' then
      begin
        fn := trim(mid(Server.FidoName,9));
        assign(t,fn);
        if not existf(t) then 
          s1 := getreps(621,fn)    { 'Origin-Datei fehlt: %s' }
        else
        begin
          settextbuf(t,buf);
          reset(t);
          try
            num := 0;
            while not eof(t) do   { Origins zÑhlen }
            begin    
              readln(t,s1);
              if s1 <>'' then inc(num);
            end;
          finally
            close(t);
          end;
            reset(t);
          try
            Num := random(num)+1;
            while not eof(t) and (Num>0) do 
            begin
              readln(t,s1);
              if s1<>'' then dec(num);
            end;
            s1 := LeftStr(s1,54);
          finally
            close(t);
          end;
        end;
      end else
        s1 := Server.FidoName;

      Result := Result + #13#10' * Origin: '+s1+' ('+Server.FidoAbsAddr+')';
      FidoOriginSav := Result;
    end;

    function MayCrash:boolean;
    begin
      Result := (not NoServer) and PM and Server.IsFTN;
    end;

    procedure SetCrashInfo;
    var ni : nodeinfo;
        fa : TFTNAddress;
    begin
      fa := TFTNAddress.Create(hdp.FirstEmpfaenger,DefaultZone);
      try
        GetNodeinfo(fa,ni,2);
        fa.ispoint:=ni.ispoint;
        SetCrash(fa,true);
      finally
        fa.Free
      end;
    end;

    {-- CreateSingleMessage: Encryption ------------------------------ }

    procedure SetCryptFlag;
    var flag: byte;
    begin
      dbReadN(mbase,mb_unversandt,flag);
      flag:=flag or 4;
      dbWriteN(mbase,mb_unversandt,flag);
    end;

    procedure EncryptMessage(des:Boolean;var data:TStream);
    var d2: TMemoryStream;
      _pwd: string[255];
      x,y: Integer;
      pos: smallword;
      show: boolean;
    
    begin
      show:=des and (data.size>2000);
      if show then begin
        message(getres(605));    { 'DES-Codierung...     %' }
        x:=wherex-5; y:=wherey;
        end
      else begin
        x:=0;
        y:=0;
      end;
    
      pos:=0;
    
      if data is TMemoryStream then
        d2:= TMemoryStream(data)             // use as is
      else begin
        d2:= TMemoryStream.Create;           // make a copy in memory
        try d2.Size:=data.Size except end;   // if data.Size does not work
        data.Seek(0,soFromBeginning);
        CopyStream(data,d2);
        data.Free;
        data:=d2;
      end;
    
      if DES then
      begin
        DES_PW(EncrPwd);
        DES_code(false,PChar(d2.Memory)^,0,d2.Size,d2.Size,x,y)
      end else
      begin
        _pwd:=EncrPwd;
        QPC(false,PChar(d2.Memory)^,d2.Size,@_pwd,pos);
      end;
    
      if show then closebox;
    
      SetCryptFlag;
      hdp.betreff:=LeftStr(iifs(des,DES_ID,QPC_ID)+hdp.betreff,BetreffLen);
      hdp.archive:=false;
    
       if not des then
         hdp.attrib:=hdp.attrib or AttrQPC;
    
       hdp.crypt.komlen:=hdp.komlen; hdp.komlen:=0;      { KOM anpassen   }
       hdp.crypt.typ:=hdp.typ;
       hdp.crypt.charset:=hdp.charset;
       hdp.crypt.method:=iifs(DES,'DES/XPOINT','QPC'); // OpenXP supports none of the ZConnect DES-Methods :-(
       hdp.charset:='';
       hdp.x_charset:='';
       hdp.typ:='B';

       hdp.MIME.ContentType.AsString:='application/octet-stream';  // There's no MIME content
       hdp.MIME.encoding:=MimeEncodingBase64;       // type (did I mention that
                                                    // we maintain this encoding
                                                    // schemes for backward com-
                                                    // patibility only?!)
       hdp.groesse:=data.size;
    end;
    
    procedure pmEncryptMessage(var data:TStream);
    var fi,fo:string;
        fs:TStream;
         s:string;
      anal:TMimeAnalyzer;
    begin
      if pmcrypt[Ord(EncryptionMethod)-2].binary and not ((not NoServer) and ntBinary(server.netztyp)) then 
      begin
        rfehler(609);     { In diesem Netz sind keine BinÑrnachrichten mîglich. }
        exit;
      end;

      fi:=temppath+uncryptedfile;
      fo:=temppath+cryptedfile;

      if FileExists(fo) then _era(fo);

      fs := nil;
      try
        fs := TFileStream.Create(fi,fmCreate);
        hdp.WriteZ38(fs);
        CopyStream(data,fs);
      finally
        fs.Free;
      end;

      s:=pmcrypt[Ord(EncryptionMethod)-2].encode;
      rps(s,'$KEY',EncrPwd);
      rps(s,'$INFILE',fi);
      rps(s,'$OUTFILE',fo);
      rps(s,'$USER',EMP[0]);
      shell(s,500,3);                     { Nachricht codieren }

      if not FileExists(fo) then
        rfehler(603)    { 'Datei wurde nicht codiert!' }
      else
      begin
        fs:=TTemporaryFileStream.Create(fo,fmOpenRead);
        if not pmcrypt[Ord(EncryptionMethod)-2].binary then anal:=TMimeAnalyzer.Create else anal:=nil;
        CopyStream(fs,anal);
        
        if(pmcrypt[Ord(EncryptionMethod)-2].binary or (anal.IsBinary or (not anal.EOLAllowed[MimeEOLCRLF])))
          and not ((Not NoServer) and ntBinary(server.netztyp)) then
        begin
          fs.Free;
          rfehler(609);     { In diesem Netz sind keine BinÑrnachrichten mîglich. }
        end else
        begin
          SetCryptFlag;

          hdp.betreff:=PMC_ID+' by XP ('+pmcrypt[Ord(EncryptionMethod)-2].name+') '+
            hex(hdp.groesse,6);
          hdp.crypt.method:='PMCRYPT2';
          hdp.crypt.komlen:=hdp.komlen; hdp.komlen:=0;      { KOM anpassen   }
          hdp.crypt.typ:=hdp.typ;
          hdp.crypt.charset:=hdp.charset;

          if(pmcrypt[Ord(EncryptionMethod)-2].binary or (anal.IsBinary or (not anal.EOLAllowed[MimeEOLCRLF])))then
          begin
            hdp.typ:='B';
            hdp.mime.encoding:=MimeEncodingBase64;
          end else
          begin
            hdp.typ:='T';
            hdp.mime.encoding:=MimeEncodingQuotedPrintable;
          end;
          hdp.mime.contenttype.AsString:='application/octet-stream';

          hdp.attrib:=hdp.attrib or AttrPmcrypt;

          data.Free;
          data:=fs;
          anal.Free;
        end;
      end;

      if FileExists(fi) then
        _era(fi);
    end;

    {-- CreateSingleMessage ------------------------------------------ }

  var
    I    : integer;
    L      : Longint;
    Lr: RNetzMsg;
    Ablage : integer;
    sendedat: longint;

    MPSize : Longint;
    MPAdr  : Longint;

    NewMidType : byte;
    fn2      : string;
    s1,s2,s3: TStream;
    hdptyp   : char;
    b: byte;

    shortmid : string;
    flags    : longint;

  begin
    s1 := nil;
    s2 := nil;
    s3 := nil;

    { --- 0. Schritt: Ein paar vorarbeiten ---------------------------- }

    Hdp := THeader.Create;
    NoServer := not assigned(Server);
    if NoServer then Server := TXPServer.CreateByName(DefaultBox);

    if not NoServer then Hdp.Netztyp := Server.Netztyp;

    { --- 1. Schritt: Body erzeugen ----------------------------------- }

    if NoServer or ntMIME(Server.netztyp) then
      hdp.MIME.version := '1.0';

    // We need the information for individual body parts when sending
    // encrypted or signed
    if (not partsex) and ((EncryptionMethod <> secNone) or (SigningMethod <> sigNone)) then
      MIMEDecompose;

    if (not partsex) and assigned(orghdp) then
    begin
      // just pass-through
      FreeAndNil(s1);
      s1 := TFileStream.Create(PartFile,fmOpenRead);

      hdp.typ           := orghdp.typ;
      hdp.mime.contenttype.asstring    := orghdp.mime.contenttype.asstring;
      hdp.mime.encoding := orghdp.mime.encoding;
      hdp.mime.disposition.asstring := orghdp.mime.disposition.asstring;
      hdp.mime.cid      := orghdp.mime.cid;
      hdp.charset       := orghdp.charset;
      hdp.x_charset     := orghdp.x_charset;
    end else
    begin
      assert(parts.count>0);
      assert(SigningMethod <> sigMIME_or_PGPOld);
      assert(EncryptionMethod <> secMIME_or_PGPOld);

      // We have to use MIME for multipart messages
      if parts.count > 1 then
      begin
        if EncryptionMethod in [secPGPOld,secMIME_or_PGPOld] then
          EncryptionMethod := secMIME;
        if SigningMethod in [sigPGPOld,sigMIME_or_PGPOld] then
          SigningMethod := sigMIME;
      end;

      // If we use MIME, make sure all encoding methods are compliant
      // with multipart/signed or multipart/encrypted
      if (SigningMethod in [sigMIME]) or
         (EncryptionMethod in [secMIME]) then
        for i:=0 to parts.Count-1 do
          with TSendAttach_Part(parts[i]) do
            if not Analyzed.EncodingSafeForSigned[ContentEncoding] then
              ContentEncoding := MimeEncodingQuotedPrintable;

      case parts.count of
      1: with TSendAttach_Part(parts[0]) do begin
//         if ntMIME(netztyp) and not ntConv(netztyp) then
//           hdp.typ := 'M' else // ZConnect with MIME
           if(TSendAttach_Part(parts[0]).FileEOL=MimeEOLNone) or
             (TSendAttach_Part(parts[0]).Analyzed.IsBinary) then
             hdp.typ := 'B'  // UUZ will encode
           else
             hdp.typ := 'T'; // UUZ will encode

           if NoServer or ntMIME(Server.netztyp) then
           begin
             hdp.mime.contenttype.asstring    := TSendAttach_Part(parts[0]).ContentType.AsString;
             hdp.mime.encoding := TSendAttach_Part(parts[0]).ContentEncoding;
             hdp.mime.disposition.asstring := TSendAttach_Part(parts[0]).ContentDisposition.AsString;
             hdp.mime.description := TSendAttach_Part(parts[0]).ContentDescription;
           end; // ntMIME

           hdp.datei := TSendAttach_Part(parts[0]).FileNameO;
           hdp.summary := TSendAttach_Part(parts[0]).ContentDescription;
           if (NoServer or ntMIME(Server.netztyp))
              and not IsNAN(FileModify) then
             hdp.ddatum := DateTimeToZCDateTime(TSendAttach_Part(parts[0]).FileModify);

           if ContentType.NeedCharset then
           begin
             FileCharset:=MimeCharsetCanonicalName(FileCharset);
             ContentCharset:=MimeCharsetCanonicalName(ContentCharset);
           end;

           // Determine whether we can use the file unchanged:
           if ((not NoServer) and (Server.netztyp=nt_Fido)) or
              (FileEOL in [MimeEOLLF,MIMEEOLCR]) or
              ((IsMessage) and not flOhnesig and (SigData<>'')) or
              (ContentType.NeedCharset and not MIMESaveCharsetAsCP437(FileCharset)) then
           begin
             FreeAndNil(s1);
             s1 := TRopeStream.Create;
             MIMEWriteContent(s1,TSendAttach_Part(parts[0]),hdp.typ='M',
               SigData,FidoOrigin);
             if ContentType.NeedCharset then begin
               hdp.charset:=MimeCharsetToZC(FileCharset);
               hdp.x_charset:=ContentCharset;
             end;
           end else
           begin
             FreeAndNil(s1);
             s1 := TFileStream.Create(FileName,fmOpenRead);
             if ContentType.NeedCharset then begin
               hdp.charset:=MimeCharsetToZC(FileCharset);
               hdp.x_charset:=ContentCharset;
             end;
           end;

         end; // with
      else
       begin
        Boundary:=MimeCreateMultipartBoundary(Server.username); // does not contain chars that must be quoted

        hdp.boundary:=boundary;
        hdp.typ:='M';
        hdp.MIME.contenttype.AsString := 'multipart/mixed; boundary="'+Boundary+'"';
        hdp.MIME.encoding := MimeEncoding7Bit;

        FreeAndNil(s1);
        s1 := TRopeStream.Create;

        for i:=0 to parts.Count-1 do
        begin
          with TSendAttach_Part(parts[i]) do
          begin
            if ContentType.NeedCharset then
            begin
              FileCharset:=MimeCharsetCanonicalName(FileCharset);
              ContentCharset:=MimeCharsetCanonicalName(ContentCharset);
            end; // if
            writeln_s(s1,#13#10'--'+boundary);
            MIMEWriteContentWithHeaders( s1,TSendAttach_Part(parts[i]),
              iifs((i=0) and (IsMessage) and not flOhnesig,SigData,''));
            if (hdp.mime.Encoding=MimeEncoding7bit) and (ContentEncoding=MimeEncoding8bit) then
              hdp.mime.Encoding:=MimeEncoding8bit else
            if (hdp.mime.Encoding in [MimeEncoding7bit,MimeEncoding8bit]) and (ContentEncoding=MimeEncodingBinary) then
              hdp.mime.Encoding:=MimeEncodingBinary;
          end; // with
        end; // for
        write_s(s1,#13#10'--'+boundary+'--');
       end; // else
      end; // case
    end; // if (not partsex) ...

    { --- 2. Schritt: Headerdaten erzeugen --------------------------- }

    if not assigned(FileStream) then
    begin

    //-- Envelope-Empf‰nger und ZConnect-Header ---------------------- }

    hdp.Empfaenger.Assign(EMP);
    hdp.Kopien    .Assign(KOP);

    hdp.DiskussionIn.Clear;
    hdp.AntwortAn.Clear;

    for i:=0 to EmpfList.Count-1 do
      case EmpfList[i].AddressType of
        atReplyTo:    hdp.AntwortAn.Add(EmpfList[i].ZcAddress);
        atFollowupTo: hdp.DiskussionIn.Add(EmpfList[i].ZcAddress);
      end;
      
    //-- RFC-Header -------------------------------------------------- }

    if (NoServer) or Server.IsRFC then begin
      hdp.UTo := RFCWriteAddressList(EmpfList,nil,[atTo]);
      hdp.CC  := RFCWriteAddressList(EmpfList,nil,[atCC]);
      hdp.BCC := RFCWriteAddressList(EmpfList,nil,[atBCC]);

      // Write U-REPLY-TO only if there's information that can't be
      // represented with ANTWORT-AN headers (i.e. if it has groups)      
      for i:= 0 to EmpfList.Count-1 do 
        if (EmpfList[i].AddressType = atReplyTo) and (EmpfList[i].Group >= 0) then begin
          hdp.UReplyTo := RFCWriteAddressList(EmpfList,nil,[atReplyTo]);
          break; end;
      hdp.UMailFollowupTo := RFCWriteAddressList(EmpfList,nil,[atMailFollowupTo]);
    end;

    hdp.archive := PM;
    hdp.betreff:=Subject;

    hdp.absender := Server.AbsAddr;
    if not Server.AliasPoint then hdp.real_box:=Server.Name;
    hdp.realname:=iifs(SenderRealname='',Server.Realname,SenderRealname);
    
    if flWAB and (not NoServer) and ntAdrCompatible(onetztyp,Server.netztyp)
    then begin
      hdp.wab:=hdp.absender; hdp.war:=hdp.realname;
      hdp.absender:=oab; hdp.realname:=oar;
      { oab:=''; }
      end;

    hdp.Keywords:=keywords;
    hdp.Summary:=summary;
    if (not NoServer) and ntAdrCompatible(onetztyp,Server.netztyp)
    then begin
      if not flWAB then begin
        hdp.oab:=oab; hdp.oar:=oar;
        end;
      hdp.oem.Assign(oem);
      end;
      hdp.ReplyGroup:=ReplyGroup;
    if not pm then
      hdp.distribution:=distribute;
    hdp.quotestring:=quotestr;
    sendedat:=ixdat(zdate);
    hdp.datum:=iifs(ReplaceEtime,LeftStr(zdate,6)+'0000',zdate);

    if (not NoServer) and (Server.Netztyp = nt_UUCP) then
      hdp.pfad := iifs(Server.AliasPoint,Server.username,Server.pointname+Server.domain+'!'+Server.username)
    else
      hdp.pfad := '';


  // == Message-ID erzeugen ============================================
    NewMidType := ntMessageID(Server.Netztyp);

  // -- Falls noch keine generiert wurde => jetzt machen ---------------

    if MsgID = '' then
    begin
      MidType := NewMidType;
      MsgID := Server.CreateMessageID(dbReadInt(mbase,'INT_NR'));
    end;

  // -- Kompatible IDs weiterverwenden ---------------------------------
    if(MidType=NewMidType)or((MidType in [midRFC,midZConnect])and(NewMidType in [midRFC,midZConnect])) then
      hdp.msgid := msgid
    else
    
  // -- Message-ID ggf. umwandeln --------------------------------------

    // -- ZC/RFC => Fido -----------------------------------------------    
      if (NewMidType=midFido) and (MidType in [midRFC,midZConnect]) then
      begin
        if pm then
          hdp.msgid := '<' + MsgID + '> ' + 
            LowerCase(Hex(CRC32Str('<'+MsgID+'>'),8))
        else
          hdp.msgid := '<' + MsgID + '> ' + 
            LowerCase(Hex(CRC32Str('<'+MsgID+'>'+
              UpperCase(Mid(hdp.FirstEmpfaenger,1+RightPos('/',hdp.FirstEmpfaenger)))),8));
      end else

    // -- geht nicht; neue ID erzeugen ---------------------------------

      begin
        MidType := NewMidType;
        MsgID := Server.CreateMessageID(dbReadInt(mbase,'INT_NR'));
        hdp.msgid := msgid
      end;

    if (References.Count>0)and assigned(orghdp) then
      hdp.replypath:=orghdp.replypath;
    
//  hdp.typ:=iifs(binary,'B','T');
    hdp.programm:= xp_xp + '/' + verstr + pformstr;
    hdp.organisation:=orga;
    if ersetzt<>''then hdp.ersetzt:=ersetzt;
    if (pm and ntPMTeleData(Server.netztyp)) or (not pm and ntAMTeleData(Server.netztyp))
    then begin
      hdp.postanschrift:=postadresse;
      hdp.telefon:=telefonnr;
      hdp.homepage:=wwwHomepage;
      end
    else if NoServer or ((Server.netztyp in netsRFC) and (not adrpmonly)) then
      hdp.homepage:=wwwHomepage;

    hdp.datei:=sendfilename;
    hdp.ddatum:=sendfiledate;

    if not has[ncFTN,false] then
      hdp.fido_to := ''
    else
      if FidoTo<>'' then
        hdp.fido_to:=fidoto
      else
        if not pm and (NoServer or (Server.netztyp in netsFTN)) then
          hdp.fido_to:=brettalle;

    hdp.attrib:=iif(pm and flRequestMDN,attrReqEB,0);
    hdp.xnoarchive := flNoArchive;
    hdp.priority := rfcprio;

    if flEB then with hdp do
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
    if flFileAttach then { Fido file attachment }
      inc(hdp.attrib,attrFile);
    if Server.netztyp=nt_Maus then
      if flQTo then inc(hdp.attrib,AttrQuoteTo);
    if ntPmReply(Server.netztyp) then
      if flpmReply then inc(hdp.attrib,AttrPmReply);
    if flControlMsg then inc(hdp.attrib,AttrControl);
    if ((hdp.typ='B') and (Server.netztyp in netsRFC) and multipartbin) or
       ((hdp.typ='B') and (Server.netztyp = nt_Maus) and mausmpbin) then
      inc(hdp.attrib,AttrMPbin);
    if flPGPkey then
      inc(hdp.pgpflags,fPGP_haskey);
    if flPGPreq then
      inc(hdp.pgpflags,fPGP_request);
    if UsePGP and not flPGPkey and ntPGP(Server.netztyp) then begin
      if not FileExists(PGPkeyfile) then UpdateKeyfile;
      if FileExists(PGPkeyfile) then
        inc(hdp.pgpflags,fPGP_avail);
      end;
    hdp.prio:=msgprio;
    hdp.nokop:=BCC;
//    if umlaute=0 then
//      if netztyp=nt_Fido then
//        hdp.x_charset:='IBMPC 2'   { s. FSC-0054, grmpf }
//      else if netztyp in netsRFC then
//        if FileContainsUmlaut then hdp.x_charset:='ISO-8859-1';
//    if iso then
//      hdp.charset:='ISO1';
    if assigned(orghdp) then
      with orghdp do begin
        { hdp.zdatum:=zdatum; hdp.orgdate:=true;  !! Unversandt/* !! }
        hdp.organisation:=organisation;
        hdp.datei:=datei; hdp.ddatum:=ddatum;
        end;
//  if _sendmaps then
//    hdp.replyto := '';
    hdp.References.Assign(References);

    hdp.Maus_MsgId := self.Maus_MsgId;
    hdp.Maus_Reference := self.Maus_Reference;

    FreeAndNil(s2);
    fn2:=TempS(s1.Size+4000);
    s2 := TFileStream.Create(fn2,fmCreate);

    hdp.groesse:=s1.Size;
    FreeAndNil(s3);

    if NoServer then
    begin
      s3 := TRopeStream.Create;
      for i := 0 to KOP.Count-1 do
        writeln_s(s3,GetRes2(600,1)+' '+KOP[i]);
      writeln_s(s3,'');
      inc(hdp.groesse,s3.Size);
    end;

    hdp.WriteToStream(s2);      // Header erzeugen

    if assigned(s3) then begin
      s3.Seek(0,soFromBeginning);
      CopyStream(s3,s2);  // ## Originalempfaenger
    end;

    s1.Seek(0,soFromBeginning);
    CopyStream(s1,s2);  // Body anh‰ngen

    FreeAndNil(s2);

    { --- 3. Schritt: Nachricht in Datenbank ablegen ------------------ }

    // 3a) Bretter/User suchen, evt. anlegen oder lˆschen
    if flIntern and (EMP.Count>=1) and (Length(EMP[0])>1) then
    begin
      dbSeek(bbase,biBrett,UpperCase(EMP[0]));
      if not dbFound then
        AddNewBrett(EMP[0],'','',StdHalteZeit,IntGruppe,0);
      EMP.Add(mbrettd('$',BBase));
      dbWriteN(bbase,bb_ldatum,sendedat);    { Brettdatum neu setzen }
    end else
    begin
      for i:=EMP.Count-1 downto 0 do
        if PM then
        begin
          Emp[i]:=Addr2DB(EMP[i]);

          dbSeek(ubase,uiName,UpperCase(Emp[i]));
          if not dbFound then             { kein bekannter B. Nutzer => anlegen }
            AddNewUser(Emp[i],Server.Name);
          EMP[i] := mbrettd('U',ubase);
          if dbreadint(ubase,'adrbuch')=0 then { Empfaenger ins Adressbuch aufnehmen }
            dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
        end else
        begin
          dbSeek(bbase,biBrett,'A'+UpperCase(EMP[i]));
          if dbFound then begin
            EMP[i] := mbrettd('A',Bbase);
            dbWriteN(bbase,bb_ldatum,sendedat);    { Brettdatum neu setzen }
          end
          else
            EMP.Delete(i);
        end;

      if (not PM) and (EMP.Count<=0) then    { kein bekanntes Brett *** GRMPF ***       }
      begin                                  { => "$/>>Nix" nehmennehmen und ggf. anlegen! }
        dbSeek(bbase,biBrett,UpperCase('$/ØNix'));
        if not dbFound then
          AddNewBrett('$/ØNix','','',StdHalteZeit,IntGruppe,0);
        EMP.Add(mbrettd('$',BBase));
        dbWriteN(bbase,bb_ldatum,sendedat);    { Brettdatum neu setzen }
      end;
    end;

    // 3b) Eintr‰ge erzeugen
    for i:=0 to EMP.Count-1 do
    begin
      if Count<>0 then
        dbAppend(mbase);      { Neuer Eintrag (hier wg. Message-ID      }
      Inc(Count);
      Ablage := iif(NoServer or ntZConnect(Server.netztyp),10,0);
      dbWriteN(mbase,mb_ablage,Ablage);

      if i=0 then begin
         Xwrite(fn2);         { In MPUFFER.?? ablegen                   }
        _era(fn2);
        mpadr  := dbReadIntN(mbase,mb_adresse);
        mpsize := dbReadIntN(mbase,mb_msgsize);
      end else
      begin
        dbWriteN(mbase,mb_adresse,mpadr);
        dbWriteN(mbase,mb_msgsize,mpsize);
      end;

      dbWriteNStr(mbase,mb_brett,EMP[i]);

      Lr := Server.NetzMsg;
      if hdp.GetLastReference <> '' then //inc(l,$100); // rÅckwÑrts-verkettet
        include(Lr.flags, mf_Verkettet);
      if flFileAttach then //inc(l,$200);
        include(Lr.flags, mf_Attachment);
      if hdp.pm_reply then //inc(l,$400);
        include(Lr.flags, mf_pm_reply);
      if (hdp.wab<>'') or (hdp.oem.Count > 0) then //inc(l,$800);
        include(Lr.flags, mf_wab);
      if (hdp.typ='T')and(hdp.charset='ISO1')then //inc(l,$2000);
        include(Lr.flags, mf_ISO1);
      if SigningMethod in [sigMIME,sigPGPOld,sigMIME_or_PGPOld] then //inc(l,$4000);
        include(Lr.flags, mf_PGPsig);
      if Emp.Count>1 then begin
        //inc(l,longint(Min(255,EMP.Count)) shl 16);
        Lr.CPanz := Min(255,EMP.Count);
        //inc(l,longint(Succ(i) mod 128)    shl 24);
        Lr.CPpos := Succ(i) mod 128; { Empfaengernummer }
      end;
      dbWriteN(mbase,mb_netztyp,Lr.i);

      shortmid:= Hdp.BinaryMsgID;
      dbWriteNStr(mbase,mb_msgid,shortmid);
      dbWriteNStr(mbase,mb_betreff,hdp.betreff);
      dbWriteNStr(mbase,mb_absender,hdp.absender);
      dbWriteNStr(mbase,mb_mimetyp,LowerCase(hdp.mime.ContentType.Verb));
      l:=ixdat(hdp.datum);
      dbWriteN(mbase,mb_origdatum,l);
      dbWriteN(mbase,mb_empfdatum,sendedat);
      dbWriteN(mbase,mb_groesse,hdp.groesse);
      hdptyp := hdp.typChar;
      dbWriteN(mbase,mb_typ,hdptyp);
      if ntEditBrettempf(Server.netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.fido_to)
      else if ntRealname(Server.netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.realname);

      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      if flHalt then b:=1
      else if flIntern then b:=0
      else if flLoesch then b:=2
      else if (HaltOwn and (not pm)) or (HaltOwnPM and pm) then b:=1
      else b:=0;
      dbWriteN(mbase,mb_halteflags,b);
      if flintern then b:=0
      else b:=1;
      if flCrash and (Server.Netztyp in netsFTN) and not NoServer then inc(b,16);    { !! Crash-Flag }
      dbWriteN(mbase,mb_unversandt,b);

      dbreadN(mbase,mb_flags,flags);                 { Farb - Flags setzen... }
      flags := flags or 256; // this mail is from yourself, needed for replaceown

      flags:=flags and not 56;
      if (not NoServer) and (Server.netztyp=nt_Zconnect) then { Zconnect-Prioritaet: }
        if msgprio=10 then flags:=flags or 16        { Direkt = Hoch }
        else if msgprio=20 then flags:=flags or 8;   { Eilmail = Hoechste }

      case rfcprio of                                { RFC - Prioritaet }
        1 : flags:=flags or 8;                       { hoechste }
        2 : flags:=flags or 16;                      { hoch }
        4 : flags:=flags or 24;                      { niedrig }
        5 : flags:=flags or 32;                      { niedrigste }
        end;

      if Uppercase(hdp.MIME.ContentType.MainType) = 'MULTIPART' then
        flags := flags or 4;
        
      dbwriteN(mbase,mb_flags,flags);

      dbWriteNStr(mbase,mb_mimetyp,LowerCase(hdp.MIME.ContentType.Verb));

      if flMark and (i=0) then // and (msgCPpos+1=msgMarkEmpf) then
        msgaddmark;
      AddBezug(hdp,iif(Emp.Count<=1,0,iif(i=0,1,2)));
      
      dbFlushClose(mbase);

      if flUngelesen then SetUngelesen;
    end;

    { --- 3. Schritt: Nachricht ggf. fuer Pollpaket kodieren --------- }

    if (not flintern) then
    begin
      s1.Seek(0,soFromBeginning);

      assert((hdp.typ<>'M') or (EncryptionMethod in [secNone,secMIME]));
      assert((hdp.typ<>'M') or (SigningMethod in [sigNone,sigMIME]));

      if (EncryptionMethod in [secQPC,secDESXP,secPMC1..secPMC3,secPGPOld]) or
         (SigningMethod in  [sigPGPOld]) then
        case EncryptionMethod of
          secQPC:
            EncryptMessage(false,s1); // QPC
          secDESXP:
            EncryptMessage(true, s1); // DES
          secPMC1,
          secPMC2,
          secPMC3:
            pmEncryptMessage(s1);  // PMC-1..3
          secPGPOld:
            XP_PGP.PGP_EncodeStream(s1,hdp,EncrPwd,true,SigningMethod <> sigNone,FidoOrigin);
          else // SigningMethod must be sigPGPOld
            XP_PGP.PGP_EncodeStream(s1,hdp,'',false,true,FidoOrigin);
        end
      else
      if (EncryptionMethod in [secMIME]) or
         (SigningMethod in  [sigMIME]) then
      begin
        assert((hdp.typ='M') or (parts.count=1));

        if hdp.typ<>'M' then
        begin
          // encode the single part NOW for MIME
          FreeAndNil(s1); s1:=TRopeStream.Create;
          with TSendAttach_Part(parts[0]) do
            MIMEWriteContent(s1,TSendAttach_Part(parts[0]),true,
              iifs((IsMessage),sigData,''),
              FidoOrigin);
          hdp.typ:='M';
        end;

        // Encrypt the MIME stream. We can also do signing in this step.
        if EncryptionMethod <> secNone then
          XP_PGP.PGP_MimeEncodeStream(s1,hdp,EncrPwd,SigningMethod <> sigNone)
        else

        // Sign the MIME stream (if it has not been signed during encryption)
        if SigningMethod <> sigNone then
          XP_PGP.PGP_MimeSignStream(s1,hdp);
      end;

    { --- 4. Schritt: Nachricht ins Pollpaket schreiben -------------- }

{      for ii:=1 to msgCPanz-1 do
        hdp.Empfaenger.Add(cc^[ii]); }

      if not flCrash or not MayCrash then
        fn2 := Server.FileName+ExtBoxfile
      else begin
        fn2 := CrashFile(hdp.FirstEmpfaenger);
        SetCrashInfo;
      end;

    // BUG: possible race condition; no other solution possible with
    //      standard PASCAL RTL.
      FreeAndNil(s2);
      if FileExists(fn2) then begin
        s2 := TFileStream.Create(fn2,fmOpenReadWrite);
        s2.Seek(0,soFromEnd);
      end else
        s2 := TFileStream.Create(fn2,fmCreate);

      hdp.groesse := s1.Size;
      hdp.WriteToStream(s2);
//    hdp.Empfaenger.Clear;

      s1.Seek(0,soFromBeginning);
      CopyStream(s1,s2);
    end; // if not flIntern

    end else // if not assigned(FileStr)
    begin
      s1.Seek(0,soFromBeginning);
      CopyStream(s1,FileStream);
    end;

    FreeAndNil(s1);
    FreeAndNil(s2);
    FreeAndNil(s3);
    if NoServer then FreeAndNil(Server);
  end;

  procedure CreateBoxMessages(const Box:string;PM:Boolean);
  var EMP,BCC,KOP: TSTringList;
      E2, K2 : TStringList;
      i: integer;
      server: TXPServer;
      SigM,SigMOverride: TSendSigningMethod;
      EncMOverride: TSendEncryptionMethod;
  label
      restart,
      done;
  begin
    FidoOriginSav := '';
    SigMOverride := sigNone;

    if (EncryptionMethod = secNone) and
      (SigningMethod = sigMIME_or_PGPOld) then
      SigMOverride := sigMIME;

  restart:
    EMP := nil;
    BCC := nil;
    KOP := nil;
    Server := nil;
   try
    EMP := TStringList.Create;
    BCC := TStringList.Create;
    KOP := TStringList.Create;
    Server := TXPServer.CreateByName(Box);

  // -- Adressen f¸r diese Box suchen ----------------------------------
    for i:=0 to EmpfList.Count-1 do
     if EmpfList[i].AddressType in [atNewsgroup,atTo,atCC,atBCC] then
      if (EmpfList[i].PM = PM) and
         (UpperCase(EmpfList[i].BoxName)=Box) and
         (
           (not (SigMOverride in [sigMIME,sigPGPOld])) or
           ((SigMOverride = sigMIME) and (EmpfList[i].EncodingMethod = 8)) or
           ((SigMOverride = sigPgpOld) and (EmpfList[i].EncodingMethod = 9))
         )
      then
        if EmpfList[i].AddressType <> atBCC then
          EMP.Add(EmpfList[i].ZcAddress)
        else
          BCC.Add(EmpfList[i].ZcAddress)
      else
        if EmpfList[i].AddressType <> atBCC then
          KOP.Add(EmpfList[i].ZcAddress);

    if (EMP.Count=0) and (BCC.Count=0) then
      Goto Done; // nothing to do for this box!

   // -- Einzelne Nachrichten bei Verschl¸sselung ---------------------------

    if EncryptionMethod <> secNone then
    begin
      E2 := nil;
      K2 := nil;
      try
        E2 := TStringList.Create;
        K2 := TStringList.Create;

        K2.AddStrings(EMP);
        K2.AddStrings(KOP);

        for i:=0 to EmpfList.Count-1 do
          if EmpfList[i].AddressType in [atNewsgroup,atTo,atCC,atBCC] then
            if (EmpfList[i].PM = PM) and
               (UpperCase(EmpfList[i].BoxName)=Box) then
            begin
              if EmpfList[i].AddressType <> atBCC then
                K2.Delete(0);

              E2.Clear;
              E2.Add(EmpfList[i].ZcAddress);

              EncMOverride := EncryptionMethod;
              SigMOverride := SigningMethod;

              if EncMOverride = secMIME_or_PGPOld then
                if EmpfList[i].EncodingMethod = 9 then
                  EncMOverride := secPGPOld
                else
                  EncMOverride := secMIME;

              if SigMOverride in [sigMIME,sigPGPOld,sigMIME_or_PGPOld] then
                if EncMOverride in [secMIME,secPGPOld,secMIME_or_PGPOld] then
                  SigMOverride := TSendSigningMethod(Ord(EncMOverride))
                else
                  SigMOverride := sigNone;

              CreateSingleMessage(Server,nil,E2,K2,PM,true,EncMOverride,SigMOverride,
                EmpfList[i].EncodingPassword);

              K2.AddStrings(E2);
            end;

      finally
        E2.Free;
        K2.Free;
      end;
      Exit;
    end;

    if(SigMOverride in [sigMIME,sigPGPOld])then
      SigM := SigMOverride
    else
      SigM := SigningMethod;

  // -- Je nach Boxtyp unterschiedliche Behandlung ---------------------

    if Server.Netztyp in netsRFC then begin

    // RFC: benˆtigt keine spezielle Behandlung von BCCs und Kopien.

    // Um sicher zu stellen, dass ‰ltere XPs nicht aus EMP/KOP plˆtzlich
    // ein To/CC machen (w‰re fatal, falls BCCs dabei sind), wird das
    // BCC-Flag gesetzt, wenn BCCs vorhanden sind.

      EMP.AddStrings(BCC);
      CreateSingleMessage(Server,nil,EMP,KOP,PM,BCC.Count>0,secNone,sigM,'');
    end else
    begin

    // Andere Netztypen:

      // Keine BCCs: Einfach Nachricht erzeugen

      if BCC.Count<=0 then begin
        CreateSingleMessage(Server,nil,EMP,KOP,PM,false,secNone,sigM,'');
      end else 

      // BCCs vorhanden: je nachdem, ob das Netz selbst BCCs unterst¸tzt
      
      if ntBCC(Server.netztyp) then begin

        // BCCs werden unterst¸tzt: Eine Nachricht mit den
        // normalen Empf‰ngern (ohne BCC-Flag) und eine mit den BCC-
        // Empf‰ngern (mit BCC-Flag) erzeugen
      
        CreateSingleMessage(Server,nil,EMP,KOP,PM,false,secNone,sigM,'');
        
        KOP.AddStrings(EMP);
        EMP.Clear;
        EMP.AddStrings(BCC);
        CreateSingleMessage(Server,nil,EMP,KOP,PM,true,secNone,sigM,'');

      end else
      begin
        // BCCs werden nicht unterst¸tzt: Eine Nachricht mit den normalen
        // Empf‰ngern erzeugen und jeweils eine f¸r jeden BCC-Empf‰nger
      
        CreateSingleMessage(Server,nil,EMP,KOP,PM,false,secNone,sigM,'');

        KOP.AddStrings(EMP);
        
        for i:=0 to BCC.Count-1 do 
        begin
          EMP.Clear;
          EMP.Add(BCC[i]);
          CreateSingleMessage(Server,nil,EMP,KOP,PM,true,secNone,sigM,'');
        end;
        
      end;
    end;
   done:
    begin end;

   finally
    Server.Free;

    EMP.Free;
    KOP.Free;
    BCC.Free;
   end;

    if SigMOverride = sigMIME then begin
      SigMOverride := sigPGPOld;
      goto restart;
    end;
  end;

  procedure CreateInternalMessage(const Box: string);
  var EMP,KOP: TStringList;
       Server: TXPServer;
  begin
    FidoOriginSav := '';

    EMP := nil;
    KOP := nil;
    Server := nil;
    try
      EMP := TStringList.Create;
      KOP := TStringList.Create;
      EMP.Add(iifs(EmpfList.Count>0,EmpfList[0].ZCAddress,'$/'#$AF'Nix'));
      Server := TXPServer.CreatebyName(Box);
      CreateSingleMessage(Server,nil,EMP,KOP,false,false,secNone,sigNone,'');
    finally
      Server.Free;
      EMP.Free;
      KOP.Free;
    end;
  end;

  procedure CreatePostponedMessage;
  var EMP,KOP: TStringList;
      i: Integer;
  begin
    FidoOriginSav := '';

    EMP := nil;
    KOP := nil;
    try
      EMP := TStringList.Create;
      EMP.Add('$/'#$AF'Unversandt');

      KOP := TStringList.Create;
      for i:=0 to EmpfList.Count-1 do
        if EmpfList[i].AddressType in [atNewsgroup,atTo,atCC,atBCC] then
          KOP.Add(EmpfList[i].ZcAddress);

      flIntern := true;
      CreateSingleMessage(nil,nil,EMP,KOP,false,false,secNone,sigNone,'');
    finally
      EMP.Free;
      KOP.Free;
    end;
  end;

  procedure CreateDatedMessage;
  var FileName: string;
      Box: string;
      EMP,KOP: TStringList;
      Server: TXPServer;
      FileStream: TFileStream;
      AutoexecDB: DB;
      TemporaryChar: Char;
      TemporaryLongint: Longint;
      TemporaryWord: SmallWord;
  begin
    FileStream := Nil;
    EMP := Nil;
    KOP := Nil;
    Server := Nil;

    Box := EmpfList[0].BoxName;

    rmessage(608);    { 'Nachricht speichern...' }
    try
      repeat
        FileName:=AutoVDir+strs(random(10000))+'.MSG';
      until not FileExists(FileName);
      
      Server := TXPServer.CreatebyName(Box);
      EMP := TStringList.Create;
      EMP.Add(Empf1Address);
      KOP := TStringList.Create;

      FileStream := TFileStream.Create(FileName,fmCreate);
      CreateSingleMessage(Server,FileStream,
        EMP,KOP,false,false,secNone,sigNone,'');
      FreeAndNil(FileStream);

      dbOpen(AutoexecDB,AutoFile,1);
      try
        dbAppend(AutoexecDB);
        dbWriteStr(AutoexecDB,'dateiname',FileName);
        dbWriteStr(AutoexecDB,'betreff',Self.Subject);
        dbWriteStr(AutoexecDB,'empfaenger',Self.Empf1Address);
        TemporaryChar := FirstChar(hdp.typ);
        dbWrite(AutoexecDB,'typ',TemporaryChar);
        dbWriteStr(AutoexecDB,'pollbox',Box);
        TemporaryLongint := DateTimeToLongDate(Self.SendDate);
        dbWrite(AutoexecDB,'datum1',TemporaryLongint);
        TemporaryWord := $FFF;
        dbWrite(AutoexecDB,'monate',TemporaryWord);
        TemporaryWord := 3;   { aktiv, lîschen }
        Inc(TemporaryWord,(RFCPrio mod 8) * 32);
        if flRequestMDN then Inc(TemporaryWord,256);
        if flNoArchive then Inc(TemporaryWord,1024);
        dbWrite(AutoexecDB,'flags',TemporaryWord);
        dbWriteXStr(AutoexecDB,'mimetype',256,hdp.mime.ContentType.AssTring);
        dbWriteStr(AutoexecDB,'charset',hdp.charset);
      finally
        dbClose(AutoexecDB);
      end;
    finally
      closebox;
      FreeAndNil(FileStream);
      FreeAndNil(EMP);
      FreeAndNil(KOP);
      FreeAndNil(Server);
    end;
  end;

  procedure BoxSort(l,r:integer);
  var p: byte;
      l2,r2: integer;
  begin
    if r<=l then exit;
    p := ntMessageID(eNetz(Ord(FirstChar(Boxen[(l+r)div 2]))));
    l2 := l;
    r2 := r;
    while l2<r2 do begin
      while (ntMessageID(eNetz(Ord(FirstChar(Boxen[l2]))))<= p) and (l2<r) do inc(l2);
      while (ntMessageID(eNetz(Ord(FirstChar(Boxen[r2])))) > p) and (r2>l) do dec(r2);
      if l2<r2 then Boxen.Exchange(l2,r2);
    end;
    BoxSort(l,r2-1);
    BoxSort(l2,r);
  end;

var i : integer;
begin
  Hdp := nil;
  try

  if SentOK then exit;
  CheckEmpfaengerList(EmpfList,false,false);
  CorrectEncryptionMethod;
  if Boxen.Count<0 then exit;

  if flShow then begin
    message(getres2(610,iif(flIntern,4,5))+   { 'Speichere Nachricht in ' / 'Sende Nachricht an ' }
            LeftStr(Empf1Address,50)+' ');
  end;
  try

// Wir m¸ssen schon hier einen Eintrag f¸r unsere Nachricht machen,
// damit wir schon eine Message-ID erzeugen kˆnnen
  dbAppend(mbase);            { neue mbase.INT_NR fuer MessageID }
  Count := 0;

  if not IsNaN(SendDate) then
  begin
    CreateDatedMessage;
  end else
  if flIntern then 
    CreateInternalMessage(DefaultBox)
  else
  if flPostPone then
    CreatePostponedMessage
  else
  begin

  // RFC- und ZConnect-Boxen m¸ssen nach vorn sortiert werden, damit wir
  // eine Message-ID erzeugen kˆnnen, die einen FQDN enth‰lt.
  // FTN erh‰lt ggf. die MSGID nach Gatebau-MSGID.DOC.
  // Maus muss nach hinten, damit wir ggf. eine Fremd-ID mittransportieren
  // kˆnnen.
    BoxSort(0,Boxen.Count-1);
    MidType := midNone;

    for i:=0 to Boxen.Count-1 do begin
      if Has_PM then CreateBoxMessages(UpperCase(Mid(Boxen[i],2)),true);
      if Has_AM then CreateBoxMessages(UpperCase(Mid(Boxen[i],2)),false);
    end;
  end;

  finally
    if flShow then
      closebox;
  end;
  finally
    FreeAndNil(Hdp);
  end;
end;
                                                      
//
// $Log: xpsendmessage_create.inc,v $
// Revision 1.26  2004/01/25 00:52:40  cl
// - FIX: [ 815945 ] Multipart unter N/U/Z
//
// Revision 1.25  2003/10/29 11:18:08  cl
// - MausTausch I
//
// Revision 1.24  2003/10/21 21:43:17  cl
// - changed version info to new scheme
//
// Revision 1.23  2003/10/21 21:25:04  cl
// - Changed THeader.MIME to use TMimeContentType and TMimeDisposition objects
// - Changed MausTausch headers for Maus-internal IDs: MID/BEZ => maus_*, org_* => MID/BEZ,
//
// Revision 1.22  2003/10/18 17:14:49  mk
// - persistent open database boxenfile (DB: boxbase)
//
// Revision 1.21  2003/09/07 14:49:42  cl
// - send window: postpone message
//   CLOESES task #76797: "Sendefenster: Parken"
//
// Revision 1.20  2003/09/06 23:03:08  cl
// - send window - time-shifted sending of message
//   cLOSES task #76792: Sendefenster: Datum
//
// Revision 1.19  2003/08/30 22:39:17  cl
// - fixes for FPC
//
// Revision 1.18  2003/08/30 22:19:27  cl
// - send window: select encryption and signature method
// - CLOSES Task #76790 Sendefenster: Kodieren/Sicherheit
//
// Revision 1.17  2003/08/24 23:33:27  cl
// - Sendefenster: Priorit‰t setzen (RFC), Keine Signatur (ohneSig),
//   Nachricht lˆschen (nach Versand), Empfangsbest‰tigungen,
//   X-No-Archive setzen
// - updated on-line help
//
// CLOSES:
//   task #76791 Sendefenster: Empfangsbest‰tigungen
//   task #76793 Sendefenster: ohne Sig
//   task #76794 Sendefenster: Priorit‰t
//   task #76796 Sendefenster: Lˆschen
//
// Revision 1.16  2003/08/22 19:08:14  mk
// - second fix for #733047: Bad User-Agent header syntax
//
// Revision 1.15  2003/08/15 21:36:19  mk
// - fixed #733047: Bad User-Agent header syntax
//
// Revision 1.14  2003/04/28 20:18:58  cl
// - CRLF at the end of a text file is now uniformly handled as the start of
//   an additional line.
//
// Revision 1.13  2003/04/03 13:57:49  mk
// - ops, typo
//
// Revision 1.12  2003/04/03 13:34:06  mk
// - POP3 and SMTP-Port is now configurable in *.bfg
//
// Revision 1.11  2003/01/13 22:48:51  cl
// - enabled TRopeStream
//
// Revision 1.10  2003/01/13 22:05:19  cl
// - send window rewrite - Fido adaptions
// - new address handling - Fido adaptions and cleanups
//
// Revision 1.9  2003/01/07 00:56:47  cl
// - send window rewrite -- part II:
//   . added support for Reply-To/(Mail-)Followup-To
//   . added support to add addresses from quoted message/group list/user list
//
// - new address handling -- part II:
//   . added support for extended Reply-To syntax (multiple addresses and group syntax)
//   . added support for Mail-Followup-To, Mail-Reply-To (incoming)
//
// - changed "reply-to-all":
//   . different default for Ctrl-P and Ctrl-B
//   . more addresses can be added directly from send window
//
// Revision 1.8  2002/12/14 07:31:40  dodi
// - using new types
//
// Revision 1.7  2002/11/14 21:06:13  cl
// - DoSend/send window rewrite -- part I
//
// Revision 1.6  2002/08/09 22:47:09  cl
// - Fixed [ 587379 ] 3.9: Netzanrufbericht ohne Subject
//
// Revision 1.5  2002/08/09 22:17:39  cl
// - Fixed #588187 3.9: keine UseNet Postings
//
// Revision 1.4  2002/05/26 12:26:11  ma
// - using "email" db field instead of "user" db field for email now
//   email may be longer than 30 chars now
//   EMAIL ADDRESS HAS TO BE RE-ENTERED IN SERVER SETTINGS
//
// Revision 1.3  2002/05/20 15:22:44  cl
// - BUGFIX: newly created messages were on hold even when they should not
//
// Revision 1.2  2002/05/09 15:18:06  cl
// - fixed internal messages
//
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//

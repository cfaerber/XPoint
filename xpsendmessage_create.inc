{  $Id$

   OpenXP/32: DoSend - Message Creation
   
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

procedure CreateMessages;
var Count: Integer;
    MidType: byte;

  procedure CreateSingleMessage(
    Box    : String;
    Netztyp: Byte;
    EMP,KOP: TStringList;
    PM,BCC : Boolean );
   
  var
    I,J    : integer;
    L      : Longint;
    Ablage : integer;
    Tmp    : TAddressList;

    MPSize : Longint;
    MPAdr  : Longint;

    NewMidType : byte;
    s      : string;
    
  begin
    { --- 0. Schritt: Ein paar vorarbeiten ---------------------------- }
  
    Hdp.Clear;
    Hdp.Netztyp:=Netztyp;

    { --- 1. Schritt: Body erzeugen ----------------------------------- }

    if ntMIME(netztyp) then
      hdp.MIME.mversion := '1.0';

    if (not partsex) and assigned(sData.orghdp) then
    begin
      // just pass-through
      s1 := TFileStream.Create(datei,fmOpenRead);

      hdp.typ           := sData.orghdp.typ;
      hdp.mime.ctype    := sData.orghdp.mime.ctype;
      hdp.mime.encoding := sData.orghdp.mime.encoding;
      hdp.mime.disposition := sData.orghdp.mime.disposition;
      hdp.mime.cid      := sData.orghdp.mime.cid;
      hdp.charset       := sData.orghdp.charset;
      hdp.x_charset     := sData.orghdp.x_charset;
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

    hdp.Empfaenger.Assign(EMP);
    hdp.Kopien    .Assign(KOP);

    hdp.UTo := RFCWriteAddressList(sData.EmpfList,nil,[atTo]);
    hdp.CC  := RFCWriteAddressList(sData.EmpfList,nil,[atCC]);
    hdp.BCC := RFCWriteAddressList(sData.EmpfList,nil,[atBCC]);

    hdp.archive := PM;
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
      5 : begin
            hdp.absender:=username+'@'+iifs(aliaspt,pointname,box)+domain;
            hdp.real_box:=box;  { Test: 'X-XP-BOX' auch bei ZConnect }
          end;
      6 : begin
            hdp.absender:=iifs(sData.SenderMail='',
                               username+'@'+iifs(aliaspt,box+ntServerDomain(box),pointname+domain),
                               sData.SenderMail);
            hdp.real_box:=box;
          end;
      7 : begin
            hdp.absender:=username+'@'+box+';'+pointname;
            hdp.real_box:=box;
          end;
      8 : begin
            hdp.absender:=iifs(sData.SenderMail='',username,sData.SenderMail);
            hdp.real_box:=box;
          end;
    end;
    hdp.realname:=realname;
    if (sendFlags and sendWAB<>0) and ntAdrCompatible(sData.onetztyp,netztyp)
    then begin
      hdp.wab:=hdp.absender; hdp.war:=hdp.realname;
      hdp.absender:=sData.oab; hdp.realname:=sData.oar;
      { sData.oab:=''; }
      end;

    if netztyp=nt_Magic then
      hdp.hd_point:=pointname;
    hdp.replyto := sData.Replyto;
    if (not pm) and (sData.followup.count>0) then
      hdp.followup.assign(sData.followup);
    hdp.Keywords:=sData.keywords;
    hdp.Summary:=sData.summary;
    if  ntAdrCompatible(sData.onetztyp,netztyp)
    then begin
      if sendFlags and sendWAB=0 then begin
        hdp.oab:=sData.oab; hdp.oar:=sData.oar;
        end;
      hdp.oem.Assign(sData.oem);
      end;
      hdp.ReplyGroup:=sData.ReplyGroup;
    if not pm then
      hdp.distribution:=sData.distribute;
    hdp.quotestring:=sData.quotestr;
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


  // == Message-ID erzeugen ============================================
    NewMidType := ntMessageID(netztyp);

  // -- Falls noch keine generiert wurde => jetzt machen ---------------
    
    if sData.MsgID = '' then
    begin
      MidType := NewMidType;
      sData.MsgID := MessageID(MidType);
    end;

  // -- Kompatible IDs weiterverwenden ---------------------------------
    if(MidType=NewMidType)or((MidType in [midRFC,midZConnect])and(NewMidType in [midRFC,midZConnect])) then
      hdp.msgid := sData.msgid
    else
    
  // -- Message-ID ggf. umwandeln --------------------------------------

    // -- ZC/RFC => Fido -----------------------------------------------    
      if (NewMidType=midFido) and (MidType in [midRFC,midZConnect]) then
      begin
        if pm then
          hdp.msgid := '<' + sData.MsgID + '> ' + 
            LowerCase(Hex(CRC32Str('<'+sData.MsgID+'>'),8))
        else
          hdp.msgid := '<' + sData.MsgID + '> ' + 
            LowerCase(Hex(CRC32Str('<'+sData.MsgID+'>'+
              UpperCase(Mid(hdp.FirstEmpfaenger,1+RightPos('/',hdp.FirstEmpfaenger)))),8));
      end else

    // -- geht nicht; neue ID erzeugen ---------------------------------

      begin
        MidType := NewMidType;
        sData.MsgID := MessageID(MidType);
        hdp.msgid := sData.msgid
      end;

    if (_beznet>=0) and ntMIDCompatible(_beznet,netztyp) then
      if sData.References.IndexOf(_bezug)=-1 then
        sData.References.Add(_bezug);

    if (_beznet>=0) then  // bugfix fÅr VP
      if ntOrigID(netztyp) and ntMIDCompatible(_Beznet,netztyp) then
        hdp.org_xref:=_orgref;

    hdp.replypath:=_replypath;
//  hdp.typ:=iifs(binary,'B','T');
    hdp.programm:=xp_xp+' '+trim(verstr)+' '+trim(pformstr)+' '+trim(betastr)
                  {$IFDEF Snapshot} + ' @ ' + compiletime {$ENDIF};
    hdp.organisation:=orga;
    if sData.ersetzt<>''then hdp.ersetzt:=sData.ersetzt;
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
      if not pm and (netztyp in netsFTN) then hdp.fido_to:=brettalle;
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
    hdp.nokop:=BCC;
//    if umlaute=0 then
//      if netztyp=nt_Fido then
//        hdp.x_charset:='IBMPC 2'   { s. FSC-0054, grmpf }
//      else if netztyp in netsRFC then
//        if FileContainsUmlaut then hdp.x_charset:='ISO-8859-1';
//    if iso then
//      hdp.charset:='ISO1';
    if assigned(sData.orghdp) then
      with sData.orghdp do begin
        { hdp.zdatum:=zdatum; hdp.orgdate:=true;  !! Unversandt/* !! }
        hdp.organisation:=organisation;
        hdp.ReplyTo := ReplyTo;
        hdp.datei:=datei; hdp.ddatum:=ddatum;
        end;
    if _sendmaps then
      hdp.replyto := '';
    hdp.References.Assign(sData.References);

    hdp.groesse:=s1.Size;
    s1.Seek(0,soFromBeginning);

    fn2:=TempS(s1.Size+4000);
    s2 := TFileStream.Create(fn2,fmCreate);

    hdp.WriteToStream(s2);      // Header erzeugen
    CopyStream(s1,s2);  // Body anh‰ngen

    s2.Free; {s2:=nil;}

    { --- 3. Schritt: Nachricht in Datenbank ablegen ------------------ }

    // 3a) Bretter/User suchen, evt. anlegen oder lˆschen
    for i:=EMP.Count-1 downto 0 do
      if PM then 
      begin
        Emp[i]:=Addr2DB(EMP[i]);
      
        dbSeek(ubase,uiName,UpperCase(Emp[i]));
        if not dbFound then             { kein bekannter B. Nutzer => anlegen }
          AddNewUser(Emp[i],Box);
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
    begin                                  { => einfach das erste nehmen und anlegen! }
      dbSeek(bbase,biBrett,UpperCase('$/ØNix'));
      if not dbFound then
        AddNewBrett('$/ØNix','','',StdHalteZeit,IntGruppe,0);
      EMP.Add(mbrettd('$',BBase));
      dbWriteN(bbase,bb_ldatum,sendedat);    { Brettdatum neu setzen }
    end;

    // 3b) Eintr‰ge erzeugen
    for i:=0 to EMP.Count-1 do 
    begin
      if Count<>0 then
        dbAppend(mbase);      { Neuer Eintrag (hier wg. Message-ID      }
      Inc(Count);
      Ablage := iif(ntZConnect(netztyp),10,0);
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

      L := netztyp;
      if hdp.GetLastReference <> '' then inc(l,$100); // rÅckwÑrts-verkettet
      if FileAttach then inc(l,$200);
      if hdp.pm_reply then inc(l,$400);
      if (hdp.wab<>'') or (hdp.oem.Count > 0) then inc(l,$800);
      if (hdp.typ='T')and(hdp.charset='ISO1')then inc(l,$2000);
      if flPGPsig then inc(l,$4000);
      if Emp.Count>1 then begin
        inc(l,longint(Min(255,EMP.Count)) shl 16);
        inc(l,longint(Succ(i mod 128))    shl 24);        { Empfaengernummer }
      end;
      dbWriteN(mbase,mb_netztyp,l);
      
      shortmid:= Hdp.BinaryMsgID;
      dbWriteNStr(mbase,mb_msgid,shortmid);
      dbWriteNStr(mbase,mb_betreff,hdp.betreff);
      dbWriteNStr(mbase,mb_absender,hdp.absender);
      dbWriteNStr(mbase,mb_mimetyp,LowerCase(Trim(LeftStr(hdp.mime.ctype,CPosX(';',hdp.mime.ctype)-1))));
      l:=ixdat(hdp.datum);
      dbWriteN(mbase,mb_origdatum,l);
      dbWriteN(mbase,mb_empfdatum,sendedat);
      dbWriteN(mbase,mb_groesse,hdp.groesse);
      hdptyp := hdp.typChar;
      dbWriteN(mbase,mb_typ,hdptyp);
      if ntEditBrettempf(netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.fido_to)
      else if ntRealname(netztyp) then
        dbWriteNStr(mbase,mb_name,hdp.realname);

      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      if sendFlags and sendHalt<>0 then b:=1
      else if flLoesch then b:=2
      else if (pm and not HaltOwnPM) then b:=0 { Eigene Nachrichten Halten gilt nicht fuer Mails }
      else b:=1;
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

      if (sendFlags and sendMark<>0) and (msgCPpos+1=msgMarkEmpf) then
        msgaddmark;
      AddBezug(hdp,iif(Emp.Count<=1,0,iif(i=0,1,2)));
      
      dbFlushClose(mbase);
    end;

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

{      for ii:=1 to msgCPanz-1 do
        hdp.Empfaenger.Add(cc^[ii]); }

      if not flCrash or not MayCrash then
        fn2 := boxfile+ExtBoxfile
      else begin
        fn2 := CrashFile(hdp.FirstEmpfaenger);
        SetCrashInfo;
      end;

    // BUG: possible race condition; no other solution possible with
    //      standard PASCAL RTL.
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
      s2.Free; {s2:=nil;}

    end; // not intern

    s1.Free; s1:=nil;
  end;

  procedure CreateBoxMessages(const Box:string;PM:Boolean);
  var EMP,BCC,KOP: TSTringList;
      Netztyp: Byte;
      i:integer;
      d: DB;
  begin
    EMP := nil;
    BCC := nil;
    KOP := nil;
   try
    EMP := TStringList.Create;
    BCC := TStringList.Create;
    KOP := TStringList.Create;

    Netztyp := 0;

  // -- Adressen f¸r diese Box suchen ----------------------------------
    for i:=0 to sData.EmpfList.Count-1 do
     if sData.EmpfList[i].AddressType in [atNewsgroup,atTo] then
      if (sData.EmpfList[i].PM = PM) and (sData.EmpfList[i].BoxName=Box) then begin
        if Netztyp=0 then Netztyp := sData.EmpfList[i].Netztyp;
        EMP.Add(sData.EmpfList[i].ZcAddress);
      end else
        KOP.Add(sData.EmpfList[i].ZcAddress);
      
    for i:=0 to sData.EmpfList.Count-1 do
     if sData.EmpfList[i].AddressType in [atcc] then
      if (sData.EmpfList[i].PM = PM) and (sData.EmpfList[i].BoxName=Box) then begin
        if Netztyp=0 then Netztyp := sData.EmpfList[i].Netztyp;
        EMP.Add(sData.EmpfList[i].ZcAddress);
      end else
        KOP.Add(sData.EmpfList[i].ZcAddress);

    for i:=0 to sData.EmpfList.Count-1 do
     if sData.EmpfList[i].AddressType in [atbcc] then
      if (sData.EmpfList[i].PM = PM) and (sData.EmpfList[i].BoxName=Box) then begin
        if Netztyp=0 then Netztyp := sData.EmpfList[i].Netztyp;
        BCC.Add(sData.EmpfList[i].ZcAddress);
      end;

    if (EMP.Count=0) and (BCC.Count=0) then
      exit; // nothing to do for this box!

     dbOpen(d,BoxenFile,1);
     dbSeek(d,boiName,Uppercase(Box));
     LoadBoxData(d);
     dbClose(d);
   
  // -- Je nach Boxtyp unterschiedliche Behandlung ---------------------

    if netztyp in netsRFC then begin

    // RFC: benˆtigt keine spezielle Behandlung von BCCs und Kopien.
    
    // Um sicher zu stellen, dass ‰ltere XPs nicht aus EMP/KOP plˆtzlich
    // ein To/CC machen (w‰re fatal, falls BCCs dabei sind), wird das 
    // BCC-Flag gesetzt, wenn BCCs vorhanden sind
    
      EMP.AddStrings(BCC);
      CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,BCC.Count>0);
    end else begin

    // Andere Netztypen:

      // Keine BCCs: Einfach Nachricht erzeugen       
    
      if BCC.Count<=0 then begin
        CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,false);
      end else 

      // BCCs vorhanden: je nachdem, ob das Netz selbst BCCs unterst¸tzt
      
      if ntBCC(netztyp) then begin

        // BCCs werden unterst¸tzt: Jeweils eine Nachricht mit den 
        // normalen Empf‰ngern (ohne BCC-Flag) und eine mit den BCC-
        // Empf‰ngern (mit BCC-Flag) erzeugen
      
        CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,false);
        
        KOP.AddStrings(EMP);
        EMP.Clear;
        EMP.AddStrings(BCC);
        CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,true);
        
      end else
      begin

        // BCCs werden nicht unterst¸tzt: Eine Nachricht mit den normalen
        // Empf‰ngern erzeugen und jeweils eine f¸r jeden BCC-Empf‰nger
      
        CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,false);

        KOP.AddStrings(EMP);
        
        for i:=0 to BCC.Count-1 do 
        begin
          EMP.Clear;
          EMP.Add(BCC[i]);
          CreateSingleMessage(Box,Netztyp,EMP,KOP,PM,true);
        end;
        
      end;
    end;

   finally
    EMP.Free;
    KOP.Free;
    BCC.Free;
   end;
  end;

  procedure BoxSort(l,r:integer);
  var p: byte;
      l2,r2: integer;
  begin
    if r<=l then exit;
    p := ntMessageID(Ord(FirstChar(sData.Boxen[(l+r)div 2])));
    l2 := l;
    r2 := r;
    while l2<r2 do begin
      while (ntMessageID(Ord(FirstChar(sData.Boxen[l2]))) < p) and (l2<=r) do inc(l2);
      while (ntMessageID(Ord(FirstChar(sData.Boxen[r2]))) > p) and (r2>=l) do dec(r2);
      if l2<r2 then sData.Boxen.Exchange(l2,r2);
    end;
    BoxSort(l,r2-1);
    BoxSort(l2,r);
  end;

var i : integer;
begin
  if sData.Boxen.Count<0 then exit;

  SendMbox;

// Wir m¸ssen schon hier einen Eintrag f¸r unsere Nachricht machen,
// damit wir schon eine Message-ID erzeugen kˆnnen
  dbAppend(mbase);            { neue mbase.INT_NR fuer MessageID }
  Count := 0;

// RFC- und ZConnect-Boxen m¸ssen nach vorn sortiert werden, damit wir
// eine Message-ID erzeugen kˆnnen, die einen FQDN enth‰lt.
// FTN erh‰lt ggf. die MSGID nach Gatebau-MSGID.DOC.
// Maus muss nach hinten, damit wir ggf. eine Fremd-ID mittransportieren
// kˆnnen.
  BoxSort(0,sData.Boxen.Count-1);
  MidType := midNone;

  for i:=0 to sData.Boxen.Count-1 do begin
    if sData.has_pm then CreateBoxMessages(Mid(sData.Boxen[i],2),true);
    if sData.has_am then CreateBoxMessages(Mid(sData.Boxen[i],2),false);
  end;

  closebox;
end;

//
// $Log$
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//

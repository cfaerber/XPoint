{   $Id$

    Subprocedures and functions for xpsendmessage.DoSend: MIME
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus F"arber

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


function MIMESaveCharsetAsCP437(const s:string):Boolean;
begin
  // These charsets are saved as-is in our local database
  result := (s='') or (s='US-ASCII') or (s='IBM437');
end;

procedure MIMEDecompose;
var s: TStream;
    c: TMimeContentType;

  function GetByte:Char;
  begin
    s.ReadBuffer(result,1);
  end;

  function GetLine:String;
  begin
    result:='';
    repeat
      try
        result:=result+GetByte;
      except
        if result='' then raise;
        exit;
      end;
    until RightStr(result,2)=#13#10;
  end;

  procedure ReadParts;
  var pa: TSendAttach_Part;
      d:TStream;
      t,t2:string;
      h,n: string;
      i: Integer;
      finished:boolean;
  begin
    pa := nil;
    d  := nil;
    finished:=false;

    try
      repeat
        t := GetLine;
        t2:= Trim(t);

        if not ( ((Length(t2)-Length(c.Boundary)) in [2,4]) and // this is only
           (t2[1]='-') and (t2[2]='-') and                // optimization
           ( (t2 = '--' + c.Boundary) or             // and this is the
             (t2 = '--' + c.Boundary + '--') ) ) then  // real test
        begin
          if assigned(d) then
            d.Write(t[1],Length(t));
          if assigned(pa) then
            pa.Analyzed.Write(t[1],Length(t));
        end else begin
          // -- 1: Write Current Part to File ----------------------------

          if assigned(pa) then
          begin
            pa.FileEncoding:=pa.ContentEncoding;
            pa.FileCharset :=pa.ContentCharset;
            pa.IsFile      :=true; // (UpperCase(pa.ContentType.MainType)<>'TEXT')
                           // or(UpperCase(pa.ContentType.SubType)<>'PLAIN')
                           // or(pa.FileNameO<>'');
            pa.IsTemp      :=true;
            pa.IsExtract   :=true;

            pa.FileSize    :=d.Size;

            if pa.Analyzed.EOLAllowed[MimeEolCrLf] and
              not pa.Analyzed.IsBinary then
              pa.FileEOL     :=MimeEolCRLF
            else
              pa.FileEOL     :=MimeEolNone;
              
            parts.Add(pa); pa:=nil;
            d.Free; d :=nil;
          end;

          if RightStr(t2,2) = '--' then
            finished := true
          else begin

          // -- 2: Clear everything --------------------------------------

          pa := TSendAttach_Part.Create;
          pa.ContentType.AsString := 'text/plain';
          pa.ContentDisposition.DispoType := MimeDispositionInline;
          pa.ContentEncoding := MimeEncoding8Bit;

          pa.FileName := TempS(s.Size-s.Position);

          d  := TFileStream.Create(pa.FileName,fmCreate);

          // -- 3: Read header -------------------------------------------

          h := '';

          repeat
            t := GetLine;
            t2:= Trim(t);

            if (Length(t2)>3) and (t[1] in [#9,' ']) then
              h:=h+iifs(length(h)>0,' ','')+t2      // continuation line
            else
            begin
              if h<>'' then
              begin
                i := CPosX(':',h);
                n := UpperCase(Trim(LeftStr(h,i-1))); // header name
                h := Trim(Mid(h,i+1));            // header content

                if n='CONTENT-TYPE' then
                  pa.ContentType.AsString:=RFCRemoveComments(h)
                else if n='CONTENT-DISPOSITION' then
                  pa.ContentDisposition.AsString:=RFCRemoveComments(h)
                else if n='CONTENT-TRANSFER-ENCODING' then
                  pa.ContentEncoding := MimeGetEncodingFromName(RFCRemoveComments(h))
                else if n='CONTENT-DESCRIPTION' then begin
                  RFC2047_Decode(h,csCP437);
                  pa.ContentDescription:=h;
                end;
              end;
              h:=t2;
            end;
          until t2='';
          end;
        end // if *boundary*
      until finished;
    except
      on EReadError do; // ignore
    end; // try;
    d.Free;
    pa.Free;
  end;

begin
  if partsex then exit; // Already done

  try
    s := TFileStream.Create(datei,fmOpenRead);
    try
      c := TMimeContentType.Create(sData^.OrgHdp.Mime.Ctype);
      try
        with sData^.OrgHdp do
        begin
          if Boundary<>'' then c.Boundary := Boundary;
          if Charset <>'' then c.Charset  := ZCCharsetToMIME(Charset);
        end;
        ReadParts;
      finally
        c.Free;
      end;
    finally
      s.Free;
    end;
  except
  end;
  partsex := true;
end;

procedure MIMEWriteContent(s1:TStream;pa:TSendAttach_Part;DoEncode:Boolean;Sigfile:String);
var s2,s3,s4,s5,s0: TStream;
begin
  s2:=nil; s3:=nil; s4:=nil; {s5:=nil;} s0:=nil;

  with pa do
  begin

    if FileExists(FileName) then
      s5 := TFileStream.Create(FileName,fmOpenRead)
    else
      s5 := TStringStream.Create(''); // ???

    if (FileEncoding<>ContentEncoding) or
       ((not DoEncode) and (FileEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable])) then
    begin
      if FileEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable] then
      begin
        s0 := s5;
        s5 := MimeCreateDecoder(FileEncoding,s0);
      end;

      if ContentEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable] then
      if DoEncode then
      begin
        s2 := s1;
        s1 := MimeCreateEncoder(ContentEncoding,s2,FileEOL in [MimeEOLCR,MimeEOLLF,MimeEOLCRLF]);
      end;
    end;

    if ContentType.NeedCharset and (ContentCharset <> FileCharset) then
    if DoEncode or not MIMESaveCharsetAsCP437(FileCharset) then
    begin
      s3 := s1;
      s1 := TCharsetEncodingStream.Create( s3,
        iifs((FileCharset<>'') and IsKnownCharset(FileCharset),FileCharset,'IBM437'),
        iifs((ContentCharset<>'') and IsKnownCharset(FileCharset),ContentCharset,'ISO-8859-1') );
    end;

    if FileEOL in [MimeEOLCR,MimeEOLLF] then
    begin
      s4 := s1;
      s1 := MimeCreateEolConverter(FileEOL,s4)
    end;

    CopyStream(s5,s1);

    s5.Free; s5:=nil;
    s0.Free; {s0:=nil;}

    if assigned(s4) then begin s1.Free; s1:=s4; {s4:=nil;} end;
    if assigned(s3) then begin s1.Free; s1:=s3; s3:=nil; end;

    if (SigFile<>'') and FileExists(SigFile) then
    begin
      if DoEncode or not MIMESaveCharsetAsCP437(FileCharset) then
      begin
        s3 := s1;
        s1 := TCharsetEncodingStream.Create(s3,'IBM437',ContentCharset);
      end;
    try
      s5 := TFileStream.Create(sigfile,fmOpenRead);
      CopyStream(s5,s1);
    except
    end;
      s5.Free; {s5:=nil;}
      if assigned(s3) then begin s1.Free; s1:=s3; {s3:=nil;} end;
    end;

    if assigned(s2) then begin s1.Free; {s1:=s2; s2:=nil;} end;
  end; // with
end;

procedure MIMEWriteContentWithHeaders(s:TStream;pa:TSendAttach_Part;Sigfile:String);
begin
  with pa do
  begin
    if (not ContentType.NeedCharset) or
       (MimeCharsetCanonicalName(ContentCharset)='US-ASCII') then ContentCharset := '';

    writeln_s(s1,'Content-Type: ' + ContentType.AsFoldedString(76-14,76,#13#10,true));
    writeln_s(s1,'Content-Transfer-Encoding: ' + MimeEncodingNames[ContentEncoding]);

    if (FileNameO<>'') or (not IsNAN(FileAccess)) or (not IsNan(FileModify)) or
       (not IsNAN(FileCreate)) or (ContentDisposition.DispoType<>MimeDispositionInline) then
      writeln_s(s1,'Content-Disposition: ' + ContentDisposition.AsFoldedString(76-21,76,#13#10,true));
    if ContentDescription <> '' then
      writeln_s(s1,'Content-Description: ' + ContentDescription);
    writeln_s(s1,'');
  end;
  MIMEWriteContent(s,pa,true,sigfile);
end;

//
// $Log$
// Revision 1.2  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.1  2001/09/08 14:42:09  cl
// - added Multipart-MIME support
// - added PGP/MIME support
// - adaptions/fixes for MIME support
// - adaptions/fixes for PGP/MIME support
//


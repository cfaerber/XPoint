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

procedure MIMEWriteContent(s1:TStream;pa:TSendAttach_Part;DoEncode:Boolean;Sigfile:String;FidoOrigin:String);
var ins,outs:TStream;
begin
  // The stream passed must not be destroyed!
  outs := TNullCodecStream.Create;
 try
  TNullCodecStream(outs).OtherStream := s1;
  TNullCodecStream(outs).DestroyOtherStream := false;

  if FileExists(pa.FileName) then
    ins := TFileStream.Create(pa.FileName,fmOpenRead)
  else
    ins := TMemoryStream.Create; // WTF?

  // we have to change the encoding if (a) the file is encoded
  // differently than we want it to be, or (b) the file is encoeded but
  // we don't want to have it encoded (binary/text messages)
  if (pa.FileEncoding<>pa.ContentEncoding) or
     ((not DoEncode) and (pa.FileEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable])) then
  begin
    // decode the existing file if necessary
    if pa.FileEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable] then
      ConnectStream(ins,MimeCreateDecoder(pa.FileEncoding));

    // encode it for output
    if pa.ContentEncoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable] then
      if DoEncode then
        ConnectStream(outs,MimeCreateEncoder(pa.ContentEncoding,pa.FileEOL in [MimeEOLCR,MimeEOLLF,MimeEOLCRLF]));
  end;

  // TODO: it would be better if we could just connect charset and EOL
  // *de*coding streams to the input stream (but these are not finished
  // yet).

  // recode the charset
  if pa.ContentType.NeedCharset and (pa.ContentCharset <> pa.FileCharset) then
    if DoEncode or not MIMESaveCharsetAsCP437(pa.FileCharset) then
      ConnectStream(outs,TCharsetEncoderStream.Create(
        iifs(IsKnownCharset(pa.FileCharset),pa.FileCharset,'IBM437'),
        iifs(IsKnownCharset(pa.ContentCharset),pa.ContentCharset,'ISO-8859-1') ));

  if pa.FileEOL in [MimeEOLCR,MimeEOLLF] then
    ConnectStream(outs,MimeCreateEolConverter(pa.FileEOL));

  CopyStream(ins,outs);

  ins.Free;

  if ((SigFile<>'') and FileExists(SigFile)) or (FidoOrigin<>'') then
  begin
    if outs is TMimeEolCodecStream then
      UnconnectStream(outs);

    if outs is TCharsetCodecStream then
      UnconnectStream(outs);

    if DoEncode or not MIMESaveCharsetAsCP437(pa.FileCharset) then
      ConnectStream(outs,TCharsetEncoderStream.Create('IBM437',pa.ContentCharset));

    if ((SigFile<>'') and FileExists(SigFile)) then
    begin
      ins := TFileStream.Create(sigfile,fmOpenRead);
      try
        CopyStream(ins,outs);
      except
      end;
      ins.Free;
    end;

    if FidoOrigin<>'' then
      writeln_s(outs,FidoOrigin);
  end;

 finally
  assert(pointer(outs)<>pointer(s1));
  outs.Free; // will not destroy s1!
 end;
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
  MIMEWriteContent(s,pa,true,sigfile,'');
end;

{$IFDEF 0}
procedure UUEncodeWriteContent(s1:TStream;pa:TSendAttach_Part;DoEncode:Boolean;Sigfile:String;FidoOrigin:String);
var ins,outs:TStream;
begin
  // The stream passed must not be destroyed!
  outs := TNullCodecStream.Create;
  try




  finally
    assert(pointer(outs)<>pointer(s1));
    outs.Free; // will not destroy s1!
  end;
end;
{$ENDIF}

//
// $Log$
// Revision 1.5  2001/09/16 23:01:20  cl
// - BUGFIX: Fido tearline now added
//
// Revision 1.4  2001/09/10 17:25:50  cl
// - BUGFIX: crash on line end conversion
//
// Revision 1.3  2001/09/09 17:40:47  cl
// - moved common code between alle en-/decoding streams to a base class
// - all en-/decoding streams can now destruct the other stream
// - much more elegant way to connect en-/decoding streams to each other
//
// Revision 1.2  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.1  2001/09/08 14:42:09  cl
// - added Multipart-MIME support
// - added PGP/MIME support
// - adaptions/fixes for MIME support
// - adaptions/fixes for PGP/MIME support
//


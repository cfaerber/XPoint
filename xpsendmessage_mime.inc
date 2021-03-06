{   $Id$

    Subprocedures and functions for xpsendmessage.DoSend: MIME
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and Claus F"arber

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

class function TSendUUData.MIMESaveCharsetAsCP437(const s:string):Boolean;
begin
  // These charsets are saved as-is in our local database
  result := (s='') or (s='US-ASCII') or (s='IBM437');
end;

procedure TSendUUData.ClearParts;
var ii:integer;
begin
  for ii:=0 to Parts.count-1 do
    TObject(Parts[ii]).Free;
  Parts.Clear;
  PartsEx := false;
  PartFile := '';
  PartFTmp := false;
end;

procedure TSendUUData.MIMEDecompose;
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
      SavedT: String;
  begin
    pa := nil;
    d  := nil;
    finished:=false;

    try
      SavedT := '';
      repeat
        t := GetLine;
        t2:= Trim(t);

        i := Length(t2)-Length(c.Boundary);
        if not ( ((i = 2) or (i = 4)) and             // this is only
           (t2[1]='-') and (t2[2]='-') and            // optimization
           ( (t2 = '--' + c.Boundary) or              // and this is the
             (t2 = '--' + c.Boundary + '--') ) ) then // real test
        begin
          if assigned(d) and (SavedT <> '') then
            d.Write(SavedT[1],Length(SavedT));
          if assigned(pa)and (SavedT <> '') then
            pa.Analyzed.Write(Savedt[1],Length(Savedt));
          SavedT := t;
        end else
        begin
          // handle last not saved T
          // the last two bytes are not part of the mime content
          if assigned(d) then
            d.Write(SavedT[1],Length(SavedT)-2);
          if assigned(pa) then
            pa.Analyzed.Write(Savedt[1],Length(Savedt)-2);
          SavedT := '';

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
    if Assigned(d) then
      d.Free; 
    if Assigned(pa) then
      pa.Free;
  end;

begin
  if (PartFile='') then PartsEx := true;
  if partsex then exit; // Already done
                                                                                                                         
  try
    s := TFileStream.Create(PartFile,fmOpenRead);

      c := OrgHdp.Mime.ContentType;
      if OrgHdp.Charset <>'' then c.Charset := ZCCharsetToMIME(OrgHdp.Charset);
      ReadParts;
      c := nil;
      s.Free;

  except
  end;
  partsex := true;
end;

procedure TSendUUData.MIMEWriteContent(s1:TStream;pa:TSendAttach_Part;DoEncode:Boolean;const Signature,FidoOrigin:string);
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

  if (pa.FileEOL in [MimeEOLCR,MimeEOLLF]) and (not pa.isFile) then
    ConnectStream(outs,MimeCreateEolConverter(pa.FileEOL));


  // presize the output stream (in case of a memory stream) to improve speed (reduces memory allocations)
  if (outs is TNullCodecStream) and (TNullCodecStream(outs).OtherStream is TMemoryStream) then
    TNullCodecStream(outs).OtherStream.Size := TNullCodecStream(outs).OtherStream.Size + Ins.Size;
  CopyStream(ins,outs);

  ins.Free;

  if (Signature<>'') or (FidoOrigin<>'') then
  begin
    if outs is TMimeEolCodecStream then
      UnconnectStream(outs);

    if outs is TCharsetCodecStream then
      UnconnectStream(outs);

    if DoEncode or not MIMESaveCharsetAsCP437(pa.FileCharset) then
      ConnectStream(outs,TCharsetEncoderStream.Create('IBM437',pa.ContentCharset));

    if Signature<>'' then
      write_s(outs,#13#10+Signature);

    if FidoOrigin<>'' then
      write_s(outs,#13#10+FidoOrigin);
  end;

 finally
  assert(pointer(outs)<>pointer(s1));
  outs.Free; // will not destroy s1!
 end;
end;

procedure TSendUUData.MIMEWriteContentWithHeaders(s:TStream;pa:TSendAttach_Part;const Signature:string);
begin
  with pa do
  begin
    if (not ContentType.NeedCharset) or
       (MimeCharsetCanonicalName(ContentCharset)='US-ASCII') then ContentCharset := '';

    writeln_s(s,'Content-Type: ' + ContentType.AsFoldedString(76-14,76,#13#10,true));
    writeln_s(s,'Content-Transfer-Encoding: ' + MimeEncodingNames[ContentEncoding]);

    if (FileNameO<>'') or (not IsNAN(FileAccess)) or (not IsNan(FileModify)) or
       (not IsNAN(FileCreate)) or (ContentDisposition.DispoType<>MimeDispositionInline) then
      writeln_s(s,'Content-Disposition: ' + ContentDisposition.AsFoldedString(76-21,76,#13#10,true));
    if ContentDescription <> '' then
      writeln_s(s,'Content-Description: ' + ContentDescription);
    writeln_s(s,'');
  end;
  MIMEWriteContent(s,pa,true,Signature,'');
end;

//
// $Log: xpsendmessage_mime.inc,v $
// Revision 1.8  2004/09/26 10:46:16  mk
// - fixed Bug #1034198: corrupted mime attatchments
//
// Revision 1.7  2003/10/21 21:25:04  cl
// - Changed THeader.MIME to use TMimeContentType and TMimeDisposition objects
// - Changed MausTausch headers for Maus-internal IDs: MID/BEZ => maus_*, org_* => MID/BEZ,
//
// Revision 1.6  2003/09/29 19:09:00  mk
// - another fix for #809655: 3.8.12. Multipart l�sst sich nicht edieren
//
// Revision 1.5  2003/09/29 17:35:22  mk
// - fixed typo in last committ
//
// Revision 1.4  2003/09/29 10:46:41  mk
// - fixed crash in ReadParts of TSendUUData.MIMEDecompose;
//   this fixes missing text with N/U/E
// - this is bug  #809655: 3.8.12. Multipart l�sst sich nicht edieren
//
// Revision 1.3  2003/04/28 20:18:58  cl
// - CRLF at the end of a text file is now uniformly handled as the start of
//   an additional line.
//
// Revision 1.2  2003/01/13 22:14:28  cl
// - send window rewrite IIa - cleanups
//
// Revision 1.1  2002/11/14 21:06:13  cl
// - DoSend/send window rewrite -- part I
//
// Revision 1.14  2002/07/26 08:17:48  mk
// - removed $O-
//
// Revision 1.13  2002/07/25 20:43:57  ma
// - updated copyright notices
//
// Revision 1.12  2002/07/23 13:27:44  mk
// - fixed Bug #476655 kein nachtr�gliches Edit bei attachment
//   at the end of every mime part, the last #0d0a is not part
//   of the content and will be stripped off
//
// Revision 1.11  2002/06/12 08:40:55  mk
// - fix for resizeing output stream
//
// Revision 1.10  2002/06/11 07:19:58  mk
// - resize stream only if TMemoryStream
//
// Revision 1.9  2002/06/10 08:07:42  mk
// - improved memory preallocation for outstream (see last fix)
//
// Revision 1.8  2002/06/05 17:49:24  mk
// <CVS: ----------------------------------------------------------------------
//
// Revision 1.7  2002/01/05 16:01:11  mk
// - changed TSendUUData from record to class
//
// Revision 1.6  2001/09/19 18:02:36  cl
// - various small fixes
//
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


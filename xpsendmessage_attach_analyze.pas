{   $Id$

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

{ Nachrichten versenden: Attachments                                   }

{$I xpdefine.inc}

unit xpsendmessage_attach_analyze;

{ ---------------------------} interface { --------------------------- }

uses
  mime, mime_analyze, xpsendmessage_attach, classes, xpnt;

procedure SendAttach_Analyze(pa:TSendAttach_Part;NewFile:Boolean;const Signature:string;netztyp:eNetz;docode:Byte;pgpsig:boolean);
function  GuessContentTypeFromFileName(FileName:String):String;

{ ------------------------} implementation { ------------------------- }

uses
  sysutils, //Systemunits bitte immer zuerst aufz�hlen!
  {$IFDEF unix} xpcurses, {$ENDIF}
  {$IFDEF Win32} windows, {$ENDIF}
  database, inout, keys, lister, typeform, xp0, xpstreams,
  xpglobal; //sollte immer am Ende stehen

function GuessContentTypeFromFileName(FileName:String):String;
var ext: string;
    i:   Integer;
{$IFDEF Win32}
    reg: HKEY;
    value_data: String;
    value_size: DWORD;
    reg_res: Cardinal;
{$ENDIF}
begin
  Result := '';

  { Read MIME type from database                           }

  ext := ExtractFileExt(FileName);
  dbSeek(mimebase, mtiExt, UpperCase(Mid(ext,2)));
  if dbFound then
    Result := DBReadNStr(MimeBase,MimeB_typ);

  { Ignore wildcard entries in database                    }

  for i:=1 to Length(Result) do
    if Result[i] in [';','/'] then
      break
    else if Result[i] in ['*'] then begin
      Result := ''; break; end;

{$IFDEF Win32}
  if (Result='') and (RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar(Ext), 0,
    KEY_QUERY_VALUE, {$IFNDEF VirtualPascal}{$IFNDEF Delphi}@{$ENDIF}{$ENDIF}reg) = ERROR_SUCCESS) then
  begin
    SetLength(value_data,255);
    value_size:=Length(value_data);
    reg_res := RegQueryValueEx(reg,'Content Type',nil,nil,@(value_data[1]),@value_size);
    if reg_res = ERROR_MORE_DATA then
    begin
      SetLength(value_data,value_size);
      reg_res := RegQueryValueEx(reg,'Content Type',nil,nil,@(value_data[1]),@value_size);
    end;
    if reg_res = ERROR_SUCCESS then
    begin
      SetLength(value_data,Longint(value_size)+iif(value_data[value_size]=#0,-1,0));
      Result:=value_data;
    end;
    RegCloseKey(Reg);
  end;
{$ENDIF}
end;

procedure SendAttach_Analyze(pa:TSendAttach_Part;NewFile:Boolean;const Signature:string;netztyp:eNetz;docode:Byte;pgpsig:boolean);
var data: TStream;
    GuessedType: String;
begin
  { first of all, look at the file type }

  GuessedType := GuessContentTypeFromFileName(
    iifs(pa.IsTemp,pa.FileName,pa.FileNameO));

  if not FileExists(pa.FileName) then
    exit;

  pa.Analyzed.Size := 0;

  // --- 1. Header ---------------------------------------------------

  // --- 2. Actual file ----------------------------------------------

  data := TFileStream.Create(pa.FileName,fmOpenRead);
  try
    pa.FileSize := data.Size;

    if pa.FileEncoding in [MimeEncodingQuotedPrintable,MimeEncodingBase64] then
      ConnectStream(data,MimeCreateDecoder(pa.FileEncoding));

    CopyStream(data,pa.Analyzed);   // looks harmless, doesn't it?
  finally
    data.Free;
  end;

  // --- 3. Signature ------------------------------------------------

  if Signature<>'' then
    writeln_s(pa.Analyzed,Signature);

  if pa.IsFile then
  { change all parameters to the values determined through our         }
  { analyzation - this is only called once when the file is attached   }
  begin
    pa.ContentType.AsString := iifs(Length(GuessedType)>0,GuessedType,
      iifs(pa.Analyzed.IsBinary,'application/octet-stream','text/plain'));

    if pa.FileEncoding in [MimeEncodingQuotedPrintable,MimeEncodingBase64] then
      pa.ContentEncoding := pa.FileEncoding // don't change it
    else
      pa.ContentEncoding := pa.Analyzed.PreferredEncoding;

    pa.ContentCharset    := pa.Analyzed.PreferredCharset;

    pa.FileCharset       := pa.Analyzed.GuessedCharset;
    pa.FileEOL           := pa.Analyzed.GuessedEOL;
  end else
  { for text parts, change everything to the best settings except for  }
  { settings the user might have changed                               }
  begin

    if NewFile or (not pa.Analyzed.EncodingAllowed[pa.ContentEncoding]) then
    begin
      if pa.Analyzed.Is8Bit then
      begin
        if (not MimeQP) and pa.Analyzed.EncodingAllowed[MimeEncoding8Bit] then
          pa.ContentEncoding := MimeEncoding8Bit
        else
          pa.ContentEncoding := MimeEncodingQuotedPrintable;
      end else
      begin
        if pa.Analyzed.EncodingAllowed[MimeEncoding7Bit] then
          pa.ContentEncoding := MimeEncoding7Bit
        else
          pa.ContentEncoding := MimeEncodingQuotedPrintable;
      end;
    end;
  
    pa.FileCharset := MimeCharsetCanonicalName(pa.FileCharset);

    if(pa.FileCharset='IBM437') or 
      (pa.FileCharset='') then 
    begin
(*    
      if ntIBM(netztyp) then
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else if ZC_MIME and ntOptISO(netztyp) and pa.Analyzed.IsLatin1DOS then
          pa.ContentCharset := 'ISO-8859-1'
        else
          pa.ContentCharset := 'IBM437';
      end else
*)      
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else if pa.Analyzed.IsLatin1DOS then
          pa.ContentCharset := 'ISO-8859-1'
        else
          pa.ContentCharset := 'UTF-8';
      end;
    end else
    if pa.FileCharset='ISO-8859-1' then 
    begin
(*    
      if ntIBM(netztyp) then
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else if ZC_MIME and ntOptISO(netztyp) then
          pa.ContentCharset := 'ISO-8859-1'
        else
          pa.ContentCharset := 'IBM437';
      end else
*)      
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else
          pa.ContentCharset := 'ISO-8859-1'
      end;
    end else
    if pa.FileCharset='UTF-8' then 
    begin
(*    
      if ntIBM(netztyp) then
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else
          pa.ContentCharset := 'IBM437';
      end else
*)      
      begin
        if not pa.Analyzed.Is8Bit then
          pa.ContentCharset := 'US-ASCII'
        else
          pa.ContentCharset := 'UTF-8'
      end;
    end else
    begin
(*    
      if ntIBM(netztyp) and 
        not (ZC_MIME and ntOptISO(netztyp)) then
          pa.ContentCharset := 'IBM437'
        else
*)        
          pa.ContentCharset := pa.FileCharset
    end;

    pa.FileEOL     := MimeEolCRLF;
    pa.FileEncoding:= MimeEncodingBinary;
  end;

  if docode=8 then
  begin
    if pa.ContentEncoding=MimeEncoding8Bit then
      pa.ContentEncoding:=MimeEncoding7Bit else
    if pa.ContentEncoding=MimeEncodingBinary then
      pa.ContentEncoding:=MimeEncodingBase64;
  end;
end;

end.

{   $Id$

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

{ Nachrichten versenden: Attachments                                   }

{$I xpdefine.inc}

unit xpsendmessage_attach_analyze;

{ ---------------------------} interface { --------------------------- }

uses
  mime, mime_analyze, xpsendmessage_attach;

procedure SendAttach_Analyze(pa:TSendAttach_Part;NewFile:Boolean;SigFile:String;docode:Byte;pgpsig:boolean);
function  GuessContentTypeFromFileName(FileName:String):String;

{ ------------------------} implementation { ------------------------- }

uses
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  classes, database, inout, keys, lister, typeform, xp0, xpglobal, xpnt,
  sysutils, windows, xpstreams;

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
  end;
{$ENDIF}
end;

procedure SendAttach_Analyze(pa:TSendAttach_Part;NewFile:Boolean;SigFile:String;docode:Byte;pgpsig:boolean);
var f,f1: TStream;
    GuessedType: String;
begin
  { first of all, look at the file type }

  GuessedType := GuessContentTypeFromFileName(
    iifs(pa.IsTemp,pa.FileName,pa.FileNameO));

  if not FileExists(pa.FileName) then
    exit;

  f:=nil;
  f1:=nil;

  try
    pa.Analyzed.Size := 0;

    // --- 1. Header ---------------------------------------------------

    // --- 2. Actual file ----------------------------------------------

    f := TFileStream.Create(pa.FileName,fmOpenRead);
    pa.FileSize := f.Size;

    if pa.FileEncoding in [MimeEncodingQuotedPrintable,MimeEncodingBase64] then
      f1 := MimeCreateDecoder(pa.FileEncoding,f)
    else begin
      f1 := f;
      f := nil;
    end;

    CopyStream(f1,pa.Analyzed);   // looks harmless, doesn't it?

    // --- 3. Signature ------------------------------------------------

    if (SigFile<>'') and FileExists(SigFile) then
    begin
      f1.Free;
      f1 := TFileStream.Create(SigFile,fmOpenRead);
      CopyStream(f1,pa.Analyzed); // drop signature right behind
    end;

  finally
    f.Free;
    f1.Free;
  end;


  if pa.IsFile then
  { change all parameters to the values determined through our         }
  { analyzation - this is only called once when the file is attached   }
  begin
    pa.ContentType.AsString := iifs(Length(GuessedType)>0,GuessedType,'application/octet-stream');

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

    if not pa.Analyzed.Is8Bit then
      pa.ContentCharset := 'US-ASCII'
    else if pa.Analyzed.IsLatin1DOS then
      pa.ContentCharset := 'ISO-8859-1'
    else
      pa.ContentCharset := 'UTF-8';

    pa.FileCharset := iifs(pa.Analyzed.Is8Bit,'IBM437','US-ASCII');
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

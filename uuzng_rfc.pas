{ $Id: uuzng_rfc.pas,v 1.1 2003/10/21 21:22:42 cl Exp $

  uuzng_rfc -- UUZ message converter - common functions
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

unit uuzng_rfc;

interface

uses uuzng,
  classes,
  xpmessage;

type
  TRFCSpoolIn = class(TNetcallSpoolIn)
  public
    constructor Create(NewFileList: TStringList); overload;
    constructor Create(const FileNamePattern: string); overload;
    destructor Destroy; override;

  protected
    procedure CopyFileTo(const src: string; dest: TNetcallSpoolOut);

  public
    procedure CopyTo(dest: TNetcallSpoolOut); override;

  private
    FFileList: TStringList; FFileListOwned: boolean;
    FForceEnvelopeTo: string;

    procedure SetFileList(NewFileList: TStringList);
    function GetFileList: TStringList;

  protected
    function GetCommonTCPIPFileFrom(var Src: TStream; mail: boolean): TXPMessage;
    function GetPOP3FileFrom(var Src: TStream; const UIDL: string): TXPMessage;

  public
    property FileList: TStringList read GetFileList write SetFileList;
    property ForceEnvelopeTo: string read FForceEnvelopeTo write FForceEnvelopeTo;
  end;

  TRFCSpoolOut = class(TNetcallSpoolOut)
  protected
    FDestDir: String;
  public
    constructor Create(const DestDir: string);
  protected
    procedure PutMail(mail: TXPMessage); virtual; abstract;
    procedure PutNews(news: TXPMessage); virtual; abstract;
  public
    procedure Put(msg: TXPMessage); override;
  end;

implementation

uses
  uuzng_rfc_util,
  fileio,
  xpstreams,
  xpnt,
{$IFDEF Delphi}
  strutils,
{$ENDIF}
  progressoutput,
  typeform,
  sysutils;

constructor TRFCSpoolIn.Create(NewFileList: TStringList);
begin
  inherited Create;
  FileList := NewFileList;
end;

constructor TRFCSpoolIn.Create(const FileNamePattern: string);
var ff: TSearchRec;
    pt: string;
begin
  if FindFirst(FileNamePattern, faAnyFile, ff) = 0 then
  try
    pt := IncludeTrailingPathDelimiter(ExtractFilePath(FileNamePattern));
    repeat
      if (ff.Attr and faDirectory) = 0 then
        FileList.Add(pt + ff.Name);
    until FindNext(ff) <> 0;
  finally
    FindClose(ff);
  end;
end;

destructor TRFCSpoolIn.Destroy;
begin
  FileList := nil;
end;

procedure TRFCSpoolIn.SetFileList(NewFileList: TStringList);
begin
  if (Self.FFileListOwned) then FreeAndNil(Self.FFileList);
  Self.FFileList := NewFileList;
  Self.FFileListOwned := false;
end;

function TRFCSpoolIn.GetFileList: TStringList;
begin
  if not assigned(FFileList) then begin
    FFileList := TStringList.Create;
    FFileListOwned := true;
  end;
  result := FFileList;
end;

function TRFCSpoolIn.GetCommonTCPIPFileFrom(var Src: TStream; mail: boolean): TXPMessage;
begin
  result := TXPMessage.Create;
  ConnectStream(src, TDotUnEscapeStream.Create);
  Result.Head.ReadRFC(src, true);
  Result.Body := Src;
  src := nil;
end;

function TRFCSpoolIn.GetPOP3FileFrom(var Src: TStream; const UIDL: string): TXPMessage;
begin
  result := GetCommonTCPIPFileFrom(src, true);
  if Assigned(result) then begin
    result.Head.POP3ID := UIDL;
    if FForceEnvelopeTo <> '' then begin
      result.Head.Kopien.AddStrings(result.Head.Empfaenger);
      result.Head.Empfaenger.Clear;
      result.Head.Empfaenger.Add(FForceEnvelopeTo);
    end;
    result.Head.netztyp := nt_POP3;  
  end;
end;

procedure TRFCSpoolIn.CopyFileTo(const src: string; dest: TNetcallSpoolOut);
var s: TStream;
    h, hu: string;

  procedure single(src: TXPMessage);
  begin
    if assigned(src) then
      dest.Put(src);
    FreeAndNil(src);
  end;

begin
  s := TFileStream.Create(src, fmOpenRead);
  try
    h := readln_s(s);
    hu := UpperCase(h);

    if LeftStr(hu,16) = 'X-XP-POP3-UIDL: ' then begin
      Out(mcVerbose,'Mail: %s (POP3)',[ExtractFileName(src)]);
      Single(GetPOP3FileFrom(s, Mid(h, 17)))
    end else
      raise Exception.Create('Unknown file format');

  finally
    FreeAndNil(s);
  end;
end;

procedure TRFCSpoolIn.CopyTo(dest: TNetcallSpoolOut);
var i: integer;
begin
  for i := 0 to FileList.Count-1 do
    CopyFileTo(FileList[i], dest);
end;

constructor TRFCSpoolOut.Create(const DestDir: string);
begin
  Self.FDestDir := AddDirSepa(DestDir);
  CreateMultipleDirectories(Self.FDestDir);
end;

procedure TRFCSpoolOut.Put(msg: TXPMessage);
var has_news, has_mail: boolean;
  i: integer;
begin
  for i := 0 to msg.head.Empfaenger.Count - 1 do
    if CPos('@', msg.head.Empfaenger[i]) > 0 then begin
      has_mail := true;
      if has_news then break;
    end else
    begin
      has_news := true;
      if has_mail then break;
    end;

  // TODO: charset conversion

  // TODO: content transfer encoding

  if has_news then Self.PutNews(msg);
  if has_mail then Self.PutMail(msg);
end;

//
// $Log: uuzng_rfc.pas,v $
end.


{ $Id$

    OpenXP Library: TMimeViewer class
    Copyright (C) 2000, Markus Kaemmerer <mk@happyarts.de>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

{$I XPDEFINE.INC}

unit viewer;

interface

uses
  xpglobal, sysutils;



type
  // Viewer for one specific message type
  TMessageViewer = class(TObject)
  private
    fProg: String;
  public
    constructor Init;
    procedure GetFromMimeType(const MimeType: String);
    procedure GetFromExtension(const Extension: String);
    procedure UseInternal;
    function IsInternal: Boolean;
    property Prog: String read fProg write fProg;
  end;

implementation

uses
  database, xp0, typeform;


constructor TMessageViewer.Init;
begin
  inherited Create;
  UseInternal;
end;

procedure TMessageViewer.GetFromMimeType(const MimeType: String);

  function SeekMimeType(MimeType: string): boolean;
  begin
    dbSeek(mimebase, mtiTyp, MimeType);
    SeekMimeType :=not dbBOF(mimebase) and not dbEOF(mimebase) and
              (UpperCase(MimeType) = UpperCase(dbReadStr(mimebase,'typ')));
  end;

begin
  if MimeType = '' then exit;

  // is there a match for MimeTyp?
  if SeekMimeType(MimeType) then
    fProg := dbReadNStr(mimebase,mimeb_programm)
  else  //
    if SeekMimeType(LeftStr(MimeType, cposx('/',MimeType))+'*') then
      fProg := dbReadNStr(mimebase,mimeb_programm);
end;

procedure TMessageViewer.GetFromExtension(const Extension: String);
begin
  if Extension = '' then exit;
  dbSeek(mimebase,mtiExt,UpperCase(mid(Extension,2)));
  if dbFound then
    fProg := dbReadNStr(mimebase,mimeb_programm);
end;

procedure TMessageViewer.UseInternal;
begin
  fProg := '';
end;

function TMessageViewer.IsInternal: Boolean;
begin
  Result := fProg = '';
end;

end.
{
  $Log$
  Revision 1.1  2000/11/18 21:42:17  mk
  - implemented new Viewer handling class TMessageViewer

}

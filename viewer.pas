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

{$I xpdefine.inc}

unit viewer;

interface

uses
  xpglobal, sysutils;



type
  // Viewer for one specific message type
  TMessageViewer = class
  private
    fProg: String;              // selected viewer
    fExt: String;               // file extension for selected viewer
  public
    constructor Init;
    procedure GetFromMimeType(const MimeType: String);
    procedure GetFromExtension(const Extension: String);
    procedure UseInternal;
    function IsInternal: Boolean;
    procedure ViewFile(const Filename: String; Fileattach:boolean);
    property Prog: String read fProg write fProg;
    property Ext: String read fExt write fExt;
  end;

implementation

uses
  database, xp0, xp1, xp1o, fileio, typeform;


constructor TMessageViewer.Init;
begin
  inherited Create;
  UseInternal;
  fExt := '';
end;

procedure TMessageViewer.GetFromMimeType(const MimeType: String);

  function SeekMimeType(const MimeType: string): boolean;
  begin
    dbSeek(mimebase, mtiTyp, UpperCase(MimeType));
    SeekMimeType :=not dbBOF(mimebase) and not dbEOF(mimebase) and
              (UpperCase(MimeType) = UpperCase(dbReadStr(mimebase,'typ')));
  end;

begin
  if MimeType = '' then exit;

  // is there a match for MimeTyp?
  if SeekMimeType(MimeType) then
  begin
    fProg := dbReadNStr(mimebase,mimeb_programm);
    fExt := '.' + dbReadNStr(mimebase,mimeb_extension);
  end else  
    if SeekMimeType(LeftStr(MimeType, cposx('/',MimeType))+'*') then
    begin
      fProg := dbReadNStr(mimebase,mimeb_programm);
      fExt := '.' + dbReadNStr(mimebase,mimeb_extension);
    end;
end;

procedure TMessageViewer.GetFromExtension(const Extension: String);
begin
  if Extension = '' then exit;
  dbSeek(mimebase,mtiExt,UpperCase(mid(Extension,2)));
  if dbFound then
  begin
    fProg := dbReadNStr(mimebase,mimeb_programm);
    fExt := Extension;
  end;
end;

procedure TMessageViewer.UseInternal;
begin
  fProg := '';
end;

function TMessageViewer.IsInternal: Boolean;
begin
  Result := fProg = '';
end;

procedure TMessageViewer.ViewFile(const Filename: String; Fileattach: Boolean);
var p         : Integer;
    prog,
    orgfn,
    fn1,
    parfn,
    Dir,
    Name,
    Ext       : string;
begin
  fn1:='';
  orgfn:= Filename;

//  orgfn:=iifs(viewer.fn<>'',GetFileDir(fn)+GetFileName(viewer.fn),'');
  if (not ValidFileName(orgfn) or FileExists(orgfn)) and (fExt<>'') and
     (cpos('.',filename)>0) then
    orgfn:= ChangeFileExt(filename, fExt);


  if not fileattach then
  begin
  if stricmp(filename,orgfn) or not ValidFileName(orgfn) or (cpos(' ',orgfn)>0)
    then orgfn:=TempS(_filesize(filename)+5000);
    if copyfile(Filename,orgfn) then fn1:=orgfn;
    end;

  prog:=fProg;
  orgfn:=iifs(fn1<>'',fn1,Filename);

  // Tempdatei bei aktivem DELVTMP nach TMP-????.??? umbenennen
  if not fileattach and delviewtmp then
  Begin
    parfn:=TempS(_filesize(Filename)+5000);
    parfn:=LeftStr(parfn,length(parfn)-8)+'TMP-'+RightStr(parfn,8);
    end
  else parfn:=orgfn;

  // Korrekte File-extension verwenden
  ParFN := ChangeFileExt(ParFN, ExtractFileExt(Orgfn));
  RenameFile(orgfn,parfn);

  p:=pos('$FILE',UpperCase(prog));
  if p=0 then prog:=prog+' '+parfn
  else prog:=LeftStr(prog,p-1)+parfn+mid(prog,p+5);
(*  urep(prog,'$TYPE',viewer.typ);
  urep(prog,'$EXT',viewer.ext); *)
  if not XPWinShell(prog,parfn,600,1,fileattach) then
  if not fileattach and (fn1<>'') then DeleteFile(parfn);
end;

{
  $Log$
  Revision 1.4  2001/10/11 09:00:40  mk
  - external viewer files now with correct file extension

  Revision 1.3  2001/10/10 22:04:09  mk
  - enabled use of external mime viewers again

  Revision 1.2  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.1  2000/11/18 21:42:17  mk
  - implemented new Viewer handling class TMessageViewer

}
end.


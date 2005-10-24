{ $Id: xpstreams_temporary.pas 6162 2003-08-26 22:46:31Z cl $

  XPLib TStream Utilities - Temporary File Streams

  Copyright (C) 2003 OpenXP Team <www.openxp.de>
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

{$I xpdefine.inc }

{** Temporary File Streams
  @cvs($Date: 2003-08-27 00:46:31 +0200 (Mi, 27 Aug 2003) $) }
unit xpstreams_temporary;

{ ---------------------------} interface { --------------------------- }

uses
  Classes;

{** @name provides a temporary storage based on files. It is similar to
  @link(TFileStream) but it deletes the file when the object is destroyed. }
type
  TTemporaryFileStream = class(TFileStream)
  private
    FFileName: String;
  public

    {** Creates an instance of @classname - a file will be created with an unique
      filename. The file will be deleted on the destruction of the object. }
    constructor Create; overload;

    {** Creates an instance of @classname - the parameters are the same as fot
      @link(TStream.Create) but the will  will be deleted on the destruction of the object. }
    constructor Create(const FileName: string; Mode: System.Word); overload;

    {** Destroys an instance of @classname }
    destructor Destroy; override;
  end;

{ ------------------------} implementation { ------------------------- }

uses
  xp1,          // TempS
  fileio,       // MakeFile
  sysutils;

constructor TTemporaryFileStream.Create;
begin
  FFileName := TempS(10000);
  MakeFile(FFileName);
  inherited Create(FFileName,fmOpenReadWrite);
end;

constructor TTemporaryFileStream.Create(const FileName: string; Mode: System.Word);
begin
  FFileName := FileName;
  inherited Create(FileName,Mode);
end;

// constructor TTemporaryFileStream.Create(const FileName: string; Mode: System.Word; Rights: Cardinal);
// begin
//   FFileName := FileName;
//   inherited Create(FileName,Mode,Rights);
// end;

destructor TTemporaryFileStream.Destroy;
begin
  DeleteFile(FFileName);
  inherited;
end;

//
// $Log: xpstreams_temporary.pas,v $
// Revision 1.1  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
end.

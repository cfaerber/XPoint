{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the Lesser GNU General Public License (LGPL) as
   published by the Free Software Foundation; either version 2,
   or (at your option) any later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LGPL
   for more details.

   You should have received a copy of the LGPL along with this
   software; see the file lgpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on November, 27th 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
   Copyright (c) 2000 by the OpenXP Team.

   'MatchMask' based upon 'FNMatch' witch is part of the Free Pascal
   run time library. Copyright (c) 1999-2000 by Michael Van Canneyt,
   members of the Free Pascal development team.

}
{$i xpdefine.inc}

{ Contains class TDirectory }

unit Direct;

{
  TDirectory can be used to explore a directory. The class
  uses the functions findfirst/findnext out of the unit sysutils.
  The main different to the known functions is, that an systems with
  case sensetive filenames you can ignore the case. This is (only)
  usefull for looking for special data files eg. *.doc, *.html...
  Use the class as shown:

  uses Direct, SysUtils;

  procedure ShowDir(mask: string);
  var dir: TDirectory;
      i  : integer;
  begin
    dir:= TDirectory.Create(mask,faAnyFile,true);
    for i:= 0 to dir.Count-1 do
      writeln(dir.Name[i]);
    writeln(dir.Count,' file(s)');
    dir.Free;
  end;

  begin
    ShowDir('*.pkt');
  end.
}

interface

uses
  Classes, SysUtils;

type
  { Generall faults }
  EDirectoryError       = class(Exception);
  { Index functions out of bound }
  EDirRange             = class(EDirectoryError);

type
  { This class is for internal use }
  TDirEntry = class
    protected

      VEntry            : TSearchRec;

      function  FName: string; virtual;

    public

      constructor Create(const item: TSearchRec);

      property Name: string read FName;

  end;

type
  TDirectory = class
    protected
      VEntries          : TList;        { The entries }
      VAttr             : integer;      { the attribute }
      VIgnC             : boolean;      { true if ignore case }
      VDir              : string;       { the directory }
      VMask             : string;       { the searchmask }

      { sets the attribute }
      procedure PAttr(const attr: integer); virtual;
      { sets the mask, should be used after PAttr! }
      procedure PMask(const path: string); virtual;
      { Returns the filename at position 'i' }
      function  FName(i: integer): string; virtual;
      { same, but as full name }
      function  FLongName(i: integer): string; virtual;
      { test the wildcards }
      function  MatchMask(fn: string): boolean; virtual;

    public

      { constructor / destructor }
      constructor Create(const mask: string;
                         const attr: integer;
                         const ignc: boolean);
      destructor Destroy; override;

      { Dir + Name at index i [0 <= i < Count] }
      property LongName[i: integer]: string read FLongName;

      { Name at index i }
      property Name[i: integer]: string read FName;

      { Deletes all entries }
      procedure Clear; virtual;

      { How many entries are available }
      function  Count: integer; virtual;

      { Do we have no entry? }
      function  isEmpty: boolean; virtual;

      { Read the directory, returns the count of files }
      function  Read: integer; virtual;

  end;

implementation

{ --- TDirEntry --------------------------------------------- }

constructor TDirEntry.Create(const item: TSearchRec);
begin
  inherited Create;
  VEntry:= item;
end;

function TDirEntry.FName: string;
begin
  result:= VEntry.name;
end;

{ --- TDirectory -------------------------------------------- }

constructor TDirectory.Create(const mask: string;
                              const attr: integer;
                              const ignc: boolean);
begin
  VEntries:= TList.Create;
  PAttr(attr);
  VIgnC:= ignc;
  PMask(mask);
  Read;
end;

destructor TDirectory.Destroy;
begin
  Clear;
  VEntries.Free;
  inherited Destroy;
end;

procedure TDirectory.PAttr(const attr: integer);
begin
  VAttr:= attr and faAnyFile;
end;

procedure TDirectory.PMask(const path: string);
begin
  VDir:= ExtractFilePath(path);
  { Complete the path if not given }
  if VDir = '' then
    VDir:= ExtractFilePath(ExpandFileName('.'));
  VMask:= ExtractFileName(path);
  if VMask='' then
    VMask:= '*.*';
{$ifdef Unix}
  { Adjust a pure wildcard for Unix (hidden files starts with dot) }
  if (VMask='*') and ((VAttr or faHidden)<>0) then
    VMask:= '*.*'
  else if (VMask='*.*') and ((VAttr or faHidden)=0) then
    VMask:= '*';
{$endif}
  if VIgnC then
    VMask:= UpperCase(VMask);
end;

procedure TDirectory.Clear;
var
  i : integer;
  de: TDirEntry;
begin
  for i:= 0 to VEntries.Count-1 do begin
    de:= VEntries[i];
    de.Free;
  end;
  VEntries.Clear;
end;

function TDirectory.Count: integer;
begin
  result:= VEntries.Count;
end;

function TDirectory.isEmpty: boolean;
begin
  result:= Count=0;
end;

function TDirectory.FName(i: integer): string;
var
  de: TDirEntry;
begin
  if (i<0) or (i>=Count) then
    raise EDirRange.Create(Format('%d not in 0..%d!',[i, Count]));
  de:= VEntries[i];
  result:= de.Name;
end;

function TDirectory.FLongName(i: integer): string;
begin
  result:= VDir + FName(i);
end;

function TDirectory.MatchMask(fn: string): boolean;
var
  LenPat, LenName : longint;

  function DoFNMatch(i,j:longint):Boolean;
  var
    found: boolean;
  begin
    Found:=true;
    while Found and (i<=LenPat) do begin
      case VMask[i] of
        '?' : Found:=(j<=LenName);
        '*' : begin
                { find the next character in pattern, different of ? and * }
                while Found and (i<LenPat) do begin
                  inc(i);
                  case VMask[i] of
                    '*' : ;
                    '?' : begin
                            inc(j);
                            Found:=(j<=LenName);
                          end;
                  else
                          Found:=false;
                  end; { case (inner) }
                end; { while }
                { Now, find in name the character which i points to,
                  if the * or ? wasn't the last character in the pattern,
                  else, use up all the chars in name }
                Found:=true;
                if (i<=LenPat) then repeat
                  { find a letter (not only first !) which maches pattern[i] }
                  while (j<=LenName) and (fn[j]<>VMask[i]) do
                    inc (j);
                  if (j<LenName) then begin
                    if DoFnMatch(i+1,j+1) then begin
                      i:=LenPat;
                      j:=LenName; { we can stop }
                      Found:=true;
                    end else
                      inc(j); { We didn't find one, need to look further }
                  end; { if }
                until (j>=LenName) else
                  j:=LenName;{we can stop}
              end; { '*' }
      else { not a wildcard character in pattern }
        Found:=(j<=LenName) and (VMask[i]=fn[j]);
      end; { case (outer) }
      inc(i);
      inc(j);
    end; { while }
    DoFnMatch:=Found and (j>LenName);
  end; { DoFNMatch }

begin {start MatcMAsk}
  if VIgnC then
    fn:= UpperCase(fn);
  LenPat:=Length(VMask);
  LenName:=Length(fn);
  result:=DoFNMatch(1,1);
end;

function TDirectory.Read: integer;
const
{$ifdef Unix}
  cSM = '*';
{$else}
  cSM = '*.*';
{$endif}
var
  sr  : TSearchRec;
  fn  : TDirEntry;
begin
  Clear;
  if FindFirst(VDir+cSM,VAttr,sr)=0 then repeat
    if MatchMask(sr.name) then begin
      fn:= TDirEntry.Create(sr);
      VEntries.Add(fn);
    end;
  until (FindNext(sr)<>0);
  FindClose(sr);
  result:= VEntries.Count;
end;

end.
{
        $Log$
        Revision 1.3  2000/12/07 18:40:07  hd
        - new function: isEmpty
        - fix: logical error in mask searching
        - fix: ignore case works proper now

        Revision 1.2  2000/11/30 19:38:39  hd
        - Fix: mask incomplete

        Revision 1.1  2000/11/29 15:15:31  hd
        - Class: TDirectory
          - Read all filenames into a list and give access by an integer
          - Can ignore case of the filenames on Unix based systems to
            search easy for files eg. *.pkt
          - Still not finished but works

}

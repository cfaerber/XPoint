unit dos;

interface

uses
  SysUtils;

const
  ReadOnly   = $01;
  Hidden     = $02;
  SysFile    = $04;
  VolumeID   = $08;
  Directory  = $10;
  Archive    = $20;
  AnyFile    = $3F;

const
  DosError: Integer = 0;

type
  FileRec = TFileRec;
  SearchRec = TSearchRec;

implementation

begin

end.


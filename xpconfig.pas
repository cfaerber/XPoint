{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
  
   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on December, 28th 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ OpenXP - Config-Module }

{$I xpdefine.inc}

unit xpconfig;

interface

uses
  Classes,
  SysUtils;

type
  { This is used to identify the correct class }
  TXPConfigType = (
        ccNumber,
        ccString,
        ccPath,
        ccFilename);
  EConfig               = class(Exception);
  EConfigInvalidKey     = class(EConfig);       { no/invalid keyword }
  EConfigRange          = class(EConfig);       { Range error }

type
  TXPConfigItem = class
    protected

      VKey              : string;       { the key in the config-file }

    public
      constructor Create(const s: string);

      function ConfigType: TXPConfigType; virtual; abstract;
  end;

  TXPConfigNumber = class(TXPConfigItem)
    protected

      VValue            : integer;      { the numeric value }
      VBounds           : boolean;      { using bounded value? }
      VLower, VUpper    : integer;      { the bounds, if any }

      { test if the value v is in bounds if bounds wanted }
      procedure TestValue(const v: integer);
      
    public
      constructor Create(const s: string);
      constructor CreateDefaultValue(const s: string; const v: integer);
      constructor CreateWithBounds(const s: string; const upper, lower: integer);
      constructor CreateWithBoundsDefaultValue(const s: string; const upper, lower,v: integer);
      
      function ConfigType: TXPConfigType; override;
  end;

  TXPConfigString = class(TXPConfigItem)
    protected
      VValue            : string;       { the string value }
    public
      function ConfigType: TXPConfigType; override;
  end;

  TXPConfigPath = class(TXPConfigString)
    protected
    public
      function ConfigType: TXPConfigType; override;
  end;
  
  TXPConfigFilename = class(TXPConfigString)
    protected
    public
      function ConfigType: TXPConfigType; override;
  end;

type
  TXPConfig = class
    protected
    
      VItems            : TList;        { all config elements }

    public

      constructor Create;
      destructor Destroy; override;
      
  end;

var
  Config                : TXPConfig;    { the config container }

implementation

{ ----- TXPConfigItem ----------------------------------------------- }

constructor TXPConfigItem.Create(const s: string);
begin
  inherited Create;
  if s='' then
    raise EConfigInvalidKey.Create('Keyword missing');
  VKey:= s;
end;

{ ----- TXPConfigNumber --------------------------------------------- }

constructor TXPConfigNumber.Create(const s: string);
begin
  inherited Create(s);
  VValue:= 0;
  VBounds:= false;
end;
      
constructor TXPConfigNumber.CreateDefaultValue(const s: string; const v: integer);
begin
  inherited Create(s);
  VValue:= 0;
  VBounds:= false;
end;

constructor TXPConfigNumber.CreateWithBounds(const s: string; const upper, lower: integer);
begin
  inherited Create(s);
  VValue:= 0;
  VBounds:= true;
  VLower:= lower;
  VUpper:= upper;
  TestValue(0);
end;

constructor TXPConfigNumber.CreateWithBoundsDefaultValue(const s: string; const upper, lower,v: integer);
begin
  inherited Create(s);
  VValue:= v;
  VBounds:= true;
  VLower:= lower;
  VUpper:= upper;
end;

procedure TXPConfigNumber.TestValue(const v: integer);
begin
  if VBounds then
    if (v<VLower) or (v>VUpper) then
      raise EConfigRange.Create(Format('%d is not in %d..%d', [v, VLower, VUpper]));
end;

function TXPConfigNumber.ConfigType: TXPConfigType;
begin
  result:= ccNumber;
end;

{ ----- TXPConfigString --------------------------------------------- }

function TXPConfigString.ConfigType: TXPConfigType;
begin
  result:= ccString;
end;

{ ----- TXPConfigPath ----------------------------------------------- }

function TXPConfigPath.ConfigType: TXPConfigType;
begin
  result:= ccPath;
end;

{ ----- TXPConfigFilename ------------------------------------------- }

function TXPConfigFilename.ConfigType: TXPConfigType;
begin
  result:= ccFilename;
end;

{ ----- TXPConfig --------------------------------------------------- }

constructor TXPConfig.Create;
begin
  inherited Create;
  VItems:= TList.Create;
end;

destructor TXPConfig.Destroy;
begin

  VItems.Free;
  inherited Destroy;
end;

end.
{
$Log$
Revision 1.3  2001/10/15 09:04:22  ml
- compilable with Kylix ;-)

Revision 1.2  2001/09/10 15:58:03  ml
- Kylix-compatibility (xpdefines written small)
- removed div. hints and warnings

Revision 1.1  2000/12/28 16:00:44  hd
- Init
  - will replace the used configuration. please be carefull by changing
    the actuall configuration methods.

}

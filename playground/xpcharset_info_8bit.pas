{
    $Id: xpcharset_codec.pas,v 1.1 2003/09/29 20:47:18 cl Exp $

    Free Pascal Unicode support
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit xpcharset_info_8bit;

{$I xpdefine.inc }

interface

uses
  xpglobal, xpunicode, xpcharset;

  TCharsetInfo = class
  public
    class function Create(const charset: string): TCharsetInfo; overload;
    class function Create(charset: TMIMECharsets): TCharsetInfo; overload;

  public
    function IsUnicodeEncoding: boolean; virtual; abstract;
    function HasCharacter(uc: TUnicodeChar); virtual; abstract;
  end;

end;

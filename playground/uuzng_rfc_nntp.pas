{ $Id: uuzng_rfc_nntp.pas,v 1.1 2003/10/21 21:22:42 cl Exp $

  UUZNG_UUCP - OpenXP Message Converter -- UUCP module

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

unit uuzng_rfc_uucp;

interface

uses uuzng,
  xpmessage,
  xpserver;

type
  TUUCPCompression = (
    compress_none,        { uncompressed }
    compress_gzip,        { gzip'ed      }
    compress_bzip2,       { bzip2'ed     }
    compress_compress,    { compressed   }
    compress_freeze );    { frozen       }

  TUUCPSpool = class(TNetcallSpoolDir)
  public
    procedure Put(msg:TXPMessage); override;
    procedure PutDone; override;
    function Get: TXPMessage; override;

  public
    constructor Create;
    constructor CreateFromServer(box: TXPServer);

  private
    FLocalUUCPName: string;
    FRemoteUUCPName: string;
    
    FOutMailCompression: TUUCPCompression;
    FOutNewsCompression: TUUCPCompression;
    FOutUseBSMTP: boolean;
    FOutCreateCommandFile: boolean;
    FOutUseECmd: boolean;

    FUUNumber: System.Word;

  public
    property LocalUUCPName: string read FLocalUUCPName write FLocalUUCPName;
    property RemoteUUCPName: string read FLocalUUCPName write FLocalUUCPName;
    
    property OutMailCompression: TUUCPCompression read FOutMailCompression write FOutMailCompression;
    property OutNewsCompression: TUUCPCompression read FOutNewsCompression write FOutNewsCompression;
    property OutUseBSMTP: boolean read FOutUseBSMTP write FOutUseBSMTP;
    property OutUseECmd: boolean read FOutUseECmd write FOutUseECmd;

    property UUNumber: system.word read FUUNumber write FUUNumber;
    function NextUUNumber: system.word;
  end;

implementation

function TUUCPSpool.NextUUNumber: system.word;
begin
  Inc(FUUNumber);
  result := FUUNumber;
end;

procedure Put(msg:TXPMessage); override;
begin

end;

procedure PutDone; override;
begin

end;

function Get: TXPMessage; override;
begin
  result := nil;
end;

//
// $Log: uuzng_rfc_nntp.pas,v $
// Revision 1.1  2003/10/21 21:22:42  cl
// - UUZNG update
//
// Revision 1.1  2003/09/29 20:47:18  cl
// - moved charset handling/conversion code to xplib
//
// Revision 1.2  2003/08/28 18:53:18  cl
// - draft update
//
// Revision 1.1  2003/08/26 22:34:32  cl
// - skeleton for UUZ Next Generation
//
end.

{ $Id: uuzng_rfc.pas,v 1.1 2003/10/21 21:22:42 cl Exp $

  uuzng_rfc_util -- UUZ message converter - common functions
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

{$INCLUDE xpdefine.inc}

unit uuzng_rfc_smtp;

interface

uses uuzng, classes, xpmessage, uuzng_rfc;

type
  TRFCSMTPSpoolOut = class(TRFCSpoolOut)
  private
    FCount: integer;
  public
    procedure PutMail(msg: TXPMessage); override;
  end;

implementation

uses
  uuzng_rfc_util,
  xpstreams,
  typeform,
  sysutils;

procedure TRFCSMTPSpoolOut.PutMail(msg: TXPMessage);
var s: string;
    f: TStream;
    i: integer;

begin
  s := Format('SMTP%4x.OUT',[FCount]);
  Inc(FCount);

  f := TFileStream.Create(FDestDir+s, fmCreate);
  try
    writeln_s(f,'MAIL FROM:<'+SMTPNormalizeAddress(msg.Head.Absender)+'>');
    for i := 0 to msg.Head.Empfaenger.Count-1 do
      if CPos('@', msg.Head.Empfaenger[i]) > 0 then
        writeln_s(f,'RCPT TO:<'+SMTPNormalizeAddress(msg.Head.Empfaenger[i])+'>');
    writeln_s(f,'DATA');
    ConnectStream(f, TDotEscapeStream.Create);
    msg.head.WriteRFC(f, true);
  finally
    f.free;
  end;
end;

end.


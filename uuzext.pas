{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

program uuzext;

{$I XPDEFINE.INC }

uses
  SysUtils, Classes, uuz, xpglobal, crt;

procedure logo;
begin
  writeln;
  writeln('ZConnect <-> RFC/UUCP/SMTP Converter with MIME (c) ''93-99 PM');
  writeln('OpenXP-Version ', verstr, pformstr, betastr, ' ', x_copyright,
    ' by ', author_name, ' <', author_mail, '>');
  writeln;
end;

procedure HelpPage;
begin
  writeln('UUZ -uz [Switches] <Source file(s)> <Destination file> [ownsite.domain]');
  writeln('UUZ -zu [Switches] <Source file> <Dest.Dir.> <fromSite> <toSite> [Number]');
  writeln;
  writeln('uz switches:  -graberec  =  grab envelope recipient from Received-header');
  writeln;
  writeln('zu switches:  -s      =  Taylor UUCP size negotiation');
  writeln('              -SMTP   =  Batched SMTP (-c/f/zSMTP = compressed)');
  writeln('              -MIME   =  Use MIME for news');
  writeln('              -noMIME =  Do not create any MIME headers');
  writeln('              -qp     =  MIME: quoted-printable (default: 8bit)');
  writeln('              -1522   =  MIME: create RFC-1522 headers');
  writeln('              -uUser  =  User to return error messages to');
  writeln('              -x      =  Export all unkown X-Lines');
  halt(1);
end;

var
  UUZC: TUUZ;
begin
  Randomize;
  UUZc := TUUZ.Create;
  with uuzc do
  try
    try
      logo;
      try
        getpar;
      except
        HelpPage;
        raise;
      end;
      testfiles;
      if u2z then
        UtoZ
      else
        ZtoU;
    except
      on E: Exception do Writeln(E.Message);
    end;
  finally
    UUZc.Free;
  end;
end.


{
  $Log$
  Revision 1.2  2000/10/10 12:27:35  mk
  - added xpdefine.inc

  Revision 1.1  2000/08/27 10:37:09  mk
  - UUZ ist jetzt intern

}


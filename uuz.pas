{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

program uuz;

{$I XPDEFINE.INC }

uses
  SysUtils, Classes, zcrfc, xpglobal, crt;

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
  writeln('uuz -uz [Switches] <Source file(s)> <Destination file> [ownsite.domain]');
  writeln('uuz -zu [Switches] <Source file> <Dest.Dir.> <fromSite> <toSite> [Number]');
  writeln;
  writeln('uz switches:  -graberec  =  grab envelope recipient from Received-header');
  writeln;
  writeln('zu switches:  -s      =  Taylor UUCP size negotiation');
  writeln('              -SMTP   =  Batched SMTP (-c/f/g/z/bSMTP = compressed)');
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
  Revision 1.78  2000/11/14 21:36:52  fe
  Renamed unit "uuz" to "zcrfc" and program "uuzext" to "uuz".
  So the program is called "uuz" again.

  Revision 1.3  2000/11/02 21:27:04  fe
  bzip2 support added.

  Revision 1.2  2000/10/10 12:27:35  mk
  - added xpdefine.inc

  Revision 1.1  2000/08/27 10:37:09  mk
  - UUZ ist jetzt intern

}

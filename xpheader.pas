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

   Created on December, 03th 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
   Copyright (c) 2000 by the OpenXP Team.

}
{$I XPDEFINE.INC }

{ Contains class THeader }

{ Headerdefinitionen, die auch von den Tools genutzt werden }

unit xpheader;

interface

uses Classes;

type
  mimedata = record
    mversion: string;                   { MIME-Version              }
    encoding: byte;                     { Content-Transfer-Encoding }
    ctype: byte;                        { Content-Type              }
    subtype: string;                    { Content-Subtype           }
    charset: string;                    { text/*; charset=...       }
    filetype: string;                   { application/o-s; type=... }
    boundary: string;                   { multipart: boundary=...   }
  end;

  THeader = class
  public
    netztyp: byte;                      { --- intern ----------------- }
    archive: boolean;                   { archivierte PM               }
    attrib: word;                       { Attribut-Bits                }
    filterattr: word;                   { Filter-Attributbits          }
    empfaenger: string; { --- allgemein --- Brett / User / TO:User }
    Kopien: TStringList;                { KOP: - Liste }
    empfanz: integer;                   { Anzahl EMP-Zeilen }
    betreff: string;
    absender: string;
    datum: string;                      { Netcall-Format               }
    zdatum: string;                     { ZConnect-Format; nur auslesen }
    orgdate: boolean;                   { Ausnahme: zdatum schreiben   }
    pfad: string;                       { Netcall-Format               }
    msgid, ref: string;                 { ohne <>                      }
    ersetzt: string;                    { ohne <>                      }
    typ: string;                        { T / B                        }
    crypttyp: string;                   { '' / T / B                   }
    charset: string;
    ccharset: string;                   { crypt-content-charset }
    groesse: longint;
    realname: string;
    programm: string;                   { Mailer-Name }
    organisation: string;
    postanschrift: string;
    telefon: string;
    homepage: string;
    replyto: tstringlist;               { Antwort-An    }
    followup: tstringlist;              { Diskussion-In }
    komlen: longint;                    { --- ZCONNECT --- Kommentar-Laenge }
    ckomlen: longint;                   { Crypt-Content-KOM }
    datei: string;                      { Dateiname                  }
    ddatum: string;                     { Dateidatum, jjjjmmtthhmmss }
    prio: byte;                         { 10=direkt, 20=Eilmail      }
    error: string;                      { ERR-Header              }
    oem, oab, wab: string;
    oar, war: string;                   { Realnames }
    real_box: string; { --- Maggi --- falls Adresse = User@Point }
    hd_point: string;                   { eigener Pointname }
    pm_bstat: string;                   { --- Maus --- Bearbeitungs-Status }
    org_msgid: string;
    org_xref: string;
    ReplyPath: string;
    ReplyGroup: string;                 { Kommentar-zu-Gruppe          }
    fido_to: string;                    { --- Fido ------------------- }
    x_charset: string;                  { --- RFC -------------------- }
    keywords: string;
    summary: string;
    expiredate: string;                 { Expires / LDA }
    priority: byte;                     { Priority: 1, 3, 5 }
    distribution: string;
    pm_reply: boolean;                  { Followup-To: poster }
    quotestring: string;
    empfbestto: string;
    pgpflags: word;                     { PGP-Attribut-Flags           }
    pgp_uid: string;                    { alternative Adresse          }
    vertreter: string;
    XPointCtl: longint;
    nokop: boolean;
    mimever: string;                    { MIME }
    mimect: string;
    boundary: string;                   { MIME-Multipart-Boundary      }
    gate: string;
    mimetyp: string;
    xnoarchive: boolean;
    Cust1, Cust2: string;
    control: string;
    uline: TStringList;
    xline: TStringList;                 // X-Zeilen, die 'uebrig' sind
    zline: TStringList;
    fline: TStringList;
    References: TStringList;            // references:
    mimereltyp: string;
    xempf: TStringList;
    mailcopies: tstringlist;
    xoem: TStringList;
    MIME: mimedata;
    gateway: string;
    sender: string;
    lines: longint;                     { "Lines:" }
    envemp: string;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

  SendUUdata = record
                     followup   : TStringlist;
                     replyto    : TStringlist;
                     References : TStringList;
                     keywords   : string;
                     summary    : string;
                     distribute : string;
                     ReplyGroup : string;     { Maus-QuoteTo }
                     oab,oem,wab: string;
                     oar,war    : string;
                     onetztyp   : byte;
                     orghdp     : THeader;
                     quotestr   : string;
                     UV_edit    : boolean;        { <Esc> -> "J" }
                     empfrealname : string;
                     msgid,
                     ersetzt    : string;
                   end;
      SendUUptr   = ^SendUUdata;

implementation

constructor THeader.Create;
begin
  inherited Create;
  Kopien := TStringList.Create;
  ULine := TStringList.Create;
  XLIne := TStringList.Create;
  fLine := TStringList.Create;
  zLIne := TStringList.Create;
  ReplyTo := TStringList.Create;
  Followup := TStringList.Create;
  MailCopies := TStringList.Create;
  References := TStringList.Create;
  XEmpf := TStringList.Create;
  XOEM := TStringList.Create;
  Clear;
end;

procedure THeader.Clear;
begin
  netztyp := 0;
  archive := false;
  attrib := 0;
  filterattr := 0;
  empfaenger := '';
  Kopien.Clear;
  empfanz := 0;
  betreff := '';
  absender := '';
  datum := '';
  zdatum := '';
  orgdate := false;
  pfad := '';
  msgid := '';
  ersetzt:= '';                    { ohne <>                      }
  typ:= '';                        { T / B                        }
  crypttyp:= '';                   { '' / T / B                   }
  charset:= '';
  ccharset:= '';                   { crypt-content-charset }
  groesse := 0;
  realname:= '';
  programm:= '';                   { Mailer-Name }
  organisation:= '';
  postanschrift:= '';
  telefon:= '';
  homepage:= '';
  replyto.clear;;               { Antwort-An    }
  followup.clear;;              { Diskussion-In }
  komlen := 0;
  ckomlen := 0;
  datei:= '';                      { Dateiname                  }
  ddatum:= '';                     { Dateidatum, jjjjmmtthhmmss }
  prio := 0;
  error:= '';                      { ERR-Header              }
  oem := '';
  oab:= '';
  wab:= '';
  oar:= '';
  war:= '';                   { Realnames }
  real_box:= ''; { --- Maggi --- falls Adresse = User@Point }
  hd_point:= '';                   { eigener Pointname }
  pm_bstat:= '';                   { --- Maus --- Bearbeitungs-Status }
  org_msgid:= '';
  org_xref:= '';
  ReplyPath:= '';
  ReplyGroup:= '';                 { Kommentar-zu-Gruppe          }
  fido_to:= '';                    { --- Fido ------------------- }
  x_charset:= '';                  { --- RFC -------------------- }
  keywords:= '';
  summary:= '';
  expiredate:= '';                 { Expires / LDA }
  priority := 0;
  distribution:= '';
  pm_reply := false;
  quotestring:= '';
  empfbestto:= '';
  pgpflags := 0;
  pgp_uid:= '';                    { alternative Adresse          }
  vertreter:= '';
  XPointCtl := 0;
  nokop:= false;
  mimever:= '';                    { MIME }
  mimect:= '';
  boundary:= '';                   { MIME-Multipart-Boundary      }
  gate:= '';
  mimetyp:= '';
  xnoarchive:= false;;
  Cust1 := '';
  Cust2:= '';
  control:= '';
  uline.clear;;
  xline.clear;;                    // X-Zeilen, die 'uebrig' sind
  zline.clear;;
  fline.clear;;
  References.Clear;
  mimereltyp:= '';
  xempf.clear;;
  mailcopies.clear;;
  xoem.clear;;
  gateway:= '';
  sender:= '';
  lines := 0;
  envemp:= '';
end;

destructor THeader.Destroy;
begin
  Kopien.Free;
  ULine.Free;
  XLine.Free;
  fLine.Free;
  zLine.Free;
  ReplyTo.Free;
  Followup.Free;
  Mailcopies.free;
  inherited destroy;
end;


end.

{
  $Log$
  Revision 1.4  2001/01/02 10:05:27  mk
  - implemented Header.References

  Revision 1.3  2000/12/30 17:47:41  mk
  - renamed AddRef to References

  Revision 1.2  2000/12/30 17:36:54  mk
  *** empty log message ***

  Revision 1.1  2000/12/03 12:38:26  mk
  - Header-Record is no an Object

  Revision 1.9  2000/11/25 10:31:48  mk
  - some fixes for new SendUUData

  Revision 1.8  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.7  2000/11/17 19:35:45  fe
  Followup-To support updated to ZC 3.1.
  Mail-Copies-To support added.

  Revision 1.6  2000/11/09 18:15:12  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.5  2000/11/05 20:14:13  fe
  Added LDA/Expires.

  Revision 1.4  2000/09/21 16:22:21  mk
  - ZFido wieder compilierbar

  Revision 1.3  2000/07/21 13:23:48  mk
  - Umstellung auf TStringList

  Revision 1.2  2000/07/09 13:21:56  mk
  - UUZ nutzt jetzt xpheader.inc

  Revision 1.1  2000/07/09 09:09:56  mk
  - Newexit in Initialization/Finalization umgewandelt

}

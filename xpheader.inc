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

{ Headerdefinitionen, die auch von den Tools genutzt werden }

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

  header = record
    netztyp: byte;                      { --- intern ----------------- }
    archive: boolean;                   { archivierte PM               }
    attrib: word;                       { Attribut-Bits                }
    filterattr: word;                   { Filter-Attributbits          }
    empfaenger: string; { --- allgemein --- Brett / User / TO:User }
    Kopien: TStringList;                { KOP: - Liste }
    empfanz: integer;                   { Anzahl EMP-Zeilen }
    Followupz: integer;                   { Anzahl EMP-Zeilen }
    betreff: string;
    absender: string;
    datum: string;                      { Netcall-Format               }
    zdatum: string;                     { ZConnect-Format; nur auslesen }
    orgdate: boolean;                   { Ausnahme: zdatum schreiben   }
    pfad: string;                       { Netcall-Format               }
    msgid, ref: string;                 { ohne <>                      }
    ersetzt: string;                    { ohne <>                      }
    refanz: integer;                    { Anzahl BEZ-Zeilen            }
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
    PmReplyTo: string;                  { Antwort-An    }
    AmReplyTo: string;                  { Diskussiom-In }
    amrepanz: integer;                  { Anzahl Diskussion-in's }
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
    xline: TStringList;                    // X-Zeilen, die 'uebrig' sind
    zline: TStringList;
    fline: TStringList;

    // UUZ
    addref: TStringList;  // Referenzen
    mimereltyp: string;
    xempf: TStringList;
    xoem: TStringList;
    followup: TStringList;
    MIME: mimedata;
    gateway: string;
    sender: string;
    lines: longint;                     { "Lines:" }
    envemp: string;
  end;
  headerp = ^header;

{
  $Log$
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

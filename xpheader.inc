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

{ Headerdefinitionen, die auch von den Tools genutzt werden }

type
  empfnodep = ^empfnode;
  empfnode = record
    next: empfnodep;
    empf: string;
  end;

  header = record
    netztyp: byte;                      { --- intern ----------------- }
    archive: boolean;                   { archivierte PM               }
    attrib: word;                       { Attribut-Bits                }
    filterattr: word;                   { Filter-Attributbits          }
    empfaenger: string; { --- allgemein --- Brett / User / TO:User }
    kopien: empfnodep;                  { KOP: - Liste }
    empfanz: integer;                   { Anzahl EMP-Zeilen }
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
    komlen: longint;                    { --- ZCONNECT --- Kommentar-L„nge }
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
  end;
  headerp = ^header;

  {
    $Log$
    Revision 1.1  2000/07/09 09:09:56  mk
    - Newexit in Initialization/Finalization umgewandelt

  }

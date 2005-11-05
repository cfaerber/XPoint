{   $Id$

    OpenXP typeform unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc }

unit typeform;

interface

uses
  xpglobal, sysutils, Classes;

{$IFNDEF DPMI}
  const  Seg0040 = $40;
         SegA000 = $a000;
         SegB000 = $b000;
         SegB800 = $b800;
{$ENDIF}

const
  ISO2IBMTab : array[128..255] of byte =
{128}  (128,177,177,177,177,177,177,177,177,177,177,177,177,177,177,177,
{144}   177,177,177,177,177,177,177,177,177,177,177,177,177,177,177,177,
{160}    32,173,155,156,177,157,124, 21, 34, 67,166,174,170, 45, 82,196,
{176}   248,241,253, 51, 39,230, 21,250, 44, 49,167,175,172,171,177,168,
{192}    65, 65, 65, 65,142,143,146,128, 69,144, 69, 69, 73, 73, 73, 73,
{208}    84,165, 79, 79, 79, 79,153,120, 79, 85, 85, 85,154, 89, 84,225,
{224}   133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
{240}   116,164,149,162,147,111,148,246,111,151,163,150,129,121,116,152);

{
  From: my@openxp16.de (Michael Heydekamp)
  Newsgroups: crosspoint.openxp16.pub.developer
  Date: 30 Jun 2002 23:52:00 +0200
  Message-ID: <8RqQgycppwB@my.openxp16.de> auf news.kannofant.de

  [...]

  Grunds‰tzlich werden in CP437 nicht darstellbare Zeichen jetzt in das
  "Nicht-konvertierbar-Zeichen" gewandelt.  Dazu verwende ich das IBM- 
  Grafikzeichen #177 (Jochen hat das mal so eingef¸hrt und ich find's  
  deutlich besser und eindeutiger als das Fragezeichen).

  Bisher wurden nicht darstellbare Zeichen jedenfalls oft einfach 1:1  
  durchgereicht, so daﬂ dann da z.B. bei einem Promillezeichen (#137 in  
  Windows-1252) ein "Î" (eben #137 in CP437) dargestellt wurde, was nicht  
  sehr sinnvoll ist.  Jetzt erkennt man wenigstens, daﬂ da irgendein nicht  
  konvertierbares Zeichen gestanden hat und eben kein "Î".

  Es gibt einige Zeichen, die sich theoretisch durch mehrere andere  
  Zeichen darstellen lieﬂen (Multichar-Konvertierung), z.B. das  
  Warenzeichen auf ISO #174 als "(R)". Bei ausgehenden Nachrichten ist das  
  bereits realisiert, bei eingehenden jedoch noch nicht (und bei der  
  ISO=>IBM-Konvertierung geht es nur um eingehende Nachrichten).  Das ist  
  auch nicht so ohne, weil der UUZ eine reine String-Konvertierung macht  
  und man sich da wegen der 255-Zeichen-Begrenzung irgendwas einfallen  
  lassen muﬂ.  In XP selbst (z.B. bei der Darstellung von MIME-Multiparts)  
  sollte das ‰hnlich zu realisieren sein wie f¸r ausgehende Nachrichten,  
  muﬂ aber auch erstmal gemacht werden.


  Bei den ‹berlegungen, in welches einzelne Zeichen man konvertiert und ob  
  ¸berhaupt, habe ich mich danach gerichtet, wie der Buchstabe  
  ausgesprochen wird bzw. welche Bedeutung ein Symbol hat (und habe dazu  
  notfalls etwas Recherche betrieben, z.B. bei den Buchstaben "Eth" und  
  "Thorn" oder dem "Pilcrow sign"). Optische ƒhnlichkeiten waren (im
  Unterschied zu den alten Tabellen) eher kein Kriterium.


  Hier also die ƒnderungen.  Zun‰chst die, die bei allen direkt  
  unterst¸tzten Zeichens‰tzen (ISO1, ISO15 und Windows-1252) identisch  
  sind:


  Alle Zeichens‰tze
  =================


  ISO #175 => IBM #196
  --------------------
  Ein langer hochstehender Strich ("Macron"), wurde fr¸her in das  
  Blockgrafikzeichen #223 (?!) konvertiert, jetzt in den etwas l‰ngeren
  Grafikstrich #196. Kann man dr¸ber streiten, ist eigentlich nicht nach  
  CP437 konvertierbar.


  ISO #179 => IBM #51 ("3")
  -------------------------
  Da hat Peter wohl CP437 und CP850 durcheinandergeworfen: Die  
  hochgestellte "3" wurde fr¸her nach IBM #252 konvertiert, das ist aber  
  in CP437 ein "^n" (<= an dieser Stelle wird man BTW die neue Multichar-
  Konvertierung in XP bemerken, ich habe im Editor <Alt-252> getippt und  
  sehe dort jetzt auch ein hochgestelltes "n", aber weil's das in ISO1/ 
  ISO15 nicht gibt, wird XP es gleich nach "^n" wandeln). Jedenfalls ist  
  "3" f¸r "hoch-3" passender als "hoch-n".  W‰re auch ein Kandidat f¸r  
  Multichar-Konvertierung bei eingehenden Nachrichten.


  ISO #182 => IBM #21 ("ß")
  -------------------------
  Das ist das sog. "Pilcrow sign" (oder auch "Paragraph sign" oder  
  "Section sign"), das h‰ufig als Absatzzeichen in Editoren verwendet  
  wird.  Dieses im englischen Sprachraum (aber auch da nur sehr selten)  
  benutzte Zeichen entspricht vom Sinn her am ehesten dem im deutschen  
  Sprachraum benutzten Paragraphenzeichen "ß" und wird deshalb jetzt auch
  dorthin konvertiert (statt wie fr¸her nach IBM #227, also einem kleinen  
  griechischen "pi" - grmpf).
  Quellen: http://www.daube.ch/docu/glossary/signs.html
           http://ppewww.ph.gla.ac.uk/~flavell/iso8859/digress.html


  ISO #183 => IBM #250 ("∑")
  --------------------------
  Schlicht ein Fehler in den alten Tabellen, dieser "middle dot" (Unicode  
  00B7) existiert sowohl im ISO- wie auch im IBM-Zeichensatz, wurde aber  
  stattdessen bisher nach #249 ("bullet operator") konvertiert.


  ISO #192/#193/#194 => IBM #65 ("A")
  -----------------------------------
  Das sind groﬂe "A" mit Akzent, die jetzt eben in ein groﬂes "A"  
  konvertiert werden (und fr¸her in kleine ‡, ·, oder ‚).


  ISO #200/#202/#203 => IBM #69 ("E")
  -----------------------------------
  Analog dasselbe f¸r groﬂe "E" mit Akzentzeichen (fr¸her Ë, Í, Î).


  ISO #204/205/206/207 => IBM #73 ("I")
  -------------------------------------
  Analog dasselbe f¸r groﬂe "I" mit Akzentzeichen (fr¸her Ï, Ì, Ó, Ô).


  ISO #208 => IBM #84 ("T") bzw. ISO #240 => IBM #116 ("t")
  ---------------------------------------------------------
  Das ist z.B. so 'n Klopfer: Das sind die Buchstaben "Eth" (groﬂ und  
  klein), aber weil die so ‰hnlich aussehen wie ein "D" bzw. "d", wurden  
  sie bisher eben in ein "D" bzw. "d" konvertiert.  Ausgesprochen wird
  "Eth" aber wie ein "Th" in "That", das w‰re also z.B. auch ein Kandidat  
  f¸r 'ne Multicharkonvertierung. Da es die noch nicht gibt, wird "Eth"  
  vorerst in ein "T" bzw. "t" konvertiert.
  Quelle: http://briem.ismennt.is/2/2.1a/2.1.1.thorn.and.eth.htm


  ISO #210/#211/#212/#213/#216 => IBM #79 ("O")
  ---------------------------------------------
  Groﬂe "O" mit Akzentzeichen jetzt nach "O" statt Ú, Û, Ù, o, ph
  (#216 war BTW auch 'n Klopfer: Da wurde ein durchgestrichenes groﬂes "O"  
  in ein kleines griechisches "phi" (IBM #237) konvertiert, wohl weil's  
  halt halbwegs ‰hnlich aussieht, wenn man nicht so genau hinguckt.)


  ISO #217/#218/#219 => IBM #85 ("U")
  -----------------------------------
  Analog dasselbe f¸r groﬂe "U" mit Akzentzeichen (fr¸her ˘, ˙, ˚).


  ISO #221 => IBM #89 ("Y")
  -------------------------
  Analog dasselbe f¸r groﬂes "Y" mit Akzentzeichen (fr¸her *kleines* "y",  
  ohne Akzent, warum?!).


  ISO #222 => IBM #84 ("T") bzw. ISO #254 => IBM #116 ("t")
  ---------------------------------------------------------
  Das sind die Buchstaben "Thorn" (groﬂ und klein), es gilt analog  
  dasselbe wie schon bei "Eth" - wird gesprochen wie ein "Th" in "Thing"  
  und deshalb f¸rs erste nach "T" bzw. "t" konvertiert.
  Die bisherige Konvertierung war ganz klasse: "Thorn" wurde einfach in  
  ein "P" bzw. "p" konvertiert... argh.



  Jetzt noch ein paar zeichensatzspezifische ƒnderungen:


  ISO1 und ISO15
  ==============


  ISO #128-#159 => IBM #177 (unkonvertierbar)
  -------------------------------------------
  Alles irgendwelche nicht darstellbaren Steuerzeichen, wurden bisher 1:1  
  durchgereicht (falls sie ¸berhaupt vorkommen konnten).  Sonderfall: Zu  
  ISO #128 bei ISO1 (Euro) siehe unten.



  ISO1 und Windows-1252
  =====================


  ISO #128 => IBM #238 ("Ä" = Epsilon)
  ------------------------------------
  Euro-Support in XP (auf Wunsch als echtes Euro-Symbol statt als Epsilon
  darstellbar). Wurde bisher 1:1 durchgereicht, ergo wurde das Euro-Symbol  
  als "«" dargestellt.


  ISO #164 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Das r‰tselhafte "currency sign", w¸rde fr¸her nach #120 ("x")  
  konvertiert (?!) und wird jetzt als unkonvertierbar behandelt.


  ISO #190 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Das Zeichen "3/4" wurde fr¸her nach #47 ("/") konvertiert - toll. ;)  
  Auch ein Multichar-Kandidat.



  ISO15 only
  ==========


  ISO #164 => IBM #238 ("Ä" = Epsilon)
  ------------------------------------
  Euro-Support in XP (auf Wunsch als echtes Euro-Symbol statt als Epsilon  
  darstellbar). Wurde bisher 1:1 durchgereicht, ergo wurde das Euro-Symbol  
  als "Ò" dargestellt.



  Windows-1252 only
  =================


  ISO #129 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Zeichen ist nicht belegt, wurde bisher 1:1 durchgereicht (¸).


  ISO #130 => IBM #44 (",")
  -------------------------
  Einfaches Anf¸hrungszeichen unten, jetzt Komma in CP437, fr¸her  
  Apostroph (aka Hochkomma, "'").


  ISO #133/#134/#135 => IBM #177 (unkonvertierbar)
  ------------------------------------------------
  "Horizontal Ellipsis", "Dagger" und "Double Dagger" - irgendwelche  
  selten benutzten Zeichen aus der Mathematik/Physik AFAIK.  Wurden fr¸her  
  1:1 durchgereicht (‡, Â, Á), werden jetzt als unkonvertierbar behandelt.


  ISO #137 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Promillezeichen, nicht in CP437 darstellbar, wurde fr¸her 1:1
  durchgereicht ("Î").  Wird vorerst als unkonvertierbar behandelt,
  Kandidat f¸r Multichar-Konvertierung.


  ISO #140 => IBM #79 ("O") bzw. ISO #156 => IBM #111 ("o")
  ---------------------------------------------------------
  Groﬂe/kleine Ligatur "OE", werden jetzt als "O" bzw. "o" dargestellt
  (vorerst, ebenfalls Kandidaten f¸r Multichar-Konvertierung).  Wurden
  bisher 1:1 durchgereicht und demzufolge als "Ó" bzw. "£" dargestellt -
  seltsam und inkonsequent eigentlich, denn dieselben Zeichen wurden bei
  ISO15 (dort #188 und #189) schon immer zu einem "O" bzw. "o"
  konvertiert...


  ISO #141/#143/#144 => IBM #177 (unkonvertierbar)
  ------------------------------------------------
  Zeichen sind nicht belegt, wurden bisher 1:1 durchgereicht (Ï, ≈, …).


  ISO #150/#151 => IBM #196
  -------------------------
  Etwas l‰ngere Bindestriche ("n-Dash", "m-Dash"), werden jetzt in das
  l‰ngere IBM-Grafikzeichen #196 konvertiert.  Bisher #45 (normaler
  Bindestrich).


  ISO #153 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Trademark-Symbol, in CP437 nicht darstellbar (daher Kandidat f¸r
  Multichar-Konvertierung "(tm)").  Wurde bisher 1:1 durchgereicht und
  daher als "÷" dargestellt.


  ISO #157 => IBM #177 (unkonvertierbar)
  --------------------------------------
  Zeichen ist nicht belegt, wurde bisher 1:1 durchgereicht (•).

  [...]
}

  IBM2ISOTab : array[0..255] of byte =
  ( 32, 32, 32, 32, 32, 32, 32, 32, 32,  9, 10, 32, 12, 13, 32, 42,
    62, 60, 32, 33, 32,167, 95, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111,
   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,

   199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197,
   201,230,198,244,246,242,251,249,255,214,220,162,163,165, 80, 32,
   225,237,243,250,241,209,170,186,191, 43,172,189,188,161,171,187,
    32, 32, 32,124, 43, 43, 43, 43, 43, 43,124, 43, 43, 43, 43, 43,
    43, 43, 43, 43, 45, 43, 43, 43, 43, 43, 43, 43, 43, 45, 43, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 32, 32, 32, 32, 32,
    97,223, 71,182, 83,115,181,110,111, 79, 79,100,111,248, 69, 32,
    61,177, 62, 60,124,124,247, 61,176,183,183, 32,179,178,183, 32);

      { Mac: éèÄê•ôö†ÖÉÑaÜáÇä àâ°çåã§¢ïìîo£óñÅ +¯õú˘·RCt'"!íO
             ÏÒÛÚùÎ‰„Ù„aoÍ_Ì  ®≠™˚ü˜^ÆØ__AAOOo --,"`'ˆ˛òY/x<>__
             +˙,"_AEAEEIIIIOO _OUUUi^~-_˙¯,",_

        fehlt: BE, DE, DF }

      Mac2IBMtab : array[128..255] of byte =
      (142,143,128,144,165,153,154,160,133,131,132, 97,134,135,130,138,
       136,137,161,141,140,139,164,162,149,147,148,111,163,151,150,129,
        43,248,155,156, 21,249, 20,225, 82, 67,116, 39, 34, 33,146, 79,
       236,241,243,242,157,230,235,228,227,227,244, 97,111,234, 32,237,
       168,173,170,251,159,247, 94,174,175, 32, 32, 65, 65, 79, 79,111,
        45, 45, 44, 32, 96, 39,246,254,152, 89, 47,120, 60, 62, 32, 32,
        43,250, 44, 32, 32, 65, 69, 65, 69, 69, 73, 73, 73, 73, 79, 79,
        32, 79, 85, 85, 85,105, 94,126, 45, 32,250,248, 44, 34, 44, 32);

type DateTimeSt = string;
     s20        = string;
     s40        = string;
     s60        = string;
     s80        = string;
     atext      = s80;

function Bin(l:longint; n:integer):string;      { Bin-Zahl mit n Stellen       }
function Blankpos(const s:string):integer;        { Position von ' ' oder #9     }
function BlankposX(const s:string): integer;       { length(s)+1, falls bp=0      }
function b30(l:longint):string;   { 30bit -> 5char }
function Center(const s:string; n:integer):string;    { String auf n Zchn. zentrieren}
function CountChar(const c: char; const s: string): integer; { zaehlt c in s }
function CPos(c:char; const s:string):integer;    { schnelles POS fuer CHARs      }
function CPosX(c:char; const s:string):integer;   { pos=0 -> pos:=length(s)+1    }
function CreditCardOk(s:string):boolean;     { Kreditkartennummer ueberpruefen }
function CVal(const s:string):longint;             { C Value Integer - nnnn/0nnn/0xnnn }
function Date:DateTimeSt;                    { dt. Datumsstring             }
function Dup(const n:integer; const c:Char):string;      { c n-mal duplizieren          }
function ExtractWord(n: Cardinal; const S: String) : String;
function WordPosition(N : Cardinal; const S: String; Delim: Char;
                      var Pos : Cardinal) : Boolean;
function ExtractWordEx(n: Cardinal; const S: String; Delim: Char): String;
function WordCount(const S: String): Integer;
function WordCountEx(const S, Delims: String): Integer;
function FileName(var f):string;                { Dateiname Assign             }
// Erstes Zeichen eines Strings, wenn nicht vorhanden dann #0
function FirstChar(const s:string):char;
// Letzctes Zeichen eines Strings, wenn nicht vorhanden dann #0
function LastChar(const s:string):char;
function fitpath(path:TFilename; n:integer):TFilename;   {+ Pfad evtl. abkuerzen    }
function FormI(const i:longint; const n:integer):string;    { i-->str.; bis n mit 0 auff.  }
function FormR(const r:real; const vk,nk:integer):string;   { r-->str.; vk+nk mit 0 auff.  }
function FormS(const s:string; n:integer):string;     { String auf n Stellen mit ' ' }
function GetToken(var s:string; delimiter:string):string;
function GetTokenC(var s:string; delim_chars:string):string;
function HBar(const len:integer):string;              { √ƒƒƒƒƒƒƒƒƒ...ƒƒƒƒƒƒƒƒƒ¥      }
function Hex(const l:integer; const n:integer):string;      { Hex-Zahl mit n Stellen       }
function HexVal(const s:string):longint;           { Hex-Val                      }
function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
function iifs(b:boolean; const s1,s2:string):string;  { IIF String                }
function IsNaN(d:double):boolean;
function IntQSum(const l:longint):longint;         { Quersumme                    }
function isnum(const s:string):boolean;            { s besteht aus [0..9]         }
function IVal(const s:string):longint;             { Value Integer                }
{$IFNDEF FPC }
function LeftStr(const s: string; Count: integer): string;
function RightStr(const s: string; Count: integer): string;
{$ENDIF }
function LoCase(const c:char):char;                { LowerCase                    }
function Max(const a,b:longint):longint;          { Maximum Integer              }
function MaxR(const a,b:real):real;                { Maximum Real                 }
function MaxS(const a,b:string):string;            { Maximum String               }
function Mid(const s:string; const n:integer):string;       { Rest des Strings ab Pos.    }
function Min(a,b:longint):longint;              { Minimum Integer              }
function MinMax(const x,min,max:longint):longint;  { x -> [min,max]               }
function MinMaxR(const x,min,max:real):real;       { x -> [min,max]               }
function MinR(const a,b:real):real;                { Minimum Real                 }
function MinS(const a,b:string):string;            { Minimum String               }
function MultiPos(const s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
function NaN:Double;
function OctVal(s:string):longint;           { Oktalstring -> Logint        }
function PosN(const s1,s2:string; n:integer):integer;    { POS ab Stelle n              }
function PosX(const s1,s2:string):integer;            { length(s)+1, falls pos=0     }
function ProgName:TFilename;                   { Name des Programms           }
function ProgPath:TFilename;                   { Pfad des Programms           }
function QSum(const s:string):longint;             { Quersumme                    }
function Range(const c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
function Reverse(const s:string):string;           { String umkehren              }
function rforms(const s:string; const n:integer):string;    { String links mit ' ' auff.   }
function RightPos(c:char; const s:string):integer;    { Pos von rechts               }
function Round(const r:real; const nk:integer):real;     { Real --> Real auf nk runden  }
function RVal(const s:string):real;                { Value Real                   }
function Sgn(const x:longint):longint;       { Signum Integer               }
function SgnR(const x:real):real;            { Signum Real                  }
function SMatch(const s1,s2:string):integer;          { Anzahl der uebereinst. Bytes  }
function SiMatch(const s1,s2:string):integer;         { dto., ignore case            }
function Sp(const n:integer):string;               { space$                       }
function Stricmp(const s1,s2:string):boolean;      { UStr-Vergleich               }
function StrS(const l:longint):string;             { "echtes" Str$, Integer       }
function StrSn(const l:longint; const n:integer):string;    { "echtes" Str$, Integer       }
function StrSr(const r:real; const nk:integer):string;      { Str$ auf nk, Real            }
function StrSrn(const r:real; const vk,nk:integer):string;  { "echtes" Str$, Real          }
function StrSrnp(const r:real; const vk,nk:integer):string; { "echtes" Str$, Real, mit DP  }
function Time:DateTimeSt;                    { dt. Zeitstring               }
function TimeDiff(t1,t2:DateTimeSt):longint; { Abstand in Sekunden          }
function TopStr(const s:string):string;            { erste Buchstabe gross         }
function TopAllStr(s:string):string;         { alle ersten Buchstaben gross  }
procedure TrimFirstChar(var s: String; c: Char);   { Spezielles Zeichen am Anfang des String entfernen }
procedure TrimLastChar(var s: String; c: Char);   { Spezielles Zeichen am Ende des String entfernen }
{$ifndef FPC}
function UpCase(const c:char):char;                { int. UpCase                  }
{$endif}
function UTF8Mid(const s:string; n:integer):string;       { Rest des Strings ab Pos.    }
function UTF8FormS(const s:string; StartColumn,Columns:integer):string; overload; { String auf n Stellen mit ' ' }
function UTF8FormS(const s:string; Columns:integer):string; overload; { String auf n Stellen mit ' ' }
{ Lo/Upcase-String fuer Files, abhaengig von UnixFS }
function FileUpperCase(const s:string):string;
function Without(const s1,s2:string):string;       { Strings "subtrahieren"       }
Procedure DeleteFirstChar(var s:string);            { ersten Buchstaben loeschen    }
Procedure DeleteLastChar(var s:string);            { letzten Buchstaben loeschen   }
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure LoString(var s:string);            { LowerString                  }
Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf Paritaet setzen  }
Procedure TruncStr(var s:string; n:integer);    { String kuerzen                }
Procedure UpString(var s:string);            { UpperString                  }
function mailstring(s: String; Reverse: boolean): string; { JG:04.02.00 Mailadresse aus String ausschneiden }
procedure UkonvStr(var s:string;len:integer);     { Umlautkonvertierung (ae,oe...) }
function DecodeRot13String(const s: String): String;
procedure Rot13(var data; Size: Integer);         { Rot 13 Kodierung }
function IsoToIbm(const s:string): String;            { Konvertiert ISO in IBM Zeichnen }
function IBMToISO(const s: String): String;
procedure Iso1ToIBM(var data; size: Integer); 
procedure Mac2IBM(var data; size: LongWord);
{ Der Filename wird zur Anzeige auf den Bildschirm in den richtigen
  Zeichensatz konvertiert }
function ConvertFileName(const s:string): String;
// siehe XPDTAUM !?
procedure ZtoZCdatumNTZ(var d1,d2:string);
//function  DecodeBase64(const s: String):String;

function HostToLittleEndian16(host:smallword):smallword;
function LittleEndianToHost16(host:smallword):smallword;
function HostToLittleEndian32(host:    dword):    dword;
function LittleEndianToHost32(host:    dword):    dword;

function HostToBigEndian16(host:smallword):smallword;
function BigEndianToHost16(host:smallword):smallword;
function HostToBigEndian32(host:    dword):    dword;
function BigEndianToHost32(host:    dword):    dword;

function StringListToString(SL: TStringList): String;
{$IFDEF FPC }
function ExcludeTrailingPathDelimiter(const s: String): String;
function IsPathDelimiter(const S: string; Index: Integer): Boolean;
{$ENDIF }
function FindURL(s: String; var x, y: Integer): Boolean;


{ ================= Implementation-Teil ==================  }

implementation

uses xpunicode;

type
{$IFDEF Delphi }
  LStrRec = packed record
    AllocSize : Longint;
    RefCount  : Longint;
    Length    : Longint;
  end;
{$ELSE }
  LStrRec = packed record
    Maxlen,
    Length,
    ref   : Longint;
  end;
{$ENDIF }

const
  StrOffset = SizeOf(LStrRec);

function CountChar(const c: char; const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i:= 1 to length(s) do
    if s[i]=c then
      inc(Result);
end;

function CPos(c: char; const s: string): integer;
var
  i: Integer;
begin
  for i := 1 to length(s) do
    if s[i]=c then
    begin
      CPos := i;
      Exit;
    end;
  CPos := 0;
end;

procedure SetParity(var b:byte; even:boolean);assembler;
{$IFDEF NOASM }
begin
end
{$ELSE }
asm
          push edi
          mov    edi,b
          mov    al,[edi]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade Paritaet }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade Paritaet }
          jpo    @spok
          or     al,80h
@spok:    mov    [edi],al
          pop edi
end;
{$ENDIF }

function Hoch(const r:real; const n:integer):real;
var i : integer;
    x : real;
begin
  x:=1;
  for i:=1 to n do
    x:=x*r;
  hoch:=x;
end;

function Round(const r:real; const nk:integer):real;
begin
  round:=int(r*hoch(10,nk)+0.5)/hoch(10,nk);
end;


function MaxR(const a,b:real):real;
begin
  if a>b then maxr:=a else maxr:=b;
end;


function MinR(const a,b:real):real;
begin
  if a<b then minr:=a else minr:=b;
end;


function Max(const a,b:longint):longint;
begin
  if a>b then max:=a else max:=b;
end;


function Min(a,b:longint):longint;
begin
  if a<b then min:=a else min:=b;
end;


function MaxS(const a,b:string):string;
begin
  if a>b then maxs:=a else maxs:=b;
end;


function MinS(const a,b:string):string;
begin
  if a<b then mins:=a else mins:=b;
end;


function MinMax(const x,min,max:longint):longint;
begin
  if x<min then MinMax:=min
  else if x>max then MinMax:=max
  else MinMax:=x;
end;


function MinMaxR(const x,min,max:real):real;
begin
  if x<min then MinMaxR:=min
  else if x>max then MinMaxR:=max
  else MinMaxR:=x;
end;


function Sgn(const x:longint):longint;
begin
  if x>0 then
    Sgn:=1
  else
    if x=0 then
      Sgn:=0
    else
      Sgn:=-1;
end;


function SgnR(const x:real):real;
begin
  if x>0 then
    SgnR:=1.0
  else
    if x=0 then
      SgnR:=0
    else
      SgnR:=-1.0;
end;


function FormI(const i:longint; const n:integer):string;
begin
  Str(i, Result);
  while length(Result)<n do
    Result:='0'+ Result;
end;


function FormR(const r:real; const vk,nk:integer):string;
var
  i  : integer;
begin
  i:=vk+nk; if nk>0 then i:=succ(i);
  str(r:i:nk,Result);
  i:=1;
  while Result[i]=' ' do
  begin
    Result[i]:='0';
    i:=succ(i);
  end;
end;

function Time:DateTimeSt;
begin
  Time:= FormatDateTime('hh:nn:ss', Now);
end;

function Date:DateTimeSt;
begin
  Date:= FormatDateTime('dd.mm.yyyy', Now);
end;

function Dup(const n:integer; const c:Char):string;
begin
  if n<=0 then
    Dup:=''
  else begin
    SetLength(Result, n);
    fillchar(Result[1],n,c);
  end;
end;

function ExtractWord(n: Cardinal; const S: String) : String;
begin
  Result := ExtractWordEx(n, s, ' ');
end;

function WordPosition(N : Cardinal; const S: String; Delim: Char;
                      var Pos : Cardinal) : Boolean;
var
  Count : Longint;
  I     : Longint;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (Delim = S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and not (Delim = S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordEx(n: Cardinal; const S: String; Delim: Char): String;
var
  C : Cardinal;
  I, J   : Longint;
begin
  Result := s;
  if WordPosition(N, S, Delim, C) then begin
    I := C;
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not
           (Delim = S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

function WordCount(const S: String): Integer;
begin
  Result := WordCountEx(S, ' ');
end;

function WordCountEx(const S, Delims: String): Integer;
var
  I    : Integer;
  SLen : Integer;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    while (I <= SLen) and (CPos(s[i], Delims) > 0) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and (CPos(s[i], Delims) = 0) do
      Inc(I);
  end;
end;

function Sp(const n:integer):string;
begin
  sp:=dup(n,' ');
end;


function UTF8Mid(const s:string; n:integer):string;
var Position: Integer;
begin
  Position := 1;
  while n > 1 do begin
    dec(n);
    UTF8NextChar(s,Position);
  end;
  Result := Mid(s,Position);
end;

function UTF8FormS(const s:string; StartColumn, Columns:integer):string; overload;
var Position, NewPosition: Integer;
    StartPosition: Integer;
    W: Integer;
    AddStart, AddEnd: string;
{$IFDEF DEBUG}
//    SaveColumns : Integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
//  SaveColumns := Columns;
{$ENDIF}

  StartPosition := 1;
  Dec(StartColumn);
  while StartPosition <= Length(s) do
  begin
    NewPosition := StartPosition;
    w := abs(UnicodeCharacterWidth(UTF8GetCharNext(s,NewPosition)));
    if w > StartColumn then
    begin
      if StartColumn >= 1 then
        AddStart := '<';
      break;
    end;
    dec(StartColumn,w);
    StartPosition := NewPosition;
  end;

  Position := StartPosition;
  while Position <= Length(s) do
  begin
    NewPosition := Position;
    w := abs(UnicodeCharacterWidth(UTF8GetCharNext(s,NewPosition)));
    if w > Columns then
    begin
      if Columns >= 1 then
        AddEnd := '>';
      Columns := 0;
      break;
    end;
    dec(Columns,w);
    Position := NewPosition;
  end;

  Result := AddStart + Copy(s,StartPosition,Position-StartPosition) + AddEnd + Sp(Columns);

{$IFDEF DEBUG}
//  assert(UTF8StringWidth(Result) = SaveColumns);
{$ENDIF}
end;

function UTF8FormS(const s:string; Columns:integer):string; overload;
begin
  result := UTF8FormS(S,1,Columns);
end;

function FormS(const s:string; n:integer):string;
var
  i: integer;
begin
  Result := s;
  SetLength(Result, n);
  for i:=Length(s)+1 to n do
    Result[i]:=' ';
end;


function StrS(const l:longint):string;
begin
  str(l:0,Result);
end;


function StrSn(const l:longint; const n:integer):string;
begin
  str(l:n,Result);
end;


function StrSr(const r:real; const nk:integer):string;
begin
  str(r:0:nk,result);
end;


function StrSrn(const r:real; const vk,nk:integer):string;
begin
  if nk=0 then
    str(r:vk:0,result)
  else
    str(r:vk+nk+1:nk,result);
end;


function StrSrnp(const r:real; const vk,nk:integer):string;
var s : string;
begin
  s:=strsrn(r,vk,nk);
  if r>=1000000 then
    s:=copy(s,3,vk-8)+'.'+copy(s,vk-5,3)+'.'+copy(s,vk-2,3)+','+RightStr(s,nk)
  else if r>=1000 then
    s:=copy(s,2,vk-4)+'.'+copy(s,vk-2,3)+','+RightStr(s,nk)
  else
    s:=copy(s,1,vk)+','+RightStr(s,nk);
  if LastChar(s)=',' then
    s:=' '+copy(s,1,length(s)-1);
  strsrnp:=s;
end;

{$IFNDEF WIN32 }

{$ifndef FPC}
function UpCase(const c:char):char;
begin
  case c of
    'a'..'z' : UpCase:=chr(ord(c) and $df);
    'Ñ'      : UpCase:='é';
    'î'      : UpCase:='ô';
    'Å'      : UpCase:='ö';
    'Ç'      : UpCase:='ê';
    'Ü'      : UpCase:='è';
    'ë'      : UpCase:='í';
    '§'      : UpCase:='•';
    'á'      : UpCase:='Ä';
  else
    UpCase:=c;
  end;
end;
{$ENDIF}

function LoCase(const c:char):char;
begin
  case c of
    'A'..'Z' : LoCase:=chr(ord(c) or $20);
    'é'      : LoCase:='Ñ';
    'ô'      : LoCase:='î';
    'ö'      : LoCase:='Å';
    'ê'      : LoCase:='Ç';
    'è'      : LoCase:='Ü';
    'í'      : LoCase:='ë';
    '•'      : LoCase:='§';
    'Ä'      : LoCase:='á';
  else
    LoCase:=c;
  end;
end;

{$ELSE}

{$ifndef FPC}
function UpCase(const c:char):char;
begin
  case c of
    'a'..'z'  : UpCase:=chr(ord(c) and $df);
    #224..#253: UpCase:=chr(ord(c) and $df);
  else
    UpCase:=c;
  end;
end;
{$endif}

function LoCase(const c:char):char;
begin
  case c of
    'A'..'Z'  : LoCase:=chr(ord(c) or $20);
    #192..#221: LoCase:=chr(ord(c) or $20);
  else
    LoCase:=c;
  end;
end;

{$ENDIF}


function FileUpperCase(const s:string):string;
begin
{$IFDEF UnixFS }
  FileUpperCase := s;
{$ELSE }
  FileUpperCase := AnsiUpperCase(s);
{$ENDIF }
end;

procedure LoString(var s:string);
begin
  s:= AnsiLowerCase(s);
end;

procedure UpString(var s:string);
begin
  s:= AnsiUpperCase(s);
end;

{$ifndef FPC}

function LeftStr(const s:string; Count: Integer):string;
begin
  LeftStr := Copy(S, 1, Count);
end;

function RightStr(const s:string; Count: Integer):string;
begin
   If Count>Length(S) then
     Count:=Length(S);
   RightStr := Copy(S, 1 + Length(S) - Count, Count);
end;

{$endif}

function Mid(const s:string; const n:integer):string;
begin
  mid:=copy(s,n,length(s));
end;

function Range(const c1,c2:char):string;
var
  c : char;
begin
  result := '';
  for c:=c1 to c2 do
    Result := Result +c;
end;


function IVal(const s:string):longint;
begin
  IVal:= StrToIntDef(s,0);
end;

function RVal(const s:string):real;
var r   : real;
    res : integer;
begin
  val(trim(s),r,res);
  RVal:=r;
end;

function CVal(const s:string):longint;
begin
  if LeftStr(s,2)='0x' then     (* 0xnn = hex *)
    CVal:=HexVal(mid(s,3))
  else if FirstChar(s)='0' then (* 0nnn = oct *)
    CVal:=OctVal(mid(s,2))
  else                          (* nnnn = dec *)
    CVal:=IVal(s);
end;

function progname: TFilename;
var s : TFilename;
    p : integer;
begin
  s:= ExtractFileName(ParamStr(0));
  p:= RightPos('.', s);
  if p>0 then
    SetLength(s,p-1);
  ProgName:= s;
end;


function progpath: TFilename;
begin
  ProgPath:= ExtractFilePath(OpenXPEXEPath);
end;


function Hex(const l:integer; const n:integer):string;
begin
  Hex:= IntToHex(l, n);
end;

function HexVal(const s:string):longint;
var l   : longint;
    res : integer;
begin
  val('$'+trim(s),l,res);
  if res=0 then HexVal:=l
  else HexVal:=0;
end;


function Bin(l:longint; n:integer):string;
var s : string;
    i : integer;
begin
  s:='';
  for i:=1 to n do begin
    if odd(l) then s:='1'+s
    else s:='0'+s;
    l:=l shr 1;
    end;
  bin:=s;
end;


function FileName(var f):string;
var
  i : integer;
begin
  i := 0; Result := '';
  while tfilerec(f).name[i] <> #0 do
  begin
    Result := Result + char(tfilerec(f).name[i]);
    inc(i);
  end;
end;

function iif(b:boolean; l1,l2:longint):longint;
begin
  if b then iif:=l1
  else iif:=l2;
end;


function iifb(b,b1,b2:boolean):boolean;
begin
  if b then iifb:=b1
  else iifb:=b2;
end;


function iifc(b:boolean; c1,c2:char):char;
begin
  if b then iifc:=c1
  else iifc:=c2;
end;


function iifr(b:boolean; r1,r2:real):real;
begin
  if b then iifr:=r1
  else iifr:=r2;
end;


function iifs(b:boolean; const s1,s2:string):string;
begin
  if b then iifs:=s1
  else iifs:=s2;
end;

function IsNaN(d:double):boolean;
begin
  result:=((Ord((PChar(@d)+6)^) and $F0)=$F0)
      and ((Ord((PChar(@d)+7)^) and $7F)=$7F)
end;

function NaN:Double;
const NaNBits: array[0..7] of byte=($FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F);
begin
  Move(NaNBits,Result,8);
end;

procedure DeleteFirstChar(var s:string);
begin
  Delete(s,1,1);
end;

procedure DeleteLastChar(var s:string);
begin
  if s<>'' then SetLength(s, Length(s)-1);
end;

function posn(const s1,s2:string; n:integer):integer;
begin
  if pos(s1,mid(s2,n))=0 then PosN:=0
  else PosN:=pos(s1,mid(s2,n))+n-1;
end;


function center(const s:string; n:integer):string;
begin
  if length(s)>=n-1 then center:=LeftStr(s,n)
  else center:=sp((n-length(s))div 2)+s+sp((n-length(s)-1)div 2);
end;


function reverse(const s:string):string;
var i,l: integer;
    r: string;
begin
  l:= Length(s);
  SetLength(r, l);
  for i:= 1 to l do
    r[i]:= s[l+1-i];
  reverse:= r;
end;

function TopStr(const s:string):string;
begin
  if s='' then TopStr:=''
  else TopStr:=UpCase(s[1])+LowerCase(mid(s,2));
end;


{$IFNDEF Win32}

function topallstr(s:string):string;
var top : boolean;
    p   : integer;
begin
  p:=1; top:=true;
  while p<=length(s) do begin
    if (s[p]>='A') and (s[p]<='Z') or (s[p]='é') or (s[p]='ô') or (s[p]='ö') then
      if top then top:=false
      else s[p]:=LoCase(s[p])
    else
      if ((s[p]<'a') or (s[p]>'z')) and (s[p]<>'Ñ') and (s[p]<>'î') and (s[p]<>'Å')
      then
        top:=true;
    inc(p);
    end;
  topallstr:=s;
end;

{$ELSE}

function topallstr(s:string):string;
var top : boolean;
    p   : integer;
begin
  p:=1; top:=true;
  while p<=length(s) do begin
    if (s[p]>='A') and (s[p]<='Z') or (s[p]>=#192) and (s[p]<=#221) then
      if top then top:=false
      else s[p]:=LoCase(s[p])
    else
      if ((s[p]<'a') or (s[p]>'z')) and ((s[p]<#224) or (s[p]>#253))
      then
        top:=true;
    inc(p);
    end;
  topallstr:=s;
end;

{$ENDIF}

procedure TrimFirstChar(var s: String; c: Char);
begin
  if (s <> '') and (s[1] = c) then Delete(s, 1, 1);
end;

procedure TrimLastChar(var s: String; c: Char);   
begin
  if (s <> '') and (s[Length(s)] = c) then SetLength(s, Length(s)-1);
end;


function fitpath(path:TFilename; n:integer):TFilename;
var dir  : TFilename;
    l    : integer;
begin
  l:= Length(path);
  if (l<n) or (l<4) then
    fitpath:= path
  else begin
    dir:= ExtractFilePath(path);
    l:= Length(dir);
    if l>4 then begin
      Delete(dir,l-4,4);
      fitpath:= dir+'...'+DirSepa+ExtractFileName(path);
    end else
      fitpath:= '...'+DirSepa+ExtractFileName(path);
  end;
end;


function MultiPos(const s1,s2:string):boolean;
var i  : integer;
    mp : boolean;
begin
  mp:=false; i:=1;
  while not mp and (i<=length(s1)) do begin
    mp:=(cpos(s1[i],s2)>0);
    inc(i);
  end;
  MultiPos:=mp;
end;

function QSum(const s:string):longint;             { Quersumme }
var l : longint;
    i : integer;
begin
  l:=0;
  for i:=1 to length(s) do
    inc(l,ord(s[i]));
  qsum:=l;
end;

function IntQSum(const l:longint):longint;         { Longint-Quersumme }
begin
  if l=0 then IntQSum:=0
  else IntQSum:=l mod 10 + IntQSum(l div 10);
end;

function Without(const s1,s2:string):string;       { Strings "subtrahieren"  }
var p,i : integer;
begin
  Result := s1;
  for i:=1 to length(s2) do
    repeat
      p:=cpos(s2[i],Result);
      if p>0 then delete(Result,p,1);
    until p=0;
end;

function FirstChar(const s:string):char;
begin
  if s = '' then
    FirstChar := #0
  else
    FirstChar := s[1];
end;

function Lastchar(const s:string):char;
begin
  if s = '' then
    LastChar := #0
  else
    LastChar := s[Length(s)];
end;

function Blankpos(const s:string):integer;        { Position von ' ' oder #9     }
var p1,p2 : integer;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9,s);
  if p1=0 then blankpos:=p2
  else if p2=0 then blankpos:=p1
  else blankpos:=min(cpos(' ',s),cpos(#9,s));
end;


function BlankposX(const s:string):integer;       { length(s)+1, falls bp=0      }
var p : integer;
begin
  p:=blankpos(s);
  if p>0 then
    BlankposX:=p
  else
    BlankposX:=length(s)+1;
end;


Procedure TruncStr(var s:string; n:integer);    { String kuerzen                }
begin
  if length(s)>n then
    SetLength(s,n);
end;


Procedure incr(var r1:real; r2:real);
begin
  r1:=r1+r2;
end;


function hbar(const len:integer):string;
begin
  hbar:='√'+dup(len-2,'ƒ')+'¥';
end;


Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : integer;
begin
  p:=pos(s1,s);
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;


function TimeDiff(t1,t2:DateTimeSt):longint;    { Abstand in Sekunden  }

  function TimeSecs(var t:DateTimeSt):longint;
  begin
    TimeSecs:=3600*ival(LeftStr(t,2))+60*ival(copy(t,4,2))+ival(RightStr(t,2));
  end;

begin
  if t1<=t2 then
    TimeDiff:=0
  else
    TimeDiff:=TimeSecs(t1)-TimeSecs(t2);
end;


function isnum(const s:string):boolean;            { s besteht aus [0..9] }
var i : integer;
begin
  if s='' then
    isnum:=false
  else begin
    i:=1;
    while (i<=length(s)) and (s[i]>='0') and (s[i]<='9') do
      inc(i);
    isnum:=(i>length(s));
    end;
end;


function RightPos(c:char; const s:string):integer;    { Pos von rechts }
var p : integer;
begin
  p:=length(s);
  while (p>0) and (s[p]<>c) do dec(p);
  RightPos:=p;
end;


function Stricmp(const s1,s2:string):boolean;      { UStr-Vergleich }
begin
  Stricmp:=(UpperCase(s1) = UpperCase(s2));
end;


function OctVal(s:string):longint;     { Oktalstring -> Logint }
var l   : longint;
    n   : integer;
    sgn : boolean;
begin
  s:=trim(s);
  sgn:=(firstchar(s)='-');
  if sgn then DeleteFirstChar(s);
  l:=0;
  for n:=1 to length(s) do
    l:=(l shl 3) + ord(s[n]) - $30;
  if l>=0 then OctVal:=iif(sgn,-l,l)
  else OctVal:=0;
end;


function CPosX(c:char; const  s:string):integer;   { pos=0 -> pos:=length(s)+1 }
var p : integer;
begin
  p:=cpos(c,s);
  if p=0 then CPosX:=length(s)+1
  else CPosX:=p;
end;


{ erstes durch 'delimiter' abgegrenztes Wort aus s extrahieren }

function GetToken(var s:string; delimiter:string):string;
var p : integer;
begin
  if delimiter=' ' then begin
    s:=trim(s);
    p:=blankposx(s);
    GetToken:=LeftStr(s,p-1);
    delete(s,1,p);
    s:=TrimLeft(s);
    end
  else begin
    p:=posx(delimiter,s);
    GetToken:=trim(LeftStr(s,p-1));
    s:=trim(mid(s,p+length(delimiter)));
    end;
end;

function GetTokenC(var s:string; delim_chars:string):string;
var
  i: integer;
begin
  i := 1;
  while i <= Length(s) do
  begin
    if cpos(s[i],delim_chars)>0 then begin
      Result:=LeftStr(s,i-1);
      while cpos(s[i],delim_chars)>0 do i:=i+1;
      s:=mid(s,i);
      exit;
    end;
    Inc(i);
  end;
  result:=s;
  s:='';
end;

function PosX(const s1,s2:string):integer;            { length(s)+1, falls pos=0 }
var p : integer;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;

function SMatch(const s1,s2:string):integer;          { Anzahl der uebereinst. Bytes  }
var
  ml: Integer;
begin
  Result :=1;
  ml := min(length(s1),length(s2));
  while (Result < ml) and (s1[Result] = s2[Result]) do
    inc(Result);
  Dec(Result);
end;


function SiMatch(const s1,s2:string):integer;         { dto., ignore case }
var p,ml : integer;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (UpCase(s1[p+1])=UpCase(s2[p+1])) do
    inc(p);
  SiMatch:=p;
end;


{ Mailadresse (mit @ in der Mitte) in einem String erkennen und ausschneiden }
{ Ist Reverse = true, dann wird aus s die Mailadresse ausgeschnitten }
function mailstring(s: String; Reverse: Boolean): string;
const
  WrongChar: set of Char = ['.', '_', '*'];
  ForbiddenChar: set of Char=['(', ')', '<', '>', ',', ';', ':', '\', '[', ']',' '];
var
  i, j: integer;
begin
  i := CPos('@',s);                              {Ists ne Mailadresse ?}
  if i <> 0 then
  begin
    while (i > 0) and (s[i] > ' ') and (s[i] < chr(128)) and
     not (s[i] in forbiddenChar) do dec(i);   { Anfang suchen... }
    repeat
      inc(i);
    until not (s[i] in WrongChar);            { '.-_' sind am Anfang ungueltig }

    j := i;
    while (j <= Length(s)) and (s[j] > ' ') and (s[j] < chr(128)) and
     not (s[j] in forbiddenChar) do Inc(j);  {Ende suchen...}
    repeat
      dec(j);
    until not (s[j] in WrongChar);                    {.-_ sind am Ende ungueltig}

    if Reverse then
    begin
      Delete(s, i, j-i + 1); { eMail aus s loeschen }
      MailString := s;
    end else
      MailString := copy(s, i, j-i+1);
  end else
    MailString:=s;
end;

function CreditCardOk(s:string):boolean;   { Kreditkartennummer ueberpruefen }
const cntab : array['0'..'9'] of byte = (0,2,4,6,8,1,3,5,7,9);
var i,sum : integer;
begin
  i:=1;
  while i<=length(s) do
    if (s[i]<'0') or (s[i]>'9') then
      delete(s,i,1)
    else
      inc(i);
  sum:=0;
  for i:=1 to length(s) do
    if odd(length(s)+1-i) then inc(sum,ord(s[i])-48)
    else inc(sum,cntab[s[i]]);
  CreditCardOk:=(sum mod 10=0);
end;


function rforms(const s:string; const n:integer):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=RightStr(s,n)
  else
    rforms:=sp(n-length(s))+s;
end;

procedure UkonvStr(var s:string;len:integer);
var s2 : string;
  procedure conv(c1,c2:char);
  var p : integer;
     c3 : char;
   begin
    repeat
      p:=cpos(c1,s2);
      if p>0 then begin
        s2[p]:=c2;
        if (c2<>'e') and (c2<>'E') then   {bei 'Ç' nur ein Zeichen ersetzen}
        begin
          if c2='s' then c3:=c2        {Ansonsten: ae,ue,oe,ss}
          else c3:='e';
          insert(c3,s2,p+1);
          end;
        end;
    until p=0;
  end;
begin
  s2:=s;
  conv('Ñ','a');
  conv('î','o');
  conv('Å','u');
  conv('·','s');
  conv('é','A');
  conv('ô','O');
  conv('ö','U');
  conv('ê','E');
  conv('Ç','e');
  s:=LeftStr(s2,len);   { Bugfix... Umlautstring darf maximal Orignalstringlaenge haben }
end;

function b30(l:longint):string;   { 30bit -> 5char }
const
  b64: array[0..63] of char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$abcdefghijklmnopqrstuvwxyz+';
var
  bc : string[5];
  i : byte;
begin
  bc := '-----';
  for i:=5 downto 1 do begin
    bc[i]:=b64[l and 63];
    l:=l shr 6;
    end;
  b30:=bc;
end;

function DecodeRot13String(const s: String): String;
var
  i: Integer;
  c: Char;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
  begin
    c := s[i];
    if (c >= 'A') and (c <= 'Z') then
    begin
      Inc(c, 13);
      if c > 'Z' then Dec(c, 26);
    end else
      if (c >= 'a') and (c <= 'z') then
      begin
        Inc(c, 13);
        if c > 'z' then Dec(c, 26);
      end;
    Result[i] := c;
  end;
end;

{ ROT13 Kodierung }
procedure Rot13(var data; Size: Integer);
var
  i: Integer;
  c: Char;
begin
  for i := 0 to size - 1 do
  begin
    c := TCharArray(data)[i];
    if (c >= 'A') and (c <= 'Z') then
    begin
      Inc(c, 13);
      if c > 'Z' then Dec(c, 26);
    end else
      if (c >= 'a') and (c <= 'z') then
      begin
        Inc(c, 13);
        if c > 'z' then Dec(c, 26);
      end;
    TCharArray(data)[i] := c;
  end;
end;

function IsoToIbm(const s:string): String;
var
  i : integer;
begin
  IsoToIBM := s;
  for i:=1 to length(s) do
    if (s[i]>=#128) then
      IsoToIBM[i] := chr(iso2ibmtab[byte(s[i])])
end;

procedure Iso1ToIBM(var data; size: Integer);
var
  i : integer;
begin
  for i:=0 to size-1 do
    if (TByteArray(data)[i]>=128) then
      TByteArray(data)[i] := iso2ibmtab[TByteArray(data)[i]];
end;

function IBMToISO(const s: String): String;
var
  i: Integer;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    Result[i] := Char(IBM2ISOTab[byte(s[i])]);
end;


procedure Mac2IBM(var data; size: LongWord);
var
  i : integer;
begin
  for i:=0 to size-1 do
    if (TByteArray(data)[i]>=128) then
      TByteArray(data)[i] := mac2ibmtab[TByteArray(data)[i]];
end;


function ConvertFileName(const s:string): String;
begin
  {$IFDEF Win32 }
    ConvertFileName := ISOToIBM(s);
  {$ELSE }
    ConvertFileName := s;
  {$ENDIF }
end;

procedure ZtoZCdatumNTZ(var d1,d2:string);
begin
  if ival(LeftStr(d1,2))<70 then d2:='20'+d1+'00W+0'
  else d2:='19'+d1+'00W+0';
end;

{ functions to convert from/to MSB and LSB }

Function Swap16(X : Word) : Word; {$IFNDEF Delphi} inline; {$ENDIF }
Begin
  result:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap32(X: DWord): DWord; {$IFNDEF Delphi} inline; {$ENDIF }
Begin
  result:=(x and $ff) shl 24 + (x and $ff00) shl 8 + (x and $ff0000) shr 8 + (x and LongWord($ff000000)) shr 24;
End;

{$IFDEF LittleEndian}
function HostToLittleEndian16(host:smallword):smallword; begin result:=host; end;
function LittleEndianToHost16(host:smallword):smallword; begin result:=host; end;
function HostToLittleEndian32(host:    dword):    dword; begin result:=host; end;
function LittleEndianToHost32(host:    dword):    dword; begin result:=host; end;

function HostToBigEndian16(host:smallword):smallword; begin result:=swap16(host); end;
function BigEndianToHost16(host:smallword):smallword; begin result:=swap16(host); end;
function HostToBigEndian32(host:    dword):    dword; begin result:=swap32(host); end;
function BigEndianToHost32(host:    dword):    dword; begin result:=swap32(host); end;
{$ELSE}
{$IFDEF BigEndian}
function HostToBigEndian16(host:smallword):smallword; begin result:=host; end;
function BigEndianToHost16(host:smallword):smallword; begin result:=host; end;
function HostToBigEndian32(host:    dword):    dword; begin result:=host; end;
function BigEndianToHost32(host:    dword):    dword; begin result:=host; end;

function HostToLittleEndian16(host:smallword):smallword; begin result:=swap16(host); end;
function LittleEndianToHost16(host:smallword):smallword; begin result:=swap16(host); end;
function HostToLittleEndian32(host:    dword):    dword; begin result:=swap32(host); end;
function LittleEndianToHost32(host:    dword):    dword; begin result:=swap32(host); end;
{$ELSE}
   !! Non-supported byte order or byte order not set in xpdefine.inc
{$ENDIF}
{$ENDIF}

function StringListToString(SL: TStringList): String;
var i: Integer;
begin
  result:='';
  for i:=0 to SL.Count-1 do
    result:=result+SL[i]+' ';
  DeleteLastChar(result);
end;

{$IFDEF FPC }
function ExcludeTrailingPathDelimiter(const s: String): String;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim);
end;
{$ENDIF }

function FindURL(s: String; var x, y: Integer): Boolean; overload;
const
  urlchars: set of char=['a'..'z','A'..'Z','0'..'9','.',':',';','/','~','?',
    '-','_','#','=','&','%','@','$',',','+'];
var
  u: string;
begin
  u := UpperCase(s);
  x := Pos('HTTP://',u); {WWW URL ?}
  if x=0 then x:=Pos('HTTPS://',u);  {HTTPS URL ?}
  if x=0 then x:=Pos('GOPHER://',u); {Gopher URL ?}
  if x=0 then x:=Pos('FTP://',u);    {oder FTP ?}
  if x=0 then x:=Pos('WWW.',u);      {oder WWW URL ohne HTTP:? }
  if x=0 then x:=Pos('HOME.',u);     {oder WWW URL ohne HTTP:? }
  if x=0 then x:=Pos('FTP.',u);      {oder FTP URL ohne HTTP:? }
  if x=0 then x:=Pos('URL:',u);      {oder explizit mark. URL? }
  if x=0 then x:=Pos('URN:',u);      {oder explizit mark. URN? }
  if x=0 then x:=Pos('URI:',u);      {oder explizit mark. URL? }
  if x=0 then x:=Pos('MAILTO:',u);   {oder MAILTO: ?}

  y:=x;
  Result := x <> 0; s := s + ' ';
  if Result then
  begin
    while (y<=length(s)) and (s[y] in urlchars) do
    begin
      // "," is a valid url char, but test for things like
      // "see on http:///www.openxp.de, where" ...
      // in this case, "," does not belong to the url
      if ((s[y] = ',') or (s[y] = '.')) and (y<Length(s)) and (not (s[y+1] in urlchars)) then
        break;
      inc(y); {Ende der URL suchen...}
    end;
  end;
end;


end.

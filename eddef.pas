{   $Id$

    OpenXP editor defines unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

{$I XPDEFINE.INC }

{ OpenXP editor defines unit }
unit eddef;

interface

uses xpglobal,keys;

type   ECB     = pointer;

const  EditfLeft        = 1;          { Cursor links                   }
       EditfRight       = 2;          { Cursor rechts                  }
       EditfUp          = 3;          { Cursor oben                    }
       EditfDown        = 4;          { Cursor unten                   }
       EditfPgUp        = 5;          { Seite nach oben                }
       EditfPgDn        = 6;          { Seite nach unten               }
       EditfWordLeft    = 7;          { Wort links                     }
       EditfWordRight   = 8;          { Wort rechts                    }
       EditfTop         = 9;          { Textanfang                     }
       EditfBottom      = 10;         { Textende                       }
       EditfPageTop     = 11;         { 1. Bildschirmzeile             }
       EditfPageBottom  = 12;         { letzte Bildschirmzeile         }
       EditfEOL         = 13;         { Zeilenende                     }
       EditfBOL         = 14;         { Zeilenanfang                   }
       EditfNextPara    = 15;         { Beginn nÑchster Absatz         }
       EditfPrevPara    = 16;         { Vorausgehender Absatzbeginn    }
       EditfScrollUp    = 17;         { Bild eine Zeile hochscrollen   }
       EditfScrollDown  = 18;         { Bild eine Zeile nach unten     }

       EditfMark1       = 30;         { Marke 1 setzen                 }
       EditfMark2       = 31;         { Marke 2 setzen                 }
       EditfMark3       = 32;         { Marke 3 setzen                 }
       EditfMark4       = 33;         { Marke 4 setzen                 }
       EditfMark5       = 34;         { Marke 5 setzen                 }
       EditfGoto1       = 35;         { Sprung zu Marke 1              }
       EditfGoto2       = 36;         { Sprung zu Marke 2              }
       EditfGoto3       = 37;         { Sprung zu Marke 3              }
       EditfGoto4       = 38;         { Sprung zu Marke 4              }
       EditfGoto5       = 39;         { Sprung zu Marke 5              }
       EditfLastpos     = 40;         { Ctrl-Q-P                       }
       EditfGotoBStart  = 41;         { Blockanfang anspringen         }
       EditfGotoBEnd    = 42;         { Blockende anspringen           }

       EditfBS          = 50;         { Zeichen links lîschen          }
       EditfDEL         = 51;         { Zeichen unter Cursor lîschen   }
       EditfDelWordRght = 52;         { Wort rechts lîschen            }
       EditfDelWordLeft = 53;         { Wort links lîschen             }
       EditfDelLine     = 54;         { Zeile lîschen                  }
       EditfDelBlock    = 55;         { markierten Block lîschen       }
       EditfBlockBegin  = 56;         { Blockbeginn setzen             }
       EditfBlockEnd    = 57;         { Blockende setzen               }
       EditfCopyBlock   = 58;         { Kopie an Cursorposition        }
       EditfMoveBlock   = 59;         { verschieben an Cursorposition  }
       EditfCutBlock    = 60;         { in Clipboard ausschneiden      }
       EditfCCopyBlock  = 61;         { in Clipboard kopieren          }
       EditfPasteBlock  = 62;         { aus Clipboard einfÅgen         }
       EditfWriteBlock  = 63;         { Block in Datei schreiben       }
       EditfReadBlock   = 64;         { Block aus Datei einlesen       }
       EditfMarkWord    = 65;         { Wort markieren                 }
       EditfMarkLine    = 66;         { Zeile markieren                }
       EditfMarkPara    = 67;         { Absatz markieren               }
       EditfMarkAll     = 68;         { ganzen Text markieren          }
       EditfNewline     = 69;         { Enter                          }
       EditfTAB         = 70;         { TAB-Sprung                     }
       EditfUndelete    = 71;         { Undelete                       }
       EditfHideBlock   = 72;         { Blockmarkierung abschalten     }
       EditfReformat    = 73;         { Block reformatieren            }
       EditfDelToEOF    = 74;         { alles ab Cursorposition lîschen }
       EditfRot13       = 75;         { Block Rot13-codieren           }
       EditfPrint       = 76;         { Block ausdrucken               }
       EditfDeltoEnd    = 77;         { Lîschen bis Absatzende         }
       EditfParagraph   = 78;         { ^P^U                           }
       EditfChangeCase  = 79;         { Alt-3                          }
       EditfReadUUeBlock= 80;         { Block aus Datei einlesen & UU-Encode }
       EditfFormatBlock = 81;         { Block reformatieren            }

       EditfFind        = 100;        { Suchen                         }
       EditfFindReplace = 101;        { Suchen + Ersetzen              }
       EditfFindRepeat  = 102;        { wiederholen (^L)               }
       EditfCtrlPrefix  = 103;        { Steuerzeichen-PrÑfix           }
       EditfWrapOn      = 104;        { Absatzumbruch einschalten      }
       EditfWrapOff     = 105;        { Absatzumbruch ausschalten      }
       EditfAllwrapOn   = 106;        { Umbruch fÅr ganzen Text ein    }
       EditfAllwrapOff  = 107;        { Umbruch fÅr ganzen Text aus    }
       EditfSetMargin   = 108;        { rechten Rand einstellen        }
       EditfText        = 109;        { *** Zeicheneingabe ***         }
       EditfChangeInsert= 110;        { EinfÅgemodus umschalten        }
       EditfAbsatzmarke = 111;        { #20 ein/ausschalten            }
       EditfRestorePara = 112;        { énderungen rÅckgÑngig machen   }
       EditfChangeIndent= 113;        { EinrÅcken umschalten           }

       EditfMenu        = 120;        { F10 - lokales MenÅ             }
       EditfSetup       = 121;        { Einstellungen                  }
       EditfSaveSetup   = 122;        { Einstellungen speichern        }
       EditfSave        = 123;        { Speichern                      }
       EditfBreak       = 124;        { Abbruch                        }
       EditfSaveQuit    = 125;        { Speichern + Ende               }
       EditfGlossary    = 126;        { Kuerzelmacros mit STRG+ENTER   }

       MaxFindLen       = 30;
       EditMenuMps      = 11;

       QuoteCharSet : set of char = [':','|']; { Weitere Quotezeichen }

       drBoth = true; // Both search Directions in GotoPos
       drForward = false; // Only Forward search

type   EdColrec = record
                    coltext,colstatus,colmarked,
                    colendmark                   : byte;
                    colquote                     : array[1..9] of byte;
                    colmenu,colmenuhi,colmenuinv,
                    colmenuhiinv                 : byte;
                  end;

       LangData = record
                    zeile,spalte : string[8];
                    ja,nein      : char;
                    errors       : array[1..6] of string[30];
                    askquit      : string[30]; { 'GeÑnderten Text speichern' }
                    askoverwrite : string[50]; { 'Datei existiert schon - Åberschreiben' }
                    askreplace   : string[40]; { 'Text ersetzen (Ja/Nein/Alle/Esc)' }
                    replacechr   : string[3];  { 'JNA' }
                    ersetzt      : string[30]; { ' Textstellen ersetzt' }
                    drucken      : string[15]; { 'Drucken ...' }
                    menue        : array[0..editmenumps] of string[20];
                  end;
       LdataPtr = ^LangData;

       EdConfig = record
                    absatzendezeichen : char;
                    rechter_rand      : word;
                    AutoIndent        : boolean;
                    PersistentBlocks  : boolean;
                    QuoteReflow       : boolean;
                  end;

       EdAskQuit   = function(ed:ECB):taste;  { J/N/Esc }
       EdAskOverwrite = function(ed:ECB; fn:string):taste;
       EdMessage   = procedure(txt:string; error:boolean);   { Meldung anzeigen }
       EdAskFile   = procedure(ed:ECB; var fn:string; save,uuenc:boolean);  { Dateinameneingabe }
       EdFindPanel = function(ed:ECB; var txt:string; var igcase:boolean):boolean;
       EdReplPanel = function(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
       EdConfigPanel = procedure(var cfg:EdConfig; var brk:boolean);

       EdProcs  = record
                    QuitFunc  : EdAskQuit;         { Frage bei Programmende }
                    Overwrite : EdAskOverwrite;    { Datei Åberschreiben?   }
                    MsgProc   : EdMessage;         { Meldung/Fehler         }
                    FileProc  : EdAskFile;         { Dateiname abfragen     }
                    FindFunc  : EdFindPanel;       { Such-Dialog            }
                    ReplFunc  : EdReplPanel;       { Ersetze-Dialog         }
                    CfgFunc   : EdConfigPanel;     { Config-Dialog          }
                  end;



implementation

end.
{
  $Log$
  Revision 1.13  2001/09/08 09:51:31  mk
  - quote reflow reimplmented

  Revision 1.12  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.11  2001/02/25 15:25:22  ma
  - added GPL headers
  - cosmetics

  Revision 1.10  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.9  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.8  2000/10/15 15:20:17  mk
  JG:- Editor Funktion Glossary implementiert

  Revision 1.7  2000/07/05 17:10:53  mk
  - AnsiString Updates

  Revision 1.6  2000/06/23 15:59:09  mk
  - 16 Bit Teile entfernt

  Revision 1.5  2000/03/17 21:22:10  rb
  vActAbs entfernt, erster Teil von 'Blîcke reformatieren' (<Ctrl K><F>)

  Revision 1.4  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}

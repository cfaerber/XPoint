{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2002 OpenXP-Team, http://www.openxp.de                 }
{ (c) 2002-2003 OpenXP/16, http://www.openxp16.de                 }
{ See list of contributors in authors.txt                         }
{                                                                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ OpenXP ist eine eingetragene Marke von Markus Kaemmerer.        }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Deklarationen f�r Unit EDITOR }

unit eddef;

interface

{$I XPDEFINE.INC }
{$F+,O+}

uses xpglobal, dos,keys,lfn;


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
       EditfNextPara    = 15;         { Beginn n�chster Absatz         }
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

       EditfBS          = 50;         { Zeichen links l�schen          }
       EditfDEL         = 51;         { Zeichen unter Cursor l�schen   }
       EditfDelWordRght = 52;         { Wort rechts l�schen            }
       EditfDelWordLeft = 53;         { Wort links l�schen             }
       EditfDelLine     = 54;         { Zeile l�schen                  }
       EditfDelBlock    = 55;         { markierten Block l�schen       }
       EditfBlockBegin  = 56;         { Blockbeginn setzen             }
       EditfBlockEnd    = 57;         { Blockende setzen               }
       EditfCopyBlock   = 58;         { Kopie an Cursorposition        }
       EditfMoveBlock   = 59;         { verschieben an Cursorposition  }
       EditfCutBlock    = 60;         { in Clipboard ausschneiden      }
       EditfCCopyBlock  = 61;         { in Clipboard kopieren          }
       EditfPasteBlock  = 62;         { aus Clipboard einf�gen         }
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
       EditfDelToEOF    = 74;         { alles ab Cursorposition l�schen }
       EditfRot13       = 75;         { Block Rot13-codieren           }
       EditfPrint       = 76;         { Block ausdrucken               }
       EditfDeltoEnd    = 77;         { L�schen bis Absatzende         }
       EditfParagraph   = 78;         { ^P^U                           }
       EditfChangeCase  = 79;         { Alt-3                          }
       EditfReadUUeBlock= 80;         { Block aus Datei einlesen & UU-Encode }
       EditfFormatBlock = 81;         { Block reformatieren            }

       EditfFind        = 100;        { Suchen                         }
       EditfFindReplace = 101;        { Suchen + Ersetzen              }
       EditfFindRepeat  = 102;        { wiederholen (^L)               }
       EditfCtrlPrefix  = 103;        { Steuerzeichen-Pr�fix           }
       EditfWrapOn      = 104;        { Absatzumbruch einschalten      }
       EditfWrapOff     = 105;        { Absatzumbruch ausschalten      }
       EditfAllwrapOn   = 106;        { Umbruch f�r ganzen Text ein    }
       EditfAllwrapOff  = 107;        { Umbruch f�r ganzen Text aus    }
       EditfSetMargin   = 108;        { rechten Rand einstellen        }
       EditfText        = 109;        { *** Zeicheneingabe ***         }
       EditfChangeInsert= 110;        { Einf�gemodus umschalten        }
       EditfAbsatzmarke = 111;        { #20 ein/ausschalten            }
       EditfRestorePara = 112;        { �nderungen r�ckg�ngig machen   }
       EditfChangeIndent= 113;        { Einr�cken umschalten           }

       EditfMenu        = 120;        { F10 - lokales Men�             }
       EditfSetup       = 121;        { Einstellungen                  }
       EditfSaveSetup   = 122;        { Einstellungen speichern        }
       EditfSave        = 123;        { Speichern                      }
       EditfBreak       = 124;        { Abbruch                        }
       EditfSaveQuit    = 125;        { Speichern + Ende               }
       EditfGlossary    = 126;        { K�rzelmakros mit <Ctrl-Enter>  }
       EditfAddMime     = 127;        { MIME-Datei anh�ngen            }

       MaxFindLen       = 30;
       EditMenuMps      = 19;
       
       QuoteCharSet : set of char = [':','|']; { Weitere Quotezeichen }

       drBoth = true; { Both search Directions in GotoPos }
       drForward = false; { Only Forward search }


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
                    errors       : array[1..7] of string[70];
                    askquit      : string[30]; { 'Ge�nderten Text speichern' }
                    askoverwrite : string[50]; { 'Datei existiert schon - �berschreiben' }
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
                    Overwrite : EdAskOverwrite;    { Datei �berschreiben?   }
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
  Revision 1.5.2.7  2003/05/01 14:22:30  mk
  - updated copyright headers

  Revision 1.5.2.6  2002/04/19 16:38:05  my
  JG[+MY]: MIME-Multipart-Versand (RFC/ZConnect) implementiert :-):
           OpenXP/16 kann jetzt standardkonforme MIME-Multipart-Nachrich-
           ten erzeugen und versenden. Es k�nnen sowohl im Sendefenster
           als auch direkt im Editor (!) Dateien und Textteile beliebiger
           Anzahl und Gr��e an die aktuelle Nachricht angeh�ngt werden.
           Die �nderung der Reihenfolge bereits angeh�ngter Nachrichten-
           teile ist m�glich, das Weiterleiten von MIME-Multipart-
           Nachrichten mittels N/W/K, N/W/O, N/W/E und N/W/R wird jetzt
           ebenfalls unterst�tzt. Weitere Details siehe Hilfe (?/S/A).
           Kompletter Sourcecode f�r XP entwickelt von JG, Anpassungen
           an und Einbau in OpenXP/16 durch MY.
           Spezieller Dank an HH f�r die Vorarbeit im Rahmen der
           Entwicklung des XP-Tools XPBMIME, dessen Arbeitsweise teilweise
           als Ansto� und Vorlage f�r die aktuelle XP-Implementation
           diente, sowie an JM f�r seine Mitarbeit daran, speziell im
           Bereich Zeichensatzbehandlung und ZConnect-Konformit�t.

  Revision 1.5.2.5  2001/09/16 20:35:49  my
  JG+MY:- Editor-Men� (<F10>) erweitert: "Suchen/Ersetzen/Weitersuchen"
          und "Beenden" hinzugef�gt

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.5.2.4  2001/09/06 16:15:58  mk
  - optimized GotoPos

  Revision 1.5.2.3  2001/07/01 15:42:12  my
  SV:- moved unit to overlay

  Revision 1.5.2.2  2000/12/12 14:03:55  mk
  - weitere lfn-fixes

  Revision 1.5.2.1  2000/07/21 17:32:26  jg
  - Editor: Glossary Funktion mit Strg+Enter bzw. Alt+G

  Revision 1.5  2000/03/17 21:22:10  rb
  vActAbs entfernt, erster Teil von 'Bl�cke reformatieren' (<Ctrl K><F>)

  Revision 1.4  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Compilerdirektiven fÅr CrossPoint (OpenXP)                      }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Wenn gesetzt, werden erweiterte Checks in der EXE-Datei durchgefÅhrt
  (Rangecheck) usw. }

{$DEFINE Debug }

{ Wenn gesetzt, werden in die TPU/TPP Debug Informationen gespeichert
  und es kann eine ausfÅhrliche Map-Datei erstellt werden. éndert den
  Code der EXE-Datei nicht }

{$DEFINE DebugInfo }

{ Keine Assembler-Routine verwenden. Ein Gegenpart in Pascal mu·
  existieren }
{.$DEFINE NOASM }

{ Ist definiert, wenn Beta-Informationen anzeigt werden sollen }
{.$DEFINE Beta }

{ Wenn dieser Schalter nicht gesetzt ist, benîtigt XP eine 386er mit
  CoProzessor, ansonsten ist es auch auf XT aufwÑrts lauffÑhig }
{.$DEFINE NO386 }

{ Wenn dieser Schalter definiert ist, wird eine Version mit CAPI-
  UnterstÅzung compiliert }
{.$DEFINE CAPI }

{ --- ab hier sind keine énderungen nîtig --- }

{$IFDEF VER70 }
  {$DEFINE BP }   { 16 Bit Borland Pascal 7 oder 8 (Delphi 1) }
  {$IFDEF DPMI }
    {$DEFINE DPMI16 }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER80 }
  {$DEFINE BP }   { 16 Bit Borland Pascal 7 oder 8 (Delphi 1) }
  {$IFDEF DPMI }
    {$DEFINE DPMI16 }
  {$ENDIF }
{$ENDIF }

{$IFDEF BP } { Nur fÅr Borland Pascal 7 und 8 }
  {$A+,B-,D-,E-,F-,I-,G+,L-,N-,O-,P-,Q-,R-,S-,T-,V-,X+,Y-}
  {$IFDEF NO386 }
    {$G-,E+,N+} { keine 286er Befehle, kein CoPro nîtig }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER130 }      { Delphi 5 }
  {$DEFINE Ver32 }
  {$DEFINE Win32 }
  {$DEFINE Delphi }
  {$A+,B-,C-,D+,E-,F-,G+,H-,I-,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U+,V-,W-,X+,Y-,Z1}
{$ENDIF }

{$IFDEF __CON__ } { Nur fÅr TMT Pascal >= 3.30 }
  {$DEFINE VER32 }
  {$DEFINE TMT }
  {$DEFINE DOS32 }
  {$A+,B-,D-,F-,I-,L-,O-,P-,Q-,R-,S-,T-,V-,X+}
{$ENDIF }

{$IFDEF VirtualPascal }  { Virtual Pascal ab 2.0 }
  {$DEFINE Ver32 }
  {$AlignCode+,AlignData+,AlignRec-,B-,D-,Delphi+,H-,I-,J+,Optimize+,P+,Speed+,V-,X+}
{$ENDIF }

{$IFDEF FPC }             { Freepascal >= 0.99.14 }
  {$DEFINE Ver32 }
  {$IFDEF GO32V2 }
    {$DEFINE DOS32 }
  {$ENDIF }
  {$C-,H-,I-,M-,P-,T-,V-,X+}
  {$MODE TP }
  {$ASMMODE Intel }
{$ENDIF }

{$IFDEF DebugInfo }
  {$IFDEF BP } { Nur fÅr Borland Pascal 7 }
    {$D+,L+,Y+}
  {$ENDIF }
  {$IFDEF VirtualPascal }
    {$D+,LocInfo+}
  {$ENDIF }
{$ENDIF }

{$IFDEF Debug }
  {$IFDEF BP } { Nur fÅr Borland Pascal 7 }
    {$R+,S+,Q+}
  {$ENDIF }
  {$IFDEF VirtualPascal }
    {$R+,S+,Q+,Optimize-}
  {$ENDIF }
{$ENDIF }

{$IFDEF Ver32 }
  {$IFDEF CAPI } { Umgeht hÑssliche Warnings in TMT Pascal }
    {$UNDEF CAPI } { Bis jetzt keine CAPI-UnterstÅtzung in 32 Bit Versionen }
  {$ENDIF CAPI }
{$ENDIF }

{
  $Log$
  Revision 1.11  2000/03/04 11:53:20  mk
  Version auf 3.21.022 beta geaendert und Debug eingeschaltet

  Revision 1.10  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.9  2000/02/28 08:57:05  mk
  - Version auf 3.20 RC1 geandert

  Revision 1.8  2000/02/27 09:05:39  mk
  Ab Beta 3.20.21 ohne Debug-Infos

  Revision 1.7  2000/02/20 11:06:33  mk
  Loginfos hinzugeueft, Todo-Liste geaendert

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
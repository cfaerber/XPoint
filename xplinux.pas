{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Matthias Leonhardt, http://www.openxp.de }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ XP - Linux - Supportroutinen }
{ $Id$ }

unit xplinux;

{$I XPDEFINE.INC }

{$IFNDEF Linux }
  {$FATAL Die Unit XPLINUX kann nur unter Linux compiliert werden }
{$ENDIF }

interface


implementation

     
end.
{
  $Log$
  Revision 1.6  2000/05/02 14:35:30  hd
  - Konvertierungsroutine entfernt. Die Konvertierung fuer den
    Bildschirm findet nun in XPCURSES.PAS statt.
  - Leere Unit bleibt bestehen, fuer Linux-spezifische Erweiterungen

  Revision 1.5  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.4  2000/04/09 13:27:07  ml
  Diverse Änderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.3  2000/03/26 11:04:10  ml
  zpr-Anzeige in linux geht jetzt

  Revision 1.2  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.1  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

}
{   $Id$

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

{ XP-ZConnect <-> FTS-0001 - Konvertierer }
{ (c) PM 06/92         FTS-0001, FSC-0039 }
{                                         }
{ Errorlevel:  0=ok, 1=Fehler             }

{$I XPDEFINE.INC }

program ZFido;

uses
  sysutils, classes,
  ZFTools,
  xpglobal;

procedure logo;
begin
  close(output);
  assign(output,'');
  rewrite(output);
  writeln;
  writeln('ZConnect <-> Fido - Konvertierer  (c) ''92-99 PM');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  Writeln;
end;

begin
  logo;
  halt(ZFidoMain);
end.
{
  $Log$
  Revision 1.47  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.46  2000/12/25 20:31:18  mk
  - zfido is now completly integrated

  Revision 1.45  2000/11/18 15:46:06  hd
  - Unit DOS entfernt

  Revision 1.44  2000/11/14 22:33:26  fe
  Dependencies fixed.

  Revision 1.43  2000/11/14 22:19:16  hd
  - Fido-Modul: Anpassungen an Linux

  Revision 1.42  2000/11/14 20:24:03  hd
  - Funktionen in Unit ZFTools ausgelagert
  - exist->FileExists
  - ZFido enthaelt keine Konvertierungen mehr

  Revision 1.41  2000/11/09 18:51:41  hd
  - Anpassungen an Linux

  Revision 1.40  2000/10/28 07:50:53  mo
  -ANSI-String Bug umschifft

  Revision 1.39  2000/10/17 10:06:02  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.38  2000/10/04 21:44:28  mo
  - kleine Korrektur

  Revision 1.37  2000/10/03 18:06:52  mo
  - jetzt klappt's auch wieder mit den exportieren

  Revision 1.34  2000/09/25 17:58:31  mk
  - Window ausgeklammert, da in 32 Bit Version nicht erlaubt

  Revision 1.33  2000/09/21 16:22:21  mk
  - ZFido wieder compilierbar

  Revision 1.32  2000/09/06 21:31:01  fe
  /home/fe/foo

  Revision 1.31  2000/08/08 13:18:16  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.30  2000/07/21 17:39:58  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.29  2000/07/13 10:23:48  mk
  - Zeiger auf Strings entfernt

  Revision 1.28  2000/07/11 21:39:23  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.27  2000/07/09 08:35:20  mk
  - AnsiStrings Updates

  Revision 1.26  2000/07/04 12:04:32  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.25  2000/07/04 09:59:04  mk
  - Sysutils eingefuegt

  Revision 1.24  2000/07/03 16:20:04  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.23  2000/07/02 14:24:56  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.22  2000/06/23 15:59:26  mk
  - 16 Bit Teile entfernt

  Revision 1.21  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.20  2000/06/03 09:21:09  mk
  - Programm verlaengert

  Revision 1.19  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.18  2000/05/28 20:25:17  mk
  - exch_8d laeuft jetzt auch unter 32 Bit

  Revision 1.17  2000/05/26 00:01:10  mk
  - Assembler-Fixes (32 Bit)

  Revision 1.16  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.15  2000/05/03 00:21:24  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.14  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.13  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.12  2000/04/18 11:23:52  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.11  2000/04/15 14:45:16  mk
  - Ops, noch ein paar ASM-Routinen portiert

  Revision 1.10  2000/04/15 14:26:04  mk
  - Assemblerroutinen portiert

  Revision 1.9  2000/04/15 12:30:58  mk
  - Compilierfaehigkeit mit VP wieder hergestellt

  Revision 1.8  2000/04/13 12:48:42  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.7  2000/03/16 10:14:25  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen fuer Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen Aenderung wieder

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

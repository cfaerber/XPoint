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

{ lokale Deklarationen fr XP6 und XP6O }

{$I xpdefine.inc}

unit xpsendmessage_internal;

interface

uses  sysutils,xp0,xpcc, xpglobal;


{ Tabelle fr IBM -> ISO-Konvertierung }

{     oempf  = '## Originalempf„nger:';  - 600 }
const maxcc  = 50;

      um     : array[1..7] of char = '„”™šá';

      flEB     : boolean = false;
      flMloc   : boolean = false;
      flMnet   : boolean = false;
type ccmore  = record
                 server : string[BoxNameLen];      { Server }
                 ccnt   : byte;
                 ccpm   : boolean;
                 cpanz  : shortint;  { n = erster von n Emf"ngern }
                 nobrett: boolean;   { Phantom-Crossposting }
                 encode : boolean;   { PM - Default: Codieren }
               end;
    ccmorea  = array[0..maxcc] of ccmore;   { [0]=erster Empf. }

var umlaute  : byte;        { 0=IBM; 1=ASCII; (2=ISO) }
    min_send : longint;     { minimales Sendedatum (fr "D"atum) }
    cc_anz   : integer16;   { Anzahl CC-Empf"nger }
    cc       : ccp;         { Kopie-Empf"nger }
    ccm      : ^ccmorea;

implementation


end.

{
  $Log$
  Revision 1.3  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.2  2001/08/29 19:50:47  ma
  - changes in net type handling (2)
  - shortened CVS logs

  Revision 1.1  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*
}

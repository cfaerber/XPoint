{   $Id$

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

{ lokale Deklarationen fÅr XP6 und XP6O }

{$I xpdefine.inc}

unit xpsendmessage_internal;

interface

uses  sysutils,xp0,xpcc, xpglobal;


{ Tabelle fÅr IBM -> ISO-Konvertierung }

{     oempf  = '## OriginalempfÑnger:';  - 600 }
const maxcc  = 50;

      um     : array[1..7] of char = 'ÑîÅéôö·';

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
    min_send : longint;     { minimales Sendedatum (fÅr "D"atum) }
    cc_anz   : integer;     { Anzahl CC-Empf"nger }
    cc       : ccp;         { Kopie-Empf"nger }
    ccm      : ^ccmorea;

implementation


end.


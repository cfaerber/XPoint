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

{$I xpdefine.inc}

unit xpview;

interface

uses
  sysutils, Viewer, XPGlobal;

(*
procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo); *)

implementation

uses
  database, xp0, xp1, xp1o, typeform, fileio;

(*
{ Bin„rdatei auf GIF- und ILBM-Signatur testen;  }
{ Betreff auf Dateiextension testen              }

procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo);
var f       : file;
    id      : string[80];               { Shortstring ist ausreichend }
    rr      : word;
    betreff : string;
    p       : byte;

label betreff_fn;

begin
  if not betreffname or not ntFilename(mbNetztyp) then begin
    viewer.fn:='';
    assign(f,fn);
    if existf(f) then begin
      resetfm(f,0);
      blockread(f,id[1],80,rr);
      id[0]:= chr(rr);                  { Anpassen an gelesenen Daten }
      close(f);
      end
    else
      id:='';
    betreff:= dbReadNStr(mbase,mb_betreff);
    if (LeftStr(id,3)='GIF') and SeekMime('image/gif') then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='gif';
      viewer.typ:='image/gif';
      if betreffname then goto betreff_fn;
      end
    else if (pos('ILMB',id)<=20) and SeekMime('image/iff') then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='iff';
      viewer.typ:='image/iff';
      if betreffname then goto betreff_fn;
      end
    else if betreffname then begin
      dbSetIndex(mimebase,0);
      dbGoTop(mimebase);
      while not dbEOF(mimebase) and
            ((dbReadStr(mimebase,'extension')='') or
             (pos('.'+LowerCase(dbReadStr(mimebase,'extension')),LowerCase(betreff))=0)) do
        dbNext(mimebase);
      if not dbEOF(mimebase) then begin
        viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
        viewer.ext:= dbReadNStr(mimebase,mimeb_extension);
        viewer.typ:= dbReadNStr(mimebase,mimeb_typ);
        if viewer.prog='' then viewer.prog:='*intern*';
      betreff_fn:
        p:=pos('.'+LowerCase(viewer.ext),LowerCase(betreff));
        truncstr(betreff,p+length(viewer.ext));
        while (p>1) and (betreff[p-1] in
               ['0'..'9','a'..'z','A'..'Z','-','_','.','~','$','(',')','„','”','','Ž','™','š','á']) do
          dec(p);
        delete(betreff,1,p-1);
        if validfilename(betreff) then viewer.fn:=betreff;
        end;
      dbSetIndex(mimebase,mtiTyp);
      end;
    end;
end;


procedure URep(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : Integer;
begin
  p:=pos(UpperCase(s1),UpperCase(s));
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;

*)


end.
{
  $Log$
  Revision 1.33  2001/10/11 09:00:40  mk
  - external viewer files now with correct file extension

  Revision 1.32  2001/10/10 22:04:10  mk
  - enabled use of external mime viewers again

  Revision 1.31  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.30  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.29  2000/11/18 21:42:19  mk
  - implemented new Viewer handling class TMessageViewer

  Revision 1.28  2000/11/18 15:46:05  hd
  - Unit DOS entfernt

  Revision 1.27  2000/11/14 15:51:38  mk
  - replaced Exist() with FileExists()

  Revision 1.26  2000/11/14 11:14:35  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.25  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.24  2000/10/19 15:25:07  mk
  - sstringp in AnsiString umgewandelt

  Revision 1.23  2000/10/17 10:06:02  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.22  2000/10/16 08:30:37  mk
  - Bugfix fuer ViewFile, wenn File ohne Extension

  Revision 1.21  2000/10/11 14:51:57  mk
  JG:- Bug bei Erstellung des Dateinamens behoben

  Revision 1.20  2000/07/12 12:57:40  hd
  - Ansistring

  Revision 1.19  2000/07/06 08:58:46  hd
  - AnsiString

  Revision 1.18  2000/07/04 12:04:32  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.17  2000/06/29 13:01:03  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.16  2000/05/04 10:43:01  mk
  - Unbenutze Units aus uses entnommen

  Revision 1.15  2000/03/28 05:13:23  jg
  - Externe/Viewer bei fehlender Dateiendung im Viewer Eintrag
    wird jetzt die des Original-Filenamen verwendet.

  Revision 1.14  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.13  2000/03/07 17:45:14  jg
  - Viewer: Bei Dateien mit Leerzeichen im Namen wird
    grundsaetzlich ein .tmp File erzeugt
  - Env.Variable DELVTMP setzt jetzt nur noch beim Start
    die Globale Variable DELVIEWTMP

  Revision 1.12  2000/03/05 19:46:12  jg
  - Edit/Viewer: kein neuerstellen von */* mehr moeglich.
  - Externe Viewer: Gesamtlaenge von Programmname+Dateiname beruecksichtigt

  Revision 1.11  2000/03/05 15:35:33  jg
  - Externe Windows-Viewer: abfangen ungueltiger Abkuerzungen von
    Lang-Filenamen,  "TMP-" kommt nur noch im aktiven DELVTMP Modus.
    Temp-File-Extension sollte jetzt nur noch TMP werden wenn kein
    Viewername angegeben ist.

  Revision 1.10  2000/03/05 07:23:23  jg
  - Externe Viewer: nur .TMP Dateien werden noch nach TMP-*.* Umbenannt

  Revision 1.9  2000/03/04 18:34:18  jg
  - Externe Viewer: zum Ansehen von Fileattaches wird keine Temp-Kopie
    mehr erstellt, und nicht mehr gewartet, da kein Loeschen noetig ist

  Revision 1.8  2000/03/04 11:55:28  mk
  Loginfo hinzugefuegt

}

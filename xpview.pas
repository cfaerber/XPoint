{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - BinÑrfile-Viewer }

{$I XPDEFINE.INC}

unit xpview;

interface

uses xpglobal,sysutils,dos,dosx,typeform,fileio,inout,database,xp0,xp1,xpnt;


type viewinfo = record
                  typ : string;
                  ext : string;
                  fn  : string;
                  prog: string;
                end;


procedure GetExtViewer(fn:string; var viewer:viewinfo);
procedure GetMimeViewer(typ:string; var viewer:viewinfo);
procedure GetDefaultViewer(typ:string; var viewer:viewinfo);
procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo);

procedure ViewFile(fn:string; var viewer:viewinfo; Fileattach:boolean);


implementation  { ---------------------------------------------------- }

uses xp1o;


{ anhand der Dateierweiterung passenden Viewer aus MIMETYP.DB1 suchen }

procedure GetExtViewer(fn:string; var viewer:viewinfo);
var p : byte;
begin
  viewer.prog:='';
  p:=rightpos('.',fn);
  if (p>0) and (p<length(fn)) then begin
    dbSeek(mimebase,mtiExt,UpperCase(mid(fn,p+1)));
    if dbFound then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.typ:='';
      viewer.ext:=UpperCase(mid(fn,p+1));
      viewer.fn:='';
      end;
    end;
end;


function SeekMime(typ:string):boolean;
begin
  dbSeek(mimebase,mtiTyp,UpperCase(typ));
  SeekMime:=not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(typ,dbReadStr(mimebase,'typ'));
end;


{ anhand des MIME-Typs passenden Viewer aus MIMETYP.DB1 suchen }

procedure GetMimeViewer(typ:string; var viewer:viewinfo);
var gt : string;
begin
  viewer.prog:='';
  if typ='' then exit;
  if stricmp(typ,'text/plain') then
    if PTextViewer<>nil then begin
      viewer.prog:=PTextViewer^;
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='txt';
      end
    else if DefTextViewer<>nil then begin
      viewer.prog:=DefTextViewer^;
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='';
      end
    else
  else
    if SeekMime(typ) then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:= dbReadNStr(mimebase,mimeb_extension);
      end
    else begin
      gt:=left(typ,cposx('/',typ))+'*';
      if SeekMime(gt) then begin
        viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
        if viewer.prog='' then viewer.prog:='*intern*';
        viewer.ext:= dbReadNStr(mimebase,mimeb_extension);
        end;
      end;
  if viewer.prog<>'' then begin
    viewer.typ:=typ;
    viewer.fn:='';
    end;
end;


{ Viewer fÅr */* ermitteln }

procedure GetDefaultViewer(typ:string; var viewer:viewinfo);
begin
  if DefaultViewer<>nil then begin
    viewer.prog:=DefaultViewer^;
    viewer.ext:='';
    viewer.typ:=typ;
    viewer.fn:='';
    end;
end;


{ BinÑrdatei auf GIF- und ILBM-Signatur testen;  }
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
    if (left(id,3)='GIF') and SeekMime('image/gif') then begin
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
               ['0'..'9','a'..'z','A'..'Z','-','_','.','~','$','(',')','Ñ','î','Å','é','ô','ö','·']) do
          dec(p);
        delete(betreff,1,p-1);
        if validfilename(betreff) then viewer.fn:=betreff;
        end;
      dbSetIndex(mimebase,mtiTyp);
      end;
    end;
end;


procedure URep(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : byte;
begin
  p:=pos(UpperCase(s1),UpperCase(s));
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;


procedure ViewFile(fn:string; var viewer:viewinfo; fileattach:boolean);
var p         : byte;
    prog      : string;  {Maximallaenge= Programmname+' '+Pfadstring(79)}
    orgfn,fn1,
    parfn     : string;
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
begin
  fn1:='';
  orgfn:=iifs(viewer.fn<>'',ExtractFilepath(fn)+ExtractFileName(viewer.fn),'');
  if (not ValidFileName(orgfn) or exist(orgfn)) and (viewer.ext<>'') and
     (cpos('.',fn)>0) then
    orgfn:=left(fn,rightpos('.',fn))+viewer.ext;

  if not fileattach then
  begin
  if stricmp(fn,orgfn) or not ValidFileName(orgfn) or (cpos(' ',orgfn)>0)
    then orgfn:=TempS(_filesize(fn)+5000);
    if copyfile(fn,orgfn) then fn1:=orgfn;
    end;

  prog:=viewer.prog;
  orgfn:=iifs(fn1<>'',fn1,fn);

                             {Tempdatei bei aktivem DELVTMP nach TMP-????.??? umbenennen }
  if not fileattach and delviewtmp then
  Begin
    parfn:=TempS(_filesize(fn)+5000);
    parfn:=left(parfn,length(parfn)-8)+'TMP-'+right(parfn,8);
    end
  else parfn:=orgfn;
                              {Korrekte File-extension verwenden}

  FSplit(Parfn, Dir, Name, Ext);
  ParFn := Dir + Name;

  parfn:=parfn+iifs(viewer.ext='',ExtractFileExt(Orgfn),'.' + viewer.ext);
  _rename(orgfn,parfn);


  p:=pos('$FILE',UpperCase(prog));
  if p=0 then prog:=prog+' '+parfn
  else prog:=left(prog,p-1)+parfn+mid(prog,p+5);
  urep(prog,'$TYPE',viewer.typ);
  urep(prog,'$EXT',viewer.ext);
  if not XPWinShell(prog,parfn,600,1,fileattach) then
  if not fileattach and (fn1<>'') then era(parfn);
end;

end.
{
  $Log$
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
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
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

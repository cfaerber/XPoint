{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Bin�rfile-Viewer }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpview;

interface

uses xpglobal, dos,dosx,typeform,fileio,inout,database,xp0,xp1,xpnt;


type viewinfo = record
                  typ : string[30];
                  ext : string[5];
                  fn  : string[40];
                  prog: string[ViewprogLen];
                end;


procedure GetExtViewer(fn:string; var viewer:viewinfo);
procedure GetMimeViewer(typ:string; var viewer:viewinfo);
procedure GetDefaultViewer(typ:string; var viewer:viewinfo);
procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo);

procedure ViewFile(fn:string; var viewer:viewinfo; fileattach:boolean);


implementation  { ---------------------------------------------------- }

uses xp1o,xp3;


{ anhand der Dateierweiterung passenden Viewer aus MIMETYP.DB1 suchen }

procedure GetExtViewer(fn:string; var viewer:viewinfo);
var p : byte;
begin
  viewer.prog:='';
  p:=rightpos('.',fn);
  if (p>0) and (p<length(fn)) then begin
    dbSeek(mimebase,mtiExt,ustr(mid(fn,p+1)));
    if dbFound then begin
      dbReadN(mimebase,mimeb_programm,viewer.prog);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.typ:='';
      viewer.ext:=ustr(mid(fn,p+1));
      viewer.fn:='';
      end;
    end;
end;


function SeekMime(typ:string):boolean;
begin
  dbSeek(mimebase,mtiTyp,ustr(typ));
  SeekMime:=not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(typ,dbReadStr(mimebase,'typ'));
end;


{ anhand des MIME-Typs passenden Viewer aus MIMETYP.DB1 suchen }

procedure GetMimeViewer(typ:string; var viewer:viewinfo);
var gt : string[30];
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
      dbReadN(mimebase,mimeb_programm,viewer.prog);
      if viewer.prog='' then viewer.prog:='*intern*';
      dbReadN(mimebase,mimeb_extension,viewer.ext);
      end
    else begin
      gt:=left(typ,cposx('/',typ))+'*';
      if SeekMime(gt) then begin
        dbReadN(mimebase,mimeb_programm,viewer.prog);
        if viewer.prog='' then viewer.prog:='*intern*';
        dbReadN(mimebase,mimeb_extension,viewer.ext);
        end;
      end;
  if viewer.prog<>'' then begin
    viewer.typ:=typ;
    viewer.fn:='';
    end;
end;


{ Viewer f�r */* ermitteln }

procedure GetDefaultViewer(typ:string; var viewer:viewinfo);
begin
  if DefaultViewer<>nil then begin
    viewer.prog:=DefaultViewer^;
    viewer.ext:='';
    viewer.typ:=typ;
    viewer.fn:='';
    end;
end;


{ Bin�rdatei auf GIF- und ILBM-Signatur testen;  }
{ Betreff auf Dateiextension testen              }

procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo);
var f       : file;
    id      : string[80];
    rr      : word;
    betreff : string[BetreffLen];
    p       : byte;

label betreff_fn;

begin
  if not betreffname or not ntFilename(mbNetztyp) then begin
    viewer.fn:='';
    assign(f,fn);
    if existf(f) then begin
      resetfm(f,0);
      blockread(f,id[1],80,rr);
      id[0]:=chr(rr);
      close(f);
      end
    else
      id:='';
    dbReadN(mbase,mb_betreff,betreff);
    if (left(id,3)='GIF') and SeekMime('image/gif') then begin
      dbReadN(mimebase,mimeb_programm,viewer.prog);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='gif';
      viewer.typ:='image/gif';
      if betreffname then goto betreff_fn;
      end
    else if (pos('ILMB',id)<=20) and SeekMime('image/iff') then begin
      dbReadN(mimebase,mimeb_programm,viewer.prog);
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
             (pos('.'+lstr(dbReadStr(mimebase,'extension')),lstr(betreff))=0)) do
        dbNext(mimebase);
      if not dbEOF(mimebase) then begin
        dbReadN(mimebase,mimeb_programm,viewer.prog);
        dbReadN(mimebase,mimeb_extension,viewer.ext);
        dbReadN(mimebase,mimeb_typ,viewer.typ);
        if viewer.prog='' then viewer.prog:='*intern*';
      betreff_fn:
        p:=pos('.'+lstr(viewer.ext),lstr(betreff));
        truncstr(betreff,p+length(viewer.ext));
        while (p>1) and (betreff[p-1] in
               ['0'..'9','a'..'z','A'..'Z','-','_','.','~','$','(',')','�','�','�','�','�','�','�']) do
          dec(p);
        delete(betreff,1,p-1);
        if validfilename(betreff) then viewer.fn:=betreff;
        end;
      dbSetIndex(mimebase,mtiTyp);
      end;
    end;
end;


Procedure URep(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : byte;
begin
  p:=pos(ustr(s1),ustr(s));
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;


procedure ViewFile(fn:string; var viewer:viewinfo; fileattach:boolean);
var p         : byte;
    prog      : string[ViewprogLen+80];  {Maximallaenge= Programmname+' '+Pfadstring(79)} 
    orgfn,fn1,
    parfn     : pathstr;
begin
  fn1:='';
  orgfn:=iifs(viewer.fn<>'',GetFileDir(fn)+GetFileName(viewer.fn),'');
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
  parfn:=left(parfn,rightpos('.',parfn))+
    iifs(viewer.ext='',mid(orgfn,rightpos('.',orgfn)+1),viewer.ext);
  _rename(orgfn,parfn);

  p:=pos('$FILE',ustr(prog));
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
  Revision 1.6.2.2  2003/01/15 16:02:20  mw
  MW: - Fehlende CVS-Infos erg�nzt (ID und Log wird jetzt
        bei allen Quelldateien in die Dateien geschrieben.
        Assembler-Quellen nur ID.

}
 

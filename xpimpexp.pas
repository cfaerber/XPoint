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

{ Import/Export }

{$I xpdefine.inc}

unit xpimpexp;

interface

uses
  sysutils,typeform,fileio,inout,maske,datadef,database,maus2,resource, zftools,
  xp0,xpglobal;


procedure ImportUserbase;     { X/Import/MB-Userbase }
procedure ImportMautauBase;   { X/Import/MauTau      }
procedure ImportYuppiebase;   { X/Import/Yuppie      }
procedure ImportQWKpacket;    { X/Import/QWK-Paket   }
procedure readfremdpuffer;    { X/Import/Fremdformat }

function imptestpollbox(var s:string):boolean;


implementation  { ----------------------------------------------------- }

uses
{$IFDEF unix}
  xplinux,
{$ENDIF }
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
xp1o,xp1o2,xp3,xp3o,xp3o2,xpmaus,xp9bp,xpconfigedit,xpnt, winxp, xp1;

const mdaten = 'MDATEN.DAT';    { f�r ImportMautaubase }
      mindex = 'MDATEN.IND';
      outtmp = 'OUTFILE.TXT';

var   impnt  : byte;


{ USERBASE.DAT aus MessageBase einlesen :-) }

procedure ImportUserbase;

type ubrec = record
               nextfree  : longint;
               typ       : byte;    { 0=User, 1=Brett }
               aufnehmen : byte;
               name      : string[80];
               adresse   : string[80];
               haltezeit : byte;
               pollbox   : string[8];
               ablage    : byte;
               xx3       : byte;
               zielnetz  : byte;    { 1=Zerberus, 2=Magic }
              end;

var fn   : string;
    brk  : boolean;
    x,y  : Integer;
    f    : file of ubrec;
    r    : ubrec;
    grnr : longint;

    getuser,getbretter   : boolean;
    repluser,replbretter : boolean;

  procedure wrhalten(d:DB);
  var halten : integer16;
  begin
    halten:=r.haltezeit;
    dbWrite(d,'haltezeit',halten);
  end;

begin
  if FileExists('userbase.dat') then         { Name bestimmen }
    fn:='userbase.dat'
  else begin
    dialog(51,7,'',x,y);
    maddtext(3,2,'Im '+xp_xp+'-Verzeichnis befindet sich keine',col.coldialog);
    maddtext(3,3,'USERBASE.DAT.  Bitte geben Sie den Namen Ihres',col.coldialog);
    maddtext(3,4,'MessageBase-Verzeichnisses an:',col.coldialog);
    fn:='';
    maddstring(3,6,'',fn,44,75,'');
    readmask(brk);
    closemask;
    closebox;
    if brk then exit;
    fn:=IncludeTrailingPathDelimiter(fn)+'userbase.dat';
    if not FileExists(fn) then begin
      fehler('Ung�ltiges Verzeichnis oder keine USERBASE.DAT vorhanden!');
      exit;
      end;
    end;

  if _filesize(fn) mod sizeof(ubrec)<>0 then begin   { Dateiformat testen }
    fehler('Sorry. Unbekanntes Dateiformat.');
    exit;
    end;

  getuser:=(useraufnahme<2); getbretter:=true;      { Optionen abfragen }
  repluser:=true; replbretter:=true;
  dialog(40,7,'UserBase einlesen',x,y);
  maddbool(3,2,'User einlesen?      ',getuser);
  maddbool(3,3,'Bretter einlesen?   ',getbretter);

  maddbool(3,5,'vorhandene User �berschreiben? ',repluser);
  maddbool(3,6,'vorh. Bretter �berschreiben?   ',replbretter);
  readmask(brk);
  closemask; closebox;
  if brk then exit;

  msgbox(78,screenlines-6,'UserBase einlesen',x,y);   { Fenster aufmachen.. }
  window(x+2,y+1,x+76,y+screenlines-8);
  assign(f,fn);
  reset(f);
  read(f,r);    { Header �berlesen }
  while not eof(f) do begin                         { .. und einlesen }
    attrtxt(col.colmbox);
    read(f,r);
    with r do begin
      if nextfree=0 then begin
        name:=LeftStr(name,79);
        if (typ=1) and getbretter and
           (LeftStr(name,2)<>'/'#0) and (LeftStr(name,2)<>'/'#255) then begin
          moff;
          write(#13#10'Brett:  ',name);
          mon;
          name:='A'+name;
          dbSeek(bbase,biBrett,UpperCase(name));
          if not dbFound then 
          begin
            dbAppend(bbase);
            dbWriteN(bbase,bb_brettname,name);
            savecursor;
            setbrettindex;
            restcursor;
          end;
          if not dbFound or replbretter then 
          begin
            dbWriteN(bbase,bb_pollbox,pollbox);
            wrhalten(bbase);
            grnr:=NetzGruppe;
            dbWriteN(bbase,bb_gruppe,grnr);
          end;
        end;
        if (typ=0) and getuser and
           ((useraufnahme<>1) or ((cPos('%',name)=0) and (cPos(':',name)=0)))
        then begin
          if cpos('@',name)=0 then name:=LeftStr(name+'@'+DefaultBox+'.ZER',79);
          moff;
          write(#13#10'User:   ',name);
          mon;
          dbSeek(ubase,uiName,UpperCase(name));
          if not dbFound then
            AddNewUser(Name, Pollbox);
          if not dbFound or repluser then
          begin
            if name<>adresse then
              dbWriteXStr(ubase,'adresse',length(adresse)+1,adresse);
            dbWriteN(ubase,ub_pollbox,pollbox);
            wrhalten(ubase);
            dbWriteN(ubase,ub_userflags,aufnehmen);
            end;
          end;
        end;
      end;
    end;
  FlushClose;
  close(f);
  moff;
  writeln; writeln;
  write('Fertig!');
  mon;
  errsound; errsound;
  wkey(2,false);
  window(1,1,screenwidth,screenlines);
  closebox;
  aufbau:=true;
end;


function imptestpollbox(var s:string):boolean;
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  SeekLeftBox(d,s);
  if dbFound then
    if (impnt<>nt_QWK) or (dbReadInt(d,'netztyp') in [nt_Fido,nt_QWK]) then
      imptestpollbox:=true
    else begin
      rfehler(2413);    { 'Falscher Netztyp - Netztyp mu� QWK oder Fido sein!' }
      imptestpollbox:=false;
      end
  else begin
    rfehler(2412);   { 'unbekannte Serverbox - w�hlen mit <F2>' }
    imptestpollbox:=false;
    end;
  dbClose(d);
end;


procedure readfremdpuffer;    { X/Import/Fremdformat }
var t   : text;
    s   : string;
    fn  : string;
    nt  : shortint;
    x,y : Integer;
    brk : boolean;
    d   : DB;
    box : string;
    red : boolean;
    eb  : boolean;
    useclip : boolean;
    ft  : longint;
begin
(*  if not ExecutableExists(MaggiBin) then begin
    rfehler(102);    { 'Netcallkonvertierer MAGGI.EXE fehlt!' }
    exit;
    end; *)
  if IsPath('PUFFER') then begin
    rfehler1(741,'PUFFER');
    exit;
    end;
  fn:=WildCard;
  useclip:=false;
  if ReadFilename(getres(2420),fn,true,useclip)   { 'Nachrichtenpaket konvertieren/einlesen' }
  then
    if not FileExists(fn) then rfehler(106)
    else begin
      UpString(fn);
      if pos(UpperCase(MausLogfile),fn)>0 then begin
        box:='';  { dummy }
        MausLogFiles(0,false,box);
        MausLogFiles(2,false,box);
        exit;
        end;
      if pos(UpperCase(MausStLog),fn)>0 then begin
        box:=UniSel(1,false,'');
        if box<>'' then
          MausLogFiles(1,false,box);
        exit;
        end;
      if RightStr(UpperCase(fn),4)='.QWK' then begin
        nt:=nt_QWK;
        if DefFidoBox<>'' then box:=DefFidoBox
        else box:='';
        end
      else begin
        assign(t,fn);
        reset(t);
        readln(t,s);
        close(t);
        if s=^A then nt:=nt_Magic else           { Puffertyp ermitteln }
        if cpos(#0,s)>0 then nt:=nt_Fido else
        if LeftStr(s,1)='#' then nt:=nt_Maus else
        if cpos('\',s)>0 then nt:=nt_Quick
        else nt:=nt_Netcall;
        if nt=nt_Fido then box:=DefFidoBox       { Vorgabe-Box ermitteln }
        else box:=DefaultBox;
        end;
      if (box='') or ((nt<>nt_Netcall) and (nt<>nt_Fido)) then begin
        dbOpen(d,BoxenFile,1);
        while not dbEOF(d) and (dbReadInt(d,'netztyp')<>nt) do
          dbNext(d);
        if not dbEOF(d) then box:= dbReadStr(d,'boxname');
        dbClose(d);
        end;

      dialog(45,8,'',x,y);                     { Pollbox einlesen }
      maddtext(3,2,getres2(2421,1),0);         { 'Pufferdatei' }
      maddtext(4+length(getres2(2421,1)),2,fitpath(fn,28),col.coldiahigh);    { 'Ursprungsbox ' }
      MaddString(3,4,getres2(2421,2),box,BoxRealLen,BoxNameLen,'>'); mhnr(760);
      mappcustomsel(BoxSelproc,false);
      msetvfunc(imptestpollbox); impnt:=nt;
      red:=false; eb:=false;
      maddbool(3,6,getres2(2421,3),red);   { 'Empfangsdatum = Erstellungsdatum' }
      maddbool(3,7,getres2(2421,4),eb);    { 'Empfangsbest�tigungen verschicken' }
      readmask(brk);
      enddialog;
      if brk then exit;

      nt:=ntBoxNetztyp(box);                   { Puffer konvertieren }
      ReadBoxpar(nt,box);
      s:='PUFFER';
      case iif(impnt<>nt_QWK,nt,impnt) of
        nt_Magic : shell(MaggiBin+' -mz -n'+boxpar^.MagicNET+' '+fn+' PUFFER '+
                         box+extBl,300,3);
        nt_Quick,
        nt_GS    : shell(MaggiBin+' -qz '+fn+' PUFFER',300,3);
        nt_Maus  : begin
                     ft:=filetime(box+'.itg');
                     shell(MaggiBin+' -sz -b'+box+' -h'+boxpar^.MagicBrett+' '+
                         '-it '+fn+' PUFFER',300,3);
                   end;
        nt_Fido  : begin
                     msgbox(70,10,GetRes2(30003,10),x,y);
                     DoZFido(2, BoxPar^.MagicBrett, fn, 'PUFFER', '', '', 0, '', '', true, KeepVia, false, false, x, y);
                     closebox;
                   end;
        nt_QWK   : if not ExecutableExists(ZQWKBin) then
                     rfehler1(2414,ZQWKBin)  { %s fehlt! alt: 'ZQWK.EXE fehlt!
                      (ZQWK.EXE ist im getrennt erh�ltlichen QWK-Paket enthalten)' }
                   else begin
                     shell(ZQWKBin+' -qz -c'+GetServerFilename(box, '')+' -b'+box+
                           ' -i'+fn+' -o'+ExtractFilePath(fn)+' -h'+BoxPar^.MagicBrett+
                           iifs(nt=nt_Fido,' -t30',''),600,1);
                     if errorlevel=100 then begin
                       errorlevel:=0;
                       s:=LeftStr(fn,length(fn)-4)+'.ZER';
                       end;
                     end;
        nt_Netcall,
        nt_ZConnect: begin
                       s:=fn;
                       errorlevel:=0;
                     end;
      else begin
        rfehler(2410);   { 'nicht unterst�tzter Netztyp' }
        exit;
        end;
      end;

      if errorlevel<>0 then
        if impnt=nt_QWK then begin
          if errorlevel in [90..110] then
            fehler(getres2(2422,4)+getres2(2422,errorlevel))  { 'Fehler bei ZQWK-Konvertierung:~ }
          else
            rfehler1(737,strs(errorlevel));  { 'ZQWK-Fehler Nr. %s bei Nachrichtenkonvertierung!' }
          freeres;
          end
        else
          rfehler(2411)   { 'Fehler bei Nachrichtenkonvertierung' }
      else begin
        if nt=nt_Maus then begin
          MausLogFiles(0,false,box);
          MausLogFiles(1,false,box);
          MausLogFiles(2,false,box);
          if filetime(box+'.itg')<>ft then
            MausImportITG(box);
          end;
        if PufferEinlesen(s,box,red,false,eb,iif(nt=nt_Fido,pe_ForcePfadbox,0))
        then begin
          if s='PUFFER' then _era(s);
          signal;
          end;
        end;
      end;
end;


{ --- nach MT2OUTF.PAS von Peter Redecker @ DO ------------------------ }

procedure MakeOutfile(var box:string; path:string);
const EndOfLine = 13;
      EndOfMsg  = 10;
      bufsize   = 4096;
type  MsgITyp   = WORD;
      MsgCrcTyp = WORD;
      Msg_Index = RECORD
                    LfCrc: MsgCrcTyp;
                    HeaderSize,      { Header-Gr��e }
                    MsgSize: WORD;   { Msg-Gr��e in Bytes }
                    DIndex: LongINT;
                    KommentarZu,
                    Antwort,
                    RechteAntwort,
                    LinkeAntwort : MsgITyp;
                    AnzahlKommentare: BYTE;
                    Datum: LongINT;
                    Status: CHAR;
                    SDatum: LongINT;
                  END;
      buft      = array[0..bufsize-1] of byte;

var   daten    : FILE;
      index    : FILE OF Msg_Index;
      outfile  : TEXT;
      x,tempx  : msg_index;
      tempdatum: TDateTime;
      was      : byte;
      mx,my    : Integer;
      n        : longint;
      buf      : ^buft;
      bufpos,
      bufend   : Integer;
      rr       : Integer;

      textzeile,idzeile,gruppe,
      betreff,absender,empfaenger,vonzeile : string;
      seek_daten_merk,seek_index_merk      : longint;

  procedure ReadBuf;
  begin
    blockread(daten,buf^,bufsize,bufend);
    bufpos:=0;
  end;

  function zeile_auslesen(var letztes_zeichen:byte):string;
  var was,p    : byte;
      ergebnis : string;
  begin
    was:=0;
    p:=0;
    while (bufpos<bufend) and (was<>EndOfLine) and (was<>EndOfMsg) do begin
      was:=buf^[bufpos];
      inc(bufpos);
      if (bufpos=bufend) and not eof(daten) then
        ReadBuf;
      if (was<>EndOfLine) and (was<>EndOfMsg) and (p<255) then begin
        inc(p);
        ergebnis[p]:=chr(was);
        end;
      end;
    SetLength(ergebnis, p); {ergebnis[0]:=chr(p);}
    letztes_zeichen:=was;
    zeile_auslesen:=ergebnis;
  end;

BEGIN
  msgbox(37,5,'MauTau-Daten konvertieren',mx,my);
  wrt(mx+3,my+2,'bearbeitete Nachrichten:');
  n:=0;
  new(buf);
  bufpos:=0; bufend:=0;

  Assign(daten,path+mdaten); Reset(daten,1);
  Assign(index,path+mindex); Reset(index);
  Assign(outfile,outtmp);    Rewrite(outfile);
  while not eof(index) do begin
    read(index,x);
    Seek(daten,x.DIndex);
    ReadBuf;

    idzeile   := zeile_auslesen(was);
    gruppe    := zeile_auslesen(was);
    betreff   := zeile_auslesen(was);
    absender  := zeile_auslesen(was);
    if cpos('@',absender)=0 then
      absender:=absender+' @ '+box;
    empfaenger:= zeile_auslesen(was);
    if (empfaenger<>'') and (cpos('@',empfaenger)=0) then
      empfaenger:=empfaenger+' @ '+box;
    vonzeile  := zeile_auslesen(was);
    inc(n);
    attrtxt(col.colmboxhigh);
    mwrt(mx+29,my+2,strs(n));

    writeln(outfile,'#',idzeile);
    writeln(outfile,'V',absender);
    IF gruppe='PRIVAT' then writeln(outfile,'A',empfaenger);
    writeln(outfile,'W',betreff);
    // !! UnPackTime(x.datum,tempdatum);
    tempdatum:= FileDateToDateTime(x.datum);
    write(outfile,'E');
    write(outfile,FormatDateTime('yyyymmddhhnn', tempdatum));
    //datum_ins_outfile(tempdatum);
    // !!UnPackTime(x.SDatum,tempdatum);
    tempdatum:= FileDateToDateTime(x.SDatum);
    write(outfile,'B',upcase(x.status));
    write(outfile,FormatDateTime('yyyymmddhhnn', tempdatum));
    //datum_ins_outfile(tempdatum);

    if gruppe<>'PRIVAT' then writeln(outfile,'G',gruppe);
    if x.KommentarZu<>0 then begin
      seek_index_merk:=FilePos(index);
      seek_daten_merk:=FilePos(daten);
      Seek(index,x.KommentarZu);
      read(index,tempx);
      Seek(daten,tempx.DIndex);
      SetLength(idzeile, 40);                  { Init }
      blockread(daten,idzeile[1],40,rr);
      SetLength(idzeile, rr);                  { Korrekt }
      writeln(outfile,'-',LeftStr(idzeile,cpos(#13,idzeile)-1));
      Seek(daten,Seek_daten_merk);
      Seek(index,seek_index_merk);
      end;

    writeln(outfile,'>',vonzeile);
    was:=0;
    while (bufpos<bufend) and (was<>EndOfMsg) do begin
      textzeile:=zeile_auslesen(was);
      if was<>EndOfMsg then writeln(outfile,':',textzeile);
      end;
    end;   { while not eof(index) }

  close(index);
  close(daten);
  close(outfile);
  dispose(buf);
  closebox;
end;


procedure ReadOutfile(var box:string);
begin
  ReadBoxPar(0,box);
  shell(MaggiBin+' -sz -b'+box+' -h'+boxpar^.MagicBrett+' '+outtmp+' PUFFER',
        300,3);
  if errorlevel<>0 then
    fehler('Fehler bei Nachrichtenkonvertierung')
  else begin
    _era(outtmp);
    if PufferEinlesen('PUFFER',box,true,false,false,0) then
      _era('PUFFER');
    end;
end;


procedure ImportMautauBase;   { X/Import/MauTau }
var mtpath : string;
    brk    : boolean;
    x,y    : Integer;
    box    : string;
begin
  if not mfehler(ExecutableExists(MaggiBin),MaggiBin+' fehlt!') then begin
    dialog(51,9,'',x,y);
    maddtext(3,2,'Geben Sie den Namen der Box, f�r die die Daten',0);
    maddtext(3,3,'eingelesen werden sollen, und den Namen Ihres',0);
    maddtext(3,4,'MauTau-Verzeichnisses an:',0);
    mtpath:=''; box:='';
    maddstring(3,6,'Box   ',box,boxnamelen,boxnamelen,'>');
    mappcustomsel(boxselproc,false);
    msetvfunc(imptestpollbox);
    maddstring(3,8,'Verz. ',mtpath,35,75,'');
    readmask(brk);
    closemask;
    closebox;
    if brk or (mtpath='') then exit;
    mtpath:= AddDirSepa(mtpath);
    if not mfehler(ntBoxNetztyp(box)=nt_Maus,box+' ist keine MausNet-Box') and
       not mfehler(IsPath(mtpath),'ung�ltiges Verzeichnis') then begin
      if not FileExists(mtpath+mdaten) and IsPath(mtpath+'daten') then
        mtpath:=mtpath+FileUpperCase('daten')+DirSepa;
      if not mfehler(FileExists(mtpath+mdaten),'In diesem Verzeichnis befindet sich keine MauTau-Datenbank.') and
         not mfehler(FileExists(mtpath+mindex),mtpath+mindex+' fehlt') and
         not mfehler(diskfree(0)>2.5*_filesize(mtpath+mdaten),
             'zu wenig freier Speicherplatz zum Einlesen der Daten') then
      begin
        MakeOutfile(box,mtpath);
        ReadOutfile(box);
        end;
      end;
    end;
end;


function FehlerFidoStammbox:boolean;
begin
  FehlerFidoStammbox:=mfehler(DefFidoBox<>'','Erst Fido-Stammbox w�hlen!') or
         mfehler(IsBox(DefFidoBox),'Ung�ltige Fido-Stammbox: '+DefFidoBox);
end;

procedure ImportYuppiebase;          { --- Yuppie-Import ----------------- }
var ypath : string;
    brk   : boolean;
    x,y   : Integer;

  procedure ImportYupbase;
  const TempPKT = '1.PKT';
  var x, y: Integer;
  begin
    shell(Yup2PktBin+' '+ypath+' '+TempPKT+' '+DefFidoBox,300,3);
    if not mfehler(errorlevel=0,'Fehler bei Nachrichtenkonvertierung') then begin
      ReadBoxPar(0,DefFidoBox);
      msgbox(70,10,GetRes2(30003,10),x,y);
      errorlevel:= DoZFido(2, BoxPar^.MagicBrett, TempPkt, 'FPUFFER', '', '', 0, '', '', true, false, false, false, x, y);
      closebox;
      if errorlevel<>0 then
        fehler('Fehler bei Nachrichtenkonvertierung')
      else begin
        _era(TempPKT);
        if PufferEinlesen('PUFFER',DefFidoBox,true,false,false,0) then
          _era('PUFFER');
        end;
      end;
  end;

begin
  if not mfehler(ExecutableExists(Yup2PktBin),'"'+Yup2PktBin+'" fehlt!') and
     not FehlerFidoStammbox then
  begin
    dialog(56,5,'',x,y);
    maddtext(3,2,'Geben Sie den Namen Ihres Yuppie-Verzeichnisses an:',0);
    ypath:='';
    maddstring(3,4,'',ypath,50,75,'');
    readmask(brk);
    closemask;
    closebox;
    if brk or (ypath='') then exit;
    ypath:=AddDirSepa(ypath);
    if not mfehler(IsPath(ypath),'ung�ltiges Verzeichnis') then begin
      if not FileExists(ypath+'AREABASE.DBF') and IsPath(ypath+FileUpperCase('mailbase')) then
        ypath:=ypath+FileUpperCase('mailbase')+DirSepa;
      { Gibt es Yuppi unter Linux? Wenn ja, sind die Dateinamen
        klein oder gross geschrieben? }
      if not mfehler(FileExists(ypath+'AREABASE.DBF'),'In diesem Verzeichnis befindet sich keine Yuppie-Datenbank.') and
         not mfehler(FileExists(ypath+'NET-MAIL.DBF'),ypath+'NET-MAIL.DBF fehlt') and
         not mfehler(diskfree(0)>2.5*FileMaskSize(ypath+'*.DBT'),
                'zu wenig freier Speicherplatz zum Einlesen der Daten')
      then
        ImportYupbase;
      end;
  end;
end;


procedure ImportQWKpacket;
var x,y     : Integer;
    fn      : string;
    useclip : boolean;
    bretth  : string;
    brk     : boolean;
begin
  if not mfehler(ExecutableExists(ZQWKBin),getres2(2422,1)) and not FehlerFidoStammbox
  then begin
    fn:='*.QWK';
    useclip:=false;
    if ReadFilename(getres2(2422,2),fn,true,useclip) then begin
      ReadBoxPar(nt_Fido,DefFidoBox);
      bretth:=BoxPar^.MagicBrett;
      dialog(50,3,fitpath(fn,46),x,y);
      maddstring(3,2,getres2(2422,3),bretth,32,32,''); mhnr(880);
      readmask(brk);
      enddialog;
      if not brk then begin
        if FirstChar(bretth)<>'/' then bretth:='/'+bretth;
        if LastChar(bretth)<>'/' then bretth:=bretth+'/';
        shell(ZQWKBin+' -qz -b'+{DefFidoBox}'blafasel'+' -h'+bretth+' '+fn,500,4);
        fn:=ChangeFileExt(fn, '.ZER');
        if not FileExists(fn) then
          fehler(getres2(2422,4))
        else begin
          if PufferEinlesen(fn,DefFidoBox,true,false,false,0) then
            signal;
          _era(fn);
          end;
        end;
      end;
    end;
  freeres;
end;

{
  $Log$
  Revision 1.47.2.1  2002/07/21 20:14:40  ma
  - changed copyright from 2001 to 2002

  Revision 1.47  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.46  2001/10/20 17:26:43  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.45  2001/10/17 22:11:48  ml
  - removed some circular unit-defs

  Revision 1.44  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.43  2001/09/08 16:29:40  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.42  2001/09/07 13:54:24  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.41  2001/09/07 10:56:02  mk
  - added GetServerFilename

  Revision 1.40  2001/08/11 23:06:38  mk
  - changed Pos() to cPos() when possible

  Revision 1.39  2001/07/23 16:05:24  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.38  2001/06/04 17:36:50  ma
  - renamed old xp9 source files

  Revision 1.37  2001/03/22 18:25:09  ma
  - FmtDateTime: "mm" means "month", *not* "minute".

  Revision 1.36  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.35  2000/12/28 13:29:57  hd
  - Fix: packets now sorted in after netcall
  - Adjusted: Open window once during sorting in

  Revision 1.34  2000/12/25 23:21:38  mk
  - commented out check for maggi.exe

  Revision 1.33  2000/12/25 20:31:18  mk
  - zfido is now completly integrated

  Revision 1.32  2000/12/05 14:58:12  mk
  - AddNewUser

  Revision 1.31  2000/11/19 17:53:35  ma
  - renamed existBin to ExecutableExists

  Revision 1.30  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.29  2000/11/15 23:00:44  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.28  2000/11/14 15:51:37  mk
  - replaced Exist() with FileExists()

  Revision 1.27  2000/11/14 11:14:35  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.26  2000/11/06 00:41:26  mk
  - fixed Bug #116657: crash with servername >15 chars

  Revision 1.25  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.24  2000/10/17 10:05:59  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.23  2000/09/25 17:58:31  mk
  - Window ausgeklammert, da in 32 Bit Version nicht erlaubt

  Revision 1.22  2000/07/30 08:49:54  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.21  2000/07/22 14:05:28  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.20  2000/07/13 10:23:47  mk
  - Zeiger auf Strings entfernt

  Revision 1.19  2000/07/06 09:12:09  mk
  - AnsiString Updates

  Revision 1.18  2000/07/06 08:58:46  hd
  - AnsiString

  Revision 1.17  2000/07/04 12:04:30  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.16  2000/06/23 15:59:26  mk
  - 16 Bit Teile entfernt

  Revision 1.15  2000/06/16 19:56:24  mk
  - jetzt geht es auch unter nicht Linux wieder zu compilieren, bitte die Aenderungen pruefen!

  Revision 1.14  2000/06/16 14:50:13  hd
  - exist an einigen Stellen durch existBin ersetzt
  - Hart codierte Dateinamen (ZQWK.EXE etc.) durch Konstanten ersetzt

  Revision 1.13  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.12  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.11  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.10  2000/04/18 11:23:52  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.9  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.8  2000/04/13 12:48:40  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.7  2000/03/30 14:05:05  mk
  - unn�tigen Debugcode entfernt

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

}
end.


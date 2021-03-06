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

{ ZPR - ZCONNECT-Pufferreparierer }
{ PM 08/93, 10/93                 }

{$I xpdefine.inc }

unit zpr;

interface

procedure StartCommandlineZPR;

implementation

uses
{$IFDEF NCRT }
  xpcurses,             { Fuer die Sonderzeichen an der Console }
{$ENDIF }
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
{$IFDEF Unix }
  xpunix,
{$ENDIF }
  sysutils, xpconst,fileio, typeform, xpconfigedit, xpglobal, xp0, xpx;

const maxhdlines  = 256;    { max. ausgewertete Headerzeilen pro Nachricht }
      bufsize     = 16384;  { Gr��e Kopier/Einlesepuffer                   }
      maxzchdlen  = 100;    { max. erlaubte L�nge von Headernamen          }
      knownheaders= 24;     { Headerzeilen, deren Syntax bekannt ist       }
      stdhdlines  = 7;      { Anzahl Pflichtheaderzeilen                   }
      TO_ID       = '/'#0#0#8#8'TO:';

{$IFDEF unix}              { unter linux keine '/' als Param verwenden }
      paramchars   = ['-'];
{$ELSE }
      paramchars   = ['-','/'];
{$ENDIF }

      logfilename = 'ZPR.LOG';

type
  PathStr = string;          { Full file path string }

var   ParLogfile  : boolean = false;    { -l Logfile anlegen                }
      ParKillmsg  : boolean = false;    { -l defekte Nachrichten l�schen    }
      ParRep      : boolean = false;    { -r Puffer reparieren              }
      ParExact    : boolean = false;    { -h strenge Headerzeilen-�berpr�f. }
      ParNowarn   : boolean = false;    { -w Warnungen unterdr�cken         }
      ParShowhd   : boolean = false;    { -z fehlerhafte Zeilen anzeigen    }

      fi          : pathstr = '';       { Name der Eingabedatei }
      fo          : pathstr = '';       { Name der Ausgabedatei }
      ferr        : pathstr = '';       { Name der Fehlerdatei  }
      errfile     : boolean = false;    { Fehlerdatei ge�ffnet  }
      prozent     : boolean = true;     { fortlaufende Prozentanzeige }
      logopen     : boolean = false;

      { Liste aller Headerzeilen, die in irgendeiner Form �berpr�ft werden. }
      { Alle �brigen Zeilen werden nicht interpretiert.                     }

const headerindex : array[1..knownheaders] of
                      record
                        name  : string[15];
                        multi : boolean;   { Zeile darf mehrfach vorkommen }
                        ampm  : byte;      { 1=PM, 2=AM, 3=beides }
                      end =
        ((name: 'ABS';           multi: false;   ampm: 3),
         (name: 'ANTWORT-AN';    multi: true;    ampm: 3),
         (name: 'BET';           multi: false;   ampm: 3),
         (name: 'BEZ';           multi: true;    ampm: 3),
         (name: 'CRYPT';         multi: false;   ampm: 1),
         (name: 'DDA';           multi: false;   ampm: 3),
         (name: 'DISKUSSION-IN'; multi: true;    ampm: 2),
         (name: 'EB';            multi: true;    ampm: 1),
         (name: 'EDA';           multi: false;   ampm: 3),
         (name: 'EMP';           multi: true;    ampm: 3),
         (name: 'FILE';          multi: false;   ampm: 3),
         (name: 'LEN';           multi: false;   ampm: 3),
         (name: 'MID';           multi: false;   ampm: 3),
         (name: 'O-EDA';         multi: false;   ampm: 3),
         (name: 'OAB';           multi: false;   ampm: 3),
         (name: 'PRIO';          multi: false;   ampm: 3),
         (name: 'ROT';           multi: false;   ampm: 3),
         (name: 'TELEFON';       multi: false;   ampm: 3),
         (name: 'TRACE';         multi: false;   ampm: 1),
         (name: 'WAB';           multi: false;   ampm: 3),
         (name: 'OEM';           multi: true;    ampm: 3),
         (name: 'KOM';           multi: false;   ampm: 3),
         (name: 'KOP';           multi: true;    ampm: 3),
         (name: 'VER';           multi: true;    ampm: 3));

const hdf_ABS     = 1;    hdf_EB   = 8;      hdf_OAB     = 15;
      hdf_ANTWORT = 2;    hdf_EDA  = 9;      hdf_PRIO    = 16;
      hdf_BET     = 3;    hdf_EMP  = 10;     hdf_ROT     = 17;
    {  hdf_BEZ     = 4;}    hdf_FILE = 11;     hdf_TELEFON = 18;
      hdf_CRYPT   = 5;    hdf_LEN  = 12;
      hdf_DDA     = 6;    hdf_MID  = 13;     hdf_WAB     = 20;
      hdf_DISK    = 7;    hdf_OEDA = 14;     hdf_OEM     = 21;

      hdf_KOM     = 22;
      hdf_KOP     = 23;
      hdf_VER     = 24;

      stdhdindex : array[1..stdhdlines] of byte =
                   (hdf_LEN, hdf_ABS, hdf_BET, hdf_EDA,
                    hdf_EMP, hdf_MID, hdf_ROT);


type  header    = record
                    adr     : longint;    { Offset in f1 (redundant zu adr0/adr1) }
                    hds     : longint;    { Gr��e inc. CR/LF am Ende }
                    fldanz  : integer;    { Anzahl eingelesener Felder }
                    fld     : array[1..maxhdlines] of string;   { Felder }
                    contpos : array[1..maxhdlines] of Integer;   { Startoffst des Feldinhalts }
                    fldtype : array[1..maxhdlines] of Integer;   { Headerindex-Nr. }
                    fldsize : array[1..maxhdlines] of longint;  { phys. Feldl�nge }
                    hdfound : array[1..knownheaders] of integer;
                    lferror : boolean;    { falsche Zeilentrennung }
                    groesse : longint;    { LEN }
                    LENpos  : integer;    { Position des LEN-Headers }
                    msgid   : string;     { MID }
                    modified: boolean;    { Header wurde korrigiert }
                    XPnt    : byte;       { X-XP-NTP }
                  end;

var
      buf       : array[0..bufsize-1] of char;  { allg. Einlese/Kopierpuffer }
      hd0,hd1   : header;
      f1,f2,f3  : file;       { Ein/Ausgabedatei, Fehlerdatei }
      logfile   : text;
      fsize     : longint;    { Gr��e der Eingabedatei }
      adr0      : longint;    { Startadresse des vorausgehenden Headers }
      adr1      : longint;    { Startadresse des aktuellen Headers }

      msgs      : longint;    { Anzahl Nachrichten gesamt }
      errmsgs   : longint;    { davon fehlerhaft          }
      warnungen : longint;    { Anzahl Warnungen          }
      unzustmsgs: longint;    { Anzahl headerlose Texte   }
      ww,wwn    : string[10]; { 'w�rde(n)'                }
      wwnn      : string[10];
      newcheckmsg:boolean;

      kchar     : set of char;  { in Header-Bezeichnern erlaubte Zeichen }
      brchar    : set of char;  { in Brettnamen erlaubte Zeichen         }

function  TestControlChar(var s:string):boolean;
var
 i: Integer;
begin
  Result := false;
  for i := 1 to Length(s) do
    if (s[i] < ' ') and (s[i] <> '9') then
      Result := true;
end;

procedure helppage;
{$IFDEF unix}
{ Linux benutzt kein Carriage Return... }
const crlf = #10;
{$ELSE}
const crlf = #13#10;
{$ENDIF}
begin
   writeln('Syntax:    ZPR [Schalter] <Quelldatei> [Zieldatei]'+crlf+
          crlf,
          'Schalter:  -f   Fehler in ZPR.LOG aufzeichnen'+crlf+
          '           -h   strenge Headerzeilen-�berpr�fung'+crlf,
          '           -l   defekte Nachrichten l�schen'+crlf+
          '           -r   Puffer reparieren'+crlf+
          '           -w   Warnungen unterdr�cken'+crlf,
          '           -z   fehlerhafte Zeilen anzeigen'+crlf,
          crlf,
          '           -d   Dateiname  fehlerhafte Nachrichten in Datei schreiben'
           );
  halt(2);
end;

procedure error(txt:string);
begin
  writeln(#13'Fehler: ',txt);
  close(f1); if IOResult=0 then ;
  close(f2); if IOResult=0 then ;
  close(f3); if IOResult=0 then ;
  if fo<>'' then begin
    assign(f1,fo);
    erase(f1);
    if IOResult=0 then ;
  end;
  if logopen then begin
    writeln(logfile,'* �berpr�fung abgebrochen: ',txt);
    writeln(logfile);
    close(logfile);
    end;
  halt(2);
end;

function ioerror(i:integer; otxt:atext):atext;
begin
  case i of
      2 : ioerror:='Datei nicht gefunden';
      3 : ioerror:='ung�ltiges Verzeichnis';
      4 : ioerror:='zu viele Dateien ge�ffnet (bitte FILES erh�hen!)';
      5 : ioerror:='Zugriff verweigert';
      7 : ioerror:='Speicherverwaltung zerst�rt';
      8 : ioerror:='ungen�gend Speicher';
     10 : ioerror:='ung�ltiges Environment';
     11 : ioerror:='ung�ltiges Aufruf-Format';
     15 : ioerror:='ung�ltige Laufwerksbezeichnung';
     16 : ioerror:='Verzeichnis kann nicht gel�scht werden';
     18 : ioerror:='zu wenig FILES (CONFIG.SYS)';
    101 : ioerror:='Diskette/Platte voll';
    150 : ioerror:='Diskette ist schreibgesch�tzt';
    152 : ioerror:='keine Diskette eingelegt';
154,156 : ioerror:='Lesefehler (Diskette/Platte defekt)';
157,158 : ioerror:='Diskette ist nicht korrekt formatiert';
    159 : ioerror:='Drucker ist nicht betriebsbereit';
    162 : ioerror:='Hardware-Fehler';
  else
    ioerror:=otxt;
  end;
end;

procedure ior;
var
  IORes: Integer;
begin
  IORes := IOResult; { Abfragen und merken }
  if IORes <> 0 then
    error(ioerror(IORes,'Lesefehler in Eingabedatei'));
end;

procedure iow;
var
  IORes: Integer;
begin
  IORes := IOResult; { Abfragen und merken }
  if IORes <>0 then
    error(ioerror(IORes,'Schreibfehler in Ausgabedatei'));
end;


procedure getpar;
var i,j : integer;
    s   : string;
    c   : char;
    err : boolean;

  procedure _err(txt:atext);
  begin
    writeln(#7,txt);
    err:=true;
  end;

  procedure GetFilename;
  begin
    if j>length(s) then
      _err('Dateiname bei "-d"-Option fehlt!')
    else begin
      ferr:=UpperCase(mid(s,j));
      j:=length(s)+1;
      end;
  end;

begin
  if paramcount=1 then helppage;
  err:=false;
  for i:=2 to paramcount do
  begin
    s:=trim(paramstr(i));
    if s <> '' then // was  if length(left(s,1)) > 0 then !?!?
    begin
      if FirstChar(s) in paramchars then
      begin
        delete(s,1,1);
        s:=LowerCase(s);
        j:=1;
        while (j<=length(s)) do begin
          c:=s[j];
          inc(j);
          if c='h' then ParExact:=true
          else if c='l' then ParKillmsg:=true
          else if c='f' then ParLogfile:=true
          else if c='r' then ParRep:=true
          else if c='d' then GetFilename
          else if c='w' then ParNowarn:=true
          else if c='z' then ParShowhd:=true
          else if (c='?') or (c='h') then helppage
          else begin
            writeln(#7'unbekannter Schalter:  ',c);
            err:=true;
            end;
          end;
      end
    else begin
{$IFNDEF unix}
      UpString(s);
{$ENDIF }
      if fi='' then fi:=s
      else if fo='' then begin
        fo:=s; ParRep:=true; end
      else error('�berfl�ssiger Parameter: "'+s+'"');
      end;
    end;
  end;
  if err then halt(2);
end;

Function ValidFileName(name:PathStr):boolean;
var f : file;
begin
  if (name='') or
     (cPos('*',name)+cPos('?',name)>0) then    { Fehler in DR-DOS 5.0 umgehen }
    ValidFileName:=false
  else begin
    assign(f,name);
    filemode:= fmOpenRead + fmShareDenyNone;
    reset(f,1);
    if ioresult=0 then
      ValidFilename:=true
    else begin
      filemode:= fmOpenWrite + fmShareDenyNone;
      rewrite(f);
      close(f);
      erase(f);
      ValidFileName:=(ioresult=0);
      end;
    end;
end;

procedure checkpar;
begin
  if fi='' then error('keine Quelldatei angegeben');
  filemode:= fmOpenRead + fmShareDenyNone;
  assign(f1,fi); reset(f1,1); close(f1);
  if ioresult<>0 then
    error('Datei "'+fi+'" nicht vorhanden oder nicht lesbar.');
  if (fo<>'') and not validfilename(fo) then
    error('ung�ltige Zieldatei: "'+fo+'"');
  if ferr<>'' then begin
    errfile:=true;
    if not validfilename(ferr) then
      error('ung�ltige Fehlerausgabedatei: "'+ferr+'"');
    end;
  if ExpandFilename(fi)=ExpandFileName(fo) then fo:='';
  if ParRep and (pos('MPUFFER.',fi)>0) and ((fo='') or (pos('MPUFFER.',fo)>0))
  then
    error('CrossPoint-MPUFFER-Dateien d�rfen nicht direkt modifiziert werden!');
end;

procedure openfiles;
begin
  if ParRep then begin
    if fo='' then
      fo := ChangeFileExt(fi, '$$$')
    else
      makebak(fo, ExtBak);
    filemode:=fmOpenWrite + fmShareDenyNone;
    assign(f2,fo); rewrite(f2,1);
    if ioresult<>0 then error('Kann Tempor�rdatei "'+fo+'" nicht �ffnen.');
    end;
  filemode:= fmOpenRead + fmShareDenyNone;
  assign(f1,fi); reset(f1,1);
  fsize:=filesize(f1);
  writeln('Eingabedatei: ',fi);
  write('Ausgabedatei: ');
  if not ParRep then
    writeln('-keine-')
  else
    if RightStr(fo,3)='$$$' then writeln(fi)
    else writeln(fo);
  if errfile then
    writeln('Fehlerdatei:  ',ferr);

  writeln;
  if ParLogfile then begin
    assign(logfile,logfilename);
    filemode:=fmOpenWrite + fmShareDenyNone;
    append(logfile);
    if ioresult<>0 then rewrite(logfile);
    if ioresult<>0 then error('Kann '+logfilename+' nicht �ffnen.');
    logopen:=true;
    writeln(logfile,date,'/',time,':  �berpr�fe ',fi);
    writeln(logfile);
    end;
  if errfile then begin
    filemode:= fmOpenWrite + fmShareDenyNone;
    assign(f3,ferr); rewrite(f3,1);
    if ioresult<>0 then
      error('Kann Fehlerausgabedatei "'+ferr+'" nicht �ffnen.');
    end;
end;


procedure initvar;
begin
  msgs:=0; errmsgs:=0;
  warnungen:=0; unzustmsgs:=0;
  if not ParRep then begin
    ww:='w�rde '; wwn:='w�rden '; wwnn:='w�rde(n) '; end
  else begin
    ww:=''; wwn:=''; wwnn:=''; end;
  prozent:=not Sysoutputredirected;
  kchar:=['A'..'Z','a'..'z','0'..'9','-','_'];
  brchar:=kchar + ['/','!','+'];
  brchar:=brchar + ['.','-'];     { eigentlich nicht erlaubt, aber �blich }
end;

procedure closefiles;
begin
  close(f1);
  if ParRep then begin
    close(f2);
    if RightStr(fo,3)='$$$' then begin
      makebak(fi,ExtBak);
      rename(f2,fi);
      if ioresult<>0 then error('"'+fi+'" konnte nicht �berschrieben werden');
      end;
    end;
  if logopen then begin
    if errmsgs>0 then writeln(logfile);
    writeln(logfile,'Nachrichten gesamt: ',msgs,', davon fehlerhaft: ',errmsgs);
    writeln(logfile);
    writeln(logfile);
    close(logfile);
    end;
  if errfile then close(f3);
end;


{ Header aus f1 ab adr in hdp^ einlesen; ok = Header korrekt   }
{                                                              }
{ minstdh: so viele der 7 Pflichtzeilen m�ssen vorhanden sein, }
{ damit der Header als korrekt erkannt wird.                   }

procedure ReadHeader(adr:longint; var hdp:header; minstdh:byte; var ok:boolean);
var bufanz,
    bufpos  : Integer;
    s       : string;
    p,i     : Integer;
    feld    : string[maxzchdlen];
    stdh    : Integer;
    totallen: longint;    { Zeilenl�nge komplett incl Zeilentrennung }

  procedure ReadBuf;
  begin
    blockread(f1,buf, BufSize,bufanz);
    ior;
    bufpos:=0;
  end;

  {$R-}
  procedure IncO;
  begin
    inc(bufpos);
    inc(totallen);
    if (bufpos=bufanz) and not eof(f1) then begin
      inc(hdp.hds,bufanz);
      ReadBuf;
      { bufpos:=0; Wird in ReadBuf schon gemacht }
      end;
  end;

  procedure GetString;
  begin
    totallen:=0; s := '';
    while (bufpos<bufanz) and (buf[bufpos]<>#13) and (buf[bufpos]<>#10) do
    begin
      s := s + buf[bufpos];
      IncO;
    end;
    if bufpos=bufanz then ok:=false
    else if buf[bufpos]=#10 then begin   { LF statt CR/LF }
      IncO; hdp.lferror:=true;
      end
    else begin
      IncO;
      if bufpos=bufanz then ok:=false    { nur CR am Dateiende }
      else if buf[bufpos]=#10 then IncO
           else hdp.lferror:=true;      { CR statt CR/LF }
      end;
  end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }

begin
  fillchar(hdp, Sizeof(hdp), 0);
  hdp.adr:=adr;
  seek(f1,adr);
  ReadBuf;
  ok:=true;
  with hdp do begin
    repeat
      GetString;
      p:=cpos(':',s);
      if p>1 then begin
        feld:=UpperCase(LeftStr(s,p-1));
        if (fldanz<maxhdlines) or (feld='LEN') then begin
          if fldanz<maxhdlines then inc(fldanz);
          fldsize[fldanz]:=totallen;
          fld[fldanz]:=s;
          inc(p);
          while (p<=length(s)) and ((s[p]=' ') or (s[p]=#9)) do inc(p);
          if (feld='LEN') and (lenpos=0) then begin
            groesse:=ival(mid(s,p)); lenpos:=fldanz; end
          else
            if (feld='MID') and (msgid='') then msgid:=mid(s,p)
          else
            if (feld='X-XP-NTP') then xpnt:=minmax(ival(mid(s,p)),0,255);
          if xpnt=40 then xpnt:=0;     { UUCP/RFC komplett �berpr�fen }
          contpos[fldanz]:=p;
          i:=1;
          while (i<=knownheaders) and (feld<>headerindex[i].name) do inc(i);
          if i<=knownheaders then begin               { !! bin�r suchen }
            fldtype[fldanz]:=i;
            inc(hdfound[i]);
            end;
          end;
        end
      else
        if s<>'' then ok:=false;
    until (s='') or not ok or (bufpos=bufanz);
    inc(hds,bufpos);
    if hdfound[stdhdindex[1]]=0 then
      ok:=false                           { kein LEN: }
    else begin
      stdh:=0;
      for i:=1 to stdhdlines do
        if hdfound[stdhdindex[i]]>0 then
          inc(stdh);
      if stdh<minstdh then ok:=false;
    end;
  end;
end;


procedure WriteHeader(var f:file; var hdp:header);
const bufs  = 1024;
type  charr = array[0..bufs-1] of char;
var i,j : integer;
    s   : string;
    fp  : longint;  { Offest des akt. Feldes im Header }
    buf : ^charr;
    size: longint;
    rr  : Integer;
begin
  new(buf);
  fp:=0;
  with hdp do
    for i:=1 to fldanz do
      if fld[i]<>'' then begin                      { langes Feld:         }
        if (length(fld[i])=253) and (fldsize[i]>255) then begin
          blockwrite(f,fld[i][1],contpos[i]-1);     { Headername schreiben }
          seek(f1,adr+fp+contpos[i]-1);
          size:=max(0,min(8192,fldsize[i]-2)-(contpos[i]-1));
          while size>0 do begin                     { Rest kopieren }
            blockread(f1,buf^,min(size,bufs),rr);
            for j:=0 to rr-1 do
              if (buf^[j]<' ') and (buf^[j]<>#9) then
                buf^[j]:=' ';                   { Steuerzeichen wegwerfen }
            blockwrite(f,buf^,rr);
            dec(size,rr);
            end;
          s:=#13#10;
          blockwrite(f,s[1],2);
          end
        else begin
          s:=fld[i]+#13#10;                   { Feld <= 253 Zeichen }
          blockwrite(f,s[1],length(s));
          end;
        inc(fp,fldsize[i]);
        end;
  s:=#13#10;
  blockwrite(f,s[1],2);
  iow;
  dispose(buf);
end;


var   proz : byte = 101;

procedure wrproz(adr:longint);
var p2 : byte;
begin
  if prozent and (fsize>0) then
  begin
//    p2:=system.round(iif(adr<=0,0,adr) * 100 div fsize);
    if p2<>proz then begin
      proz:=p2;
      write(#13,proz:4,'%');
      end;
    end;
end;


{ Meldung mit hd0.MessageID auf Bildschirm und in Logfile schreiben }

procedure wr(txt:atext; modi:boolean);
const middl = 39;
begin
  with hd0 do begin
    if prozent then write(#13);
    if newcheckmsg then begin
      if length(msgid)<middl then begin
        write(forms(msgid,middl));
        if ParLogfile then write(logfile,forms(msgid,middl));
        end
      else begin
        writeln(msgid); write(sp(middl));
        if ParLogfile then write(logfile,msgid,#13#10,sp(middl));
        end;
      newcheckmsg:=false;
      end
   else begin
     write(sp(middl));
     if ParLogfile then write(logfile,sp(middl));
     end;
   writeln(txt);
   if ParLogfile then writeln(logfile,txt);
   if modi then modified:=true;
   end;
end;


procedure warnung(txt:atext);
begin
  if not ParNowarn then begin
    wr('Warnung: '+txt,false);
    inc(warnungen);
    end;
end;


procedure SetLen(newlen:longint);    { Nachrichtenl�nge korrigieren }
begin
  with hd0 do
    if newlen<>groesse then begin
      if newlen<groesse then wr('Nachricht '+ww+'gek�rzt',true)
      else wr('Nachricht '+ww+'vergr��ert',true);
      groesse:=newlen;
      fld[LENpos]:=LeftStr(fld[LENpos],contpos[LENpos]-1)+strs(newlen);
      end;
end;


procedure CheckContents;   { hd0^ auf ZCONNET-Konformit�t �berpr�fen }
var i,j  : integer;
    cont : string;
    chok: set of char;
    ampm : byte;    { 1=PM, 2=AM, 3=beides }

  procedure wrehd(n:integer);      { fehlerhafte Zeile ausgeben }
  var s : string[39];
      i : integer;
  begin
    if ParShowHd then begin
      s:=hd0.fld[n];
      for i:=1 to length(s) do if s[i]=#9 then s[i]:=' ';
      wr(LeftStr('('+s+')',39),false);
      end;
  end;

  procedure DateCheck;                   { Datum �berpr�fen }
  var zone    : string[20];
      j,j2    : integer;
      t,m,h,min,sec : shortint;
      t2,m2,h2,
      min2,sec2     : shortint;
      res    : integer;
      res2   : integer;
      p      : Integer;
      zh,zm  : longint;
  begin
    zone:=copy(cont,15,20);
    truncstr(cont,14);
    val(copy(cont,1,4),j,res);         { linken Teil �berpr�fen }
    val(copy(cont,5,2),m,res);
    val(copy(cont,7,2),t,res);
    val(copy(cont,9,2),h,res);
    val(copy(cont,11,2),min,res);
    val(copy(cont,13,2),sec,res);
    j2:=minmax(j,1980,2099); m2:=minmax(m,1,12); t2:=minmax(t,1,31);
    h2:=minmax(h,0,23); min2:=minmax(min,0,59); sec2:=minmax(sec,0,59);
    if (j<>j2) or (m<>m2) or (t<>t2) or (h<>h2) or (min<>min2) or (sec<>sec2)
    then
      cont:=formi(j,4)+formi(m,2)+formi(t,2)+formi(h,2)+formi(min,2)+formi(sec,2);

    if length(zone)<3 then zone:='W+0'   { rechten Teil �berpr�fen }
    else begin
      if (zone[1]<>'S') and (zone[1]<>'W') then zone[1]:='W';
      if (zone[2]<>'+') and (zone[2]<>'-') then zone[2]:='+';
      p:=cpos(':',zone);
      if p=0 then begin
        if not IsIntVal(mid(zone,3)) then
          zone:=LeftStr(zone,2)+'0';
      end else begin
        val(copy(zone,3,p-3),zh,res);
        val(mid(zone,p+1),zm,res2);
        if res+res2<>0 then
          zone:=LeftStr(zone,2)+strs(zh)+':'+formi(zm,2);
        end;
     end;

   with hd0 do
     if cont+zone<>mid(fld[i],contpos[i]) then begin
       wr('Datum '+ww+'korrigiert',true);
       wrehd(i);
       fld[i]:=LeftStr(fld[i],contpos[i]-1)+cont+zone;
       end;
  end;

  procedure AdrCheck(_xpnt:boolean);     { Useradresse �berpr�fen }
  var p1: integer;
  begin
    p1:=pos(' (',cont);
    if (p1>1) and (cont[p1-1]=' ') then begin   { zuviele Leerz. vor Realname }
      wr('Leerzeichen '+ww+'aus '+headerindex[hd0.fldtype[i]].name+' entfernt',true);
      wrehd(i);
      with hd0 do
        fld[i]:=LeftStr(fld[i],contpos[i]-1)+trim(LeftStr(cont,p1-1))+mid(cont,p1);
      end;
    if not _xpnt then 
    begin
      if p1>0 then TruncStr(cont,p1-1);
      if not IsMailAddress(cont) then
      begin
        warnung('Fehler in '+headerindex[hd0.fldtype[i]].name);
        wrehd(i);
        end;
      end;
  end;

  procedure CheckEmpfEmpty;
{  var
    DummyI: Integer; }
  begin
    if cont='' then begin
      wr('Leerer Empf�nger '+ww+'entfernt',true);
      with hd0 do begin
        wrehd(i);
        fld[i]:='';
        dec(hdfound[fldtype[i]]);
        end;
      end;
(* N�tzlichkeit dieses FIX wird noch diskutiert, ersteinmal ausgeklammert
{ !!! bei AM wird EMP geupcaset }
    { nach ZC 3.1a _3.3.1.1.3 Brettnamenformat }
    { KE 01/00 }
    if cPos('@',hd0^.fld[i]) = 0 then { keine PM, sondern AM }
    begin;
      for DummyI := 1 to Length(hd0^.fld[i]) do
        if hd0^.fld[i][DummyI] in ['a'..'z'] then
        begin;
          hd0^.fld[i][DummyI] := UpCase(hd0^.fld[i][DummyI]);
          hd0^.modified := true;
        end;
    end; *)
  end;

  procedure BrettCheck(_xpnt:boolean);
  var error : boolean;
      j     : integer;
  begin
    if cont='' then
      CheckEmpfEmpty
    else if LeftStr(cont,length(TO_ID))<>TO_ID then begin
      error:=(cont[1]<>'/');
      if not _xpnt then begin
        j:=2;
        while not error and (j<=length(cont)) do
          if cont[j] in brchar then inc(j)
          else error:=true;
        end;
      if error then begin
        wr('Fehlerhafter Brettname '+ww+'korrigiert',true);
        wrehd(i);
        with hd0 do begin
          j:=contpos[i];
          if cont[1]<>'/' then insert('/',fld[i],j);
          if not _xpnt then    { '/' am Anfang fehlt }
            while j<=length(fld[i]) do
              if fld[i,j] in brchar then inc(j)
              else delete(fld[i],j,1);
          end;
        end;
      end;
  end;

  procedure MidCheck(msgid:boolean);     { MID/BEZ �berpr�fen }
  var p1,p2 : Integer;
  begin
    p1:=cpos('@',cont);
    p2:=cPos('.',mid(cont,p1+1));
    if (p1<=1) or (p2<=1) or (lastchar(cont)='.') then begin
      warnung('Fehler in '+iifs(msgid,'Message-ID','BEZugs-ID'));
      wrehd(i);
      end;
  end;

  procedure FileCheck;                   { FILE �berpr�fen }
  var p : Integer;
  begin
    if multipos('\/:',cont) then begin
      p:=length(cont);
      while (p>0) and not (cont[p] in ['/','\',':']) do dec(p);
      with hd0 do
        if p=length(cont) then begin
          wr('fehlerhafter Dateiname '+ww+'entfernt',true);
          wrehd(i);
          fld[i]:='';
          end
        else begin
          wr('Dateipfad '+ww+'entfernt',true);
          wrehd(i);
          fld[i]:=LeftStr(fld[i],contpos[i]-1)+mid(cont,p+1);
          end;
      end;
  end;

  procedure PrioCheck;                   { PRIO �berpr�fen }
  begin
    if not IsIntVal(cont) then begin
      wr('PRIO '+ww+'korrigiert',true);
      wrehd(i);
      with hd0 do
        fld[i]:=LeftStr(fld[i],contpos[i]-1)+'0';
      end;
  end;

  procedure TeleCheck;                   { TELEFON �berpr�fen }
  var p,j : Integer;
      nr  : string;
      tok : boolean;
  begin
    cont:=LeftStr(trim(cont),254)+' ';
    repeat
      p:=blankpos(cont);
      nr:=LeftStr(cont,p-1);
      cont:=trimleft(mid(cont,p+1));
      while FirstChar(nr) in ['V','F','B','P'] do DeleteFirstChar(nr);
      if FirstChar(nr)<>'+' then
        tok:=false
      else begin
        DeleteFirstChar(nr);
        TrimLastChar(nr, 'Q');
        tok:=true;
        for j:=1 to length(nr) do
          if not (nr[j] in ['0'..'9','-']) then
            tok:=false;
        end;
    until not tok or (cont='');
    if not tok then begin
      warnung('TELEFON: falsches Format');
      wrehd(i);
      end;
  end;

  procedure AddLine(txt:string);         { fehlende Pflichtzeile erg�nzen }
  var hdf : string[15];
  begin
    with hd0 do
      if fldanz<maxhdlines then begin
        hdf:=headerindex[stdhdindex[i]].name;
        wr(hdf+' '+ww+'erg�nzt',true);
        inc(fldanz);
        fld[fldanz]:=hdf+': '+txt;
        contpos[fldanz]:=length(hdf)+2;
        fldtype[fldanz]:=stdhdindex[i];
        fldsize[fldanz]:=length(hdf)+length(txt)+4;
        inc(hdfound[stdhdindex[i]]);
        end;
  end;

  procedure LEN_check;                   { Test auf ung�ltigen LEN-Inhalt }
  var l   : longint;
      res : integer;
      s   : string[1];
  begin
    val(cont,l,res);
    if (res<>0) or (l<0) then begin
      wr('LEN '+ww+'korrigiert',true);
      wrehd(i);
      with hd0 do begin
        if length(fld[i])=4 then s:=#9
        else s:='';
        fld[i] := LeftStr(fld[i],contpos[i]-1) + s + iifs(l<0,'0',strs(ival(cont)));
                     { statt ival(cont) darf nicht l verwendet werden, }
                     { wegen evtl. angeh�ngter Leerzeichen!            }
        end;
      end;
  end;

  procedure KOM_check;
  var l   : longint;
      res : integer;
      s   : string[1];
  begin
    val(cont,l,res);
    if (res<>0) or (l<0) or (l>hd0.groesse) then begin
      wr('KOM '+ww+'korrigiert',true);
      wrehd(i);
      with hd0 do begin
        if length(fld[i])=4 then s:=#9
        else s:='';
        fld[i] := LeftStr(fld[i],contpos[i]-1) + s + '0';
        end;
      end;
  end;

  { Header-Bezeichner auf korrekte Schreibweise testen }

  procedure FldBezCheck(n:integer);
  var i,j  : integer;

      flag : boolean;
  begin
    with hd0 do begin
      i:=1;                 { Test auf Whitespaces vor Header }
      while (i <= Length(fld[n])) and ((fld[n][i]=' ') or (fld[n][i]=#9)) do inc(i);
      flag:=(i>1);
      if flag then begin
        wr('Leerzeichen vor Header '+ww+'entfernt',true);
        wrehd(n);
        delete(fld[n],1,i-1);
        dec(contpos[n],i-1);
        end;
      i:=1;                 { Test auf unerlaubte Zeichen }
      while (i<contpos[n]) and (i <= Length(fld[n])) and (fld[n][i] in kchar) do
        inc(i);
      if (i <= Length(fld[n])) and (fld[n][i]<>':') then
      begin
        wr('ung�ltiger Header '+ww+'entfernt',true);
        wrehd(n);
        fld[n]:='';
        end
      else if flag then                 { ist ein bekannter Header }
        for j:=1 to knownheaders do     { daraus geworden?         }
          if UpperCase(LeftStr(fld[n],i-1))=headerindex[j].name then begin
            fldtype[n]:=j;
            inc(hdfound[j]);
            end;
      end;
  end;

begin
  with hd0 do
  begin
    if lferror then                   { fehlerhafte Zeilentrennungen }
      wr('Zeilentrennungen '+wwn+'korrigiert',true);

    for i:=1 to knownheaders do       { Test auf doppelte Zeilen }
      if not headerindex[i].multi and (hdfound[i]>1) then begin
        wr(strs(hdfound[i]-1)+' mehrfache '+headerindex[i].name+' '+wwnn+'entfernt',true);
        j:=1;
        while (j<=fldanz) and (fldtype[j]<>i) do inc(j);
        inc(j);
        while j<=fldanz do begin
          if fldtype[j]=i then begin
            wrehd(j); fld[j]:=''; end;
          inc(j);
          end;
        end;

    for i:=1 to fldanz do             { Test auf Steuerzeichen }
      if TestControlChar(fld[i]) then
        if ParRep then begin
          if fldtype[i]>0 then
            wr('Steuerzeichen in '+headerindex[fldtype[i]].name+' '+ww+'entfernt',true)
          else
            wr('Steuerzeichen in Headerzeile '+ww+'entfernt',true);
          wrehd(i);
          chok:=[#9,' '..#255];
          j:=1;
          while j<=length(fld[i]) do
            if not (fld[i,j] in chok) then delete(fld[i],j,1)
            else inc(j);
          end
        else begin
          if fldtype[i]>0 then
            warnung('Steuerzeichen in '+headerindex[fldtype[i]].name)
          else
            warnung('Steuerzeichen in Headerzeile');
          wrehd(i);
          end;

    for i:=1 to fldanz do       { �berpr�fung gel�schter Zeilen verhindern }
      if fld[i]='' then fldtype[i]:=0;

    ampm:=0;                    { AM/PM ermitteln }
    for i:=1 to fldanz do
      if fldtype[i]=hdf_EMP then
        if cpos('@',fld[i])>0 then ampm:=ampm or 1
        else ampm:=ampm or 2;
    if ampm=0 then ampm:=2;

    for i:=1 to fldanz do begin                  { AM/PM �berpr�fen }
      cont:=Mid(fld[i],contpos[i]);
      if (fldtype[i]=hdf_CRYPT) and (ampm=2) then begin
        warnung('CRYPT-Zeile in AM');
        wrehd(i);
        end
      else if (fldtype[i]=hdf_PRIO) and (ampm=2) and (cont<>'0') then begin
        wr('PRIO '+ww+'aus AM entfernt',true);
        wrehd(i);
        fld[i]:='';
        end
      else if (fldtype[i]>0) and (ampm and headerindex[fldtype[i]].ampm=0)
      then begin
        wr(headerindex[fldtype[i]].name+' '+ww+'aus '+iifs(ampm=1,'PM','AM')+
           ' entfernt',true);
        fld[i]:='';
        end
      else if fldtype[i]=hdf_LEN then
        LEN_check
      else if fldtype[i]=hdf_KOM then
        KOM_check
      else if (fldtype[i]=0) and (contpos[i]>1) then
        FldBezCheck(i)    { Header-Bezeichner-Syntax �berpr�fen }
      else if (fldtype[i]=hdf_EMP) and not ParExact then
        CheckEmpfEmpty
      else if ParExact then                   { Feldinhalte �berpr�fen }
        case fldtype[i] of
          hdf_EMP, hdf_OEM,
          hdf_DISK             : if cpos('@',cont)>0 then AdrCheck(xpnt<>0)
                                 else BrettCheck(xpnt<>0);
          hdf_KOP              : if FirstChar(cont)='/' then BrettCheck(false)
                                 else AdrCheck(false);
          hdf_EDA, hdf_DDA,
          hdf_OEDA             : DateCheck;
          hdf_ABS, hdf_OAB,
          hdf_WAB, hdf_ANTWORT,
          hdf_VER              : if xpnt=0 then AdrCheck(xpnt<>0);
          hdf_EB               : if cont<>'' then AdrCheck(xpnt<>0);
          hdf_MID              : if xpnt=0 then MidCheck(true);
        { hdf_BEZ              : if xpnt=0 then MidCheck(false); }
          hdf_FILE             : FileCheck;
          hdf_PRIO             : PrioCheck;
          hdf_TELEFON          : TeleCheck;
        end;
      end;

    for i:=1 to stdhdlines do         { fehlende Zeilen erg�nzen }
      if hdfound[stdhdindex[i]]=0 then
        case stdhdindex[i] of
          hdf_EMP : AddLine('/UNZUSTELLBAR');
          hdf_ABS : AddLine('Absender_fehlt@Z.P.R');
          hdf_BET : AddLine('<ZPR: Betreff fehlt>');
          hdf_EDA : AddLine('19800101000000W+0');
          hdf_ROT : AddLine('');
          hdf_MID : AddLine('Message-ID_fehlt@Z.P.R');
        end;
    end;
end;


{ Block von f1 nach f2 kopieren }

procedure fmove(adr,size:longint);
var rr : Integer;
begin
  if not ParRep then exit;
  seek(f1,adr);
  while size>0 do begin
    blockread(f1,buf,min(size,bufsize),rr); ior;
    blockwrite(f2,buf,rr); iow;
    dec(size,rr);
    if size>0 then wrproz(filepos(f1));
    end;
end;


{ Block von f1 nach f3 kopieren }

procedure fmove2(adr,size:longint);
var rr : Integer;
begin
  seek(f1,adr);
  while size>0 do begin
    blockread(f1,buf,min(size,bufsize),rr); ior;
    blockwrite(f3,buf,rr); iow;
    dec(size,rr);
    if size>0 then wrproz(filepos(f1));
    end;
end;


{ n�chsten korrekten Header ab adr suchen }

function SeekHeader(adr:longint):longint;
var ok       : boolean;
    hdlc,i   : integer;    { Zeilenz�hler }
    minpos,p : Integer;
    minfld   : Integer;
begin
  repeat
    wrproz(adr);
    seek(f1,adr);
    ReadHeader(adr,hd1,6,ok);
    if ok then begin    { ung�ltige Zeichen/Zeilen vom Headeranfang entfernen }
      hdlc:=1;
      repeat
        minpos:=255; minfld := 1;
        for i:=1 to knownheaders do
          if i<>hdf_LEN then begin
            p:=pos(headerindex[i].name+':',UpperCase(hd1.fld[hdlc]));
            if (p>0) and (p<minpos) then begin
              minpos:=p; minfld:=i;
              end;
            end;
        if minpos=255 then begin    { Zeile komplett unbrauchbar }
          hd1.fld[hdlc]:='';
          inc(adr,hd1.fldsize[hdlc]);
          hd1.adr:=adr;
          dec(hd1.hds,hd1.fldsize[hdlc]);      { || �nderung bei v1.02 }
          inc(hdlc);
          end
        else begin
          with hd1 do begin
            fld[hdlc]:=mid(fld[hdlc],minpos);
            fldtype[hdlc]:=minfld;
            dec(contpos[hdlc],minpos-1);
            dec(fldsize[hdlc],minpos-1);
            inc(hdfound[minfld]);
            inc(adr,minpos-1); dec(hds,minpos-1);
            end;
          inc(adr,minpos-1);
          ok:=true;
          end;
        inc(hdlc);   { MUSS zu einem Ende kommen, da Standardheaderzeilen }
      until ok;      { korrekt vorhanden sind!                            }
      end
    else
      inc(adr,hd1.hds)
  until ok or (hd1.hds=0);
  if ok then SeekHeader:=adr
  else SeekHeader:=fsize;
end;


procedure CheckRepair;
var ok  : boolean;
    hdt : header;

  procedure Write0msg;
  begin
    if adr0>=0 then
      if hd0.modified then begin
        if errfile then begin
          WriteHeader(f3,hd0);     { vorausgehende Nachricht kopieren }
          fmove2(adr0+hd0.hds,hd0.groesse);
          end
        else if ParRep and not ParKillmsg then begin
          WriteHeader(f2,hd0);
          fmove(adr0+hd0.hds,hd0.groesse);
          end;
        inc(errmsgs);
        end
      else
        fmove(adr0,hd0.hds+hd0.groesse);
  end;

begin
  adr0:=-1; hd0.hds:=1;    { -1 = nicht vorhanden; hds=1 gleicht -1 aus }
  adr1:=0;
  newcheckmsg:=true;
  while adr0<fsize do
  begin
    wrproz(adr0);
    if adr0>=0 then inc(msgs);
    if adr1<=fsize then
    begin
      if adr1=fsize then
        ok:=true
      else
        ReadHeader(adr1,hd1,7,ok);
     if not ok then
      begin
        adr1:=SeekHeader(adr0+hd0.hds);
        if adr0>=0 then SetLen(adr1-adr0-hd0.hds)
        else begin
          hd0.msgid:='';
          if adr1>0 then
            wr('headerloser Text '+ww+'entfernt',false);
          end;
      end;
      Write0msg;
      hdt:=hd0; hd0:=hd1; hd1:=hdt;
      adr0:=adr1;
      inc(adr1,hd0.hds+hd0.groesse);
      newcheckmsg:=true;
      if adr0<fsize then CheckContents;
      end
    else begin     { letzte Nachricht k�rzen }
      SetLen(fsize-adr0-hd0.hds);
      Write0msg;
      adr0:=fsize;
      end;
    end;
  if prozent then write(#13'      '#13);
end;


procedure statistik;
begin
  if errmsgs+warnungen>0 then writeln;
  writeln('Nachrichten gesamt: ',msgs:6);
  writeln('davon fehlerhaft:   ',errmsgs:6);
  if unzustmsgs>0 then
    writeln('headerlose Texte:   ',unzustmsgs:6);
end;

procedure StartCommandlineZPR;
begin
  Logo;
  writeln('ZPR - ZCONNECT(R)-Pufferreparierer - Freeware');
  writeln('(c) 1994-96 by Peter Mandrella <p.mandrella@ldb.han.de>');
  writeln;
  getpar;
  checkpar;
  initvar;
  openfiles;
  CheckRepair;
  closefiles;
  statistik;
  halt(sgn(errmsgs));
end;

{
  $Log: zpr.pas,v $
  Revision 1.56  2004/01/17 16:33:50  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

  Revision 1.55  2003/10/18 17:14:51  mk
  - persistent open database boxenfile (DB: boxbase)

  Revision 1.54  2003/08/24 21:43:40  mk
    - simplified and corrected FileMode Handling (now uses OS dependend
      constants instead of hard coded values, this may prevent problems
      with linux and other OS)

  Revision 1.53  2002/12/14 07:31:42  dodi
  - using new types

  Revision 1.52  2002/12/12 11:58:54  dodi
  - set $WRITEABLECONT OFF

  Revision 1.51  2002/07/25 20:43:58  ma
  - updated copyright notices

  Revision 1.50  2002/05/20 07:47:58  mk
  - fixed backup extension: now ExtBak and EditorExtBak

  Revision 1.49  2002/05/05 22:47:20  mk
  - use correct case for 'bak' extension

  Revision 1.48  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.47  2001/12/22 16:39:56  mk
  - show help with "openxp zpr"

  Revision 1.46  2001/12/09 13:01:00  mk
  - fixed range check error

  Revision 1.45  2001/12/05 11:18:31  mk
  - use BufSize instead of numeric constant in blockreaad

  Revision 1.44  2001/12/04 22:47:49  mk
  - fixed range check error in ReadHeader

  Revision 1.43  2001/10/20 17:26:44  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.42  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.41  2001/09/08 16:29:43  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.40  2001/09/07 17:27:24  mk
  - Kylix compatiblity update

  Revision 1.39  2001/09/07 13:54:26  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.38  2001/09/07 02:07:44  mk
  - use IsMailAddress when possilbe, removed duplicate code

  Revision 1.37  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.36  2001/08/11 23:06:40  mk
  - changed Pos() to cPos() when possible

  Revision 1.35  2001/07/31 16:18:42  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.34  2001/07/31 13:10:36  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.33  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.32  2001/01/11 13:07:39  mk
  - fixed two ansistring bugs

  Revision 1.31  2000/12/31 12:49:08  mk
  - integrated zpr in openxp

  Revision 1.30  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.29  2000/10/20 08:00:38  mk
  - outputredirected auf SysOutputRedirected umgestellt

  Revision 1.28  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.27  2000/10/17 10:06:02  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.26  2000/10/04 15:39:09  mk
  - Range-Check-Error in FldBezCheck beseitigt

  Revision 1.25  2000/09/25 18:53:25  mk
  - jetzt auf Ansistring portiert

  Revision 1.24  2000/07/23 10:00:14  mk
  - ZPR compiliert wieder

  Revision 1.23  2000/07/21 17:39:58  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.22  2000/07/20 16:50:00  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.21  2000/07/09 09:09:56  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.20  2000/07/09 08:35:20  mk
  - AnsiStrings Updates

  Revision 1.19  2000/07/04 12:04:33  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.18  2000/07/04 09:59:04  mk
  - Sysutils eingefuegt

  Revision 1.17  2000/07/03 16:20:04  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.16  2000/06/29 13:01:03  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.15  2000/06/23 15:59:27  mk
  - 16 Bit Teile entfernt

  Revision 1.14  2000/05/05 15:27:57  ml
  zpr und uuz wieder unter linux lauff�hig (ncrt)

  Revision 1.13  2000/05/02 19:14:04  hd
  xpcurses statt crt in den Units

  Revision 1.12  2000/04/30 21:03:35  mk
  - kein crt noetig

  Revision 1.11  2000/04/13 12:48:42  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.10  2000/04/04 21:01:24  mk
  - Bugfixes f�r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.9  2000/03/26 11:04:10  ml
  zpr-Anzeige in linux geht jetzt

  Revision 1.8  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.7  2000/03/17 11:16:35  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.6  2000/02/26 16:01:09  ml
  Carriage Return in linux nicht noetig

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.4  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ ZPR - ZCONNECT-Pufferreparierer }
{ PM 08/93, 10/93                 }

{$I XPDEFINE.INC }

{$M 16384,65536,65536}

uses
  dos, typeform, dosx, xpglobal, lfn;

const maxhdlines  = 120;    { max. ausgewertete Headerzeilen pro Nachricht }
      bufsize     = 16384;  { Grî·e Kopier/Einlesepuffer                   }
      maxzchdlen  = 100;    { max. erlaubte LÑnge von Headernamen          }
      knownheaders= 24;     { Headerzeilen, deren Syntax bekannt ist       }
      stdhdlines  = 7;      { Anzahl Pflichtheaderzeilen                   }
      TO_ID       = '/'#0#0#8#8'TO:';

      paramchars   = ['-','/'];

      logfilename = 'ZPR.LOG';

      ReadFilemode  = 2;
      WriteFilemode = 2;

type
  PathStr = string;          { Full file path string }
  DirStr  = string;          { Drive and directory string }
  NameStr = string;          { File name string }
  ExtStr  = string;          { File extension string }

const ParLogfile  : boolean = false;    { -l Logfile anlegen                }
      ParKillmsg  : boolean = false;    { -l defekte Nachrichten lîschen    }
      ParRep      : boolean = false;    { -r Puffer reparieren              }
      ParExact    : boolean = false;    { -h strenge Headerzeilen-öberprÅf. }
      ParNowarn   : boolean = false;    { -w Warnungen unterdrÅcken         }
      ParShowhd   : boolean = false;    { -z fehlerhafte Zeilen anzeigen    }

      fi          : pathstr = '';       { Name der Eingabedatei }
      fo          : pathstr = '';       { Name der Ausgabedatei }
      ferr        : pathstr = '';       { Name der Fehlerdatei  }
      errfile     : boolean = false;    { Fehlerdatei geîffnet  }
      prozent     : boolean = true;     { fortlaufende Prozentanzeige }
      logopen     : boolean = false;

      { Liste aller Headerzeilen, die in irgendeiner Form ÅberprÅft werden. }
      { Alle Åbrigen Zeilen werden nicht interpretiert.                     }

      headerindex : array[1..knownheaders] of
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

      hdf_ABS     = 1;    hdf_EB   = 8;      hdf_OAB     = 15;
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
                    hds     : longint;    { Grî·e inc. CR/LF am Ende }
                    fldanz  : integer;    { Anzahl eingelesener Felder }
                    fld     : array[1..maxhdlines] of string;   { Felder }
                    contpos : array[1..maxhdlines] of byte;   { Startoffst des Feldinhalts }
                    fldtype : array[1..maxhdlines] of byte;   { Headerindex-Nr. }
                    fldsize : array[1..maxhdlines] of longint;  { phys. FeldlÑnge }
                    hdfound : array[1..knownheaders] of integer;
                    lferror : boolean;    { falsche Zeilentrennung }
                    groesse : longint;    { LEN }
                    LENpos  : integer;    { Position des LEN-Headers }
                    msgid   : string;     { MID }
                    modified: boolean;    { Header wurde korrigiert }
                    XPnt    : byte;       { X-XP-NTP }
                  end;
      headerp   = ^header;


var   oldexit   : pointer;
      buf       : array[0..bufsize-1] of char;  { allg. Einlese/Kopierpuffer }
      hd0,hd1   : headerp;
      f1,f2,f3  : file;       { Ein/Ausgabedatei, Fehlerdatei }
      logfile   : text;
      fsize     : longint;    { Grî·e der Eingabedatei }
      adr0      : longint;    { Startadresse des vorausgehenden Headers }
      adr1      : longint;    { Startadresse des aktuellen Headers }

      msgs      : longint;    { Anzahl Nachrichten gesamt }
      errmsgs   : longint;    { davon fehlerhaft          }
      warnungen : longint;    { Anzahl Warnungen          }
      unzustmsgs: longint;    { Anzahl headerlose Texte   }
      ww,wwn    : string[10]; { 'wÅrde(n)'                }
      wwnn      : string[10];
      newcheckmsg:boolean;

      kchar     : set of char;  { in Header-Bezeichnern erlaubte Zeichen }
      brchar    : set of char;  { in Brettnamen erlaubte Zeichen         }

function  TestControlChar(var s:string):boolean; assembler;
asm
         push ds
         lds   si,s
         cld
         lodsb
         mov   cl,al
         mov   ch,0                     { true }
         xor   bx,bx
         jcxz  @tcende
@tclp:   lodsb
         cmp   al,' '
         jae   @tok
         cmp   al,9
         je    @tok
         inc   bx                      { false }
         jmp   @tcende
@tok:    loop  @tclp
@tcende: mov   ax,bx                   { Funktionsergebnis }
         pop ds
end;

procedure logo;
begin
  writeln;
  writeln('ZPR - ZCONNECT(R)-Pufferreparierer - Freeware');
  writeln('(c) 1994-96 by Peter Mandrella <p.mandrella@ldb.han.de>');
  Writeln;
  writeln('OpenXP-Version ',verstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;

procedure helppage;
const crlf = #13#10;
begin
   writeln('Syntax:    ZPR [Schalter] <Quelldatei> [Zieldatei]'+crlf+
          crlf,
          'Schalter:  -f   Fehler in ZPR.LOG aufzeichnen'+crlf+
          '           -h   strenge Headerzeilen-öberprÅfung'+crlf,
          '           -l   defekte Nachrichten lîschen'+crlf+
          '           -r   Puffer reparieren'+crlf+
          '           -w   Warnungen unterdrÅcken'+crlf,
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
    writeln(logfile,'* öberprÅfung abgebrochen: ',txt);
    writeln(logfile);
    close(logfile);
    end;
  halt(2);
end;

function ioerror(i:integer; otxt:atext):atext;
begin
  case i of
      2 : ioerror:='Datei nicht gefunden';
      3 : ioerror:='ungÅltiges Verzeichnis';
      4 : ioerror:='zu viele Dateien geîffnet (bitte FILES erhîhen!)';
      5 : ioerror:='Zugriff verweigert';
      7 : ioerror:='Speicherverwaltung zerstîrt';
      8 : ioerror:='ungenÅgend Speicher';
     10 : ioerror:='ungÅltiges Environment';
     11 : ioerror:='ungÅltiges Aufruf-Format';
     15 : ioerror:='ungÅltige Laufwerksbezeichnung';
     16 : ioerror:='Verzeichnis kann nicht gelîscht werden';
     18 : ioerror:='zu wenig FILES (CONFIG.SYS)';
    101 : ioerror:='Diskette/Platte voll';
    150 : ioerror:='Diskette ist schreibgeschÅtzt';
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
    s   : string[127];
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
      ferr:=UStr(mid(s,j));
      j:=length(s)+1;
      end;
  end;

begin
  if paramcount=0 then helppage;
  err:=false;
  for i:=1 to paramcount do begin
    s:=trim(paramstr(i));
    if length(left(s,1)) > 0 then
    begin    { ML 13.02.2000 ‹berpr¸fung nun mit paramchars }
      if s[1] in paramchars then begin
      delete(s,1,1);
      s:=lstr(s);
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
      UpString(s);
      if fi='' then fi:=s
      else if fo='' then begin
        fo:=s; ParRep:=true; end
      else error('ÅberflÅssiger Parameter: "'+s+'"');
      end;
    end;
  end;
  if err then halt(2);
end;

Function ValidFileName(name:PathStr):boolean;
var f : file;
begin
  if (name='') or
     (cpos('*',name)+cpos('?',name)>0) then    { Fehler in DR-DOS 5.0 umgehen }
    ValidFileName:=false
  else begin
    assign(f,name);
    filemode:=ReadFilemode;
    reset(f,1);
    if ioresult=0 then
      ValidFilename:=true
    else begin
      filemode:=WriteFilemode;
      rewrite(f);
      close(f);
      erase(f);
      ValidFileName:=(ioresult=0);
      end;
    end;
end;

Procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
    dir     : dirstr;
    name    : namestr;
    ext     : extstr;
begin
  assign(f,n);
  filemode:=ReadFilemode;
  reset(f,1); if ioresult<>0 then exit;
  close(f);
  fsplit(n,dir,name,ext);
  bakname:=dir+name+'.'+newext;
  assign(f,bakname);
  setfattr(f,archive);  { evtl. altes BAK lîschen }
  erase(f);
  assign(f,n);
  setfattr(f,archive);
  rename(f,bakname);
  if ioresult<>0 then;
end;


procedure checkpar;
begin
  if fi='' then error('keine Quelldatei angegeben');
  filemode:=ReadFilemode;
  assign(f1,fi); reset(f1,1); close(f1);
  if ioresult<>0 then
    error('Datei "'+fi+'" nicht vorhanden oder nicht lesbar.');
  if (fo<>'') and not validfilename(fo) then
    error('ungÅltige Zieldatei: "'+fo+'"');
  if ferr<>'' then begin
    errfile:=true;
    if not validfilename(ferr) then
      error('ungÅltige Fehlerausgabedatei: "'+ferr+'"');
    end;
  if FExpand(fi)=FExpand(fo) then fo:='';
  if ParRep and (pos('MPUFFER.',fi)>0) and ((fo='') or (pos('MPUFFER.',fo)>0))
  then
    error('CrossPoint-MPUFFER-Dateien dÅrfen nicht direkt modifiziert werden!');
end;

{$F+}
procedure newexit;
begin
  if (right(fo,3)='$$$') then begin     { evtl. Tempfile lîschen }
    assign(f2,fo);
    erase(f2);
    if ioresult<>0 then;
    end;
  exitproc:=oldexit;
end;
{$F-}

procedure openfiles;
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
begin
  if ParRep then begin
    if fo='' then begin
      fsplit(fi,dir,name,ext);
      fo:=dir+name+'.$$$';
      end
    else
      makebak(fo,'BAK');
    filemode:=WriteFilemode;
    assign(f2,fo); rewrite(f2,1);
    if ioresult<>0 then error('Kann TemporÑrdatei "'+fo+'" nicht îffnen.');
    end;
  oldexit:=exitproc;
  exitproc:=@newexit;
  filemode:=ReadFilemode;
  assign(f1,fi); reset(f1,1);
  fsize:=filesize(f1);
  writeln('Eingabedatei: ',fi);
  write('Ausgabedatei: ');
  if not ParRep then
    writeln('-keine-')
  else
    if right(fo,3)='$$$' then writeln(fi)
    else writeln(fo);
  if errfile then
    writeln('Fehlerdatei:  ',ferr);

  writeln;
  if ParLogfile then begin
    assign(logfile,logfilename);
    filemode:=WriteFilemode;
    append(logfile);
    if ioresult<>0 then rewrite(logfile);
    if ioresult<>0 then error('Kann '+logfilename+' nicht îffnen.');
    logopen:=true;
    writeln(logfile,date,'/',time,':  ÅberprÅfe ',fi);
    writeln(logfile);
    end;
  if errfile then begin
    filemode:=WriteFilemode;
    assign(f3,ferr); rewrite(f3,1);
    if ioresult<>0 then
      error('Kann Fehlerausgabedatei "'+ferr+'" nicht îffnen.');
    end;
end;


procedure initvar;
begin
  msgs:=0; errmsgs:=0;
  warnungen:=0; unzustmsgs:=0;
  new(hd0); new(hd1);
  if not ParRep then begin
    ww:='wÅrde '; wwn:='wÅrden '; wwnn:='wÅrde(n) '; end
  else begin
    ww:=''; wwn:=''; wwnn:=''; end;
  prozent:=not outputredirected;
  kchar:=['A'..'Z','a'..'z','0'..'9','-','_'];
  brchar:=kchar + ['/','!','+'];
  brchar:=brchar + ['.','-'];     { eigentlich nicht erlaubt, aber Åblich }
end;

procedure closefiles;
begin
  close(f1);
  if ParRep then begin
    close(f2);
    if right(fo,3)='$$$' then begin
      makebak(fi,'BAK');
      rename(f2,fi);
      if ioresult<>0 then error('"'+fi+'" konnte nicht Åberschrieben werden');
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
{ minstdh: so viele der 7 Pflichtzeilen mÅssen vorhanden sein, }
{ damit der Header als korrekt erkannt wird.                   }

procedure ReadHeader(adr:longint; hdp:headerp; minstdh:byte; var ok:boolean);
var bufanz,
    bufpos  : word;
    s       : string;
    p,i     : byte;
    feld    : string[maxzchdlen];
    stdh    : byte;
    totallen: longint;    { ZeilenlÑnge komplett incl Zeilentrennung }

  procedure ReadBuf;
  begin
    blockread(f1,buf,2048,bufanz);
    ior;
    bufpos:=0;
  end;

  {$R-}
  procedure IncO;
  begin
    inc(bufpos);
    inc(totallen);
    if (bufpos=bufanz) and not eof(f1) then begin
      inc(hdp^.hds,bufanz);
      ReadBuf;
      { bufpos:=0; Wird in ReadBuf schon gemacht }
      end;
  end;

  procedure GetString;
  var len : byte;
  begin
    len:=0;
    totallen:=0;
    while (bufpos<bufanz) and (buf[bufpos]<>#13) and (buf[bufpos]<>#10) do begin
      if len<253 then begin
        inc(len); s[len]:=buf[bufpos];
        end;
      IncO;
      end;
    s[0]:=chr(len);
    if bufpos=bufanz then ok:=false
    else if buf[bufpos]=#10 then begin   { LF statt CR/LF }
      IncO; hdp^.lferror:=true;
      end
    else begin
      IncO;
      if bufpos=bufanz then ok:=false    { nur CR am Dateiende }
      else if buf[bufpos]=#10 then IncO
           else hdp^.lferror:=true;      { CR statt CR/LF }
      end;
  end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }

begin
  fillchar(hdp^,sizeof(hdp^),0);
  hdp^.adr:=adr;
  seek(f1,adr);
  ReadBuf;
  ok:=true;
  with hdp^ do begin
    repeat
      GetString;
      p:=cpos(':',s);
      if p>1 then begin
        feld:=ustr(left(s,p-1));
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
          if xpnt=40 then xpnt:=0;     { UUCP/RFC komplett ÅberprÅfen }
          contpos[fldanz]:=p;
          i:=1;
          while (i<=knownheaders) and (feld<>headerindex[i].name) do inc(i);
          if i<=knownheaders then begin               { !! binÑr suchen }
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
    else
    begin
      stdh:=0;
      for i:=1 to stdhdlines do
        if hdfound[stdhdindex[i]]>0 then
          inc(stdh);
      end;
      if stdh<minstdh then ok:=false;
    end;
end;


procedure WriteHeader(var f:file; hdp:headerp);
const bufs  = 1024;
type  charr = array[0..bufs-1] of char;
var i,j : integer;
    s   : string;
    fp  : longint;  { Offest des akt. Feldes im Header }
    buf : ^charr;
    size: longint;
    rr  : word;
begin
  new(buf);
  fp:=0;
  with hdp^ do
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


procedure wrproz(adr:longint);
const proz : byte = 101;
var p2 : byte;
begin
  if prozent and (fsize>0) then
  begin
    p2:=system.round(iif(adr<=0,0,adr) / fsize*100);
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
  with hd0^ do begin
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


procedure SetLen(newlen:longint);    { NachrichtenlÑnge korrigieren }
begin
  with hd0^ do
    if newlen<>groesse then begin
      if newlen<groesse then wr('Nachricht '+ww+'gekÅrzt',true)
      else wr('Nachricht '+ww+'vergrî·ert',true);
      groesse:=newlen;
      fld[LENpos]:=left(fld[LENpos],contpos[LENpos]-1)+strs(newlen);
      end;
end;


procedure CheckContents;   { hd0^ auf ZCONNET-KonformitÑt ÅberprÅfen }
var i,j  : integer;
    cont : string;
    chok: set of char;
    ampm : byte;    { 1=PM, 2=AM, 3=beides }

  procedure wrehd(n:integer);      { fehlerhafte Zeile ausgeben }
  var s : string[39];
      i : integer;
  begin
    if ParShowHd then begin
      s:=hd0^.fld[n];
      for i:=1 to length(s) do if s[i]=#9 then s[i]:=' ';
      wr(left('('+s+')',39),false);
      end;
  end;

  procedure DateCheck;                   { Datum ÅberprÅfen }
  var zone    : string[20];
      j,j2    : integer;
      t,m,h,min,sec : shortint;
      t2,m2,h2,
      min2,sec2     : shortint;
      res    : integer;
      res2   : integer;
      p      : byte;
      zh,zm  : longint;
  begin
    zone:=copy(cont,15,20);
    truncstr(cont,14);
    val(copy(cont,1,4),j,res);         { linken Teil ÅberprÅfen }
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

    if length(zone)<3 then zone:='W+0'   { rechten Teil ÅberprÅfen }
    else begin
      if (zone[1]<>'S') and (zone[1]<>'W') then zone[1]:='W';
      if (zone[2]<>'+') and (zone[2]<>'-') then zone[2]:='+';
      p:=cpos(':',zone);
      if p=0 then begin
        val(mid(zone,3),zh,res);
        if res<>0 then zone:=left(zone,2)+'0';
        end
      else begin
        val(copy(zone,3,p-3),zh,res);
        val(mid(zone,p+1),zm,res2);
        if res+res2<>0 then
          zone:=left(zone,2)+strs(zh)+':'+formi(zm,2);
        end;
     end;

   with hd0^ do
     if cont+zone<>mid(fld[i],contpos[i]) then begin
       wr('Datum '+ww+'korrigiert',true);
       wrehd(i);
       fld[i]:=left(fld[i],contpos[i]-1)+cont+zone;
       end;
  end;

  procedure AdrCheck(_xpnt:boolean);     { Useradresse ÅberprÅfen }
  var p1,p2 : byte;
  begin
    p1:=pos(' (',cont);
    if (p1>1) and (cont[p1-1]=' ') then begin   { zuviele Leerz. vor Realname }
      wr('Leerzeichen '+ww+'aus '+headerindex[hd0^.fldtype[i]].name+' entfernt',true);
      wrehd(i);
      with hd0^ do
        fld[i]:=left(fld[i],contpos[i]-1)+trim(left(cont,p1-1))+mid(cont,p1);
      end;
    if not _xpnt then begin
      if p1>0 then TruncStr(cont,p1-1);
      p1:=cpos('@',cont);
      p2:=cpos('.',mid(cont,p1+1));
      if (p1<=1) or (p2<=1) or (lastchar(cont)='.') then begin
        warnung('Fehler in '+headerindex[hd0^.fldtype[i]].name);
        wrehd(i);
        end;
      end;
  end;

  procedure CheckEmpfEmpty;
{  var
    DummyI: Integer; }
  begin
    if cont='' then begin
      wr('Leerer EmpfÑnger '+ww+'entfernt',true);
      with hd0^ do begin
        wrehd(i);
        fld[i]:='';
        dec(hdfound[fldtype[i]]);
        end;
      end;
(* NÅtzlichkeit dieses FIX wird noch diskutiert, ersteinmal ausgeklammert
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
    else if left(cont,length(TO_ID))<>TO_ID then begin
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
        with hd0^ do begin
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

  procedure MidCheck(msgid:boolean);     { MID/BEZ ÅberprÅfen }
  var p1,p2 : byte;
  begin
    p1:=cpos('@',cont);
    p2:=cpos('.',mid(cont,p1+1));
    if (p1<=1) or (p2<=1) or (lastchar(cont)='.') then begin
      warnung('Fehler in '+iifs(msgid,'Message-ID','BEZugs-ID'));
      wrehd(i);
      end;
  end;

  procedure FileCheck;                   { FILE ÅberprÅfen }
  var p : byte;
  begin
    if multipos('\/:',cont) then begin
      p:=length(cont);
      while (p>0) and not (cont[p] in ['/','\',':']) do dec(p);
      with hd0^ do
        if p=length(cont) then begin
          wr('fehlerhafter Dateiname '+ww+'entfernt',true);
          wrehd(i);
          fld[i]:='';
          end
        else begin
          wr('Dateipfad '+ww+'entfernt',true);
          wrehd(i);
          fld[i]:=left(fld[i],contpos[i]-1)+mid(cont,p+1);
          end;
      end;
  end;

  procedure PrioCheck;                   { PRIO ÅberprÅfen }
  var res : integer;
      l   : longint;
  begin
    val(cont,l,res);
    if res<>0 then begin
      wr('PRIO '+ww+'korrigiert',true);
      wrehd(i);
      with hd0^ do
        fld[i]:=left(fld[i],contpos[i]-1)+'0';
      end;
  end;

  procedure TeleCheck;                   { TELEFON ÅberprÅfen }
  var p,j : byte;
      nr  : string[80];
      tok : boolean;
  begin
    cont:=left(trim(cont),254)+' ';
    repeat
      p:=blankpos(cont);
      nr:=left(cont,p-1);
      cont:=ltrim(mid(cont,p+1));
      while (nr<>'') and (nr[1] in ['V','F','B','P']) do delfirst(nr);
      if nr[1]<>'+' then
        tok:=false
      else begin
        delfirst(nr);
        if right(nr,1)='Q' then dellast(nr);
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

  procedure AddLine(txt:string);         { fehlende Pflichtzeile ergÑnzen }
  var hdf : string[15];
  begin
    with hd0^ do
      if fldanz<maxhdlines then begin
        hdf:=headerindex[stdhdindex[i]].name;
        wr(hdf+' '+ww+'ergÑnzt',true);
        inc(fldanz);
        fld[fldanz]:=hdf+': '+txt;
        contpos[fldanz]:=length(hdf)+2;
        fldtype[fldanz]:=stdhdindex[i];
        fldsize[fldanz]:=length(hdf)+length(txt)+4;
        inc(hdfound[stdhdindex[i]]);
        end;
  end;

  procedure LEN_check;                   { Test auf ungÅltigen LEN-Inhalt }
  var l   : longint;
      res : integer;
      s   : string[1];
  begin
    val(cont,l,res);
    if (res<>0) or (l<0) then begin
      wr('LEN '+ww+'korrigiert',true);
      wrehd(i);
      with hd0^ do begin
        if length(fld[i])=4 then s:=#9
        else s:='';
        fld[i] := left(fld[i],contpos[i]-1) + s + iifs(l<0,'0',strs(ival(cont)));
                     { statt ival(cont) darf nicht l verwendet werden, }
                     { wegen evtl. angehÑngter Leerzeichen!            }
        end;
      end;
  end;

  procedure KOM_check;
  var l   : longint;
      res : integer;
      s   : string[1];
  begin
    val(cont,l,res);
    if (res<>0) or (l<0) or (l>hd0^.groesse) then begin
      wr('KOM '+ww+'korrigiert',true);
      wrehd(i);
      with hd0^ do begin
        if length(fld[i])=4 then s:=#9
        else s:='';
        fld[i] := left(fld[i],contpos[i]-1) + s + '0';
        end;
      end;
  end;

  { Header-Bezeichner auf korrekte Schreibweise testen }

  procedure FldBezCheck(n:integer);
  var i,j  : integer;

      flag : boolean;
  begin
    with hd0^ do begin
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
      while (i<contpos[n]) and (fld[n][i] in kchar) do
        inc(i);
      if fld[n][i]<>':' then begin
        wr('ungÅltiger Header '+ww+'entfernt',true);
        wrehd(n);
        fld[n]:='';
        end
      else if flag then                 { ist ein bekannter Header }
        for j:=1 to knownheaders do     { daraus geworden?         }
          if ustr(left(fld[n],i-1))=headerindex[j].name then begin
            fldtype[n]:=j;
            inc(hdfound[j]);
            end;
      end;
  end;

begin
  with hd0^ do
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

    for i:=1 to fldanz do       { öberprÅfung gelîschter Zeilen verhindern }
      if fld[i]='' then fldtype[i]:=0;

    ampm:=0;                    { AM/PM ermitteln }
    for i:=1 to fldanz do
      if fldtype[i]=hdf_EMP then
        if cpos('@',fld[i])>0 then ampm:=ampm or 1
        else ampm:=ampm or 2;
    if ampm=0 then ampm:=2;

    for i:=1 to fldanz do begin                  { AM/PM ÅberprÅfen }
      cont:=copy(fld[i],contpos[i],255);
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
        FldBezCheck(i)    { Header-Bezeichner-Syntax ÅberprÅfen }
      else if (fldtype[i]=hdf_EMP) and not ParExact then
        CheckEmpfEmpty
      else if ParExact then                   { Feldinhalte ÅberprÅfen }
        case fldtype[i] of
          hdf_EMP, hdf_OEM,
          hdf_DISK             : if cpos('@',cont)>0 then AdrCheck(xpnt<>0)
                                 else BrettCheck(xpnt<>0);
          hdf_KOP              : if left(cont,1)='/' then BrettCheck(false)
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

    for i:=1 to stdhdlines do         { fehlende Zeilen ergÑnzen }
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
var rr : word;
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
var rr : word;
begin
  seek(f1,adr);
  while size>0 do begin
    blockread(f1,buf,min(size,bufsize),rr); ior;
    blockwrite(f3,buf,rr); iow;
    dec(size,rr);
    if size>0 then wrproz(filepos(f1));
    end;
end;


{ nÑchsten korrekten Header ab adr suchen }

function SeekHeader(adr:longint):longint;
var ok       : boolean;
    hdlc,i   : integer;    { ZeilenzÑhler }
    minpos,p : byte;
    minfld   : byte;
begin
  repeat
    wrproz(adr);
    seek(f1,adr);
    ReadHeader(adr,hd1,6,ok);
    if ok then begin    { ungÅltige Zeichen/Zeilen vom Headeranfang entfernen }
      hdlc:=1;
      repeat
        minpos:=255; minfld := 1;
        for i:=1 to knownheaders do
          if i<>hdf_LEN then begin
            p:=pos(headerindex[i].name+':',ustr(hd1^.fld[hdlc]));
            if (p>0) and (p<minpos) then begin
              minpos:=p; minfld:=i;
              end;
            end;
        if minpos=255 then begin    { Zeile komplett unbrauchbar }
          hd1^.fld[hdlc]:='';
          inc(adr,hd1^.fldsize[hdlc]);
          hd1^.adr:=adr;
          dec(hd1^.hds,hd1^.fldsize[hdlc]);      { || énderung bei v1.02 }
          inc(hdlc);
          end
        else begin
          with hd1^ do begin
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
      inc(adr,hd1^.hds)
  until ok or (hd1^.hds=0);
  if ok then SeekHeader:=adr
  else SeekHeader:=fsize;
end;


procedure CheckRepair;
var ok  : boolean;
    hdt : headerp;

  procedure Write0msg;
  begin
    if adr0>=0 then
      if hd0^.modified then begin
        if errfile then begin
          WriteHeader(f3,hd0);     { vorausgehende Nachricht kopieren }
          fmove2(adr0+hd0^.hds,hd0^.groesse);
          end
        else if ParRep and not ParKillmsg then begin
          WriteHeader(f2,hd0);
          fmove(adr0+hd0^.hds,hd0^.groesse);
          end;
        inc(errmsgs);
        end
      else
        fmove(adr0,hd0^.hds+hd0^.groesse);
  end;

begin
  adr0:=-1; hd0^.hds:=1;    { -1 = nicht vorhanden; hds=1 gleicht -1 aus }
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
        adr1:=SeekHeader(adr0+hd0^.hds);
        if adr0>=0 then SetLen(adr1-adr0-hd0^.hds)
        else begin
          hd0^.msgid:='';
          if adr1>0 then
            wr('headerloser Text '+ww+'entfernt',false);
          end;
      end;
      Write0msg;
      hdt:=hd0; hd0:=hd1; hd1:=hdt;
      adr0:=adr1;
      inc(adr1,hd0^.hds+hd0^.groesse);
      newcheckmsg:=true;
      if adr0<fsize then CheckContents;
      end
    else begin     { letzte Nachricht kÅrzen }
      SetLen(fsize-adr0-hd0^.hds);
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


begin
  logo;
  getpar;
  checkpar;
  initvar;
  openfiles;
  CheckRepair;
  closefiles;
  statistik;
  dispose(hd0); dispose(hd1);
  halt(sgn(errmsgs));
end.
{
  $Log$
  Revision 1.14.2.5  2001/08/11 22:18:07  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.14.2.4  2000/10/15 09:28:09  mk
  - LFN fixes

  Revision 1.14.2.3  2000/10/04 15:41:30  mk
  - Range-Check-Error in FldBezCheck beseitigt

  Revision 1.14.2.2  2000/07/02 10:43:01  mk
  - pformstr entfernt

  Revision 1.14.2.1  2000/06/22 17:13:46  mk
  - 32 Bit Teile entfernt

  Revision 1.14  2000/05/05 15:27:57  ml
  zpr und uuz wieder unter linux lauff‰hig (ncrt)

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
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

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

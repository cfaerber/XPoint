{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }


var uunum : word;    { fortlaufende 16-Bit-Nummer der UUCP-Dateien }


function uu_nummer:word;     { nÑchste Paketnummer aus UUNUMMER.DAT lesen }
var t : text;
    s : string;
begin
  if _filesize(UUnumdat)<2 then
    uu_nummer:=1
  else begin
    assign(t,UUnumdat);
    reset(t);
    readln(t,s);
    close(t);
    uu_nummer:=minmax(ival(s),0,$ffff);
    end;
end;

function GetNextUUnummer:word;   { nÑchste Nummer aus C-File auslesen }
var t : text;
    s : string;
    w : word;
begin
  w:=uu_nummer;
  if exist(XFerDir+caller) and (_filesize(XFerDir+caller)>0) then begin
    assign(t,XFerDir+caller);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if LeftStr(s,4)='S D.' then begin
        s:=trim(mid(s,cpos(' ',s)));
        s:=LeftStr(s,cpos(' ',s)-1);
        w:=hexval(RightStr(s,4));
        end;
      end;
    close(t);
    if w=$ffff then w:=0
    else inc(w);
    end;
  GetNextUUnummer:=w;
end;

procedure WriteUUnummer(w:word);    { nÑchste Nr. in UUNUMER.DAT schreiben }
var t : text;
begin
  assign(t,UUnumdat);
  rewrite(t);
  writeln(t,w);
  close(t);
end;


(*
procedure NoUUZ;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(105,30);    { 'Netcall-Konvetierer UUZ.EXE fehlt!' }
  twin;
end; *)

procedure NoUUCICO;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(110,30);    { 'UUCICO.EXE fehlt!' }
  twin;
end;

procedure PackFehler;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(713,30);    { 'Fehler beim Packen!' }
  twin;
end;


{ Puffer in RFC-Files konvertieren }

procedure ZtoRFC(cleardir:boolean; source,destdir:string);
var sr    : searchrec;
    f1,f2 : ^file;
    s     : shortstring;
    p     : byte;
    cunb  : string;
    news  : boolean;
    freeze: boolean;
    gzip  : boolean;
    f     : boolean;
    uu: TUUZ;

 procedure DelAll(fn:string);
 var sr: searchrec;
 begin
    findfirst(DestDir+fn,AnyFile-Directory,sr);
    while doserror=0 do begin
      DeleteFile(DestDir+sr.name);
      findnext(sr);
    end;
    FindClose(sr);
  end;

  procedure NoCompSmtp(w:word);       { rcsmtp -> csmtp }
  var f1,f2 : file;
      s     : string[40];
      p     : byte;
      rr    : word;
      adr   : longint;
  begin
    if w=$ffff then w:=0
    else inc(w);
    assign(f1,DestDir+'X-'+hex(w,4)+'.OUT');
    if existf(f1) then begin
      reset(f1,1);
      adr:=0;
      assign(f2,DestDir+'smtp.tmp');
      rewrite(f2,1);
      repeat
        seek(f1,adr);
        blockread(f1,s[1],40,rr);
        s[0]:=chr(rr);
        p:=cpos(#10,s);
        s[0]:=chr(p-1);
        inc(adr,p);
        if (s='C rcsmtp') or (s='C rfsmtp') or (s='C rgsmtp') or (c='rzsmtp')
        then
          s:='C rsmtp';
        s:=s+#10;
        blockwrite(f2,s[1],length(s));
      until adr>=filesize(f1);
      close(f1);
      close(f2);
      erase(f1);
      rename(f2,DestDir+'X-'+hex(w,4)+'.OUT');
      end;
  end;

  function filesum(fmask:string):longint;
  var sum : longint;
      sr  : searchrec;
  begin
    sum:=0;
    findfirst(fmask,ffAnyFile,sr);
    while doserror=0 do begin
      inc(sum,sr.size);
      findnext(sr);
    end;
    FindClose(sr);
    filesum:=sum;
  end;

begin
  if cleardir then begin                { Spool rÑumen }
    DelAll('*.');
    DelAll('*.OUT');
  end;
  spacksize:=0;
  spufsize:=0;
  MakeMimetypCfg;
  with boxpar^ do begin
    uu := TUUZ.Create;
    if SizeNego then uu.parsize := true;
    if UUsmtp then
      if UpArcer='' then uu.SMTP := true
      else if pos('freeze',LowerCase(uparcer))>0 then uu.fSMTP := true
      else if pos('gzip',LowerCase(uparcer))>0 then uu.zSMTP := true
      else uu.cSMTP := true;
    if NewsMIME then uu.NewsMime := true;
    if MIMEqp then uu.MakeQP := true;
    if RFC1522 then uu.RFC1522 := true;
    uu.MailUser := BoxPar^.UserName;
    uu.NewsUser := BoxPar^.UserName;
    uu.FileUser := BoxPar^.UserName;
    f:=OutFilter(source);
    uu.Source := source;
    uu.Dest := DestDir;
    uu._from := boxpar^.pointname;
    uu._to := boxpar^.boxname;
    uu.uunumber := hexval(copy(caller,3,4));
    uu.ztou;
    uu.Free;

    if f then _era(source);
    end;
  if errorlevel<>0 then exit;
  if (BoxPar^.uparcer='') or (Netztyp in [nt_NNTP, nt_POP3, nt_IMAP]) then begin             { Mail/News nicht packen }
    spufsize:=filesum(DestDir+'D*.OUT');
    spacksize:=spufsize;
    end
  else begin                                   { Mail/News packen }
    freeze:=pos('freeze',LowerCase(BoxPar^.uparcer))>0;
    gzip:=pos('gzip',LowerCase(BoxPar^.uparcer))>0;
    new(f1); new(f2);
    p:=pos('$PUFFER',UpperCase(boxpar^.uparcer));
    s[0]:=#8;
    if freeze then cunb:='#! funbatch'#10
    else if gzip then cunb:='#! gunbatch'#10
    else cunb:='#! cunbatch'#10;
    findfirst(DestDir+'D*.OUT',ffAnyFile,sr);
    while doserror=0 do begin
      inc(spufsize,sr.size);
      assign(f1^,DestDir+sr.name);
      reset(f1^,1);
      blockread(f1^,s[1],8);
      close(f1^);
      news:=(s='#! rnews');
      if news or (LeftStr(s,5)='HELO ') then begin    { News/SMTPmail packen }
        shell(LeftStr(boxpar^.UpArcer,p-1)+DestDir+sr.name+mid(boxpar^.UpArcer,p+7),
              500,3);
        if not existf(f1^) then begin    { Datei wurde gepackt }
          if freeze then assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-2)+'XZ')
          else assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-1)+'Z');
          if (errorlevel<>0) or not existf(f1^) then begin
            PackFehler;
            dispose(f1); dispose(f2);
            exit;
            end;
          if news then begin
            reset(f1^,1);
            assign(f2^,DestDir+sr.name);
            rewrite(f2^,1);                          { cunbatch erzeugen }
            blockwrite(f2^,cunb[1],length(cunb));
            fmove(f1^,f2^);
            close(f1^); close(f2^);
            erase(f1^);
            end
          else
            rename(f1^,DestDir+sr.name);
          end
        else
          if not news then     { SMTP-File nicht gepackt - Packrate zu schlecht }
            NoCompSmtp(hexval(copy(sr.name,3,4)));
        end;
      inc(spacksize,_filesize(DestDir+sr.name));
      findnext(sr);
    end;
    FindClose(sr);
    dispose(f1); dispose(f2);
    end;
  uunum:=GetNextUUnummer;
end;


{ RFC-Daten aus SPOOL\ konvertieren und einlesen }

function ImportUUCPfromSpool(XFerDir:string):boolean;
var sr      : searchrec;
    f1,f2   : ^file;
    s       : string[80];
    rr      : word;
    uncompy : byte;
    dummy   : longint;
    uu : TUUZ;

  procedure uncompress(fn:string; freeze,gzip:boolean);
  var s : string;
  begin
    if freeze then s:=boxpar^.unfreezer
    else if gzip then s:=boxpar^.ungzipper
    else s:=BoxPar^.downarcer;
    exchange(s,'$DOWNFILE',XFerDir+fn+'.Z');
    gotoxy(1,uncompy);
    shell(s,600,5);
    inc(uncompy);
    if uncompy=screenlines-fnkeylines-5 then begin
      clrscr;
      uncompy:=2;
      end;
    if not exist(XFerDir+fn) then
      if _rename(XFerDir+fn+'.Z',XFerDir+fn) then
        MoveToBad(XFerDir+fn);
  end;

begin
  ImportUUCPfromSpool:=false;
  findfirst(XFerDir+'D*.',ffAnyFile,sr);   { Datenfiles - ohne Extension }
  if doserror=0 then begin
    twin;
    clrscr;
    uncompy:=2;
    cursor(curoff);
    new(f1); new(f2);
    while doserror=0 do begin
      inc(NC^.recpack,sr.size);
      assign(f1^,XFerDir+sr.name);
      reset(f1^,1);
      blockread(f1^,s[1],40,rr);
      s[0]:=chr(rr);
      if (LeftStr(s,11)='#! cunbatch') or (LeftStr(s,11)='#! funbatch') or   { Datei entpacken }
         (LeftStr(s,11)='#! gunbatch') or (LeftStr(s,11)='#! zunbatch')
      then begin
        assign(f2^,XFerDir+sr.name+'.Z');
        rewrite(f2^,1);
        seek(f1^,cpos(#10,s));
        fmove(f1^,f2^);
        close(f1^); close(f2^);
        uncompress(sr.name,pos('funbatch',s)>0,
                   (pos('gunbatch',s)>0) or (pos('zunbatch',s)>0));
        end
      else begin
        close(f1^);
        if (LeftStr(s,2)=#$1f#$9d) or (LeftStr(s,2)=#$1f#$9f) or
           (LeftStr(s,2)=#$1f#$8b) then begin     { compressed/frozen SMTP o.Ñ. }
          rename(f1^,XFerDir+sr.name+'.Z');
          uncompress(sr.name,s[2]=#$9f,s[2]=#$8b);
          end;
        end;
      inc(NC^.recbuf,_filesize(XFerDir+sr.name));
      findnext(sr);
    end;
    FindClose(sr);
    dispose(f1); dispose(f2);
    clrscr;
    uu := TUUZ.Create;
    uu.source := XFerDir+'X*.';
    uu.dest := dpuffer;
    uu.OwnSite := boxpar^.pointname+domain;
    uu.utoz;
    uu.free;

    findfirst(XFerDir+'*.0??',ffAnyFile,sr);
    while doserror=0 do begin       { abgebrochene UUCP-Files -> BAD }
      MoveToBad(XFerDir+sr.name);
      findnext(sr);
    end;
    FindClose(sr);
    findfirst(XFerDir+'D*',ffAnyFile,sr);   { Åbriggebliebene D-Files sicherstellen }
    while doserror=0 do begin
      if sr.attr and dos.Archive<>0 then
        MoveToBad(XFerDir+sr.name);
      findnext(sr);
    end;
    FindClose(sr);
    EmptySpool('D*.OUT');        { ausgehende Pakete lîschen }
    EmptySpool('X*.OUT');        { C-File mu· stehenbleiben! }
    if nDelPuffer and (errorlevel=0) and (testpuffer(dpuffer,false,dummy)>=0)
    then
      EmptySpool('*.*');         { entpackte Dateien lîschen }
    CallFilter(true,dpuffer);
    if _filesize(dpuffer)>0 then
      if PufferEinlesen(dpuffer,box,false,false,true,pe_Bad) then begin
        _era(dpuffer);
        ImportUUCPfromSpool:=true;
        end;
    end
  else
    CallFilter(true,dpuffer);
end;


function UUCPnetcall: Boolean;
var
    res  : integer;
    f       : file;
begin
  recs:='';
  netcall_connect:=true;
  fidologfile:=TempFile('');
  if not existBin(UUCICOBin) then begin
    NoUUCICO;
    res:=uu_parerr;
    end
  else begin
    if not comn[comnr].fossil then ReleaseC;
    {$IFNDEF Ver32 }
    res:=uucico(XFerDir+caller,ConnTicks,ende,      { --- UUCICO ---------- }
                   NC^.waittime,NC^.sendtime,NC^.rectime,fidologfile);
    {$ENDIF }
    if not comn[comnr].fossil then Activate;
    end;
  aufhaengen;
  DropDtr(comnr);
  ReleaseC;
  if (res<>uu_nologin) and (res<>uu_parerr) then
    WriteUUnummer(uunum);
  UUCPnetcall:=(res=uu_ok);
  cursor(curoff);
  if (res=uu_ok) or (res=uu_recerr) then
  begin
    NC^.sendbuf:=spufsize;
    NC^.sendpack:=spacksize;
    NC^.abbruch:=(res<>uu_ok);
    moment;
    outmsgs:=0;
    ClearUnversandt(ppfile,box);
    if exist(ppfile) then
      _era(ppfile);
    if exist(eppfile) then
      _era(eppfile);
    if res=uu_ok then
      wrtiming('NETCALL '+boxpar^.boxname);
    if res=uu_recerr then begin    { doppeltes Senden verhindern }
      assign(f,XFerDir+caller);
      rewrite(f,1);                   { Inhalt des C-Files lîschen }
      close(f);
      end;
    closebox;
    end
  else
    NC^.abbruch:=true;
  if ImportUUCPfromSpool(XFerDir) and (res=uu_recerr) then
    erase_mask(XFerDir+'*.');         { Doppeltes Einlesen verhindern }
  SendNetzanruf(once,false);
  SendFilereqReport;    { ... falls vorhanden }
  AppLog(fidologfile,UUCPlog);
  if exist(fidologfile) then _era(fidologfile);
  twin;
end;


procedure UUCPSysopTransfer;
var dummy : longint;

  procedure EmptySysin;
  var sr : searchrec;
  begin
    if not IsPath(BoxPar^.SysopInp) then exit;
    findfirst(BoxPar^.sysopinp+WildCard,ffAnyFile,sr);
    while doserror=0 do begin
      _era(BoxPar^.sysopinp+sr.name);
      findnext(sr);
    end;
    FindClose(sr);
  end;

begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if not IsPath(SysopInp) then begin              { Verzeichnisse testen }
      trfehler(727,30);   { 'ungÅltiges Eingabeverzeichnis' }
      exit;
      end;
    if not IsPath(SysopOut) then begin
      trfehler(728,30);   { 'ungÅltiges Ausgabeverzeichnis' }
      exit;
      end;

    NC^.sendbuf:=_filesize(ppfile);
    if NC^.sendbuf>0 then begin               { -- Ausgabepaket -- }
      outmsgs:=testpuffer(ppfile,false,dummy);
      twin;
      cursor(curoff);
      ZtoRFC(false,ppfile,SysopOut);
      {window(1,1,screenwidth,screenlines);}
      WriteUUnummer(uunum);
      Moment;
      RemoveEPP;
      outmsgs:=0;
      ClearUnversandt(ppfile,box);
      closebox;
      _era(ppfile);
      if exist(eppfile) then _era(eppfile);
      end;

    if exist(SysopInp+WildCard) then                   { -- Eingangspaket -- }
      if ImportUUCPfromSpool(SysopInp) then
        EmptySysin;
    Netcall_connect:=true;
    end;
end;
{
  $Log$
  Revision 1.21  2000/10/17 10:05:55  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.20  2000/08/27 10:37:10  mk
  - UUZ ist jetzt intern

  Revision 1.19  2000/08/15 16:51:02  mk
  - Updates fuer neue Boxentypen NNTP, POP3/SMTP und IMAP

  Revision 1.18  2000/07/30 08:49:54  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.17  2000/07/22 10:10:25  hd
  - Ein paar vergessene (Ansistring, hasHugeString, dbRead etc.)

  Revision 1.16  2000/07/05 17:35:37  hd
  - AnsiString

  Revision 1.15  2000/07/04 12:04:27  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.14  2000/06/23 15:59:25  mk
  - 16 Bit Teile entfernt

  Revision 1.13  2000/06/20 18:20:08  hd
  - Anpassungen an Linux

  Revision 1.12  2000/05/30 17:17:06  jg
  - UUCP Sysoptransfer mehr Sicherheit bei DEL *.*

  Revision 1.11  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.10  2000/05/14 15:04:51  hd
  - Anpassungen Linux

  Revision 1.9  2000/04/18 11:23:51  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.8  2000/04/15 14:18:21  mk
  - Fix fuer FindFirst mit Diretories

  Revision 1.7  2000/04/13 12:48:39  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.6  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.4  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

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


function uu_nummer:word;     { n�chste Paketnummer aus UUNUMMER.DAT lesen }
var t : text;
    s : string[20];
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

function GetNextUUnummer:word;   { n�chste Nummer aus C-File auslesen }
var t : text;
    s : string[60];
    w : word;
begin
  w:=uu_nummer;
  if exist(XFerDir+caller) and (_filesize(XFerDir+caller)>0) then begin
    assign(t,XFerDir+caller);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if left(s,4)='S D.' then begin
        s:=trim(mid(s,cpos(' ',s)));
        s:=left(s,cpos(' ',s)-1);
        w:=hexval(right(s,4));
        end;
      end;
    close(t);
    if w=$ffff then w:=0
    else inc(w);
    end;
  GetNextUUnummer:=w;
end;

procedure WriteUUnummer(w:word);    { n�chste Nr. in UUNUMER.DAT schreiben }
var t : text;
begin
  assign(t,UUnumdat);
  rewrite(t);
  writeln(t,w);
  close(t);
end;


procedure NoUUZ;
begin
  window(1,1,80,25);
  trfehler(105,30);    { 'Netcall-Konvetierer UUZ.EXE fehlt!' }
  twin;
end;

procedure NoUUCICO;
begin
  window(1,1,80,25);
  trfehler(110,30);    { 'UUCICO.EXE fehlt!' }
  twin;
end;

procedure PackFehler;
begin
  window(1,1,80,25);
  trfehler(713,30);    { 'Fehler beim Packen!' }
  twin;
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
{ Puffer in RFC-Files konvertieren }

procedure ZtoRFC(cleardir:boolean; source,destdir:pathstr);
var sr    : searchrec;
    f1,f2 : ^file;
    s     : string[8];
    p     : byte;
    cunb  : string[15];
    opt   : string[60];
    news  : boolean;
    freeze: boolean;
    gzip  : boolean;
    f     : boolean;

 procedure DelAll(const fn:string);
 var
   f1: File;
 begin
    findfirst(DestDir+fn,$20,sr); { Normal und Archiv, kein Readonly }
    while doserror=0 do begin
      assign(f1, DestDir+sr.name);
      if IOResult = 0 then Erase(f1);
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

begin
  if cleardir then
  begin                        { Spool r�umen }
    DelAll('*.');
    DelAll('*.OUT');
  end;
  spacksize:=0;
  spufsize:=0;
  if not existBin(UUZBin) then begin
    NoUUZ; exit;
    end;
  MakeMimetypCfg;
  with boxpar^ do
  begin
    opt := '';
    if not client then
    begin
      if SizeNego then opt:='-s ';
      if UUSmtp or ClientSmtp then
        if UpArcer='' then opt:=opt+'-SMTP '
        else if pos('freeze',lstr(uparcer))>0 then opt:=opt+'-fSMTP '
        else if pos('gzip',lstr(uparcer))>0 then opt:=opt+'-zSMTP '
        else opt:=opt+'-cSMTP ';
      opt:=opt+'-u'+BoxPar^.username+' ';
    end else
    begin
      opt := opt + '-SMTP '
    end;
    if ClientSmtp or client then opt := opt+'-client ';
    if NewsMIME then opt:=opt+'-MIME ';
    if MIMEqp then opt:=opt+'-qp ';
    if RFC1522 then opt:=opt+'-1522 ';
    f:=OutFilter(source);
    if client then
      shell(UUZBin+' -zu '+opt+OwnPath+source+' '+DestDir+' '+boxpar^.pointname+' '+
          boxpar^.boxname,400,3)
    else
      shell(UUZBin+' -zu '+opt+source+' '+DestDir+' '+boxpar^.pointname+' '+
          boxpar^.boxname+' '+copy(caller,3,4),400,3);
    if f then _era(source);
  end;
  if errorlevel<>0 then exit;
  if (BoxPar^.uparcer='') or client then
  begin             { Mail/News nicht packen }
    spufsize:=filesum(DestDir+'D*.OUT');
    spacksize:=spufsize;
    end
  else begin                                   { Mail/News packen }
    freeze:=pos('freeze',lstr(BoxPar^.uparcer))>0;
    gzip:=pos('gzip',lstr(BoxPar^.uparcer))>0;
    new(f1); new(f2);
    p:=pos('$PUFFER',ustr(boxpar^.uparcer));
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
      if news or (left(s,5)='HELO ') then begin    { News/SMTPmail packen }
        shell(left(boxpar^.UpArcer,p-1)+DestDir+sr.name+mid(boxpar^.UpArcer,p+7),
              500,3);
        if not existf(f1^) then begin    { Datei wurde gepackt }
          if freeze then assign(f1^,DestDir+left(sr.name,length(sr.name)-2)+'XZ')
          else assign(f1^,DestDir+left(sr.name,length(sr.name)-1)+'Z');
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

function ImportUUCPfromSpool(XFerDir:pathstr):boolean;
var sr      : searchrec;
    f1,f2   : ^file;
    s       : string[80];
    rr      : word;
    uncompy : byte;
    dummy   : longint;

  procedure uncompress(fn:string; freeze,gzip:boolean);
  var s : string[120];
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
  if doserror=0 then
  begin
    twin;
    clrscr;
    uncompy:=2;
    cursor(curoff);
    new(f1); new(f2);
    while doserror=0 do
    begin
      inc(NC^.recpack,sr.size);
      assign(f1^,XFerDir+sr.name);
      reset(f1^,1);
      blockread(f1^,s[1],40,rr);
      s[0]:=chr(rr);
      if (left(s,11)='#! cunbatch') or (left(s,11)='#! funbatch') or   { Datei entpacken }
         (left(s,11)='#! gunbatch') or (left(s,11)='#! zunbatch')
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
        if (left(s,2)=#$1f#$9d) or (left(s,2)=#$1f#$9f) or
           (left(s,2)=#$1f#$8b) then begin     { compressed/frozen SMTP o.�. }
          rename(f1^,XFerDir+sr.name+'.Z');
          uncompress(sr.name,s[2]=#$9f,s[2]=#$8b);
          end;
        end;
      inc(NC^.recbuf,_filesize(XFerDir+sr.name));
      findnext(sr);
    end;
    findclose(sr);
    dispose(f1); dispose(f2);
    clrscr;
    window(1,1,80,25);
    shell(UUZBin+' -uz -w:'+strs(screenlines)+
        ' '+XFerDir+'X*. '+dpuffer+' '+boxpar^.pointname+domain,600,3);
    findfirst(XFerDir+'*.0??',ffAnyFile,sr);
    while doserror=0 do begin       { abgebrochene UUCP-Files -> BAD }
      MoveToBad(XFerDir+sr.name);
      findnext(sr);
    end;
    findclose(sr);
    findfirst(XFerDir+'D*',ffAnyFile,sr);   { �briggebliebene D-Files sicherstellen }
    while doserror=0 do begin
      if sr.attr and dos.Archive<>0 then
        MoveToBad(XFerDir+sr.name);
      findnext(sr);
    end;
    findclose(sr);
    EmptySpool('D*.OUT');        { ausgehende Pakete l�schen }
    EmptySpool('X*.OUT');        { C-File mu� stehenbleiben! }
    if nDelPuffer and (errorlevel=0) and (testpuffer(dpuffer,false,dummy)>=0)
    then
      EmptySpool('*.*');         { entpackte Dateien l�schen }
    CallFilter(true,dpuffer);
    if _filesize(dpuffer)>0 then
      if PufferEinlesen(dpuffer,box,false,false,true,pe_Bad) then
      begin
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
    f    : file;
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
  if not existBin(UUZBin) then begin
    nouuz; exit;
    end;
  window(1,1,80,25);
  if (res=uu_ok) or (res=uu_recerr) then begin
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
      wrtiming('NETCALL '+ustr(boxpar^.boxname));
    if res=uu_recerr then begin      { doppeltes Senden verhindern }
      assign(f,XFerDir+caller);
      rewrite(f,1);                   { Inhalt des C-Files l�schen }
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
var
  dummy    : longint;
  s        : string;
  error    : boolean;
  inbound  : pathstr;
  outbound : pathstr;

  procedure EmptyDir(const Dir, Mask: String);
  var
    sr : searchrec;
  begin
    if not IsPath(Dir) then exit;
    findfirst(Dir+Mask,ffAnyFile,sr);
    while doserror=0 do
    begin
      _era(Dir+sr.name);
      findnext(sr);
    end;
    FindClose(sr);
  end;

  { ermitteln, welche Nachrichten nicht versandt wurden }
  procedure GetUnversandtMessages;
  var
    MsgFile: file;
    IDFile: text;
    s: String;
    sr: SearchRec;
    p: byte;
    Found: boolean;
    c : char;
  begin
    Error := false;
    with BoxPar^ do
    begin
      Assign(IDFile, 'UNSENT.ID');
      ReWrite(IDFile);
      FindFirst(PPPSpool+'*.OUT', ffAnyFile, sr);
      while doserror=0 do
      begin
        Assign(MsgFile, PPPSpool+sr.name);
        Reset(MsgFile, 1);
        Found := false;
        while (not eof(MsgFile)) and (not Found) do
        begin
          s := '';
          repeat
            BlockRead(MsgFile, c, 1);
            if c >= ' ' then s := s + c;
          until (c = #10) or EOF(MsgFile);
          if pos('Message-ID:', s) <> 0 then Found := true;
        end;
        close(MsgFile);

        if Found then
        begin
          p := cpos('<', s);
          Writeln(IDFile, Copy(s, p+1, Length(s)-p-1));
          Error := true;
        end;
        Findnext(sr);
      end;
      FindClose(sr);
      Close(IDFile);
      if IOResult = 0 then ;
    end;
  end;

  procedure RenameFiles;
  var
    sr : searchrec;
  begin
    findfirst(inbound + '*.MSG',ffAnyFile,sr);
    while doserror=0 do
    begin
      _rename(inbound + sr.name, inbound + GetBareFileName(sr.name) + '.IN');
      findnext(sr);
    end;
    FindClose(sr);
  end;

  var
   res: integer;

begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do
  begin
    inbound:=iifs(SysopMode,SysopInp,PPPSpool);
    outbound:=iifs(SysopMode,SysopOut,PPPSpool);
    if SysopMode then
    begin
      if not IsPath(SysopInp) then     { Verzeichnisse testen }
      begin
        trfehler(727,30);   { 'ung�ltiges Eingabeverzeichnis' }
        exit;
      end;
      if not IsPath(SysopOut) then
      begin
        trfehler(728,30);   { 'ung�ltiges Ausgabeverzeichnis' }
        exit;
      end;
    end else if client then
    begin
      MkLongDir(PPPSpool, res);
      if IOResult = 0 then ;
      if not IsPath(PPPSpool) then
      begin
        trfehler(728,44);     { 'ung�ltiges Spoolverzeichnis' }
        exit;
      end;
      EmptyDir(PPPSpool, '*.IN');
      EmptyDir(PPPSpool, '*.OUT');
    end;

    NC^.Sendbuf := _filesize(ppfile);
    if NC^.sendbuf>0 then                     { -- Ausgabepaket -- }
    begin
      testpuffer(ppfile,false,dummy);
      twin;
      cursor(curoff);

      ZtoRFC(false,ppfile,outbound);
    end;

    if client and (not SysopMode) then        { Client-Aufruf }
    begin
      NC^.Sendbuf:= filesum(outbound+'*.OUT');
      s := PPPClient;
      exchange(s, '$CONFIG', bfile);
      exchange(s, '$CLPATH+', PPPClientpath);
      exchange(s, '$CLPATH', PPPClientpath);
      exchange(s, '$CLPATH', PPPClientpath);
      exchange(s, '$CLSPOOL', PPPSpool);
      attrtxt(col.colkeys);
      if XPdisplayed then
        FWrt(64,Screenlines,xp_client);      { '       CrossPoint' }
      shell(s,600,3);
      showscreen(false);
    end;

    if NC^.sendbuf>0 then                     { -- Ausgabepaket -- }
    begin
      window(1,1,80,25);
      if not client then WriteUUnummer(uunum);
      Moment;
      RemoveEPP;
      if client and (not SysopMode) then
        GetUnversandtMessages;
      ClearUnversandt(ppfile,box);
      _era(ppfile);
      if exist(eppfile) then _era(eppfile);
      if client and (not SysopMode) then
      begin
        if Exist('UNSENT.PP') then
         if filecopy('UNSENT.PP', ownpath+ppfile) then
           _era('UNSENT.PP');
        _era('UNSENT.ID');
        EmptyDir(PPPSpool, '*.OUT'); { nicht verschickte N. l�schen }
        if error then trfehler(745, 30); { 'Es konnten nicht alle Nachrichten versandt werden!' }
      end;
      Closebox;
    end;

    if client then                         { -- Eingangspaket RFC/Client -- }
    begin
      if exist(inbound+'*.MSG') then
      begin
        NC^.Recbuf := filesum(inbound+'*.MSG');
        if (NC^.RecBuf + NC^.SendBuf) > 0 then wrtiming('NETCALL ' + boxname);
        shell(UUZBin+' -uz -w:'+strs(screenlines)+iifs(PPPMailInUseEnvTo,
          ' -UseEnvTo ',' ')+inbound+'*.MSG '+OwnPath + dpuffer,600,3);
        if nDelPuffer and (errorlevel=0) and
          (testpuffer(dpuffer,false,dummy)>=0) then
            EmptyDir(inbound, '*.MSG'); { entpackte Dateien l�schen }
        CallFilter(true,dpuffer);
      end;
      if _filesize(dpuffer)>0 then
      begin
        if PufferEinlesen(dpuffer,box,false,false,true,pe_Bad) then
        begin
          _era(dpuffer);
          if not SysopMode then RenameFiles
          else EmptyDir(inbound, Wildcard);
        end;
      end;
    end else
      if exist(SysopInp+WildCard) then       { -- Eingangspaket RFC/UUCP -- }
        if ImportUUCPfromSpool(SysopInp) then
          EmptyDir(SysopInp, Wildcard);

    Netcall_connect:=true;
  end;
end;
{
  $Log$
  Revision 1.13.2.24  2002/01/06 17:56:58  my
  MY:- Letzten Commit korrigiert: -UseEnvTo wird jetzt incoming (-uz) statt
       outgoing (-zu) aufgerufen.

  Revision 1.13.2.23  2002/01/04 12:03:36  mk
  - implemented UseEnvTo

  Revision 1.13.2.22  2001/12/20 15:07:18  my
  MY+MK:- Umstellung "RFC/Client" auf neue Netztypnummer 41 und in der
          Folge umfangreiche Code-Anpassungen. Alte RFC/Client-Boxen
          m�ssen einmal manuell von RFC/UUCP wieder auf RFC/Client
          umgeschaltet werden.

  MY:- Sysop-Mode wird jetzt �ber einen Schalter aktiviert/deaktiviert.

  MY:- Sysop-Mode RFC/Client funktioniert jetzt.

  Revision 1.13.2.21  2001/08/26 16:04:50  my
  - String "CrossPoint" instead of "CrossPoint/OpenXP" is displayed upon
    RFC/Client netcalls only then if "CrossPoint/OpenXP" was displayed
    there before (and not removed anyway due to function keys display).

  Revision 1.13.2.20  2001/08/11 22:43:56  mk
  - changed Pos() to cPos() when possible, saves additional 1000 Bytes ;)

  Revision 1.13.2.19  2001/08/11 08:54:03  mk
  - use local file variable in DelAll instead of global file^

  Revision 1.13.2.18  2001/08/11 08:50:37  mk
  - check for IOResult in DelAll
  - DelAll does not try to delete ReadOnly-Files

  Revision 1.13.2.17  2001/06/27 15:35:06  my
  - changed string for RFC/Client netcalls

  Revision 1.13.2.16  2001/04/23 18:43:24  mk
  - Spoolverzeichnis auch in versch. Unterverzeichnissen anlegen

  Revision 1.13.2.15  2001/04/19 15:03:05  mk
  - -client

  Revision 1.13.2.14  2001/04/14 10:07:01  mk
  - Anpassungen Client-Modus

  Revision 1.13.2.13  2001/04/11 07:54:24  mk
  - weitere Arbeiten am Client-Modus

  Revision 1.13.2.12  2001/04/10 11:36:38  mk
  - weitere Anpassungen Client-Modus

  Revision 1.13.2.11  2001/04/09 16:47:18  mk
  - arbeiten am Client-Modus

  Revision 1.13.2.10  2001/02/28 13:57:41  mk
  - grossschreibung Boxname

  Revision 1.13.2.9  2001/02/11 12:32:03  mk
  - Client-Modus Updates

  Revision 1.13.2.8  2001/01/30 10:01:23  mk
  - weitere arbeiten am Client-Modus

  Revision 1.13.2.7  2001/01/19 09:09:00  mk
  - Unversandmeldung nach erstellung des neuen .PP

  Revision 1.13.2.6  2001/01/18 23:59:59  mk
  - verschiedene Aenderungen fuer PPP-Modus

  Revision 1.13.2.5  2001/01/10 17:39:06  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette,
    UUZ und Bugfixes

  Revision 1.13.2.4  2001/01/02 17:06:15  mk
  - bei UUCP den Eingangsfilter auch anwerfen wenn kein Pollpaket
    vorhanden und PPP Option eingeschaltet ist

  Revision 1.13.2.3  2000/12/20 18:09:21  mk
  - Schalter fuer PPP-Option von UUZ

  Revision 1.13.2.2  2000/12/12 11:30:30  mk
  - FindClose hinzugefuegt

  Revision 1.13.2.1  2000/06/29 16:59:32  mk
  - 32 Bit Teile entnommen, Window wieder hergestellt

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
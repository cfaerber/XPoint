{  $Id$

   OpenXP UUCP netcall routines
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

var uunum : word;    { fortlaufende 16-Bit-Nummer der UUCP-Dateien }

{ Activate, ReleaseC etc. are OBSOLETE functions! - will be replaced
  by routines from modem.pas soon. }

procedure Activate;
begin
  // !!ActivateCom(comnr,10000,COMn[comnr].u16550);
end;

procedure ReleaseC;
var p : ScrPtr;
begin
  begin
    sichern(p);
//    ReleaseCom(comnr);
    holen(p);
  end;
end;

procedure aufhaengen;
begin
(*var n,i : byte;
  display:=ParDebug;
    if HayesComm then begin
      n:=5;
      time(100);
      while carrier(comnr) and (n>0) do begin
        tb;tb;tb;
        dec(n);
        if (IgnCD or carrier(comnr)) and GetCTS(comnr) then begin
          SendStr('+++');
          mdelay(500);
          multi2;
          mdelay(700);
          if (IgnCD or carrier(comnr)) and GetCTS(comnr) then begin
            SendStr('AT H'#13);
            mdelay(1000);
            end;
          end;
        if IgnCD or carrier(comnr) then begin
          DropDtr(comnr);
          mdelay(500);
          i:=1;
          while (i<=iif(IgnCd,1,6)) and carrier(comnr) do begin
            mdelay(500); tb;
            inc(i);
            end;
          end;
        flushin;
        if IgnCD then n:=0;
        end;
      NC^.hanguptime:=100-zaehler[2];   { +1 fÅr Rundungsfehler }
      NC^.endtime:=zdate;
      sendstr(#13); mdelay(300);
      flushin;
      if Comn[comnr].MExit<>'' then begin
        SendmStr(Comn[comnr].MExit);
        mdelay(500);
        flushin;
        end;
      end
    else begin
      NC^.hanguptime:=0;
      NC^.endtime:=zdate;
      end; *)
end;

procedure MakeMimetypCfg;
var t   : text;
    typ : string;
    ext : string;
begin
  assign(t, MimeCfgFile);
  rewrite(t);
  writeln(t,'# ',getres(728));   { 'temporÑre MAGGI- und UUZ-Konfigurationsdatei' }
  writeln(t);
  dbSetIndex(mimebase,0);
  dbGoTop(mimebase);
  while not dbEOF(mimebase) do begin
    typ:= dbReadNStr(mimebase,mimeb_typ);
    ext:= dbReadNStr(mimebase,mimeb_extension);
    if (typ<>'') and (ext<>'') then
      writeln(t,ext,'=',extmimetyp(typ));
    dbNext(mimebase);
    end;
  dbSetIndex(mimebase,mtiTyp);
  close(t);
end;

function uu_nummer:word;     { n‰chste Paketnummer aus UUNUMMER.DAT lesen }
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

function GetNextUUnummer:word;   { n‰chste Nummer aus C-File auslesen }
var t : text;
    s : string;
    w : word;
begin
  w:=uu_nummer;
  if FileExists(XFerDir+caller) and (_filesize(XFerDir+caller)>0) then begin
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

procedure WriteUUnummer(w:word);    { n‰chste Nr. in UUNUMER.DAT schreiben }
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
var sr    : tsearchrec;
    rc    : integer;
    f1,f2 : ^file;
    s     : shortstring;
    p     : byte;
    cunb  : string;
    news  : boolean;
    freeze: boolean;
    gzip  : boolean;
    bzip  : boolean;
    f     : boolean;
    uu: TUUZ;

  procedure NoCompSmtp(w:word);       { rcsmtp -> csmtp }
  var f1,f2 : file;
      s     : string[40];
      p     : byte;
      rr    : word;
      adr   : longint;
  begin
    if w=$ffff then w:=0
    else inc(w);
    if FileExists(DestDir+'X-'+hex(w,4)+'.OUT') then begin
      assign(f1,DestDir+'X-'+hex(w,4)+'.OUT');
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
  if cleardir then begin                { Spool r‰umen }
    erase_mask(DestDir+'*.');
    erase_mask(DestDir+'*.OUT');
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
      else if pos('bzip2',LowerCase(uparcer))>0 then uu.bSMTP := true
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
    spufsize:=FileMaskSize(DestDir+'D*.OUT');
    spacksize:=spufsize;
    end
  else begin                                   { Mail/News packen }
    freeze:=pos('freeze',LowerCase(BoxPar^.uparcer))>0;
    gzip:=pos('gzip',LowerCase(BoxPar^.uparcer))>0;
    bzip:=pos('bzip2',LowerCase(BoxPar^.uparcer))>0;
    new(f1); new(f2);
    p:=pos('$PUFFER',UpperCase(boxpar^.uparcer));
    s[0]:=#8;
    if freeze then cunb:='#! funbatch'#10
    else if gzip then cunb:='#! gunbatch'#10
    else if bzip then cunb:='#! bunbatch'#10
    else cunb:='#! cunbatch'#10;
    rc:= findfirst(DestDir+'D*.OUT',faAnyFile,sr);
    while rc=0 do begin
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
      rc:= findnext(sr);
    end;
    FindClose(sr);
    dispose(f1); dispose(f2);
    end;
  uunum:=GetNextUUnummer;
end;


{ RFC-Daten aus SPOOL\ konvertieren und einlesen }

function ImportUUCPfromSpool(const XFerDir:string):boolean;
var sr      : tsearchrec;
    rc      : integer;
    f1,f2   : ^file;
    s       : string[80];
    rr      : word;
    uncompy : byte;
    dummy   : longint;
    uu : TUUZ;

  procedure uncompress(fn:string; freeze,gzip,bzip:boolean);
  var s : string;
  begin
    if freeze then s:=boxpar^.unfreezer
    else if gzip then s:=boxpar^.ungzipper
    else if bzip then s:=boxpar^.unbzipper
    else s:=BoxPar^.downarcer;
    exchange(s,'$DOWNFILE',XFerDir+fn+'.Z');
    gotoxy(1,uncompy);
    shell(s,600,5);
    inc(uncompy);
    if uncompy=screenlines-fnkeylines-5 then begin
      clrscr;
      uncompy:=2;
      end;
    if not FileExists(XFerDir+fn) then
      if RenameFile(XFerDir+fn+'.Z',XFerDir+fn) then
        MoveToBad(XFerDir+fn);
  end;

begin
  ImportUUCPfromSpool:=false;
  rc:= findfirst(XFerDir+'D*.',faAnyFile,sr);   { Datenfiles - ohne Extension }
  if rc=0 then begin
    twin;
    clrscr;
    uncompy:=2;
    cursor(curoff);
    new(f1); new(f2);
    while rc=0 do begin
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
                   (pos('gunbatch',s)>0) or (pos('zunbatch',s)>0),
                   pos('bunbatch',s)>0);
        end
      else begin
        close(f1^);
        if (LeftStr(s,2)=#$1f#$9d) or (LeftStr(s,2)=#$1f#$9f) or
           (LeftStr(s,2)=#$1f#$8b) or (LeftStr(s,2)=#$42#$5a) then
           begin     { compressed/frozen SMTP o.‰. }
          rename(f1^,XFerDir+sr.name+'.Z');
          uncompress(sr.name,s[2]=#$9f,s[2]=#$8b,s[2]=#$5a);
          end;
        end;
      inc(NC^.recbuf,_filesize(XFerDir+sr.name));
      rc:= findnext(sr);
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

    rc:= findfirst(XFerDir+'*.0??',faAnyFile,sr);
    while rc=0 do begin       { abgebrochene UUCP-Files -> BAD }
      MoveToBad(XFerDir+sr.name);
      rc:= findnext(sr);
    end;
    FindClose(sr);
    rc:= findfirst(XFerDir+'D*',faAnyFile,sr);   { ¸briggebliebene D-Files sicherstellen }
    while rc=0 do begin
      if sr.attr and faArchive<>0 then
        MoveToBad(XFerDir+sr.name);
      rc:= findnext(sr);
    end;
    FindClose(sr);
    erase_mask(xp0.XFerDir+'D*.OUT');        { ausgehende Pakete lˆschen }
    erase_mask(xp0.XFerDir+'X*.OUT');        { C-File muﬂ stehenbleiben! }
    if nDelPuffer and (errorlevel=0) and (testpuffer(dpuffer,false,dummy)>=0)
    then
      erase_mask(xp0.XFerDir+WildCard);         { entpackte Dateien lˆschen }
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
  if not ExecutableExists(UUCICOBin) then begin
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
  // !!DropDtr(comnr);
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
    if FileExists(ppfile) then
      _era(ppfile);
    if FileExists(eppfile) then
      _era(eppfile);
    if res=uu_ok then
      wrtiming('NETCALL '+boxpar^.boxname);
    if res=uu_recerr then begin    { doppeltes Senden verhindern }
      assign(f,XFerDir+caller);
      rewrite(f,1);                   { Inhalt des C-Files lˆschen }
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
  if FileExists(fidologfile) then _era(fidologfile);
  twin;
end;


procedure UUCPSysopTransfer;
var dummy : longint;
begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if not IsPath(SysopInp) then begin              { Verzeichnisse testen }
      trfehler(727,30);   { 'ung¸ltiges Eingabeverzeichnis' }
      exit;
      end;
    if not IsPath(SysopOut) then begin
      trfehler(728,30);   { 'ung¸ltiges Ausgabeverzeichnis' }
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
      if FileExists(eppfile) then _era(eppfile);
      end;

    if FileExists(SysopInp+WildCard) then                   { -- Eingangspaket -- }
      if ImportUUCPfromSpool(SysopInp) then
        erase_mask(BoxPar^.sysopinp+WildCard);
    Netcall_connect:=true;
    end;
end;

function BoxParOk:string;
var uucp : boolean;
begin
  uucp:=(logintyp=ltUUCP);
  with BoxPar^ do begin
    SysopMode:=(SysopInp+SysopOut<>'');
    if SysopMode then
      if sysopinp='' then
        BoxParOK:=getres2(706,1)    { 'kein Eingangspuffer-Name' }
      else if sysopout='' then
        BoxParOK:=getres2(706,2)    { 'kein Ausgangspuffer-Name' }
      else
        BoxParOk:=''
    else
      if LoginTyp in [ltNNTP, ltPOP3] then
        // Hier evtl. nîtige Tests der Parameter einstellen
      else
      if (pointname='') or (not (_fido or uucp) and (passwort='')) or
        ((not SysopMode and (telefon='') or (telefon='08-15'))) then
        BoxParOk:=getres2(706,3)    { 'unvollstÑndige Pointdaten' }
      else if (((not (_fido or uucp) or (UpArcer<>'')) and
                ((not uucp and (pos('$UPFILE',UpperCase(UpArcer))=0)) or
                 (pos('$PUFFER',FileUpperCase(UpArcer))=0))) or
              (pos('$DOWNFILE',FileUpperCase(DownArcer))=0)) then
        BoxParOk:=getres2(706,4)    { 'unvollstÑndige Packer-Angaben' }
//      else if ntDownarcPath(netztyp) and not FindDownarcer then
//        BoxparOk:=getres2(706,6)    { 'Entpacker fehlt' }
      else if (logintyp<>ltFido) and not uucp and (trim(uploader)='') then
        BoxParOk:=getres2(706,5)    { 'fehlende UpLoader-Angabe' }
      else
        BoxParOk:='';
    end;
  freeres;
end;

//function zeit:string;
//var h,m,s,s100:rtlword;
//begin
//  decodetime(h,m,s,s100);
//  zeit:=formi(h,2)+':'+formi(m,2)+':'+formi(s,2);
//end;

function restzeit:string;
begin
  restzeit:=formi(zaehler[2] div 60,2)+':'+formi(zaehler[2] mod 60,2);
end;

procedure mwriteln;
begin
  moff;
  writeln;
  mon;
end;

procedure time(l:longint);
begin
  zaehler[2]:=l;
  brkadd:=0;
end;

function timeout(ctest:boolean):boolean;
begin
  timeout:=(zaehler[2]=0) or (not IgnCD and ctest {!!and not carrier(comnr)} );
end;

procedure flushin;
begin
 //!!  flushinput(comnr);
end;

procedure testbyte;
begin
(*!! var b : byte;
    l : integer;
   if receive(comnr,b) then
   begin
    if in7e1 then b:=b and $7f;
    if display then begin
      moff; write(chr(b)); mon; end;
    if (b=13) or (b=10) then begin
      if showconn and (recs<>'') and
         (LeftStr(recs,2)<>'OK') and (LeftStr(recs,1)<>'0') and (LeftStr(recs,2)<>'AT') and
         (recs<>'RINGING') and (recs<>'RRING') then begin
        moff;
        if net then write('  -  ');
        writeln(recs);
        mon;
        showconn:=false;
        NC^.connstr:=LeftStr(recs,60);
        if pos('connect',lrec)>0 then mwriteln;
        display:=ShowLogin;
        end
      else
        if showconn and ((recs='RINGING') or (recs='RRING')) then
          write('  -  ',recs,dup(length(recs)+5,#8));
      if logopen and (b=13) then writeln(netlog^,recs);
      recs:=''; lrec:='';
      end
    else
      begin
        l:= length(recs);       { Die ewige Zaehlerei frisst Zeit }
        if l<255 then begin
          Inc(l);
          SetLength(recs,l);
          recs[l]:= chr(b);
          l:= length(lrec);
          Inc(l);
          SetLength(lrec,l);
          lrec[l]:= LoCase(chr(b));
        end;
      end;
    zaehler[4]:=IdleTimeout;
    end; *)
end;

procedure tb;
begin
  testbyte;
  multi2;
end;

procedure tkey;
var c : char;
begin
  if keypressed then begin
    c:=readkey;
    case c of
       #0 : c:=readkey;
      #27 : begin
              writeln; writeln(getres(707));   { '<abgebrochen>' }
              brkadd:=zaehler[2];
              zaehler[2]:=0;
            end;
    else begin
(*        if IgnCTS then SendByte(comnr,byte(c))
          else HSendByte(comnr,byte(c)); *)
      end;
    end;
  end;
end;

procedure sendstr(s:string);
var i : byte;
begin
  for i:=1 to length(s) do begin
    testbyte;
//!!    if IgnCTS then SendByte(comnr,byte(s[i]))
//      else HSendByte(comnr,byte(s[i]));
  end;
end;

procedure esctime0;
var c : char;
begin
  if keypressed then begin
    c:=readkey;
    case c of
      #27 : begin
              brkadd:=zaehler[2];
              zaehler[2]:=0;
              noconnstr:=getres(708);   { 'Abbruch' }
            end;
      '+' : begin
              inc(zaehler[2]);
              dec(brkadd);
            end;
      '-' : if zaehler[2]>0 then begin
              dec(zaehler[2]);
              inc(brkadd);
            end;
    end;
  end;
end;

procedure sendcomm(s:string);
begin
(*var p : byte;
  flushin;
  recs:=''; lrec:='';
  if not HayesComm or ISDN then exit;
  repeat
    p:=cpos('~',s);
    if p>0 then begin
      sendstr(LeftStr(s,p-1));
      delete(s,1,p);
      mdelay(200);
      while received(comnr) do tb;
      mdelay(850);
      end;
  until p=0;
  sendstr(s+#13);
  zaehler[3]:=COMn[comnr].warten;
  repeat
    tb;
    esctime0;
  until (zaehler[3]=0) or timeout(false) or (recs='OK') or (recs='0')
        or (recs='ERROR');
  repeat
    tb;
    esctime0;
  until (zaehler[3]=0) or timeout(false) or (recs='');   { auf CR warten }
  if display then writeln;
  mdelay(500);             *)
end;

procedure sendmstr(s:string);
var p : byte;
begin
    while (length(trim(s))>1) and not timeout(false) do begin
      p:=pos('\\',s);
      if p=0 then p:=length(s)+1;
      sendcomm(trim(LeftStr(s,p-1)));
      s:=trim(mid(s,p+2));
      end;
end;

function testc(s1,s2:string):boolean;
begin
  if pos(s1,UpperCase(recs))>0 then begin
    testc:=true;
    noconnstr:=s2;
    end
  else
    testc:=false;
end;

function busy:boolean;
begin
  busy:=testc('BUSY',getres2(709,1)) or          { 'besetzt' }
        testc('VOICE',getres2(709,2)) or         { 'Voice?!' }
        testc('NO DIAL TONE',getres2(709,3)) or  { 'kein Freizeichen' }
        testc('NO DIALTONE',getres2(709,3)) or
        testc('NO ANSWER',getres2(709,5)) or     { 'keine Antwort' }
        testc('NO CARRIER',getres2(709,4)) or    { 'keine Verbindung' }
        testc('NO USER RESPONDING',getres2(709,5)) or  { Elink: 'keine Antwort' }
        testc('NO B-CHANNEL',getres2(709,6));    { Elink: 'kein B-Kanal frei' }
  freeres;
end;

procedure sb(b:byte);
begin
//    if IgnCTS then SendByte(comnr,b)
//    else HSendByte(comnr,b);
end;

procedure MausAuflegen;
begin
(*!!  if IgnCD then begin
    NC^.hanguptime:=0;
    NC^.endtime:=zdate;
    mdelay(2000);
    end
  else begin
    display:=showlogin;
    time(45);
    while not timeout(true) do begin
      tb; tkey;
      if ((LeftStr(recs,9)='Ihr Anruf') or (pos('tschuess',lrec)>0) or
          (pos('wiedersehen',lrec)>0)) and (zaehler[2]>2) then
        time(2);
      end;
    if carrier(comnr) then
      aufhaengen
    else begin
      NC^.hanguptime:=60-zaehler[2];   { +1 fÅr Rundungsfehler }
      NC^.endtime:=zdate;
      end;
    end; *)
end;

procedure TimeoutStop1;
begin
  aufhaengen; mwriteln;
  cursor(curoff);
  SendNetzanruf(once,false);
  cursor(curon);
end;

procedure showkeys(nr:integer);
begin
  savecursor;
  {window(1,1,80,25);}
  xp1help.showkeys(nr);
  ttwin;
  restcursor;
  attrtxt(7);
end;

procedure emptyrecs;
begin
  if logopen then write(netlog^,recs);
  recs:=''; lrec:='';
end;

procedure login;
const crlf = #13#10;
var
    cc     : integer;
    login  : boolean;   { Unix login: }
    endbef : string[20];
    janus  : string[10];
    lcount : integer;   { ZÑhler, um Doppellogin bei GS-Box zu verhindern }
    scount : integer;
    pcount : integer;
begin
  with boxpar^ do begin
{      if showlogin then display:=true;}
    if logintyp in [ltNetcall,ltZConnect] then
      zaehler[3]:=18        { 18 Sekunden lang ^X versuchen }
    else
      zaehler[3]:=0;
    retries:=retrylogin+2;
    cc:=0;
    if relogin then begin
      recs:='Username:'; lrec:=LowerCase(recs);
      end;
    showkeys(17);
    endbef:='running arc';
    case logintyp of
      ltGS : begin
               lcount:=1; scount:=1; pcount:=1;
             end;
      else   begin
               lcount:=maxint; scount:=maxint; pcount:=maxint;
             end;
    end;
    repeat
      tb; tkey;
      esctime0;
      if (zaehler[3] mod 4 = 1) and startscreen and (lrec<>'') and
         (lrec[1]<>'u') and (cc<>zaehler[3]) then begin
          cc:=zaehler[3];
          sb(ord(^X));
          end;
      if zaehler[4]=0 then begin
        case logintyp of
          ltUUCP  : sendstr(#13);
        end;
        zaehler[4]:=IdleTimeout;
        end;
      case logintyp of
        ltGS    : login:=(RightStr(lrec,10)='zu laden <') or
                         (RightStr(lrec,12)='zunamen ein:');
        else      login:=(RightStr(lrec,5)='ogin:');
      end;
      if ((lrec='username:') or login) and (lcount>0) then begin
        startscreen:=false;
        if net then begin
          moff;
          if not display and (logintyp<>ltGS) then write('Username:');
          display:=ParDebug or (logintyp=ltUUCP);
          if not display and (logintyp<>ltGS) then write(#8'...');
          mon;
          mdelay(200);
          case logintyp of
            ltQuick : begin
                        if loginname='' then loginname:='NET410';
                        sendstr(loginname+#13);
                      end;
            ltGS    : begin
                        writeln;
                        mdelay(500);
                        if loginname='' then loginname:='NET410';
                        sendstr(loginname+#13);
                      end;
          else begin
            if JanusPlus then janus:='JANUS2'
            else janus:='JANUS';
            if login then
              if logintyp=ltZconnect then sendstr(LowerCase(janus)+#13) else
              if logintyp=ltUUCP then sendstr(LoginName+#13)
              else sendstr('zerberus'#13)
            else
              if logintyp=ltZconnect then sendstr(janus+#13)
              else if logintyp=ltUUCP then sendstr(LoginName+#13)
              else sendstr('ZERBERUS'#13);
            end;
          end;  { case }
          end
        else begin
          mdelay(200);
          sendstr(user+#13);
        end;
        emptyrecs;
        dec(retries);
        dec(lcount);
        end
      else if (lrec='systemname:') and (scount>0) then begin
        dec(retries);
        moff;
        if not Display then write(crlf,getres(710));   { 'Systemname...' }
        mon;
        mdelay(200);
        sendstr(pointname+#13);
        if (logintyp=ltUUCP) and (passwort='') then
          endbef:=^P'shere';
        emptyrecs;
        dec(scount);
        end
      else if ((RightStr(lrec,9)='passwort:') or (RightStr(lrec,9)='password:'))
             and (pcount>0) then
        begin
        moff;
        if not Display then write(crlf,getres(711));  { 'Pa·wort...' }
        mon;
        mdelay(200);
        if logintyp = ltUUCP then
          endbef:=^P'shere';
        if net then begin
          sendstr(passwort+#13);
          if (logintyp=ltQuick) or (logintyp=ltGS) then lrec:='running arc'
          else lrec:='';
          end
        else begin
          sendstr(o_passwort+#13);
          lrec:=endbef;
          end;
        if logopen then write(netlog^,recs);
        recs:='';
        dec(pcount);
        end;
    until (retries=0) or (pos(endbef,lrec)>0) or timeout(true);
    showkeys(0);
    if retries=0 then time(0);
    if net then begin
      mwriteln;
      mwriteln;
      if (logintyp<>ltNetcall) and (logintyp<>ltZConnect) then begin
        mdelay(400);
        flushin;
        end;
      Display:=ParDebug;
      end;
    end;
end;

procedure MagicLogin;
begin
  with boxpar^ do begin
    retries:=retrylogin+2;
    repeat
      tb; tkey;
    until ((LeftStr(lrec,7)='(8-n-1)') and (RightStr(lrec,1)='>')) or timeout(true);
    display:=ParDebug;
    if not timeout(true) then begin
      mwriteln;
      mdelay(500);
      if net then begin
        if LightLogin then sendstr('\')
        else sendstr(^F);
        if netztyp=nt_Pronet then mdelay(5000)
        else mdelay(500);
        mwriteln;
        repeat
          moff;
          write('Login...');
          mon;
          emptyrecs;
          if netztyp=nt_Pronet then
            sendstr(box+';'+pointname+#13+passwort+#13+zerbid+#13)
          else
            sendstr(pointname+crlf+passwort+crlf+zerbid+'HI'+crlf);
          repeat
            tb; tkey;
            if pronet and (recs='***') then begin
              moff; writeln;
              writeln('ÆAnruf um diese Uhrzeit ist nicht erlaubtØ'); mon;
              if logopen then
                writeln(netlog^,'ÆAnruf um diese Uhrzeit ist nicht erlaubtØ');
              mdelay(500);
              flushin;
              ende:=true;
              exit;
              end;
          until timeout(true) or multipos('!?',recs) and (length(recs)>2);
          mwriteln;
          mdelay(300);
          flushin;
          dec(retries);
          if retries=0 then time(0);
        until ((cpos('!',lrec)>0) and (cpos('?',lrec)=0)) or timeout(true);
        end;
      mwriteln;
      mdelay(500);
      end
    else
      sendstr(#13);
    end;
end;

procedure WaitForMaus;
var mstop : array[1..3] of string[40];
    stop  : boolean;
    i     : integer;
begin
  display:=true;
  for i:=1 to 3 do
    mstop[i]:=LowerCase(getres2(30000,i));    { 'protokoll startet' / 'downloaden !' / '' }
  freeres;
  stop:=false;
  repeat
    tb; tkey;
    for i:=1 to 3 do
      if (mstop[i]<>'') and (pos(mstop[i],lrec)>0) then stop:=true;
  until stop or timeout(true);
  flushin;
  emptyrecs;
  mwriteln;
end;

procedure MausLogin;
begin
  with boxpar^ do begin
    if prototyp='' then prototyp:='Z';
    if relogin then begin
      sendstr('T');
      mdelay(1000);
      sendstr(prototyp);
      WaitForMaus;
      end
    else begin
      retries:=retrylogin+2;
      repeat
        tb; tkey;
      until (pos('(j/n)',lrec)>0) or timeout(true);
      if not timeout(true) then begin
        mwriteln;
        mdelay(500);
        sendstr(iifs(net,'MausTausch','J'));
        mdelay(200);
        sendstr(username+#13+passwort+#13+prototyp);
        if net then WaitForMaus;
        end;
      end;
    end;
end;

function BimodemFehler:boolean;      { BiModem-Logfile ÅberprÅfen }
var f      : file;
    birec  : record
               date,time : longint;
               direction : char;
               filename  : string[78];
               abort     : char;
               comment   : string[79];
               fill      : array[0..86] of byte;
             end;
    sok,rok: boolean;
    rr     : word;
    p      : byte;
begin
  assign(f,BiLogFile);
  if not existf(f) then BimodemFehler:=true
  else begin
    reset(f,1);
    sok:=false; rok:=false;
    while not eof(f) do with birec do begin
      fillchar(birec,sizeof(birec),0);
      blockread(f,birec,256,rr);
      p:=0;
      while (p<=79) and (filename[p]<>#0) do inc(p);
      Move(filename[0],filename[1],p);
      filename[0]:=chr(p);
      if (direction='S') and (pos('CALLER',UpperCase(filename))>0) then
        sok:=(abort<>'A');
      if (direction='R') and (pos('CALLED',UpperCase(filename))>0) then
        rok:=(abort<>'A');
      end;
    close(f);
    BimodemFehler:=not (sok and rok);
    end;
end;

procedure DelPronetfiles;
begin
  with boxpar^ do begin
    if FileExists(boxname+'.REQ') then _era(boxname+'.REQ');
    if FileExists(boxname+'.UPD') then _era(boxname+'.UPD');
    end;
end;

procedure ZtoMaggi(source,dest:string; pronet:boolean; screen:byte);
var c : string[10];
    f : boolean;
begin
  f:=OutFilter(source);
  if pronet then DelPronetfiles;
  if pronet then c:='-zp'
  else c:='-zm'+iifs(msgids,' -m','');
  with BoxPar^ do
    shell('MAGGI.EXE '+c+' -n'+MagicNET+' '+source+' '+dest+' '+box+'.BL',400,screen);
  if f then _era(source);
end;

procedure MaggiToZ(source,dest:string; pronet:boolean; screen:byte);
var c : string[10];
begin
  if pronet then c:='-pz'
  else c:='-mz';
  with BoxPar^ do
    shell('MAGGI.EXE '+c+' -n'+MagicNET+' '+source+' '+dest+' '+box+'.BL',400,
          screen);
end;

procedure ZtoQuick(source,dest:string; gs:boolean; screen:byte);
var f : boolean;
begin
  f:=OutFilter(source);
  shell('MAGGI.EXE -zq '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
  if f then _era(source);
end;

procedure QuickToZ(source,dest:string; gs:boolean; screen:byte);
begin
  shell('MAGGI.EXE -qz '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
end;

procedure ZtoMaus(source,dest:string; screen:byte);
var opt : string[10];
    f   : boolean;
begin
  MakeMimetypCfg;
  f:=OutFilter(source);
  if MausPSA then opt:=''
  else opt:='-psa ';
  if not boxpar^.Brettmails then opt:=opt+'-on ';
  with BoxPar^ do
    shell('MAGGI.EXE -zs '+opt+'-b'+box+' -h'+MagicBrett+' -i -it '+
          iifs(maxmaus,'-mm ','')+source+' '+dest,300,screen);
  if f then _era(source);
end;

procedure MausToZ(source,dest:string; screen:byte);
begin
  with BoxPar^ do
    shell('MAGGI.EXE -sz -b'+box+' -h'+MagicBrett+' -it '+source+' '+dest,
          600,screen);
end;

Procedure ZFilter(source,dest: string);
var
  f:boolean;
begin
  f := Outfilter(source);   { Filtern und merken ob Filtrat existiert }
  CopyFile(source,dest);    { ppfile/Filtrat ins Outfile kopieren }
  if f then _era(source);   { falls gefiltert wurde Filtratfile lˆschen }
  errorlevel:=0;
end;

procedure SysopTransfer;
var f1,f2 : file;
    fn    : string;
    dummy : longint;
    ft    : longint;

  procedure RemoveMausmark;   { Schlu·zeile mit '#' entfernen }
  var s  : string[10];
      rr : word;
      p  : byte;
  begin
    if filesize(f2)>=3 then begin
      seek(f2,max(0,filesize(f2)-5));
      blockread(f2,s[1],10,rr);
      s[0]:=chr(rr);
      p:=pos('#'#13#10,s);
      if p>0 then begin
        seek(f2,filesize(f2)-length(s)+p-1);
        truncate(f2);
        end;
      end;
  end;

begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do
  begin
    if not ValidFilename(SysopOut) then
    begin
      trfehler(723,30);   { 'ungÅltige Ausgabedatei' }
      exit;
    end;

    assign(f2,SysopOut);
    if existf(f2) then
    begin
      reset(f2,1);
      seek(f2,filesize(f2));
    end else
      rewrite(f2,1);

    assign(f1,ppfile);

    if (logintyp=ltMaus) and not existf(f1) then
    begin
      rewrite(f1,1); close(f1); { fÅr leeres INFILE }
    end;

    if existf(f1) then
    begin
      if logintyp in [ltMagic,ltQuick,ltGS,ltMaus,ltZConnect] then
      begin
        fn:=TempS(_filesize(ppfile)+10000);
        case logintyp of
          ltMagic : ZtoMaggi(ppfile,fn,netztyp=nt_Pronet,3);
          ltQuick : ZtoQuick(ppfile,fn,false,3);
          ltGS    : ZtoQuick(ppfile,fn,true,3);
          ltMaus  : ZtoMaus(ppfile,fn,3);
          ltZConnect: ZFilter(ppfile,fn);
        end;
        if errorlevel=MaggiFehler then
        begin
          trfehler(724,30);   { 'Fehler bei der MAGGI-Konvertierung' }
          if FileExists(fn) then _era(fn);
          close(f2);
          exit;
        end;
        assign(f1,fn);
      end;

      { Puffer vor Reset(f1, 1) testen, da sonst sharing violation }

      outmsgs:=testpuffer(ppfile,false,dummy);

      reset(f1,1);
      NC^.sendbuf:=filesize(f1);
      if logintyp=ltMaus then
        RemoveMausmark;

      fmove(f1,f2);            { .PP an Ausgabepuffer hÑngen }
      close(f1);
      close(f2);

      Moment;

      outmsgs:=0;
      RemoveEPP;
      ClearUnversandt(ppfile,box);
      closebox;

      if logintyp in [ltMagic,ltQuick,ltGS,ltMaus,ltZConnect] then erase(f1);
      _era(ppfile);
      if FileExists(eppfile) then _era(eppfile);
      end
    else begin
      close(f2);
    end;
    if _filesize(SysopOut)=0 then _era(SysopOut);

    fn:=SysopInp;
    assign(f1,fn);
    if not existf(f1) then begin
      rewrite(f1,1);
      close(f1);
      end
    else
      if logintyp in [ltMagic,ltQuick,ltGS,ltMaus] then begin
        fn:=TempS(_filesize(fn)+10000);
        case logintyp of
          ltMagic : MaggiToZ(SysopInp,fn,netztyp=nt_Pronet,3);
          ltQuick : QuickToZ(SysopInp,fn,false,3);
          ltGS    : QuickToZ(SysopInp,fn,true,3);
          ltMaus  : begin
                      ft:=filetime(box+'.itg');
                      MausToZ(SysopInp,fn,3);
                      MausGetInfs(box,mauslogfile);
                      MausLogFiles(0,false,box);
                      MausLogFiles(1,false,box);
                      MausLogFiles(2,false,box);
                      if ft<>filetime(box+'.itg') then
                        MausImportITG(box);
                    end;
      end;
    end;
    NC^.recbuf:=_filesize(SysopInp);
    CallFilter(true,fn);
    if PufferEinlesen(fn,box,false,false,true,pe_Bad) then begin
      _era(SysopInp);               { Eingabepuffer lîschen }
    { if _maus and not MausLeseBest then
        MausPMs_bestaetigen(box);   - abgeschafft, da im MausNet unerwÅnscht }
      end;
    if logintyp in [ltMagic,ltQuick,ltGS,ltMaus] then
      if FileExists(fn) then _era(fn);
    Netcall_connect:=true;
    end;
end;

procedure FidoSysopTransfer;
var dummy : longint;
    i     : integer;
    dir   : TDirectory;
  procedure ferror(nr:word);
  begin
    trfehler(nr,30);
    if FileExists(upuffer) then _era(upuffer);
    nc^.sendbuf:=0;
    outmsgs:=0; outemsgs:=0;
  end;

  procedure CopyFileAttaches(ppfile:string);
  var hd  : THeader;
      hds : longint;
      adr : longint;
      f   : file;
      ok  : boolean;
  begin
    if _filesize(ppfile)>0 then begin
      hd:=THeader.Create;
      assign(f,ppfile);
      reset(f,1);
      adr:=0; ok:=true;
      while ok and (adr<filesize(f)) do begin
        seek(f,adr);
        MakeHeader(true,f,0,0,hds,hd,ok,false, true);
        if (hd.attrib and attrFile<>0) then
          if not FileExists(hd.betreff) then
            trfehler1(725,hd.betreff,15)   { '%s fehlt!' }
          else
            if not filecopy(hd.betreff,boxpar^.SysopOut+extractFileName(hd.betreff)) then
              trfehler1(726,hd.betreff,15);  { 'Fehler beim Kopieren von %s' }
        inc(adr,hds+hd.groesse);
        end;
      close(f);
      Hd.Free;
      end;
  end;

begin { FidoSysopTransfer }
  Debug.DebugLog('xpncuucp','FidoSysopTransfer starting',DLInform);
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if not IsPath(SysopInp) then begin          // Verzeichnisse testen
      trfehler(727,30);                         // 'ungÅltiges Eingabeverzeichnis'
      exit;
      end;
    if not IsPath(SysopOut) then begin
      trfehler(728,30);                         // 'ungÅltiges Ausgabeverzeichnis'
      exit;
      end;

    NC^.sendbuf:=_filesize(ppfile);
    for i:=1 to addpkts^.anzahl do
      inc(NC^.sendbuf,_filesize(addpkts^.abfile[i]+BoxFileExt));
    if (NC^.sendbuf>0) or (SendAKAs<>'') then begin                     // Ausgabepaket
      outmsgs:=testpuffer(ppfile,false,dummy);
      for i:=1 to addpkts^.anzahl do
        inc(outmsgs,testpuffer(addpkts^.abfile[i]+BoxFileExt,false,dummy));

      ztofido(ppfile,upuffer,ownfidoadr,3,pointer(addpkts),alias);

      if SysopPack then begin
        if (errorlevel=MaggiFehler) and FileExists(ppfile) then begin
          ferror(729);                          // 'Fehler bei der ZFIDO-Konvertierung'
          exit;
          end;
        if errorlevel=0 then begin
          Debug.DebugLog('xpncuucp','compressing outgoing packets',DLInform);
          exchange(uparcer,'$UPFILE',caller);
          SetCurrentDir(SysopOut);
          shell(uparcer,500,3);                 // packen
          end;
        if FileExists(SysopOut+upuffer) then begin
          Debug.DebugLog('xpncuucp','deleting upuffer',DLInform);
          _era(SysopOut+upuffer);
          end;
        if errorlevel=0 then begin // Caution!- Errorlevel from shell(uparcer...)!
          Debug.DebugLog('xpncuucp','compression successful, deleting old packets',DLInform);
          with addpkts^ do
            for i:=1 to anzahl do
              if FileExists(SysopOut+addpkt[i]) then _era(SysopOut+addpkt[i]);
          end;
        end;

      if (errorlevel<>0) and FileExists(ppfile) then begin
        ferror(729);     { 'Fehler bei ZFIDO-Konvertierung' }
        exit;
        end;
      Debug.DebugLog('xpncuucp','copying file attachments',DLInform);
      CopyFileAttaches(ppfile);
      for i:=1 to addpkts^.anzahl do
        CopyFileAttaches(addpkts^.abfile[i]+BoxFileExt);
      Moment;
      RemoveEPP;
      outmsgs:=0;
      ClearUnversandt(ppfile,box);
      for i:=1 to addpkts^.anzahl do
        ClearUnversandt(addpkts^.abfile[i]+BoxFileExt,addpkts^.abox[i]);
      closebox;
      if FileExists(ppfile) then _era(ppfile);
      if FileExists(eppfile) then _era(eppfile);
      Debug.DebugLog('xpncuucp','deleting some files...',DLInform);
      with addpkts^ do
        for i:=1 to anzahl do
          if FileExists(abfile[i]+BoxFileExt) then _era(abfile[i]+BoxFileExt);

      Debug.DebugLog('xpncuucp','processing incoming files',DLInform);
      if not isEmptyDir(BoxPar^.sysopinp) then begin
        Debug.DebugLog('xpncuucp','deleting old packets: "'+xp0.XFerDir+'*.pkt"',DLInform);
        erase_mask(xp0.XFerDir+'*.pkt');                        // Lîsche alle alten pakete im spool

        Debug.DebugLog('xpncuucp','searching for uncompressed packets: "'+BoxPar^.sysopinp+'*.pkt"',DLInform);
        dir:= TDirectory.Create(BoxPar^.sysopinp+'*.pkt', faAnyFile, true);
        if dir.Count>0 then begin                               // *.pkt im SysopIn Verzeichnisvorhanden
          for i:=0 to dir.Count-1 do begin                              //
            Debug.DebugLog('xpncuucp','found a packet: "'+XFerDir+dir.Name[i]+'"',DLDebug);
            if filecopy(dir.LongName[i],XFerDir+dir.Name[i]) then begin // move *.pkt von SysOpIn -> Spool
              Debug.DebugLog('xpncuucp','copied ok, deleting',DLDebug);
              _era(dir.LongName[i]);
              end;
            end; // for
          end;   // Dir.Count
        if FidoImport(SysopInp,box) then begin
          Debug.DebugLog('xpncuucp','FidoImport successful, deleting packets',DLDebug);
          for i:=0 to dir.Count-1 do                            // alles *.pkts im SysOpIn lîschen
            if FileExists(BoxPar^.sysopinp+dir.Name[i]) then    // Archive werden in func FidoImport gelîscht
              _era(BoxPar^.sysopinp+dir.Name[i]);
          end
        else
          Debug.DebugLog('xpncuucp','FidoImport not successful',DLError);
        dir.Free;
        end; // if not isEmptyDir...

      end // SendAKAs<>''...
    else
      Debug.DebugLog('xpncuucp','no akas',DLInform);

// if not isEmptyDir(BoxPar^.sysopinp)
//    if not isEmptyDir(SysopInp) then begin   { -- Eingangspaket -- }
//      erase_mask(xp0.XFerDir+WildCard);
//      CopyPKTs;
//      if FidoImport(SysopInp,box,addpkts^.anzahl>0) then
//        erase_mask(BoxPar^.sysopinp+WildCard);
//      EmptySysin;
//      window(1,1,80,25);
//      end;

    Debug.DebugLog('xpncuucp','diffing',DLInform);
    if DoDiffs(FilePath+'*.*',true)=0 then;
    Netcall_connect:=true;
    end; // with boxpar...
  Debug.DebugLog('xpncuucp','FidoSysopCall complete',DLInform);
end;


procedure QWKSysopTransfer;
var sr     : tsearchrec;
    rc     : integer;
{    sysdir : string; }
    dummy  : longint;
{    qwkext : string[3]; }
    qfg    : QfgRec;
    brk    : boolean;
    replace: shortint;
    source : string;
    f      : boolean;
    ex     : string[3];   { REP-Extension }

  procedure ZQWKfehler;
  begin
    if errorlevel in [90..110] then
      tfehler(getres2(2422,4)+getres2(2422,errorlevel),esec)  { 'Fehler bei ZQWK-Konvertierung:~ }
    else
      trfehler1(737,strs(errorlevel),esec);  { 'ZQWK-Fehler Nr. %s bei Nachrichtenkonvertierung!' }
  end;

  function RepExtension:string;
  var t : text;
      s : string[80];
  begin
    RepExtension:='REP';
    assign(t,bfile+QfgExt);
    if existf(t) then begin
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        if LeftStr(LowerCase(s),4)='rep:' then
          RepExtension:=UpperCase(trim(mid(s,5)));
        end;
      close(t);
      end;
  end;

begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if (SysopOut<>'') and not IsPath(SysopOut) then begin
      trfehler(728,30);    { 'ungÅltiges Ausgabeverzeichnis' }
      exit;
      end;

    if not isEmptyDir(SysopInp) then begin                { 1. Import }
      NC^.recpack:=DirectorySize(SysopInp);
{      SysDir:=GetFileDir(SysopInp); }
      erase_mask(XFerDir+'*.ZER');
      shell('ZQWK.EXE -qz -c'+bfile+' -b'+boxname+' -h'+MagicBrett+
            ' -i'+SysopInp+' -o'+XferDir+' -a'+iifs(DelQwk,' -del',''),
            600,3);
{      qwkext:=GetFileext(sysopinp); }
      if errorlevel<>100 then
        ZQWKfehler;
      rc:= findfirst(XferDir+'*.ZER',faAnyFile,sr);
      while rc=0 do begin
        inc(NC^.recbuf,sr.size);
        CallFilter(true,XFerDir+sr.name);
        if PufferEinlesen(XFerDir+sr.name,box,false,false,true,pe_Bad) then begin
          DeleteFile(XFerDir+sr.name);    { Eingabepuffer lîschen }
          { _era(sysdir+LeftStr(sr.name,cpos('.',sr.name))+qwkext); } { QWK-Paket lîschen }
        end;
        rc:= findnext(sr);
      end; // while
      FindClose(sr);
    end;
    freeres;

    NC^.sendbuf:=_filesize(ppfile);              { 2. Export }
    if (NC^.sendbuf>0) then begin
      if not FileExists(bfile+'.QFG') then begin
        trfehler(736,esec);  {'Bitte zuerst Nachrichtenpaket einlesen, um Serverdaten zu ermitteln!' }
        exit;
        end;
      ReadQfg(bfile,qfg);
      ex:=RepExtension;
      if not FileExists(ExtractFilePath(SysopOut)+qfg.repfile+'.'+ex) then
        replace:=1
      else begin
        replace:=ReadIt(ival(getres2(726,0)),
                        getreps2(726,1,UpperCase(bfile+'.'+ex)),  { '%s ist bereits vorhanden.' }
                        getres2(726,2),   { ' ^Åberschreiben , ^anhÑngen ' }
                        2,brk);
        if brk then exit;
        end;
      outmsgs:=testpuffer(ppfile,false,dummy);
      source:=ppfile;
      f:=OutFilter(source);
      shell('ZQWK.EXE -zq -c'+bfile+' -b'+box+' -i'+ppfile+
            iifs(replace=2,' -a','')+
            iifs(SysopOut<>'',' -o'+SysopOut,''),600,3);
      if f then _era(source);
      if errorlevel<>100 then
        ZQWKfehler
      else begin
        Moment;
        RemoveEPP;
        outmsgs:=0;
        ClearUnversandt(ppfile,box);
        closebox;
        _era(ppfile);
        if FileExists(eppfile) then _era(eppfile);
        end;
      end;
    end;
end;

function tickdiff:longint;
begin
  if ticker>=ticks then
    tickdiff:=system.round((ticker-ticks)/TickFreq)
  else
    tickdiff:=system.round((max(system.round(24*3600*TickFreq)-ticks,0)+ticker)/TickFreq);
end;

function testconnect:boolean;
var
    c : boolean;
    t : longint;
begin
  c:=false;
  t:=zaehler[2];
  while (zaehler[2]>0) and (t-zaehler[2]<2) and not c do begin
    tb;
    if pos('connect',lrec)>0 then c:=true;
    end;
  testconnect:=c;
end;

procedure CallerToTemp; { gepackten Puffer wg. Namensgleichheit umbenennen }
var f : file;
begin
  if (logintyp in [ltMagic,ltQuick,ltGS]) and (netztyp<>nt_Pronet) then begin
    assign(f,caller);
    if existf(f) then begin
      if FileExists('$caller.tmp') then _era('$caller.tmp');
      rename(f,'$caller.tmp');
      end;
    end;
end;

procedure TempToCaller;
var f : file;
begin
  if (logintyp=ltQuick) or (logintyp=ltGS) or
     ((logintyp=ltMagic) and (netztyp<>nt_Pronet) and not FileExists(caller)) then
  begin
    assign(f,'$caller.tmp');
    rename(f,caller);
    if ioresult<>0 then;
    end;
end;

procedure waitpack(entpack:boolean);
var p : byte;
begin
   p := 0; { !! mK 12/99 }
   time(boxpar^.packwait);                { auf Packer warten }
   rz:='';
   repeat
     tb; tkey;
     if rz<>restzeit then begin
       moff;
       write(#13,getres(iif(entpack,714,715)),restzeit);   { 'Box (ent)packt Daten... ' }
       mon;
       rz:=restzeit;
       end;
     case logintyp of
       ltNetcall,
       ltZConnect   : p:=pos(NAK,recs);
       ltMagic      : p:=length(recs);
       ltQuick,ltGS : if entpack then p:=length(recs)
                      else p:=pos('**'^X,recs);
     end;
   until (p>0) or timeout(true);
   if not timeout(true) then delete(recs,1,p);
   mwriteln;
end;

procedure makepuf(fn:string; twobytes:boolean);
var f : file;
begin
  assign(f,fn);
  rewrite(f,1);
  if twobytes then blockwrite(f,crlf,2);
  close(f);
end;

procedure testBL;
var f : file;
begin
  if not FileExists(FileUpperCase(bfile+'.BL')) then begin
    assign(f,FileUpperCase(bfile+'.BL'));
    rewrite(f,1);
    close(f);
    end;
end;

function CrashPassword(var CrashBox:string):string;
var d : DB;
begin
  CrashPassword:='';
  dbOpen(d,'systeme',1);
  dbSeek(d,siName,UpperCase(CrashBox));
  if dbFound and (dbReadStr(d,'fs-passwd')<>'') then
    CrashPassword:=dbReadStr(d,'fs-passwd');
  dbClose(d);
end;

function GetTelefon:string;
var p : byte;
begin
  with boxpar^ do begin
    telefon:=trim(telefon);
    p:=cpos(' ',telefon);
    if p=0 then GetTelefon:=telefon
    else begin                         { Nummern rotieren }
      GetTelefon:=LeftStr(telefon,p-1);
      telefon:=trim(mid(telefon,p))+' '+LeftStr(telefon,p-1);
      end;
    end;
end;

function TeleCount:integer;
var n : integer;
    s : string[80];
begin
  s:=trim(boxpar^.telefon);
  n:=1;
  while cpos(' ',s)>0 do begin
    s:=trim(mid(s,cpos(' ',s)));
    inc(n);
    end;
  TeleCount:=n;
end;


{
  $Log$
  Revision 1.4  2001/01/16 08:14:17  mk
  - added Fileuppercase

  Revision 1.3  2001/01/14 10:13:36  mk
  - MakeHeader() integreated in new unit

  Revision 1.2  2001/01/06 18:23:42  ma
  - cleaned up
  - added debug logs to fidosysopcall
  - some error checks in fidosysopcall added
  - todo: move non-uucp routines out of this file

  Revision 1.1  2001/01/04 16:04:13  ma
  - renamed, was xp7u.inc
  - todo: split, simplify, merge with uucico and change to unit

}

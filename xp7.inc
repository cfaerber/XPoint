{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

function BoxParOk:string;
var uucp : boolean;

  function FindDownarcer:boolean;
  begin
    chdir(XferDir_);
    with boxpar^ do
      if not FileDa(downarcer) then begin     { FileDa sucht zuerst im }
        chdir('..');                          { aktuellen Verzeichnis  }
        if FileDa(downarcer) then begin
          downarcer:=ownpath+downarcer;
          FindDownarcer:=true;
          end
        else
          FindDownarcer:=false;
        end
      else begin
        FindDownarcer:=true;
        chdir('..');
        end;
  end;

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
      if (pointname='') or (not (_fido or uucp) and (passwort='')) or
         (telefon='') or (telefon='08-15') then
        BoxParOk:=getres2(706,3)    { 'unvollstÑndige Pointdaten' }
      else if (((not (_fido or uucp) or (UpArcer<>'')) and
                ((not uucp and (pos('$UPFILE',ustr(UpArcer))=0)) or
                 (pos('$PUFFER',ustr(UpArcer))=0))) or
              (pos('$DOWNFILE',ustr(DownArcer))=0)) then
        BoxParOk:=getres2(706,4)    { 'unvollstÑndige Packer-Angaben' }
      else if ntDownarcPath(netztyp) and not FindDownarcer then
        BoxparOk:=getres2(706,6)
      else if (logintyp<>ltFido) and not uucp and (trim(uploader)='') then
        BoxParOk:=getres2(706,5)    { 'fehlende UpLoader-Angabe' }
      else
        BoxParOk:='';
    end;
  freeres;
end;

function zeit:string;
var h,m,s,s100:rtlword;
begin
  gettime(h,m,s,s100);
  zeit:=formi(h,2)+':'+formi(m,2)+':'+formi(s,2);
end;

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
  timeout:=(zaehler[2]=0) or (not IgnCD and ctest and not carrier(comnr));
end;

procedure mdelay(ms:word);
begin
  if not ISDN then
{$IFDEF NCRT }
    xpcurses.mdelay(ms);
{$ELSE }
    inout.mdelay(ms);
{$ENDIF }
end;

procedure flushin;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_flushinput
  else
{$ENDIF CAPI }
    flushinput(comnr);
end;

function carrier(bport:byte):boolean;
begin
{$IFDEF CAPI }
  if ISDN then
    carrier:=CAPI_Carrier
  else
{$ENDIF }
    carrier:=uart.carrier(bport);
end;


procedure testbyte;
var b : byte;
begin

{$IFDEF CAPI }
   if (ISDN and CAPI_getchar(char(b))) or (not ISDN and receive(comnr,b)) then
{$ELSE }
   if receive(comnr,b) then
{$ENDIF }
   begin
    if in7e1 then b:=b and $7f;
    if display then begin
      moff; write(chr(b)); mon; end;
    if (b=13) or (b=10) then begin
      if showconn and (recs<>'') and
         (left(recs,2)<>'OK') and (left(recs,1)<>'0') and (left(recs,2)<>'AT') and
         (recs<>'RINGING') and (recs<>'RRING') then begin
        moff;
        if net then write('  -  ');
        writeln(recs);
        mon;
        showconn:=false;
        NC^.connstr:=left(recs,60);
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
      if length(recs)<255 then begin
        inc(byte(recs[0]));
        recs[length(recs)]:=chr(b);
        inc(byte(lrec[0]));
        lrec[length(lrec)]:=LoCase(chr(b));
        end;
    zaehler[4]:=IdleTimeout;
    end;
end;

procedure tb;
begin
  testbyte;
  multi2(curon);
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
      if out7e1 then SetParity(byte(c),true);
{$IFDEF CAPI }
      if ISDN then
        CAPI_SendStr(c)
      else
{$ENDIF }
        if IgnCTS then SendByte(comnr,byte(c))
          else HSendByte(comnr,byte(c));
      end;
    end;
  end;
end;

procedure sendstr(s:string);
var i : byte;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_Sendstr(s)
  else
{$ENDIF }
    for i:=1 to length(s) do begin
      testbyte;
      if out7e1 then SetParity(byte(s[i]),true);
      if IgnCTS then SendByte(comnr,byte(s[i]))
      else HSendByte(comnr,byte(s[i]));
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
var p : byte;
begin
  flushin;
  recs:=''; lrec:='';
  if not HayesComm or ISDN then exit;
  repeat
    p:=cpos('~',s);
    if p>0 then begin
      sendstr(left(s,p-1));
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
  mdelay(500);
end;

procedure sendmstr(s:string);
var p : byte;
begin
  if not ISDN then
    while (length(trim(s))>1) and not timeout(false) do begin
      p:=pos('\\',s);
      if p=0 then p:=length(s)+1;
      sendcomm(trim(left(s,p-1)));
      s:=trim(mid(s,p+2));
      end;
end;

function testc(s1,s2:string):boolean;
begin
  if pos(s1,ustr(recs))>0 then begin
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
  if out7e1 then SetParity(b,true);
{$IFDEF CAPI }
  if ISDN then
    CAPI_SendStr(char(b))
  else
{$ENDIF }
    if IgnCTS then SendByte(comnr,b)
    else HSendByte(comnr,b);
end;

procedure aufhaengen;
var n,i : byte;
begin
  display:=ParDebug;
{$IFDEF CAPI }
  if ISDN then begin
    time(100);
    if not CAPI_hangup then writeln('ISDN/CAPI hanup error');
    NC^.hanguptime:=100-zaehler[2];   { +1 fÅr Rundungsfehler }
    NC^.endtime:=zdate;
    end
  else
{$ENDIF }
    if HayesComm then begin
      n:=5;
      time(100);
      while carrier(comnr) and (n>0) do begin
        tb;tb;tb;
        dec(n);
        if (IgnCD or carrier(comnr)) and GetCTS(comnr) then begin
          SendStr('+++');
          mdelay(500);
          multi2(curoff);
          mdelay(700);
          if (IgnCD or carrier(comnr)) and GetCTS(comnr) then begin
            SendStr('AT H'#13);
            mdelay(1000);
            end;
          end;
        if IgnCD or carrier(comnr) then begin
          DropDtr(comnr);
          mdelay(500);
{$IFDEF BP }
          SetDtr(comnr);
{$ENDIF}
          i:=1;
          while (i<=iif(IgnCd,1,6)) and carrier(comnr) do begin
            mdelay(500); tb;
            inc(i);
            end;
          end;
        flushin;
        if IgnCD then n:=0;
        end;
{$IFDEF BP }
      setdtr(comnr);
{$ENDIF}
      NC^.hanguptime:=100-zaehler[2];   { +1 fÅr Rundungsfehler }
      NC^.endtime:=zdate;
      sendstr(#13); mdelay(300);
      flushin;
      if Comn[comnr].MExit^<>'' then begin
        SendmStr(Comn[comnr].MExit^);
        mdelay(500);
        flushin;
        end;
      end
    else begin
      NC^.hanguptime:=0;
      NC^.endtime:=zdate;
      end;
end;

procedure MausAuflegen;
begin
  if IgnCD then begin
    NC^.hanguptime:=0;
    NC^.endtime:=zdate;
    mdelay(2000);
    end
  else begin
    display:=showlogin;
    time(45);
    while not timeout(true) do begin
      tb; tkey;
      if ((left(recs,9)='Ihr Anruf') or (pos('tschuess',lrec)>0) or
          (pos('wiedersehen',lrec)>0)) and (zaehler[2]>2) then
        time(2);
      end;
    if carrier(comnr) then
      aufhaengen
    else begin
      NC^.hanguptime:=60-zaehler[2];   { +1 fÅr Rundungsfehler }
      NC^.endtime:=zdate;
      end;
    end;
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
      recs:='Username:'; lrec:=lstr(recs);
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
        ltGS    : login:=(right(lrec,10)='zu laden <') or
                         (right(lrec,12)='zunamen ein:');
        else      login:=(right(lrec,5)='ogin:');
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
              if logintyp=ltZconnect then sendstr(lstr(janus)+#13) else
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
      else if ((right(lrec,9)='passwort:') or (right(lrec,9)='password:'))
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
    until ((left(lrec,7)='(8-n-1)') and (right(lrec,1)='>')) or timeout(true);
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
    mstop[i]:=lstr(getres2(30000,i));    { 'protokoll startet' / 'downloaden !' / '' }
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

procedure Activate;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_resume
  else
{$ENDIF }
    ActivateCom(comnr,max(3000,min(maxavail-20000,10000)),COMn[comnr].u16550);
end;

procedure ReleaseC;
var p : ScrPtr;
begin
{$IFDEF CAPI }
  if ISDN then
    CAPI_suspend
  else
{$ENDIF }
  begin
    sichern(p);
    ReleaseCom(comnr);
    holen(p);
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
      if (direction='S') and (pos('CALLER',ustr(filename))>0) then
        sok:=(abort<>'A');
      if (direction='R') and (pos('CALLED',ustr(filename))>0) then
        rok:=(abort<>'A');
      end;
    close(f);
    BimodemFehler:=not (sok and rok);
    end;
end;

procedure DelPronetfiles;
begin
  with boxpar^ do begin
    if exist(boxname+'.REQ') then _era(boxname+'.REQ');
    if exist(boxname+'.UPD') then _era(boxname+'.UPD');
    end;
end;

procedure MakeMimetypCfg;
var t   : text;
    typ : string[30];
    ext : string[5];
begin
  assign(t,'mimetyp.cfg');
  rewrite(t);
  writeln(t,'# ',getres(728));   { 'temporÑre MAGGI- und UUZ-Konfigurationsdatei' }
  writeln(t);
  dbSetIndex(mimebase,0);
  dbGoTop(mimebase);
  while not dbEOF(mimebase) do begin
    dbReadN(mimebase,mimeb_typ,typ);
    dbReadN(mimebase,mimeb_extension,ext);
    if (typ<>'') and (ext<>'') then
      writeln(t,ext,'=',extmimetyp(typ));
    dbNext(mimebase);
    end;
  dbSetIndex(mimebase,mtiTyp);
  close(t);
end;


procedure ZtoMaggi(source,dest:pathstr; pronet:boolean; screen:byte);
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

procedure MaggiToZ(source,dest:pathstr; pronet:boolean; screen:byte);
var c : string[10];
begin
  if pronet then c:='-pz'
  else c:='-mz';
  with BoxPar^ do
    shell('MAGGI.EXE '+c+' -n'+MagicNET+' '+source+' '+dest+' '+box+'.BL',400,
          screen);
end;

procedure ZtoQuick(source,dest:pathstr; gs:boolean; screen:byte);
var f : boolean;
begin
  f:=OutFilter(source);
  shell('MAGGI.EXE -zq '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
  if f then _era(source);
end;

procedure QuickToZ(source,dest:pathstr; gs:boolean; screen:byte);
begin
  shell('MAGGI.EXE -qz '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
end;

procedure ZtoMaus(source,dest:pathstr; screen:byte);
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

procedure MausToZ(source,dest:pathstr; screen:byte);
begin
  with BoxPar^ do
    shell('MAGGI.EXE -sz -b'+box+' -h'+MagicBrett+' -it '+source+' '+dest,
          600,screen);
end;

Procedure ZFilter(source,dest: Pathstr);
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
    fn    : pathstr;
    dummy : longint;
    ft    : longint;
    TempPPFileName: String;

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
          if exist(fn) then _era(fn);
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
      if exist(eppfile) then _era(eppfile);
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
      if exist(fn) then _era(fn);
    Netcall_connect:=true;
    end;
end;

procedure EmptySpool(filemask:string);
var sr : searchrec;
begin
  findfirst(XFerDir+filemask,ffAnyFile,sr);
  while doserror=0 do begin
    _era(XFerDir+sr.name);
    findnext(sr);
  end;
  {$IFDEF Ver32}
  FindClose(sr);
  {$ENDIF}
end;


procedure FidoSysopTransfer;
var dummy : longint;
    i     : integer;

  procedure ferror(nr:word);
  begin
    trfehler(nr,30);
    if exist(upuffer) then _era(upuffer);
    nc^.sendbuf:=0;
    outmsgs:=0; outemsgs:=0;
  end;

  procedure CopyPKTs;   { PKT's vom Sysopeingangsvereichnis -> SPOOL }
  var sr : searchrec;
  begin
    findfirst(BoxPar^.sysopinp+'*.PKT',ffAnyFile,sr);
    while doserror=0 do begin
      if filecopy(BoxPar^.sysopinp+sr.name,XFerDir+sr.name) then
        _era(BoxPar^.sysopinp+sr.name);
      findnext(sr);
    end;
    {$IFDEF Ver32}
    FindClose(sr);
    {$ENDIF}
  end;

  procedure EmptySysin;
  var sr : searchrec;
  begin
    findfirst(BoxPar^.sysopinp+'*.*',ffAnyFile,sr);
    while doserror=0 do begin
      _era(BoxPar^.sysopinp+sr.name);
      findnext(sr);
    end;
    {$IFDEF Ver32}
    FindClose(sr);
    {$ENDIF}
  end;

  procedure CopyFileAttaches(ppfile:pathstr);
  var hd  : headerp;
      hds : longint;
      adr : longint;
      f   : file;
      ok  : boolean;
  begin
    if _filesize(ppfile)>0 then begin
      new(hd);
      assign(f,ppfile);
      reset(f,1);
      adr:=0; ok:=true;
      while ok and (adr<filesize(f)) do begin
        seek(f,adr);
        MakeHeader(true,f,0,0,hds,hd^,ok,false);
        if (hd^.attrib and attrFile<>0) then
          if not exist(hd^.betreff) then
            trfehler1(725,hd^.betreff,15)   { '%s fehlt!' }
          else
            if not filecopy(hd^.betreff,boxpar^.SysopOut+GetFileName(hd^.betreff)) then
              trfehler1(726,hd^.betreff,15);  { 'Fehler beim Kopieren von %s' }
        inc(adr,hds+hd^.groesse);
        end;
      close(f);
      dispose(hd);
      end;
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
    for i:=1 to addpkts^.anzahl do
      inc(NC^.sendbuf,_filesize(addpkts^.abfile[i]+'.PP'));
    if (NC^.sendbuf>0) or (SendAKAs<>'') then begin     { -- Ausgabepaket -- }
      outmsgs:=testpuffer(ppfile,false,dummy);
      for i:=1 to addpkts^.anzahl do
        inc(outmsgs,testpuffer(addpkts^.abfile[i]+'.PP',false,dummy));
      if SysopPack then begin
        ztofido(ppfile,upuffer,ownfidoadr,3,addpkts,alias);    { ZFIDO }
        if (errorlevel=MaggiFehler) and exist(ppfile) then begin
          ferror(729);   { 'Fehler bei der ZFIDO-Konvertierung' }
          exit;
          end;
        if errorlevel=0 then begin
          exchange(uparcer,'$UPFILE',caller);
          GoDir(SysopOut);
          shell(uparcer,500,3);                         { packen }
          end;
        if exist(SysopOut+upuffer) then _era(SysopOut+upuffer);
        with addpkts^ do
          for i:=1 to anzahl do
            if exist(SysopOut+addpkt[i]) then _era(SysopOut+addpkt[i]);
        end
      else
        ztofido(ppfile,upuffer,ownfidoadr,3,addpkts,alias);  { ZFIDO }

      if (errorlevel<>0) and exist(ppfile) then begin
        ferror(729);     { 'Fehler bei ZFIDO-Konvertierung' }
        exit;
        end;
      CopyFileAttaches(ppfile);
      for i:=1 to addpkts^.anzahl do
        CopyFileAttaches(addpkts^.abfile[i]+'.PP');
      Moment;
      RemoveEPP;
      outmsgs:=0;
      ClearUnversandt(ppfile,box);
      for i:=1 to addpkts^.anzahl do
        ClearUnversandt(addpkts^.abfile[i]+'.PP',addpkts^.abox[i]);
      closebox;
      if exist(ppfile) then _era(ppfile);
      if exist(eppfile) then _era(eppfile);
      with addpkts^ do
        for i:=1 to anzahl do
          if exist(abfile[i]+'.PP') then _era(abfile[i]+'.PP');
      end;

    if exist(SysopInp+'*.*') then begin             { -- Eingangspaket -- }
      EmptySpool('*.*');
      CopyPKTs;
      if FidoImport(SysopInp,box,addpkts^.anzahl>0) then
        EmptySysin;
      {window(1,1,80,25);}
      end;
    if DoDiffs(FilePath+'*.*',true)=0 then;
    Netcall_connect:=true;
    end;
end;


procedure QWKSysopTransfer;
var sr     : searchrec;
{    sysdir : pathstr; }
    dummy  : longint;
{    qwkext : string[3]; }
    qfg    : QfgRec;
    brk    : boolean;
    replace: shortint;
    source : pathstr;
    f      : boolean;
    ex     : string[3];   { REP-Extension }

  function totalsize(files:dirstr):longint;
  var sr  : searchrec;
      sum : longint;
  begin
    sum:=0;
    findfirst(files,ffAnyFile,sr);
    while doserror=0 do begin
      inc(sum,sr.size);
      findnext(sr);
    end;
    {$IFDEF Ver32}
    FindClose(sr);
    {$ENDIF}
    totalsize:=sum;
  end;

  procedure EraFiles(files:dirstr);
  var sr : searchrec;
  begin
    findfirst(files,ffAnyFile,sr);
    while doserror=0 do begin
      _era(GetFiledir(files)+sr.name);
      findnext(sr);
    end;
    {$IFDEF Ver32}
    FindClose(sr);
    {$ENDIF}
  end;

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
        if left(lstr(s),4)='rep:' then
          RepExtension:=ustr(trim(mid(s,5)));
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

    if exist(SysopInp) then begin                { 1. Import }
      NC^.recpack:=totalsize(SysopInp);
{      SysDir:=GetFileDir(SysopInp); }
      EraFiles(XFerDir+'*.ZER');
      shell('ZQWK.EXE -qz -c'+bfile+' -b'+boxname+' -h'+MagicBrett+
            ' -i'+SysopInp+' -o'+XferDir+' -a'+iifs(DelQwk,' -del',''),
            600,3);
{      qwkext:=GetFileext(sysopinp); }
      if errorlevel<>100 then
        ZQWKfehler;
      if exist(XferDir+'*.ZER') then begin
        findfirst(XferDir+'*.ZER',ffAnyFile,sr);
        while doserror=0 do begin
          inc(NC^.recbuf,sr.size);
          CallFilter(true,XFerDir+sr.name);
          if PufferEinlesen(XFerDir+sr.name,box,false,false,true,pe_Bad) then begin
            _era(XFerDir+sr.name);    { Eingabepuffer lîschen }
            { _era(sysdir+left(sr.name,cpos('.',sr.name))+qwkext);  { QWK-Paket lîschen }
            end;
          findnext(sr);
        end;
        {$IFDEF Ver32}
        FindClose(sr);
        {$ENDIF}
      end;
    end;
    freeres;

    NC^.sendbuf:=_filesize(ppfile);              { 2. Export }
    if (NC^.sendbuf>0) then begin
      if not exist(bfile+'.QFG') then begin
        trfehler(736,esec);  {'Bitte zuerst Nachrichtenpaket einlesen, um Serverdaten zu ermitteln!' }
        exit;
        end;
      ReadQfg(bfile,qfg);
      ex:=RepExtension;
      if not exist(GetFileDir(SysopOut)+qfg.repfile+'.'+ex) then
        replace:=1
      else begin
        replace:=ReadIt(ival(getres2(726,0)),
                        getreps2(726,1,ustr(bfile+'.'+ex)),  { '%s ist bereits vorhanden.' }
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
        if exist(eppfile) then _era(eppfile);
        end;
      end;
    end;
end;


procedure ringsignal;
var mz    : longint;
    i     : byte;
    _ende : boolean;
begin
  mz:=zaehler[2];
  moff;
  writeln; writeln;
  attrtxt(15);
  write(getres(712));   { 'Anruf eingegangen!   <Enter>=weiter, <Esc>=Abbruch' }
  attrtxt(7);
  writeln;
  mon;
  for i:=1 to 5 do begin
    sound(1000);
    mdelay(75);
    sound(745);
    mdelay(75);
    end;
{$IFNDEF VP }
  nosound;
{$ENDIF }
  mwriteln;

  _ende:=false;
  zaehler[2]:=300;
  repeat
    multi2(curoff);
    write(#13,restzeit);
    if keypressed then begin
      c:=readkey;
      ende:=(c=keyesc);
      _ende:=(c=keycr);
      end;
  until ende or _ende or timeout(false);
  mwriteln; mwriteln;
  flushin;       { RINGs lîschen }
  zaehler[2]:=max(1,mz-(300-zaehler[2]));
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
      if exist('$caller.tmp') then _era('$caller.tmp');
      rename(f,'$caller.tmp');
      end;
    end;
end;

procedure TempToCaller;
var f : file;
begin
  if (logintyp=ltQuick) or (logintyp=ltGS) or
     ((logintyp=ltMagic) and (netztyp<>nt_Pronet) and not exist(caller)) then
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

procedure makepuf(fn:pathstr; twobytes:boolean);
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
  if not exist(bfile+'.bl') then begin
    assign(f,bfile+'.bl');
    rewrite(f,1);
    close(f);
    end;
end;

function CrashPassword(var CrashBox:string):string;
var d : DB;
begin
  CrashPassword:='';
  dbOpen(d,'systeme',1);
  dbSeek(d,siName,ustr(CrashBox));
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
      GetTelefon:=left(telefon,p-1);
      telefon:=trim(mid(telefon,p))+' '+left(telefon,p-1);
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

procedure FossilTest;
begin
  if comn[comnr].fossil and not FOSSILdetect then begin
    {window(1,1,80,25);}
    trfehler(732,esec);   { 'Kein FOSSIL-Treiber installiert - verwende eingebauten Treiber' }
    twin;
    writeln;
    comn[comnr].fossil:=false;
    end;
end;
{
  $Log$
  Revision 1.22  2000/05/29 20:21:41  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.21  2000/05/14 15:04:51  hd
  - Anpassungen Linux

  Revision 1.20  2000/05/12 20:03:02  mk
  - Temp-Dateien werden bei ZConnect jetzt auch geloescht

  Revision 1.19  2000/05/07 15:19:49  hd
  Interne Linux-Aenderungen

  Revision 1.18  2000/05/04 10:32:59  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.17  2000/04/23 13:35:00  jg
  - Fix?: beim Zconnect-Ausgangsfilter-Aufruf wird Errorlevel=0 gesetzt.

  Revision 1.16  2000/04/23 07:58:53  mk
  - OS/2-Portierung

  Revision 1.15  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.14  2000/04/15 14:18:21  mk
  - Fix fuer FindFirst mit Diretories

  Revision 1.13  2000/04/15 09:05:13  mk
  JG: - Fix fuer Ausgangsfilter bei ZConnect

  Revision 1.12  2000/04/13 12:48:38  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.10  2000/03/30 14:05:05  mk
  - unnˆtigen Debugcode entfernt

  Revision 1.9  2000/03/16 18:45:13  mk
  - Fix fuer < 3.12er Bug: Ausgangsfilter wird jetzt auch bei Boxtyp ZConnect und Sysoppoll aufgerufen

  Revision 1.8  2000/03/16 10:14:25  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.7  2000/03/14 15:15:40  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.5  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.4  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

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

  { --- Bearbeitungs-Routinen fÅr Boxen-Liste -----------------------}

  procedure NameRead(nt:byte; var name,user,komm,maps,realname,domain,fqdn,email,
                     bdomain,fidoname:string; txt:atext; var brk:boolean);
  var x,y  : byte;
      real : boolean;
      dom  : boolean;
      fqdom: boolean;
      bdom : boolean;
      map  : boolean;
      blen : byte;
      btxt : string[15];
      fido : boolean;
      s    : string[20];
      b,yy : byte;
  label restart;
  begin
  restart: 
    DomainNt:=nt;
    real:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP];
    dom:=(nt=nt_ZConnect) or (nt=nt_Magic) or (nt=nt_Pronet) or (nt=nt_Fido) or (nt=nt_UUCP);
    fqdom:=(nt=nt_ZConnect) or (nt=nt_UUCP);  {Eingabe fÅr FQDN nur bei ZConnect und UUCP}
    bdom:=(nt=nt_UUCP);
    map:=(nt<>nt_Maus) and (nt<>nt_Fido) and (nt<>nt_UUCP)
         and (nt<>nt_QWK);
    fido:=(nt=nt_Fido);
    dialog(ival(getres2(912,0)),7+iif(map,2,0)+iif(real,2,0)+iif(dom,2,0)+
           iif(fqdom,2,0)+iif(bdom,4,0)+iif(fido,2,0),txt,x,y);
    blen:=ntBoxnameLen(nt);
    btxt:=getres2(912,iif(fido,1,2));    { 'Nodenummer','Boxname   ' }
    {16.01.00 HS: Kleinschreibung fÅr Boxname zulassen}
    s:=iifs(nt=nt_UUCP,'','');
    maddstring(3,2,btxt,name,blen,blen,
               s+'"!'+range('#','?')+range('A',#126)+'éôö');
                { ^^ wegen "!" = TopCase }
      mhnr(iif(fido,197,190));
    gf_fido:=fido;
    msetvfunc(xp9_testbox);
    maddstring(3,4,getres2(912,3),user,30,30,grosschar(ntGrossUser(nt))+
               'A'+without(allchar,'()'+iifs(ntNamespace(nt),'',' ')));
              { ^^ wegen "!" = TopCase }
      mhnr(191);                                       { 'Username  ' }
    if not bdom then msetvfunc(notempty);
    yy:=6;
    if fido then begin
      maddstring(3,6,getres2(912,4),fidoname,30,40,range(' ',#126)); mhnr(196);
      inc(yy,2);                               { 'Origin    ' }
      end;
    maddstring(3,yy,getres2(912,6),komm,30,30,'');    { 'Kommentar ' }
      mhnr(192);
    inc(yy,2);
    if map then begin
      btxt:=getres2(912,7);                    { 'MAPS-Name' }
      s:=iifs(nt=nt_UUCP,'','>');
      maddstring(3,yy,btxt,maps,20,20,s); mhnr(193);
      if nt=nt_Pronet then
        mappsel(false,'SYSTEM˘VERWALTER')
      else
        mappsel(false,'MAPS˘AREAFIX˘REQUEST˘MAF˘SYSTEM');
      msetvfunc(notempty);
      inc(yy,2);
      end;
    if real then begin                                  { 'Realname  ' }
      maddstring(3,yy,getres2(912,8),realname,30,40,
                 iifs(ntRealUmlaut(nt),'',without(range(' ',#126),'()')));
      mhnr(194);
      inc(yy,2);
      end;
    if dom then begin
      maddstring(3,yy,getres2(912,9),domain,30,60,          { 'Domain    ' }
                 'A'+without(range('!','~'),' @<>/')); mhnr(195);
      if nt=nt_Fido then
        mappsel(false,'fidonet˘fidonet.org˘gernet˘virnet˘globalnet˘metronet')
      else
        case nt of
        { nt_UUCP    : mappsel(false,'.ruessel.sub.org˘.cluster.sub.org˘.rni.sub.org˘.sh.sub.org'); }
          nt_Pronet  : mappsel(false,'.pro');
        { nt_ZCONNECT: mappsel(false,'.zer.sub.org˘.zer.de˘.comlink.de'); }
        end;
      mset3proc(SetDomain);
      inc(yy,2);
      end;
    if bdom then begin
      bDomainNt:=nt;
      maddstring(3,yy,getres2(912,10),bdomain,30,60,        { 'Serverdomain' }
                 'A'+without(range('!','~'),' @<>/')); mhnr(8034);
      mset3proc(SetDomain2);
      inc(yy,2);
      end
    else
      bDomainNt:=0;
    if nt=nt_uucp then               { RFC/UUCP eMail-Adresse vereinfacht }
    begin  
      maddstring(3,yy,getres2(912,12),email,30,80,''); mhnr(30001);
      inc(yy,2);
      end;
    if fqdom then
    begin
      maddstring(3,yy,getres2(912,11),fqdn,30,60,          { 'FQDN' }
                 'A'+without(range('!','~'),' @<>/_')); mhnr(30002);
      msetvfunc(xp9_FQDNTest);
      inc(yy,2);
      end;
    freeres;
    readmask(brk);
    if not brk then
    begin
      if name=DefFidoBox then
      begin
        HighlightName:=ustr(user);
        aufbau:=true;
      end;
    end;

    if not brk and (nt=nt_uucp) then
    begin
      if (email<>'') then
      begin
        if not is_mailaddress(email)
        then begin
          rfehler(908);
          enddialog;
          goto restart;
          end;
        end;
      if (user='') then
        if (email='') then begin
          hinweis(getres2(10900,40)); enddialog; goto restart;
          end
        else user:=left(email,cpos('@',email)-1);
      if (domain='') and (email<>'') then
      begin
        domain:=mid(email,cpos('@',email));
        if cpos('.',domain)=0 then domain:=''
         else domain:=mid(domain,cpos('.',domain));
        end;
      end; 

    enddialog;
  end;

  procedure ReadNetztyp(box:string; var nt:byte; var brk:boolean);
  var x,y  : byte;
      ntyp : string[10];
      i    : integer;
  begin
    ntyp:=Netz_Typ(nt);
    dialog(ival(getres2(913,0)),3,box,x,y);
    maddstring(3,2,getres2(913,1),ntyp,10,10,''); mhnr(660);   { 'Netztyp ' }
    freeres;
    mappsel(true,ntname(nt_uucp_c));  { RFC/Client }
    mappsel(true,ntname(nt_uucp_u));  { RFC/UUCP   }
    for i:=1 to enetztypen-1 do
      if (ntnr[i] in ntAllowed) then
        mappsel(true,ntName(ntnr[i]));
    readmask(brk);
    enddialog;
    if not brk then begin
      if lstr(ntyp)=lstr(ntName(nt_uucp_c)) then nt:=nt_uucp_c
      else if lstr(ntyp)=lstr(ntName(nt_uucp_u)) then nt:=nt_uucp
      else for i:=1 to enetztypen-1 do
        if lstr(ntyp)=lstr(ntName(ntnr[i])) then
          nt:=ntnr[i];
      end;
  end;

  procedure unique(var dname:string);    { eindeutigen Dateinamen erzeugen }
  begin
    dbSeek(d,boiDatei,dname);
    if dbFound then begin
      dname:=left(dname,6)+'01';
      repeat
        dbSeek(d,boiDatei,dname);
        if dbFound then dname:=left(dname,6)+formi(ival(right(dname,2))+1,2);
      until not dbFound;
      end;
  end;


  procedure _EditPPP(var brk:boolean; var name,komm,email,realname,replyto,fqdn,PPPClientPath,
                     PPPClient,PPPAddServers,PPPDialUp,PPPPhone,PPPLogin,PPPPass:String; Titel:String);
  var x,y,b:byte;
  label restart;
  begin
  restart:

    dialog(66,20,Titel,x,y);   { 'Client-Einstellungen' }
  
    maddstring(3,2,getres2(920,81),name,20,20,              { 'Server' }
             '"!'+range('#','?')+range('A',#126)+'éôö');
              { ^^ wegen "!" = TopCase }
    mhnr(30101);
    gf_fido:=false;
    msetvfunc(xp9_testbox);

    maddstring(36,2,getres2(920,82),komm,17,30,'');         { 'Kommentar' }

    maddstring(3,4,getres2(920,83),email,46,80,'');         { 'eMail-Adresse' }
    msetvfunc(xp9_setclientFQDN);

    maddstring(3,6,getres2(920,84),realname,46,40,'');      { 'Realname' }

    maddstring(3,8,getres2(920,85),replyto,46,eAdrLen,'');  { 'Vertreter' }
    msetvfunc(testreplyto);

    maddstring(3,10,getres2(920,86),fqdn,46,60,             { 'FQDN' }
     'A'+without(range('!','~'),' @<>/_'));
    msetvfunc(xp9_FQDNTest);

    maddstring(3,12,getres2(920,87),PPPClientPath,41,60,ValidDirCh);   { 'Client-Verzeichnis' }
    mappsel(false,'UKAD\˘UKAW\˘UKAX\˘UKA_PPP\˘XPNEWS\');
    msetvfunc(PPPClientpathTest);

    maddstring(3,14,getres2(920,88),PPPClient,41,60,'');    { Client-Aufruf }
    mappsel(false,'UKAD.BAT /M $CLPATH $CONFIG˘UKAW.BAT /M $CLPATH $CONFIG˘UKAX.BAT /M $CLPATH $CONFIG˘' +
     'UKA_PPP.BAT /M $CLPATH $CONFIG˘start /w $CLPATH+XPNEWS.EXE $CONFIG˘$CLPATH+XPNEWS.EXE $CONFIG');
    msetvfunc(pppclienttest);

    maddstring(3,16,getres2(920,89),PPPAddServers,41,60,ValidDirCh);   { 'ZusÑtzliche Server' }

    maddstring(3,18,getres2(920,90),PPPDialUp,18,60,'');    { 'Zugang' }

    maddstring(3,20,getres2(920,91),PPPPhone,18,60,'');     { 'Telefon' }

    maddstring(36,18,getres2(920,92),PPPLogin,18,60,'');    { 'Login' }

    maddstring(36,20,getres2(920,93),PPPPass,18,20,'');     { 'Pa·wort' }

    maddtext(0,3,'√'+dup(66,'ƒ')+'¥', 0);  { Linie }
    maddtext(0,11,'√'+dup(66,'ƒ')+'¥', 0); { Linie }
    maddtext(0,17,'√'+dup(66,'ƒ')+'¥', 0); { Linie }

    freeres;
    readmask(brk);
    enddialog;
  end;

  procedure NewBox;
  var name : string[BoxNameLen];
      dname: string[8];
      replyto : string[AdrLen];
      user : string[30];
      komm : string[30];
      maps : string[20];
      real : string[40];
      dom  : string[60];
      fqdom: string[60];
      email: string[80];
      bdom : string[60];
      fido : string[40];
      brk  : boolean;
      fn   : string[12];
      nt,i : byte;
      b,x,y: byte;
      pppm : boolean;
      pc,pp,ps,cldup,clphn,cllog,clpas : string;
      
  begin
    dbGo(d,drec[p]);
    dbRead(d,'dateiname',fn);
    ReadBox(nt,fn,boxpar);
    pppm:=boxpar^.pppmode;
    if empty then
      nt:=nt_Netcall
    else begin
      dbRead(d,'netztyp',nt);
      if (nt=40) and pppm then nt:=41;
      end;
    ReadNetztyp(getres(914),nt,brk); if brk then exit;   { 'neue Box' }
    if nt<>41 then
    begin
      pppM:=false;
      pp:='';
      pc:='';
      ps:='';
      cldup:='';
      clphn:='';
      cllog:='';
      clpas:='';
      boxpar^.PPPMailInSrvr:='';
      boxpar^.PPPMailInPort:='';
      boxpar^.PPPMailOutSrvr:='';
      boxpar^.PPPMailOutPort:='';
      boxpar^.PPPNewsSrvr:='';
      boxpar^.PPPNewsPort:='';
      end
    else begin
      pppM:=true;
      nt:=40;
      pp:=boxpar^.PPPClientPath;
      pc:=boxpar^.PPPClient;
      ps:=boxpar^.PPPAddServers;
      cldup:=boxpar^.PPPDialUp;
      clphn:=boxpar^.PPPPhone;
      cllog:=boxpar^.PPPLogin;
      clpas:=boxpar^.PPPPass;
      end;
    name:=''; user:=''; komm:=''; fido:='';
    maps:=DefaultMaps(nt);
    dom:=ntDefaultDomain(nt); bdom:=dom;
    email:='';
    real:='';
    replyto:='';
    fqdom:='';
    boxpar^.PPPMailInEnv:='';
    boxpar^.PPPMailInUser:='';
    boxpar^.PPPMailInPass:='';
    boxpar^.PPPMailOutEnv:='';
    boxpar^.PPPMailOutUser:='';
    boxpar^.PPPMailOutPass:='';
    boxpar^.PPPNewsUser:='';
    boxpar^.PPPNewsPass:='';

    if pppm then 

      _EditPPP(brk,name,komm,email,real,replyto,fqdom,pp,pc,ps,cldup,clphn,cllog,clpas,getreps(915,netz_typ(nt_UUCP_C)))

    else

      NameRead(nt,name,user,komm,maps,real,dom,fqdom,email,bdom,fido,getreps(915,Netz_Typ(nt)),brk);


    if not brk then begin            { 'neue Box anlegen (Typ: %s)' }
      dbSeek(d,boiName,ustr(name));
      if dbFound then rfehler(921)   { 'Box ist bereits vorhanden' }
      else begin 
        b:=cpos('@',email);
        if user='' then user:=left(email,b-1);
        if dom='' then
        begin
          dom:=mid(email,b);
          if cpos('.',dom)=0 then dom:=''
            else dom:=mid(dom,cpos('.',dom));
          end;
        boxpar^.PPPClientpath:=pp;
        boxpar^.PPPClient:=pc;
        boxpar^.PPPAddServers:=ps;
        boxpar^.PPPDialUp:=cldup;
        boxpar^.PPPPhone:=clphn;
        boxpar^.PPPLogin:=cllog;
        boxpar^.PPPPass:=clpas;
        boxpar^.pppMode:=pppm;
        boxpar^.boxname:=name;
        boxpar^.username:=user;
        boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;
        boxpar^.passwort:=iifs(deutsch,'GEHEIM','SECRET');
        if pppm then boxpar^.PPPMailInEnv:=email;
        nt_bpar(nt,boxpar^);
        if boxpar^.prototyp='' then
          boxpar^.prototyp:='Z';
        boxpar^.sysopinp:=''; boxpar^.sysopout:='';
        boxpar^.sysopstart:=''; boxpar^.sysopend:='';
        if nt=nt_Pronet then boxpar^.pointname:='01'
          else if email='' then boxpar^.pointname:=''
            else begin
              boxpar^.pointname:=mid(email,b+1);
              truncstr(boxpar^.pointname,min(25,cposx('.',boxpar^.pointname)-1));
              end;
        boxpar^.script:='';
        boxpar^.o_script:=''; boxpar^.o_passwort:='';
        for i:=1 to excludes do begin
          boxpar^.exclude[i,1]:='  :  ';
          boxpar^.exclude[i,2]:='  :  ';
          end;
        with boxpar^ do begin
          if ntExtProt(nt) then begin
            uploader:='Zmodem';
            downloader:='Zmodem';
            end;
          if (nt=nt_UUCP) and exist('UUCP.SCR') then
            script:='UUCP.SCR';
          if nt=nt_Fido then begin
            case ival(left(name,cpos(':',name)-1)) of
               8 : magicbrett:='/METRONET/';
               9 : magicbrett:='/VIRNET/';
              10 : magicbrett:='/BEATNET/';
              16 : magicbrett:='/ZyXELNET';
              21 : magicbrett:='/GERNET/';
              23 : magicbrett:='/InvisibleNet/';
              52 : magicbrett:='/GLOBALNET/';
             144 : magicbrett:='/GAMESNET/';
             255 : magicbrett:='/FDNET/';
            end;
            AreaPW:='';
            FilescanPW:='';
            EMSIenable:=true;
            AKAs:=''; SendAKAS:='';
            GetTime:=false;
            end;
          if nt=nt_Maus then begin
            boxpar^.exclude[1,1]:='04:00';
            boxpar^.exclude[1,2]:='06:00';
            end;
          end;
        dname:=ustr(getdname(nt,name));      { eindeutigen Dateinamen bestimmen }
        unique(dname);
        WriteBox(dname,boxpar);
        dbAppend(d);
        dbWrite(d,'netztyp',nt);
        dbWrite(d,'boxname',name);
        dbWrite(d,'username',user);
        dbWrite(d,'pointname',boxpar^.pointname);
        dbWrite(d,'dateiname',dname);
        dbWrite(d,'kommentar',komm);
        dbWrite(d,'nameomaps',maps);
        dbWrite(d,'realname',real);
        dbWrite(d,'domain',dom);
        dbWrite(d,'fqdn',fqdom);
        dbWrite(d,'email',email);
        dbWrite(d,'boxdomain',bdom);
        dbWrite(d,'fidoname',fido);
        if (name='99:99/99') or (name='99:99/98') then begin  { Fido-Crashbox }
          b:=2;   { kein /Netcall/Alle }
          dbWrite(d,'script',b);
          end;
        dbFlushClose(d);
        inc(ntused[nt]);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure DelBox;
  var name  : string[20];
      dname : string[20];
      pp    : boolean;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'boxname',name);
    if name=DefaultBox then
      rfehler(922)            { 'Stammbox kann nicht gelîscht werden!' }
    else begin
      dbRead(d,'dateiname',dname);
      if (dname='') or (length(dname)>8) then begin
        write(#7);
        dbRead(d,'boxname',dname);
        end;
      pp:=_filesize(ownpath+dname+'.PP')>0;
      if ReadJN(iifs(pp,getres(916),'')+getreps(917,name),not pp)   { 'Polldaten vorhanden!! ' / '%s wirklich lîschen' }
      then begin
        dec(ntused[dbReadInt(d,'netztyp')]);
        dbDelete(d);
        dbFlushClose(d);
        if exist(ownpath+dname+bfgext) then     { .BFG lîschen }
          _era(ownpath+dname+bfgext);
        if exist(ownpath+dname+'.BL') then      { Brettliste lîschen }
          _era(ownpath+dname+'.BL');
        if exist(ownpath+dname+'.FL') then      { Fileliste lîschen }
          _era(ownpath+dname+'.FL');
        if exist(ownpath+dname+'.FBL') then     { Filearea-Liste lîschen }
          _era(ownpath+dname+'.FBL');
        if exist(ownpath+dname+'.INF') then     { Maus-Infofileliste lîschen }
          _era(ownpath+dname+'.INF');
        if pp then _era(OwnPath+dname+'.PP');   { Pollpaket lîschen }
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        if name=DefFidoBox then begin
          DefFidoBox:=''; SaveConfig2;
          end;
        aufbau:=true;
        end;
      end;
  end;

  procedure SetDefaultBox;
  const nn : shortint = 1;
  var name : string[20];
      n    : shortint;
  begin
    dbGo(d,drec[p]);
    pushhp(78);
    n:=MiniSel(x+10,min(y+p+1,screenlines-4),'',getres(918),nn);  { '^Stammbox,^Fido-Stammbox' }
    pophp;
    if n<>0 then nn:=abs(n);
    if n>0 then begin
      dbRead(d,'boxname',name);
      if n=1 then DefaultBox:=name
      else begin
        if dbReadInt(d,'netztyp')<>nt_Fido then begin
          rfehler(923);     { 'Die gewÑhlte Box ist keine Fido-Box!' }
          exit;
          end;
        if DefFidobox=name then DefFidobox:=''
        else begin
          DefFidoBox:=name;
          HighlightName:=ustr(dbReadStr(d,'username'));
          aufbau:=true;
          SetDefZoneNet;
          end;
        end;
      SaveConfig2;
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  function kompname:string;
  begin
    kompname:=dbReadStr(d,'username')+' @ '+dbReadStr(d,'boxname');
    if (ntBoxNetztyp(dbReadStr(d,'boxname'))=nt_UUCP)
    then begin
      if dbreadstr(d,'email')<>'' then kompname:=dbreadstr(d,'email')
      else if (dbReadStr(d,'pointname')<>'') then
        kompname:=dbReadStr(d,'username')+' @ '+dbReadStr(d,'pointname')
      end;
  end;

  procedure EditPointdaten(nt:byte; var brk:boolean);

  var x,y,br : byte;
      pname  : string[25];
      add    : shortint;
      fido   : boolean;
      uucp   : boolean;
      pronet : boolean;
      zc     : boolean;
      pnr    : word;     { Fido - Pointnummer }
      lhapar : string[5];
      xx,yy  : byte;
      Ug,U_G : boolean;  { UUCP-g, UUCP-G }
      Ue,Uz  : boolean;  { UUCP-e }
      Uf     : boolean;  { UUCP-f }
      flags  : byte;
      fnode  : boolean;  { Fido-Absender: Nodenummer }
      pp     : byte;
      TempSmtp: String;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'pointname',pname);
    dbRead(d,'script',flags);
    fido:=(nt=nt_Fido);
    uucp:=(nt=nt_UUCP);
    pronet:=(nt=nt_Pronet);
    zc:=(nt=nt_ZConnect);
    xx:=66; yy:=11;
    case nt of
      nt_Fido    : begin
                     xx:=56; yy:=18;
                     fnode:=(flags and 4<>0);
                   end;
      nt_UUCP    : yy:=18;
      nt_pronet  : xx:=55;
      nt_ZConnect: yy:=12;
    end;
    dialog(xx,yy,kompname,x,y);
    with boxpar^ do
    begin
      if UUSmtp then TempSmtp := 'SMTP/UUCP'
        else if ClientSmtp then TempSmtp := 'SMTP/Client'
        else TempSmtp := Copy(getres2(920,63), 1, Pos('˘', Getres2(920,63))-1);
      case nt of
        nt_Maus : begin
                    dbRead(d,'boxname',pname);
                    maddstring(3,2,getres2(920,1),passwort,20,20,''); mhnr(141);
                  end;                                        { 'Pa·wort  ' }
        nt_Fido : begin
                    pnr:=ival(pname);
                    maddint(3,2,getres2(920,2),pnr,5,5,0,32767); mhnr(151);  { 'Pointnummer' }
                    maddint(3,3,getres2(920,3),fPointNet,5,5,0,32767);  { 'Pointnetz  ' }
                  end;
        nt_Pronet:begin
                    maddstring(3,2,getres2(920,5),telefon,20,60,''); mhnr(142);
                    malltrim;                             { 'Telefon ' }
                    maddstring(3,3,getres2(920,6),passwort,20,20,'>'); mhnr(141); { 'Pa·wort ' }
                    maddstring(39,2,getres2(920,7),pname,2,2,'0123456789');  { 'Pointnr.  ' }
                      mhnr(8005);
                  end;

      else        begin
                    if uucp then
                      maddstring(3,2,getres2(920,10),pname,20,25,'<')
                    else
                      maddstring(3,2,getres2(920,10),pname,20,25,'>');
                    mhnr(140);                             { 'Pointname' }
                    if nt=nt_ZConnect then
                      msetvfunc(testZCpointname);
                    if uucp then mset3proc(uucp_getloginname);
                    maddstring(3,3,getres2(920,11),passwort,20,20,
                                   iifs(ntGrossPW(nt),'>',''));
                  end;                                       { 'Pa·wort  ' }
      end;
      if not pronet then
      begin
        maddstring(iif(fido,27,38),2,getres2(920,12),telefon,
                   iif(fido,18,17),60,'');                   { 'Telefon'  }
        mhnr(142);
        malltrim;
        end;
      if nt in [nt_Netcall,nt_ZConnect,nt_Magic,nt_Pronet] then
      begin
        maddstring(iif(pronet,39,38),3,getres2(920,13)+iifs(pronet,' ',''),zerbid,4,4,'>');
        msetvfunc(zidtest); mhnr(143);                         { 'Ser.Nr.' }
        end;
      if nt in [nt_Quick,nt_GS] then
      begin
        if loginname='' then loginname:='NET410';
        maddstring(38,3,getres2(920,14),loginname,17,20,'');   { 'Login  ' }
        mappsel(false,'NET410˘NETCALL');
        mhnr(8006);
        end;
      if fido then
      begin
        maddstring(27,3,getres2(920,15),passwort,18,18,''); mhnr(153);
        mset3proc(fidotestpasslen);                            { 'Pa·wort' }
        end;
      if uucp then
      begin
        maddstring(38,3,getres2(920,16),loginname,17,60,'');   { 'Login  ' }
        mhnr(8016);
        loginfld:=fieldpos;
        end;

      add:=iif(nt=nt_Maus,0,1);
      if not (fido or uucp) then
      begin
        br:=iif(pronet,30,41);
        maddstring(3,4+add,getres2(920,20),uploader,br,100,''); mhnr(144);
          mappsel(false,'Zmodem˘gsz.exe portx $ADDRESS,$IRQ sz $UPFILE˘'+
                        'hslink.exe -P$PORT -U$DOWNPATH $UPFILE˘bimodem /L $PORT /U $UPFILE /I $LOGFILE');
          msetvfunc(progtest);                      { 'Upload-Protokoll  ' }
        maddstring(3,5+add,getres2(920,21),downloader,br,100,'');
          if pronet then mappsel(false,'Zmodem˘dsz.exe port $PORT rz˘gsz.exe portx $ADDRESS,$IRQ rz')
          else mappsel(false,'Zmodem˘gsz.exe portx $ADDRESS,$IRQ rz $DOWNFILE');
          if zc then mappsel(false,'gsz.exe portx $ADDRESS,$IRQ rz $DOWNPATH');
          msetvfunc(progtest);                      { 'Download-Protokoll' }
          downprotnr:=fieldpos;
        if prototyp='' then prototyp:='Z';
        if nt=nt_Maus then begin
          maddstring(3,6,getres2(920,22),prototyp,1,1,'>'); mhnr(150);
          mappsel(false,'Z˘M');                     { 'Protokoll-Kennung ' }
          end;
        add:=1;
        lhapar:='';
        end
      else
      begin
        add:=-2;
        lhapar:=' -m';
        br:=iif(uucp,41,31);
        end;

      begin
        maddstring(3,7+add,getres2(920,23),uparcer,br,60,''); mhnr(146);
        if uucp then
        begin                                     { 'Upload-Packer' }
          mappsel(false,'compress -vf -b12 $PUFFER˘compress -vf -b16 $PUFFER˘freeze -vif $PUFFER˘gzip -vf $PUFFER');
          mhnr(8020);
          end
        else
          mappsel(false,'pkarc a $UPFILE $PUFFER˘pkpak a $UPFILE $PUFFER˘lharc a'+lhapar+' $UPFILE $PUFFER˘'+
                        'lha a'+lhapar+' $UPFILE $PUFFER˘pkzip $UPFILE $PUFFER˘arj a $UPFILE $PUFFER˘'+
                        'rar -std a $UPFILE $PUFFER˘copy $PUFFER $UPFILE');
          msetvfunc(progtest);
          mset3proc(set_uparcext);
          maddstring(3,8+add,getres2(920,iif(uucp,25,24)),downarcer,br,60,'');
        if uucp then                              { 'Download-Entpacker' }
          mappsel(false,'compress -vdf $DOWNFILE˘gzip -vdf $DOWNFILE')
        else                                      { 'Compress-Entpacker' }
          mappsel(false,'pkxarc $DOWNFILE˘pkunpak $DOWNFILE˘lharc e $DOWNFILE˘lha e $DOWNFILE˘'+
                        'pkunzip $DOWNFILE˘arj e $DOWNFILE˘copy $DOWNFILE $PUFFER˘'+
                        'rar -std e $DOWNFILE');
          msetvfunc(progtest);
          mset3proc(set_downarcext);
        if uucp then
        begin
          maddstring(3,9+add,getres2(920,26),unfreezer,br,40,'');  { 'Freeze-Entpacker' }
          mappsel(false,'freeze -vdif $DOWNFILE');
          msetvfunc(progtest);
          maddstring(3,10+add,getres2(920,27),ungzipper,br,40,''); { 'GNU-ZIP-Entpacker' }
          mappsel(false,'gzip -vdf $DOWNFILE');
          msetvfunc(progtest);
          end;
        if not fido then
          if uucp then begin
            maddint(3,10,getres2(920,30),maxpacketsize,5,4,64,4096);
              mhnr(8010);                           { 'max. UUCP-Paketgrî·e ' }
            mappsel(true,'64˘128˘256˘512˘1024˘2048˘4096');
            maddint(3,11,getres2(920,31),maxwinsize,3,1,2,7);  { 'max. UUCP-Fenstergrî·e  ' }
            maddint(3,12,getres2(920,61),maxfsize,5,4,0,9999); { 'maximale Dateigrî·e   ' }
              mhnr(8025);
            maddtext(37,12,'KB',0);
            maddstring(3,13,getres2(920,62),TempSmtp,11,11,''); { 'Batched SMTP-Modus' }
            mappsel(true,getres2(920,63)); mhnr(8017);

            maddbool(3,15,getres2(920,32),VarPacketSize);     { 'variable Paketgrî·e' }
              mhnr(8012);
            maddbool(3,16,getres2(920,38),ForcePacketsize);   { 'Ausgangspaketgrî·e vorgeben' }
            maddbool(3,17,getres2(920,39),SizeNego);          { 'Dateigrî·e Åbertragen' }

            Ug:=cpos('g',uuprotos)>0; U_G:=cpos('G',uuprotos)>0;
            Ue:=cpos('e',uuprotos)>0; Uz:=cpos('z',uuprotos)>0;
            Uf:=cpos('f',uuprotos)>0;
            maddbool(43,10,getres2(920,33),Ug);    { 'g-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            uup1:=fieldpos;
            maddbool(43,11,getres2(920,34),U_G);   { 'G-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,12,getres2(920,35),Uz);    { 'z-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,13,getres2(920,36),Uf);    { 'f-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,14,getres2(920,37),Ue);    { 'e-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            uupl:=fieldpos;
            maddbool(43,17,getres2(920,60),uucp7e1);   { 'Login mit 7e1' }
            mhnr(8024);
            uparcnr:=0; downarcnr:=0;
            end
          else begin
            if zc then begin
              maddbool(3,10+add,getres2(920,44),JanusPlus);    { 'Janus+' }
              mset1func(JanusSwitch);
              mhnr(8019);
              maddstring(33,10+add,getres2(920,43),uparcext,3,3,'>');
              end                                   { 'Packer-Extension    ' }
            else
              maddstring(3,10+add,getres2(920,40),uparcext,3,3,'>');
                mappsel(false,'ZIP˘LZH˘ARJ˘RAR');     { 'Packer-Extension  ' }
                UpArcNr:=fieldpos;
                mset3proc(TestArcext);    { TXT im MausNet verhindern }
                mhnr(148);
            maddstring(iif(pronet,35,33),iif(zc,11,10)+add,getres2(920,
                       iif(pronet,41,42)),downarcext,3,3,'>');
                mappsel(false,'ZIP˘LZH˘ARJ˘RAR');     { 'Entpacker-Extension' }
                DownArcNr:=fieldpos;
                mset3proc(TestArcext);    { TXT im MausNet verhindern }
            end
        else begin
          maddstring(3,7,getres2(920,28),ZMOptions,br,60,''); mhnr(8042);  { 'Zmodem-Optionen' }
          maddstring(3,9,getres2(920,45),AKAs,43,AKAlen,''); mhnr(154);  { 'AKA''s ' }
          maddstring(3,11,getres2(920,53),SendAKAs,31,AKAlen,'');   { 'Pakete mitsenden ' }
          msetvfunc(TestAKAservers);
          maddbool(3,13,getres2(920,46),f4d);         { '4D-Adressen' }
          maddbool(3,14,getres2(920,47),ftosscan);    { 'TosScan o.Ñ.' }
          maddbool(29,13,'EMSI',EMSIenable);
            mset3proc(fidotestpasslen); EMSIfield:=fieldpos;
          maddbool(29,14,getres2(920,48),gettime);    { 'Uhrzeit setzen' }
          maddbool(3,16,getres2(920,49),PacketPW); mhnr(8000);  { 'Paketpa·wort' }
          maddbool(3,17,getres2(920,50),fnode);       { 'Node statt Point' }
          maddbool(29,16,getres2(920,51),NotSEmpty);  { 'leeres Paket senden' }
          maddbool(29,17,getres2(920,52),ExtPFiles);  { 'erw. Paketdateinamen' }
            mhnr(8003);
          UpArcNr:=0; DownArcNr:=0;
          end;
        end;
      end;
    freeres;
    EditPnt:=nt;
    readmask(brk);
    if not brk then
    begin
      if fido then with boxpar^ do
      begin
        flags:=flags and (not 4) + iif(fnode,4,0);
        dbWrite(d,'script',flags);
        pname:=strs(pnr);
        pp:=pos(boxname,SendAKAs);
        if (pp=1) or ((pp>0) and (SendAKAs[pp-1]=' ')) then
          delete(SendAKAs,pp,length(boxname)+1);
        end;
      dbWrite(d,'pointname',pname);
      boxpar^.pointname:=pname;
      if ntOnePW(nt) then boxpar^.o_passwort:=boxpar^.passwort;
      if uucp then with boxpar^ do
      begin
        uuprotos:='';
        if Ug then uuprotos:=uuprotos+'g';
        if U_G then uuprotos:=uuprotos+'G';
        if Uz then uuprotos:=uuprotos+'z';
        if Uf then uuprotos:=uuprotos+'f';
        if Ue then uuprotos:=uuprotos+'e';
        UUSmtp := false; ClientSmtp := false;
        if TempSmtp = 'SMTP/UUCP' then UUSmtp := true else
        if TempSmtp = 'SMTP/Client' then ClientSmtp := true;
      end;
    end;
    enddialog;
    dbGo(d,drec[1]);
    aufbau:=true;;
  end;

  procedure EditExtern(const bfg:string; var brk:boolean);
  var cfgfile       : string[60];
      fullClientDir : dirstr;
      dir           : dirstr;
      name          : namestr;
      ext           : extstr;
  begin
    with boxpar^ do begin
      fullClientDir := FExpand(pppClientPath);
      if (not (IsPath(fullClientDir))) or (fullClientDir = '') then
      begin
        rfehler1(949,ustr(fullClientDir));  { 'Client-Verzeichnis "%s" ist nicht vorhanden!' }
        pushkey('e'); pushkey('c');
        exit;
      end;
      if not (right(fullClientDir,1) = DirSepa) then
        fullClientDir := fullClientDir + DirSepa;
      cfgfile := trim(pppExternCfg);

      { Hier wird nur der Wert pppExternCfg aus <Boxname>.BFG string-  }
      { technisch aufbereitet und an ReadExtCfgFilename in xp9sel.pas  }
      { Åbergeben. Die eigentlichen PrÅfungen finden dann dort statt.  }

      { Dateinamen im oder unterhalb des XP-Verzeichnisses werden in   }
      { der <Boxname>.BFG mit dem vorangestellten String "<XP>\",      }
      { Dateinamen im Client-Verzeichnis ohne Pfad abgelegt. Dies hat  }
      { den Zweck, die Datei auch dann finden zu kînnen, wenn sich das }
      { XP-Verzeichnis und/oder -Laufwerk geÑndert haben sollte. Die   }
      { gesamte Routine verhÑlt sich so, als sei das Client-Verzeich-  }
      { nis das aktuelle Verzeichnis, d.h. auch (hÑndisch in die .BFG  }
      { hinter "Client-ExternConfig=" eingeflickte) Angaben ohne voll- }
      { stÑndigen Pfad oder ohne fÅhrenden Backslash werden als        }
      { relativ zum Client-Verzeichnis betrachtet.                     }

      if ustr(left(cfgfile,5))='<XP>' + DirSepa then
        cfgfile:= ownpath + Mid(cfgfile,6);
      if LFNEnabled then cfgfile := GetLongName(cfgfile);
      pushhp(30129);
      { 'Externe Konfigurationsdatei laden' }
      if ReadExtCfgFilename(getres2(927,10),cfgfile,fullClientDir,true) then
      begin
        if not exist(cfgfile) then
          if not ReadJN(getres(2725),false) then exit; { 'Datei nicht vorhanden - neu anlegen' }
        EditFile(cfgfile,false,false,0,false);
        if not exist(cfgfile) then exit;
        if LFNEnabled then cfgfile := GetShortName(cfgfile);
        fsplit(cfgfile,dir,name,ext);
        if ustr(dir) = ustr(fullClientDir) then
          cfgfile := name + ext
        else
        if ustr(left(dir, Length(ownpath))) = ustr(ownpath) then
          cfgfile := '<XP>' + mid(cfgfile, Length(ownpath));
        if not (ustr(cfgfile) = ustr(pppExternCfg)) then
        begin
          pppExternCfg := cfgfile;
          WriteBox(bfg,boxpar);
        end;
      end;
      freeres;
      pophp;
    end;
  end;


  procedure EditTransport(nt:byte; var brk:boolean);
  var x,y, j: byte;
  begin
    dialog(58,20,kompname,x,y);
    with boxpar^ do begin
      maddtext(3,2,getres2(920,100),col.coldiahigh);
      maddstring(3,4,getres2(920,106),PPPMailInSrvr,35,160,'');   { 'Servername  ' }
        mappsel(false,getres2(920,101));
        mhnr(30115);
      maddstring(3,5,getres2(920,107),PPPMailInEnv,35,160,'');    { 'Envelope-Adresse' }
      msetvfunc(Multi_Mailstring);
      maddstring(3,6,getres2(920,108),PPPMailInUser,35,160,'');   { 'Benutzername' }
      maddstring(3,7,getres2(920,109),PPPMailInPass,18,75,'');    { 'Pa·wort     ' }
      maddstring(43,7,getres2(920,110),PPPMailInPort,7,50,'0123456789 ');  { 'Port' }
        mappsel(false,'110˘25˘143˘585˘993');
      maddtext(3,9,getres2(920,102),col.coldiahigh);
      maddstring(3,11,getres2(920,106),PPPMailOutSrvr,35,160,'');  { 'Servername  ' }
        mappsel(false,getres2(920,103));
      maddstring(3,12,getres2(920,107),PPPMailOutEnv,35,160,'');   { 'Envelope-Adresse' }
      msetvfunc(Multi_Mailstring);
      maddstring(3,13,getres2(920,108),PPPMailOutUser,35,160,'');  { 'Benutzername' }
      maddstring(3,14,getres2(920,109),PPPMailOutPass,18,75,'');   { 'Pa·wort     ' }
      maddstring(43,14,getres2(920,110),PPPMailOutPort,7,50,'0123456789 '); { 'Port' }
        mappsel(false,'25');
      maddtext(3,16,getres2(920,104),col.coldiahigh);
      maddstring(3,18,getres2(920,106),PPPNewsSrvr,35,160,'');   { 'Servername  ' }
        mappsel(false,getres2(920,105));
      maddstring(3,19,getres2(920,108),PPPNewsUser,35,160,'');   { 'Benutzername' }
      maddstring(3,20,getres2(920,109),PPPNewsPass,18,75,'');    { 'Pa·wort     ' }
      maddstring(43,20,getres2(920,110),PPPNewsPort,7,50,'0123456789 ');  { 'Port' }
        mappsel(false,'119');

      maddtext(0,8,'√'+dup(58,'ƒ')+'¥', 0);  { Linie }
      maddtext(0,15,'√'+dup(58,'ƒ')+'¥', 0); { Linie }

      readmask(brk);
      if not brk then
      begin
        msgbox(63,12,_hinweis_,x,y);

        { 'Diese Einstellungen sind nur wirksam, wenn sie von Ihrem'  }
        { 'Client oder Client-Kit ausgewertet werden.             '   }
        { 'Ihr Client unterstÅtzt und/oder erfordert mîglicherweise'  }
        { 'weitere Einstellungen, die Sie in einer externen Konfigu-' }
        { 'rationsdatei vornehmen mÅssen.'                            }
        { 'NÑheres entnehmen Sie bitte Ihrer Client-Dokumentation.'   }
        for j := 2 to 9 do
          mwrt(x+3,y+j,getres2(10900,39+j));

        errsound;
        wait(curoff);
        closebox;
        end;
      enddialog;
      freeres;
      exit;
      end;
    end;


  procedure EditModemdaten(nt:byte; var brk:boolean);
  var x,y,add  : byte;
      schnitte : string[4];
      fido     : boolean;
  begin
    fido:=(nt=nt_Fido);
    add:=iif(fido,0,1);
    dialog(ival(getres2(921,0)),9+add,kompname,x,y);
    with boxpar^ do begin
      maddint(3,2,getres2(921,1),connwait,5,4,1,1000);       { 'Warten auf Verbindung' }
      if (nt=nt_UUCP) and pppMode then mhnr(30130) else mhnr(160);
      if not fido then
        maddint(3,3,getres2(921,2),loginwait,5,4,1,1000);    { 'Warten auf Login     ' }
        if (nt=nt_UUCP) and pppMode then mhnr(30131) else mhnr(161);
      maddint(3,4+add,getres2(921,3),redialwait,5,4,2,1000); { 'WÑhlpause           ' }
      if (nt=nt_UUCP) and pppMode then mhnr(30132) else mhnr(162);
      maddint(3,5+add,getres2(921,4),redialmax,5,4,iif(fido,2,1),1000);   { 'maximale WÑhlversuche' }
      if (nt=nt_UUCP) and pppMode then mhnr(30133) else mhnr(163);
      if not fido then begin
        maddint(35,2,getres2(921,5),retrylogin,5,4,1,100);   { 'Login-Versuche  ' }
        if (nt=nt_UUCP) and pppMode then mhnr(30134) else mhnr(164);
        if (nt<>nt_UUCP) then
          maddint(35,3,getres2(921,6),packwait,5,4,1,9999);  { 'Warten auf Daten' }
        end;
      maddint(35,4+add,getres2(921,7),connectmax,5,4,1,10);  { 'max. Connects  ' }
      if (nt=nt_UUCP) and pppMode then mhnr(30135) else mhnr(166);
      maddint(35,5+add,getres2(921,8),mincps,5,4,0,9999);    { 'min. cps-Rate   ' }
      if (nt=nt_UUCP) or not (fido or stricmp(uploader,'ZMODEM') or
                                      stricmp(downloader,'ZMODEM')) then
        mdisable;
      maddstring(3,7+add,getres2(921,9),modeminit,41,60,''); { 'Modem-Init ' }
      if (nt=nt_UUCP) and pppMode then mhnr(30136) else mhnr(168);
{$IFDEF CAPI }
      if bport=5 then
        schnitte:='ISDN'
      else
{$ENDIF }
      schnitte:='COM'+strs(bport);
      maddstring(3,9+add,getres2(921,10),schnitte,4,4,'');   { 'Schnittstelle ' }
      if (nt=nt_UUCP) and pppMode then mhnr(30137) else mhnr(169);
      mappsel(true,'COM1˘COM2˘COM3˘COM4');
{$IFDEF CAPI }
      if ntISDN(nt) then mappsel(true,'ISDN');
{$ENDIF }
      maddint(28,9+add,getres2(921,11),baud,6,6,150,115200); { 'öbertragungsrate:' }
      if (nt=nt_UUCP) and pppMode then mhnr(30138) else mhnr(170);
      mappsel(false,'300˘1200˘2400˘4800˘9600˘19200˘38400˘57600˘115200');
      msetvfunc(testbaud);
      maddtext(length(getres2(921,11))+39,9+add,getres2(921,12),0);  { 'bd' }
      freeres;
      readmask(brk);
      if not brk then
{$IFDEF CAPI }
          if schnitte='ISDN' then bport:=5
        else
{$ENDIF }
          bport:=ival(right(schnitte,1));
      enddialog;
      end;
  end;

  procedure Editgebuehren(nt:byte; var brk:boolean);
  var x,y   : byte;
      crash : boolean;
  begin
    crash:=(boxpar^.boxname='99:99/98') or (boxpar^.boxname='99:99/99');
    dialog(ival(getres2(922,0)),iif(crash,3,5),kompname,x,y);
    with boxpar^ do begin
      if not crash then begin
        maddstring(3,2,getres2(922,1),gebzone,20,20,'');   { 'Tarifzone      ' }
        AppPhoneZones;
        if (nt=nt_UUCP) and pppMode then mhnr(30140) else mhnr(180);
        end;
      maddint(3,iif(crash,2,4),getres2(922,5),conn_time,2,2,0,99);    { 'Connect-Dauer: ' }
      if (nt=nt_UUCP) and pppMode then mhnr(30141) else mhnr(181);
      maddtext(length(getres2(922,5))+9,iif(crash,2,4),getres2(922,6),0);   { 'Sek.' }
      freeres;
      readmask(brk);
      enddialog;
      end;
  end;

  
  Procedure Rename_box(old_dname,dname:string);
  var newdir: String;

    procedure renbox2(ext:string);
    var f : file;
    begin
      assign(f,BoxPar^.PPPClientpath+old_dname+ext);
      if existf(f) then begin
        if exist(BoxPar^.PPPClientpath+dname+ext) then
          _era(BoxPar^.PPPClientpath+dname+ext);
        rename(f,BoxPar^.PPPClientpath+dname+ext);
        end;
    end;

    procedure renbox(ext:string);
    var f : file;
    begin
      assign(f,ownpath+old_dname+ext);
      if existf(f) then begin
        if exist(ownpath+dname+ext) then
          _era(ownpath+dname+ext);
        rename(f,ownpath+dname+ext);
        end;
    end;

  begin
    renbox('.PP');
    renbox(BfgExt);
    renbox('.FL');
    renbox('.BL');
    renbox('.FBL');
    renbox('.INF');
    renbox('.CFG');
    renbox2('.BL');
    renbox2('.GR');
    renbox2('.RC');
    renbox2('.CFG');
    renbox2(BfgExt);
    ChDir(XFerDir_);
    GetDir(0, NewDir);
    RenameDir(old_dname, dname);
    ChDir(Copy(OwnPath, 1, Length(OwnPath)-1));
  end;



  procedure EditName(var brk:boolean);
  var name,user,komm  : string[40];
      dname,old_dname : string[8];
      realname        : string[40];
      email           : string[80];
      dom,bdom,fqdom  : string[60];
      maps            : string[20];
      nt              : byte;
      newbox,newibox  : boolean;
      fidoname        : string[40];
  begin
    dbGo(d,drec[p]);
    dbRead(d,'netztyp',nt);
    dbRead(d,'boxname',name);
    dbRead(d,'dateiname',dname);
    dbRead(d,'username',user);
    dbRead(d,'kommentar',komm);
    dbRead(d,'nameomaps',maps);
    dbRead(d,'realname',realname);
    dbRead(d,'domain',dom);
    dbRead(d,'fqdn',fqdom);
    dbread(d,'email',email);
    dbRead(d,'boxdomain',bdom);
    dbRead(d,'fidoname',fidoname);
    NameRead(nt,name,user,komm,maps,realname,dom,fqdom,email,bdom,fidoname,kompname,brk);
    if not brk then begin
      newbox:=(name<>dbReadStr(d,'boxname'));
      newibox:=(ustr(name)<>ustr(dbReadStr(d,'boxname')));
      if newbox then begin
        if newibox then begin
          dbSeek(d,boiName,ustr(name));
          if dbFound then begin
            rfehler(921);         { 'Box bereits vorhanden!' }
            brk:=true;
            end;
          end;
        if not brk then begin
          dbGo(d,drec[p]);
          if ustr(dbReadStr(d,'boxname'))=ustr(DefaultBox) then begin
            DefaultBox:=name; SaveConfig2; end else
          if dbReadstr(d,'boxname')=DefFidoBox then begin
            DefFidoBox:=name; SaveConfig2; end;
          old_dname:=dname;
          dname:='';
          dbWrite(d,'dateiname',dname);   { ! }
          dname:=getdname(nt,name);
          unique(dname);        { verÑndert pos(d)! }
          dbGo(d,drec[p]);
          dbWrite(d,'dateiname',dname);
          { evtl. User&Bretter Åberarbeiten }
        { DefaultBox:=name;   ?????
          SaveConfig2; }
          if dname<>old_dname then  { kann bei Fido evtl. gleich sein! }
            Rename_box(old_dname,dname);
          end;
        end;
      if not brk then begin
        boxpar^.boxname:=name;
        boxpar^.username:=user;
        if (nt=nt_Maus) then
          boxpar^.pointname:=name
        else if (boxpar^.pointname='') and (email<>'')
        then begin
          boxpar^.pointname:=mid(email,cpos('@',email)+1);
          truncstr(boxpar^.pointname,
            min(25,cposx('.',boxpar^.pointname)-1));
          end;
        boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;
        WriteBox(dname,boxpar);
        dbWrite(d,'pointname',boxpar^.pointname);
        dbWrite(d,'boxname',name);
        dbWrite(d,'username',user);
        dbWrite(d,'dateiname',dname);
        dbWrite(d,'kommentar',komm);
        dbWrite(d,'nameomaps',maps);
        dbWrite(d,'realname',realname);
        dbWrite(d,'domain',dom);
        dbWrite(d,'fqdn',fqdom);
        dbWrite(d,'email',email);
        dbWrite(d,'boxdomain',bdom);
        dbWrite(d,'fidoname',fidoname);
        dbFlushClose(d);
        dbGo(d,drec[1]); aufbau:=true;
        end;
      brk:=true;  { siehe unten! }
      end;
  end;

  procedure EditSysopdaten(nt:byte; var brk:boolean);
  var x,y   : byte;
      flags : byte;
      nobox : boolean;
      fl,el : byte;      { 923.fl = eines der Eingabefelder (fÅr LÑngenermittlung) }
  begin
    dialog(ival(getres2(923,iif(nt in [nt_Fido,nt_UUCP],0,1))),
           iif(nt=nt_QWK,12,11),
           getres2(923,iif(nt=nt_QWK,14,2)),x,y);
    dbRead(d,'script',flags);                             { 'Sysop-Mode' }
    nobox:=(flags and 16<>0);
    with boxpar^ do begin
      if (nt=nt_Fido) or (nt=nt_UUCP) then begin
        fl:=3; el:=35;
        maddstring(3,2,getres2(923,3),sysopinp,el,MaxLenPathname,ValidDirCh);   { 'Eingangsverzeichnis  ' }
        if nt=nt_UUCP then begin
          if pppMode then mhnr(30150) else mhnr(8036);
          end
        else mhnr(183);
        msetvfunc(testfidodir);
        maddstring(3,4,getres2(923,4),sysopout,el,MaxLenPathname,ValidDirCh);   { 'Ausgangsverzeichnis  ' }
        if nt=nt_UUCP then begin
          if pppMode then mhnr(30150) else mhnr(8036);
          end
        else mhnr(183);
        msetvfunc(testfidodir);
        end
      else if nt=nt_QWK then begin
        fl:=12; el:=33;
        maddstring(3,2,getres2(923,12),sysopinp,el,MaxLenPathname,ValidDirCh); mhnr(8035);  { 'Eingangspakete       ' }
        msetvfunc(testqwkinfiles);
        maddstring(3,4,getres2(923,13),sysopout,el,MaxLenPathname,ValidDirCh); mhnr(8035);  { 'Ausgangsverzeichnis  ' }
        msetvfunc(testfidodir);
        end
      else begin
        fl:=5; el:=37;
        maddstring(3,2,getres2(923,5),sysopinp,el,MaxLenPathname,ValidDirCh);  mhnr(182);  { 'Eingangspuffer  ' }
        msetvfunc(validfile);
        maddstring(3,4,getres2(923,6),sysopout,el,MaxLenPathname,ValidDirCh); mhnr(182);   { 'Ausgangspuffer  ' }
        msetvfunc(validfile);
        end;
      fl:=length(getres2(923,fl));
      maddstring(3,6,forms(getres2(923,20),fl),sysopstart,el,MaxLenPathname,''); mhnr(8037);
          { 'Startprogramm ' }
      maddstring(3,8,forms(getres2(923,21),fl),sysopend,el,MaxLenPathname,''); mhnr(8037);
          { 'Endprogramm   ' }
      if nt=nt_QWK then begin
        maddbool(3,10,getres2(933,8),delqwk);  { 'QWK-Pakete nach Verarbeitung lîschen' }
        mhnr(8039);
        end;
      maddbool(3,iif(nt=nt_QWK,11,10),getres2(923,7),SysopNetcall); mhnr(184);
          { '/ØNetzanruf-Bericht' }
      if nt=nt_Netcall then begin  { s. auch XP6 - Absender-Erzeugung f. PP }
        maddbool(ival(getres2(923,10)),10,getres2(923,8),nobox); mhnr(185);
        end;                                      { 'Username ohne @BOX' }
      if nt=nt_Fido then begin
        maddbool(34,10,getres2(923,9),sysoppack); mhnr(186);  { 'Ausgangspakete packen' }
        end;
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        flags:=flags and (not 16) + iif(nobox,16,0);
        dbWrite(d,'script',flags);
        end;
      end;
  end;

  procedure EditDiverses(nt:byte; var brk:boolean);
  var x,y,i : byte;
      flags : byte;
      nall  : boolean;
      x2    : byte;
      add   : shortint;
      av,pv : string[20];
  begin
    add:=0;
    if (nt=nt_uucp) and boxpar^.pppMode then
      dialog(ival(getres2(924,0)),11,getres2(924,1),x,y)
    else
      dialog(ival(getres2(924,0)),
             14+iif(nt=nt_UUCP,1,0)+iif(ntNetcall(nt),excludes+iif(nt=nt_Fido,1,3),0),getres2(924,1),x,y);
    dbRead(d,'script',flags);            { 'Verschiedene Einstellungen' }
    dbRead(d,'AVertreter',av);
    dbRead(d,'PVertreter',pv);
    nall:=(flags and 2=0);
    with boxpar^ do begin
      if (nt=nt_uucp) and pppMode then add:=-4
      else begin
        maddtext(3,2,getres2(924,2),col.coldiahigh);    { 'Online-Anruf' }
        maddstring(20,2,getres2(924,3),o_passwort,25,25,''); mhnr(187);  { 'Pa·wort ' }
        maddstring(20,3,getres2(924,4),o_logfile,25,60,''); { 'Logfile ' }
        msetvfunc(testlogfile);
        maddstring(20,4,getres2(924,13),o_script,25,50,''); { 'Script'   }
        msetvfunc(testscript); mhnr(8030);
        mset3proc(scripterrors);
        if ntNetcall(nt) then begin
          maddtext(3,6,getres2(924,5),col.coldiahigh);    { 'Netzanruf'  }
          maddtext(20,6,getres2(924,14),0);               { 'Ausschlu·-' }
          maddtext(20,7,getres2(924,15),0);               { 'zeiten:'    }
          x2:=42+length(getres2(924,6));
          for i:=1 to excludes do begin
            maddtime(32,5+i,getres2(924,6),exclude[i,1],false); mhnr(189);  { 'von' }
            maddtime(x2,5+i,getres2(924,7),exclude[i,2],false); mhnr(189);  { 'bis' }
            end;
          inc(add,excludes+1);
          if nt<>nt_Fido then begin
            maddstring(20,7+excludes,getres2(924,13),script,25,50,'');  { 'Script' }
            msetvfunc(testscript); mhnr(8031);
            mset3proc(scripterrors);
            inc(add,2);
            end;
          end;
        end;
      maddtext(3,6+add,getres2(924,8),col.coldiahigh);  { 'Filter' }
      maddstring(20,6+add,getres2(924,9),eFilter,25,60,''); mhnr(199);  { 'Eingang' }
      maddstring(20,7+add,getres2(924,10),aFilter,25,60,''); mhnr(8050); { 'Ausgang' }
      maddtext(3,9+add,getres2(924,16),col.coldiahigh);   { 'Vertretersystem' }
      DomainNt:=nt;
      maddstring(20,9+add,getres2(924,17),av,20,20,'>'); mhnr(8032);  { 'fÅr AMs' }
      amvfield:=fieldpos;
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddstring(20,10+add,getres2(924,18),pv,20,20,'>'); mhnr(8033);  { 'fÅr PMs' }
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddtext(3,12+add,getres2(924,11),col.coldiahigh);    { 'Sonstiges' }
      maddbool(13,12+add,getres2(924,19),ReplaceOwn); mhnr(8051);  { 'lokale Kopien ersetzen' }
      if nt=nt_uucp then begin
        maddbool(13,13+add,getres2(924,20),EB_Daemon); mhnr(8052);  { 'EB-Absender: MAILER-DAEMON' }
        maddbool(13,14+add,getres2(924,12),nall); mhnr(198);  { '/Netcall/Alle' }
        end
      else
        maddbool(13,13+add,getres2(924,12),nall); mhnr(198);  { '/Netcall/Alle' }
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        flags:=flags and $fd+iif(nall,0,2);
        dbWrite(d,'script',flags);
        dbWrite(d,'AVertreter',av);
        dbWrite(d,'PVertreter',pv);
        for i:=1 to excludes do
          if cpos(' ',exclude[i,1]+exclude[i,2])>0 then begin
            exclude[i,1]:='  :  '; exclude[i,2]:='  :  ';
            end;
        if ntOnePW(nt) then passwort:=o_passwort;
        end;
      end;
  end;

  procedure EditNet(var brk:boolean);
  var x,y,a  : byte;
      width  : byte;
      alias  : boolean;
      mids   : boolean;
      replyto: string[AdrLen];
      maps   : string[20];
      nt     : byte;
      _nname : boolean;   { Netzname }
      _alias : boolean;
      _mids  : boolean;
      _brett : boolean;   { Brettebene }
      _atp   : boolean;   { @Point.zer }
      _repto : boolean;   { PM-Vertreter }
      _light : boolean;   { LightNET-Login }
      _brettm: boolean;   { Brettnachrichten anfordern }
  begin
    with boxpar^ do begin
      dbRead(d,'netztyp',nt);
      pp_da:=exist(dbReadStr(d,'dateiname')+'.PP');
      _nname:=false; _alias:=false; _mids:=false; _brett:=false;
      _atp:=false; _repto:=false; _light:=false; _brettm:=false;
      width:=34;
      case nt of
        nt_Netcall  : begin
                        _atp:=true;
                        width:=35;
                      end;
        nt_ZConnect : begin
                        _repto:=true; _atp:=true;
                        width:=50;
                      end;
        nt_Magic    : begin
                        _nname:=true; _alias:=true; _mids:=true; _brett:=true;
                        _light:=true;
                      end;
        nt_Pronet   : begin
                        _nname:=true; _brett:=true;
                      end;
        nt_Quick    : begin
                        fehler('keine Einstellungen fÅr QuickMail nîtig');
                        exit;
                      end;
        nt_GS       : begin
                        fehler('keine Einstellungen fÅr G&S-Mailbox nîtig');
                        exit;
                      end;
        nt_Maus     : begin
                        _brett:=true; _brettm:=true;
                        width:=40;
                      end;
        nt_QWK      : _brett:=true;
      end;
      dialog(width,iif(_nname,2,0)+iif(_alias,1,0)+iif(_mids,2,0)+iif(_brett,2,0)+
                   iif(_atp,2,0)+iif(_repto,2,0)+iif(_light,1,0)+iif(_brettm,2,0)+1,
                   getres2(925,1),x,y);   { 'netzspezifische Einstellungen' }
      dbRead(d,'script',a);
      alias:=(a and 4<>0);
      mids:=(a and 8=0);
      dbRead(d,'replyto',replyto);
      dbRead(d,'nameomaps',maps);
      y:=0;
      if _mids then begin
        inc(y,2);
        maddbool(3,y,getres2(925,2),mids); mhnr(650);   { 'Message-IDs' }
        end;
      if _alias then begin
        inc(y);
        maddbool(3,y,getres2(925,3),alias); mhnr(653);  { 'Alias-Point' }
        end;
      if _light then begin
        inc(y);
        maddbool(3,y,getres2(925,4),LightLogin); mhnr(657);  { 'LightNET-Login' }
        end;
      if _nname then begin
        inc(y,2);
        maddstring(3,y,getres2(925,5),magicnet,8,8,''); mhnr(651);
        if nt=nt_Magic then                              { 'Netzname ' }
          mappsel(true,'MagicNET˘LightNET')
        else
          mappsel(true,'ProNET˘TopNET');
        end;
      if _brett then begin
        inc(y,2);
        maddstring(3,y,getres2(925,6),magicbrett,17,25,'A'+without(AllChar,' ')); mhnr(652);
        if (nt<>nt_Pronet) then                          { 'Bretter  ' }
          mappsel(false,'/MAGIC/˘/MAUS/˘/')
        else
          mappsel(false,'/PRONET/˘/TOPNET/');
        msetvfunc(testmbretter);
        end;
      if _brettm then begin
        inc(y,2);                          { 'Brettnachrichten anfordern' }
        maddbool(3,y,getres2(920,55),Brettmails); mhnr(8040);
        end;
      if _repto then begin
        inc(y,2);
        maddstring(3,y,getres2(925,8),replyto,33,eAdrLen,''); mhnr(656);
        msetvfunc(testreplyto);                          { 'Vertreter ' }
        end;
      if _atp then begin
        inc(y,2);
        maddbool(3,y,getres2(925,iif(nt=nt_Netcall,7,12)),alias); mhnr(655);
        end;             { 'Absender User@Point.ZER' / '..@Point.domain' }
      freeres;
      readmask(brk);
      if not brk then begin
        a:=a and (not (4+8+32));
        if alias then inc(a,4);
        if not mids then inc(a,8);
        dbWrite(d,'script',a);
        dbWrite(d,'replyto',replyto);
        dbWrite(d,'nameomaps',maps);
        dbFlushClose(d);
        end;
      end;
    enddialog;
  end;

  procedure EditFido(var brk:boolean);
  var x,y : byte;
      maps: string[20];
      nli : boolean;
  begin
    dbGo(d,drec[p]);
    pp_da:=exist(dbReadStr(d,'dateiname')+'.PP');
    with boxpar^ do begin
      dbRead(d,'nameomaps',maps);
      dialog(ival(getres2(926,0)),13,getres2(926,1),x,y);  { 'Fido-Einstellungen' }
      maddtext(3,2,getres2(926,2),col.coldiahigh);        { 'Areafix' }
      maddstring(14,2,getres2(926,3),maps,12,20,''); mhnr(700);  { 'Name       ' }
      mappsel(false,'Areafix˘Areamgr');
      msetvfunc(notempty);
      maddstring(14,3,getres2(926,4),AreaPW,12,12,'');   { 'Pa·wort    ' }
      maddbool(14,5,getres2(926,5),AreaPlus);      { '"+" bei Bestellungen' }
      maddbool(14,6,getres2(926,6),AreaBetreff);   { 'Befehle im Betreff' }
      nli:=not LocalINTL;
      maddbool(14,7,getres2(926,12),nli);    { 'd''Bridge-Areafix' }
      maddtext(3,9,getres2(926,9),col.coldiahigh);       { 'FileScan' }
      maddstring(14,9,getres2(926,10),Filescanner,12,15,''); { 'Name       ' }
      mappsel(false,'FileScan˘FileMgr˘FileFix˘AllFix');
      maddstring(14,10,getres2(926,11),FilescanPW,12,12,'');  { 'Pa·wort    ' }
      maddtext(3,12,getres2(926,7),col.coldiahigh); { 'Diverses' }
      maddstring(14,12,getres2(926,8),MagicBrett,12,25,'A'+without(AllChar,' '));  { 'Brettebene ' }
      mappsel(false,'/FIDO/˘/GERNET/˘/VIRNET/');
      msetvfunc(testmbretter);
      freeres;
      readmask(brk);
      enddialog;
      end;
    if not brk then begin
      dbWrite(d,'nameomaps',maps);
      BoxPar^.LocalINTL:=not nli;
      end;
  end;

  procedure EditUUCP(var brk:boolean);
  var x,y,bl   : byte;
      bmname,
      _bmtyp   : string[20];
      replyto  : string[AdrLen];
      alias    : boolean;
      a        : byte;
  begin
    with BoxPar^ do begin
      dialog(53,13,getres2(928,1),x,y);   { 'UUCP-Einstellungen' }
      dbRead(d,'replyto',replyto);
      bl:=length(getres2(928,3));
      maddtext(3,2,getres2(928,2),col.coldiahigh);     { 'Vertreter' }
      maddstring(3+bl,2,'',replyto,30,eAdrLen,''); mhnr(656);
        msetvfunc(testreplyto);
      maddtext(3,4,getres2(928,3),col.coldiahigh);     { 'Brettmanager' }
      case BMtyp of
        bm_changesys : _bmtyp:='Changesys';
        bm_GUP       : _bmtyp:='GUP';
        bm_Feeder    : _bmtyp:='Feeder';
        bm_AutoSys   : _bmtyp:='AutoSys';
      else             _bmtyp:='manuell';
      end;
      maddstring(3+bl,4,getres2(928,4),_bmtyp,15,15,'');   { 'Typ     ' }
      mappsel(true,'Changesys˘GUP˘Feeder˘AutoSys˘manuell'); mhnr(658);
      dbRead(d,'nameomaps',bmname);
      maddstring(3+bl,5,getres2(928,5),bmname,15,15,'');   { 'Name    ' }
      mappsel   (false,'changesys˘GUP˘feeder˘autosys˘postmaster'); mhnr(658);
      maddstring(3+bl,6,getres2(928,6),AreaPW,12,12,'');   { 'Pa·wort ' }
        mhnr(658);
      maddbool  (3+bl,8,getres2(928,7),BMdomain); mhnr(658);  { 'Domain mitsenden' }
      maddstring(3,10,getres2(928,8),chsysbetr,30,50,'');   { 'Sysfile-Betreff ' }
        mhnr(658);
      alias:=(dbReadInt(d,'script') and 4<>0);
      maddbool(3,12,getres2(928,9),alias);  { 'Absender User@Server.Serverdomain' }
        mhnr(655);
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        dbWrite(d,'nameomaps',bmname);
        dbWrite(d,'replyto',replyto);
        a:=dbReadInt(d,'script') and not 4;
        if alias then inc(a,4);
        dbWrite(d,'script',a);
        if stricmp(_bmtyp,'changesys') then BMtyp:=bm_changesys else
        if stricmp(_bmtyp,'GUP')       then BMtyp:=bm_GUP       else
        if stricmp(_bmtyp,'Feeder')    then BMtyp:=bm_Feeder    else
        if stricmp(_bmtyp,'AutoSys')   then BMtyp:=bm_AutoSys   else
          BMtyp:=bm_postmaster;
        end;
      end;
  end;

  procedure EditPPP(var brk:boolean);
  var
      b,x,y,nt     : byte;
      replyto      : string[AdrLen];
      email        : string[80];
      domain,fqdn  : string[60];
      user         : string[40];
      name         : string[20];
      komm         : string[30];
      realname     : string[40];
      dname,
      old_dname    : string[8];

  begin
    with boxpar^ do begin

      dbGo(d,drec[p]);
      dbread(d,'email',email);
      dbRead(d,'realname',realname);
      dbRead(d,'replyto',replyto);
      dbRead(d,'fqdn',fqdn);
      dbRead(d,'username',user);
      dbRead(d,'domain',domain);
      dbRead(d,'pointname',pointname);
      dbRead(d,'boxname',name);
      dbRead(d,'kommentar',komm);
      dbRead(d,'dateiname',dname);

      _EditPPP(brk,name,komm,email,realname,replyto,fqdn,PPPClientPath,
               PPPClient,PPPAddServers,PPPDialUp,PPPPhone,PPPLogin,PPPPass,getres2(920,80));

      if not brk then begin
        b:=cpos('@',email);
        user:=left(email,b-1);
        domain:=mid(email,b);
        if cpos('.',domain)=0 then domain:=''
          else domain:=mid(domain,cpos('.',domain));
        pointname:=mid(email,b+1);
        truncstr(pointname,min(25,cposx('.',pointname)-1));
        sysopinp:='';

        if name<>dbReadStr(d,'boxname')
        then begin
          if ustr(name)<>ustr(dbReadStr(d,'boxname'))
          then begin
            dbSeek(d,boiName,ustr(name));
            if dbFound then begin
              rfehler(921);         { 'Box bereits vorhanden!' }
              brk:=true;
              end;
            end;
          if not brk then begin
            dbGo(d,drec[p]);
            if ustr(dbReadStr(d,'boxname'))=ustr(DefaultBox) then begin
              DefaultBox:=name; SaveConfig2; end;
            old_dname:=dname;
            dname:='';
            dbWrite(d,'dateiname',dname);   { ! }
            dname:=getdname(nt,name);
            unique(dname);        { verÑndert pos(d)! }
            dbGo(d,drec[p]);
            dbWrite(d,'dateiname',dname);
            if dname<>old_dname then  { kann bei Fido evtl. gleich sein! }
              Rename_box(old_dname,dname);
            end;
          end;

        if not brk then
        begin
          boxpar^._fqdn:=fqdn;
          boxpar^.username:=user;
          boxpar^._domain:=domain;
          boxpar^.boxname:=name;
          WriteBox(dname,boxpar);
          dbwrite(d,'dateiname',dname);
          dbWrite(d,'pointname',pointname);
          dbWrite(d,'email',email);
          dbWrite(d,'boxname',name);
          dbWrite(d,'kommentar',komm);
          dbWrite(d,'realname',realname);
          dbWrite(d,'replyto',replyto);
          dbWrite(d,'fqdn',fqdn);
          dbWrite(d,'username',user);
          dbWrite(d,'domain',domain);
          dbFlushClose(d);
          dbGo(d,drec[1]); aufbau:=true; brk:=true;
          end;
        end;
      end;
  end;


  procedure EditQWK(dateiname:pathstr; var brk:boolean);
  const maxnmt = 10;
  var   x,y    : byte;
        eqfg   : boolean;
        qfg    : QfgRec;
        nmta   : array[1..maxnmt] of record
                                       nr   : byte;
                                       name : string[30];
                                     end;
        nmtn,i : integer;
        nmtyp  : string[30];

    procedure ReadNmta;
    var t   : text;
        tmp : pathstr;
        s   : string[50];
    begin
      tmp:=TempS(4096);
      shell('zqwk.exe -nmt >'+tmp,350,3);
      nmtn:=0;
      assign(t,tmp);
      if existf(t) then begin
        reset(t);
        while not eof(t) and (nmtn<maxnmt) do begin
          readln(t,s);
          inc(nmtn);
          nmta[nmtn].nr:=ival(GetToken(s,' '));
          nmta[nmtn].name:=trim(left(s,30));
          end;
        close(t);
        erase(t);
        end;
    end;

    procedure AppQwkEchos;
    var t : text;
        s : string[80];
    begin
      assign(t,dateiname+'.bl');
      if existf(t) then begin
        reset(t);
        while not eof(t) do begin
          readln(t,s);
          if (ival(left(s,3))>0) or (left(s,3)='  0') then begin
            s:=trim(mid(s,5));
            mappsel(false,trim(left(s,50)));   { Flags in BlueWave-Listen abschneiden }
            end;
          end;
        close(t);
        end;
    end;

  begin
    eqfg:=exist(dateiname+QfgExt) and exist('zqwk.exe');
    if eqfg then begin
      ReadQfg(dateiname,qfg);
      ReadNmta;
      end;
    dialog(ival(getres2(933,0)),iif(eqfg,12,3),getres2(933,1),x,y);  { 'QWK-Einstellungen' }
    with boxpar^,qfg do begin
      maddstring(3,2,getres2(933,2),MagicBrett,21,25,'A'+without(AllChar,' '));
      mappsel(false,'/FIDO/˘/QWK/˘/');
      msetvfunc(testmbretter); mhnr(980);      { 'Brettebene     ' }
      if eqfg then begin
        nmtyp:='';
        for i:=1 to nmtn do
          if nmt=nmta[i].nr then nmtyp:=nmta[i].name;
        maddstring(3,4,getres2(933,3),nmtyp,21,30,'');    { 'Netmail-Typ  ' }
        for i:=1 to nmtn do
          mappsel(true,nmta[i].name);
        maddstring(3,5,getres2(933,4),netecho,21,50,'');  { 'Netmail-Echo ' }
        AppQwkEchos;
        maddstring(3,6,getres2(933,5),privecho,21,50,''); { 'PM-Echo      ' }
        AppQwkEchos;
        maddstring(3,7,getres2(933,9),emailecho,21,50,''); { 'EMail-Echo   ' }
        AppQwkEchos;
        maddint   (3,9,getres2(933,6),midtyp,2,1,0,9);    { 'Message-ID-Typ' }
        maddbool  (3,11,getres2(933,7),hdr);     { 'Header im Nachrichtentext' }
        end;
      readmask(brk);
      if not brk and eqfg then begin
        for i:=1 to nmtn do
          if stricmp(nmtyp,nmta[i].name) then nmt:=nmta[i].nr;
        bretter:=MagicBrett;
        WriteQFG(dateiname,qfg);
        end;
      enddialog;
      end;
  end;


  procedure EditBox;
  const edb_pos : shortint = 1;
     clientpos  : array[1..7] of Byte = (1,4,5,1,1,6,7);
     lastclient : boolean = false;
  var n   : shortint;
      fn  : string[12];
      brk : boolean;
      nt  : byte;
      nts : string[20];
      nts0: string[70];
      nts2: string[40];
  begin
    dbGo(d,drec[p]);
    dbRead(d,'dateiname',fn);
    if fn='' then fn:=left(dbReadStr(d,'boxname'),8);
    dbRead(d,'netztyp',nt);
    ReadBox(nt,fn,boxpar);
    nts:=''; nts2:=getres2(927,5)+getres2(927,6);   { ',^SysopMode' }
    if (nt=nt_uucp) and boxpar^.pppMode then
    begin
      Pushhp(132);
      nts0:='^Client,'+getres2(927,8)+getres2(927,9)+getres2(927,2)+getres2(927,3);
      nts2:=getres2(927,5)+getres2(927,6);
      if not lastclient then edb_pos:=clientpos[edb_pos];
      lastclient:=true;
      end 
    else begin
      Pushhp(131);
      if lastclient then
      begin
        if (edb_pos=2) or (edb_pos=3) then
          edb_pos:=1
        else if (edb_pos=4) or (edb_pos=5) then
          dec(edb_pos,2);
        end;
      lastclient:=false;
      nts0:=getres2(927,1)+getres2(927,2)+getres2(927,3)+getres2(927,4);
      case nt of
        nt_Netcall : nts:='^Z-Netz,';
        nt_ZConnect: nts:='^ZConnect,';
        nt_Maus    : nts:='M^aus,';
        nt_Magic   : nts:='M^agicNET,';
        nt_Pronet  : nts:='P^ronet,';
        nt_Fido    : nts:='^Fido,';
        nt_UUCP    : begin nts:='^RFC/UUCP,'; { nts2:=''; } end;
        nt_QWK     : begin
                       nts:=getres2(927,4)+'^QWK,'; nts0:='';
                       nts2:=getres2(927,5)+getres2(927,7);  { ',^Transfer' }
                       end;
        end;
      end; 
    n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts0+nts+nts2,edb_pos);
                 { '^Point,^Modem,^GebÅhren,^Namen,'+nts+'^Diverses'+nts2 }
    freeres;
    if n<>0 then edb_pos:=abs(n);
    if n>0 then begin
      if (nt=nt_uucp) and boxpar^.pppMode then 
      begin
        case n of
          1 : begin dbGo(d,drec[p]); EditPPP(brk); end;
          2 : EditTransport(nt,brk);
          3 : EditExtern(fn,brk);
          4 : EditModemdaten(nt,brk);
          5 : EditGebuehren(nt,brk);
          6 : EditDiverses(nt,brk);
          7 : EditSysopdaten(nt,brk);
          end;
        end
      else begin 
        if (nts='') and (n>=5) then inc(n);
        if nts0='' then inc(n,3);
        case n of
          1 : EditPointdaten(nt,brk);
          2 : EditModemdaten(nt,brk);
          3 : EditGebuehren(nt,brk);
          4 : EditName(brk);
          5 : case nt of
                nt_Fido : EditFido(brk);
                nt_QWK  : EditQWK(fn,brk);
                nt_UUCP : EditUUCP(brk);
                else      EditNet(brk);
              end;
          6 : EditDiverses(nt,brk);
          7 : EditSysopdaten(nt,brk);
          end;
        end;
      if not brk then
        WriteBox(fn,boxpar);
      brk:=reg_hinweis;
      reg_hinweis:=false; { Damit der Lizenz-Bildschirm nicht angezeigt wird }
      ReadDomainList;
      reg_hinweis:=brk;
      end;
    pophp;
  end;


  procedure EditNetztyp;
  var brk : boolean;
      nt  : byte;
      nt1 : byte;
      x,y : byte;
      maps: string[20];
      fn  : String[12];
      box : string[BoxNameLen];

    procedure SetBrettNetztyp;
    var n    : integer;
        i    : LongInt;
        x,y  : byte;
        flags: byte;
        uucp : byte;
        mi   : byte;
    begin
      msgbox(38,3,'',x,y);
      wrt(x+3,y+1,'öberarbeite Brettliste ...     %');
      mi:=dbGetIndex(bbase);
      dbSetIndex(bbase,0);
      dbGoTop(bbase);
      n:=dbRecCount(bbase);
      uucp:=iif(nt=nt_UUCP,16,0);
      i:=1;
      while not dbEOF(bbase) do begin
        gotoxy(x+31,y+1); moff;
        write(i*100 div n); mon;
        if ustr(dbReadStrN(bbase,bb_pollbox))=ustr(box) then begin
          flags:=dbReadInt(bbase,'flags') and not 16 + uucp;
          dbWriteN(bbase,bb_flags,flags);
          end;
        dbNext(bbase);
        inc(i);
        end;
      dbSetIndex(bbase,mi);
      dbGoTop(bbase);
      closebox;
      xp0.aufbau:=true;
    end;

  begin
    dbGo(d,drec[p]);
    if exist(dbReadStr(d,'dateiname')+'.pp') then begin
      msgbox(60,9,_fehler_,x,y);
      mwrt(x+3,y+2,getreps2(929,1,    { 'Es sind Polldaten fÅr diese Box vorhanden (%s.PP)' }
                   ustr(dbReadStr(d,'dateiname'))));
      mwrt(x+3,y+3,getres2(929,2));   { '- Netztyp kann nicht geÑndert werden.  Versenden oder' }
      mwrt(x+3,y+4,getres2(929,3));   { 'lîschen Sie bitte zuerst die unversandten Nachrichten' }
      mwrt(x+3,y+5,getres2(929,4));   { 'fÅr diese Box!' }
      mwrt(x+3,y+7,getres(12));       { 'Taste drÅcken...' }
      errsound;
      wait(curoff);
      closebox;
      end
    else begin
      dbRead(d,'netztyp',nt);
      dbread(d,'dateiname',fn);
      ReadBox(nt,fn,boxpar);
      if (Boxpar^.pppMode) and (nt = 40) then nt:=41;  
      nt1:=nt;
      dbRead(d,'boxname',box);
      ReadNetztyp(box,nt,brk);
      if not brk and (nt<>nt1) and
         ReadJN(getreps2(929,5,box),true) then begin
                       { 'Mîchten Sie den Netztyp fÅr %s wirklich Ñndern' }
        Boxpar^.pppMode:=nt=41;
        if nt=41 then nt:=40;
        WriteBox(fn,BoxPar);
        dbWrite(d,'netztyp',nt);
        dec(ntused[nt1]);
        inc(ntused[nt]);
        if nt=nt_Maus then begin
          maps:='MAUS';
          dbWrite(d,'nameomaps',maps);
          end;
        dbFlushClose(d);
        SetBrettNetztyp;
        dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
    freeres;
  end;

{
  $Log$
  Revision 1.10.2.45  2001/08/12 12:03:33  mk
  - optimized last checkin a bit more ;)

  Revision 1.10.2.44  2001/08/12 11:45:57  my
  - optimized code a bit, removed unnecessary resources, adjusted resource
    numbers
  - removed XPEasy resources

  Revision 1.10.2.43  2001/08/12 11:20:38  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.10.2.42  2001/08/11 22:18:02  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.10.2.41  2001/08/03 00:38:50  my
  - added missing 'ustr' in EditExtern

  Revision 1.10.2.40  2001/08/02 14:33:53  my
  - EditExtern: filename of a non-existing file is written to
    <server>.BFG only if the user confirmed file creation *and*
    indeed created the file
  JG:- EditExtern: optimized suboptimal (but working) code;

  Revision 1.10.2.39  2001/08/01 15:51:41  my
  - EditExtern: If LFNs are enabled, LFN is displayed in input field
    and Editor (but as before, SFN is stored in .BFG)
  - changed order of "Client call" selection list (<F2>)

  Revision 1.10.2.38  2001/07/31 15:36:01  my
  - implemented is_emailaddress in NameRead
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.10.2.37  2001/07/23 17:05:36  my
  - changed "fehler(getres2(10900,8))" to "rfehler(908)"

  Revision 1.10.2.36  2001/07/23 16:52:48  my
  JG+MY:- RFC/Client: implemented check for valid (multiple) eMail addresses
          under Edit/Servers/Edit/Mail/News_Servers/Envelope_address (In+Out)
  JG+MY:- RFC/Client: removed unnecessary mask test in _EditPPP
  JG+MY:- RFC/UUCP: improved check for valid eMail address under
          Edit/Servers/Edit/Names/eMail_address

  Revision 1.10.2.35  2001/07/11 01:48:57  my
  JG:- Fix: write server name to .BFG after rename of server
  JG:- Display net type "RFC" for RFC/UUCP and RFC/Client
       in Edit User and Edit Message Area dialogues

  Revision 1.10.2.34  2001/07/09 22:17:05  my
  - Copying server parameters from an existing server upon creation of a new
    RFC/Client server changed to a useful behaviour

  Revision 1.10.2.33  2001/06/29 16:36:39  my
  - extended POP3/SMTP/NNTP server list
  - moved list to ressources
  - extended list of ports for incoming mail

  Revision 1.10.2.32  2001/06/29 01:24:56  my
  - RFC/Client: implemented Mail/News server configuration

  Revision 1.10.2.31  2001/06/24 21:24:38  my
  - added RFC/client specific items for Edit/Servers/Edit...
    Modem, Charges and SysopMode

  Revision 1.10.2.30  2001/06/22 18:14:00  my
  - optimized code a little bit

  Revision 1.10.2.29  2001/06/19 17:01:47  my
  - minor fixes with nil effect :-)

  Revision 1.10.2.28  2001/06/19 01:24:45  my
  - RFC/Client: removed superfluos items in Edit/Servers/Edit/Misc.

  Revision 1.10.2.27  2001/06/16 15:19:43  my
  - New server configuration parameter "Client-Phone", "Client-Login"
    and "Client-Password"

  Revision 1.10.2.26  2001/06/16 02:10:36  my
  - New help item @132 for Edit/Servers/Edit/<F1> (RFC/Client)
  - New server configuration parameter "Client-DialUp="
  - Re-arranged Edit/Servers/Edit/Client dialogue a bit

  Revision 1.10.2.25  2001/06/13 02:10:09  my
  JG/MY:- New Server type "RFC/Client" (formerly "Client Mode"):
          - All vital client settings from Edit/Point, Edit/Names and
            Edit/RFC/UUCP are summarized under one item Edit/Client now.
            Superfluous RFC/UUCP settings have been removed (well, more
            hidden in fact ;)).
          - introduced simplified entry "eMail address" (rather than composing
            it of removed entries user name, point name and domain).
          - new FQDN festures: "@" is replaced with ".", and "_" with "-"
            automatically. <F2> selection now shows the result of the
            proposed FQDN rather than a fixed string. Special T-Online FQDN
            support (".dialin.").
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
          - new unit XP9SEL as unit XP9 exceeded 64K size.
  JG/MY:- Server type RFC/UUCP:
          - introduced simplified entry "eMail address". If empty, the entries
            user name, point name and domain are automatically filled with the
            appropriate values taken from this eMail address.
          - re-designed Edit/Point to the "old" stage (removed Client Mode specific
            stuff). Kept new BSMTP options "SMTP/UUCP" and "SMTP/Client".
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
        - Removed superfluous code in connection with the changes above, updated
          and cleaned up resource and help files (still a lot to do for the English
          part).

  Revision 1.10.2.24  2001/04/28 13:38:55  mk
  - Client-Boxen umbenennen implementiert

  Revision 1.10.2.23  2001/04/23 18:43:08  mk
  - Spoolverzeichnis bei Boxen umbenennen loeschen

  Revision 1.10.2.22  2001/04/23 06:38:47  mk
  - E/B/E/P Maske vergroessert

  Revision 1.10.2.21  2001/04/20 17:28:50  mk
  - misc updates

  Revision 1.10.2.20  2001/04/19 15:03:06  mk
  - -client

  Revision 1.10.2.19  2001/04/14 21:10:41  mk
  - Client-Modus verbessern

  Revision 1.10.2.18  2001/04/14 10:07:01  mk
  - Anpassungen Client-Modus

  Revision 1.10.2.17  2001/04/10 11:36:38  mk
  - weitere Anpassungen Client-Modus

  Revision 1.10.2.16  2001/04/09 16:47:18  mk
  - arbeiten am Client-Modus

  Revision 1.10.2.15  2001/04/03 17:19:45  mk
  - fixes fuer Client-Modus

  Revision 1.10.2.14  2001/03/19 17:35:46  mk
  - neuer Brettmanager

  Revision 1.10.2.13  2001/01/30 10:01:23  mk
  - weitere arbeiten am Client-Modus

  Revision 1.10.2.12  2001/01/18 23:59:59  mk
  - verschiedene Aenderungen fuer PPP-Modus

  Revision 1.10.2.11  2001/01/16 08:13:05  mk
  - Grosschreibung von Dateinamen

  Revision 1.10.2.10  2001/01/10 17:39:06  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.10.2.9  2000/12/23 12:07:58  mk
  - fixed last commit

  Revision 1.10.2.8  2000/12/23 09:49:10  mk
  - Pointname bei ZConnect jetzt auch kleingeschr. erlaubt

  Revision 1.10.2.7  2000/12/21 21:55:46  mk
  - Janus+ jetzt Hilfe-Nr. 8019

  Revision 1.10.2.6  2000/12/20 18:09:22  mk
  - Schalter fuer PPP-Option von UUZ

  Revision 1.10.2.5  2000/12/19 18:58:35  mk
  - Pointname unter UUCP jetzt auch kleingeschrieben erlaubt

  Revision 1.10.2.4  2000/12/03 14:06:58  mk
  - Serverdom mit Punkt

  Revision 1.10.2.3  2000/10/05 20:29:49  fe
  RFC/UUCP: Hostname masquerading / UUCP-Alias-Points repariert:
  Statt "User@Server.domain" jetzt "User@Server.Serverdomain".

  Revision 1.10.2.2  2000/09/25 03:46:22  mk
  - Loginfo hinzugefuegt

  16.01.00 HS: Eingabemîglichkeit fÅr FQDNs

}

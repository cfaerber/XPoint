{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }


  { --- Bearbeitungs-Routinen fÅr Boxen-Liste -----------------------}

  {16.01.00 HS: Eingabemîglichkeit fÅr FQDNs hinzugefÅgt.}
  procedure NameRead(nt:byte; var name,user,komm,maps,realname,domain,fqdn,
                     bdomain,fidoname:string; txt:atext; var brk:boolean);
  var x,y  : byte;
      real : boolean;
      dom  : boolean;
      fqdom: boolean;  {16.01.00 HS}
      bdom : boolean;
      map  : boolean;
      blen : byte;
      btxt : string[15];
      fido : boolean;
      s    : string[20];
      yy   : byte;
  begin
    DomainNt:=nt;
    real:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP,nt_Turbo];
    dom:=(nt=nt_ZConnect) or (nt=nt_Magic) or (nt=nt_Pronet) or (nt=nt_Fido) or (nt=nt_UUCP);
    fqdom:=(nt=nt_ZConnect);  {16.01.00 HS: Eingabe fÅr FQDN nur bei ZConnect}
    bdom:=(nt=nt_UUCP);
    map:=(nt<>nt_Maus) and (nt<>nt_Fido) and (nt<>nt_Turbo) and (nt<>nt_UUCP)
         and (nt<>nt_QWK);
    fido:=(nt=nt_Fido);
    dialog(ival(getres2(912,0)),7+iif(map,2,0)+iif(real,2,0)+iif(dom,2,0)+
           iif(fqdom,2,0)+iif(bdom,2,0)+iif(fido,2,0),txt,x,y);  {16.01.00 HS}
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
    msetvfunc(notempty);
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
      if nt=nt_Turbo then mhnr(8038)
      else mhnr(194);
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
      inc(yy,2);
      end
    else
      bDomainNt:=0;
    if fqdom then begin  {16.01.00 HS: Eingabemîglichkeit fÅr FQDNs}
      maddstring(3,yy,getres2(912,11),fqdn,30,60,          { 'FQDN      ' }
                 'A'+without(range('!','~'),' @<>/')); mhnr(30001);
      inc(yy,2);
      end;
    freeres;
    readmask(brk);
    if not brk and (name=DefFidoBox) then begin
      HighlightName:=ustr(user);
      aufbau:=true;
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
    for i:=0 to enetztypen-1 do
      if (ntnr[i] in ntAllowed) then
        mappsel(true,ntName(ntnr[i]));
    readmask(brk);
    enddialog;
    if not brk then
      for i:=0 to enetztypen-1 do
        if lstr(ntyp)=lstr(ntName(ntnr[i])) then
          nt:=ntnr[i];
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

  procedure NewBox;
  var name : string[BoxNameLen];
      dname: string[8];
      user : string[30];
      komm : string[30];
      maps : string[20];
      real : string[40];
      dom  : string[60];
      fqdom: string[60];  {16.01.00 HS}
      bdom : string[60];
      fido : string[40];
      brk  : boolean;
      fn   : string[12];
      nt,i : byte;
      b    : byte;
  begin
    if empty then
      nt:=nt_Netcall
    else begin
      dbGo(d,drec[p]);
      dbRead(d,'netztyp',nt);
      end;
    ReadNetztyp(getres(914),nt,brk); if brk then exit;   { 'neue Box' }
    name:=''; user:=''; komm:=''; fido:='';
    maps:=DefaultMaps(nt);
    dom:=ntDefaultDomain(nt); bdom:=dom;
    fqdom:='';  {17.01.00 HS}
    real:='';
    {16.01.00 HS}
    NameRead(nt,name,user,komm,maps,real,dom,fqdom,bdom,fido,getreps(915,Netz_Typ(nt)),brk);
    if not brk then begin            { 'neue Box anlegen (Typ: %s)' }
      dbSeek(d,boiName,ustr(name));
      if dbFound then rfehler(921)   { 'Box ist bereits vorhanden' }
      else begin
        dbGo(d,drec[p]);
        dbRead(d,'dateiname',fn);
        ReadBox(nt,fn,boxpar);
        boxpar^.boxname:=name;
        boxpar^.username:=user;
        boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;  {16.01.00 HS}
        boxpar^.passwort:='GEHEIM';
        boxpar^.telefon:='';
        nt_bpar(nt,boxpar^);
        if boxpar^.prototyp='' then
          boxpar^.prototyp:='Z';
        boxpar^.sysopinp:=''; boxpar^.sysopout:='';
        boxpar^.sysopstart:=''; boxpar^.sysopend:='';
        if nt=nt_Pronet then boxpar^.pointname:='01'
        else boxpar^.pointname:='';
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
          if nt=nt_Turbo then
            magicbrett:='/'+name+'/';
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
        dname:=getdname(nt,name);      { eindeutigen Dateinamen bestimmen }
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
        dbWrite(d,'fqdn',fqdom);  {16.01.00 HS}
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
      pp:=_filesize(ownpath+dname+'.pp')>0;
      if ReadJN(iifs(pp,getres(916),'')+getreps(917,name),not pp)   { 'Polldaten vorhanden!! ' / '%s wirklich lîschen' }
      then begin
        dec(ntused[dbReadInt(d,'netztyp')]);
        dbDelete(d);
        dbFlushClose(d);
        if exist(ownpath+dname+bfgext) then     { .BFG lîschen }
          _era(ownpath+dname+bfgext);
        if exist(ownpath+dname+'.bl') then      { Brettliste lîschen }
          _era(ownpath+dname+'.bl');
        if exist(ownpath+dname+'.fl') then      { Fileliste lîschen }
          _era(ownpath+dname+'.fl');
        if exist(ownpath+dname+'.fbl') then     { Fileare-Liste lîschen }
          _era(ownpath+dname+'.fbl');
        if exist(ownpath+dname+'.inf') then     { Maus-Infofileliste lîschen }
          _era(ownpath+dname+'.inf');
        if pp then _era(OwnPath+dname+'.pp');   { Pollpaket lîschen }
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
    if (ntBoxNetztyp(dbReadStr(d,'boxname'))=nt_UUCP) and
       (dbReadStr(d,'pointname')<>'') then
      kompname:=dbReadStr(d,'username')+' @ '+dbReadStr(d,'pointname')
    else
      kompname:=dbReadStr(d,'username')+' @ '+dbReadStr(d,'boxname')
  end;

  procedure EditPointdaten(nt:byte; var brk:boolean);
  var x,y,br : byte;
      pname  : string[25];
      add    : shortint;
      fido   : boolean;
      turbo  : boolean;
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
  begin
    dbGo(d,drec[p]);
    dbRead(d,'pointname',pname);
    dbRead(d,'script',flags);
    fido:=(nt=nt_Fido);
    turbo:=(nt=nt_Turbo);
    uucp:=(nt=nt_UUCP);
    pronet:=(nt=nt_Pronet);
    zc:=(nt=nt_ZConnect);
    xx:=66; yy:=11;
    case nt of
      nt_Fido    : begin
                     xx:=56; yy:=18;
                     fnode:=(flags and 4<>0);
                   end;
      nt_Turbo   : begin xx:=56; yy:=7; end;
      nt_UUCP    : yy:=18;
      nt_pronet  : xx:=55;
      nt_ZConnect: yy:=12;
    end;
    dialog(xx,yy,kompname,x,y);
    with boxpar^ do begin
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
        nt_Turbo: begin
                    pname:=dbReadStr(d,'username');
                    maddstring(3,2,getres2(920,4),passwort,10,10,''); mhnr(141);
                  end;                                    { 'Pa·wort' }
        nt_Pronet:begin
                    maddstring(3,2,getres2(920,5),telefon,20,60,''); mhnr(142);
                    malltrim;                             { 'Telefon ' }
                    maddstring(3,3,getres2(920,6),passwort,20,20,'>'); mhnr(141); { 'Pa·wort ' }
                    maddstring(39,2,getres2(920,7),pname,2,2,'0123456789');  { 'Pointnr.  ' }
                      mhnr(8005);
                  end;

      else        begin
                    maddstring(3,2,getres2(920,10),pname,20,25,iifc(uucp,'<','>'));
                      mhnr(140);                             { 'Pointname' }
                    if nt=nt_ZConnect then
                      msetvfunc(testZCpointname);
                    if uucp then mset3proc(uucp_getloginname);
                    maddstring(3,3,getres2(920,11),passwort,20,20,
                                   iifs(ntGrossPW(nt),'>',''));
                  end;                                       { 'Pa·wort  ' }
      end;
      if not pronet then begin
        maddstring(iif(fido or turbo,27,38),2,getres2(920,12),telefon,
                   iif(fido,18,17),60,'');                   { 'Telefon'  }
        mhnr(142);
        malltrim;
        end;
      if nt in [nt_Netcall,nt_ZConnect,nt_Magic,nt_Pronet] then begin
        maddstring(iif(pronet,39,38),3,getres2(920,13)+iifs(pronet,' ',''),zerbid,4,4,'>');
        msetvfunc(zidtest); mhnr(143);                         { 'Ser.Nr.' }
        end;
      if nt in [nt_Quick,nt_GS] then begin
        if loginname='' then loginname:='NET410';
        maddstring(38,3,getres2(920,14),loginname,17,20,'');   { 'Login  ' }
        mappsel(false,'NET410˘NETCALL');
        mhnr(8006);
        end;
      if fido then begin
        maddstring(27,3,getres2(920,15),passwort,18,18,''); mhnr(153);
        mset3proc(fidotestpasslen);                            { 'Pa·wort' }
        end;
      if uucp then begin
        maddstring(38,3,getres2(920,16),loginname,17,20,'');   { 'Login  ' }
        mhnr(8016);
        loginfld:=fieldpos;
        end;

      add:=iif(nt=nt_Maus,0,1);
      if not (fido or turbo or uucp) then begin
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
      else begin
        add:=-2;
        lhapar:=' -m';
        br:=iif(uucp,41,31);
        end;

      if not (turbo) then begin
        maddstring(3,7+add,getres2(920,23),uparcer,br,60,''); mhnr(146);
          if uucp then begin                        { 'Upload-Packer     ' }
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
          else
            mappsel(false,'pkxarc $DOWNFILE˘pkunpak $DOWNFILE˘lharc e $DOWNFILE˘lha e $DOWNFILE˘'+
                          'pkunzip $DOWNFILE˘arj e $DOWNFILE˘copy $DOWNFILE $PUFFER˘'+
                          'rar -std e $DOWNFILE');
          msetvfunc(progtest);
          mset3proc(set_downarcext);
        if uucp then begin
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
            maddint(3,12,getres2(920,62),maxfsize,5,4,0,9999); { 'maximale Dateigrî·e   ' }
              mhnr(8025);
            maddtext(35,12,'KB',0);
            maddbool(3,14,getres2(920,32),VarPacketSize);     { 'variable Paketgrî·e' }
              mhnr(8012);
            maddbool(3,15,getres2(920,60),ForcePacketsize);   { 'Ausgangspaketgrî·e vorgeben' }
            maddbool(3,16,getres2(920,35),SizeNego);          { 'Dateigrî·e Åbertragen' }
            maddbool(3,17,getres2(920,36),uusmtp); mhnr(8017);  { 'SMTP' }
            Ug:=cpos('g',uuprotos)>0; U_G:=cpos('G',uuprotos)>0;
            Ue:=cpos('e',uuprotos)>0; Uz:=cpos('z',uuprotos)>0;
            Uf:=cpos('f',uuprotos)>0;
            maddbool(43,10,getres2(920,34),Ug);    { 'g-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            uup1:=fieldpos;
            maddbool(43,11,getres2(920,33),U_G);    { 'G-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,12,getres2(920,37),Uz);    { 'z-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,13,getres2(920,38),Uf);    { 'f-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            maddbool(43,14,getres2(920,39),Ue);    { 'e-Protokoll' }
            mset1func(testuucp); mhnr(8015);
            uupl:=fieldpos;
            maddbool(43,16,getres2(920,61),uucp7e1);   { 'Login mit 7e1' }
            mhnr(8024);
            uparcnr:=0; downarcnr:=0;
            end
          else begin
            if zc then begin
              maddbool(3,10+add,getres2(920,44),JanusPlus);    { 'Janus+' }
              mset1func(JanusSwitch);
              mhnr(8018);
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
      if turbo then begin                            { 'Seriennr. ' }
        maddint(3,4,getres2(920,56),seriennr,5,4,0,9999); mhnr(8041);
        maddbool(3,6,getres2(920,55),Brettmails); mhnr(8040);
        end;                    { 'Brettnachrichten anfordern' }
      end;
    freeres;
    EditPnt:=nt;
    readmask(brk);
    if not brk then begin
      if fido then with boxpar^ do begin
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
      if uucp then with boxpar^ do begin
        uuprotos:='';
        if Ug then uuprotos:=uuprotos+'g';
        if U_G then uuprotos:=uuprotos+'G';
        if Uz then uuprotos:=uuprotos+'z';
        if Uf then uuprotos:=uuprotos+'f';
        if Ue then uuprotos:=uuprotos+'e';
        end;
      end;
    enddialog;
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
      maddint(3,2,getres2(921,1),connwait,5,4,1,1000); mhnr(160);   { 'Warten auf Verbindung' }
      if not fido then
        maddint(3,3,getres2(921,2),loginwait,5,4,1,1000);    { 'Warten auf Login     ' }
      maddint(3,4+add,getres2(921,3),redialwait,5,4,         { 'WÑhl-Pause           ' }
                      2,1000); mhnr(162);
      maddint(3,5+add,getres2(921,4),redialmax,5,4,iif(fido,2,1),1000);   { 'maximale WÑhlversuche' }
      if not fido then begin
        maddint(35,2,getres2(921,5),retrylogin,5,4,1,100);   { 'Login-Versuche  ' }
        if (nt<>nt_Turbo) and (nt<>nt_UUCP) then
          maddint(35,3,getres2(921,6),packwait,5,4,1,9999);  { 'Warten auf Daten' }
        end;
      maddint(35,4+add,getres2(921,7),connectmax,5,4,1,10); mhnr(166);  { 'max. Connects  ' }
      maddint(35,5+add,getres2(921,8),mincps,5,4,0,9999);    { 'min. cps-Rate   ' }
      if (nt=nt_UUCP) or not (fido or stricmp(uploader,'ZMODEM') or
                                      stricmp(downloader,'ZMODEM')) then
        mdisable;
      maddstring(3,7+add,getres2(921,9),modeminit,41,60,''); { 'Modem-Init ' }
{$IFDEF CAPI }
      if bport=5 then
        schnitte:='ISDN'
      else
{$ENDIF }
      schnitte:='COM'+strs(bport);
      maddstring(3,9+add,getres2(921,10),schnitte,4,4,'');   { 'Schnittstelle ' }
      mappsel(true,'COM1˘COM2˘COM3˘COM4');
{$IFDEF CAPI }
      if ntISDN(nt) then mappsel(true,'ISDN');
{$ENDIF }
      maddint(28,9+add,getres2(921,11),baud,6,6,150,115200); { 'öbertragungsrate:' }
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

  procedure Editgebuehren(var brk:boolean);
  var x,y   : byte;
      crash : boolean;
  begin
    crash:=(boxpar^.boxname='99:99/98') or (boxpar^.boxname='99:99/99');
    dialog(ival(getres2(922,0)),iif(crash,3,5),kompname,x,y);
    with boxpar^ do begin
      if not crash then begin
        maddstring(3,2,getres2(922,1),gebzone,20,20,'');   { 'Tarifzone      ' }
        AppPhoneZones; mhnr(180);
        end;
      maddint(3,iif(crash,2,4),getres2(922,5),conn_time,2,2,0,99);    { 'Connect-Dauer: ' }
        mhnr(181);
      maddtext(length(getres2(922,5))+9,iif(crash,2,4),getres2(922,6),0);   { 'Sek.' }
      freeres;
      readmask(brk);
      enddialog;
      end;
  end;

  procedure EditName(var brk:boolean);
  var name,user,komm  : string[40];
      dname,old_dname : string[8];
      realname        : string[40];
      dom,bdom,fqdom  : string[60];  {16.01.00 HS}
      maps            : string[20];
      nt              : byte;
      newbox,newibox  : boolean;
      fidoname        : string[40];

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
    dbGo(d,drec[p]);
    dbRead(d,'netztyp',nt);
    dbRead(d,'boxname',name);
    dbRead(d,'dateiname',dname);
    dbRead(d,'username',user);
    dbRead(d,'kommentar',komm);
    dbRead(d,'nameomaps',maps);
    dbRead(d,'realname',realname);
    dbRead(d,'domain',dom);
    dbRead(d,'fqdn',fqdom);  {16.01.00 HS}
    dbRead(d,'boxdomain',bdom);
    dbRead(d,'fidoname',fidoname);
    {16.01.00 HS}
    NameRead(nt,name,user,komm,maps,realname,dom,fqdom,bdom,fidoname,kompname,brk);
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
          if dname<>old_dname then begin  { kann bei Fido evtl. gleich sein! }
            renbox('.pp');
            renbox(BfgExt);
            renbox('.fl');
            renbox('.bl');
            renbox('.fbl');
            renbox('.inf');
            end;
          end;
        end;
      if not brk then begin
        boxpar^.boxname:=name;
        boxpar^.username:=user;
        if (nt=nt_Maus) then
          boxpar^.pointname:=name;
        boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;  {16.01.00 HS}
        WriteBox(dname,boxpar);
        dbWrite(d,'boxname',name);
        dbWrite(d,'username',user);
        dbWrite(d,'dateiname',dname);
        dbWrite(d,'kommentar',komm);
        dbWrite(d,'nameomaps',maps);
        dbWrite(d,'realname',realname);
        dbWrite(d,'domain',dom);
        dbWrite(d,'fqdn',fqdom);  {16.01.00 HS}
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
        maddstring(3,2,getres2(923,3),sysopinp,el,60,'>');   { 'Eingangsverzeichnis  ' }
        if nt=nt_UUCP then mhnr(8036) else mhnr(183);
        msetvfunc(testfidodir);
        maddstring(3,4,getres2(923,4),sysopout,el,60,'>');   { 'Ausgangsverzeichnis  ' }
        if nt=nt_UUCP then mhnr(8036) else mhnr(183);
        msetvfunc(testfidodir);
        end
      else if nt=nt_QWK then begin
        fl:=12; el:=33;
        maddstring(3,2,getres2(923,12),sysopinp,el,60,'>'); mhnr(8035);  { 'Eingangspakete       ' }
        msetvfunc(testqwkinfiles);
        maddstring(3,4,getres2(923,13),sysopout,el,60,'>'); mhnr(8035);  { 'Ausgangsverzeichnis  ' }
        msetvfunc(testfidodir);
        end
      else begin
        fl:=5; el:=37;
        maddstring(3,2,getres2(923,5),sysopinp,el,60,'>');  mhnr(182);  { 'Eingangspuffer  ' }
        msetvfunc(validfile);
        maddstring(3,4,getres2(923,6),sysopout,el,60,'>'); mhnr(182);   { 'Ausgangspuffer  ' }
        msetvfunc(validfile);
        end;
      fl:=length(getres2(923,fl));
      maddstring(3,6,forms(getres2(923,20),fl),sysopstart,el,60,''); mhnr(8037);
          { 'Startprogramm ' }
      maddstring(3,8,forms(getres2(923,21),fl),sysopend,el,60,''); mhnr(8037);
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
    dialog(ival(getres2(924,0)),
           13+iif(ntNetcall(nt),excludes+iif(nt=nt_Fido,1,3),0),getres2(924,1),x,y);
    dbRead(d,'script',flags);            { 'Verschiedene Einstellungen' }
    dbRead(d,'AVertreter',av);
    dbRead(d,'PVertreter',pv);
    nall:=(flags and 2=0);
    with boxpar^ do begin
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
      maddbool(20,12+add,getres2(924,12),nall); mhnr(198);  { '/Netcall/Alle' }
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        flags:=flags and $fd+iif(nall,0,2);
        dbWrite(d,'script',flags);
        dbWrite(d,'AVertreter',av);
        dbWrite(d,'PVertreter',pv);
        for i:=1 to excludes do
          if pos(' ',exclude[i,1]+exclude[i,2])>0 then begin
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
        nt_Turbo    : _brett:=true;
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
      maddbool(3,12,getres2(928,9),alias);  { 'Absender User@Server.Domain' }
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
  var n   : shortint;
      fn  : string[12];
      brk : boolean;
      nt  : byte;
      nts : string[20];
      nts0: string[30];
      nts2: string[20];
  begin
    dbGo(d,drec[p]);
    dbRead(d,'dateiname',fn);
    if fn='' then fn:=left(dbReadStr(d,'boxname'),8);
    dbRead(d,'netztyp',nt);
    pushhp(131);
    nts:=''; nts2:=getres2(927,3);   { ',^SysopMode' }
    nts0:=getres2(927,1);
    case nt of
      nt_Netcall : nts:='^Z-Netz,';
      nt_ZConnect: nts:='^ZConnect,';
      nt_Maus    : nts:='M^aus,';
      nt_Magic   : nts:='M^agicNET,';
      nt_Pronet  : nts:='P^ronet,';
      nt_Fido    : nts:='^Fido,';
      nt_Turbo   : begin nts:='^Turbo-Box,'; nts2:=''; end;
      nt_UUCP    : begin nts:='^RFC/UUCP,'; { nts2:=''; } end;
      nt_QWK     : begin
                     nts:='^QWK,'; nts0:=''; nts2:=getres2(927,4);  { ',^Transfer' }
                   end;
    end;
    n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts0+getreps2(927,2,nts)+nts2,edb_pos);
                 { '^Point,^Modem,^GebÅhren,^Namen,'+nts+'^Diverses'+nts2 }
    freeres;
    if n<>0 then edb_pos:=abs(n);
    if n>0 then begin
      ReadBox(nt,fn,boxpar);
      if (nts='') and (n>=5) then inc(n);
      if nts0='' then inc(n,3);
      case n of
        1 : EditPointdaten(nt,brk);
        2 : EditModemdaten(nt,brk);
        3 : EditGebuehren(brk);
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
      if not brk then
        WriteBox(fn,boxpar);
      end;
    pophp;
  end;


  procedure EditNetztyp;
  var brk : boolean;
      nt  : byte;
      nt1 : byte;
      x,y : byte;
      maps: string[20];
      box : string[BoxNameLen];

    procedure SetBrettNetztyp;
    var i,n  : integer;
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
        if ustr(dbReadStr(bbase,'pollbox'))=ustr(box) then begin
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
      wait(curon);
      closebox;
      end
    else begin
      dbRead(d,'netztyp',nt); nt1:=nt;
      dbRead(d,'boxname',box);
      ReadNetztyp(box,nt,brk);
      if not brk and (nt<>nt1) and
         ReadJN(getreps2(929,5,box),true) then begin
                       { 'Mîchten Sie den Netztyp fÅr %s wirklich Ñndern' }
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


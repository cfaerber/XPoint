{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

  procedure NameRead(nt:byte; var name,user,komm,maps,realname,domain,fqdn, email,
                     bdomain,fidoname:string; WindowHeadline:atext; var brk:boolean);
  var x,y  : Integer;
      real : boolean;
      dom  : boolean;
      fqdom: boolean;
      bdom : boolean;
      map  : boolean;
      blen : byte;
      btxt : string;
      fido : boolean;
      s    : string;
      p, yy: Integer;
  label restart;
  begin
  restart:
    DomainNt:=nt;
    real:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_UUCP];
    dom:=nt in [nt_ZConnect,nt_Magic,nt_Pronet,nt_Fido,nt_UUCP];
    {Eingabe fuer FQDN nur bei ZC und RFC }
    fqdom:=nt in [nt_ZConnect,nt_UUCP];
    bdom:=(nt=nt_UUCP);
    map:=not (nt in [nt_Maus,nt_Fido,nt_UUCP,nt_QWK]);
    fido:=(nt=nt_Fido);
    dialog(ival(getres2(912,0)),7+iif(map,2,0)+iif(real,2,0)+iif(dom,2,0)+
           iif(fqdom,2,0)+iif(bdom,4,0)+iif(fido,2,0),WindowHeadline,x,y);
    blen:=ntBoxnameLen(nt);
    btxt:=getres2(912,iif(fido,1,2));    { 'Nodenummer','Boxname   ' }
    { Kleinschreibung fuer Boxname zulassen}
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
    begin  { Eingabemoeglichkeit fuer FQDNs}
      maddstring(3,yy,getres2(912,11),fqdn,30,60,          { 'FQDN      ' }
                 'A'+without(range('!','~'),' @<>/_')); mhnr(40001);
      msetvfunc(xp9_FQDNTest);
      inc(yy,2);
    end;
    freeres;
    readmask(brk);
    if not brk and (name=DefFidoBox) then
    begin
      HighlightName:=UpperCase(user);
      aufbau:=true;
    end;

    if not brk and (nt=nt_uucp) then
    begin
      if (email<>'') then
      begin
        if not IsMailaddress(email)
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
        else user := LeftStr(email,cpos('@',email)-1);
      if (domain='') and (email<>'') then
      begin
        domain:=mid(email,cpos('@',email));
        if cpos('.',domain)=0 then domain:=''
         else domain:=mid(domain,cpos('.',domain));
        end;
      end;

    enddialog;
  end;

  // 'Namen' fuer die Internet-Boxentypen
  procedure InetNameRead(nt:byte; var name,user,komm,realname,fqdn:string;
    const WindowHeadline: string; var brk:boolean);
  var x,y, bLen  : Integer;
  begin
    DomainNt:=nt;
    dialog(ival(getres2(912,0)),11,WindowHeadline,x,y);
    blen:=ntBoxnameLen(nt);
    maddstring(3,2,getres2(912,2),name,blen,blen, '"!'+range('#','?')+range('A',#126)+'éôö'); { 'Boxname' }
      gf_fido:=false; msetvfunc(xp9_testbox);
      mhnr(190);
    maddstring(3,4,getres2(912,6),komm,30,30,'');    { 'Kommentar ' }
      mhnr(192);
    maddstring(3,6,getres2(912,8),realname,30,40,'A '+without(range('!','~'),' @<>/')); { 'Realname' }
      msetvfunc(notempty);
      mhnr(194);
    maddstring(3,8,getres2(912,12),user,30,30,grosschar(ntGrossUser(nt))+
               'A'+without(allchar,'() '));
      msetvfunc(notempty);
      mhnr(40002);

    maddstring(3,10,getres2(912,11),fqdn,30,60,          { 'FQDN      ' }
           'A'+without(range('!','~'),' @<>/'));
      mhnr(40001);
    freeres;
    readmask(brk);
    enddialog;
  end;


  procedure ReadNetztyp(box:string; var nt:byte; var brk:boolean);
  var x,y  : Integer;
      ntyp : string;
      i    : integer;
  begin
    ntyp:=Netz_Typ(nt);
    dialog(ival(getres2(913,0)),3,box,x,y);
    maddstring(3,2,getres2(913,1),ntyp,10,10,''); mhnr(660);   { 'Netztyp ' }
    freeres;
    for i:=0 to High(SupportedNetTypes) do
      mappsel(true,ntName(SupportedNetTypes[i]));
    readmask(brk);
    enddialog;
    if not brk then
      for i:=0 to High(SupportedNetTypes) do
        if LowerCase(ntyp)=LowerCase(ntName(SupportedNetTypes[i])) then
          nt:= SupportedNetTypes[i];
  end;

  procedure unique(var dname:string);    { eindeutigen Dateinamen erzeugen }
  begin
    dbSeek(d,boiDatei,dname);
    if dbFound then begin
      dname:=LeftStr(dname,6)+'01';
      repeat
        dbSeek(d,boiDatei,dname);
        if dbFound then dname:=LeftStr(dname,6)+formi(ival(RightStr(dname,2))+1,2);
      until not dbFound;
      end;
  end;

  procedure _EditPPP(var brk:boolean; var name,komm,email,realname,replyto,fqdn,PPPClientPath,
                     PPPClient,PPPAddServers,PPPDialUp,PPPPhone,PPPLogin,PPPPass:String; Titel:String);
  var
    x,y: Integer ;
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
    mappsel(false,'UKAD\˙UKAW\˙UKAX\˙UKA_PPP\˙XPNEWS\');
    msetvfunc(PPPClientpathTest);

    maddstring(3,14,getres2(920,88),PPPClient,41,60,'');    { Client-Aufruf }
    mappsel(false,'UKAD.BAT /M $CLPATH $CONFIG˙UKAW.BAT /M $CLPATH $CONFIG˙UKAX.BAT /M $CLPATH $CONFIG˙' +
     'UKA_PPP.BAT /M $CLPATH $CONFIG˙$CLPATH+XPNEWS.EXE $CONFIG˙START /W $CLPATH+XPNEWS.EXE $CONFIG');
    msetvfunc(pppclienttest);

    maddstring(3,16,getres2(920,89),PPPAddServers,41,60,ValidDirCh);   { 'ZusÑtzliche Server' }
    maddstring(3,18,getres2(920,90),PPPDialUp,18,60,'');    { 'Zugang' }
    maddstring(3,20,getres2(920,91),PPPPhone,18,60,'');     { 'Telefon' }
    maddstring(36,18,getres2(920,92),PPPLogin,18,60,'');    { 'Login' }
    maddstring(36,20,getres2(920,93),PPPPass,18,20,'');     { 'Pa·wort' }
    maddtext(0,3,'+'+dup(66,'-')+'›', 0);  { Linie }
    maddtext(0,11,'+'+dup(66,'-')+'›', 0); { Linie }
    maddtext(0,17,'+'+dup(66,'-')+'›', 0); { Linie }

    freeres;
    readmask(brk);
    enddialog;
  end;

  procedure NewBox;
  var
    email, name, dname, replyto, user, komm, maps, real, dom, fqdom, bdom, fido: String;
      brk  : boolean;
      fn   : string;
      nt,i : byte;
      b    : byte;
      pc,pp,ps,cldup,clphn,cllog,clpas : string;
      pppm : boolean;
  begin
    dbGo(d,drec[p]);
    fn := dbReadStr(d,'dateiname');
    ReadBox(nt, fn, boxpar);
    pppm:=boxpar^.Clientmode;
    if empty then
      nt:=nt_Netcall
    else begin
      dbRead(d,'netztyp',nt);
      if (nt=nt_UUCP) and pppm then nt:=nt_UUCP_C;
    end;
    ReadNetztyp(getres(914),nt,brk); if brk then exit;   { 'neue Box' }
    if nt<>nt_UUCP_C then
    begin
      pppM:=false;
      pp:='';
      pc:='';
      ps:='';
      cldup:='';
      clphn:='';
      cllog:='';
      clpas:='';
      boxpar^.ClientMailInServer := '';
      boxpar^.ClientMailInPort:='';
      boxpar^.ClientMailOutServer:='';
      boxpar^.ClientMailOutPort:='';
      boxpar^.ClientNewsServer:='';
      boxpar^.ClientNewsPort:='';
      end
    else begin
      pppM:=true;
      nt:=nt_UUCP;
      pp:=boxpar^.ClientPath;
      pc:=boxpar^.ClientExec;
      ps:=boxpar^.ClientAddServers;
      cldup:=boxpar^.ClientDialUp;
      clphn:=boxpar^.ClientPhone;
      cllog:=boxpar^.ClientLogin;
      clpas:=boxpar^.ClientPass;
    end;

    name:=''; user:=''; komm:=''; fido:='';
    maps:=DefaultMaps(nt);
    dom:=ntDefaultDomain(nt); bdom:=dom;
    email:='';
    real:='';
    replyto:='';
    fqdom:='';
    boxpar^.ClientMailInEnv:='';
    boxpar^.ClientMailInUser:='';
    boxpar^.ClientMailInPass:='';
    boxpar^.ClientMailOutEnv:='';
    boxpar^.ClientMailOutUser:='';
    boxpar^.ClientMailOutPass:='';
    boxpar^.ClientNewsUser:='';
    boxpar^.ClientNewsPass:='';

    if pppm then
      _EditPPP(brk,name,komm,email,real,replyto,fqdom,pp,pc,ps,cldup,clphn,cllog,clpas,getreps(915,netz_typ(nt_UUCP_C)))
    else
      if nt in [nt_NNTP, nt_POP3] then
        INetNameRead(nt,name,user,komm,real,fqdom,getreps(915,Netz_Typ(nt)),brk)
      else
        NameRead(nt,name,user,komm,maps,real,dom,fqdom,email, bdom,fido,getreps(915,Netz_Typ(nt)),brk);
    if not brk then begin            { 'neue Box anlegen (Typ: %s)' }
      dbSeek(d,boiName,UpperCase(name));
      if dbFound then rfehler(921)   { 'Box ist bereits vorhanden' }
      else
      begin
        b:=cpos('@',email);
        if user='' then user:= leftStr(email,b-1);
        if dom='' then
        begin
          dom:=mid(email,b);
          if cpos('.',dom)=0 then dom:=''
            else dom:=mid(dom,cpos('.',dom));
          end;
        boxpar^.ClientPath:=pp;
        boxpar^.ClientExec:=pc;
        boxpar^.ClientAddServers:=ps;
        boxpar^.ClientDialUp:=cldup;
        boxpar^.ClientPhone:=clphn;
        boxpar^.ClientLogin:=cllog;
        boxpar^.ClientPass:=clpas;
        boxpar^.ClientMode:=pppm;
        boxpar^.boxname:=name;
        boxpar^.username:=user;
        boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;
        boxpar^.passwort:='GEHEIM';
        if pppm then boxpar^.ClientMailInEnv:=email;
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
          if (nt=nt_UUCP) and FileExists('uucp.scr') then
            script:='uucp.scr';
          if nt=nt_Fido then begin
            case ival(LeftStr(name,cpos(':',name)-1)) of
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
            AdditionalServers:='';
            GetTime:=false;
            end;
          if nt=nt_Maus then begin
            boxpar^.exclude[1,1]:='04:00';
            boxpar^.exclude[1,2]:='06:00';
            end;
          end;
          if nt=nt_nntp then
          begin
            Boxpar^.nntp_port := 119;
          end;
        dname:=getdname(nt,name);      { eindeutigen Dateinamen bestimmen }
        unique(dname);
        WriteBox(dname,boxpar);
        dbAppend(d);
        dbWrite(d,'netztyp',nt);
        dbWriteStr(d,'boxname',name);
        dbWriteStr(d,'username',user);
        dbWriteStr(d,'pointname',boxpar^.pointname);
        dbWriteStr(d,'dateiname',dname);
        dbWriteStr(d,'kommentar',komm);
        dbWriteStr(d,'nameomaps',maps);
        dbWriteStr(d,'realname',real);
        dbWriteStr(d,'domain',dom);
        dbWriteStr(d,'fqdn',fqdom);  {16.01.00 HS}
        dbWriteStr(d,'boxdomain',bdom);
        dbWriteStr(d,'fidoname',fido);
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
  var name  : string;
      dname : string;
      pp    : boolean;
  begin
    dbGo(d,drec[p]);
    name:= dbReadStr(d,'boxname');
    if name=DefaultBox then
      rfehler(922)            { 'Stammbox kann nicht geloescht werden!' }
    else begin
      dname:= dbReadStr(d,'dateiname');
      if (dname='') or (length(dname)>8) then begin
        write(#7);
        dname:= dbReadStr(d,'boxname');
        end;
      pp:=_filesize(ownpath+dname+BoxFileExt)>0;
      if ReadJN(iifs(pp,getres(916),'')+getreps(917,name),not pp)   { 'Polldaten vorhanden!! ' / '%s wirklich loeschen' }
      then begin
        dec(ntused[dbReadInt(d,'netztyp')]);
        dbDelete(d);
        dbFlushClose(d);
        if FileExists(ownpath+dname+bfgext) then     { .BFG loeschen }
          _era(ownpath+dname+bfgext);
        if FileExists(ownpath+dname+'.bl') then      { Brettliste loeschen }
          _era(ownpath+dname+'.bl');
        if FileExists(ownpath+dname+'.fl') then      { Fileliste loeschen }
          _era(ownpath+dname+'.fl');
        if FileExists(ownpath+dname+'.fbl') then     { Fileare-Liste loeschen }
          _era(ownpath+dname+'.fbl');
        if FileExists(ownpath+dname+'.inf') then     { Maus-Infofileliste loeschen }
          _era(ownpath+dname+'.inf');
        if pp then _era(OwnPath+dname+BoxFileExt);   { Pollpaket loeschen }
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
  var name : string;
      n    : shortint;
  begin
    dbGo(d,drec[p]);
    pushhp(78);
    n:=MiniSel(x+10,min(y+p+1,screenlines-4),'',getres(918),nn);  { '^Stammbox,^Fido-Stammbox' }
    pophp;
    if n<>0 then nn:=abs(n);
    if n>0 then begin
      name:= dbReadStr(d,'boxname');
      if n=1 then DefaultBox:=name
      else begin
        if dbReadInt(d,'netztyp')<>nt_Fido then begin
          rfehler(923);     { 'Die gewaehlte Box ist keine Fido-Box!' }
          exit;
          end;
        if DefFidobox=name then DefFidobox:=''
        else begin
          DefFidoBox:=name;
          HighlightName:=UpperCase(dbReadStr(d,'username'));
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
  var x,y,FieldW : Integer;
      pname  : string;
      lhapar : string;
      YOffset    : shortint;
      fido   : boolean;
      uucp   : boolean;
      _conn_mode: string;
      pronet : boolean;
      zc     : boolean;
      pnr    : word;     { Fido - Pointnummer }
      xx,yy  : byte;
      Ug,U_G : boolean;  { UUCP-g, UUCP-G }
      Ue,Uz  : boolean;  { UUCP-e, UUCP-z }
      Uf     : boolean;  { UUCP-f }
      Ut     : boolean;  { UUCP-t }
      flags  : byte;
      fnode  : boolean;  { Fido-Absender: Nodenummer }
      pp     : byte;
  begin
    dbGo(d,drec[p]);
    pname:= dbReadStr(d,'pointname');
    dbRead(d,'script',flags);
    fido:=(nt=nt_Fido);
    uucp:=(nt=nt_UUCP);
    pronet:=(nt=nt_Pronet);
    zc:=(nt=nt_ZConnect);
    xx:=66; yy:=11; YOffset:=0;
    case nt of
      nt_Fido    : begin
                     yy:=16;
                     fnode:=(flags and 4<>0);
                   end;
      nt_UUCP    : yy:=21;
      nt_pronet  : xx:=55;
      nt_ZConnect: yy:=9;
    end;
    dialog(xx,yy,kompname,x,y);
    with boxpar^ do
    begin

      case nt of
        nt_Maus : begin
                    pname:= dbReadStr(d,'boxname');
                    maddstring(3,2,getres2(920,1),passwort,20,20,''); mhnr(141);  { 'Passwort  ' }
                  end;
        nt_Fido : begin
                    pnr:=ival(pname);
                    maddint(3,2,getres2(920,2),pnr,6,6,0,32767); mhnr(151);  { 'Pointnr.' }
                    maddstring(23,2,getres2(920,15),passwort,12,18,'');  { 'Passwort' }
                    maddint(48,2,getres2(920,3),fPointNet,5,5,0,32767);  { 'Pointnetz  ' }
                    mset3proc(fidotestpasslen);
                    Dec(YOffset,1);
                  end;
        nt_UUCP : begin
                    maddstring(3,2,getres2(920,16),loginname,15,20,'');    { 'Login  ' }
                      mhnr(8016);
                    loginfld:=fieldpos;
                    maddstring(3,3,getres2(920,17),passwort,15,20,'');     { 'Password' }
                      mhnr(141);
                    maddstring(33,2,getres2(920,18),pname,20,25,'');
                      mhnr(140);                                           { 'UUCP-Name' }
                    mset3proc(uucp_getloginname);
                  end;
      else        begin // ZConnect
                    maddstring(3,2,getres2(920,10),pname,20,25,'>');
                      mhnr(140);                             { 'Pointname' }
                    if nt=nt_ZConnect then
                      msetvfunc(testZCpointname);
                    maddstring(3,3,getres2(920,11),passwort,20,20,
                                   iifs(ntGrossPW(nt),'>',''));
                  end;                                       { 'Passwort  ' }
      end; { case }

      uucp_pefld:=0; // needed by conn_setmode
      case nt of
        nt_Fido,nt_UUCP: begin
                    _conn_mode:=iifs((conn_mode=conn_mode_modem),getres2(920,71),
                                iifs((conn_mode=conn_mode_tcpip),getres2(920,72),getres2(920,76)));
                    maddstring(3,5+YOffset,getres2(920,70),_conn_mode,6,6,'');     { 'Modus   ' }
                      mhnr(8006);
                    {$IFDEF DOS32}
                    mappsel(true,getres2(920,71));
                    {$ELSE}
                    mappsel(true,getres2(920,71)+'˘'+getres2(920,72)+'˘'+getres2(920,76));
                    {$ENDIF}
                    mset1func(Conn_setmode);
                    Conn_modefld:=fieldpos;

                    maddstring(23,5+YOffset,getres2(920,73),telefon,31,60,'');     { 'Telefon ' }
                      Conn_telfld:=fieldpos;
                      mhnr(142);
                    maddstring(23,5+YOffset,getres2(920,74),conn_ip,18,60,'');     { 'Hostname' }
                      Conn_ipfld:=fieldpos;
                      mhnr(8008);
                    maddint   (53,5+YOffset,getres2(920,75),conn_port,5,5,1,65535);{ 'Port' }
                    mappsel(false,iifs(nt=nt_UUCP,'540','24554'));
                      Conn_portfld:=fieldpos;
                  end;
        nt_ZConnect: begin
                    maddstring(iif(fido,27,38),2,getres2(920,12),telefon,
                               iif(fido,18,17),60,'');                   { 'Telefon'  }
                    mhnr(142);
                    malltrim;
                    maddstring(iif(pronet,39,38),3,getres2(920,13)+iifs(pronet,' ',''),zerbid,4,4,'>');
                    msetvfunc(zidtest); mhnr(143);                         { 'Ser.Nr.' }
                  end;
        end;

      if nt=nt_Maus then Inc(YOffset,1);

      case nt of
        nt_Fido,nt_UUCP: begin
          FieldW:=41;
          lhapar:=' -m';
          end;
      else begin
          if nt=nt_Maus then begin
            maddstring(3,6,getres2(920,22),prototyp,1,1,'>'); mhnr(150);
            mappsel(false,'Z˘M');                     { 'Protokoll-Kennung ' }
            end; { nt_Maus }
          Dec(YOffset,2);
          lhapar:='';
          FieldW:=41;
          end;
      end;

      maddstring(3,7+YOffset,getres2(920,23),uparcer,FieldW,60,''); mhnr(146);

      if uucp then begin                        { 'Upload-Packer     ' }
         mappsel(false,'compress -vf -b12 $PUFFER˘compress -vf -b16$PUFFER˘freeze -vif $PUFFER˘gzip -vf $PUFFER˘bzip2 -vf $PUFFER');
         mhnr(8020);
      end else
         mappsel(false,'pkarc a $UPFILE $PUFFER˘pkpak a $UPFILE $PUFFER˘lharc a'+lhapar+' $UPFILE $PUFFER˘'+
                       'lha a'+lhapar+' $UPFILE $PUFFER˘pkzip $UPFILE $PUFFER˘arj a $UPFILE $PUFFER˘'+
                       'rar -std a $UPFILE $PUFFER˘copy $PUFFER $UPFILE');
      msetvfunc(testexecutable);
      mset3proc(set_uparcext);

      maddstring(3,8+YOffset,getres2(920,iif(uucp,25,24)),downarcer,FieldW,60,'');

      if uucp then                              { 'Download-Entpacker' }
        mappsel(false,'compress -vdf $DOWNFILE˘gzip -vdf $DOWNFILE')
      else
        mappsel(false,'pkxarc $DOWNFILE˘pkunpak $DOWNFILE˘lharc e $DOWNFILE˘lha e $DOWNFILE˘'+
                      'pkunzip $DOWNFILE˘arj e $DOWNFILE˘copy $DOWNFILE $PUFFER˘'+
                      'rar -std e $DOWNFILE');
      msetvfunc(testexecutable);
      mset3proc(set_downarcext);

      if fido then
      begin
        maddstring(3,9,getres2(920,53),AdditionalServers,41,AKAlen,'');   { 'Pakete mitsenden' }
          mhnr(154);
        msetvfunc(TestAKAservers);
        maddbool(3,11,getres2(920,46),f4d);         { '4D-Adressen' }
          mhnr(156);
        maddbool(3,12,getres2(920,47),ftosscan);    { 'TosScan o.ae.' }
        maddbool(29,11,'EMSI',EMSIenable);
          emsifield:=fieldpos;
        maddbool(29,12,getres2(920,48),gettime);    { 'Uhrzeit setzen' }
        maddbool(3,14,getres2(920,49),PacketPW); mhnr(8000);  { 'Paketpasswort' }
        maddbool(3,15,getres2(920,50),fnode);       { 'Node statt Point' }
        maddbool(29,14,getres2(920,51),NotSEmpty);  { 'leeres Paket senden' }
        maddbool(29,15,getres2(920,52),ExtPFiles);  { 'erw. Paketdateinamen' }
          mhnr(8003);
        UpArcNr:=0; DownArcNr:=0;
        Conn_setmode  (_conn_mode);
      end else

      if uucp then
      begin
        maddstring(3,9+YOffset,getres2(920,26),unfreezer,FieldW,40,'');  { 'Freeze-Entpacker' }
        mappsel(false,'freeze -vdif $DOWNFILE');
        msetvfunc(testexecutable);
        maddstring(3,10+YOffset,getres2(920,27),ungzipper,FieldW,40,''); { 'gzip-Entpacker' }
        mappsel(false,'gzip -vdf $DOWNFILE');
        msetvfunc(testexecutable);
        maddstring(3,11+YOffset,getres2(920,57),unbzipper,FieldW,40,''); { 'bzip2-Entpacker' }
        mappsel(false,'bzip2 -vdf $DOWNFILE');
          mhnr(8026);
        msetvfunc(testexecutable);

        Ug:=cpos('g',uuprotos)>0; U_G:=cpos('G',uuprotos)>0;
        Ue:=cpos('e',uuprotos)>0; Uz:=cpos('z',uuprotos)>0;
        Uf:=cpos('f',uuprotos)>0;
        Ut:=cpos('t',uuprotos)>0;

        maddbool(3,13,getres2(920,36),uusmtp); mhnr(8017); { 'Batched SMTP'                  }
          mhnr(8017);
        maddbool(3,14,getres2(920,35),SizeNego);           { 'Dateigroesse uebertragen'      }
          mhnr(8014);
          mset1func(uucp_setsznego);
        maddbool(3,15,getres2(920,32),VarPacketSize);      { 'variable Paketgroesse'         }
          mhnr(8012);
          if not (ug or u_g) then MDisable;
          uucp_gvarfld:=fieldpos;
        maddbool(3,16,getres2(920,60),ForcePacketsize);    { 'Ausgangspaketgroesse vorgeben' }
          mhnr(8013);
          if not (ug or u_g) then MDisable;
          uucp_gforfld:=fieldpos;

        maddint(3,18,getres2(920,62),maxfsize,5,4,0,9999); { 'maximale Dateigroesse   '      }
          mhnr(8025);
          if not SizeNego then MDisable;
          uucp_maxsizefld:=fieldpos;
        maddtext(35,18,'kB',0);
        maddint(3,19,getres2(920,30),maxpacketsize,5,4,64,4096); { 'max. UUCP-Paketgroesse ' }
          mappsel(true,'64˘128˘256˘512˘1024˘2048˘4096');
          mhnr(8010);
          if not (ug or u_g) then MDisable;
          uucp_gpktfld:=fieldpos;
        maddint(3,20,getres2(920,31),maxwinsize,3,1,2,7);  { 'max. UUCP-Fenstergroesse  '    }
          mappsel(true,'7˘6˘5˘4˘3˘2');
          mhnr(8011);
          if not (ug or u_g) then MDisable;
          uucp_gwinfld:=fieldpos;

        uup1:=fieldpos+1;
        maddbool(43,13,getres2(920,66),Ut);    { 't-Protokoll' }
          mhnr(8015);
          uucp_ptfld:=fieldpos;
        maddbool(43,14,getres2(920,39),Ue);    { 'e-Protokoll' }
          mhnr(8015);
          uucp_pefld:=fieldpos;
        maddbool(43,15,getres2(920,33),U_G);   { 'G-Protokoll' }
          mhnr(8015);
          uucp_p_Gfld:=fieldpos;
          mset1func(uucp_setprot);
        maddbool(43,16,getres2(920,34),Ug);    { 'g-Protokoll' }
          mhnr(8015);
          uucp_pgfld:=fieldpos;
          mset1func(uucp_setprot);
        maddbool(43,17,getres2(920,37),Uz);    { 'z-Protokoll' }
          mhnr(8015);
        maddbool(43,18,getres2(920,38),Uf);    { 'f-Protokoll' }
          mhnr(8015);
        uupl:=fieldpos;

        maddbool(43,20,getres2(920,61),uucp7e1);   { 'Login mit 7e1' }
        mhnr(8024);
        uparcnr:=0; downarcnr:=0;

        Conn_setmode  (_conn_mode);

        masksetqfunc(testuucp);
      end else

      if zc then
      begin
        maddstring(3,10+YOffset,getres2(920,43),uparcext,3,3,'>');
        mhnr(148);
      end                                         { 'Packer-Extension    ' }

      else { not fido, not uucp, not zc }
      begin
        maddstring(3,10+YOffset,getres2(920,40),uparcext,3,3,'>');
          mappsel(false,'ZIP˘LZH˘ARJ˘RAR');     { 'Packer-Extension  ' }
          UpArcNr:=fieldpos;
          mset3proc(TestArcext);    { TXT im MausNet verhindern }
          mhnr(148);
        maddstring(iif(pronet,35,33),iif(zc,11,10)+YOffset,getres2(920,
                   iif(pronet,41,42)),downarcext,3,3,'>');
          mappsel(false,'ZIP˘LZH˘ARJ˘RAR');     { 'Entpacker-Extension' }
          DownArcNr:=fieldpos;
          mset3proc(TestArcext);    { TXT im MausNet verhindern }
      end;
    end; { with boxpar^ do }

    freeres;
    EditPnt:=nt;

    readmask(brk);

    if not brk then
    begin
      if ntOnePW(nt) then
        boxpar^.o_passwort:=boxpar^.passwort;

      if fido then with boxpar^ do
      begin
        conn_mode:=iif(_conn_mode=getres2(920,71),conn_mode_modem,
                   iif(_conn_mode=getres2(920,72),conn_mode_tcpip,conn_mode_telnet));
        flags:=flags and (not 4) + iif(fnode,4,0);
        dbWrite(d,'script',flags);
        pname:=strs(pnr);
        pp:=pos(boxname,AdditionalServers);
        if (pp=1) or ((pp>0) and (AdditionalServers[pp-1]=' ')) then
          delete(AdditionalServers,pp,length(boxname)+1);
      end else

      if uucp then with boxpar^ do
      begin
        conn_mode:=iif(_conn_mode=getres2(920,71),conn_mode_modem,
                   iif(_conn_mode=getres2(920,72),conn_mode_tcpip,conn_mode_telnet));
        uuprotos:='';
        if Ut and (conn_mode<>conn_mode_modem) then
          uuprotos:=uuprotos+'t';
        if Ue and (conn_mode<>conn_mode_modem) then
          uuprotos:=uuprotos+'e';
        if U_G then uuprotos:=uuprotos+'G';
        if Ug then uuprotos:=uuprotos+'g';
        if Uz then uuprotos:=uuprotos+'z';
        if Uf then uuprotos:=uuprotos+'f';
      end;

      dbWriteStr(d,'pointname',pname);
      boxpar^.pointname:=pname;
    end; { not brk }
    enddialog;
  end;

  procedure EditExtern(const bfg:string; var brk:boolean);
  var cfgfile       : String;
      fullClientDir : String;
      Dir, Name, Ext: String;
  begin
    with boxpar^ do
    begin
      fullClientDir := ExpandFileName(ClientPath);
      if (not (IsPath(fullClientDir))) or (fullClientDir = '') then
      begin
        rfehler1(948, fullClientDir);  { 'Client-Verzeichnis "%s" ist nicht vorhanden!' }
        pushkey('e'); pushkey('c');
        exit;
      end;
      fullClientDir := IncludeTrailingPathDelimiter(fullClientDir);
      cfgfile := trim(ClientExternalConfig);

      { Hier wird nur der Wert ClientExternalCnofig aus <Boxname>.BFG string-  }
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

      if Copy(Uppercase(cfgfile), 1, 5) = ('<XP>' + DirSepa) then
        cfgfile := ownpath + Copy(cfgfile, 6, Length(cfgfile)-5);
      pushhp(30129);
      { 'Externe Konfigurationsdatei laden' }
      if ReadExtCfgFilename(getres2(927,10),cfgfile,fullClientDir,true) then
      begin
        if not FileExists(cfgfile) then
          if not ReadJN(getres(2725),false) then exit; { 'Datei nicht vorhanden - neu anlegen' }
        EditFile(cfgfile,false,false,0,false);
        fsplit(cfgfile,dir,name,ext);
        if UpperCase(dir) = Uppercase(fullClientDir) then
          cfgfile := name + ext
        else
        if UpperCase(Copy(dir, 1, Length(ownpath))) = Uppercase(ownpath) then
          cfgfile := '<XP>' + Copy(cfgfile, Length(ownpath), Length(cfgfile)-Length(ownpath)+1);
        if not (Uppercase(cfgfile) = Uppercase(ClientExternalConfig)) then
        begin
          ClientExternalConfig := cfgfile;
          WriteBox(bfg,boxpar);
        end;
      end;
      freeres;
      pophp;
    end;
  end;

  procedure EditTransport(nt:byte; var brk:boolean);
  var x,y : Integer;
  begin
    dialog(58,20,kompname,x,y);
    with boxpar^ do begin
      maddtext(3,2,getres2(920,100),col.coldiahigh);
      maddstring(3,4,getres2(920,106),ClientMailInServer,35,160,'');   { 'Servername  ' }
        mappsel(false,getres2(920,101));
        mhnr(30115);
      maddstring(3,5,getres2(920,107),ClientMailInEnv,35,160,'');    { 'Envelope-Adresse' }
      msetvfunc(Multi_Mailstring);
      maddstring(3,6,getres2(920,108),ClientMailInUser,35,160,'');   { 'Benutzername' }
      maddstring(3,7,getres2(920,109),ClientMailInPass,18,75,'');    { 'Pa·wort     ' }
      maddstring(43,7,getres2(920,110),ClientMailInPort,7,50,'0123456789 ');  { 'Port' }
        mappsel(false,'110˙25˙143˙585˙993');
      maddtext(3,9,getres2(920,102),col.coldiahigh);
      maddstring(3,11,getres2(920,106),ClientMailOutServer,35,160,'');  { 'Servername  ' }
        mappsel(false,getres2(920,103));
      maddstring(3,12,getres2(920,107),ClientMailOutEnv,35,160,'');   { 'Envelope-Adresse' }
      msetvfunc(Multi_Mailstring);
      maddstring(3,13,getres2(920,108),ClientMailOutUser,35,160,'');  { 'Benutzername' }
      maddstring(3,14,getres2(920,109),ClientMailOutPass,18,75,'');   { 'Pa·wort     ' }
      maddstring(43,14,getres2(920,110),ClientMailOutPort,7,50,'0123456789 '); { 'Port' }
        mappsel(false,'25');
      maddtext(3,16,getres2(920,104),col.coldiahigh);
      maddstring(3,18,getres2(920,106),ClientNewsServer,35,160,'');   { 'Servername  ' }
        mappsel(false,getres2(920,105));
      maddstring(3,19,getres2(920,108),ClientNewsUser,35,160,'');   { 'Benutzername' }
      maddstring(3,20,getres2(920,109),ClientNewsPass,18,75,'');    { 'Pa·wort     ' }
      maddstring(43,20,getres2(920,110),ClientNewsPort,7,50,'0123456789 ');  { 'Port' }
        mappsel(false,'119');

      maddtext(0,8,'+'+dup(58,'-')+'›', 0);  { Linie }
      maddtext(0,15,'+'+dup(58,'-')+'›', 0); { Linie }

      readmask(brk);
      if not brk then
      begin
        msgbox(63,12,_hinweis_,x,y);
        mwrt(x+3,y+2,getres2(10900,41));   { 'Diese Einstellungen sind nur wirksam, wenn sie von Ihrem'  }
        mwrt(x+3,y+3,getres2(10900,42));   { 'Client oder Client-Kit ausgewertet werden.             '   }
        mwrt(x+3,y+4,getres2(10900,43));
        mwrt(x+3,y+5,getres2(10900,44));   { 'Ihr Client unterstÅtzt und/oder erfordert mîglicherweise'  }
        mwrt(x+3,y+6,getres2(10900,45));   { 'weitere Einstellungen, die Sie in einer externen Konfigu-' }
        mwrt(x+3,y+7,getres2(10900,46));   { 'rationsdatei vornehmen mÅssen.'                            }
        mwrt(x+3,y+8,getres2(10900,43));
        mwrt(x+3,y+9,getres2(10900,47));   { 'NÑheres entnehmen Sie bitte Ihrer Client-Dokumentation.'   }
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
  var x,y,add  : Integer;
      schnitte : string;
      fido     : boolean;
  begin
    if((nt in [nt_UUCP,nt_Fido]) and (boxpar^.conn_mode<>conn_mode_modem)) then
      rfehler(936)
    else
  begin
    fido:=(nt=nt_Fido);
    add:=iif(fido,0,1);
    dialog(ival(getres2(921,0)),9+add,kompname,x,y);
    with boxpar^ do begin
      maddint(3,2,getres2(921,1),connwait,5,4,1,1000);       { 'Warten auf Verbindung' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30130) else mhnr(160);
      if not fido then
        maddint(3,3,getres2(921,2),loginwait,5,4,1,1000);    { 'Warten auf Login     ' }
        if (nt=nt_UUCP) and ClientMode then mhnr(30131) else mhnr(161);
      maddint(3,4+add,getres2(921,3),redialwait,5,4,2,1000); { 'WÑhlpause           ' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30132) else mhnr(162);
      maddint(3,5+add,getres2(921,4),redialmax,5,4,iif(fido,2,1),1000);   { 'maximale WÑhlversuche' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30133) else mhnr(163);
      if not fido then begin
        maddint(35,2,getres2(921,5),retrylogin,5,4,1,100);   { 'Login-Versuche  ' }
        if (nt=nt_UUCP) and ClientMode then mhnr(30134) else mhnr(164);
        if (nt<>nt_UUCP) then
          maddint(35,3,getres2(921,6),packwait,5,4,1,9999);  { 'Warten auf Daten' }
        end;
      maddint(35,4+add,getres2(921,7),connectmax,5,4,1,10);   { 'max. Connects  ' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30135) else mhnr(166);
      maddint(35,5+add,getres2(921,8),mincps,5,4,0,9999);    { 'min. cps-Rate   ' }
      if (nt=nt_UUCP) or not (fido or stricmp(uploader,'ZMODEM') or
                                      stricmp(downloader,'ZMODEM')) then
        mdisable;
      maddstring(3,7+add,getres2(921,9),modeminit,41,60,''); { 'Modem-Init ' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30136) else mhnr(168);
      schnitte:='COM'+strs(bport);
      maddstring(3,9+add,getres2(921,10),schnitte,4,4,'');   { 'Schnittstelle ' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30137) else mhnr(169);
      mappsel(true,'COM1˘COM2˘COM3˘COM4');
      maddint(28,9+add,getres2(921,11),baud,6,6,150,115200); { 'Uebertragungsrate:' }
      if (nt=nt_UUCP) and ClientMode then mhnr(30138) else mhnr(170);
      mappsel(false,'300˘1200˘2400˘4800˘9600˘19200˘38400˘57600˘115200');
      msetvfunc(testbaud);
      maddtext(length(getres2(921,11))+39,9+add,getres2(921,12),0);  { 'bd' }
      freeres;
      readmask(brk);
      if not brk then bport:=ival(RightStr(schnitte,1));
      enddialog;
      end;
    end;
  end;

  // procedure Editgebuehren(var brk:boolean);  Removed in Rev. 1.37

  procedure EditNNTP(var brk:boolean);
  var x,y   : Integer;
  begin
    dialog(ival(getres2(950,0)),11,GetRes2(950,1),x,y);         { 'NNTP-Einstellungen' }
    with boxpar^ do begin
      maddstring(3,2,getres2(950,2),nntp_ip,25,255,             { 'Server    ~' }
                 range('0','9')+range('A','Z')+range('a','z')+'.'+'-');
        mhnr(40060);
      maddint(3,4,getres2(950,3),nntp_port,4,5,0,65535);        { 'Port      ~' }
      maddstring(3,6,getres2(950,4),nntp_id,25,255,'');         { 'Loginname ~' }
      maddstring(3,8,getres2(950,5),nntp_pwd,25,255,'');        { 'Passwort  ~' }
        mhnr(40062);
      maddint(3,10,getres2(950,6),nntp_maxnews,4,5,0,1000000);  { 'letzten X Nachrichten ' }
      freeres;
      readmask(brk);
      enddialog;
      end;
  end;

  procedure EditPOP3(var brk:boolean);
  var x,y: Integer;
  begin
    dialog(ival(getres2(951,0)),13,GetRes2(951,1),x,y);         { 'POP3-Einstellungen' }
    with boxpar^ do
    begin
      maddstring(3,2,GetRes2(951,2),pop3_ip,25,255,             { 'Server    ~' }
                 range('0','9')+range('A','Z')+range('a','z')+'.'+'-'); mhnr(40020);
      maddstring(3,4,GetRes2(951,3), pop3_id,25,255,'');        { 'Loginname ~' }
      maddstring(3,6,GetRes2(951,4), pop3_pwd,25,255,'');       { 'Passwort  ~' }
        mhnr(40021);
      maddbool(3,8, GetRes2(951,5), pop3_clear);                { 'Abgeholte Mails loeschen' }
      maddbool(3,9, GetRes2(951,6), pop3_APOP);                 { 'APOP benutzen' }
      maddbool(3,10, GetRes2(951,7), pop3_OnlyNew);             { 'Nur neue Mails' }
      maddbool(3,12, GetRes2(951,8), pop3_ForceOneArea);        { 'Alle Mails in ein Brett einordnen' }
      freeres;
      readmask(brk);
      enddialog;
    end;
  end;

  procedure EditSMTP(var brk:boolean);
  var x, y: Integer;
  begin
    dialog(ival(getres2(952,0)),9,GetRes2(952,1),x,y);          { 'SMTP-Einstellungen' }
    with boxpar^ do
    begin
      maddstring(3,2,GetRes2(952,2),smtp_ip,25,255,             { 'Server    ~' }
                 range('0','9')+range('A','Z')+range('a','z')+'.'+'-');
        mhnr(40040);
      maddstring(3,4,GetRes2(952,3),smtp_id,25,255,'');         { 'Loginname ~' }
      maddstring(3,6,GetRes2(952,4), smtp_pwd,25,255,'');       { 'Passwort  ~' }
        mhnr(40041);
      maddbool(3,8,GetRes2(952,5), smtpafterpop);               { 'Vor SMTP einen POP3 Login ausfuehren' }
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
      assign(f,BoxPar^.ClientPath+old_dname+ext);
      if existf(f) then begin
        if FileExists(BoxPar^.ClientPath+dname+ext) then
          _era(BoxPar^.Clientpath+dname+ext);
        rename(f,BoxPar^.Clientpath+dname+ext);
        end;
    end;

    procedure renbox(ext:string);
    var f : file;
    begin
      assign(f,ownpath+old_dname+ext);
      if existf(f) then begin
        if FileExists(ownpath+dname+ext) then
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
  var name,user,komm  : string;
      dname,old_dname : string;
      realname, email : String;
      dom,bdom,fqdom  : string;
      maps            : string;
      nt              : byte;
      newbox,newibox  : boolean;
      fidoname        : string;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'netztyp',nt);
    name:= dbReadStr(d,'boxname');
    dnAme:= dbReadStr(d,'dateiname');
    user:= dbReadStr(d,'username');
    komm:= dbReadStr(d,'kommentar');
    maps:= dbReadStr(d,'nameomaps');
    realname:= dbReadStr(d,'realname');
    dom:= dbReadStr(d,'domain');
    fqdom:= dbReadStr(d,'fqdn');
    eMail := dbreadStr(d,'email');
    bdom:= dbReadStr(d,'boxdomain');
    fidoname:= dbReadStr(d,'fidoname');
    NameRead(nt,name,user,komm,maps,realname,dom,fqdom,email,bdom,fidoname,kompname,brk);
    if not brk then begin
      newbox:=(name<>dbReadStr(d,'boxname'));
      newibox:=(UpperCase(name)<>UpperCase(dbReadStr(d,'boxname')));
      if newbox then begin
        if newibox then begin
          dbSeek(d,boiName,UpperCase(name));
          if dbFound then begin
            rfehler(921);         { 'Box bereits vorhanden!' }
            brk:=true;
            end;
          end;
        if not brk then begin
          dbGo(d,drec[p]);
          if UpperCase(dbReadStr(d,'boxname'))=UpperCase(DefaultBox) then begin
            DefaultBox:=name; SaveConfig2; end else
          if dbReadstr(d,'boxname')=DefFidoBox then begin
            DefFidoBox:=name; SaveConfig2; end;
          old_dname:=dname;
          dname:='';
          dbWriteStr(d,'dateiname',dname);   { ! }
          dname:=getdname(nt,name);
          unique(dname);        { veraendert pos(d)! }
          dbGo(d,drec[p]);
          dbWriteStr(d,'dateiname',dname);
          { evtl. User&Bretter ueberarbeiten }
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
        dbWriteStr(d,'boxname',name);
        dbWriteStr(d,'username',user);
        dbWriteStr(d,'dateiname',dname);
        dbWriteStr(d,'kommentar',komm);
        dbWriteStr(d,'nameomaps',maps);
        dbWriteStr(d,'realname',realname);
        dbWriteStr(d,'domain',dom);
        dbWriteStr(d,'fqdn',fqdom);
        dbWriteStr(d,'email',email);
        dbWriteStr(d,'boxdomain',bdom);
        dbWriteStr(d,'fidoname',fidoname);
        dbFlushClose(d);
        dbGo(d,drec[1]); aufbau:=true;
        end;
      brk:=true;  { siehe unten! }
      end;
  end;

  // Edit Namen fuer alle Internet-Boxtypen ausser UUCP
  procedure EditInetName(var brk:boolean);
  var name,user,komm  : string;
      dname,old_dname : string;
      realname        : string;
      fqdom           : string;
      nt              : byte;
      newbox,newibox  : boolean;

    procedure renbox(ext:string);
    var f : file;
    begin
      assign(f,ownpath+old_dname+ext);
      if existf(f) then begin
        if FileExists(ownpath+dname+ext) then
          _era(ownpath+dname+ext);
        rename(f,ownpath+dname+ext);
        end;
    end;

  begin
    dbGo(d,drec[p]);
    dbRead(d,'netztyp',nt);
    name:= dbReadStr(d,'boxname');
    dnAme:= dbReadStr(d,'dateiname');
    user:= dbReadStr(d,'username'); // hier eMail
    komm:= dbReadStr(d,'kommentar');
    realname:= dbReadStr(d,'realname');
    fqdom:= dbReadStr(d,'fqdn');  {16.01.00 HS}
    INetNameRead(nt,name,user,komm,realname,fqdom,name+', '+user,brk);
    if not brk then begin
      newbox:=(name<>dbReadStr(d,'boxname'));
      newibox:=(UpperCase(name)<>UpperCase(dbReadStr(d,'boxname')));
      if newbox then begin
        if newibox then begin
          dbSeek(d,boiName,UpperCase(name));
          if dbFound then begin
            rfehler(921);         { 'Box bereits vorhanden!' }
            brk:=true;
            end;
          end;
        if not brk then begin
          dbGo(d,drec[p]);
          if UpperCase(dbReadStr(d,'boxname'))=UpperCase(DefaultBox) then begin
            DefaultBox:=name; SaveConfig2; end else
          if dbReadstr(d,'boxname')=DefFidoBox then begin
            DefFidoBox:=name; SaveConfig2; end;
          old_dname:=dname;
          dname:='';
          dbWriteStr(d,'dateiname',dname);   { ! }
          dname:=getdname(nt,name);
          unique(dname);        { veraendert pos(d)! }
          dbGo(d,drec[p]);
          dbWriteStr(d,'dateiname',dname);
          { evtl. User&Bretter ueberarbeiten }
        { DefaultBox:=name;   ?????
          SaveConfig2; }
          if             dname<>old_dname then begin  { kann bei Fido evtl. gleich sein! }
            renbox(BoxFileExt);
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
        // boxpar^._domain:=dom;
        boxpar^._fqdn:=fqdom;  {16.01.00 HS}
        WriteBox(dname,boxpar);
        dbWriteStr(d,'boxname',name);
        dbWriteStr(d,'username',user);
        dbWriteStr(d,'dateiname',dname);
        dbWriteStr(d,'kommentar',komm);
        dbWriteStr(d,'realname',realname);
        dbWriteStr(d,'fqdn',fqdom);  {16.01.00 HS}
        dbFlushClose(d);
        dbGo(d,drec[1]); aufbau:=true;
        end;
      brk:=true;  { siehe unten! }
      end;
  end;

  procedure EditSysopdaten(nt:byte; var brk:boolean);
  var x,y   : Integer;
      flags : byte;
      nobox : boolean;
      fl,el : byte;      { 923.fl = eines der Eingabefelder (fuer Laengenermittlung) }
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
          if ClientMode then mhnr(30150) else mhnr(8036);
          end
        else mhnr(183);
        msetvfunc(testfidodir);

        maddstring(3,4,getres2(923,4),sysopout,el,MaxLenPathname,ValidDirCh);   { 'Ausgangsverzeichnis  ' }
        if nt=nt_UUCP then begin
          if ClientMode then mhnr(30150) else mhnr(8036);
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
        maddbool(3,10,getres2(933,8),delqwk);  { 'QWK-Pakete nach Verarbeitung loeschen' }
        mhnr(8039);
        end;
      maddbool(3,iif(nt=nt_QWK,11,10),getres2(923,7),SysopNetcall); mhnr(184);
          { '/>>Netzanruf-Bericht' }
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
  var x,y,i : Integer;
      flags : byte;
      nall  : boolean;
      x2    : byte;
      add   : shortint;
      av,pv : string;
  begin
    add:=0;
    dialog(ival(getres2(924,0)),
           14+iif(ntNetcall(nt),excludes+iif(nt=nt_Fido,1,3),0),getres2(924,1),x,y);
    dbRead(d,'script',flags);            { 'Verschiedene Einstellungen' }
    av:= dbReadStr(d,'AVertreter');
    pv:= dbReadStr(d,'PVertreter');
    nall:=(flags and 2=0);
    with boxpar^ do begin
      maddtext(3,2,getres2(924,2),col.coldiahigh);    { 'Online-Anruf' }
      maddstring(20,2,getres2(924,3),o_passwort,25,25,''); mhnr(187);  { 'Passwort ' }
      maddstring(20,3,getres2(924,4),o_logfile,25,60,''); { 'Logfile ' }
      msetvfunc(testlogfile);
      maddstring(20,4,getres2(924,13),o_script,25,50,''); { 'Script'   }
      msetvfunc(testscript); mhnr(8030);
      mset3proc(scripterrors);
      if ntNetcall(nt) then begin
        maddtext(3,6,getres2(924,5),col.coldiahigh);    { 'Netzanruf'  }
        maddtext(20,6,getres2(924,14),0);               { 'Ausschluss-' }
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
      maddstring(20,9+add,getres2(924,17),av,20,20,'>'); mhnr(8032);  { 'fuer AMs' }
      amvfield:=fieldpos;
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddstring(20,10+add,getres2(924,18),pv,20,20,'>'); mhnr(8033);  { 'fuer PMs' }
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddtext(3,12+add,getres2(924,11),col.coldiahigh);    { 'Sonstiges' }
      maddbool(20,12+add,getres2(924,20),ReplaceOwn); mhnr(8051);  { 'lokale Kopien ersetzen' }
      maddbool(20,13+add,getres2(924,12),nall); mhnr(198);  { '/Netcall/Alle' }
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        flags:=flags and $fd+iif(nall,0,2);
        dbWrite(d,'script',flags);
        dbWriteStr(d,'AVertreter',av);
        dbWriteStr(d,'PVertreter',pv);
        for i:=1 to excludes do
          if pos(' ',exclude[i,1]+exclude[i,2])>0 then begin
            exclude[i,1]:='  :  '; exclude[i,2]:='  :  ';
            end;
        if ntOnePW(nt) then passwort:=o_passwort;
        end;
      end;
  end;

  procedure EditNNTPMisc(nt:byte; var brk:boolean);
  var x,y,i : Integer;
      flags : byte;
      nall  : boolean;
      x2    : byte;
      add   : shortint;
      av,pv : string;
  begin
    add:=0;
    dialog(ival(getres2(924,0)),13+excludes,getres2(924,1),x,y);
    dbRead(d,'script',flags);            { 'Verschiedene Einstellungen' }
    av:= dbReadStr(d,'AVertreter');
    pv:= dbReadStr(d,'PVertreter');
    nall:=(flags and 2=0);
    with boxpar^ do begin
      maddtext(3,2,getres2(924,19),col.coldiahigh);             { 'Aufruf' }
      maddstring(20,2,getres2(924,4),o_logfile,25,60,'');       { 'Logfile ' }
        mhnr(188);
        msetvfunc(testlogfile);
      maddtext(3,4,getres2(924,5),col.coldiahigh);    { 'Netzanruf'  }
      maddtext(20,4,getres2(924,14),0);               { 'Ausschluss-' }
      maddtext(20,5,getres2(924,15),0);               { 'zeiten:'    }
      x2:=42+length(getres2(924,6));
      for i:=1 to excludes do begin
        maddtime(32,3+i,getres2(924,6),exclude[i,1],false); mhnr(189);  { 'von' }
        maddtime(x2,3+i,getres2(924,7),exclude[i,2],false); mhnr(189);  { 'bis' }
      end;
      inc(add,excludes);
      maddtext(3,5+add,getres2(924,8),col.coldiahigh);  { 'Filter' }
      maddstring(20,5+add,getres2(924,9),eFilter,25,60,''); mhnr(199);  { 'Eingang' }
      maddstring(20,6+add,getres2(924,10),aFilter,25,60,''); mhnr(8050); { 'Ausgang' }
      maddtext(3,8+add,getres2(924,16),col.coldiahigh);   { 'Vertretersystem' }
      DomainNt:=nt;
      maddstring(20,8+add,getres2(924,17),av,20,20,'>'); mhnr(8032);  { 'fuer AMs' }
      amvfield:=fieldpos;
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddstring(20,9+add,getres2(924,18),pv,20,20,'>'); mhnr(8033);  { 'fuer PMs' }
      mappcustomsel(boxselproc,false);
      msetvfunc(testvertreterbox);
      maddtext(3,11+add,getres2(924,11),col.coldiahigh);    { 'Sonstiges' }
      maddbool(20,11+add,getres2(924,20),ReplaceOwn); mhnr(8051);  { 'lokale Kopien ersetzen' }
      maddbool(20,12+add,getres2(924,12),nall); mhnr(198);  { '/Netcall/Alle' }
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        flags:=flags and $fd+iif(nall,0,2);
        dbWrite(d,'script',flags);
        dbWriteStr(d,'AVertreter',av);
        dbWriteStr(d,'PVertreter',pv);
        for i:=1 to excludes do
          if pos(' ',exclude[i,1]+exclude[i,2])>0 then begin
            exclude[i,1]:='  :  '; exclude[i,2]:='  :  ';
            end;
        if ntOnePW(nt) then passwort:=o_passwort;
        end;
      end;
  end;

  procedure EditNet(var brk:boolean);
  var x,y,a  : Integer;
      width  : Integer;
      alias  : boolean;
      mids   : boolean;
      replyto: string;
      maps   : string;
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
      pp_da:=FileExists(dbReadStr(d,'dateiname')+BoxFileExt);
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
                        fehler('keine Einstellungen fuer QuickMail noetig');
                        exit;
                      end;
        nt_GS       : begin
                        fehler('keine Einstellungen fuer G&S-Mailbox noetig');
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
      replyto:= dbReadStr(d,'replyto');
      maps:= dbReadStr(d,'nameomaps');
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
        dbWriteStr(d,'replyto',replyto);
        dbWriteStr(d,'nameomaps',maps);
        dbFlushClose(d);
        end;
      end;
    enddialog;
  end;

  procedure EditFido(var brk:boolean);
  var x,y : Integer;
      maps: string;
      nli : boolean;
  begin
    dbGo(d,drec[p]);
    pp_da:=FileExists(dbReadStr(d,'dateiname')+BoxFileExt);
    with boxpar^ do begin
      maps:= dbReadStr(d,'nameomaps');
      dialog(ival(getres2(926,0)),13,getres2(926,1),x,y);  { 'Fido-Einstellungen' }
      maddtext(3,2,getres2(926,2),col.coldiahigh);        { 'Areafix' }
      maddstring(14,2,getres2(926,3),maps,12,20,''); mhnr(700);  { 'Name       ' }
      mappsel(false,'Areafix˘Areamgr');
      msetvfunc(notempty);
      maddstring(14,3,getres2(926,4),AreaPW,12,12,'');   { 'Passwort    ' }
      maddbool(14,5,getres2(926,5),AreaPlus);      { '"+" bei Bestellungen' }
      maddbool(14,6,getres2(926,6),AreaBetreff);   { 'Befehle im Betreff' }
      nli:=not LocalINTL;
      maddbool(14,7,getres2(926,12),nli);    { 'd''Bridge-Areafix' }
      maddtext(3,9,getres2(926,9),col.coldiahigh);       { 'FileScan' }
      maddstring(14,9,getres2(926,10),Filescanner,12,15,''); { 'Name       ' }
      mappsel(false,'Allfix˘FileFix˘FileMgr˘FileScan˘JunkTick');
      maddstring(14,10,getres2(926,11),FilescanPW,12,12,'');  { 'Passwort    ' }
      maddtext(3,12,getres2(926,7),col.coldiahigh); { 'Diverses' }
      maddstring(14,12,getres2(926,8),MagicBrett,12,25,'A'+without(AllChar,' '));  { 'Brettebene ' }
      mappsel(false,'/FIDO/˘/GERNET/˘/VIRNET/');
      msetvfunc(testmbretter);
      freeres;
      readmask(brk);
      enddialog;
      end;
    if not brk then begin
      dbWriteStr(d,'nameomaps',maps);
      BoxPar^.LocalINTL:=not nli;
      end;
  end;

  procedure EditUUCP(var brk:boolean);
  var x,y,bl   : Integer;
      bmname,
      _bmtyp   : string;
      replyto  : string;
      alias    : boolean;
      a        : byte;
  begin
    with BoxPar^ do begin
      dialog(53,13,getres2(928,1),x,y);   { 'UUCP-Einstellungen' }
      replyto:= dbReadStr(d,'replyto');
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
      bmname:= dbReadStr(d,'nameomaps');
      maddstring(3+bl,5,getres2(928,5),bmname,15,15,'');   { 'Name    ' }
      mappsel   (false,'changesys˘GUP˘feeder˘autosys˘postmaster'); mhnr(658);
      maddstring(3+bl,6,getres2(928,6),AreaPW,12,12,'');   { 'Passwort ' }
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
        dbWriteStr(d,'nameomaps',bmname);
        dbWriteStr(d,'replyto',replyto);
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

  procedure EditQWK(dateiname:string; var brk:boolean);
  const maxnmt = 10;
  var   x,y    : Integer;
        eqfg   : boolean;
        qfg    : QfgRec;
        nmta   : array[1..maxnmt] of record
                                       nr   : byte;
                                       name : string;
                                     end;
        nmtn,i : integer;
        nmtyp  : string;

    procedure ReadNmta;
    var t   : text;
        tmp : string;
        s   : string;
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
          nmta[nmtn].name:=trim(LeftStr(s,30));
          end;
        close(t);
        erase(t);
        end;
    end;

    procedure AppQwkEchos;
    var t : text;
        s : string;
    begin
      assign(t,dateiname+'.bl');
      if existf(t) then begin
        reset(t);
        while not eof(t) do begin
          readln(t,s);
          if (ival(LeftStr(s,3))>0) or (LeftStr(s,3)='  0') then begin
            s:=trim(mid(s,5));
            mappsel(false,trim(LeftStr(s,50)));   { Flags in BlueWave-Listen abschneiden }
            end;
          end;
        close(t);
        end;
    end;

  begin
    eqfg:=FileExists(dateiname+QfgExt) and FileExists('zqwk.exe');
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
  const
    edb_pos : shortint = 1;
  var n   : shortint;
      fn  : string;
      brk : boolean;
      nt  : byte;
      nts : string;
      { nts2: string; }
  begin
    dbGo(d,drec[p]);
    fn:= dbReadStr(d,'dateiname');
    if fn='' then fn:=LeftStr(dbReadStr(d,'boxname'),8);
    dbRead(d,'netztyp',nt);
    nts:=''; {nts2:=getres2(927,3);}   { ',^SysopMode' }

    case nt of
      nt_Netcall : begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,20)       { Netcall ^3.8, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_ZConnect: begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,21)       { ^ZConnect, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_Maus:     begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,22)       { M^ausTausch, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_Magic:    begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,23)       { M^aigNet, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_Pronet:   begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,24)       { P^ronet, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_Fido:     begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                         +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,25)       { ^FTS, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_QWK:      begin
                     pushhp(131);
                     nts:=
//                       +getres2(927,8)        { ^Gebuehren, }
                         getres2(927,9)        { ^Namen, }
                         +getres2(927,29)       { ^QWK, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,4);       { ,^Transfer }
                   end;
      nt_UUCP:     begin
                     pushhp(131);
                     nts:=getres2(927,5)        { ^Point, }
                         +getres2(927,7)        { ^Modem, }
//                       +getres2(927,8)        { ^Gebuehren, }
                         +getres2(927,9)        { ^Namen, }
                         +getres2(927,28)       { ^UUCP, }
                         +getres2(927,10)       { ^Diverse }
                         +getres2(927,11);      { ,^Sysop-Mode }
                   end;
      nt_NNTP:     begin
                     pushhp(132);
                     nts:=getres2(927,9)        { ^Namen, }
                         +getres2(927,26)       { NNT^P, }
                         +getres2(927,10);      { ^Diverses }
                   end;
      nt_POP3:     begin
                     pushhp(133);
                     nts:=getres2(927,9)        { ^Namen, }
                         +getres2(927,27)       { ^POP3, }
                         +getres2(927,30)       { ^SMTP, }
                         +getres2(927,10);      { ^Diverses }
                   end;
    end; { case nt }
    {n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts0+getreps2(927,2,nts)+nts2,edb_pos);}
                 { '^Point,^Modem,^GebÅhren,^Namen,'+nts+'^Diverses'+nts2 }
    n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts,edb_pos);
    freeres;
    if n<>0 then edb_pos:=abs(n);
    if n>0 then begin
      ReadBox(nt,fn,boxpar);
      {if (nts='') and (n>=5) then inc(n);
      if nts0='' then inc(n,3);}
      case nt of
        nt_Netcall  : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditNet(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_ZConnect : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditNet(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_Maus     : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditNet(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_Magic    : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditNet(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_Pronet   : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditNet(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_Fido     : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditFido(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_QWK      : case n of
//                      1: EditGebuehren(brk);
                        1: EditName(brk);
                        2: EditQWK(fn,brk);
                        3: EditDiverses(nt,brk);
                        4: EditSysopdaten(nt,brk);
                      end;
        nt_UUCP     : case n of
                        1: EditPointdaten(nt,brk);
                        2: EditModemdaten(nt,brk);
//                      3: EditGebuehren(brk);
                        3: EditName(brk);
                        4: EditUUCP(brk);
                        5: EditDiverses(nt,brk);
                        6: EditSysopdaten(nt,brk);
                      end;
        nt_NNTP     : case n of
                        1: EditInetName(brk);
                        2: EditNNTP(brk);
                        3: EditNNTPMisc(nt,brk);
                      end;
        nt_POP3     : case n of
                        1: EditInetName(brk);
                        2: EditPOP3(brk);
                        3: EditSMTP(brk);
                        4: EditNNTPMisc(nt,brk);
                      end;
      end; { case nt }
      if not brk then
        WriteBox(fn,boxpar);
      ReadDomainList;
      end;
    pophp;
  end;


  procedure EditNetztyp;
  var brk : boolean;
      nt  : byte;
      nt1 : byte;
      x,y : Integer;
      maps: string[20];
      box : string[BoxNameLen];

    procedure SetBrettNetztyp;
    var i,n  : integer;
        x,y  : Integer;
        flags: byte;
        mi   : byte;
    begin
      msgbox(38,3,'',x,y);
      wrt(x+3,y+1,'öberarbeite Brettliste ...     %');
      mi:=dbGetIndex(bbase);
      dbSetIndex(bbase,0);
      dbGoTop(bbase);
      n:=dbRecCount(bbase);
      i:=1;
      while not dbEOF(bbase) do begin
        gotoxy(x+31,y+1); moff;
        write(i*100 div n); mon;
        if UpperCase(dbReadStr(bbase,'pollbox'))=UpperCase(box) then begin
          flags:=dbReadInt(bbase,'flags') and not 16 + iif(nt IN [nt_UUCP,nt_NNTP],16,0);
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
    if FileExists(dbReadStr(d,'dateiname')+BoxFileExt) then begin
      msgbox(60,9,_fehler_,x,y);
      mwrt(x+3,y+2,getreps2(929,1,    { 'Es sind Polldaten fuer diese Box vorhanden (%s.PP)' }
                   FileUpperCase(dbReadStr(d,'dateiname'))));
      mwrt(x+3,y+3,getres2(929,2));   { '- Netztyp kann nicht geaendert werden.  Versenden oder' }
      mwrt(x+3,y+4,getres2(929,3));   { 'loeschen Sie bitte zuerst die unversandten Nachrichten' }
      mwrt(x+3,y+5,getres2(929,4));   { 'fuer diese Box!' }
      mwrt(x+3,y+7,getres(12));       { 'Taste druecken...' }
      errsound;
      wait(curon);
      closebox;
      end
    else begin
      dbRead(d,'netztyp',nt); nt1:=nt;
      box:= dbReadStr(d,'boxname');
      ReadNetztyp(box,nt,brk);
      if not brk and (nt<>nt1) and
         ReadJN(getreps2(929,5,box),true) then begin
                       { 'Mîchten Sie den Netztyp fÅr %s wirklich Ñndern' }
        dbWrite(d,'netztyp',nt);
        dec(ntused[nt1]);
        inc(ntused[nt]);
        if nt=nt_Maus then begin
          maps:='MAUS';
          dbWriteStr(d,'nameomaps',maps);
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
  Revision 1.14  2001/08/01 01:00:30  mk
  - added IncludeTrailingPathDelimiter and fixed compile problems

  Revision 1.13  2001/07/31 18:05:39  mk
  - implemented is_emailaddress in NameRead
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)
  MY+JG:- new function is_mailaddress, also implemented in all
          functions and procedures involved (multi_Mailstring and
          xp9_setclientFQDN in xp9sel.pas, NameRead in xp9.inc and
          get_first_box in xp9.pas)
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.12  2001/07/31 13:10:34  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.11  2001/07/29 13:58:23  ma
  - removed nt_UUCP_U, some fixes

  Revision 1.10  2001/07/29 12:59:02  ma
  - cleaned up server config dialog
  - removed ntAllowed variable

  Revision 1.9  2001/07/27 18:35:11  mk
  JG+MY:- RFC/Client: implemented check for valid (multiple) eMail addresses
          under Edit/Servers/Edit/Mail/News_Servers/Envelope_address (In+Out)
  JG+MY:- RFC/Client: removed unnecessary mask test in _EditPPP
  JG+MY:- RFC/UUCP: improved check for valid eMail address under
          Edit/Servers/Edit/Names/eMail_address
  VS: ----------------------------------------------------------------------

  Revision 1.8  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.7  2001/07/22 21:05:18  mk
  - fixed double RFC/UUCP in Edit/Boxen/Neu
  - renamed ntnr to NetTypes and removed eNetztypen

  Revision 1.6  2001/07/22 13:46:40  ma
  - fixed: NNTP/POP3 box name was automatically altered if previously
    editing a Fido box

  Revision 1.5  2001/07/21 18:06:54  mk
  - modified duplicate help topics

  Revision 1.4  2001/07/21 16:02:11  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.3  2001/06/23 14:24:17  ma
  - added/refined online help for new netcall types

  Revision 1.2  2001/06/09 10:58:53  ma
  - added ForceOneArea feature (for POP3 server type)

  Revision 1.1  2001/06/04 17:41:12  ma
  - renamed, was xp9

}

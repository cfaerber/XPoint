{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ XPFIDO - Nodeliste auslesen/abfragen }

Procedure ReadNData(nfile:byte; adr:longint; var ni:NodeInfo);
Var s  : ^string;
    rr : word;

  Procedure SetInfo;
  Var p : byte;
      x : string[10];

    Function getstr:string;
    Var b: byte;
    Begin
      p:=cpos(',',s^);
      If p=0 then
      Begin
        getstr:='';
      End
      Else Begin
        For b:=1 to p-1 Do If s^[b]='_' then s^[b]:=' ';
        getstr:=copy(s^,1,p-1);
        delete(s^,1,p);
      End;
    End;

  Begin  { of SetInfo }
    With ni Do
    Begin
      If s^[length(s^)]<>',' then s^:=s^+',';
      { for p:=1 to length(s^) do       nach GetStr verschoben, damit im
        if s^[p]='_' then s^[p]:=' ';  String FFlags '_' erhalten bleibt }
      status:=getstr;
      p:=cpos(',',s^);
      If p > 0 then
      Begin
        If status='' then status:=iifs(ispoint,'Point','Node');
        delete(s^,1,p);    { Nodenummer }
        boxname:=getstr;
        standort:=getstr;
        sysop:=getstr;
        telefon:=getstr;
        baud:=minmax(ival(getstr),110,65535);
        fflags:=s^;
        dellast(fflags);    { Komma entfernen }
        Repeat
          x:=getstr;
          If x='V32B' then flags:=flags or nfV32b Else
          If x='V32' then flags:=flags or nfV32 Else
          If (x='HST') or (x='H14') then flags:=flags or nfHST Else
          If x='PEP' then flags:=flags or nfPEP Else
          If x='ZYX' then flags:=flags or nfZYXEL Else
          If x='H16' then flags:=flags or nfHST16 Else
          If pos('ISDN',x)>0 then flags:=flags or nfISDN Else
          { MK 01/00 Zeile eingefÅgt, erkennt jetzt ISDN-Boxen richtig }
          If (x='X75') then flags:=flags or nfISDN Else
          If (x='VFC') then flags:=flags or nfVFC Else
          If (x='V32T') then flags:=flags or nfTerbo Else
          If (x='V34') then flags:=flags or nfV34 Else
          If x='CM' then flags:=flags or nfCM Else
          If x='XA' then request:=rfWaZOO+rfUpWaz+rfBark+rfUpBark Else
          If x='XB' then request:=rfBark+rfUpBark+rfWaZOO Else
          If x='XC' then request:=rfBark+rfWaZOO+rfUpWaz Else
          If x='XP' then request:=rfBark+rfUpBark Else
          If x='XR' then request:=rfBark+rfWaZOO Else
          If x='XW' then request:=rfWaZOO Else
          If x='XX' then request:=rfWaZOO+rfUpWaz Else
          If x='MN' then flags:=flags and (not nfComp);
        Until x='';
      End;
    End;
  End;  { of SetInfo }

Begin  { of ReadNData }
  ni.found:=false;
  If (nfile<1) or (nfile>NL_anz) then exit;
  new(s);
  new(nodelf);
  assign(nodelf^,FidoDir+NLfilename(nfile));
  resetfm(nodelf^,fmRead);
  If ioresult=0 then
  Begin
    { reset(nodelf^,1);  !?!? }
    seek(nodelf^,adr);
    blockread(nodelf^,s^[1],255,rr);
    s^[0]:=chr(rr);
    s^[0]:=chr(cpos(#13,s^)-1);
    SetInfo;
    ni.found:=true;
    close(nodelf^);
  End;
  dispose(nodelf);
  dispose(s);
End;  { of ReadNData }

{ Pointtyp: 0=nur Node, 1=Point/Node, 2=bei nicht gef. Point wiederholen }
Procedure GetNodeInfo(adr:string; var ni:nodeinfo; pointtyp:shortint);
Begin
  GetNodeInfoN(adr,ni,pointtyp,0); {TS 06.08.02 Neue Proc. mit komp. Parameter}
End;

Procedure GetNodeInfoN(adr:string; var ni:nodeinfo; pointtyp:shortint; _nlid:Byte);
var fa     : fidoadr;
    i,netp : integer;
    bp     : ^netrecl;
    banz   : word;
    nanz   : word;
    nadr,l : longint;
    nfile  : byte;
    np     : ^nodea;
    found  : boolean;
    _adr   : longint;
    points : integer;
    pp     : ^pointa;

label again;

Begin
  fillchar(ni,sizeof(ni),0); { Nodeinfo lîschen }
  If not nodeopen then Exit; { Abbruch wenn keine Nodeliste geîffnet }
  splitfido(adr,fa,2);
  If pointtyp=0 then fa.ispoint:=false; { nur Nodeliste durchsuchen }
  If not nodelistopen then
  Begin
    reset(nodef,1);
    If ioresult <> 0 then Exit;
  End;

again:
  i:=bereiche;
  While (i>0) and ((berliste^[i].fromzone>fa.zone) or ((berliste^[i].fromzone=fa.zone)
    and (berliste^[i].fromnet>fa.net))) Do dec(i);
  fillchar(ni,sizeof(ni),0);
  If i>0 then
  Begin
    new(bp);
    seek(nodef,NX_adrnetx+berliste^[i].adr);
    banz:=berliste^[i].anz;
    If banz>bersize then writeln(getres(2121),#7);   { 'Fehler in Nodelisten-Index!' }
    blockread(nodef,bp^,banz*sizeof(netrec));
    l:=$10000*fa.zone+fa.net;
    {$R-}
    netp:=1;
    While (netp<=banz) and (bp^[netp].sortl<l) Do inc(netp);
    Repeat
      found:=(netp<=banz) and (bp^[netp].sortl=l);
      If found then
      Begin
        nanz:=bp^[netp].anz;
        nadr:=bp^[netp].adr;
        nfile:=bp^[netp].fnr;
        ni.datei:=nfile;
      End;
      If found and (fa.ispoint=odd(bp^[netp].flags)) then
      Begin
        getmem(np,nanz*sizeof(noderec));
        seek(nodef,nadr);
        blockread(nodef,np^,nanz*sizeof(noderec));
        i:=0;
        While (i<nanz) and (np^[i].node<fa.node) Do inc(i);
{$IFDEF Debug }
  {$R+}
{$ENDIF }
        If (i<nanz) and (np^[i].node=fa.node) then _adr:=np^[i].adr Else _adr:=-1;
        freemem(np,nanz*sizeof(noderec));
        If (_adr>=0) and fa.ispoint then
        Begin
          seek(nodef,_adr);
          blockread(nodef,points,2);
          getmem(pp,points*sizeof(pointrec));
          blockread(nodef,pp^,points*sizeof(pointrec));
          i:=0;
          While (i<points) and (pp^[i].point<fa.point) Do inc(i);
          If (i<points) and (pp^[i].point=fa.point) then _adr:=pp^[i].adr
            Else _adr:=-1;
          freemem(pp,points*sizeof(pointrec));
        End;
        If _adr>=0 then
        Begin
          ni.ispoint:=fa.ispoint;
          If _nlid <> 0 Then
          Begin
            If _nlid = nfile Then ReadNData(nfile,_adr,ni) Else ni.found:=False;
          End
          Else Begin
            ReadNData(nfile,_adr,ni);
          End;
        End;
      End;
      inc(netp);
    Until not found or ni.found;
    dispose(bp);
  End;
  If (pointtyp=2) and not ni.found and fa.ispoint then
  Begin
    fa.ispoint:=false;
    Goto again;
  End;
  ni.ispoint:=fa.ispoint;
  If not nodelistopen then close(nodef);
End;  { of GetNodeInfoN }

Function IsFidoNode(adr:string):boolean;
Var ni : NodeInfo;
Begin
  GetNodeInfo(adr,ni,1);
  IsFidoNode:=ni.found;
End;

Procedure GetNodeUserInfo(var fa:FidoAdr; var ni:NodeInfo);
Type ubufa  = array[0..blocksize-1] of byte;
Var  f     : file;
     name  : string[MaxNamelen];
     vname : string[MaxNamelen];
     p,x,y : byte;
     buf   : ^ubufa;
     bufp  : word;
     l,r,m : longint;
     user  : UserRec;
     last  : boolean;
     anz   : longint;
     s     : string[91];
     brk   : boolean;
     height: word;
     res   :Word;

Label ende;

  Procedure GetNextUser;
  var
     b      : Byte;
  Begin
    With user Do
    Begin
      inc(bufp,2);                       {TS 06.08.02 n.b. Åberlesen }
      b:=buf^[bufp];                     {TS 06.08.02 LÑnge Name}
      FastMove(buf^[bufp],name[0],b+1);  {TS 06.08.02 kopieren Name}
      inc(bufp,b+1);                     {TS 06.08.02}
      FastMove(buf^[bufp],adr[0],8);     {TS 06.08.02 FIDO-Adr}
      inc(bufp,8);                       {TS 06.08.02}
      fnr:=buf^[bufp];                   {TS 06.08.02 NL Nummer}
      inc(bufp,1);                       {TS 06.08.02}
      FastMove(buf^[bufp],fadr,4);       {TS 06.08.02 Dateioffset}
      inc(bufp,4);                       {TS 06.08.02}
      last:=buf^[bufp]=$ff;              {TS 06.08.02}
    End;
  End;

  Procedure GetFirstUser;
  Begin
    bufp:=0;
    user.name:='';
    GetNextUser;
  End;

  Procedure ReadBlock(nr:longint);
  Begin
    seek(f,nr*BlockSize);
    blockread(f,buf^,BlockSize);
    GetFirstUser;
    vname:=left(user.name,length(name));
  End;

Begin  { of GetNodeUserInfo }
  fillchar(ni,sizeof(ni),0);
  If UserBlocks=0 then exit;    { keine Nodelisten -> leerer Index }
  With fa Do
  Begin
    p:=cpos(',',username);      { Name formatieren }
    If p>0 then
    Begin
      name:=left(username,p-1)+' '+trim(mid(username,p+1));
    End
    Else Begin
      p:=length(username);
      While (p>1) and (username[p]<>' ') Do dec(p);
      If p > 1 then name:=mid(username,p+1)+' '+left(username,p-1) Else name:=username;
    End;
  End;
  name:=trim(name);
  If name='' then exit;
  UpString(name);
  assign(f,UserIndexF);
  reset(f,1);
  new(buf);
  l:=1; r:=UserBlocks;
  Repeat
    m:=(l+r)div 2;
    ReadBlock(m);
    If vname<name then l:=m Else r:=m;
  Until (r-l<2) or (vname=name);
  If (name<>vname) and (l<m) then ReadBlock(l) Else l:=m;
  If name<vname then Goto ende;
  While (name=vname) and (l>1) do
  Begin
    dec(l);
    ReadBlock(l);
  End;
  Repeat
    While not last and (user.name<name) Do GetNextUser;
    If (user.name<name) and (l<userblocks) then
    Begin
      inc(l);
      ReadBlock(l);
    End;
  Until last or (left(user.name,length(name))>=name);
  If (left(user.name,length(name))=name) then
  Begin
    openlist(2,78,10,11,0,'/NS/SB/DM/APGD/');
    anz:=0;
    Repeat
      With user Do
      Begin
        ReadNdata(fnr,fadr,ni);
        app_l(' '+forms(TopAllStr(user.name),22)+' '+
          forms(strs(adr[0])+':'+strs(adr[1])+'/'+strs(adr[2])+
          iifs(adr[3]=0,'','.'+strs(adr[3])),16)+' '+
          forms((iifs(adr[3]=0,ni.boxname+', ',''))+ni.standort,31)+'  '+
          forms(NLfilename(fnr),12)+' ['+formi(fnr,2)+']');
        inc(anz);
        If not last then
        Begin
          GetNextUser;
        End
        Else Begin
          inc(l);
          If l<=userblocks then ReadBlock(l);
        End;
      End;
    Until (left(user.name,length(name))<>name) or (l>userblocks);
    If anz > 0 then
    Begin
      If anz = 1 then
      Begin
        s:=first_line;
      End
      Else Begin
        selbox(75,min(anz+2,screenlines-6),'',x,y,true);
        height:=min(anz+2,screenlines-6)-2;
        SetListsize(x+1,x+73,y+1,y+height);
        listarrows(x,y+1,y+height,col.colselbox,col.colselbox,'≥');
        listboxcol;
        pushhp(80);
        list(brk);
        pophp;
        closebox;
        If brk then s:='' else s:=get_selection;
      End;
      If s <> '' then
      Begin
        SplitFido(trim(copy(s,25,16)),fa,DefaultZone);
        GetNodeInfoN(trim(copy(s,25,16)),ni,1,IVal(Copy(s,length(s)-2,2)));
      End
      Else Begin
        ni.found:=false;
      End;
    End;
    closelist;
  End;

ende:
  close(f);
  dispose(buf);
End;  { of GetNodeUserInfo }

Procedure NodelistSeekN(_nlid_:Byte);
Const active : boolean = false;
Var x,y,b: byte;
    brk  : boolean;
    ni   : ^NodeInfo;
    adr  : string[30];
    fa   : fidoadr;
    first: boolean;

Begin
  If active or not TestNodelist or DisableAltN then exit;
  new(ni);
  active:=true;
{ MH: AltN-Screen zur besseren Lesbarkeit verbreitert }
  diabox(77,11,getres2(2100,1),x,y);    { Node-Infos abfragen }
  If MainNodelist>0 then
  Begin
    attrtxt(col.coldiarahmen);
    mwrt(x+70,y,' '+formi(nodelist^[MainNodelist].number,3)+' ');
    attrtxt(col.coldialog);
  End;    { MH: Aufbau geÑndert, damit mîglichst viel angezeigt werden kann }
  mwrt(x+3, y+2, GetRes2( 2100, 2));     { Box   }          { MH 03.02.2000 }
  mwrt(x+3, y+3, GetRes2( 2100, 4));     { Sysop }
  mwrt(x+3, y+4, GetRes2( 2100, 5));     { TelNr }
  mwrt(x+3, y+5, GetRes2( 2100, 6));     { Flags }
  mwrt(x+3, y+7, GetRes2( 2100, 3));     { ~⁄ƒƒƒØ [                    ] }
  mwrt(x+35, y+7, GetRes2( 2100, 10));   { E-Mail }
  mwrt(x+35, y+8, GetRes2( 2100, 9));    { Status }
  mwrt(x+55, y+8, GetRes2( 2100, 8));    { Datei }
  adr:='';
  first:=true;
  TempOpen;
  Repeat
    If first and (aktdispmode in [10..19]) then
    Begin
      dbGo(mbase,AktDisprec);
      If not dbEOF(mbase) and not dbBOF(mbase) and (mbNetztyp=nt_Fido) then
      Begin
        splitfido(dbReadStrN(mbase,mb_absender),fa,DefaultZone);
        adr:=MakeFidoAdr(fa,false);
        brk:=false;
      End;
    End;
    If not first or (adr='') then
    Begin
      pushhp(750);
      ReadString(x+3, y+8, Forms(GetRes2( 2100, 7), 6), adr, 20, 20, '', brk);
      pophp;                   { AKAƒƒØ }
    End;
    first:=false;
    If not brk then
    Begin
      If not isNodeAddress(adr) then
      Begin
        fa.username:=adr;
        getNodeUserInfo(fa,ni^);
        End Else Begin
        splitfido(adr,fa,DefaultZone);
        adr:=MakeFidoAdr(fa,true);
        GetNodeInfoN(adr,ni^,1,_nlid_); {TS 06.08.02}
      End;
      attrtxt(col.coldialog);
      moff;
    { MH: Fenster putzen }
      clwin(x+10, x+75, y+2, y+5); { Oberer Block }
      clwin(x+43, x+51, y+8, y+8); { Status      }
      clwin(x+62, x+73, y+8, y+8); { Datei      }
      clwin(x+11, x+30, y+7, y+7); { AKA [ ]   }
      clwin(x+43, x+75, y+7, y+7); { eMAiL    }
      If ni^.found then with ni^ Do
      Begin
        attrtxt(col.coldiahigh);   { MH: 42 -> 65 }
        wrt(x+10, y+2, left(BoxName + ', ' + Standort,65));
        wrt(x+10, y+3, Sysop);
        wrt(x+10, y+4, Telefon);
        wrt(x+10, y+5, copy(MailString(FFlags, True),1,65)); { eMail lîschen }
        wrt(x+11, y+7, MakeFidoAdr(fa, True));
        b := cpos('@', FFlags);
        If b = 0 then FFlags := '';
        wrt(x+43, y+7, MailString(FFlags, False)); { eMail extrahieren }
        wrt(x+43, y+8, Status);
        wrt(x+62, y+8, NLfilename(datei));
      End;
      mon;
    End;
  Until brk;
  freeres;
  closebox;
  dispose(ni);
  active:=false;
End;  { of NodelistSeekN }

Procedure NodeListSeek;
Begin
  NodeListSeekN(0);
End;

{ --- Nodelist-Browser ---------------------------------------------- }

Var  rdispx,rdispy : byte;

Procedure ShowRQ(s:string); { MH: Liste angepasst }
Var ni    : NodeInfo;
    add,p : byte;
    res   : Word;

Begin
  GetNodeInfoN(Trim(copy(s,39,18)),ni,1,IVal(Copy(s,length(s)-2,2))); {TS/my 06.08.02}
  add:=max(length(getres2(2131,30)),length(getres2(2131,32)));
  attrtxt(col.colselbox);
  If ni.ispoint then
  Begin
    mwrt(rdispx,rdispy,getres2(2131,33));    { 'Point' }
    mwrt(rdispx+54-length(getres2(2131,34)),rdispy-1,getres2(2131,34));
  End
  Else Begin                                 { 'Pointliste' }
    mwrt(rdispx,rdispy,getres2(2131,30));    { 'Sysop' }
    mwrt(rdispx+54-length(getres2(2131,31)),rdispy-1,getres2(2131,31));
  End;                                       { 'Nodeliste' }
  mwrt(rdispx,rdispy+1,'eMail');             { 'eMail' }       { my 02/2003 }
  mwrt(rdispx,rdispy+2,getres2(2131,32));    { 'Flags' }
  mwrt(rdispx,rdispy-1,getres2(2131,35));    { 'Status' }
  attrtxt(col.colselhigh);
  If ni.found then
  Begin
    mwrt(rdispx+add+2,rdispy,forms(iifs(ni.ispoint,ni.boxname,ni.sysop),33));
    mwrt(rdispx+55,rdispy-1,forms(NLfilename(ni.datei),12));
    p:=pos('IEM:',ustr(ni.fflags));
    if (cpos('@',ni.fflags)>0) and (p>0) then           { my 02/2003: eMail }
    begin
      mwrt(rdispx+add+2,rdispy+1,forms(mailstring(ni.fflags,false),65)); 
      ni.fflags:=mailstring(ni.fflags,true);            { my: eMail lîschen }
      p:=pos('IEM:',ustr(ni.fflags));              { my: nur zur Sicherheit }
      if (p>0) and ((ni.fflags[p+4]=',') or       { my: ggf. 'IEM:' lîschen }
                    (p+4>=length(ni.fflags))) then
        delete(ni.fflags,iif(p>1,p-1,p),iif(p>1,5,4));
    end;
    mwrt(rdispx+add+2,rdispy+2,forms(ni.fflags,65)); { MH: 40 -> 65 }
    mwrt(rdispx+7,rdispy-1,forms(ni.status,12));
  End
  Else Begin
    mwrt(rdispx+add+2,rdispy,sp(33));
    mwrt(rdispx+55,rdispy-1,sp(12));
    mwrt(rdispx+add+2,rdispy+1,sp(65)); { MH: 40 -> 65 }
    mwrt(rdispx+add+2,rdispy+2,sp(65)); { my 02/2003: eMail }
    mwrt(rdispx+7,rdispy-1,sp(12));
  End;
End;

Procedure NodelistBrowser;
Const orflags  = 10;
      andflags = 5;
Type NodeBrec = Record
                  nodeadr : string[20];
                  sysop   : string[30];
                  boxname : string[30];
                  standort: string[30];
                  telefon : string[20];
                  flags   : string[80]; { MH: 40 -> 80 }
                  fnl,snl,
                  pl      : boolean;
                End;
     NodeBRP  = ^NodeBrec;

Const NB_data : NodeBRP = nil;
      bufsize = 2048;

Var   x,y,h   : byte;
      brk     : boolean;
      flag    : array[1..orflags,1..andflags] of string[12];
      flaganz : array[1..orflags] of byte;
      flags   : byte;     { Anzahl or-Flags }
      i       : integer;
      t       : text;
      s,ss    : string;
      buf     : pointer;
      found,n : longint;
      k       : string[10];
      p       : byte;
      nn      : word;
      azone,                   { aktuelle Nodeadresse beim Suchen }
      anet,
      anode,
      apoint  : word;
      lastnet : word;
      sphone  : string[30];    { in Nodelistenformat konvertierte Tel.Nr. }
      skip    : boolean;
      ni      : nodeinfo;
      adr     : string[adrlen];
      d       : DB;
      res     : Word;
      _s      : String;

label again, NewStart;

  Procedure ParseFlags;
  Var s,s2 : string[80]; { MH: 40 -> 65 }
      p    : byte;
  Begin
    flags:=0;
    s:=nb_data^.flags;
    While (s <> '') and (flags < orflags) Do
    Begin
      inc(flags);
      flaganz[flags]:=0;
      p:=blankposx(s);
      s2:=left(s,p-1);
      While s2 <> '' Do
      Begin
        If flaganz[flags]<andflags then
        Begin
          inc(flaganz[flags]);
          flag[flags,flaganz[flags]]:=','+left(s2,cposx(',',s2)-1)+',';
        End;
        delete(s2,1,cposx(',',s2));
      End;
      s:=trim(mid(s,p+1));
    End;
  End;

  Procedure GetAddress(format:shortint; var skip:boolean);
  Var p  : byte;
      fa : FidoAdr;
  Begin
    skip:=false;
    Case format of
      nlNodelist,
      nl4Dpointlist: If k='ZONE' then
                     Begin
                       azone:=nn; anet:=nn; anode:=nn;
                       skip:=(format=nl4Dpointlist);
                     End
                     Else If (k='HOST') or (k='REGION') then
                     Begin
                       anet:=nn; anode:=0;
                       skip:=(format=nl4Dpointlist);
                     End
                     Else if k='POINT' then
                     Begin
                       apoint:=nn;
                     End
                     Else Begin
                       anode:=nn;
                       skip:=(format=nl4Dpointlist);
                     End;

      nlPoints24   : If k='HOST' then
                     Begin
                       ss:=left(ss,cposx(',',ss)-1);  { Nodeadresse isolieren }
                       p:=cpos('/',ss);
                       If p > 0 then
                       Begin
                         anet:=ival(left(ss,p-1));
                         anode:=ival(mid(ss,p+1));
                       End;
                       skip:=true;
                     End
                     Else If k='REGION' then
                     Begin            {TS 06.08.02 Region-EintrÑge Åberlesen}
                       skip:=True;    {TS 06.08.02 Region-EintrÑge Åberlesen}
                     End
                     Else If (k='') or (k='PVT') then
                     Begin
                       apoint:=nn;
                     End;

      nlFDpointlist: If k='BOSS' then
                     Begin
                       ss:=left(ss,cposx(',',ss)-1);
                       splitfido(ss,fa,azone);
                       azone:=fa.zone; anet:=fa.net; anode:=fa.node;
                       skip:=true;
                     End
                     Else If (k='') or (k='PVT') then
                     Begin
                       apoint:=nn;
                     End;

      nlNode       : If (k='') or (k='PVT') then apoint:=nn;
    End;  { case }
  End;

  Function ntest(ts:string):boolean;
  Var p : byte;
  Begin
    UpString(ts);
    p:=cposx(',',ss);
    ntest:=(ts='') or (pos(ts,left(ss,p-1))>0);
    delete(ss,1,p);
  End;

  Function gets:string;
  Var p : byte;
  Begin
    p:=cposx(',',s);
    gets:=left(s,p-1);
    delete(s,1,p);
  End;

  Function testphone:boolean;
  Begin
    testphone:=(left(GetToken(ss,','),length(sphone))=sphone);
  End;

  Function node_str:string;
  Begin
    If apoint=0 then node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)
      else node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)+'.'+strs(apoint);
  End;

  Function testaddress(var s:string):boolean;
  Begin
    If s='' then testaddress:=true else testaddress:=(pos(s,node_str)>0);
  End;

  Function testflags:boolean;
  Var i,j   : integer;
      _s    : string;
  Begin
    _s:=','+ss+',';
    If flags=0 then
    Begin
      testflags:=true;
    End
    Else Begin
      i:=0;
      Repeat
        inc(i);
        j:=1;
        While (j<=flaganz[i]) and (pos(flag[i,j],_s)>0) Do inc(j);
      Until (i>flags) or (j>flaganz[i]);
      testflags:=(i<=flags) and (j>flaganz[i]);
    End;
  End;

Begin  { of NodelistBrowser }
  If not TestNodelist then exit;
  If not assigned(NB_Data) then
  Begin
    new(NB_data);
    fillchar(NB_data^,sizeof(NB_Data^),0);
    NB_data^.fnl:=true; NB_data^.snl:=true;
  End;

again:
  dialog(ival(getres2(2131,0)),17,getres2(2131,1),x,y); { 'Nodelisten durchsuchen' }
  With NB_Data^ Do
  Begin
    maddstring(3,2,getres2(2131,2),sysop,30,30,'');     { 'Sysop' }
      mhnr(950);
      mnotrim;
    maddstring(3,3,getres2(2131,3),standort,30,30,'');  { 'Standort' }
      mnotrim;
    maddstring(3,4,getres2(2131,4),boxname,30,30,'');   { 'Boxname' }
      mnotrim;
    maddstring(3,6,getres2(2131,5),nodeadr,20,20,'0123456789:/.'); { 'Nodeadresse' }
    maddstring(3,7,getres2(2131,6),telefon,20,20,'0123456789-');   { 'Telefon' }
    maddstring(3,9,getres2(2131,8),flags,30,65,'>');  {MH: 40>65}  { 'Flags' }
    maddbool (3,11,getres2(2131,9),fnl);   { 'FidoNet-Nodeliste durchsuchen' }
    maddbool (3,12,getres2(2131,10),snl);  { 'sonstige Nodelisten durchsuchen' }
      mhnr(956);
    maddbool (3,13,getres2(2131,11),pl);   { 'Pointlisten durchsuchen' }
      mhnr(956);
    readmask(brk);
    closemask;
  End;
  If brk then
  Begin
    closebox;
    freeres;
    exit;  { TS 06.08.02 und raus aus der Routine }
  End;
  sphone:=NB_Data^.telefon;    { Telefon in Nodelistenformat konvertieren }
  If (IntVorwahl<>'') and (left(sphone,length(IntVorwahl))=IntVorwahl) then
    delete(sphone,1,length(IntVorwahl))
  Else If (NatVorwahl<>'') and (left(sphone,length(NatVorwahl))=NatVorwahl) then
    sphone:=left(Vorwahl,cpos('-',vorwahl))+mid(sphone,length(NatVorwahl)+1);
  ParseFlags;
  getmem(buf,bufsize);
  attrtxt(col.coldialog);
  mwrt(x+2,y+14,getres2(2131,20));   { 'Datei:' }
  mwrt(x+2,y+15,getres2(2131,21));   { 'Netz:'  }
  mwrt(x+28,y+14,getres2(2131,22));  { 'EintrÑge:' }
  mwrt(x+28,y+15,getres2(2131,23));  { 'passend:' }
  openlist(2,78,10,11,0,'/NS/SB/DM/');      { Koordinaten beliebig }
  ListInitEMS(2048);                        { 2 MB EMS reservieren }
  found:=0; n:=0;
  For i:=1 To NL_anz Do With Nodelist^[i],NB_Data^ Do
  Begin
    If ((fnl and (listfile='NODELIST.###') and (format=nlNodelist)) or
      (snl and (listfile<>'NODELIST.###') and (format=nlNodelist)) or
      (pl and (format<>nlNodelist))) and exist(FidoDir+NLfilename(i)) then
    Begin
      attrtxt(col.coldiahigh);
      mwrt(x+10,y+14,forms(NLfilename(i),12));
      assign(t,FidoDir+NLfilename(i));
      settextbuf(t,buf^,bufsize);
      reset(t);
      If zone<>0 then azone:=zone   { Start-Nodeadresse setzen }
        Else azone:=DefaultZone;
      apoint:=0;
      If format=nlNode then
      Begin
        anet:=net; anode:=node;
        End Else Begin
        anet:=0;
        anode:=0;
      End;
      lastnet:=65535;
      While not eof(t) and not brk Do
      Begin
        readln(t,s);
        If (s<>'') and (s[1]<>';') and (cpos(',',s)>0) then
        Begin
          inc(n);
          ss:=ustr(s);
          While cpos('_',ss)>0 Do ss[cpos('_',ss)]:=' ';
          k:=left(ss,cpos(',',ss)-1);
          delete(ss,1,cpos(',',ss));
          If k <> 'BOSS' then
          Begin
            p:=cposx(',',ss);
            nn:=minmax(ival(left(ss,p-1)),0,65535);
            delete(ss,1,p);
          End;
          GetAddress(format,skip);     { akt. Adresse ermitteln }
          If anet<>lastnet then mwrt(x+10,y+15,forms(strs(azone)+':'+strs(anet),15));
          lastnet:=anet;
          If n mod 100=0 then mwrt(x+38,y+14,strsn(n,7));
          If not skip and ntest(boxname) and ntest(standort) and
            ntest(sysop) and testphone and testaddress(nodeadr) and
            testflags then
          Begin
            inc(found);
            mwrt(x+38,y+15,strsn(found,7));
            gets; gets;  { Nodetyp und -nummer Åberlesen }
            While cpos('_',s)>0 Do s[cpos('_',s)]:=' ';
            ni.boxname:=gets; ni.standort:=gets;
            ni.sysop:=gets; ni.telefon:=gets;
            With ni Do
              app_l(' '+forms(iifs(apoint<>0,sysop,boxname)+', '+
                    standort,35)+'  '+forms(node_str,17)+' '+
                    forms(FormFidoPhone(telefon),17)+'  '+
                    forms(NLfilename(i),12)+' ['+formi(i,2)+']');
          End;
        End;
        testbrk(brk);
      End;   { not eof }
      close(t);
    End;
  End;
  freemem(buf,bufsize);
  closebox;
  If found = 0 then
  Begin
    closelist;
    If not brk then
    Begin
      rfehler(2126);        { 'Es wurden keine passenden EintrÑge gefunden.' }
      Goto again;
    End;
  End;
  If found > 0 then
  Begin
    signal;
    h:=min(found+7,screenlines-6);
    selbox(76,h,'',x,y,true);
    dec(h,6);
    rdispx:=x+2; rdispy:=y+h+2;
    attrtxt(col.colselrahmen);
    mwrt(x,rdispy-2,hbar(76));       { MH: rdispy-1 -> rdispy-2 }
    SetListsize(x+1,x+74,y+1,y+h-1); { MH: h -> h-1             }
    listboxcol;                      { MH: Liste angepasst      }
    listarrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
    listDp(ShowRQ);
    listTp(listext); listmakros:=0;

    Repeat

NewStart:                  { nach Break im AltN-Screen hier einspringen }
      pushhp(958);
      list(brk);
      pophp;
      n:=0;
      If not brk then
      Begin
        _s:=get_selection;
        GetNodeInfoN(trim(copy(_s,39,18)),ni,2,IVal(Copy(_s,length(_s)-2,2)));
        If not ni.found then
        Begin
          errsound;
        End
        Else Begin
          adr:=ni.sysop + ' @ ' +trim(copy(get_selection,39,18));
          If ni.ispoint then s:=adr else s:=ni.boxname + ', ' +adr;
          pushhp(959);
          n:=ReadIt(max(length(s)-4,length(getres(2132)))+11,s,
            getres(2132),    { ' ^Nachricht , ^Request , ^Info , ^ZurÅck ' }
            1,brk);          { ^Info Taste hinzugefÅgt                     }
          pophp;
          brk:=false;
        End;
      End;
    Until brk or (n=1) or (n=2) or (n=3);
    If n <> 3 then         { Break aus AltN-Screen hier nicht zulassen }
    Begin
      closelist;
      closebox;
    End;
    If not brk then case n of
      1 : Begin
            _keyboard(adr+keycr);
            dbOpen(d,BoxenFile,0);
            If dbRecCount(d)>1 then _keyboard(keyup+DefFidoBox+keycr);
            dbClose(d);
            msgdirect;
          End;
      2 : Begin
            _keyboard(keycr);
            AutoCrash:=FidoRequest(trim(mid(adr,cpos('@',adr)+1)),'');
          End;
      3 : Begin   { AltN-Screen aufrufen und Aka Åbergeben }
            _s:=get_selection;                    {TS 06.08.02 öbern. ausgew. Zeile}
            _keyboard(trim(copy(_s,39,18))+keycr);{TS 06.08.02}
            NodeListSeekN(IVal(Copy(_s,length(_s)-2,2))); { AltN-Screen starten}
            Goto newstart;                        { ZurÅck zur Liste}
          End;
    End;
  End;
  freeres;
End;  { of NodelistBrowser }

{
  $Log$
  Revision 1.7.2.4  2003/03/17 22:36:58  my
  TS [+MY]:- Fido: Abfrage, Durchsuchen und Verwalten von Nodelisten
                   geÑndert/korrigiert/erweitert
             ----------------------------------------------------------------------
             - Userindex NODEUSER.IDX "entschlackt", Code kleiner und
               Åbersichtlicher, dadurch Laufzeit auf langsamen Rechnern
               schneller. Bisherige Komprimierung des Index aufgehoben,
               dadurch diverse Probleme behoben und die Mîglichkeit
               zusÑtzlicher Erweiterungen geschaffen (s.u.).
               (ToDo: Nodelisten-Index bei Update automatisch neu
                      schreiben)
             - Fix: Es kam vor, da· manche Listen (z.B. die Zone21-
               Pointliste) zwar eingebunden und indiziert wurden, bei
               einer Nodelist-Abfrage mit <Alt-N> auf die in der
               Auswahlliste angezeigten EintrÑge aber trotzdem nicht
               zugegriffen werden konnte (nach Auswahl mit <Enter> war die
               Anzeige leer). Dies ist durch den Wegfall der Komprimierung
               jetzt behoben.
             - Fix: Beim Durchsuchen der Node-/Pointlisten mit F/N/D ist
               jetzt sichergestellt, da· a) weder fehlende noch b) unzu-
               treffende noch c) doppelte EintrÑge in der Suchergebnis-
               liste auftreten kînnen. Z.B. wurden u.U. EintrÑge gefunden,
               die gar nicht den Suchkriterien entsprachen, und im Sucher-
               gebnis dann mit falschem Sysop-Namen angezeigt; kam eine
               AKA in mehreren Listen vor (z.B. POINTS24 und R24PNT),
               wurde nach dem Zufallsprinzip nur der Eintrag aus einer der
               Listen (der aber dafÅr mehrfach) angezeigt.
             - In allen Suchergebnis-/Auswahllisten steht jetzt hinter dem
               sichtbaren Eintrag der Dateiname sowie die interne Nummer
               der Liste, aus der der jeweilige Eintrag stammt (sichtbar
               zu machen durch Scrollen mit <Cursor-rechts>). Damit ist
               bei mehreren EintrÑgen mit identischer AKA, die aus
               unterschiedlichen Listen stammen, die Herkunft des Eintrags
               erkennbar. Au·erdem werden diese Daten fÅr die weitere
               interne Verarbeitung benîtigt:
             - Fix: Bei der Auswahl eines Eintrags aus der jeweiligen
               Auswahlliste mit <Enter> ist jetzt bei mehreren EintrÑgen
               mit identischer AKA sichergestellt, da· auch wirklich auf
               die Daten des ausgewÑhlten Eintrags in der zugehîrigen
               Node-/Pointliste zugegriffen wird. Bisher wurden die Daten
               unabhÑngig vom ausgewÑhlten Eintrag immer derselben
               (zufÑlligen) Liste entnommen, und an die Daten der EintrÑge
               aus den Åbrigen Listen kam man gar nicht heran. Speziell
               bei EintrÑgen aus Listen FTN-kompatibler Netze wurde
               stellenweise auf die Daten eines beliebigen Eintrags in der
               Fido-Nodeliste zugegriffen.
             - Einige (vorlÑufige) énderungen in der Detailanzeige
               unterhalb der Auswahlliste beim Durchsuchen mit F/N/D.
             - Fix: Bei Listen im Points24-Format werden "Region"-EintrÑge
               (i.d.R. sind das PLK-EintrÑge) beim Durchsuchen mit F/N/D
               ignoriert (Verhalten damit jetzt identisch mit der
               Nodelisten-Abfrage bei <Alt-N> bzw. F/N/A).
             - Listen in einem Points24-kompatiblen Format werden jetzt
               nicht mehr nur am Dateinamen "POINTS24.###", sondern auch
               am Format selbst erkannt. Dadurch wird beim Einbinden
               solcher Listen (z.B. Zone21-Liste) das korrekte Listen-
               format "Points24" (bisher: "Nodeliste") vorgeschlagen.
             - Interne énderung: Neuer Schalter "FIDOTST" in XPDEFINE.INC,
               mit dem der Nodelisten-Index in Textform ausgegeben werden
               kann.

  Revision 1.7.2.3  2001/08/12 11:20:39  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.7.2.2  2000/08/20 10:41:23  mk
  MO:- erster Nodelistenindex ist jetzt kleiner

  Revision 1.7.2.1  2000/08/10 08:31:30  jg
  - Fix: Fido...Nodelisten abfragen
    Direkte Nachricht an Suchergebnis schicken funktioniert wieder.

  Revision 1.7  2000/04/25 21:19:20  mk
  - 32 Bit: Potentieller Fix fuer Suche nach Points in der Pointliste

  Revision 1.6  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

}

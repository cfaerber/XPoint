{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2002 OpenXP-Team, http://www.openxp.de                 }
{ (c) 2002-2003 OpenXP/16, http://www.openxp16.de                 }
{ See list of contributors in authors.txt                         }
{                                                                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ OpenXP ist eine eingetragene Marke von Markus Kaemmerer.        }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ XPFIDO - Nodeliste auslesen/abfragen/durchsuchen }


{ --- my: Deklarationen fÅr Nodelist-Browser ---  }

var   x,y,
      rdispx,rdispy,                 { fÅr ShowRQ }
      ndispx,ndispy   : byte;        { fÅr ShowNL }
      foundanz,
      foundback,n     : longint;
      active,listopen,
      NewSeek,NewGrep,
      first,seek_brk,
      grep_brk        : boolean;
      ni              : NodeInfo;
      adr             : string[adrlen];
      sel             : string;

const width = 76;

{ --- /my: Deklarationen fÅr Nodelist-Browser --- }


Procedure ReadNData(var nfile:byte; var adr:longint; var ni:NodeInfo);
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
Procedure GetNodeInfo(const adr:string; var ni:nodeinfo; const pointtyp:shortint);
Begin
  GetNodeInfoN(adr,ni,pointtyp,0); {TS 06.08.02 Neue Proc. mit komp. Parameter}
End;

procedure GetNodeInfoN(const adr:string; var ni:nodeinfo;
                       const pointtyp:shortint; const _nlid:Byte);
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

Function IsFidoNode(var adr:string):boolean;
Var ni : NodeInfo;
Begin
  GetNodeInfo(adr,ni,1);
  IsFidoNode:=ni.found;
End;

{ my: neue Routine fÅr Nodelist-Browser   03/2003 }

procedure OpenBrowseList;
begin
  openlist(2,78,10,11,0,'/NS/SB/DM/');  { Koordinaten beliebig }
  ListInitEMS(2048);                    { 2 MB EMS reservieren }
  listopen:=true;
end;

{ my: neue Routine fÅr Nodelist-Browser   03/2003 }

procedure app_Browserline(var ni:NodeInfo; const adr:string);
begin
  if active and listopen then
  begin
    closelist;
    listopen:=false;
    active:=false;
  end;
  if not listopen then OpenBrowseList;
  with ni do
    app_l(' '+forms(iifs(ispoint,sysop,boxname)+', '+
          standort,35)+'  '+forms(adr,17)+' '+
          forms(FormFidoPhone(telefon),17)+'  '+
          forms(NLfilename(datei),12)+' ['+formi(datei,2)+']');
  inc(foundanz);
end;

{ my: Status und Nodeliste unterhalb Auswahlliste anzeigen   03/2003 }

procedure ShowNL(s:string);
var ni : NodeInfo;
begin
  GetNodeInfoN(trim(copy(s,25,16)),ni,1,IVal(Copy(s,length(s)-2,2)));
  mwrt(ndispx,ndispy,getres2(2131,32));                 { 'Status'   }
  if ni.ispoint then
    mwrt(ndispx+58-length(getres2(2131,31)),ndispy,getres2(2131,31))
  else
    mwrt(ndispx+58-length(getres2(2131,30)),ndispy,getres2(2131,30));
  attrtxt(col.colselhigh);
  mwrt(ndispx+length(getres2(2131,32))+1,ndispy,forms(ni.status,12));
  mwrt(ndispx+59,ndispy,forms(NLfilename(ni.datei),12));
end;

procedure GetNodeUserInfo(var fa:FidoAdr; var ni:NodeInfo; const browser:boolean);
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
    if not browser then
    begin
      openlist(2,78,10,11,0,'/NS/SB/DM/APGD/');
      anz:=0;
    end;
    Repeat
      With user Do                    { my: Nodelist-Browser berÅcksichtigt }
      Begin
        ReadNdata(fnr,fadr,ni);
        if browser then
        begin
          ni.ispoint:=adr[3]<>0; ni.datei:=fnr;
          s:=strs(adr[0])+':'+strs(adr[1])+'/'+strs(adr[2])+      { Adresse }
                  iifs(ni.ispoint,'.'+strs(adr[3]),'');
          app_Browserline(ni,s);
        end
        else begin
          app_l(' '+forms(TopAllStr(user.name),22)+' '+
            forms(strs(adr[0])+':'+strs(adr[1])+'/'+strs(adr[2])+
            iifs(adr[3]=0,'','.'+strs(adr[3])),16)+' '+
            forms((iifs(adr[3]=0,ni.boxname+', ',''))+ni.standort,31)+'  '+
            forms(NLfilename(fnr),12)+' ['+formi(fnr,2)+']');
          inc(anz);
        end;
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
    if not browser then               { my: Nodelist-Browser berÅcksichtigt }
    begin
      If anz > 0 then
      Begin
        If anz = 1 then
        Begin
          s:=first_line;
        End
        Else Begin
          height:=min(anz+4,screenlines-8);
          selbox(75,height,'',x,y,true);
          dec(height,4);
          ndispx:=x+2; ndispy:=y+height+2;
          attrtxt(col.colselrahmen);
          mwrt(x,ndispy-1,hbar(75));
          SetListsize(x+1,x+73,y+1,y+height);
          listboxcol;
          listarrows(x,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
          listDp(ShowNL);     { my: Status und Nodeliste anzeigen   03/2003 }
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
    end;
  End;
ende:
  close(f);
  dispose(buf);
End;  { of GetNodeUserInfo }


{ --- Nodelist-Browser ---------------------------------------------- }
{     (Deklarationen siehe Anfang der Unit)                           }
{                                                                     }
{     my: Komplettes Redesign unter Einbeziehung der                  }
{         Nodelist-Abfrage mit <Alt-N> bzw. F/N/A                     } 
{         (diese verwendet jetzt dieselbe Ausgabe inkl.               }
{         Zusatzfunktionen wie das Durchsuchen mit F/N/D),            }
{         Hotkey <AltGr-N> fÅr Nodeliste durchsuchen                  }
{         implementiert, Bugfixes, u.v.m.                     03/2003 }

procedure NodelistBrowserDisabled;
begin
  if IsNodelistBrowserMsg then
    hinweis(getres2(12100,27))  { 'Nachricht aus Nodelist-Browser erzeugt -> Nodelist-Browser deaktiviert.' }
  else if IsNodelistBrowserReq then
    hinweis(getres2(12100,28)); { 'F'Request aus Nodelist-Browser erzeugt -> Nodelist-Browser deaktiviert.' }
end;

procedure NodeListSeek;
begin
  NodelistBrowser(true);
end;

procedure NodeListGrep;
begin
  NodelistBrowser(false);
end;

procedure BrowserSeek;       { my: Nodelistenabfrage mit <Alt-N> oder F/N/A }
var x,y,i,l    : byte;
    fa         : fidoadr;
    SeekOnMail : boolean;

type  SeekStrT = string[30];
      SeekStrP = ^SeekStrT;

const SeekStr : SeekStrP = nil;

begin
  SeekOnMail:=false;
  if not assigned(SeekStr) then
  begin
    new(SeekStr);
    SeekStr^:='';
  end;
  if not first then foundback:=foundanz;
  foundanz:=0;
  if first and (aktdispmode in [10..19]) then
  begin
    dbGo(mbase,AktDisprec);
    If not dbEOF(mbase) and not dbBOF(mbase) and (mbNetztyp=nt_Fido) then
    begin
      splitfido(dbReadStrN(mbase,mb_absender),fa,DefaultZone);
      SeekStr^:=MakeFidoAdr(fa,false);
      SeekOnMail:=SeekStr^<>'';
    end;
  end;
  if not SeekOnMail then
  begin
    pushhp(750);
    l:=length(getres2(2100,2))+sizeof(SeekStr^)+6;
    dialog(l,3,getres2(2100,1),x,y);                { 'Node-Infos abfragen' }
    mwrt((l+x)-6,y-1,iifs(MainNodelist>0,
         ' '+formi(nodelist^[MainNodelist].number,3)+' ',''));
    maddstring(3,2,getres2(2100,2),SeekStr^,                     { 'Suchen' }
               sizeof(SeekStr^)-1,sizeof(SeekStr^)-1,'');
    repeat
      readmask(seek_brk);
      if not seek_brk and (SeekStr^='') then errsound;
    until seek_brk or (SeekStr^<>'');
    enddialog;
    pophp;
  end;
  if not seek_brk then
  begin
    if not isNodeAddress(SeekStr^) then
    begin
      fa.username:=SeekStr^;
      GetNodeUserInfo(fa,ni,true);                       { Suche nach Namen }
    end
    else begin                                             { Suche nach AKA }
      if not SeekOnMail then
      begin
        splitfido(SeekStr^,fa,DefaultZone);
        SeekStr^:=MakeFidoAdr(fa,true);
      end
      else SeekOnMail:=fa.ispoint;
      l:=1+byte(SeekonMail);
      repeat
        dec(l);
        for i:=1 to NL_anz do                        { alle EintrÑge in     }
        begin                                        { allen Listen suchen! }
          GetNodeInfoN(SeekStr^,ni,1,i);
          if ni.found then
            app_Browserline(ni,SeekStr^);
        end;
        if SeekOnMail then
        begin                                        { Bei <Alt-N> auf Mail }
          SeekStr^:=MakeFidoAdr(fa,true);            { auch Point suchen!   }
          SeekOnMail:=false;
        end;
      until l=0;
    end;
  end
  else if not first then  { seek_brk }
    foundanz:=foundback;
end;  { of BrowserSeek }

procedure BrowserGrep; { my: Nodeliste durchsuchen mit <AltGr-N> oder F/N/D }
type  NodeBrec = Record
                   nodeadr : string[20];
                   sysop   : string[30];
                   boxname : string[30];
                   standort: string[30];
                   telefon : string[20];
                   flags   : string[80];
                   fnl,snl,
                   pl      : boolean;
                 End;
      NodeBRP  = ^NodeBrec;

const NB_data  : NodeBRP = nil;
      orflags  = 10;
      andflags = 5;
      bufsize  = 2048;

var   flag    : array[1..orflags,1..andflags] of string[12];
      flaganz : array[1..orflags] of byte;
      flags   : byte;     { Anzahl or-Flags }
      i       : integer;
      t       : text;
      buf     : pointer;
      k       : string[10];
      p       : byte;
      n1      : longint;
      n2      : word;
      azone,                   { aktuelle Nodeadresse beim Suchen }
      anet,
      anode,
      apoint  : word;
      lastnet : word;
      sphone  : string[30];    { in Nodelistenformat konvertierte Tel.Nr. }
      skip    : boolean;
      s,ss    : string;
      NB_back : NodeBrec;

  Procedure ParseFlags;
  Var s,s2 : string[80];
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

  Procedure GetAddress(const format:shortint; var skip:boolean);
  Var p  : byte;
      fa : FidoAdr;
  Begin
    skip:=false;
    Case format of
      nlNodelist,
      nl4Dpointlist: If k='ZONE' then
                     Begin
                       azone:=n2; anet:=n2; anode:=0;
                       skip:=(format=nl4Dpointlist);
                     End
                     Else If (k='HOST') or (k='REGION') then
                     Begin
                       anet:=n2; anode:=0;
                       skip:=(format=nl4Dpointlist);
                     End
                     Else if k='POINT' then
                     Begin
                       apoint:=n2;
                     End
                     Else Begin
                       anode:=n2;
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
                     Else If (k='ZONE') or (k='REGION') then
                     Begin                     { ts/my 03/2003: Region- und }
                       skip:=True;             { Zone-EintrÑge Åberlesen    }
                     End
                     Else If (k='') or (k='PVT') then
                     Begin
                       apoint:=n2;
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
                       apoint:=n2;
                     End;

      nlNode       : If (k='') or (k='PVT') then apoint:=n2;
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
    If apoint=0 then
      node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)
    else
      node_str:=strs(azone)+':'+strs(anet)+'/'+strs(anode)+'.'+strs(apoint);
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

begin  { of BrowserGrep }

  If not assigned(NB_Data) then
  Begin
    new(NB_data);
    fillchar(NB_data^,sizeof(NB_data^),0);
    NB_data^.fnl:=true; NB_data^.snl:=true;
  End;

  if not first then
  begin
    NB_back:=NB_data^;
    foundback:=foundanz;
  end;

  repeat
    dialog(ival(getres2(2131,0)),17,getres2(2131,1),x,y); { 'Nodelisten durchsuchen' }
    mwrt((ival(getres2(2131,0))+x)-6,y-1,iifs(MainNodelist>0,
         ' '+formi(nodelist^[MainNodelist].number,3)+' ',''));
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
      maddstring(3,9,getres2(2131,8),flags,30,80,'>');               { 'Flags' }
      maddbool (3,11,getres2(2131,9),fnl);   { 'FidoNet-Nodeliste durchsuchen' }
      maddbool (3,12,getres2(2131,10),snl);  { 'sonstige Nodelisten durchsuchen' }
        mhnr(956);
      maddbool (3,13,getres2(2131,11),pl);   { 'Pointlisten durchsuchen' }
        mhnr(956);
      readmask(grep_brk);
      closemask;
    End;
    If grep_brk then
    begin
      closebox;
      break;  { Schleife verlassen }
    end;
    foundanz:=0; n1:=0;
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
        While not eof(t) and not grep_brk Do
        Begin
          readln(t,s);
          If (s<>'') and (s[1]<>';') and (cpos(',',s)>0) then
          Begin
            inc(n1);
            ss:=ustr(s);
            While cpos('_',ss)>0 Do ss[cpos('_',ss)]:=' ';
            k:=left(ss,cpos(',',ss)-1);
            delete(ss,1,cpos(',',ss));
            If k <> 'BOSS' then
            Begin
              p:=cposx(',',ss);
              n2:=minmax(ival(left(ss,p-1)),0,65535);
              delete(ss,1,p);
            End;
            GetAddress(format,skip);     { akt. Adresse ermitteln }
            If anet<>lastnet then mwrt(x+10,y+15,forms(strs(azone)+':'+strs(anet),15));
            lastnet:=anet;
            If n1 mod 100=0 then mwrt(x+38,y+14,strsn(n1,7));
            If not skip and ntest(boxname) and ntest(standort) and
              ntest(sysop) and testphone and testaddress(nodeadr) and
              testflags then
            Begin
              gets; gets;  { Nodetyp und -nummer Åberlesen }
              While cpos('_',s)>0 Do s[cpos('_',s)]:=' ';
              ni.ispoint:=apoint<>0; ni.boxname:=gets; ni.standort:=gets;
              ni.sysop:=gets; ni.telefon:=gets; ni.datei:=i;
              app_Browserline(ni,node_str);
              mwrt(x+38,y+15,strsn(foundanz,7));
            End;
          End;
          testbrk(grep_brk);
        End;   { not eof(t) and not grep_brk }
        close(t);
      End;
    End;
    grep_brk:=grep_brk and (foundanz=0);  { wenn > 0, Suchergebnis anzeigen }
    closebox;
    freemem(buf,bufsize);
  until not grep_brk;

  if grep_brk and not first then
  begin
    NB_data^:=NB_back;
    foundanz:=foundback;
  end;

end;  { of BrowserGrep }

procedure BrowserKeys(var t:taste);
var c : char;

  procedure DoSelection;
  var dummy : boolean;
  begin
    sel:=get_selection;
    if foundanz=0 then
      errsound
    else begin
      GetNodeInfoN(trim(copy(sel,39,18)),ni,2,IVal(Copy(sel,length(sel)-2,2)));
      if not ni.found then  { dÅrfte nicht vorkommen }
        errsound
      else begin
        adr:=ni.sysop + ' @ ' +trim(copy(sel,39,18));
        If ni.ispoint then sel:=adr else sel:=ni.boxname + ', ' +adr;
        pushhp(959);
        n:=ReadIt(max(length(sel)-4,length(getres(2132)))+11,sel,
          getres(2132),1,dummy);      { ' ^Nachricht , ^Request , ^ZurÅck ' }
        pophp;
        if n>=3 then n:=0;
      end;
    end;
  end;

begin
  c:=t[1];
  if (cpos(UpCase(c),getres2(2131,29))=1) or                    {    'A'    }
     (t=keyaltn) then                                           {  <Alt-N>  }
  begin
    BrowserSeek;
    if not seek_brk then
    begin
      NewSeek:=true;
      NewGrep:=false;
      t:=keyesc;          { Lister verlassen! }
    end;
  end
  else if (cpos(UpCase(c),getres2(2131,29))=2) or               {    'D'    }
          AltGr_N_pressed then                                  { <AltGr-N> }
  begin
    BrowserGrep;
    if not grep_brk then
    begin
      NewGrep:=true;
      NewSeek:=false;
      t:=keyesc;          { Lister verlassen! }
    end;
  end
  else if t=keycr then                                          {  <Enter>  }
  begin
    if NodelistBrowserButtonsDisabled then
    begin
      t:='';              { Lister nicht verlassen! }
      hinweis(getres2(12100,29));
    end
    else begin
      DoSelection;
      if n=0 then t:='';  { Lister nicht verlassen, wenn keine Aktion! }
    end;
  end
  else if (UpCase(c)=k4_D) or (UpCase(c)=k4_W) or           { ^D, 'W', 'E', }
          (UpCase(c)='E') or (t=keytab) or                  { 's', <Tab>    }
          (t='s') or (t=^S) then
  begin
    if t=^S then t:='s';       { <Ctrl-S> fÅr Markiersuche in engl. Version }
    listext(t);
  end;
end;

procedure ShowRQ(s:string);
var        ni : NodeInfo;
    add,i,p,l : byte;
          ffl : array[0..2] of string[63];
Begin
  if foundanz=0 then                                       { ts/my  03/2003 }
    fillchar(ni,sizeof(ni),0)
  else
    GetNodeInfoN(Trim(copy(s,39,18)),ni,1,IVal(Copy(s,length(s)-2,2)));
  add:=0;
  for i:=32 to 37 do
    add:=max(add,length(getres2(2131,i)));
  inc(add);
  l:=(width-add)-4;         { LÑnge der angezeigten Zeile }
  attrtxt(col.colselbox);
  for i:=0 to 2 do ffl[i]:='';
  mwrt(rdispx,rdispy,getres2(2131,32));                 { 'Status'   }
  mwrt(rdispx,rdispy+2,getres2(2131,35));               { 'Standort' }
  mwrt(rdispx,rdispy+4,getres2(2131,38));               { 'Flags'    }
  if ni.ispoint then
  begin                                                 { 'Pointliste' }
    mwrt(rdispx+54-length(getres2(2131,31)),rdispy,getres2(2131,31));
    mwrt(rdispx,rdispy+1,forms(getres2(2131,34),add));  { 'Name'     }
    mwrt(rdispx,rdispy+3,forms(getres2(2131,37),add));  { 'Point'    }
  end
  else begin                                            { 'Nodeliste' }
    mwrt(rdispx+54-length(getres2(2131,30)),rdispy,getres2(2131,30));
    mwrt(rdispx,rdispy+1,forms(getres2(2131,33),add));  { 'Boxname'  }
    mwrt(rdispx,rdispy+3,forms(getres2(2131,36),add));  { 'Sysop'    }
  end;
  attrtxt(col.colselhigh);
  If ni.found then
  begin
    mwrt(rdispx+add,rdispy,forms(ni.status,12));
    mwrt(rdispx+55,rdispy,forms(NLfilename(ni.datei),12));
    mwrt(rdispx+add,rdispy+1,forms(iifs(ni.ispoint,ni.sysop,ni.boxname),l));
    mwrt(rdispx+add,rdispy+2,forms(ni.standort,l));
    mwrt(rdispx+add,rdispy+3,forms(iifs(ni.ispoint,ni.boxname,ni.sysop),l));
    if length(ni.fflags) < l then
    begin
      ffl[0]:=ni.fflags;
    end
    else begin                                      { my: Flags in 3 Zeilen }
      i:=0;
      while (ni.fflags<>'') and (i<=2) do
      begin
        p:=l;
        while (ni.fflags[p]<>',') and (p>1) do dec(p);
        p:=iif((p<1) or (i=2),l,p);
        ffl[i]:=left(ni.fflags,p);
        ni.fflags:=mid(ni.fflags,p+1);
        inc(i);
      end;
    end;
    for i:=0 to 2 do
      mwrt(rdispx+add,rdispy+4+i,forms(ffl[i],l));
  end
  else begin                                                 { Maske putzen }
    mwrt(rdispx+add,rdispy,sp(12));
    mwrt(rdispx+55,rdispy,sp(12));
    for i:=1 to 6 do
      mwrt(rdispx+add,rdispy+i,sp(l));
  end;
end;


procedure NodelistBrowser(AltN:boolean);
var   h,zen,i,add,
      AltFunc      : byte;
      brk,
      Old_ShowSeek : boolean;
      d            : DB;

  procedure ResetGlobalVars;
  begin
    ListShowSeek:=Old_ShowSeek;
    IsNodelistBrowserMsg:=false;
    IsNodelistBrowserReq:=false;
    if AltFunc=0 then                       { Prozedur Åber MenÅ aufgerufen }
    begin                                   { => alle aktiv-Flags resetten  }
      altproc[5].aktiv:=false;
      altproc[maxalt+1].aktiv:=false;
    end else                          { Prozedur Åber Hotkey aufgerufen =>  }
      altproc[AltFunc].aktiv:=false;  { nur manuell gesetztes Flag resetten }
    freeres;
  end;

begin

  if not TestNodelist or DisableAltN then exit;

  Old_ShowSeek:=ListShowSeek; ListShowSeek:=false;
  active:=false; listopen:=false;
  brk:=false; seek_brk:=false; grep_brk:=false;
  NewSeek:=false; NewGrep:=false; first:=true;

                                             { aktiv-Flags fÅr Hotkeys      }
  if altproc[5].aktiv then                   { setzen um zu verhindern, da· }
    AltFunc:=maxalt+1                        { <Alt-N> oder <AltGr-N> die   }
  else if altproc[maxalt+1].aktiv then       { Prozedur rekursiv aufrufen   }
    AltFunc:=5                               { kînnen ('AltFunc' erhÑlt den }
  else                                       { Wert der Funktion, deren     }
    AltFunc:=0;                              { Flag manuell gesetzt wurde   }
  altproc[5].aktiv:=true;        { Alt-N   } { und der daher spÑter zurÅck- }
  altproc[maxalt+1].aktiv:=true; { AltGr-N } { gesetzt werden darf/mu·).    }

  TempOpen;                                           { Datenbanken îffnen! }

  { Erste Suche (weitere SuchlÑufe siehe 'BrowserKeys'!) }
  if AltN then
    BrowserSeek                                     { Nodeliste abfragen    }
  else
    BrowserGrep;                                    { Nodeliste durchsuchen }

  if ((AltN and seek_brk) or             { Ganz raus, wenn User bei erster  }
      (not AltN and grep_brk)) then      { Suche <Esc> gedrÅckt hat         }
  begin
    if first then              { bei Folgesuchen (= not first) alte Listbox }
    begin                      { erhalten, wenn User <Esc> gedrÅckt hat     }
      ResetGlobalVars;
      exit;
    end;
  end

  else repeat                    { ab hier wird Listbox immer neu aufgebaut }

    active:=true;

    if foundanz=0 then
      errsound
    else if not AltN then            { Signal nur bei Nodeliste durchsuchen }
      signal;
    if not first then closebox;                { aktuelle Listbox schlie·en }
    first:=false;
    if foundanz=0 then                     { bei foundanz>0 wurde Lister in }
    begin                                  { 'app_BrowserLine' geschlossen! }
      if listopen then closelist;
      OpenBrowseList;
      app_l(dup((width-2-length(getres2(12100,26))) div 2,' ')+
            getres2(12100,26));       { 'Keine passenden EintrÑge gefunden' }
    end;
    h:=min(iif(foundanz=0,1,foundanz)+10,screenlines-8);
    sel:=getres2(2131,28-byte(NodelistBrowserButtonsDisabled));
    zen:=(width-length(sel)) div 2;       { zum Zentrieren der Hotkey-Zeile }
    add:=0;
    selbox(width,h,'',x,y,true);
    mwrt(x+1,y,getres2(2131,26)); { 'Box/Point, Standort ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ AKA ƒƒƒƒƒƒƒƒƒƒƒƒƒ Telefon' }
    mwrt(x+zen,y+h-1,sel);        { 'Nodelisten: < >bfragen ƒ < >urchsuchen ƒƒƒƒ <     > ƒƒƒƒ <   >' }
    attrtxt(col.colselhigh);
    rdispx:=length(getres2(2131,29));      { 'rdispx' fÅr LÑnge mi·brauchen }
    for i:=1 to (rdispx+2)-byte(NodelistBrowserButtonsDisabled) do
    begin
      n:=cpos('<',sel);
      if n=0 then break;
      mwrt(x+zen+n+add,y+h-1,     { '             A            D                  Enter        Esc ' }
           iifs(i<=rdispx,copy(getres2(2131,29),i,1),
                          iifs((i=rdispx+1) and not NodelistBrowserButtonsDisabled,'Enter','Esc')));
      sel:=mid(sel,n+1);
      add:=n+add;
    end;
    dec(h,10);
    rdispx:=x+2; rdispy:=y+h+2;
    attrtxt(col.colselrahmen);
    mwrt(x,rdispy-1,hbar(width));
    SetListsize(x+1,x+(width-2),y+1,y+h);
    listboxcol;
    listarrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
    listDp(ShowRQ);
    listTp(BrowserKeys);
    listmakros:=0;

    repeat   { Browser starten }
      n:=0;
      pushhp(958);
      list(brk);
      pophp;
      if NewSeek or NewGrep then   { neue Suche }
      begin
        AltN:=NewSeek;
        brk:=false;
      end;
    until brk or NewSeek or NewGrep or (n>0);

    If brk or (n>0) then
    begin
      closelist;
      active:=false;
      listopen:=false;
    end;

    if not (brk or NewSeek or NewGrep) and (n>0) then case n of
      1 : Begin
            _keyboard(adr+keycr);
            dbOpen(d,BoxenFile,0);
            If dbRecCount(d)>1 then _keyboard(keyup+DefFidoBox+keycr);
            dbClose(d);
            IsNodelistBrowserMsg:=true;
            msgdirect;
          End;
      2 : Begin
            _keyboard(keycr);
            IsNodelistBrowserReq:=true;
            AutoCrash:=FidoRequest(trim(mid(adr,cpos('@',adr)+1)),'');
          End;
    End;

    NewSeek:=false; NewGrep:=false;

  until brk or (n>0);

  ResetGlobalVars;
  closebox;

end;  { of NodelistBrowser }

{
  $Log$
  Revision 1.7.2.6  2003/05/01 14:22:58  mk
  - updated copyright headers

  Revision 1.7.2.5  2003/03/17 23:02:20  my
  MY:- Fido: Grundlegendes Redesign der Nodelist-Abfrage mit <Alt-N> bzw.
             F/N/A und des Durchsuchens mit F/N/D ("Nodelist-Browser")
       ----------------------------------------------------------------------
       - Die Nodelist-Abfrage mit <Alt-N> bzw. F/N/A verwendet zur Ausgabe
         jetzt den "Nodelist-Browser", der bisher nur von der Durchsuchen-
         Funktion bei F/N/D verwendet wurde. Dadurch werden bei mehreren
         gefundenen EintrÑgen alle zugehîrigen Detailinformationen jetzt
         direkt beim Scrollen unterhalb der Auswahlliste angezeigt, ohne fÅr
         jeden der EintrÑge eine erneute Suche starten zu mÅssen. Des
         weiteren stehen damit jetzt auch bei der Nodelist-Abfrage die
         Zusatzfunktionen "Nachricht" und "Request" sowie die Åbrigen Tasten
         fÅr Markiersuche ("s"), Schreiben in Datei oder Clipboard ("w") und
         Drucken (<Ctrl-D>) zur VerfÅgung.
         Die bisherige gesonderte Ausgabe fÅr die Nodelist-Abfrage machte
         noch nie wirklich Sinn, da sie weniger funktional war, aber auch
         nicht mehr Information enthielt.
       - Als unmittelbare Folge davon entfÑllt der jetzt ÅberflÅssige Button
         "Info", wenn ein User im Nodelist-Browser mit <Enter> ausgewÑhlt
         wird.
       - Die bisherige Form der Ausgabe des Suchergebnisses einer Nodelist-
         Abfrage bei mehreren gefundenen EintrÑgen (Liste, aus der ein
         Eintrag mit <Enter> ausgewÑhlt werden mu·) wird jedoch weiterhin
         verwendet bei Fido/Request, Nachricht/Direkt und beim Anlegen eines
         Users. Unterhalb der Auswahlliste werden jetzt auch hier der Status
         des Users und der Name der Node-/Pointliste angezeigt, aus der der
         aktive Eintrag stammt.
       - Aus dem Nodelist-Browser heraus kann jetzt direkt per Hotkey "A"
         oder "D" unmittelbar eine neue Abfrage bzw. ein neues Durchsuchen
         der Node-/Pointlisten gestartet werden - unabhÑngig davon, welche
         Art der Suche vorher durchgefÅhrt wurde (daher kein Abbruch und
         Neustart der Routine fÅr neue Suche mehr notwendig, sondern direkter
         Wechsel zwischen den verschiedenen Arten der Suche mîglich). Wird
         der auf den Hotkey folgende Dialog mit <Esc> abgebrochen, bleibt das
         vorherige Suchergebnis in der Anzeige erhalten.
         Der Nodelist-Browser fungiert somit als "Shell" fÅr alle Formen der
         Suche in Node- und Pointlisten, sobald und solange er auf dem
         Bildschirm ist.
       - Das Durchsuchen der Node-/Pointlisten mit F/N/D ist jetzt (fast)
         Åberall in XP auch mit dem Hotkey <AltGr-N> erreichbar (z.B. aus dem
         Lister oder Editor heraus).
       - Im Eingabedialog fÅr das Durchsuchen von Node-/Pointlisten wird
         jetzt - wie auch seit jeher bei der Nodelist-Abfrage - rechts
         oben die Day-Number der Haupt-Nodeliste angezeigt.
       - Wird aus dem Nodelist-Browser heraus eine Nachricht oder ein
         File-Request erzeugt, wird bis zur Beendigung der Aktion der
         rekursive Aufruf des Nodelist-Browsers (z.B. aus dem Editor) mit
         <Alt-N> oder <AltGr-N> verhindert.
       - Eine erfolglose Suche fÅhrt nicht zu einer Fehlermeldung mit
         anschlie·endem Abbruch der Routine, sondern es wird der Nodelist-
         Browser mit einem entsprechenden Hinweis angezeigt. Dort kann dann
         unmittelbar mit "A" eine neue Nodelist-Abfrage oder mit "D" ein
         neues Durchsuchen ausgelîst (oder die gesamte Routine mit <Esc>
         abgebrochen) werden.
       - Box- bzw. Pointname sowie der Standort werden jetzt unterhalb der
         Auswahlliste nochmals in voller LÑnge angezeigt (im Suchergebnis
         werden sie auf zusammen 35 Zeichen gekÅrzt dargestellt).
       - Die Flags werden jetzt vollstÑndig (in 3 Zeilen † 63 Zeichen)
         angezeigt.
       - Eine eMail-Adresse wird nicht mehr aus den Flags herausoperiert und
         gesondert angezeigt, weil a) die Flags auch mehrere eMail-Adressen
         enthalten kînnen (und es deshalb willkÅrlich ist, wie bisher einfach
         die erste zu nehmen), b) es sich nicht um "anschreibbare" Adressen
         handelt, und c) aufgrund der vollstÑndigen Anzeige der Flags (s.o.)
         jetzt ohnehin *alle* Adressen angezeigt werden.
       - Wenn ein Durchsuchen der Node-/Pointlisten gestartet und mitten im
         Suchlauf durch <Esc> abgebrochen wird, ohne da· bereits ein Eintrag
         gefunden werden konnte, bricht XP die Routine nicht mehr komplett
         ab, sondern baut den Eingabedialog mit den aktuell eingegebenen
         Daten neu auf. Wird dann auch der Eingabedialog abgebrochen und
         handelt es sich nicht um den ersten Suchlauf, werden die beim
         vorherigen Suchlauf benutzten Daten restauriert (weil sich diese
         noch in der Anzeige des Nodelist-Browsers befinden), und XP kehrt in
         den Nodelist-Browser zurÅck.
         Waren zum Zeitpunkt des Suchlauf-Abbruchs mit <Esc> jedoch bereits
         EintrÑge gefunden worden, dann wird der Nodelist-Browser mit den
         gefundenen EintrÑgen neu aufgebaut.
       - Der zuletzt benutzte Suchbegriff der Nodelist-Abfrage mit <Alt-N>
         bzw. F/N/A bleibt fÅr die Dauer der XP-Session jetzt erhalten und
         wird bei der nÑchsten Abfrage wieder vorgeschlagen.
       - Fix: Wenn aus dem Nodelist-Browser heraus eine neue Suche gestartet
         oder ein Eintrag mit <Enter> ausgewÑhlt, der nachfolgende Dialog
         aber mit <Esc> abgebrochen wird, dann ist danach jetzt weiterhin der
         Eintrag in der Auswahlliste aktiv, der auch vor dem AusfÅhren der
         Aktion aktiv war. Bisher wurde der Cursorbalken immer auf den ersten
         Eintrag zurÅckgesetzt (unnîtiger Neustart des Listers beseitigt).
       - Da die Funktion, einen User im Nodelist-Browser mit <Enter> auswÑh-
         len und an diesen eine Mail oder einen Request erzeugen zu kînnen,
         jetzt auch in Situationen erreichbar ist, in denen sie bisher nicht
         erreichbar war (weil sie nur Åber das MenÅ F/N/D aufgerufen werden
         konnte), ist sie in bestimmten Situationen, in denen sie Probleme
         verursachen kînnte, aus SicherheitsgrÅnden deaktiviert (z.B. beim
         Editieren oder Weiterleiten einer Nachricht, beim Netcall u.Ñ.).
       - Fix: Eine Nodelist-Abfrage mit <Alt-N> bzw. F/N/A nach Fido-AKA gibt
         als Suchergebnis jetzt *alle* gefundenen EintrÑge in *allen* Listen
         zurÅck (z.B. bei mehreren Pointlisten wie POINTS24 und R24PNT, die
         Åberwiegend identische AKAs enthalten). Bisher wurde nur der Eintrag
         angezeigt, der zufÑllig als erster im Index vorkam.
       - Fix: Wenn auf einer Fido-Nachricht eines Points mit <Alt-N> eine
         Nodelist-Abfrage ausgelîst wurde, dann wurde nicht der Eintrag bzw.
         die EintrÑge des Points aus der oder den Pointliste(n), sondern nur
         der des Bossnodes aus der Nodeliste angezeigt (es ist nicht ganz
         klar, ob das ein Bug oder Absicht war). Es werden jetzt *alle* zur
         jeweiligen AKA gehîrenden EintrÑge des Bossnodes *und* des Points
         aus *allen* eingebundenen Listen angezeigt.
       - Fix: Beim Durchsuchen der Nodelisten mit F/N/D bzw. <AltGr-N> wird
         bei "Zone"-EintrÑgen jetzt die richtige Nodenummer "0" verwendet
         (bisher wurde als Nodenummer die Zonennummer eingesetzt, so da· z.B.
         fÅr den ZC2 die falsche Nodenummer "2:2/2" statt "2:2/0" angezeigt
         wurde und daher weder die Detailanzeige noch nachfolgende Aktionen
         mit diesem Eintrag funktionieren konnten).
       - Fix: "Zone"-EintrÑge in Points24-kompatiblen Listen werden beim
         Durchsuchen mit F/N/D bzw. <AltGr-N> ignoriert (Verhalten damit
         jetzt identisch mit der Nodelisten-Abfrage bei <Alt-N> bzw. F/N/A).
       - Anzeige-Fix: Der Pfeil nach unten, der anzeigt, da· in der Liste
         noch weitere EintrÑge folgen, wurde eine Zeile zu tief ausgegeben.
       - Fix: Tasten und Tastenkombinationen, die durch zwischenzeitliche
         Erweiterungen Im Nachrichten-Lister plîtzlich auch im Nodelist-
         Browser eine (unerwÅnschte) Wirkung hatten, sind jetzt deaktiviert
         (z.B. brach <Ctrl-W> den Nodelist-Browser ab). Es haben nur noch die
         Tasten eine Wirkung, die auch in der Hilfe zum Nodelist-Browser
         dokumentiert sind.

  MY:- Source-Header aktualisiert/korrigiert.

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

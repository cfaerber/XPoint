{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Overlay-Unit mit Editierroutinen u.a. }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp4e;

interface

uses  crt,dos,typeform,fileio,inout,keys,maske,datadef,database,winxp,
      win2,dosx,maus2,resource, xpglobal, xp0,xp1,xp1input,xp3;


const auto_empfsel_default : byte = 1;
      autoe_showscr        : boolean = false;


function  newuser:boolean;
function  modiuser(msgbrett:boolean):boolean;
function  newverteiler:boolean;
function  modiverteiler:boolean;
procedure editpass(msgbrett:boolean);

function  newbrett:boolean;
function  modibrett:boolean;
function  modibrett2:boolean;           { Zugriff }
procedure _multiedit(user:boolean);
procedure _multiloesch(user:boolean);

procedure EditTime(x,y:byte; txt:atext; var d:datetimest; var brk:boolean);

procedure ReadDirect(txt:atext; var empf,betr,box:string; pmonly:boolean;
                     var brk:boolean);
procedure msgdirect;
function  get_lesemode(var showtime:boolean):shortint;

procedure auto_new;
procedure auto_edit;
procedure auto_del;
procedure auto_active;
procedure auto_copy;
procedure auto_fileinfo;

procedure _AlphaBrettindex;
procedure MoveBretter;
procedure Bretttrennung;
procedure ChangePollbox;

procedure copy_address(var s:string);
procedure get_address(var s:string);
function  test_verteiler(var s:string):boolean;
function  usertest(var s:string):boolean;
function  writecode(var s:string):boolean;
function  testgruppe(var s:string):boolean;
function  empftest(var s:string):boolean;

procedure AutoFilename(var cr:CustomRec);
procedure auto_empfsel(var cr:CustomRec);                   {JG: gesplittet Teil 1}
procedure auto_empfsel_do(var cr:CustomRec;user:boolean);   {JG:            Teil 2}
procedure auto_usersel(var cr:CustomRec);                   {JG: suche nur nach Usern}
function  auto_testempf(var s:string):boolean;
procedure testwot(var s:string);
procedure auto_tagtest3(var s:string);
procedure testmon(var s:string);
function  AutoExistfile(var s:string):boolean;
procedure atestdate(var s:string);
function  atestpollbox(var s:string):boolean;
function  vtestpollbox(var s:string):boolean;
function  testpollbox(var s:string):boolean;    { nicht extern aufrufen! }
procedure dnotepollbox(var s:string);
function  dtestpollbox(var s:string):boolean;
procedure pb_wrntyp(var s:string);
function  tnotempty(var s:string):boolean;
function  brettaffe(var s:string):boolean;
procedure setbrett(var s:string);
function  testhaltetyp(var s:string):boolean;
function  mbshowtext(var s:string):boolean;
procedure mbshowtxt0(var s:string);
procedure mbsetvertreter(var s:string);


implementation  { --------------------------------------------------- }

uses  xp1o,xp1o2,xp2,xp3o,xp3o2,xpnt,xp4,xp6,xp9bp,xp9,xpcc,xpauto,xpfido;

var   adp         : ^atext;
      wcy         : byte;          { fÅr writecode() }
      grnr_found  : longint;       { von Testgruppe gefundene INT_NR }
      empfx,empfy : byte;          { msgdirect() -> empftest()       }
      _pmonly     : boolean;       {    "                            }
      adrfieldpos : integer;
      pb_netztyp  : byte;          { Netztyp von testpollbox() }
      ntyp_y      : byte;          { intern EditBrett          }
      brettfld    : integer;       { intern EditBrett          }
      userfld     : integer;       { intern EditUser           }
      pb_field    : integer;
      pbox        : string[BoxNameLen]; { intern EditBrett/ReadDirect }
      rdforcebox  : boolean;            { intern ReadDirect    }
      rdorgbox    : string[BoxNameLen]; { intern ReadDirect    }
      mbx,mby     : byte;          { Text fÅr modibrett2() }
      mblasttext  : shortint;


procedure addbox(var s:string);
var box : string[BoxNameLen];
begin
  box:=getfield(pb_field);
  s:=left(s,AdrLen-5-length(box))+'@'+box+ntAutoDomain(box,true);
end;

procedure FormFido(var s:string);  { lokal }
var fa   : FidoAdr;
    user : string[60];
begin
  SplitFido(s,fa,0);
  with fa do begin
    user:=username;
    if cpos('#',user)>0 then
      user:=left(s,cposx('@',s)-1);
    s:=user+'@'+iifs(zone<>0,strs(zone)+':','')+strs(net)+'/'+strs(node)+
       iifs(ispoint,'.'+strs(point),'');
    end;
end;

procedure copy_address(var s:string);
begin
  if cpos('@',s)=0 then
    AddBox(s)
  else
    if pb_netztyp=nt_Fido then
      FormFido(s);
  adp^:=s;
end;

procedure get_address(var s:string);
begin
  if s='' then s:=adp^;
  if (s<>'') and (cpos('@',s)=0) then
    AddBox(s)
  else
    if pb_netztyp=nt_Fido then
      FormFido(s);
end;

function TestFido(var s:string):boolean;
var fa : fidoadr;
    ni : nodeinfo;
begin
  if IsNodeaddress(s) then begin
    splitfido(s,fa,DefaultZone);
    fa.username:=MakeFidoAdr(fa,true);
    GetNodeinfo(fa.username,ni,1);
    end
  else begin
    fa.username:=s;
    getNodeUserInfo(fa,ni);
    end;
  if ni.found then
    s:=ni.sysop+'@'+MakeFidoAdr(fa,true);
  TestFido:=ni.found;
end;

function usertest(var s:string):boolean;
var p : byte;
begin
  if trim(s)='' then
    if fieldpos=adrfieldpos then
      usertest:=true
    else begin
      errsound;
      usertest:=false;
      end
  else begin
    usertest:=true;
    p:=cpos('@',s);
    if p>0 then
      if (pos('.',mid(s,p+1))=0) then
        s:=s+ntAutoDomain(pbox,false);
    if p>0 then
      s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1))
    else if (pb_netztyp=nt_fido) and nodeopen then
      usertest:=Testfido(s);
    end;
end;

function writecode(var s:string):boolean;
var cname : string[20];
begin
  attrtxt(col.coldialog);
  if (lstr(left(s,3))='pmc') and (ival(s[length(s)]) in [1..maxpmc]) then begin
    cname:=pmcrypt[ival(s[length(s)])].name;
    if cname='' then cname:=getres(2700);    { 'noch nicht definiert' }
    mwrt(39,wcy,forms('('+cname+')',30));
    end
  else
    mwrt(39,wcy,sp(30));
  writecode:=true;
end;

function testgruppe(var s:string):boolean;
var d : DB;
begin
  dbOpen(d,GruppenFile,1);
  dbSeek(d,giName,ustr(s));
  if not dbFound then rfehler(2701)   { 'unbekannte Brettgruppe - wÑhlen mit <F2>' }
  else dbRead(d,'INT_NR',grnr_found);
  dbClose(d);
  testgruppe:=dbFound;
end;


procedure pb_wrntyp(var s:string);
begin
  attrtxt(col.coldiahigh);
  if ntyp_y>0 then begin
    mwrt(49+length(getres2(2701,2)),ntyp_y,forms(ntName(pb_netztyp),12));
    freeres;
    end;
end;


procedure set_ubrett;
begin
  if brettfld>=0 then
    if ntGrossBrett(pb_netztyp) then set_chml(brettfld,'>')
    else if ntKleinBrett(pb_netztyp) then set_chml(brettfld,'<')
    else set_chml(brettfld,'');
  if userfld>=0 then
    if ntGrossUser(pb_netztyp) then set_chml(userfld,'>')
    else set_chml(userfld,'');
  if adrfieldpos>=0 then
    if ntGrossUser(pb_netztyp) then set_chml(adrfieldpos,'>')
    else set_chml(adrfieldpos,'');
end;


function testpollbox(var s:string):boolean;
var d : DB;
begin
  pbox:=s;
  if (aktdispmode=0) and (s='') then testpollbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then begin
      dbRead(d,'netztyp',pb_netztyp);
      dbRead(d,'boxname',s);
      pbox:=s;
      end;
    dbClose(d);
    if not dbFound then rfehler(2702);   { 'unbekannte Serverbox - wÑhlen mit <F2>' }
    testpollbox:=dbFound;
    if dbFound then begin
      pb_wrntyp(s);
      set_ubrett;
      end;
    end;
end;


procedure edituser(txt:atext; var user,adresse,komm,pollbox:string;
                   var halten:integer; var flags:byte; edit:boolean;
                   var brk:boolean);
var x,y  : byte;
    filt : boolean;
    uml  : boolean;
    ebs  : boolean;
begin
  new(adp);
  dialog(57,13,txt,x,y);
  maddstring(3,2,getres2(2701,1),pollbox,BoxRealLen,BoxRealLen,'>'); mhnr(423);
  pb_field:=fieldpos;                     { 'Server   ' }
  mappcustomsel(BoxSelProc,false);
  mset0proc(pb_wrntyp);
  msetvfunc(testpollbox);
  pb_netztyp:=ntBoxNetztyp(pollbox);
  maddtext(36,2,getres2(2701,2),0);       { 'Netztyp' }
  ntyp_y:=y+1;
  brettfld:=-1;
  if edit then begin
    maddtext(3,4,getres2(2701,3),col.coldialog);    { 'User     ' }
    maddtext(13,4,' '+left(user,41),col.coldiahigh);
    adp^:=user;
    userfld:=-1;
    end
  else begin
    maddstring(3,4,getres2(2701,3),user,40,eAdrLen,'');    { 'User     ' }
    msetvfunc(usertest); mset3proc(copy_address); mhnr(420);
    userfld:=fieldpos;
    end;
  maddstring(3,6,getres2(2701,4),adresse,40,eAdrLen,'');   { 'Adresse  ' }
    mhnr(421);
  adrfieldpos:=fieldpos;
  mappcustomsel(seluser,false);
  msetvfunc(usertest);
  msetprocs(get_address,get_address);
  set_ubrett;
  maddstring(3,8,getres2(2701,5),komm,30,30,''); mhnr(422);    { 'Kommentar' }
  uml:=(flags and 8=0);
  maddbool(3,10,getres2(2701,8),uml);      { 'Umlaute' }
  filt:=not odd(flags); mhnr(424);
  maddbool(3,11,getres2(2701,9),filt);   { 'Nachrichtenfilter' }
  ebs:=(flags and 16<>0);
  maddbool(3,12,getres2(2701,10),ebs);   { 'EmpfangsbestÑtigungen' }
  maddint(35,10,getres2(2701,6),halten,4,4,0,9999);   { 'Haltezeit' }
  maddtext(52,10,getres2(2701,7),col.coldialog);      { 'Tage'      }
  readmask(brk);
  if not brk then
    flags:=flags and $e6 + iif(filt,0,1) + iif(uml,0,8) + iif(ebs,16,0);
  enddialog;
  freeres;
  dispose(adp);
end;


function newuser:boolean;
var user,adresse : string[AdrLen];
    komm         : string[30];
    pollbox      : string[BoxNameLen];
    halten       : integer;
    b            : byte;
    brk          : boolean;
    flags        : byte;
begin
  user:=''; adresse:='';
  komm:=''; pollbox:=DefaultBox;
  halten:=stduhaltezeit;
  newuser:=false;
  flags:=1;  { neuer User <- Aufnehmen }
  edituser(getres(2702),user,adresse,komm,pollbox,halten,flags,false,brk);   { 'neuen User anlegen' }
  if not brk then begin
    dbSeek(ubase,uiName,ustr(user));
    if dbFound then
      rfehler(2703)    { 'Dieser User ist bereits vorhanden!' }
    else begin
      dbAppend(ubase);
      dbWrite(ubase,'username',user);
      if ustr(adresse)=ustr(user) then adresse:='';
      dbWriteX(ubase,'adresse',iif(adresse='',0,length(adresse)+1),adresse);
      dbWrite(ubase,'kommentar',komm);
      dbWrite(ubase,'pollbox',pollbox);
      dbWrite(ubase,'haltezeit',halten);
      dbWrite(ubase,'userflags',flags);
      b:=1;
      dbWrite(ubase,'adrbuch',b);
      dbWrite(ubase,'codierer',b);
      dbFlushClose(ubase);
      newuser:=true;
      end;
    end;
end;


function test_verteiler(var s:string):boolean;
begin
  s:=trim(s);
  if left(s,1)<>'[' then s:='['+s;
  if right(s,1)<>']' then s:=s+']';
  if length(s)<3 then begin
    errsound;
    test_verteiler:=false;
    end
  else begin
    s:=left(s,min(39,length(s)-1))+right(s,1);
    test_verteiler:=true;
    end;
end;

function vtestpollbox(var s:string):boolean;
var d : DB;
begin
  if s='' then vtestpollbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then dbRead(d,'boxname',s);
    dbClose(d);
    if not dbFound then rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
    vtestpollbox:=dbFound;
    end;
end;


procedure editverteiler(txt:atext; var name,komm,pollbox:string;
                        edit:boolean; var brk:boolean);
var x,y : byte;
begin
  dialog(57,7,txt,x,y);
  name:=vert_name(name);
  maddstring(3,2,getres2(2703,1),name,40,40,without(allchar,'@')); mhnr(610);
  msetvfunc(test_verteiler);                     { 'Name     ' }
  maddstring(3,4,getres2(2703,2),komm,30,30,''); mhnr(422);  { 'Kommentar' }
  maddstring(3,6,getres2(2703,3),pollbox,BoxRealLen,BoxRealLen,'>'); mhnr(612);
  mappcustomsel(BoxSelProc,false);               { 'Server   ' }
  freeres;
  msetvfunc(vtestpollbox);
  readmask(brk);
  if not brk then
    name:=vert_long(name);
  enddialog;
end;


function newverteiler:boolean;
var name    : string[AdrLen];
    komm    : string[30];
    pollbox : string[BoxNameLen];
    b       : byte;
    brk     : boolean;
begin
  name:='';
  komm:='';
  pollbox:='';
  newverteiler:=false;
  editverteiler(getres(2704),name,komm,pollbox,false,brk);  { 'neuen Verteiler anlegen' }
  if not brk then begin
    dbSeek(ubase,uiName,ustr(name));
    if dbFound then
      rfehler(2704)   { 'Dieser Verteiler ist bereits vorhanden!' }
    else begin
      dbAppend(ubase);
      dbWriteN(ubase,ub_username,name);
      dbWriteN(ubase,ub_kommentar,komm);
      dbWriteN(ubase,ub_pollbox,pollbox);
      b:=1;
      dbWriteN(ubase,ub_adrbuch,b);
      dbWriteN(ubase,ub_codierer,b);      { dÅrfte egal sein }
      b:=5;
      dbWriteN(ubase,ub_userflags,b);     { aufnehmen & Verteiler }
      dbFlushClose(ubase);
      newverteiler:=true;
      end;
    end;
end;


function modiverteiler:boolean;
var name,oldname : string[AdrLen];
    komm         : string[30];
    pollbox      : string[BoxNameLen];
    brk          : boolean;
    cc           : ccp;
    anz          : integer;
    rec          : longint;
begin
  modiverteiler:=false;
  dbReadN(ubase,ub_username,name);
  oldname:=name;
  dbReadN(ubase,ub_kommentar,komm);
  dbReadN(ubase,ub_pollbox,pollbox);
  editverteiler(getres(2705),name,komm,pollbox,true,brk);   { 'Verteiler bearbeiten' }
  if not stricmp(name,oldname) then begin
    rec:=dbRecno(ubase);
    dbSeek(ubase,uiName,ustr(name));
    if dbFound then begin
      rfehler(2704);   { 'Dieser Verteiler ist bereits vorhanden!' }
      brk:=true;
      end
    else
      dbGo(ubase,rec);
    end;
  if not brk then begin
    dbWriteN(ubase,ub_username,name);
    dbWriteN(ubase,ub_kommentar,komm);
    dbWriteN(ubase,ub_pollbox,pollbox);
    dbFlushClose(ubase);
    if name<>oldname then begin
      new(cc);
      oldname:=vert_name(oldname);
      name:=vert_name(name);
      read_verteiler(oldname,cc,anz);
      del_verteiler(oldname);
      write_verteiler(name,cc,anz);
      dispose(cc);
      aufbau:=true;
      end;
    modiverteiler:=true;
    end;
end;


function GetMsgBrettUser:boolean;
var hdp      : headerp;
    hds      : longint;
    suchname : string[AdrLen];

  procedure makeuser;
  var absender : string[AdrLen];
      pollbox  : string[BoxNameLen];
  begin
    dbReadN(mbase,mb_absender,absender);
    dbSeek(bbase,biIntnr,copy(dbReadStr(mbase,'brett'),2,4));
    if dbFound then       { mÅ·te IMMER true sein }
      dbReadN(bbase,bb_pollbox,pollbox)
    else
      pollbox:=DefaultBox;
    ReplaceVertreterbox(pollbox,true);
    xp3.makeuser(absender,pollbox);
  end;

begin
  GetMsgBrettUser:=true;
  if MarkUnversandt and (left(dbReadStr(mbase,'brett'),1)='U') then begin
    new(hdp);
    readheader(hdp^,hds,true);
    suchname:=hdp^.empfaenger;
    dispose(hdp);
    if left(suchname,length(TO_ID))=TO_ID then
      suchname:=mid(suchname,length(TO_ID)+1);
    end
  else
    dbReadN(mbase,mb_absender,suchname);
  dbSeek(ubase,uiName,ustr(suchname));
  if not dbFound then
    if ReadJN(getres(2709),true) then   { 'User nicht in der Datenbank - neu anlegen' }
      makeuser
    else
      GetMsgBrettUser:=false;
end;


procedure editpass(msgbrett:boolean);
var pw    : string;
    typ   : byte;
    cod   : string[5];
    name  : string[AdrLen];
    size  : smallword;
    x,y   : byte;
    brk   : boolean;
    adrb  : byte;
    i     : integer;
    defcode : boolean;
    flags   : byte;
    netztyp : byte;
    adr     : string[AdrLen];
    fa      : FidoAdr;
begin
  if msgbrett and not GetMsgBrettUser then
    exit;
  netztyp:=ntBoxNetztyp(dbReadStr(ubase,'pollbox'));
  if netztyp=nt_Fido then begin
    dbReadN(ubase,ub_username,adr);
    SplitFido(adr,fa,2);
(*  if fa.zone<=6 then begin
      message(getres(2737));  { 'Warnung: Nachrichtencodierung ist im FidoNet nicht zulÑssig!' }
      errsound;
      wkey(2,false);
      closebox;
      end; *)
    end;
  size:=0;
  dbReadX(ubase,'passwort',size,pw);
  if size=0 then pw:='';
  dbRead(ubase,'codierer',typ);
  if typ=9 then
    cod:='PGP'
  else if not ntBinary(netztyp) and (typ<3) then
    cod:='pmc-1'
  else
    case typ of
      0,1  : cod:='QPC';
      2    : cod:='DES';
      3..2+maxpmc : cod:='pmc-'+strs(typ-2);
    end;
  dbRead(ubase,'username',name);
  dialog(67,7,left(fuser(name),60),x,y);
  wcy:=y+3;
  maddstring(3,2,getres2(2706,1),pw,52,250,''); mhnr(480);   { 'Pa·wort ' }
  mnotrim;
  maddstring(3,4,getres2(2706,2),cod,5,5,'');   { 'Codier-Verfahren   ' }
  if ntBinary(netztyp) then
    mappsel(true,'QPC˘DES');
  mappsel(true,'PGP');
  for i:=1 to maxpmc do
    mappsel(true,'pmc-'+strs(i));
  mset1func(writecode);
  if writecode(cod) then;
  dbReadN(ubase,ub_userflags,flags);
  defcode:=(flags and 2<>0);
  maddbool(3,6,getres2(2706,3),defcode);   { 'Default: Codieren' }
  maskdontclear;
  readmask(brk);
  freeres;
  closemask;
  closebox;
  if not brk then begin
    dbWriteX(ubase,'passwort',iif(pw='',0,length(pw)+1),pw);
    if ustr(cod)='QPC' then typ:=1
    else if ustr(cod)='DES' then typ:=2
    else if ustr(cod)='PGP' then typ:=9
    else typ:=2+ival(right(cod,1));
    dbWrite(ubase,'codierer',typ);
    if pw<>'' then begin
      adrb:=1;
      dbWrite(ubase,'adrbuch',adrb);
      end;
    flags:=flags and (not 2)+iif(defcode,2,0);
    dbWriteN(ubase,ub_userflags,flags);
    dbFlushClose(ubase);
    end;
end;


function brettaffe(var s:string):boolean;
var x,y : byte;
begin
  if cpos('@',s)=0 then
    brettaffe:=true
  else begin
    msgbox(53,8,_fehler_,x,y);
    wrt(x+3,y+2,getres2(2707,1));   { '"@" ist in Brettnamen nicht erlaubt! Falls Sie' }
    wrt(x+3,y+3,getres2(2707,2));   { 'die /BRETT@BOX.ZER-Adressierung im Z-Netz ver-' }
    wrt(x+3,y+4,getres2(2707,3));   { 'wenden mîchten, legen Sie das Brett bitte als'  }
    wrt(x+3,y+5,getres2(2707,4));   { 'User an!'                                       }
    freeres;
    errsound;
    wkey(60,false);
    closebox;
    brettaffe:=false;
    end;
end;

procedure setbrett(var s:string);
begin
  if left(s,1)<>'/' then
    if (pb_netztyp<>nt_Fido) or (cpos('/',s)>0) then
      s:=left('/'+s,79)
    else begin
      ReadBoxPar(0,pbox);
      s:=left(BoxPar^.MagicBrett+s,79);
      end;
end;

function testhaltetyp(var s:string):boolean;
var tg,na : string[10];
begin
  if (length(s)=1) and (lastkey<>keybs) then begin
    tg:=ustr(getres2(2708,1));
    na:=ustr(getres2(2708,2));
    if upcase(s[1])=tg[1] then
      s:=getres2(2708,1)    { 'Tage' }
    else if upcase(s[1])=na[1] then
      s:=getres2(2708,2);   { 'Nachr.' }
    if length(s)>1 then _keyboard(keyend);
    end;
  testhaltetyp:=true;
end;


procedure editbrett(var brett,komm,box,origin:string; var gruppe:longint;
                    var halten:integer; var flags:byte; edit:boolean;
                    var brk:boolean);
var x,y    : byte;
    askloc : boolean;
    d      : DB;
    grname : string[30];
    trenn  : boolean;
    pba    : byte;
    filter : boolean;   { Nachrichtenfilter erlaubt }
    haltetyp:string[6];
    na,tg  : string[10];
    brtyp  : char;
    isfido : boolean;
begin
  dbOpen(d,gruppenfile,1);
  dbSeek(d,giIntnr,dbLongStr(gruppe));
  if not dbFound then dbGoTop(d);   { sollte nicht vorkommen! }
  dbRead(d,'name',grname);
  dbClose(d);
  askloc:=not edit or (left(brett,1)<>'$');
  trenn:=(left(brett,3)='$/T');
  filter:=(flags and 4=0);
  pb_netztyp:=ntBoxNetztyp(box);
  isfido:=(pb_netztyp=nt_Fido) and (left(brett,1)<>'$');
  dialog(57,iif(askloc or ParXX,iif(isfido,13,11),iif(trenn,5,iif(isfido,9,7))),
         getres2(2708,iif(edit,3,4)),x,y);   { 'Brett bearbeiten' / 'neues Brett anlegen' }
  userfld:=-1;
  adrfieldpos:=-1;
  brtyp:=brett[1];
  if not trenn then begin
    if askloc or ParXX then begin
      maddstring(3,2,getres2(2708,5),box,BoxRealLen,BoxRealLen,'>'); mhnr(402);
      mappcustomsel(BoxSelProc,false);       { 'Server    ' }
      msetvfunc(testpollbox);
      mset0proc(pb_wrntyp);
      maddtext(36,2,getres2(2708,6),0);      { 'Netztyp' }
      ntyp_y:=y+1;
      pba:=2;
      end
    else
      pba:=0;
    if not edit or (brtyp='A') then begin
      delete(brett,1,1);
      maddstring(3,2+pba,getres2(2708,7)+' ',brett,40,eBrettLen,''); mhnr(400);
      msetvfunc(notempty);                   { 'Brettname' }
      mset1func(brettaffe);
      mset3proc(setbrett);
      brettfld:=fieldpos;
      set_ubrett;
      end
    else begin
      maddtext(3,2+pba,getres2(2708,7),0);   { 'Brettname' }
      maddtext(14,2+pba,mid(brett,2),col.coldiahigh);
      brettfld:=-1;
      end;
    end
  else begin
    maddtext(3,2,getres2(2708,7),0);         { 'Brettname' }
    maddtext(14,2,getres2(2708,8),col.coldiahigh);  { 'Trennzeile' }
    pba:=2;
    end;

  maddstring(3,iif(trenn,2,4)+pba,getres2(2708,9),komm,30,30,''); mhnr(401);
  tg:=getres2(2708,1);
  na:=getres2(2708,2);
  if not trenn then begin                           { 'Kommentar ' }
    maddint(3,6+pba,getres2(2708,10),halten,4,4,0,9999); mhnr(403);  { 'Haltezeit ' }
    haltetyp:=iifs(odd(flags),na,tg);
    maddstring(22,6+pba,'',haltetyp,6,6,'');
    mappsel(true,na+'˘'+tg);
    mset1func(testhaltetyp);
    if brtyp='A' then begin
      maddbool(37,6+pba,getres2(2708,11),filter);     { 'Nachr.filter' }
      mhnr(405);
      end;
    if askloc then begin                              { 'Gruppe    ' }
      maddcustomsel(3,8+pba,getres2(2708,12),grname,30,GruppenSelProc);
      mhnr(406);
      end;
    if IsFido then begin
      maddstring(3,10+pba,getres2(2708,13),origin,40,54,range(' ',#126));
      mhnr(407);
      end;
    end;
  readmask(brk);
  closemask;
  closebox;
  freeres;
  if not brk then begin
    dbOpen(d,gruppenfile,1);
    dbSeek(d,giName,ustr(grname));
    dbRead(d,'Int_nr',gruppe);
    flags:=flags and (not (1+4+32)) + iif(filter,0,4) +
           iif(lstr(haltetyp)=lstr(tg),0,1) + iif(origin<>'',32,0);
    dbClose(d);
    end;
end;


function mbshowtext(var s:string):boolean;
var newstate : shortint;
    len,i    : byte;
    f1,f2    : string[1];
begin
  mbshowtext := true;
  if fieldpos=1 then f1:=trim(s) else f1:=trim(getfield(1));
  if fieldpos=2 then f2:=s else f2:=getfield(2);
  if f1='' then
    newstate:=iif(f2=_jn_[1],2,1)
  else
    newstate:=iif(f2=_jn_[1],3,iif(ntFollowup(pb_netztyp),5,4));
  if newstate<>mblasttext then begin
    mblasttext:=newstate;
    attrtxt(col.coldialog);
    len:=ival(getres2(2735,0))-3;
    for i:=0 to 2 do
      mwrt(mbx,mby+i,forms(getres2(2735,10*newstate+i),len));
    end;
end;

procedure mbshowtxt0(var s:string);
begin
  mbshowtext(s);
end;

procedure mbsetvertreter(var s:string);
begin
  if trim(s)<>'' then
    if (cpos('@',s)=0) and (left(s,1)<>'/') then
      s:='/'+s;
end;

function modibrett2:boolean;
var x,y,wdt  : byte;
    brk      : boolean;
    rec      : longint;
    adresse  : string[AdrLen];
    gesperrt : boolean;
    b        : byte;
begin
  modibrett2:=false;
  if left(dbReadStr(bbase,'brettname'),1)<'A' then begin
    rfehler(2712);
    exit;
    end;
  wdt:=ival(getres2(2735,0));
  diabox(wdt+2,11,getres2(2735,1),x,y);   { 'Brettzugriff/Brettvertreter Ñndern' }
  attrtxt(col.coldiarahmen);
  wrt(x,y+6,'√'+dup(wdt,'ƒ')+'¥');
  openmask(x+1,x+wdt,y+1,y+5,false);
  masksetfninfo(x+wdt-7,y+10,' [F2] ','ƒ');
  mbx:=x+3; mby:=y+7;
  mblasttext:=-1;
  rec:=dbRecno(bbase);
  if dbReadInt(bbase,'flags') and 32<>0 then adresse:=''
  else dbReadN(bbase,bb_adresse,adresse);
  gesperrt:=(dbReadInt(bbase,'flags')and 8<>0);
  pb_netztyp:=ntBoxNetztyp(dbReadStr(bbase,'pollbox'));
  maddstring(3,2,getres2(2735,2),adresse,36,eAdrLen,'');   { 'Vertreter-Adresse' }
  mappcustomsel(selbrett,false);  mhnr(860);
  mset1func(mbshowtext); mset0proc(mbshowtxt0);
  mset3proc(mbsetvertreter);
  maddbool(3,4,getres2(2735,3),gesperrt);    { 'Schreibsperre' }
  mset1func(mbshowtext);
  readmask(brk);
  closemask;
  closebox;
  freeres;
  dbGo(bbase,rec);
  if not brk then begin
    if { (ustr(adresse)=ustr(mid(dbReadStr(bbase,'brettname'),2))) or }
       (not gesperrt and ntFollowup(pb_netztyp) and (cpos('@',adresse)>0)) then
      adresse:='';
    dbWriteN(bbase,bb_adresse,adresse);
    b:=dbReadInt(bbase,'flags') and (not 8);
    if gesperrt then inc(b,8);
    if adresse<>'' then begin
      b:=b and (not 32);
      dbWriteN(bbase,bb_adresse,adresse);
      end;
    dbWriteN(bbase,bb_flags,b);
    dbFlushClose(bbase);
    modibrett2:=true;
    end;
end;

function modiuser(msgbrett:boolean):boolean;
var user,adresse : string[AdrLen];
    komm         : string[30];
    pollbox      : string[BoxNameLen];
    size         : smallword;
    halten       : integer;
    flags        : byte;
    brk          : boolean;
    rec          : longint;
begin
  modiuser:=false;
  if msgbrett and not GetMsgbrettUser then
    exit;
  dbRead(ubase,'username',user);
  if dbXsize(ubase,'adresse')=0 then adresse:=user
  else begin
    size:=0;
    dbReadX(ubase,'adresse',size,adresse);
    if adresse='' then adresse:=user;
    end;
  dbRead(ubase,'kommentar',komm);
  dbRead(ubase,'pollbox',pollbox);
  dbRead(ubase,'haltezeit',halten);
  dbRead(ubase,'userflags',flags);
  rec:=dbRecno(ubase);
  edituser(getres(2710),user,adresse,komm,pollbox,halten,flags,true,brk);
  dbGo(ubase,rec);
  if not brk then begin                 { 'User bearbeiten' }
    if ustr(adresse)=ustr(user) then adresse:='';
    dbWriteX(ubase,'adresse',iif(adresse='',0,length(adresse)+1),adresse);
    dbWrite(ubase,'kommentar',komm);
    dbWrite(ubase,'pollbox',pollbox);
    dbWrite(ubase,'haltezeit',halten);
    dbWrite(ubase,'userflags',flags);
    dbFlushClose(ubase);
    if msgbrett then
      dbFlushClose(ubase)
    else
      modiuser:=true;
    end;
end;


function newbrett:boolean;
var brett : string[brettLen];
    komm  : string[30];
    box   : string[BoxNameLen];
    origin: string[80];
    gruppe: longint;
    halten: integer;
    flags : byte;
    brk   : boolean;
    d     : DB;
begin
  newbrett:=false;
  brett:=''; komm:=''; box:=DefaultBox; origin:='';
  halten:=stdhaltezeit;
  gruppe:=NetzGruppe;
  flags:=0;
  editbrett(brett,komm,box,origin,gruppe,halten,flags,false,brk);
  if brk then exit;
  dbSeek(bbase,biBrett,'A'+ustr(brett));
  if dbFound then begin
    rfehler(2706);    { 'Dieses Brett gibt es bereits.' }
    exit;
    end;
  if (box='') and
     not ReadJN(getres(2711),true)   { 'Keine Box angegeben - internes Brett anlegen' }
     then exit;
  if box<>'' then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,ustr(box));
    dbClose(d);
    if not dbFound and
       not ReadJN(getres(2712),false)    { 'Unbekannte Serverbox - Brett trotzdem anlegen' }
       then exit;
    end;
  dbAppend(bbase);
  brett:='A'+brett;
  dbWriteN(bbase,bb_brettname,brett);
  dbWriteN(bbase,bb_kommentar,komm);
  dbWriteN(bbase,bb_pollbox,box);
  dbWriteN(bbase,bb_haltezeit,halten);
  dbWriteN(bbase,bb_gruppe,gruppe);
  if origin<>'' then
    dbWriteN(bbase,bb_adresse,origin);
  flags:=flags and (not 16);
  if ntBoxNetztyp(box)=nt_UUCP then inc(flags,16);
  dbWriteN(bbase,bb_flags,flags);
  SetBrettindex;
  newbrett:=true;
end;


function modibrett:boolean;
var brett  : string[BrettLen];
    komm   : string[30];
    box    : string[BoxNameLen];
    origin : string[60];
    oldorig: string[60];
    halten : integer;
    flags  : byte;
    brk    : boolean;
    gruppe : longint;
    modin  : boolean;
    _brett : string[5];
    x,y    : byte;
    n      : longint;
    mi     : shortint;
    rec    : longint;
label ende;
begin
  modibrett:=false;
  dbReadN(bbase,bb_brettname,brett);
  _brett:=mbrettd(brett[1],bbase);
  dbReadN(bbase,bb_kommentar,komm);
  dbReadN(bbase,bb_pollbox,box);
  dbReadN(bbase,bb_haltezeit,halten);
  dbReadN(bbase,bb_flags,flags);
  dbReadN(bbase,bb_gruppe,gruppe);
  if flags and 32=0 then origin:=''
  else dbReadN(bbase,bb_adresse,origin);
  oldorig:=origin;
  editbrett(brett,komm,box,origin,gruppe,halten,flags,true,brk);
  if not brk then begin
    dbWriteN(bbase,bb_kommentar,komm);
    dbWriteN(bbase,bb_pollbox,box);
    dbWriteN(bbase,bb_haltezeit,halten);
    flags:=flags and (not 16);
    if ntBoxNetztyp(box)=nt_UUCP then inc(flags,16);
    dbWriteN(bbase,bb_flags,flags);
    dbWriteN(bbase,bb_gruppe,gruppe);
    if (origin+oldorig)<>'' then
      dbWriteN(bbase,bb_adresse,origin);
    if left(brett,1)='/' then brett:='A'+brett;
    modin:=ustr(brett)<>ustr(dbReadStr(bbase,'brettname'));
    if modin then begin
      rec:=dbRecno(bbase);
      dbSeek(bbase,biBrett,ustr(brett));
      if dbFound then begin
        rfehler(2714);       { 'Umbennenen nicht mîglich - Brett existiert bereits!' }
        dbGo(bbase,rec);
        goto ende;
        end;
      dbGo(bbase,rec);
      mi:=dbGetIndex(mbase);
      dbSetIndex(mbase,miBrett);
      dbSeek(mbase,miBrett,_brett+#255);
      if dbEOF(mbase) then dbGoEnd(mbase)
      else dbSkip(mbase,-1);
      while not dbBOF(mbase) and (dbReadStr(mbase,'brett')=_brett) and
            (dbReadInt(mbase,'unversandt') and 8<>0) do
        dbSkip(mbase,-1);     { Wiedervorlage-Nachrichten Åberspringen }
      if not dbBOF(mbase) and (dbReadStr(mbase,'brett')=_brett) and
         odd(dbReadInt(mbase,'unversandt')) then begin
        rfehler(2711);    { 'Unversandte Nachrichten vorhanden - Brettname nicht Ñnderbar' }
        dbReadN(bbase,bb_brettname,brett);
        modin:=false;
        end;
      dbSetIndex(mbase,mi);
      end;
    dbWriteN(bbase,bb_brettname,brett);
    if modin then begin
      dbSeek(mbase,mb_brett,_brett);
      if not dbEOF(mbase) and (dbReadStr(mbase,'brett')=_brett) and
         ReadJN(getres(2713),true) then begin   { 'Brettname geÑndert - Nachrichtenkîpfe anpassen' }
        msgbox(32,3,'',x,y);
        wrt(x+2,y+1,getres(2714));   { 'Einen Moment bitte ...' }
        n:=0;
        while not dbEOF(mbase) and (dbReadStr(mbase,'brett')=_brett) do begin
          inc(n);
          gotoxy(x+24,y+1); write(n:4);
          NeuerEmpfaenger(mid(brett,2));
          dbNext(mbase);
          end;
        closebox;
        end;
      end;
  ende:
    modibrett:=true;
    dbFlushClose(bbase);
    end;
end;


procedure _multiedit(user:boolean);
const nn : shortint = 1;
var n,w    : shortint;
    x,y    : byte;
    brk    : boolean;
    s      : string[30];
    halten : integer;
    htyp   : string[6];
    hzahl  : boolean;
    grnr   : longint;
    i      : integer;
    d,dispdat : DB;
    vert   : boolean;
    brett  : string[BrettLen];
    umlaut : boolean;
    b      : byte;
    filter : boolean;
    flags  : byte;
    na,tg  : string[10];
    uucp   : byte;
    sperre : boolean;    { Brett - Schreibsperre }
begin
  if user then dispdat:=ubase
  else dispdat:=bbase;
  pushhp(iif(user,429,409));
  n:=MiniSel(34,10+(screenlines-25)div 2,'',getres2(2715,iif(user,1,2)),nn);
  if n<>0 then nn:=abs(n);       { ^Kommentar,^Serverbox,^Haltezeit,^Gruppe/^Umlaute,^Filter }
  pophp;
  case n of
    1   : w:=49;    { Kommentar }
    2,3 : w:=37;    { Pollbox, Haltezeit }
    4   : if user then w:=40
          else w:=46;   { Gruppe }
    5,6 : w:=37;
  else begin
    freeres;
    exit;
    end;
  end;
  dialog(w,3,getreps2(2715,iif(user,3,4),strs(bmarkanz)),x,y);   { '%s markierte User/Bretter bearbeiten' }
  dbGo(dispdat,bmarked^[0]);
  s:='';
  na:=getres2(2715,7);
  tg:=getres2(2715,8);
  case n of
    1 : begin
          maddstring(3,2,getres2(2715,5),s,30,30,'');   { 'Kommentar  ' }
          mhnr(iif(user,422,401));
        end;
    2 : begin
          maddstring(3,2,getres2(2715,6),s,BoxRealLen,BoxRealLen,'>');  { 'Server   ' }
          mappcustomsel(BoxSelProc,false);
          ntyp_y:=0;
          brettfld:=-1; userfld:=-1; adrfieldpos:=-1;
          msetvfunc(testpollbox);
          mhnr(iif(user,423,410));
        end;
    3 : begin
          halten:=dbReadInt(dispdat,'haltezeit');
          hzahl:=(dispdat=bbase);
          if hzahl then
            htyp:=iifs(odd(dbReadInt(bbase,'flags')),na,tg);
          maddint(3,2,getres2(2715,9),halten,4,4,0,9999); mhnr(iif(user,430,411));
          if hzahl then begin                 { 'Haltezeit: ' }
            maddstring(23,2,'',htyp,6,6,''); mhnr(411);
            mappsel(true,na+'˘'+tg);
            mset1func(testhaltetyp);
            end;
        end;
    4 : if user then begin
          umlaut:=true;
          maddbool(3,2,getres2(2715,10),umlaut);   { 'IBM-Umlaute verwenden' }
          end
        else begin
          dbReadN(bbase,bb_gruppe,grnr);
          dbOpen(d,GruppenFile,1);
          dbSeek(d,giIntnr,dbLongStr(grnr));
          dbRead(d,'name',s);
          dbClose(d);
          maddstring(3,2,getres2(2715,11),s,30,30,''); mhnr(406);  { 'Gruppe  ' }
          mappcustomsel(GruppenSelProc,false);
          msetvfunc(testgruppe);
          end;
    5 : begin
          filter:=not user;
          maddbool(3,2,getres2(2715,12),filter); mhnr(431);  { 'Nachrichtenfilter' }
        end;
    6 : begin
          dbGo(bbase,bmarked^[0]);
          sperre:=(dbReadInt(bbase,'flags')and 8<>0);
          maddbool(3,2,getres2(2715,13),sperre); mhnr(432);  { 'Schreibsperre' }
        end;
  end;
  readmask(brk);
  enddialog;
  if not brk then begin
    if n=2 then uucp:=iif(ntBoxNetztyp(s)=nt_UUCP,16,0);
    for i:=0 to bmarkanz-1 do begin
      dbGo(dispdat,bmarked^[i]);
      vert:=user and (dbReadInt(ubase,'userflags')and 4<>0);
      case n of
        1 : dbWrite(dispdat,'kommentar',s);
        2 : begin
              dbWrite(dispdat,'pollbox',s);
              if not user then begin
                dbRead(dispdat,'flags',flags);
                flags:=flags and (not 16)+uucp;
                dbWrite(dispdat,'flags',flags);
                end;
            end;
        3 : if not vert then begin
              dbWrite(dispdat,'haltezeit',halten);
              if hzahl then begin
                dbReadN(bbase,bb_flags,flags);
                flags:=flags and (not 1);
                if lstr(htyp)=lstr(na) then inc(flags);
                dbWriteN(bbase,bb_flags,flags);
                end;
              end;
        4 : if user then begin
              dbReadN(ubase,ub_userflags,b);
              b:=b and (not 8) + iif(umlaut,0,8);
              dbWriteN(ubase,ub_userflags,b);
              end
            else begin
              dbReadN(bbase,bb_brettname,brett);
              if left(brett,1)='A' then
                dbWriteN(bbase,bb_gruppe,grnr_found)
              else
                rfehler1(2707,copy(brett,2,26));   { '%s ist internes Brett - Gruppe nicht Ñnderbar!' }
              end;
        5 : if user then begin
              dbReadN(ubase,ub_userflags,flags);
              if filter then flags:=flags and $f2
              else flags:=flags or 1;
              dbWriteN(ubase,ub_userflags,flags);
              end
            else begin
              dbReadN(bbase,bb_flags,flags);
              if filter then flags:=flags and (not 4)
              else flags:=flags or 4;
              dbWriteN(bbase,bb_flags,flags);
              end;
        6 : begin
              dbReadN(bbase,bb_flags,flags);
              if sperre then flags:=flags or 8
              else flags:=flags and (not 8);
              dbWriteN(bbase,bb_flags,flags);
            end;
      end;
      end;
    aufbau:=true;
    end;
  freeres;
end;


procedure _multiloesch(user:boolean);
var i              : integer;
    brett          : string[90];
    _brett,_brett2 : string[5];
    dispdat        : DB;
begin
  if user then dispdat:=ubase
  else dispdat:=bbase;
  if ReadJN(getreps(iif(user,2716,2717),strs(bmarkanz)),false)   { '%s markierte User/Bretter lîschen' }
  then begin
    moment;
    i:=0;
    while (i<bmarkanz) do begin
      dbGo(dispdat,bmarked^[i]);
      if user and (dbReadInt(ubase,'userflags') and 4<>0) then
        dbSkip(dispdat,1)
      else begin
        if user then _brett:=mbrettd('U',ubase)
        else begin
          dbRead(bbase,'Brettname',brett);
          _brett:=mbrettd(brett[1],bbase);
          end;
        dbSeek(mbase,miBrett,_brett);
        if not dbEOF(mbase) then
          dbRead(mbase,'Brett',_brett2);
        if not dbEOF(mbase) and (_brett=_brett2) then begin
          if user then dbReadN(ubase,ub_username,brett)
          else brett:=mid(brett,2);
          rfehler1(2708,left(brett,50));   { 'Brett %s ist nicht leer.' }
          { i:=bmarkanz; }
          end
        else
          dbDelete(dispdat);
        end;
      inc(i);
      end;
    bmarkanz:=0;
    closebox;
    aufbau:=true;
    end;
end;


procedure EditTime(x,y:byte; txt:atext; var d:datetimest; var brk:boolean);
var width,height : byte;
begin
  width:=length(txt)+14; height:=3;
  if x=0 then getpos(width,height,x,y);
  blindon(true);
  attrtxt(col.coldiarahmen);
  forcecolor:=true;
  wpushs(x,x+width-1,y,y+height-1,'');
  forcecolor:=false;
  openmask(x+1,x+length(txt)+10,y+1,y+1,false);
  maskrahmen(0,0,0,0,0);
  maddtime(3,1,txt,d,false);
  readmask(brk);
  closemask;
  wpop;
  blindoff;
end;


function empftest(var s:string):boolean;
var ok    : boolean;
    brett : boolean;
    d     : DB;
    _pbox : string[BoxNameLen];
    oldpb : string[BoxNameLen];
    size  : smallword;
    p     : byte;
  function ShrinkEmpf(user,system:string):string;
  begin
    ShrinkEmpf:=left(user,eAdrLen-length(system)-1)+'@'+system;
  end;
begin
  ok:=true;
  oldpb:=pbox;
  brett:=(left(s,1)='/') and (cpos('@',s)=0);
  if trim(s)='' then
    exit
  else if brett and _pmonly then begin
    rfehler(2709);    { 'Direktnachricht an ein Brett ist NICHT mîglich' }
    ok:=false;
    end
  else
    if brett then begin
      if left(s,1)<>'/' then s:='/'+s;
      end
    else begin
      if ustr(s)='SYSOP' then
        s:=ShrinkEmpf(s,pbox+ntAutoDomain(pbox,true))
      else if (cpos('@',s)=0) or (cpos('@',s)=length(s)) then begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,ustr(s));
        if dbFound then begin
          dbRead(d,'Langname',s);
          dbRead(d,'pollbox',_pbox);
          if (_pbox<>'') and
             (not rdforcebox or
              not ntAdrCompatible(ntBoxNetztyp(_pbox),pb_netztyp))
          then begin
            if pb_field>0 then
              setfield(pb_field,_pbox);
            pbox:=_pbox;
            end;
          end
        else begin
          if (cpos('@',s)=0) and (pb_netztyp=nt_Fido) and nodeopen then
            if TestFido(s) then;
          if cpos('@',s)=0 then s:=s+'@';
          dbSeek(ubase,uiName,ustr(s));
          if not dbEOF(ubase) and
             (ustr(s)=ustr(left(dbReadStr(ubase,'username'),length(s)))) then
            dbReadN(ubase,ub_username,s)
          else
            if cpos('@',s)=length(s) then begin
              dellast(s);
              s:=ShrinkEmpf(s,pbox+ntAutoDomain(pbox,true));
              end;
          end;
        dbClose(d);
        end
      else if pos('.',mid(s,cpos('@',s)))=0 then
        s:=left(s+ntAutoDomain(pbox,false),eAdrLen);
      end;
  if ok then begin
    if cpos('@',s)=0 then dbSeek(bbase,biBrett,ustr(s))
    else dbSeek(ubase,uiName,ustr(s));
    attrtxt(iif(dbFound,col.coldialog,col.coldiahigh));
    wrt(empfx,empfy,getres2(2718,2));    { 'EmpfÑnger' }
    freeres;
    if dbFound then begin
      if cpos('@',s)=0 then
        dbReadN(bbase,bb_Pollbox,_pbox)
      else begin
        dbReadN(ubase,ub_pollbox,_pbox);
        size:=0;
        if dbXsize(ubase,'adresse')>0 then   { Vertreter }
          dbReadX(ubase,'adresse',size,s);
        end;
      if (_pbox<>'') and
         (not rdforcebox or
          not ntAdrCompatible(ntBoxNetztyp(_pbox),pb_netztyp))
      then begin
        pbox:=_pbox;
        if pb_field>0 then
          setfield(pb_field,pbox);
        end;
      end;
    end;
  if (pbox<>oldpb) and (pb_field>0) then begin
    pb_netztyp:=ntBoxNetztyp(pbox);
    pb_wrntyp(pbox);
    end;
  p:=cpos('@',s);
  if p>0 then
    s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
  empftest:=ok;
end;


procedure dnotepollbox(var s:string);
begin
  rdorgbox:=s;
end;

function dtestpollbox(var s:string):boolean;
var d  : DB;
    adr: string[AdrLen];
{    orgnt : byte; }
begin
{  orgnt:=ntBoxNetztyp(pbox); }
  pbox:=s;
  dbOpen(d,BoxenFile,1);
  SeekLeftBox(d,s);
  if dbFound then begin
    dbRead(d,'netztyp',pb_netztyp);
    dbRead(d,'boxname',s);
    pbox:=s;
    end;
  dbClose(d);
  if not dbfound then begin
    dbOpen(d,PseudoFile,1);
    dbSeek(d,piKurzname,ustr(s));
    if dbFound {and (pos('@',dbReadStr(d,'langname'))>0)} then begin
      dbRead(d,'pollbox',pbox);
      if (pbox='') or not IsBox(pbox) then
        pbox:=DefaultBox;
      s:=pbox;
      pb_netztyp:=ntBoxNetztyp(pbox);
      dbRead(d,'langname',adr);
      setfield(userfld,adr);
      _keyboard(keycr);
      end;
    dbClose(d);
    end;
  dtestpollbox:=dbfound;
  if dbfound then begin
    pb_wrntyp(s);
    set_ubrett;
    if not stricmp(s,rdorgbox) then
      rdforcebox:=true;   { Benutzer hat abweichenden Server gewÑhlt }
    end
  else
    rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
end;


procedure ReadDirect(txt:atext; var empf,betr,box:string; pmonly:boolean;
                     var brk:boolean);
var x,y : byte;
    pb  : boolean;
    pba : integer;
    d   : DB;
begin
  dbOpen(d,BoxenFile,0);
  pb:=(dbRecCount(d)>1);
  dbClose(d);
  pba:=iif(pb,2,0);
  dialog(58,5+pba,txt,x,y);
  box:=DefaultBox; pbox:=DefaultBox;
  empfx:=x+2; empfy:=y+1+pba;
  rdforcebox:=false;
  if pb then begin
    maddstring(3,2,getres2(2718,1),box,BoxRealLen,BoxRealLen,'>');
    mappcustomsel(BoxSelproc,false);     { 'Server    ' }
    mset0proc(dnotepollbox);
    msetvfunc(dtestpollbox);
    brettfld:=-1; userfld:=2; adrfieldpos:=-1;
    ntyp_y:=y+1;
    pb_field:=1;
    end
  else begin
    pb_field:=0;
    pb_Netztyp:=ntBoxNetztyp(pbox);
    end;
  maddstring(3,2+pba,getres2(2718,2),empf,40,eAdrLen,   { 'EmpfÑnger ' }
    iifs(ntGrossUser(ntBoxNetztyp(box)),'>',''));
{JG:05.02.00}
  mappcustomsel(auto_usersel,false);          
{/JG}
  msetvfunc(empftest);
  maddstring(3,4+pba,getres2(2718,3),betr,40,BetreffLen,'');  { 'Betreff   ' }
  _pmonly:=pmonly;
  freeres;
  readmask(brk);
  betr:=left(trim(betr),ntBetreffLen(pb_netztyp));
  box:=pbox;
  enddialog;
  if (empf='') or (not brk and (betr='') and not ReadJN(getres(618),false))
    then brk:=true;                     { 'Nachricht ohne Betreff absenden' }
end;

procedure msgdirect;
var brk  : boolean;
    empf : string[adrlen];
    betr : string[BetreffLen];
    real : string[40];
    box  : string[BoxNameLen];
    fn   : pathstr;
    headf: string[12];
    sigf : string[12];
    sdata: SendUUPtr;
begin
  empf:=''; betr:='';
  ReadDirect(getres(2719),empf,betr,box,true,brk);   { 'private Nachricht' }
  if brk then exit;
  fn:=TempS(2000);
  dbGo(mbase,0);    { -> Kennung fÅr dosend(), da· kein Brett-Reply }
  real:='';
  BriefSchablone(true,HeaderPriv,fn,empf,real);
  headf:='';
  sigf:=PrivSignat;
  if autocpgd then pgdown:=true;
  forcebox:=box;
  new(sdata);
  fillchar(sdata^,sizeof(sdata^),0);
  sdata^.empfrealname:=real;
  if DoSend(true,fn,empf,betr,true,false,true,false,true,sdata,headf,sigf,0)
  then;
  dispose(sdata);
  pgdown:=false;
  if exist(fn) then era(fn);
end;


function get_lesemode(var showtime:boolean):shortint;
var n   : shortint;
    d   : datetimest;
    brk : boolean;
    sich: string[20];
    x,y : byte;
begin
  get_lesemode:=-1;
  pushhp(50);
  sich:=iifs(readmode>=2,getres2(2720,2),'');    { ',^Sichern' }
  x:=iif(mauskey,40,20);
  y:=iif(mauskey,4,10+(screenlines-25)div 2);
  n:=MiniSel(x,y,'',getres2(2720,1)+sich,   { '^Alles,^Ungelesen,^Neues,^Heute,^Datum,^Zeit' }
             -(readmode+1));
  if (n>0) and ((readmode>=4) or (n<>readmode+1)) then begin
    showtime:=false;
    brk:=false;
    case n of
      3 : readdate:=NewDate;
      4 : readdate:=ixdat(left(Zdate,6)+'0000');
      5 : begin
            d:=fdat(longdat(readdate));
            EditDate(15,11+(screenlines-25)div 2,getres2(2720,3),d,brk);   { 'Lesen ab Datum:' }
            if not brk then readdate:=ixdat(copy(d,7,2)+copy(d,4,2)+copy(d,1,2)+'0000');
          end;
      6 : begin
            d:=ftime(longdat(readdate));
            EditTime(15,11+(screenlines-25)div 2,getres2(2720,4),d,brk);   { 'Lesen ab Uhrzeit:' }
            if not brk then begin
              readdate:=ixdat(left(longdat(readdate),6)+copy(d,1,2)+copy(d,4,2));
              showtime:=true;
              end;
          end;
      7 : begin
            write_lastcall(longdat(readdate));
            brk:=true;  { Readmode-Einstellung nicht Ñndern }
          end;
    end;
    if not brk then get_lesemode:=n-1;
    end;
  freeres;
  pophp;
end;


{ ----- automatischer Nachrichtenversand ----------------------------- }

{ const wotag = 'MoDiMiDoFrSaSo'; }


procedure AutoFilename(var cr:CustomRec);
var ps  : pathstr;
    dir : dirstr;
    name: namestr;
    ext : extstr;
begin
  selcol;
  fsplit(cr.s,dir,name,ext);
  if dir='' then dir:=SendPath
  else
    if cpos(':',dir)=0 then begin
      if left(dir,1)<>'\' then dir:='\'+dir;
      dir:=left(dospath(0),2)+dir;
      end;
  if right(dir,1)<>'\' then dir:=dir+'\';
  ps:=fsbox(screenlines div 2 - 5,dir+'*.*','',name+ext,true,false,false);
  cr.brk:=(ps='');
  if not cr.brk then cr.s:=ps;
end;

{ wird auch von XP6.EDIT_CC und XP9.ReadPseudo verwendet: }

{JG:05.02.00 Gesplittet in auto_empfsel und auto_empfsel_do    }
{            damit man nur User oder Brettfenster waehlen kann.}

procedure auto_empfsel_do (var cr:Customrec;user:boolean) ;
var p    : scrptr;
    mt   : boolean;    
begin
  with cr do begin        
    sichern(p);
    if autoe_showscr then showscreen(false);
    mt:=m2t;
    pushhp(1); select(iif(user,3,-1)); pophp;
    m2t:=mt;
    holen(p);
    brk:=(selpos=0);
    if not brk then
      if user then begin
        dbGo(ubase,selpos);
        dbReadN(ubase,ub_username,s);
        s:=vert_name(s);
        end
      else begin
        dbGo(bbase,selpos);
        dbReadN(bbase,bb_brettname,s);
        delete(s,1,1);
      end;
    end;
end;


procedure auto_empfsel(var cr:CustomRec);
var user : boolean;

begin
  with cr do begin
    user:=multipos('@',s) or (left(s,1)='[');
    if not user and (left(s,1)<>'/') then begin
      user:=(ReadIt(length(getres2(2721,2))+8,getres2(2721,1),getres2(2721,2),auto_empfsel_default,brk)=2);
      freeres;                        { 'EmpfÑnger:' / ' ^Brett , ^User ' }
      end
    else
      brk:=false;
    if not brk then auto_empfsel_do(cr,user)
    end;
end;


procedure auto_usersel(var cr:Customrec);               {Nur aus Userliste auswaehlen lassen}
begin
{JG:07.02.00  Verteiler bei Nachricht-Direkt verboten}
  with cr do begin
    auto_empfsel_do(cr,true);
    if (dbReadInt(ubase,'userflags') and 4<>0) then begin
      rfehler(301);   { 'bei Verteilern nicht mîglich' }
      brk:=True;
      end;
    end;
{/JG}
end;             

{/JG}

function auto_testempf(var s:string):boolean;
var p : byte;
begin
  p:=cpos('@',s);
  if (s<>'') and (p=0) and (left(s,1)<>'/') and (left(s,1)<>'[') then
    s:='/'+s
  else
    if p>0 then s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
  auto_testempf:=true;
end;

function wostring(wotage:byte):string;
var i   : integer;
    wot : string[23];
begin
  if wotage=127 then
    wostring:=getres(2723)     { 'tÑglich' }
  else begin
    wot:='';
    for i:=1 to 7 do
      if wotage and (1 shl (i-1))<>0 then
        wot:=wot+copy(_wotag_,2*i-1,2)+',';
    if wot<>'' then dellast(wot);
    wostring:=wot;
    end;
end;

function wobyte(wot:string):byte;
var i : integer;
    b : byte;
begin
  UpString(wot);
  if wot=ustr(getres(2723)) then     { 'TéGLICH' }
    wobyte:=127
  else begin
    b:=0;
    for i:=1 to 7 do
      if pos(ustr(copy(_wotag_,2*i-1,2)),wot)>0 then
        inc(b,1 shl (i-1));
    wobyte:=b;
    end;
end;

procedure testwot(var s:string);
begin
  s:=wostring(wobyte(s));
end;

function taglong(tage:string):longint;
var i,l : longint;
begin
  tage:=','+tage+',';
  l:=0;
  for i:=1 to 31 do
    if pos(','+strs(i)+',',tage)>0 then
      inc(l,1 shl (i-1));
  taglong:=l;
end;

function tagstring(l:longint):string;
var i : longint;
    s : string;
begin
  s:='';
  for i:=1 to 31 do
    if l and (1 shl (i-1))<>0 then
      s:=s+strs(i)+',';
  if s<>'' then dellast(s);
  tagstring:=s;
end;

procedure auto_tagtest3(var s:string);
begin
  s:=tagstring(taglong(s));
end;

function monword(s:string):word;
var i,w : word;
begin
  if (s='') or (ustr(s)=ustr(getres(2724))) then   { 'ALLE' }
    monword:=$fff
  else begin
    s:=','+s+',';
    w:=0;
    for i:=1 to 12 do
      if pos(','+strs(i)+',',s)>0 then
        inc(w,1 shl (i-1));
    monword:=w;
    end;
end;

function monstring(w:word):string;
var i : word;
    s : string[40];
begin
  if w=$fff then
    monstring:=getres(2724)      { 'alle' }
  else begin
    s:='';
    for i:=1 to 12 do
      if w and (1 shl (i-1))<>0 then
        s:=s+strs(i)+',';
    if s<>'' then dellast(s);
    monstring:=s;
    end;
end;

procedure testmon(var s:string);
begin
  s:=monstring(monword(s));
end;

function AutoExistfile(var s:string):boolean;
var fn : pathstr;
begin
  autoexistfile := false;              { MK 12/99 Zur Sicherheit }
  if s<>'' then begin
    fn:=s;
    adddir(fn,sendpath);
    if not exist(fn) then begin
      if ReadJN(getres(2725),true) then    { 'Datei nicht vorhanden - neu anlegen' }
        EditFile(fn,false,false,0,false);
      AutoExistfile:=exist(fn);
      end
    else
      AutoExistfile:=true;
    end;
end;

procedure atestdate(var s:string);
begin
  if smdl(ixdispdat(s),ixdat(left(zdate,6)+'0000')) then
    s:='  .  .  ';
end;


function atestpollbox(var s:string):boolean;
var d : DB;
begin
  if (s='') or (ustr(s)='*CRASH*') then atestpollbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then begin
      dbRead(d,'netztyp',pb_netztyp);
      dbRead(d,'boxname',s);
      pbox:=s;
      end
    else
      rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
    dbClose(d);
    atestpollbox:=dbFound;
    end;
end;

procedure AutoEdit(kopie:boolean; var ar:AutoRec; var brk:boolean);
var x,y    : byte;
    wot    : string[21];
    tg     : string[60];
    mon    : string[40];
    dat1,
    dat2   : DateTimeSt;
    bin    : boolean;
    loesch : boolean;
    modif  : boolean;
    nt     : byte;
    pm     : boolean;

  function dl(d:datetimest):longint;
  begin
    if d='  .  .  ' then
      dl:=0
    else
      dl:=ixdat(copy(d,7,2)+copy(d,4,2)+left(d,2)+'0000');
  end;

begin
  with ar do begin
    wot:=wostring(wotage);
    if datum1=0 then dat1:=''
    else dat1:=fdat(longdat(datum1));
    if datum2=0 then dat2:=''
    else dat2:=fdat(longdat(datum2));
    bin:=(UpCase(typ)='B');
    loesch:=(flags and 2<>0);
    modif:=(flags and 4<>0);
    tg:=tagstring(tage);
    mon:=monstring(monate);
    dialog(59,12,getres2(2726,iif(kopie,2,1)),x,y);   { 'AutoVersand-Nachricht (kopieren) }
    maddstring(3,2,getres2(2726,3),betreff,42,40,'');  mhnr(570);  { 'Betreff   ' }
    maddstring(3,3,getres2(2726,4),datei,42,80,'>');  { 'Datei     ' }
    malltrim;
    mappcustomsel(AutoFilename,false);
    msetvfunc(AutoExistfile);
    maddstring(3,4,getres2(2726,5),empf,42,eAdrLen,'');
    mappcustomsel(auto_empfsel,false);                { 'EmpfÑnger ' }
    msetvfunc(auto_testempf);
    maddstring(3,6,getres2(2726,6),box,17,17,'>');    { 'Server    ' }
    mappcustomsel(BoxSelProc,false);
    msetvfunc(atestpollbox);
    maddstring(3,8,getres2(2726,7),tg,17,60,'0123456789,');  { 'Tage      ' }
    mset3proc(auto_tagtest3);
    maddstring(3,10,getres2(2726,8),wot,17,20,'');    { 'Wochentage' }
    mappsel(false,getres2(2726,9));   { 'Mo˘Di˘Mi˘Do˘Fr˘Sa˘So' }
    mappsel(false,getres(2723));      { 'tÑglich' }
    mset3proc(testwot);
    maddstring(3,11,getres2(2726,10),mon,17,30,'0123456789,');  { 'Monate    ' }
    mappsel(false,getres(2724));   { 'alle' }
    mset3proc(testmon);
    maddbool  (39,6,getres2(2726,11),bin);          { 'binÑr' }
    maddbool  (39,7,getres2(2726,12),loesch);       { 'lîschen' }
    maddbool  (39,8,getres2(2726,13),modif);        { 'bei énderung' }
    madddate  (39,10,getres2(2726,14),dat1,false,true);   { 'Datum 1 ' }
    mset3proc(atestdate);
    madddate  (39,11,getres2(2726,15),dat2,false,true);   { 'Datum 2 ' }
    mset3proc(atestdate);
    readmask(brk);
    freeres;
    if not brk then begin
      betreff:=trim(betreff);
      wotage:=wobyte(wot);
      typ:=iifc(bin,'B','T');
      tage:=taglong(tg);
      monate:=monword(mon);
      datum1:=dl(dat1);
      datum2:=dl(dat2);
      flags:=flags and (not (2+4)) + iif(loesch,2,0) + iif(modif,4,0);
      pm:=multipos('@',empf);
      nt:=0;
      if box<>'' then
        nt:=ntBoxNetztyp(box)
      else
        if pm then begin
          dbSeek(ubase,uiName,ustr(empf));
          if dbFound then nt:=ntBoxNetztyp(dbReadStr(ubase,'pollbox'));
          end
        else begin
          dbSeek(bbase,biBrett,'A'+ustr(empf));
          if dbFound then nt:=ntBoxNetztyp(dbReadStr(bbase,'pollbox'));
          end;
      if nt=0 then nt:=ntBoxNetztyp(defaultbox);
      if (pm and ntGrossUser(nt)) or (not pm and ntGrossBrett(nt)) then
        UpString(empf);
      end;
    enddialog;
    end;
end;

procedure auto_new;
var ar  : autorec;
    brk : boolean;
begin
  fillchar(ar,sizeof(ar),0);
  with ar do begin
    typ:='T';
    monate:=$fff;
    flags:=1;
    AutoEdit(false,ar,brk);
    if not brk then begin
      dbAppend(auto);
      AutoWrite(ar);
      dbFlush(auto);
      aufbau:=true;
      end;
    end;
end;

procedure auto_edit;
var ar  : AutoRec;
    brk : boolean;
begin
  AutoRead(ar);
  AutoEdit(false,ar,brk);
  if not brk then begin
    AutoWrite(ar);
    dbFlush(auto);
    aufbau:=true;
    end;
end;

procedure auto_del;
var nr  : shortint;
    brk : boolean;
    fn  : pathstr;
    ar  : AutoRec;
begin
  AutoRead(ar);
  pushhp(77);
  nr:=ReadIt(31,getres(2727),getres(2728),   { 'GewÑhlten Eintrag lîschen?' / ' ^Ja , ^Nein , ^Datei ' }
             iif(right(ar.datei,4)='.MSG',3,1),brk);
  pophp;
  if (not brk) and (nr<>2) then begin
    if nr=3 then begin
      fn:=ar.datei;
      adddir(fn,sendpath);
      if not exist(fn) then begin
        rfehler(106);    { 'Datei nicht vorhanden!' }
        exit;
        end;
      _era(fn);
      end;
    dbDelete(auto);
    dbFlush(auto);
    aufbau:=true;
    end;
end;

procedure auto_active;
var flags : word;
begin
  dbRead(auto,'flags',flags);
  flags:=flags xor 1;
  dbWrite(auto,'flags',flags);
end;

procedure auto_copy;
var ar  : AutoRec;
    brk : boolean;
begin
  AutoRead(ar);
  AutoEdit(true,ar,brk);
  if not brk then begin
    dbAppend(auto);
    AutoWrite(ar);
    aufbau:=true;
    end;
end;

procedure auto_fileinfo;
var x,y : byte;
    ar  : AutoRec;
    fn  : pathstr;
    sr  : searchrec;
    dt  : DateTime;
begin
  AutoRead(ar);
  fn:=ar.datei;
  adddir(fn,sendpath);
  msgbox(minmax(length(fn)+14,35,70),8,getres2(2729,1),x,y);  { 'AutoVersand-Datei' }
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(2729,2));    { 'Datei:' }
  mwrt(x+3,y+3,getres2(2729,3));    { 'Grî·e:' }
  mwrt(x+3,y+4,getres2(2729,4));    { 'Datum:' }
  attrtxt(col.colmbox);
  mwrt(x+11,y+2,fitpath(fn,56));
  findfirst(fn,0,sr);
  moff;
  if doserror<>0 then
    wrt(x+11,y+3,getres2(2729,5))   { '- Datei fehlt -' }
  else begin
    wrt(x+11,y+3,trim(strsrnp(_filesize(fn),15,0))+getres(13));   { ' Bytes' }
{$IFNDEF WIN32}
    unpacktime(sr.time,dt);
{$ENDIF}
    gotoxy(x+11,y+4);
    with dt do
      write(formi(day,2),'.',formi(month,2),'.',year mod 100,', ',
            formi(hour,2),':',formi(min,2),':',formi(sec,2));
    end;
  wrt(x+3,y+6,getres(12));    { 'Taste drÅcken ...' }
  mon;
  freeres;
  wait(curon);
  closebox;
end;


{ --- Brettindex ----------------------------------------------------- }

procedure _AlphaBrettindex;
begin
  if ReadJN(getres(2730),true) then begin   { 'Bretter alphabetisch sortieren' }
    dbClose(bbase);
    AlphaBrettindex;  { xp3o }
    dbOpen(bbase,BrettFile,1);
    aufbau:=true;
    end;
end;


{ liefert neue Indexnummern fÅr Bretter, die an der aktuellen }
{ Position in bbase einzufÅgen sind; ggf. Index-Reorg         }

procedure GetIndexnr(anz:integer; var nr:longint; var step:integer);
var rec,_nr,
    _nr2     : longint;
    bi       : shortint;
begin
  rec:=dbRecno(bbase);
  bi:=dbGetIndex(bbase);
  dbReadN(bbase,bb_index,_nr);
  dbSetIndex(bbase,biIndex);
  dbSkip(bbase,-1);
  if dbBOF(bbase) then
    if _nr<200 then begin
      ReorgBrettindex;
      dbGo(bbase,rec);
      nr:=dbReadInt(bbase,'index')-anz*50;
      step:=50;
      end
    else begin
      step:=min(100,_nr div (anz+1));
      nr:=_nr-step*anz;
      end
  else begin
    dbReadN(bbase,bb_index,_nr2);
    if _nr-_nr2<anz+5 then begin
      ReorgBrettindex;
      step:=100 div (anz+1);
      nr:=dbReadInt(bbase,'index')+step;
      end
    else begin
      step:=(_nr-_nr2) div (anz+1);
      nr:=_nr2+step;
      end;
    end;
  dbSetIndex(bbase,bi);
  dbGo(bbase,rec);
  aufbau:=true;
end;


function tnotempty(var s:string):boolean;
begin
  if s='' then begin
    errsound;
    tnotempty:=false;
    end
  else
    tnotempty:=true;
end;

procedure Bretttrennung;
var x,y   : byte;
    brk   : boolean;
    oldtc : string[1];
    bi    : shortint;
    s     : string[AdrLen];
    rec   : longint;
    rec2  : longint;
    nr    : longint;
    step  : integer;
    komm  : string[30];
begin
  oldtc:=trennchar;
  dialog(50,5,getres2(2731,1),x,y);    { 'Trennzeile einfÅgen' }
  komm:='';
  maddstring(3,2,getres2(2731,2),trennchar,1,1,range(' ',#254)); mhnr(620);  
  mappsel(false,'ƒ˘Õ˘˘∞˘±˘Ø˘Æ˘˙˘˛');              { 'Trennzeichen ' }
  mnotrim;
  msetvfunc(tnotempty);
  maddstring(3,4,getres2(2731,3),komm,30,30,'');   { 'Kommentar    ' }
  readmask(brk);
  enddialog;
  if not brk then begin
    rec:=dbRecno(bbase);
    dbAppend(bbase);
    rec2:=dbRecno(bbase);
    s:='$/T'+trennchar;
    dbWriteN(bbase,bb_brettname,s);
    dbWriteN(bbase,bb_kommentar,komm);
    dbWriteN(bbase,bb_gruppe,LocGruppe);
    bi:=dbGetIndex(bbase);
    dbSetIndex(bbase,biBrett);
    dbGo(bbase,rec);
    GetIndexnr(1,nr,step);
    dbGo(bbase,rec2);
    dbWriteN(bbase,bb_index,nr);
    dbSetIndex(bbase,bi);
    aufbau:=true;
    if trennchar<>oldtc then
      SaveConfig;      { Trennzeichen merken }
    end;
end;


procedure MoveBretter;
var rec,nr : longint;
    step   : integer;
    bi     : shortint;
    f      : file of longint;
    l      : longint;
begin
  if (bmarkanz=0) or ReadJN(getreps(iif(bmarkanz=1,2732,2733),strs(bmarkanz)),true)
  then if bmarkanz>90 then          { '%s markierte Bretter verschieben' }
    rfehler(2710)   { 'Es kînnen maximal 90 Bretter gleichzeitig verschoben werden.' }
  else begin
    rec:=dbRecno(bbase);
    wlpos:=rec; wltrenn:=true;
    select(-1);
    if selpos>0 then begin
      dbGo(bbase,selpos);
      GetIndexnr(iif(bmarkanz=0,1,bmarkanz),nr,step);
      if bmarkanz=0 then begin
        dbGo(bbase,rec);
        dbWriteN(bbase,bb_index,nr);
        end
      else begin
        moment;
        bi:=dbGetIndex(bbase);
        dbSetIndex(bbase,biIndex);
        dbGoTop(bbase);
        assign(f,'b_index.$$$');
        rewrite(f);
        while not dbEOF(bbase) do begin
          l:=dbRecno(bbase);
          if ubmarked(l) then write(f,l);
          dbNext(bbase);
          end;
        seek(f,0);
        while not eof(f) do begin
          read(f,l);
          dbGo(bbase,l);
          dbWriteN(bbase,bb_index,nr);
          inc(nr,step);
          end;
        close(f); erase(f);
        dbSetIndex(bbase,bi);
        closebox;
        end;
      aufbau:=true;
      end;
    end;
end;


procedure ChangePollbox;
var oldbox,newbox   : string[BoxNameLen];
    user,bretter    : boolean;
    localuser       : boolean;
    autov,pseudos   : boolean;
    nn              : longint;
    x,y,i,uucp,flags: byte;
    brk             : boolean;
    d               : DB;
    mi,p            : shortint;
    mapsname        : string[40];
    anew,s           : string[50];
begin
  dialog(38,13,getres2(2734,1),x,y);    { 'Server-Wechsel' }
  oldbox:=''; newbox:='';
  user:=true; bretter:=true; localuser:=true;
  autov:=true; pseudos:=true;
  maddstring(3,2,getres2(2734,2),oldbox,BoxRealLen,BoxRealLen,'>'); mhnr(780);
  mappcustomsel(BoxSelProc,false);                { 'alte Serverbox ' }
  msetvfunc(notempty);
  maddstring(3,3,getres2(2734,3),newbox,BoxRealLen,BoxRealLen,'>');
  mappcustomsel(BoxSelProc,false);                { 'neue Serverbox ' }
  msetvfunc(vtestpollbox);
  maddbool(3,5,getres2(2734,4),bretter);          { 'Bretter bearbeiten' }
  maddbool(3,6,getres2(2734,5),user);             { 'User bearbeiten' }
  maddbool(3,7,getres2(2734,6),localuser);    { 'lokale User bearbeiten' }
  maddbool(3,8,getres2(2734,9),autov);        { 'AutoVersand bearbeiten' }
  maddbool(3,9,getres2(2734,10),pseudos);       { 'Kurznamen bearbeiten' }
  readmask(brk);
  closemask;
  if (newbox<>'') and not brk then begin
    UpString(oldbox);
    dbOpen(d,BoxenFile,1);                    { oldbox.Mapsname ermitteln }
    dbSeek(d,boiName,ustr(oldbox));
    if not dbFound then mapsname:=''
    else mapsname:=ustr(dbReadStr(d,'nameomaps')+'@'+oldbox);
    dbClose(d);
    if mapsname<>'' then mapsname:=mapsname+ustr(ntAutoDomain(oldbox,true));
    uucp:=iif(ntBoxNetztyp(newbox)=nt_UUCP,16,0);
    attrtxt(col.coldialog);
    wrt(x+2,y+10,getres2(2734,7));    { 'Bretter' }
    wrt(x+2,y+11,getres2(2734,8));    { 'User'    }
    wrt(x+19,y+10,getres2(2734,11));  { 'AutoVersand' }
    wrt(x+19,y+11,getres2(2734,12));  { 'Kurznamen' }
    for i:=1 to 2 do
      if ((i=1) and bretter) or ((i=2) and user) then begin
        if i=1 then d:=bbase
        else d:=ubase;
        mi:=dbGetIndex(d); dbSetIndex(d,0);
        dbGoTop(d);
        nn:=0;
        while not dbEOF(d) do begin
          if ustr(dbReadStr(d,'pollbox'))=oldbox then
            if (i=1) or
               (localuser and (ustr(dbReadStr(d,'username'))<>mapsname)) or
               (pos('@'+oldbox,ustr(dbReadStr(d,'username')))=0)
            then begin
              inc(nn);
              attrtxt(col.coldiahigh);
              wrt(x+10,y+9+i,strsn(nn,4));
              dbWrite(d,'pollbox',newbox);
              if i=1 then begin
                flags:=dbReadInt(d,'flags') and (not 16) + uucp;
                dbWrite(d,'flags',flags);
                end;
              end;
          dbNext(d);
          end;
        dbSetIndex(d,mi);
        end;
    for i:=1 to 2 do
      if ((i=1) and AutoV) or ((i=2) and pseudos) then begin
        if i=1 then dbOpen(d,AutoFile,0)
        else dbOpen(d,PseudoFile,0);
        nn:=0;
        while not dbEOF(d) do begin
          if ustr(dbReadStr(d,'pollbox'))=oldbox then begin
            inc(nn);
            attrtxt(col.coldiahigh);
            wrt(x+32,y+9+i,strsn(nn,4));
            dbWrite(d,'pollbox',newbox);
            end;
          dbNext(d);
          end;
        dbClose(d);
        end;
    if bretter and (ntBoxNetztyp(oldbox)=nt_Fido) then begin
      if ntBoxNetztyp(newbox)=nt_Fido then anew:=newbox
      else anew:='';
      dbOpen(d,GruppenFile,0);
      while not dbEOF(d) do begin
        p:=pos(oldbox,dbReadStr(d,'adresse'));
        if p>0 then begin
          if anew='' then s:=''
          else s:=left(dbReadStr(d,'adresse'),p-1)+anew+
                  mid(dbReadStr(d,'adresse'),p+length(oldbox));
          dbWrite(d,'adresse',s);
          end;
        dbNext(d);
        end;
      dbClose(d);
      end;

    if user or bretter or autov or pseudos then begin
      signal;
      wkey(2,false);
      end;
    aufbau:=true;
    end;
  closebox;
end;


end.


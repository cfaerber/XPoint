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

{ Overlay-Unit mit Editierroutinen u.a. }

{$I xpdefine.inc}

unit xp4e;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  sysutils,typeform,fileio,inout,keys,maske,datadef,database,winxp, xpheader,
  win2,maus2,resource,xpglobal,xp0,xp1,xp1input,xp3,fidoglob;


var   testmailstring_nt : byte; { Netztyp fuer Testmailstring }


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
procedure MoveUser;
procedure Bretttrennung;
procedure Usertrennung;
procedure ChangePollbox;

procedure copy_address(var s:string);
procedure get_address(var s:string);
function  test_verteiler(var s:string):boolean;
function  usertest(var s:string):boolean;
function  testmailstring(var s:string):boolean;
function  writecode(var s:string):boolean;
function  testgruppe(var s:string):boolean;
function  empftest(var s:string):boolean;

procedure edituser(txt:atext; var user,adresse,komm,pollbox:string;
                   var halten: Integer16; var adr:byte; var flags:byte; edit:boolean;
                   var brk:boolean);

procedure AutoFilename(var cr:CustomRec);
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
{procedure mbshowtxt0(var s:string);}
procedure mbsetvertreter(var s:string);
function testnoverteiler(var s:string):boolean; {Verteileradressen verboten}
procedure AddNewBrett(const Brettname, Kommentar, Pollbox: String; Haltezeit: Integer16;
  Gruppe: Integer; Flags: Byte);


implementation  { --------------------------------------------------- }

uses  xp1o,xp1o2,xp2,xp3o,xp3o2,xpnt,xp4,xpsendmessage,xp9bp,xpconfigedit,xpcc,xpauto,xpfido,xp5;

var   adp         : string;     { War ^atext (atext = s80, also shortstring) }
      wcy         : byte;       { fÅr writecode() }
      grnr_found  : longint;    { von Testgruppe gefundene INT_NR }
      empfx,empfy : byte;       { msgdirect() -> empftest()       }
      _pmonly     : boolean;    {    "                            }
      adrfieldpos : integer;
      pb_netztyp  : byte;       { Netztyp von testpollbox() }
      ntyp_x, ntyp_y: Integer;  { intern EditBrett          }
      brettfld    : integer;    { intern EditBrett          }
      userfld     : integer;    { intern EditUser           }
      pb_field    : integer;
      pbox        : string;     { intern EditBrett/ReadDirect }
      rdforcebox  : boolean;    { intern ReadDirect    }
      rdorgbox    : string;     { intern ReadDirect    }
      mbx,mby     : byte;       { Text fÅr modibrett2() }
      mblasttext  : shortint;


procedure addbox(var s:string);
var box : string;
begin
  box:=getfield(pb_field);
  s:=LeftStr(s,AdrLen-5-length(box))+'@'+box+ntAutoDomain(box,true);
end;

procedure FormFido(var s:string);  { lokal }
var fa   : FidoAdr;
    user : string;
begin
  SplitFido(s,fa,0);
  with fa do begin
    user:=username;
    if cpos('#',user)>0 then
      user:=LeftStr(s,cposx('@',s)-1);
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
  adp:=s;
end;

procedure get_address(var s:string);
begin
  if s='' then s:=adp;
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
      if (cPos('.',mid(s,p+1))=0) then
        s:=s+ntAutoDomain(pbox,false);
    if p>0 then
      s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1))
    else if (pb_netztyp=nt_fido) and Nodelist.Open then
      usertest:=Testfido(s);
    end;
end;

function testmailstring(var s:string):boolean;
var ok:boolean;
    st:string;
    i:byte;
begin
  st:=s;
  if testmailstring_nt in [nt_fido,nt_maus,255] then
  begin
    for i:=1 to length(st) do
    begin
      if (st[i]=' ') and (st[i+1]<>' ') then st[i]:='_';     { einzelnes Leerzeichen erlauben }
      if upcase(st[i]) in ['é','ô','ö','·',':'] then
        if ((testmailstring_nt=nt_fido) and (st[i]=':')) or  { Fido: ':'aber keine Umlaute }
           ((testmailstring_nt=nt_maus) and (st[i]<>':')) or { Maus: Umlaute aber kein ':' }
           (testmailstring_nt=255) then st[i]:='-';          { All:  Umlaute und ':' erlaubt. }
      end;
    end;
  ok:=(st=mailstring(st,false));
  if not ok then rfehler(2251);
  testmailstring:=ok;
end;

function writecode(var s:string):boolean;
var cname : string;
begin
  attrtxt(col.coldialog);
  if (LowerCase(LeftStr(s,3))='pmc') and
        ((length(s)>0) and (ival(LastChar(s)) in [1..maxpmc])) then begin
    cname:=pmcrypt[ival(LastChar(s))].name;
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
  dbSeek(d,giName,UpperCase(s));
  if not dbFound then rfehler(2701)   { 'unbekannte Brettgruppe - wÑhlen mit <F2>' }
  else dbRead(d,'INT_NR',grnr_found);
  dbClose(d);
  testgruppe:=dbFound;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }
procedure pb_wrntyp(var s:string);
begin
  attrtxt(col.coldiahigh);
  if ntyp_y>0 then begin
    mwrt(ntyp_x+36+length(getres2(2701,2)),ntyp_y,forms(ntName(pb_netztyp),12));
    freeres;
    end;
end;
{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

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
      s:= dbReadStr(d,'boxname');
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


Procedure Select_Gruppen;
var i      : byte;
    oldrec : longint;
begin
  oldrec:=dbrecno(ubase);
  i:=0;
  repeat
    inc(i);
    dbseek(ubase,uiAdrbuch,chr(i));
    if not dbEOF(ubase) then
    begin
      dbreadN(ubase,ub_adrbuch,i);
      mappsel(false,strs(i));
      end;
    until (i=99) or dbEOF(ubase);
    dbgo(ubase,oldrec);
end;


procedure edituser(txt:atext; var user,adresse,komm,pollbox:string;
                   var halten: Integer16; var adr:byte; var flags:byte; edit:boolean;
                   var brk:boolean);
var x,y: Integer;
    filt : boolean;
    uml  : boolean;
    ebs  : boolean;
    farb : byte;
    OldAdr: Byte;
begin
  OldAdr := adr;
  if LeftStr(user,4)<>#0+'$/T' then
  begin
    dialog(57,13,txt,x,y);
    maddstring(3,2,getres2(2701,1),pollbox,BoxRealLen,BoxNameLen,'>'); mhnr(423);
    pb_field:=fieldpos;                     { 'Server   ' }
    mappcustomsel(BoxSelProc,false);
    mset0proc(pb_wrntyp);
    msetvfunc(testpollbox);
    pb_netztyp:=ntBoxNetztyp(pollbox);
    maddtext(36,2,getres2(2701,2),0);       { 'Netztyp' }
    ntyp_x := x;
    ntyp_y:=y+1;
    brettfld:=-1;
    if edit then begin
      maddtext(3,4,getres2(2701,3),col.coldialog);    { 'User     ' }
      maddtext(13,4,' '+LeftStr(user,41),col.coldiahigh);
      adp:=user;
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
    farb:=(flags shr 5);
    if farb >2 then inc(farb);
    maddint(35,11,getres2(272,8),farb,2,2,0,5);       { ' Prioritaet ' }
    mhnr(8075);
    maddint(35,12,getres2(2701,11),adr,2,2,0,99);       { 'Adressbuchgruppe' }
    Select_Gruppen;
    mhnr(8069);
    end

  else begin { Trennzeile }
    dialog(57,7,txt,x,y);
    maddtext(3,2,getres2(2701,3),0);                   { 'User' }
    maddtext(14,2,getres2(2708,8),col.coldiahigh);     { 'Trennzeile' }
    maddstring(3,4,getres2(2708,9),komm,30,30,''); mhnr(401); { 'Kommentar' }
    maddint(3,6,getres2(2701,11),adr,2,2,1,99); mhnr(8069);  { 'Adressbuchgruppe' }
    end;

  readmask(brk);
  if not brk then
  begin
    if (OldAdr<>0) and (adr=0) then // Adr was changed from <> 0 to 0
     if not readJN(GetRes(2738),false) then
       adr:= OldAdr; 
    if farb=3 then Farb:=0;
    if farb>3 then dec(farb);
    flags:=(flags and not $E0) or (farb shl 5);
    flags:=flags and $e6 + iif(filt,0,1) + iif(uml,0,8) + iif(ebs,16,0);
    end;
  enddialog;
  freeres;
end;


function newuser:boolean;
var
    user,adresse : string;
    komm         : string;
    pollbox      : string;
    halten       : integer16;
    adr          : Byte;
    b            : byte;
    brk          : boolean;
    flags        : byte;
begin
  user:=''; adresse:='';
  komm:=''; pollbox:=DefaultBox;
  halten:=stduhaltezeit;
  newuser:=false;
  flags:=1;  adr:=1; { neuer User <- Aufnehmen }
  edituser(getres(2702),user,adresse,komm,pollbox,halten,adr,flags,false,brk);   { 'neuen User anlegen' }
  if not brk then begin
    dbSeek(ubase,uiName,UpperCase(user));
    if dbFound then
      rfehler(2703)    { 'Dieser User ist bereits vorhanden!' }
    else begin
      dbAppend(ubase);
      dbWriteNStr(ubase,ub_username,user);
      if UpperCase(adresse)=UpperCase(user) then adresse:='';
      dbWriteXStr(ubase,'adresse',iif(adresse='',0,length(adresse)+1),adresse);
      dbWriteNStr(ubase,ub_kommentar,komm);
      dbWriteNStr(ubase,ub_pollbox,pollbox);
      dbWriteN(ubase,ub_haltezeit,halten);
      dbWriteN(ubase,ub_userflags,flags);
      b:=1;
      dbWriteN(ubase,ub_adrbuch,adr);
      dbWriteN(ubase,ub_codierer,b);
      dbFlushClose(ubase);
      newuser:=true;
      end;
    end;
end;


function test_verteiler(var s:string):boolean;
begin
  s:=trim(s);
  if FirstChar(s)<>'[' then s:='['+s;
  if LastChar(s)<>']' then s:=s+']';
  if length(s)<3 then begin
    errsound;
    test_verteiler:=false;
    end
  else begin
    s:=LeftStr(s,min(39,length(s)-1))+LastChar(s);
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
    if dbFound then s := dbReadStr(d,'boxname');
    dbClose(d);
    if not dbFound then rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
    vtestpollbox:=dbFound;
    end;
end;


procedure editverteiler(txt:atext; var name,komm,pollbox:string; Var adr:integer16;
                        var brk:boolean);
var x,y: Integer;
begin
  dialog(57,7,txt,x,y);
  name:=vert_name(name);
  maddstring(3,2,getres2(2703,1),name,40,40,without(allchar,'@')); mhnr(610);
  msetvfunc(test_verteiler);                     { 'Name     ' }
  maddstring(3,4,getres2(2703,2),komm,30,30,''); mhnr(422);  { 'Kommentar' }
  maddstring(3,6,getres2(2703,3),pollbox,BoxRealLen,BoxNameLen,'>'); mhnr(612);
  mappcustomsel(BoxSelProc,false);               { 'Server   ' }
  msetvfunc(vtestpollbox);
  maddint(35,6,getres2(2701,11),adr,2,2,1,99); mhnr(8069);       { 'Adressbuchgruppe' }
  freeres;
  readmask(brk);
  if not brk then
    name:=vert_long(name);
  enddialog;
end;


function newverteiler:boolean;
var
    name         : string;
    komm         : string;
    pollbox      : string;
    b       : byte;
    brk     : boolean;
    adr     : integer16;
begin
  name:='';
  komm:='';
  pollbox:='';
  newverteiler:=false;
  adr:=1;
  editverteiler(getres(2704),name,komm,pollbox,adr,brk);  { 'neuen Verteiler anlegen' }
  if not brk then begin
    dbSeek(ubase,uiName,UpperCase(name));
    if dbFound then
      rfehler(2704)   { 'Dieser Verteiler ist bereits vorhanden!' }
    else 
    begin
      dbAppend(ubase);
      dbWriteNStr(ubase,ub_username,name);
      dbWriteNStr(ubase,ub_kommentar,komm);
      dbWriteNStr(ubase,ub_pollbox,pollbox);
      b:=1;
      dbWriteN(ubase,ub_adrbuch,adr); {NeuUserGruppe nicht fuer Verteiler...}
      dbWriteN(ubase,ub_codierer,b);      { dÅrfte egal sein }
      b:=5;
      dbWriteN(ubase,ub_userflags,b);     { aufnehmen & Verteiler }
      dbFlushClose(ubase);
      aufbau:=true;
      newverteiler:=true;
      end;
    end;
end;


function modiverteiler:boolean;
var
    name,oldname : string;
    komm         : string;
    pollbox      : string;
    brk          : boolean;
    cc           : ccp;         { String-Array }
    anz,adr      : integer16;
    rec          : longint;
begin
  modiverteiler:=false;
  name:= dbReadNStr(ubase,ub_username);
  oldname:=name;
  komm:= dbReadNStr(ubase,ub_kommentar);
  pollbox:= dbReadNStr(ubase,ub_pollbox);
  dbReadN(ubase,ub_adrbuch,adr);
  editverteiler(getres(2705),name,komm,pollbox,adr,brk);   { 'Verteiler bearbeiten' }
  if not stricmp(name,oldname) then begin
    rec:=dbRecno(ubase);
    dbSeek(ubase,uiName,UpperCase(name));
    if dbFound then begin
      rfehler(2704);   { 'Dieser Verteiler ist bereits vorhanden!' }
      brk:=true;
      end
    else
      dbGo(ubase,rec);
    end;
  if not brk then begin
    dbWriteNStr(ubase,ub_username,name);
    dbWriteNStr(ubase,ub_kommentar,komm);
    dbWriteNStr(ubase,ub_pollbox,pollbox);
    dbWriteN(ubase,ub_adrbuch,adr);
    dbFlushClose(ubase);
    if name<>oldname then begin
      new(cc);
      oldname:=vert_name(oldname);
      name:=vert_name(name);
      read_verteiler(oldname,cc,anz);
      del_verteiler(oldname);
      write_verteiler(name,cc,anz);
      dispose(cc);
      end;
    aufbau:=true;
    modiverteiler:=true;
    end;
end;


function GetMsgBrettUser:boolean;
var hdp      : Theader;
    hds      : longint;
    suchname : string;

  procedure makeuser;
  var absender : string;
      pollbox  : string;
  begin
    absender:= dbReadNStr(mbase,mb_absender);
    dbSeek(bbase,biIntnr,copy(dbReadStrN(mbase,mb_brett),2,4));
    if dbFound then       { mÅ·te IMMER true sein }
      pollbox:= dbReadNStr(bbase,bb_pollbox)
    else
      pollbox:=DefaultBox;
    ReplaceVertreterbox(pollbox,true);
    AddNewUser(absender,pollbox);
  end;

begin
  GetMsgBrettUser:=true;
  if MarkUnversandt and (FirstChar(dbReadStrN(mbase,mb_brett))='U') then begin
    hdp:= THeader.Create;
    readheader(hdp,hds,true);
    suchname:=hdp.FirstEmpfaenger;
    Hdp.Free;
    if LeftStr(suchname,length(TO_ID))=TO_ID then
      suchname:=mid(suchname,length(TO_ID)+1);
    end
  else
    suchname:= dbReadNStr(mbase,mb_absender);
  dbSeek(ubase,uiName,UpperCase(suchname));
  if not dbFound then
    if ReadJN(getres(2709),true) then   { 'User nicht in der Datenbank - neu anlegen' }
      makeuser
    else
      GetMsgBrettUser:=false;
end;


procedure editpass(msgbrett:boolean);
var pw    : string;
    typ   : byte;
    cod   : string;
    name  : string;
    adr   : string;
    size  : integer;
    x,y   : Integer;
    brk   : boolean;
    adrb  : byte;
    i     : integer;
    defcode : boolean;
    flags   : byte;
    netztyp : byte;
    fa      : FidoAdr;
begin
  if msgbrett and not GetMsgBrettUser then
    exit;
  netztyp:=ntBoxNetztyp(dbReadStrN(ubase,ub_pollbox));
  if netztyp=nt_Fido then begin
    adr:= dbReadNStr(ubase,ub_username);
    SplitFido(adr,fa,2);
{  if fa.zone<=6 then begin
      message(getres(2737)); } { 'Warnung: Nachrichtencodierung ist im FidoNet nicht zulÑssig!' }
{     errsound;
      wkey(2,false);
      closebox;
      end; }
    end;
  size:=0;
  pw:= dbReadXStr(ubase,'passwort',size);
  if size=0 then pw:='';
  dbReadN(ubase,ub_codierer,typ);
  if typ=9 then
    cod:='PGP'
  else if (typ=8) and ntMIME(netztyp) then
    cod:='PGP/MIME'
  else if not ntBinary(netztyp) and (typ<3) then
    cod:='pmc-1'
  else
    case typ of
      0,1  : cod:='QPC';
      2    : cod:='DES';
      3..2+maxpmc : cod:='pmc-'+strs(typ-2);
    end;
  name:= dbReadStrN(ubase,ub_username);
  dialog(67,7,LeftStr(fuser(name),60),x,y);
  wcy:=y+3;
  maddstring(3,2,getres2(2706,1),pw,52,250,''); mhnr(480);   { 'Pa·wort ' }
  mnotrim;
  maddstring(3,4,getres2(2706,2),cod,8,8,'');   { 'Codier-Verfahren   ' }
  if ntBinary(netztyp) then
    mappsel(true,'QPC˘DES');
  mappsel(true,'PGP');
  if ntMIME(netztyp) then
    mappsel(true,'PGP/MIME');
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
    dbWriteXStr(ubase,'passwort',iif(pw='',0,length(pw)+1),pw);
    if UpperCase(cod)='QPC' then typ:=1
    else if UpperCase(cod)='DES' then typ:=2
    else if UpperCase(cod)='PGP/MIME' then typ:=8
    else if UpperCase(cod)='PGP' then typ:=9
    else typ:=2+ival(LastChar(cod));
    dbWriteN(ubase,ub_codierer,typ);
    if pw<>'' then begin
      dbReadN(ubase,ub_adrbuch,adrb);
      if adrb=0 then adrb:=NeuUserGruppe;
      dbWriteN(ubase,ub_adrbuch,adrb);
      end;
    flags:=flags and (not 2)+iif(defcode,2,0);
    dbWriteN(ubase,ub_userflags,flags);
    dbFlushClose(ubase);
    end;
end;


function brettaffe(var s:string):boolean;
var x,y: Integer;
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
  if FirstChar(s)<>'/' then
    if (pb_netztyp<>nt_Fido) or (cpos('/',s)>0) then
      s:=LeftStr('/'+s,79)
    else begin
      ReadBoxPar(0,pbox);
      s:=LeftStr(BoxPar^.MagicBrett+s,79);
      end;
end;

function testhaltetyp(var s:string):boolean;
var tg,na : string;
begin
  if (length(s)=1) and (lastkey<>keybs) then begin
    tg:=UpperCase(getres2(2708,1));
    na:=UpperCase(getres2(2708,2));
    if upcase(s[1])=tg[1] then
      s:=getres2(2708,1)    { 'Tage' }
    else if upcase(s[1])=na[1] then
      s:=getres2(2708,2);   { 'Nachr.' }
    if length(s)>1 then _keyboard(keyend);
    end;
  testhaltetyp:=true;
end;


procedure editbrett(var brett,komm,box,origin:string; var gruppe:longint;
                    var halten:integer16; var flags:byte; edit:boolean;
                    var brk:boolean);
var x,y    : Integer;
    askloc : boolean;
    d      : DB;
    grname : string;
    haltetyp:string;
    na,tg  : string;
    trenn  : boolean;
    pba    : byte;
    filter : boolean;   { Nachrichtenfilter erlaubt }
    brtyp  : char;
    isfido : boolean;
begin
  dbOpen(d,gruppenfile,1);
  dbSeek(d,giIntnr,dbLongStr(gruppe));
  if not dbFound then dbGoTop(d);   { sollte nicht vorkommen! }
  grname:= dbReadStr(d,'name');
  dbClose(d);
  askloc:=not edit or (FirstChar(brett)<>'$');
  trenn:=(LeftStr(brett,3)='$/T');
  filter:=(flags and 4=0);
  pb_netztyp:=ntBoxNetztyp(box);
  isfido:=(pb_netztyp=nt_Fido) and (FirstChar(brett)<>'$');
  dialog(57,iif(askloc or ParXX,iif(isfido,13,11),iif(trenn,5,iif(isfido,9,7))),
         getres2(2708,iif(edit,3,4)),x,y);   { 'Brett bearbeiten' / 'neues Brett anlegen' }
  userfld:=-1;
  adrfieldpos:=-1;
  if Edit then
    brtyp:=brett[1]
  else
    brtyp := ' ';
  if not trenn then begin
    if askloc or ParXX then begin
      maddstring(3,2,getres2(2708,5),box,BoxRealLen,BoxNameLen,'>'); mhnr(402);
      mappcustomsel(BoxSelProc,false);       { 'Server    ' }
      msetvfunc(testpollbox);
      mset0proc(pb_wrntyp);
      maddtext(36,2,getres2(2708,6),0);      { 'Netztyp' }
      ntyp_x := x;
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
    dbSeek(d,giName,UpperCase(grname));
    dbRead(d,'Int_nr',gruppe);
    flags:=flags and (not (1+4+32)) + iif(filter,0,4) +
           iif(LowerCase(haltetyp)=LowerCase(tg),0,1) + iif(origin<>'',32,0);
    dbClose(d);
    end;
end;


function mbshowtext(var s:string):boolean;
var newstate : shortint;
    len,i    : byte;
    f1,f2    : string;
begin
  mbshowtext := true;
  if fieldpos=1 then f1:=trim(s)
  else f1:=trim(getfield(1));
  if fieldpos=2 then f2:=s
  else f2:=getfield(2);
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
    if (cpos('@',s)=0) and (FirstChar(s)<>'/') then
      s:='/'+s;
end;


function testnoverteiler(var s:string):boolean;
begin
  testnoverteiler:=true;
  if (FirstChar(s)='[') and (LastChar(s)=']') then
  begin
    rfehler(313);      { 'Verteiler sind hier nicht erlaubt!' }
    testnoverteiler:=false;
    end;
end;


function modibrett2:boolean;
var x,y,wdt  : Integer;
    brk      : boolean;
    rec      : longint;
    adresse  : string;
    gesperrt : boolean;
    b        : byte;
begin
  modibrett2:=false;
  if FirstChar(dbReadStrN(bbase,bb_brettname))<'A' then begin
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
  else adresse:= dbReadNStr(bbase,bb_adresse);
  gesperrt:=(dbReadInt(bbase,'flags')and 8<>0);
  pb_netztyp:=ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox));
  maddstring(3,2,getres2(2735,2),adresse,36,eAdrLen,'');   { 'Vertreter-Adresse' }
  mappcustomsel(auto_empfsel,false);  mhnr(860);
  msetvfunc(testnoverteiler);                       { nur Verteileradressen sind Ungueltig }
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
    if { (UpperCase(adresse)=UpperCase(mid(dbReadStrN(bbase,bb_brettname),2))) or }
       (not gesperrt and ntFollowup(pb_netztyp) and (cpos('@',adresse)>0)) then
      adresse:='';
    dbWriteNStr(bbase,bb_adresse,adresse);
    b:=dbReadInt(bbase,'flags') and (not 8);
    if gesperrt then inc(b,8);
    if adresse<>'' then begin
      b:=b and (not 32);
      { MAcht das Sinn, dass dieser Datensatz u.U. zum 2. Mal geschrieben wird?
        (hd/2000-07-11) }
      dbWriteNStr(bbase,bb_adresse,adresse);
      end;
    dbWriteN(bbase,bb_flags,b);
    dbFlushClose(bbase);
    modibrett2:=true;
    end;
end;

function modiuser(msgbrett:boolean):boolean; {us}
var
    user,adresse : string;
    komm         : string;
    pollbox      : string;
    size         : integer;
    halten       : integer16;
    adr          : byte;
    flags        : byte;
    brk          : boolean;
    rec          : longint;
begin
  modiuser:=false;
  if msgbrett and not GetMsgbrettUser then
    exit;
  user:= dbReadStrN(ubase,ub_username);
  if dbXsize(ubase,'adresse')=0 then adresse:=user
  else begin
    size:=0;
    adresse:= dbReadXStr(ubase,'adresse',size);
    if adresse='' then adresse:=user;
    end;
  komm:= dbReadStr(ubase,'kommentar');
  pollbox:= dbReadStrN(ubase,ub_pollbox);
  dbReadN(ubase,ub_haltezeit,halten);
  dbReadN(ubase,ub_userflags,flags);
  if Pollbox='' then adr:=NeuUsergruppe {Bei neuem User Standard-Adressbuchgruppe}
    else dbReadN(ubase,ub_adrbuch,adr);
  rec:=dbRecno(ubase);
  edituser(getres(2710),user,adresse,komm,pollbox,halten,adr,flags,true,brk);
  dbGo(ubase,rec);
  if not brk then begin                 { 'User bearbeiten' }
    if UpperCase(adresse)=UpperCase(user) then adresse:='';
    dbWriteXStr(ubase,'adresse',iif(adresse='',0,length(adresse)+1),adresse);
    dbWriteNStr(ubase,ub_kommentar,komm);
    dbWriteNStr(ubase,ub_pollbox,pollbox);
    dbWriteN(ubase,ub_haltezeit,halten);
    dbWriteN(ubase,ub_userflags,flags);
    dbWriteN(ubase,ub_adrbuch,adr);
    dbFlushClose(ubase);
    if msgbrett then
      dbFlushClose(ubase)
    else
      modiuser:=true;
    end;
  aufbau:=true;
end;

procedure AddNewBrett(const Brettname, Kommentar, Pollbox: String; Haltezeit: Integer16;
  Gruppe: Integer; Flags: Byte);
begin
  dbAppend(bbase);
  dbWriteNStr(bbase,bb_brettname, Brettname);
  dbWriteNStr(bbase,bb_kommentar, Kommentar);
  dbWriteNStr(bbase,bb_pollbox, Pollbox);
  dbWriteN(bbase,bb_haltezeit, haltezeit);
  dbWriteN(bbase,bb_gruppe, Gruppe);
  dbWriteN(bbase,bb_flags, Flags);
end;

function newbrett:boolean;
var
    brett : string;
    komm  : string;
    box   : string;
    origin: string;
    gruppe: longint;
    halten: integer16;
    flags : byte;
    brk   : boolean;
    d     : DB;
begin
  newbrett:=false;
  _UserAutoCreate:=true;
  brett:=''; komm:=''; box:=DefaultBox; origin:='';
  halten:=stdhaltezeit;
  gruppe:=NetzGruppe;
  flags:=0;
  editbrett(brett,komm,box,origin,gruppe,halten,flags,false,brk);
  if brk then exit;
  dbSeek(bbase,biBrett,'A'+UpperCase(brett));
  if dbFound then begin
    rfehler(2706);    { 'Dieses Brett gibt es bereits.' }
    exit;
    end;
  if (box='') and
     not ReadJN(getres(2711),true)   { 'Keine Box angegeben - internes Brett anlegen' }
     then exit;
  if box<>'' then 
  begin
    if not IsBox(Box) then 
      if not ReadJN(getres(2712),false)    { 'Unbekannte Serverbox - Brett trotzdem anlegen' }
       then exit;
    end;

  flags:=flags and (not 16);
  if ntBoxNetztyp(box) in netsRFC then inc(flags,16);
  AddNewBrett('A' + Brett, Komm, Box, Halten, Gruppe, Flags);
  if origin<>'' then
    dbWriteNStr(bbase,bb_adresse,origin);
  SetBrettindex;
  newbrett:=true;
end;


function modibrett:boolean;
var
    brett  : string;
    komm   : string;
    box    : string;
    origin : string;
    oldorig: string;
    _brett : string;
    halten : integer16;
    flags  : byte;
    brk    : boolean;
    gruppe : longint;
    modin  : boolean;
    x,y    : Integer;
    n      : longint;
    mi     : shortint;
    rec    : longint;
label ende;
begin
  modibrett:=false;
  brett:= dbReadNStr(bbase,bb_brettname);
  _brett:=mbrettd(brett[1],bbase);
  komm:= dbReadNStr(bbase,bb_kommentar);
  box:= dbReadNStr(bbase,bb_pollbox);
  dbReadN(bbase,bb_haltezeit,halten);
  dbReadN(bbase,bb_flags,flags);
  dbReadN(bbase,bb_gruppe,gruppe);
  if flags and 32=0 then origin:=''
  else origin:= dbReadNStr(bbase,bb_adresse);
  oldorig:=origin;
  editbrett(brett,komm,box,origin,gruppe,halten,flags,true,brk);
  if not brk then begin
    dbWriteNStr(bbase,bb_kommentar,komm);
    dbWriteNStr(bbase,bb_pollbox,box);
    dbWriteN(bbase,bb_haltezeit,halten);
    flags:=flags and (not 16);
    if ntBoxNetztyp(box) in netsRFC then inc(flags,16);
    dbWriteN(bbase,bb_flags,flags);
    dbWriteN(bbase,bb_gruppe,gruppe);
    if (origin+oldorig)<>'' then
      dbWriteNStr(bbase,bb_adresse,origin);
    if FirstChar(brett)='/' then brett:='A'+brett;
    modin:=UpperCase(brett)<>UpperCase(dbReadStrN(bbase,bb_brettname));
    if modin then begin
      rec:=dbRecno(bbase);
      dbSeek(bbase,biBrett,UpperCase(brett));
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
      while not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
            (dbReadInt(mbase,'unversandt') and 8<>0) do
        dbSkip(mbase,-1);     { Wiedervorlage-Nachrichten Åberspringen }
      if not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
         odd(dbReadInt(mbase,'unversandt')) then begin
        rfehler(2711);    { 'Unversandte Nachrichten vorhanden - Brettname nicht Ñnderbar' }
        brett:= dbReadNStr(bbase,bb_brettname);
        modin:=false;
        end;
      dbSetIndex(mbase,mi);
      end;
    dbWriteNStr(bbase,bb_brettname,brett);
    if modin then begin
      dbSeek(mbase,mb_brett,_brett);
      if not dbEOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
         ReadJN(getres(2713),true) then begin   { 'Brettname geÑndert - Nachrichtenkîpfe anpassen' }
        msgbox(32,3,'',x,y);
        wrt(x+2,y+1,getres(2714));   { 'Einen Moment bitte ...' }
        n:=0;
        while not dbEOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) do begin
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
    x,y    : Integer;
    brk    : boolean;
    s      : string;
    htyp   : string;
    brett  : string;
    na,tg  : string;
    halten,adr : integer16;
    hzahl  : boolean;
    grnr   : longint;
    i      : integer;
    d,dispdat : DB;
    vert   : boolean;
    umlaut : boolean;
    b      : byte;
    filter : boolean;
    flags  : byte;
    RFCNetFlag   : byte;
    sperre : boolean;    { Brett - Schreibsperre }
    oldbmarkanz : integer;

begin
  if user then dispdat:=ubase
  else dispdat:=bbase;
  pushhp(iif(user,429,409));
  n:=MiniSel(34,10+(screenlines-25)div 2,'',getres2(2715,iif(user,1,2)),nn);
  if n<>0 then nn:=abs(n);
  { ^Kommentar,^Serverbox,^Haltezeit,Umlaute,^Filter,^Gruppe^,^PrioritÑt,^Empfangsbest.,^Vertreteradr.,^Lîschen }
  if n<>0 then nn:=abs(n);
  pophp;
  case n of
    1,9 : w:=49;    { Kommentar }
    2,3 : w:=37;    { Pollbox, Haltezeit }
    4   : if user then w:=40
          else w:=46;   { Gruppe }
    5   : w:=37;
    6,7,8 : if user then w:=46
            else w:=37;

    10 : begin
           _multiloesch(user);
           freeres;
           aufbau:=true;
           exit;
           end;

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
          maddstring(3,2,getres2(2715,6),s,BoxRealLen,BoxNameLen,'>');  { 'Server   ' }
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
          if hzahl then begin                 { 'Haltezeit ' }
            maddstring(23,2,'',htyp,6,6,''); mhnr(411);
            mappsel(true,na+'˘'+tg);
            mset1func(testhaltetyp);
          end else
            maddtext(21,2,getres2(2715,8),col.coldialog);   { 'Tage' }
        end;
    4 : if user then begin
          umlaut:=true;
          maddbool(3,2,getres2(2715,10),umlaut);   { 'IBM-Umlaute verwenden' }
          end
        else begin
          dbReadN(bbase,bb_gruppe,grnr);
          dbOpen(d,GruppenFile,1);
          dbSeek(d,giIntnr,dbLongStr(grnr));
          s:= dbReadStr(d,'name');
          dbClose(d);
          maddstring(3,2,getres2(2715,11),s,30,30,''); mhnr(406);  { 'Gruppe  ' }
          mappcustomsel(GruppenSelProc,false);
          msetvfunc(testgruppe);
          end;
    5 : begin
          filter:=not user;
          maddbool(3,2,getres2(2715,12),filter); mhnr(431);  { 'Nachrichtenfilter' }
        end;
    6 : if not user then
        begin
          dbGo(bbase,bmarked^[0]);
          sperre:=(dbReadInt(bbase,'flags')and 8<>0);
          maddbool(3,2,getres2(2715,13),sperre); mhnr(432)  { 'Schreibsperre' }
          end
        else begin
          adr:=NeuUserGruppe;
          maddint(3,2,getres2(2701,11),adr,2,2,1,99); mhnr(8069);
          Select_Gruppen;
          end;
    7 : begin
          flags:=0;
          maddint(3,2,getres2(272,8),flags,2,2,0,5);       { ' Prioritaet ' }
          mhnr(8075);
          end;
    8 : begin
          filter:=false;
          maddbool(3,2,getres2(2701,10),filter);   { 'EmpfangsbestÑtigungen' }
          mhnr(426);
          end;
    9 : begin
          oldbmarkanz:=bmarkanz;
          maddstring(3,2,getres2(2701,4),s,30,eAdrLen,'');   { 'Adresse  ' }
          mhnr(421);
          mappcustomsel(seluser,false);
          {msetvfunc(usertest);}
          end;

  end;
  readmask(brk);
  enddialog;
  if not brk then
  begin
    if n=9 then bmarkanz:=oldbmarkanz;

    if n=7 then begin
      if flags=3 then Flags:=0;
      if flags>3 then dec(flags);
      end;
    if n=2 then rfcnetflag:=iif(ntBoxNetztyp(s) in [nt_UUCP,nt_Client],16,0);
    for i:=0 to bmarkanz-1 do begin
      dbGo(dispdat,bmarked^[i]);
      vert:=user and (dbReadInt(ubase,'userflags')and 4<>0);
      case n of
        1 : dbWriteStr(dispdat,'kommentar',s);
        2 : begin
              dbWriteStr(dispdat,'pollbox',s);
              if not user then begin
                dbRead(dispdat,'flags',flags);
                flags:=flags and (not 16)+RFCNetFlag;
                dbWrite(dispdat,'flags',flags);
                end;
            end;
        3 : if not vert then begin
              dbWrite(dispdat,'haltezeit',halten);
              if hzahl then begin
                dbReadN(bbase,bb_flags,flags);
                flags:=flags and (not 1);
                if LowerCase(htyp)=LowerCase(na) then inc(flags);
                dbWriteN(bbase,bb_flags,flags);
                end;
              end;
        4 : if user then begin
              dbReadN(ubase,ub_userflags,b);
              b:=b and (not 8) + iif(umlaut,0,8);
              dbWriteN(ubase,ub_userflags,b);
              end
            else begin
              brett:= dbReadNStr(bbase,bb_brettname);
              if FirstChar(brett)='A' then
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
        6 : if not user then
            begin
              dbReadN(bbase,bb_flags,flags);
              if sperre then flags:=flags or 8
              else flags:=flags and (not 8);
              dbWriteN(bbase,bb_flags,flags);
              end
            else begin
              dbwrite(dispdat,'adrbuch',adr);
              end;
        7 : if not vert then
            begin
              dbReadN(ubase,ub_userflags,b);
              b:=(b and not $E0) or (flags shl 5);
              dbWriteN(ubase,ub_userflags,b);
            end;

        8 : if not vert then
            begin
              dbreadN(ubase,ub_userflags,flags);
              if filter then flags:=flags or 16
                 else flags:=flags and (not 16);
              dbWriteN(ubase,ub_userflags,flags);
            end;

        9 : if not vert then dbWriteX(ubase,'adresse',iif(s='',0,length(s)+1),s);
      end;
    end;
    aufbau:=true;
  end;
  freeres;
end;


procedure _multiloesch(user:boolean);
var i              : integer;
    brett          : string;
    _brett,_brett2 : string;
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
          brett:= dbReadStrN(bbase,bb_brettname);
          _brett:=mbrettd(brett[1],bbase);
          end;
        dbSeek(mbase,miBrett,_brett);
        if not dbEOF(mbase) then
          _brett2:= dbReadStrN(mbase,mb_brett);
        if not dbEOF(mbase) and (_brett=_brett2) then begin
          if user then brett:= dbReadNStr(ubase,ub_username)
          else brett:=mid(brett,2);
          rfehler1(2708,LeftStr(brett,50));   { 'Brett %s ist nicht leer.' }
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

function empftest(var s:string):boolean;
var 
    brett : boolean;
    d     : DB;
    _pbox : string;
    oldpb : string;
    size  : integer;
    p     : byte;
    verteiler : boolean;
    newbrett  : boolean;

  function ShrinkEmpf(user,system:string):string;
  begin
    ShrinkEmpf:=LeftStr(user,eAdrLen-length(system)-1)+'@'+system;
  end;

begin
  result:=true;
  oldpb:=pbox;
  verteiler:=false;
  newbrett:=false;
  _UserAutoCreate:=true;

  brett:=(FirstChar(s)='/') and (cpos('@',s)=0);
  if trim(s)='' then
    exit
  else if brett and _pmonly then begin
    rfehler(2709);    { 'Direktnachricht an ein Brett ist NICHT mîglich' }
    result:=false;
    end
  else
    if brett then begin
      if FirstChar(s)<>'/' then s:='/'+s;
      end
    else if (FirstChar(s)='[') and (LastChar(s)=']') then verteiler:=true
    else begin
      if UpperCase(s)='SYSOP' then
        s:=ShrinkEmpf(s,pbox+ntAutoDomain(pbox,true))
      else if (cpos('@',s)=0) or (cpos('@',s)=length(s)) then begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,UpperCase(s));
        if dbFound then begin
          s:= dbReadStr(d,'Langname');
          _pbox:= dbReadStr(d,'pollbox');
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
          if (cpos('@',s)=0) and (pb_netztyp=nt_Fido) and Nodelist.Open then
            if TestFido(s) then;
          if cpos('@',s)=0 then s:=s+'@';
          dbSeek(ubase,uiName,UpperCase(s));
          if not dbEOF(ubase) and
             (UpperCase(s)=UpperCase(LeftStr(dbReadStrN(ubase,ub_username),length(s)))) then
            s:= dbReadNStr(ubase,ub_username)
          else
            if cpos('@',s)=length(s) then begin
              DeleteLastChar(s);
              s:=ShrinkEmpf(s,pbox+ntAutoDomain(pbox,true));
              end;
          end;
        dbClose(d);
        end
      else if cPos('.',mid(s,cpos('@',s)))=0 then
        s:=LeftStr(s+ntAutoDomain(pbox,false),eAdrLen);
      end;
  if result and not verteiler then begin
    if cpos('@',s)=0 then dbSeek(bbase,biBrett,'A'+mid(UpperCase(s),1)) {ohne "/"}
    else dbSeek(ubase,uiName,UpperCase(s));
    attrtxt(iif(dbFound,col.coldialog,col.coldiahigh));
    wrt(empfx,empfy,getres2(2718,2));    { 'EmpfÑnger' }
    freeres;

    if not dbFound and (cpos('@',s)=0) then  { Nicht Vorhandenes Brett }
    begin
      dbSeek(bbase,biBrett,'1'+mid(UpperCase(s),1));  { Internes PM-Brett ? }
      if dbfound or (s='/') then
      begin
        errsound;
        result:=false;
        end
      else result:=cc_testempf(s);
      if result then newbrett:=true;
      end;

    if dbfound or newbrett then
    begin
      if cpos('@',s)=0 then
        _pbox:= dbReadNStr(bbase,bb_Pollbox)
      else begin
        _pbox:= dbReadNStr(ubase,ub_pollbox);
        size:=0;
        if dbXsize(ubase,'adresse')>0 then   { Vertreter }
          s:= dbReadXStr(ubase,'adresse',size);
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
    s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1));
  testmailstring_nt:=pb_netztyp;
  if not verteiler then empftest:=result and testmailstring(s)
  else begin
    dbseek(ubase,uiname,UpperCase(vert_char+s+'@V')); { Nur existierende Verteiler sind erlaubt }
    empftest:=dbfound;
    if not dbfound then errsound;
    end;
end;


procedure dnotepollbox(var s:string);
begin
  rdorgbox:=s;
end;

function dtestpollbox(var s:string):boolean;
var d  : DB;
    adr: string;
{    orgnt : byte; }
begin
{  orgnt:=ntBoxNetztyp(pbox); }
  pbox:=s;
  dbOpen(d,BoxenFile,1);
  SeekLeftBox(d,s);
  if dbFound then begin
    dbRead(d,'netztyp',pb_netztyp);
    s:= dbReadStr(d,'boxname');
    pbox:=s;
    end;
  dbClose(d);
  if not dbfound then begin
    dbOpen(d,PseudoFile,1);
    dbSeek(d,piKurzname,UpperCase(s));
    if dbFound {and (cPos('@',dbReadStr(d,'langname'))>0)} then begin
      pbox:= dbReadStr(d,'pollbox');
      if (pbox='') or not IsBox(pbox) then
        pbox:=DefaultBox;
      s:=pbox;
      pb_netztyp:=ntBoxNetztyp(pbox);
      adr:= dbReadStr(d,'langname');
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
var x,y: Integer;
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
    maddstring(3,2,getres2(2718,1),box,BoxRealLen,BoxNameLen,'>');
    mappcustomsel(BoxSelproc,false);     { 'Server    ' }
    mset0proc(dnotepollbox);
    msetvfunc(dtestpollbox);
    brettfld:=-1; userfld:=2; adrfieldpos:=-1;
    ntyp_x := x;
    ntyp_y:=y+1;
    pb_field:=1;
    end
  else begin
    pb_field:=0;
    pb_Netztyp:=ntBoxNetztyp(pbox);
    end;
  maddstring(3,2+pba,getres2(2718,2),empf,40,eAdrLen,   { 'EmpfÑnger ' }
    iifs(ntGrossUser(ntBoxNetztyp(box)),'>',''));
  if pmonly then mappcustomsel(seluser,false)
  else mappcustomsel(auto_empfsel,false);
  msetvfunc(empftest);
  maddstring(3,4+pba,getres2(2718,3),betr,40,BetreffLen,'');  { 'Betreff   ' }
  _pmonly:=pmonly;
  freeres;
  sel_verteiler:=true;
  if pb then _keyboard(keydown);
  readmask(brk);
  sel_verteiler:=false;
  betr:=LeftStr(trim(betr),ntBetreffLen(pb_netztyp));
  box:=pbox;
  enddialog;
  if (empf='') or (not brk and (betr='') and not ReadJN(getres(618),false))
    then brk:=true;                     { 'Nachricht ohne Betreff absenden' }
  if (FirstChar(empf)='[') and (LastChar(empf)=']') then
    empf:=vert_char+empf+'@V';     { Verteilernamen anpassen }
end;

procedure msgdirect; // Nachricht/Direkt
var brk  : boolean;
    empf : string;
    betr : string;
    real : string;
    box  : string;
    headf: string;
    sigf : string;
    fn   : string;
    sdata: TSendUUData;
    pm   : boolean;
    d    : DB;
    grnr : longint;
begin
  empf:=''; betr:='';
  ReadDirect(getres(2719),empf,betr,box,false,brk);   { 'private Nachricht' }
  if brk then exit;
  fn:=TempS(2000);
  dbGo(mbase,0);    { -> Kennung fÅr dosend(), da· kein Brett-Reply }
  real:='';
  pm:=(cPos('@',empf)>0);
  if pm then
  begin                                            {User}
    BriefSchablone(true,HeaderPriv,fn,empf,real);
    headf:='';
    sigf:=PrivSignat;
    end
  else begin                                       {Brett}
    empf:='A'+empf;
    dbSeek(bbase,biBrett,UpperCase(empf));
    dbReadN(bbase,bb_gruppe,grnr);
    dbOpen(d,GruppenFile,1);
    dbSeek(d,giIntnr,dbLongStr(grnr));
    headf:=dbReadStr(d,'kopf')+extXps;
    sigf:=dbReadStr(d,'signatur')+extXps;
    dbclose(d);
    BriefSchablone(false,headf,fn,empf,real);
    headf:='';
    end;
  if autocpgd then pgdown:=true;
  forcebox:=box;
  sdata:= TSendUUData.Create;
// sdata.empfrealname:=real;
//  with sData.EmpfList.AddNew do begin
//    Address := empf;
//    RealName := real;
//  ed;
  with sData do begin
    sData.Empf1Address := empf;
    sData.Empf1RealName := real;
    DoSend(pm,fn,true,false,'',betr,true,false,true,false,true,sdata,sigf,0);
  end;
  sData.Free;
  pgdown:=false;
  if FileExists(fn) then DeleteFile(fn);
end;


function get_lesemode(var showtime:boolean):shortint;
var n   : shortint;
    d   : string;
    brk : boolean;
    getdate: boolean;
    sich: string;
    x,y: Integer;
begin
  get_lesemode:=-1;
  pushhp(50);
  sich:=iifs(readmode>=2,getres2(2720,2),'');    { ',^Sichern' }
  x:=iif(mauskey,40,20);
  y:=iif(mauskey,4,10+(screenlines-25)div 2);
  n:=MiniSel(x,y,'',getres2(2720,1)+sich,   { '^Alles,^Ungelesen,^Neues,^Heute,^Reorg.,^Datum/Zeit' }
             -(readmode+1));

  if (n>0) and ((readmode>=4) or (n<>readmode+1)) or
    (readmode = 3) then // allow to change readmode 'heute', see #500563
  begin
    showtime:=false;
    brk:=false;
    case n of
      3 : readdate:=NewDate;
      4 : readdate:=ixdat(LeftStr(Zdate,6)+'0000');
      5:  begin
            d:=reorgdate;
            if d<>'' then readdate:=ixdat(d);
            if rightstr(d,4)<>'0000' then showtime:=true;
          end;
      6 : begin
            d:=longdat(readdate);
            if (Aktdispmode>=10) and not empty
              then getdate:=true else getdate:=false;
            EditDate(15,11+(screenlines-25)div 2,getres2(2720,3),d,getdate,brk);   { 'Lesen ab Datum:' }
            if not brk then begin
              if getdate then d:=longdat(dbreadint(mbase,'empfdatum'));
              readdate:=ixdat(d);
              if rightstr(d,4)<>'0000' then showtime:=true;
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
var ps  : string;
    dir : string;
begin
  selcol;
  dir:= ExtractFilePath(cr.s);
  if dir='' then dir:= AddDirSepa(SendPath);
{$ifndef UnixFS}
  { Laufwerksbuchstaben hinzufuegen }
  dir := AddDirSepa(dir);
{$endif}
  ps:=fsbox(screenlines div 2 - 5,dir+WildCard,'',ExtractFileName(cr.s),true,false,false);
  cr.brk:=(ps='');
  if not cr.brk then cr.s:=ps;
end;


function auto_testempf(var s:string):boolean;
var p : byte;
begin
  p:=cpos('@',s);
  if (s<>'') and (p=0) and (FirstChar(s)<>'/') and (FirstChar(s)<>'[') then
    s:='/'+s
  else
    if p>0 then s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1));
  auto_testempf:=true;
end;


function wostring(wotage:byte):string;
var i   : integer;
    wot : string;
begin
  if wotage=127 then
    wostring:=getres(2723)     { 'tÑglich' }
  else begin
    wot:='';
    for i:=1 to 7 do
      if wotage and (1 shl (i-1))<>0 then
        wot:=wot+copy(_wotag_,2*i-1,2)+',';
    if wot<>'' then DeleteLastChar(wot);
    wostring:=wot;
    end;
end;

function wobyte(wot:string):byte;
var i : integer;
    b : byte;
begin
  UpString(wot);
  if wot=UpperCase(getres(2723)) then     { 'TéGLICH' }
    wobyte:=127
  else begin
    b:=0;
    for i:=1 to 7 do
      if pos(UpperCase(copy(_wotag_,2*i-1,2)),wot)>0 then
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
  if s<>'' then DeleteLastChar(s);
  tagstring:=s;
end;

procedure auto_tagtest3(var s:string);
begin
  s:=tagstring(taglong(s));
end;

function monword(s:string):word;
var i,w : word;
begin
  if (s='') or (UpperCase(s)=UpperCase(getres(2724))) then   { 'ALLE' }
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
    s : string;
begin
  if w=$fff then
    monstring:=getres(2724)      { 'alle' }
  else begin
    s:='';
    for i:=1 to 12 do
      if w and (1 shl (i-1))<>0 then
        s:=s+strs(i)+',';
    if s<>'' then DeleteLastChar(s);
    monstring:=s;
    end;
end;

procedure testmon(var s:string);
begin
  s:=monstring(monword(s));
end;

function AutoExistfile(var s:string):boolean;
var fn : string;
begin
  autoexistfile := true;
  if s<>'' then begin
    fn:=s;
    adddir(fn,sendpath);
    if not FileExists(fn) then begin
      if ReadJN(getres(2725),true) then    { 'Datei nicht vorhanden - neu anlegen' }
        EditFile(fn,false,false,false,0,false);
      AutoExistfile:=FileExists(fn);
      end
    else
      AutoExistfile:=true;
    end;
end;

procedure atestdate(var s:string);
begin
  if smdl(ixdispdat(s),ixdat(LeftStr(zdate,6)+'0000')) then
    s:='  .  .  ';
end;


function atestpollbox(var s:string):boolean;
var d : DB;
begin
  if (s='') or (UpperCase(s)='*CRASH*') then atestpollbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then begin
      dbRead(d,'netztyp',pb_netztyp);
      s:= dbReadStr(d,'boxname');
      pbox:=s;
      end
    else
      rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
    dbClose(d);
    atestpollbox:=dbFound;
    end;
end;

procedure AutoEdit(kopie:boolean; var ar:AutoRec; var brk:boolean);
var x,y: Integer;
    wot    : string;
    tg     : string;
    mon    : string;
    dat1,
    dat2   : string;
    bin    : boolean;
    loesch : boolean;
    modif  : boolean;
    supers : boolean;
    nt     : byte;
    pm     : boolean;

  function dl(d:datetimest):longint;
  begin
    if d='  .  .  ' then
      dl:=0
    else
      dl:=ixdat(copy(d,7,2)+copy(d,4,2)+LeftStr(d,2)+'0000');
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
    supers:=(flags and 8<>0);
    tg:=tagstring(tage);
    mon:=monstring(monate);
    dialog(59,13,getres2(2726,iif(kopie,2,1)),x,y);   { 'AutoVersand-Nachricht (kopieren) }
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
    maddstring(3,12,getres2(2726,10),mon,17,30,'0123456789,');  { 'Monate    ' }
    mappsel(false,getres(2724));   { 'alle' }
    mset3proc(testmon);
    maddbool  (39,6,getres2(2726,11),bin);          { 'binÑr' }
    maddbool  (39,7,getres2(2726,12),loesch);       { 'lîschen' }
    maddbool  (39,8,getres2(2726,13),modif);        { 'bei énderung' }
    maddbool  (39,9,getres2(2726,16),supers);       { 'ersetzen' }
    madddate  (39,11,getres2(2726,14),dat1,false,true);   { 'Datum 1 ' }
    mset3proc(atestdate);
    madddate  (39,12,getres2(2726,15),dat2,false,true);   { 'Datum 2 ' }
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
      flags:=flags and (not (2+4+8))
             + iif(loesch,2,0) + iif(modif,4,0) + iif(supers,8,0);
      pm:=multipos('@',empf);
      nt:=0;
      if box<>'' then
        nt:=ntBoxNetztyp(box)
      else
        if pm then begin
          dbSeek(ubase,uiName,UpperCase(empf));
          if dbFound then nt:=ntBoxNetztyp(dbReadStrN(ubase,ub_pollbox));
          end
        else begin
          dbSeek(bbase,biBrett,'A'+UpperCase(empf));
          if dbFound then nt:=ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox));
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
  fillchar(ar,sizeof(autorec),0);
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
    fn  : string;
    ar  : AutoRec;
begin
  AutoRead(ar);
  pushhp(77);
  nr:=ReadIt(31,getres(2727),getres(2728),   { 'GewÑhlten Eintrag lîschen?' / ' ^Ja , ^Nein , ^Datei ' }
             iif(RightStr(ar.datei,4)='.MSG',3,1),brk);
  pophp;
  if (not brk) and (nr<>2) then begin
    if nr=3 then begin
      fn:=ar.datei;
      adddir(fn,sendpath);
      if not FileExists(fn) then begin
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
var flags : smallword;
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
var x,y: Integer;
    ar  : AutoRec;
    fn  : string;
    sr  : tsearchrec;
    rc  : integer;
    dt  : TDateTime;
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
  rc:= findfirst(fn,faAnyFile,sr);
  moff;
  if rc<>0 then
    wrt(x+11,y+3,getres2(2729,5))   { '- Datei fehlt -' }
  else begin
    wrt(x+11,y+3,trim(strsrnp(_filesize(fn),15,0))+getres(13));   { ' Bytes' }
    dt := FileDateToDateTime(sr.time);
    gotoxy(x+11,y+4);
    write(DateToStr(dt), ', ', TimeToStr(dt));
  end;
  wrt(x+3,y+6,getres(12));    { 'Taste drÅcken ...' }
  mon;
  findclose(sr);
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
var x,y: Integer;
    brk   : boolean;
    oldtc : string;
    komm  : string;
    bi    : shortint;
    rec   : longint;
    rec2  : longint;
    nr    : longint;
    step  : integer;
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
  if not brk then 
  begin
    rec:=dbRecno(bbase);
    AddNewBrett('$/T'+trennchar, Komm,  '', 0, LocGruppe, 0);
    rec2:=dbRecno(bbase);
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


procedure Usertrennung;
var x,y: Integer;
    brk   : boolean;
    oldtc : string;
    s     : string;
    komm  : string;
    rec   : longint;
    rec2  : longint;
    ab    : integer;
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
    rec:=dbRecno(ubase);
    dbAppend(ubase);
    rec2:=dbRecno(ubase);
    s:=#0+'$/T'+trennchar;
    dbWriteNStr(ubase,ub_username,s);
    dbWriteNStr(ubase,ub_kommentar,komm);
    s:=#0;
    dbWriteNStr(ubase,ub_pollbox,s);
    dbGo(ubase,rec);
    dbreadN(ubase,ub_adrbuch,ab);
    dbgo(ubase,rec2);
    dbWriteN(ubase,ub_adrbuch,ab);
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

procedure MoveUser;
var rec    : longint;
    ab     : integer;
    i      : integer;
begin
  if (bmarkanz=0) or ReadJN(getreps(iif(bmarkanz=1,2732,2733),strs(bmarkanz)),true)
  then if bmarkanz>90 then          { '%s markierte Bretter verschieben' }
    rfehler(2710)   { 'Es kînnen maximal 90 Bretter gleichzeitig verschoben werden.' }
  else begin
    rec:=dbRecno(ubase);
    wlpos:=rec; wltrenn:=true;
    i:=bmarkanz;
    select(3);    { loescht bmarkanz!!! }
    bmarkanz:=i;
    if selpos>0 then begin
      dbGo(ubase,selpos);
      dbreadN(ubase,ub_adrbuch,ab);
      if bmarkanz=0 then begin
        dbGo(ubase,rec);
        dbWriteN(ubase,ub_adrbuch,ab);
        end
      else begin
        moment;
        for i:=0 to bmarkanz-1 do begin
          dbGo(ubase,bmarked^[i]);
          dbwriteN(ubase,ub_adrbuch,ab);
          end;
        closebox;
        end;
      aufbau:=true;
      end;
    end;
  end;

procedure ChangePollbox;
var
    oldbox,newbox   : string;
    mapsname        : string;
    anew,s          : string;
    user,bretter    : boolean;
    localuser       : boolean;
    autov,pseudos   : boolean;
    nn              : longint;
    x,y,i,RFCNetFlag,flags: Integer;
    brk             : boolean;
    d               : DB;
    mi,p            : shortint;
begin
  dialog(38,13,getres2(2734,1),x,y);    { 'Server-Wechsel' }
  oldbox:=''; newbox:='';
  user:=true; bretter:=true; localuser:=true;
  autov:=true; pseudos:=true;
  maddstring(3,2,getres2(2734,2),oldbox,BoxRealLen,BoxNameLen,'>'); mhnr(780);
  mappcustomsel(BoxSelProc,false);                { 'alte Serverbox ' }
  msetvfunc(notempty);
  maddstring(3,3,getres2(2734,3),newbox,BoxRealLen,BoxNameLen,'>');
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
    oldbox:= UpperCase(oldbox); {UpString(oldbox);}
    dbOpen(d,BoxenFile,1);                    { oldbox.Mapsname ermitteln }
    dbSeek(d,boiName,UpperCase(oldbox));
    if not dbFound then mapsname:=''
    else mapsname:=UpperCase(dbReadStr(d,'nameomaps')+'@'+oldbox);
    dbClose(d);
    if mapsname<>'' then mapsname:=mapsname+UpperCase(ntAutoDomain(oldbox,true));
    RFCNetFlag:=iif(ntBoxNetztyp(newbox) in netsRFC,16,0);
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
          if UpperCase(dbReadStr(d,'pollbox'))=oldbox then
            if (i=1) or
               (localuser and (UpperCase(dbReadStr(d,'username'))<>mapsname)) or
               (pos('@'+oldbox,UpperCase(dbReadStr(d,'username')))=0)
            then begin
              inc(nn);
              attrtxt(col.coldiahigh);
              wrt(x+10,y+9+i,strsn(nn,4));
              dbWriteStr(d,'pollbox',newbox);
              if i=1 then begin
                flags:=dbReadInt(d,'flags') and (not 16) + RFCNetFlag;
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
          if UpperCase(dbReadStr(d,'pollbox'))=oldbox then begin
            inc(nn);
            attrtxt(col.coldiahigh);
            wrt(x+32,y+9+i,strsn(nn,4));
            dbWriteStr(d,'pollbox',newbox);
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
          else s:=LeftStr(dbReadStr(d,'adresse'),p-1)+anew+
                  mid(dbReadStr(d,'adresse'),p+length(oldbox));
          dbWriteStr(d,'adresse',s);
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

{
  $Log$
  Revision 1.90  2002/04/18 21:14:19  mk
  - fixed Bug #500563

  Revision 1.89  2002/04/14 22:26:56  cl
  - changes for new address handling

  Revision 1.88  2002/01/28 20:32:25  mk
  - completed 3.40 merge, source is compilable for dos and win
    linux is still untested

  Revision 1.87  2002/01/22 19:15:29  mk
  - after 3.40 merge fixes

  Revision 1.86  2002/01/21 23:30:12  cl
  - post-3.40 merge fixes

  Revision 1.85  2002/01/13 15:15:52  mk
  - new "empfaenger"-handling

  Revision 1.84  2002/01/13 15:07:29  mk
  - Big 3.40 Update Part I

  Revision 1.83  2002/01/05 16:01:09  mk
  - changed TSendUUData from record to class

  Revision 1.82  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.81  2001/10/12 22:59:38  mk
  - fixed writing "Kommentar" in AddNewBrett

  Revision 1.80  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.79  2001/09/08 16:29:34  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.78  2001/09/08 14:31:02  cl
  - adaptions/fixes for MIME support
  - adaptions/fixes for PGP/MIME support

  Revision 1.77  2001/09/07 13:54:20  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.76  2001/09/07 10:56:00  mk
  - added GetServerFilename

  Revision 1.75  2001/09/07 09:17:56  mk
  - added AddNewBrett procedure

  Revision 1.74  2001/09/07 08:28:02  mk
  - added new procedure: AddNewBezug, collects three pieces of code

  Revision 1.73  2001/09/07 02:07:44  mk
  - use IsMailAddress when possilbe, removed duplicate code

  Revision 1.72  2001/09/05 23:17:30  mk
  - EditUser: renamed AdrbuchDef to OldAdr; OldAdr is now always valid

  Revision 1.71  2001/09/05 23:13:12  mk
  - corrected position of netname in EditUser if columncount > 80

  Revision 1.70  2001/09/05 22:59:02  mk
  - additional fix: adr in modiuser now also byte

  Revision 1.69  2001/09/05 22:55:13  mk
  - changed adr parameter in EditUser back to var

  Revision 1.68  2001/09/01 15:22:51  ma
  - net type handling fixes

  Revision 1.67  2001/08/27 09:13:43  ma
  - changes in net type handling (1)

  Revision 1.66  2001/08/23 11:15:03  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.65  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.64  2001/08/12 11:50:39  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.63  2001/08/11 23:06:32  mk
  - changed Pos() to cPos() when possible

  Revision 1.62  2001/07/29 12:58:16  ma
  - fixed setting of NNTP area db flags

  Revision 1.61  2001/07/28 12:04:12  mk
  - removed crt unit as much as possible

  Revision 1.60  2001/07/23 16:05:19  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.59  2001/06/04 17:36:50  ma
  - renamed old xp9 source files

  Revision 1.58  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.57  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.56  2001/01/06 21:13:35  mo
  - ƒnderung an TnodeListItem

  Revision 1.55  2000/12/27 22:36:35  mo
  -new class TfidoNodeList

  Revision 1.54  2000/12/05 14:58:09  mk
  - AddNewUser

  Revision 1.53  2000/12/03 12:38:22  mk
  - Header-Record is no an Object

  Revision 1.52  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.51  2000/11/16 21:31:06  hd
  - DOS Unit entfernt

  Revision 1.50  2000/11/15 23:00:41  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.49  2000/11/14 15:51:30  mk
  - replaced Exist() with FileExists()

  Revision 1.48  2000/11/14 11:14:32  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.47  2000/11/06 00:41:25  mk
  - fixed Bug #116657: crash with servername >15 chars

  Revision 1.46  2000/10/19 20:52:22  mk
  - removed Unit dosx.pas

  Revision 1.45  2000/10/17 10:05:50  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.44  2000/10/10 13:58:58  mk
  RB:- Ersetzt-Nachrichten in Autoversand

  Revision 1.43  2000/10/05 23:10:54  mk
  - Resource 2738 angelegt

  Revision 1.42  2000/10/03 16:02:11  mk
  JG:- Beim Editieren von Usern mit Adressbuchgruppe "0" bleibt
    diese erhalten und wird nicht in "1" geaendert"
  MK:- Bugfix fuer RangeCheck-Problem in EditUser

  Revision 1.41  2000/08/23 13:55:13  mk
  - Datenbankfunktionen mit Const-Parametern wo moeglich
  - dbReadX und Co auf 32 Bit angepasst

  Revision 1.40  2000/08/18 07:49:49  mk
  JG: - beim Neuanlegen von Usern mit Strg+U wird jetzt
    die Standard-Addressbuchgruppe benutzt

  Revision 1.39  2000/08/14 14:43:26  mk
  - Kommentar hinzugefuegt

  Revision 1.38  2000/08/08 13:18:15  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.37  2000/07/21 20:56:25  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.36  2000/07/21 18:57:52  mk
  - weiteren Zugriff auf nicht initialisierten String beseitigt

  Revision 1.35  2000/07/21 18:48:34  mk
  - Zugriff auf nicht initialisierten String beseitigt

  Revision 1.34  2000/07/13 16:44:58  mk
  JG: - User-Editmenu: einstellbare Prioritaetsfarbe fuer Msgs des User

  Revision 1.33  2000/07/11 21:39:21  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.32  2000/07/11 14:59:30  hd
  - Ansistring
  - Ein paar Linux-Anpassungen

  Revision 1.31  2000/07/09 08:35:16  mk
  - AnsiStrings Updates

  Revision 1.30  2000/07/05 13:55:01  hd
  - AnsiString

  Revision 1.29  2000/07/05 12:47:27  hd
  - AnsiString

  Revision 1.28  2000/07/04 12:04:23  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.27  2000/07/03 13:31:40  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.26  2000/06/29 13:00:56  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.25  2000/05/13 18:23:52  jg
  - Nachricht/Direkt + Weiterleiten..Direkt:
    Bretter sind jetzt sowohl bei Direkteingabe (mit einleitendem "/")
    als auch per F2-Auswahl erlaubt.

  Revision 1.24  2000/05/13 09:14:40  jg
  - Ueberpruefung der Adresseingaben jetzt auch Fido und Maus kompatibel

  Revision 1.23  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.22  2000/05/01 17:49:07  jg
  - Bugfix Verteiler. (Merken: Variablen initialisieren spart Telefonkosten)

  Revision 1.21  2000/05/01 17:26:33  jg
  - Verteiler als Empfaenger bei Nachricht/Direkt;  Nachricht/Weiterleiten
    Und Sendefenster-Empfaengeraendern erlaubt

  Revision 1.20  2000/05/01 08:40:57  jg
  - Addressbuch-Trennzeilen sind jetzt immer vor Verteilern einsortiert.
  - Adressbuchgruppe von Verteilern direkt bestimmbar

  Revision 1.19  2000/04/30 19:17:35  mk
  - Y2K-Fix fuer Info in Autoversand, nur optisch

  Revision 1.18  2000/04/29 19:11:51  jg
  - Ueberpruefung der Usernameneingabe bei Nachricht/Direkt, Verteilern
    und "Kopien an" + "Empfaenger aendern" im Sendefenster

  Revision 1.17  2000/04/27 07:23:34  jg
  - Bugfixes Adressbuchgruppen:
    Beim Editieren eines einzelnen Users wurde die Anzeige nicht aktualisiert
    Schnellsuchfunktion (. oder /) funktionierte nicht.

  Revision 1.16  2000/04/24 13:19:25  jg
  - Bugfix: User-Passwort aendern ueberschrieb Adressbuchgruppe

  Revision 1.15  2000/04/18 11:23:49  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.14  2000/04/15 21:44:46  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.13  2000/04/15 21:22:46  jg
  - Trennzeilen fuer Userfenster eingebaut (STRG+T im Spezialmenue)
  - STRG+P im UserSpezialmenue (Position) verschiebt wie P im Brett-SpezialMenue
    einen oder mehrere Markierte User in eine andere Adressbuchgruppe.

  Revision 1.12  2000/04/15 09:58:00  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.11  2000/04/13 12:48:36  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.10  2000/03/10 00:09:08  mk
  Fix: Autoversand/Hinzufuegen benoetigt keinen Dateinamen mehr

  Revision 1.9  2000/02/27 10:08:42  jg
  - Bei Brettvertreteradresse (Spezial..zUgriff) ist jetzt
    alles ausser Verteilerlisten erlaubt.

  Revision 1.8  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.7  2000/02/20 11:06:33  mk
  Loginfos hinzugeueft, Todo-Liste geaendert

  Revision 1.6  2000/02/20 09:51:39  jg
  - auto_empfsel von XP4E.PAS nach XP3O.PAS verlegt
    und verbunden mit selbrett/seluser
  - Bei Brettvertreteradresse (Spezial..zUgriff) kann man jetzt
    mit F2 auch User direkt waehlen. Und Kurznamen eingeben.

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
end.


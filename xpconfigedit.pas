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

{ CrossPoint - UniSel (Boxen, Gruppen, Systeme, Kurznamen, Mime-Typen) }

{$I xpdefine.inc}

unit xpconfigedit;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ENDIF}
  sysutils,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
  maus2,mouse,resource,xpglobal,
  xp0,xp1,xp1o,xp1o2,xp1input,xp2c,fidoglob;

function  UniSel(typ:byte; edit:boolean; default:string):string;
procedure BoxSelProc(var cr:customrec);
procedure GruppenSelproc(var cr:customrec);

procedure get_first_box(d:DB);

procedure SetUsername(s:string);

procedure SelSchab(var cr:CustomRec);
function  zidtest(var s:string):boolean;
function  validfile(var s:string):boolean;
function  testfidodir(var s:string):boolean;
function  testqwkinfiles(var s:string):boolean;
procedure set_uparcext(var s:string);
procedure set_downarcext(var s:string);
function  testmbretter(var s:string):boolean;
procedure gf_getntyp(var s:string);
function  testbaud(var s:string):boolean;
function  testbossnode(var s:string):boolean;
procedure setfidoadr(var s:string);
function  xp9_testbox(var s:string):boolean;
function  xp9_setclientFQDN(var s:string):boolean;
function  xp9_FQDNTest(var s:string):boolean;
procedure ps_setempf(var s:string);
function  notempty2(var s:string):boolean;
function  testreplyto(var s:string):boolean;
procedure uucp_getloginname(var s:string);
function  Conn_setmode(var s:string):boolean;
function  uucp_setprot(var s:string):boolean;
function  uucp_setsznego(var s:string):boolean;
function  testuucp(brk,modif:boolean):boolean;
//function  testuucp(var s:string):boolean;
procedure SetDomain(var s:string);
procedure testArcExt(var s:string);
function  testscript(var s:string):boolean;
procedure scripterrors(var s:string);
procedure setpasswdfield(var s:string);
procedure fidotestpasslen(var s:string);
function  testvertreterbox(var s:string):boolean;
function  testsysname(var s:string):boolean;
function  testlogfile(var s:string):boolean;
function  TestAKAservers(var s:string):boolean;
function  testZCpointname(var s:string):boolean;
function  JanusSwitch(var s:string):boolean;

function  PPPClientPathTest(var s:string):boolean;
function  PPPClientTest(var s:string):boolean;
function  multi_Mailstring(var s:string):boolean;
function  IsMailAddress(const s:string):boolean;
function  ReadExtCfgFilename(txt:atext; var s1:string; var cdir: String; subs:boolean):boolean;

implementation  {---------------------------------------------------}

uses
  {$IFDEF unix}xplinux,{$ENDIF}
  xp2,xp3,xp3o,xp9bp,xpnt,xpterminal,xpmodemscripts, replytoall;

const umtyp : array[0..5] of string[5] =
              ('IBM','ASCII','ISO','Tab.1','Tab.2','Tab.3');

{$IFNDEF DOS32}
      SupportedNetTypes: array[0..5] of byte =
        (nt_Client, nt_POP3, nt_NNTP, nt_UUCP, nt_Fido, nt_ZConnect);
{$ELSE}
      SupportedNetTypes: array[0..3] of byte =
        (nt_Client, nt_UUCP, nt_Fido, nt_ZConnect);
{$ENDIF}

var   UpArcnr   : integer;    { fÅr EditPointdaten }
      DownArcNr : integer;
      userfield : integer;    { Masken-Nr., s. get_first_box }
      gf_fido   : boolean;
      loginfld  : integer;    { UUCP: login                     }

      Conn_ModeFld:integer;   { UUCP/Fido: field of mode selector    }

      Conn_TelFld:integer;    { UUCP/Fido: field for phone numbers   }
      Conn_IPFld: integer;    { UUCP/Fido: field for ip no./hostname }
      Conn_PortFld:integer;   { UUCP/Fido: field for ip port         }

      UUp1,UUpl : integer;    { UUCP: first and last protocol   }

      UUCP_peFld: integer;    { UUCP: field of UUCP-e protocol  }
      UUCP_ptFld: integer;    { UUCP: field of UUCP-t protocol  }
      UUCP_p_GFld:integer;    { UUCP: field of UUCP-G protocol  }
      UUCP_pgFld: integer;    { UUCP: field of UUCP-g protocol  }

      UUCP_gWinFld:integer;   { UUCP: field of window size      }
      UUCP_gPktFld:integer;   { UUCP: field of packet size      }
      UUCP_gVarFld:integer;   { UUCP: field of window size      }
      UUCP_gForFld:integer;   { UUCP: field of packet size      }
      UUCP_MaxSizeFld:integer;{ UUCP: field of max. file size   }

      DomainNt  : shortint;   { Netztyp f. setdomain() und testvertreterbox() }
      bDomainNt : byte;                                                { u.a. }
      EditPnt   : byte;       { Netztyp f. EditPointdaten }
      EMSIfield : integer;
      pp_da     : boolean;    { unversandte Nachrichten vorhanden }
      amvfield  : integer;    { EditDiverses }
      downprotnr: integer;    { Edit/Point - Download-Protokoll }



function CreateServerFilename(d: db; nt:byte; const boxname:string):string;
var fa : fidoadr; i: integer;
begin
  if (nt=nt_Fido) or ((nt=nt_QWK) and multipos(_MPMask,boxname)) then begin
    splitfido(boxname,fa,0);
    result:=formi(fa.net mod 10000,4)+formi(fa.node mod 10000,4);
    end
  else begin
    result:='';
    for i:=1 to length(boxname)do
      if UpCase(boxname[i]) in ['0'..'9','A'..'Z']then
        result:=result+boxname[i];
    if result='' then result:='box-0001';
    result:=FileUpperCase(LeftStr(result,8));
    end;

  // this function will be called when creating the first server
  // no database is available then
  if not assigned(d) then exit;

  // assure no other server uses the same file
  dbSeek(d,boiDatei,result);
  if dbFound then begin
    result:=LeftStr(result,6)+'01';
    repeat
      dbSeek(d,boiDatei,result);
      if dbFound then result:=LeftStr(result,6)+formi(ival(RightStr(result,2))+1,2);
    until not dbFound;
    end;
end;


procedure SelSchab(var cr:CustomRec);
var ps, dir, name, ext: String;
begin
  selcol;
  ps:=fsbox(screenlines div 2 - 5,'*' + extXps,'',cr.s+extXps,false,false,false);
  fsplit(ps,dir,name,ext);
  cr.brk:=(name='');
  if not cr.brk then cr.s:=name;
end;


function zidtest(var s:string):boolean;       { Pointdaten - Serienner }
begin
  if length(s)=4 then zidtest:=true
  else begin
    rfehler(903);    { 'Die Seriennummer mu· 4 Zeichen lang sein.' }
    zidtest:=false;
    end;
end;


function validfile(var s:string):boolean;     { Sysop-Mode }
begin
  if (trim(s)<>'') and not ValidFilename(s) then begin
    rfehler(904);    { 'ungÅltiger Dateiname' }
    validfile:=false
    end
  else
    validfile:=true;
end;

function testfidodir(var s:string):boolean;   { Fido Sysop-Mode }
begin
  if s='' then
    result:=true
  else begin
    result:=false;
    s:=AddDirSepa(ExpandFileName(s));
    if s=OwnPath then
      rfehler(905)    { 'Verzeichnis darf nicht gleich dem XP-Verzeichnis sein' }
    else
      if IsPath(s) then
        result:=true
      else
        if ReadJN(getres(900),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
          if not CreateDir(s)then
            rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
          else
            result:=true;
    end;
end;

function testqwkinfiles(var s:string):boolean;
var
    qd  : string;
begin
  testqwkinfiles:=false;
  if s<>'' then begin
    qd:=ExtractFilePath(s);
    testqwkinfiles:=testfidodir(qd);
    s:=qd+ExtractFilename(s);
    end;
end;

procedure set_uparcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if UpArcNr<1 then exit;
  ls:=LowerCase(s);
  ext:='*';
  if (LeftStr(ls,5)='pkarc') or (LeftStr(ls,5)='pkpak') then ext:='arc'
  else if LeftStr(ls,3)='lha' then ext:='lzh'
  else if LeftStr(ls,5)='pkzip' then ext:='zip'
  else if LeftStr(ls,3)='arj' then ext:='arj'
  else if (LeftStr(ls,4)='copy') and (getfield(UpArcNr)<>'txt') then ext:='';
  if ext<>'*' then setfield(UpArcNr,ext);
end;

procedure set_downarcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if DownArcNr<1 then exit;
  ls:=LowerCase(s);
  ext:='*';
  if (LeftStr(ls,6)='pkxarc') or (LeftStr(ls,7)='pkunpak') then ext:='arc'
  else if LeftStr(ls,3)='lha' then ext:='lzh'
  else if LeftStr(ls,7)='pkunzip' then ext:='zip'
  else if LeftStr(ls,3)='arj' then ext:='arj'
  else if (LeftStr(ls,4)='copy') and (getfield(DownArcNr)<>'txt') then ext:='';
  if ext<>'*' then setfield(DownArcNr,ext);
end;

function testmbretter(var s:string):boolean;
begin
  if pp_da and (UpperCase(s)<>UpperCase(BoxPar^.MagicBrett)) then begin
    s:=BoxPar^.MagicBrett;
    rfehler(927);
    testmbretter:=false;
    end
  else begin
    if LastChar(s)<>'/' then s:=s+'/';
    if FirstChar(s)<>'/' then s:='/'+s;
    testmbretter:=true;
    end;
end;

function testbaud(var s:string):boolean;
begin
  if ival(s)=0 then testbaud:=false
  else testbaud:=(115200 mod ival(s))=0;
end;

function testbossnode(var s:string):boolean;
var fa : fidoadr;
begin
  testbossnode:=false;
  if trim(s)='' then errsound
  else begin
    splitfido(s,fa,DefaultZone);
    with fa do
      if net+node=0 then errsound
      else begin
        s:=strs(zone)+':'+strs(net)+'/'+strs(node);
        testbossnode:=true;
        end;
    end;
end;

procedure setfidoadr(var s:string);   { Gruppen-Adresse }
var fa : FidoAdr;
begin
  if trim(s)<>'' then begin
    splitfido(s,fa,2);
    with fa do
      s:=strs(zone)+':'+strs(net)+'/'+strs(node)+iifs(ispoint,'.'+strs(point),'');
    end;
end;

procedure ps_setempf(var s:string);
var p : byte;
begin
  p:=cpos('@',s);
  if p>0 then
    s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1));
end;

function testreplyto(var s:string):boolean;
var p : byte;
    d : DB;
begin
  if s='' then
    testreplyto:=true
  else begin                            { Wenns keine gueltige Adresse ist...}
    p:=cpos('@',s);
    if (p=0) or (cPos('.',mid(s,p))=0) then
    begin
      dbOpen(d,PseudoFile,1);
      dbSeek(d,piKurzname,UpperCase(s));
      if dbFound then
      begin
        s:= dbReadStr(d,'Langname');    { ists ein Kurzname ? }
        dbclose(d);
        testreplyto:=true;
        if cPos(' ',s)<>0 then           { jetzt der Langname jetzt gueltig ? }
          begin
            rfehler(908);               { 'ungÅltige Adresse' }
            testreplyto:=false;
            end;
        end
      else begin
        rfehler(908);     { 'ungÅltige Adresse' }
        dbclose(d);
        testreplyto:=false;
        end;
      end
    else
      testreplyto:=true;
  end;
end;

procedure uucp_getloginname(var s:string);
begin
  if getfield(loginfld)='' then
    setfield(loginfld,s);
end;

function Conn_setmode(var s:string):boolean;
  var modem: boolean;
begin
  modem:=(s=getres2(920,71));
  setfieldenable(Conn_telfld,     modem);
  setfieldenable(Conn_ipfld,  not modem);
  setfieldenable(Conn_portfld,not modem);
  setfieldnodisp(Conn_telfld, not modem);
  setfieldnodisp(Conn_ipfld,      modem);
  setfieldnodisp(Conn_portfld,    modem);

  if uucp_pefld<>0 then begin
    setfieldenable(uucp_pefld,  not modem);
    setfieldenable(uucp_ptfld,  not modem);
    end;
  Conn_setmode:=true;
end;

function uucp_setprot(var s:string):boolean;
var has_g: boolean;
begin
  has_g := (s=_jn_[1]) or (getfield(iif(uucp_p_gfld=fieldpos,uucp_pgfld,uucp_p_gfld))=_jn_[1]);
  setfieldenable(uucp_gwinfld, has_g);
  setfieldenable(uucp_gpktfld, has_g);
  setfieldenable(uucp_gvarfld, has_g);
  setfieldenable(uucp_gforfld, has_g);
  result:=true;
end;

function uucp_setsznego(var s:string):boolean;
begin
  setfieldenable(uucp_MaxSizeFld,s=_jn_[1]);
  result:=true;
end;

function DefaultMaps(nt:byte):string;
begin
  case nt of
    nt_Netcall,
    nt_ZConnect : DefaultMaps:='MAPS';
    nt_Magic    : DefaultMaps:='MAF';
    nt_GS,
    nt_ProNet   : DefaultMaps:='SYSTEM';
    nt_Maus     : DefaultMaps:='MAUS';    { nicht editierbar! }
    nt_Fido     : DefaultMaps:='Areafix';
    nt_UUCP, nt_Client: DefaultMaps:='changesys';
    nt_QWK      : DefaultMaps:='ZQWK';
  else            DefaultMaps:='SYSOP';   { Quick, Turbo }
  end;
end;

function testuucp(brk,modif:boolean):boolean;
var i  : integer;
  modem: boolean;
begin
  if brk or (not modif) then
  begin
    result:=true;
    exit;
  end;

  modem:=(getfield(Conn_modefld)=getres2(920,71));

  for i:=uup1 to uupl do
    if not (modem and (i in[uucp_pefld,uucp_ptfld])) then
      if getfield(i)=_jn_[1] then
      begin
        result:=true;
        exit;
      end;

  rfehler(909);    { 'Mindestens ein Protokoll mu· eingeschaltet sein!' }
  result:=false;
end;


procedure SetDomain(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (FirstChar(s)='.') or (FirstChar(s)='@') do
        DeleteFirstChar(s)
    else begin
      if FirstChar(s)<>'.' then
         s:='.'+s;
      if (bDomainNt<>0) and (getfield(fieldpos+1)='') then
        setfield(fieldpos+1,s);                                                   
      end;
end;

procedure SetDomain2(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (FirstChar(s)='.') or (FirstChar(s)='@') do
        DeleteFirstChar(s)
    else begin
      if s[1]<>'.' then
         s:='.'+s;
    end;
end;

procedure testArcExt(var s:string);
begin
  if (EditPnt=nt_Maus) and (s='TXT') then
    s:='';
end;

function testscript(var s:string):boolean;
var dir, name, ext: String;
begin
  if trim(s)='' then
    testscript:=true
  else begin
    fsplit(s,dir,name,ext);
    if ext='' then s:=dir+name+'.scr';
    if Fileexists(s) then
      testscript:=true
    else begin
      rfehler(22);     { 'Datei ist nicht vorhanden!' }
      testscript:=false;
      end;
    end;
end;

procedure scripterrors(var s:string);
begin
  if (s<>'') and Fileexists(s) and (RunScript(nil,nil,nil,true,s,false,false)<>0) then begin
    rfehler(925);    { 'Syntaxfehler in Script' }
    if listfile(LogPath+ScErrlog,scerrlog,true,false,false,0)=0 then;
    end;
end;

{ Fileserver: Feldbezeichnung Ñndern }

procedure setpasswdfield(var s:string);
begin
  setfieldtext(4,getres2(903,iif(UpperCase(s)=UpperCase(uuserver),7,6)));
end;

{ Fido: YooHoo-PW auf 8 Zeichen begrenzen }

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure fidotestpasslen(var s:string);
begin
  if (getfield(EMSIfield)='N') and (length(getfield(4))>8) then begin
    rfehler(926);
    setfield(4,LeftStr(getfield(4),8));
    end;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

function testvertreterbox(var s:string):boolean;
var d  : DB;
    nt : ShortInt;
begin
  if s='' then  
    testvertreterbox:=true
  else 
  begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then 
    begin
      s:= dbReadStr(d,'boxname');
      nt:=dbReadInt(d,'netztyp');
      if fieldpos=amvfield then    { AM-Vertreterbox }
        Result :=(DomainNt=nt)
      else                         { PM-Vertreterbox }
        Result :=ntAdrCompatible(DomainNt,nt);
      if not Result then 
        rfehler(2713);
    end else
    begin
      rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
      testvertreterbox:=false;
    end;
    dbClose(d);
  end;
end;

function testsysname(var s:string):boolean;
begin
  if trim(s)='' then begin
    errsound;
    testsysname:=false;
    end
  else
    testsysname:=true;
end;

function testlogfile(var s:string):boolean;
var fn : string;
begin
  if s='' then
    testlogfile:=true
  else begin
    if LowerCase(s)='logfile' then           { Diese Pruefung ist nun wirklich der Hit (hd) }
      if s[1]='l' then s:=s+'.log'
      else s:=s+'.LOG';
    if not multipos(_MPMask,s) then
      fn:=logpath+s
    else
      fn:=s;
    if validfilename(fn) then
      testlogfile:=true
    else begin
      rfehler(928);         { 'ungÅltiger Dateiname!' }
      testlogfile:=false;
      end;
    end;
end;


function TestAKAservers(var s:string):boolean;
var ok : boolean;
    p  : byte;
    s2 : string;
begin
  ok:=true;
  if s<>'' then begin
    s2:=s;
    repeat
      p:=blankpos(s2);
      if p=0 then p:=length(s2)+1;
      if ntBoxNetztyp(LeftStr(s2,p-1))<>nt_Fido then begin
        rfehler1(929,LeftStr(s2,p-1));  { '%s ist keine eingetragene Fido-Serverbox!' }
        ok:=false;
        end;
      s2:=trim(mid(s2,p+1));
    until s2='';
    end;
  TestAKAservers:=ok;
end;


{ ZCONNECT-Pointname auf ungÅltige Zeichen ÅberprÅfen }

function testZCpointname(var s:string):boolean;
var us : string[40];
    i  : integer;
begin
  us:='';
  for i:=1 to length(s) do
    if not (s[i] in ['A'..'Z','0'..'9','-']) and (cpos(s[i],us)=0) then
    begin
      if us<>'' then us:=us+', ';
      us:=us+s[i];
      end;
  if us<>'' then
    rfehler1(930,us);    { 'Warnung: UngÅltige Zeichen im Pointname: %s' }
  testZCpointname:=true;  { (us=''); }
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

function JanusSwitch(var s:string):boolean;
var x,y, anz,i : integer;
    t     : taste;
begin
  JanusSwitch:=true;
  if LowerCase(getfield(downprotnr))='zmodem' then exit;
  anz:=res2anz(932);
  msgbox(63,anz+5,_hinweis_,x,y);
  for i:=1 to anz do
    wrt(x+3,y+1+i,getres2(932,i));
  wrt(x+3,y+3+anz,getres(12));    { 'Taste drÅcken ...' }
  errsound;
  get(t,curon);
  closebox;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }



{ Typ :  1=Boxen, 2=Gruppen, 3=Systeme, 4=Kurznamen, 5=MIME-Typen }
{ edit:  true=editieren, false=nur auswÑhlen                      }

function UniSel(typ:byte; edit:boolean; default:string):string;
const maxgl   = 100;
      dsellen = 20;
var d         : DB;
    p0,p,gl : integer;
    t         : taste;
    drec      : array[1..maxgl] of longint;
    x,y       : Integer;
    width     : byte;
    buttons   : string;
    bp,rb     : shortint;
    okb,edb   : shortint;
    aufbau    : boolean;
    c         : char;
    empty     : boolean;
    s         : string;
    setdefault: boolean;
    umlaut    : byte;
    poutside  : boolean;
    startmkey : boolean;   { beim Start war Maustaste gedrÅckt }
    directsel : string;
    nameofs   : byte;

  function Netz_Typ(nt:byte):string;
  var
    i: Integer;
  begin
    Netz_Typ:=ntName(nt_Netcall);
    for i:=0 to High(SupportedNetTypes) do
      if nt=SupportedNetTypes[i] then Netz_Typ:=ntName(SupportedNetTypes[i]);
  end;

  procedure displine(i:integer);
  var s1,s2,s3: string;
      limit,grnr : longint;
      w          : smallword;
      hd,sig,qt  : char;
      qm         : string[8];
      nt,b       : byte;
      dc         : string[2];
      adr        : string;
  begin
    drec[i]:=dbRecno(d);
    case typ of
      1 : s1 := dbReadStr(d,'Boxname');
      2 : s1 := dbReadStr(d,'Name');
    end;
    if setdefault and (UpperCase(s1)=UpperCase(default)) then begin
      p:=i;
      setdefault:=false;
      end;
    case typ of
      1 : begin     { Boxen }
            s2 := dbReadStr(d,'Username');
            s3 := dbReadStr(d,'Kommentar');
            dbRead(d,'Netztyp',nt);
            if s1=DefaultBox then
              if s1=DefFidoBox then dc:='F '
            {$IFDEF Unix }
              else dc:='* '
            {$ELSE }
              else dc:='˚ '
            {$ENDIF }
            else
              if s1=DefFidoBox then dc:='f '
              else dc:='  ';
            s:=dc+forms(s1,11)+' '+forms(Netz_Typ(nt),12)+forms(s2,17)+' '+
               forms(s3,23);
          end;
      2 : begin     { Gruppen }
            dbRead(d,'msglimit',limit);
            dbRead(d,'int_nr',grnr);
            dbRead(d,'umlaute',umlaut);
            hd:=iifc(UpperCase(dbReadStr(d,'kopf')+ extXps)<>UpperCase(headerfile),'K',' ');
            qm:=dbReadStr(d,'quotemsk');
            qt:=iifc((qm<>'') and (UpperCase(qm+extXps)<>UpperCase(quotemsk)),'Q',' ');
            sig:=iifc(UpperCase(dbReadStr(d,'signatur')+ extXps)<>UpperCase(signatfile),'S',' ');
            s:=strsn(grnr,5)+' '+hd+qt+sig+' '+forms(s1,28)+' '+
               forms(umtyp[umlaut],6)+
               iifs(limit>0,strsrnp(limit,12,0),sp(11)+' Ï')+' ';
          end;
      3 : begin     { Systeme }
            s1 := dbReadStr(d,'name');
            s2 := dbReadStr(d,'kommentar');
            s3 := dbReadStr(d,'fs-passwd');
            dbRead(d,'flags',w);
            dbRead(d,'fs-typ',b);
            if b=3 then dc:=' U'
            else if dbReadStr(d,'FS-Name')<>'' then dc:=' F'
            else dc:='  ';
            s:=dc+iifs((s3='') or (b=3),'  ','P ')+forms(s1,15)+' '+forms(s2,31);
          end;
      4 : begin     { Kurznamen }
            s1  := dbReadStr(d,'kurzname');
            adr := dbReadStr(d,'langname');
            s2  := dbReadStr(d,'pollbox');
            s:=' '+forms(s1,12)+' '+forms(adr,36)+' '+forms(s2,12);
          end;
      5 : begin     { MIME-Typen }
            s1 := dbReadStr(d,'typ');
            s2 := dbReadStr(d,'extension');
            s3 := dbReadStr(d,'programm');
            if s3='' then s3:=getres(934)    { '(intern)' }
            else if length(s3)>31 then s3:=LeftStr(s3,31)+'...';
            s1:=extmimetyp(s1);
            if LeftStr(s1,12)='application/' then s1:='appl.'+mid(s1,12);
            s:=' '+forms(s1,26)+' '+forms(s2,6)+forms(s3,31);
          end;
    end;
    if not setdefault and (i=p) then attrtxt(col.colsel2bar)
    else attrtxt(col.colsel2box);
    mwrt(x+1,y+i,s);
  end;

  procedure display;
  var i : integer;
      b : boolean;
  begin
    if drec[1]=0 then begin
      dbGoTop(d); b:=true; end
    else begin
      dbSkip(d,-1);
      b:=dbBOF(d);
      if b then dbGoTop(d)
      else dbSkip(d,1);
      end;
    fillchar(drec,sizeof(drec),0);
    i:=1;
    while (i<=gl) and not dbEOF(d) do begin
      displine(i);
      dbSkip(d,1);
      inc(i);
      end;
    attrtxt(col.colsel2box);
    if i<=gl then begin
      moff;
      clwin(x+1,x+width,y+i,y+gl);
      mon;
      end;
    mwrt(x,y+1,iifc(b,'≥',#30));
    mwrt(x,y+gl,iifc(dbEOF(d),'≥',#31));
    if i=1 then begin
      attrtxt(col.colsel2bar);
      mwrt(x+1,y+1,sp(width));
      end;
    aufbau:=false;
    p0:=p;
  end;

  {$I xpconfigedit-servers.inc}
  {$I xpconfigedit-groups.inc}
  {$I xpconfigedit-systems.inc}
  {$I xpconfigedit-pseudos.inc}
  {$I xpconfigedit-mimetypes.inc}

  { sonstige Funktionen }

  procedure readbutt;
  begin
    rbx:=x+1; rby:=y+p;
    rb:=readbutton(x+2,y+gl+2,2,buttons,bp,false,t);
  end;

  procedure maus_bearbeiten;
  var ins1    : boolean;
      inside  : boolean;
      outside : boolean;
      xx,yy   : integer;
  begin
    maus_gettext(xx,yy);
    ins1:=(xx>x) and (xx<=x+width) and (yy>y);
    inside:=ins1 and (yy<=y+gl);
    outside:=not ins1 or (yy>y+gl+iif(edit,2,0));
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        p:=yy-y else
      if (t=mausunright) or (t=mausunleft) then begin
        if not poutside and not edit and (t=mausunleft) then
          if startmkey then startmkey:=false
          else t:=keycr;
        poutside:=false
        end else
      if t=mausldouble then begin
        rb:=edb; t:=keycr; end
      end;
    if outside then begin
      if (t=mausleft) or (t=mausright) or (t=mauslmoved) or (t=mausrmoved) then
        poutside:=true else
      if poutside and ((t=mausunleft) or (t=mausunright)) then begin
        rb:=okb; t:=keyesc; end;
      end;
  end;

  procedure _DirectSel;
  var nfeld : string;
      dnew  : string;
      i     : integer;
  begin
    if (c<' ') and (c<>#8) then
      exit;
    if ((c=#8) and (directsel='')) or ((c>=' ') and (length(directsel)=dsellen)) then begin
      errsound;
      exit;
    end;
    case typ of
      1 : nfeld:='boxname';
      2 : nfeld:='name';
      3 : nfeld:='name';
      4 : nfeld:='kurzname';
    end;
    if c=#8 then
      dnew:=LeftStr(directsel,length(directsel)-1)
    else
      dnew:=directsel+c;
    dbSeek(d,1,UpperCase(dnew));
    if dbBOF(d) then
      dbGoTop(d);
    if dbEOF(d) or (UpperCase(LeftStr(dbReadStr(d,nfeld),length(dnew)))<>UpperCase(dnew)) then
      errsound
    else begin
      i:=1;
      while (i<=maxgl) and (drec[i]<>dbRecno(d)) do
        inc(i);
      if i<=maxgl then
        p:=i
      else begin
        aufbau:=true;
        p:=1;
      end;
      DirectSel:=UpperCase(dnew);
    end;
  end;

begin { --- UniSel --- }
  UniSel := '';
  if typ>5 then exit;
  case typ of
    1 : begin     { Boxen }
          dbOpen(d,BoxenFile,1);
          if not edit and (dbRecCount(d)=1) and (lastkey<>keyf2) then begin
            unisel:=dbReadStr(d,'boxname');
            dbClose(d);
            exit;
          end;
          width:=67;
          buttons:=getres(907);   { ' ^Neu , ^Lîschen , ^WÑhlen , ^Edit , Netz^typ , ^OK ' }
          okb:=6; edb:=4;
          pushhp(iif(edit,130,139));
          nameofs:=3;
        end;
    2 : begin     { Gruppen }
          dbOpen(d,GruppenFile,1);
          width:=59;
          buttons:=getres(908);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,200,209));
          nameofs:=11;
        end;
    3 : begin     { Systeme }
          dbOpen(d,SystemFile,1);
          width:=51;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,460,469));
          nameofs:=5;
        end;
    4 : begin     { Kurznamen }
          dbOpen(d,PseudoFile,1);
          width:=63;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,710,719));
          nameofs:=2;
        end;
    5 : begin     { MIME-Typen }
          d:=mimebase;
          width:=65;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(820);
          nameofs:=2;
        end;
  end;
  if typ<>5 then miscbase:=d;
  drec[1]:=0;
  gl:=screenlines-11;
  if screenlines>30 then dec(gl,2);
  if screenlines>40 then dec(gl,2);
  selbox(width+2,gl+4,'',x,y,false);

  p:=1; bp:=1; p0:=p;
  if not edit then inc(gl,2);
  if edit then begin
    attrtxt(col.colsel2rahmen);
    mwrt(x,y+gl+1,'√'+dup(width,'ƒ')+'¥');
    t:='!';    { Buttons nur anzeigen }
    readbutt;
    end;

  aufbau:=true;
  setdefault:=(default<>'');
  maus_pushinside(x+1,x+width,y+1,y+gl);
  poutside:=false;
  startmkey:=(maust<>0);
  directsel:='';
  repeat
    while (p>1) and (drec[p]=0) do dec(p);
    if aufbau then display;
    if setdefault then begin
      setdefault:=false;
      dbSeek(d,1,default);
      if dbFound then display
      else begin
        dbGo(d,drec[1]);
        displine(1);
        end;
      end;
    empty:=(drec[1]=0);
    if not empty then begin
      while drec[p]=0 do dec(p);
      if p<>p0 then begin
        if drec[p0]>0 then begin
          dbGo(d,drec[p0]); displine(p0); end;
        dbGo(d,drec[p]); displine(p);
        p0:=p;
        end;
      end;
    if edit then begin
      t:='*';
      readbutt;
      bp:=abs(rb);
      end
    else begin
      gotoxy(x+length(directsel)+nameofs,y+p);
      get(t,curon);
      end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    c:=UpCase(t[1]);
    if not edit then
      _DirectSel
    else
      if rb>0 then
        case typ of
          1 : case rb of
                1 : NewBox;
                2 : if not empty then DelBox;
                3 : if not empty then SetDefaultBox;
                4 : if not empty then EditBox;
                5 : if not empty then EditNetztyp;
              end;
          2 : case rb of
                1 : NeueGruppe;
                2 : DelGruppe;
                3 : EditGruppe;
              end;
          3 : case rb of
                1 : NeuesSystem;
                2 : DelSystem;
                3 : EditSystem;
              end;
          4 : case rb of
                1 : EditPseudo(true);
                2 : if not empty then DelPseudo;
                3 : if not empty then EditPseudo(false);
              end;
          5 : case rb of
                1 : EditMimetyp(true);
                2 : if not empty then DelMimetyp;
                3 : if not empty then EditMimetyp(false);
              end;
        end;
    if not empty and (not edit or (rb<0)) then begin
      if t=keyup then
        if p>1 then dec(p)
        else begin
          dbGo(d,drec[1]);
          dbSkip(d,-1);
          if not dbBOF(d) then aufbau:=true;
          end;
      if t=keydown then
        if p<gl then inc(p)
        else begin
          dbGo(d,drec[gl]);
          dbSkip(d,1);
          if not dbEOF(d) then begin
            dbGo(d,drec[2]); aufbau:=true;
            end;
          end;
      if t=keyhome then begin
        drec[1]:=0; aufbau:=true; p:=1;
        end;
      if t=keyend then
        if drec[gl]=0 then p:=gl
        else begin
          dbGoEnd(d);
          if not dbEOF(d) then begin
            dbSkip(d,-gl+1);
            if dbBOF(d) then dbGoTop(d);
            aufbau:=true; p:=gl;
            end;
          end;
      if t=keychom then p:=1;
      if t=keycend then p:=gl;
      if t=keypgup then begin
        dbGo(d,drec[1]);
        dbSkip(d,-1);
        if dbBOF(d) then p:=1
        else begin
          dbSkip(d,-gl+2);
          if dbBOF(d) then dbGoTop(d);
          aufbau:=true;
          end;
        end;
      if t=keypgdn then
        if drec[gl]=0 then p:=gl
        else begin
          dbGo(d,drec[gl]);
          dbSkip(d,1);
          if dbEOF(d) then p:=gl
          else begin
            dbGo(d,drec[gl]);
            aufbau:=true;
            end;
          end;
      if typ=2 then
        if t='+' then addhzeit(1)
        else if t='-' then addhzeit(-1);
      end;

  until (edit and ((rb=0) or (rb=okb))) or
        (not edit and ((t=keycr) or (t=keyesc)));
  maus_popinside;
  pophp;

  if edit then
    UniSel:=''
  else
    if empty or (t=keyesc) then UniSel:=''
    else begin
      dbGo(d,drec[p]);
      case typ of
        1   : UniSel:=dbReadStr(d,'boxname');
        2,3 : UniSel:=dbReadStr(d,'name');
        4   : UniSel:=dbReadStr(d,'kurzname');   { Dummy }
      end;
    end;

  if typ<>5 then begin
    dbClose(d);
    miscbase:=nil;
    end;
  closebox;
  if (typ = 1) and edit then
    askRTA (false);
end;


{ fÅr maske.CustomSel }

procedure BoxSelProc(var cr:customrec);
var
  TempBoxPar: BoxRec;
begin
  TempBoxPar := BoxPar^;
  with cr do begin
    s:=UniSel(1,false,s);
    brk:=(s='');
    end;
  BoxPar^ := TempBoxPar;
end;


procedure GruppenSelproc(var cr:customrec);
begin
  with cr do begin
    s:=UniSel(2,false,s);
    brk:=(s='');
    end;
end;


{ s = '<BOX> <USERNAME> [/ Realname]'}

procedure SetUsername(s:string);
var x,y  : Integer;
    brk  : boolean;
    user : string;
    real : string;
    box  : string;
    p    : byte;
    d    : DB;
    gross   : boolean;
    hasreal : boolean;
begin
  s:=trim(s);
  if s='' then
    rfehler(916)      { 'SETUSER - Parameter fehlen' }
  else begin
    p:=cpos(' ',s);
    if p=0 then begin
      box:=UpperCase(s); user:=''; real:='';
      end
    else begin
      box:=UpperCase(LeftStr(s,p-1));
      user:=trim(mid(s,p+1));
      p:=pos(' (',user);
      if p=0 then real:=''
      else begin
        real:=copy(user,p+2,length(user)-p-2);
        user:=trim(LeftStr(user,p-1));
        end;
      end;
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,box);
    if not dbFound then
      rfehler1(918,box)   { 'SETUSER - Box "%s" unbekannt!' }
    else begin
      hasreal:=ntRealname(dbReadInt(d,'netztyp'));
      if user='' then begin
        user:=dbReadStr(d,'username');
        real:=dbReadStr(d,'realname');
        dialog(length(getres(930))+length(box)+35,iif(hasreal,5,3),'',x,y);
        gross:=ntGrossUser(dbReadInt(d,'netztyp'));
        maddstring(3,2,getreps(930,box),user,30,30,iifs(gross,'>',''));   { 'Neuer Username fÅr %s:' }
        mhnr(1502);
        if hasreal then
          maddstring(3,4,forms(getreps(931,box),length(getreps(930,box))),real,30,40,'');  { 'Neuer Realname:' }
        readmask(brk);
        enddialog;
        end
      else
        brk:=false;
      if not brk then begin
        dbWriteStr(d,'username',user);
        if hasreal { and (real<>'') 29.07.96 } then dbWriteStr(d,'realname',real);
        if box=DefFidoBox then begin
          HighlightName:=UpperCase(user);
          aufbau:=true;
          end;
        if not dispusername then begin
          message(getres(910)+user+' @ '+box+iifs(real='','',' ('+real+')'));    { 'Username: ' }
          mdelay(1000);
          closebox;
          end;
        end;
      end;
    dbClose(d);
    showusername;
    end;
end;


procedure gf_getntyp(var s:string);
var uucp,client : boolean;
begin
  setfieldtext(fieldpos+1,getres2(912,iif(LowerCase(s)=LowerCase(ntName(nt_Client)),13,2)));
  gf_fido:=(LowerCase(s)=LowerCase(ntName(nt_Fido)));
  uucp:=(LowerCase(s)=LowerCase(ntName(nt_UUCP)));
  client:=(LowerCase(s)=LowerCase(ntName(nt_Client)));
  if (LowerCase(s)=LowerCase(ntName(nt_Maus))) or gf_fido or uucp or client then
    set_chml(userfield,'')
  else
    set_chml(userfield,'>');
  {if uucp or client then
    set_chml(fieldpos+1,'')
  else
    set_chml(fieldpos+1,'>');}
  setfieldtext(userfield,getres2(912,iif(client,12,3)));
end;

function xp9_testbox(var s:string):boolean;
var nt : string[15];
begin
  if trim(s)='' then begin
    rfehler(919);    { 'Bitte Boxname eingeben (Hilfe mit F1).' }
    xp9_testbox:=false;
    end
  else
    if gf_fido then
      xp9_testbox:=testbossnode(s)
    else begin
      if DomainNt<0 then nt:=LowerCase(getfield(1))   { Netztyp als String }
      else nt:=LowerCase(ntName(DomainNt));
      if nt=LowerCase(ntName(nt_Maus)) then begin
        if (length(s)>4) and (UpperCase(LeftStr(s,4))='MAUS') then
          s:=mid(s,5);
        if cpos('.',s)>0 then s:=LeftStr(s,cpos('.',s)-1);
        s:=LeftStr(s,6);
        end
      else if nt=LowerCase(ntName(nt_Netcall)) then         { Domain abschneiden }
        if RightStr(s,4)='.ZER' then s:=LeftStr(s,length(s)-4)
        else
      else if (nt=LowerCase(ntName(nt_ZCONNECT))) or (nt=LowerCase(ntName(nt_UUCP))) then
        if cpos('.',s)>0 then truncstr(s,cpos('.',s)-1);
      xp9_testbox:=true;
      end;
end;


function notempty2(var s:string):boolean;
begin
  if trim(s)<>'' then
    notempty2:=true
  else begin
    rfehler(920);    { 'Bitte Username eingeben (Hilfe mit F1).' }
    notempty2:=false;
    end;
end;

procedure get_first_box(d:DB);
var x,y  : Integer;
    brk  : boolean;
    name : string;
    dname: string;
    user : string;
    maps : string;
    dom  : string;
    fqdom: string;  
    email: string;
    ntyp : string;
    nt   : byte;
    i,b : integer;
label restart;
begin
restart:
  dialog(ival(getres2(911,0)),13,'',x,y);
  maddtext(3,2,getres2(911,1),col.coldiahigh);    { 'Bitte geben Sie Netztyp und Name Ihrer Stamm-' }
  maddtext(3,3,getres2(911,2),col.coldiahigh);    { 'box sowie Username bzw. eMail-Adresse ein.' }
  maddtext(3,5,getres2(911,3),col.coldiahigh);    { 'Bei Einsatz des Netztyps RFC/Client benîtigen' }
  maddtext(3,6,getres2(911,4),col.coldiahigh);    { 'Sie einen externen Mail-/News-Client.' }
  name:=''; user:='';
  ntyp:=ntName(nt_Client); nt:=nt_Client;
  maddstring(3,8,getres2(911,5),ntyp,20,20,''); mhnr(681);   { 'Netztyp   ' }
  for i:=0 to High(SupportedNetTypes) do
    mappsel(true,ntName(SupportedNetTypes[i]));
  mset3proc(gf_getntyp);
  maddstring(3,10,getres2(912,13),name,20,20,'>-_0123456789:/.'+range('A','Z')+'éôö');
    mhnr(680);                                       { 'Server' bzw. 'Boxname' }
  DomainNt:=-1;
  msetvfunc(xp9_testbox);
  maddstring(3,12,getres2(912,12),user,30,80,'>'); mhnr(682);   { 'eMail-Adr.' bzw. 'Username' }
  userfield:=fieldpos;
  msetvfunc(notempty2);
  masksetstat(true,false,keyf2);    { <- zwingt zur korrekten Eingabe }
  readmask(brk);
  for i:=0 to High(SupportedNetTypes) do
    if LowerCase(ntyp)=LowerCase(ntName(SupportedNetTypes[i])) then
      nt:= SupportedNetTypes[i];
  closemask;
  closebox;
  email:='';

  dom:=ntDefaultDomain(nt);
  if nt = nt_Client then 
  begin
    email:=user;
    if not IsMailaddress(email) then
    begin
      rfehler(908);
      goto restart;
      end
    else 
    begin
      b := cpos('@', eMail);
      user:=LeftStr(email,b-1);
      dom:=mid(email,b);
      if cpos('.',dom)=0 then 
        dom:=''
        else 
      dom:=mid(dom,cpos('.',dom));
    end;
  end;

  user:=LeftStr(user,30);

  if not ntNameSpace(nt) then
    for i:=1 to length(user) do    { Leerzeichen aus Username -> "_" }
      if user[i]=' ' then user[i]:='_';
  DefaultBoxPar(nt,boxpar);      { neue Box mit Default-Werten anlegen }
  dbAppend(d);
  dbWrite(d,'netztyp',nt);
  dbWriteStr(d,'boxname',name);
  dbWriteStr(d,'username',user);
  dname:=CreateServerFilename(nil,nt,name);
  dbWriteStr(d,'dateiname',dname);
  maps:=DefaultMaps(nt);
  dbWriteStr(d,'NameOMaps',maps);

  dbWriteStr(d,'Domain',dom);
  fqdom:=''; dbWriteStr(d,'FQDN',fqdom);
  dbWriteStr(d,'EMail',email);
  case nt of
    nt_Maus   : boxpar^.pointname:=name;
    nt_Pronet : boxpar^.pointname:='01';
    else      if not nt = nt_Client then boxpar^.pointname:=''
              else 
              begin
                b := cpos('@', eMail);
                boxpar^.pointname:=mid(email,b+1);
                truncstr(boxpar^.pointname,min(25,cposx('.',boxpar^.pointname)-1));
              end;
  end;
  dbWriteStr(d,'Pointname',boxpar^.pointname);
  dbFlushClose(d);
  boxpar^.boxname:=name;
  boxpar^.username:=user;
  boxpar^._Domain:=dom;
  if (nt=nt_UUCP) and FileExists('uucp.scr') then
    boxpar^.script:=FileUpperCase('uucp.scr');
  WriteBox(dname,boxpar);
  DefaultBox:=name;
  if nt=nt_Fido then begin
    DefFidobox:=name;
    SetDefZoneNet;
    end;
  SaveConfig2;
  if nt=nt_UUCP then begin
//    XP_ID_AMs:=false;
    SaveConfig;
    end;
  end;


function xp9_setclientFQDN(var s:string):boolean;
var
  s1: string;
  b, u: Integer;
begin
  Result := false;
  mclearsel(6);                    { FQDN = Feld 6 !!! }
  if s='' then
  begin
    errsound;
    exit;
  end;
  if not IsMailAddress(s) then
  begin
     rfehler(908);
     exit;
  end;
  Result :=true;
  b := cpos('@', s);
  s1:=s; s1[b]:='.';
  for u:=cposx('_',s1) to length(s1) do
    if s1[u]='_' then s1[u]:='-';
  if Lowercase(mid(s1,b))='.t-online.de'
    then insert('.dialin',s1,b);
  mappendsel(6,false,s1);          { FQDN = Feld 6 !!! }
end;

function xp9_FQDNTest(var s:string): boolean;
var
  s1 : string;
  b: Integer;
begin
  Result :=true;
  s1:=mailstring(s,false);
  for b:=1 to length(s1) do
    case s1[b] of
      '@'  :  s1[b]:='.';
      '_'  :  s1[b]:='-';
      end;
  while FirstChar(s1) = '.' do
    delete(s1,1,1);
  if s1<>s then
  begin
    errsound;
    Result := false;
  end;
  s:=s1;
end;

function PPPClientPathTest(var s:string):boolean;
var ok   : boolean;
    fn   : String;
    x,y  : Integer;
begin
  PPPClientPathTest:=true;
  fn:=trim(s);
  if (fn<>'') then
  begin
    s := AddDirSepa(s);
    if Copy(fn, 1, 2) = '.\' then fn := Copy(fn, 3, Length(fn));
    if LastChar(fn) = '\' then DeleteLastChar(fn);
    ok := (cPos(':', fn) = 0) and (cPos('\', fn) = 0) and (cPos('.', fn) < 2)
      and (Length(fn) > 0) and (LastChar(fn) <> '.');
    if not ok then
    begin
      msgbox(62,6,_fehler_,x,y);
      mwrt(x+3,y+2,getres2(10900,37));   { 'Pfadangabe mu· RELATIV sein und auf ein Verzeichnis EINE' }
      mwrt(x+3,y+3,getres2(10900,38));   { 'Ebene DIREKT unterhalb des XP-Verzeichnisses verweisen!' }
      errsound;
      wait(curoff);
      closebox;
      freeres;
      PPPClientPathTest := false;
      Exit;
    end;
    if not IsPath(s) then
      if ReadJN(getres(900),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
      begin
        if CreateMultipleDirectories(s) = '' then
        begin
          PPPClientPathTest:=false;
          rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
        end;
      end else
        PPPClientPathTest:=false;
  end else
  begin
    PPPClientPathTest:=false;
    rfehler(939)           { 'Dieser Pfad darf nicht leer sein!' }
  end;
end;

function PPPClientTest(var s:string):boolean;
var ok   : boolean;
    fn, dir, name, ext: String;
    s1   : String;
begin
  PPPClientTest:=true;
  fn:=trim(s);
  if Pos('start /wait ', LowerCase(fn)) = 1 then fn := Copy(fn, 13, MaxInt);
  if Pos('start /wai ', LowerCase(fn)) = 1 then fn := Copy(fn, 12, MaxInt);
  if Pos('start /wa ', LowerCase(fn)) = 1 then fn := Copy(fn, 11, MaxInt);
  if Pos('start /w ', LowerCase(fn)) = 1 then fn := Copy(fn, 10, MaxInt);
  if cpos(' ',fn)>0 then fn:= LeftStr(fn,cpos(' ',fn)-1);
  if (fn<>'') then
  begin
    fsplit(fn,dir,name,ext);
    ok := dir = '';
    s1 := GetField(fieldpos-1);
    if Pos('.\', s1) = 1 then s1 := Mid(s1, 3);
    { if ustr(s1) =  ustr(Dir) then Ok := true; }
    if Dir = '$CLPATH+' then ok := true;
    if not ok then
    begin
      rfehler1(936, UpperCase(fn)); { 'Eintrag darf entweder keine oder nur "$CLPATH+" als Pfadangabe enthalten!' }
      PPPClientTest:=false;
    end else
    begin
      exchange(fn, '$CLPATH+', s1);
      if ext<>'' then
        ok:= FileSearch(fn,ownpath)<>''
      else
        ok:=(FileSearch(fn+'.exe',ownpath)<>'') or
          (FileSearch(fn+'.com',ownpath)<>'') or
          (FileSearch(fn+ extBatch,ownpath)<>'');
      if not ok then rfehler1(907, UpperCase(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
    end;
  end else
    begin
    PPPClientTest:=false;
    errsound;
  end;
end;

function multi_Mailstring(var s:string):boolean;
var n   : Integer;
    s1,s2 : string;
begin
  multi_Mailstring:=true;
  s1:=trim(s);
  if s1='' then exit;
  repeat
    n:=cpos(' ',s1);
    if n=0 then s2:=s1
    else begin
      s2:= LeftStr(s1,n-1);
      s1:=trim(mid(s1,n+1));
      end;
    if not IsMailAddress(s) then
    begin
      multi_mailstring:=false;
      fehler(Getres2(10900,8)+': ' +s2); { 'Ung_ltige Adresse: 's2 }
      exit;
    end;
  until n=0;
end;

function isMailAddress(const s: String): boolean;
var b: Integer;
begin
  Result := true;
  b:=cpos('@',s);
  if (b<=1) or (cpos('@',mid(s,b+1))<>0)
    or (cpos('.',mid(s,b+1))=0) or (cpos(' ',s)<>0)
    or (s <> MailString(s, false)) then
    Result :=false;
end;

function  ReadExtCfgFilename(txt:atext; var s1:string; var cdir: String; subs:boolean):boolean;
var   x,y,n   : Integer;
      brk     : boolean;
      fn      : string[20];
      cconfig : TSearchrec;
      seldir  : String;
      s2      : string;
      dir, name, ext: String;
      FindRes: Integer;
const
  cfgext  : array [1..4] of string[5] = ('*.CFG','*.BFG','*.BFE','*.$CF');
label restart;
begin
restart:
  s2 := '';
  if (cpos(':',s1) = 2) or (cpos(DirSepa, s1) = 1) then
  begin
    fsplit(ExpandFileName(s1),dir,name,ext);
    seldir := dir;
  end
  else seldir := cdir;
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  maddstring(3,2,fn,s1,37,60,'');   { Dateiname: }
  for n:= 1 to 4 do
  begin
    FindRes := FindFirst(seldir+cfgext[n],ffAnyfile,cconfig);
    while FindRes = 0 do
    begin
      if seldir = cdir then
        mappsel(false,cconfig.name)
      else
        mappsel(false,seldir+cconfig.name);
      FindRes := FindNext(cconfig);
    end;
    FindClose(CConfig);
  end;
  readmask(brk);
  enddialog;
  if not brk then
  begin
    if (trim(s1) = '') then s2 := WildCard else s2 := s1;
    if (cpos(':',s2) = 2) or (cpos(DirSepa, s2) = 1) then
      s2 := ExpandFileName(s2)
    else
      s2 := ExpandFileName(cdir + s2);
    if ((length(s2) = 2) and (s2[2] = ':')) or
       (LastChar(s2) = DirSepa) then
      s2 := ExpandFileName(s2 + WildCard)
    else
    if IsPath(s2) then
      s2 := ExpandFileName(s2 + DirSepa + WildCard);
    fsplit(s2,dir,name,ext);
    if not IsPath(dir) then
    begin
      rfehler1(949,dir);  { 'Verzeichnis "%s" ist nicht vorhanden!' }
      goto restart;
    end;
    if multipos('*?',s2) then
    begin
      selcol;
      pushhp(89);
      s2:=fsbox(Screenlines div 2 - 5,s2,'','',subs,false,false);
      pophp;
      if s2 <> '' then   { <Esc> gedrÅckt? }
      begin
        fsplit(s2,dir,name,ext);
        if dir = cdir then s1 := name + ext else s1 := s2;
      end;
      goto restart;
    end;
    if (s2<>'') and ({IsDevice(s2) or }not ValidFilename(s2)) then
    begin
      rfehler(3);   { UngÅltiger Pfad- oder Dateiname! }
      goto restart;
    end;
    s1 := s2;
    ReadExtCfgFilename := (s1<>'');
  end else
    ReadExtCfgFilename := false;
end;


{
  $Log$
  Revision 1.35  2002/01/03 19:19:13  cl
  - added and improved UTF-8/charset switching support

  Revision 1.34  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.33  2001/12/24 23:07:04  mk
  - updates for nt_Client

  Revision 1.32  2001/12/24 11:45:37  mk
  - fixed last committ

  Revision 1.31  2001/12/23 13:06:16  mk
  - fixed gf_getnettype for ClientMode

  Revision 1.30  2001/12/13 17:06:46  mk
  - removed unused variable

  Revision 1.29  2001/11/24 20:29:25  mk
  - removed Boxpar.Clientmode-parameter, ClientMode is now nettype 41

  Revision 1.28  2001/11/24 16:20:36  mk
  - reenabled client mode again

  Revision 1.27  2001/10/21 13:09:05  ml
  - removed some more warnings (only 130 yet...)

  Revision 1.26  2001/10/11 11:18:39  ma
  - changed server file name creation strategy

  Revision 1.25  2001/10/01 19:52:22  ma
  - disabled client mode II

  Revision 1.24  2001/09/27 23:44:53  ma
  - disabled client mode

  Revision 1.23  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.22  2001/09/08 16:29:38  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.21  2001/09/07 13:54:23  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.20  2001/09/07 02:07:44  mk
  - use IsMailAddress when possilbe, removed duplicate code

  Revision 1.19  2001/09/06 22:01:14  mk
  - client mode updates

  Revision 1.18  2001/09/06 18:53:36  mk
  - fixed big bug in get_first_box: variable b was not initialized

  Revision 1.17  2001/08/27 09:13:43  ma
  - changes in net type handling (1)

  Revision 1.16  2001/08/11 23:06:37  mk
  - changed Pos() to cPos() when possible

  Revision 1.15  2001/08/02 22:14:57  mk
  JG:- ReadExtCfgFilename: optimized suboptimal (but working) code

  Revision 1.14  2001/07/31 18:05:39  mk
  - implemented is_emailaddress in NameRead
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)
  MY+JG:- new function is_mailaddress, also implemented in all
          functions and procedures involved (multi_Mailstring and
          xp9_setclientFQDN in xp9sel.pas, NameRead in xp9.inc and
          get_first_box in xp9.pas)
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.13  2001/07/31 17:12:20  mk
  - implemented is_emailaddress in get_first_box

  Revision 1.12  2001/07/29 13:58:23  ma
  - removed nt_UUCP_U, some fixes

  Revision 1.11  2001/07/29 12:59:02  ma
  - cleaned up server config dialog
  - removed ntAllowed variable

  Revision 1.10  2001/07/28 12:04:15  mk
  - removed crt unit as much as possible

  Revision 1.9  2001/07/27 18:35:11  mk
  JG+MY:- RFC/Client: implemented check for valid (multiple) eMail addresses
          under Edit/Servers/Edit/Mail/News_Servers/Envelope_address (In+Out)
  JG+MY:- RFC/Client: removed unnecessary mask test in _EditPPP
  JG+MY:- RFC/UUCP: improved check for valid eMail address under
          Edit/Servers/Edit/Names/eMail_address
  VS: ----------------------------------------------------------------------

  Revision 1.8  2001/07/27 18:14:40  mk
  - use AddDirSepa instead of own function

  Revision 1.7  2001/07/27 18:10:14  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.6  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.5  2001/07/22 21:06:22  mk
  - fixed crash in get_first_box: email is ansistring, not shortstring

  Revision 1.3  2001/07/21 16:02:11  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.2  2001/07/20 21:29:22  mk
  - Vertreterauswahl doesn't change System settings anymore,
    TempRec saves global BoxPar in procedure BoxSelProc

  Revision 1.1  2001/06/04 17:42:03  ma
  - renamed, was xp9
  - implemented role feature

  --- renamed, was xp9.pas
  Revision 1.61  2001/05/19 16:12:53  ma
  - removed XP_ID (shareware notice)

  Revision 1.60  2001/04/17 00:19:37  ma
  - fixed crash occurring with screen lines > 50+-c

  Revision 1.59  2001/03/21 19:17:07  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output
}
end.


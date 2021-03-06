{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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
  typeform, //atext
  maske,    //customrec
  xpnt,     //types
  datadef;  //DB

const
    maxboxen = 127;         { max. Groesse des Arrays 'boxlist' }
var  own_Nt    : eNetz = nt_Any; // byte = 255; ???
        { Netztyp f. "Zusaetzliche Server" (RFC/Client) bzw. "AKAs/Pakete mitsenden" (Fido) }
      own_Name  : string = '';
        { Boxname f. "Zusaetzliche Server" (RFC/Client) bzw. "AKAs/Pakete mitsenden" (Fido) }
      showErrors: boolean = true;
        { Flag fuer 'addServersTest' in xp9sel.pas }
      BfgToBoxOk: boolean = true;
        { Flag fuer 'ChkAddServers' in xp7.inc }
      maxbox    : byte = maxboxen;
        { max. Boxen-Anzahl in Box-Config bzw. NETCALL.DAT }

      delete_on_cDel  : boolean = false; { Steuerung des Verhaltens...   }
      leave_on_cDel   : boolean = false; { ... bei <Ctrl-Del> in Feldern }
      may_insert_clip : boolean = true;  { Clipboard in Felder (nicht) einfuegen }

type
  { Typ :  Boxen, Gruppen, Systeme, Kurznamen, MIME-Typen }
  TUniSelType = (usBoxes, usGroups, usSystems, usShortNames, usMIMETypes);

function  Netz_Typ(nt:eNetz):string;
function  UniSel(UniSelType: TUniSelType; edit:boolean; const default:string):string;
procedure BoxSelProc(var cr:customrec);
procedure GruppenSelproc(var cr:customrec);

procedure get_first_box(d:DB);

procedure SetUsername(s:string);

procedure SelSchab(var cr:CustomRec);
function  zidtest(var s:string):boolean;
function  toggleSysop(var s:string):boolean;
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

function  PPPClientPathTest(var s:string):boolean;
function  PPPClientTest(var s:string):boolean;
function  multi_Mailstring(var s:string):boolean;
function  check_envelope(var s:string):boolean;
function  IsMailAddress(const s:string):boolean;
function  ReadExtCfgFilename(txt:atext; var s1:string; var cdir: String; subs:boolean):boolean;
procedure EditAddServersList(var cr:customrec);
procedure SingleServerSel(var cr:customrec);
procedure set_AddServers_Allowances(var s:string);
procedure set_ExtCfg_Allowances;
procedure reset_Allowances(var s:string);
function  addServersTest(var s:string):boolean;
function  BfgToBox(s:string):string;
function  BoxToBfg(var s:string):string;

implementation  {---------------------------------------------------}

uses
  sysutils,
  {$IFDEF NCRT}xpcurses,{$ENDIF}
  {$IFDEF unix}xpunix,{$ENDIF}
  {$IFDEF Win32}xpwin32,{$ENDIF }
  fileio,inout,keys,winxp,win2,database,maus2,mouse,resource,fidoglob,lister,
  xp0,xp1,xp1o,xp1input,xp2,xp2c,xp3,xp3o,xp9bp, classes, xpconst,
  xpmodemscripts,
  addresses,
  xpglobal;

const umtyp : array[0..5] of string[5] =
              ('IBM','ASCII','ISO','Tab.1','Tab.2','Tab.3');

{$IFNDEF DOS32}
      SupportedNetTypes: array[0..7] of eNetz =
        (nt_Client, nt_POP3, nt_IMAP, nt_NNTP, nt_UUCP, nt_Fido, nt_ZConnect, nt_Maus);
{$ELSE}
      SupportedNetTypes: array[0..4] of eNetz =
        (nt_Client, nt_UUCP, nt_Fido, nt_ZConnect, nt_Maus);
{$ENDIF}

var   UpArcnr   : integer;    { fuer EditPointdaten }
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

//todo: remove dupes?
      DomainNt  : eNetz;  //shortint;   { Netztyp f. setdomain() und testvertreterbox() }
      bDomainNt : eNetz;  //byte;                                                { u.a. }
      EditPnt   : eNetz;       { Netztyp f. EditPointdaten }
      
      EMSIfield : integer;
      pp_da     : boolean;    { unversandte Nachrichten vorhanden }
      amvfield  : integer;    { EditDiverses }
      MailInServerFld : integer; { Name MailInServer RFC/Client }
      SDB_nn : shortint = 1;  //static in SetDefaultBox


function CreateServerFilename(d: db; nt:eNetz; const boxname:string):string;
var fa : TFTNAddress; i: integer;
begin
  if (nt=nt_Fido) or ((nt=nt_QWK) and multipos(_MPMask,boxname)) then begin
    fa := TFTNAddress.Create(boxname,0);
    try
      result:=formi(fa.net mod 10000,4)+formi(fa.node mod 10000,4);
    finally
      fa.free;
    end;
  end
  else begin
    result:='';
    for i:=1 to length(boxname)do
      if UpCase(boxname[i]) in ['0'..'9','A'..'Z', '_', '-'] then
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
    rfehler(903);    { 'Die Seriennummer muss 4 Zeichen lang sein.' }
    zidtest:=false;
    end;
end;


function toggleSysop(var s:string):boolean;   { Sysop-Mode on/off }
var b   : boolean;
    i,j : byte;
begin
  b:=s=_jn_[1];
  j:=6;
  if own_Nt in [nt_Netcall,nt_Fido,nt_QWK] then j:=7;
  for i:=2 to j do setfieldenable(i,b);
  toggleSysop:=true;
end;

function validfile(var s:string):boolean;     { Sysop-Mode }
begin
  if (trim(s)<>'') and not ValidFilename(s) then begin
    rfehler(904);    { 'ungueltiger Dateiname' }
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

function SetArcExtension(s: String; nr: Integer): String;
var
  Ext: String;
begin
  s:=LowerCase(s);
  ext:='*';
  if (LeftStr(s,5)='pkarc') or (LeftStr(s,5)='pkpak') then ext:='arc'
  else if LeftStr(s,3)='lha' then ext:='lzh'
  else if LeftStr(s,5)='pkzip' then ext:='zip'
  else if LeftStr(s,3)='arj' then ext:='arj'
  else if LeftStr(s,3)='rar' then ext:='rar'
  else if (LeftStr(s,4)='copy') and (getfield(nr)<>'txt') then ext:='';
  if ext<>'*' then SetField(nr, ext);
end;

procedure set_uparcext(var s:string);
begin
  if UpArcNr > 0 then
    SetArcExtension(s, UpArcNr);
end;

procedure set_downarcext(var s:string);
begin
  if DownArcNr > 0 then
    SetArcExtension(s, DownArcNr);
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
var fa : TFTNAddress;
begin
  result:=false;
  if trim(s)='' then errsound
  else begin
    fa := TFTNAddress.Create(s,DefaultZone);
    try
      if (fa.Net=0)and(fa.Node=0) then
        ErrSound
      else begin
        fa.Point := 0;
        s := fa.FidoAddr;
        result := true;
      end;
    finally
      fa.Free;
    end;
  end;
end;

procedure setfidoadr(var s:string);   { Gruppen-Adresse }
var fa : TFTNAddress;
begin
  fa := TFTNAddress.Create(s,DefaultZone);
  try
    s := fa.FidoAddr;
  finally
    fa.Free;
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
            rfehler(908);               { 'ungueltige Adresse' }
            testreplyto:=false;
            end;
        end
      else begin
        rfehler(908);     { 'ungueltige Adresse' }
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

function DefaultMaps(nt:eNetz):string;
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

  rfehler(909);    { 'Mindestens ein Protokoll muss eingeschaltet sein!' }
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
      if (bDomainNt<>nt_Netcall) and (getfield(fieldpos+1)='') then
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

{ Fileserver: Feldbezeichnung aendern }

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
var
  nt: eNetz;
begin
  if s='' then
    Result :=true
  else
  begin
    if SeekLeftBox(s, nt) then
    begin
      if fieldpos=amvfield then    { AM-Vertreterbox }
        Result := (DomainNt=nt)
      else                         { PM-Vertreterbox }
        Result := ntAdrCompatible(DomainNt,nt);
      if not Result then 
        rfehler(2713);
    end else
    begin
      rfehler(2702);    { 'unbekannte Serverbox - waehlen mit <F2>' }
      Result := false;
    end;
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
var
  fn : string;
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
      rfehler(928);         { 'ungueltiger Dateiname!' }
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


{ ZCONNECT-Pointname auf ungueltige Zeichen ueberpruefen }

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
    rfehler1(930,us);    { 'Warnung: Ungueltige Zeichen im Pointname: %s' }
  testZCpointname:=true;  { (us=''); }
end;

{ Typ :  1=Boxen, 2=Gruppen, 3=Systeme, 4=Kurznamen, 5=MIME-Typen }
{ edit:  true=editieren, false=nur auswaehlen                      }


var //static or dynamic initialization?
    edb_pos : shortint = 1;
    lastclient : boolean = false;

function  UniSel(UniSelType: TUniSelType; edit:boolean; const default:string):string;
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
    startmkey : boolean;   { beim Start war Maustaste gedrueckt }
    directsel : string;
    nameofs   : byte;

  procedure displine(i:integer);
  var s1,s2,s3: string;
      limit,grnr : longint;
      w          : smallword;
      hd,sig,qt  : char;
      qm         : string[8];
      nt        : eNetz;
      b         : byte;
      dc         : string[2];
      adr        : string;
  begin
    drec[i]:=dbRecno(d);
    case UniSelType of
      usBoxes : s1 := dbReadStr(d,'Boxname');
      usGroups : s1 := dbReadStr(d,'Name');
    end;
    if setdefault and (UpperCase(s1)=UpperCase(default)) then begin
      p:=i;
      setdefault:=false;
    end;
    case UniSelType of
      usBoxes: begin     { Boxen }
            dbRead(d,'Netztyp',nt);
            if nt in netsRFC then
              s2 := ComputeUserAddress(d)
            else
              s2 := dbReadStr(d,'Username');
            s3 := dbReadStr(d,'Kommentar');
            if s1=DefaultBox then begin
              if s1=DefFidoBox then dc:='F '
            {$IFDEF Unix }
              else dc:='* '
            {$ELSE }
              else dc:='� '
            {$ENDIF }
            end else if s1=DefFidoBox then dc:='f '
            else dc:='  ';
            s:=dc+forms(s1,11)+' '+forms(Netz_Typ(nt),12)+forms(s2,17)+' '+
               forms(s3,23);
          end;
      usGroups: begin     { Gruppen }
            dbRead(d,'msglimit',limit);
            dbRead(d,'int_nr',grnr);
            dbRead(d,'umlaute',umlaut);
            hd:=iifc(UpperCase(dbReadStr(d,'kopf')+ extXps)<>UpperCase(headerfile),'K',' ');
            qm:=dbReadStr(d,'quotemsk');
            qt:=iifc((qm<>'') and (UpperCase(qm+extXps)<>UpperCase(quotemsk)),'Q',' ');
            sig:=iifc(UpperCase(dbReadStr(d,'signatur')+ extXps)<>UpperCase(signatfile),'S',' ');
            s:=strsn(grnr,5)+' '+hd+qt+sig+' '+forms(s1,28)+' '+
               forms(umtyp[umlaut],6)+
               iifs(limit>0,strsrnp(limit,12,0),sp(11)+' �')+' ';
          end;
      usSystems: begin     { Systeme }
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
      usShortNames : begin     { Kurznamen }
            s1  := dbReadStr(d,'kurzname');
            adr := dbReadStr(d,'langname');
            s2  := dbReadStr(d,'pollbox');
            s:=' '+forms(s1,12)+' '+forms(adr,36)+' '+forms(s2,12);
          end;
      usMIMETypes : begin     { MIME-Typen }
            s1 := dbReadStr(d,'typ');
            s2 := dbReadStr(d,'extension');
            s3 := dbReadStr(d,'programm');
            if s3='' then s3:=getres(934)    { '(intern)' }
            else if length(s3)>31 then s3:=LeftStr(s3,31)+'...';
            s1:=extmimetyp(s1);
            if LeftStr(s1,12)='application/' then s1:='appl.'+mid(s1,12);
            s:=' '+forms(s1,26)+' '+forms(s2,6)+forms(s3,31);
          end;
    end;  //case
    if not setdefault and (i=p) then attrtxt(col.colsel2bar)
    else attrtxt(col.colsel2box);
    fwrt(x+1,y+i,s);
  end;

  procedure display;
  var i : integer;
      b : boolean;
  begin
    moff;
    if drec[1]=0 then begin
      dbGoTop(d); b:=true;
    end else begin
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
    fwrt(x,y+1,iifc(b,'�',#30));
    fwrt(x,y+gl,iifc(dbEOF(d),'�',#31));
    if i=1 then begin
      attrtxt(col.colsel2bar);
      fwrt(x+1,y+1,sp(width));
    end;
    aufbau:=false;
    p0:=p;
    mon;
  end;

  {$I xpconfigedit-servers.inc} //> 2000 lines

  procedure ReadGruppe(edit:boolean; var name:string; var hzeit:integer16;
                       var limit:longint; var umlaut:byte; var hd,qt,sig,qt2,pmhd,pmqt,pmsig, qstring:string;
                       var flags:byte; var brk:boolean);
  const fname = '1234567890$_-';

    function retypes(nr:byte):string;
    begin
      retypes:=getres2(901,15+nr);
    end;

  var x,y,i : Integer;
      ums   : string;
      ss    : string;
      retyp : string;  { Re^n / Re / Default / nein }
  begin
    dialog(ival(getres2(901,0)),14,getres2(901,iif(edit,1,2)),x,y);    { 'Brettgruppe bearbeiten','neue Brettgruppe anlegen' }

    if not odd(flags) then begin
      maddstring(2,2,getres2(901,3),name,30,30,''); mhnr(201);   { 'Name    ' }
      msetvfunc(notempty);
    end else begin
      maddtext(2,2,getres2(901,4),col.coldialog);      { 'Name' }
      maddtext(12,2,name,col.coldiahigh);
    end;
    maddint   (2,4,getres2(901,5),limit,6,8,0,99999999); mhnr(202);   { 'Limit   ' }
    maddtext  (length(getres2(901,5))+14,4,getres(13),col.coldialog);
    maddint   (2,6,getres2(901,6),hzeit,4,4,0,9999);   { 'Halten: ' }
    maddtext  (length(getres2(901,6))+11,6,getres2(901,7),col.coldialog);   { 'Tage' }
    ums:=umtyp[umlaut];
    maddstring(2,7,getres2(901,8),ums,5,5,'');         { 'Sonderz.' }
    for i:=0 to 1 do
      mappsel(true,umtyp[i]);
    retyp:=retypes((flags and 6) shr 1);
    maddstring(2,8,getres2(901,20),retyp,7,7,'');      { 'Replies ' }
    for i:=0 to 3 do
      mappsel(true,retypes(i));

    maddstring(23,8,GetRes2(901,30),qstring,4,20,range(' ',#126)); {'Quote-Zeichen'}
    mset3proc(xp2c.testqc);
    mnotrim; mhnr(210);

    ss:=range('A','Z')+range('a','z')+fname;
    maddtext(3,10,GetRes2(901,13),0);  { 'Brettantworten' }
    maddstring(2,11,getres2(901,9),hd,8,8,ss); mhnr(206);   { 'Kopf    ' }
    mappcustomsel(SelSchab,false);
    maddstring(2,12,getres2(901,10),qt,8,8,ss); mhnr(206);  { 'Quote   ' }
    mappcustomsel(SelSchab,false);
    maddstring(2,13,getres2(901,12),qt2,8,8,ss); mhnr(206); { 'QuoteTo ' }
    mappcustomsel(SelSchab,false);
    maddstring(2,14,getres2(901,11),sig,8,8,ss); mhnr(206); { 'Signatur' }
    mappcustomsel(SelSchab,false);

    maddtext(24,10,GetRes2(901,55),0);  { 'PM-Antworten' }
    maddstring(23,11,getres2(901,50),pmhd,8,8,ss); mhnr(206);  { 'Kopf    ' }
    mappcustomsel(SelSchab,false);
    maddstring(23,12,getres2(901,51),pmqt,8,8,ss); mhnr(206);  { 'Quote   ' }
    mappcustomsel(SelSchab,false);
    maddstring(23,14,getres2(901,52),pmsig,8,8,ss); mhnr(206); { 'Signatur' }
    mappcustomsel(SelSchab,false);

    readmask(brk);

    if not brk then begin
      for i:=0 to 5 do
        if UpperCase(ums)=UpperCase(umtyp[i]) then umlaut:=i;
      flags:=flags and (not 6);
      LoString(retyp);
      if retyp=LowerCase(retypes(1)) then inc(flags,2)        { re^n: }
      else if retyp=LowerCase(retypes(2)) then inc(flags,4)   { re:   }
      else if retyp=LowerCase(retypes(3)) then inc(flags,6);  { nein  }
    end;
    enddialog;
    freeres;
  end;

  procedure NeueGruppe;
  var name   : string;
      hd,sig : string;
      qt,qt2 : string;
      pmhd   : string;
      pmqt   : string;
      pmsig  : string;
      qstring: string;
      hzeit  : integer16;
      limit  : longint;
      umlaut : byte;
      flags  : byte;
      brk    : boolean;
  begin
    name:=''; hzeit:=stdhaltezeit; limit:=MaxNetMsgs;
    hd:='header'; sig:='signatur'; qt:='qbrett'; qt2:='quoteto';
    hd:='privhead'; pmsig:='privsig'; pmqt:='qpriv';
    qstring := '';

    umlaut:=0;   { IBM-Umlaute, keine Konvertierung }
    flags:=0;    { keine Standard-Gruppe; Re^n: Default }
    readgruppe(false,name,hzeit,limit,umlaut,hd,qt,sig,qt2,pmhd,pmqt,pmsig,qstring,flags,brk);
    if not brk then begin
      dbSeek(d,giName,UpperCase(name));
      if dbFound then
        rfehler(910)   { 'Eine Gruppe mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWriteStr(d,'Name',name);
        dbWrite(d,'haltezeit',hzeit);
        dbWrite(d,'MsgLimit',limit);
        dbWrite(d,'umlaute',umlaut);
        dbWriteStr(d,'kopf',hd);
        dbWriteStr(d,'signatur',sig);
        dbWriteStr(d,'quotemsk',qt);

        dbWriteStr(d,'quotetomsk',qt2);
        dbWriteStr(d,'pmkopf',pmhd);
        dbWriteStr(d,'pmsignatur',pmsig);
        dbWriteStr(d,'pmquotemsk',pmqt);

        dbWriteStr(d,'quotechar',qstring);

        dbWrite(d,'flags',flags);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
      end;
    end;
  end;

  procedure EditGruppeAllgemein;
  var name   : string;
      hd,sig : string;
      qt,qt2 : string;
      pmhd   : string;
      pmqt   : string;
      pmsig  : string;
      qstring: string;
      hzeit  : integer16;
      limit  : longint;
      flags  : byte;
      umlaut : byte;
      brk    : boolean;
  begin
    dbGo(d,drec[p]);
    name:= dbReadStr(d,'Name');
    dbRead(d,'haltezeit',hzeit);
    dbRead(d,'MsgLimit',limit);
    dbRead(d,'flags',flags);
    dbRead(d,'umlaute',umlaut);
    hd:= dbReadStr(d,'kopf');
    sig:= dbReadStr(d,'signatur');
    qt:= dbReadStr(d,'quotemsk');
    qt2 := dbReadStr(d,'quotetomsk');
    pmhd := dbReadStr(d,'pmkopf');
    pmsig := dbReadStr(d,'pmsignatur');
    pmqt := dbReadStr(d,'pmquotemsk');
    qstring := dbReadStr(d,'quotechar');    
    readgruppe(true,name,hzeit,limit,umlaut,hd,qt,sig,qt2,pmhd,pmqt,pmsig,qstring,flags,brk);
    if not brk then begin
      dbWriteStr(d,'Name',name);
      dbWrite(d,'haltezeit',hzeit);
      dbWrite(d,'MsgLimit',limit);
      dbWrite(d,'Umlaute',umlaut);
      dbWriteStr(d,'kopf',hd);
      dbWriteStr(d,'signatur',sig);
      dbWriteStr(d,'quotemsk',qt);
      dbWrite(d,'flags',flags);

      dbWriteStr(d,'quotetomsk',qt2);
      dbWriteStr(d,'pmkopf',pmhd);
      dbWriteStr(d,'pmsignatur',pmsig);
      dbWriteStr(d,'pmquotemsk',pmqt);

      dbWriteStr(d,'quotechar',qstring);

      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
    end;
  end;

  procedure EditGruppeFido;
  var x,y  : Integer;
      brk  : boolean;
      orig : string;
      addr : string;
  begin
    dbGo(d,drec[p]);
    orig:= dbReadStr(d,'origin');
    addr:= dbReadStr(d,'adresse');
    dialog(46,5,getres2(902,1),x,y);    { 'Fido-Einstellungen' }
    maddstring(3,2,getres2(902,2),orig,32,48,range(' ',#126)); mhnr(690);   { 'Origin ' }
    maddstring(3,4,getres2(902,3),addr,15,15,'');   { 'Adresse' }
    mset3proc(setfidoadr);
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWriteStr(d,'origin',orig);
      dbWriteStr(d,'adresse',addr);
      dbFlushClose(d);
    end;
  end;

  procedure EditGruppeRFC;
  var x,y  : Integer;
      brk  : boolean;
      AMRealname,AMMail,AMReplyTo,AMFQDN : string;
      PMRealname,PMMail,PMReplyTo,PMFQDN : string;
  begin
    dbGo(d,drec[p]);
    AMRealname:= dbReadStr(d,'amrealname');
    AMMail:= dbReadStr(d,'ammail');
    AMReplyTo:= dbReadStr(d,'amreplyto');
    AMFQDN:= dbReadStr(d,'amfqdn');
    PMRealname:= dbReadStr(d,'pmrealname');
    PMMail:= dbReadStr(d,'pmmail');
    PMReplyTo:= dbReadStr(d,'pmreplyto');
    PMFQDN:= dbReadStr(d,'pmfqdn');
    dialog(49,15,getres2(902,10),x,y);    { 'RFC-Einstellungen' }
    maddtext(3,2,getres2(902,11),col.coldiahigh);     { 'oeffentliche Nachrichten' }
    maddstring(3,4,getres2(902,13),AMRealname,32,40,''); mhnr(695);   { 'Realname' }
    maddstring(3,5,getres2(902,14),AMMail,32,80,range(' ',#126));     { 'E-Mail  ' }
    maddstring(3,6,getres2(902,15),AMReplyTo,32,80,range(' ',#126));  { 'Reply-To' }
    maddstring(3,7,getres2(902,16),AMFQDN,32,60,range(' ',#126));     { 'FQDN    ' }
    maddtext(3,9,getres2(902,12),col.coldiahigh);     { 'private Nachrichten' }
    maddstring(3,11,getres2(902,13),PMRealname,32,40,''); mhnr(695);  { 'Realname' }
    maddstring(3,12,getres2(902,14),PMMail,32,80,range(' ',#126));    { 'E-Mail  ' }
    maddstring(3,13,getres2(902,15),PMReplyTo,32,80,range(' ',#126)); { 'Reply-To' }
    maddstring(3,14,getres2(902,16),PMFQDN,32,60,range(' ',#126));    { 'FQDN    ' }
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWriteStr(d,'amrealname',AMRealname);
      dbWriteStr(d,'ammail',AMMail);
      dbWriteStr(d,'amreplyto',AMReplyTo);
      dbWriteStr(d,'amfqdn',AMFQDN);
      dbWriteStr(d,'pmrealname',PMRealname);
      dbWriteStr(d,'pmmail',PMMail);
      dbWriteStr(d,'pmreplyto',PMReplyTo);
      dbWriteStr(d,'pmfqdn',PMFQDN);
      dbFlushClose(d);
    end;
  end;

  procedure DelGruppe;
  var grnr  : longint;
      flags : byte;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'flags',flags);
    if odd(flags) then
      rfehler(911)       { 'Gruppe kann nicht geloescht werden!' }
    else begin
      dbRead(d,'INT_NR',grnr);
      dbSetindex(bbase,biGruppe);
      dbSeek(bbase,biGruppe,dbLongStr(grnr));
      if dbFound then
        rfehler(912)     { 'Es sind noch Bretter in dieser Gruppe vorhanden.' }
      else begin
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
      end;
    end;
  end;

  procedure addhzeit(add:integer);
  var hzeit : integer16;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'haltezeit',hzeit);
    hzeit:=max(0,min(hzeit+add,9999));
    dbWrite(d,'haltezeit',hzeit);
    displine(p);
  end;


  procedure EditGruppe;
  //const edb_pos : shortint = 1; - static or dynamic initialization?
  var n   : shortint;
      nts : string;
  begin
    pushhp(207);
    nts:=getres2(901,21); { ' ^Allgemein,^RFC,^Fido ' }
    n:=MiniSel(x+10,min(y+p+1,screenlines-8),'',nts,edb_pos);
    freeres;
    if n<>0 then edb_pos:=abs(n);
    if n>0 then
      case n of
        1: EditGruppeAllgemein;
        2: EditGruppeRFC;
        3: EditGruppeFido;
      end;
    pophp;
  end;

  {.$I xpconfigedit-systems.inc}

  procedure ReadSystem(var name,komm,fs_name,fs_passwd,converter:string;
                       fs_typ:byte; var brk:boolean);
  var
    x,y: Integer;
  begin
    dialog(ival(getres2(903,0)),11,getres2(903,iif(edit,1,2)),x,y);    { 'Systeme bearbeiten','neues System anlegen' }
    maddstring(3,2,getres2(903,3),name,BoxNameLen,BoxNameLen,'>'); mhnr(461);   { 'Systemname ' }
    mappcustomsel(BoxSelProc,false);
    msetvfunc(testsysname);
    maddstring(3,4,getres2(903,4),komm,30,30,'');       { 'Kommentar  ' }
    maddstring(3,6,getres2(903,5),fs_name,20,20,'');    { 'Fileserver ' }
    mappsel(false,'FILESERVER�'+uuserver);
    mset3proc(setPasswdField);
    maddstring(3,8,getres2(903,iif(fs_typ=3,7,6)),fs_passwd,20,20,'');  { 'Index-Datei' / 'Passwort    ' }
    maddstring(3,10,getres2(903,8),converter,30,60,'>');  { 'Konvertierer' }
    mappsel(false,'UUCP-FL1.EXE $INFILE $OUTFILE�COPY $INFILE $OUTFILE');
    readmask(brk);
    freeres;
    if not brk then
      if UpperCase(fs_name)<>UpperCase(uuserver) then
        UpString(fs_name)
      else begin
        if fs_passwd='' then fs_passwd:='index';
        if converter='' then converter:='COPY $INFILE $OUTFILE';
      end;
    enddialog;
  end;

  procedure NeuesSystem;
  var name   : string;
      komm   : string;
      fsuser : string;
      fspass : string;
      convert: string;
      brk    : boolean;
      w      : xpWord;
      b      : byte;
  begin
    name:=''; komm:='';
    fsuser:=''; fspass:='';
    convert:='';
    readsystem(name,komm,fsuser,fspass,convert,0,brk);
    if not brk then begin
      dbSeek(d,siName,UpperCase(name));
      if dbFound then
        rfehler(913)     { 'Ein System mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWriteStr(d,'Name',name);
        dbWriteStr(d,'Kommentar',komm);
        dbWriteStr(d,'fs-name',fsuser);
        dbWriteStr(d,'fs-passwd',fspass);
        dbWriteStr(d,'ZBV1',convert);
        w:=iif(fsuser<>'',1,0);
        dbWrite(d,'flags',w);
        b:=iif(UpperCase(fsuser)=UpperCase(uuserver),3,0);
        dbWrite(d,'fs-typ',b);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
      end;
    end;
  end;

  procedure EditSystem;
  var name   : string;
      komm   : string;
      fsuser : string;
      fspass : string;
      convert: string;
      brk    : boolean;
      w      : xpWord;
      typ    : byte;
  begin
    dbGo(d,drec[p]);
    name:= dbReadStr(d,'Name');
    komm:= dbReadStr(d,'Kommentar');
    fsuser:= dbReadStr(d,'fs-name');
    fspass:= dbReadStr(d,'fs-passwd');
    dbRead(d,'fs-typ',typ);
    convert:= dbReadStr(d,'ZBV1');
    readsystem(name,komm,fsuser,fspass,convert,typ,brk);
    if not brk then begin
      dbWriteStr(d,'Name',name);
      dbWriteStr(d,'Kommentar',komm);
      dbWriteStr(d,'fs-name',fsuser);
      dbWriteStr(d,'fs-passwd',fspass);
      dbWriteStr(d,'ZBV1',convert);
      w:=iif(fsuser<>'',1,0);
      dbWrite(d,'flags',w);
      if UpperCase(fsuser)=UpperCase(uuserver) then typ:=3
      else if typ=3 then typ:=0;
      dbWrite(d,'fs-typ',typ);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
    end;
  end;

  procedure DelSystem;
  begin
    if dbRecCount(d)<2 then
      rfehler(914)    { 'Es muss mindestens ein System eingetragen sein!' }
    else begin
      dbGo(d,drec[p]);
      if ReadJN(getreps(904,dbReadStr(d,'name')),true) then begin   { '%s l�schen' }
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
      end;
    end;
  end;

  procedure ReadPseudo(edit:boolean; var kurz,lang,pollbox:string;
                       var brk:boolean);
  var
    x,y: Integer;
  begin
    dialog(ival(getres2(905,0)),7,getres2(905,iif(edit,1,2)),x,y);   { 'Kurzname bearbeiten' / 'Kurzname anlegen' }
    maddstring(3,2,getres2(905,3),kurz,15,15,without(allchar,'@')); mhnr(711);   { 'Kurzname   ' }
    msetvfunc(notempty);
    maddstring(3,4,getres2(905,4),lang,35,79,iifs(ntZonly and not smallnames,'>',''));   { 'Brett/User ' }
    mappcustomsel(Auto_Empfsel,false);
    mset3proc(ps_setempf);
    maddstring(3,6,getres2(905,5),pollbox,BoxRealLen,BoxNameLen,'');   { 'Server     ' }
    mappcustomsel(BoxSelProc,false);
    freeres;
    readmask(brk);
    enddialog;
  end;

  { Pseudo editieren und anlegen. Funktion 'NeuesPseudo' gibt es nicht mehr  }
  procedure EditPseudo(isNew: boolean);
  var kurz    : string;
      lang    : string;
      pollbox : string;
      brk     : boolean;
  begin
    if isNew then begin
      kurz:='';
      lang:='';
      pollbox:='';
    end else begin
      dbGo(d,drec[p]);
      kurz:= dbReadStr(d,'Kurzname');
      lang:= dbReadStr(d,'Langname');
      pollbox:= dbReadStr(d,'pollbox');
    end;
    readpseudo(true,kurz,lang,pollbox,brk);
    if not brk then begin
      if isNew then begin
        dbSeek(d,piKurzname,UpperCase(kurz));
        if dbFound then begin
          rfehler(915);     { 'Diesen Kurznamen gibt es bereits.' }
          exit;
        end;
        dbAppend(d);
      end; { isNew }
      dbWriteStr(d,'Kurzname',kurz);
      dbWriteStr(d,'Langname',lang);
      dbWriteStr(d,'pollbox',pollbox);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      if isNew then
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
      aufbau:=true;
    end;
  end;

  procedure DelPseudo;
  begin
    dbGo(d,drec[p]);
    if ReadJN(getreps(906,dbReadStr(d,'kurzname')),true) then begin   { '"%s" loeschen' }
      dbDelete(d);
      dbFlushClose(d);
      if p=1 then dbGoTop(d)
      else dbGo(d,drec[1]);
      aufbau:=true;
    end;
  end;

  procedure ReadMimetyp(edit:boolean; var typ,ext,prog:string;
                        var brk:boolean);
  var
    x,y,add: Integer;
  begin
    typ:=extmimetyp(typ);
    add:=iif(typ='*/*',0,2);
    dialog(ival(getres2(935,0)),5+add,getres2(935,iif(edit,2,1)),x,y);  { 'Viewer aendern' / 'Viewer hinzufuegen' }
    if typ='*/*' then begin
      maddtext(3,2,getres2(935,3),0);                  { 'MIME-Typ         ' }
      maddtext(3+length(getres2(935,3))+2,2,typ,col.coldiahigh);
    end else begin
      maddstring(3,2,getres2(935,3),typ,33,40,         { 'MIME-Typ         ' }
           '"!'+without(range('#','~'),'()<>@,;:\"[]?=')); { MK 12/99 Zeichen "/" zugelassen }
        mhnr(821); {JG: 1051->821}
      maddstring(3,4,getres2(935,4),ext,5,5,'<');              { 'Dateierweiterung ' }
      mhnr(822); {JG}
    end;
    maddstring(3,4+add,getres2(935,5),prog,33,ViewprogLen,''); mhnr(823); {JG} { 'Viewer-Programm  ' }
    msetvfunc(testexecutable);
    freeres;
    repeat
      readmask(brk);
      if not brk and (typ+ext='') then
        rfehler(932);    { 'Es muss ein MIME-Typ oder eine Dateierweiterung angegeben werden!' }
    until brk or (typ+ext<>'');
    enddialog;
    typ:=compmimetyp(typ);
  end;

  // procedure SortMIMETypes;  removed in Rev. 1.54

  procedure EditMimetyp(isNew: boolean);
  var typ  : string;
      ext  : string;
      prog : string;
      brk  : boolean;
      isValid: boolean;
  begin
    if isNew then begin
      typ:= ''; ext:= ''; prog:= '';
    end else begin
      dbGo(d,drec[p]);
      typ:= dbReadNStr(d,mimeb_typ);
      ext:= dbReadNStr(d,mimeb_extension);
      prog:= dbReadNStr(d,mimeb_programm);
    end;
    if typ = '*/*' then begin
      RFehler(935); // 'Standardeintrag kann nicht editiert werden'
      exit;
    end;
    readmimetyp(not isNew,typ,ext,prog,brk);
    if not brk then begin
      {  check for duplicate entries }
      isValid := true;
      if typ <> '' then begin
        dbSeek(mimebase,mtiTyp,UpperCase(typ));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(typ,dbReadStr(mimebase,'typ')));
      end;
      if Ext <> '' then begin
        dbSeek(mimebase,mtiExt,UpperCase(Ext));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := isValid and not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(ext,dbReadStr(mimebase,'extension')));
      end;
      if not IsNew and (typ = '*/*') then IsValid := true;

      if isValid then begin
        if isNew then
          dbAppend(d)
        else
          dbGo(d,drec[p]);
        dbWriteNStr(d,mimeb_typ,typ);
        dbWriteNStr(d,mimeb_extension,ext);
        dbWriteNStr(d,mimeb_programm,prog);
      end else
        RFehler(934); { Doppelte MIME-Typen oder Dateierweiterungen sind nicht erlaubt! }
      dbFlushClose(d);
      dbGo(d,drec[1]);
      if isNew then
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
      aufbau:=true;
    end;
  end;

  procedure DelMimetyp;
  var
    s     : string;
  begin
    dbGo(d,drec[p]);
    s:=dbReadStr(d,'typ');
    if s='*/*' then
      rfehler(931)          { 'Standardeintrag kann nicht geloescht werden' }
    else begin
      if s='' then s:=dbReadStr(d,'extension');
      if ReadJN(getreps(906,s),true) then begin   { '"%s" loeschen' }
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
      end;
    end;
  end;

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
      end else if t=mausldouble then begin
        rb:=edb; t:=keycr;
      end
    end;
    if outside then begin
      if (t=mausleft) or (t=mausright) or (t=mauslmoved) or (t=mausrmoved) then
        poutside:=true else
      if poutside and ((t=mausunleft) or (t=mausunright)) then begin
        rb:=okb; t:=keyesc;
      end;
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
    case UniSelType of
      usBoxes: nfeld:='boxname';
      usGroups: nfeld:='name';
      usSystems: nfeld:='name';
      usShortNames: nfeld:='kurzname';
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
//really strange Delphi warning here: Result might be undefined?
  Result := ''; //should be sufficient, but the following 2 assignments also are required.

  case UniSelType of
    usBoxes: begin     { Boxen }
          d := Boxbase;
          if not edit and (dbRecCount(boxbase)=1) and (lastkey<>keyf2) then
          begin
            dbGoTop(boxbase);
            unisel:=dbReadStr(boxbase,'boxname');
            exit;
          end;
          width:=67;
          buttons:=getres(907);   { ' ^Neu , ^Loeschen , ^Waehlen , ^Edit , Netz^typ , ^OK ' }
          okb:=6; edb:=4;
          pushhp(iif(edit,130,139));
          nameofs:=3;
        end;
    usGroups: begin     { Gruppen }
          dbOpen(d,GruppenFile,1);
          width:=59;
          buttons:=getres(908);   { ' ^Neu , ^Loeschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,200,209));
          nameofs:=11;
        end;
    usSystems: begin     { Systeme }
          dbOpen(d,SystemFile,1);
          width:=51;
          buttons:=getres(909);   { ' ^Neu , ^Loeschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,460,469));
          nameofs:=5;
        end;
    usShortNames: begin     { Kurznamen }
          dbOpen(d,PseudoFile,1);
          width:=63;
          buttons:=getres(909);   { ' ^Neu , ^Loeschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,710,719));
          nameofs:=2;
        end;
    usMimeTypes: begin     { MIME-Typen }
          d:=mimebase;
          width:=65;
          buttons:=getres(909);   { ' ^Neu , ^Loeschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(820);
          nameofs:=2;
        end;
  end;
  if not (UniSelType in [usMimeTypes, usBoxes]) then
    miscbase:=d;
  drec[1]:=0;
  gl:=screenlines-11;
  if screenlines>30 then dec(gl,2);
  if screenlines>40 then dec(gl,2);
  selbox(width+2,gl+4,'',x,y,false);

  p:=1; bp:=1; p0:=p;
  if not edit then inc(gl,2);
  if edit then begin
    attrtxt(col.colsel2rahmen);
    mwrt(x,y+gl+1,'�'+dup(width,'�')+'�');
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
      get(t,curoff);
      end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    c:=UpCase(t[1]);
    if not edit then
      _DirectSel
    else
      if rb>0 then
        case UniSelType of
          usBoxes: case rb of
                1 : NewBox;
                2 : if not empty then DelBox;
                3 : if not empty then SetDefaultBox;
                4 : if not empty then EditBox;
                5 : if not empty then EditNetztyp;
              end;
          usGroups: case rb of
                1 : NeueGruppe;
                2 : DelGruppe;
                3 : EditGruppe;
              end;
          usSystems: case rb of
                1 : NeuesSystem;
                2 : DelSystem;
                3 : EditSystem;
              end;
          usShortNames: case rb of
                1 : EditPseudo(true);
                2 : if not empty then DelPseudo;
                3 : if not empty then EditPseudo(false);
              end;
          usMimeTypes: case rb of
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
      if UniSelType= usGroups then
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
      case UniSelType of
        usBoxes: UniSel:=dbReadStr(d,'boxname');
        usGroups, usSystems: UniSel:=dbReadStr(d,'name');
        usShortNames: UniSel:=dbReadStr(d,'kurzname');   { Dummy }
      end;
    end;

  if not (UniSelType in [usMimeTypes, usBoxes]) then
  begin
    dbClose(d);
    miscbase:=nil; // see above, miscbase is set with different db
    end;
  closebox;
//if (typ = 1) and edit then
//  askRTA (false);
end;


{ fuer maske.CustomSel }


function Netz_Typ(nt:eNetz):string;
begin
  Result := ntName(nt);
end;


procedure BoxSelProc(var cr:customrec);
begin
  with cr do
  begin
    s:=UniSel(usBoxes,false,s);
    brk:=(s='');
  end;
end;


procedure GruppenSelproc(var cr:customrec);
begin
  with cr do begin
    s:=UniSel(usGroups, false,s);
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
    dbSeek(Boxbase,boiName,box);
    if not dbFound then
      rfehler1(918,box)   { 'SETUSER - Box "%s" unbekannt!' }
    else begin
      hasreal:=ntRealname(dbNetztyp(Boxbase));
      if user='' then begin
        user:=dbReadStr(boxbase,'username');
        real:=dbReadStr(Boxbase,'realname');
        dialog(length(getres(930))+length(box)+35,iif(hasreal,5,3),'',x,y);
        gross:=ntGrossUser(dbNetztyp(Boxbase));
        maddstring(3,2,getreps(930,box),user,30,30,iifs(gross,'>',''));   { 'Neuer Username fuer %s:' }
        mhnr(1502);
        if hasreal then
          maddstring(3,4,forms(getreps(931,box),length(getreps(930,box))),real,30,40,'');  { 'Neuer Realname:' }
        readmask(brk);
        enddialog;
        end
      else
        brk:=false;
      if not brk then begin
        dbWriteStr(Boxbase,'username',user);
        if hasreal { and (real<>'') 29.07.96 } then dbWriteStr(Boxbase,'realname',real);
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
    showusername;
    end;
end;


procedure gf_getntyp(var s:string);
var uucp,rfc : boolean;
begin
  rfc:=(LowerCase(s)=LowerCase(ntName(nt_Client)))or
       (LowerCase(s)=LowerCase(ntName(nt_NNTP))) or
       (LowerCase(s)=LowerCase(ntName(nt_IMAP))) or
       (LowerCase(s)=LowerCase(ntName(nt_POP3)));
  gf_fido:=(LowerCase(s)=LowerCase(ntName(nt_Fido)));
  uucp:=(LowerCase(s)=LowerCase(ntName(nt_UUCP)));
  setfieldtext(fieldpos+1,getres2(912,iif(rfc,13,2)));
  if (LowerCase(s)=LowerCase(ntName(nt_Maus))) or gf_fido or uucp or rfc then
    set_chml(userfield,'')
  else
    set_chml(userfield,'>');
  {if uucp or client then
    set_chml(fieldpos+1,'')
  else
    set_chml(fieldpos+1,'>');}
  setfieldtext(userfield,getres2(912,iif(rfc,12,3)));
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
      if DomainNt = nt_Any {<0} then nt:=LowerCase(getfield(1))   { Netztyp als String }
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
      own_name := s;
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
    nt   : eNetz;
    i,b : integer;
label restart;
begin
restart:
  dialog(ival(getres2(911,0)),13,'',x,y);
  maddtext(3,2,getres2(911,1),col.coldiahigh);    { 'Bitte geben Sie Netztyp und Name Ihrer Stamm-' }
  maddtext(3,3,getres2(911,2),col.coldiahigh);    { 'box sowie Username bzw. eMail-Adresse ein.' }
  maddtext(3,5,getres2(911,3),col.coldiahigh);    { 'Bei Einsatz des Netztyps RFC/Client benoetigen' }
  maddtext(3,6,getres2(911,4),col.coldiahigh);    { 'Sie einen externen Mail-/News-Client.' }
  name:=''; user:='';
  ntyp:=ntName(nt_Client); nt:=nt_Client;
  maddstring(3,8,getres2(911,5),ntyp,20,20,''); mhnr(681);   { 'Netztyp   ' }
  for i:=0 to High(SupportedNetTypes) do
    mappsel(true,ntName(SupportedNetTypes[i]));
  mset3proc(gf_getntyp);
  maddstring(3,10,getres2(912,13),name,20,20,'"!'+range('#','?')+range('A',#126)+'���');
    mhnr(680);                                       { 'Server' bzw. 'Boxname' }
  DomainNt:=nt_Any;  //-1;
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
    else      if nt <> nt_Client then boxpar^.pointname:=''
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
  WriteBox(nt,dname,boxpar);
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
    if LeftStr(fn, 2) = '.' + DirSepa then
      fn := Mid(fn, 3);
    TrimLastChar(fn, DirSepa);
    ok := (cPos(':', fn) = 0) and (cPos(DirSepa, fn) = 0) and (cPos('.', fn) < 2)
      and (Length(fn) > 0) and (LastChar(fn) <> '.');
    if not ok then
    begin              
      msgbox(62,6,_fehler_,x,y);
      mwrt(x+3,y+2,getres2(10900,37));   { 'Pfadangabe muss RELATIV sein und auf ein Verzeichnis EINE' }
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
        if not CreateDir(ExcludeTrailingPathDelimiter(s)) then
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
  if Pos('start /wait ', LowerCase(fn)) = 1 then fn := Mid(fn, 13);
  if Pos('start /wai ', LowerCase(fn)) = 1 then fn := Mid(fn, 12);
  if Pos('start /wa ', LowerCase(fn)) = 1 then fn := Mid(fn, 11);
  if Pos('start /w ', LowerCase(fn)) = 1 then fn := Mid(fn, 10);
  if cpos(' ',fn)>0 then fn:= LeftStr(fn,cpos(' ',fn)-1);
  if (fn<>'') then
  begin
    fsplit(fn,dir,name,ext);
    s1 := GetField(fieldpos-1);
    if Pos('.\', s1) = 1 then s1 := Mid(s1, 3);
    if (Dir = '') or (UpperCase(Dir) = '$CLPATH+') then
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
      fehler(Getres2(10900,8)+': ' +s2); { 'Ungueltige Adresse: 's2 }
      exit;
    end;
  until n=0;
end;

function check_envelope(var s:string):boolean;
begin
  check_envelope:=false;
  if s <> '' then
    if not multi_Mailstring(s) then exit;
  if (getfield(MailInServerFld) <> '') and (s = '') then
  begin
    rfehler(970);        { 'Envelope-Adresse muss angegeben werden' }
    exit;
  end;
  check_envelope:=true;
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

type box_array = array[0..maxboxen] of string;
                 { Box-Ergebnisliste => Eingabefeld }


function BoxSelect(const entries:byte; boxlist:box_array; colsel2:boolean):string;
const width = 51+BoxNameLen;
var
      nt         : eNetz;
      x,y,height,
      i, sel_anz : integer;     { Anzahl der auszuwaehlenden Boxen }
      box        : string;      { Name der aktuellen Box          }
      user       : string;      { Username der aktuellen Box      }
      komm       : string;      { Kommentar der aktuellen Box     }
      boxline    : string;      { angezeigte Zeile in Boxauswahl  }
      list       : TLister;
label nextBox;
begin
  BoxSelect:='';
  height:=screenlines-17;
  if screenlines>30 then dec(height,2);
  if screenlines>40 then dec(height,2);
  dbGoTop(Boxbase);
  sel_anz:=0;
  list := nil;  //prevent warning "list not initialized"
  while not dbEOF(Boxbase) do
  begin
    box:=dbReadStr(Boxbase, 'Boxname');
    if own_Name <> '' then
      for i:=1 to entries do
        if uppercase(box)=uppercase(boxlist[i]) then  { Box schon ausgewaehlt?      }
          goto nextBox;                     { ...dann naechsten Datensatz }
    dbRead(Boxbase,'Netztyp',nt);
    if ((nt=own_Nt) and (uppercase(box)<>UpperCase(own_Name)))   { passende Box gefunden }
      or (own_name='') then
    begin
      inc(sel_anz);
      komm:=dbReadStr(Boxbase,'Kommentar');
      if nt in netsRFC then user:=dbReadStr(Boxbase,'Email')
      else user:=dbReadStr(Boxbase,'Username');
      boxline:=' '+forms(box,BoxNameLen)+'  '+forms(user,20)+
               '  '+forms(komm,25);
      if sel_anz=1 then
      begin     { bei erster gefundener Box Dialog aufbauen }
        if own_name <> '' then        { 'Serverboxen (Netztyp %s)' }
          selbox(width+2,height+4,getreps2(936,3,Netz_Typ(nt)),x,y,true)
        else                          { '/Netcall/Spezial bei:' }
          selbox(width+2,height+4,getres2(1024,3)+' '+getres2(1024,5),x,y,false);

        List := TLister.CreateWithOptions(x+1,x+width,y+1,y+height+2,0,'/NS/SB/DM/NLR/NA');
        ListBoxCol(list);
        if ColSel2 or (own_name = '') then
          List.SetArrows(x,y+1,y+height+2,col.colsel2rahmen,col.colsel2rahmen,'�')
        else
          List.SetArrows(x,y+1,y+height+2,col.colselrahmen,col.colselrahmen,'�');
      end;
      List.Addline(boxline);
    end;
nextBox:
    dbNext(Boxbase);
  end;
  if sel_anz > 0 then       { Wenn Box(en) gefunden, Auswahl }
  begin
    if List.Show then
      Result := ''
    else
      Result :=trim(copy(list.getselection,2,BoxNameLen));
    CloseBox;
    List.Free;
  end else
    rfehler(953); { 'Keine (weiteren) hinzuzufuegenden Serverboxen vorhanden!' }
end;


procedure EditAddServersList(var cr:customrec);
var
      x,y     : integer;
      nt: eNetz;  // byte!
      t          : taste;
      nr,bp      : shortint;
      gl,width   : Integer;
      buttons    : string;
      okb,edb    : shortint;
      p          : Integer;
      a,ii       : integer;
      s1         : string;
      modi       : boolean;
      poutside   : boolean;
      movefrom   : integer;
      entries    : integer;
var   boxlist    : box_array;
label Start;

  { Die hier mehrfach vorkommende Pruefung "if own_Name <> '' then..."  }
  { dient zur Feststellung, ob wir in einem Box-Config-Dialog (z.B.    }
  { 'Zusaetzliche Server' bei RFC/Client oder 'Pakete mitsenden' bei    }
  { Fido) sind oder aus 'EditNetcallDat' in xp10.pas kommen. In        }
  { letzterem Fall ziehen wir abweichende (= weniger restriktive)      }
  { Konsequenzen - z.B. lassen wir nach positiv beantworteter          }
  { Rueckfrage Dupes zu und uebergehen ueberfluessige bzw. unzutreffende   }
  { Tests wie "eingetragene Box = editierte Box?".                     }

  { Hinweis: 'EditAddServersList' wird sowohl als 'normale' Prozedur   }
  { als auch als Select-Routine (User drueckt <F2> im Eingabefeld)      }
  { mittels 'mappcustomsel(EditAddServersList,true)' in '_EditPPP'     }
  { (xp9.inc) aufgerufen. Aus diesem Grund koennen wir die oben         }
  { angesprochenen Bedingungen nicht als Parameter uebergeben (geht bei }
  { 'mappcustomsel' halt nicht) und fragen sie daher ueber Variablen ab.}

  procedure display;
  var i    : Integer;
      box  : string;
  begin
    moff;
    for i:=1 to gl do
    begin
      if i=p then
        if own_Name <> '' then
          attrtxt(col.colsel2bar)
        else
          attrtxt(col.colselbar)
      else
        if own_Name <> '' then
          attrtxt(col.colsel2box)
        else
          attrtxt(col.colselbox);
      if i+a>entries then
        FWrt(x+1, y+i, sp(width))
      else begin
        box:=boxlist[a+i];
        FWrt(x+1, y+i, ' ' + iifc(a+i=movefrom,#16,' ') +
             forms(box,width-2));
      end;
    end;
    if own_Name <> '' then
      attrtxt(col.colsel2box)
    else
      attrtxt(col.colselbox);
    fwrt(x+width+1,y+1,iifc(a=0,'�',#30));
    fwrt(x+width+1,y+gl,iifc(a+gl<entries,#31,'�'));
    mon;
  end;

  procedure InsertBox;
  var
    i, add:  Integer;
    box: string;
  begin
    box:=BoxSelect(entries,boxlist,iifb(own_Name <> '',false,true));
    if box <> '' then
    begin
      if own_Name = '' then      { Abfrage nicht bei Box-Config-Dialog }
      begin
        i:=entries;
        while (i>0) and (uppercase(box)<>uppercase(boxlist[i])) do
          dec(i);                           { Eintrag schon vorhanden? }
        if i > 0 then
          if not ReadJN(getreps2(10900,57,box),false) then Exit;
           { 'Serverbox "%s" bereits vorhanden - trotzdem hinzuf�gen?' }
      end;
      inc(entries);
      if entries=1 then add:=0 else add:=1;
      boxlist[0]:=boxlist[a+p+add];  { Ziel = (a+p) }
      boxlist[a+p+add]:=box;
      for i:=(a+p+1+add) to entries do
      begin
        box:=boxlist[i];
        boxlist[i]:=boxlist[0];
        boxlist[0]:=box;
      end;
      modi:=true;
    end;
    if (a+p<entries) then
      if p<gl then inc(p)
      else inc(a);
  end;

  procedure MoveBox;
  var s : string;
      i : integer;
  begin                           { Ziel = (a+p); Quelle = movefrom }
    boxlist[0]:=boxlist[a+p];
    boxlist[a+p]:=boxlist[movefrom];
    if movefrom<a+p then
      for i:=(a+p-1) downto movefrom do begin
        s:=boxlist[i];
        boxlist[i]:=boxlist[0];
        boxlist[0]:=s;
      end
    else if movefrom>a+p then
      for i:=(a+p+1) to movefrom do begin
        s:=boxlist[i];
        boxlist[i]:=boxlist[0];
        boxlist[0]:=s;
      end;
    movefrom:=0;
    modi:=true;
  end;

  procedure DelBox;
  var s : string;
      i : integer;
  begin
    s:=boxlist[a+p];
    s:=mid(s,blankpos(s)+1);
    if ReadJN(getreps2(936,4,s),true) then
    begin { 'Serverbox "%s" loeschen' }
      if a+p<entries then
      begin  { a+p = Ziel }
        for i:=(a+p) to entries-1 do
          boxlist[i]:=boxlist[i+1];
        boxlist[entries]:='';
      end
      else
        boxlist[a+p]:='';
      dec(entries);
      modi:=true;
    end;
  end;

  procedure readbutt;
  begin
    if auswahlcursor then begin
      rbx:=x+1; rby:=y+p;
      end;
    nr:=readbutton(x+2,y+gl+2,2,buttons,bp,false,t);
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
    outside:=not ins1 or (yy>y+gl+2);
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        if entries>0 then p:=min(entries-a,yy-y) else else
      if (t=mausunright) or (t=mausunleft) then
        poutside:=false else
      if (t=mausldouble) and (edb<>0) then
        nr:=edb;
      end;
    if outside then begin
      if (t=mausleft) or (t=mausright) then
        poutside:=true else
      if poutside and ((t=mausunleft) or (t=mausunright)) then
        nr:=okb;
      end;
  end;

begin  { --- of EditAddServersList --- }
  showErrors:=true;
  if own_Name <> '' then maxbox:=80;
  s1:=trim(cr.s);
  if (s1='') and (own_Name<>'') then      { Sind Boxen im Eingabefeld? }
  begin                        { Wenn nicht, auf passende Boxen pruefen }
    dbGoTOp(Boxbase);
    while not dbEOF(boxbase) do
    begin
      dbRead(boxbase,'Netztyp',nt);
      if (nt=own_Nt) and (uppercase(dbReadStr(boxbase,'boxname')) <> UpperCase(own_Name)) then
                           { erste passende Box gefunden... }
        goto start;        { ...dann Schleife verlassen und los geht's }
      dbNext(boxbase);
    end;
    rfehler(953); { 'Keine (weiteren) hinzuzufuegenden Serverboxen vorhanden!' }
    exit;
  end;
  Start:
  width:=ival(getres2(936,1));
  buttons:=getres2(936,2);  { ' ^Einfuegen , ^Verschieben , ^Loeschen ,  ^OK  ' }
  okb:=4; edb:=0;
  if own_name = '' then pushhp(508);
  for ii:=0 to maxbox do boxlist[ii] := '';
  entries:=0;
  if s1 <> '' then
    repeat
      inc(entries);
      p:=cpos(' ',s1);
      if p=0 then boxlist[entries]:=s1
      else begin
        boxlist[entries]:=leftstr(s1,p-1);
        s1:=trim(mid(s1,p+1));
      end;
    until p=0;
  gl:=screenlines-fnkeylines-12;
  bp:=1;
  if own_Name <> '' then
  begin
    selbox(width+2,gl+4,getres2(920,92),x,y,false);
    attrtxt(col.colsel2rahmen);                 { 'Zusaetzliche Server' }
  end
  else begin
    selbox(width+2,gl+4,getres2(1024,3)+' #'+strs(cr.y)+' '+
           getres2(1024,5),x,y,true);    { '/Netcall/Spezial #%s bei:' }
    attrtxt(col.colselrahmen);
  end;
  mwrt(x,y+gl+1,'�'+dup(width,'�')+'�');
  t:='!';    { Buttons nur anzeigen }
  a:=0; p:=1; movefrom:=0;
  readbutt;
  modi:=false;
  maus_pushinside(x+1,x+width,y+1,y+gl);
  autobremse:=true;
  poutside:=false;
  repeat
    if p+a>entries then
      if p>1 then dec(p)
      else if a>0 then dec(a);
    display;
    autoupenable:=(a+p>1);
    autodownenable:=(a+p<entries);
    t:='*';
    readbutt;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if (t=keyesc) or ((movefrom>0) and (nr=0)) then begin
      movefrom:=0; t:=#0; nr:=99;
      end;
    if (t=keyins) or (t=keyf2) then nr:=1
    else if t=keydel then nr:=3;
    if (nr<>0) and (nr<>99) then bp:=abs(nr);
    if (nr=1) and (entries >= maxbox) then
      rfehler1(954,strs(maxbox)) { 'Maximal %s Serverbox-Eintraege moeglich!' }
    else
      if (nr>0) and (movefrom<>0) then
        MoveBox
      else
        case nr of
          1 : InsertBox;
          2 : if entries > 1 then
                movefrom:=a+p
              else begin
                movefrom:=0;
                errsound;
              end;
          3 : if entries=0 then errsound
              else DelBox;
        end;
    if nr<0 then
    begin
      if t=keyup then
        if p>1 then dec(p)
        else if a>0 then dec(a);
      if (t=keydown) and (a+p<entries) then
        if p<gl then inc(p)
        else inc(a);
      if t=keypgup then
        if a=0 then t:=keyhome
        else a:=max(0,a-gl);
      if t=keypgdn then begin
        if a+gl>=entries then p:=entries-a
        else inc(a,gl);
        p:=max(1,min(p,entries-a));
        end;
      if t=keyhome then begin
        a:=0; p:=1;
        end;
      if t=keyend then begin
        a:=max(0,entries-gl);
        p:=max(1,entries-a);
        end;
      if t=keychom then p:=1;
      if t=keycend then p:=minmax(gl,1,entries-a);
    end;
    if nr=okb then
    begin
      cr.brk:=false;
      if modi then
      begin
        s1:='';
        for ii:=1 to entries do
          s1:=s1+boxlist[ii]+' ';  { neuen Eintrag fuer Eingabefeld erstellen }
        cr.s:=trim(s1);
        modi:=false;
      end;
    end;
    if nr=0 then cr.brk:=true;
  until ((nr=0) or ((nr=okb) and addServersTest(cr.s))) and
        (not modi or ReadJN(getres(1015),false)); { '�nderungen verwerfen' }
  maus_popinside;
  if own_name = '' then pophp;
  closebox;
  freeres;
end;


function addServersTest(var s: String): Boolean;
var
  i, Dummy: Integer;
  s1: string;
  BoxList: TStringList;

  { Die hier mehrfach vorkommende Pr�fung "if own_Name <> '' then..."  }
  { dient zur Feststellung, ob wir in einem Box-Config-Dialog (z.B.    }
  { 'Zus�tzliche Server' bei RFC/Client oder 'Pakete mitsenden' bei    }
  { Fido) sind oder aus 'EditNetcallDat' in xp10.pas kommen. In        }
  { letzterem Fall ziehen wir abweichende (= weniger restriktive)      }
  { Konsequenzen - z.B. lassen wir nach positiv beantworteter          }
  { R�ckfrage Dupes zu und �bergehen �berfl�ssige bzw. unzutreffende   }
  { Tests wie "eingetragene Box = editierte Box?".                     }

  { Die Variable 'showErrors' dient als Flag, ob die Einzel-Fehlermel- }
  { dungen angezeigt werden sollen. Ist 'showErrors' false (z.B. bei   }
  { einem Netcall, siehe 'ChkAddServers' in xp7.inc), werden a) keine  }
  { Fehler ausgegeben, und es wird b) die Funktion beim ersten Fehler  }
  { sofort verlassen.                                                  }

  { Hinweis: 'addServersTest' wird sowohl als 'normale' Funktion als   }
  { auch als Masken-Testfunktion mittels 'msetvfunc(addServersTest)'   }
  { (siehe '_EditPPP' in xp9.inc) aufgerufen. Aus diesem Grund k�nnen  }
  { wir die oben angesprochenen Bedingungen nicht als Parameter        }
  { �bergeben (geht bei 'msetvfunc' halt nicht) und fragen sie daher   }
  { �ber Variablen ab.                                                 }

begin
  Result := true;
  s1:=trim(s);
  if s1='' then
    Exit;

  BoxList := TStringList.Create;
  BoxList.Sorted := true;
  try
    for i := 1 to WordCount(s1) do
    begin
      s := Trim(UpperCase(ExtractWord(i, s1)));
      if (Own_Name <> '') and BoxList.Find(s, Dummy) then
      begin
        Result := false;
        if ShowErrors then
          fehler(getreps2(10900,60, s)) { 'Serverbox "%s" ist mehrfach vorhanden!' }
        else
          break;
      end else
        BoxList.Add(s);

      if GetServerFileName(s, '') = '' then
      begin
        Result := false;
        if ShowErrors then
          RFehler1(962, s)   { 'Serverbox "%s" existiert nicht!' }
        else
          break;
      end else
      if own_Name <> '' then                   { Box-Config-Dialog? }
      begin
        if s = Uppercase(own_Name) then
        begin
          Result := false;
          if ShowErrors then
            RFehler1(963, s)
                { Serverbox "%s" ist identisch mit editierter Serverbox!'}
          else
            break;
        end else
        begin
          if ntBoxNetztyp(s) <> own_Nt then
          begin
            Result := false;
            if ShowErrors then
              Fehler(getreps2(10900,60, s) + ' ' +
                     getreps2(10900,64,Netz_Typ(own_Nt)))
                     { 'Serverbox "%s" ist nicht vom Netztyp %s!' }
            else
              break;
          end;
        end;
      end;
    end;
  finally
    BoxList.Free;
  end;
  s := s1;
end;


procedure ConvertAddServersFehler(const s:string);
var x,y : integer;
begin
  msgbox(length(s)+6,6,_fehler_,x,y);
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(920,92)+':');  { '"Zusaetzliche Server:"' }
  attrtxt(col.colmbox);
  mwrt(x+3,y+3,s);
  errsound;
  wait(curoff);
  closebox;
  freeres;
end;


function BfgToBox(s:string):string;
var
      p      : Integer;
      s1     : string;              { BFG-Datei }
      fehler : string;

  function isValidBfgName(const s1:string):boolean;
  var   i  : Integer;
        vb : boolean;
  const ValidBfgCh : set of char=['A'..'Z','0'..'9','_','^','$','~','!',
                          '#','%','&','-','{','}','(',')','@','''','`'];
  begin
    if (length(s1) > 8) or (IsDOSDevice(s1)) then
    begin
      isValidBfgName:=false;
      exit;
    end;
    vb:=true; i:=1;
    while vb and (i<=length(s1)) do
      if s1[i] in ValidBfgCh then inc(i)
      else vb:=false;
    isValidBfgName:=vb;
  end;

begin
  BfgToBoxOk:=true;  { Flag fuer 'ChkAddServers' in xp7.inc }
  if s = '' then
  begin
    BfgToBox:='';
    exit;
  end;
  s1:=''; Result := '';
  repeat
    p:=cpos(' ',s);
    if p=0 then s1:=s
    else begin
      s1:=leftstr(s,p-1);
      s:=trim(mid(s,p+1));
    end;
    if not isValidBfgName(uppercase(s1)) then
    begin
      BfgToBoxOk:=false;
      if showErrors then
      begin
        fehler:=getreps2(10900,67,uppercase(s1));
        { 'Ungueltiger Name fuer Serverbox-Konfigurationsdatei: "%s.BFG"' }
        ConvertAddServersFehler(fehler);
      end else
        break;
    end
    else begin
      dbSeek(boxbase,boidatei,uppercase(s1));
      if dbFound then
        Result := Result + dbReadStr(boxbase,'boxname') + ' '
      else
      begin
        BfgToBoxOk:=false;
        if showErrors then
        begin
          fehler:=getreps2(10900,68,uppercase(s1));
          { 'Serverbox zu Dateiname "%s.BFG" nicht gefunden!' }
          ConvertAddServersFehler(fehler);
        end else
          break;
      end;
    end;
  until p=0;
  Result := Trim(Result);
end;

// check s for correct and existing boxnames
function BoxToBfg(var s:string):string;
var
  i: Integer;
  BoxName: string;
begin
  Result := '';
  for i := 1 to WordCount(s) do
  begin
    BoxName := ExtractWord(i, s);
    if length(BoxName) > BoxNameLen then
      ConvertAddServersFehler(getreps2(10900, 69, BoxName)) { 'Ungueltiger Serverbox-Name: %s' }
    else
    begin
      dbSeek(boxbase,boiname,uppercase(BoxName));
      if dbFound then
        Result := Result + dbReadStr(boxbase,'dateiname') + ' '
      else
        ConvertAddServersFehler(getreps2(10900, 62, BoxName)); { 'Serverbox "%s" existiert nicht!' }
    end;
  end;
  Result := Uppercase(Trim(Result));
end;


procedure SingleServerSel(var cr:customrec); { einzelne Serverbox (nur vom }
var i     : Integer;                            { eigenen Netztyp) auswaehlen  }
    s1    : string;
    dummy : box_array; { wir brauchen keine Serverboxen-Liste zu uebergeben }
begin
  for i:=0 to maxboxen do dummy[i] := '';
  cr.brk:=false;
  s1:=BoxSelect(0,dummy,true);
  if s1 <> '' then cr.s:=trim(s1)
  else cr.brk:=true;
end;


procedure set_AddServers_Allowances(var s:string);
begin
  delete_on_cDel:=true;
  leave_on_cDel:=false;
  may_insert_clip:=false;
  cDel_pressed:=false;
end;


procedure set_ExtCfg_Allowances;
begin
  delete_on_cDel:=true;
  leave_on_cDel:=true;
  may_insert_clip:=true;
  cDel_pressed:=false;
end;


procedure reset_Allowances(var s:string);
begin
  delete_on_cDel:=false;
  leave_on_cDel:=false;
  may_insert_clip:=true;
  cDel_pressed:=false;
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
const cfgext  : array [1..7] of string[5] = ('*.CFG','*.BFG','*.BFE','*.$CF',
                                             '*.EXE','*.COM','*.BAT');
label restart;
begin
restart:
  set_ExtCfg_Allowances;   { Loeschen mit <Ctrl-Del> erlauben }
  s2 := '';
  if (cpos(':',s1) = 2) or (cpos(DirSepa, s1) = 1) then
  begin
    fsplit(ExpandFileName(s1),dir,name,ext);
    seldir := dir;
  end
  else seldir := cdir;
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  maddstring(3,2,fn,s1,37,MaxLenPathname,'');   { Dateiname: }
  for n := 1 to 7 do
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
  if (cDel_pressed) then          { <Ctrl-Del> gedrueckt => Dateiname loeschen }
  begin
    reset_Allowances(s1);  { s1 = Dummy }
    if boxpar^.ClientExternalConfig <> '' then
    begin
      if ReadJN(getres2(927,11),true) then  { 'Gespeicherten Dateinamen aus Konfigurationsdatei entfernen' }
      begin
        s1 := '';
        ReadExtCfgFilename := true;
        exit;
      end
      else goto restart;
    end
    else goto restart;
  end;
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
      if s2 <> '' then   { <Esc> gedrueckt? }
      begin
        fsplit(s2,dir,name,ext);
        if dir = cdir then s1 := name + ext else s1 := s2;
      end;
      goto restart;
    end;
    if (s2<>'') and ({IsDevice(s2) or }not ValidFilename(s2)) then
    begin
      rfehler(3);   { Ungueltiger Pfad- oder Dateiname! }
      goto restart;
    end;
    s1 := s2;
    ReadExtCfgFilename := (s1<>'');
  end else
    ReadExtCfgFilename := false;
end;


{
  $Log: xpconfigedit.pas,v $
  Revision 1.75  2004/01/25 18:36:11  cl
  - fixed crash on Netcall/Einzeln with single box

  Revision 1.74  2004/01/17 16:33:47  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

  Revision 1.73  2003/11/22 11:41:14  mk
  - added support for maximum mail size (pop3) and automatic dialing on win32

  Revision 1.72  2003/11/09 14:36:09  mk
  - fixed closing of misc databases in Unisel while dbTempClose

  Revision 1.71  2003/10/18 17:14:48  mk
  - persistent open database boxenfile (DB: boxbase)

  Revision 1.70  2003/10/03 11:35:06  mk
  - fixed addServersTest

  Revision 1.69  2003/10/01 18:37:11  mk
  - simplyfied seeknextbox

  Revision 1.68  2003/09/21 20:17:40  mk
  - rewrite of Listdisplay:
    removed Assemlber function MakeListDisplay, now
    recoded in Pascal in ListDisplay
  - use Integer instead of xpWord in TListerDisplayLineEvent
  - removed global Variable CharBuf
  - new parameters for ConsoleWrite, removed CharBuf support
  - Highlight Lines with URL in Lister
  - Added support for Highlighting in Lister with Unicode-Display

  Revision 1.67  2003/09/15 16:06:30  mk
  - cleaned up RFC/Client code
  - removed some limits
  - fixed old FreeXP bugs

  Revision 1.66  2003/09/13 15:23:50  mk
  - fixed generation of valid box filenames (added "-" and "_"), this is
    for compatibilty with freexp
  - fixed adding of additional servers in Boxen/Edit/Servers/Neu/Zusaetzliche Server

  Revision 1.65  2003/09/03 00:54:55  mk
  - added multiserver netcall

  Revision 1.64  2003/08/28 14:13:03  mk
  - TUniSelType for UniSel instead of numeric constants

  Revision 1.63  2003/08/26 22:41:25  cl
  - better compatibility with OpenXP-16/FreeXP with config files:
    - don't overwrite line number settings with incompatible values
    - don't store unnecessary parameters for IP netcalls

  Revision 1.62  2003/08/25 17:44:52  mk
  - fixed directory create in Edit/Client

  Revision 1.61  2003/05/01 09:52:29  mk
  - added IMAP support

  Revision 1.60  2003/01/13 22:05:19  cl
  - send window rewrite - Fido adaptions
  - new address handling - Fido adaptions and cleanups

  Revision 1.59  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.58  2002/12/21 05:38:00  dodi
  - removed questionable references to Word type

  Revision 1.57  2002/12/16 01:05:13  dodi
  - fixed some hints and warnings

  Revision 1.56  2002/12/14 07:25:51  dodi
  - added MausNet support

  Revision 1.55  2002/12/12 11:58:49  dodi
  - set $WRITEABLECONT OFF

  Revision 1.54  2002/12/09 14:49:01  dodi
  remove merged include files

  Revision 1.53  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.52  2002/12/06 14:27:29  dodi
  - updated uses, comments and todos

  Revision 1.51  2002/11/01 14:17:05  ma
  - reimplemented Janus+ switch, should solve problems with non-standard
    servers

  Revision 1.50  2002/07/31 19:26:21  ma
  - user=>email db field code synchronized with v3.8
    (does not need re-entering email address when upgrading from old
     versions now)

  Revision 1.49  2002/07/25 20:43:56  ma
  - updated copyright notices

  Revision 1.48  2002/07/13 12:11:11  ma
  - enabled lowercase for box name in initial box creation

  Revision 1.47  2002/05/26 12:26:11  ma
  - using "email" db field instead of "user" db field for email now
    email may be longer than 30 chars now
    EMAIL ADDRESS HAS TO BE RE-ENTERED IN SERVER SETTINGS

  Revision 1.46  2002/05/25 07:24:44  mk
  - UserName has only 30 characters

  Revision 1.45  2002/02/10 14:23:56  mk
  - misc 3.40 update fixes

  Revision 1.44  2002/02/10 13:32:26  mk
  - fixed some display problems with Netcall/Special

  Revision 1.43  2002/01/30 23:07:58  mk
  - fixed Netz_Typ

  Revision 1.42  2002/01/22 13:59:22  mk
  - after 3.40 merge fixes

  Revision 1.41  2002/01/22 01:25:37  mk
  - hopefully fixed net_type

  Revision 1.40  2002/01/21 22:45:48  cl
  - fixes after 3.40 merge

  Revision 1.39  2002/01/19 14:17:03  mk
  - Big 3.40 update part IV

  Revision 1.38  2002/01/19 13:46:10  mk
  - Big 3.40 udpate part III

  Revision 1.37  2002/01/19 10:27:46  mk
  - Big 3.40 Update Part II

  Revision 1.36  2002/01/09 02:40:56  mk
  - fixed DirSepa for UnixFS

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


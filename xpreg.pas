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

{ Registrierung }

{$I XPDEFINE.INC }

unit xpreg;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
      dos,typeform,fileio,inout,keys,winxp,montage,
      video,datadef,database,maus2,maske,clip,resource,printerx,
      xp0,xp1,xp1o,xp1o2,xp1input,xpnt, xpglobal;

procedure copyright(wait:boolean);
{ MK 19.01.2000 }
procedure BetaMessage;

procedure regbeiproc(var s:string);
procedure calcpreis(var s:string);
procedure zahlweisproc(var s:string);
function  sammelpreis(var s:string):boolean;
function  setregorder(var s:string):boolean;
function  calcshipping(var s:string):boolean;
function  setmailreg(var s:string):boolean;
procedure regwegproc(var s:string);
function  regtestfilename(var s:string):boolean;
function  carddate_valid(var s:string):boolean;
function  testupdates(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses  xp6,xp9bp,xpauto;

const regfile  = 'regdata.cfg';

      regsites = 1;
      hregwege = 3;    { Drucken, Datei, E-Mail, Online }
      regrueck = 2;    { Post, E-Mail }
      zahlwege = 6;    { Scheck/bar, ö, EuroCard, VISA-Card, MasterCard }
      preisf   = 10;
      maxmail  = 5;    { max. Adressen je RegSite }
      maxphone = 4;    { max. Telefonnummern je RegSite }

type  SiteRec = record
                  name,str,ort : string[30];
                  zusatz       : string[20];   { Land }
                  p1,p2,p3     : integer;      { einfach, komplett, Update }
                  waehrung     : string[5];
                  rwege        : set of byte;  { mîgl. Reg.-Wege }
                  zahlwege     : set of byte;  { mîgl. Zahlunswege }
                  mailanz      : byte;
                  mailadr      : array[1..maxmail] of string[60];
                  mailnetz     : array[1..maxmail] of byte;
                  phoneanz     : byte;
                  rphone       : array[1..maxphone] of string[30];
                  rcdrom       : boolean;
                end;
      SR_Array= array[1..regsites] of SiteRec;
      SA_Ptr  = ^SR_Array;

type  RegRec = record
                 reg_bei    : byte;     { Registration site }
                 regweghin  : byte;     { Registrierungsweg }
                 regfilename: pathstr;
                 regmailadr : string[65];
                 regphone   : string[27];
                 regwegrueck: byte;
                 regwithdisk: boolean;
                 regwithcd  : boolean;
                 zahlweg    : byte;     { Zahlungsweg       }
                 uucpregs,
                 sonstregs,
                 komplettregs,
                 updates    : integer;  { Anzahl Registrierungen }
                 keyorder   : boolean;
                 sammelreg  : boolean;
                 name1,name2,
                 str,ort    : string[40];  { Adresse }
                 email      : string[79];
                 telefon    : string[telelen];
                 cardnr     : string[19];
                 cardvalid  : string[5];
                 comment1   : string[39];
                 comment2   : string[39];
               end;

var   pfp   : array[1..preisf] of byte;      { 1..4: Einzelpreise  }
      tfp   : array[1..preisf] of pointer;   { 5/6/7: Porto/Diskette/CD-ROM }
      preis : array[1..preisf] of integer;   { 8: Rabatt, 9: Summe }

      rsite       : SA_Ptr;
      rsitenr     : byte;

      waehrtext   : pointer;
      sammelfld   : byte;
      zahlweisfld : byte;
      cardnrfld   : byte;
      cardnrtxt   : pointer;
      carddatefld : byte;
      carddatetxt1: pointer;
      carddatetxt2: pointer;
      konto1pos   : pointer;
      konto2pos   : pointer;
      mailregfld  : byte;
      lupdates    : integer;
      cdromfld    : byte;

      regweghfld  : byte;
      regfilefld  : byte;
      regmailfld  : byte;
      regphonefld : byte;
      regposttxt  : pointer;


function _zahlweg(n:byte):string;
begin
  case n of
    1 : _zahlweg:='Scheck liegt bei';
    2 : _zahlweg:='Geld liegt bei';
    3 : _zahlweg:='Der Betrag wurde Åberwiesen:';
    4 : _zahlweg:='EuroCard';
    5 : _zahlweg:='VISA-Card';
    6 : _zahlweg:='MasterCard';
    else _zahlweg:='';   { ?! }
  end;
end;


function _regwegh(n:byte):string;
begin
  case n of
    1 : _regwegh:='Formular ausdrucken';
    2 : _regwegh:='Formular in Datei speichern';
    3 : _regwegh:='Formular per Mail verschicken';
    4 : _regwegh:='Online-Registrierung';
    else _regwegh:='';   { ?! }
  end;
end;


procedure SetSels(var s:string);
var i,n : integer;
begin
  with rsite^[rsitenr] do begin
    mclearsel(regmailfld); n:=0;
    for i:=1 to mailanz do begin
      mappendsel(regmailfld,false,mailadr[i]);
      if stricmp(mailadr[i],getfield(regmailfld)) then n:=i;
      end;
    if n=0 then
      setfield(regmailfld,mailadr[1]);
    mclearsel(regphonefld); n:=0;
    for i:=1 to phoneanz do begin
      mappendsel(regphonefld,false,rphone[i]);
      if stricmp(rphone[i],getfield(regphonefld)) then n:=i;
      end;
    if n=0 then
      setfield(regphonefld,rphone[1]);
    mclearsel(zahlweisfld);
    n:=0;
    for i:=1 to xpreg.zahlwege do
      if i in zahlwege then begin
        mappendsel(zahlweisfld,true,_zahlweg(i));
        if stricmp(getfield(zahlweisfld),_zahlweg(i)) then
          n:=i;
        end;
    if n=0 then setfield(zahlweisfld,_zahlweg(1));
    end;
end;


procedure regbeiproc(var s:string);
var ss : string[40];
    i  : integer;
begin
  for i:=1 to regsites do
    if pos(ustr(rsite^[i].name),ustr(s))>0 then rsitenr:=i;
  settexttext(waehrtext,'('+rsite^[rsitenr].waehrung+')');
  ss:=getfield(zahlweisfld);
  zahlweisproc(ss);
  if not rsite^[rsitenr].rcdrom then
    setfield(cdromfld,_jn_[2]);
  calcpreis(s);
  SetSels(s);
end;


{ der Åbergebene Parameter wird nicht ausgewertet }

procedure calcpreis(var s:string);
var anz,p,i,sum : longint;
    anzn,preisn : array[1..4] of longint;
    rabatt,n    : longint;

  procedure _set(i:byte);
  begin
    preis[i]:=p;
    if p=0 then settexttext(tfp[i],sp(7))
    else settexttext(tfp[i],strsn(p,5)+',-');
  end;

label again;

begin
again:
  sum:=0;
  for i:=1 to 8 do begin
    if i<5 then begin
      anz:=ival(getfield(pfp[i]));
      anzn[i]:=anz;
      end
    else if (i<8) then
      anz:=iif(getfield(pfp[i])=_jn_[1],1,0)
    else
      anz:=iif(getfield(pfp[10])=_jn_[1],1,0);
    case rsitenr of
      1 : case i of                   { ** Jochen Herz }
            1 : p:=50;   4 : p:=40;
            2 : p:=50;   5 : p:=3;    { Porto }
            3 : p:=80;   6 : p:=4;    { Diskette }
            7 : p:=30;   8 : p:=10;   { CD-ROM / Keynachbestellung }
          end;
      2 : case i of                   { ** Roland Lipovits }
            1 : p:=370;   4 : p:=295;
            2 : p:=370;   5 : p:=25;
            3 : p:=590;   6 : p:=15;
            7 : p:=300;   8 : p:=0;
          end;
    end;
    if i<5 then preisn[i]:=p;
    p:=anz*p;
    inc(sum,p);
    if (i<8) then _set(i)
    else _set(10);
    end;
  rabatt:=0;
  if getfield(sammelfld)=_jn_[1] then begin
    for i:=3 to 4 do begin
      while anzn[i]>5 do begin   { Komplettregistrierungen und Updates }
        dec(anzn[i],6);
        inc(rabatt,preisn[i]);
        end;
      if anzn[i]=5 then begin
        setfield(pfp[i],strsn(ival(getfield(pfp[i]))+1,2));
        goto again;
        end;
      end;
    n:=anzn[1]+anzn[2]+anzn[3];
    while n>5 do begin         { einfache Registrierungen und }
      dec(n,6);                { Mischrabatte                 }
      inc(rabatt,preisn[1]);
      end;
    if n=5 then begin
      if anzn[2]>0 then i:=2
      else i:=1;
      setfield(pfp[i],strsn(ival(getfield(pfp[i]))+1,2));
      goto again;
      end;
    end;
  dec(sum,rabatt);
  p:=-rabatt;
  _set(8);
  p:=sum;
  _set(9);
end;


function sammelpreis(var s:string):boolean;
var anzn : array[1..4] of integer;
    i    : integer;
begin
  for i:=1 to 4 do
    anzn[i]:=ival(getfield(pfp[i]));
  if (s=_jn_[1]) and (anzn[3]<5) and (anzn[4]<5) and
     (anzn[1]+anzn[2]+anzn[3]<5) then begin
    fehler('Rabatte sind erst ab 6 Registrierungen mîglich (s. Online-Hilfe)');
    sammelpreis:=false;
    end
  else begin
    setfield(fieldpos,s);
    calcpreis(s);
    sammelpreis:=true;
    end;
end;


function setregorder(var s:string):boolean;
var i : integer;
begin
  if (s=_jn_[1]) then begin
    for i:=1 to 4 do
      setfield(pfp[i],' 0');
    setfield(sammelfld,_jn_[2]);
    end;
  setfield(fieldpos,s);
  calcpreis(s);
  setregorder:=true;
end;


function calcshipping(var s:string):boolean;
var i : integer;
begin
  if (fieldpos=cdromfld) and (s=_jn_[1]) and not rsite^[rsitenr].rcdrom
  then begin
    fehler('CD-ROM-Bestellungen sind nur bei Jochen Herz mîglich.');
    s:=_jn_[2];
    end;
  calcshipping:=true;
  if pfp[5]=fieldpos then i:=5
  else if pfp[6]=fieldpos then i:=6
  else i:=7;
  case i of
      5 : begin  { Porto }
            if s=_jn_[2] then begin
              setfield(pfp[6],_jn_[2]);
              settexttext(tfp[6],sp(6));
              setfield(pfp[7],_jn_[2]);
              settexttext(tfp[7],sp(6));
              if mailregfld<>0 then setfield(mailregfld,_jn_[1]);
              end
            else
              if mailregfld<>0 then setfield(mailregfld,_jn_[2]);
          end;
    6,7 : begin  { Diskette / CD-ROM }
            if getfield(pfp[5])=_jn_[2] then begin
              s:=_jn_[2];
              errsound;
              calcshipping:=false;
              end;
            if s=_jn_[1] then
              setfield(pfp[6],_jn_[2]);   { Diskette abschalten }
          end;
  end;
  setfield(fieldpos,s);
  calcpreis(s);
end;


procedure zahlweisproc(var s:string);
var c,ue : boolean;
    ss   : string[80];
begin
  c:=stricmp(s,_zahlweg(4)) or stricmp(s,_zahlweg(5)) or stricmp(s,_zahlweg(6));
  ue:=stricmp(s,_zahlweg(3));
  if (regweghfld<>0) and not ue and not c then
    if stricmp(getfield(regweghfld),_regwegh(3)) or
       stricmp(getfield(regweghfld),_regwegh(4)) then begin
      ss:=_regwegh(1);               { Geld/Scheck liegt bei -> }
      setfield(regweghfld,ss);       { keine Mail- oder Online-Reg. }
      regwegproc(ss);
      end;
  setfieldenable(cardnrfld,c);
  settexttext(cardnrtxt,iifs(c,'Kartennummer  ',''));
  settexttext(carddatetxt1,iifs(c,'gÅltig bis    ',''));
  settexttext(carddatetxt2,iifs(c,sp(20),''));
  setfieldenable(carddatefld,c);
  if ue then
    case rsitenr of
      1 : begin                { *** Jochen Herz }
            settexttext(konto1pos,forms('Konto 291 817 05',50));
            settexttext(konto2pos,'Volksbank Worms, BLZ 553 900 00       ');
          end;
      2 : begin                { *** Roland Lipovits }
            settexttext(konto1pos,forms('Konto 603.126   ',50));
            settexttext(konto2pos,'Raiffeisenbank Markt-Allhau, BLZ 33036');
          end;
    end
  else if c then begin
    settexttext(konto1pos,'');
    settexttext(konto2pos,'');
    end
  else begin
    settexttext(konto1pos,sp(50));
    settexttext(konto2pos,sp(50));
    end;
end;


function setmailreg(var s:string):boolean;
begin
  if s=_jn_[1] then begin
    setfield(pfp[5],_jn_[2]); settexttext(tfp[5],sp(6));
    setfield(pfp[6],_jn_[2]); settexttext(tfp[6],sp(6));
    setfield(pfp[7],_jn_[2]); settexttext(tfp[7],sp(6));
    end
  else begin
    setfield(pfp[5],_jn_[1]); settexttext(tfp[5],'   3,-');
    end;
  calcpreis(s);
end;


function DruckerStr:string;
begin
  DruckerStr:=forms('(Druck auf LPT'+strs(DruckLpt)+')',43);
end;


procedure regwegproc(var s:string);
var ss : string[80];
begin
  if stricmp(s,_regwegh(1)) then settexttext(regposttxt,DruckerStr)
  else settexttext(regposttxt,'');
  setfieldenable(regfilefld,stricmp(s,_regwegh(2)));
  setfieldenable(regmailfld,stricmp(s,_regwegh(3)));
  setfieldenable(regphonefld,stricmp(s,_regwegh(4)));
  if (stricmp(s,_regwegh(3)) or stricmp(s,_regwegh(4))) and
     (stricmp(getfield(zahlweisfld),_zahlweg(1)) or
      stricmp(getfield(zahlweisfld),_zahlweg(2)))
  then begin
    ss:=_zahlweg(3);
    setfield(zahlweisfld,ss);
    zahlweisproc(ss);
    end;
end;


function regtestfilename(var s:string):boolean;
begin
  if s='' then
    regtestfilename:=true
  else
    if validfilename(s) then
      regtestfilename:=true
    else begin
      fehler('ungÅltiger Dateiname');
      regtestfilename:=false;
      end;
end;


function carddate_valid(var s:string):boolean;
var j,m : longint;
    dat : longint;
    kdat: longint;
begin
  m:=ival(left(s,2)); j:=ival(right(s,2));
  dat:=ival(right(date,4))*100 + ival(copy(date,4,2));
  if j<80 then kdat:=(j+2000)*100+m
  else kdat:=(j+1900)*100+m;
  if (ival(left(s,2))<1) or (ival(left(s,2))>12) or
     (kdat<dat) or (kdat>dat+500) then begin
    hinweis('ungÅltiges Kreditkarten-GÅltigkeitsdatum');
    end;
  carddate_valid:=true;
end;


function testupdates(var s:string):boolean;
var x,y : byte;
    t   : taste;
begin
  if (ival(s)>0) and (ival(s)<>lupdates) and not registriert.r2 then begin
    msgbox(55,8,'Hinweis',x,y);
    wrt(x+3,y+2,'Ein Update ist nur mîglich, wenn Sie bereits eine');
    wrt(x+3,y+3,'"einfache" XP-Registrierung besitzen. Sind Sie');
    wrt(x+3,y+4,'sicher, da· Sie '+iifs(ival(s)=1,'ein Update','Updates')+
                ' bestellen mîchten?');
    errsound;
    t:='';
    case readbutton(x+3,y+6,2,getres(107),iif(ival(s)>1,1,2),true,t) of
      0,2 : testupdates:=false;                { '  ^Ja  , ^Nein ' }
      1   : begin
              testupdates:=true;
              lupdates:=ival(s);
            end;
    end;
    closebox;
    end
  else begin
    testupdates:=true;
    lupdates:=ival(s);
    end;
end;


procedure RegFormular;
var brk,modi : boolean;
    ok       : boolean;
    regdata  : RegRec;
    regsite  : array[1..regsites] of SiteRec;

  function GetAbsAddress(d:DB):string;
  var box   : string[20];
      point : string[25];
      user  : string[30];
      domain: string[60];
      alias : boolean;
      adr   : string[80];
  begin
    dbRead(d,'boxname',box);
    dbRead(d,'username',user);
    dbRead(d,'pointname',point);
    dbRead(d,'domain',domain);
    alias:=dbReadInt(d,'script') and 4<>0;
    case dbReadInt(d,'netztyp') of
      nt_ZConnect : adr:=user+'@'+box+domain;
      nt_Maus     : adr:=user+' @ '+box;
      nt_Fido     : if left(box,2)<>'2:' then adr:=''
                    else if alias then adr:=user+' @ '+left(box,cpos('/',box))+point
                    else adr:=user+' @ '+box+'.'+point;
      nt_UUCP     : if alias then adr:=user+'@'+box+domain
                    else adr:=user+'@'+point+domain;
      else          adr:='';
    end;
    GetAbsAddress:=adr;
  end;

  procedure InitVar;
  var uucp  : boolean;
      sonst : boolean;
      i     : integer;
      d     : DB;

    procedure ReadRegadr;
    var t   : text;
        s   : string;
        key : string[10];
    begin
      with regsite[1] do begin
        mailanz:=0;
        phoneanz:=0;
        assign(t,'SUPPORT.CFG');
        if existf(t) then begin
          reset(t);
          while not eof(t) do begin
            readln(t,s);
            key:=lstr(GetToken(s,'='));
            if (key='regmail') and (mailanz<maxmail) then begin
              inc(mailanz);
              mailadr[mailanz]:=s;
              if pos('fido',lstr(s))>0 then mailnetz[mailanz]:=nt_Fido else
              if pos('maus',lstr(s))>0 then mailnetz[mailanz]:=nt_Maus else
              if pos('magic',lstr(s))>0 then mailnetz[mailanz]:=nt_Magic else
              if pos('internet',lstr(s))>0 then mailnetz[mailanz]:=nt_UUCP else
                mailnetz[mailanz]:=nt_UUCP;
              end
            else if (key='box') and (phoneanz<maxphone) then begin
              inc(phoneanz);
              rphone[phoneanz]:=s;
              end;
            end;
          close(t);
          end;
        end;
    end;

  begin
    with regsite[1] do begin
      name:='Jochen Herz'; str:='Schoefferstr. 23'; ort:='D-67547 Worms';
      zusatz:='';
      waehrung:='DM';
      rwege:=[1..4]; zahlwege:=[1..6];
      ReadRegadr;
      if mailanz=0 then begin
        mailanz:=1;
        mailadr[1]:='joherz@codev.de'; mailnetz[1]:=nt_UUCP;
        end;
      if phoneanz=0 then begin
        phoneanz:=3;
        rphone[1]:='02672-910152 (Modem)';
        rphone[2]:='02672-910153 (Modem)';
        rphone[3]:='02672-910156 (ISDN)';
        end;
      rcdrom:=true;
      end;
{   with regsite[2] do begin
      name:='Roland E. Lipovits'; str:='Nr. 424'; ort:='A-7411 Markt Allhau';
      zusatz:=' (ôsterreich)';
      waehrung:='ôS';
      rwege:=[1..4];
      zahlwege:=[1..3];
      mailanz:=3;
      mailadr[1]:='Roland E. Lipovits @ 2:31/11 (Fido)'; mailnetz[1]:=nt_Fido;
      mailadr[2]:='rel@lipo.co.at';                      mailnetz[2]:=nt_UUCP;
      mailadr[3]:='Roland Lipovits @ A-W (MausNet)';     mailnetz[3]:=nt_Maus;
      phoneanz:=1;
      rphone[1]:='03356-75525 (Modem)';
      rcdrom:=false;
      end; }
    rsite:=@regsite;
    fillchar(regdata,sizeof(regdata),0);
    with regdata do begin
      reg_bei:=1;      { Nunz }
      regweghin:=1;    { ausdrucken }
      regwegrueck:=1;  { Post }
      zahlweg:=1;      { Scheck }
      uucp:=(ntused[nt_UUCP]<>0);
      sonst:=false;
      for i:=0 to 99 do
        if (i<>nt_UUCP) and (ntused[i]<>0) then sonst:=true;
      if uucp and sonst then
        if registriert.r2 then
          if not registriert.uucp then updates:=1 else
          if not registriert.non_uucp then updates:=1 else
        else
          if uucp and sonst then komplettregs:=1 else
          if uucp then uucpregs:=1 else
          if sonst then sonstregs:=1;
      cardnr:='0000-0000-0000-0000';
      cardvalid:='01/95';
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,ustr(DefaultBox));
      if dbFound then
        email:=GetAbsAddress(d);
      telefon:=xp0.telefonnr^;
      dbClose(d);
      end;
  end;


  procedure AppMailAddr;
  var d   : DB;
      adr : string;
  begin
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do begin
      adr:=GetAbsAddress(d);
      if adr<>'' then mappsel(false,adr);
      dbNext(d);
      end;
    dbClose(d);
  end;


  procedure SaveRegData;
  var t : text;
  begin
    assign(t,regfile);
    rewrite(t);
    with regdata do begin
      writeln(t,'# In dieser Datei merkt sich XP die Daten aus dem Registrierungsformular.');
      writeln(t,'# Dadurch mu· das Formular nicht komplett neu ausgefÅllt werden, wenn Sie');
      writeln(t,'# es z.B. erneut ausdrucken mîchten oder wenn Sie ein Update bestellen.');
      writeln(t,'# Wenn Sie die Daten aus dem Formular nicht mehr benîtigen, kînnen Sie');
      writeln(t,'# diese Datei lîschen.');
      writeln(t);
      writeln(t,'Name1=',name1);
      if name2<>'' then writeln(t,'Name2=',name2);
      writeln(t,'Strasse=',str);
      writeln(t,'Ort=',ort);
      writeln(t,'Email=',email);
      writeln(t,'Telefon=',telefon);
      writeln(t);
      writeln(t,'Registrierung_bei=',reg_bei);
      writeln(t,'Registrierungsweg=',regweghin);
      if regfilename<>'' then
        writeln(t,'Datei=',regfilename);
      if regmailadr<>'' then
        writeln(t,'MailTo=',regmailadr);
      if regphone<>'' then
        writeln(t,'OnlineNummer=',regphone);
      writeln(t,'Zustellweg=',regwegrueck);
      writeln(t,'Diskette=',iifc(regwithdisk,'J','N'));
      writeln(t,'CD-ROM=',iifc(regwithcd,'J','N'));
      writeln(t,'Zahlungsweg=',zahlweg);
    { if cardnr<>'' then writeln(t,'Kreditkarte=',cardnr); }
    { if cardvalid<>'' then writeln(t,'Gueltig_bis=',cardvalid); }
      writeln(t);
      writeln(t,'UUCP-Registrierungen=',uucpregs);
      writeln(t,'sonstige_Registrierungen=',sonstregs);
      writeln(t,'Komplettregistrierungen=',komplettregs);
      writeln(t,'Updates=',updates);
      writeln(t,'Sammelregistrierung=',iifc(sammelreg,'J','N'));
      writeln(t,'Key-Nachbestellungen=',iifc(keyorder,'J','N'));
      writeln(t);
      if comment1<>'' then writeln(t,'Kommentar1=',comment1);
      if comment2<>'' then writeln(t,'Kommentar2=',comment2);
      end;
    close(t);
  end;


  procedure ReadRegData;
  var t   : text;
      s   : string;
      tag : string[35];
  begin
    assign(t,regfile);
    if existf(t) then begin
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        if (s<>'') and (s[1]<>'#') and (cpos('=',s)>0) then with regdata do
        begin
          tag:=lstr(GetToken(s,'='));
          if tag='name1'          then name1:=s else
          if tag='name2'          then name2:=s else
          if tag='strasse'        then str:=s else
          if tag='ort'            then ort:=s else
          if tag='email'          then email:=s else
          if tag='telefon'        then telefon:=s else
          if tag='registrierung_bei' then reg_bei:=minmax(ival(s),1,regsites) else
          if tag='registrierungsweg' then regweghin:=minmax(ival(s),1,hregwege) else
          if tag='datei'          then regfilename:=s else
          if tag='mailto'         then regmailadr:=s else
          if tag='onlinenummer'   then regphone:=s else
          if tag='zustellweg'     then regwegrueck:=minmax(ival(s),1,regrueck) else
          if tag='diskette'       then regwithdisk:=(ustr(s)<>'N') else
          if tag='cd-rom'         then regwithcd:=(ustr(s)<>'N') else
          if tag='zahlungsweg'    then zahlweg:=minmax(ival(s),1,zahlwege) else
          if tag='kreditkarte'    then cardnr:=s else
          if tag='gueltig_bis'    then cardvalid:=s else
          if tag='uucp-registrierungen' then uucpregs:=minmax(ival(s),0,99) else
          if tag='sonstige_registrierungen' then sonstregs:=minmax(ival(s),0,99) else
          if tag='komplettregistrierungen' then komplettregs:=minmax(ival(s),0,99) else
          if tag='updates' then updates:=minmax(ival(s),0,99) else
          if tag='sammelregistrierung' then sammelreg:=(ustr(s)<>'N') else
          if tag='key-nachbestellungen' then keyorder:=(ustr(s)<>'N') else
          if tag='kommentar1'     then comment1:=s else
          if tag='kommentar2'     then comment2:=s else
            fehler('unbekannter Eintrag in REGDATA.CFG: '+tag);
          end;
        end;
      close(t);
      end;
  end;


  function RegdataOk:boolean;
  var x,y : byte;
      t   : taste;
  begin
    RegdataOk:=false;
    with regdata do
      if name1='' then
        fehler('Geben Sie bitte Ihren Namen an!')
      else if ((preis[9]<40) and (preis[10]=0)) then
        fehler('Geben Sie bitte an, was Sie registrieren mîchten!')
      else if (regwegrueck=1) and (ort='') then
        fehler('Geben Sie bitte Ihre Postadresse an!')
      else if (regwegrueck=2) and (email='') then
        fehler('Geben Sie bitte Ihre E-Mail-Adresse an!')
      else if (regweghin=2) and (regfilename='') then
        fehler('Geben Sie bitte den Dateinamen zum Speichern des Formulars an!')
      else if (regweghin=3) and (regmailadr='') then
        fehler('Geben Sie bitte die Mail-Adresse zum Verschicken des Formulars an!')
      else if (regweghin=4) and (regphone='') then
        fehler('WÑhlen Sie bitte eine Telefonnummer fÅr die Online-Registrierung aus!')
      else if (updates>1) and (comment1+comment2='') then begin
        msgbox(62,8,'',x,y);
        mwrt(x+3,y+2,'Geben Sie im Feld "Anmerkungen" bitte die unter /XPoint');
        mwrt(x+3,y+3,'/Registrierung angezeigte Lizenznummer der User ein, fÅr');
        mwrt(x+3,y+4,'die Sie Updates bestellen mîchten.');
        mwrt(x+3,y+6,getres(12));    { 'Taste drÅcken' }
        errsound;
        get(t,curon);
        closebox;
        end
      else
        RegdataOk:=true;
  end;


  procedure EditRegData(var brk,modi:boolean);
  var x,y      : byte;
      s        : string[80];
      xreg_bei : string[40];
      i        : integer;
      zahl     : string[33];
      regpost  : boolean;
      regmail  : boolean;
      regwegh  : string[40];
  begin
    with regdata do begin
      mailregfld:=0;
      regweghfld:=0;
      rsitenr:=reg_bei;

      dialog(76,screenlines-5,getres2(522,2),x,y);   { 'Registrierungsformular' }
      maddtext(3,2,'Dieses Formular besteht aus mehreren Seiten,  zwischen denen   ≥',0);
      maddtext(3,3,'Sie mit PgUp/PgDn (Bild'#24'/Bild'#25') blÑttern kînnen. Beenden Sie   ≥',0);
      maddtext(3,4,'die Eingabe mit Ctrl-Enter (Strg+Enter).',0);
      maddtext(66,4,'∆ÕÕÕÕÕÕÕÕ',0);
      maddtext(69,2,'Preis',col.coldiahigh);
      maddtext(70,3,'('+regsite[reg_bei].waehrung+')',col.coldiahigh);
        waehrtext:=mtextpos;
      { maddtext(51,4,'Hilfe mit F1',col.coldiahigh); }
      for i:=5 to 44 do
        if i<>34 then maddtext(66,i,'≥',0);

      xreg_bei:=regsite[reg_bei].name+regsite[reg_bei].zusatz;
        maddtext(3,6,'Registrierung bei',col.coldiahigh);
        maddstring(22,6,'',xreg_bei,39,40,'');
        for i:=1 to regsites do
          mappsel(true,regsite[i].name+regsite[i].zusatz);
        mset3proc(regbeiproc);
        mhnr(1560);

      maddtext(3,8,'Ihre Anschrift',col.coldiahigh);
      maddstring(22, 8,'Name    ',name1,30,40,'');
      maddstring(22, 9,'Adresse ',name2,30,40,'');
      maddstring(22,10,'        ',str,30,40,'');
      maddstring(22,11,'PLZ/Ort ',ort,30,40,'');
      maddstring(22,12,'Mail    ',email,30,79,'');
        appmailaddr;
      maddstring(22,13,'Telefon ',telefon,30,telelen,'');
      maddtext(3,15,'Registrierungen',col.coldiahigh);

      s:='';
      maddtext(68,15,s,col.coldiahigh);
        tfp[2]:=mtextpos;
        maddint (22,15,'',sonstregs,4,2,0,99);
        mset3proc(calcpreis);
        pfp[2]:=fieldpos;
        maddtext(27,15,'mal fÅr Fido, Z-Netz, MausNet etc.',0);
      maddtext(68,16,s,col.coldiahigh);
        tfp[1]:=mtextpos;
        maddint (22,16,'',uucpregs,4,2,0,99);
        mset3proc(calcpreis);
        pfp[1]:=fieldpos;
        maddtext(27,16,'mal fÅr RFC/UUCP (Usenet/Internet)',0);
      maddtext(68,17,s,col.coldiahigh);
        tfp[3]:=mtextpos;
        maddint (22,17,'',komplettregs,4,2,0,99);
        mset3proc(calcpreis);
        pfp[3]:=fieldpos;
        maddtext(27,17,'Komplettregistrierungen',0);
      maddtext(68,18,s,col.coldiahigh);
        tfp[4]:=mtextpos;
        maddint (22,18,'',updates,4,2,0,99);
        msetvfunc(testupdates);
        mset3proc(calcpreis);
        pfp[4]:=fieldpos;
        maddtext(27,18,'Updates auf Komplettregistrierungen',0);
        lupdates:=updates;

      maddbool(22,20,'Sammelregistrierung / Rabatte',sammelreg);
        sammelfld:=fieldpos;
        mset1func(sammelpreis);
        maddtext(68,20,s,col.coldiahigh);
        tfp[8]:=mtextpos;
        maddtext(29,21,'(siehe Online-Hilfe)',0);

      maddtext(68,22,s,col.coldiahigh);
        tfp[10]:=mtextpos;
        maddbool(22,22,'verlorenen Registr.-Code zusenden',keyorder);
        mset1func(setregorder);
        mset3proc(calcpreis);
        pfp[10]:=fieldpos;
        mhnr(1589);

      maddtext(3,24,'Zahlungsweise',col.coldiahigh);
      zahl:=_zahlweg(zahlweg);
        maddstring(22,24,'',zahl,33,33,'');
        for i:=1 to zahlwege do
          mappsel(true,_zahlweg(i));
        mset3proc(zahlweisproc);
        zahlweisfld:=fieldpos;
        mhnr(1572);
      maddtext(22,26,'Kartennummer  ',0);
        cardnrtxt:=mtextpos;
      maddform(36,26,'',cardnr,'    -    -    -    ','0123456789');
        cardnrfld:=fieldpos;
        MDisabledNodisplay;
      maddtext(22,27,'gÅltig bis    ',0);
        carddatetxt1:=mtextpos;
      maddform(36,27,'',cardvalid,'  /  ','0123456789');
        carddatefld:=fieldpos;
        msetvfunc(carddate_valid);
        MDisabledNodisplay;
      maddtext(43,27,sp(20),0);
        carddatetxt2:=mtextpos;
      maddtext(22,26,'',0); konto1pos:=mtextpos;
      maddtext(22,27,'',0); konto2pos:=mtextpos;
      zahlweisproc(zahl);

      maddtext(3,29,'Wie mîchten Sie Ihre Registrierung erhalten?',col.coldiahigh);
      s:='';
      maddtext(68,31,s,col.coldiahigh);
        tfp[5]:=mtextpos;
        regpost:=(regwegrueck=1);
        maddbool(22,31,'per Post (am sichersten)',regpost);
        mset1func(calcshipping);
        pfp[5]:=fieldpos;
      s:=iifc(regpost,_jn_[1],_jn_[2]);
      maddtext(68,32,s,col.coldiahigh);
        tfp[6]:=mtextpos;
        if not regpost then regwithdisk:=false;
        maddbool(22,32,'+ akt. Programmversion auf Diskette',regwithdisk);
        mset1func(calcshipping);
        pfp[6]:=fieldpos;
      s:=iifc(regwithdisk,_jn_[1],_jn_[2]);
      maddtext(68,33,s,col.coldiahigh);
        tfp[7]:=mtextpos;
        if not regpost then regwithcd:=false;
        maddbool(22,33,'+ CD-ROM (siehe Online-Hilfe)',regwithcd);
        mset1func(calcshipping);
        pfp[7]:=fieldpos;
        cdromfld:=fieldpos;
      s:=iifc(regwithcd,_jn_[1],_jn_[2]);
      regmail:=(regwegrueck=2);
        maddbool(22,34,'per E-Mail',regmail);
        mset1func(setmailreg);
        mailregfld:=fieldpos;

      maddtext(66,34,'∆ÕÕÕÕÕÕÕÕ',0);
        s:='';
        maddtext(68,35,s,col.coldiahigh);    { Summe }
        tfp[9]:=mtextpos;
        calcpreis(s);

      maddtext(3,36,'Wie mîchten Sie das Registrierungsformular verschicken?',
               col.coldiahigh);

      regwegh:=_regwegh(regweghin);
        maddstring(22,38,'',regwegh,39,39,'');
        regweghfld:=fieldpos;
        for i:=1 to hregwege do
          mappsel(true,_regwegh(i));
        mset3proc(regwegproc);
      maddstring(22,40,'Dateiname   ',regfilename,26,70,'>');
        regfilefld:=fieldpos;
        msetvfunc(regtestfilename);
        if regweghin<>2 then mdisable;
        MDisabledNodisplay;
      maddstring(22,40,'Mailadresse ',regmailadr,26,65,'');
        regmailfld:=fieldpos;
        if regweghin<>3 then mdisable;
        MDisabledNodisplay;
      maddstring(22,40,'Boxnummer   ',regphone,26,27,'0123456789-,@>W');
        regphonefld:=fieldpos;
        if regweghin<>4 then mdisable;
        MDisabledNodisplay;
      maddtext(22,40,iifs(regweghin=1,DruckerStr,''),0);
        regposttxt:=mtextpos;

      maddtext(3,42,'Anmerkungen',col.coldiahigh);
        maddstring(22,42,'',comment1,39,39,'');
        maddstring(22,43,'',comment2,39,39,'');

      masksetstat(true,false,keyf2);
      masksetautojump(10);
      repeat
        readmask(brk);
        if mmodified then modi:=true;
        for i:=1 to regsites do
          if stricmp(xreg_bei,regsite[i].name+regsite[i].zusatz) then
            reg_bei:=i;
        for i:=1 to zahlwege do
          if stricmp(zahl,_zahlweg(i)) then
            zahlweg:=i;
        for i:=1 to hregwege do
          if stricmp(regwegh,_regwegh(i)) then
            regweghin:=i;
        if regpost then regwegrueck:=1
        else regwegrueck:=2;
      until brk or RegdataOk;
      enddialog;
      end;
  end;


  function WriteFormular(fn:pathstr):boolean;
  const lr = '     ';
  var   t  : text;

    procedure wrl(s:string);
    begin
      writeln(t,lr,'   ',s);
    end;

    procedure wrp(anz:integer; txt:string; pnr:byte);
    begin
      with regdata do
        if preis[pnr]<>0 then
          wrl(forms(iifs(anz=0,txt,strs(anz)+txt),49)+
              regsite[reg_bei].waehrung+strsn(preis[pnr],8));
    end;

  begin
    assign(t,fn);
    rewrite(t);
    if ioresult<>0 then begin
      fehler('Fehler beim Schreiben des Registrierungsformulars');
      WriteFormular:=false;
      exit;
      end
    else
      WriteFormular:=true;
    with regdata do begin
      writeln(t);
      writeln(t,lr,right(dup(50,'-')+' CrossPoint-Registrierung ['+verstr+']',62));
      writeln(t);

      if regweghin in [1,2] then begin   { Formular ausdrucken / speichern }
        writeln(t);                      { -> Adresse schreiben            }
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t,lr,'   Herrn');
        writeln(t,lr,'   ',regsite[reg_bei].name);
        writeln(t,lr,'   ',regsite[reg_bei].str);
        writeln(t,lr,'   ',regsite[reg_bei].ort);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t);
        writeln(t,lr,dup(52,'-'),' Anschrift');
        writeln(t);
        end;

      writeln(t);
      wrl('Name:      '+name1);
      wrl('Adresse:   '+name2);
      if str<>'' then wrl('           '+str);
      wrl('Ort:       '+ort);
      writeln(t);
      if email<>''   then wrl('E-Mail:    '+email);
      if telefon<>'' then wrl('Telefon:   '+telefon);
      if email+telefon<>'' then writeln(t);
      writeln(t);
      writeln(t,lr,dup(51,'-'),' Bestellung');
      writeln(t);
      writeln(t);
      wrp(uucpregs,' mal XP fuer RFC/UUCP',1);
      wrp(sonstregs,' mal XP fuer alle Netze ausser RFC/UUCP',2);
      wrp(komplettregs,' mal XP Komplettpaket',3);
      wrp(updates,' Updates'+iifs(updates=1,' ['+strs(registriert.nr)+']',''),4);
      wrp(0,'Rabatte fuer Sammelregistrierung',8);
      wrp(0,'Nachbestellung Registrierungscode',10);
      wrp(0,'Versand per Post',5);
      wrp(0,'+ aktuelle Programmversion auf Diskette',6);
      wrp(0,'+ CD-ROM',7);
      writeln(t,lr,sp(52),'----------');
      writeln(t,lr,sp(52),regsite[reg_bei].waehrung,strsn(preis[9],8));
      writeln(t,lr,sp(52),'==========');
      writeln(t);
      writeln(t);
      writeln(t,lr,dup(48,'-'),' Zahlungsweise');
      writeln(t);
      writeln(t);
      if zahlweg=3 then
        wrl('Der Betrag wurde ueberwiesen.')
      else
        wrl(_zahlweg(zahlweg));
      if zahlweg in [4..6] then
        wrl('Kartennummer: '+cardnr+', gueltig bis: '+cardvalid);
      writeln(t);
      if comment1+comment2<>'' then begin
        writeln(t);
        if comment1<>'' then wrl(comment1);
        if comment2<>'' then wrl(comment2);
        writeln(t);
        end;
      if regweghin in [1,2] then begin
        writeln(t);
        writeln(t);
        writeln(t);
        wrl(left(date,6)+right(date,2)+'  '+dup(36,'_'));
        wrl('          (Unterschrift bei Postregistrierung)');
        end;
      if regweghin=3 then
        writeln(t);
      end;
    close(t);
  end;


  function PrintFormular:boolean;
  var tmp : pathstr;
      t   : text;
      s   : string;
  begin
    tmp:=TempS(4096);
    if WriteFormular(tmp) then begin
      message('Registrierungsformular wird gedruckt ...');
      InitPrinter;
      assign(t,tmp);
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        writeln(lst,s);
        end;
      close(t);
      write(lst,#12);
      closebox;
      _era(tmp);
      end;
    PrintFormular := true;
  end;


  function WriteToFile:boolean;
  begin
    WriteToFile:=WriteFormular(regdata.regfilename);
    message('Registrierungsformular wurde gespeichert.');
    wkey(1,false);
    closebox;
  end;


  function SendEmail:boolean;
  var header : string[12];
      tmp    : pathstr;
      mto    : string[adrlen];
      d      : DB;
      i      : integer;
  begin
    if cpos('@',regdata.regmailadr)=0 then begin
      fehler('Keine korrekte Mailadresse angegeben!');
      SendEmail:=false;
      end
    else begin
      tmp:=TempS(4096);
      if WriteFormular(tmp) then begin
        header:='';
        mto:=regdata.regmailadr;
        dbOpen(d,BoxenFile,0);       { passenden Server suchen }
        with rsite^[rsitenr] do
          for i:=1 to mailanz do
            if stricmp(mto,mailadr[i]) then begin
              dbGoTop(d);
              while not dbEOF(d) and (mailnetz[i]<>dbReadInt(d,'netztyp')) do
                dbNext(d);
              if not dbEOF(d) then forcebox:=dbReadStr(d,'boxname');
              end;
        dbClose(d);
        if pos(' (',mto)>0 then
          mto:=trim(left(mto,pos(' (',mto)-1));
        SendEmail:=DoSend(true,tmp,mto,'XP-Registrierungsformular',
                          false,false,true,false,false,nil,header,header,0);
        _era(tmp);
        end;
      end;
  end;


  function OnlineReg:boolean;
  const FormFile  = 'xp-reg.txt';
        DummyReq  = '001F000B.req';
  var d    : DB;
      name : string[40];
      padr : string[25];   { Fido-Pointadresse }
      logf : pathstr;
      lipo : boolean;

    procedure MakeFidoCfg;    { FIDO.CFG erzeugen - siehe auch XP7F.PAS! }
    var t : text;
        p : byte;
    begin
      assign(t,'fido.cfg');
      rewrite(t);
      writeln(t,'Language=',ParLanguage);
      writeln(t,'Colors=$',hex(col.colmailer,2),' $',hex(col.colmailerhigh,2),
                ' $',hex(col.colmailerhi2,2));
      writeln(t,'Name=',name);
      writeln(t,'Address=',padr);
      if lipo then
        writeln(t,'Called=2:31/11')
      else
        writeln(t,'Called=2000:20/99');
      if orga^<>'' then writeln(t,'SysName=',orga^);
      if ParDebug then writeln(t,'Debug=Y');
      with boxpar^,comn[boxpar^.bport] do begin
        if ModemInit+minit^<>'' then begin
          write(t,'ModemInit=');
          if (ModemInit<>'') and (minit^<>'') then
            writeln(t,minit^+'\\'+ModemInit)
          else
            writeln(t,minit^+ModemInit);
          end;
        if IgCTS then writeln(t,'CTS=N');
        writeln(t,'RTS=',iifc(UseRTS,'Y','N'));
        writeln(t,'Line=',bport);
        writeln(t,'FOSSIL=',iifc(fossil,'Y','N'));
        if not fossil then begin
          writeln(t,'Port=',hex(Cport,4));
          writeln(t,'IRQ=',Cirq);
          writeln(t,'TriggerLevel=',tlevel);
          end;
        writeln(t,'Baud=',baud);
        writeln(t,'DialCommand=',MDial^);
        writeln(t,'ConnWait=',connwait);
        writeln(t,'RedialWait=',redialwait);
        if postsperre then
          writeln(t,'RedialWait2=',redialwait);
        writeln(t,'RedialMax=',redialmax);
        writeln(t,'MaxConn=',connectmax);
        end;
      p:=pos(' (',regdata.regphone);
      if p>0 then
        regdata.regphone:=trim(left(regdata.regphone,p-1));
      writeln(t,'Phone=',regdata.regphone);
      writeln(t,'InPath=',FilePath);
      writeln(t,'MailPath=',ownpath+AutoxDir);
      writeln(t,'ExtendedFilenames=N');
      writeln(t,'Send=',FormFile);
      if lipo then writeln(t,'Send=',DummyReq);
      writeln(t,'Text=Online-Registrierung');
      writeln(t,'EMSI=Y');
      writeln(t,'LogNew=',logf);
      close(t);
    end;

    procedure MakeDummyReq;
    var t : text;
    begin
      assign(t,DummyReq);
      rewrite(t);
      writeln(t,'XPREG99');
      close(t);
    end;

  begin
    OnlineReg:=true;   { nicht wieder in die Maske zurÅckspringen }
    if not exist('xp-fm.exe') then begin
      rfehler(2901);  { 'FÅr die Online-Registrierung wird das XP-Fido-Paket benîtigt.' }
      exit;
      end;
    if not exist('zm.exe') then begin
      fehler('ZM.EXE fehlt!');
      exit;
      end;
    if not WriteFormular(formfile) then begin
      fehler('Kann Datei '+formfile+' nicht erzeugen.');
      exit;
      end;
    lipo:=(pos('lipovits',lstr(rsite^[rsitenr].name))>0);
    if lipo then MakeDummyReq;
    name:=regdata.name1;
    padr:='2000:10/99.99';
    if DefFidoBox<>'' then begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,DefFidoBox);
      if dbFound then begin
        dbRead(d,'username',name);
        if not lipo then
          padr:=dbReadStr(d,'boxname')+'.'+dbReadStr(d,'pointname');
        end;
      dbClose(d);
      end;
    ReadBoxPar(nt_Fido {Netztyp egal} ,DefaultBox);
    logf:=TempS(8192);
    MakeFidoCfg;
    if exist(AutoxDir+'*.PKT') then
      AutoExec(false);
    shell('xp-fm.exe fido.cfg',400,3);
    if not exist(AutoxDir+'*.PKT') then
      fehler('Anruf wurde abgebrochen.')
    else begin
      message('Anruf war erfolgreich.');
      wkey(2,false);
      closebox;
      if errorlevel=0 then AutoExec(false);
      end;
    if exist(logf) then _era(logf);
    if exist(DummyReq) then _era(DummyReq);
  end;


begin
  InitVar;
  ReadRegData;
  repeat
    modi:=false;
    repeat
      EditRegData(brk,modi);
    until not brk or not modi or ReadJN(getres2(522,1),false);
    if not brk then begin                     { 'Eingaben verwerfen' }
      SaveRegData;
      case regdata.regweghin of
        1 : ok:=PrintFormular;
        2 : ok:=WriteToFile;
        3 : ok:=SendEmail;
        4 : ok:=OnlineReg;
      end;
      end;
  until brk or ok;
  freeres;
end;


{ Test, ob XP zu lange unregistriert benutzt wird.             }
{                                                              }
{ Stufe 1 (D=3 Monate, I=1 Monat):  Cursor auf "Registrierung" }
{ Stufe 2 (D=5 Monate, I=2 Monate): 10 Sekunden Wartepause     }
{ je weitere 2/1 Monate: Wartepause + 10 Sekunden              }
{                                                              }
{ Brettdatenbank.Userflags[1]: Zeitpunkt der Erstinstallation  }
{ Nachrichtendb.Userflags[3]:  letzte Copyright-Anzeige        }

procedure TestUnregtime(x,y:byte; var stufe1:boolean);
var timestamp  : word;
    first,last : word;
    j,m,d,dow  : rtlword;
    diff       : integer;
begin
  stufe1:=false;
  getdate(j,m,d,dow);
  timestamp:=max(0,(j-1970)*12*30 + (m-1)*30 + d);
  first:=dbReadUserflag(bbase,1);
  last:=dbReadUserflag(mbase,3);
  if (first=0) or (first>timestamp) or
     ((timestamp>last) and (timestamp-last>65)) then
    dbWriteUserflag(bbase,1,timestamp)
  else begin
    diff:=(timestamp-first) div 30;
    if (diff>=3) or (not deutsch and (diff>=1)) then begin
      stufe1:=true;
      if (diff>=5) or (not deutsch and (diff>=2)) then begin
        mwrt(x,y,getreps2(520,26,strs(diff)));
          { 'Sie arbeiten seit %s Monaten unregistriert mit CrossPoint!' }
        if deutsch then zaehler[2]:=((diff-3) div 2)*10
        else zaehler[2]:=(diff-1)*10;
        repeat
          mwrt(x,y+2,strs(zaehler[2])+' ');
          multi2;
        until zaehler[2]=0;
        clearkeybuf;
        mwrt(x,y,sp(66));
        mwrt(x,y+2,'    ');
        end;
      end;
    end;
  dbWriteUserflag(mbase,3,timestamp);
end;


{ wait:  TRUE  = Aufruf bei Programmstart }
{        FALSE = Aufruf per /XPoint/Info  }

procedure copyright(wait:boolean);
var x,y,i : byte;
    s     : string;
    p     : byte;

    msglines    : byte;
    timeover    : boolean;

    sely  : byte;
    sels  : string[80];
    n     : shortint;
    z     : taste;
    regform : boolean;

  procedure ReadReg;
  var x,y  : byte;
      brk  : boolean;
      code : string[20];
      t    : text;
      nr   : longint;
      z    : taste;
  begin
    if registriert.uucp and registriert.non_uucp then begin
      pushhp(1553);
      if ReadJN(getres2(521,1),false) then begin { 'Sie sind bereits registriert. Mîchten Sie die Registrierung lîschen' }
        _era('regdat.xp');
        fillchar(registriert,sizeof(registriert),0);
        regstr1:=''; regstr2:='';
        message(getres2(521,8));    { 'Registrierung wurde gelîscht.' }
        mdelay(1500);
        closebox;
        end;
      pophp;
      exit;
      end;
    code:='';
    dialog(length(getres2(521,2))+27,3,'',x,y);
    maddstring(3,2,getres2(521,2),code,20,20,'');   { 'Registrierungs-Code: ' }
      mhnr(1554);
    readmask(brk);
    enddialog;
    if (code<>'') and (firstchar(code)<'A') then code:='A'+code;
    if brk or (code='') then begin
      message(getres2(521,3));    { 'Na, dann eben nicht.' }
      wkey(2,false);
      closebox;
      end
    else if (cpos('-',code)=0) or
            (ival(copy(code,2,cpos('-',code)-2))=0) then begin
      message(getres2(521,4));   { 'UngÅltiger Registrieruns-Code.' }
      wkey(2,false);
      closebox;
      end
    else begin
      if ival(firstchar(code))>0 then
        nr:=ival(left(code,cpos('-',code)-1))
      else
        nr:=ival(copy(code,2,cpos('-',code)-2));
      if IsKomCode(nr) or IsOrgCode(nr) then begin
        msgbox(53,8,'',x,y);
        mwrt(x+3,y+2,'Sie sind als nicht-privater '+xp_xp+'-Anwender');
        mwrt(x+3,y+3,'registriert. Soll dies in Ihren Nachrichten er-');
        mwrt(x+3,y+4,'kennbar sein?');
        z:='';
        pushhp(1585);
        if readbutton(x+3,y+6,2,getres(107),1,true,z)=1 then  { Ja/Nein }
          code:='!'+code;
        pophp;
        closebox;
        end;
      assign(t,regdat);
      rewrite(t);
      writeln(t,code);
      close(t);
      msgbox(47,8,'',x,y);
      moff;
      wrt(x+3,y+2,getres2(521,5));   { 'Der Registrierungscode wurde gespeichert.' }
      wrt(x+3,y+3,getres2(521,6));   { 'CrossPoint beendet sich jetzt selbst und' }
      wrt(x+3,y+4,getres2(521,7));   { 'mu· danach neu gestartet werden.' }
      wrt(x+3,y+6,getres(12));       { 'Taste drÅcken ...' }
      mon;
      xp1.wait(curon);
      quit:=true;
      end;
  end;

  function LizenzNummer:string;
  begin
    with registriert do
      if orgreg then LizenzNummer:='Org-'+tc+strs(nr)
      else if komreg then LizenzNummer:='Kom-'+tc+strs(nr)
      else LizenzNummer:=tc+strs(nr);
  end;

label again,noclose;

begin
  msglines:=ival(getres2(520,0));
  msgbox(70,msglines+8+iif(wait,3,0),'',x,y);
  moff;
  wrt(x+3,y+1,'Cross \\//    '+
              Right('           ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 50));
  wrt(x+3,y+2,'      //\\ Point');
  s:=x_copyright + ' ' + author_name;
  wrt(x+67-length(s),y+2,s);

  if registriert.r2 then begin
    s:=getres2(520,19)+LizenzNummer;   { 'Lizenznummer: ' }
    wrt(x+67-length(s),y+3,s);
    end;
  for i:=1 to msglines do begin
    s:=getres2(520,i);
    gotoxy(x+3,y+4+i);
    repeat
      p:=cposx('*',s);
      write(left(s,p-1));
      delete(s,1,p);
      p:=cposx('*',s);
      attrtxt(col.colmboxhigh);
      write(left(s,p-1));
      attrtxt(col.colmbox);
      delete(s,1,p);
    until s='';
    end;
  regform:=(getres2(520,99)='J');
  if wait then begin
    TestUnregtime(x+3,y+6+msglines,timeover);
    wrt(x+3,y+6+msglines,getres2(520,20));
    wrt(x+3,y+7+msglines,getres2(520,21));
    sels:=getres2(520,30);   { ' ^LIZENZ.DOC , ^Registrierung , ^Weiter , ^Abbruch' }
    end
  else
    if regform then
      sels:=getres2(520,31)   { ' ^LIZENZ.DOC , ^Registrierungsformular , ^Code eingeben ' }
    else
      sels:=getres2(520,32);  { ' ^LIZENZ.DOC , ^Code eingeben ' }
  mon;
again:
  sely:=y+msglines+6+iif(wait,3,0);
  attrtxt(col.colmbox);
  mwrt(x+3,sely,sp(65));
  z:='';
  n:=iif(wait,iif(timeover,2,3),1);
  pushhp(iif(wait,1550,1551));
  n:=ReadButton(x+3,sely,2,'*'+sels,n,true,z);
  pophp;
{$IFDEF UnixFS }
  s:='doc/' + getres2(520,40);   { Muss noch an die Grundstruktur angepasst werden }
{$ELSE }
  s:='DOC\' + getres2(520,40);   { 'LIZENZ.DOC' }
{$ENDIF }
  case n of
    1 : begin
          if not exist(s) then           { 'LIZENZ.DOC' }
            fehler(getreps2(520,41,s))   { '%s fehlt!' }
          else
            if listfile(s,getres2(520,42),true,false,0)=0 then;    { 'D=Drucken    <Esc>=Ende' }
          goto again;
          end;
    2 : if wait then begin
          if not regform then
            n:=2
          else begin
            attrtxt(col.colmbox);
            mwrt(x+3,sely,sp(65));
            z:='';
            pushhp(1552);
            n:=ReadButton(x+3,sely,2,getres2(520,50),1,true,z);
            pophp;
            end;
          case n of
            1 : begin
                  RegFormular;
                  goto again;
                end;
            2 : begin
                  closebox;
                  ReadReg;
                  goto noclose;
                end;
            else goto again;
          end;
          end
        else
          if regform then begin
            RegFormular;
            goto again;
            end
          else
            ReadReg;
    3 : if not wait then begin
          ReadReg;
          if not quit then goto again;
        end;
  else  if wait then quit:=true;
  end;
  closebox;
noclose:
  freeres;
end;

procedure BetaMessage;
var x,y,i : byte;
    msglines    : byte;
    z     : taste;
    s:String;
begin
  msglines:=ival(getres2(530,0));
  msgbox(73,msglines+7,'',x,y);
  moff;
  wrt(x+3,y+1,'Cross \\//    '+
              Right('           ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 50));
  wrt(x+3,y+2,'      //\\ Point');
  s:=x_copyright + ' ' + author_name;
  wrt(x+67-length(s),y+2,s);

  for i:=1 to msglines do
  begin
    s:=getres2(530,i);
    wrt(x+3,y+3+i, s);
  end;
  mon;
  pushhp(1550);
  quit := (ReadButton(x+45,y+msglines+5,2,'*'+getres2(530,30),1,true,z) <> 1);
  pophp;
  closebox;
  freeres;
end;

end.
{
  $Log$
  Revision 1.12  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.11  2000/06/29 13:01:02  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.10  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.9  2000/05/07 18:16:04  hd
  Kleine Linux-Anpassungen

  Revision 1.8  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

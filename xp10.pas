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

{ CrossPoint - Timing-Listen, Tastenmakros, GebÅhrenzonen, Header,  }
{              Nodelisten, Tarifgruppen                             }

{$I XPDEFINE.INC}

unit xp10;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  stack,typeform,fileio,inout,keys,montage,feiertag,winxp,classes,
  maske,datadef,database,maus2,lister,resource,xpglobal,xp0,xp1,xp1o2,
  xp1help,xp1input,xp5,fidoglob;


procedure UniEdit(typ:byte);     { 1=Timing, 2=Tasten, 3=GebÅhren, 4=Header, 5=Nodelisten, 6=Tarifgruppen }

procedure AutoTiming(tnr:integer; callall,crashall:boolean);
procedure GetPhoneGebdata(var telefon:string);  { -> BoxPar^ }
procedure AppPhoneZones;   { mappsel() fÅr GebÅhrenzonen }
function  CalcGebuehren(var startdate,starttime:datetimest; secs:real):real;
function  Einheitenpreis:real;
procedure gtest;

procedure readkeydefs;
procedure Makroliste(nr:byte);

function __dateok(var s:string):boolean;
function __timeok(var s:string):boolean;
function testaction(var s:string):boolean;

procedure MakSelKeys(Self: TLister; var t:taste);
function checkday(var s:string):boolean;
function _getmacro(s:string):string;

implementation  { ---------------------------------------------------- }

uses  xp2,xp3,xp3o,xp4o,xp4o2,xpnetcall,xp9bp,xpauto,xpfido,xpfidonl;

const maxentries  = 100;   { s. auch XP0.maxkeys }
      TimingWidth = 116;
      KeymacWidth = 250;
      GebWidth    = 80;
      comms       = 11;
      comstr      : array[1..comms] of string =
                    ('NETCALL','REORG','PACK','EXEC','QUIT','QUIT_ONCE','AUTOEXEC',
                     'CRASHS','REQUESTS','END','NODEDIFFS');

      mtypes = 8;
   {  mtyp : array[1..mtypes] of string[9] =
             ('Alles','Bretter','User','Nachr.','Lister','ArcViewer',
              'Editor','Terminal'); }

      mincode  = 3;    { Nummernbereich fÅr erweiterte Tasten }
      codes    = 140;

      maxphone = 9*105;
      maxzones = 17;   { max. GebÅhrenzonen }
      maxzeitbereiche = 12;   { mu· vielfaches von 4 sein! }
      maxtables= 10;   { max. Tabellen (Wochentage+Feiertage) }
      maxwotage= 10;   { max. Tagesbereiche }

      wofeiertag: array[1..maxwotage] of string =
                  ('Mo','Di','Mi','Do','Fr','Sa','So','F1','F2','F3');
      pagepos   : byte = 1;
      gpagepos  : byte = 1;

type  TimeRec   = record
                    active    : boolean;
                    von,bis   : string[5];  { Uhrzeit }
                    vond,bisd : string[6];  { Datum   }
                    wotag     : array[1..7] of boolean;
                    action    : string[80];
                    comm      : byte;       { 1=NC, 2=Reorg, 3=Pack, 5=Exec }
                                            { 5=Quit, 6=Quit_Once }
                    box       : string[BoxNameLen];
                    crash     : boolean;
                    crashtime : boolean;    { Crash - TimeSync }
                    qerrlevel : byte;
                    nxtime    : string[11];
                    ncconn    : integer;    { CONNECT-Countdown }
                    comport   : byte;
                    redialwait: integer;
                  end;
      TRP       = ^TimeRec;

      tasten = array[mincode..codes] of string;
      tap    = ^tasten;

      phone1   = string;
      phonearr = array[1..maxphone] of phone1;
      phoneap  = ^phonearr;
      phonea2  = array[0..maxzones] of record
                   anz    : integer;
                   ph     : phoneap;
                   komment: string;
                 end;
      wt_array = array[1..maxwotage] of boolean;
      tarifrec = record
                   sekunden : real;
                   pfennig  : integer;
                   anwahl   : integer;  { Pfennige fÅr nicht erfolgreiche Anwahl }
                 end;
      tarifarr = array[1..maxtables] of record
                   wochentag   : wt_array;
                   zeitbereiche: integer;
                   zeitbereich : array[1..maxzeitbereiche] of record
                                   von,bis : string;
                                   tarif   : array[1..maxzones] of tarifrec;
                                 end; end;
      tarifap  = ^tarifarr;

const anzahl    : integer = 0;        { Reentrance - s. GetPhoneGebData! }
var   e         : TStringList;
      filewidth : integer;
      _bunla    : string{[mtypes]};     { 'BUNLAET' }

      phones    : ^phonea2;
      tarif     : tarifap;
      tables    : integer;    { Anzahl Tarif-Tabellen }
      dayused   : wt_array;   { fÅr CheckDay() }


function mtyp(nr:byte):string;
begin
  mtyp:=getres2(1000,nr);   { 'Alles','Bretter','User','Nachr.','Lister','ArcViewer','Terminal' }
end;


{ --- Tastenmakros --------------------------------------------------- }

{ Aufbau von KEYDEF.CFG:

_x             ***       abc<F3>x<Alt P><<>
^B             *         xy
!Kommentar (max. 24 Zeichen)
<Tab>           **       <Sp><BS>00
               ^^^       ^^^ max. 200 Zeichen
        Bretter/User/Msgs/Lister/ArcViewer

^^^ einfache Taste, Ctrl-Taste oder Sondertaste
}


procedure settap(var ta:tap);

  procedure _set(von:byte; s:string);
  var p,p2: byte;
  begin
    p:=1;
    repeat
      p2:=p+iif(s[p]='/',0,1);
      while (p2<=length(s)) and (s[p2]<>'/') do inc(p2);
      case s[p] of
        '_' : ta^[von]:='Shift '+copy(s,p+1,p2-p-1);
        '^' : ta^[von]:='Ctrl '+copy(s,p+1,p2-p-1);
        '.' : ta^[von]:='Alt '+copy(s,p+1,p2-p-1);
      else
        ta^[von]:=copy(s,p,p2-p);
      end;
      inc(von);
      p:=p2+1;
    until (p>length(s));
  end;

begin
  new(ta);
  fillchar(ta^,sizeof(ta^),0);
  _set(3,'^2');
  _set(15,'_Tab/.Q/.W/.E/.R/.T/.Y/.U/.I/.O/.P');
  _set(30,'.A/.S/.D/.F/.G/.H/.J/.K/.L');
  _set(44,'.Z/.X/.C/.V/.B/.N/.M');
  _set(59,'F1/F2/F3/F4/F5/F6/F7/F8/F9/F10');
  _set(71,'Home/ob/PgUp//li//re//End/un/PgDn/Ins/Del');
  _set(84,'_F1/_F2/_F3/_F4/_F5/_F6/_F7/_F8/_F9/_F10');
  _set(94,'^F1/^F2/^F3/^F4/^F5/^F6/^F7/^F8/^F9/^F10');
  _set(104,'.F1/.F2/.F3/.F4/.F5/.F6/.F7/.F8/.F9/.F10');
  _set(114,'^PrtSc/^li/^re/^End/^PgDn/^Home');
  _set(120,'.1/.2/.3/.4/.5/.6/.7/.8/.9/.0/.·/.''');
  _set(132,'^PgUp/F11/F12/_F11/_F12/^F11/^F12/.F11/.F12');
end;

(* function ctrlkey(c:char):string;
begin
  case c of
     #8 : ctrlkey:='<BS>';
     #9 : ctrlkey:='<Tab>';
    #10 : ctrlkey:='<Ctrl Enter>';
    #13 : ctrlkey:='<Enter>';
    #27 : ctrlkey:='<Esc>';
    #32 : ctrlkey:='<Sp>';
   #127 : ctrlkey:='<Ctrl BS>';
  else
    ctrlkey:='<Ctrl '+chr(ord(c)+64)+'>';
  end;
end; *)


function extkey(s:string; ta:tap):taste;
var t : taste;
    i : integer;
begin
  if length(s)<3 then extkey:=''
  else begin
    s:=UpperCase(copy(s,2,length(s)-2));
    if (s='<') or (s='>') or (s='^') then extkey:=s
    else begin
      t:='';
      i:=mincode;
      while (t='') and (i<=codes) do begin
        if UpperCase(ta^[i])=s then t:=#0+chr(i);
        inc(i);
        end;
      extkey:=t;
      end;
    end;
end;


function getflags(s:string):byte;
var fl,i : byte;
    f    : word;
begin
  f:=1; fl:=0;
  for i:=1 to 8 do begin
    if s[i]='*' then inc(fl,f);
    f:=f*2;
    end;
  getflags:=fl;
end;


function getmacro(s:string; ta:tap):string;
var m : string;
    p : byte;
begin
  m:='';
  while (length(m)<253) and (s<>'') do
    case s[1] of
      '^' : begin
              delete(s,1,1);
              if s<>'' then begin
                if s[1]='0' then m:=m+'^'
                else m:=m+chr(ord(UpCase(s[1]))-64);
                delete(s,1,1);
                end;
            end;
      '<' : begin
              delete(s,1,1);
              if s<>'' then
                if (s[1]='<') or (s[1]='>') or (s[1]='^') or (s[1]=' ') then begin
                  m:=m+s[1];
                  delete(s,1,2);
                  end
                else begin
                  p:=cpos('>',s);
                  if p=0 then s:=''
                  else begin
                    if (p=7) and (LeftStr(s,4)='Ctrl') then
                      m:=m+chr(ord(s[6])-64)
                    else
                      m:=m+extkey('<'+LeftStr(s,p),ta);
                    delete(s,1,p);
                    end;
                  end;
            end;
    else begin
      m:=m+s[1];
      delete(s,1,1);
      end;
    end;
  getmacro:=m;
end;


procedure readkeydefs;
var ta  : tap;
    t   : text;
    s,m : string;
    tt  : taste;
begin
  keymacros:=0;
  assign(t,keydeffile);
  if existf(t) then begin
    settap(ta);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      tt:='';
      if s<>'' then
        case s[1] of
          '_' : tt:=s[2];
          '^' : tt:=chr(ord(UpCase(s[2]))-64);
          '<' : tt:=extkey(trim(LeftStr(s,15)),ta);
        end;
      if tt<>'' then begin
        m:=getmacro(copy(s,26,200),ta);
        if m<>'' then begin
          inc(keymacros);
          {getmem(macrodef[keymacros],length(m)+1);}
          macrodef[keymacros]:=m;
          macrokey[keymacros]:=tt;
          macroflags[keymacros]:=getflags(copy(s,16,8));
          end;
        end;
      end;
    close(t);
    dispose(ta);
    end;
end;

function _getmacro(s:string):string;
var ta  : tap;
begin
  settap(ta);
  _getmacro:=getmacro(s,ta);
  dispose(ta);
end;

{ -------------------------------------------------------------------- }


{ typ: 1=Timing; 2=Makros, 3=GebÅhrenzonen, 4=Nachrichtenkopf, 5=Nodelisten }

procedure loadfile(typ:byte; fn:string);
var t   :text;
    s   :string;
    lastIdx :integer;
begin
  e.Clear;
  if FileExists(fn) then
  begin
    assign(t,fn);
    reset(t);
    while not eof(t) do
    begin
      readln(t,s);
      if trim(s)<>'' then
        if (typ=2) and (FirstChar(s)='!') then                          //ein kommentar?
          if e.count > 0 then
            e.Strings[lastIdx]:=forms(e.Strings[lastIdx],225)+copy(s,2,24)      //Kommentar anhÑngen
          else
        else begin
          lastIdx:=e.Add(LeftStr(s,filewidth));
        end;
    end;
    close(t);
  end;
  anzahl:=e.Count;                                                      //globale var setzen :(
end;


procedure savefile(typ:byte; fn:string);
var t : text;
    i : integer;
begin
  assign(t,fn);
  rewrite(t);
  case typ of
    1 : for i:=0 to e.Count-1 do
          writeln(t,e[i]);
    2 : for i:=0 to e.Count-1 do begin
          writeln(t,trim(LeftStr(e.Strings[i],225)));
          if mid(e.Strings[i],226)<>'' then
            writeln(t,'!',mid(e.Strings[i],226));
          end;
  end;
  close(t);
end;

procedure releaseliste;
begin
  anzahl:=0;
end;

function ReadTimingNr(var brk:boolean):integer;
var x,y,nr : Integer;
begin
  dialog(length(getres(1001))+9,1,'',x,y);
  nr:=Timing_Nr;
  maddint(3,1,getres(1001),nr,2,2,1,99);   { 'Timing-Liste Nr. ' }
  readmask(brk);
  enddialog;
  if not brk then Timing_Nr:=nr;
  ReadTimingNr:=nr;
end;

procedure Str2Time(const s:string; var tr:TimeRec);
var i : integer;
begin
  with tr do
  begin
    active:=s[1]='+';
    von:=copy(s,3,5);
    bis:=copy(s,9,5);
    vond:=copy(s,15,6);
    bisd:=copy(s,22,6);
    for i:=1 to 7 do
      wotag[i]:=s[28+i]<>' ';
    action:=copy(s,37,80);
  end;
end;

function Time2Str(var tr:TimeRec):string;
var w : string[7];
    i : Integer;
begin
  with tr do begin
    for i:=1 to 7 do
      w[i]:=iifc(wotag[i],'˛',' ');
    w[0]:=#7;
    Time2Str:=iifc(active,'+',' ')+' '+von+' '+bis+' '+vond+' '+bisd+' '+
              w+' '+action;
    end;
end;


{$I xp10p.inc}


function __dateok(var s:string):boolean;
begin
  monat[2].zahl:=29;
  __dateok:=(ival(copy(s,4,2))<=12) and (ival(copy(s,4,2))>=1) and
            (ival(LeftStr(s,2))<=monat[ival(copy(s,4,2))].zahl);
end;

function __timeok(var s:string):boolean;
begin
  if cPos(' ',s)>0 then
    case fieldpos of
      1 : s:='00:00';
      2 : s:='23:59';
    end;
  __timeok:=true;
end;

function testaction(var s:string):boolean;
var p   : byte;
    box : string;
    x   : string;
    d   : DB;
    ni  : nodeinfo;
  function IsCommand(von,bis:byte):boolean;
  var i : byte;
  begin
    IsCommand:=false;
    if x='PACKEN' then x:='PACK';
    for i:=von to bis do
      if x=comstr[i] then
        IsCommand:=true;
  end;
begin
  if trim(s)='' then begin
    s:='NETCALL '+defaultbox;
    testaction:=true;
    exit;
    end;
  p:=cpos(' ',s);
  if p>0 then begin
    box:=trim(mid(s,p));
    x:=trim(copy(s,1,min(p,20)));
    end
  else begin
    box:='';
    x:=trim(LeftStr(s,20));
    end;
  UpString(x);
  if IsCommand(2,comms) then
    testaction:=true
  else
    if x='CRASH' then begin
      GetNodeinfo(box,ni,1);
      if not ni.found then begin
        rfehler(2116);    { 'unbekannte Nodeadresse' }
        testaction:=false;
        end
      else
        testaction:=true;
      end
    else if x<>'NETCALL' then begin
      testaction:=false;
      rfehler(1001);      { 'unbekannter Befehl' }
      end
    else begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,UpperCase(box));
      if dbFound then
        testaction:=true
      else begin
        testaction:=false;
        rfehler(1002);    { 'unbekannte Box' }
        end;
      dbClose(d);
      end;
end;

function CheckDay(var s:string):boolean;
begin
  if (s=_jn_[1]) and dayused[fieldpos] then begin
    if fieldpos>7 then
      rfehler1(1015,getres2(1022,8)+' '+strs(fieldpos-7))   { 'Feiertag' }
    else
      rfehler1(1015,trim(copy(_days_,(fieldpos-1)*_daylen_+1,_daylen_)));
    CheckDay:=false; { '%s ist bereits durch eine andere Tarifgruppe belegt.' }
    end
  else
    CheckDay:=true;
end;


{ typ: 1=Timing; 2=Tasten, 3=GebÅhren, 4=Header, 5=Nodelisten, 6=Tarife }

procedure UniEdit(typ:byte);
var
    brk, eList : boolean;
    x,y        : Integer;
    tnr        : integer;
    t          : taste;
    nr,bp      : integer;
    gl,width   : byte;
    buttons    : string;
    okb,edb    : integer;
    CurRow,n   : integer;
    { ii     : integer; }
    a        : integer;
    modi     : boolean;
    reindex  : boolean;
    boxsel1,
    boxsel2  : string;
    poutside : boolean;
    xhd      : extheadertype;
    movefrom : integer;
    oldft    : longint;

  function eanzahl:integer;
  begin
    case typ of
      1: result:=e.Count;                       //Timingliste or
      2: result:=e.Count;                       //TastenMakros
      3: result:=anzahl;                        //GebÅhren
      4: result:=xhd.anz+1;                     //Header
      5: result:=Nodelist.Count;                //nodeliste
      6: result:=tables;                        //Tarif
    else
      result:=anzahl;
    end;
  end;

  function daytxt(nr:integer):string;    { Tarife: 'Mo-Fr' etc. }
  var s   : string;
      i,j : integer;
      tage: string;
  begin
    s:='';
    tage:=getres2(1022,2);   { 'MoDiMiDoFrSaSoF1F2F3' }
    with tarif^[nr] do begin
      i:=1;
      repeat
        while (i<=maxwotage) and not wochentag[i] do inc(i);
        if (i<=maxwotage) then begin
          j:=i;
          while (j<maxwotage) and (j<>7) and wochentag[j+1] do inc(j);
          if s<>'' then s:=s+',';
          if j=i then
            s:=s+copy(tage,i*2-1,2)
          else if j=i+1 then
            s:=s+copy(tage,i*2-1,2)+','+copy(tage,j*2-1,2)
          else
            s:=s+copy(tage,i*2-1,2)+'-'+copy(tage,j*2-1,2);
          i:=j+1;
          end;
      until i>maxwotage;
      end;
    daytxt:=s;
  end;

  procedure display;
  var i,j, eanz :integer;
      tr     : timerec;
      tt     : string;
      komm   : string;
      bunla  : string;
      s      : string;
  begin
  { 1=Timing, 2=Tasten, 3=GebÅhren, 4=Header, 5=Nodelisten, 6=Tarifgruppen }
    moff;
    eanz:=eanzahl;
    for i:=1 to gl do begin
      gotoxy(x+1,y+i);                                  //alle Zeilen anzeigen
      if i=CurRow then attrtxt(col.colsel2bar)
      else attrtxt(col.colsel2box);
      if i+a>eanz then                             //kein Eintrag (mehr) vorhanden ->Leerstring
        Wrt2(sp(width))
      else begin
        case typ of
          1 : begin                               { Timingliste }
                str2time(e.Strings[i+a-1],tr);    { e.Strings Lesen -1 ba 0 bassierend}
                with tr do
                begin
                  Wrt2(' ' + iifc(active,'+',' ') + ' ' + von + '-' + bis + '  ' + vond + '-' +
                  bisd + '  ' + copy(e.Strings[i+a-1],29,8) + '  ' + forms(action,33));
                end;
              end;
          2 : begin                           { Tastenmakros }
                tt:=LeftStr(e.Strings[i+a-1],13);
                case tt[1] of
                  '_' : tt:=copy(tt,2,12)+' ';
                  '^' : tt:='<Ctrl '+tt[2]+'>     ';
                end;
                komm:=mid(e.Strings[i+a-1],226);
                Setlength(bunla, mtypes-1); {bunla[0]:=chr(mtypes-1);}
                for j:=2 to mtypes do
                  bunla[j-1]:=iifc(e.Strings[i+a-1][14+j]=' ',' ',_bunla[j]);
                Wrt2(' ' + tt + bunla + ' ' + forms(mid(e.Strings[i+a-1],26),50-length(komm)) +
                      ' ' + komm + ' ');
              end;
          3 : with phones^[i+a] do begin      { GebÅhrenliste Array}
                s:=' '+forms(komment,25);
                if anz>0 then
                  if anz=1 then s:=s+'1 '+getres2(1003,1)   { 'Eintrag' }
                  else s:=s+strs(anz)+' '+getres2(1003,2);  { 'EintrÑge' }
                Wrt2(forms(s,53));
              end;
          4 : begin
                s:=getres2(222,xhd.v[i+a-1]);                    { headerzeilen in der Nachrichtenanzeige }
                Wrt2(' ' + iifc(i+a=movefrom,#16,' ') +
                      forms(mid(s,blankpos(s)),width-2));
              end;
          5 : with TNodeListItem(Nodelist.Items[a+i-1]) do
                Wrt2(' '+forms(listfile,14)+                                    // NL-Dateiname
                      iifs(pos('###',listfile)>0,formi(number,3),'   ')+'  '+   //Nodelistennummer
                      forms(fupdatefile,14)+forms(fupdatearc,14)+
                      iifs(fdodiff,'Diff  ','      ')+
                      forms(getres2(2128,fformat),16));
          6 : with tarif^[a+i] do                             { array }
                Wrt2(forms(' '+getres2(1022,1)+' '+          { 'Tarifgruppe ' }
                            strs(a+i)+':   '+daytxt(a+i),53));
          end; // case typ of
        end;   // if i+a>eanz then
      end;
    attrtxt(col.colsel2box);
    wrt(x,y+1,iifc(a=0,'≥',#30));
    wrt(x,y+gl,iifc(a+gl<eanz,#31,'≥'));
    mon;
  end;

  procedure _insert(s:string; from,len:byte);
  var i : integer;
  begin
    i:=0;                       //kleinster index
    while (i<e.Count) and (copy(e.Strings[i],from,len)<copy(s,from,len)) do
      inc(i);
    e.Insert(i, s);
    inc(anzahl);
  end;

  procedure sort_e;
  begin
    E.Sort;
  end;

  //
  procedure ReadTiming(edit:boolean; var s:string; var brk:boolean);
  var tr  : TimeRec;
      wot : string;
      i   : byte;
      x,y : Integer;
      all : boolean;
      wtage:array[1..7] of string;
      aVon, aBis: String;
      aVonD, aBisD: String;
      aAction: String;
  begin
    for i:=1 to 7 do
      wtage[i]:=copy(_wotag_,i*2-1,2);
    Str2Time(s,tr);
    with tr do begin
      wot:='';
      all:=true;
      for i:=1 to 7 do
        all:=all and wotag[i];
      if all then
        wot:=getres2(1004,1)    { 'alle' }
      else begin
        for i:=1 to 7 do
          if wotag[i] then wot:=wot+','+wtage[i];
        if LeftStr(wot,1)=',' then delete(wot,1,1);
        end;

      // Umkopieren wegen AnsiStrings
      aVon := Von; aBis := bis; aVonD := VonD; aBisD := BisD; aAction := Action;
      dialog(48,9,getres2(1004,iif(edit,2,3)),x,y);   { 'Eintrag bearbeiten' / 'neuer Eintrag' }
      maddtime(3,2,getres2(1004,4),avon,false); mhnr(520);   { 'Uhrzeit von' }
      msetvfunc(__timeok);
      maddtime(24,2,getres2(1004,5),abis,false);       { 'bis ' }
      msetvfunc(__timeok);
      maddform(3,4,getres2(1004,6),avond,'  .  .','0123456789');   { 'Datum vom  ' }
      msetvfunc(__dateok);
      maddform(25,4,getres2(1004,7),abisd,'  .  .','0123456789');  { 'bis zum ' }
      msetvfunc(__dateok);
      maddstring(3,6,getres2(1004,8),wot,20,20,'');   { 'Wochentage ' }
      mappsel(false,getres2(1004,1));
      for i:=1 to 7 do mappsel(false,wtage[i]);
      maddstring(3,8,getres2(1004,9),aaction,30,80,'');   { 'Aktion     ' }
      mappsel(false,boxsel1);
      if boxsel2<>'' then
        mappsel(false,boxsel2);
      for i:=2 to comms do mappsel(false,comstr[i]);
      msetvfunc(testaction);
      readmask(brk);
      enddialog;
      if not brk then
      begin
        Von := aVon; Bis := abis; VonD := aVonD; BisD := aBisD; Action := aAction;
        wot:=LowerCase(trim(wot));
        for i:=1 to 7 do
          wotag[i]:=(wot=getres2(1004,1){'alle'}) or (pos(LowerCase(wtage[i]),wot)>0);
        s:=Time2Str(tr);
      end;
      freeres;
      end;
  end;

  procedure NewTiming;
  var s   : string;
      brk : boolean;
  begin
    s:='+   :     :   01.01. 31.12. ˛˛˛˛˛˛˛ ';
    ReadTiming(false,s,brk);
    if not brk then begin
      _insert(s,3,11);
      modi:=true;
      end;
  end;

  procedure DelEntry( strIdx :integer);
  begin
    if ReadJN(getres(1005),true) then
    begin    { 'Eintrag lîschen' }
      if strIdx<e.Count then begin
        e.Delete(strIdx);
        dec(anzahl);                           //behalten wir noch volÑufig bei
      end;
      modi:=true;
    end;
  end;
  //Editieren eines Tieming-Eintrages
  procedure EditTiming( strIdx :integer);
  var s   : string;
      brk : boolean;
  begin
    s:=e.Strings[strIdx];
    ReadTiming(true,s,brk);
    if not brk then begin
      e.Strings[strIdx]:=s;
      modi:=true;
      end;
  end;

  procedure ChangeActive(strIdx :integer);
  var tr : TimeRec;
  begin
    Str2Time(e.Strings[strIdx],tr);
    tr.active:=not tr.active;
    e.Strings[strIdx]:=Time2Str(tr);
    modi:=true;
  end;


  function keyok(ta:tap; t:taste):boolean;
  begin
    keyok:=(t[1]>#0) or ((t[2]>=chr(mincode)) and (t[2]<=chr(codes))
                         and (ta^[ord(t[2])]<>''));
  end;

  procedure readmkey(edit:boolean; x,y: Integer; ta:tap; var tt:string);
  var t1,t2 : taste;

    function gett:string;
    begin
      case tt[1] of
        '_' : gett:=forms(mid(tt,2),15);
        '^' : gett:='<Ctrl '+tt[2]+'>       ';
      else
        gett:=forms(tt,15);
      end;
    end;

  begin
    attrtxt(col.coldiainp); wrt(x,y,sp(17));
    attrtxt(col.coldiamarked);
    mwrt(x+1,y,trim(gett));
    case tt[1] of                   { Taste einlesen }
      ' ' : t1:='';
      '_' : t1:=tt[2];
      '^' : t1:=chr(ord(tt[2])-64);
      '<' : t1:=extkey(trim(tt),ta);
    end;
    gotoxy(x+1,y);
    repeat
      get(t2,curon);
      if t2=keyf1 then begin
        pushhp(546); hilfe; pophp;
        end;
    until (t2<>keyf1) and (t2<>'') and keyok(ta,t2);
    if not edit or ((t2<>keycr) and (t2<>keyesc)) or not kb_shift then
      if t2[1]>#0 then
        if t2[1]>=' ' then tt:='_'+t2
        else tt:='^'+chr(ord(t2[1])+64)
      else
        tt:='<'+ta^[ord(t2[2])]+'>';
    attrtxt(col.coldiahigh);
    gotoxy(x,y);
    moff;
    Wrt2(' ' + gett +' ');
    mon;
  end;

  procedure ReadMacro(var s:string; var brk:boolean);
  var i,nr,a : integer;
      x,y    : Integer;
      mt     : string;
      t1     : taste;
      ta     : tap;
      tt     : string;
      MacroString: string;
      ok,ESCPressed: boolean;
      komm   : string;
  begin
    dialog(58,9,getres2(1006,1),x,y);    { 'Tastatur-Makro anlegen' }
    if copy(s,16,3)='***' then mt:=mtyp(1)
    else if s[16]='*' then mt:=mtyp(2)   { Bretter     }
    else if s[17]='*' then mt:=mtyp(3)   { User        }
    else if s[18]='*' then mt:=mtyp(4)   { Nachrichten }
    else if s[19]='*' then mt:=mtyp(5)   { Lister      }
    else if s[20]='*' then mt:=mtyp(6)   { ArcViewer   }
    else mt:=mtyp(7);
    freeres;
    maddstring(3,2,getres2(1006,2),mt,9,9,''); mhnr(545);   { 'Makro fÅr ' }
    for i:=1 to mtypes do
      mappsel(true,mtyp(i));
    freeres;
    komm:=copy(s,226,24);
    maddstring(3,4,getres2(1006,3),komm,24,24,''); mhnr(549);  { s. EditMacro! } { 'Kommentar ' }
    maddtext(3,6,getres2(1006,4),0);   { 'Taste' }
      { maddtext(12,4,LeftStr(s,12),col.coldiahigh); }
    maddtext(3,8,getres2(1006,5),0);   { 'Makro' }
      maddtext(12,6,copy(s,26,40),col.coldiahigh);
    readmask(brk);
    closemask;
    if not brk then begin
      settap(ta);
      attrtxt(col.coldiahigh);
      mwrt(x+13,y+1,' '+forms(mt,12));
      mwrt(x+13,y+3,' '+forms(komm,25));
      for i:=1 to mtypes do
        if LowerCase(mt)=LowerCase(mtyp(i)) then nr:=i;
      freeres;
      case nr of
        1:mt:='***'; 2:mt:='*  '; 3:mt:=' * '; 4:mt:='  *';
        5:mt:='   *'; 6:mt:='    *'; 7:mt:='     *';
        8:mt:='      *';
      end;

      spush(hotkeys,1);
      hotkeys:=false;
      tt:=LeftStr(s,15);
      readmkey(false,x+13,y+5,ta,tt);

      attrtxt(col.coldialog);
      mwrt(x+33,y+1,'<ESC> '#17'ƒ    = '+getres2(1006,6));   { 'lîschen' }
      mwrt(x+33,y+2,'<ESC> c     = '+getres2(1006,7));       { 'Abbruch' }
      mwrt(x+33,y+3,'<ESC> '#17'ƒŸ   = '+getres2(1006,8));   { 'ok'      }
      mwrt(x+33,y+4,'<ESC> <ESC> = <ESC>');

      MacroString:='';                        { Definition einlesen }
      a:=0;
      brk:=false; ok:=false;
      repeat
        ESCPressed:=false;
        attrtxt(col.coldiainp);
        mwrt(x+13,y+7,' '+forms(mid(MacroString,a+1),40)+' ');
        gotoxy(x+14+length(MacroString)-a,y+7);

        repeat
          get(t1,curon);
          if t1=keyf1 then begin
            pushhp(547); hilfe; pophp;
            end;
          if t1=keyesc then
            if not ESCPressed then begin
              // remember ESC and read in next key
              ESCPressed:=true;
              t1:=keyf1;
              end else begin
              // ESC pressed twice, this is a real ESC
              ESCPressed:=false;
              end;
        until (t1<>keyf1) and keyok(ta,t1);

        if ESCPressed and (t1=keybs) then begin
          // Delete last macro key
          if (MacroString<>'') then begin
            if RightStr(MacroString,1)='>' then
            begin
              setlength(MacroString, length(MacroString)-2); { 2 wg. '>', '<' und '^' }
              while (MacroString<>'') and (RightStr(MacroString,1)<>'<') do
                Dellast(MacroString);
              Dellast(MacroString)
            end
            else if (length(MacroString)>=2) and (MacroString[length(MacroString)-1]='^') then
              SetLength(MacroString, length(MacroString)-2)
            else
              DelLast(MacroString);
            a:=max(0,min(a,length(MacroString)-40));
            end;
          end
        else begin
          brk:=(t1='c') and ESCPressed;
          ok:=(t1=keycr) and ESCPressed;
          if (length(MacroString)<190) and not (ok or brk) then begin
            if t1='>' then MacroString:=MacroString+'<>>'
            else if t1='<' then MacroString:=MacroString+'<<>'
            else if t1='^' then MacroString:=MacroString+'<^>'
            else if t1>=' ' then MacroString:=MacroString+t1
            else if t1>=#1 then MacroString:=MacroString+'^'+chr(ord(t1[1])+64)
            else MacroString:=MacroString+'<'+ta^[ord(t1[2])]+'>';
            a:=max(a,length(MacroString)-40);
            end;
          end;
      until brk or ok;
      spop(hotkeys);
      if not brk then begin
        s:=forms(tt,15)+forms(mt,10)+MacroString;
        if komm<>'' then s:=forms(s,225)+komm;
        end;
      dispose(ta);
      end;
    closebox;
    freeres;
  end;

  procedure NewMacro;
  var s   : string;
      brk : boolean;
  begin
    s:=sp(15)+'***';
    ReadMacro(s,brk);
    if not brk then begin
      _insert(s,1,12);
      modi:=true;
      end;
  end;

  procedure EditMacro(strIdx : integer);
  var x,y  : Integer;
      s    : string;
      komm : string;
      brk  : boolean;
  begin
    s:=trim(copy(e.Strings[strIdx],26,200));
    komm:=copy(e.Strings[strIdx],226,24);
    dialog(60,5,getres2(1007,1),x,y);   { 'Tastenmakro bearbeiten' }
    maddstring(3,2,getres2(1007,2),s,42,200,range(' ',#255)); mhnr(548);
    Mnotrim;                                         { 'Makro     ' }
    maddstring(3,4,getres2(1007,3),komm,24,24,'');   { 'Kommentar '}
    readmask(brk);
    enddialog;
    if not brk then begin
      e.Strings[strIdx]:=LeftStr(e.Strings[strIdx],25)+s;
      if komm<>'' then
        e.Strings[strIdx]:=forms(e.Strings[strIdx],225)+komm;
      modi:=true;
      end;
    freeres;
  end;

  procedure MacroKey(strIdx : integer);
  var x,y    : Integer;
      tt,ttt : string;
      ta     : tap;
  begin
    tt:=LeftStr(e.Strings[strIdx],15);
    diabox(35,5,'',x,y);
    mwrt(x+20,y,' <Shift Esc> ');
    mwrt(x+3,y+2,getres(1008));   { 'neue Taste' }
    ttt:=tt;
    settap(ta);
    spush(hotkeys,1);
    hotkeys:=false;
    readmkey(true,x+15,y+2,ta,tt);
    spop(hotkeys);
    dispose(ta);
    closebox;
    if tt<>ttt then begin
      e.Strings[strIdx]:=forms(tt,15)+mid(e.Strings[strIdx],16);
      sort_e;
      modi:=true;
      end;
  end;

  procedure MacroScope(strIdx : integer);
  var x,y,i  : Integer;
      brk    : boolean;
      enable : array[1..mtypes-1] of boolean;
      s: String;
  begin
    for i:=1 to mtypes-1 do
      enable[i]:=(e[strIdx][15+i]<>' ');
    dialog(24,mtypes+1,getres2(1009,0),x,y);    { 'Makro gÅltig im..' }
    for i:=1 to mtypes-1 do begin
      maddbool(3,1+i,getres2(1009,i),enable[i]); mhnr(589+i);
      end;
    freeres;
    readmask(brk);
    enddialog;
    if not brk then begin
      for i:=1 to mtypes-1 do
      begin
        s := e.Strings[strIdx];
        s[15+i]:=iifc(enable[i],'*',' ');
        e.Strings[strIdx] := s;
      end;
      modi:=true;
      end;
  end;


  { --- GebÅhrenzonen --------------------------------------------- }

  procedure EditPhoneEntry(neu:boolean; nr:integer; var brk:boolean);
  var x,y   : Integer;
      n     : integer;
      add   : integer;
      phe   : phoneap;
      i,j   : integer;
      first : boolean;

    procedure qsort(l,r:integer);
    var i,j : integer;
        x,w : phone1;
    begin
      i:=l; j:=r;
      x:=phe^[(l+r) div 2];
      repeat
        while phe^[i]<x do inc(i);
        while phe^[j]>x do dec(j);
        if i<=j then begin
          w:=phe^[i]; phe^[i]:=phe^[j]; phe^[j]:=w;
          inc(i); dec(j);
          end;
      until i>j;
      if l<j then qsort(l,j);
      if r>i then qsort(i,r);
    end;

  begin
    first:=(nr=1) or (nr=2);
    if first or neu then
      n:=1
    else begin
    { pushhp(806);}
      n:=minisel(34,10+(screenlines-25)div 2,'',getres2(1010,10),gpagepos);
    { pophp;}                                     { 'Seite ^1,...,Seite ^5' }
      if n<>0 then gpagepos:=abs(n);
      if n<1 then exit;
      end;
    add:=(n-1)*105;
    new(phe);
    fillchar(phe^,sizeof(phe^),0);
    dialog(iif(first,31,73),iif(first,3,iif(n=1,20,17)),
           iifs(n=1,'',phones^[nr].komment+' / ')+
           iifs(first,'',getreps2(1010,11,strs(gpagepos))),
           x,y);                                     { 'Seite %s' }
    with phones^[nr] do begin
      if n=1 then begin
        maddstring(3,2,getres2(1010,1),komment,19,19,''); mhnr(801);   { 'Zone ' }
        end;
      if nr>2 then begin
        if n=1 then begin
          maddtext(36,2,getres2(1010,2),col.ColDiaHigh);   { 'Die Vorwahlentabelle wird nur fÅr' }
          maddtext(36,3,getres2(1010,3),col.CoLDiaHigh);   { 'Fido-Direktanrufe benîtigt.' }
          end;
        if anz>0 then
          Move(ph^,phe^,anz*sizeof(phone1));
        for i:=0 to 6 do
          for j:=1 to 15 do begin
            maddstring(3+i*10,iif(n=1,4,1)+j,'',phe^[add+i*15+j],7,15,'0123456789-');
            mhnr(802);
            end;
        end;
      freeres;
      readmask(brk);
      enddialog;
      if not brk then begin
        modi:=true;
        if nr>2 then begin
          i:=0;
          for j:=1 to maxphone do         { leere EintrÑge entfernen }
            if (phe^[j]<>'') then begin
              inc(i);
              if i<>j then phe^[i]:=phe^[j];
              end;
          if anz>0 then
            freemem(ph,anz*sizeof(phone1));
          anz:=i;
          if anz>0 then begin
            qsort(1,anz);                 { Nummern sortieren }
            i:=0;                         { doppelte EintrÑge entfernen }
            for j:=1 to anz do
              if (i=0) or (phe^[j]<>phe^[i]) then begin
                inc(i);
                if i<>j then phe^[i]:=phe^[j];
                end;
            anz:=i;
            if anz>0 then begin
              getmem(ph,anz*sizeof(phone1));
              Move(phe^,ph^,anz*sizeof(phone1));
              end;
            end;
          end;
        end;
      end;
    dispose(phe);
  end;

  procedure NewPhone;
  var brk : boolean;
      i,j : integer;
  begin
    if anzahl=maxzones then begin
      rfehler1(1011,strs(maxzones));   { 'Maximal %s Zonen mîglich!' }
      exit;
      end;
    inc(anzahl);
    phones^[anzahl]:=phones^[1];
    phones^[anzahl].komment:='neue Zone';
    EditPhoneEntry(true,anzahl,brk);
    if brk then
      dec(anzahl)
    else
      for i:=1 to tables do
        for j:=1 to tarif^[i].zeitbereiche do
          with tarif^[i].zeitbereich[j].tarif[anzahl] do begin
            sekunden:=0; pfennig:=0; anwahl:=0;
            end;
  end;

  procedure DelPhone(nr:integer);
  var i,j : integer;
  begin
    if nr<=2 then
      rfehler(1003)   { 'Dieser Eintrag kann nicht gelîscht werden.' }
    else
      if ReadJN(getres(1005),true) then begin   { 'Eintrag lîschen' }
        if phones^[nr].anz>0 then
          freemem(phones^[nr].ph,phones^[nr].anz*sizeof(phone1));
        if a+CurRow<anzahl then begin
          Move(phones^[nr+1],phones^[nr],(anzahl-nr)*sizeof(phones^[1]));
          for i:=1 to tables do
            for j:=1 to tarif^[i].zeitbereiche do
              with tarif^[i].zeitbereich[j] do
                Move(tarif[nr+1],tarif[nr],(anzahl-nr)*sizeof(tarif[1]));
          end;
      dec(anzahl);
      modi:=true;
      end;
  end;

  procedure EditTarif(nr,page:integer; var brk:boolean);
  const etlen = 14;
  type  tet1  = array[1..5,1..maxzones] of string;
  var   x,y   : Integer;
        tt    : tet1;
        add   : integer;
        i,j : integer;
        sort  : array[1..maxzones] of byte;
        s     : string;
  begin
    add:=(page-1)*4;
    with tarif^[nr] do begin
      for i:=1 to anzahl do sort[i]:=i;
   {  with zeitbereich[add+1] do
        for i:=anzahl downto 2 do
          for j:=1 to i-1 do
            if tarif[sort[j]].sekunden/max(1,tarif[sort[j]].pfennig) <
               tarif[sort[j+1]].sekunden/max(1,tarif[sort[j+1]].pfennig)
            then begin
              k:=sort[j]; sort[j]:=sort[j+1]; sort[j+1]:=k;
              end; }
      for i:=1 to 4 do
        for j:=1 to anzahl do with zeitbereich[add+i].tarif[sort[j]] do
          if (sekunden=0) and (pfennig=0) and (anwahl=0) then
            tt[i,j]:=''
          else begin
            tt[i,j]:=strs(pfennig)+'/'+strsr(sekunden,3);
            while lastchar(tt[i,j])='0' do dellast(tt[i,j]);
            if lastchar(tt[i,j])='.' then dellast(tt[i,j]);
            if anwahl>0 then tt[i,j]:=tt[i,j]+'/'+strs(anwahl);
            end;
      s:=daytxt(nr);
      if s<>'' then s:=' ('+s+')';
      dialog(72,anzahl+5,getres2(1022,1)+' '+strs(nr)+s+  { 'Tarifgruppe' }
                         ' / '+getres2(1022,6)+' '+strs(page),x,y);  { 'Seite' }
      maddtext(17,2,getres2(1022,4),0);    { 'von' }
      maddtext(17,3,getres2(1022,5),0);    { 'bis' }
      for j:=1 to anzahl do
        maddtext(3,j+4,phones^[sort[j]].komment,0);
      for i:=1 to 4 do with zeitbereich[add+i] do begin
        maddtime(24+(i-1)*12,2,'',von,false); mhnr(807);
        maddtime(24+(i-1)*12,3,'',bis,false); mhnr(807);
        for j:=1 to anzahl do begin
          maddstring(24+(i-1)*12,j+4,'',tt[i,j],9,etlen,'0123456789/.');
          mhnr(808);
          end;
        end;
      readmask(brk);
      if not brk then begin
        for i:=1 to 4 do
          for j:=1 to anzahl do with zeitbereich[add+i].tarif[sort[j]] do
            if tt[i,j]='' then begin
              pfennig:=0;
              sekunden:=0;
              anwahl:=0;
              end
            else begin
              pfennig:=minmax(ival(GetToken(tt[i,j],'/')),0,9999);
              sekunden:=minmaxr(rval(GetToken(tt[i,j],'/')),0.1,9999);
              anwahl:=minmax(ival(tt[i,j]),0,9999);
              end;
        zeitbereiche:=maxzeitbereiche;
        while (zeitbereiche>1) and
              (length(trim(zeitbereich[zeitbereiche].von))<5) do
           dec(zeitbereiche);
        modi:=true;
        end;
      enddialog;
      end;
  end;


  procedure EditTarifTage(nr:integer; var brk:boolean);
  var x,y : Integer;
      i,j : integer;
  begin
    for i:=1 to maxwotage do DayUsed[i]:=false;
    for i:=1 to tables do
      if i<>nr then
        for j:=1 to maxwotage do
          if tarif^[i].wochentag[j] then DayUsed[j]:=true;
    dialog(ival(getres2(1022,9)),9,getreps2(1022,7,strs(nr)),x,y);
    with tarif^[nr] do begin      { 'GÅltigkeitsbereich der Tarifgruppe %s' }
      for i:=1 to 7 do begin
        maddbool(3,i+1,copy(_days_,(i-1)*_daylen_+1,_daylen_),wochentag[i]);
        mset1func(CheckDay);
        mhnr(809);
        end;
      for i:=8 to 10 do begin
        maddbool(ival(getres2(1022,10)),i-6,getres2(1022,8)+' '+strs(i-7),
                 wochentag[i]);              { 'Feiertag' }
        mset1func(CheckDay);
        mhnr(809);
        end;
      end;
    readmask(brk);
    if not brk then modi:=true;
    enddialog;
  end;


  procedure NewTarif;
  var i,j : integer;
      wt  : wt_array;
  begin
    if tables=maxtables then begin
      rfehler1(1012,strs(maxtables));   { 'Maximal %s Tarifgruppen mîglich!' }
      exit;
      end;
    fillchar(wt,sizeof(wt),true);
    for i:=1 to tables do
      for j:=1 to maxwotage do
        if tarif^[i].wochentag[j] then
          wt[j]:=false;
    i:=1;
    while (i<=maxwotage) and not wt[i] do inc(i);
    if i>maxwotage then begin
      rfehler(1013);       { 'Alle Wochen-/Feiertage sind bereits belegt.' }
      exit;
      end;
    for j:=i+1 to maxwotage do wt[j]:=false;
    inc(tables);
    with tarif^[tables] do begin
      wochentag:=wt;
      zeitbereiche:=1;
      fillchar(zeitbereich,sizeof(zeitbereich),0);
      zeitbereich[1].von:='00:00';
      zeitbereich[1].bis:='23:59';
      end;
    CurRow:=tables-a;    { alle Zeilen passen in 'gl' }
    modi:=true;
  end;

  procedure DelTarif(nr:integer);
  var i : integer;
  begin
    if nr=1 then
      rfehler(1014)      { 'Tarifgruppe 1 kann nicht gelîscht werden.' }
    else if readjn(getreps2(1022,11,strs(nr)),false) then begin  { 'Tarifgruppe %s lîschen' }
      for i:=nr+1 to maxtables do
        tarif^[i-1]:=tarif^[i];
      dec(tables);
      modi:=true;
      end;
  end;


  { --- Nachrichtenheader ----------------------------------------- }

  procedure InsertHeaderLine;
  var
    List: TLister;
      anz  : integer;
      used : set of byte;
      i    : integer;
      brk  : boolean;
      s    : string[40];
  begin
    if xhd.anz=maxheaderlines then begin
      rfehler1(1007,strs(maxheaderlines));    { 'Maximal %s Zeilen mîglich!' }
      exit;
      end;
    used:=[];
    for i:=0 to xhd.anz do
      include(used,xhd.v[i]);
    anz:=1;
    for i:=1 to res2anz(222)-1 do
      if not (i in used) then inc(anz);
    List := Listbox(30,min(anz,screenlines-5),getres2(1018,3));
    for i:=1 to res2anz(222)-1 do
      if not (i in used) then
        List.AddLine(' '+mid(getres2(222,i),cPos(' ',getres2(222,i))));
    List.AddLine('  ------------- '+getres2(1018,4));
    brk := List.Show;
    closebox;
    if not brk then begin
      s:=trim(List.GetSelection);
      i:=res2anz(222);
      while (i>0) and ((pos(s,getres2(222,i))=0) or (pos(s,getres2(222,i))>10) or
        (pos(s,getres2(222,i))+length(s)<length(getres2(222,i))-1)) do
        dec(i);
      inc(anzahl);
      Move(xhd.v[CurRow+a-1],xhd.v[CurRow+a],(xhd.anz-CurRow-a+1));    {um eine Position nach unten verschieben }
      xhd.v[CurRow+a-1]:=i;
      inc(xhd.anz);
      modi:=true;
    end;
    List.Free;
  end;

  procedure MoveHeaderLine;
  var b : byte;
  begin
    b:=xhd.v[movefrom-1];                   { zu verschiebendes Element }
    if movefrom<a+CurRow then               { nach unten einfÅgen }
      Move(xhd.v[movefrom],xhd.v[movefrom-1],a+CurRow-movefrom)  { Elemente eine Pos nach oben moven }
    else if movefrom>a+CurRow then
      Move(xhd.v[a+CurRow-1],xhd.v[a+CurRow],movefrom-a-CurRow); { Elemente eine Pos nach unten moven }
    xhd.v[a+CurRow-1]:=b;
    movefrom:=0;                            { Element an CursorPosition einfÅgen }
    modi:=true;
  end;

  procedure DelHeaderLine;
  var s : string;
  begin
    if xhd.anz = 0 then
      rfehler(1008)         { 'Es mu· mindestens eine Zeile vorhanden sein.' }
    else begin
      s:=getres2(222,xhd.v[a+CurRow-1]);
      s:=mid(s,blankpos(s)+1);
      if ReadJN(getreps2(1018,iif(xhd.v[a+CurRow-1]=0,6,5),s),true) then begin   { 'Zeile "%s" lîschen' }
        if a+CurRow-1<xhd.anz then Move(xhd.v[a+CurRow],xhd.v[a+CurRow-1],xhd.anz-a-CurRow+1);   { / 'Trennzeile lîschen' }
        dec(anzahl);
        dec(xhd.anz);
        modi:=true;
        end;
      end;
  end;


  { --- Nodelisten ------------------------------------------------ }

  procedure EditNodeEntry(strIdx :integer);
  var nlr : TNodeListItem;
      brk : boolean;
  begin
    nlr:=TNodeListItem(Nodelist.Items[strIdx]);
    EditNLentry(nlr,brk);
    if not brk then
    begin
      reindex:=reindex or
               (nlr.fformat<>TNodeListItem(nodelist.Items[strIdx]).fformat) or
               (nlr.zone<>TNodeListItem(nodelist.Items[strIdx]).zone) or
               ((nlr.format=3) and
                ((nlr.net<> TNodeListItem(nodelist.Items[strIdx]).net) or
                 (nlr.node<> TNodeListItem(nodelist.Items[strIdx]).node)));
      Nodelist.Items[strIdx] :=nlr;
      modi:=true;
    end;
  end;

  procedure TextEditNodelist(n:integer);
  var fn : string;
      ft : longint;
  begin
    fn:=FidoDir+NodeList.GetFilename(n);
    ft:=filetime(fn);
    editfile(fn,false,false,0,false);
    if filetime(fn)<>ft then reindex:=true;
  end;


  procedure DelNodeentry( strIdx: Integer);
  var brk : boolean;

    procedure del_it;
    var
      Item: TNodeListItem;
    begin
      if a+CurRow-1<anzahl then
      begin
        Item := NodeList.Items[strIdx];
        Item.Free;
        NodeList.Delete(strIdx);
        dec(anzahl);
        modi:=true;
        reindex:=true;
      end;
    end;

  begin
    pushhp(932);
    case ReadIt(ival(getres2(1019,2)),getres2(1019,3),
                                 { 'Node-/Pointlisteneintrag lîschen' }
                getres2(1019,4), { ' ^Ja , ^Nein , incl. ^Datei ' }
                1,brk) of
      1 : del_it;
      3 : begin
            if FileExists(FidoDir+NodeList.GetFilename(strIdx)) then
              _era(FidoDir+NodeList.GetFilename(strIdx));
            del_it;
          end;
    end;
    pophp;
  end;


  procedure NL_info;
  const bs = 4096;
  var x,y : Integer;
      fn  : string[12];
      t   : text;
      buf : pointer;
      s   : string;
      n   : longint;
      brk : boolean;
  begin
    msgbox(ival(getres2(2129,0)),8,getres2(2129,1),x,y);   { 'Node-/Pointlisten-Info' }
    moff;
    attrtxt(col.colmboxhigh);
    wrt(x+3,y+2,getres2(2129,2));       { 'Datei' }
    wrt(x+3,y+3,getres2(2129,3));       { 'Bytes' }
    wrt(x+3,y+4,getres2(2129,4));       { 'EintrÑge' }
    attrtxt(col.colmbox);
    fn:=NodeList.GetFilename(a+CurRow-1);
    wrt(x+14,y+2,fn);
    if not FileExists(FidoDir+fn) then
      wrt(x+14,y+3,' - fehlt -')
    else
      wrt(x+14,y+3,trim(strsrnp(_filesize(FidoDir+fn),15,0)));
    mon;
    if FileExists(FidoDir+fn) then begin
      getmem(buf,bs);
      assign(t,FidoDir+fn);
      settextbuf(t,buf^,bs);
      reset(t);
      n:=0;
      brk:=false;
      while not eof(t) and not brk do begin
        readln(t,s);
        if (s<>'') and (FirstChar(s)<>';') then inc(n);
        if (n mod 100=0) then begin
          mwrt(x+14,y+4,strs(n));
          testbrk(brk);
          end;
        end;
      close(t);
      freemem(buf,bs);
      mwrt(x+14,y+4,strs(n));
      end;
    mwrt(x+3,y+6,getres(12));     { 'Taste drÅcken ...' }
    wait(curon);
    closebox;
  end;


  { --- Eingabe --------------------------------------------------- }

  procedure getboxsel;
  var d   : DB;
      box : string[40];
  begin
    boxsel1:='';
    boxsel2:='';
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do begin
      box:='˘NETCALL '+dbReadStr(d,'boxname');
      if length(boxsel1)<220 then
        boxsel1:=boxsel1+box
      else if length(boxsel2)<220 then
        boxsel2:=boxsel2+box;
      dbNext(d);
      end;
    dbClose(d);
    delfirst(boxsel1);
    delfirst(boxsel2);
  end;

  procedure readbutt;
  begin
    if auswahlcursor then begin
      rbx:=x+1; rby:=y+CurRow;
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
        if eanzahl>0 then CurRow:=min(eanzahl-a,yy-y) else else //???
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

begin   {procedure UniEdit(typ:byte); }
  eList := false;                   //kein 0 basierende  TStringList (e)
  case typ of
    1 : begin                       { Timing-Liste }
          filewidth:=TimingWidth;
          tnr:=ReadTimingNr(brk);
          if brk then exit;
          loadfile(1,TimingFile+strs(tnr));
          width:=74;
          buttons:=getres(1011);   { ' ^Neu , ^Lîschen , ^Edit , ^Aktiv , ^Sichern , ^OK ' }
          okb:=6; edb:=3;
          getboxsel;
          pushhp(510);
          eList := true;
        end;
    2 : begin                       { Tastenmakros }
          filewidth:=KeymacWidth;
          loadfile(2,keydeffile);
          sort_e;
          width:=66+mtypes;
          buttons:=getres(1012);   { ' ^Neu , ^Lîschen , ^Edit , ^Taste , ^*** , ^Sichern , ^OK ' }
          okb:=7; edb:=3;
          _bunla:='˘'+getres2(1000,0); freeres;
          pushhp(540);
          eList := true;
        end;
    3,
    6 : begin                       { GebÅhrenzonen; Tarifgruppen }
          filewidth:=gebWidth;
          LoadPhoneZones;
          width:=53;
          buttons:=getres(1013);   { ' ^Neu , ^Lîschen , ^Edit , ^Sichern , ^OK ' }
          okb:=5; edb:=3;
          pushhp(iif(typ=3,800,805));
        end;
    4 : begin                       { Nachrichtenkopf }
          filewidth:=30;
          anzahl:=xhd.anz;
          width:=ival(getres2(1018,1));
          buttons:=getres2(1018,2);   { ' ^EinfÅgen , ^Verschieben , ^Lîschen ,  ^OK  ' }
          okb:=4; edb:=0;
          pushhp(900);
          xhd:=ExtraktHeader;
          anzahl:=xhd.anz;          { das ist ÅberflÅssig ?}
        end;
    5 : begin                       { Nodelisten }
          DisableAltN:=true;
          filewidth:=255;
          anzahl:=NodeList.Count;
          width:=70;
          buttons:=getres2(1019,1);   { ' ^Neu , ^Edit , ^TextEdit , ^Lîschen , ^Info , ^OK ' }
          okb:=6; edb:=2;
          pushhp(930);
          reindex:=false;
        end;
  end;
  gl:=screenlines-fnkeylines-12;      {Anzahl der Schirm Zeilen - Anzahl der Funktionstasten - 12 Leerzeilen }
  bp:=1;
  selbox(width+2,gl+4,'',x,y,false);  { Rahmen zeichnen }
  attrtxt(col.colsel2rahmen);
  case typ of
    1 : mwrt(x+width-4,y,' '+strs(tnr)+' ');
  end;
  mwrt(x,y+gl+1,'√'+dup(width,'ƒ')+'¥');
  t:='!';    { Buttons nur anzeigen }
  a:=0; CurRow:=1; movefrom:=0;             { Curser Zeile }
  readbutt;

  modi:=false;
  maus_pushinside(x+1,x+width,y+1,y+gl);
  autobremse:=true;
  poutside:=false;
  repeat
    if CurRow+a>eanzahl then
      if CurRow>1 then dec(CurRow)
      else if a>0 then dec(a);
    display;
    autoupenable:=(a+CurRow>1);
    autodownenable:=(a+CurRow<eanzahl);
    t:='*';
    readbutt;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if (t=keyesc) or ((movefrom>0) and (nr=0)) then begin
      movefrom:=0; t:=#0; nr:=99;
      end;
    if typ=4 then
      if t=keyins then nr:=1
      else if t=keydel then nr:=3;
    if (nr<>0) and (nr<>99) then bp:=abs(nr);
    { c:=UpCase(t[1]); }
    if (nr=1) and (eanzahl=maxentries) then
      rfehler1(1004,strs(maxentries))   { 'Maximal %s EintrÑge mîglich!' }
    else
      case typ of
        1 : case nr of
              1 : NewTiming;
              2 : if CurRow+a<=anzahl then DelEntry(a+CurRow-1);    //a+CurRow-1
              3 : if CurRow+a<=anzahl then EditTiming(a+CurRow-1);
              4 : if CurRow+a<=anzahl then ChangeActive(a+CurRow-1);
              5 : begin
                    savefile(1,TimingFile+strs(tnr));
                    modi:=false;
                    keyboard(keyrght);
                  end;
            end;
        2 : case nr of
              1 : NewMacro;
              2 : if CurRow+a<=anzahl then DelEntry(a+CurRow-1);  //EintragLîschen
              3 : if CurRow+a<=anzahl then EditMacro(a+CurRow-1); //Macro Tastenfolge bearbeiten
              4 : if CurRow+a<=anzahl then MacroKey(a+CurRow-1);  //
              5 : if CurRow+a<=anzahl then MacroScope(a+CurRow-1);
              6 : begin
                    savefile(2,KeydefFile);
                    modi:=false;
                    keyboard(keyrght);
                  end;
            end;
        3 : case nr of
              1 : NewPhone;
              2 : if a+CurRow<=anzahl then DelPhone(a+CurRow);
              3 : if a+CurRow<=anzahl then EditPhoneEntry(false,a+CurRow,brk);
              4 : begin
                    SavePhoneZones;
                    modi:=false;
                    keyboard(keyrght);
                  end;
            end;
        4 : if (nr>0) and (movefrom<>0) then
              MoveHeaderline
            else
              case nr of
                1 : InsertHeaderLine;
                2 : movefrom:=a+CurRow;
                3 : DelHeaderLine;
              end;
        5 : case nr of
              1 : if NewNodeentry then
                  begin
                    Anzahl := NodeList.Count;
                    modi:=true;
                    reindex:=true;
                  end;
              2 : if a+CurRow<=anzahl then EditNodeentry(a+CurRow-1);
              3 : if a+CurRow<=anzahl then TextEditNodelist(a+CurRow-1);
              4 : if a+CurRow<=anzahl then DelNodeentry(a+CurRow-1);
              5 : if a+CurRow<=anzahl then NL_Info;
            end;
        6 : case nr of
              1 : NewTarif;
              2 : DelTarif(a+CurRow);
              3 : begin
                    pushhp(806);
                    n:=minisel(x+20,y+5,'',getres2(1022,3),pagepos);
                    pophp;
                    if n<>0 then pagepos:=abs(n);   { 'Seite ^1,Seite ^2,Seite ^3,^Tage' }
                    case n of
                      1..3 : EditTarif(a+CurRow,n,brk);

                         4 : EditTariftage(a+CurRow,brk);
                    end;
                  end;
              4 : begin
                    SavePhoneZones;
                    modi:=false;
                    keyboard(keyrght);
                  end;
            end;
      end;
    if nr<0 then begin
      if t=keyup then
        if CurRow>1 then dec(CurRow)
        else if a>0 then dec(a);
      if (t=keydown) and (a+CurRow<eanzahl) then
        if CurRow<gl then inc(CurRow)
        else inc(a);
      if t=keypgup then
        if a=0 then t:=keyhome
        else a:=max(0,a-gl);
      if t=keypgdn then begin
        if a+gl>=eanzahl then CurRow:=eanzahl-a
        else inc(a,gl);
        CurRow:=max(1,min(CurRow,eanzahl-a));
        end;
      if t=keyhome then begin
        a:=0; CurRow:=1;
        end;
      if t=keyend then begin
        a:=max(0,eanzahl-gl);
        CurRow:=max(1,eanzahl-a);
        end;
      if t=keychom then CurRow:=1;
      if t=keycend then CurRow:=minmax(gl,1,eanzahl-a);
      end;
    if (typ=4) and (nr=okb) and modi then begin
      { if ReadJNesc(getres(1019),true,brk) then begin  } { 'énderungen sichern' }
      ExtraktHeader:=xhd;
      GlobalModified;
      modi:=false;
      end;
    if (typ=5) and ((nr=0) or (nr=okb)) and modi then begin
      oldft:=filetime(NodelistCfg);
      NodeList.SaveConfigToFile;
      if (oldft<>0) and not reindex then  { autom. Neuindizierung bei }
        setfiletime(NodelistCfg,oldft);   { nÑchstem Programmstart verhindern }
      modi:=false;
      end;
  until ((nr=0) or (nr=okb)) and
        (not modi or ReadJN(getres(1015),false));   { 'énderungen verwerfen' }
  maus_popinside;
  pophp;
  closebox;
  case typ of
    1 : releaseliste;
    2 : begin
          releaseliste;
          {for ii:=keymacros downto 1 do
            freemem(macrodef[ii],length(macrodef[ii])+1);}
          readkeydefs;
        end;
    3 : FreePhoneZones;
    4 : freeres;
    5 : begin
          if reindex then
          begin
            KeepNodeindexClosed;
            if Nodelist.Open then CloseNodeIndex;
            if NodeList.Count=0 then
            begin
              DeleteFile(NodeIndexF);
              DeleteFile(UserIndexF);
              DeleteFile(NodelistCfg);
              Nodelist.Open:=false;
            end
            else begin
              MakeNodelistindex;
              OpenNodeindex(NodeIndexF);
            end;
          end;
          DisableAltN:=false;
        end;
    6 : begin
          FreePhoneZones;
          freeres;
        end;
  end;
end;


{ --- GebÅhrendaten fÅr Telefonnummer -> BoxPar^ lesen --------------- }

procedure GetPhoneGebdata(var telefon:string);
var pfound: integer;
    lfound: integer;
    i     : integer;
    lvw   : string[5];
    manz  : integer;
    mfwdt : integer;

  procedure Seek(i:integer; exact:boolean);
  var j : integer;
      s : phone1;
  begin
    with phones^[i] do
      for j:=1 to anz do begin
        s:=ph^[j];
        if LeftStr(s,length(natvorwahl))=natvorwahl then
          delete(s,1,length(natvorwahl));              { '0' entfernen }
        if cpos('-',s)=0 then
          s:=lvw+s;
        while cpos('*',s)>0 do
          delete(s,cpos('*',s),1);
        if (exact and (LeftStr(telefon,length(s)+1)=s+'-') or
            not exact and (LeftStr(telefon,length(s))=s))
           and (length(s)>lfound)
        then begin
          pfound:=i;
          lfound:=length(s);
          end;
        end;
  end;

begin
  manz:=anzahl;
  if manz>0 then begin    { Reentrance ... }
    mfwdt:=filewidth;
    spush(e,manz*4);
    end;
  LoadPhonezones;
  lvw:=LeftStr(vorwahl,cpos('-',vorwahl));   { eigene Landesvorwahl incl. "-" }
  if LeftStr(telefon,cpos('-',vorwahl))=lvw then
    BoxPar^.gebzone:=phones^[1].komment
  else
    BoxPar^.gebzone:=phones^[2].komment;
  pfound:=0; lfound:=0;
  for i:=1 to anzahl do
    Seek(i,true);
{  if pfound=0 then begin    (was soll das??)
    i:=1;
    while (i<=anzahl) and (pfound=0) do begin
      seek(i,false);
      inc(i);
      end;
    end; }
  if pfound=0 then
    for i:=1 to anzahl do
      Seek(i,false);
  if pfound>0 then with phones^[pfound] do
    BoxPar^.gebzone:=komment;
  FreePhonezones;
  if manz>0 then begin
    anzahl:=manz;
    filewidth:=mfwdt;
    spop(e);
    end;
end;


{ GebÅhren anhand von boxpar^.GebZone berechnen }

function CalcGebuehren(var startdate,starttime:datetimest; secs:real):real;
var i       : integer;
    dow,tag : integer;    { 1 = Mo }
    zone    : integer;
    h,m     : word;
    s,sum   : real;
    manz    : integer;

  function IsBilligtag(d:fdate):boolean;
  begin
    IsBilligtag:=(d.m=12) and (d.t>=24) and (d.t<=31);
  end;

  procedure GetDow;     { Wochentag bzw. Feiertagskategorie ermitteln }
  var datum : fdate;
      t     : text;
      s     : string;
  begin
    dow:=0;
    assign(t,FeierDat);
    if existf(t) then begin
      reset(t);
      while not eof(t) and (dow=0) do begin
        readln(t,s);
        if (firstchar(s)<>'#') and
           (GetToken(s,' ')=LeftStr(startdate,6)+RightStr(startdate,2)) then
          dow:=7+ival(GetToken(s,' '));
        end;
      close(t);
      end;
    if (dow<1) or (dow>maxwotage) then begin
      datum.t:=ival(LeftStr(startdate,2));
      datum.m:=ival(copy(startdate,4,2));
      datum.j:=ival(RightStr(startdate,4));
      if IsBilligtag(datum) or IsFeiertag(datum) then
        dow:=8
      else
        dow:=montage.ddow(datum);       { zum Wochentag passende Tabelle }
      end;
  end;

begin           {function CalcGebuehren(var startdate,starttime:datetimest; secs:real):real;}
  manz:=anzahl;     { Reentrance aus Timingliste }
  sum:=0;
  LoadPhonezones;
  zone:=anzahl;     { Nummer der GebÅhrenzone ermitteln }
  while (zone>0) and not stricmp(boxpar^.gebzone,phones^[zone].komment) do
    dec(zone);
  GetDow;           { Wochentag bzw. Feiertagskategorie ermitteln }
  tag:=tables;
  while (tag>0) and not tarif^[tag].wochentag[dow] do
    dec(tag);

  if (zone>0) and (tag>0) then with tarif^[tag] do begin
    h:=ival(LeftStr(starttime,2));
    m:=ival(copy(starttime,4,2));
    s:=ival(copy(starttime,7,2));
    starttime:=LeftStr(starttime,5);   { Sekunden abschneiden }
    repeat                          { ZÑhlschleife; wird pro Einheit }
      i:=zeitbereiche+1;            { einmal durchlaufen             }
      repeat
        dec(i);
      until (i=0) or
            ((starttime>=zeitbereich[i].von) and (starttime<=zeitbereich[i].bis)) or
            ((zeitbereich[i].von>zeitbereich[i].bis) and
             ((starttime>=zeitbereich[i].von) or (starttime<=zeitbereich[i].bis)));
      if i=0 then
        secs:=0
      else with zeitbereich[i].tarif[zone] do begin
        if sekunden<0.01 then break;
        incr(sum,pfennig);
        secs := secs-sekunden;      { berechnete Sekunden abziehen   }
        s := s + sekunden;          { Startzeitpunkt der nÑchsten... }
        while (s>59) do begin       { Einheit berechnen              }
          s := s-60; inc(m);
          if m>59 then begin
            dec(m,60); inc(h);
            if h>23 then h:=0;
            end;
          end;
        starttime:=formi(h,2)+':'+formi(m,2);
        end;
    until secs <= 0;
    end;

  FreePhonezones;
  anzahl:=manz;
  CalcGebuehren := sum/100;
end;


function Einheitenpreis:real;
var manz : integer;
begin
  manz:=anzahl;
  LoadPhonezones;
  if tables>0 then
    Einheitenpreis := tarif^[1].zeitbereich[1].tarif[1].pfennig / 100.0
  else
    Einheitenpreis := 0.12;
  FreePhonezones;
  anzahl:=manz;
end;


procedure AppPhoneZones;   { mappsel() fÅr GebÅhrenzonen }
var i : integer;
begin
  LoadPhoneZones;
  for i:=1 to anzahl do
    mappsel(true,phones^[i].komment);
  FreePhoneZones;
end;


procedure gtest1;
var nr : string;
begin
  attrtxt(7);
  inout.cursor(curon);
  writeln;
  repeat
    write('Nummer im Nodelist-Format: ');
    readln(nr);
    if nr<>'' then begin
      GetPhoneGebData(nr);
      writeln('Zone                     : ',boxpar^.gebzone);
      end;
    writeln;
  until nr='';
end;


procedure gtest2;
var d,t,ss : datetimest;
    secs   : longint;
begin
  attrtxt(7);
  inout.cursor(curon);
  writeln;
  repeat
    write('Zone:     '); readln(boxpar^.gebzone);
    write('Datum:    '); readln(d);
    write('Zeit:     '); readln(t);
    write('Sekunden: '); readln(ss);
    secs:=ival(ss);
    if d<>'' then
      writeln('GebÅhren: ',CalcGebuehren(d,t,secs):6:2);
    writeln;
  until d='';
end;


procedure gtest;
begin
  if ParG1 then gtest1;
  if ParG2 then gtest2;
end;

{$I XP10.INC}    { Timinglisten-Interpreter }

initialization
  e := TStringList.Create;
finalization
  e.free;
end.
{
  $Log$
  Revision 1.58  2001/08/11 23:06:28  mk
  - changed Pos() to cPos() when possible

  Revision 1.57  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.56  2001/07/27 19:01:01  ma
  - changed behaviour of macro entering routine, works with Win9x now

  Revision 1.55  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.54  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.53  2001/01/07 12:34:37  mo
  - einig  ƒnderungen an TNodeList

  Revision 1.52  2001/01/06 21:13:35  mo
  - ƒnderung an TnodeListItem

  Revision 1.51  2001/01/06 17:18:07  mk
  - fixed some TNodeListItem-Bugs

  Revision 1.50  2001/01/04 16:10:45  ma
  - adjusted unit names in "uses" statement

  Revision 1.49  2000/12/29 16:44:25  mo
  - class TNodeList, new procedure AddEntry

  Revision 1.48  2000/12/27 22:36:36  mo
  -new class TfidoNodeList

  Revision 1.47  2000/12/25 14:02:40  mk
  - converted Lister to class TLister

  Revision 1.46  2000/12/10 10:54:56  mo
  -TNodelistItem in eine Klasse umgewandelt

  Revision 1.45  2000/12/06 22:29:44  mo
  -indexfehler bei nodelistenlˆschung besetigt

  Revision 1.44  2000/12/05 17:55:56  ml
  - removed illegal character

  Revision 1.43  2000/11/16 19:42:57  hd
  - DOS Unit entfernt

  Revision 1.42  2000/11/14 15:51:27  mk
  - replaced Exist() with FileExists()

  Revision 1.41  2000/10/17 10:05:45  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.40  2000/10/11 23:03:12  mk
  - Gebuehrenaenderung rueckgaengig gemacht

  Revision 1.38  2000/09/29 11:27:43  fe
  Ungenutzte, lokale Variablen entfernt.

  Revision 1.37  2000/08/25 23:02:07  mk
  JG:
  - "< >" in Macros funktioniert jetzt wie dokumentiert als Leertastenersatz
    XP10.PAS
  - Parameter -K verarbeitet jetzt ganze Zeichenketten. Benoetigt
    Anfuehrungszeichenauswertung damit Tasten wie <Enter> funktionieren !
    XP10.PAS,XP2.PAS
  - Neuer Parameter -mailto: dem direkt ein Mailto-Link uebergeben wird
    Subjects mit Leerzeichen benoetigen Anfuehrungszeichenauswertung !
    XP2.PAS

  Revision 1.36  2000/08/20 11:57:32  mk
  MO:- weitere Index-Fehler behoben

  Revision 1.35  2000/08/15 11:12:23  mk
  MO: Bugfixes und Anpassungen fuer > 80 Spalten

  Revision 1.34  2000/08/14 23:04:35  mk
  MO:- eanzahl() liefert jetzt immer den richtigen Wert

  Revision 1.32  2000/08/14 14:45:14  mk
  MO: Umfangreiche Aenderung fuer Null basierende Stringlisten

  Revision 1.31  2000/08/13 10:39:44  mk
  - Fixes fuer Variable e

  Revision 1.30  2000/08/09 19:51:34  mk
  - verschiedene Fixes fuer Timeingliste
  - Netcall/Alle funktioniert jetzt

  Revision 1.29  2000/08/08 17:15:31  mk
  MO: TextEditNodelist nutzt jetzt den richtigen Index

  Revision 1.28  2000/08/08 13:18:14  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.27  2000/08/04 09:06:25  mk
  - Bug in NLFilename nach Stringlistumestellung behoben

  Revision 1.26  2000/08/03 21:30:14  mk
  - FPC Kompatiblitaetsfix

  Revision 1.25  2000/08/03 20:08:17  mk
  - e[] auf TStringlist umgestellt

  Revision 1.24  2000/08/01 16:29:56  mk
  - FPC Kompatibliltaet erhoeht

  Revision 1.23  2000/08/01 08:40:40  mk
  - einige String-Parameter auf const geaendert

  Revision 1.22  2000/07/21 21:17:44  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.21  2000/07/20 13:36:39  hd
  - ExErase entfernt (DeleteFile ist in SysUtils)
  - AnsiString und 32-Bit
  - Fix: Copy(x,y,255) kopiert evtl. nur noch einen Teilstring.

  Revision 1.20  2000/07/13 17:15:15  mk
  - noch einige ^String-spezifische Probleme beseitigt

  Revision 1.19  2000/07/13 10:23:46  mk
  - Zeiger auf Strings entfernt

  Revision 1.18  2000/07/12 15:27:01  hd
  - Ansistring

  Revision 1.17  2000/07/12 14:43:44  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.16  2000/07/06 09:23:08  mk
  - _days_ in String umgewandelt

  Revision 1.15  2000/07/05 17:58:26  hd
  - Ansistring

  Revision 1.14  2000/07/04 12:04:19  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.13  2000/07/03 13:31:39  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.12  2000/07/02 14:24:52  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.11  2000/06/23 15:59:16  mk
  - 16 Bit Teile entfernt

  Revision 1.10  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.9  2000/05/06 17:29:21  mk
  - DOS DPMI32 Portierung

  Revision 1.8  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

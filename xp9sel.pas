{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }


{ CrossPoint - UniSel: Select-/Test-Routinen fuer Unisel-MenÅs }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp9sel;

interface

uses
  crt,dos,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
  maus2,mouse,resource,xpglobal,
     xp0,xp1,xp1o,xp1o2,xp1input,xp2c,dosx,lfn;

procedure SelSchab(var cr:CustomRec);
function  xp9_testbox(var s:string):boolean;
function  xp9_setclientFQDN(var s:string):boolean;
function  xp9_FQDNTest(var s:string):boolean;
function  zidtest(var s:string):boolean;
function  toggleSysop(var s:string):boolean;
function  validfile(var s:string):boolean;
function  testfidodir(var s:string):boolean;
function  testqwkinfiles(var s:string):boolean;
procedure set_uparcext(var s:string);
procedure set_downarcext(var s:string);
function  progtest(var s:string):boolean;
function  testmbretter(var s:string):boolean;
procedure gf_getntyp(var s:string);
function  testbaud(var s:string):boolean;
function  testbossnode(var s:string):boolean;
procedure setfidoadr(var s:string);
procedure ps_setempf(var s:string);
function  notempty2(var s:string):boolean;
function  testreplyto(var s:string):boolean;
procedure uucp_getloginname(var s:string);
function  testuucp(var s:string):boolean;
procedure SetDomain(var s:string);
procedure SetDomain2(var s:string);
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
function  is_mailaddress(const s:string):boolean;
function  multi_Mailstring(var s:string):boolean;
function  check_envelope(var s:string):boolean;
function  ReadExtCfgFilename(const txt:atext; var s1:string; var cdir:PathStr; subs:boolean):boolean;
procedure EditAddServersList(var cr:customrec);
procedure SingleServerSel(var cr:customrec);
procedure set_AddServers_Allowances(var s:string);
procedure set_ExtCfg_Allowances;
procedure reset_Allowances(var s:string);
function  addServersTest(var s:string):boolean;
function  BfgToBox(var s:string):string;
function  BoxToBfg(var s:string):string;
procedure SetUsername(s:string);

implementation

uses
  xp2b,xp2,xp3,xp3o,xp4e,xp9,xp9bp,xp10,xpnt,xpterm,xpovl,lister;


type box_array = array[0..maxboxen] of string[BoxNameLen];
                 { Box-Ergebnisliste => Eingabefeld }


function BoxSelect(const entries:byte; boxlist:box_array; colsel2:boolean):string;
const width = 51+BoxNameLen;
var   d          : DB;
      brk        : boolean;
      x,y,height,
      i,nt,
      sel_anz    : byte;               { Anzahl der auszuwÑhlenden Boxen }
      box        : string[BoxNameLen]; { Name der aktuellen Box          }
      user       : string[20];         { Username der aktuellen Box      }
      komm       : string[25];         { Kommentar der aktuellen Box     }
      boxline    : string[width];      { angezeigte Zeile in Boxauswahl  }
label nextBox;
begin
  BoxSelect:=''; brk:=false;
  height:=screenlines-17;
  if screenlines>30 then dec(height,2);
  if screenlines>40 then dec(height,2);
  dbOpen(d,BoxenFile,1);
  sel_anz:=0;
  while not dbEOF(d) do
  begin
    box:=dbReadStr(d,'Boxname');
    if own_Name <> '' then
      for i:=1 to entries do
        if ustr(box)=ustr(boxlist[i]) then  { Box schon ausgewÑhlt?      }
          goto nextBox;                     { ...dann nÑchsten Datensatz }
    dbRead(d,'Netztyp',nt);
    if ((nt=own_Nt) and (ustr(box)<>own_Name))   { passende Box gefunden }
      or (own_name='') then
    begin
      inc(sel_anz);
      komm:=dbReadStr(d,'Kommentar');
      if nt=nt_Client then user:=dbReadStr(d,'Email')
      else user:=dbReadStr(d,'Username');
      boxline:=' '+forms(box,BoxNameLen)+'  '+forms(user,20)+
               '  '+forms(komm,25);
      if sel_anz=1 then      { bei erster gefundener Box Dialog aufbauen }
      begin
        if own_name <> '' then
        begin
          if colsel2 then
          begin                             { 'Serverboxen (Netztyp %s)' }
            selbox(width+2,height+4,getreps2(936,3,Netz_Typ(nt)),x,y,false);
            openlist(x+1,x+width,y+1,y+height+2,0,'/NS/SB/DM/NLR/NA');
            Listbox2col;
            listarrows(x,y+1,y+height+2,col.colsel2rahmen,
                       col.colsel2rahmen,'≥');
          end
          else begin                        { 'Serverboxen (Netztyp %s)' }
            selbox(width+2,height+4,getreps2(936,3,Netz_Typ(nt)),x,y,true);
            openlist(x+1,x+width,y+1,y+height+2,0,'/NS/SB/DM/NLR/NA');
            Listboxcol;
            listarrows(x,y+1,y+height+2,col.colselrahmen,
                       col.colselrahmen,'≥');
          end;
        end
        else begin                             { '/Netcall/Spezial bei:' }
          selbox(width+2,height+4,getres2(1024,3)+' '+getres2(1024,5),
          x,y,false);
          openlist(x+1,x+width,y+1,y+height+2,0,'/NS/SB/DM/NLR/NA');
          Listbox2col;
          listarrows(x,y+1,y+height+2,col.colsel2rahmen,
                     col.colsel2rahmen,'≥');
        end;
      end;
      app_L(boxline);
    end;
    nextBox:
    dbNext(d);
  end;
  dbClose(d);
  if sel_anz > 0 then       { Wenn Box(en) gefunden, Auswahl }
  begin
    list(brk);
    BoxSelect:=trim(copy(get_selection,2,BoxNameLen));
    closelist;
    closebox;
    if brk then BoxSelect:='';
  end else
    rfehler(953); { 'Keine (weiteren) hinzuzufÅgenden Serverboxen vorhanden!' }
  if own_Name <> '' then
end;


procedure EditAddServersList(var cr:customrec);
var   d          : DB;
      x,y,nt     : byte;
      t          : taste;
      nr,bp      : shortint;
      gl,width   : byte;
      buttons    : string[60];
      okb,edb    : shortint;
      p,n        : shortint;
      a,ii       : integer;
      s1         : string;
      modi       : boolean;
      poutside   : boolean;
      movefrom   : integer;
      entries    : integer;
var   boxlist    : box_array;
label Start;

  { Die hier mehrfach vorkommende PrÅfung "if own_Name <> '' then..."  }
  { dient zur Feststellung, ob wir in einem Box-Config-Dialog (z.B.    }
  { 'ZusÑtzliche Server' bei RFC/Client oder 'Pakete mitsenden' bei    }
  { Fido) sind oder aus 'EditNetcallDat' in xp10.pas kommen. In        }
  { letzterem Fall ziehen wir abweichende (= weniger restriktive)      }
  { Konsequenzen - z.B. lassen wir nach positiv beantworteter          }
  { RÅckfrage Dupes zu und Åbergehen ÅberflÅssige bzw. unzutreffende   }
  { Tests wie "eingetragene Box = editierte Box?".                     }

  { Hinweis: 'EditAddServersList' wird sowohl als 'normale' Prozedur   }
  { als auch als Select-Routine (User drÅckt <F2> im Eingabefeld)      }
  { mittels 'mappcustomsel(EditAddServersList,true)' in '_EditPPP'     }
  { (xp9.inc) aufgerufen. Aus diesem Grund kînnen wir die oben         }
  { angesprochenen Bedingungen nicht als Parameter Åbergeben (geht bei }
  { 'mappcustomsel' halt nicht) und fragen sie daher Åber Variablen ab.}

  procedure display;
  var i    : shortint;
      box  : string[BoxNameLen];
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
    fwrt(x+width+1,y+1,iifc(a=0,'≥',#30));
    fwrt(x+width+1,y+gl,iifc(a+gl<entries,#31,'≥'));
    mon;
  end;

  procedure InsertBox;
  var   i         : byte;
        boxlen,
        bfglen    : word;
        box       : string[BoxNameLen];
        bfg       : string[8];
        add       : byte;
        d         : DB;
        too_long  : boolean;
  const maxboxlen : byte = 255;
        maxbfglen = 160;
  begin
    if own_Name = '' then maxboxlen:=249;  { wegen mappsel-String-Addition  }
    dbOpen(d,BoxenFile,1);                 { (lfd. Nr.) in 'EditNetcallDat' }
    boxlen:=0; bfglen:=0;
    too_long:=false;
    for i:=1 to entries do
    begin
      boxlen:=boxlen + (length(boxlist[i])+1);  { GesamtlÑnge Boxnamen }
      if own_Name <> '' then
      begin
        dbSeek(d,boiName,ustr(boxlist[i]));
        if dbfound then
        begin
          bfg:=dbreadStr(d,'dateiname');
          bfglen:=bfglen + (length(bfg)+1);    { GesamtlÑnge BFG-Namen }
        end;
      end;
    end;
    if boxlen >= maxboxlen then
    begin { 'Maximale EingabelÑnge (%s) fÅr Serverbox-Namen erreicht!' }
      too_long:=true;
      rfehler1(955,strs(maxboxlen));
    end;
    if own_Name <> '' then
      if bfglen >= maxbfglen then
      begin { 'Maximale EingabelÑnge (%s) fÅr Dateinamen (.BFG) erreicht!' }
        too_long:=true;
        rfehler1(956,strs(maxbfglen));
      end;
    if too_long then
    begin
      dbClose(d);
      exit;
    end;
    box:=BoxSelect(entries,boxlist,iifb(own_Name <> '',false,true));
    if box <> '' then
    begin
      if own_Name = '' then      { Abfrage nicht bei Box-Config-Dialog }
      begin
        i:=entries;
        while (i>0) and (ustr(box)<>ustr(boxlist[i])) do
          dec(i);                           { Eintrag schon vorhanden? }
        if i > 0 then
          if not ReadJN(getreps2(10900,57,box),false) then
           { 'Serverbox "%s" bereits vorhanden - trotzdem hinzufÅgen?' }
          begin
            dbClose(d);
            exit;
          end;
      end;
      if own_Name <> '' then
      begin
        dbSeek(d,boiName,ustr(box));
        if dbfound then
          bfg:=dbreadStr(d,'dateiname');
      end;
      if boxlen + length(box) > maxboxlen then
      begin
        too_long:=true;
        rfehler1(958,strs(maxboxlen-boxlen));
     { 'Eingabe zu lang (Serverbox-Namen)! Noch %s Zeichen verfÅgbar.' }
      end;
      if own_Name <> '' then
        if bfglen + length(bfg) > maxbfglen then
        begin
          too_long:=true;
          rfehler1(959,strs(maxbfglen-bfglen));
   { 'Eingabe zu lang (Dateinamen (.BFG))! Noch %s Zeichen verfÅgbar.' }
        end;
      if too_long then
      begin
        dbClose(d);
        exit;
      end
      else begin
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
    end;
    dbClose(d);
    if (a+p<entries) then
      if p<gl then inc(p)
      else inc(a);
  end;

  procedure MoveBox;
  var s : string[BoxNameLen];
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
  var s : string[BoxNameLen];
      i : integer;
  begin
    s:=boxlist[a+p];
    s:=mid(s,blankpos(s)+1);
    if ReadJN(getreps2(936,4,s),true) then begin { 'Serverbox "%s" lîschen' }
      if a+p<entries then begin  { a+p = Ziel }
        for i:=(a+p) to entries-1 do
          boxlist[i]:=boxlist[i+1];
        boxlist[i+1]:='';
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
  begin                        { Wenn nicht, auf passende Boxen prÅfen }
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do
    begin
      dbRead(d,'Netztyp',nt);
      if (nt=own_Nt) and (ustr(dbReadStr(d,'boxname')) <> own_Name) then
      begin                           { erste passende Box gefunden... }
        dbClose(d);
        goto start;        { ...dann Schleife verlassen und los geht's }
      end;
      dbNext(d);
    end;
    dbClose(d);                          { keine passende Box gefunden }
    rfehler(953); { 'Keine (weiteren) hinzuzufÅgenden Serverboxen vorhanden!' }
    exit;
  end;
  Start:
  width:=ival(getres2(936,1));
  buttons:=getres2(936,2);  { ' ^EinfÅgen , ^Verschieben , ^Lîschen ,  ^OK  ' }
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
        boxlist[entries]:=left(s1,p-1);
        s1:=trim(mid(s1,p+1));
      end;
    until p=0;
  gl:=screenlines-fnkeylines-12;
  bp:=1;
  if own_Name <> '' then
  begin
    selbox(width+2,gl+4,getres2(920,92),x,y,false);
    attrtxt(col.colsel2rahmen);                 { 'ZusÑtzliche Server' }
  end
  else begin
    selbox(width+2,gl+4,getres2(1024,3)+' #'+strs(cr.y)+' '+
           getres2(1024,5),x,y,true);    { '/Netcall/Spezial #%s bei:' }
    attrtxt(col.colselrahmen);
  end;
  mwrt(x,y+gl+1,'√'+dup(width,'ƒ')+'¥');
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
      rfehler1(954,strs(maxbox)) { 'Maximal %s Serverbox-EintrÑge mîglich!' }
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
          s1:=s1+boxlist[ii]+' ';  { neuen Eintrag fÅr Eingabefeld erstellen }
        cr.s:=trim(s1);
        modi:=false;
      end;
    end;
    if nr=0 then cr.brk:=true;
  until ((nr=0) or ((nr=okb) and addServersTest(cr.s))) and
        (not modi or ReadJN(getres(1015),false)); { 'énderungen verwerfen' }
  maus_popinside;
  if own_name = '' then pophp;
  closebox;
  freeres;
end;


function addServersTest(var s:string):boolean;
var   p,nt,i,j,
      box_anz    : byte;
      boxlen,
      bfglen     : word;
      s1         : string;
      d          : DB;
      boxlist    : array[1..maxboxen] of string[BoxNameLen];
      dupelist   : array[1..maxboxen] of byte;       { Array fÅr Dupes }
const maxboxlen  : byte = 255;
      maxbfglen = 160;

  { Die hier mehrfach vorkommende PrÅfung "if own_Name <> '' then..."  }
  { dient zur Feststellung, ob wir in einem Box-Config-Dialog (z.B.    }
  { 'ZusÑtzliche Server' bei RFC/Client oder 'Pakete mitsenden' bei    }
  { Fido) sind oder aus 'EditNetcallDat' in xp10.pas kommen. In        }
  { letzterem Fall ziehen wir abweichende (= weniger restriktive)      }
  { Konsequenzen - z.B. lassen wir nach positiv beantworteter          }
  { RÅckfrage Dupes zu und Åbergehen ÅberflÅssige bzw. unzutreffende   }
  { Tests wie "eingetragene Box = editierte Box?".                     }

  { Die Variable 'showErrors' dient als Flag, ob die Einzel-Fehlermel- }
  { dungen angezeigt werden sollen. Ist 'showErrors' false (z.B. bei   }
  { einem Netcall, siehe 'ChkAddServers' in xp7.inc), werden a) keine  }
  { Fehler ausgegeben, und es wird b) die Funktion beim ersten Fehler  }
  { sofort verlassen.                                                  }

  { Hinweis: 'addServersTest' wird sowohl als 'normale' Funktion als   }
  { auch als Masken-Testfunktion mittels 'msetvfunc(addServersTest)'   }
  { (siehe '_EditPPP' in xp9.inc) aufgerufen. Aus diesem Grund kînnen  }
  { wir die oben angesprochenen Bedingungen nicht als Parameter        }
  { Åbergeben (geht bei 'msetvfunc' halt nicht) und fragen sie daher   }
  { Åber Variablen ab.                                                 }

begin
  addServersTest:=true;
  s1:=trim(s);
  if s1='' then exit;
  if own_Name = '' then maxboxlen:=249  { wegen mappsel-String-Addition  }
  else maxbox:=80;                      { (lfd. Nr.) in 'EditNetcallDat' }
  for i:=1 to maxbox do boxlist[i] := '';
  box_anz:=0; bfglen:=0; boxlen:=0;
  repeat
    inc(box_anz);
    p:=cpos(' ',s1);
    if p=0 then boxlist[box_anz]:=s1
    else begin
      boxlist[box_anz]:=left(s1,p-1);             { Boxen-Array fÅllen }
      s1:=trim(mid(s1,p+1));
    end;
  until p=0;
  { ------------------------------------------------------ }
  { Dupeschleife - fÅllt ein Array mit den Werten:         }
  {   0 = Box ist ein Dupe                                 }
  {   i = Anzahl gleicher EintrÑge (wird im ersten         }
  {       (Element, in dem die Box vorkommt, eingetragen)  }
  { In AbhÑngigkeit von diesen Werten in 'dupelist' werden }
  { die in 'boxlist' hinterlegten Boxen durch die Funktion }
  { gejagt oder Åbersprungen (wenn Wert=0). Grund: Wir     }
  { wollen fÅr jede mehrfach vorkommende Box nur einmal    }
  { die Fehlermeldung(en) ausgeben (und damit auch die     }
  { Performance erhîhen).                                  }
  { Diese Dupebehandlung gilt nur im Box-Config-Dialog     }
  { (weil in 'EditNetcallDat' Dupes zulÑssig sind).        }
  { ------------------------------------------------------ }
  for i:=1 to box_anz do dupelist[i] := 1;
  for i:=1 to box_anz do                           { Dupe-Array fÅllen }
  begin
    if dupelist[i]=0 then continue;
    for j:=i to box_anz do
    begin
      if (j=i) or (dupelist[j]=0) then continue;
      if ustr(boxlist[j]) = ustr(boxlist[i]) then
      begin
        inc(dupelist[i]);                { Anzahl der EintrÑge erhîhen }
        dupelist[j]:=0;                  { 0 = Dupe                    }
      end;
    end;
  end;
  { ----------------- Ende Dupeschleife ------------------ }
  dbOpen(d,BoxenFile,1);
  for i:=1 to box_anz do
  begin
    if own_Name <> '' then
    begin
      if dupelist[i]=0 then continue;
      if dupelist[i] > 1                          { Box-Config-Dialog? }
      then begin
        addServersTest:=false;
        if showErrors then
          fehler(getreps2(10900,60,boxlist[i]) + ' ' +
                 getreps2(10900,61,strs(dupelist[i])))
                              { 'Serverbox "%s" ist %s mal vorhanden!' }
        else begin
          dbClose(d);
          exit;
        end;
      end;
    end;
    dbSeek(d,boiName,ustr(boxlist[i]));
    if not dbFound then
    begin
      addServersTest:=false;
      if showErrors then
        rfehler1(962,boxlist[i])   { 'Serverbox "%s" existiert nicht!' }
      else begin
        dbClose(d);
        exit;
      end;
    end
    else if own_Name <> '' then                   { Box-Config-Dialog? }
    begin
      if ustr(boxlist[i]) = own_Name then
      begin
        addServersTest:=false;
        if showErrors then
          rfehler1(963,boxlist[i])
              { Serverbox "%s" ist identisch mit editierter Serverbox!'}
        else begin
          dbClose(d);
          exit;
        end;
      end
      else begin
        dbRead(d,'Netztyp',nt);
        if nt <> own_Nt then
        begin
          addServersTest:=false;
          if showErrors then
            fehler(getreps2(10900,60,boxlist[i])+' '+
                   getreps2(10900,64,Netz_Typ(own_Nt)))
                   { 'Serverbox "%s" ist nicht vom Netztyp %s!' }
          else begin
            dbClose(d);
            exit;
          end;
        end;
      end;
      if own_Name <> '' then
        bfglen:=bfglen+length(dbReadStr(d,'dateiname'))+1;
                                               { GesamtlÑnge BFG-Namen }
(*    hinweis('Anzahl = '+strs(box_anz)+', BFG-LÑnge = '+strs(bfglen)); *)
    end;
    boxlen:=boxlen+length(boxlist[i])+1;        { GesamtlÑnge Boxnamen }
  end;
  if own_Name <> '' then                          { Box-Config-Dialog? }
  begin
    if bfglen > 0 then dec(bfglen);  { letztes Leerzeichen eliminieren }
    if bfglen > maxbfglen then
    begin
      addServersTest:=false;
      if showErrors then
        rfehler1(965,strs(maxbfglen))
    { 'Maximale GesamtlÑnge (%s) der Dateinamen (.BFG) Åberschritten!' }
      else begin
        dbClose(d);
        exit;
      end;
    end;
  end;
  dec(boxlen);                       { letztes Leerzeichen eliminieren }
  if boxlen > maxboxlen then
  begin
    addServersTest:=false;
    if showErrors then
      rfehler1(966,strs(maxboxlen))
      { 'Maximale GesamtlÑnge (%s) der Serverbox-Namen Åberschritten!' }
    else begin
      dbClose(d);
      exit;
    end;
  end;
  if box_anz >= maxbox then
  begin
    addServersTest:=false;
    if showErrors then
      rfehler1(954,strs(maxbox)) { 'Maximal %s Serverbox-EintrÑge mîglich!' }
    else begin
      dbClose(d);
      exit;
    end;
  end;
  dbClose(d);
end;


procedure ConvertAddServersFehler(const s:string);
var x,y : byte;
begin
  msgbox(length(s)+6,6,_fehler_,x,y);
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(920,92)+':');  { '"ZusÑtzliche Server:"' }
  attrtxt(col.colmbox);
  mwrt(x+3,y+3,s);
  errsound;
  wait(curoff);
  closebox;
  freeres;
end;


function BfgToBox(var s:string):string;
var   d      : DB;
      i,p    : byte;
      s1     : string;              { BFG-Datei }
      s2     : string[BoxNameLen];  { Boxname   }
      s3     : string;              { Gesamtstring aller Boxnamen }
      fehler : string;
const maxboxlen = 255;

  function isValidBfgName(const s1:string):boolean;
  var   i  : byte;
        vb : boolean;
  const ValidBfgCh : set of char=['A'..'Z','0'..'9','_','^','$','~','!',
                          '#','%','&','-','{','}','(',')','@','''','`'];
  begin
    if (length(s1) > 8) or (IsDevice(s1)) then
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
  BfgToBoxOk:=true;  { Flag fÅr 'ChkAddServers' in xp7.inc }
  if s = '' then
  begin
    BfgToBox:='';
    exit;
  end;
  s1:=''; s2:=''; s3:='';
  dbOpen(d,BoxenFile,1);
  repeat
    p:=cpos(' ',s);
    if p=0 then s1:=s
    else begin
      s1:=left(s,p-1);
      s:=trim(mid(s,p+1));
    end;
    if not isValidBfgName(ustr(s1)) then
    begin
      BfgToBoxOk:=false;
      if showErrors then
      begin
        fehler:=getreps2(10900,67,ustr(s1));
       { 'UngÅltiger Name fÅr Serverbox-Konfigurationsdatei: "%s.BFG"' }
        ConvertAddServersFehler(fehler);
      end
      else exit;
    end
    else begin
      dbSeek(d,boidatei,ustr(s1));
      if dbFound then
      begin
        s2:=dbReadStr(d,'boxname');
        if length(s3)+length(s2) > maxboxlen then
        begin
          BfgToBoxOk:=false;
          if showErrors then
          begin
            fehler:=getreps2(10900,66,strs(maxboxlen));
      { 'Maximale GesamtlÑnge (%s) der Serverbox-Namen Åberschritten!' }
            ConvertAddServersFehler(fehler);
          end
          else exit;
        end
        else
          s3:=s3+s2+' ';
      end
      else begin
        BfgToBoxOk:=false;
        if showErrors then
        begin
          fehler:=getreps2(10900,68,ustr(s1));
                   { 'Serverbox zu Dateiname "%s.BFG" nicht gefunden!' }
          ConvertAddServersFehler(fehler);
        end
        else exit;
      end;
    end;
  until p=0;
  dbClose(d);
  BfgToBox:=trim(s3);
end;


function BoxToBfg(var s:string):string;
var   d      : DB;
      i,p    : byte;
      s1     : string;              { Boxname   }
      s2     : string[8];           { BFG-Datei }
      s3     : string;              { Gesamtstring aller BFG-Dateinamen }
      fehler : string;
const maxbfglen = 160;
begin
  if s = '' then
  begin
    BoxToBfg:='';
    exit;
  end;
  s1:=''; s2:=''; s3:='';
  dbOpen(d,BoxenFile,1);
  repeat
    p:=cpos(' ',s);
    if p=0 then s1:=s
    else begin
      s1:=left(s,p-1);
      s:=trim(mid(s,p+1));
    end;
    if length(s1) > BoxNameLen then
    begin
      fehler:=getreps2(10900,69,s1); { 'UngÅltiger Serverbox-Name: %s' }
      ConvertAddServersFehler(fehler);
    end
    else begin
      dbSeek(d,boiname,ustr(s1));
      if dbFound then
      begin
        s2:=dbReadStr(d,'dateiname');
        if length(s3)+length(s2) > maxbfglen then
        begin
          fehler:=getreps2(10900,65,strs(maxbfglen));
    { 'Maximale GesamtlÑnge (%s) der Dateinamen (.BFG) Åberschritten!' }
          ConvertAddServersFehler(fehler);
        end else
          s3:=s3+s2+' ';
      end else
      begin
        fehler:=getreps2(10900,62,s1);
        ConvertAddServersFehler(fehler); { 'Serverbox "%s" existiert nicht!' }
      end;
    end;
  until p=0;
  dbClose(d);
  BoxToBfg:=ustr(trim(s3));
end;


procedure SingleServerSel(var cr:customrec); { einzelne Serverbox (nur vom }
var i     : byte;                            { eigenen Netztyp) auswÑhlen  }
    s1    : string[BoxNameLen];
    dummy : box_array; { wir brauchen keine Serverboxen-Liste zu Åbergeben }
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


function  ReadExtCfgFilename(const txt:atext; var s1:string; var cdir:PathStr; subs:boolean):boolean;
var   x,y,n   : byte;
      brk     : boolean;
      fn      : string[20];
      cconfig : Searchrec;
      seldir  : dirstr;
      s2      : string;
      dir     : dirstr;
      name    : namestr;
      ext     : extstr;
const cfgext  : array [1..7] of string[5] = ('*.CFG','*.BFG','*.BFE','*.$CF',
                                             '*.EXE','*.COM','*.BAT');
label restart;
begin
restart:
  set_ExtCfg_Allowances;   { Lîschen mit <Ctrl-Del> erlauben }
  s2 := '';
  if (cpos(':',s1) = 2) or (cpos(DirSepa, s1) = 1) then
  begin
    fsplit(Fexpand(s1),dir,name,ext);
    seldir := dir;
  end
  else seldir := cdir;
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  maddstring(3,2,fn,s1,37,MaxLenPathname,'');   { Dateiname: }
  for n := 1 to 7 do
  begin
    findfirst(seldir+cfgext[n],ffAnyfile,cconfig);
    while Doserror = 0 do
    begin
      if seldir = cdir then mappsel(false,cconfig.name)
      else mappsel(false,seldir+cconfig.name);
      findnext(cconfig);
    end;
    FindClose(cconfig);
  end;
  readmask(brk);
  enddialog;
  if (cDel_pressed) then          { <Ctrl-Del> gedrÅckt => Dateiname lîschen }
  begin
    reset_Allowances(s1);  { s1 = Dummy }
    if boxpar^.pppExternCfg <> '' then
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
      s2 := FExpand(s2)
    else s2 := FExpand(cdir + s2);
    if ((length(s2)=2) and (s2[2]=':'))
      or (Lastchar(s2)=DirSepa) then
      s2 := FExpand(s2 + WildCard)
    else
    if IsPath(s2) then
      s2 := FExpand(s2 + DirSepa + WildCard);
    fsplit(s2,dir,name,ext);
    if not IsPath(dir) then
    begin
      rfehler1(952,dir);  { 'Verzeichnis "%s" ist nicht vorhanden!' }
      goto restart;
    end;
    if multipos('*?',s2) then
    begin
      selcol;
      pushhp(89);
      s2:=fsbox(actscreenlines div 2 - 5,s2,'','',subs,false,false);
      pophp;
      if s2 <> '' then   { <Esc> gedrÅckt? }
      begin
        fsplit(s2,dir,name,ext);
        if dir=cdir then s1:=name+ext else s1:=s2;
      end;
      goto restart;
    end;
    if (s2<>'') and (IsDevice(s2) or not ValidFilename(s2)) then
    begin
      rfehler(3);   { 'UngÅltiger Pfad- oder Dateiname!' }
      goto restart;
    end;
    s1 := s2;
    ReadExtCfgFilename := (s1<>'');
  end else
    ReadExtCfgFilename := false;
  reset_Allowances(s1);
end;

function is_mailaddress(const s:string):boolean;
var b: byte;
begin
  is_mailaddress:=true;
  b:=cpos('@',s);
  if (b<=1) or (cpos('@',mid(s,b+1))<>0)
    or (cpos('.',mid(s,b+1))=0) or (cpos(' ',s)<>0)
    or (s<>mailstring(s,false))
  then is_mailaddress:=false;
end;


function multi_Mailstring(var s:string):boolean;
var n     : byte;
    s1,s2 : string[160];
begin
  multi_Mailstring:=true;
  s1:=trim(s);
  if s1='' then exit;
  repeat
    n:=cpos(' ',s1);
    if n=0 then s2:=s1
    else begin
      s2:=left(s1,n-1);
      s1:=trim(mid(s1,n+1));
      end;
    if not is_mailaddress(s2) then
    begin
      multi_mailstring:=false;
      fehler(Getres2(10900,8)+': ' +s2); { 'UngÅltige Adresse: 's2 }
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
    rfehler(970);        { 'Envelope-Adresse mu· angegeben werden' }
    exit;
  end;
  check_envelope:=true;
end;


procedure SelSchab(var cr:CustomRec);
var ps  : pathstr;
    dir : dirstr;
    name: namestr;
    ext : extstr;
begin
  selcol;
  ps:=fsbox(screenlines div 2 - 5,'*.xps','',cr.s+'.xps',false,false,false);
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
    rfehler(904);    { 'ungÅltiger Dateiname' }
    validfile:=false
    end
  else
    validfile:=true;
end;

function testfidodir(var s:string):boolean;   { Fido Sysop-Mode }
var res : integer;
begin
  if s='' then
    testfidodir:=true
  else begin
    testfidodir:=false;
    if right(s,1)<>DirSepa then s:=s+DirSepa;
    s:=FExpand(s);
    if s=OwnPath then
      rfehler(905)    { 'Verzeichnis darf nicht gleich dem XP-Verzeichnis sein' }
    else
      if IsPath(s) then
        testfidodir:=true
      else
        if ReadJN(getres(900),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
        begin
          mklongdir(s,res);
          if res<0 then
            rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
          else
            testfidodir:=true;
          end;
    end;
end;

function testqwkinfiles(var s:string):boolean;
var
    qd  : pathstr;
begin
  testqwkinfiles:=false;
  if s<>'' then begin
    qd:=GetFileDir(s);
    testqwkinfiles:=testfidodir(qd);
    s:=qd+getFileName(s);
    end;
end;

procedure set_uparcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if UpArcNr<1 then exit;
  ls:=lstr(s);
  ext:='*';
  if (left(ls,5)='pkarc') or (left(ls,5)='pkpak') then ext:='arc'
  else if left(ls,3)='lha' then ext:='lzh'
  else if left(ls,5)='pkzip' then ext:='zip'
  else if left(ls,3)='arj' then ext:='arj'
  else if (left(ls,4)='copy') and (getfield(UpArcNr)<>'txt') then ext:='';
  if ext<>'*' then setfield(UpArcNr,ext);
end;

procedure set_downarcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if DownArcNr<1 then exit;
  ls:=lstr(s);
  ext:='*';
  if (left(ls,6)='pkxarc') or (left(ls,7)='pkunpak') then ext:='arc'
  else if left(ls,3)='lha' then ext:='lzh'
  else if left(ls,7)='pkunzip' then ext:='zip'
  else if left(ls,3)='arj' then ext:='arj'
  else if (left(ls,4)='copy') and (getfield(DownArcNr)<>'txt') then ext:='';
  if ext<>'*' then setfield(DownArcNr,ext);
end;

function progtest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    path : string[127];
begin
  progtest:=true;                               { Warum immer TRUE? (hd/22.5.2000) }
  path:=getenv('PATH');
  if ustr(left(s+' ',7))='ZMODEM ' then
    fn:='ZM.EXE'
  else
    fn:=trim(s);
  if cpos(' ',fn)>0 then fn:=left(fn,cpos(' ',fn)-1);
  if (fn<>'') and (pos('*'+ustr(fn)+'*','*COPY*DIR*PATH*')=0) then begin
    fsplit(fn,dir,name,ext);
    if ext<>'' then
      ok:=fsearch(fn,path)<>''
    else
      ok:=(fsearch(fn+'.exe',path)<>'') or
          (fsearch(fn+'.com',path)<>'') or
          (fsearch(fn+'.bat',path)<>'');
    if not ok then rfehler1(907,ustr(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
  end;
end;

function PPPClientPathTest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    res  : Integer;
    x,y  : byte;
begin
  PPPClientPathTest:=true;
  fn:=trim(s);
  if (fn<>'') then
  begin
    if right(s,1)<>DirSepa then s:=s+DirSepa;
    if Copy(fn, 1, 2) = '.\' then fn := Copy(fn, 3, Length(fn));
    if fn[length(fn)] = '\' then fn := Copy(fn, 1, length(fn)-1);
    ok := (cPos(':', fn) = 0) and (cPos('\', fn) = 0) and (cPos('.', fn) < 2)
      and (Length(fn) > 0) and (fn[length(fn)] <> '.');
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
        mklongdir(s,res);
        if res<0 then
        begin
          PPPClientPathTest:=false;
          rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
        end;
      end else
        PPPClientPathTest:=false;
  end else
  begin
    PPPClientPathTest:=false;
    rfehler(939)                 { 'Pfad darf nicht leer sein!' }
  end;
end;

function PPPClientTest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    s1   : String;
begin
  PPPClientTest:=true;
  fn:=trim(s);
  if Pos('start /wait ', lstr(fn)) = 1 then fn := Copy(fn, 13, MaxInt);
  if Pos('start /wai ', lstr(fn)) = 1 then fn := Copy(fn, 12, MaxInt);
  if Pos('start /wa ', lstr(fn)) = 1 then fn := Copy(fn, 11, MaxInt);
  if Pos('start /w ', lstr(fn)) = 1 then fn := Copy(fn, 10, MaxInt);
  if cpos(' ',fn)>0 then fn:=left(fn,cpos(' ',fn)-1);
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
      rfehler1(936, UStr(fn)); { 'Eintrag darf entweder keine oder nur "$CLPATH+" als Pfadangabe enthalten!' }
      PPPClientTest:=false;
    end else
    begin
      exchange(fn, '$CLPATH+', s1);
      if ext<>'' then
        ok:=fsearch(fn,ownpath)<>''
      else
        ok:=(fsearch(fn+'.exe',ownpath)<>'') or
          (fsearch(fn+'.com',ownpath)<>'') or
          (fsearch(fn+'.bat',ownpath)<>'');
      if not ok then rfehler1(907,ustr(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
    end;
  end else
    begin
    PPPClientTest:=false;
    errsound;
  end;
end;

function testmbretter(var s:string):boolean;
begin
  if pp_da and (ustr(s)<>ustr(BoxPar^.MagicBrett)) then begin
    s:=BoxPar^.MagicBrett;
    rfehler(927);
    testmbretter:=false;
    end
  else begin
    if right(s,1)<>'/' then s:=s+'/';
    if left(s,1)<>'/' then s:='/'+s;
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
    s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
end;

function testreplyto(var s:string):boolean;
var p : byte;
    d : DB;
begin
  if s='' then
    testreplyto:=true
  else begin                            { Wenn's keine gueltige Adresse ist...}
    p:=cpos('@',s);
    if (p=0) or (cpos('.',mid(s,p))=0) then
    begin
      dbOpen(d,PseudoFile,1);
      dbSeek(d,piKurzname,ustr(s));
      if dbFound then
      begin
        dbRead(d,'Langname',s);         { ist's ein Kurzname ? }
        dbclose(d);
        testreplyto:=true;
        if cpos(' ',s)<>0 then          { Langname jetzt gueltig ? }
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


function testuucp(var s:string):boolean;
var ok : boolean;
    i  : integer;
begin
  ok:=false;
  for i:=uup1 to uupl do
    if i=fieldpos then
      if s=_jn_[1] then ok:=true   { 'J' }
      else
    else
      if getfield(i)=_jn_[1] then ok:=true;
  testuucp:=ok;
  if not ok then
    rfehler(909);    { 'Mindestens ein Protokoll mu· eingeschaltet sein!' }
end;


procedure SetDomain(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (left(s,1)='.') or (left(s,2)='@') do
        delfirst(s)
    else begin
      if s[1]<>'.' then
         s:='.'+s;
      if (bDomainNt<>0) and (getfield(fieldpos+1)='') then
        setfield(fieldpos+1,s);
      end;
end;

procedure SetDomain2(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (left(s,1)='.') or (left(s,2)='@') do
        delfirst(s)
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
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
begin
  if trim(s)='' then
    testscript:=true
  else begin
    fsplit(s,dir,name,ext);
    if ext='' then s:=dir+name+'.scr';
    if exist(s) then
      testscript:=true
    else begin
      rfehler(22);     { 'Datei ist nicht vorhanden!' }
      testscript:=false;
      end;
    end;
end;

procedure scripterrors(var s:string);
begin
  if (s<>'') and exist(s) and (RunScript(true,s,false,false,nil)<>0) then begin
    rfehler(925);    { 'Syntaxfehler in Script' }
    if listfile(LogPath+ScErrlog,scerrlog,true,false,0)=0 then;
    end;
end;

{ Fileserver: Feldbezeichnung Ñndern }

procedure setpasswdfield(var s:string);
begin
  setfieldtext(4,getres2(903,iif(ustr(s)=ustr(uuserver),7,6)));
end;

{ Fido: YooHoo-PW auf 8 Zeichen begrenzen }

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure fidotestpasslen(var s:string);
begin
  if (getfield(EMSIfield)='N') and (length(getfield(4))>8) then begin
    rfehler(926);
    setfield(4,left(getfield(4),8));
    end;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

function testvertreterbox(var s:string):boolean;
var d  : DB;
    nt : byte;
    ok : boolean;
begin
  if s='' then testvertreterbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then
    begin
      dbRead(d,'boxname',s);
      dbRead(d,'netztyp',nt);
      if fieldpos=amvfield then    { AM-Vertreterbox }
        if (DomainNt=nt_Client) or (DomainNt=nt_UUCP) then
          ok:=(nt=nt_Client) or (nt=nt_UUCP)
        else ok:=(DomainNt=nt)
      else                         { PM-Vertreterbox }
        ok:=ntAdrCompatible(DomainNt,nt);
      if not ok then rfehler(2713);
      testvertreterbox:=ok;
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
var fn : pathstr;
begin
  if s='' then
    testlogfile:=true
  else begin
    if lstr(s)='logfile' then           { Diese Pruefung ist nun wirklich der Hit (hd) }
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
      if ntBoxNetztyp(left(s2,p-1))<>nt_Fido then begin
        rfehler1(929,left(s2,p-1));  { '%s ist keine eingetragene Fido-Serverbox!' }
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
var x,y   : byte;
    anz,i : integer;
    t     : taste;
begin
  JanusSwitch:=true;
  if lstr(getfield(downprotnr))='zmodem' then exit;
  anz:=res2anz(932);
  msgbox(63,anz+5,_hinweis_,x,y);
  for i:=1 to anz do
    wrt(x+3,y+1+i,getres2(932,i));
  wrt(x+3,y+3+anz,getres(12));    { 'Taste drÅcken ...' }
  errsound;
  get(t,curoff);
  closebox;
end;


procedure gf_getntyp(var s:string);
var uucp,client : boolean;
begin
  setfieldtext(fieldpos+1,getres2(912,iif(lstr(s)=lstr(ntName(nt_Client)),13,2)));
  gf_fido:=(lstr(s)=lstr(ntName(nt_Fido)));
  uucp:=(lstr(s)=lstr(ntName(nt_UUCP)));
  client:=(lstr(s)=lstr(ntName(nt_Client)));
  if (lstr(s)=lstr(ntName(nt_Maus))) or gf_fido or uucp or client then
    set_chml(userfield,'')
  else
    set_chml(userfield,'>');
  if uucp or client then
    set_chml(fieldpos+1,'')
  else
    set_chml(fieldpos+1,'>');
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
      if DomainNt<0 then nt:=lstr(getfield(1))   { Netztyp als String }
      else nt:=lstr(ntName(DomainNt));
      if nt=lstr(ntName(nt_Maus)) then begin
        if (length(s)>4) and (ustr(left(s,4))='MAUS') then
          s:=mid(s,5);
        if cpos('.',s)>0 then s:=left(s,cpos('.',s)-1);
        s:=left(s,6);
        end
      else if nt=lstr(ntName(nt_Netcall)) then         { Domain abschneiden }
        if right(s,4)='.ZER' then s:=left(s,length(s)-4)
        else
      else if (nt=lstr(ntName(nt_ZCONNECT))) or (nt=lstr(ntName(nt_UUCP)))
           or (nt=lstr(ntName(nt_Client))) then
        if cpos('.',s)>0 then truncstr(s,cpos('.',s)-1);
      xp9_testbox:=true;
      end;
end;

function xp9_setclientFQDN(var s:string):boolean;
var s1:string;
     b:byte;
     u:byte;
begin
  xp9_setclientFQDN:=false;
  mclearsel(6);                    { FQDN = Feld 6 !!! }
  if s='' then begin
    errsound;
    exit;
    end;
  b:=cpos('@',s);
  if not is_mailaddress(s)
  then begin
    rfehler(908);
    exit;
    end;
  xp9_setclientFQDN:=true;
  s1:=s; s1[b]:='.';
  for u:=cposx('_',s1) to length(s1) do
    if s1[u]='_' then s1[u]:='-';
  if lstr(mid(s1,b))='.t-online.de'
    then insert('.dialin',s1,b);
  mappendsel(6,false,s1);          { FQDN = Feld 6 !!! }
end;

function xp9_FQDNTest(var s:string):boolean;
var
  s1 : string;
  b  : byte;
begin
   XP9_FQDNTest:=true;
   s1:=mailstring(s,false);
   for b:=1 to length(s1) do
     case s1[b] of
       '@'  :  s1[b]:='.';
       '_'  :  s1[b]:='-';
       end;
   while (s1[1]='.') and (s1[0]<>#0) do
     delete(s1,1,1);
   if s1<>s then begin
     errsound;
     xp9_FQDNTest:=false;
     end;
   s:=s1;
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

{ s = '<BOX> <USERNAME> [/ Realname]'}

procedure SetUsername(s:string);
var x,y  : byte;
    brk  : boolean;
    user : string[50];
    real : string[40];
    p    : byte;
    d    : DB;
    box  : string[BoxNameLen];
    gross   : boolean;
    hasreal : boolean;
begin
  s:=trim(s);
  if s='' then
    rfehler(916)      { 'SETUSER - Parameter fehlen' }
  else begin
    p:=cpos(' ',s);
    if p=0 then begin
      box:=UStr(s); user:=''; real:='';
      end
    else begin
      box:=ustr(left(s,p-1));
      user:=trim(mid(s,p+1));
      p:=pos(' (',user);
      if p=0 then real:=''
      else begin
        real:=copy(user,p+2,length(user)-p-2);
        user:=trim(left(user,p-1));
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
        dbWrite(d,'username',user);
        if hasreal { and (real<>'') 29.07.96 } then dbWrite(d,'realname',real);
        if box=DefFidoBox then begin
          HighlightName:=ustr(user);
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


end.

{
  $Log$
  Revision 1.1.2.25  2001/12/20 15:09:12  my
  MY+MK:- Umstellung "RFC/Client" auf neue Netztypnummer 41 und in der
          Folge umfangreiche Code-Anpassungen. Alte RFC/Client-Boxen
          mÅssen einmal manuell von RFC/UUCP wieder auf RFC/Client
          umgeschaltet werden.

  MY:- Sysop-Mode wird jetzt Åber einen Schalter aktiviert/deaktiviert.

  MK:- Einige Displayroutinen beschleunigt ('FWrt()' statt 'Wrt()').

  Revision 1.1.2.24  2001/12/11 17:51:37  my
  MY:- RFC/Client: Client- und Server-Konfiguration erheblich umgestaltet
       und erweitert. Neue Einstellungen:
       - D/B/E/C/Verbindung: RÅckfrage vor Anwahl
                             RÅckfrage vor Auflegen
                             Verbindungsstatus halten
       - D/B/E/N/Mail (In) : Protokoll
                             Envelope-To auswerten
                             Mail auf Server belassen
                             APOP-Authentifizierung
       - D/B/E/N/Mail (Out): SMTP after POP
                             SMTP-Login nach RFC 2554
       - D/B/E/N/News      : Newsgroup-Liste pflegen
                             Max. Artikelgrî·e (KB)
                             Max. Artikel je Gruppe

  MY:- Envelope-Adresse (Mail-in) ist jetzt ein Pflichtfeld (falls ein
       POP3/SMTP/IMAP-Server eingetragen ist).

  MY:- Bei den Select-Routinen fÅr "ZusÑtzliche Server" und "Fallback-
       Server" wird bei <Esc> nicht mehr zum nÑchsten Feld gesprungen.

  MY:- PrÅfung auf gÅltigen .BFG-Dateinamen vereinfacht ('IsDevice').

  MY:- Das gegen Eingaben gesperrte Feld "ZusÑtzliche Server" kann jetzt
       mit <Ctrl-Del> gelîscht werden und ist gegen das EinfÅgen des
       Clipboard-Inhalts mit <Ctrl-C> geschÅtzt.

  MY:- Bei D/B/E/X (Externe Einstellungen) kann jetzt auch ein Programm-
       name (EXE, COM, BAT) eingetragen und mit <Enter> gestartet werden.
       Der Boxname (ohne Dateierweiterung .BFG) kann mit $CONFIG als
       Parameter Åbergeben werden. Mit <Ctrl-Del> kann der in <Box>.BFG
       gespeicherte Dateiname entfernt werden.

  MY:- Anpassungen an neue/geÑnderte Ressourcen-Nummern.

  MY:- Typos im CVS-Log bereinigt.

  Revision 1.1.2.23  2001/11/21 02:59:35  my
  MY:- Unwichtige Code-Kosmetik

  Revision 1.1.2.22  2001/11/20 23:24:26  my
  MY:- Konfiguration Multiserverbetrieb (D/B/E/C/ZusÑtzliche_Server und
       D/B/E/N/Fallback) gemÑ· Vereinbarung mit XP2 implementiert, Details
       siehe MenÅs und Hilfe; umfangreiche Auswahl- und Testroutinen. In
       den Dialogen werden immer die Boxnamen angezeigt, in der .BFG der
       editierten Box jedoch die BFG-Namen der ausgewÑhlten Box(en)
       abgelegt.
  MY:- öberflÅssige Deklarationen entfernt
  MY:- Lizenz-Header aktualisiert

  Revision 1.1.2.21  2001/09/06 18:44:52  mk
  - optimized testvertreterbox

  Revision 1.1.2.20  2001/08/12 12:03:34  mk
  - optimized last checkin a bit more ;)

  Revision 1.1.2.19  2001/08/12 11:45:58  my
  - optimized code a bit, removed unnecessary resources, adjusted resource
    numbers
  - removed XPEasy resources

  Revision 1.1.2.18  2001/08/11 22:18:04  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.1.2.17  2001/08/11 20:16:30  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.1.2.16  2001/08/11 10:58:38  mk
  - debug switch on
  - moved some procedures and functions, because code size of unit

  Revision 1.1.2.15  2001/08/06 15:32:31  mk
  JG:- fix fuer Sonderbehandung UUCP_C und UUCP_U

  Revision 1.1.2.14  2001/08/05 11:45:36  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.1.2.13  2001/08/02 14:35:02  my
  JG:- ReadExtCfgFilename: optimized suboptimal (but working) code

  Revision 1.1.2.12  2001/07/31 17:54:05  mk
  - added missing FindClose

  Revision 1.1.2.11  2001/07/31 17:25:41  mk
  - is_mailadress hat einen const statt var-parameter

  Revision 1.1.2.10  2001/07/31 15:36:41  my
  MY+JG:- new function is_mailaddress, also implemented in all
          functions and procedures involved (multi_Mailstring and
          xp9_setclientFQDN in xp9sel.pas, NameRead in xp9.inc and
          get_first_box in xp9.pas)
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.1.2.9  2001/07/23 16:53:14  my
  JG+MY:- RFC/Client: implemented check for valid (multiple) eMail addresses
          under Edit/Servers/Edit/Mail/News_Servers/Envelope_address (In+Out)
  JG+MY:- RFC/Client: improved check for valid eMail address under
          Edit/Servers/Edit/Client/eMail_address

  Revision 1.1.2.8  2001/07/21 15:15:26  mk
  - removed some unused variables

  Revision 1.1.2.7  2001/06/30 01:01:46  my
  - just changed order of functions "PPPClientTest" and "PPPClientPathTest"

  Revision 1.1.2.6  2001/06/19 17:09:57  my
  - tried to add correct CVS $Id string, works hopefully

  Revision 1.1.2.5  2001/06/19 17:02:49  my
  - *really* added CVS log infos :-)

  Revision 1.1.2.4  2001/06/19 06:53:00  mk
  - added CVS Log Infos

  Revision 1.1.2.3  2001/06/18 23:55:58  my
  - RFC/Client: Edit/Client/Client_call is a mandatory field now
    (procedure ClientTest)

  Revision 1.1.2.2  2001/06/16 15:22:16  my
  - New field description "Servername" for first_box if RFC/Client

  Revision 1.1.2.1  2001/06/13 01:41:41  my
  JG:- new unit XP9SEL, unit XP9 exceeded 64K size

}

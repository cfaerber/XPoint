{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2001 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }


{ CrossPoint - Reply-To-All- (RTA) Routinen }

{$I XPDEFINE.INC }
{$O+,F+}

unit xp4RTA;

interface

uses xpglobal,crt,dos,typeform,fileio,inout,winxp,keys,video,maske,datadef,database,
  resource,xp0,lfn,xpnt,xp1,xp1input,xp2,xp3,xp4,xp4e,xp6,xp9,xp9sel,maus2,lister;

procedure askRTA (const XPStart :boolean);
procedure ReplyToAll (var brk, adresseAusgewaehlt :boolean; var empf, realname :string; var dispdat :DB);

implementation

{ 'true', wenn Adresse im Baum vorhanden; 'false', wenn nicht. }

function eigeneAdresse (baum :domainNodeP; adresse :AdrStr) :boolean;
var p :domainNodeP;
begin
  adresse := uStr (adresse);
  p := baum;
  while assigned (p) and (p^.domain^ <> adresse) do
    if adresse < p^.domain^ then p := p^.left
      else p := p^.right;
  eigeneAdresse := assigned (p);
end;

{ Eine Adresse mit allen Parametern (RTAEmpfaenger, Vertreter, Typ) vorne (!)
  an eine RTA-EmpfÑngerliste anfÅgen }

procedure addToRTAList (var list :RTAEmpfaengerP; const empf :AdrStr; const RTAEmpf, vertreter, userUnbekannt :boolean;
                        const typ :byte);
var neu :RTAEmpfaengerP;
begin
  if not assigned (list) then        { Wenn RTA-EmpfÑngerliste leer... }
  begin
    new (list);
    list^.empf := empf;
    list^.RTAEmpf := RTAEmpf;
    list^.vertreter := vertreter;
    list^.userUnbekannt := userUnbekannt;
    list^.typ := typ;
    list^.next := nil;
  end else
  begin
    new (neu);
    neu^.empf := empf;
    neu^.RTAEmpf := RTAEmpf;
    neu^.vertreter := vertreter;
    neu^.userUnbekannt := userUnbekannt;
    neu^.typ := typ;
    neu^.next := list;               { vorne anfÅgen }
    list := neu;
  end;
end;

{ Ganze EmpfÑngerlisten an eine RTA-EmpfÑngerliste anfÅgen }

procedure addList (var orginalList :RTAEmpfaengerP; var newList :empfNodeP; const typ :byte);
var lauf, neu :RTAEmpfaengerP;
    loesch :empfNodeP;
begin
  if not assigned (newList) then exit;

  if assigned (orginalList) then   { An bestehende Liste anfÅgen }
  begin
    neu := orginalList;
    while assigned (neu^.next) do
      neu := neu^.next;
  end else
    neu := nil;

  if not assigned (neu) then       { bestehende Liste ist leer }
  begin
    new (neu);
    neu^.next := nil;
    neu^.empf := newList^.empf;
    neu^.typ := typ;
    neu^.RTAEmpf := false;
    neu^.vertreter := false;
    neu^.userUnbekannt := false;
    loesch := newlist;
    newList := newList^.next;
    dispose (loesch);
    orginalList := neu;
  end;

  while assigned (newList) do      { Einzelne Elemente in die neue Liste kopieren }
  begin
    if newList^.empf <> '' then    { Leerstrings sind keine Adressen! }
    begin
      new (neu^.next);
      neu := neu^.next;
      neu^.empf := newList^.empf;
      neu^.typ := typ;
      neu^.RTAEmpf := false;
      neu^.vertreter := false;
      neu^.userUnbekannt := false;
      neu^.next := nil;
    end;
    loesch := newList;
    newList := newList^.next;
    dispose (loesch);
  end;

end;

{ RTA-EmpfÑngerlisten freigeben }

procedure disposeRTAEmpfList (var list :RTAEmpfaengerP);
var lauf :RTAEmpfaengerP;
begin
  while assigned (list) do
  begin
    lauf := list^.next;
    dispose (list);
    list := lauf;
  end;
  list := nil;
end;

{ Baumstuktur freigeben }

procedure freeEigeneAdressenBaum (var baum :domainNodeP);
var lauf :domainNodeP;
begin
  if Assigned (baum) then
  begin
    freeEigeneAdressenBaum (baum^.left);
    lauf := baum^.right;
    freemem (baum^.domain, length (baum^.domain^) + 1);
    dispose (baum);
    freeEigeneAdressenBaum (lauf);
  end;
end;

{ Bei dem "EmpfÑnger auswÑhlen"-Dialog aus XP4.ReplyToAll werden eigene
  Adressen ausgenommen. 'getEigeneAdressen' liest die Adressen aus
  den Boxenkonfigurationen und der XPOINT.CFG aus.
  Als Datenstruktur wird ein "Baum" gewÑhlt }

procedure getEigeneAdressen (var eigeneAdressenBaum :domainNodeP);
var d         :DB;
    adresse   :string[90];
    flags     :byte;
    username  :string[30];
    pointname :string[25];
    domain    :string[60];
    box       :string[BoxNameLen];
    email     :string[80];
    aliaspt   :boolean;
    s         :string;
    notEigeneAdressenbaum :domainNodeP;

  procedure insertNode (var node :domainNodeP; const adresse :string);
  begin
    if not assigned (node) then
    begin
      new (node);
      node^.left := nil;
      node^.right := nil;
      getmem (node^.domain, length (adresse) + 1);
      node^.domain^ := adresse;
    end else
      if node^.domain^ > adresse then
        insertNode (node^.left, adresse)
      else
        insertNode (node^.right, adresse)
  end;

begin
  eigeneAdressenBaum := nil;
  notEigeneAdressenbaum := nil;

  if assigned (RTANoOwnAddresses) then          { Adressen aus dem Config-     }
  begin                                         { Setting RTANotEigeneAdressen }
    s := RTANoOwnAddresses^;                    { verwerten                    }
    repeat
      if pos (' ', s) <> 0 then
      begin
        adresse := trim (copy (s, 1, pos (' ', s)));
        delete (s, 1, pos (' ', s));
      end else
      begin
        adresse := s;
        s := '';
      end;
      if is_mailaddress (adresse) then
        insertNode (notEigeneAdressenBaum, uStr (adresse));
    until s = '';
  end;

  dbopen (d, BoxenFile, 0);     { eigene Adressen aus Boxenkonfigurationen auslesen }
  while not dbEof (d) do
  begin
    if ntReplyToAll (dbReadInt (d, 'netztyp')) then { nur ZConnect und RFC/UUCP }
    begin                                           { Boxen berÅcksichtigen     }
      dbRead (d, 'username', username);
      dbRead (d, 'pointname', pointname);
      dbRead (d, 'script', flags);
      aliaspt := (flags and 4 <> 0);
      dbRead (d, 'domain', domain);
      dbRead (d, 'boxname', box);
      dbRead (d, 'email', email);
      case ntDomainType (dbReadInt (d, 'netztyp')) of
        5: adresse := username + '@' + iifs (aliaspt, pointname, box) + domain;
        6: if email <> '' then adresse := email
           else adresse := username + '@' +
            iifs (aliaspt, box + ntServerDomain (box), pointname + domain);
        else adresse := '';
      end;
      if (adresse <> '') and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertnode (eigeneAdressenBaum, uStr (adresse));
      dbRead (d, 'replyto', adresse);
      if (adresse <> '') and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertnode (eigeneAdressenBaum, uStr (adresse));
    end;
    dbNext (d);
  end;
  dbClose (d);

  if assigned (RTAOwnAddresses) then    { Adressen aus dem Config-Setting }
  begin                                 { RTAEigeneAdressen verwerten     }
    s := RTAOwnAddresses^;
    repeat
      if pos (' ', s) <> 0 then
      begin
        adresse := trim (copy (s, 1, pos (' ', s)));
        delete (s, 1, pos (' ', s));
      end else
      begin
        adresse := s;
        s := '';
      end;
      if is_mailaddress (adresse) and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertNode (eigeneAdressenBaum, uStr (adresse));
    until s = '';
  end;

  freeEigeneAdressenBaum (notEigeneAdressenbaum);
end;

{ RTA-EmpfÑngerliste in eine EmpfÑngerliste umwandeln, die XP6.DoSend versteht }

procedure translateRTAEmpfList (var RTAEmpfList :RTAEmpfaengerP; var sendEmpfList :empfNodeP);
var neu :empfNodeP;
    loesch :RTAEmpfaengerP;
begin
  while assigned (RTAEmpfList) and not RTAEmpfList^.RTAEmpf do
  begin  { Nur als RTAEmpf markierte Adressen berÅcksichtigen }
    loesch := RTAEmpfList;
    RTAEmpfList := RTAEmpfList^.next;
    dispose (loesch);
  end;

  if assigned (RTAEmpfList) then
  begin
    neu := sendEmpfList;
    while assigned (neu) do neu := neu^.next;  { sendEmpfList nicht leer? Seltsam }

    if not assigned (neu) then                 { Erstes Element einfÅgen }
    begin
      new (neu);
      neu^.next := nil;
      neu^.empf := RTAEmpfList^.empf;
      loesch := RTAEmpfList;
      RTAEmpfList := RTAEmpfList^.next;
      dispose (loesch);
      sendEmpfList := neu;
    end;

    while assigned (RTAEmpfList) do { Alle weiteren EmpfÑnger Åbernehmen }
    begin
      if RTAEmpfList^.RTAEmpf then  { RTAEmpf beachten                   }
      begin
        new (neu^.next);
        neu := neu^.next;
        neu^.empf := RTAEmpfList^.empf;
        neu^.next := nil;
      end;
      loesch := RTAEmpfList;
      RTAEmpfList := RTAEmpfList^.next;
      dispose (loesch);
    end;
    RTAEmpfList := nil;
  end;
end;

procedure askRTA (const XPStart :boolean);
var x,y,i       :byte;
    msglines, p :byte;
    z           :taste;
    res         :boolean;
    s           :string;
begin
  if (ntUsed [nt_UUCP] + ntUsed [nt_ZConnect] > 0) and (RTAMode and 128 = 128) and
    (not XPFirstStart) then
  begin
    msglines := ival (getres2 (2750, 0));
    msgbox (64, msglines + 5, '', x, y);
    moff;
    for i := 1 to msglines do
    begin
      s:=getres2 (2750, i);
      gotoxy(x + 3, y + 1 + i);
      repeat
        p := cposx ('*', s);
        write (left (s, p-1));
        delete (s, 1, p);
        p := cposx ('*', s);
        attrtxt (col.colmboxhigh);
        write (left (s, p - 1));
        attrtxt (col.colmbox);
        delete (s, 1, p);
      until s = '';
    end;
    mon;
    res := (ReadButton (x + 44, y + msglines + 3, 2, '*' + getres2(2750, 20), 1, true, z) = 1);
    closebox;
    freeres;
    if res then
      RTAMode := iif (askReplyTo, 15, 13)
    else
      RTAMode := iif (askReplyTo, 3, 0);

    if XPStart then
      saveConfig
    else
      globalModified;
  end else
  if XPFirstStart then RTAMode := 13;
end;

procedure saveList (list :RTAEmpfaengerP; var sList :RTAEmpfaengerP);
var lauf :RTAEmpfaengerP;
begin
  sList := nil;
  lauf := nil;
  while assigned (list) do   { RTA-EmpfÑngerliste sichern }
  begin
    if not assigned (sList) then
    begin
      new (sList);
      sList^.empf := list^.empf;
      sList^.RTAEmpf := list^.RTAEmpf;
      sList^.vertreter := list^.vertreter;
      sList^.typ := list^.typ;
      sList^.next := nil;
      lauf := sList;
    end else
    begin
      new (lauf^.next);
      lauf := lauf^.next;
      lauf^.empf := list^.empf;
      lauf^.RTAEmpf := list^.RTAEmpf;
      lauf^.vertreter := list^.vertreter;
      lauf^.typ := list^.typ;
      lauf^.next := nil;
    end;
    list := list^.next;
  end;
end;

function userUnbekannt (const user :string) :boolean;
begin
  dbSeek (ubase, uiName, uStr (user));
  userUnbekannt := (not dbFound) and (user <> '');
end;

procedure vertausche (var s1, s2 :RTAEmpfaengerT);
var h :RTAEmpfaengerT;
begin
  h.empf := s1.empf;
  s1.empf := s2.empf;
  s2.empf := h.empf;

  h.typ := s1.typ;
  s1.typ := s2.typ;
  s2.typ := h.typ;

  h.RTAEmpf := s1.RTAEmpf;
  s1.RTAEmpf := s2.RTAEmpf;
  s2.RTAEmpf := h.RTAEmpf;

  h.vertreter := s1.vertreter;
  s1.vertreter := s2.vertreter;
  s2.vertreter := h.vertreter;

  h.userUnbekannt := s1.userUnbekannt;
  s1.userUnbekannt := s2.userUnbekannt;
  s2.userUnbekannt := h.userUnbekannt;
end;

procedure exchangeByte (var i, j :byte);
var h :byte;
begin
  h := i;
  i := j;
  j := h;
end;

procedure removeFromList (var list, vor, lauf :RTAEmpfaengerP);
begin
  if assigned (vor) then
  begin
    vor^.next := lauf^.next;
    dispose (lauf);
    lauf := vor^.next;
  end else
  begin
    list := lauf^.next;
    dispose (lauf);
    lauf := list;
  end;
end;

{ die Åbergebene Adresse wird durch die Vertreteradresse ersetzt,
  sofern vorhanden. Es wird 'true' zurÅckgeben, wenn Vertreter
  vorhanden. }

function getVertreter (var adr :adrStr) :boolean;
var size :word;
begin
  dbSeek (ubase, uiName, uStr (adr));
  if dbFound then
  begin
    size := 0;
    if dbXsize (ubase, 'adresse') <> 0 then
    begin
      dbReadX (ubase, 'adresse', size, adr);
      getVertreter := true;
    end else
      getVertreter := false;
  end else
    getVertreter := false;
end;


procedure ReplyToAll (var brk, adresseAusgewaehlt :boolean; var empf, realname :string; var dispdat :DB);
type str90 = string[90];
var RTAEmpfList :RTAEmpfaengerP;
    eigeneAdressenBaum :domainNodeP;
    auswahlMarkierte :boolean;
    RTA :boolean;
    pmReplyTohasVertreter,
    absenderHasVertreter,
    wabHasVertreter,
    oabHasVertreter,
    pmReplyToIsUnknown,
    absenderIsUnknown,
    wabIsUnknown,
    oabIsUnknown :boolean;
    hdp     : headerp;
    hds     : longint;

    { Diese Prozedur ÅberprÅft die Åbergebene Liste auf Dupes,
      ungÅltige Adressen und andere SpezialfÑlle. Nebenbei wird
      sie auch noch alphabetisch sortiert }

  procedure checklist (var list :RTAEmpfaengerP);
  var lauf, vor :RTAEmpfaengerP;
      anzahl, i :word;
      uEmpf :string[90];
  begin
    anzahl := 0;
    lauf := list;
    vor := nil;
    while assigned (lauf) do
    begin
      if pos (' ', lauf^.empf) <> 0 then delete (lauf^.empf, pos (' ', lauf^.empf), 255);
      { ^^ Realname entfernen }
      uEmpf := uStr (lauf^.empf);
      if (uEmpf = uStr (hdp^.absender)) or (cpos ('@', lauf^.empf) = 0)
        or (uEmpf = uStr (hdp^.pmReplyTo))
        or (uEmpf = uStr (hdp^.wab))
        or (uEmpf = uStr (hdp^.oab))
        or (not is_mailaddress (lauf^.empf))
        {or (eigeneAdresse (lauf^.empf) and (lauf^.typ <> 9))} then
        removeFromList (list, vor, lauf)
      else begin
        if eigeneAdresse (eigeneAdressenbaum, lauf^.empf) then lauf^.RTAEmpf := false
        else lauf^.RTAEmpf := true;
        lauf^.userUnbekannt := userUnbekannt (lauf^.empf);
        inc (anzahl);
        vor := lauf;
        lauf := lauf^.next;
      end;
    end;

    { alphabetisch sortieren }
    for i := 1 to anzahl do
    begin
      lauf := list^.next;
      vor := list;
      while assigned (lauf) do
      begin
        if ustr (lauf^.empf) < ustr (vor^.empf) then
          vertausche (lauf^, vor^);
        vor := lauf;
        lauf := lauf^.next;
      end;
    end;

    { Dupes lîschen }
    if assigned (list) then
    begin
      lauf := list^.next;
      vor := list;
      while assigned (lauf) do
        if uStr (lauf^.empf) = uStr (vor^.empf) then
        begin
          if lauf^.typ > vor^.typ then exchangeByte (lauf^.typ, vor^.typ);
          lauf^.vertreter := lauf^.vertreter or vor^.vertreter;
          removefromlist (list, vor, lauf)
        end
        else begin
          vor := lauf;
          lauf := lauf^.next;
        end;
    end;

    uEmpf := uStr (hdp^.wab);
    if (uEmpf = uStr (hdp^.oab)) or (uEmpf = uStr (hdp^.pmReplyTo))
    or (uEmpf = uStr (hdp^.absender)) then
      hdp^.wab := '';

    uEmpf := uStr (hdp^.oab);
    if (uEmpf = uStr (hdp^.pmReplyTo)) or (uEmpf = uStr (hdp^.absender)) then
      hdp^.oab := '';

    if (uStr (hdp^.pmReplyTo) = uStr (hdp^.absender)) then
      hdp^.pmReplyTo := '';
  end;

  { Alle Adressen werden durch Vertreteradressen ersetzt (sofern
    vorhanden). Die ersetzten Adressen werden markiert, damit im
    Auswahl-Dialog durch ein Sternchen (*) angezeigt werden kann,
    dass eine Vertreteradresse vorhanden ist. }

  procedure checkVertreterAdressen (var list :RTAEmpfaengerP);
  var lauf :RTAEmpfaengerP;
      s    :str90;
  begin
    lauf := list;
    while assigned (lauf) do
    begin
      lauf^.vertreter := getVertreter (lauf^.empf);
      lauf := lauf^.next;
    end;

    absenderHasVertreter := getVertreter (hdp^.absender);
    absenderIsUnknown := userUnbekannt (hdp^.absender);

    pmReplyToHasVertreter := getVertreter (hdp^.pmReplyTo);
    pmReplyToIsUnknown := userUnbekannt (hdp^.pmReplyTo);

    wabHasVertreter := getVertreter (hdp^.wab);
    wabIsUnknown := userUnbekannt (hdp^.wab);

    oabHasVertreter := getVertreter (hdp^.oab);
    oabIsUnknown := userUnbekannt (hdp^.oab);
  end;

  { Es wird ÅberprÅft, ob die gewÑhlten EmpfÑnger in der Userdatenbank
    eingetragen sind. Wenn nicht, werden sie mit Adressbuchgruppe 0 an-
    gelegt. Als Server wird der Server des Brettes der Ursprungsnachricht
    gewÑhlt. Wenn kein gÅltiger Server gefunden werden konnte, dann
    wird die Defaultbox vorgeschlagen }

  function checkEmpf (var empf :str90; var RTAEmpfList :RTAEmpfaengerP) :boolean;
  var unbekannteUser, lauf, vor :RTAEmpfaengerP;
      brett :string[5];
      box :string[BoxNameLen];
      result :boolean;
      anz :integer;
      auswahl :byte;

    procedure getPollBox;
    begin
      box := '';
      dbRead (mbase, 'brett', brett);
      if brett[1] in ['1', 'A'] then         { Brett }
      begin
        dbSeek (bbase, biIntNr, copy (brett, 2, 4));
        if dbBOF (bbase) or dbEOF (bbase) then box := ''
        else dbRead (bbase, 'pollbox', box);
      end;
      if not isBox (box) then box := DefaultBox;
    end;

    procedure getUnbekannteUser;
    var lauf :RTAEmpfaengerP;
    begin
      unbekannteUser := nil;
      if userUnbekannt (empf) then addToRTAList (unbekannteUser, empf, true, false, true, 3);
      lauf := RTAEmpfList;
      while assigned (lauf) do
      begin
        if lauf^.RTAEmpf and userUnbekannt (lauf^.empf) then
          addToRTAList (unbekannteUser, lauf^.empf, true, false, true, 3);
        lauf := lauf^.next;
      end;
    end;

    { Die User werden mit Standardeinstellungen und Adressbuchgruppe 0
      angelegt }

    procedure userAnlegen (user, box :string);
    var halten :integer16;
        b :byte;
    begin
      dbAppend(ubase);                        { neuen User anlegen }
      dbWrite(ubase,'username',user);
      dbWrite(ubase,'pollbox',box);
      halten:=stduhaltezeit;
      dbWrite(ubase,'haltezeit',halten);
      b:= 1+iif(newuseribm,0,8);
      halten := 0;
      dbWrite(ubase,'adrbuch', halten);
      dbWrite(ubase,'userflags',b);      { aufnehmen }
      dbFlushClose(ubase);
    end;

    { Allen neuen Usern wird der gleiche Server zugewiesen }

    procedure pollBoxZuweisen (box :string);
    var lauf :RTAEmpfaengerP;
    begin
      lauf := unbekannteUser;
      while assigned (lauf) do
      begin
        userAnlegen (lauf^.empf, box);
        lauf := lauf^.next;
      end;
    end;

    { FÅr jeden User erscheint das bekannte Dialogfenster "User bearbeiten" }

    procedure UserDialog (const box :string);
    var lauf :RTAEmpfaengerP;
    begin
      lauf := unbekannteUser;
      while assigned (lauf) do
      begin
        userAnlegen (lauf^.empf, box);
        modiUser (false);
        lauf := lauf^.next;
      end;
    end;

    function serverDialog (var box :string; const anz :integer) :byte;
    var x, y, breite, auswahl :byte;
        z :taste;
        s :string;
        oldbox :string[BoxNameLen];
    begin
      pushhp (3001);
      repeat
        oldbox := box;
        s := reps (getreps2 (2740, 0, box), formI (anz, 0));
        breite := length (s) + 4;
        msgBox (breite, 5, '', x, y);
        moff;
        wrt (x + 2, y + 1, s); { 'Allen unbekannten Usern (%s) als Serverbox "%s" zuweisen' }
        mon;
        auswahl := readButton (x + 2, y + 3, 2, '' + getres2 (2740,1), 1, true, z);
{        ReadJNesc (reps (getreps (2740, formi (anz, 0)), box), true, brk);}
        closeBox;
        freeRes;
        if auswahl = 3 then
        begin
          box := uniSel (1, false, box);
          if box = '' then box := oldBox;
        end;
      until auswahl <> 3;
      pophp;
      serverDialog := auswahl;
    end;

    function checkAnzahl :boolean;
    var lauf :RTAEmpfaengerP;
        anz :integer;
        ersterEmpfWirdErsetzt :boolean;
    begin
      lauf := RTAEmpfList;
      anz := 0;
      ersterEmpfWirdErsetzt := eigeneAdresse (eigeneAdressenbaum, empf) and not auswahlMarkierte;
      while assigned (lauf) do
      begin if lauf^.RTAEmpf then inc (anz); lauf := lauf^.next; end;
      if anz > iif (ersterEmpfWirdErsetzt, 126, 125) then
      begin
        rfehler1 (748, formI (iif (ersterEmpfWirdErsetzt, anz, anz + 1), 0));
          { Max. 126 EmpfÑnger erlaubt (ausgewÑhlt: %s)! (Hilfe mit F1) }
        checkAnzahl := false;
      end else
        checkAnzahl := true;
    end;

  begin
    result := true;
    getPollBox;
    getUnbekannteUser;
    if checkAnzahl then
    begin
      if assigned (unbekannteUser) then
      begin
        lauf := unbekannteUser;
        anz := 0;
        while assigned (lauf) do
        begin inc (anz); lauf := lauf^.next; end;
        auswahl := ServerDialog (box, anz);

        case auswahl of
          0,4: result := false;
          1:   PollBoxZuweisen (box);
          2:   UserDialog (box);
        end;
      end;
      if result and eigeneAdresse (eigeneAdressenbaum, empf) and not auswahlMarkierte then
                                  { Bei RTA wird eine eigene Adresse als         }
      begin                       { "erster EmpfÑnger" durch eine fremde ersetzt }
        lauf := RTAEmpfList;
        vor := nil;
        while not lauf^.RTAEmpf do
          removefromList (RTAEmpfList, vor, lauf);
        if assigned (RTAEmpfList) then
        begin
          empf := RTAEmpfList^.empf;
          vor := RTAEmpfList;
          RTAEmpfList := RTAEmpfList^.next;
          dispose (vor);
        end;
      end;
    end else
      result := false;
    checkEmpf := result;
    disposeRTAEmpfList (unbekannteUser);
  end;

  { Je nach Wert der Åbergebenen Variable wird ÅberprÅft, ob
    - mind. eine fremde Adresse in der RTA-EmpfÑngerliste steht
    - mind. eine fremde Adresse in der Liste steht, sofern
      der vorgesehene EmpfÑnger der Nachricht keine eigene Adresse
      ist oder mind zwei fremde Adresse vorhanden sind }

  function RTAEmpfVorhanden (const one :boolean) :boolean;
  var lauf :RTAEmpfaengerP;
      anz :byte;
  begin
    anz := 0;
    lauf := RTAEmpfList;
    while assigned (lauf) and (anz < 2) do
    begin
      if lauf^.RTAEmpf then inc (anz);
      lauf := lauf^.next;
    end;
    if one then
      RTAEmpfVorhanden := anz >= 1
    else
      RTAEmpfVorhanden := (anz >= 1) and
        (not eigeneAdresse (eigeneAdressenbaum, empf) or (anz >= 2));
  end;

  { 'EmpfÑnger auswÑhlen'-Dialogfenster }

  function GetEmpfaenger (const replyTo :AdrStr) :string;
  const leadingchar = #7;      { Das Zeichen durch das RTA-EmpfÑnger kenntlich gemacht werden }
  var abs, s    :str90;
      anz       :integer;      { Anzahl der Adressen im Fenster }
      h         :word;         { Hîhe des Fensters/Listers }
      x,y       :byte;
      brk       :boolean;
      auswahl   :string[110];
      userError :boolean;      { Wenn der User alle Adressen markiert und 'alle' auswÑhlt :) }
      RTAEmpfaengerVorhanden :boolean;
      savedList :RTAEmpfaengerP;
  label again;                 { Sprungmarke, um den Lister bei
                                 Userfehlern/-abbruch wieder zu starten }

    { öbergibt alle Adressen an den Lister }

    procedure adressenHinzufuegen;

      procedure add (const s :str90; const typ :byte; const RTAEmpf, vertreter, userUnbekannt :boolean);
      var s1 :string;
      begin
        app_l (iifs (RTAEmpf and RTAEmpfaengerVorhanden, leadingChar, ' ') + getres2 (476, typ) +
          iifs (not vertreter and not userUnbekannt, '  ', iifs (vertreter xor userUnbekannt, ' ', '')) +
          iifs (vertreter, '*', '') + iifs (userUnbekannt, '(', '') + s + iifs (userUnbekannt, ')', ''));
        inc (anz);
      end;

      procedure addLists;

        procedure hinzu (const typ :byte);
        var lauf :RTAEmpfaengerP;
        begin
          lauf := RTAEmpfList;
          while assigned (lauf) do
          begin
            if lauf^.typ = typ then
              add (lauf^.empf, typ, lauf^.RTAEmpf, lauf^.vertreter, lauf^.userUnbekannt);
            lauf := lauf^.next;
          end;
        end;

      begin
        hinzu (9);                                   { 'EmpfÑnger          :' }
        hinzu (8);                                   { 'Original-EmpfÑnger :' }
        hinzu (3);                                   { 'Kopien-EmpfÑnger   :' }
      end;

    begin
      if is_mailaddress (hdp^.pmReplyTo) then                   { 'Reply-To-EmpfÑnger :' }
        add (hdp^.pmReplyTo, 7, not eigeneAdresse (eigeneAdressenbaum, hdp^.pmReplyTo),
             pmReplyToHasVertreter, pmReplyToIsUnknown);
(*      if is_mailaddress (hdp^.wab) then                       { 'Original-Absender  :' }
        add (hdp^.absender, 1, (hdp^.pmReplyTo = '') and not eigeneAdresse (eigeneAdressenbaum, hdp^.absender),
             absenderHasVertreter, absenderIsUnknown)
      else *)                                        { 'Absender           :' }
        add (hdp^.absender, 5, (hdp^.pmReplyTo = '') and not eigeneAdresse (eigeneAdressenbaum, hdp^.absender),
             absenderHasVertreter, absenderIsUnknown);
      if is_mailaddress (hdp^.wab) then                     { 'Weiterleit-Absender:' }
        add (hdp^.wab, 2, false, wabHasVertreter, wabIsUnknown);
      if is_mailaddress (hdp^.oab) then
        add (hdp^.oab, 1, false, oabHasVertreter, oabIsUnknown);
      addLists; { EmpfÑnger, Original-EmpfÑnger und Kopien-Empfaenger }
    end;

    { Adresse aus den vom Lister zurÅckgegeben Strings extrahieren }

    function getAdresse (const s :string) :str90;
    var adr :String[90];
    begin
      adr := trim (copy (s, length (getres2 (476, 1)) + 3, 91));
      if adr[1] = '*' then delete (adr, 1, 1);
      if adr[1] = '(' then delete (adr, 1, 1);
      if adr[length (adr)] = ')' then delete (adr, length (adr), 1);
      getAdresse := adr;
    end;

    procedure markierteAdressenEntfernen (var userError :boolean);
    var lauf, vor, markierteAdressen, tempList :RTAEmpfaengerP;
        s :string;

      function AdresseMarkiert (const s: str90) :boolean;
      var lauf :RTAEmpfaengerP;
      begin
        lauf := markierteAdressen;
        while assigned (lauf) and (lauf^.empf <> s) do
          lauf := lauf^.next;
        AdresseMarkiert := assigned (lauf);
      end;

    begin
      userError := false;
      markierteAdressen := nil;  { Liste der markierten Adressen aufbauen }
      s := first_marked;
      repeat
        if pos ('@', s) > 0 then
          addToRTAList (markierteAdressen, uStr (getAdresse (trim (s))), true, false, false, 3);
        s := next_marked;
      until s = #0;

      tempList := nil;                  { RTA-EmpfÑngerliste sichern, um bei    }
      saveList (RTAEmpfList, tempList); { einem Usererror wiederholen zu kînnen }

      vor := nil;
      lauf := tempList;
      while assigned (lauf) do
      begin
        if adresseMarkiert (uStr (lauf^.empf)) then { markierte Adressen lîschen }
          removeFromList (tempList, vor, lauf)
        else begin
          vor := lauf;
          lauf := lauf^.next;
        end;
      end;

      if adresseMarkiert (uStr (abs)) then
      begin { Wenn der "erste" EmpfÑnger markiert ist... }
        vor := nil;
        lauf := tempList;
        while assigned (lauf) and not lauf^.RTAEmpf do
          removeFromList (tempList, vor, lauf);
        if assigned (lauf) then
        begin
          abs := lauf^.empf; { falls noch gÅltige Adressen vorhanden sind,    }
                             { wird die erste als neuer EmpfÑnger eingetragen }
          removeFromList (tempList, vor, lauf);
        end else             { Da hat der User wohl alle Adressen markiert... }
        begin
          abs := '';
          RTA := false;
          rfehler (747);     { 'Oops - *alle* passenden EmpfÑnger markiert/gefiltert!?' }
          userError := true;
        end;
      end;

      disposeRTAEmpfList (markierteAdressen);

      if not userError then
      begin
        disposeRTAEmpfList (RTAEmpfList); { Wenn kein Fehler, dann neue Liste }
        RTAEmpfList := tempList;          { Åbernehmen }
      end else
        disposeRTAEmpfList (tempList);    { neue Liste freigeben und noch einmal }
    end;

  begin
    savedList := nil;
    RTAEmpfaengerVorhanden := RTAEmpfVorhanden (false) and (RTAMode and 8 = 8);
    openlist (2, 78, 10, 11, 0, '/NS/SB/NLR/DM/M/');  { Koordinaten beliebig }
    if RTAEmpfaengerVorhanden then
      app_l (' ' + getres2 (476, 10));    { 'alle'   }
    app_l (' ' + getres2 (476, 11));      { 'markierte' }
    anz := 0;
    adressenHinzufuegen;

    saveList (RTAEmpfList, savedList);

    h := min(anz + iif (RTAEmpfaengerVorhanden, 4, 3), screenlines - 6);
    selbox (65, h, getres2 (476, 4), x, y, true);  { 'EmpfÑnger wÑhlen' }
    dec(h,2);
    attrtxt(col.colselrahmen);
    SetListsize(x + 1, x + 63, y + 1, y + h);
    listboxcol;
    listarrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
    listSetStartPos (iif (RTAEmpfaengerVorhanden, iif (RTAStandard, 1, 3), 2));
again:
    pushhp (3000);
    list(brk);
    pophp;
    if brk then abs := '' else
    begin
      auswahlMarkierte := false;
      auswahl := trim (get_selection);
      abs := getAdresse (auswahl);
      if auswahl = getres2 (476,10) then      { 'alle' }
      begin
        RTA := true;
        if replyTo <> '' then
          abs := replyTo
        else
{          dbRead (dispdat, 'absender', abs);}
          abs := hdp^.absender;
        if list_markanz <> 0 then
        begin
          markierteAdressenEntfernen (usererror);
          if userError then goto again;
        end;
      end
      else if auswahl = getres2(476,11) then  { 'markiert' }
      begin
        if list_markanz = 0 then
        begin
          rfehler(743);                       { 'Keine EintrÑge markiert!' }
          listSetStartPos (iif (RTAEmpfaengerVorhanden, 2, 1));
          goto again;
        end else
        begin
          auswahlMarkierte := true;
          disposeRTAEmpfList (RTAEmpfList);
          abs := getAdresse (trim (first_marked));
          if pos ('@', abs) = 0 then abs := '';
          s := next_marked;
          repeat
            if pos ('@', s) > 0 then  { MenÅzeilen filtern }
              if abs = '' then
                abs := getAdresse (trim (s))
              else
                addToRTAList (RTAEmpfList, getAdresse (trim (s)), true, false, false, 3);
            s := next_marked;
          until s = #0;
          if assigned (RTAEmpfList) then RTA := true;
        end;
        if abs = '' then
        begin
          rfehler(746);                       { 'UngÅltige Auswahl' }
          listSetStartPos (iif (RTAEmpfaengerVorhanden, 2, 1));
          disposeRTAEmpfList (RTAEmpfList);
          saveList (savedList, RTAEmpfList);
          goto again;
        end;
      end;
      if RTA then
        if not checkEmpf (abs, RTAEmpfList) then
        begin
          disposeRTAEmpfList (RTAEmpfList);
          saveList (savedList, RTAEmpfList);
          RTA := false;
          goto again;
        end;
    end;
    closelist;
    closebox;
    freeres;
    disposeRTAEmpfList (savedList);
    adresseAusgewaehlt := true;
    GetEmpfaenger := abs;
  end;

begin
  RTA := false;
  RTAEmpfList := nil;
  getEigeneAdressen (eigeneAdressenBaum);
  brk := false;
  dbRead(dispdat,'absender',empf);
  if ntRealName(mbNetztyp) then dbRead (dispdat, 'name', realname);
  new (hdp);
  readkoplist := true;      { KOP-Header auslesen }
  readempflist := true;     { EMP-Header auslesen }
  readHeadEmpf := 127;
  readOEMList := true;      { OEM-Header auslesen }
  readheader (hdp^, hds, false);

  addList (RTAEmpfList, xp3.empfList, 9);
  addList (RTAEmpfList, hdp^.oemList, 8);
  addList (RTAEmpfList, hdp^.kopien, 3);

  if (RTAMode and 4 = 0) and (RTAMode and 8 = 0) and (RTAMode and 64 = 0) then
    disposeRTAEmpfList (RTAEmpfList);

  checkVertreterAdressen (RTAEmpfList);

  checkList (RTAEmpfList);

  if ((hdp^.pmReplyTo <> '') and (RTAMode and 2 = 2) and (uStr (hdp^.pmReplyTo) <> uStr (hdp^.absender))
       and is_mailaddress (hdp^.pmReplyTo)
    or (hdp^.wab <> '') and is_mailaddress (hdp^.wab) and (RTAMode and 1 = 1)
    or RTAEmpfVorhanden (true) and (RTAMode and 4 = 4)
    or RTAEmpfVorhanden (false) and (RTAMode and 8 = 8)
    or (RTAMode and 64 = 64))
    and (RTAMode <> 0)
  then
    empf := GetEmpfaenger (hdp^.pmReplyTo);

  dispose (hdp);

  if not RTA then
    disposeRTAEmpfList (RTAEmpfList)
  else
    translateRTAEmpfList (RTAEmpfList, sendEmpfList);

  freeEigeneAdressenBaum (eigeneAdressenBaum);
end;

end.

{
  $Log$
  Revision 1.1.2.4  2001/08/02 17:44:33  my
  - now using function is_mailaddress from xp9sel.pas instead of adrOkay
  - removed function adrOkay

  Revision 1.1.2.3  2001/07/01 21:54:02  my
  - fixed last commit (eMail address was also taken after a net type change
    RFC/* => ZConnect)

  Revision 1.1.2.2  2001/07/01 20:00:36  my
  - Fix: eMail addresses are recognized as own addresses now, especially
         if eMail address is different from server@point.domain
  - added ID-Header

  Revision 1.1.2.1  2001/07/01 15:41:04  my
  SV:- moved RTA code to new unit xp4rta.pas
  SV:- Fixes:
       - Own ZConnect addresses are now recognized correctly
       - OAB Header is not disregarded anymore (same behaviour as WAB header)
       - RTA dialogue does not come up upon first install anymore
       - When WAB header is existent, sender is now named "Sender" rather
         "Original sender"

}

{ $Id$

   OpenXP main source file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.mandrella.de)

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

{ Reply-To-All- (RTA) Routinen }

{$I xpdefine.inc }

unit replytoall;

interface

uses xpglobal,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
typeform,fileio,inout,winxp,keys,maske,datadef,database,
  resource,xp0,xpnt,xp1,xp1input,xp2,xp3,xp4,xp4e,xpsendmessage, maus2,lister, sysutils,
  classes, xpHeader, xpconfigedit, xpmakeheader;

procedure askRTA(const XPStart :boolean);
procedure DoReplyToAll (var brk, adresseAusgewaehlt :boolean; var empf, realname :string; var dispdat :DB);

implementation

type
  TRTAEmpfaenger = class
  protected
    Empf          : String;
    RTAEmpf,
    Vertreter,
    UserUnbekannt :boolean;
    Typ           :byte;
  public
    constructor Create;
    constructor CreateWithOptions(const aEmpf: String; aRTAEmpf, aVertreter, aUserUnbekannt :boolean; aTyp: byte);
    destructor Destroy; override;
  end;

  TRTAEmpfaengerList = class
  private
    FItems: TList;
    function GetItems(Index: Integer): TRTAEmpfaenger;
    procedure SetItems(Index: Integer; const Value: TRTAEmpfaenger);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Empf: TRTAEmpfaenger);
    procedure Assign(Source: TRTAEmpfaengerList);
    function Count: Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Sort;
    property Items[Index: Integer]: TRTAEmpfaenger read GetItems write SetItems; default;
  end;


{ TRTAEmpfaenger }

constructor TRTAEmpfaenger.Create;
begin
  empf := '';
  RTAEmpf := false;
  vertreter := false;
  userUnbekannt := false;
  typ := 0;
end;


{ 'true', wenn Adresse im Baum vorhanden; 'false', wenn nicht. }

function eigeneAdresse (baum :domainNodeP; adresse :String) :boolean;
var p :domainNodeP;
begin
  adresse := UpperCase(adresse);
  p := baum;
  while assigned (p) and (p^.domain <> adresse) do
    if adresse < p^.domain then p := p^.left
      else p := p^.right;
  eigeneAdresse := assigned (p);
end;

{ Eine Adresse mit allen Parametern (RTAEmpfaenger, Vertreter, Typ) vorne (!)
  an eine RTA-EmpfÑngerliste anfÅgen }

procedure addToRTAList(List: TRTAEmpfaengerList; const empf :String; const RTAEmpf, vertreter, userUnbekannt :boolean;
                        const typ :byte);
var neu : TRTAEmpfaenger;
begin
  Neu := TRTAEmpfaenger.Create;
  neu.empf := empf;
  neu.RTAEmpf := RTAEmpf;
  neu.vertreter := vertreter;
  neu.userUnbekannt := userUnbekannt;
  neu.typ := typ;
  List.Add(neu);
end;

{ Ganze EmpfÑngerlisten an eine RTA-EmpfÑngerliste anfÅgen }

procedure addList (orginalList :TRTAEmpfaengerList; newList: TStringList; const typ :byte);
var
  i: Integer;
begin
  if NewList.Count = 0 then exit;

  // Einzelne Elemente in die neue Liste kopieren
  for i := 0 to NewList.Count - 1 do
    if newList[i] <> '' then    { Leerstrings sind keine Adressen! }
      orginalList.Add(TRTAEmpfaenger.CreateWithOptions(newList[i], false, false, false, typ));
  NewList.Clear;
end;

{ Baumstuktur freigeben }

procedure freeEigeneAdressenBaum (var baum :domainNodeP);
var lauf :domainNodeP;
begin
  if Assigned (baum) then
  begin
    freeEigeneAdressenBaum (baum^.left);
    lauf := baum^.right;
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
      node^.domain := adresse;
    end else
      if node^.domain > adresse then
        insertNode (node^.left, adresse)
      else
        insertNode (node^.right, adresse)
  end;

begin
  eigeneAdressenBaum := nil;
  notEigeneAdressenbaum := nil;

  if RTANoOwnAddresses <> '' then          { Adressen aus dem Config-     }
  begin                                         { Setting RTANotEigeneAdressen }
    s := RTANoOwnAddresses;                     { verwerten                    }
    repeat
      if cpos (' ', s) <> 0 then
      begin
        adresse := trim (copy (s, 1, cpos (' ', s)));
        delete (s, 1, cpos (' ', s));
      end else
      begin
        adresse := s;
        s := '';
      end;
      if IsMailAddress(adresse) then
        insertNode (notEigeneAdressenBaum, UpperCase(adresse));
    until s = '';
  end;

  dbopen (d, BoxenFile, 0);     { eigene Adressen aus Boxenkonfigurationen auslesen }
  while not dbEof (d) do
  begin
    if ntReplyToAll (dbReadInt (d, 'netztyp') and $FF) then { nur ZConnect und RFC/UUCP }
    begin                                           { Boxen berÅcksichtigen     }
      Username := dbReadStr (d, 'username');
      PointName := dbReadStr (d, 'pointname');
      dbRead (d, 'script', flags);
      aliaspt := (flags and 4 <> 0);
      Domain := dbReadStr (d, 'domain');
      Box := dbReadStr (d, 'boxname');
      eMail := dbReadStr (d, 'email');
      case ntDomainType (dbReadInt (d, 'netztyp')) of
        5: adresse := username + '@' + iifs (aliaspt, pointname, box) + domain;
        6: if email <> '' then adresse := email
           else adresse := username + '@' +
            iifs (aliaspt, box + ntServerDomain (box), pointname + domain);
        else adresse := '';
      end;
      if (adresse <> '') and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertnode (eigeneAdressenBaum, UpperCase(adresse));
      Adresse := dbReadStr (d, 'replyto');
      if (adresse <> '') and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertnode (eigeneAdressenBaum, UpperCase(adresse));
    end;
    dbNext (d);
  end;
  dbClose (d);

  if RTAOwnAddresses <> '' then    { Adressen aus dem Config-Setting }
  begin                                 { RTAEigeneAdressen verwerten     }
    s := RTAOwnAddresses;
    repeat
      if cpos (' ', s) <> 0 then
      begin
        adresse := trim (copy (s, 1, cpos (' ', s)));
        delete (s, 1, cpos (' ', s));
      end else
      begin
        adresse := s;
        s := '';
      end;
      if IsMailAddress(adresse) and not eigeneAdresse (notEigeneAdressenbaum, adresse) then
        insertNode (eigeneAdressenBaum, UpperCase(adresse));
    until s = '';
  end;

  freeEigeneAdressenBaum (notEigeneAdressenbaum);
end;

{ RTA-EmpfÑngerliste in eine EmpfÑngerliste umwandeln, die XP6.DoSend versteht }

procedure translateRTAEmpfList (RTAEmpfList :TRTAEmpfaengerList; sendEmpfList :TStringList);
var
  i: Integer;
begin
  for i := 0 to RTAEmpfList.Count - 1 do
    if RTAEmpfList[I].RTAEmpf then
      sendEmpfList.Add(RTAEmpfList[i].Empf);
  RTAEmpfList.Clear;
end;

procedure askRTA (const XPStart :boolean);
var x,y,i       : Integer;
    msglines, p :byte;
    z           :taste;
    res         :boolean;
    s           :string;
begin
  if (ntUsed[nt_UUCP] + ntUsed[nt_ZConnect] + ntUsed[nt_Client] +
    ntUsed[nt_NNTP] + ntUsed[nt_POP3] + ntUsed[nt_IMAP]> 0) and (RTAMode and 128 = 128) {and
     (not XPFirstStart) } then
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
        Wrt2(LeftStr(s, p-1));
        delete (s, 1, p);
        p := cposx ('*', s);
        attrtxt (col.colmboxhigh);
        Wrt2(LeftStr(s, p - 1));
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
  end;{ else }
{  if XPFirstStart then RTAMode := 13; }
end;

function IsUserUnbekannt (const user :string) :boolean;
begin
  dbSeek (ubase, uiName, UpperCase(user));
  Result := (not dbFound) and (user <> '');
end;

procedure exchangeByte (var i, j :byte);
var h :byte;
begin
  h := i;
  i := j;
  j := h;
end;

(*procedure removeFromList (var list, vor, lauf :RTAEmpfaengerP);
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
end; *)

{ die Åbergebene Adresse wird durch die Vertreteradresse ersetzt,
  sofern vorhanden. Es wird 'true' zurÅckgeben, wenn Vertreter
  vorhanden. }

function getVertreter (var adr :String) :boolean;
var 
  size: Integer;
begin
  dbSeek (ubase, uiName, UpperCase(adr));
  if dbFound then
  begin
    size := 0;
    if dbXsize (ubase, 'adresse') <> 0 then
    begin
      adr := dbReadXStr (ubase, 'adresse', size);
      getVertreter := true;
    end else
      getVertreter := false;
  end else
    getVertreter := false;
end;


procedure DoReplyToAll (var brk, adresseAusgewaehlt :boolean; var empf, realname :string; var dispdat :DB);
var RTAEmpfList : TRTAEmpfaengerList;
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
    hdp     : THeader;
    hds     : longint;
    List: TLister;

    { Diese Prozedur ÅberprÅft die Åbergebene Liste auf Dupes,
      ungÅltige Adressen und andere SpezialfÑlle. Nebenbei wird
      sie auch noch alphabetisch sortiert }

  procedure checklist (List : TRTAEmpfaengerList);
  var
    i: Integer;
    uEmpf :string;
  begin
    i := 0;
    while i < List.Count do 
      with List[i] do
      begin
        if cpos(' ', empf) <> 0 then Empf := LeftStr(Empf, cpos(' ', Empf));
        { ^^ Realname entfernen }
        uEmpf := UpperCase(Empf);
        if (uEmpf = UpperCase (hdp.absender)) or (cpos ('@', Empf) = 0)
          or (uEmpf = UpperCase (hdp.ReplyTo))
          or (uEmpf = UpperCase (hdp.wab))
          or (uEmpf = UpperCase (hdp.oab))
          or (not IsMailAddress(Empf)) then
            List.Delete(i)
        else begin
          Inc(i);
          RTAEmpf := not eigeneAdresse (eigeneAdressenbaum, empf);
          userUnbekannt := IsUserUnbekannt(Empf);
        end;
      end;

    { alphabetisch sortieren }
    List.Sort;

    { Dupes lîschen }
(*    if assigned (list) then
    begin
      lauf := list^.next;
      vor := list;
      while assigned (lauf) do
        if UpperCase (lauf^.empf) = UpperCase (vor^.empf) then
        begin
          if lauf^.typ > vor^.typ then exchangeByte (lauf^.typ, vor^.typ);
          lauf^.vertreter := lauf^.vertreter or vor^.vertreter;
          removefromlist (list, vor, lauf)
        end
        else begin
          vor := lauf;
          lauf := lauf^.next;
        end;
    end; *)

    uEmpf := UpperCase (hdp.wab);
    if (uEmpf = UpperCase (hdp.oab)) or (uEmpf = UpperCase (hdp.ReplyTo))
    or (uEmpf = UpperCase (hdp.absender)) then
      hdp.wab := '';

    uEmpf := UpperCase (hdp.oab);
    if (uEmpf = UpperCase (hdp.ReplyTo)) or (uEmpf = UpperCase (hdp.absender)) then
      hdp.oab := '';

    if (UpperCase (hdp.ReplyTo) = UpperCase (hdp.absender)) then
      hdp.ReplyTo := '';
  end;

  { Alle Adressen werden durch Vertreteradressen ersetzt (sofern
    vorhanden). Die ersetzten Adressen werden markiert, damit im
    Auswahl-Dialog durch ein Sternchen (*) angezeigt werden kann,
    dass eine Vertreteradresse vorhanden ist. }

  procedure checkVertreterAdressen (list : TRTAEmpfaengerList);
  var
    I: Integer;
  begin
    for i := 0 to List.Count - 1 do
      with List[i] do
        Vertreter := GetVertreter (Empf);

    absenderHasVertreter := getVertreter (hdp.absender);
    absenderIsUnknown := IsUserUnbekannt (hdp.absender);

    pmReplyToHasVertreter := getVertreter (hdp.ReplyTo);
    pmReplyToIsUnknown := IsUserUnbekannt (hdp.ReplyTo);

    wabHasVertreter := getVertreter (hdp.wab);
    wabIsUnknown := IsUserUnbekannt (hdp.wab);

    oabHasVertreter := getVertreter (hdp.oab);
    oabIsUnknown := IsUserUnbekannt (hdp.oab);
  end;

  { Es wird ÅberprÅft, ob die gewÑhlten EmpfÑnger in der Userdatenbank
    eingetragen sind. Wenn nicht, werden sie mit Adressbuchgruppe 0 an-
    gelegt. Als Server wird der Server des Brettes der Ursprungsnachricht
    gewÑhlt. Wenn kein gÅltiger Server gefunden werden konnte, dann
    wird die Defaultbox vorgeschlagen }

  function checkEmpf (var empf : String; var RTAEmpfList : TRTAEmpfaengerList) :boolean;
  var
    unbekannteUser: TRTAEmpfaengerList;
    brett :string[5];
    box :string;
    auswahl :byte;

    procedure getPollBox;
    var 
      hdp2: THeader;
      hds2 :longint;
    begin
      box := '';
      brett := dbReadStr (mbase, 'brett');
      if brett[1] in ['1', 'A'] then         { Brett }
      begin
        dbSeek (bbase, biIntNr, copy (brett, 2, 4));
        if dbBOF (bbase) or dbEOF (bbase) then box := ''
        else Box := dbReadStr (bbase, 'pollbox');
      end else
      if brett[1] = 'U' then                 { User }
      begin
        hdp := THeader.Create;
        readHeader (hdp2, hds2, false);
        dbseek (ubase, uiname, UpperCase(hdp2.empfaenger));
        if dbFound then dbRead (ubase, 'pollbox', box);
        hdp.Free;
      end;
      if not isBox (box) then box := DefaultBox;
    end;

    procedure getUnbekannteUser;
    var
      i: Integer;
    begin
      unbekannteUser := TRTAEmpfaengerList.Create;
      if ISuserUnbekannt (empf) then unbekannteUser.Add(TRTAEmpfaenger.CreateWithOptions(empf, true, false, true, 3));

      for i := 0 to RTAEmpfList.Count - 1 do
        with RTAEMpfList[i] do
          if RTAEmpf and IsUserUnbekannt(Empf) then
            addToRTAList(unbekannteUser, empf, true, false, true, 3);
    end;

    { Allen neuen Usern wird der gleiche Server zugewiesen }

    procedure pollBoxZuweisen (const box :string);
    var
      i: Integer;
    begin
      for i := 0 to unbekannteUser.Count - 1 do
        AddNewUser(unbekannteUser[i].empf, box);
    end;

    { FÅr jeden User erscheint das bekannte Dialogfenster "User bearbeiten" }

    procedure UserDialog (const box :string);
    var
      i: Integer;
    begin
      for i := 0 to unbekannteUser.Count - 1 do
      begin
        AddNewUser(unbekannteUser[i].empf, box);
        modiUser (false);
      end;
    end;

    function serverDialog (var box :string; const anz :integer) :byte;
    var x, y, breite, auswahl : Integer;
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
        Mwrt (x + 2, y + 1, s); { 'Allen unbekannten Usern (%s) als Serverbox "%s" zuweisen' }
        auswahl := readButton (x + 2, y + 3, 2, '' + getres2 (2740,1), 1, true, z);
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

  begin
    result := true;
    getPollBox;
    getUnbekannteUser;
    if unbekannteUser.Count > 0 then
    begin
      auswahl := ServerDialog (box, unbekannteUser.Count);

      case auswahl of
        0,4: result := false;
        1:   PollBoxZuweisen (box);
        2:   UserDialog (box);
      end;
    end;
(*    if result and eigeneAdresse (eigeneAdressenbaum, empf) and not auswahlMarkierte then
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
    end; *)
    checkEmpf := result;
    unbekannteUser.Free;
  end;

  { Je nach Wert der Åbergebenen Variable wird ÅberprÅft, ob
    - mind. eine fremde Adresse in der RTA-EmpfÑngerliste steht
    - mind. eine fremde Adresse in der Liste steht, sofern
      der vorgesehene EmpfÑnger der Nachricht keine eigene Adresse
      ist oder mind zwei fremde Adresse vorhanden sind }

  function RTAEmpfVorhanden (const one :boolean) :boolean;
  var
    i, anz: Integer;
  begin
    anz := 0;
    for i:=0 to RTAEmpfList.Count-1 do
      if RTAEmpfList[i].RTAEmpf then
      begin
        Inc(anz);
        if anz=2 then break;
      end;

    if one then
      RTAEmpfVorhanden := anz >= 1
    else
      RTAEmpfVorhanden := (anz >= 1) and
        (not eigeneAdresse (eigeneAdressenbaum, empf) or (anz >= 2));
  end;

  { 'EmpfÑnger auswÑhlen'-Dialogfenster }

  function GetEmpfaenger (const replyTo: String) :string;
  const leadingchar = #7;      { Das Zeichen durch das RTA-EmpfÑnger kenntlich gemacht werden }
  var abs, s    :String;
      anz       :integer;      { Anzahl der Adressen im Fenster }
      h         :word;         { Hîhe des Fensters/Listers }
      x,y       : Integer;
      brk       :boolean;
      auswahl   :string[110];
      userError :boolean;      { Wenn der User alle Adressen markiert und 'alle' auswÑhlt :) }
      RTAEmpfaengerVorhanden :boolean;
      savedList : TRTAEmpfaengerList;
  label again;                 { Sprungmarke, um den Lister bei
                                 Userfehlern/-abbruch wieder zu starten }

    { öbergibt alle Adressen an den Lister }

    procedure adressenHinzufuegen;

      procedure add(const s :String; const typ :byte; const RTAEmpf, vertreter, userUnbekannt :boolean);
      begin
        List.AddLine (iifs (RTAEmpf and RTAEmpfaengerVorhanden, leadingChar, ' ') + getres2 (476, typ) +
          iifs (not vertreter and not userUnbekannt, '  ', iifs (vertreter xor userUnbekannt, ' ', '')) +
          iifs (vertreter, '*', '') + iifs (userUnbekannt, '(', '') + s + iifs (userUnbekannt, ')', ''));
        inc (anz);
      end;

      procedure addLists;

        procedure hinzu (aTyp: Byte);
        var
          i: Integer;
        begin
          for i := 0 to RTAEmpfList.Count - 1 do
            with RTAEmpfList[i] do
              if typ = aTyp then
                add (empf, typ, RTAEmpf, vertreter, userUnbekannt);
        end;

      begin
        hinzu (9);                                   { 'EmpfÑnger          :' }
        hinzu (8);                                   { 'Original-EmpfÑnger :' }
        hinzu (3);                                   { 'Kopien-EmpfÑnger   :' }
      end;

    begin
      if IsMailAddress(hdp.ReplyTo) then                   { 'Reply-To-EmpfÑnger :' }
        add (hdp.ReplyTo, 7, not eigeneAdresse (eigeneAdressenbaum, hdp.ReplyTo),
             pmReplyToHasVertreter, pmReplyToIsUnknown);
(*      if adrOkay (hdp^.wab) then                       { 'Original-Absender  :' }
        add (hdp^.absender, 1, (hdp^.pmReplyTo = '') and not eigeneAdresse (eigeneAdressenbaum, hdp^.absender),
             absenderHasVertreter, absenderIsUnknown)
      else *)                                        { 'Absender           :' }
        add (hdp.absender, 5, (hdp.ReplyTo = '') and not eigeneAdresse (eigeneAdressenbaum, hdp.absender),
             absenderHasVertreter, absenderIsUnknown);
      if IsMailAddress(hdp.wab) then                     { 'Weiterleit-Absender:' }
        add (hdp.wab, 2, false, wabHasVertreter, wabIsUnknown);
      if IsMailAddress(hdp.oab) then
        add (hdp.oab, 1, false, oabHasVertreter, oabIsUnknown);
      addLists; { EmpfÑnger, Original-EmpfÑnger und Kopien-Empfaenger }
    end;

    { Adresse aus den vom Lister zurÅckgegeben Strings extrahieren }

    function getAdresse (const s :string) :String;
    begin
      Result := trim (copy (s, length (getres2 (476, 1)) + 3, 91)); //!!
      if FirstChar(Result) = '*' then Delete(Result, 1, 1);
      if FirstChar(Result)= '(' then Delete(Result, 1, 1);
      if LastChar(Result) = ')' then SetLength(Result, Length(Result)-1);
    end;

    procedure markierteAdressenEntfernen (var userError :boolean);
    var
      markierteAdressen, tempList : TRTAEmpfaengerList;
      s :string;
      i: Integer;

      function AdresseMarkiert (const s: String) :boolean;
      var
        i: Integer;
      begin
        for i := 0 to markierteAdressen.Count - 1 do
          if markierteAdressen[i].Empf = s then
          begin
            Result := true;
            Exit;
          end;
        Result := false;
      end;

    begin
      userError := false;
      markierteAdressen := TRTAEmpfaengerList.Create; { Liste der markierten Adressen aufbauen }
      s := List.FirstMarked;
      repeat
        if cpos ('@', s) > 0 then
          addToRTAList (markierteAdressen, UpperCase (getAdresse (trim (s))), true, false, false, 3);
        s := List.NextMarked;
      until s = #0;

      tempList := TRTAEmpfaengerList.Create; { RTA-EmpfÑngerliste sichern, um bei    }
      tempList.Assign(RTAEmpfList);          { einem Usererror wiederholen zu kînnen }

      i := 0;
      while i < tempList.Count do
        if adresseMarkiert(UpperCase(tempList[i].empf)) then { markierte Adressen lîschen }
          tempList.Delete(i)
        else 
          Inc(i);

      if adresseMarkiert (UpperCase (abs)) then
      begin { Wenn der "erste" EmpfÑnger markiert ist... }
        i := 0;
        while i < tempList.Count do
          if not tempList[i].RTAEmpf then
            tempList.Delete(i)
          else
            Inc(i);

        if tempList.Count > 0 then
        begin
          abs := tempList[0].empf; { falls noch gÅltige Adressen vorhanden sind,    }
                                   { wird die erste als neuer EmpfÑnger eingetragen }
          tempList.Delete(0);
        end else             { Da hat der User wohl alle Adressen markiert... }
        begin
          abs := '';
          RTA := false;
          rfehler (747);     { 'Oops - *alle* passenden EmpfÑnger markiert/gefiltert!?' }
          userError := true;
        end;
      end;

      markierteAdressen.Free;

      if not userError then
      begin
        // Wenn kein Fehler, dann neue Liste Åbernehmen
        RTAEmpfList.Assign(tempList);
      end else
        tempList.Clear;  // neue Liste freigeben und noch einmal
    end;

  begin
    savedList := TRTAEmpfaengerList.Create;
    RTAEmpfaengerVorhanden := RTAEmpfVorhanden (false) and (RTAMode and 8 = 8);
    List := TLister.CreateWithOptions(2, 78, 10, 11, 0, '/NS/SB/NLR/DM/M/');  { Koordinaten beliebig }
    if RTAEmpfaengerVorhanden then
      List.AddLine(' ' + getres2 (476, 10));    { 'alle'   }
    List.AddLine(' ' + getres2 (476, 11));      { 'markierte' }
    anz := 0;
    adressenHinzufuegen;

    savedList.Assign(RTAEmpfList);

    h := min(anz + iif (RTAEmpfaengerVorhanden, 4, 3), screenlines - 6);
    selbox (65, h, getres2 (476, 4), x, y, true);  { 'EmpfÑnger wÑhlen' }
    dec(h,2);
    attrtxt(col.colselrahmen);
    List.SetSize(x + 1, x + 63, y + 1, y + h);
    listboxcol(List);
    List.SetArrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
    List.StartPos := iif(RTAEmpfaengerVorhanden, iif (RTAStandard, 1, 3), 2);
again:
    pushhp (3000);
    brk := List.Show;
    pophp;
    if brk then abs := '' else
    begin
      auswahlMarkierte := false;
      auswahl := trim (List.GetSelection);
      abs := getAdresse (auswahl);
      if auswahl = getres2 (476,10) then      { 'alle' }
      begin
        RTA := true;
        if replyTo <> '' then
          abs := replyTo
        else
          abs := hdp.absender;
        if List.SelCount <> 0 then
        begin
          markierteAdressenEntfernen (usererror);
          if userError then goto again;
        end;
      end
      else if auswahl = getres2(476,11) then  { 'markiert' }
      begin
        if List.SelCount = 0 then
        begin
          rfehler(743);                       { 'Keine EintrÑge markiert!' }
          List.StartPos :=  iif (RTAEmpfaengerVorhanden, 2, 1);
          goto again;
        end else
        begin
          auswahlMarkierte := true;
          RTAEmpfList.Clear;
          abs := getAdresse (trim (List.FirstMarked));
          if cpos ('@', abs) = 0 then abs := '';
          repeat
            s := List.NextMarked;
            if cpos ('@', s) > 0 then  { MenÅzeilen filtern }
              if abs = '' then
                abs := getAdresse (trim (s))
              else
                addToRTAList (RTAEmpfList, getAdresse (trim (s)), true, false, false, 3);
          until s = #0;
          if assigned (RTAEmpfList) then RTA := true;
        end;
        if abs = '' then
        begin
          rfehler(746);                       { 'UngÅltige Auswahl' }
          List.StartPos := iif (RTAEmpfaengerVorhanden, 2, 1);
          RTAEmpfList.Assign(savedList);
          goto again;
        end;
      end;
      if RTA then
        if not checkEmpf (abs, RTAEmpfList) then
        begin
          RTAEmpfList.Assign(savedList);
          RTA := false;
          goto again;
        end;
    end;
    List.Free;
    closebox;
    freeres;
    savedList.Free;
    adresseAusgewaehlt := true;
    GetEmpfaenger := abs;
  end;

begin
  RTA := false;
  RTAEmpfList := TRTAEmpfaengerList.Create;
  getEigeneAdressen (eigeneAdressenBaum);
  brk := false;
  empf := dbReadStr(dispdat,'absender');
  if ntRealName(mbNetztyp) then Realname := dbReadStr (dispdat, 'name');
  hdp := THeader.Create;
  readkoplist := true;      { KOP-Header auslesen }
  readempflist := true;     { EMP-Header auslesen }
  readHeadEmpf := 127;
  readheader (hdp, hds, false);

  addList (RTAEmpfList, empfList, 9);
  addList (RTAEmpfList, hdp.oem, 8);
  addList (RTAEmpfList, hdp.kopien, 3);

  if (RTAMode and 4 = 0) and (RTAMode and 8 = 0) and (RTAMode and 64 = 0) then
    RTAEmpfList.Clear;

  checkVertreterAdressen (RTAEmpfList);

  checkList (RTAEmpfList);

  if ((hdp.ReplyTo <> '') and (RTAMode and 2 = 2) and (UpperCase (hdp.ReplyTo) <> UpperCase (hdp.absender))
       and IsMailAddress(hdp.ReplyTo)
    or (hdp.wab <> '') and IsMailAddress(hdp.wab) and (RTAMode and 1 = 1)
    or RTAEmpfVorhanden (true) and (RTAMode and 4 = 4)
    or RTAEmpfVorhanden (false) and (RTAMode and 8 = 8)
    or (RTAMode and 64 = 64))
    and (RTAMode <> 0)
  then
    empf := GetEmpfaenger (hdp.ReplyTo);

  hdp.Free;

  if not RTA then
    RTAEmpfList.Free
  else
    translateRTAEmpfList (RTAEmpfList, sendEmpfList);

  freeEigeneAdressenBaum (eigeneAdressenBaum);
end;


constructor TRTAEmpfaenger.CreateWithOptions(const aEmpf: String;
  aRTAEmpf, aVertreter, aUserUnbekannt :boolean; aTyp: byte);
begin
  Empf := aempf;
  RTAEmpf := aRTAEmpf;
  vertreter := avertreter;
  userUnbekannt := auserUnbekannt;
  typ := atyp;
end;

destructor TRTAEmpfaenger.Destroy;
begin

  inherited Destroy;
end;

{ TRTAEmpfaengerList }

procedure TRTAEmpfaengerList.Add(Empf: TRTAEmpfaenger);
begin
  FItems.Add(Empf);
end;

procedure TRTAEmpfaengerList.Assign(Source: TRTAEmpfaengerList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    with Source[i] do
      Add(TRTAEmpfaenger.CreateWithOptions(Empf, RTAEmpf, Vertreter, UserUnbekannt, Typ));
end;

procedure TRTAEmpfaengerList.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TRTAEmpfaenger(FItems[i]).Free;
  FItems.Clear;
end;

function TRTAEmpfaengerList.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TRTAEmpfaengerList.Create;
begin
  FItems := TList.Create;
end;

procedure TRTAEmpfaengerList.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TRTAEmpfaengerList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TRTAEmpfaengerList.GetItems(Index: Integer): TRTAEmpfaenger;
begin
  Result := TRTAEmpfaenger(FItems[Index]);
end;

procedure TRTAEmpfaengerList.SetItems(Index: Integer;
  const Value: TRTAEmpfaenger);
begin
 FItems[Index] := Value;
end;

procedure TRTAEmpfaengerList.Sort;
begin
  // !!
end;
{
  $Log$
  Revision 1.23  2001/11/01 17:45:40  mk
  -SV: Statt der Stammbox wird nun der richtige Server angeboten (Dank an
    Jochen Gehring fuer die Codevorlage)

  Revision 1.22  2001/11/01 14:43:42  mk
  - do askRTA when client and pop3, nntp and imap-boxes are available

  Revision 1.21  2001/10/17 20:11:05  mk
  - fixed range check error in ntReplyToAll

  Revision 1.20  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.19  2001/09/09 09:25:53  mk
  - completed last fix

  Revision 1.18  2001/09/09 09:12:04  cl
  - BUGFIX: crash on CTRL-B/P with certain mail messages

  Revision 1.17  2001/09/08 14:18:17  cl
  - VirtualPascal fixes

  Revision 1.16  2001/09/08 12:50:38  mk
  - numerous fixes

  Revision 1.15  2001/09/07 18:21:02  mk
  - fixed initialization of markierteAdressen in GetAdresse

  Revision 1.14  2001/09/07 02:23:26  mk
  - useranlegen is duplicate to AddNewUser, replaced

  Revision 1.13  2001/08/23 11:15:01  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.12  2001/08/12 20:01:39  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.11  2001/08/12 11:50:36  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.10  2001/08/11 22:20:32  mk
  - fixed crash in main function
  - pos() -> cPos()
  - add THeader.OEM to Reply-List

  Revision 1.9  2001/08/11 10:24:46  mk
  - numerous RTA fixes

  Revision 1.8  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.7  2001/08/02 21:21:04  mk
  - now using function is_mailaddress from xpconfigedit.pas instead of adrOkay
  - removed function adrOkay

  Revision 1.6  2001/07/31 16:18:39  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.5  2001/07/31 13:10:32  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.4  2001/07/28 19:35:58  mk
  - askRTA asks not every time ;)

  Revision 1.3  2001/07/28 18:13:56  mk
  - fixed AnsiString database read/write
  - fixed use of crt.write to Wrt2

  Revision 1.2  2001/07/27 22:51:29  mk
  - fixed some Freepascal compile problems

  Revision 1.1  2001/07/27 18:10:10  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

}

end.


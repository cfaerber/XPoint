{  $Id$

   OpenXP/32: DoSend - Address Editing
   
   (C) Copyright 1991-2001 Peter Mandrella
   (C) Copyright 2001-2002 by OpenXP/32 team <http://www.openxp.de>

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

{$i xpdefine.inc }

unit xpsendmessage_addr_edit;

{ -------------------------- } interface { --------------------------- }

uses 
  addresslist,xpnt,xpheader;

procedure EditEmpfaengerList(
  const DialogueTitle: String;          // Dialog-Titel

  EditRecipients:   Boolean;            // Empfänger bearbeiten
  EmpfList:         TAddressList;       // Primäre Empfänger/Newsgroups

  SavedList:        TAddressList;       // Übrige Emfänger aus der 
                                        // ursprünglichen Nachricht
                                        
  EditSubject:      Boolean;            // Betreff bearbeiten
  ShowSubject:      Boolean;            // Betreff überhaupt anzeigen

  var Subject:      String;             // Betreff

  PMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für PMs
  AMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für AMs
  
  sData:            TSendUUData         // may be nil
);

procedure CheckEmpfaengerList(
  List:             TAddressList;       // Addresse to check
  Prompt:           Boolean;            // Ask user for new addresses
  AutoAdd:          Boolean;            // Add addresses to DB

  sData:            TSendUUData         // may be nil
);

{ ------------------------ } implementation { ------------------------ }

uses
  xpglobal, sysutils, rfc2822, resource, maske, typeform, 
  xp0, xp1, xp3, winxp, xp4e, database, xpcc, datadef, addresses;

var done: boolean;

var
  TypStr: array [TAddressListType] of String;
  List: TAddressList;
  i,x,y,w1: integer; max_h,new_h,h: 1..20;
  brk: boolean;

  FieldUp:  integer;
  FieldDwn: integer;
  Field1st: integer;
  FieldInc: integer;

  AddrCont: array[0..2,0..19] of String;
  AddrText:  array[0..2,0..19] of Pointer;
  Dummy1,Dummy2: string;
  Offset: Integer;

  procedure ScrollDone2(i,f:integer);
  var show_type : boolean;
      show_grp  : boolean;
      show_grp_start : boolean;
      show_grp_stop  : boolean;
  begin
    if Offset+i<List.Count then
    begin
      show_grp       := (List[i+Offset].Group>=0);
      show_grp_start := Show_Grp and ((i+Offset<=0)or(List[i+Offset].Group<>List[i+Offset-1].Group));
      show_grp_stop  := Show_Grp and ((i+Offset>=List.Count-1)or(List[i+Offset].Group<>List[i+Offset+1].Group));
      show_type      := (List[i+Offset].PM or List[i+Offset].Empty) and (show_grp_start or not show_grp);

      setfield(F+0,iifs(List[i+Offset].AddressType=atUnused,TypStr[atTo],TypStr[List[i+Offset].AddressType]));
      setfieldenable(F+0,    show_type);
      setfieldnodisp(F+0,not show_type);
      settexttext(AddrText[2,i],iifs(not(List[i+Offset].PM or List[i+Offset].Empty or show_grp),TypStr[atNewsgroup],''));

      if show_grp then setfield(F+1,List.GroupNames[List[i+Offset].Group])
      else             setfield(f+1,'');
      setfieldenable(F+1,    show_grp_start);
      setfieldnodisp(F+1,not show_grp_start);
      settexttext(AddrText[0,i],iifs(show_grp_start,':',''));

      setfield(F+2,List[i+Offset].DisplayString);
      setfieldenable(F+2,    show_grp);
      setfieldnodisp(F+2,not show_grp);

      setfield(F+3,List[i+Offset].DisplayString);
      setfieldenable(F+3,not show_grp);
      setfieldnodisp(F+3,    show_grp);

      settexttext(AddrText[1,i],iifs(show_grp_stop,';',' '));
    end else
    begin
      setfield(f+0,'');
      setfieldenable(f+0,false);
      setfieldnodisp(f+0,true);

      setfield(f+1,'');
      setfieldenable(f+1,false);
      setfieldnodisp(f+1,true);

      setfield(f+2,'');
      setfieldenable(f+2,false);
      setfieldnodisp(f+2,true);

      setfield(f+3,'');
      setfieldenable(f+3,false);
      setfieldnodisp(f+3,true);

      settexttext(AddrText[0,i],'');
      settexttext(AddrText[1,i],'');
      settexttext(AddrText[2,i],'');
    end;
  end;

  procedure ScrollDone;
  var i: integer;
      f: integer;

  begin
    if (h<max_h) and (h<List.Count) then
    begin
      new_h := Min(max_h,h+3);
      done := false;
      MQuit(false);
      exit;
    end;

    if not done then
      exit;

    if Offset>List.Count-h then Offset := List.Count-h;
    if Offset<0 then Offset := 0;

    f:=Field1st;

    for i := 0 to h-1 do
    begin
      ScrollDone2(i,f);
      inc(f,FieldInc);
    end;

    setfieldnodisp(FieldUp,true);
    setfieldnodisp(FieldDwn,true);
    setfieldenable(FieldUp, Offset>0);
    setfieldenable(FieldDwn,Offset+h<List.Count);
    setfieldpos(fieldpos);
  end;

  function ScrollFn(Dist: Integer): boolean;
  begin
    Offset := Offset+Dist;
    ScrollDone;
    result := true;
  end;

  procedure ScrollUp(var inhalt:string);
  begin
    MScroll(-1);
    SetFieldPos(Field1st);
  end;
                  
  procedure ScrollDown(var inhalt:string);
  begin
    MScroll(+1);
    SetFieldPos(FieldDwn-2);
  end;

  function IndexPos: integer;
  begin 
    result := (FieldPos - Field1st) div FieldInc + Offset; 
    if result >= List.Count then result := List.Count-1;
    if result < 0 then result := 0;
  end;

  
  function CurFieldStart: Integer;
  begin result := FieldPos - ((FieldPos - Field1st) mod FieldInc); end;

  function CheckType(var inhalt:string):boolean;
  var at: TAddressListType;
      mg: integer;
      ii: Integer;
  begin
    if inhalt=TypStr[atTo] then 
      at := atTo else
    if inhalt=TypStr[atCC] then 
      at := atCC 
    else
      at := atBCC;

    ii := IndexPos;
    mg := List[ii].Group;

    repeat  
      List[ii].AddressType := at;
      inc(ii);
    until (mg<0) or (ii>=List.Count) or (List[ii].Group<>mg);
          
    result := true;
  end;

  function CheckGroup(var inhalt:string):boolean;
  var G,ii,j: integer;
  begin
    ii := IndexPos;
    inhalt := Trim(inhalt);

//  Wrt(1,1,'<<FieldPos='+StrS(FieldPos)+', IndexPos='+StrS(ii)+
//    ', List.Count='+StrS(List.Count)+', List.GroupNames.Count='+StrS(List.GroupNames.count)+'>>');
    
    if inhalt='' then
    begin
      G := List[ii].Group;
      while (List[ii].Group=G) and (ii<List.Count) do
        if List[ii].Empty then
          List.Delete(ii) 
        else begin
          List[ii].Group := -1;
          Inc(ii); 
        end;
      ScrollDone;
    end else begin
      List.GroupNames[List[ii].Group] := inhalt;
    end;
    result := true;
  end;

  function CheckAddress(var inhalt:string):boolean;

    procedure ParseAddresses(const inhalt: string; list: TAddressList);
    var i:          Integer;    // current position
        j:          Integer;    // position of last address start
        Quote:      Boolean;    // within dquotes
        Angle:      Boolean;    // within angle brackets
        CommentCnt: Integer;    // comment count
        Group:      Integer;    // current group index
        SkipNext:   boolean;    // Skip next character

      procedure AddGroup;
      var s:string;
          d:DB;
      begin
        S := Trim(Copy(Inhalt,J,I-J));

      // Nothing => Ignore colon
        If S='' then
        begin
          J := I+1;
          exit;
        end;

      // Starts with '+' => Box Name
        If FirstChar(S)='+' then
        begin
          dbOpen(d,BoxenFile,1);
          try
            dbSeek(d,boiName,Uppercase(Trim(Mid(s,2))));
            if dbFound then             // Existing Box
              exit;                     // => Not a Group Name
          finally
            dbClose(d);
          end;
        end;

        Group := List.GroupNames.Add(RFCUnquotePhrase(s));
        J := I+1;
      end;

      procedure AddAddr;
      var s,t: string;
          r,n: string;
          p: integer;
          d: DB;
          bb : string;
          nu: boolean;
          a: TAddress;
      begin (* procedure AddAddr *)
        s := Trim(Copy(Inhalt,J,I-J));
        bb := '';
        nu := false;

        if FirstChar(s)='+' then
        begin
          P := CPos(':',S);
          if P>0 then
          begin
            dbOpen(d,BoxenFile,1);
            try
              bb := Trim(Copy(S,2,P-2));

              dbSeek(d,boiName,Uppercase(bb));
              if dbFound then begin
                S := Mid(S,P+1);
                nu := true;
              end else
                bb := '';
            finally
              dbClose(d);
            end;
          end;
        end;

        dbOpen(d,PseudoFile,1);
        try
          dbSeek(d,piKurzname,UpperCase(s));
          if dbFound then 
          begin
            s := dbReadStr(d,'Langname');
            bb := dbReadStr(d,'pollbox');
            nu := true;
          end;
        finally
          dbClose(d);
        end;

        t := Trim(RFCRemoveComments(s));

        if(FirstChar(t)='[')and(LastChar(t)=']')then
          a := TVerteiler.Create(Copy(t,2,Length(t)-2))
        else if CPos('@',t)>0 then
          a := TEMailAddress.Create(s)
        else
          a := TNewsgroupAddress.Create(t);

        with List.AddNew do
        begin
          Address := a;
          BoxName := bb;
          NewUser := nu;
        end;

      end; (* procedure AddAddr *)
        
    begin
      Quote := false;
      Angle := false;
      Group := -1;
      SkipNext := false;
      J := 1;
    
      for i:=1 to length(inhalt) do
        if SkipNext then
          SkipNext:=false else
        case inhalt[i] of
          '\':  SkipNext := true;
          '"':  Quote := not Quote;
          '<':  if not Quote then Angle:=true;
          '>':  if not Quote then Angle:=false;
          '(':  If not Quote then Inc(CommentCnt);
          ')':  If(not Quote)then If CommentCnt>0 then Dec(CommentCnt);
          ':':  If(not Quote)and(not Angle)then AddGroup;
          ',':  If(not Quote)and(not Angle)then AddAddr;
          ';':  If(not Quote)and(not Angle)then begin AddAddr; Group:=-1; end;
        end;
        
      AddAddr;
    end;  

 {function CheckAddress(var inhalt:string):boolean;}
  var ii: integer;
     i,j: integer;
      nl: TAddressList;
      lg: integer;
      lp: integer;
      lt: TAddressListType;      
      ch: boolean;
  begin
    ch := false;
    ii := IndexPos;

    Inhalt := Trim(Inhalt);

//  Wrt(1,1,'<<FieldPos='+StrS(FieldPos)+', IndexPos='+StrS(ii)+
//    ', List.Count='+StrS(List.Count)+', List.GroupNames.Count='+StrS(List.GroupNames.count)+'>>');
    
    while LastChar(inhalt)=' ' do SetLength(inhalt,Length(inhalt)-1);

  //-- Kein Inhalt => Adresse löschen ----------------------------------
    if inhalt='' then
      if (ii<List.Count-1) and ((List[ii].Group<0) or
        (List[ii].Group=List[ii+1].Group)) then 
      begin
        List.Delete(ii);
        ScrollDone;
        result := false;
        exit;
      end else 
      begin
        result := true;
        exit;
      end;

  //-- Liste erzeugen --------------------------------------------------
    nl := TAddressList.Create;
  try
    ParseAddresses(inhalt,nl);
    CheckEmpfaengerList(nl,true,false,nil);

    lg := List[ii].Group;
    lt := List[ii].AddressType;

  // -- Neue Adresse(n) einfügen ---------------------------------------

    if nl.Count<>1 then 
    begin
      List.Delete(ii);
      if nl.Count>0 then
        List.InsertList(ii,nl);
      ch := true;
    end else
      List[ii].Assign(nl[0]);

  // -- Gruppe/Addresstyp wieder setzen --------------------------------

    j := ii;
    lp := j;

    while (j<List.Count) and ((j<nl.Count+ii) or (List[j].Group>=0)) do
    begin
    // -- Gruppe wieder setzen, ggf. nach vorne schieben ---------------
      if (j<nl.Count+ii) then begin
        if (lg>=0) and (j<nl.Count+ii) then
          if List[j].Group<0 then 
            List[j].Group := Lg;
        List[j].AddressType := lt;
      end;
          
      if List[j].Group=Lg then 
      begin
        if lp<j then begin 
          List.Move(j,lp); 
          ch := true;
        end;
        inc(lp);
      end;

    // -- Überflüssige Leerfelder löschen ------------------------------
      if (List[j].Empty) then
      begin
        if ((List[j].Group>=0) and (j<List.Count-1) and (List[j].Group=List[j+1].Group)) 
         or((List[j].Group< 0) and (j<List.Count-1)) then 
        begin
          List.Delete(j);
          dec(j);
          ch := true;
        end;

    // -- Fehlende Leerfelder einfügen ---------------------------------
      end else // List[j].Address<>''
      begin
        if (List[j].Group>=0) and ((j>=List.Count-1) or (List[j].Group<>List[j+1].Group)) then 
        begin
          List.InsertNew(j+1).Group := List[j].Group;
          inc(j);
          ch := true;
        end;
      end;
        
      inc(j);
    end;
(*
    // TODO: Sortieren und Leerfelder einfügen
  
    for j:= 0 to nl.Count-1 do
    while j < i do
    begin
      if lg>=0 then 
        if List[ii+j].Group<0 then 
          List[ii+j].Group := Lg
        else 
          lg := -1; // Letzte erreicht (war ja sortiert); nicht mehr prüfen
      if (List[ii+j].PM) then
        List[ii+j].AddressType := Lt
      else
        List[ii+j].AddressType := atNewsgroup;
    end;
*)
    if (List.Count<=0) or (not List[List.Count-1].Empty)
                       or (List[List.Count-1].Group>=0) then 
    begin
      with List.AddNew do begin
        for j := List.Count-2 downto 0 do 
          if List[j].PM then begin
            AddressType := List[j].AddressType;
            break;
          end;
      end;
      ch := true;
    end;

    inhalt := List[ii].DisplayString;

    if (nl.Count<>1) or ch then 
      ScrollDone
    else
      ScrollDone2(ii-Offset,(ii-Offset)*FieldInc+Field1st);
      
  finally
    nl.Free;
  end;  
    result := true;
  end;
  
procedure EditEmpfaengerList(
  const DialogueTitle: String;          // Dialog-Titel

  EditRecipients:   Boolean;            // Empfänger bearbeiten
  EmpfList:         TAddressList;       // Primäre Empfänger/Newsgroups

  SavedList:        TAddressList;       // Übrige Emfänger aus der 
                                        // ursprünglichen Nachricht
                                        
  EditSubject:      Boolean;            // Betreff bearbeiten
  ShowSubject:      Boolean;            // Betreff überhaupt anzeigen

  var Subject:      String;             // Betreff

  PMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für PMs
  AMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für AMs

  sData:            TSendUUData         // may be nil
);

  procedure ComposeEditableList;
  begin
    List.Assign(EmpfList);
    List.AddNew.Group := -1;
  end;

  procedure UnComposeEditableList;
  var ii: integer;
      it: TAddressListType;
      LastGroup: array [TAddressListType] of Integer;
  
  begin
    EmpfList.Clear;
    EmpfList.GroupNames.Clear;

    for it := Low(TAddressListType) to High(TAddressListType) do
      LastGroup[it] := -1;
    
    for ii := 0 to List.Count-1 do
      with EmpfList.AddNew do 
      begin
        Assign(List[ii]);        
        if List[ii].Group>=0 then
          if List[ii].Group=LastGroup[List[ii].AddressType] then
            Group := EmpfList.GroupNames.Count-1
          else
            Group := EmpfList.GroupNames.Add(List.GroupNames[List[ii].Group]);
        LastGroup[List[ii].AddressType] := List[ii].Group;
      end;
  end;  

begin
  TypStr[atNewsgroup] := GetRes2(2203,1);
  TypStr[atTo       ] := GetRes2(2203,2);
  TypStr[atCC       ] := GetRes2(2203,3);
  TypStr[atBCC      ] := GetRes2(2203,4);
  
  w1 := Max(Max(Length(TypStr[atNewsgroup]),Length(TypStr[atTo ])),
            Max(Length(TypStr[atCC       ]),Length(TypStr[atBCC])));  

  Offset := 0;

  List := TAddressList.Create;
  try //..except
  try //..finally
    ComposeEditableList;
    
    max_h := 20;
    if showsubject then dec(max_h,2);

    if showsubject or true then
      new_h := Min(Max(1,List.Count),max_h)
    else 
      new_h := max_h;
  
    repeat
      h := new_h;
      done := true;
      
      diabox(76,h+iif(ShowSubject,4,2),DialogueTitle,x,y);    { 'Kopien an:' }
      inc(x); inc(y);
      openmask(x,x+73,y,y+h+iif(ShowSubject,2,0),false);
      
      maskdontclear;
      MasksetScrollFunc(ScrollFn);

      Maddstring(2,1,'',Dummy1,0,0,''); FieldUp := FieldPos;
      MSet0Proc(ScrollUp);

      for i:= 0 to h-1 do
      begin
        if i=0 then Field1st := fieldpos + 1;

        MaddText  (2,      1+i,'',col.coldiahigh); AddrText[2,i] := MTextPos;
        Maddstring(2,      1+i,'',AddrCont[0,i],w1,      MAXINT,'');
        MappSel   (true,TypStr[atTo]+'ù'+TypStr[atCC]+'ù'+TypStr[atBCC]);
        MSetVFunc(CheckType);
        
        Maddstring(2+w1+4, 1+i,'',AddrCont[1,i],10,      MAXINT,'');
        MSetVFunc(CheckGroup);
        MaddText  (2+w1+16,1+i,'',col.coldiahigh); AddrText[0,i] := MTextPos;
        
        Maddstring(2+w1+18,1+i,'',AddrCont[2,i],74-w1-22,MAXINT,'');
        MSetVFunc(CheckAddress);

        Maddstring(2+w1+4, 1+i,'',AddrCont[2,i],74-w1-8, MAXINT,'');
        MSetVFunc(CheckAddress);
                
        MaddText  (2+72,1+i,'',col.coldiahigh);    AddrText[1,i] := MTextPos;

        if i=0 then FieldInc := fieldpos + 1 - Field1st;        
      end;

      Maddstring(2+70,1+h-1,'',Dummy2,0,0,''); FieldDwn := FieldPos;
      MSet0Proc(ScrollDown);

      if ShowSubject then
      begin
        MaddText  (2,h+2,GetRes2(2203,6),col.coldiahigh);
        if EditSubject then
          MaddString(2+w1+4,h+2,'',Subject,74-w1-8,MAXINT,'')
        else
          MaddText  (2,h+2,LeftStr(Subject,74-w1-6),col.coldiahigh);
      end;

      ScrollDone;
      
      readmask(brk);
      closemask;
      closebox;
    until done;

    if not brk then UnComposeEditableList;
    
  finally
    List.Free;
  end;
  except
    on e:Exception do fehler(e.message);
  end;
end;

// -- Adressen überprüfen ----------------------------------------------

procedure CheckEmpfaengerList(
  List:             TAddressList;
  Prompt:           Boolean;
  AutoAdd:          Boolean;
  sData:            TSendUUData         // may be nil
);

var Index: integer;

  procedure Verteiler(v: TVerteiler);
  var G:    Integer;            // Gruppennummer
      Box:  String;             // Boxname
      T:    Text;               // Datei zum Einlesen (VERTEIL.DAT)
      S:    String;
      UName:String;
      
  begin 
    UName := UpperCase(List[Index].ZCAddress);
  
  // -- Box suchen -----------------------------------------------------
    Box:=''; dbSeek(ubase,uiName,UpperCase(List[Index].XPAddress));    
    if dbFound then 
      if dbReadInt(ubase,'userflags') and 4<>0 then
        Box := dbReadStrN(ubase,ub_pollbox);

  // -- Neue Gruppe anlegen --------------------------------------------
    G := List.GroupNames.Add(v.VerteilerName);
    List.Delete(Index);

  // -- Adressen einlesen ----------------------------------------------
    Assign(t,CCfile);
    Reset(t);
    if IOResult<>0 then exit;

    repeat 
      Readln(t,s);
    until eof(t) or (UpperCase(s)=UName);

    if not eof(t) then
      repeat
        readln(t,s); s:=Trim(s);
        if (s<>'') and not is_vname(s) then
        with List.InsertNew(Index) do begin
          ZCAddress := s;
          BoxName := Box;
          Group := G;
        end;
      until eof(t) or is_vname(s);
    close(t);
    if IOResult<>0 then {noop};
  end;

  procedure PM(email: TEmailAddress);
  var UFlags:   Integer;        // User flags
      UName:    string;         // Upper case name
      Size:     Longint;       
      D:        DB;
  label
      NoDB, BoxAgain;

    function PMEdit: boolean;
    var user,adresse,komm,pollbox: string;
        halten: Integer16;
        adr,flags,b: byte;        
        brk: boolean;
    begin
      user    := EMail.XPAddress;
      adresse := EMail.XPAddress;
      komm    := '';
      pollbox := List[Index].BoxName; if pollbox='' then pollbox := DefaultBox;
      halten  := StdUHaltezeit;
      Adr     := NeuUserGruppe;
      Flags   := 1 + iif(newuseribm,0,8);

      EditUser('<TODO>',user,adresse,komm,pollbox,halten,adr,flags,true,brk);

      if brk then
        result := false
      else
      begin
        List[Index].ZCAddress := Adresse;

       dbSeek(ubase,uiName,UpperCase(user));
       if dbFound then
         rfehler(2703)    { 'Dieser User ist bereits vorhanden!' }
       else 
       begin
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
       end;

        result := true;
      end;
    end;

  begin
  
    UName := UpperCase(email.XPAddress);

    dbSeek(ubase,uiName,UName);
    if dbFound then
  // -- Empfänger bekannt ----------------------------------------------
    begin
      UFlags := dbReadInt(ubase,'userflags');
//    if(UFlags and 16)<>0 then flEB:=true;

      if (dbXsize(ubase,'adresse')<>0) then 
      begin
        List[Index].ZCAddress := dbReadXStr(ubase,'adresse',size);
        exit;                                // Gleiche Addresse nochmal
      end;
      
//    dbReadN(ubase,ub_codierer,cancode);
//    if (not (cancode in [8,9])) and (dbXsize(ubase,'passwort')=0) then
//      cancode:=0
//    else begin
//      if cancode<>0 then
//        if dbReadInt(ubase,'userflags') and 2<>0 then
//          docode:=cancode;
//      passwd:= dbReadXStr(ubase,'passwort',si0);
//    end;

      List[Index].BoxName := dbReadStrN(ubase,ub_pollbox);

    end else
  // -- Empfänger unbekannt --------------------------------------------
    begin
    // -- Benutzer fragen ----------------------------------------------
      if prompt and (not List[Index].NewUser) then
      begin
        if not PMEdit then
          List.Delete(Index);
        exit;                                // Gleicher Eintrag nochmal
      end else

    // -- Einfach anlegen ----------------------------------------------
      if autoadd then    
      begin
        if List[Index].BoxName='' then
          List[Index].BoxName := DefaultBox;
        AddNewUser(List[Index].XPAddress,List[Index].BoxName);
      end else

    // -- Nicht in der DB ----------------------------------------------
      begin
        List[Index].NewUser := true;
        UFlags  := iif(NewUserIBM,0,8); { NewUserIBM beruecksichtigen }
        List[Index].NewUser := true;
        Goto NoDB;
      end;
    end;

    List[Index].NewUser := false;

  NoDB:
  
  BoxAgain:
    if List[Index].BoxName<>'' then 
    begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,Uppercase(List[Index].BoxName));
      if not dbFound then
      begin
        dbClose(d);
        rfehler1(607,List[Index].BoxName);  { 'Unbekannte Serverbox: %s  -  Bitte ueberpruefen!' }
        if Uppercase(List[Index].BoxName)<>UpperCase(DefaultBox) then
        begin
          List[Index].BoxName := DefaultBox;
          goto BoxAgain;
        end else
          List[Index].BoxName := '';
      end else 
      begin
        List[Index].BoxName := dbReadStr(d,'boxname');       { Schreibweise korrigieren }
        List[Index].Netztyp := dbReadInt(d,'netztyp');
        if assigned(sData) then sData.Boxen.Add(Chr(List[Index].Netztyp)+List[Index].Boxname);
      end;
      dbClose(d);
    end else
    begin
      List[Index].BoxName := DefaultBox;
      goto BoxAgain;
    end;

    // -- Für alle Empfänger: Zeichensätze -----------------------------

    if (UFlags and 8)<>0 then
      List[Index].Charsets.CommaText := ''
    else
      if List[Index].Netztyp in netsRFC then
        List[Index].Charsets.CommaText := 'ISO-8859-1,ISO-8859-15,UTF-8' else 
      if List[Index].Netztyp in [nt_ZConnect] then
        List[Index].Charsets.CommaText := iifs(ZC_ISO,'ISO-8859-1','IBM437') else
      if List[Index].Netztyp in netsFTN then
        List[Index].Charsets.CommaText := 'IBM437' else
      if List[Index].Netztyp in [nt_Maus] then
        List[Index].Charsets.CommaText := 'IBM437';

    if List[Index].AddressType in [atNewsgroup] then
      List[Index].AddressType := atTo;

    if assigned(sData) then
    begin
      sData.MergeCharsets(List[Index].Netztyp,List[Index].Charsets);
      sData.MergeMsgType(List[Index].Netztyp,true);
    end;   
        
    Inc(Index);
  end;

  procedure AM(Group:TNewsgroupAddress);
  var s: string;
      f: byte;
      g: Longint;
      d: DB;

      u: byte;
      
  label
      BoxAgain,
      GroupAgain;
  begin
    dbSeek(bbase,bb_brettname,Uppercase(Group.XPAddress));

  // -- Brett bekannt ------------------------------------------------
    if dbFound then
    begin
      f := dbReadByteN(bbase,bb_flags);
      s := dbReadStrN(bbase,bb_adresse);

      List[Index].BoxName:=dbReadStrN(bbase,bb_pollbox);

    // -- Bei Schreibsperre andere Adresse einsetzen -----------------
      if (s<>'') and ((f and 8)<>0) then begin
        List[Index].ZCAddress := s;
        exit;                                                 // nomml
      end else

    // -- Schreibsperre ohne Vertreter -------------------------------
      if (f and 8)<>0 then begin
        fehler(Format(GetRes2(611,100),[String(List[Index].DisplayString)]));        
        List.Delete(Index);
        exit;
      end;

      G := dbReadIntN(bbase,bb_gruppe);

      List[Index].NewUser := false;
    end else

  // -- Brett unbekannt ------------------------------------------------
    begin
      if List[Index].BoxName='' then
        List[Index].BoxName := DefaultBox;
      G := NetzGruppe;
      List[Index].NewUser := true;
    end;

  // -- Box suchen -----------------------------------------------------
  
  BoxAgain:
    if List[Index].BoxName<>'' then 
    begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,Uppercase(List[Index].BoxName));
      if not dbFound then
      begin
        dbClose(d);
        rfehler1(607,List[Index].BoxName);  { 'Unbekannte Serverbox: %s  -  Bitte ueberpruefen!' }
        if Uppercase(List[Index].BoxName)<>UpperCase(DefaultBox) then
        begin
          List[Index].BoxName := DefaultBox;
          goto BoxAgain;
        end else
          List[Index].BoxName := '';
      end else 
      begin
        List[Index].BoxName := dbReadStr(d,'boxname');       { Schreibweise korrigieren }
        List[Index].Netztyp := dbReadInt(d,'netztyp');

        if assigned(sData) then sData.Boxen.Add(Chr(List[Index].Netztyp)+List[Index].Boxname);
      end;
      dbClose(d);
    end;

  // -- Gruppe suchen und Daten setzen ---------------------------------
  
  GroupAgain:
  
    dbOpen(d,GruppenFile,1);
    dbSeek(d,giIntNr,dbLongStr(G));

    if dbFound then
    begin
      u := dbReadByte(d,'flags');
      
    end else
    begin
      if G<>Netzgruppe then begin
        G:= Netzgruppe;
        Goto GroupAgain;
      end;
      u := 0;      
    end;

    case u of 
      1: {ASCII}
        List[Index].Charsets.Clear;
      else {IBM, ISO}
        if List[Index].Netztyp in netsRFC then
          List[Index].Charsets.CommaText := 'ISO-8859-1,ISO-8859-15,UTF-8' else 
        if List[Index].Netztyp in [nt_ZConnect] then
          List[Index].Charsets.CommaText := iifs(u=0,'IBM437','ISO-8859-1') else
        if List[Index].Netztyp in netsFTN then
          List[Index].Charsets.CommaText := 'IBM437' else
        if List[Index].Netztyp in [nt_Maus] then
          List[Index].Charsets.CommaText := 'IBM437';
    end;

    if List[Index].AddressType in [atTo,atCC,atBCC] then
      List[Index].AddressType := atNewsgroup;    

    if assigned(sData) then
    begin
      sData.MergeCharsets(List[Index].Netztyp,List[Index].Charsets);
      sData.MergeMsgType(List[Index].Netztyp,false);
    end;   

    Inc(Index);
  end;

begin
  Index := 0; 

  while(Index<List.Count) do
    if List[Index].Empty then
      List.Delete(Index)
    else  
    if List[Index].Verteiler then
      Verteiler(TVerteiler(List[Index].Address)) 
    else
    if List[Index].PM then
      PM(TEmailAddress(List[Index].Address))
    else 
      AM(TNewsgroupAddress(List[Index].Address));
end;

//
// $Log$
// Revision 1.2  2002/04/17 19:35:03  mk
// - added xpdefine.inc
//
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//

end.

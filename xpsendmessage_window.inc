// $Id$
// 
// OpenXP/32: TSendUUData - Address Editing and Send Window
//    
// (C) Copyright 1991-2001 Peter Mandrella
// (C) Copyright 2001-2002 by OpenXP/32 team <http://www.openxp.de>
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


{-- Global functions (necessary because of maske.pas/inc...) ----------}

var done: boolean;

procedure CheckEmpfaengerList(
  List:     TAddressList;
  Prompt:   Boolean;
  AutoAdd:  Boolean;
  sData:    TSendUUData
); forward;

var
  TypStr: array [TAddressListType] of String;

  List: TAddressList;
  Server: string;

  x,y,w1: integer;
  max_h,new_h,h: 1..20;
  brk: boolean;

  FieldUp:  integer;
  FieldDwn: integer;
  Field1st: integer;
  FieldInc: integer;

  FieldServer: Integer;
  FieldEncryption: Integer;
  FieldSigning: Integer;

  AddrCont: array[0..2,0..19] of String;
  AddrText:  array[0..1,0..19] of Pointer;
  Dummy1,Dummy2: string;
  Offset: Integer;

  gsData: TSendUUData;
  
procedure SetServerField(Field: integer);
var sx: boolean;
    ii: integer;
begin
  SX := false;

  for ii := 0 to List.Count-1 do
    if Server = '' then
      Server := List[ii].BoxName
    else
    if (not sx) and
       (not List[ii].Empty) and
       (UpperCase(Server) <> UpperCase(List[ii].BoxName)) then
    begin
      Server := GetRes2(610,94);
      SX := true;
    end;

  if Field >= 0 then
    SetField(Field,Server);
end;

type set_of_TSendEncryptionMethod = set of TSendEncryptionMethod;
type set_of_TSendSigningMethod    = set of TSendSigningMethod;

procedure CorrectEncryptionMethod(
  List: TAddressList;
  GSData: TSendUUData;
  var Methods: set_of_TSendEncryptionMethod);
var
  i: integer;
begin
  Methods := [];

  for i :=  0 to List.Count -1 do
    if not List[i].Empty then
      if (not List[i].PM) or
         ( List[i].EncodingMethod in [0] ) or
         ((List[i].EncodingMethod in [1,2,3..5]) and (Trim(List[i].EncodingPassword)= '')) then
      begin
        Methods := [];
        break;
      end else
      if List[i].EncodingMethod in [1,2,3..5,8,9] then
        Methods := Methods + [TSendEncryptionMethod(List[i].EncodingMethod)];

  // For multipart messages, switch from leagacy PGP to PGP/MIME
  if (GSData.IsMIMEMultipart) and (Methods * [secMIME_or_PGPOld,secPGPOld] <> []) then
    Methods := (Methods + [secMIME]) - [secMIME_or_PGPOld,secPGPOld];

  // If there's both leagacy PGP and MIME, activate "combination" switch
  if (secMIME in Methods) and (secPGPOld in Methods) then
    Methods := (Methods + [secMIME_or_PGPOld]) - [secMIME,secPGPOld];

  // We don't support combining symmetric ZC and PGP encryption
  if (Methods * [secQPC,secDESXP,secPMC1..secPMC2] <> []) and
     (Methods * [secMIME,secPGPOld,secMIME_or_PGPOld] <> []) then
    Methods := [];

  Methods := Methods + [secNone];

  if not(GSData.EncryptionMethod in Methods) then
    if GSData.EncryptionMethod in [secMIME,secMIME_or_PGPOld,secPGPOld] then
      if secMIME in Methods then
        GSData.EncryptionMethod := secMIME
      else
      if secPGPOld in Methods then
        GSData.EncryptionMethod := secPGPOld
      else
      if secMIME_or_PGPOld in Methods then
        GSData.EncryptionMethod := secMIME_or_PGPOld;

  if not(GSData.EncryptionMethod in Methods) then
    GSData.EncryptionMethod := secNone;
end;

procedure SetEncryptionField;
var Methods : set_of_TSendEncryptionMethod;
    s: string;
    i: integer;
    j: TSendEncryptionMethod;
begin
  if FieldEncryption < 0 then exit;
  CorrectEncryptionMethod(List,GSData,Methods);

  if Methods = [secNone] then
  begin
    mclearsel(FieldEncryption);
    SetField(FieldEncryption,GetRes2(610,129));
  end else
  begin
    for j := Low(j) to High(j) do
    begin
      if j in Methods then
      begin
        if s<>'' then s := s + #$f9;
        s := s + GetRes2(610, Ord(j)+130);
        Inc(i);
      end;
    end;

    if GSData.IsMIMEMultipart and
      (GSData.EncryptionMethod in [secMime,secPGPOld,secMime_or_PGPOld]) then
      SetField(FieldEncryption,GetRes2(610,130+Ord(secMime)))
    else
      SetField(FieldEncryption,GetRes2(610,130+Ord(GSData.EncryptionMethod)));

    mclearsel(FieldEncryption);
    mappendsel(FieldEncryption, true, s);
  end;
end;

procedure CorrectSigningMethod(
  List: TAddressList;
  GSData: TSendUUData;
  var Methods: set_of_TSendSigningMethod);
var
  i      : integer;
  NoMIME : boolean;  
begin
  Methods := [];
  NoMIME := false;
  
  if GSData.EncryptionMethod in [secMIME, secPGPOld, secMIME_or_PGPOld, secNone] then
  begin
    for i :=  0 to List.Count -1 do
      if not List[i].Empty then
        if List[i].PM and (List[i].EncodingMethod in [8,9]) then
          Methods := Methods + [TSendSigningMethod(List[i].EncodingMethod)]
        else
        if (List[i].Netztyp>0) and not ntMIME(List[i].Netztyp) then
          NoMime := true;

    // For multipart messages, switch from leagacy PGP to PGP/MIME
    if (GSData.IsMIMEMultipart) and (Methods * [sigMIME_or_PGPOld,sigPGPOld] <> []) then
      Methods := (Methods + [sigMIME]) - [sigMIME_or_PGPOld,sigPGPOld];

    // If there's both leagacy PGP and MIME, activate "combination" switch
    if (sigMIME in Methods) and (sigPGPOld in Methods) then
      Methods := (Methods + [sigMIME_or_PGPOld]) - [sigMIME,sigPGPOld];

    // If there are no methods, always allow PGP/MIME and PGPOld
    if Methods = [] then
      Methods := Methods + [sigMIME,sigPGPOld];

    // Remove MIME if we have non-MIME eNetz types (but keep "combination")
    if NoMime then
      Methods := Methods - [sigMIME];
  end;

  Methods := Methods + [sigNone];
  
  if not(GSData.SigningMethod in Methods) then
    if GSData.SigningMethod in [sigMIME,sigMIME_or_PGPOld,sigPGPOld] then
      if sigMIME in Methods then
        GSData.SigningMethod := sigMIME
      else
      if sigPGPOld in Methods then
        GSData.SigningMethod := sigPGPOld
      else
      if sigMIME_or_PGPOld in Methods then
        GSData.SigningMethod := sigMIME_or_PGPOld;

  if not(GSData.SigningMethod in Methods) then
    GSData.SigningMethod := sigNone;
end;

procedure SetSigningField;
var Methods : set_of_TSendSigningMethod;
    s: string;
    i: integer;
    j: TSendSigningMethod;
begin
  if FieldSigning < 0 then exit;
  CorrectSigningMethod(List,GSData,Methods);

  if Methods = [sigNone] then
  begin
    mclearsel(FieldSigning);
    SetField(FieldSigning,GetRes2(610,129));
  end else
  begin
    for j := Low(j) to High(j) do
    begin
      if j in Methods then
      begin
        if s<>'' then s := s + #$f9;
        s := s + GetRes2(610, Ord(j)+130);
        Inc(i);
      end;
    end;

    if GSData.IsMIMEMultipart and
      (GSData.SigningMethod in [sigMime,sigPGPOld,sigMime_or_PGPOld]) then
      SetField(FieldSigning,GetRes2(610,130+Ord(sigMime)))
    else
      SetField(FieldSigning,GetRes2(610,130+Ord(GSData.SigningMethod)));

    mclearsel(FieldSigning);
    mappendsel(FieldSigning, true, s);
  end;
end;

procedure TSendUUData.CorrectEncryptionMethod;
var
  dummy_e: set_of_TSendEncryptionMethod;
  dummy_s: set_of_TSendSigningMethod;
begin
  XPSendMessage.CorrectEncryptionMethod(EmpfList,self,dummy_e);
  XPSendMessage.CorrectSigningMethod(EmpfList,self,dummy_s);
end;

  procedure ScrollDone2(i,f:integer);
  var show_type : boolean;
      show_grp  : boolean;
      show_grp_start : boolean;
//    show_grp_stop  : boolean;
  begin
    if Offset+i<List.Count then
    begin
      show_grp       := (List[i+Offset].Group>=0);
      show_grp_start := Show_Grp and ((i+Offset<=0)or(List[i+Offset].Group<>List[i+Offset-1].Group));
//    show_grp_stop  := Show_Grp and ((i+Offset>=List.Count-1)or(List[i+Offset].Group<>List[i+Offset+1].Group));
      show_type      := show_grp_start or not show_grp;

      setfield(F+0,iifs(List[i+Offset].AddressType=atUnused,TypStr[atTo],TypStr[List[i+Offset].AddressType]));
      setfieldenable(F+0,    show_type);
      setfieldnodisp(F+0,not show_type);

//    settexttext(AddrText[1,i],iifs(not(List[i+Offset].PM or List[i+Offset].Empty or show_grp),TypStr[atNewsgroup],''));

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

//    settexttext(AddrText[2,i],iifs(show_grp_stop,';',' '));
      {$IFDEF ANALYSE} assert(show_grp_stop);{$ENDIF} //prevent hint "never used"
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
//    settexttext(AddrText[2,i],'');
      settexttext(AddrText[1,i],'');
    end;
  end;

  procedure ScrollDone;
  var i: integer;
      f: integer;

  begin
    if (h<max_h) and (h<List.Count) then
    begin
      new_h := Min(max_h,Max(List.Count,h+3));
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

(*  
  function CurFieldStart: Integer;
  begin 
    result := FieldPos - ((FieldPos - Field1st) mod FieldInc); 
  end;
*)

  procedure ListerOnSelect(Lister: TLister;var cr:customrec;MaxLen: Integer);
  var x1,x2,y1,y2: Integer;
  begin
    x1 := cr.x;   x2 := cr.x+4+MaxLen;
    y1 := cr.y+1; y2 := cr.y+2+Lister.Lines.Count;

    if y2 > ScreenLines then begin y1 := Max(1,y1-(y2-ScreenLines)); y2:= ScreenLines; end;
    if x2 > ScreenWidth then begin x1 := Max(1,x1-(x2-ScreenWidth)); x2:= ScreenWidth; end;

    Lister.SetSize(x1+1,x2-1,y1+1,y2-1);
    openboxat(x2-x1+1,y2-y1+1,'',x1,y1,cr.acol.ColSelBox,cr.acol.ColSelBox);
    try
      cr.brk := Lister.Show;
    finally
      closebox;
    end;
  end;


  procedure SelectType(var cr:customrec);
  var ii: Integer;
      ls: TLister;
  begin
    ii := IndexPos;
    ls := TLister.CreateWithOptions(0,0,0,0,0,'/NS/SB/NLR/DM/');
    try
      if List[ii].PM or List[ii].Empty then
      begin
        ls.AddLine(' '+TypStr[atTo          ]);
        ls.AddLine(' '+TypStr[atCC          ]);
        ls.AddLine(' '+TypStr[atBCC         ]);
      end;

      if List[ii].PM or List[ii].Empty then
      begin
        ls.AddLine(' '+TypStr[atReplyTo     ]);
      end;
      
      if (not List[ii].PM) or List[ii].Empty then
      begin
        ls.AddLine(' '+TypStr[atNewsgroup   ]);
      end;
        ls.AddLine(' '+TypStr[atFollowupTo  ]);
        
      ls.SetSize(cr.x+1,cr.x+2+w1,cr.y+2,cr.y+1+ls.Lines.Count);
      openboxat(w1+4,ls.Lines.Count+2,'',cr.x,cr.y+1,
          cr.acol.ColSelBox,cr.acol.ColSelBox);
      try
        cr.brk := Ls.Show;
      finally
        closebox;
      end;
      cr.s   := Trim(Ls.GetSelection);
    finally
    ls.Free;
    end;
  end;

  function CheckType(var inhalt:string):boolean;
  var at,a2: TAddressListType;
      mg: integer;
      ii: Integer;
  begin
    at := atBCC;
    for a2 := Low(TAddressListType) to High(TAddressListType) do
      if Inhalt=TypStr[a2] then begin
        at := a2;
        break;
      end;

    ii := IndexPos;
    mg := List[ii].Group;

    repeat  
      List[ii].AddressType := at;
      inc(ii);
    until (mg<0) or (ii>=List.Count) or (List[ii].Group<>mg);

    result := true;
  end;

  function CheckGroup(var inhalt:string):boolean;
  var G,ii: integer;
  begin
    ii := IndexPos;
    inhalt := Trim(inhalt);

    if inhalt='' then
    begin
      G := List[ii].Group;
      while (List[ii].Group=G) and (ii<List.Count-1) do
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

  function SelectAddress_OnTestMark(const s: string; block: boolean): boolean;
  begin
    result := FirstChar(s)<>'=';
  end;

  function SelectAddress_OnTestSelbar(const s: string; down: boolean): boolean;
  begin
    result := (FirstChar(s)<>'=') or ((Length(s)>=2)and(s[2]<>'-'));
  end;

  procedure SelectAddress_OnDisplay(x, y: xpWord; var s: string);
  var i: Integer;
  begin
    i := CPos('=',s);

    if (i=1) and (Length(s)>=2) then
    begin
      if s[2] = '-' then
      begin
        attrtxt(Col.ColSelRahmen);
        s[1]:='-'; s[2]:=' ';          
        for i := Length(s) downto 2 do
          if (i=2)or(s[i-1]=' ') then s[i]:='-' else break;
        FWrt(x,y,s);
      end else
      begin
        i:=CPos('^',s);
        if i>0 then begin
          FWrt(x+i-1,y,Mid(s,i+2)+'  ');
          FWrt(x,y,' '+Copy(s,3,i-3));
          if (LastAttr = Col.ColSelBar) then attrtxt(Col.ColSelBarHigh)
          else                               attrtxt(Col.ColSelHigh);
          FWrt(x+i-2,y,s[i+1]);
        end else
          FWrt(x,y,' '+Mid(s,3)+' ');
      end;
    end else
    begin
      FWrt(x,y,' '+LeftStr(s,i-1)+Mid(s,i+1));
    end;
  end;

  var SelectAddress_KeyPressed: integer;

  procedure SelectAddress_OnKeyPressed(LSelf: TLister; var t: taste);
  begin
    if (t='b')or(t=keyaltb) then begin
      SelectAddress_KeyPressed := 2;
      t := keycr;
    end else
    if (t='u')or(t=keyaltu) then begin
      SelectAddress_KeyPressed := 1;
      t := keycr;
    end;
  end;

  procedure CheckAddressSetAddressList(nl: TAddressList; AutoAdd: boolean); forward;

  procedure SelectAddress(var cr:customrec);
  var ii,i,j: Integer;
      Lister: TLister;
      DontAdd: TStringList;
      NewList: TAddressList;
      TypeWidth: Integer;
      AddrWidth: Integer;
  var x1,x2,y1,y2: Integer;
      res: Integer;
      s: string;

    procedure rm(const AddrSpec: String);
    begin
      DontAdd.Add(UpperCase(AddrSpec));
    end;

    procedure add(const AddrSpec, DisplayString: String; AddressType: String); overload;
    var dummy: Integer;
    begin
      if DontAdd.Find(UpperCase(AddrSpec),dummy) then exit;
      if res>0 then begin Lister.AddLine('=-'+GetRes2(610,res)); res := -1; end;
      AddrWidth := Max(Length(DisplayString),AddrWidth);
      Lister.AddLine(FormS(AddressType,TypeWidth)+'='+DisplayString);
      rm(AddrSpec);
    end;

    procedure addz(const zc_header: string; AddressTypeMail, AddressTypeNews: String); overload;
    var AddressType: String;
    begin
      if zc_header='' then exit;
      if (AddressTypeMail <> AddressTypeNews) and (CPos('@',zc_header)>0) then
        AddressType := AddressTypeMail
      else
        AddressType := AddressTypeNews;
      add(RFCRemoveComments(zc_header),zc_header,AddressType);
    end;

    procedure addz(const zc_header: TStrings; AddressTypeMail, AddressTypeNews: String); overload;
    var i: Integer;
    begin
      for i := 0 to zc_header.Count -1 do
        addz(zc_header[i],AddressTypeMail, AddressTypeNews);
    end;

    procedure addu(const uucp_header: String; const AddressType: String);
    var List: TAddressList;
        i: Integer;
    begin
      List := TAddressList.Create;
      try
        RFCReadAddressList(uucp_header, List, nil);
        for i := 0 to List.Count -1 do
          add(List[i].AddrSpec,List[i].DisplayString,AddressType);
      finally
        List.Free;
      end;
    end;

  begin
    ii := IndexPos;
    TypeWidth := Length(GetRes2(610,83))+1;
    AddrWidth := 0;

    Lister := TLister.CreateWithOptions(0,0,0,0,0,'/NS/SB/NLR/DM/M/');
    try
      SelectAddress_KeyPressed := 0;
      Lister.OnDisplayLine := SelectAddress_OnDisplay;
      Lister.OnTestMark := SelectAddress_OnTestMark;
      Lister.OnTestSelect := SelectAddress_OnTestSelbar;
      Lister.OnKeyPressed := SelectAddress_OnKeyPressed;      
      
      DontAdd := TStringList.Create;
      try
        DontAdd.Sorted := true;
        DontAdd.Duplicates := dupIgnore;

        // --- addresses from current message ----------------------------
        res := 80;
        for i := 0 to List.Count-1 do
          if i<>ii then begin
            if ((List[ii].AddressType in [atTo,atCC,atBCC]) and (List[i].AddressType in [atReplyTo,atMailFollowupTo,atFrom,atSender,atFollowupTo])) or
               ((List[ii].AddressType in [atReplyTo]) and (List[i].AddressType in [atTo,atCC,atBCC,atMailFollowupTo,atFrom,atSender,atFollowupTo])) or
               ((List[ii].AddressType in [atMailFollowupTo]) and (List[i].AddressType in [atTo,atCC,atBCC,atReplyTo,atFrom,atSender,atNewsgroup])) or
               ((List[ii].AddressType in [atFrom,atSender]) and (List[i].AddressType in [atTo,atCC,atBCC,atReplyTo,atMailFollowupTo])) or
               ((List[ii].AddressType in [atNewsgroup]) and (List[i].AddressType in [atFollowupTo,atMailFollowupTo,atFrom,atSender])) or
               ((List[ii].AddressType in [atFollowupTo]) and (List[i].AddressType in [atNewsgroup,atTo,atCC,atBCC,atReplyTo,atFrom,atSender]))
            then
              add(List[i].AddrSpec,List[i].DisplayString,TypStr[List[i].AddressType])
            else
              rm(List[i].AddrSpec);
          end;

        // --- addresses from provious message ---------------------------
        if assigned(gsData) and assigned(gsData.OrgHdp) then
        with gsData.OrgHdp do
        begin
          res := 81;
          addz(AbSender,      TypStr[atFrom  ],       TypStr[atFrom  ]);
          addz(Sender,        TypStr[atSender],       TypStr[atSender]); 
          addz(wab,           GetRes2(610,87),         GetRes2(610,87)); 
   
          if (UTo<>'') or (CC<>'') or (BCC<>'') then
          begin
            if UTo<>'' then addu(UTo,  TypStr[atTo]);
            if CC <>'' then addu(CC,   TypStr[atCC]);
            if BCC<>'' then addu(BCC,  TypStr[atBCC]);
            addz(Empfaenger,  GetRes2(610,84),TypStr[atNewsgroup]);
            addz(Kopien,      GetRes2(610,84),TypStr[atNewsgroup]);
          end else
          begin
            addz(Empfaenger,  TypStr[atTo]   ,TypStr[atNewsgroup]);
            addz(Kopien,      TypStr[atTo]   ,TypStr[atNewsgroup]);
          end;
          addz(oab,           GetRes2(610,85), GetRes2(610,85)); 
          addz(oem,           GetRes2(610,86), GetRes2(610,86));
       
          if UMailReplyTo   <>'' then addu(UMailReplyTo,   TypStr[atReplyTo]) else
          if UReplyTo       <>'' then addu(UReplyTo,       TypStr[atReplyTo]);
          if UMailFollowupTo<>'' then addu(UMailFollowupTo,TypStr[atFollowupTo]);
        
          addz(DiskussionIn,  TypStr[atMailFollowupTo],TypStr[atFollowupTo]);
          addz(AntwortAn,     TypStr[atReplyTo],       TypStr[atReplyTo]);

//        res := 82;
//        TODO: U-Resent-* headers
        end;      

      finally
        Dontadd.Free;
      end;

      // --- add other entries in list ---------------------------------
      if Lister.Lines.Count>0 then begin
        Lister.AddLine('=-');
        Lister.AddLine('=*'+GetRes2(610,92));
      end;
      Lister.AddLine('=*'+GetRes2(610,90));
      Lister.AddLine('=*'+GetRes2(610,91));

      x1 := cr.x;   x2 := cr.x+4+Max(50,TypeWidth+AddrWidth);
      y1 := cr.y+1; y2 := cr.y+2+Lister.Lines.Count;

      if y2 > ScreenLines then begin y1 := Max(1,y1-(y2-ScreenLines)); y2:= ScreenLines; end;
      if x2 > ScreenWidth then begin x1 := Max(1,x1-(x2-ScreenWidth)); x2:= ScreenWidth; end;

      Lister.SetSize(x1+1,x2-1,y1+1,y2-1);
      openboxat(x2-x1+1,y2-y1+1,'',x1,y1,cr.acol.ColSelBox,cr.acol.ColSelBox);
      try
        cr.brk := Lister.Show;
        if cr.brk then exit;
      finally
        closebox;
      end;

      NewList := TAddressList.Create;
      try
        if SelectAddress_KeyPressed = 0 then 
          SelectAddress_KeyPressed := Lister.Lines.Count - Lister.SelLine;
        case SelectAddress_KeyPressed of
        1: (* select user *)
          begin
            wpush(1,ScreenWidth,1,ScreenLines,'-');
            xp4.ArchivWeiterleiten := false;
            xp4.select(3);
            wpop;
            if xp4.SelPos <= 0 then exit;
            dbGo(ubase,selpos);

            if dbReadByteN(ubase,ub_userflags) and 4 <> 0 then
            begin
              NewList.AddNew.XPAddress := dbReadNStr(ubase,ub_username);
            end else
            begin
              if dbXsize(ubase,'adresse')=0 then 
                s := dbReadNStr(ubase,ub_username)
              else
                s := dbReadXStr(ubase,'adresse');
              NewList.AddNew.ZCAddress := s;
            end;
          end;
        2: (* select group *)
          begin
            wpush(1,ScreenWidth,1,ScreenLines,'-');
            xp4.ArchivWeiterleiten := false;
            xp4.select(-1);
            wpop;
            if xp4.SelPos <= 0 then exit;
            dbGo(bbase,selpos);
            NewList.AddNew.XPAddress := dbReadNStr(bbase,bb_brettname);
          end;
        else (* use marked addresses *)
          for j := 0 to Lister.Lines.Count-1-3 do
            if Lister.Marked[j] or (Lister.Selline = j) then
              NewList.AddNew.DisplayString := Mid(Lister.Lines[j],TypeWidth+2);
        end; // case
        
        CheckAddressSetAddressList(NewList,true);
        if FieldServer >= 0 then SetServerField(FieldServer);
        cr.s := List[ii].DisplayString;

      finally
        NewList.Free;
      end;
      
    finally
      Lister.Free;
    end;
  end;

  function CheckAddress(var inhalt:string):boolean;

    procedure ParseAddresses(const inhalt: string; list: TAddressList);
    var i:          Integer;    // current position
        j:          Integer;    // position of last address start
        AfterAt:    boolean;    // after @ sign
        Quote:      Boolean;    // within dquotes
        Literal:    Boolean;    // within domain literal
        Angle:      Boolean;    // within angle brackets
        CommentCnt: Integer;    // comment count
        CGroup:     Integer;    // current group index
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

        CGroup := List.GroupNames.Add(RFCUnquotePhrase(s));
        J := I+1;
      end;

      procedure AddAddr;
      var s,t: string;
          p: integer;
          d: DB;
          bb : string;
          nu: boolean;
          a: TAddress;
      begin (* procedure AddAddr *)
        s := Trim(Copy(Inhalt,J,I-J));

      // Nothing => Ignore comma
        If s='' then
        begin
          J := I+1;
          exit;
        end;

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
          Group := CGroup;
        end;
        J := I+1;
      end; (* procedure AddAddr *)

    begin
      CommentCnt := 0;
      AfterAt := false;
      Quote := false;
      Angle := false;
      Literal := false;
      CGroup := -1;
      SkipNext := false;
      J := 1; i := 1;

      while i <= Length(Inhalt) do
      begin
        if SkipNext then
          SkipNext:=false else
        case inhalt[i] of
          '@':  AfterAt := true;
          '\':  SkipNext := true;
          '"':  Quote := not Quote;
          '<':  if(not Quote)and(CommentCnt<=0)then Angle:=true;
          '>':  if(not Quote)and(CommentCnt<=0)then Angle:=false;
          '(':  If not Quote then Inc(CommentCnt);
          ')':  If(not Quote)then If CommentCnt>0 then Dec(CommentCnt);
          '[':  If(not Quote)and(AfterAt)then Literal := true;
          ']':  If(not Quote)and(AfterAt)then Literal := false;
          ':':  If(not Quote)and(not Angle)and(CommentCnt<=0)and(not AfterAt)and(not Literal) then AddGroup;
          ',':  If(not Quote)and(not Angle)and(CommentCnt<=0)and(not Literal)then begin AddAddr; AfterAt := false; end;
          ';':  If(not Quote)and(not Angle)and(CommentCnt<=0)and(not Literal)then begin AddAddr; CGroup:=-1; end;
        end;
        Inc(i);
      end;
        
      AddAddr;
    end;  

 {function CheckAddress(var inhalt:string):boolean;}
  var ii: integer;
      nl: TAddressList;
  begin
    ii := IndexPos;

    Inhalt := Trim(Inhalt);

    if (Inhalt = List[ii].DisplayString) or (not Done) then begin
      result := true;
      exit;
    end;

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
      CheckAddressSetAddressList(nl,false);
      inhalt := List[ii].DisplayString;
    finally
      nl.Free;
    end;
    if FieldServer>=0 then SetServerField(FieldServer);
    result := true;
  end;


  procedure CheckAddressSetAddressList(nl: TAddressList; AutoAdd: boolean);
  var ii,j: Integer;
      lg: integer;
      lp: integer;
      pm_type,am_type: TAddressListType;      
      ch: boolean;
  begin    
    ch := false;
    ii := IndexPos;
  
    lg := List[ii].Group;
    
    pm_type := List[ii].AddressType;
    am_type := pm_type;

    if am_type in [atTo,atCC,atBCC] then am_type := atNewsgroup;
    if am_type in [atReplyto,atMailFollowupTo] then am_type := atFollowupTo;

    if pm_type in [atNewsgroup] then pm_type := atTo;
    if pm_type in [atFollowupTo] then pm_type := atMailFollowupTo;

  // -- Read data from database ----------------------------------------
    
    CheckEmpfaengerList(nl,not AutoAdd,false,nil);

  // -- Neue Adresse(n) einfügen ---------------------------------------

    for j := 0 to nl.Count-1 do 
      if nl[j].pm then
        nl[j].AddressType := pm_type
      else
        nl[j].AddressType := am_type;

    if nl.Count<>1 then 
    begin
      List.Delete(ii);
      if nl.Count>0 then
        List.InsertList(ii,nl);
      ch := true;
    end else begin
      List[ii].Assign(nl[0]);
      if nl[0].Group>=0 then
        List[ii].Group := List.GroupNames.Add(nl.GroupNames[nl[0].Group])
    end;

  // -- Gruppe/Addresstyp wieder setzen --------------------------------

    j := ii;
    lp := j;

    while (j<=List.Count-1) and ((j<nl.Count+ii) or (List[j].Group>=0)) do
    begin
    // -- Gruppe wieder setzen, ggf. nach vorne schieben ---------------
      if (j<nl.Count+ii) then begin
        if (lg>=0) and (j<nl.Count+ii) then
          if List[j].Group<0 then 
            List[j].Group := Lg;
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

    if (nl.Count<>1) or ch then 
      ScrollDone
    else
      ScrollDone2(ii-Offset,(ii-Offset)*FieldInc+Field1st);
  end;

// ---------------------------------------------------------------------
{-- Global functions (address editing) --------------------------------}
// ---------------------------------------------------------------------

  procedure TSendUUData.ComposeEditableList;
  var i: Integer;
     lg: integer;
  begin
    List.Clear;
    List.Capacity := EmpfList.Count+1;
    Lg := -1;

    Server := '';

    for i:=0 to EmpfList.Count-1 do
    begin
      if (EmpfList[i].Group<>LG) then
        if(LG>0) then
          List.AddNew.Group := LG;

      with List.AddNew do 
      begin
        Assign(EmpfList[i]);
        if EmpfList[i].Group>=0 then begin
          if (EmpfList[i].Group<>LG) then
            LG := List.GroupNames.Add(EmpfList.GroupNames[EmpfList[i].Group]);
          Group  := LG;
        end;
        LG := Group;
      end;
    end;
    List.AddNew.Group := -1;
    CorrectEncryptionMethod;
  end;

  procedure TSendUUData.UnComposeEditableList;
  var ii: integer;
      it: TAddressListType;
      LastGroup: array [TAddressListType] of Integer;
  
  begin
    EmpfList.Clear;
    EmpfList.GroupNames.Clear;

    for it := Low(TAddressListType) to High(TAddressListType) do
      LastGroup[it] := -1;
    
    for ii := 0 to List.Count-1 do
     if not List[ii].Empty then
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

var   SendWindowInitGlobals_Done: boolean = false;  

procedure SendWindowInitGlobals(sd: TSendUUData);
begin
  Offset := 0;
  gsData := sd;

  if SendWindowInitGlobals_Done then exit;

  TypStr[atNewsgroup   ] := GetRes2(610,30); {'Gruppe:'}
  TypStr[atTo          ] := GetRes2(610,31); {'An:'}
  TypStr[atCC          ] := GetRes2(610,32); {'CC:'}
  TypStr[atBCC         ] := GetRes2(610,33); {'BCC:'}
  
  TypStr[atFollowupTo  ] := GetRes2(610,36); {'Antw.-An:'}
  TypStr[atReplyTo     ] := GetRes2(610,35); {'Disk.-In:'}
  TypStr[atMailFollowupTo]:=GetRes2(610,36); {'Disk.-In:'}

  TypStr[atFrom        ] := GetRes2(610,40); {'Autor:'}
  TypStr[atSender      ] := GetRes2(610,41); {'Sender:'}
  
  w1 := Max(Length(TypStr[atUnused      ]),
        Max(Length(TypStr[atNewsgroup   ]),
        Max(Length(TypStr[atTo          ]),
        Max(Length(TypStr[atCC          ]),
        Max(Length(TypStr[atBCC         ]),
        Max(Length(TypStr[atFrom        ]),
        Max(Length(TypStr[atSender      ]),
        Max(Length(TypStr[atFollowupTo  ]),
            Length(TypStr[atReplyTo     ]) ))))))));

  SendWindowInitGlobals_Done := true;
end;

function TSendUUData.EditEmpfaengerList(
  const DialogueTitle: String;          // Dialog-Titel
  EditRecipients:   Boolean;            // Empfänger bearbeiten
  EditSubject:      Boolean;            // Betreff bearbeiten
  ShowSubject:      Boolean;            // Betreff überhaupt anzeigen
  PMAllowedNets:    TNetClassSet;       // Erlaubte Netztypen für PMs
  AMAllowedNets:    TNetClassSet        // Erlaubte Netztypen für AMs
): boolean;
var
  i: Integer;
  s: string;
begin
  SendWindowInitGlobals(self);
  FieldServer := -1;
  FieldEncryption := -1;
  FieldSigning := -1;

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

      dialog(76,h+iif(ShowSubject,2,0),DialogueTitle,x,y);    { 'Kopien an:' }
      
      maskdontclear;
      MasksetScrollFunc(ScrollFn);

      Maddstring(2,1,'',Dummy1,0,0,''); FieldUp := FieldPos;
      MSet0Proc(ScrollUp);

      for i:= 0 to h-1 do
      begin
        if i=0 then Field1st := fieldpos + 1;

        MaddText  (2,      1+i,'',col.coldiahigh); AddrText[1,i] := MTextPos;
        Maddstring(2,      1+i,'',AddrCont[0,i],w1,      MAXINT,'');
//      MappSel   (true,TypStr[atTo]+'ù'+TypStr[atCC]+'ù'+TypStr[atBCC]+'ù'+
//                      TypStr[atReplyTo]+'ù'+(* TypStr[atMailFollowupTo]+'ù'+ *)
//                      TypStr[atFollowupTo]);
        MAppCustomSel(@SelectType,true);
                        
        MSetVFunc(CheckType);
        MHnr(2390);

        Maddstring(2+w1+4, 1+i,'',AddrCont[1,i],10,      MAXINT,'');
        MSetVFunc(CheckGroup);
        MHnr(2391);
        MaddText  (2+w1+16,1+i,'',col.coldiahigh); AddrText[0,i] := MTextPos;

        Maddstring(2+w1+18,1+i,'',AddrCont[2,i],76-w1-22,MAXINT,'');
        MSetVFunc(CheckAddress);
        MAppCustomSel(@SelectAddress,false);
        MHnr(2392);

        Maddstring(2+w1+4, 1+i,'',AddrCont[2,i],76-w1-8, MAXINT,'');
        MSetVFunc(CheckAddress);
        MAppCustomSel(@SelectAddress,false);
        MHnr(2392);

//      MaddText  (2+72,1+i,'',col.coldiahigh);    AddrText[2,i] := MTextPos;

        if i=0 then FieldInc := fieldpos + 1 - Field1st;
      end;

      Maddstring(2+70,1+h-1,'',Dummy2,0,0,''); FieldDwn := FieldPos;
      MSet0Proc(ScrollDown);

      if ShowSubject or EditSubject then
      begin
        if EditSubject then
        begin
          MaddString(1,h+2,GetRes2(610,50),Subject,76-w1-8,MAXINT,'');
          MHNr(2300);
        end else
        begin
          s := GetRes2(610,50); { '^Betreff' }
          i := Cpos('^',s); if i>0 then Delete(s,i,1);

          MaddText  (1,h+2,s,0);
          MaddText  (2+w1+4,h+2,LeftStr(Subject,76-w1-6),0);
        end;
      end;

      ScrollDone;

      if Editsubject then
        if List.Count>=2 then
          SetFieldPos(FieldPos)   
        else 
          SetFieldPos(Field1st+1);
      
      readmask(brk);
      enddialog;
    until done;

    if not brk then UnComposeEditableList;
    
  finally
    List.Free;
  end;
  except
    on e:Exception do fehler(e.message);
  end;

  result := not brk;
end;

// ---------------------------------------------------------------------

procedure OnAttachButton(var cont: string);
begin
  gsData.EditAttach;

  SetEncryptionField;
  SetSigningField;
end;

function OnExit(brk,modif:boolean):boolean;
var dummy: boolean;
begin
  result := true;
  if brk then
    case ReadIt(60,GetRes2(600,4),GetRes2(600,5),1,dummy) of
      1: MQuit(3);              // postpone, only changes return value
      2: ;
      else result := false;       // cancel
    end;
end;

procedure OnTextButton(var cont: string);
begin
  gsData.EditText;
end;

procedure OnServerSelect(var cr:customrec);
var Lister: TLister;
  MaxLen: Integer;

  procedure ScanServers;
  var i: Integer;
      o: boolean;
      d: DB;
      BoxName: String;
      BoxType: eNetz;
  begin
    dbOpen(d,BoxenFile,0);
    while not dbEOF(d) do begin
      BoxName := dbReadStr(d,'boxname');
      BoxType := dbReadByte(d,'netztyp');

      o := true;
      for i := 0 to List.Count-1 do
        if (not List[i].Empty) and
           (List[i].AddressType in [atNewsgroup, atTo, atCC, atBCC]) and
           (not ntAdrCompatible(BoxType,List[i].Address)) then
        begin
          o:=false;
          break;
        end;
      if o then begin
        Lister.Lines.Add(' '+BoxName);
        MaxLen := Max(Length(BoxName),MaxLen);
      end;

      dbNext(d);
    end;
    dbClose(d);
  end;

  procedure SetServer(const name: string);
  var i: integer;
      UName: string;
  begin
    UName := UpperCase(Name);
    for i := 0 to List.Count-1 do
      if not List[i].Empty then
      if UpperCase(List[i].BoxName)<>UName then
       begin
        List[i].BoxName := name;
        List[i].BoxForce := true;
      end;
    cr.s := name;
    ScrollDone;
  end;

begin
  Lister := TLister.CreateWithOptions(0,0,0,0,0,'/NS/SB/NLR/DM/M/');
  MaxLen := 16;
  try
    ScanServers;
    if Lister.Lines.Count<=0 then
      fehler(GetRes2(610,93))
    else begin
      ListerOnSelect(Lister,cr,MaxLen);
      if not cr.brk then
        SetServer(Trim(Lister.FirstMarked));
    end;
  finally
    Lister.Free;
  end;
end;

function OnEncryptionMethodChange(var inhalt:string):boolean;
var J: TSendEncryptionMethod; 
begin
  for j := Low(j) to High(j) do
    if GetRes2(610,130+Ord(j)) = inhalt then begin
      GSData.EncryptionMethod := j;
      SetSigningField;
      exit;
    end;
end;

function OnSigningMethodChange(var inhalt:string):boolean;
var J: TSendSigningMethod;
begin
  for j := Low(j) to High(j) do
    if GetRes2(610,130+Ord(j)) = inhalt then begin
      GSData.SigningMethod := j;
      SetEncryptionField;
      exit;
    end;
end;

{$INCLUDE xpsendmessage_datesend.inc}

function TSendUUData.SendWindow(dlg_title: string):boolean;
var   i: Integer;
 senden: integer;
  dummy: integer;
s,s1,s2: string;
x1,x2,x3,x4: string;
     xx: Integer;
     yy: Integer;
      p: Integer;
 ok_pos: Integer;
  ll,rr: Integer;

rfcprio_str: string;
 Dumms: String;

const
  add_h = 5;
begin
  if dlg_title='' then dlg_title := GetRes2(610,10);
  SendWindowInitGlobals(self);

  List := TAddressList.Create;
  try //..except
  try //..finally
    ComposeEditableList;
      // Number of options on the left hand side
      ll := 4;

      // Number of options on the right hand side
      if flReEdit then
        rr := 3
      else
        rr := 4;

      max_h := 20-add_h-Max(ll,rr);
      new_h := Min(Max(1,List.Count),max_h);
  
    repeat
      h := new_h;
      done := true;

      dialog(76,h+add_h+Max(ll,rr),dlg_title,x,y);
//    inc(x); inc(y);
//    openmask(x,x+73,y,y+h+2,false);
      
      maskdontclear;
      MasksetScrollFunc(ScrollFn);

      Maddstring(2,1,'',Dummy1,0,0,''); FieldUp := FieldPos;
      MSet0Proc(ScrollUp);

      for i:= 0 to h-1 do
      begin
        if i=0 then Field1st := fieldpos + 1;

        MaddText  (2,      1+i,'',col.coldiahigh); AddrText[1,i] := MTextPos;
        Maddstring(2,      1+i,'',AddrCont[0,i],w1,      MAXINT,'');
//      MappSel   (true,TypStr[atTo]+'ù'+TypStr[atCC]+'ù'+TypStr[atBCC]+'ù'+
//                      TypStr[atReplyTo]+'ù'+(* TypStr[atMailFollowupTo]+'ù'+ *)
//                      TypStr[atFollowupTo]);
        MAppCustomSel(@SelectType,true);
        MSetVFunc(CheckType);
        MHNr(2390);

        Maddstring(2+w1+4, 1+i,'',AddrCont[1,i],10,      MAXINT,'');
        MSetVFunc(CheckGroup);
        MaddText  (2+w1+16,1+i,'',col.coldiahigh); AddrText[0,i] := MTextPos;
        MHNr(2391);

        Maddstring(2+w1+18,1+i,'',AddrCont[2,i],76-w1-4-18,MAXINT,'');
        MSetVFunc(CheckAddress);
        MAppCustomSel(@SelectAddress,false);
        MHNr(2392);

        Maddstring(2+w1+4, 1+i,'',AddrCont[2,i],76-w1-4-4 ,MAXINT,'');
        MSetVFunc(CheckAddress);
        MAppCustomSel(@SelectAddress,false);
        MHNr(2392);

//      MaddText  (2+72,1+i,'',col.coldiahigh);    AddrText[2,i] := MTextPos;

        if i=0 then FieldInc := fieldpos + 1 - Field1st;
      end;

      Maddstring(2+70,1+h-1,'',Dummy2,0,0,''); FieldDwn := FieldPos;
      MSet0Proc(ScrollDown);

      MaddString(1,h+2,GetRes2(610,50) {'^Betreff'},Subject,76-w1-8,MAXINT,'');
      MHNr(2300);

      Dumms := 'N/A';

      x1 := GetRes2(610,72); // 'Lesebestätigung'
      x2 := GetRes2(610,75); // 'keine Signatur anhängen'
      x3 := GetRes2(610,76);  // 'nach Versand löschen'
      x4 := GetRes2(610,77); // 'Nachricht nicht archvieren'
      xx := Max(Max(Length(x1),Length(x2)),Max(Length(x3),Length(x4))) + 7;

      yy := h+4;

      SetServerField(-1); // sets global variable 'Server'
      MaddString(1,yy,GetRes2(610,55) {'S^erver'}, Server, 76-w1-xx-5-8, MAXINT,'');
      MHNr(2901);
      Inc(yy);
      MAppCustomSel(@OnServerSelect,false);
      FieldServer := FieldPos;

      MaddString(1,yy,GetRes2(610,56) {'Verschlüsselung'}, Dumms, 76-w1-xx-5-8, MAXINT,'');
      MHNr(2903);
      Inc(yy);
      MSet1Func(OnEncryptionMethodChange);
      FieldEncryption := FieldPos;
      SetEncryptionField;

      MaddString(1,yy,GetRes2(610,57) {'Signatur'}, Dumms, 76-w1-xx-5-8, MAXINT,'');
      MHNr(2909);
      Inc(yy);
      MSet1Func(OnSigningMethodChange);
      FieldSigning := FieldPos;
      SetSigningField;

      if rfcprio in [1..5] then rfcprio_str := GetRes2(272,rfcprio)
      else                      rfcprio_str := GetRes2(272,6);
      MaddString(1,yy,GetRes2(610,60) {'Priorität'}, rfcprio_str, 76-w1-xx-5-8, MAXINT,'');
      MappSel(true,GetRes2(272,9));
      MHNr(2902);
      Inc(yy);

//      MaddString(1,yy,GetRes2(610,70) {'Datum'}, Dumms, 76-w1-xx-5-8, MAXINT,'');
//      MHNr(2904);
//      Inc(yy);

      yy := h+4;

      MaddBool(76-xx,yy, x1, flRequestMDN);
      MHNr(2905);
      Inc(yy);

      if not flReEdit then begin
        MaddBool(76-xx,h+5, x2, flOhneSig);
        MHNr(2906);
        Inc(yy);
      end else if ll>=rr then
        Inc(yy);

      MaddBool(76-xx,yy, x3, flLoesch);
      MHNr(2907);
      Inc(yy);

      MaddBool(76-xx,yy, x4, flNoArchive);
      MHNr(2908);
      Inc(yy);

      MaddHLine(0,h+add_h-1+Max(ll,rr));

      p := 2;

      s := GetRes2(610,20); { ' ^Ja ' }
      MAddButton(p,h+add_h+Max(ll,rr),s,senden,1,true);
      ok_pos := FieldPos;
      inc(p,Length(s)+3);
      MHNr(2925);

      s := GetRes2(610,21); { ' ^Nein ' }
      MAddButton(p,h+add_h+Max(ll,rr),s,senden,-1,true);
      inc(p,Length(s)+3);
      MHNr(2926);
      MaskSetQFunc(OnExit);

      s := GetRes2(610,65); { '^Datun' }
      MAddButton(p,h+add_h+Max(ll,rr),s,senden,0,false);
      Mset0Proc(OnDateButton);
      inc(p,Length(s)+3);
      MHNr(2927);

      s1 := GetRes2(610,26); { '^Anhang' }
      s2 := GetRes2(610,25); { '^Text' }

      MaddButton(76-Length(s2)-Length(s1)-4,h+add_h+Max(ll,rr),s1,dummy,0,false);
      Mset0Proc(OnAttachButton);
      MHNr(2928);

      MaddButton(76-Length(s2)           -1,h+add_h+Max(ll,rr),s2,dummy,0,false);
      Mset0Proc(OnTextButton);
      MHNr(2929);

      ScrollDone;
      SetFieldPos(ok_pos);

      Senden := readmask(brk);

      enddialog;
    until done;

    if not brk then
    begin
      UnComposeEditableList;

      rfcprio := 0;
      for i := 1 to 5 do
        if rfcprio_str = GetRes2(272,i) then begin
          rfcprio := i; break;
        end;

      case senden of
        2: Self.flIntern := true;
        3: Self.flPostpone := true;
      end;
    end;

  finally
    List.Free;
  end;
  except
    on e:Exception do fehler(e.message);
  end;

  result := not brk;    
end;

// ---------------------------------------------------------------------
// -- Adressen überprüfen ----------------------------------------------
// ---------------------------------------------------------------------

{ The method is only a wrapper around the real function                }

procedure TSendUUData.CheckEmpfaengerList(
  List:             TAddressList;
  Prompt:           Boolean;
  AutoAdd:          Boolean
);
var Index: Integer;
begin
  XPSendmessage.CheckEmpfaengerList(List,Prompt,AutoAdd,self);

  if List = EmpfList then 
    for Index := 0 to List.Count-1 do
    begin
      fHas[ntClass(List[Index].Netztyp),List[Index].PM] := true;
      fHas[ncNone                      ,List[Index].PM] := true;
    end;
end;

{ The actual function                                                  }

procedure CheckEmpfaengerList(
  List:     TAddressList;
  Prompt:   Boolean;
  AutoAdd:  Boolean;
  sData:    TSendUUData
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
    var user,adresse,komm,pollbox,real,oreal: string;
        halten: Integer16;
        adr,flags,b: byte;
        brk: boolean;
    begin
      user    := EMail.XPAddress;
      adresse := EMail.XPAddress;
      komm    := '';

      if EMail is TDomainEMailAddress then
      begin
        real    := TDomainEMailAddress(EMail).RealName;
        oreal   := real;
      end;

      pollbox := List[Index].BoxName; 
      if pollbox='' then 
      begin
        if EMail is TFTNAddress then  
          PollBox := DefFidoBox 
        else
          PollBox := DefaultBox;
      end;
      
      halten  := StdUHaltezeit;
      Adr     := NeuUserGruppe;
      Flags   := 1 + iif(newuseribm,0,8);

      EditUser(GetRes2(610,200),user,adresse,komm,pollbox,halten,adr,flags,true,brk);

      if brk then
        result := false
      else
      begin
        List[Index].ZCAddress := Adresse;

        if List[Index].Address is TDomainEMailAddress then
          TDomainEMailAddress(List[Index].Address).RealName  := iifs(real='',oreal,real);

       dbSeek(ubase,uiName,UpperCase(user));
       if dbFound then
         rfehler(2703)    { 'Dieser User ist bereits vorhanden!' }
       else
       begin
         dbAppend(ubase);
         dbWriteNStr(ubase,ub_username,LeftStr(user,80));
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

    UName := UpperCase(LeftStr(email.XPAddress,80));

    dbSeek(ubase,uiName,UName);
    if dbFound then begin
  // -- Empfänger bekannt ----------------------------------------------
      UFlags := dbReadInt(ubase,'userflags');
//    if(UFlags and 16)<>0 then flEB:=true;

      if (dbXsize(ubase,'adresse')<>0) then begin
        List[Index].ZCAddress := dbReadXStr(ubase,'adresse');
        if UpperCase(LeftStr(List[Index].XPAddress,80))<>UName then
          exit;                                // Gleiche Addresse nochmal
      end;

      List[Index].EncodingMethod := Integer(dbReadByteN(ubase,ub_codierer));
      List[Index].EncodingPassword := dbReadXStr(ubase,'passwort');

      if assigned(sData) then
      begin
        if UFlags and 16<>0 then
          sData.flRequestMDN := true;

        // Set*Method cleans everything up and handles collisions
        if UFlags and 2<>0 then begin
          sData.EncryptionMethod := TSendEncryptionMethod(List[Index].EncodingMethod);
          if List[Index].EncodingMethod in [8,9] then
            sData.SigningMethod := TSendSigningMethod(List[Index].EncodingMethod);
        end;
      end;

      if not List[Index].BoxForce then
        List[Index].BoxName := dbReadStrN(ubase,ub_pollbox)
      else
      if UpperCase(dbReadStrN(ubase,ub_pollbox)) = UpperCase(List[Index].BoxName) then
        List[Index].BoxForce := false;

    end else begin
  // -- Empfänger unbekannt --------------------------------------------
      UFlags := 0;  //default?
    // -- Benutzer fragen ----------------------------------------------
      if prompt and (not List[Index].NewUser) then begin
        if not PMEdit then
          List.Delete(Index);
        exit;                                // Gleicher Eintrag nochmal
      end else if autoadd then begin
    // -- Einfach anlegen ----------------------------------------------
        if List[Index].BoxName='' then
          List[Index].BoxName := DefaultBox;
        AddNewUser(List[Index].XPAddress,List[Index].BoxName);
        //UFlags := ???
      end else begin
    // -- Nicht in der DB ----------------------------------------------
        List[Index].NewUser := true;
        if not NewUserIBM then UFlags := 8; { NewUserIBM beruecksichtigen }
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
        if assigned(sData) and (Uppercase(List[Index].BoxName)<>UpperCase(sData.DefaultBox)) then
        begin
          List[Index].BoxName := sData.DefaultBox;
          goto BoxAgain;
        end else
        if Uppercase(List[Index].BoxName)<>UpperCase(DefaultBox) then
        begin
          List[Index].BoxName := DefaultBox;
          goto BoxAgain;
        end else
          List[Index].BoxName := '';
      end else
      begin
        List[Index].BoxName := dbReadStr(d,'boxname');       { Schreibweise korrigieren }
        List[Index].Netztyp := dbNetztyp(d);
        if assigned(sData) then sData.Boxen.Add(Chr(ord(List[Index].Netztyp))+List[Index].Boxname);
      end;
      dbClose(d);
    end else
    if assigned(sData) then
    begin
      List[Index].BoxName := sData.DefaultBox;
      goto BoxAgain;
    end;

    // -- Für alle Empfänger: Zeichensätze -----------------------------

    List[Index].Kopf         := HeaderPriv;
    List[Index].Signatur     := PrivSignat;

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
      List[Index].AddressType := atTo else
    if List[Index].AddressType in [atFollowupTo] then
      List[Index].AddressType := atMailFollowupTo;

    if assigned(sData) then
    begin
      sData.MergeCharsets(List[Index].Netztyp,List[Index].Charsets);
      sData.MergeMsgType(List[Index].Netztyp,true);

//    if (Index<=0) then
//    begin
//      HeadFile := HeaderPriv;
//      FSigTemplate := PrivSignat;
//    end;
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

      if not List[Index].BoxForce then
        List[Index].BoxName:=dbReadStrN(bbase,bb_pollbox)
      else
      if UpperCase(dbReadStrN(bbase,bb_pollbox)) = UpperCase(List[Index].BoxName) then
        List[Index].BoxForce := false;

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
      if assigned(sData) and (List[Index].BoxName='') then
        List[Index].BoxName := sData.DefaultBox;
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
        if assigned(sData) and (Uppercase(List[Index].BoxName)<>UpperCase(sData.DefaultBox)) then
        begin
          List[Index].BoxName := sData.DefaultBox;
          goto BoxAgain;
        end else
        if Uppercase(List[Index].BoxName)<>UpperCase(DefaultBox) then
        begin
          List[Index].BoxName := DefaultBox;
          goto BoxAgain;
        end else
          List[Index].BoxName := '';
      end else 
      begin
        List[Index].BoxName := dbReadStr(d,'boxname');       { Schreibweise korrigieren }
        List[Index].Netztyp := dbNetztyp(d);

        if assigned(sData) then sData.Boxen.Add(Chr(ord(List[Index].Netztyp))+List[Index].Boxname);
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
      List[Index].Kopf         := dbReadStr(d,'kopf')+ExtXPS;
      List[Index].Signatur     := dbReadStr(d,'Signatur')+ExtXPS;
      List[Index].Origin       := dbReadStr(d,'Origin')+ExtXPS;
      List[Index].Adresse      := dbReadStr(d,'Adresse')+ExtXPS;

      List[Index].Realname   := dbReadStr(d,'AMRealname');
      List[Index].Mail       := dbReadStr(d,'AMMail');
      List[Index].ReplyTo    := dbReadStr(d,'AMReplyTo');
      List[Index].FQDN       := dbReadStr(d,'AMFQDN');

      List[Index].EncodingMethod := 0;
      List[Index].EncodingPassword := '';
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
      List[Index].AddressType := atNewsgroup else
    if List[Index].AddressType in [atReplyTo,atMailFollowupTo] then
      List[Index].AddressType := atFollowupTo;

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

// ---------------------------------------------------------------------
// $Log$
// Revision 1.13  2003/09/07 14:49:42  cl
// - send window: postpone message
//   CLOESES task #76797: "Sendefenster: Parken"
//
// Revision 1.12  2003/09/06 23:03:08  cl
// - send window - time-shifted sending of message
//   cLOSES task #76792: Sendefenster: Datum
//
// Revision 1.11  2003/08/30 22:19:27  cl
// - send window: select encryption and signature method
// - CLOSES Task #76790 Sendefenster: Kodieren/Sicherheit
//
// Revision 1.10  2003/08/27 21:24:31  cl
// - send window: overwrite/select server
//   CLOSES: task #76789 Sendefenster: Box überschreiben
//
// Revision 1.9  2003/08/24 23:33:27  cl
// - Sendefenster: Priorität setzen (RFC), Keine Signatur (ohneSig),
//   Nachricht löschen (nach Versand), Empfangsbestätigungen,
//   X-No-Archive setzen
// - updated on-line help
//
// CLOSES:
//   task #76791 Sendefenster: Empfangsbestätigungen
//   task #76793 Sendefenster: ohne Sig
//   task #76794 Sendefenster: Priorität
//   task #76796 Sendefenster: Löschen
//
// Revision 1.8  2003/03/16 18:59:09  cl
// - fixed bug that would delete addresses from the list under certain
//   circumstances.
//
// Revision 1.7  2003/01/27 18:45:46  cl
// - send window rewrite IIb: implemented hotkeys
//
// Revision 1.6  2003/01/13 22:14:28  cl
// - send window rewrite IIa - cleanups
//
// Revision 1.5  2003/01/11 19:54:07  cl
// - fixes for FTN addresses
//
// Revision 1.4  2003/01/07 00:56:47  cl
// - send window rewrite -- part II:
//   . added support for Reply-To/(Mail-)Followup-To
//   . added support to add addresses from quoted message/group list/user list
//
// - new address handling -- part II:
//   . added support for extended Reply-To syntax (multiple addresses and group syntax)
//   . added support for Mail-Followup-To, Mail-Reply-To (incoming)
//
// - changed "reply-to-all":
//   . different default for Ctrl-P and Ctrl-B
//   . more addresses can be added directly from send window
//
// Revision 1.3  2002/12/14 07:31:40  dodi
// - using new types
//
// Revision 1.2  2002/12/12 11:58:52  dodi
// - set $WRITEABLECONT OFF
//
// Revision 1.1  2002/11/14 21:06:13  cl
// - DoSend/send window rewrite -- part I
//
// ==> renamed from xpsendmessage_addr_edit.pas to xpsendmessage_window.inc
//
// Revision 1.8  2002/08/10 00:29:27  cl
// - various address editor fixes
//
// Revision 1.7  2002/07/25 21:23:18  mk
// - removed unused variables
// - i is now a local variable
//
// Revision 1.6  2002/07/25 20:57:39  mk
// - possible fix for #585559: OXP/Linux 3.9: neue User verkrüppelt
//   AddAdr used for loop varialbe i from CheckAdresses, this varialbe
//   is undefined after the loop; changed this loop in a while loop
//
// Revision 1.5  2002/06/23 15:03:07  cl
// - Adapted Nachricht/Direkt to new address handling.
//
// Revision 1.4  2002/06/23 13:49:37  cl
// - more intelligent cursor placement
// - preserve real name when creating new users
//
// Revision 1.3  2002/05/09 15:17:45  cl
// - fixed creation of editable list
//
// Revision 1.2  2002/04/17 19:35:03  mk
// - added xpdefine.inc
//
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//
// ---------------------------------------------------------------------

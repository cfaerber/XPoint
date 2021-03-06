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

{ Unterroutinen f�r TSendUUData }

{-- Signatur-Shuffler / Signatur-Makro-Ersetzer -----------------------}

//  FSigTemplate:   string;         { Signatur: Vorlagendatei          }
//  FSigData:       TMemoryStream;  { Signatur: Fertige Signatur       }
//
//  FSigTmplOK:     boolean;        { false: FSigTemplate neu suchen   }
//  FSigTmplForced: boolean;        { true: FSigTemplate nicht �ndern  }

procedure TSendUUData.MakeUserData;
var i: integer;
    a: TAddressListType;
begin
  if (not FUserDataForced) and (not FUserDataOK) then
  begin
    Self.CheckEmpfaengerList(EmpfList,false,false);
  
    for i := 0 to EmpfList.Count-1 do
    begin
      a := atUnused;
      if (EmpfList[i].AddressType in [
            atNewsgroup,
            atTo,
            atCC,
            atBCC]) and 
         (EmpfList[i].AddressType > a) and
         not EmpfList[i].Empty then 
      begin
        a := EmpfList[i].AddressType;
        
        SetHeadTemplate(EmpfList[i].Kopf);
        SetSigTemplate (EmpfList[i].Signatur);

	FRealname := EmpfList[i].Realname;
	FMail     := EmpfList[i].Mail;
	FReplyTo  := EmpfList[i].ReplyTo;
        FFQDN     := EmpfList[i].FQDN;
        
        if a=atBCC then break;
      end;
    end;
    FUserDataOK := true;

    FUserDataForced := false;
  end;
end;

function TSendUUData.GetAutoUserData:boolean;
begin
  result := not FUserDataForced;
end;

procedure TSendUUData.SetAutoUserData(x: boolean);
begin
  if x then FUserDataOK := false else if not FUserDataOK then MakeUserData;
  FUserDataForced := not x;
end;

// -- Schablonen -------------------------------------------------------

procedure TSendUUData.SetSigTemplate(NewTmplFile: string);
begin
  FUserDataForced := true;
  if (NewTmplFile<>'') and (Cpos('.',NewTmplFile)<=0) then NewTmplFile := NewTmplFile + ExtXps;
  
  if FSigTemplate <> NewTmplFile then begin
    FSigData := '';
    FSigDataOK := false;
  end;

  FSigTemplate := NewTmplFile;
end;

procedure TSendUUData.SetHeadTemplate(NewTmplFile: string);
begin
  FUserDataForced := true;
  if (NewTmplFile<>'') and (Cpos('.',NewTmplFile)<=0) then NewTmplFile := NewTmplFile + ExtXps;

  if FHeadTemplate <> NewTmplFile then begin
    FHeadData := '';
    FHeadDataOK := false;
  end;

  FHeadTemplate := NewTmplFile;
end;

function TSendUUData.GetSigTemplate: string;
begin
  MakeUserData;
  result := FSigTemplate;
end;

function TSendUUData.GetHeadTemplate: string;
begin
  MakeUserData;
  result := FHeadTemplate;
end;

function TSendUUData.GetSigData: string;
var TmplFile: string;
    t       : text;
    s       : string;
    num,n   : xpWord;
    Buf: array[0..4095] of Char;
const
    sigsep = '-*-';

begin
  if flOhneSig then begin
    result := ''; exit;
  end;

  TmplFile := SigTemplate;

  if  (not FSigDataOK) and (TmplFile<>'') and FileExists(TmplFile) then
  begin
    assign(t,TmplFile);
    SetTextBuf(t,buf);
    reset(t);
    readln(t,s);

    if s=sigsep then
    begin
      num:=0;
      while not eof(t) do
      begin
        readln(t,s);
        if s=sigsep then inc(num);
      end;
      n:=random(num)+1;    { zuf�llige Signatur ausw�hlen }
    end
    else
      n:=0;
    close(t);
    reset(t);
    
    while n>0 do 
    begin     { ausgew�hlte Signatur suchen }
      repeat
        readln(t,s);
      until s=sigsep;
      dec(n);
    end;

    FSigData := '';

    s:='';
    while not eof(t) and (s<>sigsep) do 
    begin
      readln(t,s);
      if (EmpfList.Count>0) and EmpfList[0].PM then rpsuser(s,Empf1Address,Empf1RealName);
      rpsdate(s);
      if s<>sigsep then FSigData := FSigData+s+#13#10;
    end;
    close(t);

    while (Length(FSigData)>=1) and (FSigData[Length(FSigData)] in [#13,#10]) do 
      SetLength(FSigData,Length(FSigData)-1);

    FSigDataOK := true;
  end;

  Result := FSigData;
end;

function TSendUUData.GetHeadData: string;
var TmplFile: string;
    t       : text;
    s       : string;
    Buf: array[0..4095] of Char;

begin
  TmplFile := HeadTemplate;

  if  (not FHeadDataOK) and (TmplFile<>'') and FileExists(TmplFile) then
  begin
    assign(t,TmplFile);
    SetTextBuf(t,buf);
    reset(t);
    
    while not eof(t) do
    begin
      readln(t,s);
      rpsdate(s);
      FHEadData := FHeadData+s+#13#10;
    end;
    close(t);

    FHeadDataOK := true;
  end;

  Result := FHeadData;
end;

{--------------------------------------------------------------------- }

function TSendUUData.GetReplyTo: string;
begin
  MakeUserData;
  result := FReplyTo;
end;

procedure TSendUUData.SetReplyTo(const NewValue: string);
begin
  FUserDataForced := true;
  FReplyTo := NewValue;
end;

function TSendUUData.GetSenderRealname: string;
begin
  MakeUserData;
  result := FRealname;
end;

procedure TSendUUData.SetSenderRealname(const NewValue: string);
begin
  FUserDataForced := true;
  FRealname := NewValue;
end;

function TSendUUData.GetSenderMail: string;
begin
  MakeUserData;
  result := FMail;
end;

procedure TSendUUData.SetSenderMail(const NewValue: string);
begin
  FUserDataForced := true;
  FMail := NewValue;
end;

function TSendUUData.GetFQDN: string;
begin
  MakeUserData;
  result := FFQDN;
end;

procedure TSendUUData.SetFQDN(const NewValue: string);
begin
  FUserDataForced := true;
  FFQDN := NewValue;
end;

{-- Find Default Box ------------------------------------------------- }

function TSendUUData.GetDefaultBox: string;
begin
  if OrgBox<>''                                 then result := OrgBox else
  if Assigned(OrgHdp) and (OrgHdp.Real_Box<>'') then result := OrgHdp.Real_Box else
  result := xp0.DefaultBox;
end;

procedure TSendUUData.SetForceBox(const NewBox: string);
var i: integer;
begin
  FForceBox := NewBox;
  for i := 0 to EmpfList.Count-1 do
    EmpfList[i].Boxname := NewBox;
end;

////////////////////////////////////////////////////////////////////////

{$IFDEF __undefined__}
function getsize:boolean;
var i:integer;
begin

{ TODO: Bessere, genauere Berechnung! }

  fs:=0; 

  if not sdata.partsex then
    fs := _filesize(sdata.datei)
  else
    for i:=0 to sdata.parts.count-1 do
      inc(fs,TSendAttach_Part(sdata.parts[i]).FileSize);

  if ((sdata.parts.Count<=0) or
     ((sdata.parts.Count=1) and (TSendAttach_Part(sdata.parts[0]).FileSize=0)))
(*  and not ntMsg0(netztyp) *)
then begin
    rfehler(602);   { 'leere Nachricht - nicht abgeschickt' }
    getsize:=false;
    end
  else begin
    getsize:=true;
    end;
end;

procedure showbetreff;
begin
  attrtxt(col.coldiahigh);
  mwrt(x+13,y+4,' '+forms(sdata.Subject,52)+' ');
end;


procedure showbox;
var ss : string;

   function SameServer:boolean;
   var i : integer;
   begin
     ss:=ccm^[iif(verteiler,1,0)].server;
     SameServer:=true;
     for i:=1 to cc_anz do
       if ccm^[i].server<>ss then begin
         ss:=''; SameServer:=false;
         end;
     GetServerName(ss); { -> korrekte Schreibweise des Systemnamens }
   end;

begin
  attrtxt(col.coldiahigh);
  gotoxy(x+13,y+8);
  moff;
  if cc_anz=0 then
    if forcebox = '' then Wrt2(forms(' '+box,BoxNameLen+5))
    else begin
      Wrt2(' '+forcebox);
      attrtxt(col.coldialog);
      Wrt2(' (');
      attrtxt(col.coldiahigh);
      Wrt2('*');
      attrtxt(col.coldialog);
      Wrt2(forms(')',(BoxNameLen+1)-length(forcebox)));
    end
  else if forcebox='' then
          if SameServer then Wrt2(forms(' '+ss,BoxNameLen+5))
          else begin
            attrtxt(col.coldialog);
            Wrt2('(');
            attrtxt(col.coldiahigh);
            Wrt2(box);
            attrtxt(col.coldialog);
            Wrt2(forms(')',(BoxNameLen+4)-length(box)));
          end
       else begin
         Wrt2(' '+forcebox);
         attrtxt(col.coldialog);
         Wrt2(' (');
         attrtxt(col.coldiahigh);
         Wrt2('*');
         attrtxt(col.coldialog);
        Wrt2(forms(')',(BoxNameLen+1)-length(forcebox)));
       end;
  mon;
end;

procedure showsize;
var ms : longint;
begin
  attrtxt(col.coldialog);
  moff;
  wrt(x+14,y+8,sp(18));
  attrtxt(col.coldiahigh);
  wrt(x+14,y+8,strs(fs+addsize));
  attrtxt(col.coldialog);
  Wrt2(getres(13));    { ' Bytes' }
  ms:=sdata.maxsize;
  if binary (* and ntBinEncode(netztyp) *) then  { aut. uu-Codierung }
    ms:=system.round((ms-400)/1.415);
  oversize:=iif((sdata.maxsize>0) and (fs+addsize>ms),fs+addsize-ms,0);
  if oversize>0 then Wrt2('!');
  mon;
end;
(*
function FidoAbsAdr:string;
begin
  if AltAdr<>'' then
    FidoAbsAdr:=AltAdr
  else
    if sdata.aliaspt then
      FidoAbsAdr:=LeftStr(box,cpos('/',box))+pointname
    else
      FidoAbsAdr:=box+'.'+pointname;
end;
*)
function orishuffle(fn:string):string;
var t     : text;
    s     : string;
    buf   : array[0..511] of byte;
    num,n : xpWord;
begin
  assign(t,fn);
  if not existf(t) then
    orishuffle:=getreps(621,fn)    { 'Origin-Datei fehlt: %s' }
  else begin
    settextbuf(t,buf);
    reset(t);
    num:=0;
    while not eof(t) do begin      { Origins z�hlen }
      readln(t,s);
      if s<>'' then inc(num);
      end;
    close(t);
    reset(t);
    n:=random(num)+1; s:='';
    while not eof(t) and (n>0) do begin
      readln(t,s);
      if s<>'' then dec(n);
      end;
    orishuffle:=LeftStr(s,54);
    close(t);
    end;
end;

(*
function fido_origin:string;
var s : string;
begin
  if not sdata.has_ftn then
    fido_origin:=''
  else begin
    if not pm then
      if XP_Tearline then
        s:=#13#10+'---'+xp_prver
      else
        s:=#13#10+'---'
    else
      s:='';
    if not pm then begin
      if LeftStr(LowerCase(fidoname),8)='shuffle:' then
        fidoname:=OriShuffle(trim(mid(fidoname,9)));
      s:=s+#13#10' * Origin: '+fidoname+' ('+FidoAbsAdr+')';
      end;
    fido_origin:=s;
    end;
end;
*)

procedure calc_hdsize;

begin
  addsize:=0;

  if not sdata.flOhnesig and (sigfile<>'') then
    inc(addsize,_filesize(sigfile));
    
//  inc(addsize,length(fido_origin));
{ if flMnet then inc(addsize,length(MausNet)+2);
  if flMloc then inc(addsize,length(MausLoc)+2); }
end;

procedure showcode;
  function pmcode(c:byte):string;
  begin
    pmcode:=LeftStr(pmcrypt[c-2].name,14);
  end;
begin
  attrtxt(col.coldialog);
  moff; (* 05.02.2000 MH: x+45 -> x+51 *) { unbedenklich }
  wrt(x+51,y+6,sp(22));
  gotoxy(x+51,y+6);
  if sdata.docode=0 then
  begin
    if sdata.Parts.Count=1 then
    case sdata.cancode of
       0 : Wrt2(getres2(601,iif(pm,2,3)));      { 'kein Pa�wort' / 'nicht m�glich' }
       1 : if binary then Wrt2(getres2(601,4))  { 'evtl. QPC m�glich' }
           else Wrt2(getreps2(601,1,'QPC'));    { 'QPC m�glich' }
       2 : Wrt2(getreps2(601,1,'DES'));         { 'DES m�glich' }
       3..2+maxpmc : Wrt2(getreps2(601,1,pmcode(sdata.cancode)));
       8 : Wrt2(getreps2(601,1,'PGP/MIME'));    { 'PGP/MIME moeglich' }
       9 : Wrt2(getreps2(601,1,'PGP'));         { 'PGP m�glich' }
      10 : Wrt2(getreps2(601,1,'Rot13'));       { '%s m�glich' }
    end else
    if (sdata.cancode in [8,9]) (* and ntMIME(netztyp) *) then
      Wrt2(getreps2(601,1,'PGP/MIME'))          { 'PGP/MIME moeglich' }
  end
  else begin
    attrtxt(col.coldiahigh);
    case sdata.docode of
       0 : Wrt2(getres2(601,3));
       1 : Wrt2('QPC');
       2 : Wrt2('DES');
       3..2+maxpmc: Wrt2(pmcode(sdata.docode));
       8 : Wrt2('PGP/MIME');
       9 : Wrt2('PGP');
      10 : Wrt2('Rot13');
    end;
  end;
  mon;
  freeres;
end;

procedure KorrCode;
begin
  if sdata.docode=0 then exit;
(*
  if sdata.parts.count<>1 then
  begin
    if ntMIME(netztyp) and (docode=9) then
      docode:=8         // switch PGP to PGP/MIME
    else
      docode:=0;        // switch off encoding
    showcode;
  end else // parts.count=1
    if (cancode=9) and (docode=8) then
    begin
      docode:=9;        // switch PGP/MIME to PGP
      showcode;
    end;
  *)
end;

procedure ShowLine(spezial:boolean);
begin
  attrtxt(col.coldialog);
  if spezial then begin
    mwrt(x+3,y+10,getres2(603,1));   { 'Spezial..  ' }
    mwrt(x+39,y+10,'               ');
    end (* 05.02.2000 MH: x+38 -> x+42 *) { unbedenklich }
  else begin
    mwrt(x+3,y+10,getres2(603,2));   { 'Absenden..' }
    mwrt(x+39,y+10,getres2(603,3));  { '�ndern..' }
    end;
end;

procedure ShowFlags;
var flags : string;

  function pmcc_anz:integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i:=1 to cc_anz do
      if IsMailAddr(cc^[i]) then
        Inc(Result);
  end;

begin
  if flCrash then Flags:='Crash '
  else flags:='';
  if sdata.flohnesig then flags:=flags+getres2(604,1);    { 'ohneSig '  }
  if flEB then flags:=flags+getres2(604,2);         { 'EmpfBest ' }
  if sdata.flLoesch then flags:=flags+getres2(604,3);     { 'l�schen '  }
  if flMnet then flags:=flags+getres2(604,4);       { '(MausNet) ' }
  if flMloc then flags:=flags+getres2(604,5);       { '(lokal) ' }
  if flNokop and (pmcc_anz>0) then
    flags:=flags+getres2(604,13);     { 'NOKOP ' }
  case msgprio of
    10 : flags:=flags+getres2(604,iif(length(flags)<20,6,7));  { 'Direktmail ' / 'Direkt ' }
    20 : flags:=flags+getres2(604,iif(length(flags)<20,8,9));  { 'Eilmail ' / 'Eilmail ' }
  end;

  { RFC: Gew�hlten X-Priority-Flag im Sendefenster anzeigen   }
  case RFCPrio of
     1 : Flags := Flags + GetRes2(604, 14);       { 'H�chste '}
     2 : Flags := Flags + GetRes2(604, 15);          { 'Hoch '}
     3 : Flags := Flags + GetRes2(604, 16);        { 'Normal '}
     4 : Flags := Flags + GetRes2(604, 17);       { 'Niedrig '}
     5 : Flags := Flags + GetRes2(604, 18);    { 'Niedrigste '}
  end;

 with sdata do if flPGPkey then flags:=flags+getres2(604,10);   { 'PGP-Key ' }

 (*
 with sdata do if flPGPsig then begin
    if((docode=8) or ((docode=0) and (cancode=8)) or
      ((docode=0) and (cancode=9) and (sdata.Parts.Count<>1))) and ntMIME(netztyp) then
      flags:=flags+getres2(604,19)    { 'PGP/MIME-Sig ' }
    else
      flags:=flags+getres2(604,11);   { 'PGP-Sig ' }
  end;
*)

 with sdata do if flPGPreq then flags:=flags+getres2(604,12);   { 'PGP-Req ' }
  attrtxt(col.coldiarahmen);
  if trim(flags)='' then
    mwrt(x+35,y2,dup(41,'�'))
  else { x+33 -> x+35                 34 -> 41 }
    mwrt(x+40,y2,RightStr(dup(39,'�')+' '+flags,36));
end;   { x+33 -> x+40                       37 -> 39          34 -> 36 }


{ Aufbau der MessageID  (hi..lo):
  16 bit   tag/monat/Jahr
  16 bit   laufender Z�hler
  12 bit   Zufallszahl
  16 bit   CRC �ber Username

  MausTausch:  2 Ziffern Tagesdatum      Fido:  13 bit  Datum
               6 Ziffern lfd. Z�hler            16 bit  laufender Z�hler
               2 Ziffern Zufallszahl             3 bit  Zufallszahl }

{ Es wird dei INT_NR des aktuellen Datensatzes der mbase verwendet }

{$IFDEF __undefined}
function MessageID(msgidtyp:integer):string;
const rev = 'C';   { Revision des MsgID-Algorithmus }
var inr     : longint;
    t,m,j   : smallword;
    h,mm,s,ss: smallword;
    dat     : xpWord;
    count   : xpWord;
    rand    : xpWord;
    csum    : xpWord;
    _domain : string;
    local_part : string[20];

begin
  if nomids or (msgidtyp=0) then
    MessageID:=''
  else begin
//  b64:='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$abcdefghijklmnopqrstuvwxyz-';
    decodedate(now,j,m,t);
    decodetime(now,h,mm,s,ss);
    dat:=(t-1)+(m-1)*32+(j mod 165)*32*12;
    dbRead(mbase,'INT_NR',inr);
    case msgidtyp of
      midMausNet : MessageID:=formi(t,2)+formi(inr mod 1000000,6)+    { MausTausch }
                     formi(random(100),2)+'@'+box;
      midFido : MessageID:=fidoAbsAdr+iifs(domain<>'','@'+domain,'')+' '+ { Fido }
                     LowerCase(hex(longint(dat and $1fff) shl 19+(inr and $ffff) shl 3
                          +random(8),8));   { eindeutig f. 16 Jahre, }
                                            { max. 65536 Msgs/Tag    }
      midProNet : MessageID:=iifc(pm,iifc(_bezug='','P','Q'),iifc(_bezug='','A','R'))
                     +'/'+formi(h,2)+formi(mm,2)+formi(inr mod 10000,4)+
                     formi(t,2)+formi(m,2)+formi(j,4)+'_013@'+box+';'+ { ProNet }
                     pointname+domain; { X/HHMMSSssDDMMYYYY_KKK@BOX;NR.pro }
    else begin
      count:=xpWord(inr and $ffff);
      rand:=random($1000);
      csum:=crc16strXP(username);
      (*
      case netztyp of
        nt_ZConnect : if fqdn='' then _domain:=rev+'@'+LowerCase(pointname+'.'+box)+domain
                        else _domain:=rev+'@'+fqdn;
        nt_UUCP     : if fqdn='' then _domain:=rev+'@'+pointname+domain
                        else _domain:=rev+'@'+fqdn;
      else
        if netztyp in netsRFC then
      *) 
          if fqdn='' then 
          begin
            if pointname<>'' then
            begin
              if box<>'' then
                _domain := rev+'@'+LowerCase(pointname+'.'+box)+domain
              else
                _domain := rev+'@'+pointname+domain
            end 
            else
              _domain:=rev+'@'+Mid(username,cPos('@',username)+1)
          end
          else
            _domain:=rev+'@'+fqdn;
      (*            
        else
          _domain:='@'+box;
      end;
      *)
      local_part:=b30(longint(dat) shl 14+count shr 2)+
                  b30(longint(count and 3) shl 28+longint(rand) shl 16 +csum);
      MessageID:=local_part+_domain;
      end;
    end;  { Case }
    end;
end;

procedure EncryptMessage(des:Boolean;var data:TStream);
var d2: TMemoryStream;
  _pwd: string[255];
   x,y: Integer;
   pos: smallword;
  show: boolean;

begin
  show:=des and (data.size>2000);
  if show then begin
    message(getres(605));    { 'DES-Codierung...     %' }
    x:=wherex-5; y:=wherey;
    end
  else begin
    x:=0;
    y:=0;
  end;

  pos:=0;

  if data is TMemoryStream then
    d2:=TMemoryStream(data)             // use as is
  else begin
    d2:=TMemoryStream.Create;           // make a copy in memory
    try d2.Size:=data.Size except end;
    data.Seek(0,soFromBeginning);
    CopyStream(data,d2);
    data.Free;
    data:=d2;
  end;

  if DES then
  begin
    DES_PW(passwd);
    DES_code(false,PChar(d2.Memory)^,0,d2.Size,d2.Size,x,y)
  end else
  begin
    _pwd:=passwd;
    QPC(false,PChar(d2.Memory)^,d2.Size,@_pwd,pos);
  end;

  if show then closebox;

  SetCryptFlag;
  hdp.betreff:=LeftStr(iifs(des,DES_ID,QPC_ID)+hdp.betreff,BetreffLen);
  hdp.archive:=false;
{  
  if hdp.Empfaenger.Count > 0 then
    hdp.Empfaenger[0] :=iifs(pm, empfaenger,mid(empfaenger,2));
}

  b:=cpos('@',hdp.absender);

   if not des then
     hdp.attrib:=hdp.attrib or AttrQPC;

   hdp.crypt.komlen:=hdp.komlen; hdp.komlen:=0;      { KOM anpassen   }
   hdp.crypt.typ:=hdp.typ;
   hdp.crypt.charset:=hdp.charset;
   hdp.crypt.method:=iifs(DES,'DES/XPOINT','QPC'); // OpenXP supports none of the ZConnect DES-Methods :-(
   hdp.charset:='';
   hdp.x_charset:='';
   hdp.typ:='B';

   hdp.MIME.ctype:='application/octet-stream';  // There's no MIME content
   hdp.MIME.encoding:=MimeEncodingBase64;       // type (did I mention that
                                                // we maintain this encoding
                                                // schemes for backward com-
                                                // patibility only?!)
   hdp.groesse:=data.size;
end;

procedure pmEncryptMessage(var data:TStream);
var fi,fo:string;
    fs:TStream;
    s:string;
    anal:TMimeAnalyzer;
begin
(*
  if pmcrypt[docode-2].binary and not ntBinary(netztyp) then begin
    rfehler(609);     { In diesem Netz sind keine Bin�rnachrichten m�glich. }
    exit;
  end;
*)

  fi:=temppath+uncryptedfile;
  fo:=temppath+cryptedfile;

  if FileExists(fo) then _era(fo);

  fs := TFileStream.Create(fi,fmCreate);
  hdp.WriteZ38(fs);
  CopyStream(data,fs);
  fs.Free;

  s:=pmcrypt[sdata.docode-2].encode;
  rps(s,'$KEY',passwd);
  rps(s,'$INFILE',fi);
  rps(s,'$OUTFILE',fo);
  rps(s,'$USER',hdp.FirstEmpfaenger);
  shell(s,500,3);                     { Nachricht codieren }

  if not FileExists(fo) then
    rfehler(603)    { 'Datei wurde nicht codiert!' }
  else
  begin
    fs:=TTemporaryFileStream.Create(fo,fmOpenRead);
    if not pmcrypt[sdata.docode-2].binary then anal:=TMimeAnalyzer.Create else anal:=nil;
    CopyStream(fs,anal);
(*
    if(pmcrypt[docode-2].binary or (anal.IsBinary or (not anal.EOLAllowed[MimeEOLCRLF])))
      and not ntBinary(netztyp) then
    begin
      fs.Free;
      rfehler(609);     { In diesem Netz sind keine Bin�rnachrichten m�glich. }
    end else
*)    
    begin
      SetCryptFlag;

      hdp.betreff:=PMC_ID+' by XP ('+pmcrypt[sdata.docode-2].name+') '+
        hex(hdp.groesse,6);
      hdp.crypt.method:='PMCRYPT2';
      hdp.crypt.komlen:=hdp.komlen; hdp.komlen:=0;      { KOM anpassen   }
      hdp.crypt.typ:=hdp.typ;
      hdp.crypt.charset:=hdp.charset;

      if(pmcrypt[sdata.docode-2].binary or (anal.IsBinary or (not anal.EOLAllowed[MimeEOLCRLF])))then
      begin
        hdp.typ:='B';
        hdp.mime.encoding:=MimeEncodingBase64;
      end else
      begin
        hdp.typ:='T';
        hdp.mime.encoding:=MimeEncodingQuotedPrintable;
      end;
      hdp.mime.ctype:='application/octet-stream';

      hdp.attrib:=hdp.attrib or AttrPmcrypt;

      data.Free;
      data:=fs;
      anal.Free;
    end;
  end;

  if FileExists(fi) then
    _era(fi);
end;
{$ENDIF}


function fileserver(var adresse:string):boolean;
var d      : DB;
    p,p2   : byte;
    fsname : string;
begin
  Result := false;
  p:=cpos('@',adresse);
  if p=0 then exit;
  p2:=p+cPos('.',Mid(adresse,p+1));
  if p2=0 then exit;
  dbOpen(d,SystemFile,siName);
  dbSeek(d,siName,copy(adresse,p+1,p2-p-1));
  if dbFound then fsname:= dbReadStr(d,'fs-name')
  else fsname:='';
  dbClose(d);
  fileserver:=LeftStr(adresse,p-1)=fsname;
end;

function QuoteOK:boolean;
var t     : text;
    buf   : array[0..2047] of byte;
    n,nq  : double;
    s     : string;
    x,y,i : Integer;
    lines : Integer;
    a     : taste;
    p     : byte;
begin
  QuoteOK:=true;
  assign(t,sdata.datei);
  settextbuf(t,buf);
  if not existf(t) or (trim(QChar)='') then exit;
  fm_ro; reset(t); fm_rw;
  n:=0; nq:=0;
  while not eof(t) do
  begin
    n := n + 1;
    readln(t,s);
    p:=cpos('>',s);
    if ((p>0) and (p<5)) or (LeftStr(s,length(QChar))=QChar) then
      nq := nq + 1;
    end;
  close(t);
  if nq / n >= 0.5 then
  begin
    lines:=ival(getres2(606,1));
    msgbox(51,lines+6,getres2(606,0),x,y);    { 'Hinweis' }
    for i:=1 to lines-1 do
      mwrt(x+3,y+i+1,getreps2(606,i+1,strsr(nq / n *100,0)));
    mwrt(x+3,y+lines+2,getres2(606,lines+1));
    errsound;
    a:='';
    n:=readbutton(x+3,y+lines+4,2,getres2(606,lines+2),2,true,a);   { '  ^Ja  , ^Nein ' }
    if (n=0) or (n=2) then begin
      QuoteOK:=false;
      if n=2 then keyboard(getres2(606,lines+3));  { 'T' }
      end;
    closebox;
    freeres;
  end;
end;


function SizeOK:boolean;
var t     : text;
    buf   : array[0..2047] of char;
    lines : longint;
begin
  SizeOK:=true;
  assign(t,sdata.datei);
  settextbuf(t,buf);
  if not existf(t) then exit;
  fm_ro; reset(t); fm_rw;
  lines:=0;
  while not eof(t) do begin
    inc(lines);
    readln(t);
    end;
  close(t);
  if lines>495 then begin
    fehler('Nachricht zu lang f�r ProNET - maximal 500 Zeilen erlaubt!');
    SizeOK:=false;
    end;
end;

procedure ParkMsg;
var f : file;
    i : integer;
  procedure wrs(s:string);
  begin
    s:=LeftStr(s,253)+#13#10;
    blockwrite(f,s[1],length(s));
  end;
begin
  if FileExists(sdata.datei) then begin
    assign(f,TempS(_filesize(fn)+200));
    rewrite(f,1);

    for i := 0 to sData.EmpfList.Count-1 do
      case sData.EmpfList[i].AddressType of
        atTo,atNewsgroup:
          wrs(getres2(600,1){oempf}+' '+sData.EmpfList[i].ZCAddress);
        atCC:
          wrs(getres2(600,2){oempf(cc)}+' '+sData.EmpfList[i].ZCAddress);
        atBCC:
          wrs(getres2(600,3){oempf(bcc)}+' '+sData.EmpfList[i].ZCAddress);
      end;
      
    wrs('');
    assign(f2,sdata.datei);
    reset(f2,1);
    fmove(f2,f);
    close(f2); close(f);
    erase(f2);
    rename(f,sdata.datei);
    if ioresult<>0 then begin
      rewrite(f2,1); reset(f,1);
      fmove(f,f2);
      close(f); close(f2);
      erase(f);
      end;
    end;
end;

function DateSend:boolean;
var brk      : boolean;
    x,y      : Integer;
    mind     : fdate;
    dd,mm,yy : smallword;
begin
  if AutoActive then    { d�rfte nicht vorkommen }
    DateSend:=false
  else begin
    decodedate(now,yy,mm,dd);
    mind.t:=dd; mind.m:=mm; mind.j:=yy;
    incd(mind);
    senddate:=formi(mind.t,2)+'.'+formi(mind.m,2)+'.'+formi(mind.j mod 100,2);
    min_send:=ixdispdat(senddate);xxx
    dialog(27,3,'',x,y);
    madddate(3,2,getres(607),senddate,false,false);   { 'absenden am ' }
    msetvfunc(test_senddate);
    readmask(brk);
    enddialog;
    DateSend:=not brk;
    end;
end;

procedure DateSendIt;
var dat   : longint;
    flags : xpWord;
    typ   : char;
    empf  : string;
    mon   : xpWord;
begin
  rmessage(608);    { 'Nachricht speichern...' }
  repeat
    fn:=SendPath+strs(random(10000))+'.MSG';
  until not FileExists(fn);
  if filecopy(sdata.datei,fn) then;
  dbOpen(auto,AutoFile,1);
  dbAppend(auto);
  dbWriteStr(auto,'dateiname',fn);
  dbWriteStr(auto,'betreff',sdata.Subject);
  empf:=sData.Empf1Address;
  dbWriteStr(auto,'empfaenger',empf);
  typ:=iifc(binary,'B','T');
  dbWrite(auto,'typ',typ);
  dbWriteStr(auto,'pollbox',box);
  dat:=IxDispdat(senddate);
  dbWrite(auto,'datum1',dat);
  mon:=$fff;
  dbWrite(auto,'monate',mon);
  flags:=3;   { aktiv, l�schen }
  dbWrite(auto,'flags',flags);
  dbClose(auto);
  closebox;
end;


procedure get_origbox;
var hdp : THeader;
    hds : longint;
begin
  Hdp := THeader.Create;
  try
    ReadHeader(hdp,hds,false);
    origbox:=pfadbox(ntZConnect(dbReadInt(mbase,'netztyp') and $ff),hdp.pfad);
  finally
    Hdp.Free;
  end;
end;

procedure SetLocalPM;
var l : byte;
begin
  { !! TODO: Geringstes Limit aller Addressen herausfinden }

//  lokalPM:=(RightStr(sData.Empf1Address,length(box)+4)=box+'.ZER') or
//           stricmp(RightStr(sData.Empf1Address,length(box)+length(domain)),box+domain) or
//           stricmp(RightStr(sData.Empf1Address,length(box)),box);
(*
  case netztyp of
    nt_Netcall,nt_ZCONNECT : l:=1;
    nt_Fido                : l:=2;
    nt_Maus                : l:=4;
    nt_Magic               : l:=5;
    nt_Quick,nt_GS         : l:=6;
  else
    if netztyp in netsRFC then
      l:=3
    else begin
      sdata.maxsize:=0; exit;      { nt_Pronet (s. SizeOK) }
      end;
  end;
*)  
  sdata.maxsize:=pmlimits[l,iif(false (* lokalPM *),2,1)];
end;

function steuerzeichen(var s:string):boolean;
var i : integer;
begin
  i:=1;
  while (i<=length(s)) and (s[i]>=' ') do inc(i);
  steuerzeichen:=(i<=length(s));
end;

(*
function Empferror:boolean;
var p  : byte;
    ee : string[40];
    fa : FidoAdr;
begin
  ee:='';
  p:=cpos('@',empfaenger);
  if p=0 then
    ee:=getres2(609,1)     { '"@" fehlt!' }
  else if (cPos(' ',LeftStr(empfaenger,p))>0) and not ntNameSpace(netztyp) then
    ee:=iifs(ReadJN(getres2(609,2),true),'','*')   { 'Warnung: Leerzeichen in Adresse! Trotzdem absenden' }
  else if steuerzeichen(empfaenger) then
    ee:=getres2(609,7)     { 'unerlaubte Steuerzeichen in Useradresse' }
  else
    case ntDomainType(netztyp) of
        0 : if RightStr(empfaenger,4)<>'.ZER' then begin
              errsound;
              ee:=iifs(ReadJN(getres2(609,3),false),'','*');   { '.ZER in Adresse fehlt! Trotzdem absenden' }
              end;
        4 : begin
              SplitFido(empfaenger,fa,DefaultZone);
              if fa.net=0 then ee:=getres2(609,4);   { 'Fido-Netznummer fehlt!' }
            end;
        5 : if cPos('.',mid(empfaenger,p+1))=0 then ee:=getres2(609,5);  { 'Domain fehlt!' }
    end;
  if (ee<>'') and (ee<>'*') then
    afehler(getreps2(609,6,ee),not sendbox);   { 'fehlerhafter Empf�nger: %s' }
  EmpfError:=(ee<>'');
end;
*)

(*
function FidoAdrOK(testbox:boolean):boolean;
var ni : NodeInfo;
    fa : fidoadr;
begin
  GetNodeinfo(empfaenger,ni,2);
  if not ni.found then begin
    if testbox then rfehler(2116);    { 'Unbekannte Nodeadresse! }
    FidoAdrOk:=false;
    end
  else begin
    splitfido(empfaenger,fa,DefaultZone);
    if testbox then
      if ni.ispoint then
        ShrinkPointToNode(fa,ni)
      else
        fa.ispoint:=false;
    if testbox and IsBox(MakeFidoAdr(fa,true)) then begin
      rfehler(604);   { 'Nachricht an Serverbox bitte als normale Mail absenden' }
      FidoAdrOK:=false;
      end
    else
      FidoAdrOK:=true;
    end;
end;
*)

function CrashAdr:string;
var fa : FidoAdr;
    ni : NodeInfo;
begin
  GetNodeinfo(hdp.FirstEmpfaenger,ni,2);
  splitfido(hdp.FirstEmpfaenger,fa,DefaultZone);
  fa.ispoint:=ni.ispoint;
  ShrinkPointToNode(fa,ni);
  CrashAdr:=MakeFidoAdr(fa,true);
end;

procedure EditSdata;
var x,y : Integer;
    brk : boolean;
    asc : string[120];
    sml : string[1];
    tmpFollowUp: string;
begin
(*
  if netztyp=nt_ZConnect then
    y:=iif(pm,5,9)
  else if netztyp in netsRFC then
    y:=iif(pm,5,11);
  dialog(ival(getres2(616,0)),y,getres2(616,6),x,y);   { 'Zus�tzliche Informationen' }
  y:=2;
  asc:=range(' ',#126);
  with sData do
  begin
    if not pm then begin
      if ntGrossBrett(netztyp) then sml:='>'
      else if ntKleinBrett(netztyp) then sml:='<'
      else sml:='';
      if followup.count>0 then
        tmpFollowUp := followup[0]
      else
        tmpFollowUp := '';
      maddstring(3,y,getres2(616,1),tmpFollowUp,40,eAdrLen,sml+asc); mhnr(850);
      mappcustomsel(selbrett,false);        { 'Brettantworten in' }
      mset3proc(firstslash);
      inc(y,2);
      end;
    maddstring(3,y,getres2(616,5),ReplyTo,40,eAdrLen,asc); mhnr(851);
    mappcustomsel(seluser,false);           { 'PM-Antworten an  ' }
    msetvfunc(testReplyTo);
    inc(y,2);
    maddstring(3,y,getres2(616,2),keywords,40,60,       { 'Stichworte' }
               iifs(ntHeaderUmlaut(netztyp),'',asc)); mhnr(852);
    inc(y,2);
    if not pm then begin
      maddstring(3,y,getres2(616,3),summary,40,200,  { 'Zusammenfassung' }
               iifs(ntHeaderUmlaut(netztyp),'',asc));   { max. L�nge: 200! }
      inc(y,2);                                 { (wg. RFC-1522-Codierung) }
      if netztyp in netsRFC then begin
        maddstring(3,y,getres2(616,4),distribute,40,40,asc); { 'Verbreitung' }
        inc(y,2);
        end;
      end;
    end;
  readmask(brk);
  if not brk then
  with sData do
    if FollowUp.Count > 0 then
      FollowUp[0] := tmpFollowUp
    else
      if tmpFollowUp <> '' then FollowUp.Add(tmpFollowUp);
  enddialog;
*)  
end;

procedure SendPGPOptions;
var brk : boolean;
    x,y: Integer;
begin
 with sdata do if not UsePGP then
    rfehler(633)    { 'Aktivieren Sie zuerst PGP unter /Config/Extern/PGP!' }
  else
    if not has_zc then
       flPGPsig:=not flPGPsig
       
    else begin
      dialog(ival(getres2(645,0)),5,getres2(645,1),x,y); { 'PGP-Optionen' }
      maddbool(3,2,getres2(645,2),flPGPsig);             { 'Nachricht signieren' }
        mhnr(1020);
        mset1func(pgpo_sigtest);
      maddbool(3,3,getres2(645,3),flPGPreq);         { 'Public Key anfordern' }
        if not pm then mdisable;
      maddbool(3,4,getres2(645,4),flPGPkey);         { 'eigenen Key verschicken' }
        mset1func(pgpo_keytest);
      readmask(brk);
      enddialog;
      end;
end;

procedure SetXpointCtl;
var sum : longint;
    i   : integer;
begin
  with hdp do
    if FileExists(LeftStr(datum,6)+'.345') and stricmp(datei,SupportCfg) then begin
      sum:=0;
      for i:=1 to length(datum) do
        inc(sum,ord(datum[i])*7);
      for i:=1 to length(msgid) do
        inc(sum,ord(msgid[i])*3);
      XpointCtl:=sum*1000+random(1000);
      end;
end;

{ Dialog: Priorit�t ermitteln und in glob. Variable RFCPrio schreiben }
{ RFC: X-Priority: 1, 2, 3, 4, 5 }
procedure GetPrio;
var
  x, y: Integer;
  i: Integer;
  brk: boolean;
  Prio: string;
begin                                                  { 'Sende-Priorit�t' }
  Dialog(ival(getres2(272, 0)), 3, getres2(272, 7), x, y);
  Prio := GetRes2(272, iif(rfcprio=0, 6, rfcprio));
  MaddString(3, 2, GetRes2(272,8), Prio, 10, 10, '');       { 'Priorit�t ' }
  MappSel(false, GetRes2(272, 9));
  ReadMask(brk);          { 'H�chste�Hoch�Normal�Niedrig�Niedrigste�Keine' }

  if not brk and mmodified then
  begin
    RFCPrio := 0;                         { 'keine' }
    for i := 1 to 5 do
      if Prio = GetRes2(272, 1) then
      begin
        RFCPrio := i;
        break;
      end;
  end;
  EndDialog;
end;

function getBrettUserPollBox (brett :string) :string;
var box :string;
begin
  box := '';
  brett := dbReadNStr(mbase, mb_brett);
  if FirstChar(brett) = '1' then         { Brett }
  begin
    dbSeek (bbase, biIntNr, copy (brett, 2, 4));
    if dbBOF (bbase) or dbEOF (bbase) then box := ''
    else box := dbReadNStr (bbase, bb_pollbox);
  end;
  if not isBox (box) then box := '';
  getBrettUserPollBox := box;
end;

{$ENDIF}

{
  $Log: xpsendmessage_subs.inc,v $
  Revision 1.38  2003/10/06 16:01:35  mk
  - some little code optimizations (mostly added const parameters and
    use of new file system RTL functions)

  Revision 1.37  2003/09/21 16:10:30  mk
  - added try finally block in get_origbox

  Revision 1.36  2003/09/06 23:03:08  cl
  - send window - time-shifted sending of message
    cLOSES task #76792: Sendefenster: Datum

  Revision 1.35  2003/08/30 22:19:27  cl
  - send window: select encryption and signature method
  - CLOSES Task #76790 Sendefenster: Kodieren/Sicherheit

  Revision 1.34  2003/05/11 11:12:19  mk
  - use IsMailAddr when possible

  Revision 1.33  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.32  2002/12/21 05:38:02  dodi
  - removed questionable references to Word type

  Revision 1.31  2002/12/14 07:31:40  dodi
  - using new types

  Revision 1.30  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.29  2002/07/31 19:26:21  ma
  - user=>email db field code synchronized with v3.8
    (does not need re-entering email address when upgrading from old
     versions now)

  Revision 1.28  2002/07/29 07:17:21  mk
  - fixed AnsiString[1] to FirstChar(AnsiString)

  Revision 1.27  2002/07/28 11:31:46  cl
  - BUGFIX: [ 587626 ] 3.9: EBs verschandeln Subject
  - BUGFIX: [ 587388 ] 3.9: EBs gehen nicht immer

  Revision 1.26  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.25  2002/07/20 15:36:22  cl
  - BUGFIX: Doppelte Signatur beim nachtr�glichen Editieren

  Revision 1.24  2002/07/09 13:37:20  mk
  - merged forcebox-fixes from OpenXP/16 (sv+my), most obsolte due to new adress handling

  Revision 1.23  2002/06/23 15:03:07  cl
  - Adapted Nachricht/Direkt to new address handling.

  Revision 1.22  2002/06/12 09:14:53  mk
  - removed some length limits including AdressLength (for RFC nets only)

  Revision 1.21  2002/05/26 12:26:12  ma
  - using "email" db field instead of "user" db field for email now
    email may be longer than 30 chars now
    EMAIL ADDRESS HAS TO BE RE-ENTERED IN SERVER SETTINGS

  Revision 1.20  2002/05/05 15:10:59  ma
  - fixed: ReplyTo not correctly set with role feature (untested)

  Revision 1.19  2002/04/30 08:53:37  mk
  - fixed bug: f'ups where not saved in EditSdata

  Revision 1.18  2002/04/14 22:33:10  cl
  - New address handling, supports To, CC, and BCC
  - Nearly complete rewrite of DoSend's message creation
  - Added TAddress and TAddressList
  - Moved many local variables from DoSend into TSendUUData fields

  Revision 1.17  2002/02/21 13:52:34  mk
  - removed 21 hints and 28 warnings

  Revision 1.16  2002/01/13 15:15:54  mk
  - new "empfaenger"-handling

  Revision 1.15  2002/01/05 16:01:10  mk
  - changed TSendUUData from record to class

  Revision 1.14  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.13  2001/12/23 10:07:36  cl
  - file size shown correctly

  Revision 1.12  2001/10/26 01:22:17  ma
  - fixed: Signature line endings were not correct with Unix
    (should be CRLF)

  Revision 1.11  2001/10/17 10:19:52  mk
  - decode/docode byte instead of ShortInt

  Revision 1.10  2001/09/16 23:01:20  cl
  - BUGFIX: Fido tearline now added

  Revision 1.9  2001/09/12 21:22:12  cl
  - added flag PGP/MIME-Sig

  Revision 1.8  2001/09/08 18:46:43  cl
  - small bug/compiler warning fixes

  Revision 1.7  2001/09/08 14:42:09  cl
  - added Multipart-MIME support
  - added PGP/MIME support
  - adaptions/fixes for MIME support
  - adaptions/fixes for PGP/MIME support

  Revision 1.6  2001/09/07 13:54:25  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.5  2001/09/07 09:16:49  mk
  - fixed AnsiString-Handling for getBrettUserPollBox

  Revision 1.4  2001/09/06 19:31:20  mk
  - removed some hints und warnings

  Revision 1.3  2001/08/29 19:50:47  ma
  - changes in net type handling (2)
  - shortened CVS logs

  Revision 1.2  2001/08/23 11:15:04  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.1  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.60  2001/08/11 23:06:35  mk
  - changed Pos() to cPos() when possible

  Revision 1.59  2001/07/31 16:18:41  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.58  2001/07/31 13:10:33  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.57  2001/07/27 18:10:14  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.56  2001/07/23 16:05:21  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.55  2001/06/04 17:40:05  ma
  - implemented role feature
  - fixed: MsgId was not correct if no FQDN specified with NNTP/SMTP servers
  - removed user defined hint

  Revision 1.54  2001/05/27 09:31:19  ma
  - enabled PGP and some other things for NNTP/POP3/IMAP

  Revision 1.53  2001/05/19 16:12:53  ma
  - removed XP_ID (shareware notice)

  Revision 1.52  2001/04/17 01:03:03  ma
  - enabled FQDN with NNTP/... server type

  Revision 1.51  2001/03/14 20:46:05  mk
  - removed registration routines
}

{   $Id$

    Copyright (C) 2000-2001 OpenXP team <http://www.openxp.de>
    Copyright (C) 2001 Claus F"arber <http://www.faerber.muc.de>

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

{$I XPDEFINE.INC }

unit xpextractmessage;

{ --------------------------- } interface { -------------------------- }

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,database,resource,stack, xpheader, winxp,
  xp0,xp1,xpglobal, classes;

const xTractMsg   = 0;
      xTractHead  = 1;
      xTractHeadOnly = 5;
      xTractPuf   = 2;
      xTractQuote = 3;
      xTractDump  = 4;

      ExtCliptearline : boolean = true;
      ExtChgtearline  : boolean = false;

procedure rps(var s:string; s1,s2:string);
procedure rpsuser(var s:string; name:string; var realname:string);
procedure rpsdate(var s:string);
//procedure ExtractSetMpdata(mpdata:pointer);

procedure extract_msg(art:byte; schablone:string; name:string;
                      append:boolean; decode:shortint);
                      
function ExtractHeader(hdp:THeader;outs:TObject):Integer;
                      
{ ------------------------ } implementation { ------------------------ }

uses xp1o,xp3,xp_des,xpnt,xpfido,xpextractmessage_mime,xpmakeheader,xpstreams,lister;

//var  ex_mpdata : pmpdata;

procedure rps(var s:string; s1,s2:string);
var p : byte;
begin
  repeat
    p:=pos(s1,UpperCase(s));
    if p>0 then
      s:=LeftStr(s,p-1)+s2+mid(s, p + length(s1));
  until p=0;
end;

procedure rpsuser(var s:string; name:string; var realname:string);
var p,p2 : byte;
    komm : string[40];
    vorn : boolean;
begin
{ if _unescape(name) then; }
  vorn:=false;
  rps(s, '$FIRSTNAME', '$VORNAME');
  p:=pos('$PSEUDO',UpperCase(s));
  if p=0 then begin
    vorn:=true;
    p:=pos('$VPSEUDO',UpperCase(s));
    end;
  if p>0 then begin
    dbSeek(ubase,uiName,UpperCase(name));
    if not dbFound then komm:=''
    else komm:= dbReadNStr(ubase,ub_kommentar);
    p2:=pos('P:',UpperCase(komm));
    if p2=0 then
      s:=copy(s,1,p-1)+iifs(vorn,'$VORNAME','$TUSER')+Mid(s,p+iif(vorn,8,7))
    else
      s:=copy(s,1,p-1)+trim(mid(komm,p2+2))+Mid(s,p+iif(vorn,8,7));
    end;
  name:=vert_name(name);
  rps(s,'$USER',name);
  rps(s,'$NAME',name);
  if realname<>'' then begin
    p:=blankpos(realname);
    if p=0 then rps(s,'$VORNAME',realname)
    else rps(s,'$VORNAME',LeftStr(realname,p-1));
    end
  else begin
    p:=blankpos(name);
    if p>0 then rps(s,'$VORNAME',LeftStr(name,p-1))
    else if cpos('@',name)=0 then
      rps(s,'$VORNAME',TopAllStr(name))
      else rps(s,'$VORNAME',TopAllStr(LeftStr(name,cpos('@',name)-1)));
    end;
  p:=cpos('%',name);
  if p=0 then p:=cpos('@',name);
  if p>0 then begin
    rps(s,'$MUSER',LeftStr(name,p-1));
    rps(s,'$TUSER',TopAllStr(LeftStr(name,p-1)));
    if UpperCase(RightStr(name,4))='.ZER' then
      delete(name, length(name)-4, 4); { dec(byte(name[0]),4);}
    rps(s,'$BOX',mid(name,p+1));
    end
  else begin
    rps(s,'$MUSER',name);
    rps(s,'$TUSER',TopAllStr(name));
    rps(s,'$BOX','');
    end;
end;

procedure rpsdat(var s:string; txt:string; d:datetimest);
begin
  rps(s,txt,LeftStr(d,2)+' '+copy('JanFebMarAprMayJunJulAugSepOctNovDec',
            ival(copy(d,4,2))*3-2,3)+' '+RightStr(d,2));
end;

procedure rpsdate(var s:string);
begin
  rps(s, '$DAY2', '$TAG2');
  rps(s, '$DAY', '$TAG');
  rps(s, '$TIME', '$UHRZEIT');
  rps(s,'$DATUM',LeftStr(date,6)+RightStr(date,2));
  if pos('$DATE',s)>0 then
    rpsdat(s,'$DATE',date);
  rps(s,'$UHRZEIT',LeftStr(time,5));
  rps(s,'$TAG2',LeftStr(zdow(zdate),2));
  rps(s,'$TAG',zdow(zdate));
end;

{ Header extrahieren }

function ExtractHeader(hdp:THeader;outs:TObject):Integer;
var i,p    : Integer;
    s,hs   : string;
    hdln   : Integer;      
    count  : Integer;
    lasttrenn:boolean;
    edat   : Integer;
    ni     : NodeInfo;
    First  : Boolean;

  function MausStat(s:string):string;
  begin
    result:=copy(s,8,2)+'.'+copy(s,6,2)+'.'+copy(s,2,4)+' um '+
         copy(s,10,2)+':'+copy(s,12,2);
    case s[1] of
      'N' : result:='noch nicht gelesen';
      'Z' : result:='zurÅckgestellt am '+result;
      'B' : result:='beantwortet am '+result;
      'G' : result:='erhalten/gelesen am '+result;
      'W' : result:='weitergeleitet am '+result;
      'M' : result:='im MausNet seit '+result;
      'A' : result:='angekommen am '+result;
      'Y' : result:='angekommen beim Gateway am '+result;
      'T' : result:='im Tausch seit '+result;
    else    result:='unbekannter Status '+s[1]+' ('+result+')';
    end;
  end;

  function MausName(s:string):string;
  var p: Integer;
  begin
    p:=cpos('@',s);
    if (p=0) or ((hdp.netztyp<>nt_Maus) and (hdp.netztyp<>nt_Fido)) then
      mausname:=s
    else
      mausname:=trim(LeftStr(s,p-1))+' @ '+trim(mid(s,p+1));
  end;

  function GetStatus:string;
  begin
    result:='';
    with hdp do begin
      if attrib and attrCrash<>0 then result:=result+', Crash';
      if attrib and attrFile<>0  then result:=result+', File-Attach';
      if attrib and attrReqEB<>0 then result:=result+getres2(363,1);  { ' EB-Anforderung' }
      if attrib and attrIsEB<>0  then result:=result+getres2(363,2);  { ' Empfangsbestaetigung' }
      if attrib and attrControl<>0   then result:=result+getres2(363,3); { ' Steuernachricht' }
      freeres;
      delete(result,1,2);
    end;
  end; 

  function GetPgpStatus:string;
  var flags : longint;
  begin
    result:='';
    dbReadN(mbase,mb_flags,flags);
    with hdp do begin
      if pgpflags and fPGP_avail<>0  then result:=result+getres2(363,4); { 'PGP-Key vorhanden' }
      if pgpflags and fPGP_haskey<>0 then result:=result+getres2(363,5); { 'Nachricht enthaelt PGP-Key' }
      if pgpflags and fPGP_request<>0 then result:=result+getres2(363,6); { 'PGP-Keyanforderung' }
      if pgpflags and (fPGP_signed+fPGP_clearsig)<>0 then
        result:=result+getres2(363,9);  { 'PGP-Signatur vorhanden' }
      if (pgpflags and fPGP_sigok<>0) or (flags and 1<>0) then
        result:=result+getres2(363,7);  { 'PGP-Signatur o.k.' }
      if (pgpflags and fPGP_sigerr<>0) or (flags and 2<>0) then
        result:=result+getres2(363,8);  { 'ungueltige PGP-Signatur!' }
      freeres;
      delete(result,1,2);
    end;
  end;
  
  function Telestring(s:string):string;
  var ts    : string;
      tn,vs : string;
  begin
    s:='˘'+s;
    if not testtelefon(s) then
      telestring:=s+getres2(361,50)    { ' [ungueltiges Format]' }
    else 
    begin
      ts:='';
      repeat
        tn:=UpperCase(GetToken(s,' '));
        vs:='';
        while (tn<>'') and (tn[1]>'9') do begin
          case tn[1] of
            'V' : vs:=vs+', '+getres2(361,51);  { 'Voice' }
            'F' : vs:=vs+', '+getres2(361,52);  { 'Fax' }
            'B' : vs:=vs+', '+getres2(361,53);  { 'Mailbox' }
            'P' : vs:=vs+', '+getres2(361,54);  { 'City-Ruf' }
          end;
          DeleteFirstChar(tn);
          end;
        if lastchar(tn)='Q' then
          insert(' ',tn,length(tn));
        if cpos('-',vorwahl)>0 then
          if LeftStr(tn,cpos('-',tn))='+'+LeftStr(vorwahl,cposx('-',vorwahl)) then
            tn:=NatVorwahl+mid(tn,cpos('-',tn)+1)
          else
            if firstchar(tn)='+' then
              tn:=IntVorwahl+mid(tn,2);
        delete(vs,1,2);
        ts:=ts+', '+tn+iifs(vs<>'',' ('+vs+')','')
      until s='';
      telestring:=mid(ts,3);
    end;
  end;
     
  procedure wrs(s:string);
  begin
    if outs is TStream then
      writeln_s(TStream(outs),s)
    else (* if First then
      TLister(outs).FixedLines.Add(s)
    else *)
      TLister(outs).Lines.Add(s);
      
    inc(count);
    lasttrenn:=false;
  end;

  procedure FoldCommaSeparateList(Prefix:String;list:TStringList);
  var s:string;
      i:Integer;
  begin
    if list.count<=0 then exit;
    s:=Prefix+List[0];

    for i:=1 to List.Count-1 do
      if length(s)+length(List[i])>iif(listscroller,76,77) then
      begin
        wrs(s);
        s:=Prefix+List[i];
      end else
      begin
        s:=s+', '+List[i];
      end;
   wrs(s);
 end;

  procedure FoldText(Prefix,s:String);
  var p,lr:Integer;
  begin
    if Length(s)<=0 then exit;
  
    p:=0;
    repeat                               { langen Betreff umbrechen }
      if p=1 then 
        Prefix:=Sp(Length(Prefix));
      
      lr:=rightpos(' ',leftStr(s,78-Length(prefix)));
      if (lr=0) or (length(s)<=78-Length(prefix)) then 
      lr:=78-Length(prefix);
      wrs(Prefix+leftStr(s,lr));
      inc(p);
      s:=mid(s,lr+1);
      inc(p);
    until s='';
  end;
    
begin
  Assert((outs is TStream) or (outs is TLister));
  First:=(outs is TLister) and (TLister(outs).Lines.Count<=0);

  Count:=0;
  dbReadN(mbase,mb_EmpfDatum,edat);
  for hdln:=0 to ExtraktHeader.anz  do
    case ExtraktHeader.v[hdln] of
      hdf_Trenn  : if not lasttrenn then 
                   begin                     { Trennzeile }
                     if( length(VarLister) <> 0 ) then             { wenn externer Lister verwendet wird }
                       wrs(dup(72,'-'))
                     else
                       wrs(dup(ScreenWidth-2,'ƒ'));       { interner Lister }
                   lasttrenn:=true;
                   end;
      hdf_EMP    : begin
                     s:=GetRes2(361,2)+hdp.empfaenger;
                     for i:=0 to Hdp.Xempf.Count-1 do
                       if length(s)+length(Hdp.XEmpf[i])>iif(listscroller,76,77) then
                       begin
                         wrs(s);
                         s:=GetRes2(361,2)+Hdp.XEmpf[i];
                       end else
                       begin
                         s:=s+', '+Hdp.XEmpf[i];
                       end;
                     if hdp.fido_to<>'' then 
                     begin
                       if length(s)+length(hdp.fido_to)>iif(listscroller,75,76) then
                       begin
                         wrs(s);
                         s:=GetRes2(361,2)+'('+hdp.fido_to+')';
                       end else
                       begin
                         s:=s+' ('+hdp.fido_to+')';
                       end;
                     end;
                     wrs(s);
                   end;                     
      hdf_KOP    : FoldCommaSeparateList(GetRes2(361,28),hdp.kopien);
      hdf_DISK   : FoldCommaSeparateList(GetRes2(361,3),hdp.followup);
      hdf_ABS    : begin
                     if ((hdp.netztyp=nt_fido) or (hdp.netztyp=nt_QWK)) and
                        (hdp.realname='') and
                        (length(hdp.absender)<54) and Nodelist.Open and
                        (cpos(':',hdp.absender)>0) then 
                     begin              { sieht nach einer Fido-Adresse aus ... }
                       GetNodeinfo(hdp.absender,ni,0);
                       if ni.found then 
                       begin
                         hdp.realname:=LeftStr(ni.boxname,60-length(hdp.absender));
                         if length(hdp.absender)+length(hdp.realname)+length(ni.standort)<60 then
                           hdp.realname:=hdp.realname+', '+ni.standort;
                       end;
                     end;
                     wrs(GetRes2(361,6)+mausname(hdp.absender)+     { 'Absender   : ' }
                       iifs(hdp.realname<>'','  ('+hdp.realname+')',''));
                   end;
      hdf_OEM    : FoldCommaSeparateList(GetRes2(361,16),hdp.oem);        { 'Org.-Empf. : ' }          
      hdf_OAB    : FoldText(GetRes2(361,18),hdp.oab+iifs(hdp.oar<>'','  ('+hdp.oar+')',''));            
      hdf_WAB    : FoldText(GetRes2(361,17),hdp.wab+iifs(hdp.war<>'','  ('+hdp.war+')',''));            
      hdf_ANTW   : if UpperCase(Hdp.ReplyTo) <> UpperCase(hdp.absender) then   { 'Antwort an : ' }
                     FoldText(GetRes2(361,27),hdp.ReplyTo);
      hdf_BET    : FoldText(GetRes2(361,5),hdp.betreff);
      hdf_ZUSF   : FoldText(GetRes2(361,23),hdp.summary);            
      hdf_STW    : FoldText(GetRes2(361,22),hdp.keywords);
      hdf_ROT    : if hdp.pfad<>'' then
                   begin
                     s:=hdp.pfad;
                     hs:=GetRes2(361,7);                    
                      while s<>'' do 
                     begin
                       p:=length(s);
                       if p+length(hs)>79 then 
                       begin
                         p:=79-length(hs);
                         while (p>30) and (s[p]<>'!') and (s[p]<>' ') do
                           dec(p);
                         if p=30 then p:=79-length(hs);
                       end;
                       wrs(hs+LeftStr(s,p));
                       delete(s,1,p);
                       hs:=GetRes2(361,15);                 { sp(...) }
                     end;
                   end;
      hdf_MID    : FoldText(GetRes2(361,8),Hdp.MsgID);                { 'Message-ID : ' }
      hdf_BEZ    : if hdp.References.Count > 0 then            { 'Bezugs-ID  : ' }
                     wrs(GetRes2(361,19)+hdp.LastReference+iifs(hdp.References.Count=1,'',', ...'));
      hdf_EDA    : wrs(GetRes2(361,9)+iifs(hdp.Datum='','N/A',copy(zdow(hdp.datum),1,2)+' ' { 'Datum' }
                     +fdat(hdp.datum)+', '+ftime(hdp.datum))
                     +iifs(hdp.datum<>longdat(edat),'  ('+GetRes2(361,10)
                     +fdat(longdat(edat))+', '+ftime(longdat(edat))+')','')); { 'erhalten: ' }
      hdf_LEN    : begin
//                   sizepos:=outs.Position;
                     wrs(reps(GetRes2(361,11),strs(hdp.groesse)));  { 'Groesse    : %s Bytes' }
                   end;
      hdf_MAILER : FoldText(GetRes2(361,20),hdp.programm);    { 'Software   : ' }
      hdf_ORG    : FoldText(GetRes2(361,24),hdp.organisation);   { 'Organisat. : ' }
      hdf_POST   : FoldText(GetRes2(361,25),hdp.postanschrift);  { 'Postadresse: ' }
      hdf_TEL    : FoldText(GetRes2(361,26),telestring(hdp.telefon));  { 'Telefon    : ' }
      hdf_FILE   : FoldText(GetRes2(361,12),hdp.datei);   { 'Dateiname  : ' }
      hdf_MSTAT  : if (hdp.pm_bstat<>'') and (hdp.pm_bstat[1]<>'N') then
                     FoldText(GetRes2(361,3),mausstat(hdp.pm_bstat));     { 'PM-Status  : ' }
      hdf_STAT   : FoldText(GetRes2(361,21),GetStatus);  { 'Status:    : ' }
      hdf_PGPSTAT: FoldText(GetRes2(361,29),GetPgpStatus);  { 'PGP-Status : ' }
      hdf_ERR    : FoldText(GetRes2(361,14),hdp.error);                  { 'Fehler!    : ' }
      hdf_DIST   : FoldText(GetRes2(361,31),hdp.distribution);           { 'Distribut. : ' }
      hdf_Homepage:FoldText(GetRes2(361,32),hdp.homepage);              { 'Homepage   : ' }
//    hdf_Part   : if multipart and (mpdata.part>0) then
//                   wrs(GetRes2(361,33)+strs(mpdata.part)+           { 'Teil       : ' }
//                     GetRes2(361,34)+strs(mpdata.parts));         { ' von ' }
      hdf_Cust1  : if mheadercustom[1]<>'' then if hdp.Cust1<>'' then 
                     FoldText(LeftStr(mheadercustom[1],11)
                       +sp(Max(0,11-Length(mheadercustom[1])))+': ',hdp.Cust1);
      hdf_Cust2  : if mheadercustom[2]<>'' then if hdp.Cust2<>'' then 
                     FoldText(LeftStr(mheadercustom[2],11)
                       +sp(Max(0,11-Length(mheadercustom[2])))+': ',hdp.Cust2);
      hdf_Priority:if hdp.Priority in [1..5] then
                     wrs(GetRes2(361,35) + GetRes2(272, Hdp.Priority))
                   else if hdp.Prio>0 then                                 { und fuer Zconnect ....  }
                     if hdp.Prio<=10 then 
                       wrs(GetRes2(361,35) + GetRes2(604, 6))    { Direktmail }
                     else 
                       wrs(GetRes2(361,35) + GetRes2(604, 8));   { Eilmail }
    end; // case
  Result:=Count;
end;  

{ Aktuelle Nachricht in Tempfile extrahieren       }
{ art: 0=ohne Kopf, 1=mit Kopf, 2=Puffer, 3=Quote  }
{      4=Hex-Dump                                  }
{ decode: 0=nicht, -1=Rot13, 1=Betreff analysieren }

procedure extract_msg(art:byte; schablone:string; name:string;
                      append:boolean; decode:shortint);

  procedure Dump;
  var os: TFileStream;
      ip: TStream;
      hd: THeader;
      hds: integer;
      buf: array[0..15] of char;
      rr,p,i: Longint;
      s: string;
      h: string[2];
  begin
    hd := THeader.Create;
    try
      ReadHeader(hd,hds,true);
      if hds=1 then exit;   
    
      os:=TFileStream.Create(name,iif(append,fmOpenReadWrite,fmCreate));
      try
        if append then
          os.Seek(soFromEnd,0);

        ExtHdLines:=ExtractHeader(hd,os);
      
        ip:=XReadCreateStream(0);
        try
          p:=0;
          repeat
            rr := ip.Read(buf,sizeof(buf));
            s:=hex(p,8)+':'+sp(1+2+(3*16)+1+16+1);
            for i:=0 to Min(rr,Sizeof(buf))-1 do
            begin
              h:=hex(Ord(buf[i]),2);
              s[9+2+(i*3)+iif(i>=8,1,0)]   := h[1];
              s[9+2+(i*3)+iif(i>=8,1,0)+1] := h[2];
              s[10+2+(3*16)+1+i+iif(i>=8,1,0)]:= iifc(buf[i] in [#0..#31],'˙',buf[i]);
            end;
            writeln_s(os,s);            
            inc(p,rr);            
          until rr<sizeof(buf);
        finally
          ip.Free;
        end;
      finally  
        os.Free;
      end;
    finally
      hd.Free;
    end;
  end;                       

  procedure HeadOnly;
  var os: TFileStream;
      hd: THeader;
      hds: integer;
  begin
    hd := THeader.Create;
    try
      ReadHeader(hd,hds,true);
      if hds=1 then exit;   
    
      os:=TFileStream.Create(name,iif(append,fmOpenReadWrite,fmCreate));
      try
        if append then
          os.Seek(soFromEnd,0);
        ExtHdLines:=ExtractHeader(hd,os);
      finally  
        os.Free;
      end;
    finally
      hd.Free;
    end;
  end;                       
                      
begin
  case art of 
    xTractMsg,
    xTractHead, 
    xTractQuote: ;
    
    xTractPuf: 
      Xread(name,append);

    xTractHeadOnly:
      HeadOnly;
      
    xTractDump:
      Dump;
  end;
end;

{$IFDEF __undefined__}

procedure extract_msg(art:byte; schablone:string; name:string;
                      append:boolean; decode:shortint);
var size   : longint;
    f,decf : file;
    hdp    : THeader;
    hds    : longint;
    edat   : longint;
    tmp    : string;
    t      : text;
    s      : string;
    hs     : string[25];
    i,hdln : integer;
    p, ln, lr: Integer;
    _brett : string;
    extpos : longint;
    wempf  : string;
    ni     : NodeInfo;
    hdlines: longint;
    mstatus: string[80];
    iso1   : boolean;    { charset: ISO1 }
    lasttrenn : boolean;
//    mpdata : multi_part;
    multipart : boolean;
    sizepos : longint;
    mpsize  : longint;
    mehdl, mehds : integer;
    TempKopien: TStringList;
    QuoteEmptyLines: boolean;

  procedure wrs(s:string);
  begin
    s:=LeftStr(s,ScreenWidth)+#13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if LeftStr(s,5)<>'-----' then lasttrenn:=false;
  end;

  procedure wrslong(s:string);
  begin
    s:=s+#13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if LeftStr(s,5)<>'-----' then lasttrenn:=false;
  end;

  { dtyp: -1=Rot13, 1=QPC, 2=DES }

  procedure do_decode(dtyp:shortint; ofs:longint);
  var p     : pointer;
      ps: word;
      rr: word;
      fp    : longint;
      pw    : string;
      coder : byte;
      siz0  : integer;
      passpos : smallword;
      show  : boolean;
      x,y   : byte;
      _off  : longint;
      total : longint;
  begin
    y := 0;
    if size>0 then begin
      if (dtyp>=1) then begin
        if FirstChar(_brett)<>'U' then
          dbSeek(ubase,uiName,UpperCase(hdp.absender))
        else
          dbSeek(ubase,uiName,UpperCase(hdp.empfaenger));   { Nachricht in PM-Brett }
        if not dbFound or (dbXsize(ubase,'passwort')=0) then begin
          rfehler(308);   { 'Nachricht ist codiert, aber Passwort fehlt!' }
          exit;
          end;
        dbReadN(ubase,ub_codierer,coder);
        if coder<>dtyp then begin
          if dtyp=1 then
            rfehler(309)  { 'Nachricht ist QPC-codiert, aber es ist ein DES-Passwort eingetragen!' }
          else
            rfehler(310);  { 'Nachricht ist DES-codiert, aber es ist ein QPC-Passwort eingetragen!' }
          exit;
          end;
        siz0:=0;
        pw:= dbReadXStr(ubase,'passwort',siz0);
        end;

      ps := 32768;
      getmem(p,ps);
      seek(decf,ofs);
      passpos:=1;
      total:=filesize(decf)-ofs;
      show:=(dtyp=2) and (total>=2000);
      x:=0;
      if dtyp=2 then begin
        DES_PW(pw);
        if show then begin
          rmessage(360);   { 'DES-Decodierung...     %' }
          x:=wherex-5; y:=wherey;
          end;
        end;
      _off:=0;
      repeat
        fp:=filepos(decf);
        blockread(decf,p^,ps,rr);
        case dtyp of
         -1 : Rot13(p^,rr);
          1 : QPC(true,p^,rr,@pw,passpos);
          2 : DES_code(true,p^,_off,total,rr,x,y);
        end;
        if (dtyp<>0) and (hdp.charset='iso1') then
          Iso1ToIBM(p^,rr);
        seek(decf,fp);
        blockwrite(decf,p^,rr);
        inc(_off,rr);
      until eof(decf);
      if show then closebox;
      freemem(p,ps);
      end;
  end;

  procedure DumpMsg;
  const hc : array[0..15] of char = '0123456789ABCDEF';
  var s   : string;
      i   : integer;
      rr  : word;
      buf : array[0..15] of byte;
      adr : DWord;
      p,b : Integer;
  begin
    moment;
    adr:=0;
    repeat
      blockread(decf,buf,16,rr);
      dec(rr);
      s:=hex(adr,8)+sp(68);
      p:=11;
      for i:=0 to min(15,rr) do begin
        b:=buf[i];
        s[p]:=hc[b shr 4];
        s[p+1]:=hc[b and 15];
        inc(p,3);
        if i=7 then inc(p);
        if b<32 then
          s[i+61]:='˙'
        else
          s[i+61]:=chr(b);
        end;
      wrslong(s);
      inc(adr,16);
    until eof(decf) or (ioresult<>0);
    closebox;
  end;

  procedure SetQC(netztyp:byte);
  var p,p2,n  : byte;
      empty   : boolean;
      ac      : set of char;
      qs      : string[80];
  begin
    qchar:=QuoteChar;

    p:=cpos('&',qchar);
    p2:=cpos('#',hdp.absender);
    if p>0 then qchar[p]:='$';

    if (netztyp in netsRFC) or ((p>0) and (p2>0)) then begin

      p:=cpos('@',qchar); if p>0 then delete(qchar,p,1);
      p:=cpos('$',qchar); if p>0 then delete(qchar,p,1);
      end;
    p:=cpos('%',qchar); QuoteEmptyLines:=p>0; if QuoteEmptyLines then delete(qchar,p,1);
    {* Schneller Hack: Konfigurierbares Quoten von Leerzeilen, sauberer machen! }
    p:=cpos('@',qchar);
    empty:=false;
    if p=0 then begin
      p:=cpos('$',qchar);
      empty:=true;
      end;
    if p>0 then with hdp do
      if UpperCase(LeftStr(absender,8))='ZU_LANG_' then
        delete(qchar,p,1)
      else begin
        if cpos(' ',realname)>1 then qs:=trim(realname)
        else qs:=absender;
        ac:=['A'..'Z','a'..'z','é','ô','ö','Ñ','î','Å','·','0'..'9'];
        delete(qchar,p,1);
        insert(qs[1],qchar,p); inc(p);
        p2:=2; n:=0;
        while (p2<=length(qs)) and (qs[p2]<>'@') and
              (qs[p2]<>'%') and (qs[p2]<>'#') do begin
          if (qs[p2] in ac) and not (qs[p2-1] in ac)
          then begin
            insert(qs[p2],qchar,p);
            inc(p); inc(n);
            end;
          inc(p2);
          end;
        if (n=0) and empty then delete(qchar,p-1,1);
        end;
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP6.ClipTrealine!          }
      rr : word;
      p  : byte;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    SetLength(s, 200);
    blockread(f,s[1],200,rr);
    if rr<>200 then SetLength(s,rr);
    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then begin
      seek(f,l+p-1);
      truncate(f);
      end;
  end;

  procedure Chg_Tearline;   { Fido - Tearline + Origin verfremden }
  const
    PlusChar: Char = '+';
    SearchOffset = 200;
  var s  : string;
      rr,p, l: Integer;
  begin
    l := max(0, FileSize(f)- SearchOffset);
    seek(f,l);
    SetLength(s, SearchOffset);
    blockread(f, s[1], SearchOffset, rr);
    SetLength(s, rr);

    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then
    begin
      if s[p+4] <> '-' then dec(p);
      seek(f,l+p+2);
      blockwrite(f, PlusChar, 1);
      while (p<length(s)-11) and (copy(s,p,13)<>#13#10' * Origin: ')
        and (copy(s,p,12)<>#13' * Origin: ') do
        inc(p);
      if p<length(s)-12 then
      begin
        seek(f,l+p+2);
        blockwrite(f, PlusChar,1);
      end;
    end;
  end;

  procedure QuoteTtoF;
  var reads      : string[120];
      stmp       : string;
      lastqc     : string[20];
      qspaces    : string[QuoteLen];
      convstr    : shortstring;         { Workaround fuer iso_conv }
      p,q        : integer;
      lastquote  : boolean;   { vorausgehende Zeile war gequotet }
      blanklines : longint;
      i          : longint;
      endspace   : boolean;
      qc         : char;
      QuoteOffset: byte;

    procedure FlushStmp;
    begin
      if stmp<>'' then begin
        wrslong(trimRight(lastqc+stmp)); { Auch hier Leerzeichen entfernen }
        stmp:='';
        end;
    end;

    function GetQCpos:byte;
    var p,q : integer;
    begin
      QuoteOffset:=0;
      p:=cpos('>',s);
      if p>5 then p:=0
      else if p>0 then begin
        repeat        { korrektes Ende des (mehrfach-?)Quotezeichens }
          q:=p+1;     { ermitteln                                    }
          while (q<=length(s)) and (q-p<=4) and (s[q]<>'>') do
            inc(q);
          if (q<=length(s)) and (s[q]='>') then p:=q;
        until q>p;
        while (p<length(s)) and (s[p+1]='>') do inc(p);
        q:=p;
        while (q<length(s)) and (s[q+1]=' ') do inc(q);  { Textanfang suchen }
        QuoteOffset:=q-p;                  { Leerzeichen zwischen letztem ">" und Textanfang }
        end;
      GetQCpos:=p;
    end;

(*
    function IniQuote:boolean;
    var i : byte;
    begin
      IniQuote:=false;
      if s[1]<>'<' then
        for i:=1 to {p}cpos('>',s)-1 do
          if s[i] in ['A'..'Z','a'..'z','0'..'9','Ñ','î','Å','·','é','ô','ö'] then
            IniQuote:=true;
    end;
*)

  begin
    qspaces:=sp(length(qchar)-length(trimleft(qchar)));
    stmp:='';
    lastquote:=false;
    blanklines:=0;
    while not eof(t) do begin
      read(t,reads);                         { max. 120 Zeichen einlesen }
      endspace:=(LastChar(reads)=' ') or eoln(t);
      p:=length(reads);                      { rtrim, falls kein Leer-Quote }
      while (p>0) and (reads[p]=' ') do dec(p);
      s:=LeftStr(reads,p);
      if (leftStr(s,11)=' * Origin: ') or (leftStr(s,4)='--- ') or (s='---') then s[2]:='+';
      if not iso1 and ConvIso and (s<>'') then begin
        convstr:= s;
        ISO_conv(convstr[1],length(convstr));            { ISO-Konvertierung }
        s:= convstr;
      end;
      if s=#3 then begin
        FlushStmp;                           { #3 -> Leerzeile einfuegen }
        wrslong('');
      end else
      if s='' then begin
        FlushStmp;
        if lastquote then                    { Leerzeile quoten }
          wrslong('')
        else
          inc(blanklines)
      end
      else begin
        p:=GetQCpos;
        if blanklines>0 then
          if (p=0) { or not IniQuote } then  { naechste Zeile war nicht gequotet }
            for i:=1 to blanklines do    { -> Leerzeilen mitquoten          }
              if QuoteEmptyLines then wrslong(qchar)else wrslong('')
          else
            wrslong('');                 { sonst Leerzeilen nicht quoten }
        blanklines:=0;
        if (p=0) { or not IniQuote } then begin
          insert(qchar,s,1); inc(p,length(qchar));
          lastquote:=false;
        end
        else begin                           { neues Quote-Zeichen einfg. }
          lastquote:=true;
          q:=0;
          while (s[q+1]=#9) or (s[q+1]=' ') do inc(q);
          delete(s,1,q); dec(p,q);
          q:=1;
          while s[q]<>'>' do inc(q);
          insert('>',s,q); inc(p);
          if LastChar(qchar)=' ' then begin    { BLA>Fasel -> BLA> Fasel }
            while (q<=length(s)) and (s[q]='>') do inc(q);
            if (q<=length(s)) and (s[q]<>' ') then begin
              insert(' ',s,q); inc(p);
            end;
          end;
          insert(qspaces,s,1); inc(p,length(qspaces));
        end;
        q:=1;
        while (s[q] in [' ','A'..'Z','a'..'z','0'..'9','Ñ','î','Å','·','é','ô','ö'])
          and (q<p) do inc(q);
        qc:=s[q];
        while q<p do
        begin
          if (s[q]=' ') and (s[q+1] in [' ',qc]) then
          begin
            delete(s,q,1);
            dec(p);
          end else
            inc(q);
        end;
        p:=p+QuoteOffset;                    { Leerzeichen nach Quotezeichen dazuzaehlen }
        if stmp<>'' then begin               { Rest von letzter Zeile }
          if LeftStr(s,length(lastqc))=lastqc then
            insert(stmp,s,p+1)               { einfuegen }
          else
            FlushStmp;
          stmp:='';
        end;
        LastQC:=LeftStr(s,p);
        if (length(s)>=QuoteBreak) and
           ((lastchar(s)<#176) or (lastchar(s)>#223))  { Balkengrafik }
        then
          while length(s)>=QuoteBreak do begin   { Ueberlaenge abschneiden }
            p:=QuoteBreak;
            while (p>0) and (s[p]<>' ') and (s[p]<>#9) do dec(p);
            if p<=QuoteBreak div 2 then p:=QuoteBreak;
            stmp:=mid(s,p+iif(s[p]<=' ',1,0))+iifs(endspace,' ','');
            TruncStr(s,p-1);
            { Change hd 2000-07-03 RTrim entfernt }
            { TrimRight entfernt mehr als nur Space, also bitte pruefen!!! }
            s:= TrimRight(s);
            if not eoln(t) and (length(stmp)+length(LastQC)<QuoteBreak) then begin
              read(t,reads);      { Rest der Zeile nachladen }
              endspace:=(LastChar(reads)=' ') or eoln(t);
              if not iso1 and ConvIso and (reads<>'') then begin
                convstr:= reads;
                ISO_conv(convstr[1],length(convstr));    { ISO-Konvertierung }
                reads:= convstr;
              end;
              stmp:=stmp+trimright(reads)+iifs(endspace,' ','');
            end;
            if length(stmp)+length(LastQC)>=QuoteBreak then begin
              wrslong(s);
              s:=LastQC+trimright(stmp);
              stmp:='';
            end;
          end;
        s:= TrimRight(s);
        wrslong(s);
      end;
      readln(t);
    end;
    FlushStmp;
    wrs('');
  end;

  function ohfill(s:string;l:byte) : string;
  begin
    while (length(s)<l) do s:=s+#32;
    ohfill:=s;
  end;

Label ExitL;
begin
  extheadersize:=0; exthdlines:=0; hdlines:=0;
  TempKopien := TStringList.Create;
//  if ex_mpdata=nil then mpdata.startline:=0
//  else mpdata:=ex_mpdata^;
  ex_mpdata:=nil;
  multipart:=(mpdata.startline>0);
  _brett := dbReadNStr(mbase,mb_brett);
  if art=xTractPuf then
    Xread(name,append)
  else begin
    ReadHeadEmpf:=1; ReadKoplist:=true;
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    assign(f,name);
    if hds=1 then begin
      rewrite(f,1);
      close(f);
      Hdp.Free;
      Goto ExitL;
    end;
      if append then begin
      reset(f,1);
      if ioresult<>0 then rewrite(f,1)
      else seek(f,filesize(f));
      end
    else
      rewrite(f,1);

    extpos:=filepos(f);
    dbReadN(mbase,mb_EmpfDatum,edat);
    if smdl(IxDat('2712300000'),edat) then
      dbReadN(mbase,mb_wvdatum,edat);
    iso1:=(dbReadInt(mbase,'netztyp') and $2000)<>0;
    if (schablone<>'') and (FileExists(schablone)) then begin
      assign(t,ownpath+schablone);
      reset(t);
      while not eof(t) do with hdp do begin
        readln(t,s);
        wempf:=empfaenger;
        if cpos('Ø',wempf)>0 then begin
          delete(wempf,cpos('Ø',wempf),1);
          wempf:=wempf+getres2(361,1);   { '  (internes CrossPoint-Brett)' }
          end;
        if cpos('$',s)>0 then begin
          rps(s,'$BRETT',wempf);
          p:=length(wempf);
          while (p>0) and (wempf[p]<>'/') do dec(p);
          case firstchar(dbReadStrN(mbase,mb_brett)) of
            '$'  : rps(s,'$AREA',trim(getres2(361,1)));  { '(internes CrossPoint-Brett)' }
            'A'  : rps(s,'$AREA',mid(wempf,p+1));
            else   rps(s,'$AREA',getres2(361,48));       { 'private Mail' }
          end;
          if wempf[1]='/' then DeleteFirstChar(wempf);
          while cpos('/',wempf)>0 do wempf[cpos('/',wempf)]:='.';
          rps(s,'$NEWSGROUP',wempf);
          rpsuser(s,absender,realname);
          rps(s,'$RNAME2', realname);
          rps(s,'$RNAME', iifs(realname='','',realname+' '));
          rps(s,'$(RNAME2)',iifs(realname='','','('+realname+')'));
          rps(s,'$(RNAME)',iifs(realname='','','('+realname+') '));
          rps(s,'$FIDOEMPF',fido_to);
          rps(s,'$SUBJECT', betreff);
          rps(s,'$BETREFF',betreff);
          rps(s,'$ERSTELLT',fdat(datum));
          if pos('$MSGDATE',UpperCase(s))>0 then
            rpsdat(s,'$MSGDATE',fdat(datum));
          rps(s,'$ERSTZEIT',ftime(datum));
          rps(s,'$ERSTTAG2',LeftStr(zdow(datum),2));
          rps(s,'$ERSTTAG',zdow(datum));
          rps(s,'$ERHALTEN',fdat(longdat(edat)));
          rps(s,'$MSGID',msgid);
          rpsdate(s);
          if lastchar(s)=' ' then DeleteLastChar(s);
          end;
        wrslong(s);
        end;
      close(t);
      end;

    sizepos:=-1;
    if (art=xTractHead) or (art=xTractDump) then begin
      lasttrenn:=false;
      for hdln:=0 to ExtraktHeader.anz  do
        case ExtraktHeader.v[hdln] of

    hdf_Trenn :  if not lasttrenn then begin                     { Trennzeile }
                   if( length(VarLister) <> 0 ) then             { wenn externer Lister verwendet wird }
                     wrs(dup(iif(art=xTractHead,70,72),'-'))
                   else
                     wrs(dup(ScreenWidth-2,'ƒ'));                  { interner Lister }

                   lasttrenn:=true;
                 end;

    hdf_EMP   :  begin
                   if hdp.fido_to<>'' then s:=' ('+hdp.fido_to+')'
                   else s:='';
                   if hdp.empfanz=1 then
                     if cpos('@',hdp.empfaenger)>0 then
                       wrs(gr(2)+mausname(hdp.empfaenger)+s)   { 'Empfaenger : ' }
                     else
                       wrs(gr(2)+hdp.empfaenger+s)
                   else begin
                     s:=gr(2)+hdp.empfaenger;     { 'Empfaenger : ' }
                     for i:=2 to hdp.empfanz do begin
                       ReadHeadEmpf:=i;
                       TempKopien.Clear;
                       TempKopien.AddStrings(hdp.Kopien);
                       ReadHeader(hdp,hds,false);
                       Hdp.Kopien.Clear;
                       Hdp.Kopien.AddStrings(TempKopien);
                       if length(s)+length(hdp.empfaenger)>iif(listscroller,76,77)
                       then begin
                         wrs(s); s:=gr(2{15});
                         end
                       else
                         s:=s+', ';
                       s:=s+hdp.empfaenger;
                     end;
                     if hdp.fido_to<>'' then s:=s+' ('+hdp.fido_to+')';
                     wrs(s);
                   end;
                 end;

    hdf_KOP   : if hdp.Kopien.Count > 0 then
                begin
                  s := getres2(361,28)+hdp.Kopien[0];    { 'Kopien an  : ' }
                  for i := 1 to hdp.Kopien.Count - 1 do
                  begin
                    if length(s)+length(hdp.Kopien[i])>iif(listscroller,76,77) then
                    begin
                      wrs(s); s:=getres2(361,28);
                    end else
                      s := s + ', ';
                    s := s+ hdp.Kopien[i];
                  end;
                  Wrs(s);
                end;

    hdf_DISK  :  for i:=0 to hdp.followup.count-1 do
                   wrs(gr(3)+hdp.followup[i]);           { 'Antwort in : ' }

    hdf_ABS   :  begin
                   if ((hdp.netztyp=nt_fido) or (hdp.netztyp=nt_QWK)) and
                      (hdp.realname='') and
                      (length(hdp.absender)<54) and Nodelist.Open and
                      (cpos(':',hdp.absender)>0) then begin
                                  { sieht nach einer Fido-Adresse aus ... }
                     GetNodeinfo(hdp.absender,ni,0);
                     if ni.found then begin
                       hdp.realname:=LeftStr(ni.boxname,60-length(hdp.absender));
                       if length(hdp.absender)+length(hdp.realname)+length(ni.standort)<60
                       then
                         hdp.realname:=hdp.realname+', '+ni.standort;
                       end;
                     end;
                   wrs(gr(6)+mausname(hdp.absender)+      { 'Absender   : ' }
                       iifs(hdp.realname<>'','  ('+hdp.realname+')',''));
                 end;

    hdf_OEM    : if (hdp.oem.Count > 0) and (LeftStr(hdp.oem[0],length(hdp.empfaenger))
                     <>hdp.empfaenger) then
                   wrs(gr(16)+hdp.oem[0]);         { 'Org.-Empf. : ' }
    hdf_OAB    : if hdp.oab<>'' then            { 'Org.-Abs.  : ' }
                   wrs(gr(18)+hdp.oab+iifs(hdp.oar<>'','  ('+hdp.oar+')',''));
    hdf_WAB    : if hdp.wab<>'' then            { 'Weiterleit.: ' }
                   wrs(gr(17)+hdp.wab+iifs(hdp.war<>'','  ('+hdp.war+')',''));
    hdf_ANTW  : if (hdp.ReplyTo<>'') and
                  ((UpperCase(Hdp.ReplyTo) <> UpperCase(hdp.absender))) then   { 'Antwort an : ' }
                   wrs(gr(27)+hdp.ReplyTo);

    hdf_BET    : begin
                   tmp:=TempS(2000+dbReadInt(mbase,'msgsize')
                     *iif(art=xTractQuote,1,4));
                   assign(t,tmp);
                   Xread(tmp,false);    { Erstmal Betreff aus Nachricht holen... }
                   reset(t);
                   repeat
                     readln(t,s);
                   until (s='') or (leftStr(s,4)='BET:') or eof(t);
                   close(t);
                   _era(tmp);
                   if LeftStr(s,4)='BET:' then s:=mid(s,6)
                     else s:=hdp.betreff;
                   ln:=length(getres2(361,5));
                   p:=0;
                   repeat                               { langen Betreff umbrechen }
                     lr:=rightpos(' ',leftStr(s,78-ln));
                     if (lr=0) or (length(s)<=78-ln) then lr:=78-ln;
                     wrs(iifs(p=0,gr(5),sp(ln))+leftStr(s,lr));
                     inc(p);
                     s:=mid(s,lr+1);
                   until s='';
                 end;
    hdf_ZUSF   : if hdp.summary<>'' then        { 'Zus.fassung: ' }
                 begin
                   s:=hdp.summary;
                   p:=0;
                   ln:=length(getres2(361,23));
                   repeat                               { lange Zusammenfassung umbrechen }
                     lr:=rightpos(' ', LeftStr(s,78-ln));
                     if (lr=0) or (length(s)<=78-ln) then lr:=78-ln;
                     wrs(iifs(p=0,gr(23),sp(ln))+ LeftStr(s,lr));
                     inc(p);
                     s:=mid(s,lr+1);
                   until s='';
                 end;
    hdf_STW    : if hdp.keywords<>'' then       { 'Stichworte : ' }
                   wrs(gr(22)+hdp.keywords);

    hdf_ROT    : if hdp.pfad<>'' then begin
                   s:=hdp.pfad;
                   hs:=gr(7);                    { 'Pfad       : ' }
                   while s<>'' do begin
                     p:=length(s);
                     if p+length(hs)>79 then begin
                       p:=79-length(hs);
                       while (p>30) and (s[p]<>'!') and (s[p]<>' ')
                             and (s[p]<>'.') do
                         dec(p);
                       if p=30 then p:=79-length(hs);
                       end;
                     wrs(hs+LeftStr(s,p));
                     delete(s,1,p);
                     hs:=gr(15);                 { sp(...) }
                     end;
                   end;

    hdf_MID    : begin
                   ln:=length(getres2(361,8));                  { 'Message-ID : ' }
                   wrs(gr(8)+leftStr(hdp.msgid,78-ln));
                   if length(hdp.msgid)>78-ln then
                     wrs(sp(ln)+copy(hdp.msgid,79-ln,78-ln));
                 end;

    hdf_BEZ    : with hdp do if References.Count > 0 then            { 'Bezugs-ID  : ' }
                   wrs(gr(19)+References[References.Count-1]+iifs(hdp.References.Count=1,'',', ...'));

    hdf_EDA    : wrs(gr(9)+iifs(hdp.Datum='','N/A',copy(zdow(hdp.datum),1,2)+' ' { 'Datum' }
                  +fdat(hdp.datum)+', '+ftime(hdp.datum))
                  +iifs(hdp.datum<>longdat(edat),'  ('+gr(10)
                  +fdat(longdat(edat))+', '+ftime(longdat(edat))+')','')); { 'erhalten: ' }

    hdf_LEN    : begin
                   sizepos:=filesize(f);
                   wrs(reps(gr(11),strs(hdp.groesse)));  { 'Groesse    : %s Bytes' }
                 end;

    hdf_MAILER : if hdp.programm<>'' then begin
                   wrs(gr(20)+hdp.programm);    { 'Software   : ' }
                   end;

    hdf_ORG    : if hdp.organisation<>'' then
                   wrs(gr(24)+hdp.organisation);   { 'Organisat. : ' }
    hdf_POST   : if hdp.postanschrift<>'' then
                   wrs(gr(25)+hdp.postanschrift);  { 'Postadresse: ' }
    hdf_TEL    : if hdp.telefon<>'' then
                   wrs(gr(26)+telestring(hdp.telefon));  { 'Telefon    : ' }

    hdf_FILE   : if multipart and (mpdata.fname<>'') then
                   wrs(gr(12)+mpdata.fname)    { 'Dateiname  : ' }
                 else if hdp.datei<>'' then
                   wrs(gr(12)+hdp.datei+ddat);

    hdf_MSTAT  : if (hdp.pm_bstat<>'') and (hdp.pm_bstat[1]<>'N') then
                   wrs(gr(13)+mausstat(hdp.pm_bstat));     { 'PM-Status  : ' }
    hdf_STAT   : begin
                   GetStatus;
                   if mstatus<>'' then wrs(gr(21)+mstatus);  { 'Status:    : ' }
                 end;
    hdf_PGPSTAT: begin
                   GetPgpStatus;
                   if mstatus<>'' then wrs(gr(29)+mstatus);  { 'PGP-Status : ' }
                 end;

    hdf_ERR    : if hdp.error<>'' then
                   wrs(gr(14)+hdp.error);                  { 'Fehler!    : ' }

    hdf_DIST   : if hdp.distribution<>'' then
                   wrs(gr(31)+hdp.distribution);           { 'Distribut. : ' }

    hdf_Homepage: if hdp.homepage<>'' then
                    wrs(gr(32)+hdp.homepage);              { 'Homepage   : ' }

    hdf_Part    : if multipart and (mpdata.part>0) then
                    wrs(gr(33)+strs(mpdata.part)+           { 'Teil       : ' }
                        gr(34)+strs(mpdata.parts));         { ' von ' }

    hdf_Cust1   : if mheadercustom[1]<>'' then if hdp.Cust1<>'' then begin
                    wrs(ohfill(mheadercustom[1],11)+': '+hdp.Cust1);
                  end;
    hdf_Cust2   : if mheadercustom[2]<>'' then if hdp.Cust2<>'' then begin
                    wrs(ohfill(mheadercustom[2],11)+': '+hdp.Cust2);
                  end;

  { Prioritaet im Listenkopf anzeigen:                                    }
  { Rueckgabewert hinter dem PriorityFlag extrahieren und zuordnen        }

  hdf_Priority:
      if hdp.Priority in [1..5] then
        wrs(gr(35) + GetRes2(272, Hdp.Priority))
      else if hdp.Prio>0 then                                 { und fuer Zconnect ....  }
         if hdp.Prio<=10 then wrs(gr(35) + GetRes2(604, 6))    { Direktmail }
                          else wrs(gr(35) + GetRes2(604, 8));   { Eilmail }
  end;

      extheadersize:=filepos(f)-extpos;
      exthdlines:=min(hdlines,screenlines-5);
      end;
    dbReadN(mbase,mb_groesse,size);
    if (art<>xtractQuote) and (art<>xTractDump) then begin
      if multipart then begin
        mpsize:=filesize(f);
        close(f);
        mehdl:=exthdlines; mehds:=extheadersize;
        ExtractMultiPart(mpdata,name,true);    { rekursiver Aufruf von }
        exthdlines:=mehdl;                     { extact_msg!           }
        extheadersize:=mehds;
        reset(f,1);
        if sizepos>=0 then begin
          mpsize:=filesize(f)-mpsize;
          seek(f,sizepos);
          s:=reps(gr(11),strs(mpsize));
          blockwrite(f,s[1],length(s));
          end;
        seek(f,filesize(f));
        end
      else begin
        XReadIsoDecode:=true;
        XreadF(hds+iif(hdp.typ='B',hdp.komlen,0),f);
        end;
      if decode<>0 then begin
        Move(f,decf,sizeof(f));
        case decode of
         -1 : do_decode(-1,filesize(f)-size);      { Rot13 }
          1 : if IS_QPC(hdp.betreff) then
                do_decode(1,filesize(f)-size)
              else
                if IS_DES(hdp.betreff) then
                  do_decode(2,filesize(f)-size);
        end;
        end;
      end
    else begin                                     { Quote / Hex-Dump }
      tmp:=TempS(2000+dbReadInt(mbase,'msgsize')*iif(art=xTractQuote,1,4));
      if ListQuoteMsg<>'' then
        tmp:=ListQuoteMsg
      else begin
        XReadIsoDecode:=(art=xTractQuote);
        if multipart then ExtractMultipart(mpdata,tmp,false)
        else Xread(tmp,false);
        if decode<>0 then begin
          assign(decf,tmp);
          reset(decf,1);
          case decode of
           -1 : do_decode(-1,hds);
            1 : if IS_QPC(hdp.betreff) then
                  do_decode(1,hds)
                else
                  if IS_DES(hdp.betreff) then
                    do_decode(2,hds);
          end;
          close(decf);
          end;
        end;

      if art=xTractQuote then begin                { Quote }
        SetQC(hdp.netztyp);
        assign(t,tmp);
        reset(t);
        if not multipart or (ListQuoteMsg<>'') then  { ZC-Header 'ueberlesen' }
          if ntZCablage(dbReadInt(mbase,'ablage')) then
            repeat
              readln(t,s)
            until (s='') or eof(t)
          else
            for i:=1 to 8 do readln(t);
        QuoteTtoF;
        close(t);
        erase(t);
        end
      else begin                                   { Hex-Dump }
        assign(decf,tmp);
        reset(decf,1);
        DumpMsg;
        close(decf);
        erase(decf);
        end;
      end;
    if (hdp.netztyp=nt_Fido) and (art=xTractMsg) then
      if ExtCliptearline then
        Clip_Tearline
      else
        if ExtChgTearline then Chg_Tearline;
    close(f);
    Hdp.Free;
  end;
  freeres;
ExitL:
  ExtCliptearline:=true;
  ExtChgtearline:=false;
  TempKopien.Free;
end;

{$ENDIF __undefined__}

//
//  $Log$
//  Revision 1.1  2001/09/30 19:37:49  cl
//  - moved from main directory
//

{ --------------------------------------------------------------- } end.

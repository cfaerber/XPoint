{   $Id$

    OpenXP PGP handling unit
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

{$I xpdefine.inc }

{ OpenXP PGP handling unit }
unit  xp_pgp;

interface

uses
  sysutils,xpglobal,typeform,fileio,resource,database,maske, xpheader,
{$IFDEF unix}
{$IFDEF fpc}
  linux,
{$ENDIF}  
{$ENDIF}
  xp0,xp1,xpstreams,Classes;

procedure LogPGP(s:string);                  { s in PGP.LOG schreiben         }
procedure RunPGP(par:string);                { PGP 2.6.x bzw. 6.5.x aufrufen  }
procedure RunPGP5(exe:string;par:string);    { PGP 5.x aufrufen               }
procedure UpdateKeyfile;
procedure WritePGPkey_header(output:TStream);{ PGP-PUBLIC-KEY: ... erzeugen   }
procedure PGP_SendKey(empfaenger:string);    { Antwort auf Key-Request senden }

procedure PGP_EncodeStream(var data:TStream; var hd: Theader;
                         RemoteUserID:string; encode,sign:boolean;
                         var fido_origin:string);
procedure PGP_MimeEncodeStream(var data:TStream;hd:THeader;RemoteUserID:string);
procedure PGP_MimeSignStream(var data:TStream;hd:THeader);

procedure PGP_RequestKey;
procedure PGP_DecodeMessage(hdp:Theader; sigtest:boolean);
procedure PGP_DecodeMsg(sigtest:boolean);  { dec. und/oder Signatur testen }
procedure PGP_DecodeKey(source,dest:string);
procedure PGP_ImportKey(auto:boolean);
procedure PGP_EditKey;
procedure PGP_RemoveID;

procedure PGP_BeginSavekey;      { Key aus ZCONNECT-Header temporaer sichern }
procedure PGP_EndSavekey;

procedure PGP_SelectKey(var keyid:string; hint:string; secret:boolean);

implementation  { --------------------------------------------------- }

uses  xp3,xp3o,xp3o2,xp3ex,xpsendmessage,
  {$ifdef Win32} xpwin32, {$endif}
  {$ifdef DOS32} xpdos32, {$endif}
  {$IFDEF Kylix}
  libc,
  {$ENDIF}
  xpcc,xpnt,mime,mime_base64;

const
  savekey : string = '';
  flag_PGPSigOk = $01;
  flag_PGPSigErr = $02;

function testbin(var bdata; rr: Integer):boolean; assembler; {&uses esi}
asm
         mov   ecx,rr
         mov   esi,bdata
         cld
@tbloop: lodsb
         cmp   al,9                     { Binaerzeichen 0..8, ja: c=1 }
         jb    @tbend                  { JB = JC }
         loop  @tbloop
@tbend:  mov eax, 0
       {  adc ax,ax }                      { C=0: false, c=1: true }
         sbb eax,eax
{$IFDEF FPC }
end ['EAX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure LogPGP(s:string);
var t : text;
begin
  assign(t,LogPath+'PGP.LOG');
  if existf(t) then append(t)
  else rewrite(t);
  writeln(t,LeftStr(date,6),RightStr(date,2),' ',time,'  ',s);
  close(t);
  if ioresult<>0 then;
end;

{ PGP 2.6.x und 6.5.x }
procedure RunPGP(par:string);
const
  {$ifdef unix}
    PGPEXE = 'pgp';
    PGPBAT = 'xpgp.sh';
  {$else}
    PGPEXE = 'PGP.EXE';
    PGPBAT = 'XPGP.BAT';
  {$endif}
var
  path : string;
begin
  if FileExists(PGPBAT) then
    path:=PGPBAT
  else begin
    path:=getenv('PGPPATH');
    if path<>'' then 
    begin
      TrimLastChar(path, '\');
      path:=filesearch(PGPEXE,path);
    end;
    if path='' then
      path:=filesearch(PGPEXE,getenv('PATH'));
  end;
  if path='' then
    trfehler(217,30)    { 'PGP ist nicht vorhanden oder nicht per Pfad erreichbar.' }
  else begin
    shellkey:=PGP_WaitKey;
    if PGPVersion=PGP2 then
      shell(path+iifs(PGPbatchmode,' +batchmode ',' ')+par,500,1)
    else
      shell(path+' '+par,500,1);
    shellkey:=false;
  end;
end;

{ PGP 5.x }
procedure RunPGP5(exe,par:string);
var path : string;
    pass,batch : string;
    {$ifdef unix}
    dir, name, ext:
    {$IFDEF fpc}
    shortstring;
    {$ELSE}
    string;
    {$endif}
    {$endif}
begin
  {$ifdef unix}
  fsplit(exe,dir,name,ext);
  exe:=LowerCase(name); { aus PGPK.EXE wird pgpk etc ...}
  {$endif}
  path:=getenv('PGPPATH');
  if path<>'' then begin
    TrimLastChar(path, '\');
    path:=filesearch(exe,path);
  end;
  if path='' then
    path:=filesearch(exe,getenv('PATH'));
  if path='' then
    trfehler1(3001,exe,30)   { '%s fehlt oder ist nicht per Pfad erreichbar.' }
  else begin
    shellkey:=PGP_WaitKey;
    if PGPBatchmode then begin
      pass:=GetEnv('PASSPHRASE');
      if pass<>'' then pass:='"'+pass+'"';
      case UpCase(exe[4]) of
        'E' : batch := ' -z '+pass+' ';
        'K' : batch := ' -z ';
        'O' : batch := ' -z '+pass+' ';
        'S' : batch := ' -z '+pass+' ';
        'V' : batch := ' -z '+pass+' ';
      end;
    end;
    shell(path+iifs(PGPbatchmode,batch,' ')+par,2048{500},1);
    shellkey:=false;
  end;
end;

{ GnuPG 1.x+ }
procedure RunGPG(par:string);
const
  {$ifdef unix}
    PGPEXE = 'gpg';
    PGPBAT = 'xgpg.sh';
  {$else}
    PGPEXE = 'GPG.EXE';
    PGPBAT = 'XGPG.BAT';
  {$endif}
var
  path : string;
begin
  if FileExists(PGPBAT) then
    path:=PGPBAT
  else begin
    path:=getenv('PGPPATH');
    if path<>'' then begin
      TrimLastChar(path, '\');
      path:=filesearch(PGPEXE,path);
    end;
    if path='' then
      path:=filesearch(PGPEXE,getenv('PATH'));
  end;
  if path='' then
    trfehler(217,30)    { 'PGP ist nicht vorhanden oder nicht per Pfad erreichbar.' }
  else begin
    shellkey:=PGP_WaitKey;
    shell(path+' '+par,500,1);
    shellkey:=false;
  end;
end;

{ User-ID fuer Command-Line-Aufruf in Anfuehrungszeichen setzen }

function IDform(s:string):string;
begin
  if multipos(' /<>|',s) then begin
    if firstchar(s)<>'"' then s:='"'+s;
    if lastchar(s)<>'"' then s:=s+'"';
  end;
  result:=s;
end;


procedure UpdateKeyfile;
var secring : string;
begin
  if UsePGP and (PGP_UserID<>'') then begin
    secring:=filesearch('PUBRING.PGP',getenv('PGPPATH'));
    if (secring<>'') and (filetime(secring)>filetime(PGPkeyfile)) then begin
      SafeDeleteFile(PGPkeyfile);
      if PGPVersion=PGP2 then
        RunPGP('-kx +armor=off '+IDform(PGP_UserID)+' '+PGPkeyfile)
      else if PGPVersion=GPG then
        RunGPG('--extract '+IDform(PGP_UserID)+' '+PGPkeyfile)
      else
        RunPGP5('PGPK.EXE','-x +armor=off '+IDform(PGP_UserID)+' -o '+PGPkeyfile);
    end;
    { #### PGP6 ? #### }
  end;
end;


procedure WritePGPkey_header(output:TStream);    { PGP-PUBLIC-KEY: ... erzeugen }
var kf  : file;
    dat : array[0..29] of byte;
    rr  : Integer;
    i,j : integer;
    s   : string;
    b64 : array[0..63] of char;

  procedure wrs(s:string);
  begin
    output.WriteBuffer(s[1],length(s));
  end;

begin
  UpdateKeyfile;
  if (savekey<>'') and FileExists(savekey) then
    assign(kf,savekey)
  else
    assign(kf,PGPkeyfile);
  if existf(kf) then begin
    wrs('PGP-PUBLIC-KEY: ');
    b64:='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    reset(kf,1);
    while not eof(kf) do begin
      fillchar(dat,sizeof(dat),0);
      blockread(kf,dat,30,rr);
      i:=0; j:=0;
      while i<rr do begin
        s[j+1]:=b64[dat[i] shr 2];
        s[j+2]:=b64[(dat[i] and 3) shl 4 + dat[i+1] shr 4];
        if i+1<rr then s[j+3]:=b64[(dat[i+1] and 15) shl 2 + dat[i+2] shr 6]
        else s[j+3]:='=';
        if i+2<rr then s[j+4]:=b64[dat[i+2] and 63]
        else s[j+4]:='=';
        inc(i,3);
        inc(j,4);
      end;
      setlength(s,j);
      wrs(s);
    end;
    close(kf);
    wrs(#13#10);
  end;
end;


procedure PGP_SendKey(empfaenger:string);   { Antwort auf Key-Request senden }
var t   : text;
    tmp : string;
    hd  : string;
begin
  UpdateKeyfile;
  if not FileExists('pgp-key.bin') then exit;
  tmp:=TempS(4096);
  assign(t,tmp);
  rewrite(t);
  writeln(t);
  writeln(t,getres2(3000,2));   { 'Der Header dieser Nachricht enthaelt den angeforderten PGP-Public-Key.' }
  writeln(t,getres2(3000,3));   { 'Diese Nachricht wurde von CrossPoint automatisch erzeugt.' }
  writeln(t);
  close(t);
  hd:='';
  if DoSend(true,tmp,true,false,empfaenger,getres2(3000,1),  { 'Antwort auf PGP-Key-Anforderung' }
            false,false,false,false,true,nil,
            hd,SendPGPkey) then
    LogPGP(getreps2(3002,1,empfaenger));   { 'sende Public Key an %s' }
//  if FileExists(tmp) then _era(tmp);
  freeres;
end;


{ Text aus 'source' codieren bzw. signieren und zusammen mit }
{ Header 'hd' in Datei 'fn' ablegen.                         }
{ Bei Fido-Nachrichten Origin abschneiden und nach Codierung }
{ / Signierung wieder anhaengen.                             }

procedure PGP_EncodeStream(var data:TStream; var hd: theader;
                         RemoteUserID:string; encode,sign:boolean;
                         var fido_origin:string);
var b    : byte;
    nt   : longint;
    t    : string;
    OwnUserID  : string;

    fi,fo: string;
    fis: TStream;

begin
  if RemoteUserID='' then                       { User-ID ermitteln }
    RemoteUserID:=hd.Firstempfaenger;
  if cPos('/',RemoteUserID)>0 then
    RemoteUserID:='';                           { Empfaenger ist Brett }

  if PGPVersion=GPG then
    t:=iifs(hd.typ='T','a ',' --armor')
  else if PGPVersion=PGP2 then
    t:=iifs(hd.typ='T','t',' +textmode=off')
  else
    t:=iifs(hd.typ='T','-t','');

  fi:=TempExtS(data.Size,'PGP_','');
  fo:=TempS(data.Size+iif(encode,data.Size div 2,0)+iif(sign,2000,0)+2000);

  fis:=TFileStream.Create(fi,fmCreate or fmDenyWrite);

  if (hd.typ='T') and MimeContentTypeNeedCharset(hd.MIME.ctype) and
    (hd.x_charset<>'US-ASCII') and (hd.x_charset<>'') then
  begin
    // Translate charset to one supported by the selected
    // PGP/GPG version.

    if PGPVersion=GPG then
    begin
      if (hd.x_charset='ISO-8859-1') or         // charsets that GPG knows
         (hd.x_charset='ISO-8859-2') or
         (hd.x_charset='KOI8-R') then
        t:=t+' --charset='+hd.x_charset
      else
      begin
        t:=t+' --charset=utf8';                 // if everything else fails,
        hd.x_charset:='UTF-8';                  // use UTF-8. :-(
      end;
    end
    else // if PGPVersion=PGP2 then
    begin
      if (hd.x_charset='KOI8-R') then
        t:=t+' +charset=KOI8'
      else
      if (hd.x_charset='ISO-8859-1') then
        t:=t+' +charset=LATIN1'
      else
        t:=t+' +charset=noconv'                 // uh, oh...
    end;

    if LowerCase(MimeCharsetCanonicalName(hd.x_charset))<>
       LowerCase(MimeCharsetCanonicalName(ZCCharsetToMIME(hd.charset))) then
    begin
      ConnectStream(fis,TCharsetEncoderStream.Create(ZCCharsetToMime(hd.charset),hd.x_charset));
      hd.charset:=MimeCharsetToZC(hd.x_charset);
    end;
  end;

  if fido_origin<>'' then
    fis.CopyFrom(data,data.Size-length(fido_origin)-2)
  else
    CopyStream(data,fis);

  fis.Free;

  if PGP_UserID<>'' then
    OwnUserID:=' -u '+IDform(PGP_UserID)
  else
    OwnUserID:='';

  { --- codieren --- }
  if encode and not sign then begin
    if PGPVersion=PGP2 then
      RunPGP('-ea'+t+' '+fi+' '+IDform(RemoteUserID)+' -o '+fo)
    else if PGPVersion=PGP5 then
      RunPGP5('PGPE.EXE','-a '+t+' '+fi+' -r '+IDform(RemoteUserID)+' -o '+fo)
    else if PGPVersion=GPG then
      RunGPG('-e'+t+' -o '+fo+' -r '+IDform(RemoteUserID)+' '+PGP_GPGEncodingOptions+' '+fi)
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
//      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
//      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-e -a '+t+' '+fi+' '+IDform(RemoteUserID));
      fo:=fi+'.asc';
    end;

  { --- signieren --- }
  end else if sign and not encode then begin
    if PGPVersion=PGP2 then
      RunPGP('-sa'+t+' '+fi+OwnUserID+' -o '+fo )
    else if PGPVersion=PGP5 then
      RunPGP5('PGPS.EXE','-a '+t+' '+fi+OwnUserID+' -o '+fo )
    else if PGPVersion=GPG then
      RunGPG(iifs(hd.typ='T','--clearsign','-s')+' --force-v3-sigs -o '+fo+' '+OwnUserID+' '+fi)
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
//      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
//      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-s -a '+t+' '+fi+' '+IDform(RemoteUserID)+OwnUserID);
//      if FileExists(getbarefilename(tmp)) then _era(getbarefilename(tmp));         { Temporaerdatei loeschen }
//      if FileExists(tmp) then _era(tmp);         { xxxx wieder loeschen }
      fo:=fi+'.asc';
    end;

  { --- codieren+signieren --- }
  end else begin
    if PGPVersion=PGP2 then
      RunPGP('-esa'+t+' '+fi+' '+IDform(RemoteUserID)+OwnUserID+' -o '+fo)
    else if PGPVersion=PGP5 then
      RunPGP5('PGPE.EXE','-sa '+t+' '+fi+' -r '+IDform(RemoteUserID)+OwnUserID+' -o '+fo)
    else if PGPVersion=GPG then
      RunGPG('-es'+t+' --force-v3-sigs -o '+fo+' '+OwnUserID+' -r '+IDform(RemoteUserID)+' '+PGP_GPGEncodingOptions+' '+fi)
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
//      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
//      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-e -s -a '+t+' '+fi+' '+IDform(RemoteUserID)+OwnUserID);
//      if FileExists(tmp) then _era(tmp);         { xxxx wieder loeschen }
      fo:=fi+'.asc';
    end;
  end;

  if FileExists(fi) then
    _era(fi);         { Temporaerdatei loeschen }

  if FileExists(fo) then
  begin
    data.Free;

    if Fido_Origin<>'' then
    begin
      data:=TTemporaryFileStream.Create(fo,fmOpenReadWrite);
      data.Seek(0,soFromEnd);
      data.WriteBuffer(Fido_Origin[1],Length(Fido_Origin));
      data.Seek(0,soFromBeginning);
    end else
      data:=TTemporaryFileStream.Create(fo,fmOpenRead);

    hd.groesse:=data.Size;                    { Groesse anpassen }

    if encode or (sign and (hd.typ<>'T')) then
    begin                                     { now uses PGP armor! }
      hd.MIME.ctype:='';                        // => not a MIME-typed
      hd.MIME.encoding:=MimeEncoding7Bit;       // message any longer!
      hd.crypt.charset:=hd.charset;
      hd.charset:='';
      hd.x_charset:='';
      hd.crypt.typ:=hd.typ; hd.typ:='T';         { Typ anpassen   }
      hd.crypt.komlen:=hd.komlen; hd.komlen:=0;  { KOM anpassen   }
      hd.crypt.method:='PGP';
    end;

    if encode then inc(hd.pgpflags,fPGP_encoded);
    if sign then inc(hd.pgpflags,iif(encode,fPGP_signed,fPGP_clearsig));
    if encode then begin
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 4;                            { 'c'-Kennzeichnung }
      dbWriteN(mbase,mb_unversandt,b);
    end;
    if sign then begin
      dbReadN(mbase,mb_netztyp,nt);
      nt:=nt or $4000;                      { 's'-Kennzeichnung }
      dbWriteN(mbase,mb_netztyp,nt);
    end;
  end else
    rfehler(3002);      { 'PGP-Codierung ist fehlgeschlagen.' }
end;

procedure MimeWriteType(os:TStream;hd:THeader);
var mtype:TMimecontenttype;
    mdisp:TMimeDisposition;
begin
  mtype:=TMimecontenttype.Create(hd.mime.ctype);
  mdisp:=TMimeDisposition.Create(hd.mime.disposition);

  writeln_s(os,'Content-Type: ' + mtype.AsFoldedString(76-14,76,#13#10,true));
  writeln_s(os,'Content-Transfer-Encoding: '+MimeEncodingNames[hd.mime.encoding]);

  if Uppercase(mdisp.AsString)<>'INLINE' then
    writeln_s(os,'Content-Disposition: ' + mdisp.AsFoldedString(76-21,76,#13#10,true));

  if hd.MIME.description <> '' then
    writeln_s(os,'Content-Description: ' + RFC2047_Encode(hd.mime.description,csCP437,76-21,76,#13#10));

  mdisp.Free;
  mtype.Free;

  writeln_s(os,'');
end;

procedure MimeNewType(hd:Theader;subtype,protocol,micalg,bound_seed:string);
var mtype: TMimeContentType;
begin
  mtype:=TMimeContentType.Create('multipart/'+subtype);
  mtype.ParamValues['protocol']:=protocol;
  if micalg<>'' then mtype.ParamValues['micalg']:='pgp-'+LowerCase(micalg);
  hd.boundary:=MimeCreateMultipartBoundary(bound_seed);
  mtype.boundary:=hd.boundary;

  hd.mime.ctype   := mtype.AsString;
  hd.mime.encoding:= MimeEncoding7Bit;
  hd.mime.description:='';
  hd.mime.disposition:='';
  hd.mime.cid:='';

  hd.charset:='';
  hd.x_charset:='';
end;

procedure PGP_MimeEncodeStream(var data:TStream;hd:THeader;RemoteUserID:string);
var b    : byte;
    fi,fo: string;
    fis: TStream;
begin
  if RemoteUserID='' then                       { User-ID ermitteln }
    RemoteUserID:=hd.FirstEmpfaenger;

  fi:=TempExtS(data.Size,'PGP_','');
  fo:=TempS(data.Size+(data.Size div 2)+2000);

  fis:=TFileStream.Create(fi,fmCreate);
  MimeWriteType(fis,hd);
  data.Seek(0,soFromBeginning);
  CopyStream(data,fis);
  fis.Free;

  if PGPVersion=PGP2 then
    RunPGP('-ea +textmode=off '+fi+' '+IDform(RemoteUserID)+' -o '+fo)
  else if PGPVersion=PGP5 then
    RunPGP5('PGPE.EXE','-a  '+fi+' -r '+IDform(RemoteUserID)+' -o '+fo)
  else if PGPVersion=GPG then
    RunGPG('-e -a -o '+fo+' -r '+IDform(RemoteUserID)+' '+PGP_GPGEncodingOptions+' '+fi)
  else begin
    RunPGP('-e -a '+fi+' '+IDform(RemoteUserID));
    fo:=fi+'.asc';
  end;

  if FileExists(fi) then
    _era(fi);         { Temporaerdatei loeschen }

  if FileExists(fo) then
  begin
    data.Free;

    data := TMemoryStream.Create;
    MimeNewType(hd,'encrypted','application/pgp-encrypted','',hd.FirstEmpfaenger);

    writeln_s(data,'--'+hd.boundary);
    writeln_s(data,'Content-Type: application/pgp-encrypted');
    writeln_s(data,'');
    writeln_s(data,'Version: 1');
    writeln_s(data,'');
    writeln_s(data,'--'+hd.boundary);
    writeln_s(data,'Content-Type: application/octet-stream');
    writeln_s(data,'');
    fis:=TTemporaryFileStream.Create(fo,fmOpenRead);
    CopyStream(fis,data);
    fis.Free;
    writeln_s(data,'');
    writeln_s(data,'--'+hd.boundary+'--');

    inc(hd.pgpflags,fPGP_encoded);
    dbReadN(mbase,mb_unversandt,b);
    b:=b or 4;                            { 'c'-Kennzeichnung }
    dbWriteN(mbase,mb_unversandt,b);
  end else
    rfehler(3002);      { 'PGP-Codierung ist fehlgeschlagen.' }
end;

const PGP_HashAlgos: array[1..7] of string = (
  'MD5','SHA1','RIPEMD160','','MD2','TIGER192','HAVAL-5-160' );

procedure PGP_MimeSignStream(var data:TStream;hd:THeader);
var nt: longint;
    OwnUserID  : string;
    fi,fo: string;
    fis,fie: TStream;
    s: string;

    a8:    Byte;
    a16:   Integer16;
    a32:   Integer32;
    abl:   array[1..16] of Byte;

begin
  if PGP_UserID<>'' then
    OwnUserID:=' -u '+IDform(PGP_UserID)
  else
    OwnUserID:='';

  fi:=TempExtS(data.Size,'PGP_','');
  fo:=TempS(data.Size+(data.Size div 2)+2000);

  fis:=TFileStream.Create(fi,fmCreate);
  MimeWriteType(fis,hd);
  data.Seek(0,soFromBeginning);
  CopyStream(data,fis);
  fis.Free;

    if PGPVersion=PGP2 then
      RunPGP('-sab +textmode=off '+fi+OwnUserID+' -o '+fo)
    else if PGPVersion=PGP5 then
      RunPGP5('PGPS.EXE','-a -b '+fi+OwnUserID+' -o '+fo)
    else if PGPVersion=GPG then
      RunGPG('-s -a --detach-sign --force-v3-sigs -o '+fo+' '+OwnUserID+' '+fi)
    else begin
      RunPGP('-s -a -b '+OwnUserID+fi);
      fo:=fi+'.asc';
    end;

  if FileExists(fo) then
  begin
    data.Free;

    fis:=TTemporaryFileStream.Create(fo,fmOpenRead);

    try
      repeat
        s:=Trim(readln_s(fis));
      until s='-----BEGIN PGP SIGNATURE-----';

      // Try to find and interpret a Hash: header
      repeat
        s:=Trim(readln_s(fis));
        if UpperCase(LeftStr(s,5))='HASH:' then
        begin
          s:=Mid(s,6);
          s:=LeftStr(s,CPosX(',',s));
          s:=Trim(s);
          exit;
        end;
      until s='';

      // OK, there's no Hash: header, so read it directly from data
      fie := TBase64DecoderStream.Create;
      try
        TBase64DecoderStream(fie).OtherStream := fis;
        TBase64DecoderStream(fie).DestroyOtherStream := false;

        fie.ReadBuffer(a8,1);           // read packet type

        if (a8 and $80) = 0 then
          raise Exception.Create('');   // not an OpenPGP packet

        if (a8 and $40) = 0 then        // old packet type
        begin
          if ((a8 and $3F) shr 2)<>2 then
            raise Exception.Create(''); // not a signature packet

          case a8 and $3 of             // skip length... (uh, oh)
            0: fie.ReadBuffer(a8,1);
            1: fie.ReadBuffer(a16,2);
            2: fie.ReadBuffer(a32,4);
            3: ;
          end;
        end else                        // new packet type
        begin
          if (a8 and $3F)<>2 then
            raise Exception.Create(''); // not a signature packet

          fie.ReadBuffer(a8,1);         // read packet length

          if Byte(a8) = $FF then
            fie.ReadBuffer(a32,4)       // five-octet length
          else if Byte(a8) in [192..223] then
            fie.ReadBuffer(a8,1)        // two-octet length
          else if Byte(a8) in [224..254] then
            raise Exception.Create('')  // partial length (we just hope that these don't occur)
          else
            ; // one-octet-length
        end;

        fie.ReadBuffer(a8,1);           // read signature version

        if Byte(a8)=3 then              // version 3 signature
        begin
          fie.ReadBuffer(abl[1],16);
          if (abl[1]<>5) or not (abl[16] in [1..3,5..7]) then
            raise Exception.Create('');
          s := PGP_HashAlgos[abl[16]];
        end else
        if Byte(a8)=4 then              // version 4 signature
        begin
          fie.ReadBuffer(abl[1],3);
          if not (abl[3] in [1..3,5..7]) then
            raise Exception.Create('');
          s := PGP_HashAlgos[abl[3]];
        end else
          raise Exception.Create('');
      finally
        fie.Free;
      end;
    except
      s:='';
    end;
    fis.Seek(0,soFromBeginning);

    data := TMemoryStream.Create;

    MimeNewType(hd,'signed','application/pgp-signature',s,hd.Firstempfaenger);

    writeln_s(data,'--'+hd.boundary);
    fie:=TTemporaryFileStream.Create(fi,fmOpenRead);
    CopyStream(fie,data);
    writeln_s(data,'');
    writeln_s(data,'--'+hd.boundary);
    writeln_s(data,'Content-Type: application/pgp-signature');
    writeln_s(data,'');
    CopyStream(fis,data);
    writeln_s(data,'--'+hd.boundary+'--');
    fis.Free;
    fie.Free;

    inc(hd.pgpflags,fPGP_encoded);
    dbReadN(mbase,mb_netztyp,nt);
    nt:=nt or Longint($4000);                         { 's'-Kennzeichnung }
    dbWriteN(mbase,mb_netztyp,nt);
  end else
    rfehler(3002);      { 'PGP-Codierung ist fehlgeschlagen.' }

  if FileExists(fi) then
    _era(fi);         { Temporaerdatei loeschen }
end;

procedure PGP_RequestKey;
var user : string;
    x,y  : Integer;
    brk  : boolean;
    tmp  : string;
    t    : text;
    hd   : string;
    ok   : boolean;
    nt   : byte;
begin
  case aktdispmode of
    1..4   : if dbEOF(ubase) or dbBOF(ubase) then
               user:=''
             else
               user:=dbReadStrN(ubase,ub_username);
    10..19 : if dbEOF(mbase) or dbBOF(mbase) then
               user:=''
             else
               user:=dbReadStrN(mbase,mb_absender);
    else     user:='';
  end;
  dialog(58,3,getres2(3001,1),x,y);   { 'PGP-Key anfordern bei ...' }
  maddstring(3,2,'',user,52,AdrLen,''); mhnr(93);
  mappcustomsel(seluser,false);
  ccte_nobrett:=true;
  msetvfunc(cc_testempf);
  ok := false;
  repeat
    readmask(brk);
    if not brk then begin
      dbSeek(ubase,uiName,UpperCase(user));
      nt:=ntBoxNetztyp(dbReadStrN(ubase,ub_pollbox));
      ok:=not dbFound or ntPGP(nt);
      if not ok then
        rfehler1(3003,ntname(nt));   { 'Beim Netztyp %s wird PGP nicht unterstuetzt.' }
      end;
  until brk or ok;
  ccte_nobrett:=false;
  closemask;
  closebox;
  if not brk then begin
    tmp:=TempS(1024);
    assign(t,tmp);
    rewrite(t);
    writeln(t);
    writeln(t,getres2(3001,3));  { 'Falls Ihre Software PGP nach dem ZCONNECT-Standard unterstuetzt,' }
    writeln(t,getres2(3001,4));  { 'sollte sie auf diese Nachricht mit dem Verschicken Ihres PGP' }
    writeln(t,getres2(3001,5));  { 'Public Key antworten.' }
    writeln(t);
    writeln(t,getres2(3001,6));  { '- automatisch erzeugte Nachricht -' }
    writeln(t);
    close(t);
    hd:='';
    if DoSend(true,tmp,true,false,user,getres2(3001,2),  { 'PGP-Keyanforderung' }
              false,false,false,false,true,nil,
              hd,SendPGPreq) then;
    SafeDeleteFile(tmp);
    end;
  freeres;
end;


function IsBinaryFile(fn:string):boolean;
const bufs  = 2048;                      {         Steuerzeichen }
var   f     : file;
      isbin : boolean;
      buf   : charrp;
      rr    : Integer;
begin
  assign(f,fn);
  reset(f,1);
  getmem(buf,bufs);
  isbin:=false;
  while not isbin and not eof(f) do begin
    blockread(f,buf^,bufs,rr);
    if rr>0 then
      isbin:=testbin(buf^,rr);
    end;
  close(f);
  freemem(buf,bufs);
  IsBinaryFile:=isbin;
end;


procedure PGP_DecodeMessage(hdp:theader; sigtest:boolean);
var tmp,tmp2 : string;
    _source  : string;
    f,f2     : file;
    orgsize  : longint;
    b        : byte;
    l        : longint;
    pass     : string;

  procedure WrSigflag(n:byte);
  var l : longint;
  begin
    dbReadN(mbase,mb_flags,l);
    l:=l or n;
    dbWriteN(mbase,mb_flags,l);
  end;

begin
  tmp:=TempS(dbReadInt(mbase,'groesse'));
  assign(f,tmp);
  rewrite(f,1);
  XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
  close(f);
  tmp2:=TempS(dbReadInt(mbase,'groesse'));

  if sigtest then
    PGP_WaitKey:=true;

  pass:='';
  if PGPVersion=PGP2 then begin
    { Passphrase nicht bei Signaturtest }
    if not sigtest then begin
      pass:=GetEnv('PASSPHRASE');
      if pass<>'' then pass:='"'+pass+'"';
    end;
    { Passphrase nur bei PGP 2.x uebergeben... }
    RunPGP(tmp+' '+pass+' -o '+tmp2)
    { ... RUNPGP5 haengt sie selbst mit an, falls noetig. }
  end else if PGPVersion=PGP5 then
    RunPGP5('PGPV.EXE',tmp+' -o '+tmp2)
  else if PGPVersion=GPG then
    RunGPG('-o '+tmp2+' '+tmp)
  else begin
    { Sourcefile xxxx.TMP nach xxxx kopieren }
    _source:=ExtractFilePath(tmp)+GetBareFileName(tmp)+'.asc';
    copyfile(tmp,_source);
    { Ausgabedateiname = tmp ohne ext }
    RunPGP(_source+' '+tmp2);
    tmp2:=ExtractFilePath(tmp2)+GetBareFileName(_source);
  end;

  if sigtest then begin
    PGP_WaitKey:=false;
    SafeDeleteFile(tmp);
    SafeDeleteFile(_source);
  end;

  if not FileExists(tmp2) then begin
    if sigtest then begin
      if errorlevel=18 then begin
        trfehler(3007,30);  { 'Ungueltige Signatur!' }
        WrSigflag(flag_PGPSigErr);
      end else
        trfehler(3006,30)   { 'Ueberpruefung der Signatur ist fehlgeschlagen' }
    end else
      trfehler(3004,30);    { 'Decodierung ist fehlgeschlagen.' }
  end else begin { Ausgabedatei korrekt geschrieben: }
    if not SigTest then begin
      PGP_BeginSavekey;
      orgsize:=hdp.groesse;
      hdp.groesse:=_filesize(tmp2);
      hdp.komlen:=hdp.crypt.komlen; hdp.crypt.komlen:=0;
      hdp.typ:=iifc(IsBinaryFile(tmp2),'B','T'); hdp.crypt.typ:='';
      hdp.pgpflags:=hdp.pgpflags and (not (fPGP_encoded+fPGP_signed+fPGP_clearsig));
      if hdp.crypt.charset<>'' then begin
        hdp.charset:=UpperCase(hdp.crypt.charset);
        hdp.crypt.charset:='';
      end;
    end;
    if sigtest or (errorlevel=18) then begin
      if errorlevel<>0 then begin
        if errorlevel=18 then
          trfehler(3007,30)  { 'Ungueltige Signatur!' }
        else
          trfehler(3006,30); { 'Ueberpruefung der Signatur ist fehlgeschlagen' }
        hdp.pgpflags := hdp.pgpflags or fPGP_sigerr;
        WrSigflag(flag_PGPSigErr);
      end else begin
        hdp.pgpflags := hdp.pgpflags or fPGP_sigok;
        WrSigflag(flag_PGPSigOk);
      end
    end;

    if sigtest and (errorlevel=0) then begin
      dbReadN(mbase,mb_netztyp,l);
      l:=l or $4000;                      { Flag fuer 'Signatur vorhanden' }
      dbWriteN(mbase,mb_netztyp,l);
    end else begin
      rewrite(f,1);          { alte Datei ueberschreiben }
      WriteHeader(hdp,f);
      assign(f2,tmp2);
      reset(f2,1);
      fmove(f2,f);
      close(f2);
      close(f);
      SafeDeleteFile(tmp2);
      Xwrite(tmp);
      wrkilled;
      dbWriteN(mbase,mb_typ,hdp.typ[1]);
      dbWriteN(mbase,mb_groesse,hdp.groesse);
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 4;                          { "c"-Flag }
      dbWriteN(mbase,mb_unversandt,b);
      hdp.groesse:=orgsize;
      PGP_EndSavekey;
    end;
  end;
  { Aufraeumen: }
  SafeDeleteFile(tmp);
  SafeDeleteFile(tmp2);
end;


procedure PGP_DecodeMsg(sigtest:boolean);
var hdp : Theader;
    hds : longint;
begin
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  PGP_DecodeMessage(hdp,sigtest);
  Hdp.Free;
  aufbau:=true;
end;


{ Key aus ZCONNECT-Header auslesen und in Binaerdatei speichern }

procedure PGP_DecodeKey(source,dest:string);
const b64tab : array[43..122] of byte =         (63, 0, 0, 0,64,
                53,54,55,56,57,58,59,60,61,62, 0, 0, 0, 0, 0, 0,
                 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
                16,17,18,19,20,21,22,23,24,25,26, 0, 0, 0, 0, 0,
                 0,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                42,43,44,45,46,47,48,49,50,51,52);
var t     : text;
    f     : file;
    p,n   : integer;
    dec   : array[0..29] of byte;
    s     : string;
    found : boolean;
    b1,b2,
    b3,b4 : byte;
    eob   : byte;

  function getbyte:byte;
  var c  : char;
  begin
    c:=s[p];
    inc(p);
    if p>length(s) then begin
      if eoln(t) then s:=''
      else read(t,s);
      p:=1;
      end;
    if c='=' then inc(eob);
    if (c<'+') or (c>'z') or (b64tab[ord(c)]=0) then
      getbyte:=0
    else
      getbyte:=b64tab[ord(c)]-1;
  end;

begin
  assign(t,source);
  if not existf(t) then exit;
  reset(t);
  repeat
    read(t,s);
    found:=(LeftStr(LowerCase(s),15)='pgp-public-key:');
    if not found then readln(t);
  until found or eof(t);
  if found then begin
    assign(f,dest);
    rewrite(f,1);
    s:=trim(mid(s,16));
    p:=1;
    n:=0;
    eob:=0;
    repeat
      b1:=getbyte; b2:=getbyte; b3:=getbyte; b4:=getbyte;
      dec[n]:=b1 shl 2 + b2 shr 4;
      dec[n+1]:=(b2 and 15) shl 4 + b3 shr 2;
      dec[n+2]:=(b3 and 3) shl 6 + b4;
      inc(n,3-eob);
      if n=30 then begin
        blockwrite(f,dec,30);
        n:=0;
      end;
    until p>length(s);
    if n>0 then blockwrite(f,dec,n);
    close(f);
  end;
  close(t);
end;


procedure PGP_ImportKey(auto:boolean);
var hdp      : THeader;
    hds      : longint;
    tmp,tmp2 : string;
    mk       : boolean;
begin
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  if hdp.pgpflags and fPGP_haskey = 0 then
    extract_msg(xTractMsg,'',tmp,false,0)
  else begin
    tmp2:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(xTractPuf,'',tmp2,false,0);
    PGP_DecodeKey(tmp2,tmp);
    SafeDeleteFile(tmp2);
    end;
  if not FileExists(tmp) then
    rfehler(3005)         { 'Fehler beim Auslesen des PGP-Keys' }
  else begin
    if auto then          { 'lese Key aus Nachricht %s von %s ein' }
      LogPGP(reps(getreps2(3002,3,'<'+hdp.msgid+'>'),hdp.absender));
    mk:=PGP_WaitKey;
    if not auto then PGP_WaitKey:=true;
    if PGPVersion=PGP5 then
      RunPGP5('PGPK.EXE','-a '+tmp)
    else if PGPVersion=GPG then
      RunGPG('--import '+tmp)
    else
      RunPGP('-ka '+tmp);

    PGP_WaitKey:=mk;
    SafeDeleteFile(tmp);
  end;
  Hdp.Free;
end;


procedure PGP_EditKey;
var bm : boolean;
begin
  bm:=PGPBatchMode;
  PGPBatchMode:=false;
  if PGPVersion=PGP5 then
    RunPGP5('PGPK.EXE','-e '+IDform(PGP_UserID))
  else if PGPVersion=GPG then
    RunGPG('--edit-key '+IDform(PGP_UserID))
  else
    RunPGP('-ke '+IDform(PGP_UserID));

  PGPBatchMode:=bm;
end;


procedure PGP_RemoveID;
var bm : boolean;
begin
  bm:=PGPBatchMode;
  PGPBatchMode:=false;
  if PGPVersion=PGP5 then
    RunPGP5('PGPK.EXE','-ru '+IDform(PGP_UserID))
  else if PGPVersion=GPG then
    RunGPG('--delete-key '+IDform(PGP_UserID))
  else
    RunPGP('-kr '+IDform(PGP_UserID));

  PGPBatchMode:=bm;
end;


procedure PGP_BeginSavekey;      { Key aus ZCONNECT-Header temporaer sichern }
var hdp : theader;
    hds : longint;
    tmp : string;
begin
  hdp := THeader.Create;
  ReadHeader(hdp,hds,false);
  if hdp.pgpflags and fPGP_haskey<>0 then begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(xTractPuf,'',tmp,false,0);
    savekey:=TempS(hds);
    PGP_DecodeKey(tmp,savekey);
    SafeDeleteFile(tmp);
  end;
  Hdp.Free;
end;


procedure PGP_EndSavekey;
begin
  if Savekey <> '' then
  begin
    SafeDeleteFile(savekey);
    Savekey:='';
  end;
end;

{ Schlüssel auswählen
  keyid:  Derzeitig Keyid, neue wird dort abgelegt.
  hint:   E-Mail-Adresse, wird falls keyid='' benutzt.
  secret: Geheimen Schlüssel auswählen (sonst: öffentlichen)
}
procedure PGP_SelectKey(var keyid:string; hint:string; secret:boolean);
begin




end;

{
  $Log$
  Revision 1.61  2002/01/13 15:15:54  mk
  - new "empfaenger"-handling

  Revision 1.60  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.59  2001/12/02 12:11:21  cl
  - got two range check errors

  Revision 1.58  2001/10/21 19:31:45  ma
  - fixed range check error

  Revision 1.57  2001/10/20 17:26:41  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.56  2001/10/01 19:32:00  ma
  - compiles again (DOS32)

  Revision 1.55  2001/09/29 10:49:46  ma
  - fixed: PGP signing

  Revision 1.54  2001/09/19 18:05:08  cl
  - implemented option "PGP/MIME" in "Config/Extern/PGP"

  Revision 1.53  2001/09/11 14:23:15  cl
  - PGP/MIME application/signed: micalg is now read directly from signature
    file created by PGP.

  Revision 1.52  2001/09/10 17:26:46  cl
  - imporved detection of HASH method used

  Revision 1.51  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.50  2001/09/09 23:08:40  cl
  - BUGFIX: GnuPG was not called with -a switch for multipart/encrypted

  Revision 1.49  2001/09/09 17:40:47  cl
  - moved common code between alle en-/decoding streams to a base class
  - all en-/decoding streams can now destruct the other stream
  - much more elegant way to connect en-/decoding streams to each other

  Revision 1.48  2001/09/09 10:23:20  ml
  - Kylix compatibility stage III
  - compilable in linux

  Revision 1.47  2001/09/08 18:46:43  cl
  - small bug/compiler warning fixes

  Revision 1.46  2001/09/08 16:29:37  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.45  2001/09/08 14:33:51  cl
  - added PGP/MIME support
  - adaptions/fixes for MIME support

  Revision 1.44  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.43  2001/09/07 13:54:23  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.42  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.41  2001/08/12 11:50:43  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.40  2001/08/11 23:06:36  mk
  - changed Pos() to cPos() when possible

  Revision 1.39  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.38  2001/07/28 12:33:33  mk
  - GetEnv is now in OS dependend and not in dos unit

  Revision 1.37  2001/07/23 16:05:22  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.36  2001/06/11 22:23:27  ma
  - added GnuPG support

  Revision 1.35  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.34  2001/01/02 10:05:26  mk
  - implemented Header.References

  Revision 1.33  2000/12/03 12:38:26  mk
  - Header-Record is no an Object

  Revision 1.32  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.31  2000/11/15 23:37:34  fe
  Corrected some string things.

  Revision 1.30  2000/11/14 15:51:35  mk
  - replaced Exist() with FileExists()

  Revision 1.29  2000/11/14 11:14:34  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.28  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.27  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.26  2000/07/21 17:39:57  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.25  2000/07/09 13:37:21  mk
  - Fehlermeldung wenn PGP nicht gefunden korrigiert

  Revision 1.24  2000/07/09 08:35:19  mk
  - AnsiStrings Updates

  Revision 1.23  2000/07/05 17:10:54  mk
  - AnsiString Updates

  Revision 1.22  2000/07/04 12:04:28  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.21  2000/07/03 13:31:43  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.20  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.19  2000/05/08 22:15:55  oh
  -PGP 2.6.x: einmal war ein Space vor dem t drin, zweimal nicht. Angeglichen. Fix fuer den vorherigen Fix.

  Revision 1.18  2000/05/07 17:50:07  mk
  - Limits fuer Dateinamen entfernt

  Revision 1.17  2000/05/07 17:30:12  oh
  - PGP 2.6.3 nutzte t statt -t als Parameter

  Revision 1.16  2000/05/07 10:20:27  oh
  -PGP: Flags s und S fuer die Signatur, PGP-Verbesserungen

  Revision 1.14  2000/04/16 15:19:54  oh
  - PGP 2.6.3, 5.x und 6.5.x-Unterstuetzung komplett funktionstuechtig!

  Revision 1.13  2000/04/04 21:01:24  mk
  - Bugfixes fuer VP sowie Assembler-Routinen an VP angepasst

  Revision 1.12  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.11  2000/03/24 04:15:22  oh
  - PGP 6.5.x Unterstuetzung

  Revision 1.10  2000/03/19 12:05:42  mk
  + Flags c und s werden korrekt gesetzt
  + 2.6.x/5.x: Signatur pruefen/Nachricht dekodieren ueber N/G/(S/d).
  + Bug behoben: es wurde kodiert/signiert statt signiert und umgekehrt.

  Revision 1.9  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.8  2000/03/14 15:15:41  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/06 13:48:38  mk
  OH: PGP-Fixes

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


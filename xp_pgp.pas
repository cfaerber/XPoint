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

{$I XPDEFINE.INC }

{ OpenXP PGP handling unit }
unit  xp_pgp;

interface

uses
  sysutils,xpglobal,typeform,fileio,resource,database,maske, xpheader,
{$IFDEF unix}
  linux,
{$ENDIF}
  xp0,xp1;

procedure LogPGP(s:string);                  { s in PGP.LOG schreiben         }
procedure RunPGP(par:string);                { PGP 2.6.x bzw. 6.5.x aufrufen  }
procedure RunPGP5(exe:string;par:string);    { PGP 5.x aufrufen               }
procedure UpdateKeyfile;
procedure WritePGPkey_header(var f:file);    { PGP-PUBLIC-KEY: ... erzeugen   }
procedure PGP_SendKey(empfaenger:string);    { Antwort auf Key-Request senden }
procedure PGP_EncodeFile(var source:file; var hd: Theader;
                         fn,RemoteUserID:string; encode,sign:boolean;
                         var fido_origin:string);

procedure PGP_RequestKey;
procedure PGP_DecodeMessage(hdp:Theader; sigtest:boolean);
procedure PGP_DecodeMsg(sigtest:boolean);  { dec. und/oder Signatur testen }
procedure PGP_DecodeKey(source,dest:string);
procedure PGP_ImportKey(auto:boolean);
procedure PGP_EditKey;
procedure PGP_RemoveID;

procedure PGP_BeginSavekey;      { Key aus ZCONNECT-Header temporaer sichern }
procedure PGP_EndSavekey;


implementation  { --------------------------------------------------- }

uses  xp3,xp3o,xp3o2,xp3ex,xp6,
  {$ifdef Win32} xpwin32, {$endif}
  xpcc,xpnt;

const
  savekey : string = '';
  flag_PGPSigOk = $01;
  flag_PGPSigErr = $02;

function testbin(var bdata; rr:word):boolean; assembler; {&uses esi}
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
    if path<>'' then begin
      if lastchar(path)='\' then dellast(path);
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
    dir, name, ext: shortstring;
    {$endif}
begin
  {$ifdef unix}
  fsplit(exe,dir,name,ext);
  exe:=LowerCase(name); { aus PGPK.EXE wird pgpk etc ...}
  {$endif}
  path:=getenv('PGPPATH');
  if path<>'' then begin
    if lastchar(path)='\' then dellast(path);
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
      if lastchar(path)='\' then dellast(path);
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
  IDform:=s;
end;


procedure UpdateKeyfile;
var secring : string;
begin
  if UsePGP and (PGP_UserID<>'') then begin
    secring:=filesearch('PUBRING.PGP',getenv('PGPPATH'));
    if (secring<>'') and (filetime(secring)>filetime(PGPkeyfile)) then begin
      if FileExists(PGPkeyfile) then _era(PGPkeyfile);
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


procedure WritePGPkey_header(var f:file);    { PGP-PUBLIC-KEY: ... erzeugen }
var kf  : file;
    dat : array[0..29] of byte;
    rr  : word;
    i,j : integer;
    s   : string;
    b64 : array[0..63] of char;

  procedure wrs(s:string);
  begin
    blockwrite(f,s[1],length(s));
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
  if DoSend(true,tmp,empfaenger,getres2(3000,1),  { 'Antwort auf PGP-Key-Anforderung' }
            false,false,false,false,true,nil,
            hd,hd,SendPGPkey) then
    LogPGP(getreps2(3002,1,empfaenger));   { 'sende Public Key an %s' }
  if FileExists(tmp) then _era(tmp);
  freeres;
end;


{ Text aus 'source' codieren bzw. signieren und zusammen mit }
{ Header 'hd' in Datei 'fn' ablegen.                         }
{ Bei Fido-Nachrichten Origin abschneiden und nach Codierung }
{ / Signierung wieder anhaengen.                             }

procedure PGP_EncodeFile(var source:file; var hd: theader;
                         fn,RemoteUserID:string; encode,sign:boolean;
                         var fido_origin:string);
var tmp  : string;
    f,f2 : file;
    b    : byte;
    nt   : longint;
    t    : string;
    OwnUserID  : string;
    _source: string;

  procedure StripOrigin;
  begin
    reset(source,1);
    seek(source,filesize(source)-length(fido_origin)-2);
    truncate(source);
    close(source);
  end;

  procedure AddOrigin;
  var f : file;
      s : string;
  begin
    assign(f,tmp);
    if existf(f) then begin
      reset(f,1);
      seek(f,filesize(f));
      s:=fido_origin+#13#10;
      blockwrite(f,s[1],length(s));
      close(f);
    end;
  end;

begin
  if RemoteUserID='' then                       { User-ID ermitteln }
    RemoteUserID:=hd.empfaenger;
  if cPos('/',RemoteUserID)>0 then
    RemoteUserID:='';                           { Empfaenger ist Brett }

  fm_ro; reset(source,1); fm_rw;
  tmp:=TempS(filesize(source)*2);         { Temp-Dateinamen erzeugen }
  close(source);

  if fido_origin<>'' then StripOrigin;
  if PGPVersion=GPG then
    t:=iifs(hd.typ='T','a','')
  else if PGPVersion=PGP2 then
    t:=iifs(hd.typ='T','t',' +textmode=off')
  else
    t:=iifs(hd.typ='T','-t','');

  if PGP_UserID<>'' then
    OwnUserID:=IDform(PGP_UserID)
  else
    OwnUserID:='';

  { --- codieren --- }
  if encode and not sign then begin
    if PGPVersion=PGP2 then
      RunPGP('-ea'+t+' '+filename(source)+' '+IDform(RemoteUserID)+' -o '+tmp)
    else if PGPVersion=PGP5 then
      RunPGP5('PGPE.EXE','-a '+t+' '+filename(source)+' -r '+IDform(RemoteUserID)+' -o '+tmp)
    else if PGPVersion=GPG then
      RunGPG('-e'+t+' -o '+tmp+' -r '+IDform(RemoteUserID)+' '+PGP_GPGEncodingOptions+' '+filename(source))
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-e -a '+t+' '+_source+' '+IDform(RemoteUserID));
      if FileExists(tmp) then _era(tmp);         { xxxx wieder loeschen }
      tmp:=_source+'.asc';
    end;

  { --- signieren --- }
  end else if sign and not encode then begin
    if PGPVersion=PGP2 then
      RunPGP('-sa'+t+' '+filename(source)+OwnUserID+' -o '+tmp )
    else if PGPVersion=PGP5 then
      RunPGP5('PGPS.EXE','-a '+t+' '+filename(source)+OwnUserID+' -o '+tmp )
    else if PGPVersion=GPG then
      RunGPG(iifs(hd.typ='T','--clearsign','-s')+' --force-v3-sigs -o '+tmp+' -u '+OwnUserID+' '+filename(source))
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-s -a '+t+' '+_source+' '+IDform(RemoteUserID)+OwnUserID);
      if FileExists(getbarefilename(tmp)) then _era(getbarefilename(tmp));         { Temporaerdatei loeschen }
      if FileExists(tmp) then _era(tmp);         { xxxx wieder loeschen }
      tmp:=_source+'.asc';
    end;

  { --- codieren+signieren --- }
  end else begin
    if PGPVersion=PGP2 then
      RunPGP('-esa'+t+' '+filename(source)+' '+IDform(RemoteUserID)+OwnUserID+' -o '+tmp)
    else if PGPVersion=PGP5 then
      RunPGP5('PGPE.EXE','-sa '+t+' '+filename(source)+' -r '+IDform(RemoteUserID)+OwnUserID+' -o '+tmp)
    else if PGPVersion=GPG then
      RunGPG('-es'+t+' --force-v3-sigs -o '+tmp+' -u '+OwnUserID+' -r '+IDform(RemoteUserID)+' '+PGP_GPGEncodingOptions+' '+filename(source))
    else begin
      { Sourcefile xxxx.TMP nach xxxx kopieren }
      _source:=ExtractFilePath(filename(source))+GetBareFileName(filename(source));
      copyfile(filename(source),_source);
      { Ausgabedateiname ist _source'.asc' }
      RunPGP('-e -s -a '+t+' '+_source+' '+IDform(RemoteUserID)+OwnUserID);
      if FileExists(tmp) then _era(tmp);         { xxxx wieder loeschen }
      tmp:=_source+'.asc';
    end;
  end;

  if fido_origin<>'' then AddOrigin;

  if FileExists(tmp) then begin
    hd.groesse:=_filesize(tmp);               { Groesse anpassen }
    hd.crypttyp:=hd.typ; hd.typ:='T';         { Typ anpassen   }
    hd.ccharset:=hd.charset; hd.charset:='';  { Charset anpassen }
    hd.ckomlen:=hd.komlen; hd.komlen:=0;      { KOM anpassen   }
    if encode then inc(hd.pgpflags,fPGP_encoded);
    if sign then inc(hd.pgpflags,iif(encode,fPGP_signed,fPGP_clearsig));
    assign(f,fn);
    rewrite(f,1);
    WriteHeader(hd,f);                   { neuen Header erzeugen }
    assign(f2,tmp);
    reset(f2,1);
    fmove(f2,f);                          { ... und codierte Datei dranhaengen }
    close(f2);
    close(f);
    if FileExists(tmp) then _era(tmp);         { Temporaerdatei loeschen }
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
    if DoSend(true,tmp,user,getres2(3001,2),  { 'PGP-Keyanforderung' }
              false,false,false,false,true,nil,
              hd,hd,SendPGPreq) then;
    if FileExists(tmp) then _era(tmp);
    end;
  freeres;
end;


function IsBinaryFile(fn:string):boolean;
const bufs  = 2048;                      {         Steuerzeichen }
var   f     : file;
      isbin : boolean;
      buf   : charrp;
      rr    : word;
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
    if FileExists(tmp) then _era(tmp);
    if FileExists(_source) then _era(_source);
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
      hdp.komlen:=hdp.ckomlen; hdp.ckomlen:=0;
      hdp.typ:=iifc(IsBinaryFile(tmp2),'B','T'); hdp.crypttyp:='';
      hdp.pgpflags:=hdp.pgpflags and (not (fPGP_encoded+fPGP_signed+fPGP_clearsig));
      if hdp.ccharset<>'' then begin
        hdp.charset:=UpperCase(hdp.ccharset);
        hdp.ccharset:='';
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
      if FileExists(tmp2) then _era(tmp2);
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
  if FileExists(tmp) then _era(tmp);
  if FileExists(tmp) then _era(tmp2);
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
    if FileExists(tmp2) then _era(tmp2);
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
    if FileExists(tmp) then _era(tmp);
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
    if FileExists(tmp) then _era(tmp);
  end;
  Hdp.Free;
end;


procedure PGP_EndSavekey;
begin
  if (savekey<>'') and FileExists(savekey) then
    _era(savekey);
  savekey:='';
end;


end.
{
  $Log$
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

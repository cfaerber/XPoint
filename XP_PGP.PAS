{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
{ --------------------------------------------------------------- }

{ PGP-Codierung }

{$O+,F+,B-,R+}

unit  xp_pgp;

interface

uses  dos,typeform,fileio,resource,database,maske,xp0,xp1;


procedure LogPGP(s:string);                  { s in PGP.LOG schreiben         }
procedure RunPGP(par:string);                { PGP.EXE aufrufen               }
procedure UpdateKeyfile;
procedure WritePGPkey_header(var f:file);    { PGP-PUBLIC-KEY: ... erzeugen   }
procedure PGP_SendKey(empfaenger:string);    { Antwort auf Key-Request senden }
procedure PGP_EncodeFile(var source:file; var hd:xp0.header;
                         fn,UserID:string; encode,sign:boolean;
                         var fido_origin:string);

procedure PGP_RequestKey;
procedure PGP_DecodeMessage(hdp:headerp; sigtest:boolean);
procedure PGP_DecodeMsg(sigtest:boolean);  { dec. und/oder Signatur testen }
procedure PGP_DecodeKey(source,dest:pathstr);
procedure PGP_ImportKey(auto:boolean);
procedure PGP_EditKey;
procedure PGP_RemoveID;

procedure PGP_BeginSavekey;      { Key aus ZCONNECT-Header temporÑr sichern }
procedure PGP_EndSavekey;


implementation  { --------------------------------------------------- }

uses  xp3,xp3o,xp3o2,xp3ex,xp6,xpcc,xpnt;

const savekey : pathstr = '';


function testbin(var bdata; rr:word):boolean; near; external;
{$L xp_pgp.obj}


procedure LogPGP(s:string);
var t : text;
begin
  {$I-}
  assign(t,LogPath+'PGP.LOG');
  if existf(t) then append(t)
  else rewrite(t);
  writeln(t,left(date,6),right(date,2),' ',time,'  ',s);
  close(t);
  {$I+}
  if ioresult<>0 then;
end;


procedure RunPGP(par:string);
var path : pathstr;
begin
  if exist('XPGP.BAT') then
    path:='XPGP.BAT'
  else begin
    path:=getenv('PGPPATH');
    if path<>'' then begin
      if lastchar(path)='\' then dellast(path);
      path:=fsearch('PGP.EXE',path);
      end;
    if path='' then
      path:=fsearch('PGP.EXE',getenv('PATH'));
    end;
  if path='' then
    trfehler(3001,30)    { 'PGP.EXE fehlt oder ist nicht per Pfad erreichbar.' }
  else begin
    shellkey:=PGP_WaitKey;
    shell(path+iifs(PGPbatchmode,' +batchmode ',' ')+par,500,1);
    shellkey:=false;
    end;
end;


{ User-ID fÅr Command-Line-Aufruf in AnfÅhrungszeichen setzen }

function IDform(s:string):string;
begin
  if multipos(' /<>|',s) then begin
    if firstchar(s)<>'"' then s:='"'+s;
    if lastchar(s)<>'"' then s:=s+'"';
    end;
  IDform:=s;
end;


procedure UpdateKeyfile;
var secring : pathstr;
begin
  if UsePGP and (PGP_UserID<>'') then begin
    secring:=fsearch('PUBRING.PGP',getenv('PGPPATH'));
    if (secring<>'') and (filetime(secring)>filetime(PGPkeyfile)) then begin
      if exist(PGPkeyfile) then _era(PGPkeyfile);
      RunPGP('-kx +armor=off '+IDform(PGP_UserID)+' '+PGPkeyfile);
      end;
    end;
end;


procedure WritePGPkey_header(var f:file);    { PGP-PUBLIC-KEY: ... erzeugen }
var kf  : file;
    dat : array[0..29] of byte;
    rr  : word;
    i,j : integer;
    s   : string[40];
    b64 : array[0..63] of char;

  procedure wrs(s:string);
  begin
    blockwrite(f,s[1],length(s));
  end;

begin
  UpdateKeyfile;
  if (savekey<>'') and exist(savekey) then
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
      s[0]:=chr(j);
      wrs(s);
      end;
    close(kf);
    wrs(#13#10);
    end;
end;


procedure PGP_SendKey(empfaenger:string);   { Antwort auf Key-Request senden }
var t   : text;
    tmp : pathstr;
    hd  : string[12];
begin
  UpdateKeyfile;
  if not exist('pgp-key.bin') then exit;
  tmp:=TempS(4096);
  assign(t,tmp);
  rewrite(t);
  writeln(t);
  writeln(t,getres2(3000,2));   { 'Der Header dieser Nachricht enthÑlt den angeforderten PGP-Public-Key.' }
  writeln(t,getres2(3000,3));   { 'Diese Nachricht wurde von CrossPoint automatisch erzeugt.' }
  writeln(t);
  close(t);
  hd:='';
  if DoSend(true,tmp,empfaenger,getres2(3000,1),  { 'Antwort auf PGP-Key-Anforderung' }
            false,false,false,false,true,nil,
            hd,hd,SendPGPkey) then
    LogPGP(getreps2(3002,1,empfaenger));   { 'sende Public Key an %s' }
  _era(tmp);
  freeres;
end;


{ Text aus 'source' codieren bzw. signieren und zusammen mit }
{ Header 'hd' in Datei 'fn' ablegen.                         }
{ Bei Fido-Nachrichten Origin abschneiden und nach Codierung }
{ / Signierung wieder anhÑngen.                              }

procedure PGP_EncodeFile(var source:file; var hd:xp0.header;
                         fn,UserID:string; encode,sign:boolean;
                         var fido_origin:string);
var tmp  : pathstr;
    f,f2 : file;
    b    : byte;
    nt   : longint;
    opt  : string[5];
    t    : string[20];
    uid  : string[80];

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
  if UserID='' then                       { User-ID ermitteln }
    UserID:=hd.empfaenger;
  fm_ro; reset(source,1); fm_rw;
  tmp:=TempS(filesize(source)*2);         { Temp-Dateinamen erzeugen }
  close(source);

  if fido_origin<>'' then StripOrigin;
  t:=iifs(hd.typ='T','t',' +textmode=off');
  if PGP_UserID<>'' then uid:=' -u '+IDform(PGP_UserID)
  else uid:='';
  if encode and not sign then                           { codieren }
    RunPGP('-ea'+t+' '+filename(source)+' '+IDform(UserID)+' -o '+tmp)
  else if not encode and sign  then                     { signieren }
    RunPGP('-sa'+t+' '+filename(source)+' -o '+tmp+uid)
  else                                                  { codieren+signieren }
    RunPGP('-esa'+t+' '+filename(source)+' '+IDform(UserID)+' -o '+tmp
           {+uid wird zu lang!} );
  if fido_origin<>'' then AddOrigin;

  if exist(tmp) then begin
    hd.groesse:=_filesize(tmp);               { Grî·e anpassen }
    hd.crypttyp:=hd.typ; hd.typ:='T';         { Typ anpassen   }
    hd.ccharset:=hd.charset; hd.charset:='';  { Charset anpassen }
    hd.ckomlen:=hd.komlen; hd.komlen:=0;      { KOM anpassen   }
    if encode then inc(hd.pgpflags,fPGP_encoded);
    if sign then inc(hd.pgpflags,iif(encode,fPGP_signed,fPGP_clearsig));
    assign(f,fn);
    rewrite(f,1);
    WriteHeader(hd,f,_ref6list);          { neuen Header erzeugen }
    assign(f2,tmp);
    reset(f2,1);
    fmove(f2,f);                          { ... und codierte Datei dranhÑngen }
    close(f2);
    close(f);
    _era(tmp);                            { TemporÑrdatei lîschen }
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
    end
  else
    rfehler(3002);      { 'PGP-Codierung ist fehlgeschlagen.' }
end;


procedure PGP_RequestKey;
var user : string[AdrLen];
    x,y  : byte;
    brk  : boolean;
    tmp  : pathstr;
    t    : text;
    hd   : string[12];
    ok   : boolean;
    nt   : byte;
begin
  case aktdispmode of
    1..4   : if dbEOF(ubase) or dbBOF(ubase) then
               user:=''
             else
               user:=dbReadStr(ubase,'username');
    10..19 : if dbEOF(mbase) or dbBOF(mbase) then
               user:=''
             else
               user:=dbReadStr(mbase,'absender');
    else     user:='';
  end;
  dialog(58,3,getres2(3001,1),x,y);   { 'PGP-Key anfordern bei ...' }
  maddstring(3,2,'',user,52,AdrLen,''); mhnr(93);
  mappcustomsel(seluser,false);
  ccte_nobrett:=true;
  msetvfunc(cc_testempf);
  repeat
    readmask(brk);
    if not brk then begin
      dbSeek(ubase,uiName,ustr(user));
      nt:=ntBoxNetztyp(dbReadStr(ubase,'pollbox'));
      ok:=not dbFound or ntPGP(nt);
      if not ok then
        rfehler1(3003,ntname(nt));   { 'Beim Netztyp %s wird PGP nicht unterstÅtzt.' }
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
    writeln(t,getres2(3001,3));  { 'Falls Ihre Software PGP nach dem ZCONNECT-Standard unterstÅtzt,' }
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
    _era(tmp);
    end;
  freeres;
end;


function IsBinaryFile(fn:pathstr):boolean;
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


procedure PGP_DecodeMessage(hdp:headerp; sigtest:boolean);
var tmp,tmp2 : pathstr;
    f,f2     : file;
    orgsize  : longint;
    b        : byte;
    l        : longint;
    mw       : boolean;

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
  if sigtest then begin
    mw:=PGP_WaitKey;
    PGP_WaitKey:=true;
    end;
  RunPGP(tmp+' -o '+tmp2);
  if sigtest then begin
    PGP_WaitKey:=false;
    _era(tmp);
    _era(tmp2);
    exit;
    end;
  if not exist(tmp2) then
    if sigtest then
      if errorlevel=18 then begin
        trfehler(3007,7);  { 'PGP meldet ungÅltige Signatur!' }
        WrSigflag(2);      { Signatur fehlerhaft }
        end
      else
        trfehler(3007,6)   { 'öberprÅfung der PGP-Signatur ist fehlgeschlagen' }
    else
      trfehler(3004,5)     { 'PGP-Decodierung ist fehlgeschlagen.' }
  else begin
    PGP_BeginSavekey;
    orgsize:=hdp^.groesse;
    hdp^.groesse:=_filesize(tmp2);
    hdp^.komlen:=hdp^.ckomlen; hdp^.ckomlen:=0;
    hdp^.typ:=iifc(IsBinaryFile(tmp2),'B','T'); hdp^.crypttyp:='';
    hdp^.pgpflags:=hdp^.pgpflags and (not (fPGP_encoded+fPGP_signed+fPGP_clearsig));
    if hdp^.ccharset<>'' then begin
      hdp^.charset:=ustr(hdp^.ccharset);
      hdp^.ccharset:='';
      end;
    if sigtest or (errorlevel=18) then
      if errorlevel<>0 then begin
        hdp^.pgpflags := hdp^.pgpflags or fPGP_sigerr;
        WrSigflag(2);
        end
      else begin
        hdp^.pgpflags := hdp^.pgpflags or fPGP_sigok;
        WrSigflag(1);
        end;
    rewrite(f,1);          { alte Datei Åberschreiben }
    WriteHeader(hdp^,f,reflist);
    assign(f2,tmp2);
    reset(f2,1);
    fmove(f2,f);
    close(f2);
    close(f);
    _era(tmp2);
    Xwrite(tmp);
    wrkilled;
    dbWriteN(mbase,mb_typ,hdp^.typ[1]);
    dbWriteN(mbase,mb_groesse,hdp^.groesse);
    if sigtest then begin
      dbReadN(mbase,mb_netztyp,l);
      l:=l or $4000;                      { "s"-Flag }
      dbWriteN(mbase,mb_netztyp,l);
      end
    else begin
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 4;                          { "c"-Flag }
      dbWriteN(mbase,mb_unversandt,b);
      end;
    hdp^.groesse:=orgsize;
    PGP_EndSavekey;
    end;
  _era(tmp);
end;


procedure PGP_DecodeMsg(sigtest:boolean);
var hdp : headerp;
    hds : longint;
begin
  new(hdp);
  ReadHeader(hdp^,hds,true);
  PGP_DecodeMessage(hdp,sigtest);
  dispose(hdp);
  aufbau:=true;
end;


{ Key aus ZCONNECT-Header auslesen und in BinÑrdatei speichern }

procedure PGP_DecodeKey(source,dest:pathstr);
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
      b  : byte;
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
    found:=(left(lstr(s),15)='pgp-public-key:');
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
var hdp      : headerp;
    hds      : longint;
    tmp,tmp2 : pathstr;
    mk       : boolean;
begin
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  new(hdp);
  ReadHeader(hdp^,hds,true);
  if hdp^.pgpflags and fPGP_haskey = 0 then
    extract_msg(xTractMsg,'',tmp,false,0)
  else begin
    tmp2:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(xTractPuf,'',tmp2,false,0);
    PGP_DecodeKey(tmp2,tmp);
    _era(tmp2);
    end;
  if not exist(tmp) then
    rfehler(3005)         { 'Fehler beim Auslesen des PGP-Keys' }
  else begin
    if auto then          { 'lese Key aus Nachricht %s von %s ein' }
      LogPGP(reps(getreps2(3002,3,'<'+hdp^.msgid+'>'),hdp^.absender));
    mk:=PGP_WaitKey;
    if not auto then PGP_WaitKey:=true;
    RunPGP('-ka '+tmp);
    PGP_WaitKey:=mk;
    _era(tmp);
    end;
  dispose(hdp);
end;


procedure PGP_EditKey;
var bm : boolean;
begin
  bm:=PGPBatchMode;
  PGPBatchMode:=false;
  RunPGP('-ke '+IDform(PGP_UserID));
  PGPBatchMode:=bm;
end;


procedure PGP_RemoveID;
var bm : boolean;
begin
  bm:=PGPBatchMode;
  PGPBatchMode:=false;
  RunPGP('-kr '+IDform(PGP_UserID));
  PGPBatchMode:=bm;
end;


procedure PGP_BeginSavekey;      { Key aus ZCONNECT-Header temporÑr sichern }
var hdp : headerp;
    hds : longint;
    tmp : pathstr;
begin
  new(hdp);
  ReadHeader(hdp^,hds,false);
  if hdp^.pgpflags and fPGP_haskey<>0 then begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(xTractPuf,'',tmp,false,0);
    savekey:=TempS(hds);
    PGP_DecodeKey(tmp,savekey);
    _era(tmp);
    end;
  dispose(hdp);
end;


procedure PGP_EndSavekey;
begin
  if (savekey<>'') and exist(savekey) then
    _era(savekey);
  savekey:='';
end;


end.


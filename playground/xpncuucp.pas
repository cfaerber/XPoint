{  $Id$

   OpenXP UUCP netcall routines
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{ OpenXP UUCP netcall unit }
unit xpncuucp;

interface

implementation

var uunum : word;    { fortlaufende 16-Bit-Nummer der UUCP-Dateien }

function uu_nummer:word;     { nächste Paketnummer aus UUNUMMER.DAT lesen }
var t : text;
    s : string;
begin
  if _filesize(UUnumdat)<2 then
    uu_nummer:=1
  else begin
    assign(t,UUnumdat);
    reset(t);
    readln(t,s);
    close(t);
    uu_nummer:=minmax(ival(s),0,$ffff);
    end;
end;

function GetNextUUnummer:word;   { nächste Nummer aus C-File auslesen }
var t : text;
    s : string;
    w : word;
begin
  w:=uu_nummer;
  if FileExists(XFerDir+caller) and (_filesize(XFerDir+caller)>0) then begin
    assign(t,XFerDir+caller);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if LeftStr(s,4)='S D.' then begin
        s:=trim(mid(s,cpos(' ',s)));
        s:=LeftStr(s,cpos(' ',s)-1);
        w:=hexval(RightStr(s,4));
        end;
      end;
    close(t);
    if w=$ffff then w:=0
    else inc(w);
    end;
  GetNextUUnummer:=w;
end;

procedure WriteUUnummer(w:word);    { nächste Nr. in UUNUMER.DAT schreiben }
var t : text;
begin
  assign(t,UUnumdat);
  rewrite(t);
  writeln(t,w);
  close(t);
end;


(*
procedure NoUUZ;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(105,30);    { 'Netcall-Konvetierer UUZ.EXE fehlt!' }
  twin;
end; *)

procedure NoUUCICO;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(110,30);    { 'UUCICO.EXE fehlt!' }
  twin;
end;

procedure PackFehler;
begin
  {window(1,1,screenwidth,screenlines);}
  trfehler(713,30);    { 'Fehler beim Packen!' }
  twin;
end;


{ Puffer in RFC-Files konvertieren }

procedure ZtoRFC(cleardir:boolean; source,destdir:string);
var sr    : tsearchrec;
    rc    : integer;
    f1,f2 : ^file;
    s     : shortstring;
    p     : byte;
    cunb  : string;
    news  : boolean;
    freeze: boolean;
    gzip  : boolean;
    bzip  : boolean;
    f     : boolean;
    uu: TUUZ;

  procedure NoCompSmtp(w:word);       { rcsmtp -> csmtp }
  var f1,f2 : file;
      s     : string[40];
      p     : byte;
      rr    : word;
      adr   : longint;
  begin
    if w=$ffff then w:=0
    else inc(w);
    if FileExists(DestDir+'X-'+hex(w,4)+'.OUT') then begin
      assign(f1,DestDir+'X-'+hex(w,4)+'.OUT');
      reset(f1,1);
      adr:=0;
      assign(f2,DestDir+'smtp.tmp');
      rewrite(f2,1);
      repeat
        seek(f1,adr);
        blockread(f1,s[1],40,rr);
        s[0]:=chr(rr);
        p:=cpos(#10,s);
        s[0]:=chr(p-1);
        inc(adr,p);
        if (s='C rcsmtp') or (s='C rfsmtp') or (s='C rgsmtp') or (c='rzsmtp')
        then
          s:='C rsmtp';
        s:=s+#10;
        blockwrite(f2,s[1],length(s));
      until adr>=filesize(f1);
      close(f1);
      close(f2);
      erase(f1);
      rename(f2,DestDir+'X-'+hex(w,4)+'.OUT');
      end;
  end;

begin
  if cleardir then begin                { Spool räumen }
    erase_mask(DestDir+'*.');
    erase_mask(DestDir+'*.OUT');
  end;
  spacksize:=0;
  spufsize:=0;
  MakeMimetypCfg;
  with boxpar^ do begin
    uu := TUUZ.Create;
    if SizeNego then uu.parsize := true;
    if UUsmtp then
      if UpArcer='' then uu.SMTP := true
      else if pos('freeze',LowerCase(uparcer))>0 then uu.fSMTP := true
      else if pos('gzip',LowerCase(uparcer))>0 then uu.zSMTP := true
      else if pos('bzip2',LowerCase(uparcer))>0 then uu.bSMTP := true
      else uu.cSMTP := true;
    if NewsMIME then uu.NewsMime := true;
    if MIMEqp then uu.MakeQP := true;
    if RFC1522 then uu.RFC1522 := true;
    uu.MailUser := BoxPar^.UserName;
    uu.NewsUser := BoxPar^.UserName;
    uu.FileUser := BoxPar^.UserName;
    f:=OutFilter(source);
    uu.Source := source;
    uu.Dest := DestDir;
    uu._from := boxpar^.pointname;
    uu._to := boxpar^.boxname;
    uu.uunumber := hexval(copy(caller,3,4));
    uu.ztou;
    uu.Free;

    if f then _era(source);
    end;
  if errorlevel<>0 then exit;
  if (BoxPar^.uparcer='') or (Netztyp in [nt_NNTP, nt_POP3, nt_IMAP]) then begin             { Mail/News nicht packen }
    spufsize:=FileMaskSize(DestDir+'D*.OUT');
    spacksize:=spufsize;
    end
  else begin                                   { Mail/News packen }
    freeze:=pos('freeze',LowerCase(BoxPar^.uparcer))>0;
    gzip:=pos('gzip',LowerCase(BoxPar^.uparcer))>0;
    bzip:=pos('bzip2',LowerCase(BoxPar^.uparcer))>0;
    new(f1); new(f2);
    p:=pos('$PUFFER',UpperCase(boxpar^.uparcer));
    s[0]:=#8;
    if freeze then cunb:='#! funbatch'#10
    else if gzip then cunb:='#! gunbatch'#10
    else if bzip then cunb:='#! bunbatch'#10
    else cunb:='#! cunbatch'#10;
    rc:= findfirst(DestDir+'D*.OUT',faAnyFile,sr);
    while rc=0 do begin
      inc(spufsize,sr.size);
      assign(f1^,DestDir+sr.name);
      reset(f1^,1);
      blockread(f1^,s[1],8);
      close(f1^);
      news:=(s='#! rnews');
      if news or (LeftStr(s,5)='HELO ') then begin    { News/SMTPmail packen }
        shell(LeftStr(boxpar^.UpArcer,p-1)+DestDir+sr.name+mid(boxpar^.UpArcer,p+7),
              500,3);
        if not existf(f1^) then begin    { Datei wurde gepackt }
          if freeze then assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-2)+'XZ')
          else assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-1)+'Z');
          if (errorlevel<>0) or not existf(f1^) then begin
            PackFehler;
            dispose(f1); dispose(f2);
            exit;
            end;
          if news then begin
            reset(f1^,1);
            assign(f2^,DestDir+sr.name);
            rewrite(f2^,1);                          { cunbatch erzeugen }
            blockwrite(f2^,cunb[1],length(cunb));
            fmove(f1^,f2^);
            close(f1^); close(f2^);
            erase(f1^);
            end
          else
            rename(f1^,DestDir+sr.name);
          end
        else
          if not news then     { SMTP-File nicht gepackt - Packrate zu schlecht }
            NoCompSmtp(hexval(copy(sr.name,3,4)));
        end;
      inc(spacksize,_filesize(DestDir+sr.name));
      rc:= findnext(sr);
    end;
    FindClose(sr);
    dispose(f1); dispose(f2);
    end;
  uunum:=GetNextUUnummer;
end;


{ RFC-Daten aus SPOOL\ konvertieren und einlesen }

function ImportUUCPfromSpool(const XFerDir:string):boolean;
var sr      : tsearchrec;
    rc      : integer;
    f1,f2   : ^file;
    s       : string[80];
    rr      : word;
    uncompy : byte;
    dummy   : longint;
    uu : TUUZ;

  procedure uncompress(fn:string; freeze,gzip,bzip:boolean);
  var s : string;
  begin
    if freeze then s:=boxpar^.unfreezer
    else if gzip then s:=boxpar^.ungzipper
    else if bzip then s:=boxpar^.unbzipper
    else s:=BoxPar^.downarcer;
    exchange(s,'$DOWNFILE',XFerDir+fn+'.Z');
    gotoxy(1,uncompy);
    shell(s,600,5);
    inc(uncompy);
    if uncompy=screenlines-fnkeylines-5 then begin
      clrscr;
      uncompy:=2;
      end;
    if not FileExists(XFerDir+fn) then
      if RenameFile(XFerDir+fn+'.Z',XFerDir+fn) then
        MoveToBad(XFerDir+fn);
  end;

begin
  ImportUUCPfromSpool:=false;
  rc:= findfirst(XFerDir+'D*.',faAnyFile,sr);   { Datenfiles - ohne Extension }
  if rc=0 then begin
    twin;
    clrscr;
    uncompy:=2;
    cursor(curoff);
    new(f1); new(f2);
    while rc=0 do begin
      inc(NC^.recpack,sr.size);
      assign(f1^,XFerDir+sr.name);
      reset(f1^,1);
      blockread(f1^,s[1],40,rr);
      s[0]:=chr(rr);
      if (LeftStr(s,11)='#! cunbatch') or (LeftStr(s,11)='#! funbatch') or   { Datei entpacken }
         (LeftStr(s,11)='#! gunbatch') or (LeftStr(s,11)='#! zunbatch')
      then begin
        assign(f2^,XFerDir+sr.name+'.Z');
        rewrite(f2^,1);
        seek(f1^,cpos(#10,s));
        fmove(f1^,f2^);
        close(f1^); close(f2^);
        uncompress(sr.name,pos('funbatch',s)>0,
                   (pos('gunbatch',s)>0) or (pos('zunbatch',s)>0),
                   pos('bunbatch',s)>0);
        end
      else begin
        close(f1^);
        if (LeftStr(s,2)=#$1f#$9d) or (LeftStr(s,2)=#$1f#$9f) or
           (LeftStr(s,2)=#$1f#$8b) or (LeftStr(s,2)=#$42#$5a) then
           begin     { compressed/frozen SMTP o.ä. }
          rename(f1^,XFerDir+sr.name+'.Z');
          uncompress(sr.name,s[2]=#$9f,s[2]=#$8b,s[2]=#$5a);
          end;
        end;
      inc(NC^.recbuf,_filesize(XFerDir+sr.name));
      rc:= findnext(sr);
    end;
    FindClose(sr);
    dispose(f1); dispose(f2);
    clrscr;
    uu := TUUZ.Create;
    uu.source := XFerDir+'X*.';
    uu.dest := dpuffer;
    uu.OwnSite := boxpar^.pointname+domain;
    uu.utoz;
    uu.free;

    rc:= findfirst(XFerDir+'*.0??',faAnyFile,sr);
    while rc=0 do begin       { abgebrochene UUCP-Files -> BAD }
      MoveToBad(XFerDir+sr.name);
      rc:= findnext(sr);
    end;
    FindClose(sr);
    rc:= findfirst(XFerDir+'D*',faAnyFile,sr);   { übriggebliebene D-Files sicherstellen }
    while rc=0 do begin
      if sr.attr and faArchive<>0 then
        MoveToBad(XFerDir+sr.name);
      rc:= findnext(sr);
    end;
    FindClose(sr);
    erase_mask(xp0.XFerDir+'D*.OUT');        { ausgehende Pakete löschen }
    erase_mask(xp0.XFerDir+'X*.OUT');        { C-File muß stehenbleiben! }
    if nDelPuffer and (errorlevel=0) and (testpuffer(dpuffer,false,dummy)>=0)
    then
      erase_mask(xp0.XFerDir+WildCard);         { entpackte Dateien löschen }
    CallFilter(true,dpuffer);
    if _filesize(dpuffer)>0 then
      if PufferEinlesen(dpuffer,box,false,false,true,pe_Bad) then begin
        _era(dpuffer);
        ImportUUCPfromSpool:=true;
        end;
    end
  else
    CallFilter(true,dpuffer);
end;


function UUCPnetcall: Boolean;
var
    res  : integer;
    f       : file;
begin
  recs:='';
  netcall_connect:=true;
  fidologfile:=TempFile('');
  if not ExecutableExists(UUCICOBin) then begin
    NoUUCICO;
    res:=uu_parerr;
    end
  else begin
    if not comn[comnr].fossil then ReleaseC;
    {$IFNDEF Ver32 }
    res:=uucico(XFerDir+caller,ConnTicks,ende,      { --- UUCICO ---------- }
                   NC^.waittime,NC^.sendtime,NC^.rectime,fidologfile);
    {$ENDIF }
    if not comn[comnr].fossil then Activate;
    end;
  aufhaengen;
  // !!DropDtr(comnr);
  ReleaseC;
  if (res<>uu_nologin) and (res<>uu_parerr) then
    WriteUUnummer(uunum);
  UUCPnetcall:=(res=uu_ok);
  cursor(curoff);
  if (res=uu_ok) or (res=uu_recerr) then
  begin
    NC^.sendbuf:=spufsize;
    NC^.sendpack:=spacksize;
    NC^.abbruch:=(res<>uu_ok);
    moment;
    outmsgs:=0;
    ClearUnversandt(ppfile,box);
    if FileExists(ppfile) then
      _era(ppfile);
    if FileExists(eppfile) then
      _era(eppfile);
    if res=uu_ok then
      wrtiming('NETCALL '+boxpar^.boxname);
    if res=uu_recerr then begin    { doppeltes Senden verhindern }
      assign(f,XFerDir+caller);
      rewrite(f,1);                   { Inhalt des C-Files löschen }
      close(f);
      end;
    closebox;
    end
  else
    NC^.abbruch:=true;
  if ImportUUCPfromSpool(XFerDir) and (res=uu_recerr) then
    erase_mask(XFerDir+'*.');         { Doppeltes Einlesen verhindern }
  SendNetzanruf(once,false);
  SendFilereqReport;    { ... falls vorhanden }
  AppLog(fidologfile,UUCPlog);
  if FileExists(fidologfile) then _era(fidologfile);
  twin;
end;


procedure UUCPSysopTransfer;
var dummy : longint;
begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if not IsPath(SysopInp) then begin              { Verzeichnisse testen }
      trfehler(727,30);   { 'ungültiges Eingabeverzeichnis' }
      exit;
      end;
    if not IsPath(SysopOut) then begin
      trfehler(728,30);   { 'ungültiges Ausgabeverzeichnis' }
      exit;
      end;

    NC^.sendbuf:=_filesize(ppfile);
    if NC^.sendbuf>0 then begin               { -- Ausgabepaket -- }
      outmsgs:=testpuffer(ppfile,false,dummy);
      twin;
      cursor(curoff);
      ZtoRFC(false,ppfile,SysopOut);
      {window(1,1,screenwidth,screenlines);}
      WriteUUnummer(uunum);
      Moment;
      RemoveEPP;
      outmsgs:=0;
      ClearUnversandt(ppfile,box);
      closebox;
      _era(ppfile);
      if FileExists(eppfile) then _era(eppfile);
      end;

    if FileExists(SysopInp+WildCard) then                   { -- Eingangspaket -- }
      if ImportUUCPfromSpool(SysopInp) then
        erase_mask(BoxPar^.sysopinp+WildCard);
    Netcall_connect:=true;
    end;
end;


end.

{
  $Log$
  Revision 1.2  2001/02/01 21:20:27  ma
  - compiling!
  - only Fido: UUCP/POP3/... routines are temporarily commented out
  - untested

  Revision 1.1  2001/01/10 16:32:19  ma
  - todo: general cleanup

  --- moved to playground
  Revision 1.2  2001/01/06 18:23:42  ma
  - cleaned up
  - added debug logs to fidosysopcall
  - some error checks in fidosysopcall added
  - todo: move non-uucp routines out of this file

  Revision 1.1  2001/01/04 16:04:13  ma
  - renamed, was xp7u.inc
  - todo: split, simplify, merge with uucico and change to unit

}

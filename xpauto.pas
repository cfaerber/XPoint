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

{ Nachrichten-Autoversand; Autoexec }

{$I xpdefine.inc}

unit xpauto;

interface

uses
  sysutils,
  xpglobal;

type  AutoRec = record                     { AutoVersand-Nachricht }
                  datei   : string;
                  betreff : string;
                  typ     : char;                { 'B' / 'T'       }
                  empf    : string;      { Brett oder User }
                  box     : string;  { optional        }
                  wotage  : byte;                { Bit 0=Mo        }
                  tage    : longint;             { Bit 0=1.        }
                  monate  : smallword;           { Bit 0=Januar    }
                  datum1  : longint;
                  datum2  : longint;
                  flags   : xpWord;       { 1=aktiv, 2=loeschen, 4=Aenderung, 8=supersede }
                  lastdate: longint;
                  lastfd  : longint;             { Dateidatum }
                  lastmid : string;     { letzte verwendete mid }
                end;

procedure AutoRead(var ar:AutoRec);
procedure AutoWrite(var ar:AutoRec);
procedure AutoSend;
function  PostFile(var ar:AutoRec; sendbox:boolean):boolean;
function  AutoShow:string;      { fuer XP4D.INC }

procedure AutoExec(startbatch:boolean);
procedure AutoStop;


implementation

uses
  montage,typeform,fileio,datadef,database,resource,
  xp0,xp1,xp1o,xp3,xp3o,xpsendmessage,xp9bp,xpmaus,xpnt, debug, zftools;


procedure AutoRead(var ar:AutoRec);
begin
  with ar do begin
    datei := dbReadStr(auto,'dateiname');
    Betreff := dbReadStr(auto,'betreff');
    dbRead(auto,'typ',typ);
    empf:= dbReadStr(auto,'empfaenger');
    box := dbReadStr(auto,'pollbox');
    dbRead(auto,'wochentage',wotage);
    dbRead(auto,'tage',tage);
    dbRead(auto,'monate',monate);
    dbRead(auto,'datum1',datum1);
    dbRead(auto,'datum2',datum2);
    dbRead(auto,'flags',flags);
    dbRead(auto,'lastdate',lastdate);
    dbRead(auto,'lastfdate',lastfd);
    lastmid := dbReadStr(auto,'lastmsgid');
  end;                    
end;

procedure AutoWrite(var ar:AutoRec);
begin
  with ar do begin
    dbWriteStr(auto,'dateiname',datei);
    dbWriteStr(auto,'betreff',betreff);
    dbWrite(auto,'typ',typ);
    dbWriteStr(auto,'empfaenger',empf);
    dbWriteStr(auto,'pollbox',box);
    dbWrite(auto,'wochentage',wotage);
    dbWrite(auto,'tage',tage);
    dbWrite(auto,'monate',monate);
    dbWrite(auto,'datum1',datum1);
    dbWrite(auto,'datum2',datum2);
    dbWrite(auto,'flags',flags);
  end;
end;


{ naechstes Absendedatum fuer Automatik-Nachricht berechnen }
{ 0 -> kein zutreffendes Datum, Nachricht nicht absenden  }

function AutoNextdate(var ar:AutoRec):longint;
var mmask     : array[1..12] of boolean;
    tmask     : array[1..31] of boolean;
    wmask     : array[1..7] of boolean;
    i,_d      : longint;
    dat0,dat2 : fdate;
    dat       : fdate;
    ds        : string;

  function smd(d1,d2:fdate):boolean;
  begin
    smd:=longint(d1)<longint(d2);
  end;

  procedure fitmonth(var _dat:fdate);
  begin
    while not mmask[_dat.m] do
      with _dat do begin
        inc(m);
        if m>12 then begin
          m:=1;
          inc(j);
          end;
        t:=1;
        end;
  end;

  procedure nextd2;
  begin
    incd(dat2);
    fitmonth(dat2);
  end;

  function iid(dat:fdate):longint;
  begin
    with dat do
      iid:=ixdat(formi(j mod 100,2)+formi(m,2)+formi(t,2)+'0000');
  end;

  function amodi:boolean;
  var
    fn : string;
  begin
    fn:=ar.datei;
    adddir(fn,SendPath);
    if not FileExists(fn) then
      Result := false
    else
      amodi:=FileAge(fn)<>ar.lastfd;
  end;

begin
  with ar do
    if not odd(flags) or (monate=0) then
      AutoNextDate:=0
    else
    if (flags and 4<>0) and not amodi then
      AutoNextDate:=0
    else begin
      for i:=1 to 12 do mmask[i]:=monate and (1 shl (i-1)) <> 0;
      for i:=1 to 31 do tmask[i]:=tage and (1 shl (i-1)) <> 0;
      for i:=1 to 7 do  wmask[i]:=wotage and (1 shl (i-1)) <> 0;
      if lastdate=0 then ds:=zdate
      else ds:=longdat(lastdate);
      with dat0 do begin
        j:=ival(LeftStr(ds,2));
        if j<=70 then inc(j,2000)
        else inc(j,1900);
        m:=ival(copy(ds,3,2));
        t:=ival(copy(ds,5,2));
        end;
      if lastdate<>0 then
        incd(dat0);
      fitmonth(dat0);
      longint(dat):=0;
      if tage<>0 then begin
        dat2:=dat0;
        while not tmask[dat2.t] do nextd2;
        if (longint(dat)=0) or smd(dat2,dat) then
          dat:=dat2;
        end;
      if wotage<>0 then begin
        dat2:=dat0;
        while not wmask[ddow(dat2)] do nextd2;
        if (longint(dat)=0) or smd(dat2,dat) then
          dat:=dat2;
        end;
      _d:=iif(tage+wotage=0,0,iid(dat));
      if datum1<>0 then
        if (_d=0) or smdl(datum1,_d) then _d:=datum1;
      if datum2<>0 then
        if (_d=0) or smdl(datum2,_d) then _d:=datum2;
      AutoNextDate:=_d;
      end;
end;


{ In auto mu� korrekter Datensatz sein! }

function PostFile(var ar:AutoRec; sendbox:boolean):boolean;
var tmp  : boolean;
    t    : text;
    pm   : boolean;
    leer : string;
    dat  : longint;
    tt   : longint;
    b    : byte;
    muvs : boolean;
    sData: TSendUUData;
begin
  postfile:=false;
  with ar do
  begin
    if datei<>'' then
      adddir(datei,SendPath);
    if (datei<>'') and not FileExists(datei) then
      trfehler1(2201,fitpath(datei,42),30)    { 'AutoVersand - Datei "%s" fehlt!' }
    else if (datei<>'') and (_filesize(datei)=0) then
      trfehler(2202,30)   { 'AutoVersand - 0-Byte-Datei wurde nicht verschickt' }
    else begin
      tmp:=(datei='');
      if tmp then begin
        datei:=TempS(1000);
        assign(t,datei);
        rewrite(t);
        writeln(t);
        close(t);
      end;
      pm:= IsMailAddr(empf);
      if pm and (betreff='') then betreff:='<nope>';
      empf:=vert_long(empf);
      pm:= IsMailAddr(empf);
      if not pm then insert('A',empf,1);
      leer:='';
      sData := TSendUUData.Create;
      try
        if UpperCase(box)='*CRASH*' then begin
          box:='';
          sData.flCrash:=true;
          sData.flCrashAtOnce:=true;    { keine Rueckfrage 'sofort versenden' }
        end;
        sData.forcebox:=box;
        if not tmp then begin
          sData.sendfilename:=ExtractFileName(datei); {GetFileName(datei);}
          sData.sendfiledate:=ZCfiletime(datei);
        end;
        if sData.forcebox='' then dbGo(mbase,0);   { keine Antwort auf Brettmsg }
  //    sData.EditAttach:=false;
        muvs:=SaveUVS; SaveUVS:=false;
        sdata:= TSendUUData.Create;
        if (flags and 8<>0) then sData.Ersetzt := dbReadStr(auto,'lastmsgid');

        if PM then
          sData.EmpfList.AddNew.ZCAddress := Empf
        else
          sData.EmpfList.AddNew.XPAddress := Empf;

        sData.Subject := betreff;
        sData.flShow := true;
        if typ='B' then
          sData.AddFile(datei,tmp,'')
        else
          sData.AddText(datei,tmp);

        if sData.DoIt(GetRes2(610,120),false,false,sendbox) then
        begin
          b:=0;
          dbWriteN(mbase,mb_gelesen,b);
          dat:=ixdat(zdate);
          dbWrite(auto,'lastdate',dat);
          dbWriteStr(auto,'lastmsgid',sData.msgid);
          tt := FileAge(Datei);
          dbWrite(auto,'lastfdate', tt);
          if dat>=datum1 then begin
            datum1:=0;
            dbWrite(auto,'datum1',datum1);
            end;
          if dat>=datum2 then begin
            datum2:=0;
            dbWrite(auto,'datum2',datum2);
          end;
          if (flags and 2<>0) and (datum1=0) and (datum2=0) and (tage+wotage=0) then begin
            if ExtractFileExt(FileUpperCase(datei))=FileUpperCase('.msg') then
              SafeDeleteFile(datei);
            dbDelete(auto);
            aufbau:=true;
          end;
        end;
        SaveUVS:=muvs;
        if tmp then
          SafeDeleteFile(datei);
      finally
        sData.Free;
      end;
    end;
  end;
end;


procedure AutoSend;
var ar    : AutoRec;
    dat   : longint;
    r1,r2 : longint;
begin
  Debug.DebugLog('xpauto','AutoSend', dlTrace);
  dbOpen(auto,AutoFile,1);
  dbSetIndex(auto,0);
  dbGoTop(auto);
  while not dbEOF(auto) do begin
    AutoRead(ar);
    dat:=AutoNextdate(ar);
    if (dat<>0) and not smdl(ixDat(zdate),dat) and (length(ar.empf)>1) then begin
      r1:=dbRecno(auto);
      dbSkip(auto,1);
      r2:=dbRecno(auto);
      dbGo(auto,r1);
      PostFile(ar,false);
      dbGo(auto,r2);
      end
    else
      dbNext(auto);
    end;
  dbClose(auto);
end;


{ --- AutoExec ------------------------------------------------------ }

procedure AutoExec(startbatch:boolean);
const tfs = 20;
var sr    : tsearchrec;
    rc    : integer;
    first : boolean;
    ctlEbest,ctlErstDat : boolean;
    mgel  : boolean;       { Save fuer ParGelesen }
    fnstart: string;      { Name der Start.bat }

  function find(const ext:string):boolean;
  begin
    if first then
      rc:= findfirst(AutoxDir+'*'+FileUpperCase(ext),faAnyFile,sr)
    else
      rc:= findnext(sr);
    first:=(rc<>0);
    find:=not first;
    if first then findclose(sr);
  end;

  function NamePollbox:string;
  var p : byte;
      d : DB;
  begin
    p:=cpos('.',sr.name);
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiDatei,LeftStr(sr.name,p-1));
    if dbFound then
      NamePollbox:=dbReadStr(d,'boxname')
    else
      NamePollbox:='';
    dbClose(d);
  end;

  function MausImport:boolean;
  var box : string;
  begin
    MausImport:=false;
    if not FileExists(MaggiBin) then
      trfehler(102,tfs)    { 'MAGGI.EXE fehlt' }
    else begin
      box:=NamePollbox;
      if box='' then
        trfehler1(2204,sr.name,tfs)   { 'Kann %s nicht einlesen - ungueltige Pollbox' }
      else begin
        ReadBoxpar(nt_Netcall,box);
        shell(MaggiBin+' -sz -b'+box+' -h'+boxpar^.MagicBrett+' '+
              AutoxDir+sr.name+' MPUFFER',300,3);
        if errorlevel<>0 then
          trfehler1(2205,sr.name,tfs)   { '%s: Fehler bei Nachrichtenkonvertierung' }
        else begin
          MausLogFiles(0,false,box);
          MausLogFiles(1,false,box);
          MausLogFiles(2,false,box);
          MausImport:=PufferEinlesen('MPUFFER',box,ctlErstDat,false,ctlEbest,0);
          _era('MPUFFER');
          end;
        end;
      end;
  end;

  procedure FidoImport;
  var x,y: Integer;
  begin
    if not IsBox(DefFidoBox) then
      trfehler(2207,tfs)     { 'Keine gueltige Fido-Stammbox gewaehlt' }
    else begin
      ReadBoxpar(nt_Netcall,DefFidoBox);
      msgbox(70,10,GetRes2(30003,10),x,y);
      DoZFido(2, BoxPar^.MagicBrett, AutoxDir+'*.pkt', 'FPUFFER', '', '', 0, '', '', true, false, false, false, x, y);
      closebox;
      if errorlevel<>0 then
        trfehler(2208,tfs)   { 'Fehler bei Fido-Paketkonvertierung' }
      else begin
        if PufferEinlesen('FPUFFER',DefFidoBox,ctlErstDat,false,ctlEbest,pe_ForcePfadbox) then begin
          erase_mask(AutoXDir+'*.pkt');
          erase_mask(AutoXDir+'*.PKT');
        end;
        DeleteFile('FPUFFER');
      end;
    end;
  end;

  function SendMsg(delfile:boolean):boolean;
  var t1,t2 : text;
      p     : byte;
      empf  : string;
      betr  : string;
      box   : string;
      datei : string;
      hdr   : string;
      s     : string;
      err   : boolean;
      temp  : boolean;
      pm    : boolean;
      attach: boolean;   { Fido-FileAttach }
      nt    : eNetz;
      sData : TSendUUData;

    procedure axerr(nr:xpWord; txt:string);
    begin
      tfehler(getres2(2200,1)+getreps2(2200,nr,txt),tfs);   { 'AutoExec-Fehler: ' }
      freeres;
    end;


  begin
    sData := TSendUUData.Create;
    try
      SendMsg:=false;
      empf:=''; betr:='';
      box:=''; datei:='';
      attach := False;
      assign(t1,AutoxDir+sr.name);
      reset(t1);
      s:='*';
      while not eof(t1) and (s<>'') do begin
        readln(t1,s);
        p:=cpos(':',s);
        if p>0 then begin
          hdr:=LowerCase(LeftStr(s,p-1));
          if (hdr='empfaenger') or (hdr='empf�nger') or (hdr='to') then
            empf:=trim(mid(s,p+1)) else
          if (hdr='betreff') or (hdr='subject') then
            betr:=trim(mid(s,p+1)) else
          if hdr='server' then
            box:=trim(mid(s,p+1)) else
          if (hdr='datei') or (hdr='file') then
            datei:=FileUpperCase(trim(mid(s,p+1)));
        end;
      end;
      err:=true;
      if (box='') or not IsBox(box) then
        axerr(4,box)                  { 'ungueltige Serverbox: %s' }
      else begin
        nt:=ntBoxNetztyp(box);
        attach:= (datei<>'') and
                ((nt=nt_Fido) or
                 (nt=nt_UUCP) and (LeftStr(empf,16)='UUCP-Fileserver@'));
        if empf='' then axerr(2,'')     { 'Empfaenger fehlt' }
        else if betr='' then axerr(3,'')     { 'Betreff fehlt'   }
        else if datei<>'' then begin
          if not multipos(_MPMask,datei) then
            datei:=SendPath+datei;
          if not FileExists(datei) then
            axerr(5,UpperCase(datei))        { 'Datei "%s" fehlt' }
          else if attach and (not IsMailAddr(empf)) then
            axerr(6,'')    { 'File Attaches koennen nur als PM verschicht werden!' }
          else 
            err:=false;
        end else
          err:=false;
      end;
      if err then begin
        close(t1);
        exit;
      end;

      if attach then begin
        betr:=datei;
        datei:='';
    //    EditAttach:=false;    { ab hier kein EXIT mehr! }
      end;

      if datei='' then begin
        datei:=TempS(_filesize(AutoxDir+sr.name));
        temp:=true;
        assign(t2,datei); rewrite(t2);
        while not eof(t1) do begin
          repeat
            Readln(t1, s);
            Writeln(t2, s);
          until eoln(t1);
        end;
        close(t2);
      end else begin
        temp:=false;
        sData.SendFilename:=ExtractFilePath(datei); {getfilename(datei);}
        sData.SendFiledate:=zcfiletime(datei);
      end;
      close(t1);
      s:='';
      sData.forcebox:=box;
      empf:=vert_long(empf);
      p:=cpos('@',empf);
      pm:=(p>0);
      if pm then
        empf:=trim(LeftStr(empf,p-1))+'@'+trim(mid(empf,p+1))
      else if FirstChar(empf)<>'/' then empf:='/'+empf;
    //  EditAttach:=false;
    (*
      if DoSend(pm,datei,temp or delfile,false,iifs(pm,'','A')+empf,betr,
                false,attach or not temp,false,false,temp,nil,s,sendShow) then begin
    *)

      if attach or not temp then
        sData.AddFile(datei,temp or delfile,'')
      else
        sData.AddText(datei,temp or delfile);

      sData.EmpfList.AddNew.ZCAddress := Empf;

      sData.Subject := betr;
      sData.flShow := true;

      result := sData.DoIt('',false,false,false);
    finally
      sData.Free;
    end;
  end;

  function SendPuffer:boolean;
  var
    box: String;
  begin
    SendPuffer:=false;
    box:=NamePollbox;
    if not IsBox(box) then
      trfehler1(2209,box,tfs)    { 'IPS - ungueltige Box: %s' }
    else
      if PufferEinlesen(AutoxDir+sr.name,box,false,true,false,0) then begin
        AppPuffer(box,AutoXdir+sr.name);
        SendPuffer:=true;
        end
      else
        SendPuffer:=false;
  end;

  procedure SetCTL;
  begin
    if LeftStr(sr.name,6)='EBEST.'   then ctlEbest:=true else
    if LeftStr(sr.name,7)='EDATUM.'  then ctlErstDat:=true else
    if LeftStr(sr.name,8)='GELESEN.' then ParGelesen:=true;
  end;

begin
  Debug.DebugLog('XPAUTO','AutoExec() - test autoexec directory',DLDebug);
  if not isEmptyDir(AutoxDir) then begin
    first:=true;
    ctlEbest:=false; ctlErstDat:=false;
    mgel:=ParGelesen; ParGelesen:=false;
    while find('.ctl') do    { Control-Dateien }
      SetCTL;
    while find('.ctd') do begin
      SetCTL;
      DeleteFile(AutoXdir+sr.name);
      //delfile;
      end;
    while find('.zer') do     { Z-Puffer einlesen + loeschen }
      if PufferEinlesen(AutoxDir+sr.name,NamePollbox,ctlErstDat,false,ctlEbest,0) then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    while find('.zee') do     { Z-Puffer einlesen, EB's versenden + loeschen }
      if PufferEinlesen(AutoxDir+sr.name,NamePollbox,ctlErstDat,false,true,0) then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    while find('.out') do     { Maus-OUTFILE einlesen + loeschen }
      if MausImport then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    if FileExists(AutoxDir+'*.pkt') then    { Fido-Paket(e) einlesen + loeschen }
      FidoImport;

    while find('.ips') do     { Puffer versenden }
      if SendPuffer then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    while find('.msg') do     { Nachricht/Datei senden + loeschen }
      if SendMsg(false) then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    while find('.msd') do     { Datei senden + incl. Datei loeschen }
      if SendMsg(true) then
        DeleteFile(AutoXdir+sr.name);
        //delfile;
    while find(ExtBak) do     { BAK-files loeschen }
      DeleteFile(AutoXdir+sr.name);
      //delfile;

    while find('.bat') do     { Batchdateien ausfuehren }
      if (LeftStr(FileUpperCase(sr.name),5)<>FileUpperCase('start')) and
        (LeftStr(FileUpperCase(sr.name),4)<>FileUpperCase('stop')) then begin
        shell(AutoxDir+sr.name,600,1);
        DeleteFile(AutoXdir+sr.name);
        // delfile;
        end;
    if startbatch then begin
      fnstart:=AutoxDir+FileUpperCase('start' + extBatch);        { START.BAT }
      if FileExists(fnstart) then
        shell(fnstart,500,1);
      fnstart:=AutoxDir+FileUpperCase('start1' + extBatch);       { START1.BAT, loeschen }
      if FileExists(fnstart) then begin
        shell(fnstart,500,1);
        DeleteFile(fnstart);
        end;
      end;
    ParGelesen:=mgel;
    FindClose(sr);
  end;
end;

procedure AutoStop;
var
  fnstop: string;
begin
  Debug.DebugLog('xpauto','AutoStop', dlTrace);
  fnstop:= AutoxDir+FileUpperCase('stop' + extBatch);     { STOP.BAT }
  if FileExists(fnstop) then
    shell(fnstop,500,1);
  fnstop:= AutoxDir+FileUpperCase('stop1' + extBatch);    { STOP1.BAT, loeschen }
  if FileExists(fnstop) then 
  begin
    shell(fnstop,500,1);
    _era(fnstop);
  end;
end;


function AutoShow:string;      { fuer XP4D.INC }
var ar   : autorec;
    c    : string;
    ldat,
    sdat : string;

  procedure setfile(var s: string);
{$IFDEF UnixFS }
  begin
    s:= ExtractFilename(s);
{$ELSE }
  var dir  : string;
      name : string;
  begin
    dir:= ExtractFilePath(s);
    name:= ExtractFileName(s);
    if dir='' then s:=name
    else if (Length(dir) >= 2) and (dir[2]=':') then
      s:=LeftStr(dir,2)+name
    else s:=LeftStr(GetCurrentDir,2)+name;
{$ENDIF }
  end;

begin
  AutoRead(ar);
  with ar do begin
    if typ='T' then typ:=' ';
    c:=iifs(odd(flags),'+ ','  ');
    ldat:=LeftStr(fdat(longdat(lastdate)),5);
    if ldat='00.00' then ldat:='--.--';
    sdat:=LeftStr(fdat(longdat(AutoNextDate(ar))),5);
    if (sdat='00.00') or (empf='') then sdat:='--.--';
    setfile(datei);
    AutoShow:=c+forms(datei,15)+typ+' '+ldat+'  '+sdat+'  '+
              forms(empf,22)+' '+forms(betreff,24)+ dup(ScreenWidth-80, ' ');
    end;
end;

{
  $Log$
  Revision 1.68  2003/08/28 05:37:53  mk
  - create sData in PostFile

  Revision 1.67  2003/08/23 23:02:35  mk
  - removed hints and warnings

  Revision 1.66  2003/08/23 20:04:15  mk
  - always use FileAge to get filedate, possilbe solves autosend problems

  Revision 1.65  2003/05/11 11:12:19  mk
  - use IsMailAddr when possible

  Revision 1.64  2003/04/16 20:51:11  mk
  - fixed crash in SendMsg (initialized sData)
  - use AnsiString to copy text from file to message

  Revision 1.63  2003/01/07 00:56:46  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.62  2002/12/21 05:37:59  dodi
  - removed questionable references to Word type

  Revision 1.61  2002/12/16 01:05:13  dodi
  - fixed some hints and warnings

  Revision 1.60  2002/12/14 07:31:37  dodi
  - using new types

  Revision 1.59  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.58  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.57  2002/07/27 08:42:14  mk
  - fixed typo

  Revision 1.56  2002/07/26 10:11:04  mk
  - fixed dbWrite with AnsiStrings

  Revision 1.55  2002/07/26 07:59:28  mk
  - fixed Edit/AutoVersand for Screenwidth > 80 chars

  Revision 1.54  2002/07/25 20:43:55  ma
  - updated copyright notices

  Revision 1.53  2002/05/26 12:16:23  ma
  - replaced dbLog by standard log routines

  Revision 1.52  2002/05/20 07:47:57  mk
  - fixed backup extension: now ExtBak and EditorExtBak

  Revision 1.51  2002/05/15 22:02:26  mk
  - added debug log entry for AutoExec()

  Revision 1.50  2002/05/13 21:27:53  ma
  - fixed processing of AUTOEXEC dir

  Revision 1.49  2002/05/05 22:47:20  mk
  - use correct case for 'bak' extension

  Revision 1.48  2002/02/21 13:52:33  mk
  - removed 21 hints and 28 warnings

  Revision 1.47  2002/01/05 16:01:09  mk
  - changed TSendUUData from record to class

  Revision 1.46  2001/12/30 19:56:49  cl
  - Kylix 2 compile fixes

  Revision 1.45  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.44  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.43  2001/09/08 16:29:37  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.42  2001/09/08 14:34:33  cl
  - adaptions/fixes for MIME support

  Revision 1.41  2001/09/07 13:54:23  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.40  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.39  2001/08/11 23:06:36  mk
  - changed Pos() to cPos() when possible

  Revision 1.38  2001/07/23 16:05:22  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.37  2001/04/03 13:25:40  ma
  - cleaned up fido aka handling

  Revision 1.36  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.35  2001/02/28 14:25:47  mk
  - removed some tainted comments

  Revision 1.34  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.33  2000/12/28 13:29:57  hd
  - Fix: packets now sorted in after netcall
  - Adjusted: Open window once during sorting in

  Revision 1.32  2000/12/25 20:31:18  mk
  - zfido is now completly integrated

  Revision 1.31  2000/12/12 14:24:20  mk
  - fixed missing FindClose

  Revision 1.30  2000/12/03 12:38:26  mk
  - Header-Record is no an Object

  Revision 1.29  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.28  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.27  2000/11/14 15:51:35  mk
  - replaced Exist() with FileExists()

  Revision 1.26  2000/11/05 09:34:03  mk
  - Ansistring-Fix

  Revision 1.25  2000/10/19 20:52:23  mk
  - removed Unit dosx.pas

  Revision 1.24  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.23  2000/10/10 13:58:58  mk
  RB:- Ersetzt-Nachrichten in Autoversand

  Revision 1.22  2000/09/30 16:33:13  mk
  - LFN-Bufix

  Revision 1.21  2000/07/22 14:05:28  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.20  2000/07/21 21:17:48  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.19  2000/07/21 20:56:30  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.18  2000/07/05 12:47:28  hd
  - AnsiString

  Revision 1.17  2000/07/04 21:23:07  mk
  - erste AnsiString-Anpassungen

  Revision 1.16  2000/07/04 12:04:29  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.15  2000/07/03 13:31:43  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.14  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.13  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.12  2000/05/22 17:06:04  hd
  - Programmnamen durch Konstanten aus xp0 ersetzt
  - Fix: Fehlende und falsche FindClose
  - Batch-Namen angepasst

  Revision 1.11  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.10  2000/05/09 13:14:06  hd
  - UnixFS: getdrive entfernt

  Revision 1.9  2000/04/30 20:24:01  mk
  - FindClose an falscher Stelle

  Revision 1.8  2000/04/18 11:23:51  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.7  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.6  2000/04/13 12:48:39  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
end.


{  $Id$

   OpenXP UUCP netcall class
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)

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

{$i xpdefine.inc}

{ OpenXP UUCP netcall class }
unit ncuucp;

interface

uses ncmodem,timer,fidoglob,xpglobal,classes;

type
  TUUCPNetcall = class(TModemNetcall)

  protected
    function InitHandshake(var waittime:integer):char;
  
  public
    function PerformNetcall: Integer;

    UUprotos	  : string;
    
    MaxWinSize    : byte;
    MaxPacketSize : word;
    VarPacketSize : boolean;
    ForcePktSize  : boolean;

    SizeNego      : boolean;
    
    FilePath      : string;
    commandfile   : string;
  end;

implementation

uses zmodem, ipcclass, resource, sysutils, typeform, debug, montage, crc,
xpdiff, objcom, fileio, inout, keys;

type 

(*
  planned class hierarchy:

  TUUCProtocol
   +-- TUUCProtocolSimple
   |     +-- TUUCProtocolG 	(gGv protocols)
   |     +-- TUUCProtocolE      (e protocol)
   |     \-- TUUCProtocolFZ     (fz protocols)
   \-- TUUCProtocolI (not yet implemented)

  TUUCPNetcall will connect to the UUCP host, do the initial handshake
  (protocol selection) and create a UUCProtocol object, which it will then
  transfer control to.  

  TUUCProtocolSimple is a common base class for the gGfez protocols, which have
  a very similar structure (i.e. send files, become slave, receive). 
  More advanced protocols (such as UUCP-i) would be derieved directly from
  TUUCProtocol and only implement RunProtocol (besides con-/destructor).  
*)	 

  TUUCProtocol = class
    protected
      Netcall: 	  TUUCPNetcall; 
    public
      constructor InitProtocol(caller: tuucpnetcall);
      destructor  ExitProtocol; virtual;
      function    RunProtocol: integer; virtual; abstract;
  end;

  TUUCProtocolSimple = class(TUUCProtocol)
    public
      constructor InitProtocol(caller: tuucpnetcall);
      destructor  ExitProtocol; virtual; override;
      function    RunProtocol: integer; virtual; override;

    protected
    (* abstract protocol implementation functions *)
      function SendCommand(s:string):boolean; virtual; abstract;
      function GetCommand:string; virtual; abstract;
      function SendFile(fn:string; offset:longint):shortint; virtual; abstract;
      function RecFile(fn:string):Shortint; virtual; abstract;

    (* common protocol functions *)
      function RepeatGetcommand(c:char):string; 
      function RepeatSendFile(fn:string; offset:longint):boolean;
      function RepeatRecFile(fn:string; size:longint):boolean;

    (* helper functions *)
      procedure WriteTransfer;
      procedure ResetTransdat(blksize:word);
      procedure StartFile(send:boolean; fn:string; size:longint);
      procedure ShowFtype(var data; len:word);
      procedure TestBRK;

    (* high level protocol functions *)
      function SendFiles(CommandFile:string; var sendtime,rectime:longint):boolean;
      function RecFiles(var rectime:longint):boolean;
      function GetSlave(var ok:boolean):boolean;

    (* global vars *)
      transdata : record
        connstart   : longint;      { Ticks: CONNECT }
        files       : longint;
        filesize    : longint;      { aktuelle Datei }
        filestart   : longint;
        transferred : longint;      { davon uebertrg. }
        blocksize   : word;
        errors      : longint;
        total       : longint;
        totalfrom   : longint;      { Ticks: Sende/Empfangsstart }
        sending     : boolean;
      end;
      
      starttime     : longint;
      onlinetime    : longint;
      resopen       : boolean;
      dlogopen      : boolean;
      ulogopen      : boolean;
      break         : boolean;      { <Esc> gedrueckt }
      filetrans     : boolean;      { fuer rerrmsg }
      ShowTime      : boolean;
      FileStarted   : boolean;

      FileRetries: shortint;
  end;
  
const uu_ok      = 0;       { Ergebniscodes von ucico }
      (* uu_parerr  = 1; *)
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;
      uu_xfererr = 7;  { send or receive error }
      uu_unknown = 15; { unknown error }

      fileOK        =   0;    { Datei ok          }
      fileRepeat    =   1;    { Datei wiederholen }
      fileError     =   2;    { Datei - Abbruch   }

type 
      TUUCProtocolG = class(TUUCProtocolSimple)

      end;

{ $I ncuucp-g.inc}
{ $I ncuucp-e.inc}
{ $I ncuucp-fz.inc}

{ --- TUUCProtocol base class -------------------------------------- }

constructor TUUCProtocol.InitProtocol(caller: tuucpnetcall); begin Netcall:=caller; end;
destructor  TUUCProtocol.ExitProtocol; begin end;

{ --- UUCP-Verteiler ------------------------------------------------ }

constructor TUUCProtocolSimple.InitProtocol(caller: tuucpnetcall); 
begin 
  inherited InitProtocol(caller);

  with transdata do begin
    connstart := 0;
    files := 0;   
    filesize := 0;
    filestart := 0;
    transferred := 0;
    blocksize := 0;
    errors := 0;
    total := 0; 
    totalfrom := 0;
    sending := false;
  end;

  starttime     := 0;
  onlinetime    := 0;
  resopen       := false;
  dlogopen      := false;
  ulogopen      := false;
  break         := false;      { <Esc> gedrueckt }
  filetrans     := false;      { fuer rerrmsg }
  ShowTime      := true;
  FileStarted   := false;
  
  FileRetries := 0;
end;

destructor TUUCProtocolSimple.ExitProtocol; 
begin 
end;

function TUUCProtocolSimple.RepeatGetcommand(c:char):string;
var n : integer;
    s : string;
begin
  n:=10;              { 10 x 1 Min. Timeout }
  repeat
    s:=GetCommand;
    if (s<>'') and (LeftStr(s,1)<>c) then
      Netcall.Log(lcError,'unexpected command: '+s);
    dec(n);
  until (LeftStr(s,1)=c) or (n=0) or (not Netcall.CommObj.Carrier);
  RepeatGetcommand:=s;
end;

function TUUCProtocolSimple.RepeatSendFile(fn:string; offset:longint):boolean;
var sfresult : shortint;
begin
  repeat
    startfile(true,extractfilename(fn),_filesize(fn));
    sfresult:=SendFile(fn,offset);
    if sfresult=fileRepeat then
      Netcall.log(lcStop,'error - resending file');
  until (sfresult=fileOK) or (sfresult=fileError);
  if sfresult<>fileOK then
    Netcall.log(lcStop,'error sending file');
  RepeatSendFile:=(sfresult=fileOK);
end;

function TUUCProtocolSimple.RepeatRecFile(fn:string; size:longint):boolean;    { true = ok }
var rfresult : shortint;
    FileRetries: integer;
begin
  FileRetries:=0;
  repeat
    inc(FileRetries);
    startfile(false,extractfilename(fn),size);
    rfresult:=RecFile(fn);
    if rfresult=fileRepeat then
      Netcall.log(lcStop,'error - repeating file');
  until (rfresult=fileOK) or (rfresult=fileError);
  if rfresult<>fileOK then
    Netcall.log(lcStop,'error receiving file');
  RepeatRecFile:=(rfresult=fileOK);
end;

procedure TUUCProtocolSimple.StartFile(send:boolean; fn:string; size:longint);
begin
  inc(transdata.files);
// if ParDebug then begin
//    if send then 
   //  Netcall.WriteIPC('Sending '+fn);
//    wrdebug('sending ')
//    else 
   //   Netcall.WriteIPC('Receiving '+fn);
//    wrdebug('receiving ');
//  wrdebug('file: '+fn+' ');
//  end
//  else begin
//    attrtxt(col.colmailer);
//    clwin(mx+1,mx+lwdt-1,my+4,my+11);   { Dateifenster neu aufbauen }
//    with UWin do begin
//      if fn<>'' then
//        wrt(mx+2,my+4,iifs(send,senden,empfangen)); { 'Senden' / 'Empfangen' }
//      wrt(mx+27,my+4,dateien);                    { 'Dateien' }
//      wrt(mx+2,my+7,uebertragen);                 { 'uebertragen' }
//      wrt(mx+2,my+8,gesamt);                      { 'gesamt' }
//      wrt(mx+2,my+6,dateigroesse);                { 'Dateigroesse' }
//      wrt(mx+2,my+10,restzeit);                   { 'Restzeit' }
//      wrt(mx+27,my+6,blockgroesse);               { 'Blockgroesse' }
//      wrt(mx+27,my+7,durchsatz);                  { 'Durchsatz          cps' }
//      wrt(mx+27,my+8,dgesamt);                    { 'gesamt             cps' }
//      wrt(mx+27,my+10,fehler);                    { 'Fehler' }
//      end;
//    wrt(mx+2,my+12,sp(lwdt-2));
//    wrt(mx+2,my+hgh,sp(lwdt-2));
//    attrtxt(col.colmailerhi);
//    wrt(mx+13,my+4,fn);
//    wrt(mx+39,my+4,strsn(transdata.files,5));
//    end;
//  with transdata do begin
//    sending:=send;
//    filesize:=size; transferred:=0; errors:=0;
//    filestart:=ticker;
//    WriteTransfer;
//    end;
  FileStarted:=true;
end;

procedure tuucprotocolsimple.ResetTransdat(blksize:word);
begin
  transdata.total:=0;
  transdata.totalfrom:=ticker;
  transdata.files:=0;
  transdata.blocksize:=blksize;
end;

procedure TUUCProtocolSimple.ShowFtype(var data; len:word);
var s    : string;
    user : string;
    p,p2 : byte;
begin
  setlength(s,min(255,len));
  move(data,s[1],length(s));
  p:=cpos(#10,s);
  if p>0 then TruncStr(s,p-1);
  if LeftStr(s,3)='#! ' then
    if copy(s,4,5)='rnews' then s:=getres2(2300,60) else      { 'ungepacktes Newspaket' }
    if copy(s,4,8)='cunbatch' then s:=getres2(2300,61) else   { 'gepacktes Newspaket (compress)' }
    if copy(s,4,8)='funbatch' then s:=getres2(2300,62) else   { 'gepacktes Newspaket (freeze)' }
    if copy(s,4,8)='gunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    if copy(s,4,8)='zunbatch' then s:=getres2(2300,63) else   { 'gepacktes Newspaket (gzip)' }
    if copy(s,4,8)='bunbatch' then s:=getres2(2300,69) else   { 'gepacktes Newspaket (bzip2)' }
    s:=''
  else
    if LeftStr(s,5)='HELO '  then s:=getres2(2300,64) else   { 'ungepacktes Mailpaket' }
    if LeftStr(s,2)=#$1f#$9d then s:=getres2(2300,65) else   { 'gepackte Datei (compress)' }
    if LeftStr(s,2)=#$1f#$9f then s:=getres2(2300,66) else   { 'gepackte Datei (freeze) }
    if LeftStr(s,2)=#$1f#$8b then s:=getres2(2300,67) else   { 'gepackte Datei (gzip) }
    if LeftStr(s,2)=#$42#$5a then s:=getres2(2300,70) else   { 'gepackte Datei (bzip2) }
    begin
      LoString(s);
      if (LeftStr(s,5)='>from') or (LeftStr(s,4)='from') then begin
        p:=pos('remote from',s);
        if p>0 then begin
          delete(s,1,blankpos(s));
          user:=LeftStr(s,blankpos(s)-1);
          p2:=length(user);
          while (p2>0) and (user[p2]<>'!') do dec(p2);
          if p2>0 then begin
            s:=getres2(2300,68)+mid(user,p2+1)+'@';
            user:=LeftStr(user,p2-1);
            p2:=length(user);
            while (p2>0) and (user[p2]<>'!') do dec(p2);
            s:=s+mid(user,p2+1);
            end
          else begin
            p:=pos('remote from',s);
            s:=trim(mid(s,p+11));
            p:=blankpos(s);
            if p<>0 then TruncStr(s,p-1);
            s:=getres2(2300,68)+user+'@'+s;
            end;
          end
        else s:='Mail';
        end
      else s:={'Mail'} '';  { UUCP-Filerequest! }
      end;
(* if s<>'' then rmsg(s); *)
end;

procedure TUUCProtocolSimple.WriteTransfer;
var sec,cps : longint;
begin
(* if ParDebug then exit; *)
//  attrtxt(col.colmailerhi);
//  with transdata do begin
//    if filesize>0 then begin
//      gotoxy(mx+13,my+6); write(filesize:8);
//      end;
//    gotoxy(mx+13,my+7); write(transferred:8);
//    gotoxy(mx+13,my+8); write(total:8);
//    gotoxy(mx+39,my+6); write(blocksize:5);
//    gotoxy(mx+39,my+10); write(errors:5);
//    sec:=secondsfrom(filestart);
//    cps:=0;
//    if sec>0 then begin
//      cps:=transferred div sec;
//      gotoxy(mx+39,my+7);
//      write(cps:5);
//      if (filesize>0) and (cps>20) then begin
//        gotoxy(mx+13,my+10);
//        write(timeform((filesize-transferred)div cps));
//        end;
//      end;
//    sec:=secondsfrom(totalfrom);
//    if sec>0 then begin
//      if files>1 then
//        cps:=total div sec;
//      if cps>20 then begin
//        gotoxy(mx+39,my+8);
//        write(cps:5);
//        end;
//      end;
//    if filesize>0 then
//      wrt(mx+2,my+12,
//          dup(min(lwdt-3,system.round((lwdt-3)*(transferred/filesize))),'±'));
//    end;
//  WriteOnline;
  testbrk;
end;

procedure TUUCProtocolSimple.testbrk;
begin
  if keypressed and (readkey=#27) then begin
    break:=true;
(* rmsg(getres2(2300,51)); *)
    Netcall.Log(lcExit,'User break.');
    Sleeptime(500);
    end;
end;

{ --- TUUCPNetcall ------------------------------------------------------ }

(*
    UUprotos	  : string = "gG";
    
    MaxWinSize    : byte = 7;
    MaxPacketSize : word = 64;
    VarPacketSize : boolean = false;
    ForcePktSize  : boolean = false;

    SizeNego      : boolean = false;
    
    FilePath      : string = 'FILES\';
    commandfile   : string = '';
*)
    
function TUUCPNetcall.PerformNetcall: Integer;
var pprot : TUUCProtocol;
begin  
  result:=el_noconn; if not Connect then exit;
  DebugBadge:='ncuucp';

  case InitHandshake(5000) of
    'g','G','v': pprot := TUUCProtocolG.InitProtocol(self);
(*  'e':         pprot := TUUCProtocolE.InitProtocol(self);
    'f':         pprot := TUUCProtocolFZ.InitProtocol(self,true);
    'z':	 pprot := TUUCProtocolFZ.InitProtocol(self,false); *)
  else exit end;

  (* if not pprot.ok then
    result := el_nologin;
  else *)
    result := pprot.RunProtocol;

  pprot.ExitProtocol;
end;

function TUUCPNetcall.InitHandshake(var waittime:integer):char;
var n,i : integer;                            { --- uucp - Init-Handshake }
    s   : string;
    ti  : longint;
    proto: char;

  function GetUUStr:string;        { uucp-String empfangen }
  var recs: string;
  begin
    repeat
      if CommObj.CharAvail then s:=s+CommObj.GetChar;
    until (* timeout(true) or break or *)
          ((recs<>'') and (recs[length(recs)] in [#0,#4,#10,#13]));

    if LeftStr(recs,1)=^P then delete(recs,1,1);
    if recs='' then GetUUStr:='' else GetUUStr:=LeftStr(recs,length(recs)-1);
  end;

begin

  InitHandshake:=#0;
  mdelay(500);

  CommObj.PurgeInBuffer;
  CommObj.SendString( ^P'S'+pointname+iifs(SizeN,' -N','')+#0);

  for n := 1 to 5 do begin
    s:=GetUUStr;
  (* if timeout(true) then break; *)
  (* if break then exit; *)
    if s[1]='R' then break; 
  end;

  if LeftStr(s,3)<>'ROK' then begin
    if s='RLOGIN' then s:=s+' - wrong login name';
    if nocarrier then
      Log(lcError,'connection terminated by remote site')
    else
      Log(lcStop,'got "'+s+'" - aborting');
    exit;
    end;
  Log(lcInfo,'UUCP connection established');

  SizeN:=(s='ROKN');         { size negotiation }
  if SizeN then begin
    rmsg(getres2(2300,38));  { 'Using size negotiation' }
    log(lcInfo,'using size negotiation');
    end;

  for n := 1 to 5 do begin
    s:=GetUUStr; dec(n);
  (* if timeout(true) then break; *)
  (* if break then exit; *)
    if s[1]='P' then break;
  end
    
  if LeftStr(s,1)<>'P' then begin
    Log(lcStop,'got '+s+' - aborting');
    exit;
    end;
    
  delete(s,1,1);
  WrLog('~','remote protocols: '+s);
  if not multipos(uuprotos,s) then begin
    CommObj.SendString(^P'UN'#0);
    rmsg(getreps2(2300,37,s));    { 'no common protocol (remote supports %s)' }
    Log(lcStop,'no common protocol');
    result:=#0;    
    exit;
    end;
  for i:=length(uuprotos) downto 1 do     { bestes Protokoll ermitteln }
    if cpos(uuprotos[i],s)>0 then proto:=uuprotos[i];
  CommObj.SendString(^P'U'+proto+#0);
  Log(lcInfo,'selected protocol'+sp(cpos(proto,s))+'^');

  result:=proto;
end;


procedure FinalHandshake;           { --- uucp - Handshake vor Hangup }
var b : byte;
begin
  if not NoCarrier then begin
    sendstr(^P'OOOOOO');
    mdelay(800);
    if ParDebug then
      while receive(comnr,b) do
        wrdebug(chr(b));
    flushinput(comnr);
    end;
  {$ifdef debug}
    close(debugfile);
  {$endif}
  {$ifdef sim}
    close(siminput);
  {$endif}
end;


{----- TUUCProtocolSimple: Mails/News/Dateien senden, Dateien anfordern ---------------- }

function RunProtocol(Caller:^TUUCPNetcall: integer; 
var fuucico: integer
begin
  if not SendFiles(CommandFile,sendtime,rectime) then
    fuucico:= uu_senderr
  else if not RecFiles(rectime) then
    fuucico:= uu_recerr
  else
    fuucico:=uu_ok;

  RunProtocol:=fuucico;
end


function TUUCProtocolSimple.SendFiles(CommandFile:string; var sendtime,rectime:longint):boolean;
var t   : ^text;
    s   : string;
    s2  : string;
    sf  : string;
    o   : longint;
    fn  : string;
    ti  : longint;
    p   : byte;
    fs  : longint;
    secs: longint;
    size: longint;

label next,ende;

  procedure addtime(var t:longint);
  begin
    inc(t,secondsfrom(ti));
  end;

  procedure GetRequestFilesize;   { RY Mode Size }
  var p : byte;
  begin
    p:=blankpos(s2);
    if p>0 then s2:=trim(mid(s2,p)) else s:='';    { RY entfernen }
    p:=blankpos(s2);
    if p>0 then s2:=trim(mid(s2,p)) else s:='';    { Mode entfernen }
    p:=blankpos(s2);
    if p=0 then p:=length(s2)+1;
    size:=Cval(LeftStr(s2,p-1));
  end;

begin
  WrdLn;
  SendFiles:=false;
  new(t);
  assign(t^,CommandFile);
  reset(t^);
  ResetTransdat(gsBufSize);
  while not eof(t^) and not NoCarrier do begin
    if NoCarrier or break then goto ende;
    readln(t^,s);

    if LeftStr(s,2)='S ' then begin       { ----- Datei senden }
      ti:=ticker;
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p>0 then begin
        fn:=LeftStr(sf,p-1);
        sf:=mid(sf,p+1);
        sf:=LeftStr(sf,blankpos(sf)-1);
        end
      else
        fn:=sf;
      if multipos(':/',fn) then begin     { File Attach }
        for p:=1 to length(fn) do
          if fn[p]='/' then fn[p]:='\';
        end
      else
        fn:=XFerDir+fn[length(fn)-4]+'-'+RightStr(fn,4)+'.OUT';
      if fileexists(fn) then begin
        if not SizeN and (pos('""',s)>0) then
          s:=trim(LeftStr(s,pos('""',s)-1));
        netcall.Log('+','sending '+fn+' as '+sf);
        if not SendCommand(s) then begin
          addtime(sendtime); goto ende; end;
        s2:=RepeatGetcommand('S');   { SY / SN }
        if s2='' then begin
          addtime(sendtime); goto ende; end;
        if LeftStr(s2,2)='SY' then begin
          s2:=trim(mid(s2,4));
          p:=blankpos(s2);
          if p>0 then s2:=LeftStr(s2,p-1);
          o:=Cval(s2);
          if not SendFile(fn,o) then begin
            addtime(sendtime); goto ende;
            end;
          fs:=_filesize(fn);
          secs:=max(1,secondsfrom(ti));
          netcall.Log('*','sent '+strs(fs)+' bytes, '+strs(fs div secs)+' cps, '+
                    strs(transdata.errors)+' errors');
          s2:=RepeatGetcommand('C');   { CY / CN }
          if s2='' then begin
            addtime(sendtime); goto ende; end;
          if LeftStr(s2,2)='CN' then
            netcall.Log(lcStop,'remote error: could not move file');
          end
        else if LeftStr(s2,2)='SN' then begin
          case s2[3] of
            '2' : begin
                    rmsg(getres2(2300,42));   { 'Fehler 2' }
                    netcall.log(lcStop,'remote refuses file');
                  end;
            '4' : begin
                    rmsg(getres2(2300,44));   { 'Fehler 4 - Temporaerdatei kann nicht erzeugt werden' }
                    netcall.log(lcStop,'remote can''t create temp file');
                  end;
            '6','7' : begin
                        rmsg(getres2(2300,46));  { 'Fehler 6 - kein Platz fuer Datei' }
                        netcall.log(lcStop,'remote disk full');
                      end;
            '8' : begin
                    rmsg(getres2(2300,48));   { 'Fehler 8 - Datei schon vorhanden' }
                    netcall.log(lcStop,'file does already exist');
                  end;
          else    netcall.log(lcStop,'remote refuses file');
          end;
          end;
        end
      else
        netcall.log(lcStop,'outgoing file not found: '+fn);
      addtime(sendtime);
      end else

    if LeftStr(s,2)='R ' then begin       { ----- Datei anfordern }
      sf:=trim(mid(s,3));
      p:=blankpos(sf);
      if p=0 then goto next;
      fn:=trim(mid(sf,p+1));
      sf:=LeftStr(sf,p-1);
      p:=blankpos(fn);
      if p=0 then goto next;
      fn:=LeftStr(fn,p-1);
      if not validfilename(FilePath+fn) then begin
        netcall.log(lcStop,'invalid request destination file: '+fn);
        goto next;
        end;
      while fileexists(FilePath+fn) do begin      { Neuen Dateinamen erzeugen }
        p:=cpos('.',fn);
        if p=0 then
          fn:=fn+'.001'
        else
          if RightStr(fn,1)='9' then
            fn:=LeftStr(fn,p)+formi(min(999,ival(mid(fn,p+1)))+1,3)
          else
            fn:=LeftStr(fn,length(fn)-1)+strs(ival(RightStr(fn,1))+1);
        end;
      ti:=ticker;
      if SizeN then
        s:=s+' '+strs(diskfree(ord(FilePath[1])-64) div 3);
      netcall.log('+','requesting file: '+sf);
      if not SendCommand(s) then begin
        addtime(rectime); exit; end;
      s2:=GetCommand;
      if LeftStr(s2,2)='RY' then begin
        netcall.log(' ','request accepted');
        GetRequestFilesize;
        if RecFile(FilePath+fn,size) then begin
          fs:=_filesize(filepath+fn);
          secs:=max(1,secondsfrom(ti));
          netcall.log('*','received '+fn+', '+strs(fs)+' bytes, '+strs(fs div secs)+
                    ' cps, '+strs(transdata.errors)+' errors');
          if SendCommand('CY') then;
          end;
        end
      else
        if LeftStr(s2,3)='RN6' then
          netcall.log(lcStop,'file too large')
        else if LeftStr(s2,2)='RN' then
          netcall.log(lcStop,'request refused or file not found')
        else
          netcall.log(lcStop,'unknown error');
      addtime(rectime);
      end;

  next:
    end;
  SendFiles:=true;
ende:
  close(t^);
  dispose(t);
end;


function TUUCProtocolSimple.GetSlave(var ok:boolean):boolean;
var s : string[20];
    n : byte;
begin
  GetSlave:=false; ok:=false;
  if not SendCommand('H') then exit;   { uucp-Befehl "H" }
  n:=10;
  repeat
    dec(n);
    if n=0 then exit;
    s:=LeftStr(GetCommand,2);
  until (s='HN') or (s='HY') or NoCarrier or break;
  if not NoCarrier and not break then begin
    ok:=true;
    if s='HN' then begin            { HN -> Daten empfangen; HY -> Ende }
      GetSlave:=true;
      netcall.log('+','HN: remote has data for you');
      end
    else begin
      netcall.log('+','HY: no files to receive');
      if SendCommand('HY') then;
      end;
    end;
end;


function U2DOSfile(s:string):string;
var i : integer;
    b : byte;
begin
  s:=s[1]+'-'+RightStr(s,5);
  b:=0;
  for i:=0 to 3 do            { Schreibweise in einem Byte codieren }
    if (s[i+4]>='A') and (s[i+4]<='Z') then
      inc(b,1 shl i);
  U2DOSfile:=s+hex(b,1);
end;

{$I xpfiles.inc}

function TUUCProtocolSimple.RecFiles(var rectime:longint):boolean;
var ok    : boolean;
    s     : string;
    fn    : string;
    c     : char;
    p     : byte;
    ti,tf : longint;
    size  : longint;
    fs    : longint;
    secs  : longint;
    n     : integer;
    source: string;

label ende;

  procedure getfilesize;
  var ss   : string;
      pp,i : byte;
  begin
    ss:=s;
    for i:=1 to 6 do begin
      pp:=blankpos(ss);
      ss:=trim(mid(ss,pp+1));
      end;
    pp:=blankpos(ss);
    if pp>0 then ss:=LeftStr(ss,pp-1);
    size:=Cval(ss);                  { size negotiation - Dateigroesse }
  end;

begin
  ti:=ticker;
  RecFiles:=false;
  if not GetSlave(ok) then begin
    RecFiles:=ok;
    exit;
    end;
  c:=' ';
  WrdLn;
  ResetTransdat(grBufSize);
  n:=5;
  repeat                   { Slave-Schleife fuer eingehende Befehle }
    s:=GetCommand;
    if s='' then
      if n=0 then goto ende
      else dec(n)
    else n:=5;
    if s<>'' then begin
      c:=s[1];
      case c of
        'R' : begin
                logerror('host refuses file request');
                if SendCommand('RN2') then;   { Request-Anforderung }
              end;
        'X' : begin
                logerror('host refuses program execution');
                if SendCommand('XN') then;    { Execute-Anforderung }
              end;
        'S' : begin                  { remote uucp-Sendeanforderung }
                size:=0;
                s:=trim(mid(s,2));
                p:=blankpos(s);
                if p>0 then begin
                  source:=LeftStr(s,p-1);   { Quelldatei auf anderem Rechner }
                  s:=trim(mid(s,p));
                  p:=blankpos(s);
                  if p>0 then begin
                    if SizeN then getFilesize;
                    s:=LeftStr(s,p-1);
                    end;
                  end;
                if s='' then s:=source;
                if s='' then begin                     { S ohne Dateiname }
                  logerror('got S command without file name');
                  if not SendCommand('SN2') then goto ende;
                  end
                else if (size>0) and (maxfsize>0) and (size>1024*maxfsize)
                then begin
                  logerror('file too large ('+strs(size div 1024)+' KB)');
                  if not SendCommand('SN7') then goto ende;
                  end
                else begin
                  tf:=ticker;
                  if not SendCommand('SY 0x0') then goto ende;
                  if (LeftStr(s,2)='D.') or (LeftStr(s,2)='X.') then begin
                    fn:=XFerDir+U2DOSfile(s);
                    netcall.log('+','receiving '+s+' as '+uppercase(fn));
                    end
                  else begin
                    s:=Unix2DOSfile(s,FilePath);
                    if s='' then s:=Unix2DOSfile(source,FilePath);
                    if s='' then s:='unnamed';
                    fn:=FilePath+s;
                    netcall.log('S','receiving '+s+' as '+uppercase(fn));
                    end;
                  if not RecFile(fn,size) then
                    goto ende;
                  fs:=_filesize(fn);
                  secs:=max(1,secondsfrom(tf));
                  netcall.log('*','received '+strs(fs)+' bytes, '+strs(fs div secs)+
                            ' cps, '+strs(transdata.errors)+' errors');
                  if SendCommand('CY') then;
                  end;
              end;
      else    if c<>'H' then begin
                netcall.log(lcStop,'unknown UUCP command: '+s);
                if SendCommand(c+'N') then;    { ungueltiger Befehl }
              end;
      end;
    end;
  until (c='H') or NoCarrier or break;           { Hangup }
  if c='H' then begin
    RecFiles:=true;
    if SendCommand('HY') then
      s:=GetCommand;         { = HY }
    end;
ende:
  inc(rectime,secondsfrom(ti));
end;

// function fuucico(start:longint; var ende:boolean;
//                 var waittime:integer; var sendtime,rectime:longint):integer;
// begin
//   SizeN:=sizenego;
//   fillchar(transdata,sizeof(transdata),0);
//   transdata.connstart:=start;
//   InitInterface;
//   if ParDebug then begin             { UUDEBUG.LOG: ucico-Debug-Logfile }
//     new(deblog);
//     assign(deblog^,DebugLog);
//     rewrite(deblog^);
//     dlogopen:=true;
//     end;
//   if uulogfile<>'' then begin        { Temp: XPUUCP.LOG }
//     new(uulog);
//     assign(uulog^,uulogfile);
//     rewrite(uulog^);
//     writeln(uulog^,'----------  ',date,' -> ',boxname);
//     ulogopen:=true;
//     end
//   else
//     uulog:=nil;
//   (* if ParDebug then mcur:=curon
//   else mcur:=curoff; *)
//   ShowWindow;
//   recs:=''; _ende:=false;
//   sendtime:=0; rectime:=0; waittime:=0;
// 
//   if InitHandshake(waittime) then begin     { enthaelt InitProtocol }
//     if SendFiles(CommandFile,sendtime,rectime) then
//       if RecFiles(rectime) then begin
//         fuucico:=uu_ok; _ende:=true; end
//       else fuucico:=uu_recerr
//     else fuucico:=uu_senderr;
//     ExitProtocol;
//     end
//   else
//     fuucico:=uu_nologin;
// 
//   if NoCarrier and not break then
//     LogError('carrier lost');
//   FinalHandshake;
//   rmsg(getres2(2300,50));    { 'Modem auflegen' }
//   Log('+','hanging up');
//   ExitInterface;
//   CloseLogfiles;
//   closewindow;
//   freeres;
//   ende:=_ende;
// end;

{
  $Log$
  Revision 1.4  2001/02/26 12:47:33  cl
  - oops; reverting accidentally committed modifications

  Revision 1.2  2001/02/21 17:45:53  cl
  - first things for TUUCPNetcall

  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground, was uucico.pas
  Revision 1.6  2000/11/18 14:07:48  fe
  Replaced exist() with fileexists().

  Revision 1.5  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.4  2000/11/02 21:27:04  fe
  bzip2 support added.

  Revision 1.3  2000/10/27 16:14:29  fe
  uucico notduerftig uebersetzbar gemacht.

}

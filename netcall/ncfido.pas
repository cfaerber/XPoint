{  $Id$

   OpenXP fido netcall unit
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC}

{ OpenXP fido netcall unit }
unit ncfido;

interface

uses ncmodem,timer,fidoglob,xpglobal,classes,fileio,osdepend;

type
  hellor = record
                 signal         : word;   { = 'o'     }
                 hello_version  : word;   { = 1       }
                 product        : word;
                 HiVersion      : word;
                 LoVersion      : word;
                 MyName         : array[0..59] of char;
                 SysopName      : array[0..19] of char;
                 zone           : word;
                 net            : word;
                 node           : word;
                 point          : word;
                 my_password    : array[0..7] of char;
                 reserved2      : array[0..7] of byte;
                 capabilities   : word;
                 reserved3      : array[0..11] of byte;
               end;

type
  TFidomailer = class(TModemNetcall)
  protected
    hello: record h: hellor; crc: word end;
    aresult: integer; fa: FidoAdr;

    procedure InitHelloPacket;
    function ProductName(pc:word):string;
    function fmSS(state:byte):byte;    { 0=brk, 1=FTS-001, 2=YooHoo, 3=EMSI }
    function fmSH:byte;     { Send Hello Packet }
    function fmRH(state:byte):boolean;   { Receive Hello Packet }
    procedure LogHelloData;
    function fmYS(state:byte):byte;     { WaZOO: 0=Abbruch, 1=Again, 2=ok }
    function EMSIHandshake:byte;    { 0=Abbruch, 1=Again, 2=ok->WaZOO }
    procedure fmS;          { FTS-001 / FTS-007 }
    procedure WaZOOsession;
    function BinkPSessionSetup: byte;
    function BinkPFileTransfer: byte;

  public
    {These HAVE to be initialized when calling PerformNetcall}
    OutgoingFiles,IncomingFiles: TStringList;
    IncomingDir: String;
    AKAs: String;
    Username: String;
    OwnAddr: String;
    DestAddr: String;
    Password: String;
    SysName: String;
    SerNr: String;
    ExtFNames: Boolean;
    SendEmpty: Boolean;
    TXT: String;
    UseEMSI: Boolean;
    UseBinkP: Boolean;
    SetTime: Boolean;
    SendTrx: Boolean;
    MinCPS: Longint;
    AddTXT: String;

    function PerformNetcall: Integer;
  end;

implementation

uses
  keys,zmodem,progressoutput,resource,sysutils,typeform,debug,montage,crc,xpdiff,objcom,md5;

const {Y_DietIfna = $0001;}   { Capability Flags }
      Zed_Zipper = $0004;
      Zed_Zapper = $0008;
      {Does_Ianus = $0010; }
      Do_Domain  = $4000;
      WaZooFReq  = $8000;
      MyCap      = Zed_Zipper + Zed_Zapper;

      qTimers     = 5;

const
      EMSI_INQ   = '**EMSI_INQC816';
      EMSI_REQ   = '**EMSI_REQA77E';
      EMSI_ACK   = '**EMSI_ACKA490';
      EMSI_NAK   = '**EMSI_NAKEEC3';
      EMSI_HBT   = '**EMSI_HBTEAEE';
      EMSI_DAT   = '**EMSI_DAT';


var   TimerObj: tTimer;
      Timers : array[0..qTimers-1] of tTimer;
      Dummy  : LongInt;

{ ----- some generic routines ------------------------------------------------------}

function GetString(var buf; Len: Integer): String;
{Gibt Len Zeichen aus buf als String zurueck}
var
  s: String;
begin
  SetLength(s,Len);
  Move(Buf,s[1],Len);
  GetString:=s;
  Debug.DebugLog('ncfido','GetString: "'+s+'"',DLDebug);
end;

procedure SetZero(var buf; s:string; ml:byte);
{Bewegt max. ml Zeichen aus S in Buf und haengt #0 an}
begin
  s:=s+#0;
  Move(s[1],buf,min(ml,length(s)));
end;

const tage : array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
      tagsec = 86400;  { 24*60*60 }

procedure setfeb(y:word);
begin
  if schaltj(y) then
    tage[2]:=29
  else
    tage[2]:=28;
end;

function secsfrom70:longint;
var i         : integer;
    secs      : longint;
    y,m,d     : smallword;
    h,min,s,s1: smallword;
begin
  decodedate(now,y,m,d);
  decodetime(now,h,min,s,s1);
  secs:=0;
  for i:=1970 to y-1 do
    inc(secs,iif(schaltj(i),366,365)*tagsec);    { Jahre }
  setfeb(y);
  for i:=1 to m-1 do
    inc(secs,longint(tage[i])*tagsec);          { + Monate }

  inc(secs,longint(d-1)*tagsec);                { + Tage }
  inc(secs,longint(h)*3600+min*60+s);           { + hms  }
  secsfrom70:=secs;
end;

(*procedure set_time(secs:longint);
var y,m : word;
    h,min,s: word;
begin
  if secs<0 then exit;
  y:=1970;
  while secs>=iif(schaltj(y),366,365)*tagsec do begin
    dec(secs,iif(schaltj(y),366,365)*tagsec);
    inc(y);
    if y>2099 then exit;
  end;
  setfeb(y); m:=1;
  while (secs>=tagsec*tage[m]) do begin
    dec(secs,tagsec*tage[m]);
    inc(m);
  end;
  secs:=secs mod tagsec;
  h:=secs div 3600;       secs:=secs mod 3600;
  min:=secs div 60;       secs:=secs mod 60;
  s:=secs;
  dos.settime(h,min,s,0);
end; *)

{$I ncfido-yoohoo.inc}
{$I ncfido-emsi.inc}
{$I ncfido-wazoo.inc}
{$I ncfido-binkp.inc}

function TFidomailer.PerformNetcall: Integer;
var iTimer: Integer; Ende: Boolean;
begin
  aresult:=el_noconn; result:=el_noconn;
  if not Connect then exit;
  TimerObj.Init;

  if UseBinkP then begin // BinkP mailer
    if BinkPSessionSetup=0 then
      if BinkPFileTransfer=0 then aresult:=el_ok;
    end else begin // standard mailer
    for iTimer:=0 to qTimers-1 do Timers[iTimer].Init;
    SplitFido(OwnAddr,FA,2);
    InitHelloPacket;
    repeat
      Ende:=true; aresult:=0;
      case fmSS(0) of            { YooHoo  }
        1 : fmS;                 { FTS-001 }
        2 : case fmYS(1) of      { WaZOO   }
              0 : aresult:=EL_nologin;
              1 : ENDE:=false;
              2 : WaZOOsession;  { Batch Up/Download }
            end;
        3 : case EMSIHandshake of      { EMSI }
              0 : aresult:=EL_nologin;
              1 : ENDE:=false;
              2 : WaZOOsession;  { Batch Up/Download }
            end;
      end;
      if aresult=EL_nologin then log(lcError,'login handshake failed');
    until ENDE;
    end;

  TimerObj.Done;
  Disconnect;
  result:=aresult;
end;

end.

{
  $Log$
  Revision 1.7  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.6  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.5  2001/07/28 12:04:19  mk
  - removed crt unit as much as possible

  Revision 1.4  2001/05/19 16:20:25  ma
  - implemented secure BinkP CRAM-MD5 authentication

  Revision 1.3  2001/04/16 18:13:28  ma
  - ProgOutWin now pauses a bit on closing
    (some seconds if an error occured, one second if not)
  - removed other delays

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}

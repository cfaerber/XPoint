{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ PMs mit TO-ID versehen }

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

uses  xpglobal, dos,typeform,xpdatum;

const
      readfirst = 2500;
      TO_ID     : string[10] = '/'#0#0#8#8'TO:';
      xparc     : string[15] = 'X-XP-ARC:'#13#10;
      midlen    = 120;
      nt_ZConnect=2;

      attrCrash   = $0002;            { header.attrib: Crashmail   }
      attrFile    = $0010;            { File attached              }
      attrReqEB   = $1000;            { EB anfordern               }
      attrIsEB    = $2000;            { EB                         }
      AttrPmReply = $0100;            { PM-Reply auf AM (Maus)     }
      AttrQuoteTo = $0400;            { QuoteTo (Maus)             }

type   header = record
                  netztyp    : byte;
                  archive    : boolean;       { archivierte PM }
                  empfaenger : string[90];    { Brett / User / TO:User }
                  betreff    : string[40];
                  absender   : string[80];
                  datum      : string[11];    { Netcall-Format }
                  zdatum     : string[20];    { ZConnect-Format; nur auslesen }
                  ddatum     : string[14];    { Dateidatum, jjjjmmtthhmmss }
                  empfanz    : integer;       { Anzahl EMP-Zeilen }
                  pfad       : string;        { Netcall-Format }
                  msgid,ref  : string[midlen];{ ohne <> }
                  typ        : string[1];     { T / B }
                  groesse    : longint;
                  komlen     : longint;       { Kommentar-L„nge }
                  realname   : string[40];
                  programm   : string[40];    { Mailer-Name }
                  datei      : string[40];    { Dateiname }
                  prio       : byte;          { 10=direkt, 20=Eilmail }
                  real_box   : string[20];    { falls Adresse = User@Point }
                  hd_point   : string[25];    { eigener Pointname }
                  pm_bstat   : string[20];    { Bearbeitungs-Status }
                  attrib     : word;          { Attribut-Bits }
                  fido_to    : string[36];
                end;

      charr   = array[0..65530] of char;
      charrp  = ^charr;

var   f,f2  : file;
      nn    : longint;
      uname : string;
      zconn : boolean;


procedure helppage;
begin
  writeln('PMs in Pufferdatei konvertieren:  PMCONV <Puffer> [Username]');
  writeln;
  writeln('Dokumentation: s. XPOINT.TXT, Anhang C');
  halt(1);
end;


procedure error(txt:string);
begin
  writeln;
  writeln(txt);
  halt(1);
end;

{$I xpmakehd.inc}


procedure checkit;
var p      : charrp;
    ps     : word;
    fs,adr : longint;
    rr     : word;
    hd     : header;
    hds    : longint;
    ok     : boolean;
    n      : longint;
    c      : char;

  procedure copymsg;
  var rr   : word;
      size : longint;
  begin
    if (left(hd.empfaenger,1)<>'/') and
       (left(hd.empfaenger,length(TO_ID))<>TO_ID) and not hd.archive and
       ((uname='') or (UpperCase(left(hd.empfaenger,length(uname)))<>UpperCase(uname)))
    then begin
      if zconn then
        blockwrite(f2,xparc[1],length(xparc))
      else
        blockwrite(f2,TO_ID[1],length(TO_ID));
      inc(nn);
      end;
    seek(f,adr);
    size:=hd.groesse+hds;
    while size>0 do begin
      blockread(f,p^,min(ps,size),rr);
      blockwrite(f2,p^,rr);
      dec(size,rr);
      end;
  end;

begin
  ps:=65536;
  getmem(p,ps);
  reset(f,1);
  if ioresult<>0 then error('Puffer nicht vorhanden!'#7);
  fs:=filesize(f);
  if fs<=8 then error('Puffer ist leer.');
  adr:=0; n:=0; nn:=0;
  rewrite(f2,1);
  while (adr<fs) do begin
    seek(f,adr);
    makeheader(zconn,f,hds,hd,ok);
    if not ok then
      error('Fehlerhafter Puffer!'#7);
    inc(n);
    write(#8#8#8#8#8,n:5);
    copymsg;
    inc(adr,hd.groesse+hds);
    end;
  close(f); close(f2);
  freemem(p,ps);
  if adr-fs>2 then begin
    writeln;
    error('Letzte Nachricht am Pufferende ist unvollst„ndig!');
    writeln('Bitte mit XPCHECK reparieren.');
    halt(1);
    end;
end;


Procedure MakeBak(n,newext:string);
var bakname : string;
    f       : file;
    res     : integer;
begin
  assign(f,n);
  if cpos('.',n)=0 then bakname:=n+'.'+newext
  else bakname:=copy(n,1,pos('.',n))+newext;
  assign(f,bakname);
  {$I-}
    setfattr(f,archive);
    erase(f);
    res:=ioresult;
  {$I+}
  assign(f,n);
  setfattr(f,archive);
  rename(f,bakname);
end;


function ZC_puffer(fn:pathstr):boolean;
var t : text;
    z : boolean;
    s : string;
    abs,emp,eda : boolean;
begin
  assign(t,fn);
  {$I-}
  reset(t);
  if ioresult<>0 then
    ZC_puffer:=false
  else begin
    abs:=false; emp:=false; eda:=false;
    s:=':';
    while (cpos(':',s)>0) and not eof(t) do begin
      readln(t,s);
      UpString(s);
      if left(s,4)='ABS:' then abs:=true;
      if left(s,4)='EMP:' then emp:=true;
      if left(s,4)='EDA:' then eda:=true;
      end;
    close(t);
    ZC_puffer:=abs and emp and eda;
    end;
end;


begin
  writeln;
  if paramcount=0 then helppage;
  uname:=paramstr(2);
  assign(f,paramstr(1));
  assign(f2,'pmconv.$$$');
  zconn:=ZC_puffer(paramstr(1));
  writeln('Puffer-Format: ',iifs(zconn,'ZCONNECT','Z-Netcall'));
  write('Puffer wird konvertiert...     ');
  checkit;
  makebak(paramstr(1),'bak');
  rename(f2,paramstr(1));
  writeln(' ok.');
  writeln;
  writeln(nn,' Nachrichten wurden konvertiert.');
end.
{
  $Log$
  Revision 1.5  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.4  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.3  2000/06/05 16:16:21  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.2  2000/04/15 10:58:31  mk
  - 1001x .DOC in .TXT geandert

  Revision 1.1  2000/03/03 13:05:36  mk
  PMCONV.PAS in den Tree aufgenommen

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - UUCICO-Interface }

{$I XPDEFINE.INC}

unit  xpuu;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  typeform,fileio,resource,xp0,xp1,xpglobal;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      uu_parerr  = 1;
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;

function uucico(CommandFile:string; start:longint; var ende:boolean;
                var waittime:integer; var sendtime,rectime:longint;
                var uulogfile:string):integer;


implementation  { ---------------------------------------------------- }

const  ConfigFile = 'uucico.cfg';
       ResultFIle = 'UUCICOR.TMP';


function uucico(CommandFile:string; start:longint; var ende:boolean;
                var waittime:integer; var sendtime,rectime:longint;
                var uulogfile:string):integer;
var t        : text;
    id       : string;
    s0,s     : string;
    p        : byte;
begin
  assign(t,ConfigFile);
  rewrite(t);
  writeln(t,'# ',getres(718));
  writeln(t);
  with boxpar^,comn[boxpar^.bport] do begin
    writeln(t,'Language=',ParLanguage);
    writeln(t,'Debug=',iifc(ParDebug,'Y','N'));
    writeln(t,'DebugWindow=1 ',screenwidth,' 4 ',screenlines-2);
    writeln(t,'Colors=$',hex(col.colmailer,2),' $',hex(col.colmailerhigh,2),
              ' $',hex(col.colmailerhi2,2));
    writeln(t,'Server=',boxname);
    writeln(t,'Node=',iifs(UUCPname<>'',UUCPname,pointname));
    writeln(t,'MaxWinSize=',MaxWinSize);
    writeln(t,'MaxPacketSize=',MaxPacketSize);
    writeln(t,'VarPacketSize=',iifc(varpacketsize,'Y','N'));
    writeln(t,'ForcePacketSize=',iifc(forcepacketsize,'Y','N'));
    writeln(t,'Protocols=',uuprotos);
    writeln(t,'SizeNegotiation=',iifc(sizenego,'Y','N'));
    writeln(t,'FilereqPath=',FilePath);
    writeln(t,'C-File=',CommandFile);
    writeln(t,'UUlogfile=',uulogfile);
    writeln(t,'FOSSIL=',iifc(Fossil,'Y','N'));
    writeln(t,'PortNr=',bport);
    if not fossil then begin
      writeln(t,'PortAdr=',hex(CPort,3));
      writeln(t,'IRQ=',CIrq);
      writeln(t,'TriggerLevel=',tlevel);
      end;
    writeln(t,'Baud=',baud);
    writeln(t,'IgnoreCD=',iifc(IgCD,'Y','N'));
    writeln(t,'IgnoreCTS=',iifc(IgCTS,'Y','N'));
    writeln(t,'UseRTS=',iifc(UseRTS,'Y','N'));
    writeln(t,'OnlineTime=',start);
    if ParOS2<>0 then
      writeln(t,'ReleaseTime=',ParOS2);
    if maxfsize>0 then
      writeln(t,'MaxFileSize=',maxfsize);
    close(t);
    end;
  if FileExists(ResultFile) then _era(ResultFile);
  {$ifdef unix}
  {$hint Muss noch angepasst werden }
  {$endif}
  shell('UUCICO.EXE '+ConfigFile,500,4);            { --- uucico.exe }
  if not FileExists(ResultFile) then
    uucico:=uu_parerr
  else begin
    uucico:=uu_recerr;
    assign(t,ResultFile);
    reset(t);
    while not eof(t) do begin
      readln(t,s0);
      s:=trim(s0);
      p:=cpos('=',s);
      if (s<>'') and (LeftStr(s,1)<>';') and (LeftStr(s,1)<>'#') then begin
        id:=LowerCase(trim(LeftStr(s,p-1)));
        s:=trim(mid(s,p+1));
        if id='result'      then uucico:=ival(s) else
        if id='stopdialing' then ende:=(UpperCase(s)<>'N') else
        if id='waittime'    then waittime:=minmax(ival(s),0,maxlongint) else
        if id='sendtime'    then sendtime:=minmax(ival(s),0,maxlongint) else
        if id='rectime'     then rectime:=minmax(ival(s),0,maxlongint);
        end;
      end;
    close(t);
    _era(resultfile);
    end;
end;

end.
{
  $Log$
  Revision 1.14  2000/11/18 15:46:05  hd
  - Unit DOS entfernt

  Revision 1.13  2000/11/14 15:51:38  mk
  - replaced Exist() with FileExists()

  Revision 1.12  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.11  2000/10/17 10:06:01  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.10  2000/07/12 13:15:02  hd
  - Ansistring

  Revision 1.9  2000/07/04 12:04:32  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.8  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.7  2000/06/29 13:01:03  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.6  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.5  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}

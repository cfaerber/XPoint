{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - UUCICO-Interface }

{$I XPDEFINE.INC}
{$O+,F+}

unit  xpuu;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,fileio,resource,xp0,xp1;

const uu_ok      = 0;       { Ergebniscodes von ucico }
      uu_parerr  = 1;
      uu_nologin = 2;
      uu_senderr = 3;
      uu_recerr  = 4;

function uucico(const CommandFile:pathstr; start:longint; var ende:boolean;
                var waittime:integer; var sendtime,rectime:longint;
                var uulogfile:string):integer;


implementation  { ---------------------------------------------------- }

const  ConfigFile = 'UUCICO.CFG';
       ResultFIle = 'UUCICOR.TMP';


function uucico(const CommandFile:pathstr; start:longint; var ende:boolean;
                var waittime:integer; var sendtime,rectime:longint;
                var uulogfile:string):integer;
var t        : text;
    id       : string[20];
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
    writeln(t,'DebugWindow=1 80 4 ',screenlines-2);
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
  if exist(ResultFile) then _era(ResultFile);
  shell('UUCICO.EXE '+ConfigFile,500,4);            { --- uucico.exe }
  if not exist(ResultFile) then
    uucico:=uu_parerr
  else begin
    uucico:=uu_recerr;
    assign(t,ResultFile);
    reset(t);
    while not eof(t) do begin
      readln(t,s0);
      s:=trim(s0);
      p:=cpos('=',s);
      if (s<>'') and (left(s,1)<>';') and (left(s,1)<>'#') then begin
        id:=lstr(trim(left(s,p-1)));
        s:=trim(mid(s,p+1));
        if id='result'      then uucico:=ival(s) else
        if id='stopdialing' then ende:=(ustr(s)<>'N') else
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
  Revision 1.6.2.1  2001/08/12 08:46:47  mk
  - added const parameters

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
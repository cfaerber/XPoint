{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - First Unit }

unit xpx;

{$I XPDEFINE.INC }

interface

uses
  ems, crt, dos,dosx,typeform,fileio,mouse,inout,xp0,crc,xpglobal, mcb;

implementation

  {$IFDEF DPMI}
  const MinVersion = $330;
        MinVerStr  = '3.3';
        MaxHandles = 31;
  {$ELSE}
  uses  overlay;
  const MinVersion = $300;
        MinVerStr  = '3.0';
        MaxHandles = 30;
  var   handles    : array[1..maxhandles] of byte;
  {$ENDIF}

const starting : boolean = true;

var oldexit : pointer;


procedure stop(txt:string);
begin
  writeln;
  writeln(txt);
  runerror:=false;
  halt(1);
end;

{ Diese Funktion und deren Aufruf dÅrfen nicht verÑndert werden }
{ (siehe LIZENZ.TXT).                                           }
procedure logo;
var t : text;
begin
  assign(t,'');
  rewrite(t);
  writeln(t);
  write(t,xp_xp);
  if (xp_xp='CrossPoint') then write(t,'(R)');
  writeln(t,' ',verstr,betastr,' ',x_copyright,
            ' by ',author_name,' (',author_mail,')');
  writeln(t);
  writeln(t,'basierend auf CrossPoint(R) v3.2 (c) 1992-99 by ',pm);
  writeln(t);
  close(t);
end;


procedure readname;
var t    : text;
    name : string[10];
    short: string[2];
    code : string[20];
begin
  assign(t,progpath+'pname.dat');
  if existf(t) then begin
    reset(t);
    readln(t,name);
    readln(t,short);
    readln(t,code);
    close(t);
    if (ioresult=0) and
       (ival(code)=sqr(CRC32Str(reverse(name)) and $ffff)) then begin
      XP_xp:=name;
      XP_origin := '--- '+name;
      end;
    end;
end;

procedure SetHandles;
var i    : integer;
    regs : registers;
begin
  {$IFNDEF DPMI }
    for i:=1 to maxhandles do
      handles[i]:=$ff;
    for i:=1 to 5 do
      handles[i]:=mem[PrefixSeg:$18+pred(i)];
    MemW[PrefixSeg:$32] := MaxHandles;
    MemW[PrefixSeg:$34] := Ofs(handles);
    MemW[PrefixSeg:$36] := Seg(handles);
  {$ELSE}
    with regs do begin
      ah:=$67;
      bx:=maxhandles;
      msdos(regs);
      if flags and fcarry<>0 then
        writeln('Warnung: Fehler beim Anfordern von File Handles!');
      end;
  {$ENDIF}
end;

procedure TestOVR;
var ft   : longint;
    c,cc : char;
begin
  if not exist('xp.ovr') then
    stop('Die Datei XP.OVR fehlt!');
  ft:=filetime('xp.exe');
  if (ft<>0) and (abs(ft-filetime('xp.ovr'))>=60) then begin
    writeln;
    writeln('WARNUNG: Das Dateidatum von XP.OVR stimmt nicht mit dem von XP.EXE');
    writeln('         Åberein. XP.OVR stammt offenbar von einer anderen '+xp_xp+'-');
    writeln('         Version. Bitte spielen Sie das Programm aus einem '+xp_xp+'-');
    writeln('         Originalarchiv neu auf! Wenn Sie das Programm jetzt fortsetzen,');
    writeln('         wird es wahrscheinlich abstÅrzen.');
    writeln;
    writeln('         Falls Sie nach einem Neuaufspielen wieder die gleiche Fehler-');
    writeln('         meldung erhalten, ist Ihr Rechner mîglicherweise mit einem');
    writeln('         Virus infiziert.');
    writeln;
    write(#7'Programm fortsetzen (J/N)? ');
    c:='N';
    repeat
      write(c,#8);
      cc:=readkey;
      case cc of
        #0 : if readkey='' then;
        'j','J' : c:='J';
        'n','N' : c:='N';
      end;
    until (cc=#13) or (cc=#27);
    writeln;
    if (cc=#27) or (c='N') then begin
      runerror:=false;
      halt(1);
      end;
    end;
end;

function _deutsch:boolean;
var t : text;
    s : string;
begin
  filemode:=0;
  assign(t,'xp.res');
  reset(t);
  readln(t,s);
  close(t);
  _deutsch:=(ioresult=0) and (ustr(s)='XP-D.RES');
  filemode:=2;
end;


{$S-}
procedure setpath; far;
begin
  if ioresult = 0 then ;
  GoDir(shellpath);
  if ioresult<>0 then GoDir(ownpath);
  if runerror and not starting then begin
    attrtxt(7);
    writeln;
    writeln('Fehler: ',ioerror(exitcode,'<interner Fehler>'));
    end;
  exitproc:=oldexit;
end;
{$IFDEF Debug }
  {$S+}
{$ENDIF }


procedure TestCD;
var f    : file;
    attr : rtlword;
begin
  assign(f,paramstr(0));
  getfattr(f,attr);
  if attr and ReadOnly<>0 then begin
    assign(f,OwnPath+'XP$T.$1');
    rewrite(f);
    if ioresult=0 then begin
      close(f);
      erase(f);
      end
    else begin
      writeln;
      writeln(xp_xp+' kann nicht von einem schreibgeschÅtzten Laufwerk gestartet');
      writeln('werden. Kopieren Sie das Programm bitte auf Festplatte.');
      runerror:=false;
      halt(1);
      end;
    end;
end;

function xpshell:boolean; { true, wenn XP in seiner eigenen Shell gestartet wurde }
var mcb:mcbp;
    envseg:word;
    s:string;
begin
  xpshell:=false;
  mcb:=firstmcb;
  repeat
    s:=getmcbprog(mcb);
{   if s='' then s:=getmcbenvprog(getmcbenvseg(mcb)); }
{ FÅr DOS-Versionen kleiner 4.0 mÅsste man obige Zeile eigentlich aktivieren,
  da ich es aber nicht mit DOS < 4.0 testen konnte, bin ich nicht sicher, ob
  es 100%ig funktioniert.
}
    if (ustr(shortp(paramstr(0)))=ustr(s)) and (mcb^.psp_seg<>prefixseg)
      then xpshell:=true;
    mcb:=nextmcb(mcb);
  until mcb^.id='Z';
end;

begin
  checkbreak:=false;
  if swap(dosversion)<MinVersion then
    stop('DOS Version '+MinVerStr+' oder hîher erforderlich.');
  readname;
  if (left(getenv('PROMPT'),4)='[XP]') or xpshell then
    if _deutsch then stop('ZurÅck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
  SetHandles;
  ShellPath:=dospath(0);
  if (Shellpath+DirSepa<>progpath) then
    GoDir(progpath);
  oldexit:=exitproc;
  exitproc:=@setpath;
  mausunit_init;
  logo;
  {$IFNDEF NO386 }      { Die XT Version darf hier nicht testen }
  if Test8086 < 2 then
  begin
    Writeln('OpenXP lÑuft in dieser Version erst ab 386er CPUs mit CoProzessor.');
    Writeln('Eine XT-Version kann von der Homepage http://www.openxp.de bezogen werden.');
    runerror := false;
    Halt(1);
  end;
  {$ENDIF }

  {$IFNDEF DPMI}     { mit DPMI auch nicht }
    TestOVR;
    OvrInit('xp.ovr');
    if EmsTest and (ustr(left(paramstr(1),4))<>'/AV:') and (paramstr(1)<>'/?') then
      OvrInitEMS;
    OvrSetBuf(OvrGetBuf+40000);   { > CodeSize(MASKE.TPU) }
  {$ENDIF}

  OwnPath:=progpath;
  if ownpath='' then getdir(0,ownpath);
  if right(ownpath,1)<>'\' then
    ownpath:=ownpath+'\';
  if cpos(':',ownpath)=0 then begin
    if left(ownpath,1)<>'\' then ownpath:='\'+ownpath;
    ownpath:=getdrive+':'+ownpath;
    end;
  UpString(ownpath);
  TestCD;
  starting:=false;
end.
{
  $Log$
  Revision 1.18.2.3  2000/11/21 22:40:37  mk
  - MCB-Code von XP2 (Robert Boeck) hinzugefuegt um auf schon geladenes XP zu testen

  Revision 1.18.2.2  2000/07/01 11:17:27  mk
  - 32 Bit Teile entfernt

  Revision 1.18.2.1  2000/07/01 09:22:59  mk
  - Mailerstringanpassungen

  Revision 1.18  2000/06/19 20:23:05  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.17  2000/05/15 13:56:53  hd
  - Linux: Env-Var XPHOME uebersteuert nun die Vorgabe ~/.openxp

  Revision 1.16  2000/05/14 17:22:51  hd
  - Linux: Manuelle Init. der XPCurses

  Revision 1.15  2000/05/08 18:22:49  hd
  - Unter Linux wird jetzt $HOME/openxp/ als Verzeichnis benutzt.

  Revision 1.14  2000/05/06 15:53:51  hd
  - AssignCRT statt Assign in logo

  Revision 1.13  2000/05/03 20:38:21  hd
  Unix-Anpassung

  Revision 1.12  2000/05/02 20:51:02  hd
  OwnPath an UnixFS angepasst

  Revision 1.11  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.10  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.9  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.8  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

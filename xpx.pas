{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }

{ CrossPoint - First Unit }

unit xpx;

{$I XPDEFINE.INC }

interface

uses
  ems, xms,{$IFDEF XMSOVR} overxms, {$ENDIF} crt, dos,dosx,typeform,fileio,mouse,inout,xp0,crc,xpglobal, mcb;

implementation

  {$IFDEF DPMI}
  const MinVersion = $330;
        MinVerStr  = '3.3';
        MaxHandles = 31;
  {$ELSE}
  uses  overlay, clip, xpovl, lfn;
  const MinVersion = $300;
        MinVerStr  = '3.0';
        MaxHandles = 30;
  var   handles    : array[1..maxhandles] of byte;
  {$ENDIF}

const starting : boolean = true;

var oldexit : pointer;
    i: integer;

procedure stop(const txt:string);
begin
  writeln;
  writeln(txt);
  runerror:=false;
  halt(1);
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
    writeln('         �berein. XP.OVR stammt offenbar von einer anderen '+trim(xp_display)+'-');
    writeln('         Version. Bitte spielen Sie das Programm aus einem '+trim(xp_display)+'-');
    writeln('         Originalarchiv neu auf! Wenn Sie das Programm jetzt fortsetzen,');
    writeln('         wird es wahrscheinlich abst�rzen.');
    writeln;
    writeln('         Falls Sie nach einem Neuaufspielen wieder die gleiche Fehler-');
    writeln('         meldung erhalten, ist Ihr Rechner m�glicherweise mit einem');
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

{.$define mcbdebug}

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
      writeln(trim(xp_display)+' kann nicht von einem schreibgesch�tzten Laufwerk gestartet');
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

{$ifdef mcbdebug}
  writeln;
  writeln('PSP  Env. Typ    Gr��e  Prog.   Prog. (Environment)');
  writeln('Seg. Seg.               (MCB)');
  writeln('------------------------------------------------------------------------');
{$endif}

  mcb:=firstmcb;
  repeat
    s:=getmcbprog(mcb);
{   if s='' then s:=getmcbenvprog(getmcbenvseg(mcb)); }
{ F�r DOS-Versionen kleiner 4.0 m�sste man obige Zeile eigentlich aktivieren,
  da ich es aber nicht mit DOS < 4.0 testen konnte, bin ich nicht sicher, ob
  es 100%ig funktioniert.
}
    if (ustr(shortp(paramstr(0)))=ustr(s)) and (mcb^.psp_seg<>prefixseg)
       and (mcb^.size*16>20480)
      then xpshell:=true;

{$ifdef mcbdebug}
    write(hex(mcb^.psp_seg,4),' ',
          hex(getmcbenvseg(mcb),4),' ');
    if ispsp(mcb) then write('PSP   ') else case mcb^.psp_seg of
      $0000: write('frei  ');
      $0008: write('DOS   ');
      $0006: write('DRDOS ');
      $0007: write('DRDOS ');
      $FFF7: write('386MAX');
      $FFFA: write('386MAX');
      $FFFD: write('386MAX');
      $FFFE: write('386MAX');
      $FFFF: write('386MAX');
      else write('?     ');
    end;
    write(mcb^.size*16:6,
          getmcbprog(mcb):9,' ',
          getmcbenvprog(getmcbenvseg(mcb)));
    writeln;
{$endif}

    mcb:=nextmcb(mcb);
  until mcb^.id='Z';

{$ifdef mcbdebug}
  write(#13#10'-> Enter');
  readln;
{$endif}

end;

function _deutsch:boolean;
var t : text;
    s : string;
    i : integer;
begin
  filemode:=0;
  assign(t,'XP.RES');
  reset(t);
  readln(t,s);
  close(t);
  for i := 1 to length(s) do s[i] := UpCase(s[i]);
  _deutsch:=(ioresult<>0) or (s='XP-D.RES');
  filemode:=2;
end;

procedure logo;
var t : text;
begin
  assign(t,'');
  rewrite(t);
  writeln(t);
  write(t,xp_xp);
  if (xp_xp='CrossPoint') then write(t,'(R)');
  writeln(t,' ',verstr,betastr,iifs(xmsovrbuf,xmsstr,''));
  writeln(t,x_copyright,' by ',author_name,' (',author_mail,')');
  writeln(t);
  if _deutsch then
  begin
    writeln(t,'basierend auf CrossPoint(R) v3.2  (c) 1992-1999 Peter Mandrella');
  end else
  begin
    writeln(t,'based on CrossPoint(R) v3.2  (c) 1992-1999 Peter Mandrella');
  end;
  writeln(t);
  close(t);
end;

begin
  checkbreak:=false;
  if swap(dosversion)<MinVersion then
    stop('DOS Version '+MinVerStr+' oder h�her erforderlich.');
  readname;
  if (left(getenv('PROMPT'),4)='[XP]') or xpshell then
    if _deutsch then stop('Zur�ck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
  SetHandles;
  ShellPath:=dospath(0);
  if (Shellpath+DirSepa<>progpath) then
    GoDir(progpath);
  oldexit:=exitproc;
  exitproc:=@setpath;
  mausunit_init;
  {$IFNDEF NO386 }      { Die XT Version darf hier nicht testen }
  if Test8086 < 2 then
  begin
    Writeln('OpenXP l�uft in dieser Version erst ab 386er CPUs mit CoProzessor.');
    Writeln('Eine XT-Version kann von der Homepage http://www.' + 'openxp.de bezogen werden.');
    runerror := false;
    Halt(1);
  end;
  {$ENDIF }
  xmst:=xmstotal; 
  {$IFNDEF DPMI}     { mit DPMI auch nicht }
    TestOVR;
    OvrInit('xp.ovr'); 

    {Lightweight-Readpar}
    noovrbuf:=false;
    xmsovrbuf:=false; 
    for i:=1 to paramcount do begin     
      if ((paramstr(i)='/?') and (not noovrbuf)) then noovrbuf:=true;
      if ((ustr(left(paramstr(i),4))='/AV:') and (not noovrbuf)) then noovrbuf:=true;
      {$IFDEF XMSOVR } 
      if (ustr(paramstr(i))='/XMSOVR') then xmsovrbuf:=true;
      {$ENDIF }                                                 
    end;

    {$IFDEF XMSOVR } 
    if ((EmsTest) and (not noovrbuf) and ((EmsAvail*16)>2040)) then begin
      OvrInitEMS;
      xmsovrbuf:=false;
    end 
    else if ((XmsTest) and (not noovrbuf) and (xmsovrbuf) and (XmsAvail>2040)) then
      OvrInitXMS
    else xmsovrbuf:=false; 
    {$ELSE }
    if ((EmsTest) and (not noovrbuf)) then
      OvrInitEMS;
    {$ENDIF }
    OvrSetBuf(OvrGetBuf+50000);   { > CodeSize(MASKE.TPU) }
  {$ENDIF}
  logo;
  {$IFNDEF NO386}
  InitWinVersion;

  If (WinVersion = 3) or { Win 9x/ME/... }
     ((WinVersion = 4) and (lo(WinNTVersion)>=5)) then { Win 2k/XP/... }
    EnableLFN;
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
  Revision 1.18.2.21  2003/01/18 09:40:26  mw
  MW: - Fehler im letzten Commit korregiert (Es mu� immer
        EMSAvail/XMSAvail abgefragt werden, um festzustellen
        ob ein Anlegen des Overlay-Buffers funktionieren kann.

  Revision 1.18.2.20  2003/01/18 07:40:42  mw
  MW: - XP versucht jetzt nicht mehr den Overlay-Buffer im
        EMS bzw. XMS anzulegen, wenn weniger als 2040 KB
        des jeweiligen Speichers vorhanden ist.

  Revision 1.18.2.19  2003/01/17 17:01:26  mw
  MW: - Make XT-Version compile again (Part 1)

  Revision 1.18.2.18  2003/01/17 09:10:41  mw
  MW: - Variable xmsovrbuf ist nun nur noch dann true, wenn das
        Overlay wirklich ins XMS geladen wurde.
        (wird auf false gesetzt, wenn trotz Kommadoparameter
        /xmsovr das Overlay nicht ins XMS gel�aden wird.
        Dies geschied, wenn EMS zur Verf�gung steht [ dann wird
        das Overlay ins EMS geladen, egal ob /xmsovr gesetzt
        oder nicht] oder /AV: oder /? als Kommadoparameter
        gesetzt wurde [dann wird gar kein Overlay-Buffer
        verwendet].

  Revision 1.18.2.17  2003/01/16 23:30:33  my
  MY: - Schalter /xmsovr in Versionsbezeichnung ber�cksichtigt
      - betastr an einigen Stellen erg�nzt

  Revision 1.18.2.16  2003/01/14 21:47:08  mw
  MW: - XMS-Overlaycache nun per Kommandozeile einschaltbar
        (Wenn Compilerschalter gesetzt und kein EMS vorhanden)
      - Kommandozeilenauswertung in der xpx.pas jetzt
        positionsunabh�ngig realisiert.
      - Compilerschalter XMSOVR jetzt per Default eingeschaltet

  Revision 1.18.2.15  2003/01/10 22:02:32  my
  MY:- Log-Kosmetik

  Revision 1.18.2.14  2003/01/10 18:11:50  mw
  MW: - Overlaycache im XMS (Default: per Compilerschalter ausgeschaltet).
      - Speicherstatistik zeigt jetzt auch XMS-Belegung an.
      Achtung: Diese Features laufen noch nicht auf jedem Betriebssystem
               stabil!

  Revision 1.18.2.13  2002/03/08 23:40:11  my
  MY:- Registrierungs-, Beta-, "�ber OpenXP"- und sonstige Dialoge auf
       OpenXP/16 umgestellt und Copyright-Hinweise sowie Kontakte
       aktualisiert.

  Revision 1.18.2.12  2001/09/16 20:36:58  my
  JG+MY:- Neuer Men�punkt "?" (Hilfe) im Hauptmen� mit Untermen�s f�r
          n�tzliche und/oder in der Hilfe ansonsten nur schwer auffindbare
          Informationen. Untermen� "�ber OpenXP" zeigt Versions- und
          Snapshotnummer sowie OpenXP-Kontakte an. Beta- und
          Registrierungsfenster optisch angepa�t.

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.18.2.11  2001/08/11 20:16:30  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.18.2.10  2001/08/11 16:38:00  mk
  - XP1.pas is now overlay
  - resized Overlaybuffer

  Revision 1.18.2.9  2001/08/05 11:45:37  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.18.2.8  2001/07/02 18:40:31  cl
  - Better Windows NT/2k/XP detection (needs XP_NTVDM.DLL)
  - Clipboard support under NT/2k/XP (needs XP_NTVDM.DLL)

  Revision 1.18.2.7  2001/06/23 19:14:53  mk
  - LFN bei Win2000 automatisch einschalten

  Revision 1.18.2.6  2001/01/10 17:39:07  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette,
  UUZ und Bugfixes

  Revision 1.18.2.5  2000/12/05 13:09:42  mk
  - einige Datei/Verzeichnisnamen gross geschrieben

  Revision 1.18.2.4  2000/11/26 10:18:18  mk
  RB:- MCB-Test jetzt mit Groesse der Datei

  Revision 1.18.2.3  2000/11/21 22:40:37  mk
  - MCB-Code von XP2 (Robert Boeck) hinzugefuegt um auf schon
    geladenes XP zu testen

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
  erste Anpassungen f�r die portierung mit VP

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

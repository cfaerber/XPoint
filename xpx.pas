{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - First Unit }

unit xpx;

{$I XPDEFINE.INC }

interface

uses ems,crt, dos,dosx,typeform,fileio,mouse,inout,xp0,xpcrc32, xpglobal;

implementation

{$IFNDEF BP }
const MinVersion = $330;
      MinVerStr  = '3.3';
      MaxHandles = 31;
{$ELSE}
uses overlay;
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


function plevelstr:string;           { Patchlevel }
begin
  if lastchar(patchlevel)='0' then
    plevelstr:=''
  else
    plevelstr:=' pl'+lastchar(patchlevel);
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
  writeln(t,' ',verstr,pformstr,betastr,' ',x_copyright,
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
       (ival(code)=sqr(crc32(reverse(name)) and $ffff)) then begin
      XP_xp:=name;
      XP_name := '## '+name+' '+verstr+betastr;
      XP_origin := '--- '+name;
      XP_short := short;
      end;
    end;
end;


procedure SetHandles;
var i    : integer;
    regs : registers;
begin
  {$IFNDEF DPMI }
{$IFNDEF Ver32 }
    for i:=1 to maxhandles do
      handles[i]:=$ff;
    for i:=1 to 5 do
      handles[i]:=mem[PrefixSeg:$18+pred(i)];
    MemW[PrefixSeg:$32] := MaxHandles;
    MemW[PrefixSeg:$34] := Ofs(handles);
    MemW[PrefixSeg:$36] := Seg(handles);
{$ENDIF }
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


{$F+,S-}
procedure setpath;
var i : integer;
begin
  i:=ioresult;
  GoDir(shellpath);
  if ioresult<>0 then GoDir(ownpath);
  if runerror and not starting then begin
    attrtxt(7);
    writeln;
    writeln('Fehler: ',ioerror(exitcode,'<interner Fehler>'));
    end;
  exitproc:=oldexit;
end;
{$F-,S+}


procedure TestCD;
var f    : file;
    attr : smallword;
begin
{$IFNDEF WIN32}
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
{$ENDIF}
end;


begin
  checkbreak:=false;
{$IFNDEF Win32 }
  if swap(dosversion)<MinVersion then
    stop('DOS Version '+MinVerStr+' oder hîher erforderlich.');
{$ENDIF }
  readname;
  if left(getenv('PROMPT'),4)='[XP]' then
    if _deutsch then stop('ZurÅck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
  SetHandles;
  ShellPath:=dospath(0);
  if Shellpath+'\'<>progpath then
    GoDir(progpath);
  oldexit:=exitproc;
  exitproc:=@setpath;
  mausunit_init;
  logo;
{$IFDEF BP }            { alles andere sind sowieso 32 Bit Versionen }
  {$IFNDEF NO386 }      { Die XT Version darf hier nicht testen }
  if Test8086 < 2 then
  begin
    Writeln('OpenXP lÑuft in dieser Version erst ab 386er CPUs mit CoProzessor.');
    Writeln('Eine XT-Version kann von der Homepage http://www.openxp.de bezogen werden.');
    runerror := false;
    Halt(1);
  end;
  {$ENDIF }
{$ENDIF }

{$IFNDEF Ver32}      { Unter 32 Bit haben wir keine Overlays }
  {$IFNDEF DPMI}     { mit DPMI auch nicht }
    TestOVR;
    OvrInit('xp.ovr');
    if EmsTest and (ustr(left(paramstr(1),4))<>'/AV:') and (paramstr(1)<>'/?') then
      OvrInitEMS;
    OvrSetBuf(OvrGetBuf+40000);   { > CodeSize(MASKE.TPU) }
  {$ENDIF}
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

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Exec-Swapper }

{$I XPDEFINE.INC }
{$define debugtofile}

{$IFDEF BP }
  {$F+}
{$ENDIF }

unit  exxec;

interface

uses
  xpglobal, dos, typeform, fileio;

const ExecOk      = 0;
      ExecSwaperr = 1;
      ExecSwapweg = 2;
      ExecSwapre  = 3;

      ExecTestres : boolean = true;
      ExecUseEms  : boolean = true;
      ExecUseXms  : boolean = true;
      ExecSwapfile: pathstr = 'SWAPFILE.$$$';
      ExecDeutsch : boolean = true;

var
  ExecResident : procedure;


{ 0=ok, 1=Swap-Fehler }

function Xec(var prog:string; space,envspace:word; var prompt:string;
             var errorlevel:word):byte;


implementation  { --------------------------------------------------- }


{$IFNDEF BP }

uses
  sysutils;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

function Xec(var prog:string; space,envspace:word; var prompt:string;
             var errorlevel:word):byte;

  function environment:string;
  begin
    if envspace=0 then environment:=''
    else environment:=' /E:'+strs(envspace);
  end;
var
    pp    : byte;
    para  : string;
    dpath : pathstr;
begin
  pp:=pos(' ',prog);
  if pp=0 then para:=''
  else begin
    para:=' '+trim(copy(prog,pp+1,255));
    prog:=left(prog,pp-1);
  end;
  prog:=ustr(prog);

  if (pos('|',para)>0) or (pos('>',para)>0) or (pos('<',para)>0) then
    dpath:=''
  else begin
    if FileExists(prog) then dpath:=prog
    else dpath:=UStr(fsearch(prog,getenv('PATH')));
    if (right(dpath,4)<>'.EXE') and (right(dpath,4)<>'.COM') then
      dpath:='';
  end;
  if (para<>'') and (para[1]<>' ') then para:=' '+para;
  if dpath='' then begin
    para:=environment+' /c '+prog+para;
    dpath:=getenv('comspec');
  end;
  SwapVectors;
  Exec(dpath, para);
  SwapVectors;
  ErrorLevel := DOSExitCode;
  { Wir nicht sauber belegt, also von Hand machen }
  DosError :=0;
  { Alle anderen Fehler kînnen in 32 Bit nicht auftreten }
  Xec := ExecOk;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }


{$ELSE }

uses ems,xms;

procedure defresiprog;
begin
  if ExecTestres then begin
    writeln('Residentes Programm geladen! Programm wird beendet..');
    halt(1);
    end;
end;

var EmsFlag    : boolean;
    XmsFlag    : boolean;
    EMShandle  : word;        { EMS-Handle, oder 0 }
    XMShandle  : word;        { XMS-Handle, oder 0 }
    MorePage   : word;        { erste EMS-Seite fÅr zweiten Swapbereich }
    MoreDest   : longint;     { XMS-Offset fÅr zweiten Swapbereich }



function exec2(var dpath,para:string; swapstart,swapmore:word; envir:pointer):word;
external;
{$L exxec.obj}

function Xec(var prog:string; space,envspace:word; var prompt:string;
             var errorlevel:word):byte;
{$ifndef ver55}
  const freeptr : pointer = nil;
{$endif}
type so = record
            o,s : word;
          end;
var regs  : registers;
    p     : pointer;
    fs    : word;
    brk   : boolean;
    paras : word;            { belegte Paragraphs von M2  }
    free  : word;            { freie Paras nach Set Block }
    orgenv: word;
    envir : array[0..1023+18] of byte;    { neues Environment }
    newenv: pointer;
    dpath : pathstr;
    para  : string;
    pp    : byte;

    swapfile : file;
    swapstart: word;        { ab diesem Segment wird ausgelagert }
    swappars : word;        { auszulagernde Paragraphen          }
    swapmore : word;        { zusÑtzlich benîtigte Paragraphen   }
    heapfree : word;
    swapok   : boolean;

    fileanz  : word;        { Handletabelle: Anzahl Filehandles  }
    fileptr  : longint;     { Zeiger auf Handletabelle           }

  procedure set_newenv;
  var eseg,esize,
      o,b        : word;
      s          : string;
  begin
    eseg:=memw[prefixseg:$2c];
    esize:=memw[eseg-1:3]*16;
    if esize<=1024 then begin
      b:=16-(ofs(envir) mod 16);
      o:=0;
      repeat
        s:='';
        while mem[eseg:o]<>0 do begin
          s:=s+chr(mem[eseg:o]);
          inc(o);
          end;
        if ustr(left(s,7))='PROMPT=' then s:='PROMPT='+prompt+' '+copy(s,8,255);
        s:=left(s,254)+#0; inc(o);
        FastMove(s[1],envir[b],length(s));
        inc(b,length(s));
      until s=#0;
      b:=seg(envir)+ofs(envir)div 16 +1;
      memw[prefixseg:$2c]:=b;
      newenv:=@envir;
      end
    else
      newenv:=nil;
  end;

  function memfree:word;
  var regs : registers;
  begin
    with regs do begin
      ah:=$48;                { Test, ob residentes Prog. geladen }
      bx:=$ffff;
      msdos(regs);
      memfree:=bx;
      end;
  end;

  procedure SwapOut(swapp,count:word);
  var page,spar,rr : word;
      dest         : longint;
      XmsNeeded    : word;
  begin
    EmsFlag:=ExecUseEms and (EmsAvail>=count div 1024 +1 + swapmore div 1024 +2);
    if EmsFlag then begin
      EMSAlloc(count div 1024+1 + swapmore div 1024 +2,EMShandle);
      page:=0;
      repeat
        EmsPage(EMShandle,0,page);
        if count>=1024 then spar:=1024
        else spar:=count;
        FastMove(mem[swapp:0],mem[emsbase:0],spar*16);
        inc(swapp,spar);
        dec(count,spar);
        inc(page);
      until count=0;
      MorePage:=page;
      swapok:=true;
      XmsHandle:=0;
      XmsFlag:=false;
      end
    else begin
      EmsHandle:=0;
      XmsNeeded:=count div 64 +32 + swapmore div 64 +32;
      XmsFlag:=ExecUseXms and (XmsAvail>=XmsNeeded);
      if XmsFlag then XmsHandle:=XmsAlloc(XmsNeeded);
      if XmsFlag and (XmsResult=0) then begin
        dest:=0;
        repeat
          if count>2048 then spar:=2048
          else spar:=count;
          XmsWrite(XmsHandle,mem[swapp:0],dest,spar*16);
          if XmsResult<>0 then begin
            swapok:=false;
            exit;
            end;
          inc(swapp,spar);
          dec(count,spar);
          inc(dest,32768);
        until count=0;
        MoreDest:=dest;
        swapok:=true;
        end
      else begin
        XmsHandle:=0;
        assign(swapfile,ExecSwapfile);
        setfattr(swapfile,0);
        if ioresult<>0 then;
        rewrite(swapfile,1);
        repeat
          blockwrite(swapfile,mem[swapp:0],min(count,$ff0)*16,rr);
          if (count>0) and (rr=0) then
            inoutres:=101;
          inc(swapp,rr div 16);
          dec(count,rr div 16);
        until (count=0) or (inoutres<>0);
        close(swapfile);
        if (inoutres=0) and (swapmore=0) then
          setfattr(swapfile,readonly);
        swapok:=inoutres=0;
        if not swapok then begin
          Xec:=ExecSwaperr;
          erase(swapfile);
          if ioresult=0 then;
          end;
        end;
      end;
  end;

  procedure SwapIn(swapp,count:word);
  var rr,page,spar : word;
      src          : longint;
  begin
    if emshandle<>0 then begin
      page:=0;
      repeat
        EmsPage(EMShandle,0,page);
        if count>=1024 then spar:=1024
        else spar:=count;
        FastMove(mem[emsbase:0],mem[swapp:0],spar*16);
        inc(swapp,spar);
        dec(count,spar);
        inc(page);
      until count=0;
      EmsFree(EMShandle);
      end
    else if xmshandle>0 then begin
      src:=0;
      repeat
        if count>=2048 then spar:=2048
        else spar:=count;
        XmsRead(XmsHandle,mem[swapp:0],src,spar*16);
        inc(swapp,spar);
        dec(count,spar);
        inc(src,32768);
      until count=0;
      XmsFree(XmsHandle);
      end
    else begin
      setfattr(swapfile,0);
      reset(swapfile,1);
      if ioresult<>0 then begin
        swapok:=false;
        Xec:=ExecSwapweg;
        exit;
        end;
      { swapp:=so(heapptr).s-swappars+2; count:=swappars; }
      repeat
        blockread(swapfile,mem[swapp:0],min(count,$ff0)*16,rr);
        inc(swapp,rr div 16);
        dec(count,rr div 16);
      until (count=0) or (rr=0) or (inoutres<>0);
      if (count<>0) or (inoutres<>0) then begin
        swapok:=false;
        Xec:=ExecSwapre;
        exit;
        end;
      close(swapfile);
      erase(swapfile);
    end;
  end;

  procedure geterrorlevel;
  var regs : registers;
  begin
    errorlevel:=dosexitcode;
    if errorlevel=0 then begin
      regs.ah:=$4d;
      msdos(regs);
      errorlevel:=regs.al;
    end;
  end;

  function environment:string;
  begin
    if envspace=0 then environment:=''
    else environment:=' /E:'+strs(envspace);
  end;

  function exist(s:string):boolean;
  var sr : searchrec;
  begin
    findfirst(s,ffAnyfile,sr);
    exist:=doserror=0;
    {$IFDEF Ver32 }
      findclose(sr);
    {$ENDIF }
  end;

  {$define usebatch}
  {$ifdef debugtofile}{$i dbug.inc}{$endif}

begin
  Xec:=ExecOk;
  if so(freeptr).o>0 then          { Grî·e der Free-Liste ermitteln }
    fs:=$1000a-so(freeptr).o
  else
    fs:=0;
  if fs>0 then begin               { Freeliste sichern }
    getmem(p,fs);
    FastMove(freeptr^,p^,fs);
  end;

  pp:=pos(' ',prog);
  if pp=0 then
    para:=''
  else begin
    para:=' '+trim(copy(prog,pp+1,255));
    prog:=left(prog,pp-1);
  end;
  prog:=ustr(prog);

  if (pos('|',para)>0) or (pos('>',para)>0) or (pos('<',para)>0) then
    dpath:=''
  else begin
    if exist(prog) then
      dpath:=prog
    else
      dpath:=UStr(fsearch(prog,getenv('PATH')));
    if (right(dpath,4)<>'.EXE') and (right(dpath,4)<>'.COM') then
      dpath:='';
  end;
  if (para<>'') and (para[1]<>' ') then para:=' '+para;

  {$ifndef usebatch}
    if dpath='' then begin
      para:=environment+' /c '+prog+para;  
      dpath:=getenv('comspec');
    end;
  {$else}
    dpath:=getenv('comspec');
    WriteBatch(prog+para);
    para:=environment+' /c tmp.bat';
    {$ifdef debugtofile}dBugLog(dpath+' '+para);{$endif}
  {$endif}
  
  {$IFNDEF DPMI}
    paras:=memw[prefixseg:2]-prefixseg+1;
    space:=(space+1)*64;   { KB -> Paragraphs, + 1 extra-KB }
    heapfree:=prefixseg+paras-so(heapptr).s;
    swapok:=true;
    if (heapfree>=space) or (so(heapptr).s-ovrheaporg<64) then begin
      swappars:=0;
      swapmore:=0;
    end else begin
      swappars:=min(space-heapfree,so(heapptr).s-ovrheaporg-2);
      swapstart:=so(heapptr).s-swappars+2;
      swapmore:={0;}  max(0,space-heapfree-swappars);
      {writeln(swapmore);}
      SwapOut(swapstart,swappars);
    end;
  {$ELSE}
    swapok:=true;
    swappars:=0;
    swapmore:=0;
  {$ENDIF}

  if swapok then begin
    orgenv:=memw[prefixseg:$2c];
    set_newenv;
    {$IFNDEF DPMI}
      with regs do begin
        ah:=$4a;          { set block }
        bx:=so(heapptr).s+3-prefixseg-swappars;
        es:=prefixseg;
        msdos(regs);                   { Speicher freigeben }
      end;
      free:=memfree;

    if memw[prefixseg:$36]<>prefixseg then begin   { Filehandletabelle }
      fileanz:=memw[prefixseg:$32];                { restaurieren      }
      fileptr:=meml[prefixseg:$34];
      memw[prefixseg:$32]:=20;
      memw[prefixseg:$34]:=$18;
      memw[prefixseg:$36]:=prefixseg;
    end else
      fileanz:=0;
    {$ENDIF}

    swapvectors;
    if swapmore=0 then
      exec(dpath,para)
    else begin
      doserror:=0;
      inoutres:=Exec2(dpath,para,swapstart,swapmore,newenv);
      if ioresult<>0 then Xec:=ExecSwaperr;
    end;
    swapvectors;
    {$IFNDEF DPMI}
    if fileanz>0 then begin
      memw[prefixseg:$32]:=fileanz;
      meml[prefixseg:$34]:=fileptr;
    end;
    {$ENDIF}
    geterrorlevel;
    memw[prefixseg:$2c]:=orgenv;

    {$IFNDEF DPMI}
    { if free<>memfree then ExecResident; }
{      if swapmore=0 then }
        with regs do begin
          ah:=$4a;                { Speicherblock wieder herstellen }
          bx:=$ffff;
          es:=prefixseg;
          msdos(regs);
          ah:=$4a;
          es:=prefixseg;
          msdos(regs);
        end;
      swapok:=true;
      if swappars>0 then SwapIn(swapstart,swappars);
      if not swapok then exit;
    {$ENDIF}

    end;  { is swapok }
    {$ifdef debugtofile}dBugLog('Errorlevel: '+StrS(errorlevel));{$endif}
    {$ifdef debugtofile}dBugLog('SET TEST='+GetEnv('TEST'));{$endif}

  if fs>0 then begin
    FastMove(p^,freeptr^,fs);
    freemem(p,fs);
  end;
  {$ifdef usebatch}
  if exist('tmp.bat') then era('tmp.bat');
  {$endif}
end;

{$ENDIF }

begin
{$IFDEF BP }
  ExecResident:=DefResiprog;
{$ENDIF }
end.
{
  $Log$
  Revision 1.14  2000/05/05 00:10:49  oh
  -PGP-Aufrufe ueber Batch-Datei

  Revision 1.13  2000/05/04 15:24:47  oh
  -Parameter-Stringbegrenzung aufgehoben

  Revision 1.12  2000/04/26 18:31:21  mk
  - Para auf 255 vergroessert

  Revision 1.11  2000/04/16 20:38:49  mk
  - Fixes fuer FindFirst (2)

  Revision 1.10  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.9  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.8  2000/03/14 15:15:35  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}

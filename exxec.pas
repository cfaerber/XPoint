{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Exec-Swapper }

{$I XPDEFINE.INC }
{$F+}

unit  exxec;

interface

uses xpglobal, dos,ems,xms,typeform;

const ExecOk      = 0;
      ExecSwaperr = 1;
      ExecSwapweg = 2;
      ExecSwapre  = 3;

      ExecTestres : boolean = true;
      ExecUseEms  : boolean = true;
      ExecUseXms  : boolean = true;
      ExecSwapfile: pathstr = 'SWAPFILE.$$$';
      ExecDeutsch : boolean = true;

var   ExecResident : procedure;


{ 0=ok, 1=Swap-Fehler }

function Xec(var prog:string; space,envspace:word; var prompt:string;
             var errorlevel:word):byte;


implementation  { --------------------------------------------------- }

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



{$IFNDEF ver32}
function exec2(var dpath,para:string; swapstart,swapmore:word; envir:pointer):word;
external;  {$L exxec.obj}
{$ENDIF}


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
    para  : string[130];
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
{$IFNDEF ver32}
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
{$ENDIF}
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
{$IFNDEF ver32}
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
{$ENDIF}
  end;

  procedure SwapIn(swapp,count:word);
  var rr,page,spar : word;
      src          : longint;
  begin
    if emshandle<>0 then begin
      page:=0;
      repeat
{$IFNDEF ver32}
        EmsPage(EMShandle,0,page);
        if count>=1024 then spar:=1024
        else spar:=count;
        FastMove(mem[emsbase:0],mem[swapp:0],spar*16);
        inc(swapp,spar);
        dec(count,spar);
        inc(page);
{$ENDIF}
      until count=0;
{$IFNDEF ver32}
      EmsFree(EMShandle);
{$ENDIF}
      end
    else if xmshandle>0 then begin
      src:=0;
      repeat
        if count>=2048 then spar:=2048
        else spar:=count;
{$IFNDEF ver32}
        XmsRead(XmsHandle,mem[swapp:0],src,spar*16);
{$ENDIF}
        inc(swapp,spar);
        dec(count,spar);
        inc(src,32768);
      until count=0;
      XmsFree(XmsHandle);
      end
    else begin
{$IFNDEF ver32}
      setfattr(swapfile,0);
      reset(swapfile,1);
      if ioresult<>0 then begin
        swapok:=false;
        Xec:=ExecSwapweg;
        exit;
        end;
{$ENDIF}
      { swapp:=so(heapptr).s-swappars+2; count:=swappars; }
      repeat
{$IFNDEF ver32}
        blockread(swapfile,mem[swapp:0],min(count,$ff0)*16,rr);
{$ENDIF}
        inc(swapp,rr div 16);
        dec(count,rr div 16);
      until (count=0) or (rr=0) or (inoutres<>0);
      if (count<>0) or (inoutres<>0) then begin
        swapok:=false;
{$IFNDEF ver32}
        Xec:=ExecSwapre;
{$ENDIF}
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

  function exist(s:string):boolean;
  var sr : searchrec;
  begin
    findfirst(s,0,sr);
    exist:=doserror=0;
  end;

  function environment:string;
  begin
    if envspace=0 then environment:=''
    else environment:=' /E:'+strs(envspace);
  end;

begin
{$IFNDEF ver32}
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
  if pp=0 then para:=''
  else begin
    para:=' '+trim(copy(prog,pp+1,127));
    prog:=left(prog,pp-1);
    end;
  prog:=ustr(prog);

  if (pos('|',para)>0) or (pos('>',para)>0) or (pos('<',para)>0) then
    dpath:=''
  else begin
    if exist(prog) then dpath:=prog
    else dpath:=UStr(fsearch(prog,getenv('PATH')));
    if (right(dpath,4)<>'.EXE') and (right(dpath,4)<>'.COM') then
      dpath:='';
    end;
  if (para<>'') and (para[1]<>' ') then para:=' '+para;
  if dpath='' then begin
    para:=environment+' /c '+prog+para;
    dpath:=getenv('comspec');
    end;

  {$IFNDEF DPMI}
    paras:=memw[prefixseg:2]-prefixseg+1;
    space:=(space+1)*64;   { KB -> Paragraphs, + 1 extra-KB }
    heapfree:=prefixseg+paras-so(heapptr).s;
    swapok:=true;
    if (heapfree>=space) or (so(heapptr).s-ovrheaporg<64) then begin
      swappars:=0;
      swapmore:=0;
      end
    else begin
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
    {$IFNDEF Ver32}
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
      end
    else
      fileanz:=0;
    {$ENDIF}
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

  if fs>0 then begin
    FastMove(p^,freeptr^,fs);
    freemem(p,fs);
    end;
{$ENDIF}
end;

begin
  ExecResident:=DefResiprog;
end.

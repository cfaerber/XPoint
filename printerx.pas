{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                      UNIT printerx                      *)
(*                                                         *)
(*  Erweiterte Printer-Unit mit Fehlerbehandlung und šber- *)
(*                setzung von Zeichen                      *)
(*                                                         *)
(***********************************************************)


UNIT printerx;

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O-,F+}
{$ENDIF }

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,winxp,keys,typeform,inout,maus2;

const drlength = 20;
      dnlength = 30;
      maxdd    = 75;   { Žnderung bei Driver 1.0 nicht m”glich }

type  perrfunc  = function:boolean;
      drstring  = string[drlength];
      dnstring  = string[dnlength];
      druck_par = record
                    case integer of
                      0 : (zahl       : integer);
                      1 : (name       : dnstring;
                           randtyp    : shortint;
                           dd         : array[0..maxdd] of drstring;
                           xlatger    : boolean);
                  end;
{ dd:

  0 : reset                       xlatger : šbersetzung Umlaute -> Epson
  1 : Rand                        randtyp : 0 = n Zeichen
  2 :
  3 : FF
  4 : step
  5 : NLQ/LQ an
  6 : NLQ/LQ aus
  7 : Zeilenabstand 1/6 "
  8 : Zeilenabstand 1/8 "
  9 : breit an
 10 : breit aus
 11 : Elite an         (96 cpl)
 12 : Elite aus
 13 : Schmal an        (136 cpl)
 14 : Schmal aus
 15 : Schmal/Elite an  (168 cpi)
 16 : Boldface an
 17 : Boldface aus
 18 : Emphasized an
 19 : Emphasized aus
 20 : Italics an
 21 : Italics aus
 22 : Superscript an
 23 : Superscript aus
 24 : Subscript an
 25 : Subscript aus
 26 : Unterstreichung an
 27 : Unterstrichung aus
}

var  checklst,xlatger : boolean;
     lst              : text;
     prterror         : perrfunc;

Procedure assignlst(var f:text; d:word);
Function  PrintString(s:string):string;


{ ================= Implementation-Teil ==================  }

implementation


type textbuf = array[0..126] of char;
     textrec = record
                 handle,mode,bufsize,
                 private,bufpos,bufend : word;
                 bufptr                : ^textbuf;
                 openfunc,inoutfunc,
                 flushfunc,closefunc   : pointer;
                 prport                : word;
                 userdata              : array[1..14] of byte;
                 name                  : array[0..79] of char;
                 case integer of
                   0 : (size : byte; buffer : textbuf);
                   1 : (ostr : string[127]);
               end;

{$IFDEF BP }
var
  oldexit: pointer;

Procedure xlate(var s:string; var bufpos:word);

const con1 : string[10] = '„”Ž™šá';
      con2 : string[10] = '{|}[\]~';

var   i,p  : byte;

begin
  s[0]:=chr(succ(bufpos));
  for i:=1 to length(s) do begin
    p:=pos(copy(s,i,1),con1);
    if p>0 then
      s[i]:=con2[p];
    end;
end;
{$ENDIF }

function prtorgerror:boolean;
var handle : word;
    c      : char;
    z      : taste;
begin
  wpull(22,59,9,14,'',handle);
  savecursor;
  inout.cursor(curoff);
  mwrt(25,11,'     Drucker nicht bereit !');
  mwrt(25,12,'(W)iederholen oder (A)bbrechen ?');
  inout.cursor(curoff);
  write(#7);
  repeat
    get(z,curoff);
    c:=UpCase(z[1]);
  until (c='W') or (c='A');
  wrest(handle);
  restcursor;
  prtorgerror:=(c='A');
end;

{$IFDEF BP }
function LstOutput(var f:textrec):integer;

var p       : integer;
    regs    : registers;

begin
  with f do begin
    if xlatger and checklst then xlate(ostr,bufpos);
    with regs do begin
      p:=0;
      while checklst and (p<bufpos) do begin
        ah:=2;
        dx:=prport;
        intr($17,regs);
        if (ah and $29)=0 then begin
          ah:=0;
          al:=byte(bufptr^[p]);
          dx:=prport;
          intr($17,regs);
          end;
        if (ah and $29)<>0 then
          if prterror then checklst:=false
          else
        else
{          if (ah and $80)=0 then }
            inc(p);
        end;
      bufpos:=0;
      end;
    end;
  lstoutput:=0;
end;


function lstignore(var f:textrec):integer;
begin
  lstignore:=0;
end;
{$ENDIF }


procedure assignlst(var f:text; d:word);
begin
{$IFDEF BP }
  with textrec(f) do begin
    handle:=$ffff;
    mode:=fmclosed;
    bufsize:=sizeof(buffer);
    bufptr:=@buffer;
    openfunc:=@lstignore;
    inoutfunc:=@lstoutput;
    flushfunc:=@lstoutput;
    closefunc:=@lstignore;
    prport:=d;
    name[0]:=#0;
  end;
{$ELSE }
  Assign(lst, 'lpt1' + Chr(Ord('1')+d));
  if IOResult = 0 then ;
{$ENDIF}
end;


{$IFDEF BP }
  {$S-}
  procedure newexit;
  begin
    exitproc:=oldexit;
    close(lst);
  end;
  {$IFDEF Debug }
    {$S+}
  {$ENDIF }
{$ENDIF }

{ ^X in Steuerzeichen umsetzen;  ^0 -> ^ }

Function PrintString(s:string):string;
var i,j,p : byte;
begin
  i:=1;
  j:=0;
  while i<=length(s) do begin
    inc(j);
    if s[i]='^' then begin
      inc(i);
      if s[i]='0' then
        printstring[j]:='^'
      else if s[i]='#' then
        printstring[j]:='#'
      else
        printstring[j]:=chr(ord(s[i])-64);
      end
    else if s[i]='#' then begin
      p:=i;
      while (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9') do
        inc(i);
      if i=p then printstring[j]:='#'
      else printstring[j]:=chr(minmax(ival(copy(s,p+1,i-p)),0,255));
      end
    else
      printstring[j]:=s[i];
    inc(i);
    end;
  printstring[0]:=chr(j);
end;


begin
{$IFDEF BP }
  assignlst(lst,0);
  rewrite(lst);
  oldexit:=exitproc;
  exitproc:=@newexit;
{$ENDIF}
  checklst:=true;
  xlatger:=false;
  prterror:=prtorgerror;
end.
{
  $Log$
  Revision 1.9  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/24 11:28:54  mk
  - 32 Bit: Drucken funktioniert jetzt

  Revision 1.7  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}

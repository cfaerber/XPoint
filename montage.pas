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
(*                      UNIT montage                       *)
(*                                                         *)
(*              Definition der Monats-Tage                 *)
(*                                                         *)
(***********************************************************)


UNIT montage;

{$I XPDEFINE.INC }
{$F+,O+}

interface

uses
  typeform, xpglobal;

const
       monat : Array[1..12] of record
                                 tag : String[9];
                                 zahl: Byte;
                               end  =

               ((tag: 'Januar';   zahl: 31), (tag: 'Februar';  zahl: 28),
                (tag: 'M„rz';     zahl: 31), (tag: 'April';    zahl: 30),
                (tag: 'Mai';      zahl: 31), (tag: 'Juni';     zahl: 30),
                (tag: 'Juli';     zahl: 31), (tag: 'August';   zahl: 31),
                (tag: 'September';zahl: 30), (tag: 'Oktober';  zahl: 31),
                (tag: 'November'; zahl: 30), (tag: 'Dezember'; zahl: 31));

type fdate = packed record
               t,m : byte;
               j   : smallword;
             end;

function  schaltj(jahr:integer):boolean;
function  ddow(dd:fdate):byte;
function  dow(const d:string):byte;           { Wochentag 1..7 }
function  dowst(const d:string):string;       { Wochentag Mo..So }

procedure schalt(jahr:integer);         { Schaltjahr-Korrektur }
procedure incd(var d);
procedure decd(var d);
function  prevd(const d:datetimest):datetimest;
function  nextd(const d:datetimest):datetimest;
function  sommer(const d:datetimest):boolean;   { Sommerzeit? }

implementation

function schaltj(jahr:integer):boolean;
begin
  schaltj:=((jahr mod 4)=0) xor
           ((jahr mod 100)=0) xor
           ((jahr mod 400)=0);
end;


procedure schalt(jahr:integer);
begin
  if schaltj(jahr) then monat[2].zahl:=29
  else monat[2].zahl:=28;
end;


procedure incd(var d);
begin
  with fdate(d) do begin
    if m>12 then m:=12
    else if m<1 then m:=1;
    schalt(j);
    inc(t);
    if t>monat[m].zahl then begin
      t:=1; inc(m);
      if m>12 then begin
        m:=1; inc(j);
        end;
      end;
    end;
end;


procedure decd(var d);
begin
  with fdate(d) do begin
    if m>12 then m:=12
    else if m<1 then m:=1;
    schalt(j);
    dec(t);
    if t=0 then begin
      dec(m);
      if m=0 then begin
        m:=12;
        dec(j);
        end;
      t:=monat[m].zahl;
      end;
    end;
end;


function prevd(const d:datetimest):datetimest;
var dat : fdate;
    res : integer;
begin
  with dat do begin
    val(left(d,2),t,res);
    val(copy(d,4,2),m,res);
    val(right(d,4),j,res);
    decd(dat);
    prevd:=formi(t,2)+'.'+formi(m,2)+'.'+formi(j,4);
    end;
end;


function nextd(const d:datetimest):datetimest;
var dat : fdate;
    res : integer;
begin
  with dat do begin
    val(left(d,2),t,res);
    val(copy(d,4,2),m,res);
    val(right(d,4),j,res);
    incd(dat);
    nextd:=formi(t,2)+'.'+formi(m,2)+'.'+formi(j,4);
    end;
end;


{ d ist im DateTime-Formst (s. TYPEFORM) }
{ 1=Mo, 2=Di, ..., 7=So }
{ Algorithmus zur Wochentagberechnung nach DOS 11/87, S. 86 }

function ddow(dd:fdate):byte;
var nt  : word;
begin
  with dd do
    if t+m+j > 0 then { MK 01/00 falls Record leer, wird 1 (Mo) zurckgegeben }
    begin
      if m<3 then begin
        m:=m+12; dec(j);
      end;
      nt:=(((t+(13*m+3)div 5+(5*j)shr 2-(j div 100)+j div 400)+1)mod 7);
      if nt=0 then ddow:=7
      else ddow:=nt;
    end else
      ddow := 1;
end;


function dow(const d:string):byte;
var dd  : fdate;
    res : integer;
begin
  val(copy(d,1,2),dd.t,res);
  val(copy(d,4,2),dd.m,res);
  val(copy(d,7,4),dd.j,res);
  dow:=ddow(dd);
end;


function dowst(const d:string):string;       { Wochentag Mo..So }
begin
  dowst:=copy('MoDiMiDoFrSaSo',dow(d)*2-1,2);
end;


{ letzter Sonntag im M„rz - letzter Sonntag im September }

function sommer(const d:datetimest):boolean;
var t,m : byte;
    res : integer;
begin
  val(copy(d,1,2),t,res);
  val(copy(d,4,2),m,res);
  sommer:=((m>3) and (m<9)) or ((m=3) and (t>=21)) or ((m=9) and (t<=23));
end;

end.
{
  $Log$
  Revision 1.5.2.2  2001/08/11 20:16:28  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.5.2.1  2001/07/01 15:42:12  my
  SV:- moved unit to overlay

  Revision 1.5  2000/04/30 15:54:21  mk
  - unbenutze globale Variable adow entfernt

  Revision 1.4  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.3  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

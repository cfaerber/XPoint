{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Datumsroutinen fr XP, MAGGI, ZFIDO }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$F+,O+}
{$ENDIF }

unit xpdatum;

interface

uses
{$IFDEF Linux }
  linux,
  sysutils,
{$ENDIF } 
  typeform,
  montage,
  xpglobal;

{$IFNDEF Linux }
const timezone      : string[7] = 'W+1';
{$ELSE }
{ Unix-Systeme haben detailierte Informationen ueber die Zeitzonen.
  Diese wird hier verwendet. Es sollte auch klappen, dass ein Zeitzonen-
  wechsel ohne manuelle Konfiguration beruecksichtigt wird. }
function TimeZone: string[7];
{$ENDIF }

procedure ZtoZCdatum(var datum,zdatum:string);
procedure ZCtoZdatum(var zdatum, datum:string);


implementation  { ---------------------------------------------------- }


procedure AddD(var datum:s20; hours:shortint);
var h,min  : integer;
    t,m,j  : integer;
    res    : integer;
begin
  if hours=0 then exit;
  val(copy(datum,7,2),h,res);
  inc(h,hours);
  if (h>=0) and (h<=23) then
    datum:=left(datum,6)+formi(h,2)+mid(datum,9)
  else begin
    val(left(datum,2),j,res);
    if j<70 then inc(j,2000)
    else inc(j,1900);
    val(copy(datum,3,2),m,res);
    val(copy(datum,5,2),t,res);
    val(copy(datum,9,2),min,res);
    if h<0 then begin
      inc(h,24); dec(t);
      if t=0 then begin
        dec(m);
        if m=0 then begin
          m:=12; dec(j);
          end;
        schalt(j);
        inc(t,monat[m].zahl);
        end;
      end
    else begin
      dec(h,24); inc(t);
      schalt(j);
      { MK+RB 01/00 Verhindert zugriff auf Bereiche hinter Array }
      if m < 1 then m := 1 else if m > 12 then m := 12;
      if t>monat[m].zahl then begin
        t:=1; inc(m);
        if m>12 then begin
          m:=1; inc(j);
          end;
        end;
      end;
    datum:=formi(j mod 100,2)+formi(m,2)+formi(t,2)+formi(h,2)+formi(min,2);
    end;
end;


procedure ZtoZCdatum(var datum,zdatum:string);
var addh : shortint;
    dat  : s20;
    p    : byte;
begin
  dat:=datum;
  p:=cpos(':',timezone);
  if p=0 then p:=length(timezone)+1;
  addh:=ival(copy(timezone,3,p-3));
  if timezone[2]='-' then addh:=-addh;
  AddD(dat,-addh);
  zdatum:=iifs(ival(left(datum,2))<70,'20','19')+dat+'00'+timezone;
end;

procedure ZCtoZdatum(var zdatum, datum:string);
var addh : shortint;
    dat  : s20;
    p    : byte;
begin
  dat:=copy(zdatum,3,10);
  p:=cpos(':',zdatum); if p<18 then p:=length(zdatum)+1;
  addh:=minmax(ival(copy(zdatum,17,p-17)),-13,13);
  if zdatum[16]='-' then addh:=-addh;
  AddD(dat,addh);
  datum:=dat;
end;

{$IFDEF Linux }
function TimeZone: string[7];
var
  tzBase, tzMinutes, tzHours: LongInt;
  isNegative: boolean;
  s: string[7];
begin
  { Abweichung in positiven Minuten darstellen }
  if (tzseconds < 0) then begin
    isNegative:= true;
    tzBase:= tzseconds div -60;
  end else begin
    isNegative:= false;
    tzBase:= tzseconds div 60;
  end;
  { Minuten sind der Rest von Stunden }
  tzMinutes:= tzBase mod 60;
  { Stunde hat 60 Minuten }
  tzHours:= tzBase div 60;
  
  if (tzdaylight) then
    s:= 'S'
  else
    s:= 'W';
  { Negativ-Abweichung zu UTC? }
  if (isNegative) then
    s:= s + '-';
  { Minuten? }
  if (tzMinutes <> 0) then begin
    s:= s + ':';
    if (tzMinutes < 10) then		{ Kenne zwar keine solche Zone, aber wer weiss }
      s:= s + '0';
    s:= s + IntToStr(tzMinutes);
  end;
  TimeZone:= s;
end;
{$ENDIF }

end.
{
  $Log$
  Revision 1.4  2000/05/02 20:27:54  hd
  - Dynamische Festlegung der Zeitzone unter Linux

  Revision 1.3  2000/04/13 12:48:39  mk
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
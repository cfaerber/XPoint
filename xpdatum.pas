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

{ Datumsroutinen fr XP, MAGGI, ZFIDO }

{$I XPDEFINE.INC }
{$F+,O+}

unit xpdatum;

interface

uses typeform, montage, xpglobal;

const timezone      : string[7] = 'W+1';

function getTZ(var tzone:string):boolean;
procedure ZtoZCdatum(var datum,zdatum:string);
procedure ZCtoZdatum(var zdatum, datum:string);


implementation  { ---------------------------------------------------- }

uses dos;


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


function schaltjahr(y:word):boolean;
begin
  schaltjahr:=((y and 3)=0) and (((y mod 100)<>0) or ((y mod 400)=0));
end;


function dayspermonth(year,month:word):word;
const dpm:array[1..12] of word=(31,28,31,30,31,30,31,31,30,31,30,31);
begin
  dayspermonth:=dpm[month];
  if month<>2 then exit
  else if schaltjahr(year) then dayspermonth:=29;
end;


{ folgende 2 Kalenderroutinen siehe c't 15/1997 S. 312 ff }

function tagesnummer(year,month,day:word):word;
var d,e:word;
begin
  d:=(month+10) div 13;
  e:=day+(611*(month+2)) div 20-2*d-91;
  tagesnummer:=e+ord(schaltjahr(year))*d;
end;


function wochentag(year,n:word):word; { So=0, Sa=6 }
var j,c:word;
begin
  j:=(year-1) mod 100;
  c:=(year-1) div 100;
  wochentag:=(28+j+n+(j div 4)+(c div 4)+5*c) mod 7;
end;


function getTZ(var tzone:string):boolean;
const secspermin=60;
      secsperhour=60*secspermin;
      secsperday=24*secsperhour;
var s,tz:string;
    tzdiff,diffs,diffw,zdiff,jetzt,szbeg,wzbeg:longint;
    i,j,mons,wos,tags,monw,wow,tagw:integer;
    hr,min,sec,yr,mon,day,dummy:rtlword;

  function pcount (const s: string): integer;
  var i, count: integer;
  begin
    count := 0;
    i := 1;
    repeat
      while (i <= length (s)) and (s [i] = ',') do inc (i);
      if i <= length (s) then inc (count);
      while (i <= length (s)) and (s [i] <> ',') do inc (i);
    until i > length (s);
    pcount := count;
  end;

  function pstr (const s: string; nr: integer): string;
  var i, count: integer;
  begin
    pstr := '';
    if nr = 0 then exit;
    if nr > pcount (s) then nr := 1;
    count := 0;
    i := 1;
    repeat
      while (i <= length (s)) and (s [i] = ',') do inc (i);
      if i <= length (s) then inc (count);
      if count = nr then break;
      while (i <= length (s)) and (s [i] <> ',') do inc (i);
    until i > length (s);
    if count = nr then begin
      count := i;
      while (i <= length (s)) and (s [i] <> ',') do inc (i);
      pstr := copy (s, count, i - count);
    end;
  end;

  function makeTZ(sw:char):string;
  var s:string;
  begin
    s:=strs(-tzdiff);
    if s[1]<>'-' then s:=sw+'+'+s else s:=sw+s;
    makeTZ:=s;
  end;

begin
  tzone:='W+0';
  getTZ:=false;
  tz:=trim(getenv('TZ'));
  if (tz='') or ((pcount(tz)<>1) and (pcount(tz)<>10)) then exit;
  s:=trim(pstr(tz,1));
  i:=1;
  while (i<=length(s)) and not (s[i] in ['+','-','0'..'9']) do inc (i);
  delete(s,1,i-1);
  i:=1;
  while (i<=length(s)) and (s[i] in ['+','-','0'..'9']) do inc (i);
  s:=left(s,i-1);
  tzdiff:=ival(s);
  if pcount(tz)=1 then begin
    tzone:=makeTZ('W');
    exit;
  end;
  mons:=ival(trim(pstr(tz,2)));
  wos:=ival(trim(pstr(tz,3)));
  tags:=ival(trim(pstr(tz,4)));
  diffs:=ival(trim(pstr(tz,5)));
  monw:=ival(trim(pstr(tz,6)));
  wow:=ival(trim(pstr(tz,7)));
  tagw:=ival(trim(pstr(tz,8)));
  diffw:=ival(trim(pstr(tz,9)));
  zdiff:=ival(trim(pstr(tz,10)));
  getdate(yr,mon,day,dummy);
  gettime(hr,min,sec,dummy);
  jetzt:=longint(tagesnummer(yr,mon,day))*secsperday
         +longint(hr)*secsperhour
         +min*secspermin
         +sec;
  if wos>0 then begin
    j:=0;
    for i:=1 to dayspermonth(yr,mons) do begin
      if wochentag(yr,tagesnummer(yr,mons,i))=tags then inc(j);
      if j=wos then begin
        szbeg:=longint(tagesnummer(yr,mons,i))*secsperday+diffs;
        break;
      end;
    end;
  end
  else if wos<0 then begin
    j:=0;
    for i:=dayspermonth(yr,mons) downto 1 do begin
      if wochentag(yr,tagesnummer(yr,mons,i))=tags then dec(j);
      if j=wos then begin
        szbeg:=longint(tagesnummer(yr,mons,i))*secsperday+diffs;
        break;
      end;
    end;
  end
  else szbeg:=longint(tagesnummer(yr,mons,tags))*secsperday+diffs; { wos=0 }
  if wow>0 then begin
    j:=0;
    for i:=1 to dayspermonth(yr,monw) do begin
      if wochentag(yr,tagesnummer(yr,monw,i))=tagw then inc(j);
      if j=wow then begin
        wzbeg:=longint(tagesnummer(yr,monw,i))*secsperday+diffw;
        break;
      end;
    end;
  end
  else if wow<0 then begin
    j:=0;
    for i:=dayspermonth(yr,monw) downto 1 do begin
      if wochentag(yr,tagesnummer(yr,monw,i))=tagw then dec(j);
      if j=wow then begin
        wzbeg:=longint(tagesnummer(yr,monw,i))*secsperday+diffw;
        break;
      end;
    end;
  end
  else wzbeg:=longint(tagesnummer(yr,monw,tagw))*secsperday+diffw;
  getTZ:=true;
  if (jetzt<szbeg) or (jetzt>=wzbeg) then tzone:=makeTZ('W') { Winterzeit }
  else begin { Sommerzeit }
    dec(tzdiff,(zdiff div 3600));
    tzone:=makeTZ('S');
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

end.

{
  $Log$
  Revision 1.7.2.1  2001/10/26 17:42:14  my
  MY+JG+RB:- Automatische Zeitzonenumstellung (Optionen 'manuell',
             'Datum', 'TZ-Var.', 'TZ/Datum). Details siehe Hilfe.

  Revision 1.7  2000/05/03 20:34:35  hd
  Noch ein Fluechtigkeitsfehler

  Revision 1.6  2000/05/03 19:41:27  hd
  - Noch eine Nachlaessigkeit in TimeZone

  Revision 1.5  2000/05/03 16:53:55  hd
  - Hours in TimeZone vergessen

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

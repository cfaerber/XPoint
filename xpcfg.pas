{ ---------------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                      }
{                                                                        }
{ Urversion: Hinrich Donner <hd@tiro.de>                                 }
{                                                                        }
{ (c) 2000 OpenXP Team, http://www.openxp.de                             }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.            }
{ ---------------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC }

unit XPCfg;

interface

uses
  xpglobal,
  sysutils;

{ Sektionen. Im Implementationsteil wird MySection definiert. Dieses muss
  fuer die einzelnen Betriebssysteme angepasst werden }
type
  TCfgSection = (               { Sektionen }
    csGlobal,                   { Allgemein }
    csColor,                    { Farben }
    csUser,                     { Benutzereinstellungen }
    csFKeys,                    { Funktionstasten }
    csZC,                       { Z-Netz }
    csRFC,                      { Rfc }
    csMaus,                     { Maus Tausch }
    csFido,                     { Fido }
    csDOS,                      { DOS }
    csLinux,                    { Linux }
    csOS2,                      { OS2 }
    csWin,                      { Windows }
    csUnknown,                  { Unbekannte Sektion }
    csNone                      { Undefiniert }
  );

var
  MySection: TCfgSection;               { Fuer die OS-abhaengigen. Wird in
                                          initvar (xp2cfg.inc) gesetzt }

{ Anmelden und Einlesen der cfg. Liefert true, wenn erfolgreich, false
  wenn ein Fehler aufgetreten ist. Die Datei muss waehrend der Laufzeit
  geoeffnet bleiben. Beim Schliessen werden alle Daten freigegeben!
  Die Schliessung erfolgt durch eine Exitprozedur. }
function OpenCfg(fn: string): boolean;

{ Schliessen der Cfg }
procedure CloseCfg;

{ Liest einen Eintrag einer Sektion }
function GetCfg(k: string; cs: TCfgSection): string;

{ Schreibt den Schluessel k und den Wert v in die Sektion cs.
  Wenn die Sektion nicht existiert, so wird sie erstellt. resid
  bezeichnet die Nummer einer Resource. Diese wird mit getres2 eingelesen.
  In der Unter-ID 0 steht dabei die Anzahl der Zeilen, die vor dieses
  Schlusselwort als Kommentar eingefuegt werden soll. }
procedure PutCfg(k, v: string; cs: TCfgSection; resid: integer);


implementation

uses
  fileio,
  resource,
{$IFDEF Linux}
  xplinux,
{$ENDIF}
  typeform;

type
  PCfgEntry = ^TCfgEntry;
  TCfgEntry = record
                key    : string;                { Schluesselwort }
                value  : string;                { Wert }
                sect   : TCfgSection;           { In welcher Sektion }
                fn     : string;                { Dateiname (fuer Include spater) }
                next   : PCfgEntry;             { Nachster Eintrag }
              end;

const
  isOpen: boolean = false;
  modified: boolean = false;
  SavedExit: pointer = nil;

  ResMain       = 31000;                        { Resourcen-Offsets }


var
  f: text;
  e: PCfgEntry;
  filename: string;


function Section2Str(cs: TCfgSection): string;
begin
  case cs of
    csGlobal: Section2Str:='GLOBAL';
    csColor:  Section2Str:='COLOR';
    csDos:    Section2Str:='DOS';
    csLinux:  Section2Str:='LINUX';
    csOs2:    Section2Str:='OS2';
    csWin:    Section2Str:='WIN';
    csZC:     Section2Str:='Z-NETZ';
    csRFC:    Section2Str:='RFC';
    csMaus:   Section2Str:='MAUS';
    csFido:   Section2Str:='FTN';
    csUser:   Section2Str:='USER';
    csFKeys:  Section2Str:='F-KEYS';
  else
    Section2Str:= 'UNKNOWN';
  end;
end;

function JumpSection(cs: TCfgSection): PCfgEntry;
var
  r: PCfgEntry;
  s1, s2: string;
begin
  r:= e;
  s1:= Section2Str(cs);
  while (r^.next<>nil) do begin
    r:= r^.next;                                { Im ersten Rec steht nichts }
    if (Length(r^.key)>0) and (r^.key[1]='[') then begin
      s2:= Copy(r^.key,2,Length(r^.key)-2);
      UpString(s2);
      if (s1=s2) then begin                     { Gefunden }
        JumpSection:= r;
        exit;
      end;
    end;
  end; { while }
  JumpSection:=nil;                             { Nicht vorhanden }
end;

function JumpKey(start: PCfgEntry; k: string): PCfgEntry;
var
  s: string;
begin
  UpString(k);
  start:= start^.next;                          { Da nach JumpSektion angesprungen... }
  while (start<>nil) and (start^.key[1]<>'[') do begin
    s:= start^.key;
    UpString(s);
    if (s=k) then begin                         { Gefunden }
      JumpKey:= start;
      exit;
    end;
    start:=start^.next;
  end;
  JumpKey:= nil;                                { Nichts vorhanden }
end;

function GetCfg(k: string; cs: TCfgSection): string;
var
  r: PCfgEntry;
begin
  r:= JumpSection(cs);                          { Sektion anspringen }
  if (r=nil) then                               { keine Sektion, kein Schluessel }
    GetCfg:= ''
  else begin
    r:= JumpKey(r, k);                          { Key anspringen }
    if (r=nil) then
      GetCfg:= ''                               { Nichts da }
    else
      GetCfg:= r^.value;                        { Kopieren }
  end;
end;

{ Speicher aufraeumen }
procedure DisposeConfig;
var
  e1,e2: PCfgEntry;
begin
  e1:= e;
  while (e1^.next<>nil) do begin
    e2:= e1^.next;
    freemem(e1, sizeof(TCfgEntry));
    e1:= e2;
  end;
  e:= nil;
end;

{ Datei schliessen, speicher aufraeumen, aber alles automatisch }
procedure CloseCfg;
var
  r: PCfgEntry;
begin
  if (SavedExit <> nil) then begin              { Exit-Verkettung }
    ExitProc:= SavedExit;
    SavedExit:= nil;
  end;
  if (modified) then begin                      { Aenderungen sichern }
    assign(f, filename);
    rewrite(f);
    if ioresult = 0 then ;
    r:= e;
    while (r^.next<>nil) do begin
      r:= r^.next;
      if (r^.key<>'') then begin
        if (r^.value<>'') then
          writeln(f,r^.key,'=',r^.value)
        else
          writeln(f,r^.key);
      end else if (r^.value<>'') then
        writeln(f,r^.value)
      else
        writeln(f);
    if ioresult = 0 then ;
    end;
    close(f);
    if ioresult = 0 then ;
{$IFDEF UnixFS}
    SetAccess(filename, taUserRW);
{$ENDIF}
    modified:= false;
  end;
  if (isOpen) then begin                        { Schliessen }
    DisposeConfig;
    isOpen:= false;
  end;
end;

{ Speicher allozieren und init }
function Alloc: PCfgEntry;
var
  r: PCfgEntry;
begin
  getmem(r, sizeof(TCfgEntry));
  if (r=nil) then begin
    writeln('No more memory XPCfg::Alloc');
    halt(2);
  end;
  fillchar(r^, sizeof(TCfgEntry), 0);
  Alloc:= r;
end;

procedure InitData(fn: string);
begin
  filename:= fn;
  e:= Alloc;
  modified:= false;
end;

{ Zerlegt den String anhand des Gleichheitszeichens in Key und Value.
  Wenn es ein Kommentar ist, so bleibt Key leer }
procedure ParseValue(s: string; var k, v: string);
var
  p: integer;
begin
  k:= '';                                       { Init }
  v:= TrimLeft(s);                              { Init, aber keine fuehrende Space }
  if (Length(v)=0) or (v[1]='#') then           { Kommentar? }
    exit;                                       { -> und wech }
  p:=cpos('=',v);                               { Trenner merken }
  if (p>0) then begin                           { -> Zerlegen }
    k:=copy(v,1,p-1);
    delete(v,1,p);
    trim(v); trim(k);
  end else begin                                { -> Alles Key (gibt es das schon? }
    k:=v;
    trim(k);
    v:='';
  end;
end;

{ MAcht aus 'Linux' 'csLinux' }
function Str2Section(s: string): TCfgSection;
begin
  UpString(s);
  if (s='GLOBAL') then Str2Section:= csGlobal
  else if (s='COLOR') then Str2Section:= csColor

  else if (s='Z-NETZ') then Str2Section:= csColor
  else if (s='RFC') then Str2Section:= csColor
  else if (s='MAUS') then Str2Section:= csColor
  else if (s='FTN') then Str2Section:= csColor

  else if (s='USER') then Str2Section:= csColor
  else if (s='F-KEYS') then Str2Section:= csColor

  else if (s='DOS') then Str2Section:= csDos
  else if (s='LINUX') then Str2Section:= csLinux
  else if (s='OS2') then Str2Section:= csOs2
  else if (s='WIN') then Str2Section:= csWin
  else Str2Section:= csUnknown;                 { Unbekannt }
end;

{ Wertet '[Sektion]' aus }
procedure SetSection(s: string; var cs: TCfgSection);
var
  p1, p2: integer;
begin
  if (Length(s)=0) then exit;
  p1:= cpos('[',s);
  if (p1=0) then exit;
  p2:= cpos(']',s);
  if (p2=0) then exit;
  inc(p1);
  if (p1=p2) then exit;                         { [] kenne ich nicht }
  cs:= Str2Section(Copy(s,p1,p2-p1));
end;

procedure MakeMainHeader;
var
  i, j: integer;
  r: PCfgEntry;
begin
  r:= e;
  j:= ival(getres2(ResMain,0));                 { Anzahl Zeilen lesen }
  for i:= 1 to j do begin
    r^.next:= Alloc;
    r:= r^.next;
    r^.value:= getres2(ResMain,i);
    if (Length(r^.value)<>0) then               { Leerzeilen ohne '#' }
      r^.value:= '# '+r^.value;
    r^.sect:=csNone;
    r^.fn:= filename;
  end;
  freeres;
  modified:= true;                              { Flag setzen }
end;

function MakeSection(cs: TCfgSection): PCfgEntry;
var
  r: PCfgEntry;
  i,j: integer;
  ro: word;
begin
  r:= e;
  while (r^.next<>nil) do                       { Ganz ans Ende }
    r:= r^.next;
  r^.next:= Alloc;
  r:= r^.next;
  r^.sect:= cs;
  r^.fn:= filename;
  case cs of
    csGlobal: begin r^.key:='[Global]'; ro:= ResMain+1; end;
    csColor:  begin r^.key:='[Color]';  ro:= ResMain+2; end;
    csDos:    begin r^.key:='[Dos]';    ro:= ResMain+3; end;
    csLinux:  begin r^.key:='[Linux]';  ro:= ResMain+4; end;
    csOs2:    begin r^.key:='[OS2]';    ro:= ResMain+5; end;
    csWin:    begin r^.key:='[Win]';    ro:= ResMAin+6; end;
    csZC:     begin r^.key:='[Z-Netz]'; ro:= ResMain+7; end;
    csRFC:    begin r^.key:='[RFC]';    ro:= ResMain+8; end;
    csMaus:   begin r^.key:='[Maus]';   ro:= ResMAin+9; end;
    csFido:   begin r^.key:='[FTN]';    ro:= ResMain+10; end;
    csUser:   begin r^.key:='[User]';   ro:= ResMain+11; end;
    csFKeys:  begin r^.key:='[F-Keys]'; ro:= ResMain+12; end;
  end;
  j:= ival(getres2(ro,0));
  for i:= 1 to j do begin               { Defaultbeschreibung aus der Res holen }
    r^.next:= Alloc;
    r:= r^.next;
    r^.value:=getres2(ro,i);
    if (LEngth(r^.value)<>0) then
      r^.value:= '# '+r^.value;
    r^.sect:=cs; r^.fn:= filename;
  end;
  freeres;
  MakeSection:= r;
end;

function AddKeyInSection(start: PCfgEntry): PCfgEntry;
var
  last: PCfgEntry;
begin
  while (start^.next<>nil) and (start^.key<>'[') do begin
    last:= start;
    start:= start^.next;
  end;
  if (start^.next=nil) then begin               { Kettenende }
    start^.next:= Alloc;
    start:= start^.next
  end else begin                                { Irgendwo mitten drin }
    start:= Alloc;                              { Neues Element }
    start^.next:= last^.next;                   { Kette verknueppen }
    last^.next:= start;
  end;
  AddKeyInSection:= start;
end;

{ Fuegt eine Resource an start ein und kehrt mit aktualisiertem Ptr zurueck }
function InsertResource(cs: TCfgSection; resid: integer; start: PCfgEntry): PCfgEntry;
var
  i,j: integer;
  r1, r2: PCfgEntry;
begin
  if (resid=0) then                             { Res angegeben? }
    InsertResource:= start
  else begin
    r1:= start; r2:= r1^.next;                  { r2 merkt sich die Kette }
    j:= ival(getres2(resid,0));
    for i:= 1 to j do begin                     { Alle Zeilen lesen }
      r1^.sect:= cs; r1^.fn:= filename;
      r1^.value:= getres2(resid,i);
      if (Length(r1^.value)<>0) then
        r1^.value:= '# '+r1^.value;
      r1^.next:= Alloc; r1:= r1^.next;
    end;
    r1^.next:=r2;                               { Kette verknuepfen }
    InsertResource:= r1;
    freeres;
  end;
end;

{ Schreibt v unter dem Schluessel k in der Sektion cs. Wenn kein
  Schluessel k existiert, wird er angelegt. Zusaetzlich werden die
  Eintraege der Resource Nr. resid als Kommentar eingefuegt.
  resid.0 enthaelt die Anzahl der Zeilen. }
procedure PutCfg(k, v: string; cs: TCfgSection; resid: integer);
var
  r, r2: PCfgEntry;
begin
  r:= JumpSection(cs);
  if (r=nil) then begin                         { Sektion existiert nicht }
    r:= MakeSection(cs);
    r^.next:= Alloc;
    r:= r^.next;
    r:= InsertResource(cs, resid, r);           { Resource einfuegen }
    r^.fn:= filename; r^.sect:= cs;
    r^.key:= k; r^.value:= v;
  end else begin
    r2:=JumpKey(r, k);                          { Zum Schluessle springen }
    if (r2=nil) then begin                              { Noch nicht vorhanden }
      r2:= AddKeyInSection(r);                  { Key in akt. Sektion anfuegen }
      r2:= InsertResource(cs, resid, r2);
    end;
    r2^.key:=k;
    r2^.value:= v;
    r2^.fn:= filename; r2^.sect:=cs;
  end;
  modified:= true;
end;

function OpenCfg(fn: string): boolean;
const
  cs: TCfgSection = csNone;
var
  s: string;
  ee: PCfgEntry;
  k,v: string;
begin
  OpenCfg:=false;
  if (isOpen) then Exit;                        { Das weiss einer nicht, was er tut }
  InitData(fn);
  ee:= e;
  if not (exist(fn)) then begin                 { Nur anlegen }
    isOpen:=true;
    SavedExit:= ExitProc;
    ExitProc:= @CloseCfg;
                                                { Defaults erzeugen }
    MakeMainHEader;                             { Dateikopf erzeugen }
  end else begin                                { -> Einlesen }
    assign(f,fn);
    reset(f);
    if (ioresult<>0) then exit;                 { Zugriffsfehler }
    while not eof(f) do begin
      readln(f,s);
      SetSection(s, cs);                        { Testen auf Sektionswechsel }
      ParseValue(s, k, v);                      { Aufsplitten }
      ee^.next:= Alloc;
      ee:= ee^.next;
      ee^.key:= k;
      ee^.value:= v;
      ee^.sect:= cs;
      ee^.fn:= fn;
    end;
    isOpen:= true;
    close(f);
    SavedExit:= ExitProc;
    ExitProc:= @CloseCfg;
  end;
  OpenCfg:= isOpen;
end;

end.
{
  $Log$
  Revision 1.4  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.3  2000/07/03 13:31:44  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.2  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.1  2000/05/14 09:54:58  hd
  - 3. Cfg-Datei


}

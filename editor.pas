{   $Id$

    OpenXP editor unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

{ OpenXP editor unit }
unit editor;

interface


uses
  keys, eddef, Lister;


const
      EdTempFile      = 'TED.TMP';  //todo: filenames
      EdConfigFile    = 'EDITOR.CFG';
      EdGlossaryFile  = 'GLOSSARY.CFG';
var
      EdSelcursor     : boolean = false; { Auswahllistencursor }
      OtherQuoteChars : boolean = false; { Andere Quotezeichen neben > }
      EditResetoldpos : boolean = false;

type
      charr    = array[0..65500] of char;
      charrp   = ^charr;
      EdToken = Byte;
      EdTProc = function(var t:taste):boolean;   { true = beenden }

var
      laststartline    : longint=0;      { fuer Z-Anzeige }
      lastscx          : integer=1;
      lastscy          : integer=1;      { Bildschirm (Cursor) }
      lastxoffset      : integer=0;


procedure EdInitDefaults(color:boolean);    { einmal bei Programmstart }
procedure EdSetScreenwidth(w:byte);         { globale Einstellungen }

function  EdInit(l,r,o,u:byte; rand:integer; savesoftbreaks:boolean;
                 NeuerAbsatzUmbruch:byte; iOtherQuoteChars:boolean):ECB;
function  EdLoadFile(ed:ECB; fn:string; sbreaks:boolean; umbruch:byte):boolean;
function  EdEdit(ed:ECB):EdToken;
function  EdSave(ed:ECB):boolean;
procedure EdExit(var ed:ECB);               { Release }

procedure EdSetTproc(ed:ECB; tp:EdTProc);   { lokale Einstellungen }
procedure EdGetProcs(var p:EdProcs);
procedure EdSetProcs(p:EdProcs);
procedure EdSetLanguage(ld:LangData);
procedure EdSetColors(col:EdColrec);
procedure EdSetForcecr(newcr:boolean);
procedure EdPointswitch(yuppieon:boolean);
procedure EdGetConfig(var cf:EdConfig);
procedure EdSetConfig(cf:EdConfig);
procedure EdSetUkonv(umlaute_konvertieren:boolean);
procedure EdAutoSave;

function  EdModified(ed:ECB):boolean;       { externer Zugriff }
function  EdFilename(ed:ECB):string;
procedure EdAddToken(ed:ECB; t:EdToken);


function  EddefQuitfunc(ed:ECB):taste;
function  EddefOverwrite(ed:ECB; fn:string):taste;
procedure EddefMsgproc(txt:string; error:boolean);
procedure EddefFileproc(ed:ECB; var fn:string; save,uuenc:boolean);
function  EddefFindFunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
function  EddefReplFunc(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
procedure Glossary_ed(LSelf: TLister; var t:taste); {Lister-Tastenabfrage fuer Glossary-Funktion }


implementation  { ------------------------------------------------ }

uses
  sysutils,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  osdepend,mouse,clip,
  typeform,fileio,inout,maus2,winxp,printerx, xp0, xp1, xp2, xpe, xp_uue, 
  xpglobal;

const maxgl     = 60;
      asize     = 16;                { sizeof(absatzt)-sizeof(absatzt.cont) }
      maxtokens = 128;
      maxabslen = 16363;

var   screenwidth : byte = 80;
      message   : string = '';

type
//    charr    = array[0..65500] of char;
//    charrp   = ^charr;

      absatzp  = ^absatzt;
      absatzt  = packed record
                   next,prev  : absatzp;
                   size,msize : smallword;       { msize = allokierte Groesse }
                   umbruch    : boolean;
                   fill       : array[1..3] of byte;
                   cont       : charr;
                 end;
      position = record
                   absatz     : absatzp;
                   offset     : integer;
                 end;
      edp      = ^EdData;
      EdData   = record                       { je aktivem Editorobjekt }
                   lastakted  : edp;
                   x,y,w,h,gl : byte;         { --- Startup }
                   edfile     : string;
                   showfile   : string[40];
                   savesoftbreak : boolean;      { beim Speichern }
                   tproc      : EdTProc;
                   Procs      : EdProcs;
                   root       : absatzp;
                   firstpar   : absatzp;      { --- akt.Pos.: 1. Absatz auf Schirm }
                   firstline  : integer;      { Zeile innerhalb dieses Absatzes }
                   startline  : longint;      { fuer Z-Anzeige }
                   scx,scy    : integer;      { Bildschirm (Cursor) }
                   xoffset    : integer;      { x-Anzeigeoffset }
                   col        : EdColRec;     { --- Daten/Status }
                   insertmode : boolean;
                   modified   : boolean;
                   rrand      : byte;         { rechter Umbruch-Rand }
                   tokenfifo  : array[0..maxtokens-1] of EdToken;  { --- Befehle }
                   tnextin    : byte;
                   tnextout   : byte;
                   absatzende : char;
                   lastpos    : position;     { fuer Ctrl-Q-P }

                   { disp: 1 = Markierung oberhalb Bildausschnitt, 2=in, 3=unterhalb }

                   block      : array[1..7] of record    { 3..7 = Marker }
                                                 pos  : position;
                                                 disp : Byte;
                                               end;
                   blockinverse : boolean;  { Endmarkierung vor Anfangsmark. }
                   blockhidden  : boolean;  { Blockmarkierung ausgeschaltet  }
                   na_umbruch   : Boolean;
                   forcecr      : boolean;  { CR am Textende beim Speichern }
                   pointswitch  : boolean;  { XPoint-Editor }
                   Config       : EdConfig;
                   ukonv        : boolean;
                   autosave     : boolean;
                 end;

      delnodep = ^delnode;
      delnode  = record
                   absatz : absatzp;
                   next   : delnodep;
                 end;

      modiproc = procedure(var data; Size: Integer);


var   Defaults : edp;
      language : ldataptr = nil;
      akted    : edp;
      delroot  : delnodep;         { Liste geloeschter Bloecke }
      ClipBoard: absatzp;
      NoCursorsave : boolean;
      ECBOpen   : integer;           { Semaphor fÅr Anzahl der offenen ECB's }


{ ------------------------------------------------ externe Routinen }

function SeekStr(var data; len: LongWord;
                 var s:string; igcase:boolean):integer; assembler; {&uses ebx, esi, edi}

  { -1 = nicht gefunden, sonst Position }
{$IFDEF ANALYSE}
begin
  //no asm
  Result := 0;
end;
{$ELSE}
asm
        jmp    @start
  @uppertab:   db    'Ä','ö','ê','É','é','Ö','è','Ä','à','â','ä','ã'
               db    'å','ç','é','è','ê','í','í','ì','ô'
  @start:
         mov    esi,data
         push   esi
         mov    edi,s
         mov    ecx,len
         mov    al,[edi]              { ax:=length(s) - < 127! }
         cbw
         inc   ecx
         sub   ecx,eax
         jbe   @nfound
         mov   dh,igcase

  @sblp1:
         xor   ebx,ebx                  { Suchpuffer- u. String-Offset }
         mov   dl,[edi]                { Key-Laenge }
  @sblp2:
         mov   al,[esi+ebx]
         or    dh,dh                   { ignore case (grosswandeln) ? }
         jz    @noupper
         cmp   al,'a'
         jb    @noupper
         cmp   al,'z'
         ja    @umtest
         and   al,0dfh
         jmp   @noupper                { kein Sonderzeichen }
  @umtest:
         cmp   al,128
         jb    @noupper
         cmp   al,148
         ja    @noupper
         push  ebx
         mov   ebx,offset @uppertab-128
{$IFNDEF Delphi } //!
         segcs
{$ENDIF }
         xlatb
         pop   ebx
  @noupper:
         cmp   al,[edi+ebx+1]
         jnz   @nextb
         inc   ebx
         dec   dl
         jz    @found
         jmp   @sblp2
  @nextb:
         inc   esi
         loop  @sblp1

  @nfound:
         pop   esi
         mov   eax,-1
         jmp   @sende
  @found:
         mov   eax,esi
         pop   esi
         sub   eax,esi
  @sende:
{$IFDEF FPC }
end ['EBX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }
{$ENDIF }


function FindUmbruch(var data; zlen:integer):integer; assembler; {&uses ebx, esi}
  { rueckwaerts von data[zlen] bis data[0] nach erster Umbruchstelle suchen }
asm
            mov   esi,data
            mov   ebx,zlen
            test  ebx, ebx
            jz    @ufound
  @floop:
            mov   al,[esi+ebx]
            cmp   al,' '               { ' ' -> unbedingter Umbruch }
            jz    @ufound

            cmp   al,'-'               { '-' -> Umbruch, falls alphanum. }
            jnz   @testslash           {        Zeichen folgt: }
            mov   al,[esi+ebx+1]
            cmp   al,'0'               { '0'..'9' }
            jb    @fnext
            cmp   al,'9'
            jbe   @ufound
            cmp   al,'A'               { 'A'..'Z' }
            jb    @fnext
            cmp   al,'Z'
            jbe   @ufound
            cmp   al,'a'               { 'a'..'z' }
            jb    @fnext
            cmp   al,'z'
            jbe   @ufound
            cmp   al,'Ä'               { 'Ä'..'•' }
            jb    @fnext
            cmp   al,'•'
            jbe   @ufound
            jmp   @fnext

  @testslash:
            cmp   ebx,1
            ja    @testslash2
            mov   ebx,0
            jmp   @ufound
  @testslash2:
            cmp   al,'/'               { '/' -> Umbruch, falls kein }
            jnz   @fnext               {        Trennzeichen vorausgeht }
            cmp   byte ptr [esi+ebx-1],' '
            jz    @fnext
            cmp   byte ptr [esi+ebx-1],'-'
            jnz   @ufound

  @fnext:
            dec   ebx
            jnz   @floop
  @ufound:
            mov   eax,ebx
{$IFDEF FPC }
end ['EAX', 'EBX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure FlipCase(var data; size: Integer);
var
  i: integer;
begin
  if size>0 then
    for i:=0 to size-1 do
      if UpCase(TCharArray(data)[i])=TCharArray(data)[i] then
        TCharArray(data)[i]:=LoCase(TCharArray(data)[i])
      else
        TCharArray(data)[i]:=UpCase(TCharArray(data)[i]);
end;


{ --------------------------------------------------- Einstellungen }

procedure errsound;
begin
  write(#7);
end;

function AskJN(ed:ECB; nr:byte; default:char):taste;
var t,tt : taste;
    txt  : string[80];
begin
  with edp(ed)^ do begin
    case nr of
      1 : txt:=language^.askquit;
      2 : txt:=language^.askoverwrite;
    end;
    attrtxt(col.colstatus);
    wrt(x,y,forms(txt,w));
    t:=default;
    repeat
      mwrt(x+length(txt),y,t);
      GotoXY(x+length(txt)-1, y);
      get(tt,curon);
      tt:=UpperCase(tt);
      if tt=#13 then t:=default
      else if tt=keyesc then t:=keyesc
      else if tt>' ' then t:=tt;
    until (tt=language^.ja) or (tt=language^.nein) or (tt=keyesc) or (tt=keycr);
    AskJN:=t;
    end;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

function EddefQuitfunc(ed:ECB):taste;
begin
  EddefQuitfunc:=AskJN(ed,1,language^.ja);
end;

function EddefOverwrite(ed:ECB; fn:string):taste;
begin
  EddefOverwrite:=AskJN(ed,2,language^.ja);
end;

function EddefFindFunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
begin
  errsound;
  EddefFindfunc:=false;
end;

function EddefReplFunc(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
begin
  errsound;
  EddefReplFunc:=false;
end;


procedure EddefMsgproc(txt:string; error:boolean);
begin
  message:=txt;
  errsound;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure EddefFileproc(ed:ECB; var fn:string; save,uuenc:boolean);
var brk : boolean;
    mf  : char;
begin
  with edp(ed)^ do begin
    attrtxt(col.colstatus);
    wrt(x,y,sp(w));
    fn:='';
    mf:=fchar; fchar:=' ';
    bd(x,y,'Block '+iifs(save,'speichern','laden')
           +iifs(uuenc,' und UU-kodieren','')
           +': ',fn,min(w-20,70),1,brk);
    fchar:=mf;
    if brk then fn:='';
    end;
end;


procedure EdInitDefaults(color:boolean);
var t : text;
    s : string;
    p : byte;
    i : integer;
begin
  new(Defaults);
  akted:=Defaults;
  fillchar(Defaults^,sizeof(Defaults^),0);
  with Defaults^ do begin
    with col do
      if color then begin
        coltext:=$7; colstatus:=$c; colmarked:=$17;
        colendmark:=3;
        for i:=1 to 9 do colquote[i]:=3;
        colmenu:=$71; colmenuhi:=$74; colmenuinv:=$17; colmenuhiinv:=$17;
        end
      else begin
        coltext:=7; colstatus:=$f; colmarked:=$70;
        colmenu:=$70; colmenuhi:=$f; colmenuinv:=7; colmenuhiinv:=7;
        end;
    insertmode:=true;
    Procs.QuitFunc:=EddefQuitfunc;
    Procs.Overwrite:=EddefOverwrite;
    Procs.MsgProc:=EddefMsgProc;
    Procs.FileProc:=EddefFileProc;
    Procs.FindFunc:=EddefFindFunc;
    Procs.ReplFunc:=EddefReplFunc;
    forcecr:=true;
    config.absatzendezeichen:='˙';
    config.rechter_rand:=74;
    config.AutoIndent:=true;
    config.PersistentBlocks:=true;
    config.QuoteReflow:=true;
    assign(t,EdConfigFile);
    if existf(t) then begin
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        LoString(s);
        p:=cpos('=',s);
        if p>0 then
          if LeftStr(s,p-1)='rechterrand' then
            config.rechter_rand:=ival(mid(s,p+1))
          else if LeftStr(s,p-1)='absatzende' then
            config.absatzendezeichen:=iifc(p<length(s),s[p+1],' ')
          else if LeftStr(s,p-1)='autoindent' then
            config.AutoIndent:=(mid(s,p+1)<>'n')
          else if LeftStr(s,p-1)='persistentblocks' then
            config.PersistentBlocks:=(mid(s,p+1)<>'n')
          else if LeftStr(s,p-1)='quotereflow' then
            config.QuoteReflow:=(mid(s,p+1)<>'n');
        end;
      close(t);
      end;
    end;
  new(language);
  with language^ do begin
    zeile:='Ze'; spalte:='Sp';
    ja:='J'; nein:='N';
    errors[1]:='zu wenig freier Speicher';
    errors[2]:='Absatz zu gross';
    errors[3]:='Fehler beim Laden des Textes';
    errors[4]:='Fehler beim Speichern';
    errors[5]:='Fehler: Datei nicht vorhanden';
    errors[6]:='Text wurde nicht gefunden.';
    askquit:='Text speichern (j/n) ';
    askoverwrite:='Datei existiert schon - ueberschreiben (j/n) ';
    askreplace:='Text ersetzen (Ja/Nein/Alle/Esc)';
    replacechr:='JNA';
    ersetzt:=' Textstellen ersetzt';
    drucken:='Drucken ...';
    menue[0]:='Block';
    menue[1]:='^Kopieren       *';
    menue[2]:='^Ausschneiden   -';
    menue[3]:='^Einfuegen       +';
    menue[4]:='^Laden        ^KR';
    menue[5]:='La^den UUE    ^KU';
    menue[6]:='^Speichern    ^KW';
    menue[7]:='-';
    menue[8]:='S^uchen       ^QF';
    menue[9]:='E^rsetzen     ^QL';
    menue[10]:='Weitersuchen   ^L';
    menue[11]:='-';
    menue[12]:='^Umbruch aus   F3';
    menue[13]:='U^mbruch ein   F4';
    menue[14]:='-';
    menue[15]:='^Optionen';
    menue[16]:='-';
    menue[17]:='Beenden       ESC';
  end;
  delroot:=nil;
  Clipboard:=nil;
end;

procedure EdSetLanguage(ld:LangData);
begin
  language^:=ld;
end;

procedure EdSetScreenwidth(w:byte);
begin
  screenwidth:=w;
end;

procedure EdSetColors(col:EdColrec);
begin
  akted^.col:=col;
end;

procedure EdGetProcs(var p:EdProcs);
begin
  p:=akted^.Procs;
end;

procedure EdSetProcs(p:EdProcs);
begin
  akted^.Procs:=p;
end;

procedure EdSetForcecr(newcr:boolean);
begin
  akted^.forcecr:=newcr;
end;

procedure EdPointswitch(yuppieon:boolean);
begin
  akted^.pointswitch:=yuppieon;
end;

procedure EdGetConfig(var cf:EdConfig);
begin
  cf:=akted^.config;
end;

procedure EdSetConfig(cf:EdConfig);
begin
  if akted <> nil then
    akted^.config:=cf;
end;

procedure EdSetUkonv(umlaute_konvertieren:boolean);
begin
  akted^.ukonv:=umlaute_konvertieren;
end;

procedure EdAutoSave;
begin
  akted^.autosave:=true;
end;


{ ------------------------------------------------ externer Zugriff }

function EdModified(ed:ECB):boolean;
begin
  EdModified:=edp(ed)^.modified;
end;

function EdFilename(ed:ECB):string;
begin
  EdFilename:=edp(ed)^.edfile;
end;

procedure EdAddToken(ed:ECB; t:EdToken);
var tnext : integer;
begin
  with edp(ed)^ do begin
    tnext:=tnextin+1;
    if tnext=maxtokens then tnext:=0;
    if tnext<>tnextout then begin
      tokenfifo[tnextin]:=t;
      tnextin:=tnext;
      end;
    end;
end;


{ ------------------------------- Liste geloeschter Bloecke verwalten }

procedure AddDelEntry(ap:absatzp);
var dnp : delnodep;
begin
  new(dnp);
  dnp^.absatz:=ap;
  dnp^.next:=delroot;
  delroot:=dnp;
end;

function GetDelEntry:absatzp;
var dnp : delnodep;
begin
  if delroot=nil then
    GetDelEntry:=nil
  else begin
    GetDelEntry:=delroot^.absatz;
    dnp:=delroot^.next;
    dispose(delroot);
    delroot:=dnp;
    end;
        end;

procedure freeblock(var ap:absatzp); forward;

procedure FreeDellist;             { Liste geloeschter Bloecke freigeben }
var ap : absatzp;
begin
  repeat
    ap:=GetDelEntry;
    freeblock(ap);
  until delroot=nil;
end;

{ -------------------------------------------------------- Speicher }

procedure error(nr:integer);
var txt : string[80];
begin
  txt:=language^.errors[nr];
  akted^.Procs.MsgProc(txt,true);
end;

function AllocAbsatz(size:integer):absatzp;
var
  ms : integer;
begin
  ms:=(size+15) and $fff0;        { auf 16 Bytes aufrunden }
  Getmem(Result, asize + ms);     // evtl. hier EOutOfMemory-Fehler abfangen
  Fillchar(Result^, asize, 0);    { next, prev implizit auf NIL setzen, Rest auf 0 }
  Result^.size:=size;
  Result^.msize:=ms;
  Result^.umbruch:=true;
end;

function freeabsatz(const p:absatzp): absatzp;
var p2: absatzp;
begin
  if assigned(p) then begin
    p2:=p;
    freemem(p2,asize+p^.msize);
    end;
  result:=nil;
end;

{ ------------------------------------------------------------ Edit }

{ Block freigeben }

procedure FreeBlock(var ap:absatzp);
var p : absatzp;
begin
  while assigned(ap) do begin   { Text freigeben }
    p:=ap^.next;
    freeabsatz(ap);
    ap:=p;
  end;
end;


{ sbreaks:  Softbreaks aufloesen                                     }
{ umbruch:  0 = alles ohne Umbruch laden                            }
{           1 = nur lange Zeilen ohne Softbreak ohne Umbruch laden  }
{           2 = alles mit Umbruch laden                             }

function LoadBlock(const fn:string; sbreaks:boolean; umbruch,rrand:byte):absatzp;
var mfm   : byte;
    s, s2 : string;
    t     : text;
    p     : absatzp;
    tail  : absatzp;
    sbrk  : boolean;
    root  : absatzp;
    endlf : boolean;          { LF am Zeilenende }
    endcr : boolean;          { CR am Dateiende }
    srest : boolean;
    pp    : byte;

  procedure AppP;
  begin
    if root=nil then begin
      root:=p; tail:=p;
      end
    else begin
      p^.prev:=tail;
      tail^.next:=p;
      tail:=p;
      end;
  end;

begin
  root:=nil;
  if Fileexists(fn) then
  begin
    mfm:=filemode; filemode:=0;
    assign(t,fn); reset(t);
    filemode:=mfm;
{$IFDEF FPC }
  {$IFDEF VER1_1 }
    p:= pointer(1);
  {$ELSE}
    p:=ptr(1,1);
  {$ENDIF }
{$ENDIF }
{$IFDEF Delphi }
    p := Pointer(1);
{$ENDIF }
    tail:=nil;
    srest:=false;
    endcr:=false; 
    while (srest or not eof(t)) and assigned(p) do
    begin
      s2 := '';
      sbrk:=false;
      endlf:=false;
      while (srest and (length(s2)=0) or not (eoln(t) or endlf)) do
      begin
        if not srest then
          read(t,s)
        else
          srest:=false;
        if s = '' then break;
        pp:=cpos(#10, s);
        if pp>0 then
        begin
          endlf:=(pp=length(s));
          s2 := s2 + Copy(s, 1, pp-1);
          Delete(s, 1, pp);
          srest:=true;
        end else
        begin
          if (length(s)>40) and sbreaks and eoln(t) and (s[length(s)]=' ')
             and not eof(t) then
          begin
            SetLength(s, Length(s)-1);
            sbrk:=true;
            readln(t);
          end;
          s2 := s2 + s;
        end;
      end;
      if eoln(t) and not srest then begin
        endcr:=not eof(t);
        readln(t);
      end;

      p:=AllocAbsatz(length(s2));
      if assigned(p) then begin
        p^.umbruch:=(rrand>0) and
                    ((umbruch=2) or
                     ((umbruch=1) and ((length(s2)<=rrand) or sbrk)));
        if length(s2)>0 then
          Move(s2[1],p^.cont,length(s2));
        AppP;
      end;
    end;
    Close(t);
    if endcr then
    begin
      p:=AllocAbsatz(0);
      p^.umbruch:=(umbruch<>0);
      AppP;
    end; 
    if ioresult<>0 then error(3);
  End;
  LoadBlock:=root;
end;

function LoadUUeBlock(fn:string):absatzp;
const blen = 45;
var mfm   : byte;
    s     : string;
    t     : file;
    p     : absatzp;
    tail  : absatzp;
    ibuf  : tbytestream;
    b_read: Integer;
    root  : absatzp;

  procedure AppP;
  begin
    if root=nil then begin
      root:=p; tail:=p;
    end
    else begin
      p^.prev:=tail;
      tail^.next:=p;
      tail:=p;
    end;
  end;

  procedure Absatz;
  begin
    p:=AllocAbsatz(length(s));
    if assigned(p) then begin
      p^.umbruch:=true;
      Move(s[1],p^.cont,length(s));
      AppP;
    end else
      raise EOutOfMemory.Create('in loadUUEblock'); //or what?
  end;

begin
  root:=nil;
  if Fileexists(fn) then
  begin
    mfm:=filemode; filemode:=0;
    assign(t,fn);  reset(t,1);
    filemode:=mfm;
{$IFDEF VP }
    p := ptr(1);
{$ELSE }
{$IFDEF FPC }
{$IFDEF VER1_1 }
    p:=pointer(1);
{$ELSE }
    p:=ptr(1,1);
{$ENDIF }
{$ENDIF }
{$ENDIF }
    tail:=nil;
    fn := ExtractFileName(fn);
    s:='begin 644 '+fn;
    while not eof(t) and assigned(p) do begin
      if s='' then begin
        blockread(t,ibuf,blen,b_read);
        s := encode_UU(ibuf,b_read);
      end;

      Absatz;
      s:='';

      if eof(t) then for b_read:=1 to 3 do begin
        if b_read=1 then s:='`'
        else if b_read=2 then s:='end'
        else if b_read=3 then str(filesize(t),s);
        Absatz;
      end;

    end;
    close(t);
    if ioresult<>0 then error(3);
  end;
  LoadUUeBlock:=root;
end;


function EdLoadFile(ed:ECB; fn:string; sbreaks:boolean; umbruch:byte):boolean;
begin
  with edp(ed)^ do
  begin
    edfile:=ExpandFilename(fn);
    showfile:='  '+fitpath(edfile,max(14,w-40));
    if assigned(root) then FreeBlock(root);
    EdLoadFile:=false;
    root:=LoadBlock(fn,sbreaks,umbruch,rrand);
    if root=nil then
      root:=AllocAbsatz(0);
    firstpar:=root; firstline:=1;     { Anzeigeposition setzen }
    scx:=1; scy:=1;
    block[1].pos.absatz:=nil;
    block[1].disp := 3;       { Anfangsmarkierung am Ende }
    block[2].pos.absatz:=root;
    block[2].disp := 1;       { Endmarkierung am Anfang }
    blockinverse:=true;
    end;
end;


{ NeuerAbsatzUmbruch:  0=nein, 1=Kopie, 2=ja }

function EdInit(l,r,o,u:byte; rand:integer; savesoftbreaks:boolean;
                NeuerAbsatzUmbruch:byte; iOtherQuoteChars:boolean):ECB;
var ed : edp;
begin
  new(ed);
  Move(Defaults^,ed^,sizeof(Defaults^));
  ed^.lastakted:=akted;
  akted:=ed;
  with ed^ do begin
    x:=l; w:=r-l+1;
    y:=o; h:=min(u-o+1,maxgl+1);
    gl:=h-1;
    if rand<>0 then rrand:=rand
    else rrand:=Config.rechter_rand;
    absatzende:=Config.absatzendezeichen;
    savesoftbreak:=savesoftbreaks;
    na_Umbruch:=(NeuerAbsatzUmbruch = 2);
    OtherQuoteChars:=iOtherQuoteChars;
    end;
  inc(ecbopen);
  EdInit:=ed;
end;

procedure EdSetTproc(ed:ECB; tp:EdTProc);
begin
  edp(ed)^.tproc:=tp;
end;

{ Positionszeiger in Absatz auf naechsten Zeilenbeginn bewegen }
{ Offset muss auf Zeilenanfang zeigen!                         }

function Advance(ap:absatzp; offset,rand:xpWord):integer;
var zlen : integer;   { Zeilenlaenge }
begin
  with ap^ do
    if not umbruch or (size-offset<=rand) then
      Advance:=size
    else
    begin
      zlen:=min(rand,size-offset-1);
      if (zlen=rand) and (cont[offset+zlen] in ['-','/']) then dec(zlen);
      zlen:=FindUmbruch(cont[offset],zlen);    { in EDITOR.ASM }
//     while (zlen>0) and not (cont[offset+zlen] in [' ','-','/']) do
//        dec(zlen);
      if zlen=0 then
        Advance:=offset+rand
      else
        Advance:=offset+zlen+1;
   end;
end;


{ Block von pstart bis pende in Datei schreiben }

function SaveBlock(pstart,pende:position; fn:string; rand:integer;
                   softbreak,overwrite,forcecr:boolean):boolean;
const crlf : string[2] = #13#10;
      spc  : string[3] = ' '#13#10;
var ap  : pointer;
    f   : file;
    ofs : integer;
    nxo : integer;
    ofs0,ofse : integer;
    cr  : boolean;
begin
  if overwrite then MakeBak(fn, ExtBak);
  assign(f,fn);
  if not overwrite then begin
    reset(f,1); seek(f,filesize(f)); end;
  if overwrite or (ioresult<>0) then
    rewrite(f,1);
  ap:=pstart.absatz;
  ofs0:=pstart.offset;
  ofse:=maxint;
  cr:=true;
  while assigned(ap) do begin
    if ap=pende.absatz then ofse:=pende.offset;
    with absatzp(ap)^ do
      if softbreak then begin
        ofs:=0;

        { Signaturtrenner beachten }
        if (size<>3) or (cont[0]<>'-') or (cont[1]<>'-') or (cont[2]<>' ') then
        { Signaturtrenner, nicht anfassen }
        while (size>0) and (cont[size-1]=' ') do dec(size);
        while (ofs<min(size,ofse)) do
        begin
          nxo:=Advance(ap,ofs,rand);
          blockwrite(f,cont[ofs],min(nxo,ofse)-ofs);
          if nxo<min(size,ofse) then
          begin
            blockwrite(f,spc[1],3); cr:=true;
          end else
            cr:=false;
          ofs:=nxo;
        end;
      end else
      begin
        blockwrite(f,cont[ofs0],min(size,ofse)-ofs0);
        cr:=false;
        ofs0:=0;
      end;
    if ap=pende.absatz then ap:=nil
    else ap:=absatzp(ap)^.next;
    if assigned(ap) and (ofse=maxint) then begin
      blockwrite(f,crlf[1],2); cr:=true;
      end;
    end;
  if not cr and forcecr then
    blockwrite(f,crlf[1],2);
  close(f);
  if ioresult<>0 then begin
    error(4);     { 'Fehler beim Speichern' }
    SaveBlock:=false;
    end
  else
    SaveBlock:=true;
end;


function EdSave(ed:ECB):boolean;
var p1,p2 : position;
begin
  EdSave:=false;
  with edp(ed)^ do begin
    p1.absatz:=root; p1.offset:=0;
    p2.absatz:=nil;  p2.offset:=maxint;
    if SaveBlock(p1,p2,edfile,rrand,savesoftbreak,true,forcecr) then begin
      modified:=false;
      EdSave:=true;
      end;
    end;
end;


var
  dispnoshow : boolean = false;
  lastop     : byte    = 0;         { 1=suchen, 2=suchen/ersetzen }

  txt   : string = '';
  repby : string = '';
  igcase: boolean = true;
  lastp : byte = 1;

function EdEdit(ed:ECB):EdToken;
type displist   = array[1..maxgl] of record
                                       absatz : absatzp;
                                       offset : integer;  { innerhalb des Abs. }
                                       zeile  : integer;
                                     end;
     displp     = ^displist;
var  dl         : displp;
     t          : taste;
     aufbau     : boolean;
     ende       : boolean;
     e          : edp;
     tk         : EdToken;
     trennzeich : set of char;     { fuer Wort links/rechts }
     ShiftBlockMarker: Integer;      

  procedure showstat;
  begin
    with e^ do begin
      attrtxt(col.colstatus);
      gotoxy(x,y);
      moff;
      Wrt2(' ' + language^.zeile + ' ' + forms(strs(startline+scy),7) +
                language^.spalte + ' ' + forms(strs(xoffset+scx),7));
      if xoffset=0 then
        Wrt2(sp(8))
      else
        Wrt2(forms('+'+strs(xoffset),8));
      if message='' then
      begin
        showfile[1]:=iifc(modified,'˛',' ');
        Wrt2(sp(w-wherex-length(showfile)) + showfile +' ');
      end else
      begin
        Wrt2(sp(w-wherex-length(message)) + message + ' ');
        message:='';
      end;
    mon;
    end;
  end;

  function GetPrefixChar(p:char; igcase:boolean):char;
  var t : taste;
  begin
    with e^ do begin
      attrtxt(col.colstatus);
      mwrt(x,y,'^'+p+'       ');
      gotoxy(x+2,y);
      get(t,curon);
      if igcase then
        GetPrefixChar:=iifc(t<' ',chr(ord(t[1])+64),UpCase(t[1]))
      else
        GetPrefixChar:=t[1];
      ShowStat;
      end;
  end;

  function alines(ap:absatzp):integer;     { # Zeilen eines Absatzes }
  var o,n : integer;
  begin
    if not ap^.umbruch then
      alines:=1
    else begin
      o:=0; n:=0;
      repeat
        o:=Advance(ap,o,e^.rrand);
        inc(n);
      until o=ap^.size;
      alines:=n;
      end;
  end;

  procedure display;
  const bemax = 16384;
  var i        : integer;
      ap       : absatzp;
      dofs,nxo : integer;
      s,s2     : string;
      line     : integer;
      absende  : boolean;
      acol     : byte;
      blockstat: byte;   { 0=kommt noch, 1=mittendrin, 2=vorbei }
      banfang,bende : integer;
      banf2,bende2  : integer;

    procedure SetAbsCol;
    var p,p0 : byte;
        s    : xpWord;
        qn   : integer;
        pdiff: integer;
    begin
      p0:=0;
      s:=ap^.size;
      while (p0<15) and (p0<s) and (ap^.cont[p0]<=' ') do inc(p0);
      p:=p0;
      qn:=0;
      repeat
        while (p-p0<6) and (p<s) and
        (
          (ap^.cont[p]<>'>') and
          (not OtherQuoteChars or not (ap^.cont[p] in QuoteCharSet))
        )
        do inc(p);
        pdiff:=p-p0;
        if (p<s) and
        (
          (ap^.cont[p]='>') or
          (OtherQuoteChars and (ap^.cont[p] in QuoteCharSet))
        )
        then begin
          inc(qn);
          p0:=p;
          end;
        inc(p);
      until (p>=s) or (pdiff=6);
      if qn<1 then acol:=e^.col.coltext
      else acol:=e^.col.colquote[min(qn,9)];
    end;

  begin
    with e^ do begin
      if blockinverse or blockhidden or (block[1].disp= 3) or (block[2].disp= 1)
      then
        blockstat:=2
      else if (block[1].disp= 1) and (block[2].disp>= 2) then blockstat:=1
      else blockstat:=0;
      banfang:=0; bende:=bemax;
      ap:=firstpar; dofs:=0;
      for i:=1 to firstline-1 do
        dofs:=Advance(ap,dofs,rrand);
      SetAbscol;
      i:=0;
      line:=firstline-1;
//      inc(windmax,$100);
      attrtxt(acol);
      if not dispnoshow then moff;
      repeat
        inc(i); inc(line);
        dl^[i].absatz:=ap;
        dl^[i].offset:=dofs;
        dl^[i].zeile:=line;
        nxo:=Advance(ap,dofs,rrand);
        absende:=(nxo=ap^.size);
        if not dispnoshow then begin
          if blockstat=0 then begin
            if (ap=block[1].pos.absatz) and (block[1].pos.offset<=nxo) then begin
              blockstat:=1;
              banfang:=block[1].pos.offset-dofs;
              if (ap=block[2].pos.absatz) and (nxo>=block[2].pos.offset) then
                bende:=block[2].pos.offset-dofs
              else
                bende:=bemax;
              end;
            end
          else if blockstat=1 then begin
            banfang:=0;
            if (ap=block[2].pos.absatz) and (nxo>=block[2].pos.offset) then
              bende:=block[2].pos.offset-dofs;
            end;
          SetLength(s, minmax(nxo-dofs-xoffset,0,w));
          if s<>'' then Move(ap^.cont[dofs+xoffset],s[1],length(s));
          if length(s)<w then
          begin
            if (s<>'') and absende then
              s:= s+absatzende;          { Absatzende-Marke }
            if length(s)<w then
              s := s + Sp(w-Length(s)); { mit Space auffuellen }
          end;
          attrtxt(acol);              { Zeile anzeigen }
          if blockstat<>1 then
            fwrt(x,y+i,s)
          else begin
            banf2:=minmax(banfang-xoffset+1,1,250);
            bende2:=minmax(bende-xoffset+1,banf2,255);
            s2:=LeftStr(s,banf2-1);
            fwrt(x,y+i,s2);
            attrtxt(col.colmarked);
            s2:=copy(s,banf2,max(0,bende2-banf2));
            fwrt(x+banf2-1,y+i,s2);
            attrtxt(acol);
            s2:=mid(s,bende2);
            fwrt(x+bende2-1,y+i,s2);
            end;
          if bende<bemax then blockstat:=2;
          end;
        if absende then begin
          ap:=ap^.next;
          if assigned(ap) then SetAbsCol;
          dofs:=0; line:=0;
          end
        else
          dofs:=nxo;
      until (i=gl) or (ap=nil);
      if i<gl then begin
        scy:=min(scy,i);
        fillchar(dl^[i+1],sizeof(dl^[1])*(gl-i),0);
        if not dispnoshow then begin
          if xoffset=0 then begin
            attrtxt(col.colendmark);
            wrt(x,y+i+1,#4);
            end;
          attrtxt(acol);
          wrt(x+1-sgn(xoffset),y+i+1,sp(w-1+sgn(xoffset)));
          (* wrt(x,y+i+1,forms(mid(#4{#4#4},xoffset+1),w)); *)
          inc(i);
          if i<gl then begin
            attrtxt(col.coltext);
            clwin(x,x+w-1,y+i+1,y+gl+1);
            end;
          end;
        end;
      if not dispnoshow then mon;
//      dec(windmax,$100);
      end;
    if dispnoshow then dispnoshow:=false
    else aufbau:=false;
  end;

  procedure NoDisplay;
  begin
    dispnoshow:=true;
    Display;
  end;

{ Offset der Cursorposition in dl^[scy].absatz; kann groesser }
{ als die Laenge des Absatzes sein!                          }

function WorkPos:integer;
begin
  with e^ do
    WorkPos:=dl^[scy].offset+xoffset+scx-1;
end;

function LineLength:integer;
begin
  with dl^[e^.scy] do
    LineLength:=Advance(absatz,offset,e^.rrand)-offset;
end;

function ActAbs:absatzp;
begin
  ActAbs:=dl^[e^.scy].absatz;
end;

procedure GetPosition(var p:position);
begin
  with e^ do begin
    p.absatz:=dl^[scy].absatz;
    p.offset:=dl^[scy].offset+xoffset+scx-1;
    end;
end;

function AbsDelete(const ap:absatzp; from,len:integer; delentry,bkorr:boolean):absatzp;
         forward;

procedure TruncAbs(aptr:absatzp);
var
  p: smallword;
begin
  with aptr^ do
  begin
    p:=size;
    { Signaturtrenner nicht erfassen }
    if (p=3) and (cont[0]='-') and (cont[1]='-') and (cont[2]=' ') then
    { Signaturtrenner, nicht anfassen }
    else begin
      while (p>0) and (cont[p-1]=' ') do
        dec(p);
      if p<size then
      begin
        AbsDelete(aptr,p,size-p,false,true);
        aufbau:=true;
      end;
    end;
  end;
end;

{ ---------------------------------------------- Blockkorrektur }

procedure CheckBlockOrder;    { e^.blockinverse setzen }
var b1l,b2l : integer;        { vorauss.: disp[x]=2 fuer mind. ein x }
begin
  with e^ do
    if block[1].disp<block[2].disp then blockinverse:=false
    else if block[1].disp>block[2].disp then blockinverse:=true
    else begin
      b1l:=1;
      while (b1l<=gl) and (dl^[b1l].absatz<>block[1].pos.absatz) do
        inc(b1l);
      b2l:=1;
      while (b2l<=gl) and (dl^[b2l].absatz<>block[2].pos.absatz) do
        inc(b2l);
      if b1l<b2l then blockinverse:=false
      else if b1l>b2l then blockinverse:=true
      else blockinverse:=(block[1].pos.offset>=block[2].pos.offset);
      end;
end;

procedure bskorr(n:byte; newdisp:byte);
var blp : byte;
begin
  with e^ do begin
    blp:=1;
    while (blp<=gl) and (dl^[blp].absatz<>block[n].pos.absatz) do
      inc(blp);
    if blp<=gl then
      block[n].disp:=2
    else
      if block[n].disp=2 then block[n].disp:=newdisp;
  end;
end;

procedure KorrBlockScrolled(up:boolean);   { up = Bild nach oben! }
begin                                  { Korrektur nach Scrolling }
  NoDisplay;
  bskorr(1,iif(up,1,3));
  bskorr(2,iif(up,1,3));
end;

{ Korrektur nach Loeschen innerhalb eines Absatzes }

function BlockAbsCut(oldabs,newabs:absatzp; from,len:integer):boolean;
var nxt,prv : absatzp;
    i       : byte;
begin
  BlockAbsCut:=false;
  with e^ do begin
    nxt:=oldabs^.next;
    prv:=oldabs^.prev;
    for i:=1 to 7 do
      with block[i] do
        if oldabs=pos.absatz then begin
          if assigned(newabs) then pos.absatz:=newabs
          else pos.absatz:=nxt;
          if (from-1)<pos.offset then begin
            pos.offset:=max(from,pos.offset-len);
            if (i=1) or (i=2) then BlockAbsCut:=true;
            if from>{=}oldabs^.size{-len} then
              case i of
                1,3..7 : if assigned(nxt) then begin
                           pos.absatz:=nxt; pos.offset:=0; end;
                2      : if assigned(prv) then begin
                           pos.absatz:=prv; pos.offset:=prv^.size; end;
              end;
            end;
          end;
    end;
end;

{ Korrektur nach Einfuegen innerhalb eines Absatzes }

procedure BlockAbsInsert(oldabs,newabs:absatzp; from,len:integer);
var i : integer;
begin
  with e^ do
    for i:=1 to 7 do
      with block[i] do
        if oldabs=pos.absatz then begin
          pos.absatz:=newabs;
          if from<pos.offset then inc(pos.offset,len);
          end;
end;

{ Korrektur nach Aufspalten eines Absatzes }

procedure BlockAbsSplit(old,new1,new2:absatzp; split,ins:integer);
var i : integer;
begin
  with e^ do
    for i:=1 to 7 do
      with block[i] do
        if old=pos.absatz then
          if pos.offset<split then
            pos.absatz:=new1
          else begin
            pos.absatz:=new2;
            pos.offset:=pos.offset-split+ins;
          end;
end;


{ ---------------------------------------------- Cursor bewegen }

procedure Zeilenanfang;
begin
  with e^ do begin
    scx:=1;
    if xoffset>0 then begin
      xoffset:=0; aufbau:=true;
      end
    end;
end;

procedure Zeilenende;
begin
  with e^ do begin
    scx:=Advance(ActAbs,dl^[scy].offset,rrand)-dl^[scy].offset-xoffset+1;
    if dl^[scy].zeile<alines(ActAbs) then dec(scx);
    if scx>w then begin
      inc(xoffset,scx-w); scx:=w;
      aufbau:=true;
      end
    else if scx<1 then begin
      if LineLength=0 then begin     { End-Taste in Leerzeile in Spalte >80 }
        scx:=1; xoffset:=0;
        end
      else begin
        scx:=2;
        xoffset:=LineLength-1;
        end;
      aufbau:=true;
      end;
    end;
end;

procedure SeiteOben(korrblock:boolean);
var i  : integer;
    ap : absatzp;
begin
  TruncAbs(ActAbs);
  with e^ do
    if (dl^[1].zeile=1) and (dl^[1].absatz^.prev=nil) then
      scy:=1
    else begin
      ap:=dl^[1].absatz;
      i:=gl-dl^[1].zeile;
      while (i>0) and assigned(ap^.prev) do begin
        ap:=ap^.prev;
        dec(i,alines(ap));
        end;
      dec(startline,gl-max(0,i)-1);
      firstpar:=ap;
      firstline:=max(1,1-i);
      if korrblock then KorrBlockScrolled(false);
      aufbau:=true;
      end;
end;

procedure Seitenende; forward;
procedure SeiteUnten;
var i : integer;
begin
  TruncAbs(ActAbs);
  with e^ do begin
    if (dl^[gl].absatz=nil) or
       ((dl^[gl].absatz^.next=nil) and (dl^[gl].zeile=alines(dl^[gl].absatz)))
    then Seitenende  { Wenn Ende des Textes bereits auf dem Bildschirm ist... }
    else begin
      i:=gl;
      while dl^[i].absatz=nil do dec(i);
      inc(startline,i-1);
      firstpar:=dl^[i].absatz;
      firstline:=dl^[i].zeile;
      KorrBlockScrolled(true);
      aufbau:=true;
      end;
    end;
end;

function ScrollUp:boolean;
begin
  with e^ do
    if assigned(dl^[2].absatz) then begin
      if scy=1 then TruncAbs(ActAbs);
      firstpar:=dl^[2].absatz;
      firstline:=dl^[2].zeile;
      inc(startline);
      KorrBlockScrolled(true);
      aufbau:=true;
      ScrollUp:=true;
      end
    else
      ScrollUp:=false;
end;

function ScrollDown:boolean;
var ap : pointer;
begin
  ScrollDown:=true;
  with e^ do begin
    if scy=gl then TruncAbs(ActAbs);
    if dl^[1].zeile>1 then begin
      dec(firstline); dec(startline);
      aufbau:=true;
      end
    else begin
      ap:=dl^[1].absatz^.prev;
      if assigned(ap) then begin
        firstpar:=ap;
        firstline:=alines(ap);
        dec(startline);
        KorrBlockScrolled(false);
        aufbau:=true;
        end
      else
        ScrollDown:=false;
      end;
    end;
end;

function ZeileOben:boolean;
begin
  ZeileOben:=true;
  TruncAbs(ActAbs);
  with e^ do
    if scy>1 then dec(scy)
    else ZeileOben:=ScrollDown;
end;

function ZeileUnten:boolean;
begin
  ZeileUnten:=true;
  with e^ do begin
    if Advance(ActAbs,dl^[scy].offset,rrand)>=ActAbs^.size then
      TruncAbs(ActAbs);    { bei Absatzwechsel Leerzeichen am Ende abschneiden }
    if scy=gl then ZeileUnten:=ScrollUp
    else
      if assigned(dl^[scy+1].absatz) then inc(scy)
      else ZeileUnten:=false;
    end;
end;

procedure Scroll_Up;
begin
  if ScrollUp then
    if e^.scy>1 then
      ZeileOben;
end;

procedure Scroll_Down;
begin
  if ScrollDown then
    if e^.scy<e^.gl then
      ZeileUnten;
end;


function ZeichenLinks:boolean;
begin
  ZeichenLinks:=true;
  with e^ do
    if scx>1 then dec(scx)
    else if xoffset>0 then begin
      dec(xoffset); aufbau:=true; end
    else
      if ZeileOben then Zeilenende
      else ZeichenLinks:=false;
end;

procedure ZeichenRechts(overline:boolean);
var ll,ladd : integer;
begin
  with e^ do begin
    ll:=LineLength;
    ladd:=iif(dl^[scy].offset+ll>=ActAbs^.size,1,0);
    if scx+xoffset<iif(overline,maxabslen+1,LineLength+ladd) then begin
      if scx<w then inc(scx)
      else begin
        inc(xoffset); aufbau:=true; end;
      end
    else if not overline then
      if ZeileUnten then
        Zeilenanfang;
    end;
end;

procedure CondZeichenRechts;
begin
  with e^ do
    ZeichenRechts(not ActAbs^.umbruch or
                  (dl^[scy].zeile=alines(ActAbs)));
end;

procedure Seitenanfang;
begin
  TruncAbs(ActAbs);
  e^.scy:=1;
end;

procedure Seitenende;
begin
  TruncAbs(ActAbs);
  with e^ do
    while (scy<gl) and assigned(dl^[scy+1].absatz) do
      inc(scy);
end;

procedure Textanfang;
begin
  TruncAbs(ActAbs);
  with e^ do begin
    firstpar:=root;
    firstline:=1;
    startline:=0;
    scy:=1;
    xoffset:=0; scx:=1;
    KorrBlockScrolled(false);
    if block[1].disp<>2 then block[1].disp:=3;
    if block[2].disp<>2 then block[2].disp:=3;
    aufbau:=true;
    end;
end;

procedure Textende;
var ap,p : absatzp;
begin
  TruncAbs(ActAbs);
  with e^ do
    if (dl^[gl].absatz=nil) or
       ((dl^[gl].absatz^.next=nil) and (dl^[gl].zeile=alines(dl^[gl].absatz)))
    then
      Seitenende
    else begin
      ap:=dl^[1].absatz;
      p:=ap^.next;
      while assigned(p) do begin
        inc(startline,alines(ap));
        ap:=p;
        p:=ap^.next;
        end;
      inc(startline,alines(ap)-1);
      firstpar:=ap;
      firstline:=alines(firstpar);
      KorrBlockScrolled(true);
      if block[1].disp<>2 then block[1].disp:=1;
      if block[2].disp<>2 then block[2].disp:=1;
      SeiteOben(true);
      scy:=gl;
      end;
  e^.xoffset:=0;
  NoDisplay;
  Zeilenende;
  aufbau:=true;
end;

function IsWhitespace:boolean;
var wo  : integer;
    abs : absatzp;
begin
  with e^ do begin
    wo:=WorkPos;
    abs:=ActAbs;
    IsWhitespace:=(wo>=abs^.size) or (abs^.cont[wo] in trennzeich);
    end;
end;

procedure WortLinks;
  function LStop:boolean;
  begin
    with e^ do
      LStop:=(scx+xoffset+scy+startline=2);
  end;
begin
  if ZeichenLinks then begin
    while IsWhitespace and not LStop do begin
      if ZeichenLinks then; if aufbau then NoDisplay; end;
    while not IsWhitespace and not LStop do begin
      if ZeichenLinks then; if aufbau then NoDisplay; end;
    if IsWhitespace then ZeichenRechts(false);
    end;
end;

procedure WortRechts;
  function RStop:boolean;
  begin
    with e^ do
      RStop:=(ActAbs^.next=nil) and (workpos>=ActAbs^.size);
  end;
begin
  while not IsWhitespace and not RStop do begin
    ZeichenRechts(false); if aufbau then NoDisplay; end;
  while IsWhitespace and not RStop do begin
    ZeichenRechts(false); if aufbau then NoDisplay; end;
end;

procedure GotoPos(p:position; SearchBackward: Boolean);    { beliebige Position anspringen }
var p0      : position;
    i,n,add : integer;
    ap      : absatzp;
    l       : longint;
    b1,b2   : absatzp;
    touch1  : boolean;   { erste Blockmarkierung gesehen ... }
    touch2  : boolean;   { zweite Blockmarkierung gesehen    }
begin
  if not Assigned(p.absatz) then exit;
  TruncAbs(ActAbs);
  NoDisplay;
  with e^ do 
  begin
    GetPosition(p0);
    i:=1;
    while (i<=gl) and (dl^[i].absatz<>p.absatz) do inc(i);
    if i<=gl then begin       { Absatz ist noch auf Bildschirm }
      if i=1 then
        while dl^[1].offset>p.offset do begin
          if ScrollDown then; NoDisplay;
          end;
      n:=dl^[i].zeile;
      while (n<alines(dl^[i].absatz)) and
            (Advance(dl^[i].absatz,dl^[i].offset,rrand)<=p.offset) do begin
        if i<gl then inc(i)
        else begin if ScrollUp then; NoDisplay; end;
        inc(n);
        end;
      scy:=i;
      end
    else 
    begin
      b1:=block[1].pos.absatz;
      b2:=block[2].pos.absatz;
      touch1:=false; 
      touch2:=false;
      l := 0; // fix false warning

      if not SearchBackward then
        ap:=nil
      else 
      begin
        ap:=dl^[1].absatz; l:=dl^[1].zeile-1;    { Abs. rueckwaerts suchen }
        while (ap<>p.absatz) and assigned(ap) do 
        begin
          ap:=ap^.prev;
          if ap=b1 then touch1:=true;
          if ap=b2 then touch2:=true;
          if assigned(ap) then inc(l,alines(ap));
        end;
      end;

      if assigned(ap) then 
      begin
        firstpar:=ap; firstline:=alines(ap); scy:=1;
        dec(startline,l);
        if touch1 then block[1].disp:=2;
        if touch2 then block[2].disp:=2;
        KorrBlockScrolled(false);
        NoDisplay;
      end
      else
        if (dl^[gl].absatz=nil) or (dl^[gl].absatz^.next=nil) then
          exit
      else 
      begin                                  { .. vorwaerts suchen }
        l:=alines(dl^[gl].absatz)-dl^[gl].zeile;
        ap:=dl^[gl].absatz^.next;
        touch1:=false; touch2:=false;
        while (ap<>p.absatz) and assigned(ap) do 
        begin
          inc(l,alines(ap));
          ap:=ap^.next;
          if ap=b1 then touch1:=true;
          if ap=b2 then touch2:=true;
        end;
        if ap=nil then exit;
        i:=Advance(ap,0,rrand); n:=1; add:=1;
        while (i<=p.offset) and (add<>0) do begin    { Zeile suchen }
          inc(l);
          add:=Advance(ap,i,rrand)-i;
          if add<>0 then inc(n);
          inc(i,add);
          end;
        inc(startline,l+gl-1);
        dl^[1].absatz:=ap; dl^[1].zeile:=n;
        SeiteOben(false);
        if touch1 then block[1].disp:=2;
        if touch2 then block[2].disp:=2;
        KorrBlockScrolled(true);
        NoDisplay;
        scy:=gl;
        end;
      end;
    scx:=p.offset-dl^[scy].offset+1-xoffset;
    if scx>w then begin
      xoffset:=scx-w; scx:=w; end
    else if scx<1 then begin
      inc(xoffset,(scx-1));
      scx:=1;
      end;
    lastpos:=p0;
    aufbau:=true;
    end;
end;

procedure SetMarker(n:byte);
begin
  GetPosition(e^.block[n+2].pos);
end;

procedure GotoMarker(n:byte);
begin
  GotoPos(e^.block[n+2].pos, drBoth);
end;


{ -------------------------------------------------- Schalter }

procedure SetAbsatzmarke;
begin
  with e^ do
    if absatzende=' ' then absatzende:=Config.absatzendezeichen
    else absatzende:=' ';
  aufbau:=true;
end;


function InBlock:boolean; forward;

procedure UmbruchEin;
var ap    : absatzp;
    fpmet : boolean;
    wpos  : position;

  procedure uein;
  begin
    if not ap^.umbruch then begin
      if ap=e^.firstpar then fpmet:=true;
      ap^.umbruch:=true;
      if not fpmet then
        inc(e^.startline,alines(ap)-1);
      end;
  end;

begin
  with e^ do begin
    wpos.absatz:=ActAbs;
    wpos.offset:=min(workpos,ActAbs^.size);
    if blockinverse or blockhidden or not InBlock then
      ActAbs^.umbruch:=true
    else begin
      fpmet:=false;
      ap:=block[1].pos.absatz;
      uein;
      while ap<>block[2].pos.absatz do begin
        ap:=ap^.next;
        uein;
        end;
      end;
    NoDisplay;
    while (scy<=gl) and (dl^[scy].absatz<>wpos.absatz) do inc(scy);
    if dl^[scy].absatz<>wpos.absatz then
       GotoPos(wpos, drBoth)
    else
      while (scy<gl) and (Advance(wpos.absatz,dl^[scy].offset,rrand)<wpos.offset) do
        inc(scy);
    end;
  Zeilenanfang;
  KorrBlockScrolled(false);
  aufbau:=true;
end;


procedure UmbruchAus;
var ap,da : absatzp;
    fpmet : boolean;
    ulines: integer;

  procedure uoff(ap:absatzp);
  begin
    if ap^.umbruch then begin
      ulines:=alines(ap)-1;
      ap^.umbruch:=false;
      end
    else
      ulines:=0;
    with e^ do
      if ap=firstpar then begin
        dec(startline,dl^[1].zeile-1);
        firstline:=1;
        fpmet:=true;
        end;
  end;

begin
  with e^ do
    if blockinverse or blockhidden or not InBlock then
      uoff(ActAbs)
    else begin
      fpmet:=false;
      da:=ActAbs;
      ap:=block[1].pos.absatz;
      uoff(ap);
      while ap<>block[2].pos.absatz do begin
        if not fpmet then dec(startline,ulines);
        ap:=ap^.next;
        uoff(ap);
        end;
      NoDisplay;
      while (scy>1) and (ActAbs<>da) do dec(scy);
      end;
  KorrBlockScrolled(true);
  aufbau:=true;
end;


{ ---------------------------------------------- Text editieren }

function EndSpaces:integer;    { Abstand zwischen Absatzende + Cursor }
begin
  EndSpaces:=max(0,workpos-dl^[e^.scy].absatz^.size);
end;

procedure copyflags(abs1,abs2:absatzp);
begin
  abs2^.umbruch:=abs1^.umbruch;
end;

procedure CorrectWorkpos;      { Position innerhalb Umbruchabsatz korr. }
begin
  with e^ do
    if (EndSpaces=0) and (scx+xoffset>LineLength) then
      ZeilenEnde;
end;

procedure absatzwechsel(old,anew:absatzp; setpointer:boolean);
begin
  with e^ do begin
    if setpointer then begin
      if assigned(old^.prev) then begin
        anew^.prev:=old^.prev;
        old^.prev^.next:=anew;
        end;
      if assigned(old^.next) then begin
        anew^.next:=old^.next;
        old^.next^.prev:=anew;
        end;
      end;
    if root=old then root:=anew;
    if firstpar=old then firstpar:=anew;
    end;
end;

procedure CreateCopy(ap1:absatzp; var ap2:absatzp; from,len:integer);
begin
  ap2:= AllocAbsatz(len);
  Copyflags(ap1, ap2);
  if len > 0 then
    Move(ap1^.cont[from],ap2^.cont,len);
end;

function AbsDelete(const ap:absatzp; from,len:integer; delentry,bkorr:boolean):absatzp;
var apnew : absatzp;
begin
  result:=ap;
  if len>0 then
  begin
    if delentry then
    begin
      apnew:=AllocAbsatz(len);
      Move(ap^.cont[from],apnew^.cont,len);
      AddDelEntry(apnew);
    end;
    if ap^.size-len>=ap^.msize-15 then begin
      Move(ap^.cont[from+len],ap^.cont[from],ap^.size-from-len);
      dec(ap^.size,len);
      if blockabscut(ap,ap,from,len) then
        CheckBlockOrder;
    end
    else begin
      apnew:=AllocAbsatz(ap^.size-len);
      if assigned(apnew) then begin
        copyflags(ap,apnew);
        absatzwechsel(ap,apnew,true);
        Move(ap^.cont[0],apnew^.cont[0],from);
        Move(ap^.cont[from+len],apnew^.cont[from],ap^.size-from-len);
        if blockabscut(ap,apnew,from,len) then begin
          NoDisplay;
          CheckBlockOrder;
        end;
        freeabsatz(ap);
        result:=apnew;
      end;
    end;
    if bkorr then KorrBlockScrolled(true);
    e^.modified:=true;
  end;
end;

procedure moveworkpos(newwp:integer; wpa:absatzp);
begin
  NoDisplay;
  if assigned(wpa) then begin
    if ActAbs<>wpa then TruncAbs(ActAbs);
    while (ActAbs<>wpa) and ZeichenLinks do
      if aufbau then Display;
    end;
  with e^ do
    if (scx+xoffset>linelength) and (linelength>1) then begin
      while (dl^[scy].offset>=ActAbs^.size) and ZeileOben do NoDisplay;
      ZeilenEnde;
      end;
  while workpos>newwp do
    if ZeichenLinks then if aufbau then Display;
  while workpos<newwp do begin
    CondZeichenRechts;
    if aufbau then Display;
    end;
end;

{ see GetQChar in xp3ex.pas }
{ see GetQChar in xp3ex.pas }
function GetQuote(ap:absatzp):string;
var
  p,q : Integer;
begin
  if e^.Config.QuoteReflow and Assigned(ap) then
  begin
    p:= ap^.size;
    SetString(Result, ap^.cont, p);

    p:=cpos('>', Result);
    if p > 5 then
      p:=0
    else
      if p>0 then
      begin
        repeat        { korrektes Ende des (mehrfach-?)Quotezeichens }
          q:=p+1;     { ermitteln                                    }
          while (q<=length(Result)) and (q-p<=4) and (Result[q]<>'>') do
            inc(q);
          if (q<=length(Result)) and (Result[q]='>') then p:=q;
        until q>p;
        while (p<length(Result)) and (Result[p+1]='>') do inc(p);
        while (p<length(Result)) and (Result[p+1]=' ') do inc(p);
      end;
    Result := LeftStr(Result, p)
  end else
    Result := '';
end;

function DelQuoteAbs(ap: AbsatzP; Quote1, Quote2: String): Boolean;
begin
  if (Length(Quote1)>0) and (Trim(Quote1)=Trim(Quote2)) then
  begin
    AbsDelete(ap, 0, Length(Quote2), false, true);
    Result := true;
  end else
    Result := false;
end;

function CountSpaces(ap: AbsatzP): Integer;
begin
  Result := 0;
  if not e^.Config.AutoIndent then Exit;
  while (Result < ap^.size) and (ap^.cont[Result]=' ') do
    inc(Result);
end;

function BreakBlock(ap: AbsatzP; wp, spaces, QuoteCharCount, delSp: Integer; QuoteChars: String): AbsatzP;
var
  ap2: AbsatzP;
  CopySize: Integer;
begin
  Copysize := max(0, ap^.size-wp);
  CreateCopy(ap,ap2,wp-spaces-QuoteCharCount+delsp,copysize+spaces+QuoteCharCount-delsp);
  if spaces>0 then
    fillchar(ap2^.cont,spaces,' ')
  else
    if QuoteCharCount>0 then
      Move(QuoteChars[1],ap2^.cont[0],QuoteCharCount);
  if assigned(ap^.next) then
    ap^.next^.prev:=ap2;
  ap2^.next:=ap^.next;
  ap^.next:=ap2;
  ap2^.prev:=ap;
  blockabssplit(ap,ap,ap2,wp,spaces+QuoteCharCount-delsp);
  ap:=AbsDelete(ap,wp,copysize,false,false);
  with e^ do
  begin
    if ap^.Size < rrand - 10 then
       ap^.Umbruch := na_umbruch;
    if ap2^.Size < rrand - 10 then
       ap2^.Umbruch := na_umbruch;
  end;
  KorrBlockScrolled(false);
  Result := ap2;
end;

procedure NewLine;              { Enter - Absatz einfÅgen }
var ap,ap2   : absatzp;
    wp       : integer;
    delsp    : integer;
    spaces   : integer;
    QuoteCharCount: integer;  { Anzahl Quotezeichen fuer Quote-Reflow }
    QuoteChars: string;       { Quotezeichen im aktuellen Absatz }
begin
  with e^ do
    if insertmode then
    begin
      ap:=ActAbs;
      wp:=workpos;
      delsp:=0;

      { Quote-Reflow }
      if not kb_shift then
      begin
        QuoteChars := GetQuote(ap);
        if (wp>0) and (QuoteChars<>'') and (length(QuoteChars)>wp) then
          wp:=length(QuoteChars);
          
        if (QuoteChars <>'') and (wp>= Length(TrimRight(QuoteChars))) then
        begin
          delsp:=wp;
          while (delsp<ap^.size) and (ap^.cont[delsp]=' ') do inc(delsp);
          dec(delsp,wp);
        end;
      end else
        QuoteChars := '';
      QuoteCharCount := Length(QuoteChars);
      if wp>=ap^.size then
        QuoteCharCount:=0; { kein Reflow bei Zeilenende }

      if QuoteCharCount > wp then
        QuoteCharCount:=0;

      Spaces := iif(QuoteCharCount = 0, CountSpaces(ap), 0);
      if spaces >= wp then spaces:=0;

      ap2 := BreakBlock(ap, wp, Spaces, QuoteCharCount, delSp, QuoteChars);

      if ActAbs<>ap2 then
        ZeileUnten;
      Zeilenanfang;
      moveworkpos(spaces+QuoteCharCount,actabs);           { Cursor an Zeilenanfang }
      modified:=true;
      aufbau:=true;
    end
    else begin
      Zeilenanfang;
      ZeileUnten;
    end;
end;

function ConcatBlock(ap: AbsatzP; addspaces: Integer): AbsatzP;
var
  Size1: Integer;
  ap2: AbsatzP;
begin
  ap2 := ap^.next;
  Result := AllocAbsatz(ap^.size + ap2^.size + addspaces);
  Copyflags(ap, Result);
  Blockabsinsert(ap, Result,maxint,maxint);
  Blockabsinsert(ap2, Result,0,ap^.size+addspaces);
  if Assigned(ap^.prev) then
  begin
    Result^.prev:=ap^.prev;
    ap^.prev^.next:= Result;
  end;
  if Assigned(ap2^.next) then
  begin
    Result^.next:=ap2^.next;
    ap2^.next^.prev:= Result;
  end;
  Size1:=ap^.size;
  Move(ap^.cont, Result^.cont,size1);
  Fillchar(Result^.cont[size1],addspaces,32);
  Move(ap2^.cont,Result^.cont[size1+addspaces],ap2^.size);
  absatzwechsel(ap,Result,false);
  FreeAbsatz(ap);
  FreeAbsatz(ap2);
  KorrBlockScrolled(true);
end;

procedure DELchar;              { DEL - Zeichen lîschen }
var ap: absatzp;
    wp  : word;
    addspaces    : integer;
    wpa          : absatzp;
    QuoteChars1: string;
    QuoteChars2: string;
begin
  with e^ do begin
    ap:=ActAbs;
    wp:=workpos;
    wpa:=ap;
    if wp<ap^.size then
      wpa:=AbsDelete(ap,workpos,1,false,true)
    else
      if ap^.next=nil then    { Textende }
        errsound
      else begin
        addspaces:=EndSpaces;
        if (addspaces=0) and (ap^.size>0) and (ap^.next^.size>0)
          then if not (ap^.cont[ap^.size-1] in ['-','/'])
                  and (ap^.next^.cont[0]<>' ') then addspaces:=1;
        if ap^.size + ap^.next^.size + addspaces <= maxabslen then
        begin   { AbsÑtze zusammenhÑngen }
          { Quote-Reflow }
          QuoteChars1 := GetQuote(ap);
          QuoteChars2 := GetQuote(ap^.next);
          if Config.QuoteReflow and (Length(QuoteChars1) > 0) and
            (Trim(QuoteChars1)=Trim(QuoteChars2)) then
            AbsDelete(ap^.next, 0, Length(QuoteChars1), false, true);
          wpa := ConcatBlock(ap, addspaces);
          Modified:=true;
        end else
          error(2)    { 'Absatz zu gro·' }
      end;
    MoveWorkpos(wp,wpa);
    aufbau:=true;
  end;
end;

procedure BackSpace;            { Backspace - Zeichen loeschen }
begin
  if EndSpaces>0 then
    if Zeichenlinks then else
  else
    if ZeichenLinks then begin
      if aufbau then NoDisplay;
      DELchar;
      end;
end;

function ADWhitespace(wpnew:integer):boolean;
begin
  ADWhitespace:=(dl^[e^.scy].absatz^.cont[wpnew] in [' ',#9]);
end;

procedure WortRechtsLoeschen;   { Wort rechts loeschen }
var 
  wp,wpnew,size: integer;
begin
  size:=ActAbs^.size;
  if workpos>=size then
    DELchar
  else with e^ do begin
    wp:=workpos;
    wpnew:=workpos;
    while (wpnew<size) and not (ActAbs^.cont[wpnew] in TrennZeich) do
      inc(wpnew);
    while (wpnew<size) and ADWhitespace(wpnew) do inc(wpnew);
    if wpnew=workpos then inc(wpnew);
    MoveWorkpos(wp, AbsDelete(ActAbs,wp,wpnew-wp,true,true));
    aufbau:=true;
  end;
end;

procedure WortLinksLoeschen;    { Wort links loeschen }
var wp,wpnew: xpWord;
    wpa             : absatzp;
begin
  if workpos=0 then
    BackSpace
  else if workpos>ActAbs^.size then
    zeilenende
  else with e^ do 
  begin
    wp:=workpos;
    wpnew:=workpos;
    while (wpnew>0) and not (ActAbs^.cont[wpnew-1] in trennzeich) do dec(wpnew);
    while (wpnew>0) and ADWhiteSpace(wpnew-1) do dec(wpnew);
    if wpnew=workpos then 
      dec(wpnew);
    wpa:=AbsDelete(ActAbs,wpnew,wp-wpnew,true,true);
    MoveWorkPos(wpnew,wpa);
    aufbau:=true;
  end;
end;

procedure ZeileLoeschen;        { akt. Zeile loeschen }
var apd    : absatzp;
    bc     : boolean;
begin
  with e^ do                         { 1. Fall: letzte Zeile im Text }
    if (ActAbs^.next=nil) and (dl^[scy].zeile=alines(ActAbs)) then begin
      AbsDelete(ActAbs,dl^[scy].offset,LineLength,true,true);
    end
    else if alines(ActAbs)=1 then begin   { 2. Fall: kompletten Absatz loeschen }
      absatzwechsel(ActAbs,ActAbs^.next,false);
      bc:=BlockAbsCut(ActAbs,nil,0,ActAbs^.size);
      if assigned(ActAbs^.prev) then
        ActAbs^.prev^.next:=ActAbs^.next;
      ActAbs^.next^.prev:=ActAbs^.prev;
      apd:=AllocAbsatz(0);
      copyflags(ActAbs,apd);
      ActAbs^.next:=apd; ActAbs^.prev:=nil;  { leeren Absatz anhaengen }
      apd^.prev:=ActAbs;
      AddDelEntry(ActAbs);
      KorrBlockScrolled(true);
      if bc then CheckBlockOrder;
      modified:=true;
    end
    else                     { 3. Fall: Zeile aus Absatz loeschen }
      AbsDelete(ActAbs,dl^[scy].offset,LineLength,true,true);
  aufbau:=true;
end;

procedure AbsatzRechtsLoeschen;
var
  wp,size : integer;
begin
  size:=ActAbs^.size;
  if workpos<size then with e^ do begin
    wp:=workpos;
    AbsDelete(ActAbs,wp,size-wp,true,true);
    aufbau:=true;
  end;
end;

procedure ZeichenEinfuegen(fast:boolean);     { Texteingabe }
const u1 : string[7] = 'ÑîÅéôö·';
      u2 = 'aouAOUs';
      u3 = 'eeeeeess';
var ap     : absatzp;
    wp     : integer;
    spaces : integer;
    apnew  : absatzp;
    p      : byte;
begin
  with e^ do begin
    p:=cpos(t[1],u1);
    if ukonv and (p>0) then begin
      t:=copy(u2,p,1);
      ZeichenEinfuegen(fast);
      t:=copy(u3,p,1);
      end;
    ap:=ActAbs;
    wp:=workpos;
    if not insertmode and (wp<ap^.size) then begin   { Overwrite }
      ap^.cont[wp]:=t[1];
      MoveWorkpos(wp+1,nil);
      end
    else begin                                            { Insert }
      spaces:=EndSpaces;
      if (spaces=0) and (ap^.msize>ap^.size) then begin
        blockabsinsert(ap,ap,wp,1);
        Move(ap^.cont[wp],ap^.cont[wp+1],ap^.size-wp); { noch Platz da... }
        ap^.cont[wp]:=t[1];
        inc(ap^.size);
        MoveWorkpos(wp+1,nil);
        end
      else
          if ap^.size+spaces>=maxabslen then
            error(2)       { 'Absatz zu gross' }
          else begin
            apnew:=AllocAbsatz(ap^.size+spaces+1);
            copyflags(ap,apnew);
            absatzwechsel(ap,apnew,true);
            blockabsinsert(ap,apnew,wp,1);
            Move(ap^.cont,apnew^.cont,wp-spaces);
            fillchar(apnew^.cont[wp-spaces],spaces,32);
            apnew^.cont[wp]:=t[1];
            if ap^.size>wp then
              Move(ap^.cont[wp],apnew^.cont[wp+1],ap^.size-wp);
            FreeAbsatz(ap);
            MoveWorkpos(wp+1,nil);
          end;
      if not fast then
        KorrBlockScrolled(false);
    end;
    modified:=true;
    aufbau:=true;
  end;
end;

procedure Steuerzeichen;        { ^P - Steuerzeicheneingabe }
begin
  t:=GetPrefixChar('P',false);
  ZeichenEinfuegen(false);
end;

procedure Insert(var blk:absatzp; var endpos:position); forward;

procedure tabulator;
var n,i   : byte;
    ap    : absatzp;
    dummy : position;
    wp    : integer;
begin
  n:=8 - workpos mod 8;
  if not e^.insertmode then begin
    for i:=1 to n do CondZeichenRechts;
    end
  else begin
    wp:=workpos;
    ap:=AllocAbsatz(n);
    fillchar(ap^.cont,n,32);
    Insert(ap,dummy);
    MoveWorkpos(wp+n,nil);
  end;
end;

procedure Paragraph;
begin
  t:=^U;
  ZeichenEinfuegen(false);
end;


procedure ModiBlock(mproc:modiproc);
var ap,ap1,ap2 : absatzp;
    ofs1,ofs2  : integer;
begin
  with e^ do begin
    ap1:=block[1].pos.absatz;
    ap2:=block[2].pos.absatz;
    ofs1:=block[1].pos.offset;
    ofs2:=block[2].pos.offset;
    if ap1=ap2 then
      mproc(ap1^.cont[ofs1],ofs2-ofs1)
    else begin
      mproc(ap1^.cont[ofs1],ap1^.size-ofs1);
      ap:=ap1^.next;
      while assigned(ap) and (ap<>ap2) do begin
        mproc(ap^.cont,ap^.size);
        ap:=ap^.next;
        end;
      if assigned(ap) then
        mproc(ap2^.cont,ofs2);
      end;
    modified:=true;
    aufbau:=true;
    end;
end;

procedure BlockRot13;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else
      ModiBlock(Rot13);
end;

procedure CaseWechseln;
begin
  with e^ do
    if blockhidden or blockinverse then
      if workpos<ActAbs^.size then begin
        FlipCase(ActAbs^.cont[workpos],1);
        modified:=true;
        aufbau:=true;
        end
      else
        ErrSound
    else
      ModiBlock(FlipCase);
end;


{ -------------------------------------------- Blockbearbeitung }

procedure SetBlock(n:byte; abs:absatzp; ofs:integer; ndisp:byte);
begin
  with e^.block[n] do begin
    pos.absatz:=abs;
    pos.offset:=min(ofs,abs^.size);
    disp:=ndisp;
    end;
  CheckBlockOrder;
  e^.blockhidden:=false;
  aufbau:=true;
end;

procedure SetBlockmark(n:byte);
begin
  with e^ do
    SetBlock(n,ActAbs,workpos,2);
end;

procedure WortMarkieren(alt_trenn:boolean);
var
  wp,sp,sp0 : integer;

  function IsTrennz(p:integer):boolean;
  begin
    if alt_trenn then IsTrennz:=(ActAbs^.cont[p] in [#0..#32,#255])
    else IsTrennz:=(ActAbs^.cont[p] in trennzeich);
  end;

begin
  with e^ do begin
    wp:=workpos; sp:=-1;
    if (wp<ActAbs^.size) and not IsTrennz(wp) then
      sp:=wp
    else if (wp>0) and (wp<=ActAbs^.size) and not IsTrennz(wp-1) then
      sp:=wp-1;
    if sp>-1 then begin
      sp0:=sp;
      while (sp0>0) and not IsTrennz(sp0-1) do dec(sp0);
      while (sp<ActAbs^.size) and not IsTrennz(sp) do inc(sp);
      SetBlock(1,ActAbs,sp0,2);
      KorrBlockScrolled(true);
      SetBlock(2,ActAbs,sp,2);
      KorrBlockScrolled(false);
      end;
    end;
end;


procedure ZeileMarkieren;
begin
  with e^ do begin
    SetBlock(1,ActAbs,dl^[scy].offset,2);
    SetBlock(2,ActAbs,Advance(ActAbs,dl^[scy].offset,rrand),2);
    end;
end;

procedure AbsatzMarkieren;
begin
  with e^ do begin
    SetBlock(1,ActAbs,0,2);
    KorrBlockScrolled(true);
    if ActAbs^.next=nil then
      SetBlock(2,ActAbs,ActAbs^.size,2)
    else
      SetBlock(2,ActAbs^.next,0,2);
    KorrBlockScrolled(false);
    end;
end;

procedure KomplettMarkieren;
var ap : absatzp;
begin
  with e^ do begin
    SetBlock(1,root,0,2);
    ap:=ActAbs;
    while assigned(ap^.next) do ap:=ap^.next;
    SetBlock(2,ap,ap^.size,3);
    KorrBlockScrolled(true);
    aufbau:=true;
    end;
end;

{ Block an Cursorposition einfuegen; Blockzeiger auf Ende zurueckliefern }
{ blk^ wird freigegeben!                                               }

procedure Insert(var blk:absatzp; var endpos:position);
var ap,ap2,ap3,apn : absatzp;
    ss             : xpWord;
    wp             : integer;
    spaces         : integer;
begin
  endpos.absatz:=nil;
  if blk=nil then exit;
  ap:=ActAbs;
  spaces:=EndSpaces;
  wp:=workpos;
  if blk^.next=nil then begin        { Absatzausschnitt einfuegen }
    ss:=ap^.size+blk^.size+spaces;
    if ss>maxabslen then
      error(2)     { 'Absatz zu gross }
    else if assigned(blk)
    then begin
      ap2:=AllocAbsatz(ss);
      copyflags(ap,ap2);
      Move(ap^.cont,ap2^.cont,wp-spaces);
      fillchar(ap2^.cont[wp-spaces],spaces,32);
      ss:=blk^.size;
      Move(blk^.cont,ap2^.cont[wp],ss);
      if ap^.size>wp then
        Move(ap^.cont[wp],ap2^.cont[wp+ss],ap^.size-wp);
      endpos.absatz:=ap2;
      endpos.offset:=wp+ss;
      absatzwechsel(ap,ap2,true);
      blockabsinsert(ap,ap2,wp,ss);
      FreeAbsatz(ap);
      e^.modified:=true;
    end;
    blk:=FreeAbsatz(blk);
  end
  else begin                              { mehrere Absaetze einfuegen }
    apn:=blk;
    while assigned(apn^.next) do      { letzten Absatz in Block suchen }
      apn:=apn^.next;
    if (ap^.size+blk^.size>maxabslen) or
       (apn^.size+ap^.size>maxabslen) then begin
      error(2);     { 'Absatz zu gross }
      FreeBlock(blk);
    end
    else if assigned(blk)then begin
      ap2:=AllocAbsatz(blk^.size+wp);        { 1. Teil des ActAbs }
      copyflags(ap,ap2);                          { am Blockanfang einfg. }
      Move(ap^.cont,ap2^.cont,wp-spaces);
      fillchar(ap2^.cont[wp-spaces],spaces,32);
      Move(blk^.cont,ap2^.cont[wp],blk^.size);
      absatzwechsel(blk,ap2,true);
      if assigned(ap^.prev) then begin      { Verkettung mit vorausgeh. }
        ap2^.prev:=ap^.prev;      { Text herstellen           }
        ap2^.prev^.next:=ap2;
      end;
      ss:=ap^.size+spaces-wp;              { Groesse 2. Absatzteil }
      ap3:=AllocAbsatz(ss+apn^.size);
      copyflags(apn,ap3);
      Move(apn^.cont,ap3^.cont,apn^.size);
      Move(ap^.cont[wp],ap3^.cont[apn^.size],ss);
      absatzwechsel(apn,ap3,true);
      if assigned(ap^.next) then begin      { Verkettung mit nachfolg. }
        ap3^.next:=ap^.next;      { Text herstellen          }
        ap3^.next^.prev:=ap3;
      end;
      blockabssplit(ap,ap2,ap3,wp,0);
      absatzwechsel(ap,ap2,false);
      endpos.absatz:=ap3;
      endpos.offset:=apn^.size;
      FreeAbsatz(apn);
      blk:=FreeAbsatz(blk);
      FreeAbsatz(ap);
      e^.modified:=true;
    end
    else
      FreeBlock(blk);
  end;
  KorrBlockScrolled(false);
  aufbau:=true;
end;

procedure InsertWithMark(var ap:absatzp);
var pos : position;
begin
  if assigned(ap) then begin
    Insert(ap,pos);
    if assigned(pos.absatz) then begin
      SetBlock(1,ActAbs,workpos,2);
      SetBlock(2,pos.absatz,pos.offset,2);
      KorrBlockScrolled(false);
      GotoPos(pos, drBoth);
    end;
  end;
end;

function CopyBlock:absatzp;    { Kopie des markierten Blocks anlegen }
var 
  ap         : absatzp;
  ap0,ap1,ap2: absatzp;
  ss         : integer;
begin
  with e^ do
    if blockinverse then
      CopyBlock:=nil
    else if block[1].pos.absatz=block[2].pos.absatz then begin
      ss:=block[2].pos.offset-block[1].pos.offset;    { Absatzausschnitt }
      ap:=AllocAbsatz(ss);
      if assigned(ap) then begin
        Copyflags(block[1].pos.absatz,ap);
        Move(block[1].pos.absatz^.cont[block[1].pos.offset],
             ap^.cont,ss);
        end;
      CopyBlock:=ap;
      end
    else
    begin
      ap:=block[1].pos.absatz;
      repeat
        ap:=ap^.next;
      until (ap=block[2].pos.absatz) or (ap=nil);
      if assigned(ap) then 
      begin
        ap:=block[1].pos.absatz;
        ss:=ap^.size-block[1].pos.offset;
        ap0:=AllocAbsatz(ss); ap1:=ap0;             { Startabsatz }
        copyflags(ap,ap0);
        Move(ap^.cont[block[1].pos.offset],ap0^.cont,ss);
        ap:=ap^.next;
        while ap<>block[2].pos.absatz do begin      { Body }
          ap2:=AllocAbsatz(ap^.size);
          copyflags(ap,ap2);
          Move(ap^.cont,ap2^.cont,ap^.size);
          ap1^.next:=ap2;
          ap2^.prev:=ap1;
          ap1:=ap2;
          ap:=ap^.next;
          end;
        ap2:=AllocAbsatz(block[2].pos.offset);      { Endabsatz }
        copyflags(ap,ap2);
        Move(ap^.cont,ap2^.cont,block[2].pos.offset);
        ap1^.next:=ap2;
        ap2^.prev:=ap1;
        CopyBlock:=ap0;
        end
      else
        CopyBlock:=nil;
      end;
end;

procedure Undelete;
var ap   : absatzp;
    endp : position;
begin
  ap:=GetDelEntry;
  if ap=nil then
    errsound
  else begin
    Insert(ap,endp);
    if assigned(endp.absatz) then begin
      SetBlockMark(1);
      SetBlock(2,endp.absatz,endp.offset,2);
      KorrBlockScrolled(false);
    end;
  end;
end;

function InBlock:boolean;    { workpos innerhalb des mark. Blockes }
var b1,b2 : byte;    { 1=vor workpos, 2=gleich, 3=dahinter }
  function seek(n:byte):byte;
  var i : integer;
  begin
    with e^ do
      if (block[n].disp=1) or (block[n].disp=3) then
        seek:=block[n].disp
      else if block[n].pos.absatz=ActAbs then
        if block[n].pos.offset<workpos then seek:=1
        else if block[n].pos.offset=workpos then seek:=2
        else seek:=3
      else begin
        i:=1;
        while (i<=gl) and (block[n].pos.absatz<>dl^[i].absatz) do inc(i);
        if i<scy then seek:=1
        else seek:=3;
        end;
  end;
begin
  with e^ do
    if blockinverse or blockhidden then
      InBlock:=true
    else begin
      b1:=seek(1);
      b2:=seek(2);
      InBlock:=(b1<=2) and (b2>2);
      end;
end;

procedure RecountStartline;
var ap : absatzp;
begin
  with e^ do begin
    startline:=0; ap:=root;
    while (ap<>dl^[1].absatz) do begin
      inc(startline,alines(ap));
      ap:=ap^.next;
      end;
    inc(startline,firstline-1);
    end;
end;

function CutBlock:absatzp;      { markierten Block ausschneiden }
var ap1,ap2,ap    : absatzp;
    apnew,apl,apn : absatzp;
    ofs1,ofs2     : integer;
    ss,i,sp       : integer;
    inblk,aflag   : boolean;
    wp            : position;
    recount       : boolean;    { startline neu berechnen }

  procedure RestoreWorkpos;
  begin
    KorrBlockScrolled(true);
    with e^ do
      if inblk then
        if block[1].disp=1 then begin
          SeitenAnfang; Zeilenanfang; end
        else
          GotoPos(block[1].pos, drBoth)
      else if assigned(wp.absatz) then
        GotoPos(wp, drBoth);
  end;

begin
  with e^ do begin
    ap1:=block[1].pos.absatz;
    ap2:=block[2].pos.absatz;
    ofs1:=block[1].pos.offset;
    ofs2:=block[2].pos.offset;
    wp.absatz:=nil;
    wp.offset:=workpos;
    sp:=EndSpaces;
    inblk:=InBlock;
    recount:=false;
    CutBlock := nil; { MK 12/99 Sicherheitshalber immer ersteinmal nil }
    if blockinverse or blockhidden then begin
      errsound;
      end
    else if ap1=ap2 then begin    { Absatzteil ausschneiden }
      ss:=ofs2-ofs1;
      begin
        recount:=(block[1].disp<2);
        apnew:=AllocAbsatz(ss);
        copyflags(ap1,apnew);
        Move(ap1^.cont[ofs1],apnew^.cont,ss);
        aflag:=(ap1=ActAbs);
        ap:=AbsDelete(ap1,ofs1,ss,false,true);
        if aflag then begin
          wp.absatz:=ap;
          if ofs1<wp.offset then wp.offset:=max(ofs1,wp.offset-ss-sp);
          end
        else wp.absatz:=ActAbs;
        RestoreWorkpos;
        CutBlock:=apnew;
        modified:=true;
        aufbau:=true;
        end;
      end
    else if longint(ofs1)+ap2^.size-ofs2>maxabslen then
      error(2)                         { mehrere Absaetze ausschneiden }
    else begin
      recount:=(block[1].disp<2);
      wp.absatz:=ActAbs;
      ss:=ap1^.size-ofs1;
      apnew:=AllocAbsatz(ss);       { Ende von 1. Absatz ausschneiden }
      copyflags(ap1,apnew);
      Move(ap1^.cont[ofs1],apnew^.cont,ss);
      if ap1^.next<>ap2 then begin     { dazwischenliegende Absaetze }
        apnew^.next:=ap1^.next;   { anhaengen                   }
        apnew^.next^.prev:=apnew;
        apl:=apnew^.next;
        while apl^.next<>ap2 do apl:=apl^.next;
        end
      else
        apl:=apnew;
      apn:=AllocAbsatz(ofs2);       { Anfang vom letzten Absatz ausschn. }
      copyflags(ap2,apn);
      Move(ap2^.cont,apn^.cont,ofs2);
      apl^.next:=apn;             { .. an Cut-Block anhaengen }
      apn^.prev:=apl;

      apn:=AllocAbsatz(ofs1+(ap2^.size-ofs2));   { Join ap1+ap2 }
      copyflags(ap1,apn);
      blockabsinsert(ap1,apn,maxint,maxint);
      if blockabscut(ap2,ap2,0,ofs2) then;
      blockabsinsert(ap2,apn,0,ofs1);
      if assigned(ap1^.prev) then begin
        apn^.prev:=ap1^.prev;
        apn^.prev^.next:=apn;
        end;
      if assigned(ap2^.next) then begin
        apn^.next:=ap2^.next;
        apn^.next^.prev:=apn;
        end;
      Move(ap1^.cont,apn^.cont,ofs1);
      Move(ap2^.cont[ofs2],apn^.cont[ofs1],ap2^.size-ofs2);
      absatzwechsel(ap1,apn,false);
      absatzwechsel(ap2,apn,false);
      if wp.absatz=ap2 then begin
        wp.absatz:=apn;
        inc(wp.offset,ofs1-ofs2-sp);
        end;
      firstline:=min(firstline,alines(firstpar));

      block[2]:=block[1];           { markierter Block ist jetzt leer }
      blockinverse:=true;
      ap:=apnew^.next;
      while assigned(ap) do begin       { Pos.-Marker korrigieren }
        absatzwechsel(ap,apn,false);
        for i:=3 to 7 do
          if block[i].pos.absatz=ap then
            block[i]:=block[1];
        ap:=ap^.next;
        end;
      RestoreWorkpos;
      CutBlock:=apnew;
      modified:=true;
      aufbau:=true;
      end;

    if recount then RecountStartline;
    end;
end;

procedure BlockKopieren;     { Ctrl-K-C }
var ablock: absatzp;
begin
  ablock:=CopyBlock;
  InsertWithMark(ablock);
end;

procedure BlockEinAus;
begin
  e^.blockhidden:=not e^.blockhidden;
  aufbau:=true;
end;

procedure UmbruchKomplettEin;
begin
  KomplettMarkieren;
  UmbruchEin;
  BlockEinAus;
end;

procedure UmbruchKomplettAus;
begin
  KomplettMarkieren;
  UmbruchAus;
  BlockEinAus;
end;


procedure BlockLoeschen;
var ap : absatzp;
begin
  ap:=CutBlock;
  if assigned(ap) then AddDelEntry(ap);
end;

procedure RestLoeschen;     { ab Cursorposition bis Textende }
begin
  KomplettMarkieren;
  SetBlockMark(1);
  BlockLoeschen;
end;


procedure BlockVerschieben;
var ap : absatzp;
begin
  if InBlock then
    errsound
  else begin
    ap:=CutBlock;
    if assigned(ap) then InsertWithMark(ap);
    end;
end;


procedure BlockClpKopie(cut:boolean);
var ap : absatzp;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else
      if ClipAvailable then begin
        if SaveBlock(block[1].pos,block[2].pos,EdTempFile,rrand,false,true,false) then
        begin
          FileToClip(EdTempFile);
          { if FileExists(EdTempFile) then DeleteFile(EdTempFile); }
          if cut then begin
            ap:=CutBlock; Freeblock(ap); end;
          end;
        end
      else begin
        FreeBlock(Clipboard);
        if cut then Clipboard:=CutBlock
        else Clipboard:=CopyBlock;
        end;
end;


procedure BlockClpEinfuegen;
var lblock: absatzp;
begin
  with e^ do
    if Clipavailable then begin
      ClipToFile(EdTempFile);
      lblock:=LoadBlock(EdTempFile,false,0,rrand);
      InsertWithMark(lblock);
      { if FileExists(EdTempFile) then era(EdTempFile); }
    end
    else begin
      InsertWithMark(Clipboard);
      Clipboard:=CopyBlock;
    end;
end;


procedure BlockEinlesen;
var fn : string;
    ap : absatzp;
begin
  with e^ do begin
    Procs.FileProc(e,fn,false,false);
    if fn<>'' then
      if not FileExists(fn) then
        error(5)     { 'Fehler: Datei nicht vorhanden' }
      else begin
        ap:=LoadBlock(fn,false,0,rrand);
        if assigned(ap) then InsertWithMark(ap);
      end;
  end;
end;

procedure BlockUUeEinlesen;
var fn : string;
    ap : absatzp;
begin
  with e^ do begin
    Procs.FileProc(e,fn,false,true);
    if fn<>'' then
      if not FileExists(fn) then
        error(5)     { 'Fehler: Datei nicht vorhanden' }
      else begin
        ap:=LoadUUeBlock(fn);
        if assigned(ap) then InsertWithMark(ap);
      end;
  end;
end;

procedure BlockSpeichern;
var fn : string;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else begin
      Procs.FileProc(e,fn,true,false);
      if fn<>'' then begin
        if not FileExists(fn) then t:='J'
        else t:=Procs.Overwrite(e,fn);
        if (t='J') or (t='N') then
          if SaveBlock(block[1].pos,block[2].pos,fn,rrand,false,t='J',false) then;
      end;
    end;
end;


procedure BlockDrucken;
var ap,endap   : absatzp;
    ofs,endofs : xpWord;
    nofs       : xpWord;
    s          : string;
begin
  with e^ do begin
    attrtxt(col.colstatus);
    mwrt(x,y,forms(language^.drucken,w));
    if blockinverse or blockhidden then begin
      ap:=root; ofs:=0;
      endap:=nil; endofs:=0;
      end
    else begin
      ap:=block[1].pos.absatz;
      ofs:=block[1].pos.offset;
      endap:=block[2].pos.absatz;
      endofs:=block[2].pos.offset;
      end;
    checklst:=true;
    repeat
      nofs:=Advance(ap,ofs,rrand);
      if ap=endap then nofs:=min(nofs,endofs);
      SetLEngth(s, nofs-ofs); {s[0]:=chr(nofs-ofs);}
      Move(ap^.cont[ofs],s[1],length(s));
      writeln(lst,s);
      if nofs=ap^.size then begin
        ap:=ap^.next; ofs:=0; end
      else
        ofs:=nofs;
    until (ap=nil) or ((ap=endap) and (ofs>=endofs));
    writeln(lst);
    end;
end;

{ Block reformatieren }

procedure FormatBlock;
var
  ap,ap2    : absatzp;
  fp        : integer;
  addspaces : integer;
  spaces    : integer;
  QuoteCharCount: integer;  { Anzahl Quotezeichen fuer Quote-Reflow }
  QuoteChars1: string;
  QuoteChars2: string;
  saveumbr  : boolean;
begin
  with e^ do
  begin
    if blockinverse or blockhidden then
    begin
      { kein Block markiert oder Blockmarkierung ausgeschaltet:
        ActAbs bis Leerzeile formatieren }
      if ActAbs^.Size = 0 then
      begin // break on space
        Errsound;
        Exit;
      end;

      // mark a new block

      ap := ActAbs; // search upwards
      QuoteCharCount := Length(GetQuote(ap^.Next));
      while Assigned(ap^.Prev) and (ap^.prev^.Size > QuoteCharCount) do
      begin
        ap := ap^.Prev;
        QuoteCharCount := Length(GetQuote(ap^.Prev))
      end;
      SetBlock(1, ap, 0, 2); // set to start of block

      ap := ActAbs; // search downwards
      QuoteCharCount := Length(GetQuote(ap^.next));
      while Assigned(ap^.next) and (ap^.Next^.Size > QuoteCharCount) do
      begin
        ap := ap^.Next;
        QuoteCharCount := Length(GetQuote(ap^.Next))
      end;
    end else
    begin
      // set block 1 and 2 to the beginning and end of the block
      SetBlock(1, Block[1].Pos.Absatz, 0, 2); // mark from the beginning of the block
      with Block[2].Pos do
        if Offset = 0 then
          ap := Absatz^.Prev  // cursor is on beginning of last block, set to prev block
        else
          ap := Absatz;
    end;
    SetBlock(2, ap, MaxInt, 3); // set to end of block

    { Cursor auf Blockanfang setzen }
    GotoPos(block[1].pos, drBoth);
    ap:=ActAbs;
    saveumbr := ap^.umbruch;
    ap^.umbruch := true;

    repeat
      fp:=advance(ap,0,rrand);
      QuoteChars1 := GetQuote(ap);
      QuoteCharCount := Length(QuoteChars1);

      if fp < ap^.size then
      begin  { Absatz umbrechen }
        if QuoteCharCount > rrand div 2 then
        begin
          QuoteChars1 := TrimRight(QuoteChars1);
          QuoteCharCount := Length(QuoteChars1);
          if QuoteCharCount > rrand div 2 then
            QuoteCharCount:=0;
        end;

        Spaces := iif(QuoteCharCount = 0, CountSpaces(ap), 0);
        if Spaces > e^.rrand div 2 then
          Spaces := 0;

        if (fp<=QuoteCharCount) or (fp<=spaces) then
        begin { Absatz kann nicht umgebrochen werden }
          if ap=block[1].pos.absatz then ap^.umbruch:=saveumbr;
          errsound;
          break;
        end;

        ap2 := BreakBlock(ap, fp, Spaces, QuoteCharCount, 0, QuoteChars1);
        TruncAbs(ap);
        ap:=ap2;
      end
      else
      if assigned(ap^.next) and (ap<>block[2].pos.absatz) then
      begin  { Absaetze ggf. zusammenfuegen }
        addspaces:=1;
        if ap^.size > 0 then
          if ap^.cont[ap^.size-1] in ['-','/'] then
            addspaces:=0;
        TruncAbs(ap^.next); { Leerzeichen am Absatzende entfernen }

        if ap^.size + ap^.next^.size + addspaces <= maxabslen then
        begin
          QuoteChars2 := GetQuote(ap^.next);

          Spaces := iif((Length(QuoteChars1) = 0) and (Length(QuoteChars2) = 0), CountSpaces(ap^.next), 0);
          if Spaces > e^.rrand div 2 then
            Spaces := 0;

          TruncAbs(ap);
          TruncAbs(ap^.next);

          // keep empty lines and concat only with identical QuoteChars
          if (ap^.size > length(QuoteChars1)) and
            (ap^.next^.size > length(QuoteChars2)) and
            (Trim(QuoteChars1) = Trim(QuoteChars2)) then
          begin
            if Length(QuoteChars1) > 0 then
              AbsDelete(ap^.next, 0, Length(QuoteChars2), false, true)
            else
              AbsDelete(ap^.next, 0, spaces, false, true);

            ap := ConcatBlock(ap, addspaces);
            ap.umbruch:=true;
          end else
            ap := ap^.next;
        end else
        begin
          Error(2); // 'Absatz zu gross'
          break;
        end;
      end else
        ap := ap^.next;
    until ap=block[2].pos.absatz^.next;
    GotoPos(block[2].pos, drBoth);
    Modified := true;
    BlockHidden := true; // do not show block
  end;
  CheckBlockOrder;
  Aufbau := true;
end;


{ Markierten Block als String zurueckgeben }

function Marked2String:string;
var 
  len : Integer;
begin
  with e^ do
  begin
    len:=block[2].pos.offset-block[1].pos.offset;
    SetLength(Result, Len);
    if Len > 0 then Move(block[1].pos.absatz^.cont[block[1].pos.offset],Result[1],len);
  end;
end;


{ Glossary Funktion (Kuerzel mit STRG+Enter Uebersetzen) }

Procedure Glossary;
var ap : absatzp;
     p : position;

  function ScanGlossary(si:string):boolean;
  var t,t1 : text;
      s,s2 : String;
      lsi  : integer;
      brk  : boolean;
      List: TLister;
  begin
    ScanGlossary:=false;
    assign(t,FileUpperCase('glossary.cfg'));
    reset(t);
    if IoResult<>0 then errsound  { Keine Glossary Datei? }
    else begin
      brk:=false;
      repeat
        Readln(t,s);
      until EOF(t) or (pos('>'+AnsiUpperCase(si)+'<',AnsiUpperCase(s))=1);

      if EOF(t) then
      begin                           { Wort nicht gefunden - Kuerzelliste als Auswahlmenue }
        pushhp(64);
        reset(t);
        List := Listbox(50,8,'Glossary');
        List.AddLine(' >- ESC -<');
        repeat
         readln(t,s);
          if (FirstChar(s)='>') and (cpos('<',s)>0) then
          begin
            repeat
              readln(t,s2);
            until eof(t) or (s2<>'');
            if s2[1]<>'>' then List.AddLine(' '+forms(s,15)+'  '+forms(s2,31))
            end;
        until eof(t);
        List.OnKeyPressed := glossary_ed; {Taste E Abfragen}
        Brk := List.Show;
        s2:= List.GetSelection;
        lsi:=cpos('<',s2)-1;
        if lsi > 15 then lsi:=15;
        s2:=copy(s2,2,lsi);
        if s2='>- ESC -<' then brk:=true;
        List.Free;
        closebox;
        if not brk then begin
          reset(t);
          repeat
            readln(t,s);
          until (pos(s2,s)=1) or eof(t);
        end;
        setcolors;
        pophp;
        end;

      if not brk then
      begin
        assign(t1,FileUppercase('glossary.tmp'));     { Kuerzel gefunden, Text kopieren }
        rewrite(t1);
        readln(t, s); write(t1,s);
        while not EOF(t) and (FirstChar(s) <> '>') do
        begin
          readln(t,s);
          if FirstChar(s) <> '>' then Write(t1, #13#10, s);
        end;
        close(t1);
        ScanGlossary:=true;
        end;
      close(t);
      end;
    end;

begin
  with e^ do
  begin
    if blockinverse or blockhidden then Wortmarkieren(true);
    display;
    if scanglossary(marked2string) then
    begin
      if not (blockinverse or blockhidden)  { Markierung loeschen }
      then begin
        gotopos(block[1].pos, drBoth);
        BlockLoeschen;
        end;
      ap:=LoadBlock(FileUpperCase('glossary.tmp'),true,2,rrand);
      Insert(ap,p);
      gotopos(p, drBoth);
      end;
    Blockhidden:=true;
    Aufbau:=true;
    end;
end;


{ ---------------------------------------------- Suchfunktionen }

procedure Suchen(again,ersetzen:boolean);
var
      stxt  : string;
      spos  : integer;
      sofs  : xpWord;
      ap    : absatzp;
      insap : absatzp;
      pos   : position;
      repall: boolean;
      brk   : boolean;
      t     : taste;
      count : longint;
begin
  if again and (lastop=0) then begin
    errsound;
    exit;
  end;
  if again then ersetzen:=(lastop=2)
  else lastop:=iif(ersetzen,2,1);
  insap:=nil;
  with e^ do
  if again or
     (ersetzen and Procs.ReplFunc(e,txt,repby,igcase)) or
     (not ersetzen and Procs.FindFunc(e,txt,igcase)) then begin
    if txt='' then exit;
    if igcase then stxt:=UpperCase(txt)
    else stxt:=txt;
    brk:=false; repall:=false;
    count:=0;
    repeat
      sofs:=min(workpos+1,ActAbs^.size) {-dl^[scy].offset)};
      ap:=ActAbs;
      repeat
        spos:=SeekStr(ap^.cont[sofs],ap^.size-sofs,stxt,igcase);
      {   hinweis(strs(spos)+' / '+strs(sofs)+' / '+strs(ap^.size-sofs)); }
        if spos=-1 then begin
          ap:=ap^.next;
          sofs:=0;
        end;
      until (ap=nil) or (spos>=0);
      if ap=nil then
        if count=0 then error(6) else     { 'Text wurde nicht gefunden' }
      else begin
        inc(count);
        pos.absatz:=ap;
        pos.offset:=spos+sofs;
        GotoPos(pos, drForward);

        if not repall then begin
          setblockmark(1);
          Moveworkpos(workpos+length(stxt),ActAbs);
          setblockmark(2);
          Gotopos(pos, drForward);
          end;   

        if ersetzen then with language^ do begin
          if aufbau then Display;
          if repall then begin
            t:=replacechr[1];
            testbrk(brk);
          end
          else begin
            attrtxt(col.colstatus);
            wrt(x,y,forms(' '+askreplace,w));
            gotoxy(x+scx-1,y+scy);
            repeat
              get(t,curon); t := UpperCase(t);
            until (cpos(t[1],replacechr)>0) or (t=keyesc);
            if t[1]=replacechr[3] then begin
              repall:=true; t:=replacechr[1];
            end;
            brk:=(t=keyesc);
          end;
          if t[1]=replacechr[1] then begin  { ersetzen: Ja }
            insap:=AllocAbsatz(length(repby));  { Einfuegeabsatz erzeugen }
            if insap=nil then brk:=true
            else begin
              Move(repby[1],insap^.cont,length(repby));
              AbsDelete(ActAbs,workpos,length(txt),false,true);
              Insert(insap,pos);
              MoveWorkpos(workpos+length(repby),ActAbs);
            end;
          end;
        end
        else
          brk:=true;
      end;
    until (ap=nil) or brk;
    if repall then message:=strs(count)+language^.ersetzt;
  end;
end;


{ -------------------------------------------------------- Menue }

function LocalMenu:integer;
var mx,my,ml,i,p : byte;
    highp        : array[1..editmenumps] of byte;
    t            : taste;
    xx,yy,oldp   : integer;
    mausmenu     : boolean;
    lmcurtype    : curtype;

label sok;

  procedure display;
  var i : integer;
  begin
    moff;
    for i:=1 to editmenumps do with e^,language^ do
      if menue[i]<>'-' then begin
        if i=p then attrtxt(col.colmenuinv)
        else attrtxt(col.colmenu);
        wrt(mx+1,my+i,' '+LeftStr(menue[i],highp[i]-2));
        if i<>p then
          attrtxt(col.colmenuhi)
        else
          attrtxt(col.colmenuhiinv);
        Wrt2(menue[i,highp[i]]);
        if i<>p then
          attrtxt(col.colmenu)
        else
          attrtxt(col.colmenuinv);
        Wrt2(forms(mid(menue[i],highp[i]+1),ml-highp[i]+1));
      end;
    mon;
    with e^ do
      if EdSelcursor then begin
        gotoxy(mx+1,my+p);
        if p=0 then lmcurtype:=curoff
        else lmcurtype:=curon;
        end
      else gotoxy(x+scx-1,y+scy);
  end;

  procedure MausMenusel;
  var first : boolean;
  begin
    first:=true;
    repeat
      if p<>oldp then display;
      oldp:=p;
      if first then t:=mauslmoved
      else get(t,lmcurtype);
      if (t=mausrmoved) or (t=mauslmoved) then begin
        maus_gettext(xx,yy);
        if (yy<=my) or (yy>my+editmenumps) or (xx<=mx) or (xx>mx+ml+1) then
          p:=0
        else if (language^.menue[yy-my]<>'-') then
          p:=yy-my
        else if p=0 then
          p:=yy-my+1;
        end;
      first:=false;
    until maust=0;
  end;

begin
  with e^,language^ do begin
    ml:=length(menue[1]);
    for i:=1 to editmenumps do begin    { Achtung, ^ wird mitgerechnet }
      ml:=max(ml,length(menue[i]));
      highp[i]:=cpos('^',menue[i])+1;
      end;
    mausmenu:=(maust and 2<>0) and mausda;
    if mausmenu then
    begin
      maus_gettext(xx,yy);
      mx:=min(xx,screenwidth-ml-3);
      my:=min(yy,y+h-editmenumps-3);
      end
    else begin
      mx:=min(x+scx-1,screenwidth-ml-3);
      my:=y+min(scy,h-editmenumps-3);
      end;
    attrtxt(col.colmenu);
    forcecolor:=true;
    wpushs(mx,mx+ml+2,my,my+editmenumps+1,'');
    forcecolor:=false;
    mwrt(mx+2,my,' '+menue[0]+' ');
    for i:=1 to editmenumps do
      if menue[i]='-' then wrt(mx,my+i,HBar(ml+3));
    if (scx+x-1<mx) or (scx+x-1>mx+ml+2) or (scy+y<my) or (scy+y>my+editmenumps+1)
    then if insertmode then lmcurtype:=curon
            else lmcurtype:=cureinf
    else lmcurtype:=curoff;
    p:=0; oldp:=-1;

    if mausmenu then
      MausMenusel
    else begin
      p:=lastp;
      if (p<1) or (p>editmenumps) or (menue[p]='-') then p:=1;
      repeat
        if p<>oldp then display;
        oldp:=p; lastp:=p;
        get(t,lmcurtype);
        if (t=mausleft) or (t=mausright) then begin
          MausMenusel;
          goto sok;
          end
        else begin
          if t=keyup then
            if p=1 then p:=editmenumps
            else repeat dec(p) until menue[p]<>'-';
          if t=keydown then
            if p=editmenumps then p:=1
            else repeat inc(p) until menue[p]<>'-';
          if t=keyhome then p:=1;
          if t=keyend then p:=editmenumps;
          if (t=keyesc) or (t=keyf10) then p:=0;
          t := UpperCase(t);
          if (FirstChar(t)>='A') and (FirstChar(t)<='Z') then
            for i:=1 to editmenumps do
              if t[1]=UpCase(menue[i,highp[i]]) then begin
                p:=i; t:=keycr; end;
          end;
      until (t=keycr) or (p=0);
      if t=keycr then lastp:=p;
      end;

sok:  case p of
        1 : LocalMenu:=EditfCCopyBlock;
        2 : LocalMenu:=EditfCutBlock;
        3 : LocalMenu:=EditfPasteBlock;
        4 : LocalMenu:=EditfReadBlock;
        5 : LocalMenu:=EditfReadUUeBlock;
        6 : LocalMenu:=EditfWriteBlock;
        8 : LocalMenu:=EditfFind;
        9 : LocalMenu:=EditfFindReplace;
       10 : LocalMenu:=editfFindRepeat;
       12 : LocalMenu:=EditfWrapOff;
       13 : LocalMenu:=EditfWrapOn;
       15 : LocalMenu:=EditfSetup;
       17 : LocalMenu:=EditfBreak;
      else  LocalMenu:=0;
      end;
    wpop;
    end;
end;


procedure EinstellungenSichern;
var t : text;
begin
  assign(t,EdConfigFile);
  rewrite(t);
  with e^.Config do begin
    writeln(t,'RechterRand=',rechter_rand);
    writeln(t,'AbsatzEnde=',absatzendezeichen);
    writeln(t,'AutoIndent=',iifc(AutoIndent,'J','N'));

    writeln(t,'PersistentBlocks=',iifc(PersistentBlocks,'J','N'));
    writeln(t,'QuoteReflow=', iifc(QuoteReflow, 'J', 'N'));
  end;
  close(t);
end;


procedure Einstellungen;
var brk      : boolean;
    wp,o,nxo : xpWord;
begin
  with e^ do
    if @Procs.CfgFunc=nil then
      errsound
    else begin
      Procs.CfgFunc(Config,brk);
      if not brk then begin
        if absatzende<>' ' then absatzende:=Config.absatzendezeichen;
        if (rrand<>Config.rechter_rand) and (ActAbs=dl^[1].absatz)
           and (firstline>1) then begin
          wp:=workpos;
          firstline:=1; nxo:=0;
          repeat
            o:=nxo;
            nxo:=Advance(ActAbs,o,Config.rechter_rand);
            if (nxo>o) and (nxo<=wp) then begin
              inc(firstline); end;
          until (nxo=o) or (nxo>wp);
          end;
        if rrand<>Config.rechter_rand then
          RecountStartline;
        rrand:=Config.rechter_rand;
        EinstellungenSichern;
        aufbau:=true;
        end;
      end;
end;



function PosCoord(pos:position; disp:byte):longint; forward;

procedure InterpreteToken(tk: EdToken);

  { --------------------------------------------------- Steuerung }

  procedure Quit;
  var t : taste;
  begin
    with e^ do
      if not modified then
        ende:=true
      else
        if autosave then begin
          if EdSave(e) then;
          ende:=true;
          end
        else begin
          t:=Procs.QuitFunc(e);
          if t=language^.ja then ende:=EdSave(e)
          else ende:=(t=language^.nein);
          end;
  end;

  procedure SpeichernEnde;
  begin
    if EdSave(ed) then Quit;
  end;

  procedure ShiftMarkStart;
  var 
    MarkPos: Position;
  begin
    with e^ do
      if kb_shift then 
      begin
        GetPosition(MarkPos);
        MarkPos.offset:=min(MarkPos.offset,MarkPos.absatz^.size);
        if ((PosCoord(MarkPos,2)<>PosCoord(block[1].pos,block[1].disp))
          and (PosCoord(MarkPos,2)<>PosCoord(block[2].pos,block[2].disp)))
          or blockinverse or blockhidden then 
        begin
          SetBlockMark(1);
          SetBlockMark(2);
          ShiftBlockMarker := 3;
        end;
      end else
        if not config.persistentblocks and not blockhidden then 
        begin
          blockhidden:=true;
          aufbau:=true;
        end;
  end;

  procedure ShiftMarkEnd(MoveCursorAbove: Boolean);
  var 
    MarkPos: Position;
  begin
    if not kb_shift then Exit;
    with e^ do 
    begin
      GetPosition(MarkPos);
      MarkPos.Offset:=min(MarkPos.offset,MarkPos.absatz^.size);
      if MoveCursorAbove then 
      begin
        if (ShiftBlockMarker = 1) or (ShiftBlockMarker = 3) then 
        begin
          SetBlockMark(1);
          ShiftBlockMarker := 1;
        end else 
          if PosCoord(MarkPos,2)<=PosCoord(block[1].pos,block[1].disp) then 
          begin
             block[2]:=block[1];
             SetBlockMark(1);
             ShiftBlockMarker := 1;
          end else 
            SetBlockMark(2);
      end else 
      begin
        if (ShiftBlockMarker = 2) or (ShiftBlockMarker = 3) then
        begin
          SetBlockMark(2);
          ShiftBlockMarker := 2;
        end else 
          if PosCoord(MarkPos,2)>=PosCoord(block[2].pos,block[2].disp) then 
          begin
            block[1]:=block[2];
            SetBlockMark(2);
            ShiftBlockMarker :=2;
          end else 
            SetBlockMark(1);
      end;
    end;
  end;

begin
  with e^ do begin
    if (tk>=1) and (tk<=29) then GetPosition(lastpos);

    if tk in [editfBOL, editfEOL, editfPgUp, editfPgDn, editfUp, editfDown, 
      editfLeft, editfRight, editfPageTop, editfPageBottom, editfTop, 
      editfBottom, editfWordLeft, editfWordRight] then ShiftMarkStart;
      
    case tk of
      editCorrectWorkpos: CorrectWorkPos;
      { Blockoperationen }
      editfText         : if e^.config.persistentblocks
                           then ZeichenEinfuegen(false)
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             ZeichenEinfuegen(false);
                           end;
      editfBS           : if e^.config.persistentblocks
                           then BackSpace
                           else if (blockinverse or blockhidden)
                            then BackSpace
                            else BlockLoeschen;
      editfDEL          : if kb_shift
                           then BlockClpKopie(true)
                           else if e^.config.persistentblocks
                            then DELchar
                            else if (blockinverse or blockhidden)
                             then DELchar
                             else BlockLoeschen;
      { Blockoperationen }
      editfNewline      : if e^.config.persistentblocks
                           then NewLine
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             NewLine;
                           end;
      editfDelWordRght  : WortRechtsLoeschen;
      editfDelWordLeft  : WortLinksLoeschen;
      editfDelLine      : ZeileLoeschen;
      editfCtrlPrefix   : if e^.config.persistentblocks
                           then Steuerzeichen
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             Steuerzeichen;
                           end;
      editfTAB          : if e^.config.persistentblocks
                           then Tabulator
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             Tabulator;
                           end;
      editfUndelete     : Undelete;
      editfParagraph    : if e^.config.persistentblocks
                           then Paragraph
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             Paragraph;
                           end;
      editfRot13        : BlockRot13;
      editfChangeCase   : CaseWechseln;
      editfPrint        : BlockDrucken;

      { Block markieren }
      editfBOL          : Zeilenanfang;
      editfEOL          : Zeilenende;
      editfPgUp         : SeiteOben(true);
      editfPgDn         : SeiteUnten;
      editfScrollUp     : Scroll_Up;
      editfScrollDown   : Scroll_Down;
      { Block markieren }
      editfUp           : ZeileOben;
      editfDown         : ZeileUnten;
      editfLeft         : ZeichenLinks;
      editfRight        : CondZeichenRechts;
      editfPageTop      : Seitenanfang;
      editfPageBottom   : Seitenende;
      editfTop          : Textanfang;
      editfBottom       : Textende;
      editfWordLeft     : WortLinks;
      editfWordRight    : WortRechts;

      editfLastpos      : GotoPos(lastpos, drBoth);
      editfMark1        : SetMarker(1);
      editfMark2        : SetMarker(2);
      editfMark3        : SetMarker(3);
      editfMark4        : SetMarker(4);
      editfMark5        : SetMarker(5);
      editfGoto1        : GotoMarker(1);
      editfGoto2        : GotoMarker(2);
      editfGoto3        : GotoMarker(3);
      editfGoto4        : GotoMarker(4);
      editfGoto5        : GotoMarker(5);
      editfGotoBStart   : GotoPos(e^.block[1].pos, drBoth);
      editfGotoBEnd     : GotoPos(e^.block[2].pos, drBoth);

      editfFind         : Suchen(false,false);
      editfFindReplace  : Suchen(false,true);
      editfFindRepeat   : Suchen(true,false);

      { shift-ins: Block einfuegen - Zweitbelegung
        ctrl-ins: Block kopieren - Zweitbelegung   }
      editfChangeInsert : e^.insertmode:=not e^.insertmode;
      editfChangeIndent : e^.Config.AutoIndent:=not e^.Config.AutoIndent;
      editfAbsatzmarke  : SetAbsatzmarke;
      editfWrapOn       : UmbruchEin;
      editfWrapOff      : UmbruchAus;
      editfAllwrapOn    : UmbruchKomplettEin;
      editfAllwrapOff   : UmbruchKomplettAus;

      editfBlockBegin   : SetBlockMark(1);
      editfBlockEnd     : SetBlockMark(2);
      editfMarkWord     : WortMarkieren(false);
      editfMarkLine     : ZeileMarkieren;
      editfMarkPara     : AbsatzMarkieren;
      editfMarkAll      : KomplettMarkieren;
      editfCopyBlock    : BlockKopieren;
      editfHideBlock    : BlockEinAus;
      editfDelBlock     : BlockLoeschen;
      editfMoveBlock    : BlockVerschieben;
      editfReadBlock    : BlockEinlesen;
      editfReadUUeBlock : BlockUUeEinlesen;
      editfWriteBlock   : BlockSpeichern;
      editfCCopyBlock   : BlockClpKopie(false);
      editfCutBlock     : BlockClpKopie(true);
      { Blockoperationen }
      editfPasteBlock   : if e^.config.persistentblocks
                           then BlockClpEinfuegen
                           else begin
                             if not (blockinverse or blockhidden)
                              then BlockLoeschen;
                             BlockClpEinfuegen;
                             BlockEinAus;
                           end;
      editfFormatBlock  : FormatBlock ;
      editfDelToEOF     : RestLoeschen;
      editfDeltoEnd     : AbsatzRechtsLoeschen;

      editfMenu         : InterpreteToken(LocalMenu);
      editfSetup        : Einstellungen;
      editfSaveSetup    : EinstellungenSichern;
      editfSave         : if EdSave(ed) then;
      editfSaveQuit     : SpeichernEnde;
      editfBreak        : Quit;
      editfGlossary     : Glossary;
    end;

    if tk in [editfBOL, editfPgUp, editfUp, editfLeft, editfPageTop, 
      editfTop, editfWordLeft] then ShiftMarkEnd(true)
    else
      if tk in [editfEOL, editfPgDn, editfDown, editfRight, editfPageBottom,
        editfBottom, editfWordRight] then ShiftMarkEnd(false);
  end;
end;


procedure InterpreteKey(t:taste);   { provisorisch }
var b : EdToken;
begin
  b := 0;
  if t=#127    then b:=EditfDelWordLeft else
{$IFDEF ncrt}
  if t=keygreymult then b:=EditfCCopyBlock else
  if t=keygreyminus then b:=EditfCutBlock   else
  if t=keygreyplus  then b:=EditfPasteBlock else
{$ELSE}
  if lastscancode=GreyMult  then b:=EditfCCopyBlock else
  if lastscancode=GreyMinus then b:=EditfCutBlock   else
  if lastscancode=GreyPlus  then b:=EditfPasteBlock else
{$ENDIF}       
  if t>=' '    then b:=EditfText        else

  if t=keyesc  then b:=EditfBreak       else
  if t=keyaltx then b:=EditfBreak       else
  if t=keyleft then b:=EditfLeft        else
  if t=^S      then b:=EditfLeft        else    { WS-Zweitbelegung }
  if t=keyrght then b:=EditfRight       else
  if t=^D      then b:=EditfRight       else    { WS-Zweitbelegung }
  if t=keyup   then b:=EditfUp          else
  if t=^E      then b:=EditfUp          else    { WS-Zweitbelegung }
  if t=keydown then b:=EditfDown        else
  if t=^X      then b:=EditfDown        else    { WS-Zweitbelegung }
  if t=keypgup then b:=EditfPgUp        else
  if t=^R      then b:=EditfPgUp        else    { WS-Zweitbelegung }
  if t=keypgdn then b:=EditfPgDn        else
  if t=^C      then b:=EditfPgDn        else    { WS-Zweitbelegung }
  if t=keyclft then b:=EditfWordLeft    else
  if t=^A      then b:=EditfWordLeft    else    { WS-Zweitbelegung }
  if t=keycrgt then b:=EditfWordRight   else
  if t=^F      then b:=EditfWordRight   else    { WS-Zweitbelegung }
  if t=keycpgu then b:=EditfTop         else
  if t=keycpgd then b:=EditfBottom      else
  if t=keychom then b:=EditfPageTop     else
  if t=keycend then b:=EditfPageBottom  else
  if t=keyend  then b:=EditfEOL         else
  if t=keyhome then b:=EditfBOL         else
  if t=^Z      then b:=EditfScrollUp    else
  if t=^W      then b:=EditfScrollDown  else
  if t=keyins  then if kb_shift then b := EditfPasteBlock
                       else b:=EditfChangeInsert else
  if t=keycins then b:=editfCCopyBlock  else
  if t=keycr   then b:=EditfNewline     else
  if t=keybs   then b:=EditfBS          else
  if t=keydel  then if kb_shift then b := EditfCutBlock else
                       b:=EditfDEL else
  if t=^G      then b:=EditfDEL         else    { WS-Zweitbelegung }
  if t=keyf5   then b:=EditfAbsatzmarke else
  if t=^T      then b:=EditfDelWordRght else
  if t=keycdel then b:=EditfDelWordRght else
  if t=^Y      then b:=EditfDelLine     else
  if t=^P      then b:=EditfCtrlPrefix  else
  if t=keyf3   then b:=EditfWrapOff     else
  if t=keyf4   then b:=EditfWrapOn      else
  if t=keysf3  then b:=EditfAllwrapOff  else
  if t=keysf4  then b:=EditfAllwrapOn   else
  if t=keytab  then b:=EditfTAB         else
  if t=^U      then if kb_shift then b:=EditfParagraph
                                else b:=EditfUndelete else
  if t=keyalty then b:=EditfDelToEOF    else
  if t=^L      then b:=EditfFindRepeat  else
  if t=keyalt3 then b:=EditfChangeCase  else

  if t=keysf5  then b:=EditfHideBlock   else
  if t=keyf7   then b:=EditfBlockBegin  else
  if t=keyf8   then b:=EditfBlockEnd    else
  if t=keysf7  then b:=EditfMarkWord    else
  if t=keysf8  then b:=EditfMarkPara    else
  if t=keysf9  then b:=EditfMarkLine    else
  if t=keysf10 then b:=EditfMarkAll     else
  if t=^B      then b:=EditfFormatBlock else

  if t=keyf2   then b:=EditfSave        else
  if t=keysf2  then b:=EditfSaveQuit    else
  if t=keyf10  then b:=EditfMenu        else

  if (t=keyctcr) or (t=keyaltG)
    then b:=EditfGlossary else

  if t=^Q      then case GetPrefixChar('Q',true) of
                      'P' : b:=EditfLastpos;
                      'L' : b:=EditfRestorePara;
                      '1' : b:=EditfGoto1;
                      '2' : b:=EditfGoto2;
                      '3' : b:=EditfGoto3;
                      '4' : b:=EditfGoto4;
                      '5' : b:=EditfGoto5;
                      'B' : b:=EditfGotoBStart;
                      'K' : b:=EditfGotoBEnd;
                      'F' : b:=EditfFind;
                      'A' : b:=EditfFindReplace;
                      'I' : b:=EditfChangeIndent;
                      'Y' : b:=EditfDeltoEnd;
                      'S' : b:=EditfBOL;
                      'D' : b:=EditfEOL;
                      'R' : b:=EditfTop;
                      'C' : b:=EditfBottom;
                    end else

  if t=^K      then case GetPrefixChar('K',true) of
                      '1' : b:=EditfMark1;
                      '2' : b:=EditfMark2;
                      '3' : b:=EditfMark3;
                      '4' : b:=EditfMark4;
                      '5' : b:=EditfMark5;
                      'C' : b:=EditfCopyBlock;
                      'V' : b:=EditfMoveBlock;
                      'Y' : b:=EditfDelBlock;
                      'H' : b:=EditfHideBlock;
                      'B' : b:=EditfBlockBegin;
                      'K' : b:=EditfBlockEnd;
                      'R' : b:=EditfReadBlock;
                      'U' : b:=EditfReadUUeBlock;
                      'W' : b:=EditfWriteBlock;
                      'O' : b:=EditfRot13;
                      'T' : b:=EditfMarkWord;
                      'P' : b:=EditfPrint;
                      'D' : b:=EditfBreak;
                      'S' : b:=EditfChangeCase;
                    end else

  if t=^O      then case GetPrefixChar('O',true) of
                      'R' : b:=EditfSetup;
                    { 'S' : b:=EditfSaveSetup; }
                    end;
  if b<>0 then EdAddToken(ed,b);
end;

function PosCoord(pos:position; disp:byte):longint;
var i : integer;
begin
  PosCoord := -1;
  with e^ do
    case disp of
      1 : PosCoord:=-1;
      2 : begin
            i:=1;
            while (i<=gl) and (dl^[i].absatz<>pos.absatz) do inc(i);
            if i>gl then PosCoord:=maxlongint { !? }
            else PosCoord:=$10000*i + pos.offset;
          end;
      3 : PosCoord:=maxlongint;
    end;
end;

  procedure maus_bearbeiten;
  var xx,yy : integer;
      ax,ay : integer;
      nx,ny : integer;
      lx,ly : integer;
      mbm   : byte;        { Blockmarker, auf dem sich die Maus befindet }
      up    : boolean;
      apos  : position;
      tc    : longint;

    procedure KorrScy;
    begin
      with e^ do
        if dl^[scy].absatz=nil then begin
          while dl^[scy].absatz=nil do dec(scy);
          scx:=dl^[scy].absatz^.size-dl^[scy].offset+1;
          end;
    end;

    procedure setscx(xx:integer);
    var nxx : integer;
    begin
      with e^ do begin
        nxx:=Advance(dl^[scy].absatz,dl^[scy].offset,rrand);
        if nxx=ActAbs^.size then scx:=xx
        else scx:=minmax(xx,1,nxx-dl^[scy].offset);
        end;
    end;

  begin
    maus_gettext(xx,yy);
    with e^ do
      if (xx>=x) and (xx<x+w) and (yy>=y) and (yy<=y+h) then
        if yy>=y then
          if t=mausldouble then
            WortMarkieren(false)
          else if t=mausright then begin
            InterpreteToken(LocalMenu);
            t:='';
            end
          else if t=mausleft then begin
            lx:=xx; ly:=yy;
            TruncAbs(ActAbs);
            scy:=max(1,yy-y);
            KorrScy;
            Setscx(xx);
            SetBlockMark(1);
            SetBlockMark(2);
            display;
            mbm:=3;     { beide }
            repeat                { Blockmarkierschleife }
              repeat
                gotoxy(x+scx-1,y+scy);
                get(t,curon);
                if t=mauslmoved then begin
                  maus_gettext(ax,ay);
                  nx:=minmax(ax,x,x+w-1);
                  ny:=minmax(ay,y+1,y+h);
                  if (nx<>lx) or (ny<>ly) then begin
                    lx:=nx; ly:=ny;
                    scy:=ny-y;
                    KorrScy;
                    Setscx(nx);
                    up:=(ny<yy) or ((ny=yy) and (nx<xx));
                    GetPosition(apos);
                    if up then
                      case mbm of
                        1,3 : begin SetBlockmark(1); mbm:=1; end;
                        2   : if PosCoord(apos,2)>=PosCoord(block[1].pos,block[1].disp) then
                                SetBlockmark(2)
                              else begin
                                block[2]:=block[1];
                                SetBlockmark(1);
                                mbm:=1;
                                end;
                      end
                    else
                      case mbm of
                        1   : if PosCoord(apos,2)<=PosCoord(block[2].pos,block[2].disp) then
                                SetBlockmark(1)
                              else begin
                                block[1]:=block[2];
                                SetBlockmark(2);
                                mbm:=2;
                                end;
                        2,3 : begin SetBlockmark(2); mbm:=2; end;
                      end;
                    aufbau:=true;
                    end;
                  if ((ay<=y) and ScrolLDown) or      { AutoScrolling }
                     ((ay>=y+h-1) and assigned(dl^[gl].absatz) and ScrollUp) then begin
                    tc:=ticker;
                    keyboard(mauslmoved); inc(ly); display; showstat;
                    repeat until tc<>ticker;
                    end;
                  end;
              until not keypressed or (t=mausunleft);
              if aufbau then begin
                display; showstat; end;
            until t=mausunleft;
            end;
  end;

begin
  e:=edp(ed);
  with e^ do begin
    new(dl);
    trennzeich:=[#0..#31,' ','!','('..'/',':'..'?','['..'^',
                 '''','"','{'..#127,#255];
    aufbau:=true; ende:=false;
    cursor(curon);
    tk:=0;
    if EditResetoldpos then Begin
      EditResetoldpos:=false;
      scx:=minmax(Lastscx,1,80); xoffset:=Lastxoffset;
      display;
      for ShiftBlockMarker:=1 to (laststartline+gl-1) do ZeileUnten;
      scy:=minmax(Lastscy,1,gl);
      end;
    repeat
      if aufbau then display;
      showstat;
      InterpreteToken(EditCorrectWorkpos);         { CorrectWorkpos }
      gotoxy(x+scx-1,y+scy);
      if insertmode then get(t,curon)
      else get(t,cureinf);
      if assigned(TProc) then
        if TProc(t) then EdAddToken(e,EditfBreak);
      if (t>=mausfirstkey) and (t<=mauslastkey) then
        maus_bearbeiten;
      InterpreteKey(t);
      while tnextout<>tnextin do begin
        tk:=tokenfifo[tnextout];
        InterpreteToken(tk);
        inc(tnextout);
        if tnextout=maxtokens then
          tnextout:=0;
        end;
    until ende;
    if EditNachricht and not NoCursorsave then begin
      Lastscx:=scx; Lastxoffset:=xoffset;
      Lastscy:=scy; Laststartline:=startline;
    end;
    dispose(dl);
    end;
  EdEdit:=tk;
end;


procedure EdExit(var ed:ECB);      { Release }
begin
  if assigned(ed) then begin
    FreeBlock(edp(ed)^.root);
    akted:=edp(ed)^.lastakted;
    dispose(edp(ed));
    ed:=nil;
    dec(ecbopen);
    if ecbopen=0 then begin
      FreeDellist;
      FreeBlock(Clipboard);
      end;
    end;
end;

var
  locked:boolean=false;

procedure Glossary_ed(LSelf: TLister; var t:taste); {Lister-Tastenabfrage fuer Glossary-Funktion }
var
  en:boolean;
begin
  if (UpperCase(t)='E') and not locked then
  begin
    locked:=true;
    en:=EditNachricht;
    EditFile(FileUpperCase('glossary.cfg'),false,false,false,0,false);
    EditNachricht:=En;
    locked:=false;
    t:=keyesc;
    pushkey(keyctcr);
  end;
end;

initialization
  AktEd := nil;
  Defaults := nil;
  ECBOpen := 0;
finalization
  if assigned(Defaults) then Dispose(Defaults);
  if Assigned(Language) then Dispose(Language);
{
  $Log$
  Revision 1.96  2003/04/25 20:24:48  mk
  - added BloclFormat (<Ctrl-B>)

  Revision 1.95  2003/04/23 19:32:36  mk
  - fixed bug in LoadBlock

  Revision 1.94  2003/04/15 13:33:06  mk
  - fixed Loadblock

  Revision 1.93  2003/04/12 14:33:46  mk
  - fixed LoadBlock: Umbruch is handled correctly again

  Revision 1.92  2003/03/28 23:55:34  mk
  - fixed Bug #65208: 3.8: Quote Reflow-Einstellung fl¸chtig

  Revision 1.91  2002/12/21 05:37:49  dodi
  - removed questionable references to Word type

  Revision 1.90  2002/12/14 22:43:36  dodi
  - fixed some hints and warnings

  Revision 1.89  2002/12/14 09:25:16  dodi
  - removed gpltools and encoder units

  Revision 1.88  2002/12/14 07:31:26  dodi
  - using new types

  Revision 1.87  2002/12/12 11:58:39  dodi
  - set $WRITEABLECONT OFF

  Revision 1.86  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.85  2002/12/04 16:56:57  dodi
  - updated uses, comments and todos

  Revision 1.84  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.83  2002/05/20 07:47:55  mk
  - fixed backup extension: now ExtBak and EditorExtBak

  Revision 1.82  2002/04/06 17:07:47  mk
  - fixed some hard coded '\' to PathDelim and other functions
    should resolve misc problems with linux

  Revision 1.81  2002/02/24 21:00:37  ma
  - don't know how this ')' got lost ;-)

  Revision 1.80  2002/02/24 20:57:35  ma
  - LoadBlock's using Ansistrings now (also fixing some hangs)

  Revision 1.79  2002/02/22 08:25:10  mk
  - fixed crash with external tools

  Revision 1.78  2002/02/21 13:52:30  mk
  - removed 21 hints and 28 warnings

  Revision 1.77  2002/01/22 19:15:27  mk
  - after 3.40 merge fixes

  Revision 1.76  2002/01/19 14:17:01  mk
  - Big 3.40 update part IV

  Revision 1.75  2002/01/17 00:09:58  cl
  - more after-merge fixes

  Revision 1.74  2002/01/16 23:48:17  cl
  - after merge fixes

  Revision 1.73  2002/01/13 15:07:22  mk
  - Big 3.40 Update Part I

  Revision 1.72  2001/12/22 22:16:31  mk
  - fix for Delphi

  Revision 1.71  2001/12/04 10:17:53  mk
  - fixed persistent blocks

  Revision 1.70  2001/10/17 22:11:25  ml
  - removed some range-check Errors

  Revision 1.69  2001/10/17 20:56:24  mk
  - fixed AVs in exit parts of unit

  Revision 1.68  2001/10/17 07:29:11  mk
  - corrected range check error
  - converted some Word to Integer

  Revision 1.67  2001/09/26 23:34:18  mk
  - fixed FPC compile error with newest snapshot:
    Error: Self can only be an explicit parameter in message handlers or class methods

  Revision 1.66  2001/09/21 16:16:47  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.65  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.64  2001/09/09 10:23:20  ml
  - Kylix compatibility stage III
  - compilable in linux

  Revision 1.63  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.62  2001/09/08 09:51:31  mk
  - quote reflow reimplmented

  Revision 1.60  2001/09/06 10:37:27  mk
  - added keys shift-ins and shift-del in Editor

  Revision 1.59  2001/09/03 16:09:34  ml

  - fixed Grey-Keyboard-Editcontrol-feature kills 'J' and 'N' keys - bug

  Revision 1.58  2001/08/10 18:58:18  mk
  - added support for marking blocks with shift-cursor

  Revision 1.57  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.56  2001/07/31 16:18:38  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.55  2001/07/31 13:10:31  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.54  2001/07/28 12:04:08  mk
  - removed crt unit as much as possible

  Revision 1.53  2001/02/26 14:30:26  ma
  - removed non-GPL code part II

  Revision 1.52  2001/02/25 11:34:12  ma
  - removed non-GPL code

  Revision 1.51  2001/02/19 11:48:49  mk
  - fixed crash in config/optionen/editor

  Revision 1.50  2000/12/25 14:02:40  mk
  - converted Lister to class TLister

  Revision 1.49  2000/12/04 17:55:57  hd
  - Workaround for FPC 1.1

  Revision 1.48  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.47  2000/11/14 15:51:26  mk
  - replaced Exist() with FileExists()
}
end.


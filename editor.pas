{   $Id$

    OpenXP editor unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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
  xpglobal,
  sysutils, osdepend,
{$IFDEF unix}
  xpcurses,
{$ENDIF}
  keys,clip,mouse,eddef, encoder, Lister;


const
      EdTempFile      = 'TED.TMP';
      EdConfigFile    = 'EDITOR.CFG';
      EdGlossaryFile  = 'GLOSSARY.CFG';
      EdSelcursor     : boolean = false; { Auswahllistencursor }
      OtherQuoteChars : boolean = false; { Andere Quotezeichen neben > }
      EditResetoldpos : boolean = false;

type
      charr    = array[0..65500] of char;
      charrp   = ^charr;
      EdToken = Byte;
      EdTProc = function(var t:taste):boolean;   { true = beenden }

const
      laststartline    : longint=0;      { fÅr Z-Anzeige }
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

uses  typeform,fileio,inout,maus2,winxp,printerx, xp1, xp2;

const maxgl     = 60;
      minfree   = 12000;             { min. freier Heap }
      asize     = 16;                { sizeof(absatzt)-sizeof(absatzt.cont) }
      maxtokens = 128;
      maxabslen = 16363;

      screenwidth : byte = 80;
      message   : string[40] = '';
      ecbopen   : integer = 0;       { Semaphor fÅr Anzahl der offenen ECB's }

type
//    charr    = array[0..65500] of char;
//    charrp   = ^charr;

      absatzp  = ^absatzt;
      absatzt  = packed record
                   next,prev  : absatzp;
                   size,msize : smallword;       { msize = allokierte Grî·e }
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
                   startline  : longint;      { fÅr Z-Anzeige }
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
                   lastpos    : position;     { fÅr Ctrl-Q-P }

         { disp: 1 = Markierung oberhalb Bildausschnitt, 2=in, 3=unterhalb }

                   block      : array[1..7] of record    { 3..7 = Marker }
                                                 pos  : position;
                                                 disp : byte;
                                               end;
                   blockinverse : boolean;  { Endmarkierung vor Anfangsmark. }
                   blockhidden  : boolean;  { Blockmarkierung ausgeschaltet  }
                   na_umbruch   : byte;
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
      delroot  : delnodep;         { Liste gelîschter Blîcke }
      ClipBoard: absatzp;
      NoCursorsave : boolean;


{ ------------------------------------------------ externe Routinen }

function SeekStr(var data; len: LongWord;
                 var s:string; igcase:boolean):integer; assembler; {&uses ebx, esi, edi}

  { -1 = nicht gefunden, sonst Position }
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
         mov   dl,[edi]                { Key-LÑnge }
  @sblp2:
         mov   al,[esi+ebx]
         or    dh,dh                   { ignore case (gro·wandeln) ? }
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


function FindUmbruch(var data; zlen:integer):integer; assembler; {&uses ebx, esi}
  { rÅckwÑrts von data[zlen] bis data[0] nach erster Umbruchstelle suchen }
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
    errors[2]:='Absatz zu gro·';
    errors[3]:='Fehler beim Laden des Textes';
    errors[4]:='Fehler beim Speichern';
    errors[5]:='Fehler: Datei nicht vorhanden';
    errors[6]:='Text wurde nicht gefunden.';
    askquit:='Text speichern (j/n) ';
    askoverwrite:='Datei existiert schon - Åberschreiben (j/n) ';
    askreplace:='Text ersetzen (Ja/Nein/Alle/Esc)';
    replacechr:='JNA';
    ersetzt:=' Textstellen ersetzt';
    drucken:='Drucken ...';
    menue[0]:='Block';
    menue[1]:='^Kopieren       *';
    menue[2]:='^Ausschneiden   -';
    menue[3]:='^EinfÅgen       +';
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


{ ------------------------------- Liste gelîschter Blîcke verwalten }

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

procedure FreeDellist;             { Liste gelîschter Blîcke freigeben }
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

function allocabsatz(size:integer):absatzp;
var p  : absatzp;
    ms : integer;
begin
  ms:=(size+15) and $fff0;        { auf 16 Bytes aufrunden }
  getmem(p,asize+ms);
  fillchar(p^,asize,0); { next, prev implizit auf NIL setzen, Rest auf 0 }
  p^.size:=size;
  p^.msize:=ms;
  p^.umbruch:=true;
  allocabsatz:=p;
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


{ sbreaks:  Softbreaks auflîsen                                     }
{ umbruch:  0 = alles ohne Umbruch laden                            }
{           1 = nur lange Zeilen ohne Softbreak ohne Umbruch laden  }
{           2 = alles mit Umbruch laden                             }

function LoadBlock(fn:string; sbreaks:boolean; umbruch,rrand:byte):absatzp;
var mfm   : byte;
    s     : string;
    t     : text;
    p     : absatzp;
    tail  : absatzp;
    tbuf  : charrp;
    ibuf  : charrp;
    isize : word;
    sbrk  : boolean;
    root  : absatzp;
    endcr : boolean;          { CR am Dateiende }
    endlf : boolean;          { LF am Zeilenende }
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
    getmem(ibuf,maxabslen);
    getmem(tbuf,4096);
    mfm:=filemode; filemode:=0;
    assign(t,fn); settextbuf(t,tbuf^,4096); reset(t);
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
    endcr:=false;
    srest:=false;
    while (srest or not eof(t)) and assigned(p) do
    begin
      isize:=0;
      sbrk:=false;
      endlf:=false;
      while (srest and (isize=0) or not (eoln(t) or endlf))
            and (isize<maxabslen-255) do
      begin
        if not srest then
          read(t,s)
        else
          srest:=false;
        pp:=cpos(#10,s);
        if pp>0 then begin
          endlf:=(pp=length(s));
          Move(s[1],ibuf^[isize],pp-1);
          inc(isize,pp-1);
          delete(s,1,pp);
          srest:=true;
        end
        else begin
          if (length(s)>40) and sbreaks and eoln(t) and (LastChar(s)=' ')
             and not eof(t)
          then
          begin
            DeleteLastChar(s);
            sbrk:=true;
            readln(t);
          end;
          if (s <> '') then
          begin
          Move(s[1],ibuf^[isize],length(s));
          inc(isize,length(s));
          end;
        end;
      end;
      if eoln(t) and not srest then
      begin
        endcr:=not eof(t);
        readln(t);
      end;
      p:=AllocAbsatz(isize);
      if assigned(p) then begin
        p^.umbruch:=(rrand>0) and
                    ((umbruch=2) or
                     ((umbruch=1) and ((isize<=rrand) or sbrk)));
        Move(ibuf^,p^.cont,isize);
        AppP;
      end;
    end;
    close(t);
    freemem(tbuf);
    freemem(ibuf);
    if endcr then
    begin
      p:=AllocAbsatz(0);
      p^.umbruch:=(umbruch<>0);
      AppP;
    end;
    if ioresult<>0 then error(3);
    end;
  LoadBlock:=root;
end;

function LoadUUeBlock(fn:string):absatzp;
const blen = 45;
var mfm   : byte;
    s     : str90;
    t     : file;
    p     : absatzp;
    tail  : absatzp;
    ibuf  : ^tbytestream;
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

begin
  root:=nil;
  if Fileexists(fn) then
  begin
    getmem(ibuf,sizeof(tbytestream));
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
    while cpos(':',fn)>0 do delete(fn,1,cpos(':',fn));
    while cpos('\',fn)>0 do delete(fn,1,cpos('\',fn));
    s:='begin 644 '+fn;
    while not eof(t) and assigned(p) do begin
      if s='' then begin
        blockread(t,ibuf^,blen,b_read);
        encode_UU(ibuf^,b_read,s);
      end;

      p:=AllocAbsatz(length(s));
      if assigned(p) then begin
        p^.umbruch:=true;
        Move(s[1],p^.cont,length(s));
        AppP;
      end;
      s:='';

      if eof(t) then for b_read:=1 to 3 do begin
        if b_read=1 then s:='`'
        else if b_read=2 then s:='end'
        else if b_read=3 then str(filesize(t),s);
        p:=AllocAbsatz(length(s));
        if assigned(p) then begin
          p^.umbruch:=true;
          Move(s[1],p^.cont,length(s));
          AppP;
        end;
      end;

    end;
    close(t);
    freemem(ibuf,sizeof(tbytestream));
    if ioresult<>0 then error(3);
  end;
  LoadUUeBlock:=root;
end;


function EdLoadFile(ed:ECB; fn:string; sbreaks:boolean; umbruch:byte):boolean;
begin
  with edp(ed)^ do begin
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
    block[1].disp:=3;                 { Anfangsmarkierung am Ende }
    block[2].pos.absatz:=root;
    block[2].disp:=1;                 { Endmarkierung am Anfang }
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
    na_Umbruch:=NeuerAbsatzUmbruch;
    OtherQuoteChars:=iOtherQuoteChars;
    end;
  inc(ecbopen);
  EdInit:=ed;
end;

procedure EdSetTproc(ed:ECB; tp:EdTProc);
begin
  edp(ed)^.tproc:=tp;
end;

{ Positionszeiger in Absatz auf nÑchsten Zeilenbeginn bewegen }
{ Offset mu· auf Zeilenanfang zeigen!                         }

function Advance(ap:absatzp; offset,rand:word):integer;
var zlen : integer;   { ZeilenlÑnge }
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
  if overwrite then MakeBak(fn,'BAK');
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


function EdEdit(ed:ECB):EdToken;
const dispnoshow : boolean = false;
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
     trennzeich : set of char;     { fÅr Wort links/rechts }
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
      // Write(memavail);  Unter 32 Bit nicht sehr sinnvoll
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
        s    : word;
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
      if blockinverse or blockhidden or (block[1].disp=3) or (block[2].disp=1)
      then
        blockstat:=2
      else if (block[1].disp=1) and (block[2].disp>=2) then blockstat:=1
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
              s := s + Sp(w-Length(s)); { mit Space auffÅllen }
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


  {$I editor.inc}

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
//        editfFormatBlock  : BlockFormatieren;
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

procedure Glossary_ed(LSelf: TLister; var t:taste); {Lister-Tastenabfrage fuer Glossary-Funktion }
const 
  locked:boolean=false;
var
  en:boolean;
begin
  if (UpperCase(t)='E') and not locked then 
  begin
    locked:=true;
    en:=EditNachricht;
    EditFile(FileUpperCase('glossary.cfg'),false,false,0,false);
    EditNachricht:=En;
    locked:=false;
    t:=keyesc;
    pushkey(keyctcr);
  end;
end;

initialization
  AktEd := nil;
finalization
  if Assigned(Language) then Dispose(Language);
{
  $Log$
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


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

{ DOS-Shell }

function repfile(const prog,name:string):string;
var p : byte;
begin
  p:=pos('$FILE',ustr(prog));
  if p>0 then
    repfile:=left(prog,p-1)+name+copy(prog,p+5,127)
  else
    repfile:=prog+' '+name;
end;


procedure Resiprog; {$IFNDEF Ver32 } far; {$ENDIF }
begin
  if ExecTestres then
    interr(getres(109));  { Residentes Programm geladen! CrossPoint wird beendet.. }
end;


procedure resetvideo; forward;

{ prog:  Programmname. Falls nicht per PATH erreichbar, dann wird
         ein COMMAND /C gestartet
  space: benîtigter Speicherplatz in KByte (wenn weniger Platz ist,
         wird aber auch nur weniger freigemacht..). 640 -> Maximum.
  cls:   0=nicht lîschen; 1=lîschen, 2=lîschen+Hinweis, 3=Mitte lîschen
         -1=lîschen/25 Zeilen, 4=lîschen/nicht sichern,
         5=nicht lîschen/nicht sichern }

const trackpath : boolean = false;


procedure shell(const prog:string; space:word; cls:shortint);  { Externer Aufruf }
var sm2t     : boolean;
    _xp_     : string[4];
    maussave : mausstat;
    sp       : scrptr;
    de       : integer;

  procedure ShowPar;
  var w1,w2    : word;
      x,y,p,p2 : byte;
  begin
    savecursor;
    cursor(curoff);
    w1:=windmin; w2:=windmax;
    window(1,1,80,25);
    if length(prog)<=74 then
      message(prog)
    else begin
      msgbox(76,4,'',x,y);
      p:=blankposx(prog);
      p2:=71;
      while prog[p2]<>' ' do dec(p2);
      mwrt(x+3,y+1,left(prog,p2-1));
      mwrt(x+3+p,y+2,left(mid(prog,p2+1), 71-p));
      end;
    wkey(15,false);
    windmin:=w1; windmax:=w2;
    closebox;
    restcursor;
  end;

begin
  CloseAblage;
  de:=0;
  if (ParDebFlags and 1<>0) or ShellShowpar then
    ShowPar;
  if maxavail<$4000 then
    rfehler(7)  { Zu wenig freier Speicher fÅr externen Programmaufruf! }
  else
  begin
    { trackpath:=(dospath(0)+'\'=OwnPath) or (dospath(0)=shellpath); }
    getmaus(maussave);
    xp_maus_aus;
    if (cls<>4) and (cls<>5) then begin
      sichern(sp);
      savecursor;
      end;
    TempClose;
    freehelp;

    { -> evtl. normaler Video-Mode }
    sm2t:=m2t;
    attrtxt(7);
    case abs(cls) of
      1,2,4 : begin
                clrscr;
                m2t:=false;
              end;
      3   : begin
              clwin(1,80,4,screenlines-2);
              gotoxy(1,5);
            end;
    end;
    if (cls=2) or (cls=-1) then begin
      if shell25 and (screenlines>25) then
        setvideomode(3);
      if cls=2 then writeln(getres(113));  { Mit EXIT geht''s zurÅck zu CrossPoint. }
      end;
    cursor(curon);

    if (getenv('XPSWAP')<>'') and validfilename(getenv('XPSWAP')) then
      ExecSwapfile:=getenv('XPSWAP')
    else
      ExecSwapfile:=TempPath+SwapFileName;

    ExecTestres:=ParTestres;
    _xp_:='[XP]';
    ExecDeutsch:=deutsch;

    ExecResident:=resiprog;
    ExecUseEms:=SwapToEms;
    ExecUseXms:=SwapToXms;
    case Xec(prog,space,envspace,_xp_,errorlevel) of
      ExecSwaperr : begin
                      tfehler(ioerror(ioresult,getres(110)),60);  { Fehler beim Speicherauslagern! }
                      doserror:=0;
                    end;
      ExecSwapweg : interr(getres(111));  { SWAP-File nicht mehr vorhanden! }
      ExecSwapre  : interr(ioerror(ioresult,getres(112)));  { Fehler beim Lesen des SWAP-Files }
    end;

    de:=doserror;

    if shellkey or (ParDebFlags and 2<>0) or ShellWaitkey then begin
      if deutsch and (random<0.02) then write('Pressen Sie einen SchlÅssel ...')
      else write(getres(12));  { Taste drÅcken ... }
      m2t:=false;
      pushhp(51);
      clearkeybuf;
      wait(curon);
      pophp;
      m2t:=true;
      shellkey:=false;
      end;

    resetvideo;
    cursor(curoff);
    if (cls<>4) and (cls<>5) then holen(sp);
    m2t:=sm2t;
    Disp_DT;
    if (cls<>4) and (cls<>5) then restcursor;
    xp_maus_an(maussave.x,maussave.y);

    if (de<>0) and (de<>4) then
      fehler(ioerror(de,getres(115)));
    end;                            { Fehler bei Programm-Aufruf }

  if trackpath then
    getdir(0,shellpath);
  if dospath(0)<>OwnPath then
    GoDir(OwnPath);
  TempOpen;
end;

Function DosStartSession(Var Data:TStartData):Word; Assembler;
 Asm
        mov     ah, $64
        mov     bx, $0025
        mov     cx, $636C
        push    ds
        lds     si, Data
        int     $21
        pop     ds
 End;

Procedure Start_OS2(const Programm,Parameter,Title:String);
Var StartData:TStartData;
    Temp:String;
    PrgName,PrgParam,PrgTitle:PChar;
Begin
  GetMem(PrgName,256);
  GetMem(PrgParam,256);
  GetMem(PrgTitle,256);
  Temp:='CMD.EXE';
  StrPCopy(PrgName,Temp);
  Temp:='/C'+' '+Programm+' '+Parameter;
  StrPCopy(PrgParam,Temp);
  StrPCopy(PrgTitle,Title);
  With StartData Do Begin
    Length:=SizeOf(TStartData);
    Related:=1;
    FgBg:=0;
    TraceOpt:=0;
    PgmTitle:=PrgTitle;
    PgmName:=PrgName;
    PgmInputs:=PrgParam;
    TermQ:=Nil;
    Environment:=Nil;
    InheritOpt:=0;
    SessionType:=2;
    IconFile:=Nil;
    PgmHandle:=0;
    PgmControl:=0;
    InitXPos:=0;
    InitYPos:=0;
    InitXSize:=0;
    InitYSize:=0;
    Reserved:=0;
    ObjectBuffer:=Nil;
    ObjectBuffLen:=0;
  End;

  DosStartSession(StartData);
  FreeMem(PrgTitle,256);
  FreeMem(PrgParam,256);
  FreeMem(PrgName,256);
End;

function listheadercol:byte; { Headerzeilenfarbe entsprechend Hervorhebungsflag waehlen }
var nt : longint;
begin
  dbreadN(mbase,mb_netztyp,nt);
  listheadercol:=iif(nt and $1000 = 0,col.collistheader,col.collistheaderhigh);
end;

function listcolor(var s:string; line:longint):byte; {$IFNDEF Ver32 } far; {$ENDIF }
var p,p0,ml : byte;
    qn,pdiff: integer;
begin
  listhicol:=col.collisthigh;
  listseekcol:=col.collistfound;
  if line<=exthdlines then
    listcolor:=listheadercol
  else if s='' then
    listcolor:=0
  else if s[1]<=^c then
    listcolor:=iif((length(s)>1) and kludges,col.collistmarked,$ff)
  else begin
    p:=1;
    ml:=min(length(s),6);
    while (p<=ml) and ((s[p]=' ') or (s[p]=^I)) do
      inc(p);
    p0:=p;
    qn:=0;
    repeat
       while (p<=length(s)) and (p-p0<6) and
       (
         (s[p]<>'>') and
         (not OtherQuoteChars or not (s[p] in QuoteCharSet))
       )
       do inc(p);
      pdiff:=p-p0;

      if (p<=length(s)) and (s[p]='>') or
         (OtherQuoteChars and (s[p] in QuoteCharSet)) then
      begin
        inc(qn);
        p0:=p;
      end;
      inc(p);
    until (p>length(s)) or (pdiff=6);
    if qn<1 then
      listcolor:=0
    else begin
      listcolor:=col.collistquote[min(qn,iif(QuoteColors,9,1))];
      listhicol:=col.collistqhigh[min(qn,iif(QuoteColors,9,1))]
      end;
    end;
end;


{ 0=normal, -1=Minus, 1=Plus, 2=links, 3=rechts, 4=P/B/^P/^B (ListKey),
  5="0", 6=PgUp, 7=PgDn }

function listfile(const name,header:string; savescr,listmsg:boolean;
                  cols:shortint):shortint; { Lister }
var brk    : boolean;
    p      : scrptr;
    oldm   : byte;
    msg    : boolean;
    lf     : pathstr;
    pp     : byte;
    lt     : byte;
    lfirst : byte;     { Startzeile Lister }
    lofs   : word;     { Ladeposition Datei }
    dphb   : byte;     { Uhr Hintergrundfarbe Backup }

  procedure ShowMsgHead;
  var t : text;
      s : string;
      i : integer;
  begin
    assign(t,name); reset(t);
    attrtxt(listheadercol);
    for i:=1 to exthdlines do begin
      readln(t,s);
      if s[1]<>'-' then mwrt(1,lfirst,' '+forms(s,79))
      else mwrt(1,lfirst,dup(80,'ƒ'));
      inc(lfirst);
      inc(lofs,length(s)+2);
      end;
    close(t);
    exthdlines:=0;
    lfirst:=min(lfirst,screenlines-5);
  end;

begin
  listexit:=0;
  listseekcol:=col.collistfound;
  if Suchergebnis then listshowseek:=true 
    else listshowseek:=false; 
  if varlister<>'' then begin
    lf:=repfile(VarLister,name);
    pp:=pos('$TYPE',ustr(lf));
    if pp>0 then begin
      lt:=iif(listmsg,iif(listkommentar,2,1),0);
      lf:=left(lf,pp-1)+strs(lt)+mid(lf,pp+5);
      end;
    shell(lf,ListerKB,1);
    if errorlevel in [100..110] then ExtListKeys;
    end
  else begin
    if savescr then sichern(p);
    lfirst:=iif(listvollbild,1,4); lofs:=0;
    if listvollbild then begin                     { Bei Vollbild-Lister : }
      if {not listmsg or} not listuhr then m2t:=false { Uhr nur im Message Lister... }
      else begin
        dphb:=dphback;
        if ListFixedhead and (exthdlines>0) then   { Wenn fester Header }
          dphback:=listheadercol                   { dann Uhr aktiv mit Headerfarbe }
        else begin
          dphback:=col.colliststatus;              { bei freiem Header }
          timey:=2;                                { Uhr in Zeile 2 und Statuszeilenfarbe }
          end;
        end;
      end;
    if ListMsg and ListFixedHead then
      ShowMsgHead;

    openlist(1,iif(_maus and listscroller,79,80),lfirst,
             iif(listvollbild,screenlines,screenlines-fnkeylines-1),
             iif(listvollbild,1,4),'/F1/MS/S/APGD/'+iifs(listendcr,'CR/','')+
             iifs(_maus and ListScroller,'VSC:080/','')+
             iifs(listmsg,'ROT/',''));

    if listwrap {or listkommentar} then
      lister.listwrap(iif(_maus and listscroller,78,80));
    if listmsg and ConvIso then list_convert(ISO_conv);
    if not ListAutoscroll then listNoAutoscroll;
    msg:=(_filesize(name)>1024*100);
    if msg then rmessage(130);    { 'Lade Datei ...' }
    list_readfile(name,lofs);
    if msg then closebox;
    listheader(header);
    listTp(listExt);   llh:=listmsg;
    oldm:=ListMakros;
    if listmsg then ListMakros:=8;
    if cols<>0 then begin
      listCFunc(listColor);
      if cols and 2<>0 then begin
        listDLproc(listDisplay);
        xp1o.ListXHighlight:=ListHighlight;
        end;
      end;
    pushhp(39);
    if _maus and listscroller and listvollbild then begin
      attrtxt(col.colliststatus);
      mwrt(1,lfirst,sp(80));
      end;

    list_infos(listmsg);

    list(brk);
    Listunvers:=0; Listhalten:=0; Listflags:=0;
    pophp;
    ListMakros:=oldm;
    if listvollbild and listuhr {and ListMsg}
    then begin
      dphback:=dphb;                              { Uhrfarbe resetten  }
      if not (ListFixedhead and (exthdlines>0))
        then timey:=1;                            { und evtl. Position }
      end;
    m2t:=true;
    if savescr then holen(p);
    closelist;
    end;
  exthdlines:=0;
  llh:=false;
  if listexit<>4 then                    { Wenn nicht Editor gestartet wird...        }
    otherquotechars:=otherqcback;        { Status der Quotechars '|' und ':' resetten }
  if (listexit<>-4) and (ListWrapToggle) then
  begin
    ListWrap:=ListWrapBack;              { Status Wortumbruch resetten }
    ListWrapToggle:=false;
  end;
  listfile:=listexit;
  ListShowSeek:=false;
end;


procedure RemoveEOF(const fn:pathstr);
var f : file;
    b : byte;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult<>0 then exit;    { Datei nicht gesichert }
  if filesize(f)>0 then begin
    seek(f,filesize(f)-1);
    blockread(f,b,1);
    if b=26 then begin
      seek(f,filesize(f)-1);
      truncate(f);
      end;
    end;
  close(f);
end;


{ reedit: Nachbearbeiten einer XP-erzeugten-Nachricht - }
{         TED-Softreturns zurÅckwandeln                 }

procedure editfile(name:pathstr; nachricht,reedit,senden:boolean;
                   keeplines:byte;ed_ukonv:boolean);
var
    bak : string[3];
    ms  : boolean;
begin
  if ((exteditor=3) or ((exteditor=2) and nachricht) or
     (_filesize(name)*1.2>memavail-16383)) and (VarEditor<>'')
     and (VarEditor[1]<>'*') then begin
    ms:=shell25; shell25:=edit25;
    shell(repfile(VarEditor,name),EditorKB,-1);
    shell25:=ms;
    removeeof(name);
    bak:=BAKext;
    end
  else begin
    if nachricht then pushhp(54);
    TED(name,reedit,keeplines,ed_ukonv,nachricht,senden);
    if nachricht then pophp;
    if nachricht and (left(VarEditor,1)='*') then begin
      delfirst(VarEditor);
      shell(repfile(VarEditor,name),EditorKB,3);
      insert('*',VarEditor,1);
      end;
    bak:='BAK';
    end;
  if bak<>'' then
  begin
    name := ChangeFileExt(name, '.' + bak);
    if exist(name) then _era(name);      { .BAK lîschen }
  end;
end;


{ Achtung! ShellPath kann mit oder ohne '\' am Ende sein! }

procedure dosshell;
{$IFDEF DPMI }
const ShellBatch = 'DPMISHELL.BAT';
{$ENDIF }

  {$IFNDEF DPMI}
  function environment:string;
  begin
    if envspace=0 then environment:=''
    else environment:=' /E:'+strs(envspace);
  end;
  {$ENDIF }

begin
  if DisableDos then
    fehler(getres(116))   { DOS-Shell hier nicht mîglich }
  else begin
    GoDir(ShellPath);
    if ioresult<>0 then GoDir(ownpath);
    trackpath:=true;
    {$IFDEF DPMI}
      assign(t,TempPath+ShellBatch);
      rewrite(t);
      writeln(t,'@set prompt=[XP] '+getenv('PROMPT'));
      writeln(t,'@',getenv('COMSPEC'));
      close(t);
      shell(TempPath+ShellBatch,620,2);
      _era(TempPath+ShellBatch);
    {$ELSE}
      shell(getenv('COMSPEC')+environment,640,2);
    {$ENDIF}
    trackpath:=false;
    end;
end;
{
  $Log$
  Revision 1.24.2.14  2001/12/05 19:27:36  my
  JG+MY:- Kommentare ergÑnzt und korrigiert

  Revision 1.24.2.13  2001/12/04 23:01:02  my
  MY:- Fix fÅr Wortumbruch-Toggle (<Ctrl-W>) im Lister (beim AusfÅhren
       konnte es vorkommen, da· XP die Nachricht beantworten wollte,
       jetzt exit code -4 statt 4)

  Revision 1.24.2.12  2001/12/02 15:52:29  my
  MY:- Im Nachrichten-Lister kann der Wortumbruch in Spalte 80 jetzt mit
       <Ctrl-W> nicht-permanent umgeschaltet werden.

  Revision 1.24.2.11  2001/10/22 23:04:18  my
  MY:- Option "Parken" beim Editieren von Nachrichten erscheint nur noch,
       wenn es sich auch um eine zu versendende Nachricht handelt (also
       nicht bei N/é/T z.B.)

  Revision 1.24.2.10  2001/09/16 20:19:33  my
  JG+MY:- Markierung der bei der letzten Nachrichten-Suche verwendeten
          Suchbegriffe im Lister (inkl. Umlaut- und Wildcardbehandlung):
          Nach Suche automatisch aktiv, ansonsten durch "E" schaltbar. Mit
          <Tab> springt der Cursorbalken die nÑchste Zeile mit einem
          markierten Suchbegriff an.

  JG+MY:- Text-Markiersuche im Lister mit "S": mehrere Suchbegriffe,
          Suchoptionen (z.B. umlautunabhÑngige Suche), Suchbegriff-History
          und Suchbegriffs-Bibliothek verfÅgbar. "Alte" Suchfunktionen
          jetzt Åber <Ctrl-S> (frÅher "S") bzw. wie bisher Åber <Shift-S>
          erreichbar.

  JG+MY:- Beim Editieren von Nachrichten gibt es im "énderungen
          speichern?"-Dialog die Option "Parken", die direkt ein "P" ans
          Sendefenster weiterleitet.

  JG+MY:- Uhr im Archiv-Viewer aktiviert

  JG+MY:- öbergabe der Msg-Flags nach LISTER.PAS verlagert (Overlay)

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.24.2.9  2001/08/12 11:20:29  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.24.2.8  2001/08/11 20:16:29  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.24.2.7  2001/07/31 16:31:30  mk
  - fixed last commit, now using ChangeFileExt

  Revision 1.24.2.6  2001/07/30 08:24:45  mk
  - fixed handling of bak files in EditFile

  Revision 1.24.2.5  2001/02/24 14:37:53  mk
  - letzetn Checkin rueckgaengig gemacht

  Revision 1.24.2.4  2001/02/23 12:39:05  mk
  - Uhr im Lister mit /w abschalten

  Revision 1.24.2.3  2001/01/10 17:39:03  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.24.2.2  2000/12/29 02:22:20  mk
  - palette sichern verbessert

  Revision 1.24.2.1  2000/12/19 00:23:57  mk
  - Farbalette vor Schell/Videomodus umschalten sichern

  Revision 1.24  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.23  2000/05/10 10:31:26  hd
  - Fixe Verdrahtung 80x25 unter Curses aufgehoben

  Revision 1.22  2000/05/07 18:00:45  mk
  - Fix in ShowPar fuer extrem lange Kommaondozeilen

  Revision 1.21  2000/05/04 18:43:15  jg
  - Lister: eigene Headerfarbe fuer hervorgehobene Nachrichten
    entsprechender Menuepunkt unter Config/Anzeige/Farben/Lister

  Revision 1.20  2000/04/24 13:17:39  jg
  - Anzeige der Nachrichtenflags (Halten,Wiedervorlage etc) im Lister
  - "H" im Lister kann jetzt das Halteflag auch ausschalten
  - "V" im Lister schaltet das Wiedervorlageflag Ein/Aus

  Revision 1.19  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.18  2000/04/01 07:41:38  jg
  - "Q" im Lister schaltet otherquotechars (benutzen von | und :) um.
    neue Einstellung wird dann auch beim Quoten verwendet
  - Hilfe aktualisiert, und Englische Hilfe fuer
    Config/Optionen/Allgemeines auf Stand gebracht.

  - Externe-Viewer (Windows): "START" als Allroundviewer
    funktioniert jetzt auch mit der Loeschbatch-Variante
  - Text fuer MIME-Auswahl in englische Resource eingebaut

  Revision 1.17  2000/03/25 11:46:10  jg
  - Lister: Uhr wird jetzt auch bei freiem Nachrichtenkopf eingeblendet
  - Config/Optionen/Lister: Schalter ListUhr zum (de)aktivieren der Uhr

  Revision 1.16  2000/03/23 15:47:23  jg
  - Uhr im Vollbildlister aktiv
    (belegt jetzt 7 Byte (leerzeichen vorne und hinten)

  Revision 1.15  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.14  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.13  2000/03/08 22:13:31  rb
  nicht mehr benîtigte Routinen fÅr OS/2 Programmaufruf entfernt

  Revision 1.12  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.11  2000/03/06 05:14:15  jg
  Schînheitsfix fÅr Lister : Uhr bei riesigen Nachrichten rechtzeitig abschalten

  Revision 1.10  2000/03/02 21:19:51  jg
  - Uhr beim verlassen des Nachrichtenheaders eleganter deaktiviert

  Revision 1.9  2000/02/27 08:24:57  jg
  -Strings.StrPCopy wird wieder benutzt...

  Revision 1.8  2000/02/26 18:14:47  jg
  - StrPCopy in Xp1s.inc integriert
  - Suche aus Archivviewer wieder zugelassen
    (zwecks Headereintregsuche im "O" Fenster)

  Revision 1.7  2000/02/24 23:50:11  rb
  Aufruf externer Viewer bei OS/2 einigermassen sauber implementiert

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

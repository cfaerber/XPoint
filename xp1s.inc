{   $Id$

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

{ DOS-Shell }

function repfile(const prog,name:string):string;
var p : byte;
begin
  p:=pos('$FILE',UpperCase(prog));
  if p>0 then
    result:=LeftStr(prog,p-1)+name+copy(prog,p+5,127)
  else
    result:=prog+' '+name;
end;

const trackpath : boolean = false;

{ call of external program. errorlevel returned in gloval var errorlevel.
  errorlevel is negative if call was not performed (program not found).
  prog may be a batch file or using pipes. program extension MAY be specified
  as well as program path.
  CAUTION: Automatically chdirs back to "ownpath"!
  cls:   0=nicht loeschen; 1=loeschen, 2=loeschen+Hinweis, 3=Mitte loeschen
         -1=loeschen/25 Zeilen, 4=loeschen/nicht sichern,
         5=nicht loeschen/nicht sichern }
procedure shell(const prog:string; space:word; cls:shortint);

  { returned: errorlevel called program returned if call successful
    negative errorlevel if program not found }
  function Xec(command:string; const prompt:string):Integer;

  {$ifdef UnixFS}
  {$ifdef Unix}
  begin
    Debug.TempCloseLog(False);
    Debug.DebugLog('xp1s','saving terminal state', DLInform);
    def_prog_mode();           { save current tty modes }
    endwin();                  { restore original tty modes }
{$IFDEF Kylix}
    Result:=libc.system(PChar(command));
{$ELSE}
    Result:=linux.shell(command);
{$ENDIF}
    Debug.DebugLog('xp1s','reload terminal state', DLInform);
    refresh();                 { restore save modes, repaint screen }
    Debug.TempCloseLog(True);
    Debug.DebugLog('xp1s','called program "'+command+'": result '+
                   strs(Result),iif(Result=0,DLInform,DLError));
  end;
  {$else}
  {$error Please implement this function for your OS}
  {$endif}
  {$else} // OS is Dos, Win or OS2
  var
    pp    : byte;
    parameters,commandsave : string;
    callviacli : boolean; // "call via command line interpreter" (usually via COMMAND /C)
  begin
    pp:=cPos(' ',command);
    if pp=0 then parameters:=''
    else begin
      parameters:=trim(Mid(command,pp+1));
      command:=LeftStr(command,pp-1);
    end;
    command:=FileUpperCase(command);

    callviacli:=(cPos('|',parameters)>0) or (cPos('>',parameters)>0) or (cPos('<',parameters)>0);
    commandsave:=command;
    if not callviacli then begin
      command:=FindExecutable(command);
      if (command='') // file not found, assume it is built-in in cli
         or(UpperCase(ExtractFileExt(command))= extBatch) // batch files have to be run via cli
         or(ExtractFileExt(command)='') // FindExecutable WILL find "copy" as built-in in cli
        then begin
        callviacli:=true; command:=commandsave;
      end;
    end;
    if trim(command)='' then begin result:=-100; exit end;
    if callviacli then begin
      parameters:=' /c '+command+' '+parameters;
      command:=getenv('comspec');
    end;

    Debug.TempCloseLog(False);
    Result:=SysExec(command, parameters);
    Debug.TempCloseLog(True);
    Debug.DebugLog('xp1s','called program "'+command+'" with parameters "'+parameters+'": result '+
                   strs(Result),iif(Result=0,DLInform,DLError));
  end;
  {$endif}

  procedure ShowPar;
  var
      x,y,p,p2 : Integer;
  begin
    savecursor;
    cursor(curoff);
    if length(prog)<=74 then
      message(prog)
    else begin
      msgbox(76,4,'',x,y);
      p:=blankposx(prog);
      p2:=71;
      while prog[p2]<>' ' do dec(p2);
      mwrt(x+3,y+1,LeftStr(prog,p2-1));
      mwrt(x+3+p,y+2,LeftStr(mid(prog,p2+1), 71-p));
      end;
    wkey(15,false);
    closebox;
    restcursor;
  end;

var
  sm2t     : boolean;
  maussave : mausstat;
  sp       : scrptr;

begin
  if (ParDebFlags and 1<>0) or ShellShowpar then
    ShowPar;
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
            clwin(1,ScreenWidth,4,screenlines-2);
            gotoxy(1,5);
          end;
  end;
  {$ifdef Win32}
  // todo: adjust screen size with Win9x/ME only, not with WinNT/2000
  SysSetScreenSize(25,80);
  Window(1,1,80,25);
  {$endif}
  if (cls=2) or (cls=-1) then
  begin
    if shell25 and (screenlines>25) then
      SysSetScreenSize(25, 80);
    if cls=2 then writeln(getres(113));  { Mit EXIT geht''s zurÅck zu CrossPoint. }
  end;
  cursor(curon);

  Errorlevel := Xec(prog,'[XP]');

  if shellkey or (ParDebFlags and 2<>0) or ShellWaitkey then
  begin
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

  SysSetBackintensity;
  SetScreenSize;
  cursor(curoff);
  if (cls<>4) and (cls<>5) then holen(sp);
  m2t:=sm2t;
  Disp_DT;
  if (cls<>4) and (cls<>5) then restcursor;

  xp_maus_an(maussave.x,maussave.y);

  if (ErrorLevel<0) and (ErrorLevel<>-4) then
    fehler(ioerror(-ErrorLevel,getres(115)));   { Fehler bei Programm-Aufruf }

  if trackpath then getdir(0,shellpath);
  SetCurrentDir(OwnPath);
  TempOpen;
end;

{ Execute an external program and add any files created in current dir to SL }
function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;
var dir1,dir2: TDirectory; curdir,newfiles: string; i,j: Integer; fileexisted: boolean;
begin
  curdir:=GetCurrentDir;
  dir1:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  Shell(prog,space,cls);
  result:=errorlevel;
  newfiles:='';
  SetCurrentDir(curdir);
  dir2:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  for i:=0 to dir2.Count-1 do begin
    fileexisted:=false;
    for j:=0 to dir1.Count-1 do
      if dir2.Name[i]=dir1.Name[j] then fileexisted:=true;
    if not fileexisted then begin
      SL.Add(ExpandFilename(dir2.Name[i]));
      if newfiles<>'' then newfiles:=newfiles+', ';
      newfiles:=newfiles+ExpandFilename(dir2.Name[i]);
      end;
    end;
  dir1.destroy; dir2.destroy;
  Debug.DebugLog('xpnetcall','new files created by external program: '+newfiles,DLDebug);
  SetCurrentDir(OwnPath);
end;

function listheadercol:byte; { Headerzeilenfarbe entsprechend Hervorhebungsflag waehlen }
var nt : longint;
begin
  dbreadN(mbase,mb_netztyp,nt);
  listheadercol:=iif(nt and $1000 = 0,col.collistheader,col.collistheaderhigh);
end;

function listcolor(const s:string; line:longint):byte;
var p,p0,ml : byte;
    qn,pdiff: integer;
begin
  listhicol:=col.collisthigh;
  // highlight header lines
  if line<exthdlines then
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
       while (s <> '') and (p<=length(s)) and (p-p0<6) and
       (
         (s[p]<>'>') and
         (not OtherQuoteChars or not (s[p] in QuoteCharSet))
       )
       do inc(p);
      pdiff:=p-p0;

      if (s <> '') and (p<=length(s)) and (s[p]='>') or
         (OtherQuoteChars and (p<=length(s)) and (s[p] in QuoteCharSet)) then
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

function listfile(name,header:string; savescr,listmsg:boolean;
                  utf8:boolean;
                  cols:shortint):shortint; { Lister }
var
    List   : TLister;
    p      : scrptr;
    oldm   : byte;
    msg    : boolean;
    lf     : string;
    pp     : byte;
    lt     : byte;
    lfirst : byte;     { Startzeile Lister }
    lofs   : word;     { Ladeposition Datei }
    dphb   : byte;     { Uhr Hintergrundfarbe Backup }
    wrapb  : boolean;  { Backup no_ListWrapToggle }

    OldTCS,OldLCS: TMimeCharsets;

  procedure ShowMsgHead;
  var t : text;
      s : string;
      i : integer;
  begin
    assign(t,name); reset(t);
    attrtxt(listheadercol);
    if UTF8 then SetLogicalOutputCharset(csUTF8);
    for i:=1 to exthdlines do begin
      readln(t,s);
      mwrt(1,lfirst,' '+forms(s,79+ScreenWidth-80));
      inc(lfirst);
      inc(lofs,length(s)+2);
      end;
    if UTF8 then SetLogicalOutputCharset(csCP437);
    close(t);
    exthdlines:=0;
    lfirst:=min(lfirst,screenlines-5);
  end;

begin
  listexit:=0;
  wrapb:=no_ListWrapToggle;
  no_ListWrapToggle:=false;
  dphb := 0;
  if varlister<>'' then begin
    lf:=repfile(VarLister,name);
    pp:=pos('$TYPE',UpperCase(lf));
    if pp>0 then begin
      lt:=iif(listmsg,iif(listkommentar,2,1),0);
      lf:=LeftStr(lf,pp-1)+strs(lt)+mid(lf,pp+5);
      end;
    shell(lf,0,1);
    if errorlevel in [100..110] then ExtListKeys;
    end
  else begin
    if savescr then sichern(p);
    lfirst:=iif(listvollbild,1,4); lofs:=0;
    if listvollbild then begin                      { Bei Vollbild-lister : }
      if not listmsg or not listuhr then m2t:=false { Uhr nur im Message Lister... }
      else begin
        dphb:=dphback;
        if listmsg and ListFixedhead and (exthdlines>0) then   {   Wenn fester Header }
          dphback:=listheadercol                   {   dann Uhr aktiv mit Headerfarbe }
        else begin
          dphback:=col.colliststatus;              {   bei freiem Header }
          timey:=2;                                {   Uhr in Zeile 2 und Statuszeilenfarbe}
          end;
        end;
      end;
    if ListMsg and ListFixedHead then
    begin
      ShowMsgHead;
      if UTF8 then 
      begin
        OldTCS := GetConsoleOutputCharset;
        OldLCS := GetLogicalOutputCharset;
        SetConsoleOutputCharset(csUTF8);
        SetLogicalOutputCharset(csCP437);
      end;
    end;
      
    List := TLister.CreateWithOptions(1,iif(_maus and listscroller,screenwidth-1,screenwidth),lfirst,
             iif(listvollbild,screenlines,screenlines-fnkeylines-1),
             iif(listvollbild,1,4),'/F1/MS/S/APGD/'+iifs(listendcr,'CR/','')+
             iifs(_maus and ListScroller,'VSC/','')+
             iifs(listmsg,'ROT/',''));
    if listwrap {or listkommentar} then
      list.Stat.WrapPos := iif(_maus and listscroller,78,80)+ScreenWidth-80;
//!!    if listmsg and ConvIso then List.OnConvert := ISO_conv;
    if not ListAutoscroll then List.Stat.Autoscroll := false;
    msg:=(_filesize(name)>1024*100);
    if msg then rmessage(130);    { 'Lade Datei ...' }
    List.ReadFromFile(name,lofs);
    if msg then closebox;
    List.HeaderText := header;
    List.OnKeypressed := listExt;
    List.UTF8Mode := utf8;
    llh:=listmsg;
    oldm:=ListMakros;
    if listmsg then ListMakros:=8;
    if cols<>0 then
    begin
      List.OnColor := listColor;
      if cols and 2<>0 then
      begin
        List.OnDisplayLine := Listdisplay;
        xp1o.ListXHighlight:=ListHighlight;
        end;
      end;
    pushhp(39);
    if _maus and listscroller and listvollbild then begin
      attrtxt(col.colliststatus);
      mwrt(1,lfirst,sp(ScreenWidth));
      end;

    if listmsg then
    begin
      dbReadN(mbase,mb_halteflags,listhalten);
      dbReadN(mbase,mb_unversandt,listunvers);
      dbReadN(mbase,mb_flags,listflags);
      end
    else begin
      Listunvers:=0; Listhalten:=0; Listflags:=0;
      end;

    List.Show;
    Listunvers:=0; Listhalten:=0; Listflags:=0;
    pophp;
    ListMakros:=oldm;

    if ListMsg and ListFixedHead then
    begin
      if UTF8 then 
      begin
        SetConsoleOutputCharset(OldTCS);
        SetLogicalOutputCharset(OldLCS);
      end;
    end;
    
    if listvollbild and listuhr and ListMsg then begin
     dphback:=dphb;                        {Uhrfarbe reseten}
     if not Listfixedhead then timey:=1;   {Und evtl. Position}
     end;
   m2t:=true;
    if savescr then holen(p);
    List.Free;
  end;
  exthdlines:=0;
  llh:=false;
  if listexit<>4                         { Wenn nicht Editor gestartet wird... }
    then otherquotechars:=otherqcback;   { Status der Quotechars '|' und ':' reseten }
  listfile:=listexit;
  no_ListWrapToggle:=wrapb;
end;


procedure RemoveEOF(fn:string);
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

procedure editfile(name: string; nachricht,reedit,senden:boolean;
                   keeplines:byte;ed_ukonv:boolean);
var
    bak : string;
    ms  : boolean;
begin
  if ((exteditor=3) or ((exteditor=2) and nachricht)) and (VarEditor<>'')
     and (VarEditor[1]<>'*') then begin
    ms:=shell25; shell25:=edit25;
    shell(repfile(VarEditor,name),0,-1);
    shell25:=ms;
    removeeof(name);
    bak:=BAKext;
    end
  else begin
    if nachricht then pushhp(54);
    TED(name,reedit,keeplines,ed_ukonv,nachricht,senden);
    if nachricht then pophp;
    if nachricht and (FirstChar(VarEditor)='*') then begin
      DeleteFirstChar(VarEditor);
      shell(repfile(VarEditor,name),0,3);
      insert('*',VarEditor,1);
      end;
    bak:='BAK';
    end;
  if bak<>'' then
    SafeDeleteFile(ChangeFileExt(Name, '.'+bak)); { .BAK lˆschen }
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
    SetCurrentDir(ShellPath);
    if ioresult<>0 then SetCurrentDir(ownpath);
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
{
  $Log$
  Revision 1.76  2002/03/25 22:03:08  mk
  MY:- Anzeige der Stammbox-Adresse unterhalb der MenÅleiste korrigiert
       und Åberarbeitet (bei aktivierter Option "C/A/D/Stammbox-Adresse
       anzeigen"):
       - VollstÑndige Adresse (statt nur Feld "Username") inkl. Domain
         wird angezeigt;
       - Alias-Points werden berÅcksichtigt (RFC/UUCP und ZConnect);
       - Realname wird in Klammern angezeigt (falls es sich um einen
         Netztyp mit Realnames handelt) und ggf. automatisch gekÅrzt, wenn
         die GesamtlÑnge von Adresse und Realname grî·er als 76 Zeichen
         ist;
       - Bei einem Wechsel des Netztyps der Stammbox wird die Anzeige
         der Absenderadresse unterhalb der MenÅleiste unmittelbar nach dem
         Wechsel aktualisiert.

  Revision 1.75  2002/03/03 15:45:54  cl
  - changed TListerColorEvent's first parameter from var => const

  Revision 1.74  2002/01/28 20:32:25  mk
  - completed 3.40 merge, source is compilable for dos and win
    linux is still untested

  Revision 1.73  2002/01/22 19:15:29  mk
  - after 3.40 merge fixes

  Revision 1.72  2002/01/21 23:30:12  cl
  - post-3.40 merge fixes

  Revision 1.71  2002/01/03 19:19:13  cl
  - added and improved UTF-8/charset switching support

  Revision 1.70  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.69  2001/10/20 21:35:48  ml
  - saving/restoring terminal for calling external programs works now

  Revision 1.68  2001/10/20 19:11:10  ml
  - call done and init-curses when calling external progs (redraw needed yet)

  Revision 1.67  2001/10/17 20:55:21  cl
  - Fixed range check error

  Revision 1.66  2001/10/10 20:38:52  mk
  - removed (unnecessary) ScreenWidth from Lister option VSC
  - use correct scrollbar position with more than 80 screen columns
  - show scrollbar only if listscroller is enabled

  Revision 1.65  2001/10/07 17:12:30  cl
  - added charset recoding for external editors
    and corresponding config option

  Revision 1.64  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.63  2001/09/08 16:29:32  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.62  2001/09/07 13:54:19  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.61  2001/08/28 08:16:03  mk
  - added some const parameters

  Revision 1.60  2001/08/12 11:50:36  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.59  2001/08/11 23:06:29  mk
  - changed Pos() to cPos() when possible

  Revision 1.58  2001/08/01 00:41:35  mk
  - fixed bak file deletion in EditFilename

  Revision 1.57  2001/07/31 13:10:32  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.56  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.55  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.54  2001/05/20 12:21:06  ma
  - added ShellTrackNewFiles

  Revision 1.53  2001/05/08 17:41:32  ma
  - fixed: cli built-in commands could not be executed

  Revision 1.52  2001/04/03 13:33:15  ma
  - screen size set to 80x25 with Win32 now
  - todo: don't set screen size with WinNT/2000

  Revision 1.51  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.50  2001/03/03 10:57:43  ml
  - compilable under linux

  Revision 1.49  2001/01/15 23:27:35  mk
  - removed Editorkb, Listerkb

  Revision 1.48  2001/01/06 16:13:49  ma
  - shell call using FindExecutable now

  Revision 1.47  2001/01/05 18:35:03  ma
  - moved Exxec unit into XP1 unit

  Revision 1.46  2001/01/04 23:16:40  ma
  - refined debug logs

  Revision 1.45  2000/12/26 15:40:08  mk
  - fixed highlight list header

  Revision 1.44  2000/12/25 14:02:41  mk
  - converted Lister to class TLister

  Revision 1.43  2000/11/14 15:51:28  mk
  - replaced Exist() with FileExists()
}
end;



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

{ CrossPoint - Editor }

{$I XPDEFINE.INC}

unit xpe;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,winxp,maus2,resource,maske, sysutils,
  eddef,editor,xpglobal, xp0,xp1o,xp1help,xp1input,xpkeys,xp5,xp10;


const EditXkeyfunc : EdTProc = nil;

procedure TED(var fn:string; reedit:boolean; keeplines:byte; ukonv:boolean);
procedure SigEdit(datei:string);
procedure EditText;
procedure Notepad;
procedure EditSetbetreff(betr:string; maxlen:byte);
function  EditGetBetreff:string;

function  EditKeyFunc(var t:taste):boolean;

function  EditQuitfunc(ed:ECB):taste;   { Speichern: J/N/Esc }
function  EditOverwrite(ed:ECB; fn:string):taste;   { öberschr.: J/N/Esc }
procedure EditMessage(txt:string; error:boolean);
procedure EditAskFile(ed:ECB; var fn:string; save,uuenc:boolean);
function  EditFindfunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
function  EditReplfunc(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
procedure EditCfgFunc(var cfg:EdConfig; var brk:boolean);
procedure Editor_options;


implementation  {--------------------------------------------------------}

uses  xp1,xp6;

const
      doautosave: boolean = false;

var
      edbetreff : string;
      edbmaxlen : byte;      { maximale BetrefflÑnge }
      EdCfg     : EdConfig;


procedure setcolors;
var ecol : EdColrec;
    i    : integer;
begin
  ecol.coltext:=col.coledittext;
  ecol.colstatus:=col.coleditstatus;
  ecol.colmarked:=col.coleditmarked;
  for i:=1 to 9 do
    if quotecolors then
      ecol.colquote[i]:=col.coleditquote[i]
    else
      ecol.colquote[i]:=col.coleditquote[1];
  ecol.colendmark:=col.coleditendmark;
  ecol.colmenu:=col.coleditmenu;
  ecol.colmenuhi:=col.coleditmenuhi;
  ecol.colmenuinv:=col.coleditmenuinv;
  ecol.colmenuhiinv:=col.coledithiinv;
  EdSetColors(ecol);
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }


function EditQuitfunc(ed:ECB):taste;   { Speichern: J/N/Esc }
var brk : boolean;
begin
  case ReadIt(length(getres2(2500,2))+9,getres2(2500,1),getres2(2500,2),1,brk) of
    0 : EditQuitFunc:=keyesc;
    1 : EditQuitFunc:=_jn_[1];
    2 : EditQuitFunc:=_jn_[2];
  end;
  freeres;
end;

function EditOverwrite(ed:ECB; fn:string):taste;   { öberschr.: J/N/Esc }
var brk : boolean;
begin
  if Overwrite(fn,false,brk) then EditOverwrite:=_jn_[1]
  else EditOverwrite:=iifs(brk,keyesc,_jn_[2]);
end;

procedure EditMessage(txt:string; error:boolean);
begin
  if error then fehler(txt)
  else hinweis(txt);
end;

procedure EditAskFile(ed:ECB; var fn:string; save,uuenc:boolean);
var useclip : boolean;
begin
  if save then fn:='' else fn:=SendPath+WildCard;
  useclip:=false;          { 'Block speichern' / 'Block laden' }
  if readfilename(getres(iif(save,2504,2505))
                  +iifs(uuenc,' '+getres(2509),'')
                  ,fn,true,useclip) then begin
{$IFDEF UNIXFS}
    if not multipos('/',fn)
{$ELSE}
    if not multipos('\:',fn)
{$ENDIF}
      then fn:=sendpath+fn;
    end
  else
    fn:='';
end;

function EditFindfunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
var x,y : Integer;
    brk : boolean;
begin
  dialog(ival(getres2(2506,0)),5,getres2(2506,1),x,y);    { 40 / 'Suchen' }
  maddstring(3,2,getres2(2506,2),txt,ival(getres2(2506,0))-   { 'Text  ' }
                   length(getres2(2506,2))-7,MaxFindLen,''); mnotrim;
  maddbool(3,4,getres2(2506,3),igcase);     { 'Schreibweise ignorieren' }
  readmask(brk);
  EditFindfunc:=not brk;
  enddialog;
  freeres;
end;

function EditReplfunc(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
var x,y : Integer;
    brk : boolean;
begin
  dialog(ival(getres2(2506,10)),7,getres2(2506,11),x,y);    { 40 / 'Suchen/ersetzen' }
  maddstring(3,2,getres2(2506,12),txt,ival(getres2(2506,10))-   { 'Text  ' }
                 length(getres2(2506,12))-7,MaxFindLen,''); mnotrim;
  maddstring(3,4,getres2(2506,13),repby,ival(getres2(2506,10))-  { 'ersetzen durch ' }
                 length(getres2(2506,13))-7,MaxFindLen,''); mnotrim;
  maddbool(3,6,getres2(2506,3),igcase);     { 'Schreibweise ignorieren' }
  readmask(brk);
  EditReplfunc:=not brk;
  enddialog;
  freeres;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure EditCfgFunc(var cfg:EdConfig; var brk:boolean);
var x,y : Integer;
    ec  : string;
begin
  with cfg do begin
    dialog(ival(getres2(2508,0)),9,getres2(2508,1),x,y);  { 30 / 'Editor-Einstellungen' }
    maddint(3,2,getres2(2508,2),rechter_rand,5,2,60,77);  { 'rechter Rand  ' }
    mhnr(8063);
    ec:=absatzendezeichen;
    maddstring(3,4,getres2(2508,3),ec,1,1,range(#1,#254));  { 'Asatzendezeichen' }
    mappsel(false,'˙˘'#20'˘˛˘Æ˘'#17'˘ ');
    maddbool(3,6,getres2(2508,4),AutoIndent);             { 'autom. einrÅcken' }
    maddbool(3,7,getres2(2508,5),PersistentBlocks);       { 'persistente Blîcke' }
    maddbool(3,8,getres2(2508,6),QuoteReflow);            { 'Quote-Reflow' }
    readmask(brk);
    enddialog;
    if not brk then begin
      if ec='' then absatzendezeichen:=' '
      else absatzendezeichen:=ec[1];
      end;
    end;
end;

procedure InitEditor;
var p : EdProcs;
begin
  EdInitDefaults(color);
  EdgetConfig(EdCfg);
  setcolors;
  EdGetProcs(p);
  with p do begin
    QuitFunc:=EditQuitfunc;
    Overwrite:=EditOverwrite;
    MsgProc:=EditMessage;
    FileProc:=EditAskFile;
    FindFunc:=EditFindFunc;
    ReplFunc:=EditReplFunc;
    CfgFunc:=EditCfgFunc;
    end;
  EdSetProcs(p);
  EdSelcursor:=auswahlcursor;
end;

procedure EditSetLangdata;
var ld : LangData;
    i  : integer;
begin
  fillchar(ld,sizeof(ld),0);
  with ld do begin
    ja:=_jn_[1];
    nein:=_jn_[2];
    zeile:=getres2(2503,1);
    spalte:=getres2(2503,2);
    for i:=1 to 6 do
      errors[i]:=getres2(2503,10+i);
    askreplace:=getres2(2503,20);
    replacechr:=getres2(2503,21);
    ersetzt:=getres2(2503,22);
    drucken:=getres2(2503,23);
    for i:=0 to editmenumps do
      menue[i]:=getres2(2503,30+i);
    freeres;
    EdSetLanguage(ld);
    end;
end;


procedure EditBetreff; forward;

function EditKeyFunc(var t:taste):boolean;
begin
  if (edbetreff<>'') and (t=keyaltb) then EditBetreff;
  if (@EditXKeyFunc<>nil) and EditXKeyFunc(t) then;
  if t=keyf6 then Makroliste(6)
  else XMakro(t,32);
  EditKeyFunc:=false;
end;


procedure TED(var fn:string; reedit:boolean; keeplines:byte; ukonv:boolean);
const inited : boolean = false;
      EditFusszeile = false;
var   ed     : ECB;
      p      : scrptr;
      mb     : byte;
      mt     : boolean;
begin
  FlushClose;
  if not inited then begin
    InitEditor; inited:=true;
    end
  else begin
    SetColors;
    EdSetConfig(EdCfg);
    end;
  EditSetLangData;
  mt:=m2t;
  if keeplines>0 then begin
    mb:=dphback; dphback:=col.coledithead;
    m2t:=true;
    Disp_DT;
    end
  else
    m2t:=false;
  { screenwidth/screenlines (hd) }
  ed:=EdInit(1,screenwidth,1+keeplines,screenlines-iif(EditFusszeile,1,0),{74}0,true,2,OtherQuoteChars);
  if EdLoadFile(ed,fn,reedit,{iif(reedit,}1{,0)}) then;
  sichern(p);
  if EditFusszeile then DispFunctionkeys(true);
  EdSetTProc(ed,EditKeyFunc);
  { EdPointswitch(deutsch);  Yuppie-Schalter }
  EdSetUkonv(ukonv);
  if doautosave then EdAutosave;
  doautosave:=false;
  maus_pushinside(1,screenwidth,1,screenlines);
  if EdEdit(ed)=0 then;
  EdGetConfig(EdCfg);
  maus_popinside;
  if keeplines>0 then dphback:=mb;
  m2t:=mt;
  holen(p);
  EdExit(ed);
end;


{ --- Nachrichteneditor ------------------------------------------- }

procedure EditSetbetreff(betr:string; maxlen:byte);
begin
  edbmaxlen:=maxlen;
  edbetreff:=betr;
end;

function EditGetBetreff:string;
begin
  EditGetbetreff:=edbetreff;
  edbetreff:='';
end;

procedure EditBetreff;
var x,y : Integer;
    brk : boolean;
begin
  if edbetreff='' then exit;
  dialog(min(edbmaxlen+7+length(getres(2507)),70),3,'',x,y);
  maddstring(3,2,getres(2507),edbetreff,min(edbmaxlen,48),edbmaxlen,'');
  msetvfunc(xp6.umlauttest); mhnr(88);
  readmask(brk);
  enddialog;
  if not brk then begin
    attrtxt(col.coledithead);
    mwrt(2,2,forms(getres2(611,42)+edbetreff,79));
    freeres;
    end;
end;


{ --- Signatureditor ---------------------------------------------- }


procedure SigEdit(datei:string);
var ok   : boolean;
    x,y  : Integer;
    n,nn : shortint;
    a    : taste;
    i    : integer;
    s    : string;
    t    : text;
begin
  repeat
    editfile(datei,false,false,1,false);    { in XP1 }
    if _filesize(datei)<=MaxSigsize then
      ok:=true
    else begin
      assign(t,datei);
      reset(t);
      readln(t,s);
      close(t);
      if s='-*-' then
        ok:=true
      else begin
        nn:=ival(getres2(2501,1));
        msgbox(51,nn+6,getres2(2501,0),x,y);     { 'Hinweis' }
        for i:=1 to nn-1 do
          mwrt(x+3,y+1+i,getreps2(2501,i+1,strs(MaxSigsize)));
        mwrt(x+3,y+2+nn,getres2(2501,nn+1));      { 'ZurÅck zum Editor?' }
        errsound;
        a:='';
        n:=readbutton(x+3,y+8,2,getres2(2501,6),1,true,a);   { '  ^Ja  , ^Nein ' }
        freeres;
        closebox;
        ok:=(n<>1);
        end;
      end;
  until ok;
end;


procedure EditText;
var s       : string;
    useclip : boolean;
begin
  if keepedname then
    s:=editname
  else
    s:=sendpath+WildCard;
  useclip:=true;
  pushhp(11607);
  if readfilename(getres(2502),s,true,useclip) then begin   { 'Text bearbeiten' }
{$IFDEF UnixFS }
    if not multipos(DirSepa,s)
{$ELSE }
    if not multipos(DirSepa+':',s)
{$ENDIF }
      then s:=sendpath+s;
    editname:=s;
    EditFile(s,false,false,0,false);
    if useclip then WriteClipfile(s);
    end;
  pophp;
end;


procedure Notepad;
var s  : string;
    ma : byte;
begin
  s:='NOTEPAD';
  doautosave:=true;
  ma:=lastattr;
  savecursor;
  pushhp(54);
  EditFile(s,false,false,0,false);
  pophp;
  restcursor;
  attrtxt(ma);
end;


procedure Editor_options;  { EditorOptionen Extern aendern }
var config : Edconfig;
    brk    : boolean;
    t      : text;
    s      : string;
    p      : byte;

begin
  assign(t,editor.EdConfigFile);
  brk:=true;

  if not existf(t) then            { neues Defaultconfig erstellen }
  begin
    config.rechter_rand:=74;
    config.absatzendezeichen:='˙';
    config.AutoIndent:=true;
    config.PersistentBlocks:=true;
    config.QuoteReflow:=true;
    end

  else begin
    reset(t);                      { oder existierendes Configfile laden }
    while not eof(t) do begin
      readln(t,s);
      LoString(s);
      p:=cpos('=',s);
      with config do
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

  EditCfgFunc(config,brk);         { Menue aufrufen }

  if not brk then begin            { und Aenderungen speichern }
    edCfg := Config;
    EdSetConfig(edCfg);
    rewrite(t);
    with Config do begin
      writeln(t,'RechterRand=',rechter_rand);
      writeln(t,'AbsatzEnde=',absatzendezeichen);
      writeln(t,'AutoIndent=',iifc(AutoIndent,'J','N'));
      writeln(t,'PersistentBlocks=',iifc(PersistentBlocks,'J','N'));
      writeln(t,'QuoteReflow=',iifc(QuoteReflow,'J','N'));
      end;
    close(t);
    end;
  menurestart:=brk;
end;

end.
{
  $Log$
  Revision 1.29  2001/07/28 12:04:15  mk
  - removed crt unit as much as possible

  Revision 1.28  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.27  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.26  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.25  2000/11/24 15:23:56  mk
  - Edit/Config uebernimmt Optionen jetzt immer

  Revision 1.24  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.23  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.22  2000/10/17 10:13:23  mk
  - Unit Sysutils hinzugefuegt

  Revision 1.21  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.20  2000/07/13 17:15:16  mk
  - noch einige ^String-spezifische Probleme beseitigt

  Revision 1.19  2000/07/13 10:23:46  mk
  - Zeiger auf Strings entfernt

  Revision 1.18  2000/07/05 14:49:29  hd
  - AnsiString

  Revision 1.17  2000/06/29 13:00:59  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.16  2000/05/22 16:12:11  hd
  - Anpassung an UnixFS (Filesystem)
  - screenwidth statt 80 (Screen)

  Revision 1.15  2000/05/09 13:13:10  hd
  - UnixFS: EditText angepasst

  Revision 1.14  2000/05/07 10:42:04  hd
  - Linux: Variable Fensterbreite

  Revision 1.13  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.12  2000/04/15 14:08:06  jg
  - Bugfix: Erstaufruf der Editoroptionen ging nur aus dem Editor heraus.

  Revision 1.11  2000/04/14 14:55:35  jg
  - Bugfix: es gab zwei Routinen namens Editoptions...

  Revision 1.10  2000/04/13 12:48:40  mk
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

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - Editor }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpe;

interface

uses crt,dos,dosx,typeform,fileio,inout,keys,winxp,maus2,resource,maske,
     eddef,editor,xpglobal, xp0,xp1o,xp1help,xp1input,xpkeys,xp5,xp10;


const EditXkeyfunc : EdTProc = nil;

procedure TED(var fn:pathstr; reedit:boolean; keeplines:byte; ukonv:boolean);
procedure SigEdit(datei:pathstr);
procedure EditText;
procedure Notepad;
procedure EditSetbetreff(betr:string; maxlen:byte);
function  EditGetBetreff:string;

function  EditKeyFunc(var t:taste):boolean;

function  EditQuitfunc(ed:ECB):taste;   { Speichern: J/N/Esc }
function  EditOverwrite(ed:ECB; fn:pathstr):taste;   { öberschr.: J/N/Esc }
procedure EditMessage(txt:string; error:boolean);
{ 04.02.2000 robo }
{ procedure EditAskFile(ed:ECB; var fn:pathstr; save:boolean); }
procedure EditAskFile(ed:ECB; var fn:pathstr; save,uuenc:boolean);
{ /robo }
function  EditFindfunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
function  EditReplfunc(ed:ECB; var txt,repby:string; var igcase:boolean):boolean;
procedure EditCfgFunc(var cfg:EdConfig; var brk:boolean);


implementation  {--------------------------------------------------------}

uses  xp1,xp6;

const edbetreff : ^string = nil;
      doautosave: boolean = false;

var
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

function EditOverwrite(ed:ECB; fn:pathstr):taste;   { öberschr.: J/N/Esc }
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

{ 04.02.2000 robo }
{ procedure EditAskFile(ed:ECB; var fn:pathstr; save:boolean); }
procedure EditAskFile(ed:ECB; var fn:pathstr; save,uuenc:boolean);
{ /robo }
var useclip : boolean;
begin
  if save then fn:='' else fn:=SendPath+'*.*';
  useclip:=false;          { 'Block speichern' / 'Block laden' }
  if readfilename(getres(iif(save,2504,2505))
  { 04.02.2000 robo }
                  +iifs(uuenc,' '+getres(2509),'')
  { /robo }
                  ,fn,true,useclip) then begin
    if not multipos('\:',fn) then fn:=sendpath+fn;
    end
  else
    fn:='';
end;

function EditFindfunc(ed:ECB; var txt:string; var igcase:boolean):boolean;
var x,y : byte;
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
var x,y : byte;
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

procedure EditCfgFunc(var cfg:EdConfig; var brk:boolean);
var x,y : byte;
    ec  : string[1];
begin
  with cfg do begin
    dialog(ival(getres2(2508,0)),9,getres2(2508,1),x,y);  { 30 / 'Editor-Einstellungen' }
    maddint(3,2,getres2(2508,2),rechter_rand,5,2,60,77);  { 'rechter Rand  ' }
    ec:=absatzendezeichen;
    maddstring(3,4,getres2(2508,3),ec,1,1,range(#1,#254));  { 'Asatzendezeichen' }
    mappsel(false,'˙˘'#20'˘˛˘Æ˘'#17'˘ ');
    maddbool(3,6,getres2(2508,4),AutoIndent);             { 'autom. einrÅcken' }
    { 01/2000 oh }
    maddbool(3,7,getres2(2508,5),PersistentBlocks);       { 'persistente Blîcke' }
    { /oh }
    { 10.02.2000 robo }
    maddbool(3,8,getres2(2508,6),QuoteReflow);            { 'Quote-Reflow' }
    { /robo }
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
  if (edbetreff<>nil) and (t=keyaltb) then EditBetreff;
  if (@EditXKeyFunc<>nil) and EditXKeyFunc(t) then;
  if t=keyf6 then Makroliste(6)
  else XMakro(t,32);
  EditKeyFunc:=false;
end;


procedure TED(var fn:pathstr; reedit:boolean; keeplines:byte; ukonv:boolean);
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
  ed:=EdInit(1,80,1+keeplines,screenlines-iif(EditFusszeile,1,0),{74}0,true,2,OtherQuoteChars);
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
  getmem(edbetreff,maxlen+1);
  edbmaxlen:=maxlen;
  edbetreff^:=betr;
end;

function EditGetBetreff:string;
begin
  if edbetreff=nil then
    EditGetbetreff:=''
  else begin
    EditGetbetreff:=edbetreff^;
    freemem(edbetreff,edbmaxlen+1);
    edbetreff:=nil;
    end;
end;

procedure EditBetreff;
var x,y : byte;
    brk : boolean;
begin
  if edbetreff=nil then exit;
  dialog(min(edbmaxlen+7+length(getres(2507)),70),3,'',x,y);
  maddstring(3,2,getres(2507),edbetreff^,min(edbmaxlen,48),edbmaxlen,'');
  mset1func(xp6.umlauttest); mhnr(88);
  readmask(brk);
  enddialog;
  if not brk then begin
    attrtxt(col.coledithead);
    mwrt(2,2,forms(getres2(611,42)+edbetreff^,79));
    freeres;
    end;
end;


{ --- Signatureditor ---------------------------------------------- }


procedure SigEdit(datei:pathstr);
var ok   : boolean;
    x,y  : byte;
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
var s       : pathstr;
    useclip : boolean;
begin
  if keepedname then
    s:=editname
  else
    s:=sendpath+'*.*';
  useclip:=true;
  pushhp(11607);
  if readfilename(getres(2502),s,true,useclip) then begin   { 'Text bearbeiten' }
    if not multipos('\:',s) then s:=sendpath+s;
    editname:=s;
    EditFile(s,false,false,0,false);
    if useclip then WriteClipfile(s);
    end;
  pophp;
end;


procedure Notepad;
var s  : pathstr;
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


end.


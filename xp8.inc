{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Fido FileScan }

procedure SendFilescan(var fn:pathstr);
var leer : string[12];
begin
  leer:='';
  forcebox:=boxpar^.boxname;
  if DoSend(true,fn,BoxPar^.filescanner+'@'+boxpar^.boxname,
            boxpar^.FilescanPW,false,false,false,false,false,
            nil,leer,leer,0) then;
end;


procedure GetFilescanBox(var box:string);
begin
  box:=UniSel(1,false,DefFidoBox);
  if box='' then exit;
  if ntBoxNetztyp(box)<>nt_Fido then begin
    rfehler1(852,box);     { '%s ist keine Fido-Box!' }
    box:='';
    end
  else
    ReadBoxpar(nt_Fido,box);
end;


function fileechomarkfunc(var s:string; block:boolean):boolean;
begin
  if trim(s)='' then begin
    if not block then errsound;
    fileechomarkfunc:=false;
    end
  else
    fileechomarkfunc:=true;
end;

function fileechocolfunc(var s:string; line:longint):byte;
begin
  if s[1]='*' then
    fileechocolfunc:=col.ColMapsBest
  else
    fileechocolfunc:=0;
end;

function echoname(s:string):string;
begin
  s:=trim(s);
  while (s<>'') and (s[1]<'0') do delfirst(s);
  if blankpos(s)>0 then truncstr(s,blankpos(s)-1);
  echoname:=s;
end;

procedure FilescanList(art:shortint);     { 1=bestellen, 2=abbestellen }
var box : string[BoxNameLen];
    fl  : string[12];
    t   : text;
    fn  : pathstr;
    anz : longint;
    ask : string[40];
    s   : string[80];
    brk : boolean;
label again;

begin
  GetFilescanBox(box);
  if box='' then exit;
  fl:=BoxFilename(box)+'.FBL';
  if not exist(fl) then begin
    rfehler1(853,box); exit;
    end;
  OpenList(1,iif(_maus,79,80),4,screenlines-fnkeylines-1,-1,'/NS/M/SB/S/'+
             'APGD/'+iifs(_maus,'VSC:080/',''));
  list_readfile(fl,0);
  case art of
    1 : showkeys(12);
    2 : showkeys(11);
  end;
again:
  ListVmark(FileechoMarkfunc);
  ListCFunc(FileechoColfunc);
  list(brk);
  if not brk then begin
    anz:=list_markanz;
    if anz=0 then anz:=1;
    if (anz=1) and (echoname(first_marked)='') then begin
      errsound;
      goto again;
      end;
    case art of
      1 : ask:=getres2(852,iif(anz=1,1,2));
      2 : ask:=getres2(852,iif(anz=1,3,4));
    end;
    if anz=1 then ask:=reps(ask,echoname(first_marked))
    else ask:=reps(ask,strs(anz));
    if not ReadJN(ask,true) then
      goto again;
    fn:=TempS(20000);
    assign(t,fn);
    rewrite(t);
    s:=first_marked;
    while s<>#0 do begin
      writeln(t,iifc(art=1,'+','-'),echoname(s));
      s:=next_marked;
      end;
    close(t);
    SendFilescan(fn);
    _era(fn);
    end;
  freeres;
  closelist;
  aufbau:=true;
end;


procedure AddFileechos;
begin
  FilescanList(1);
end;


procedure RemoveFileechos;
var echo   : string[80];
    _brett : string[5];
    brk    : boolean;
    d      : DB;
    n,i    : longint;
    box    : string[BoxNameLen];
    s      : string[80];
    fn     : pathstr;
    t      : text;
begin
  echo:='';
  brk:=false;
  if (aktdispmode=10) and not dbEOF(mbase) and not dbBOF(mbase) then begin
    dbReadN(mbase,mb_brett,_brett);
    dbSeek(bbase,biIntnr,mid(_brett,2));
    if dbFound and (ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox))=nt_Fido) then begin
      dbReadN(bbase,bb_brettname,echo);
      if echo[1]='A' then delfirst(echo)
      else echo:='';
      end;
    end
  else if (aktdispmode=-1) or (aktdispmode=0) then
    if bmarkanz>0 then
      case ReadIt(length(getres2(852,6))+4,getres2(852,6),getres2(852,7),1,brk) of
        1 : echo:='*';          { 'alle markierten File Areas abbestellen' }
        2 : brk:=true;
        3 : echo:='';
      end
    else begin
      dbReadN(bbase,bb_brettname,echo);
      if (ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox))<>nt_Fido) or (echo[1]<>'A') then echo:=''
      else delfirst(echo);
      end;
  if brk then begin
    freeres; exit; end;
  if (echo<>'') and (echo<>'*') and (pos('/files/',lstr(echo))>0) then begin
    s:=getreps2(852,5,left(echo,40));
    case ReadIt(max(length(s)+4,40),s,getres2(852,7),1,brk) of
      1 : begin end;
      2 : brk:=true;
      3 : echo:='';
    end;
    end;
  freeres;
  if brk then exit;

  fn:=TempS(20000); assign(t,fn);
  if (echo='') or ((echo<>'*') and (pos('/files/',lstr(echo))=0)) then
    FilescanList(2)                                     { Auswahl aus Liste }
  else if echo<>'*' then begin          { ein Brett abbestellen }
    ReadBoxPar(nt_Fido,dbReadStrN(bbase,bb_pollbox));
    rewrite(t);
    delete(echo,1,length(boxpar^.magicbrett));
    if lstr(left(echo,6))='files/' then delete(echo,1,6);
    writeln(t,'-',echo);
    close(t);
    SendFilescan(fn);
    _era(fn);
    end
  else begin                            { markierte Bretter abbestellen }
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do begin
      if dbReadInt(d,'netztyp')=nt_Fido then begin
        dbRead(d,'boxname',box);
        UpString(box);
        ReadBoxPar(nt_Fido,box);
        n:=0;
        for i:=0 to bmarkanz-1 do begin
          dbGo(bbase,bmarked^[i]);
          dbReadN(bbase,bb_brettname,echo);
          delfirst(echo);
          if (ustr(dbReadStrN(bbase,bb_pollbox))=box) and
             (left(ustr(echo),length(boxpar^.magicbrett))=ustr(boxpar^.magicbrett))
             and (pos('/files/',lstr(echo))>0)
          then begin
            if n=0 then rewrite(t);
            delete(echo,1,length(boxpar^.magicbrett));
            if lstr(left(echo,6))='files/' then delete(echo,1,6);
            writeln(t,'-',echo);
            inc(n);
            end;
          end;
        if n>0 then begin
          close(t);
          SendFilescan(fn);
          end;
        end;
      dbNext(d);
      end;
    dbClose(d);
    if exist(fn) then _era(fn);
    end;
end;


procedure FilescanReadlist;
var fa  : FidoAdr;
    box : string[BoxnameLen];
begin
  if (aktdispmode<10) or (aktdispmode>19) or (mbNetztyp<>nt_Fido) then
    rfehler(850)     { 'Keine Filescan-Nachricht gewÑhlt!' }
  else begin
    splitfido(dbReadStrN(mbase,mb_absender),fa,DefaultZone);
    box:=MakeFidoAdr(fa,false);
    if not IsBox(box) then
      rfehler1(851,box)    { '%s ist kein eingetragener Fido-Server!' }
    else begin
      message(getreps(850,box));       { 'Fileecho-Liste fÅr %s wird eingelesen ...' }
      extract_msg(xTractMsg,'',BoxFilename(box)+'.FBL',false,0);
      mdelay(500);
      closebox;
      end;
    end;
end;


{ Fileecho-Liste aus Datei nach .FBL einlesen }

procedure FilescanReadfile;
var box     : string[BoxNameLen];
    fn      : pathstr;
    useclip : boolean;
    d       : DB;
    bfile   : string[8];
begin
  box:=UniSel(1,false,DefaultBox);
  if box='' then exit;   { brk }
  if ntBoxNetztyp(box)<>nt_Fido then begin
    rfehler1(852,box);    { '%s ist keine Fido-Box!' }
    exit;
    end;
  fn:='*.*';
  useclip:=true;
  if not ReadFilename(getres(822),fn,true,useclip) then exit;   { 'Fileecho-Liste einlesen' }
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  dbRead(d,'dateiname',bfile);
  dbClose(d);
  ReadBox(0,bfile,boxpar);
  message(getreps(806,ustr(box)));   { 'Fileecho-Liste fÅr %s wird eingelesen ...' }
  if filecopy(fn,bfile+'.FBL') then;
  closebox;
  if useclip or ReadJN(getreps(817,fn),false) then   { '%s lîschen' }
    _era(fn);
end;


procedure FilescanCommands(cmd:shortint);
var box  : string[BoxnameLen];
    comm : string[20];
    s    : string[12];
    n,i  : integer;
    brk  : boolean;
    fn   : pathstr;
    t    : text;
    x,y  : byte;
begin
  GetFilescanbox(box);
  if box='' then exit;
  brk:=false;
  if cmd=1 then
    comm:='LIST'
  else begin
    n:=ival(getres2(851,2));                          { 'Nachricht an %s' }
    listbox(ival(getres2(851,0)),n,getreps2(851,1,boxpar^.filescanner+' @ '+box));
    for i:=1 to n do
      app_l(' '+getres2(851,i+2));
    list(brk);
    closebox;
    if not brk then begin
      comm:=trim(get_selection);
      TruncStr(comm,pos('  ',comm)-1);
      end;
    closelist;
    end;
  if comm='PWD' then begin
    dialog(43,3,'',x,y);
    s:=boxpar^.FilescanPW;
    maddstring(3,2,getres2(851,20),s,12,12,'>'); mhnr(87);
    readmask(brk);
    enddialog;
    if not brk then comm:=comm+' '+s;
    end;
  if not brk then begin
    fn:=TempS(2048);
    assign(t,fn);
    rewrite(t);
    writeln(t,'%',comm);
    close(t);
    SendFilescan(fn);
    _era(fn);
    end;
  freeres;
end;
{
  $Log$
  Revision 1.3.4.1  2001/08/12 11:20:37  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.3  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
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

{ Fido FileScan }

procedure SendFilescan(var fn:string);
var leer : string;
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
  if (s<>'') and (s[1]='*') then
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
var
    box : string;
    fl  : string;
    ask : string;
    s   : string;
    t   : text;
    fn  : string;
    anz : longint;
    brk : boolean;
  List: TLister;

label
  again;
begin
  GetFilescanBox(box);
  if box='' then exit;
  fl:=GetServerFilename(box, extFbl);
  if not FileExists(fl) then begin
    rfehler1(853,box); exit;
    end;
  List := TLister.CreateWithOptions(1,iif(_maus,screenwidth-1,screenwidth),4,screenlines-fnkeylines-1,-1,'/NS/M/SB/S/'+
             'APGD/'+iifs(_maus,'VSC:080/',''));
  List.ReadFromFile(fl,0);
  case art of
    1 : showkeys(12);
    2 : showkeys(11);
  end;
again:
  List.OnTestMark := FileechoMarkfunc;
  List.OnColor := FileechoColfunc;
  brk := List.Show;
  if not brk then begin
    anz:=List.SelCount;
    if anz=0 then anz:=1;
    if (anz=1) and (echoname(List.FirstMarked)='') then begin
      errsound;
      goto again;
      end;
    case art of
      1 : ask:=getres2(852,iif(anz=1,1,2));
      2 : ask:=getres2(852,iif(anz=1,3,4));
    end;
    if anz=1 then ask:=reps(ask,echoname(List.FirstMarked))
    else ask:=reps(ask,strs(anz));
    if not ReadJN(ask,true) then
      goto again;
    fn:=TempS(20000);
    assign(t,fn);
    rewrite(t);
    s:=List.FirstMarked;
    while s<>#0 do begin
      writeln(t,iifc(art=1,'+','-'),echoname(s));
      s:=List.NextMarked;
      end;
    close(t);
    SendFilescan(fn);
    _era(fn);
    end;
  freeres;
  List.Free;
  aufbau:=true;
end;


procedure AddFileechos;
begin
  FilescanList(1);
end;


procedure RemoveFileechos;
var
    echo   : string;
    _brett : string;
    box    : string;
    s      : string;
    brk    : boolean;
    d      : DB;
    n,i    : longint;
    fn     : string;
    t      : text;
begin
  echo:='';
  brk:=false;
  if (aktdispmode=10) and not dbEOF(mbase) and not dbBOF(mbase) then begin
    _brett:= dbReadNStr(mbase,mb_brett);
    dbSeek(bbase,biIntnr,mid(_brett,2));
    if dbFound and (ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox))=nt_Fido) then begin
      echo:= dbReadNStr(bbase,bb_brettname);
      if (echo<>'') and (echo[1]='A') then delfirst(echo)
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
      if dbreccount(bbase)=0 then 
        brk:=true
      else begin
        echo := dbReadStrN(bbase,bb_brettname);
        if (ntBoxNetztyp(dbReadStrN(bbase,bb_pollbox))<>nt_Fido) or (FirstChar(echo)<>'A') then 
          echo:=''
        else 
          delfirst(echo);
      end;
    end;
  if brk then begin
    freeres; exit; end;
  if (echo<>'') and (echo<>'*') and (pos('/files/',LowerCase(echo))>0) then begin
    s:=getreps2(852,5,LeftStr(echo,40));
    case ReadIt(max(length(s)+4,40),s,getres2(852,7),1,brk) of
      1 : begin end;
      2 : brk:=true;
      3 : echo:='';
    end;
    end;
  freeres;
  if brk then exit;

  fn:=TempS(20000); assign(t,fn);
  if (echo='') or ((echo<>'*') and (pos('/files/',LowerCase(echo))=0)) then
    FilescanList(2)                                     { Auswahl aus Liste }
  else if echo<>'*' then begin          { ein Brett abbestellen }
    ReadBoxPar(nt_Fido,dbReadStrN(bbase,bb_pollbox));
    rewrite(t);
    delete(echo,1,length(boxpar^.magicbrett));
    if LowerCase(LeftStr(echo,6))='files/' then delete(echo,1,6);
    writeln(t,'-',echo);
    close(t);
    SendFilescan(fn);
    _era(fn);
    end
  else begin                            { markierte Bretter abbestellen }
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do begin
      if dbReadInt(d,'netztyp')=nt_Fido then 
      begin
        box:= UpperCase(dbReadStr(d,'boxname'));
        ReadBoxPar(nt_Fido,box);
        n:=0;
        for i:=0 to bmarkanz-1 do begin
          dbGo(bbase,bmarked^[i]);
          echo:= dbReadNStr(bbase,bb_brettname);
          delfirst(echo);
          if (UpperCase(dbReadStrN(bbase,bb_pollbox))=box) and
             (LeftStr(UpperCase(echo),length(boxpar^.magicbrett))=UpperCase(boxpar^.magicbrett))
             and (pos('/files/',LowerCase(echo))>0)
          then begin
            if n=0 then rewrite(t);
            delete(echo,1,length(boxpar^.magicbrett));
            if LowerCase(LeftStr(echo,6))='files/' then delete(echo,1,6);
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
    SaveDeleteFile(fn);
  end;
end;


procedure FilescanReadlist;
var fa  : FidoAdr;
    box : string;
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
      extract_msg(xTractMsg,'',GetServerFilename(box, extFbl),false,0);
      mdelay(500);
      closebox;
      end;
    end;
end;


{ Fileecho-Liste aus Datei nach .FBL einlesen }

procedure FilescanReadfile;
var
    box     : string;
    bfile   : string;
    fn      : string;
    useclip : boolean;
begin
  box:=UniSel(1,false,DefaultBox);
  if box='' then exit;   { brk }
  if ntBoxNetztyp(box)<>nt_Fido then begin
    rfehler1(852,box);    { '%s ist keine Fido-Box!' }
    exit;
    end;
  fn:=WildCard;
  useclip:=true;
  if not ReadFilename(getres(822),fn,true,useclip) then exit;   { 'Fileecho-Liste einlesen' }
  bfile := GetServerFilename(Box, '');
  ReadBox(0,bfile,boxpar);
  message(getreps(806,UpperCase(box)));   { 'Fileecho-Liste fÅr %s wird eingelesen ...' }
  Filecopy(fn, bfile + extFbl);
  Closebox;
  if useclip or ReadJN(getreps(817,fn),false) then   { '%s lîschen' }
    _era(fn);
end;


procedure FilescanCommands(cmd:shortint);
var
  List: TLister;
    box  : string;
    comm : string;
    s    : string;
    n,i  : integer;
    brk  : boolean;
    fn   : string;
    t    : text;
    x,y  : Integer;
begin
  GetFilescanbox(box);
  if box='' then exit;
  brk:=false;
  if cmd=1 then
    comm:='LIST'
  else begin
    n:=ival(getres2(851,2));                          { 'Nachricht an %s' }
    List := listbox(ival(getres2(851,0)),n,getreps2(851,1,boxpar^.filescanner+' @ '+box));
    for i:=1 to n do
      List.AddLine(' '+getres2(851,i+2));
    brk := List.Show;
    closebox;
    if not brk then
    begin
      comm:=trim(List.GetSelection);
      TruncStr(comm,pos('  ',comm)-1);
    end;
    List.Free;
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
  Revision 1.17  2001/09/07 13:54:22  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.16  2001/09/07 10:56:01  mk
  - added GetServerFilename

  Revision 1.15  2001/08/24 08:20:35  mk
  JG:- Fixed crash with F/A/A and empty Message Area database

  Revision 1.14  2001/08/12 11:50:42  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.13  2001/07/23 16:05:21  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.12  2001/07/08 09:42:17  ma
  - fixed: File name case (.bl was not processed correctly with Unix)

  Revision 1.11  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.10  2000/12/25 14:02:44  mk
  - converted Lister to class TLister

  Revision 1.9  2000/11/14 15:51:34  mk
  - replaced Exist() with FileExists()

  Revision 1.8  2000/10/17 10:05:55  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.7  2000/07/27 10:13:04  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.6  2000/07/21 17:14:40  hd
  - Anpassung an die Datenbank (AnsiString)

  Revision 1.5  2000/07/05 15:12:15  hd
  - AnsiString

  Revision 1.4  2000/07/04 12:04:27  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.3  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

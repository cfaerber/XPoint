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

{ Nodelisten-Konfiguration; Diffs verarbeiten }

{$I xpdefine.inc}

unit xpfidonl;

interface

uses  classes,sysutils,typeform,montage,fileio,maske,resource,archive,
      xp0,xp1,xp1o,xp3,xpglobal,fidoglob;


procedure InitNodelist;
procedure EditNLentry(var NLItem: TNodeListItem; var brk:boolean);
function  NewNodeEntry:boolean;

function  DoDiffs(files:string; auto:boolean):byte;
procedure ManualDiff;

function  setfnlenable(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses
{$IFDEF Kylix}
  xplinux,
{$ENDIF}   
  xpfido, ndiff;


{ --- Nodelisten-Konfiguration laden/speichern ---------------------- }

procedure InitNodelist;
var
    indexflag : boolean;
    i         : integer;
    xni       : boolean;

  procedure NL_Datecheck;      { testen, ob neue Liste dazugekommen }
  var
      fh: Integer;
  begin
    if FileDateToDateTime(FileAge(NodeListCfg)) > FileDateToDateTime(FileAge(UserIndexF)) then
    begin
      if FileDateToDateTime(FileAge(NodeListCfg))> now then
      begin
{$IFDEF Kylix}
        FileSetDate(NodeListCfg, DateTimeToFileDate(now));
{$ELSE}
        fh := FileOpen(NodeListCfg, fmOpenReadWrite);
        FileSetDate(fh, DateTimeToFileDate(now));
        FileClose(fh);
{$ENDIF}
      end;
      indexflag:=true;
    end;
  end;

begin           //procedure InitNodelist;
  NodeList := TNodeList.Create;
  NodeList.LoadConfigFromFile;          { Nodelist-Konfiguration im neuen Format laden }
  indexflag:=false;                     { altes Format wird nicht mehr unterstÅtzt                    }

  for i:=0 to NodeList.Count - 1 do
    if not FileExists(FidoDir+NodeList.GetFilename(i)) then
      trfehler1(214,FidoDir+NodeList.GetFilename(i),10);  { 'Node-/Pointliste %s fehlt!' }
  if NodeList.Count > 0 then
  begin
    xni:=FileExists(NodeIndexF);                //exists 'FIDO\NODELIST.IDX'
    if xni then NL_Datecheck;
    if indexflag or not xni or not FileExists (UserIndexF) then
      MakeNodelistIndex;
    OpenNodeindex(NodeIndexF);
    if xni and not FileExists(NodeIndexF) then begin  { gelîscht durch OpenNodeindex }
      MakeNodelistIndex;
      OpenNodeindex(NodeIndexF);
      end;
    end
  else
    Nodelist.Open:=false;    //mOpen wird von procedure OpenNodeindex ggf. auf true gesetzt
end;



{ --- Nodelisten-Konfiguration bearbeiten (s. auch XP10) ------------ }

var  fne_first : integer;


function setfnlenable(var s:string):boolean;
var i : integer;
begin
  setfnlenable := true;     { MK 14.02.00 Standard-Wert setzen }
  if (s='') or (length(s)=1) then
    for i:=fne_first to fne_first+2 do
      setfieldenable(i,s<>'');
end;


procedure EditNLentry(var NLItem: TNodeListItem; var brk:boolean);
var x,y,i    : Integer;
    filechar : string;
    lform    : string;
    adresse  : string;
    fa       : fidoadr;
begin
  dialog(ival(getres2(2127,0)),14,getres2(2127,2),x,y);
  with NLItem do
  begin
    filechar:='>'+range('#','~')+'!';
    maddtext(3,2,getres2(2127,5),0);                    // 'Listenname      '
    maddtext(21,2,listfile,col.coldiahigh);
    maddtext(39,2,getres2(2127,6),0);                   // 'Nummer '
    maddtext(47,2,formi(number,3),col.coldiahigh);
    lform:=getres2(2128,format);
    maddstring(3,4,getres2(2127,7),lform,15,20,'');     // 'Listenformat
    mhnr(940);
    for i:=1 to res2anz(2128) do
      mappsel(true,getres2(2128,i));
    case format of
      1     : if zone=0 then adresse:=''
              else adresse:=strs(zone);
      2,4   : adresse:=strs(zone);
      3     : adresse:=strs(zone)+':'+strs(net)+'/'+strs(node);
      5     : adresse:='';
      end;
    maddstring(3,5,getres2(2127,8),adresse,15,15,'0123456789:/');
    maddstring(3,7,getres2(2127,9),fupdatefile,12,12,filechar);          //'Update-Datei   '
    mset1func(setfnlenable);
    maddstring(3,8,getres2(2127,10),fupdatearc,12,12,filechar);          // 'Update-Archiv  ' }
    maddstring(3,10,getres2(2127,11),fprocessor,28,40,'');               // 'bearbeiten durch' }
      if fupdatefile='' then mdisable;
      fne_first:=fieldpos;
    maddbool(3,12,getres2(2127,12),fdodiff);                             // 'Update als Diff einbinden'
      if fupdatefile='' then mdisable;
    maddbool(3,13,getres2(2127,13),fdelupdate); { 'Update nach Einbinden lîschen' }
      if fupdatefile='' then mdisable;
    readmask(brk);
    if not brk then
    begin
      for i:=1 to res2anz(2128) do
        if UpperCase(lform)=UpperCase(getres2(2128,i)) then
          format:=i;
      if format in [nlNodelist,nlPoints24,nl4DPointlist] then
        zone:=ival(adresse)
      else begin
        Splitfido(adresse,fa,defaultzone);
        zone:=fa.zone; net:=fa.net; node:=fa.node;
        end;
      if fupdatefile='' then fDoDiff:=false;
      end;
    end;
  enddialog;
  freeres;
end;


function NewNodeEntry:boolean;
var fn      : string;
    ffn     : string[12];
    brk     : boolean;
    useclip : boolean;
    i       : integer;
    NLItem  : TNodeListItem;
    p       : byte;
    detect  : boolean;
    arc     : integer;
    ar      : ArchRec;
    fa      : FidoAdr;

  procedure PL_FormatDetect(fn:string; var format:byte);
  var t   : text;
      s   : string;
      n   : byte;
  begin
    assign(t,fn);
    if existf(t) then begin
      reset(t);
      n:=0;
      while (format=1) and (n<200) and not eof(t) do begin
        readln(t,s);
        if LeftStr(s,5)='Boss,' then
          format:=nlFDpointlist
        else if LeftStr(s,6)='Point,' then begin
          format:=nl4Dpointlist;
          NLItem.zone:=DefaultZone;
          end;
        inc(n);
        end;
      close(t);
      end;
  end;

begin   //function NewNodeEntry:boolean;
  NewNodeEntry:=false;
  fn:=WildCard;
  useclip:=false;        { 'Neue Node-/Pointliste einbinden' }
  pushhp(931);
  if ReadFilename(getres2(1019,10),fn,true,useclip) then
    if not FileExists(fn) then
      rfehler(22)        { 'Datei ist nicht vorhanden! }
    else begin
      NLItem:=TNodeListItem.Create;
      arc:=ArcType(fn);
      if arc>0 then begin          { gepackt -> Dateiname aus Archiv auslesen }
        ar.name:='';
        OpenArchive(fn,arc,ar);
        if stricmp(ar.name,'FILE_ID.DIZ') then
          ArcNext(ar);
        ffn:=UpperCase(ar.name);
        if ffn='' then ffn:=ExtractFilename(fn); { getfilename(fn);}
        CloseArchive(ar);
        end
      else
        ffn:=ExtractFileName(fn); {getfilename(fn);}
      i:=0;
      while (i<NodeList.Count) and (ffn<>NodeList.GetFilename(i)) do
        inc(i);
      if i<NodeList.Count then
        rfehler(1009)   { 'Diese Node-/Pointliste ist bereits eingebunden' }
      else
      with NLItem do
        if (arc=0) or UniExtract(fn,OwnPath+FidoDir,ffn) then
        begin
          if arc>0 then                                { ggf. entpacken }
            fn:=OwnPath+FidoDir+ffn;
          listfile:=ffn;
          p:=cpos('.',listfile);
          if (p>0) and isnum(mid(listfile,p+1)) then begin
            number:=ival(mid(listfile,p+1));
            listfile:=LeftStr(listfile,p)+'###';
            fDoDiff:=true;
            end;
          format:=nlNodelist;                           { Nodelist }
          detect:=false;
          if listfile='NODELIST.###' then begin         { Typvorgabe 'Nodeliste' }
            fupdatefile:='NODEDIFF.###';
            fupdatearc:='NODEDIFF.A##';
            detect:=true;
            end
          else if listfile='POINTS24.###' then begin    { Typvorgabe 'Points24' }
            fupdatefile:='PR24DIFF.###';
            fupdatearc:='PR24DIFF.A##';
            zone:=2;
            format:=2;
            detect:=true;
            end
          else if ExtractFileExt(listfile)='.PVT' then begin { Typvorgabe 'PVT-Liste' }
            fupdatefile:=listfile;
            format:=3;
            if FindFidoAddress(fn,fa) then begin
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              detect:=true;
              end;
            fDelUpdate:=true;
            end
          else
            PL_FormatDetect(fn,fformat);
          if not detect then
            EditNLentry(NLItem,brk)
          else
            brk:=false;
          if not brk then
            if (ExtractFilePath(fn)=OwnPath+FidoDir) or
               filecopy(fn,FidoDir+Extractfilename(fn)) then
            begin
              NodeList.Add(NLItem);
              if listfile='NODELIST.###' then
                ShrinkNodelist(false);
              NewNodeEntry:=true;
            end;
          end;
        end;
  pophp;
  freeres;
end;    //function NewNodeEntry:boolean;


function ReplNr(fn:string; number:integer):string;
var p : byte;
begin
  p:=pos('###',fn);
  if p>0 then
    ReplNr:=LeftStr(fn,p-1)+formi(number,3)+mid(fn,p+3)
  else begin
    p:=pos('##',fn);
    if p>0 then
      ReplNr:=LeftStr(fn,p-1)+formi(number mod 100,2)+mid(fn,p+2)
    else
      ReplNr:=fn;
    end;
end;


{ Im FILES- und TICK-Verzeichnis Diff/Update-Files fÅr alle   }
{ eingetragenen Nodelisten suchen und ausfÅhren.              }

{ Files = Pfad + Name/Wildcard der Datei(en), die eingebunden }
{         werden sollen                                       }
{                                                             }
{ Ergebnis:  0 = ok                                           }
{            1 = Fehler                                       }
{            2 = Update ist bereits eingebunden               }

{ Bei Update-Files mit fortlaufender Nummer versucht XP, die  }
{ nÑchsten vier Updates einzubinden.                          }

function  DoDiffs(files:string; auto:boolean):byte;
var diffdir  : string;
    diffnames: string[12];
    i        : integer;
    newlist  : string;
    uarchive : string;
    ufile    : string;
    nextnr   : integer;
    unarcflag: boolean;
    done     : boolean;
    reindex  : boolean;
    logfile  : text;
    logopen  : boolean;
    TmpDoDiff: boolean;
    ucount   : byte;     { ZÑhler fÅr Update-Files mit Nummern }

  procedure log(txt:string);
  begin
    if not logopen then begin
      assign(logfile,LogPath+'NODELIST.LOG');
      if existf(logfile) then append(logfile)
      else rewrite(logfile);
      writeln(logfile);
      logopen:=true;
      end;
    writeln(logfile,LeftStr(date,6),RightStr(date,2),' ',time,'  ',txt);
  end;

  function NextNumber(number:integer):integer;
  begin
    NextNumber:=(number+6) mod iif(schaltj(ival(RightStr(date,4))-1),366,365) + 1;
  end;

  procedure ExpandFilePath(var s:string);
  begin
    if s<>'' then
      if FileExists(diffdir+s) then
        s:=diffdir+s
      else if (diffdir=FilePath) and FileExists(FilePath+'TICK\'+s) then
        s:=FilePath+'TICK\'+s
      else
        s:='';
  end;

  function passend(fn:string):boolean;
  begin
    passend:=(diffnames='*.*') or (extractfilename(fn)=diffnames);
  end;

  procedure ExecProcessor(processor:string);
  var p : byte;
  begin
    p:=pos('$FILE',UpperCase(processor));
    if p>0 then
      processor:=LeftStr(processor,p-1)+ufile+mid(processor,p+5);
    log(processor);
    shell(processor,600,1);
  end;

  function UDiff:boolean;      { Update diffen }
  var s1,s2 : string[100];
      t     : text;
      fm    : byte;
  begin
    UDiff:=false;
    if not FileExists(FidoDir+NodeList.GetFilename(i)) then exit;
    fm:=filemode; filemode:=0;
    assign(t,FidoDir+NodeList.GetFilename(i));          { 1. Zeile vergleichen }
    reset(t); readln(t,s1); close(t);
    assign(t,ufile);
    reset(t); readln(t,s2); close(t);
    filemode:=fm;
    if s1<>s2 then begin
      trfehler1(2111,NodeList.GetFilename(i),30);   { '%s: Falsche Version von Nodelist oder Nodediff' }
      log(getreps2(2130,3,NodeList.GetFilename(i)));   { dito }
      exit;
      end;
    chdir(fidodir_);
      UDiff:=true;
    log(NodeList.GetFilename(i)+' + '+ufile+' -> '+extractfilename(newlist));
    try
      Processlist(NodeList.GetFileName(i), ufile);
    except
      arfehler(2112,auto);    { 'Fehler beim Bearbeiten der Node-/Pointdiff' }
      log(getres2(2130,4));   { dito }
      UDiff:=false;
    end;
  end;

  function CopyUpdateFile:boolean;    { Update kopieren }
  begin
    TmpDoDiff := false;
    CopyUpdateFile:=false;
    if (filetime(newlist)<>filetime(ufile)) or
       (_filesize(newlist)<>_filesize(ufile)) then
      if diskfree(0)+_filesize(NodeList.GetFilename(i)) < _filesize(ufile)+8192 then begin
        arfehler(2113,auto);  { 'Zu wenig Plattenplatz zum Kopieren des Nodelist-Updates' }
        log(getreps2(2130,5,extractfilename(ufile)));  { 'Zu wenig Plattenplatz zum Kopieren von %s' }
        end
      else begin
        SafeDeleteFile(newlist);
        log(ufile+' -> '+newlist);
        if filecopy(ufile,newlist) then
          CopyUpdateFile:=true
        else
          log(getreps2(2130,6,ufile));     { 'Fehler beim Kopieren von %s' }
        end
    else
      TmpDoDiff := true;
  end;

begin   //function  DoDiffs(files:string; auto:boolean):byte;
  DoDiffs:=1;
  reindex:=false;
  logopen:=false;
  diffdir:=ExtractFilePath(files);
  diffnames:=extractfilename(files);

  for i:=0 to NodeList.Count - 1 do
  with TNodeListItem(Nodelist.Items[i]) do
  begin
    ucount:=5;
    nextnr:=number;
    repeat
      done:=false;
      nextnr:=NextNumber(nextnr);
      newlist:=FidoDir+ReplNr(listfile,nextnr);   { Dateinamen expandieren }
      uarchive:=ReplNr(fupdatearc,nextnr);
      ufile:=ReplNr(fupdatefile,nextnr);
      ExpandFilePath(uarchive);
      unarcflag:=false;                           { Update-Archiv auspacken }
      if (uarchive<>'') and passend(uarchive) then begin
        SafeDeleteFile(diffdir+ufile);
        log(getreps2(2130,1,uarchive));      { 'entpacke %s' }
        unarcflag:=UniExtract(uarchive,diffdir,ufile);
        if not unarcflag then
          log(getres2(2130,2));              { 'Fehler beim Entpacken' }
        diffnames:=WildCard;
        end;
      ExpandFilePath(ufile);
      if FileExists(ufile) and passend(ufile) then begin
        if fprocessor<>'' then ExecProcessor(fprocessor);
        if fDoDiff and UDiff then begin       { Update diffen }
          number:=nextnr;
          NodeList.SaveConfigToFile;
          done:=true;
          if listfile='NODELIST.###' then
            ShrinkNodelist(false);
          end;
        if not fDoDiff and CopyUpdateFile then
        begin    { Update kopieren }
          if pos('##',listfile)>0 then begin
            _era(FidoDir+NodeList.GetFilename(i));      { alte Liste lîschen }
            number:=nextnr;
            NodeList.SaveConfigToFile;
            end;
          done:=true;
          end;
        if TmpDoDiff then DoDiffs := 2; { MK 01/00 Workaround TMT }
        if unarcflag then _era(ufile);
        end;
      if done then begin
        if auto and fDelUpdate then begin    { evtl. Update-Files lîschen }
          SafeDeleteFile(uarchive);
          SafeDeleteFile(ufile);
          end;
        reindex:=true;
        end;
      if done then DoDiffs:=0;
      dec(ucount);
    until (pos('##',listfile)=0) or (not done and (fdodiff or (ucount=0)));
    end;

  freeres;
  if logopen then close(logfile);
  if reindex then begin
    if Nodelist.Open then CloseNodeIndex;
    MakeNodelistindex;
    OpenNodeindex(NodeIndexF);
    end;
end;


procedure ManualDiff;
var fn      : string;
    useclip : boolean;
begin
  fn:=FilePath+'*.*';
  useclip:=false;
  if ReadFilename(getres(1020),fn,true,useclip) then   { 'Node-/Pointlist-Update einbinden' }
    if not FileExists(fn) then
      rfehler(22)     { 'Datei ist nicht vorhanden!' }
    else
      case DoDiffs(fn,false) of
        1 : rfehler(1010);    { 'Datei konnte nicht als Node-/Pointlist-Update eingebunden werden.' }
        2 : rfehler(2124);    { 'Diese Datei ist bereits eingebunden.' }
      end;
end;

{
  $Log$
  Revision 1.42  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.41  2001/11/22 17:40:02  mk
  - call node diff update directly

  Revision 1.40  2001/11/22 10:39:58  mk
  - NDIFF is intern, removed test for ndiff.exe

  Revision 1.39  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.38  2001/09/07 23:24:55  ml
  - Kylix compatibility stage II

  Revision 1.37  2001/09/07 13:54:24  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.36  2001/07/31 16:18:41  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.35  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.34  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.33  2001/01/07 12:34:36  mo
  - einig  ƒnderungen an TNodeList

  Revision 1.32  2001/01/06 21:13:37  mo
  - ƒnderung an TnodeListItem

  Revision 1.31  2001/01/06 17:18:08  mk
  - fixed some TNodeListItem-Bugs

  Revision 1.30  2000/12/29 22:46:57  mo
  - kein crash wenn bei Programmstart 'FIDO\NODELIST.IDX nicht vorhanden

  Revision 1.29  2000/12/29 16:44:25  mo
  - class TNodeList, new procedure AddEntry

  Revision 1.28  2000/12/28 06:23:17  mo
  -Support f¸r alte Nodelisteneonfiguration entfernt

  Revision 1.27  2000/12/27 22:36:31  mo
  -new class TfidoNodeList

  Revision 1.26  2000/12/10 10:54:56  mo
  -TNodelistItem in eine Klasse umgewandelt

  Revision 1.25  2000/11/18 21:41:26  mk
  - compile fix

  Revision 1.24  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.23  2000/11/15 23:00:43  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.22  2000/11/14 15:51:36  mk
  - replaced Exist() with FileExists()

  Revision 1.21  2000/11/14 11:14:34  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.20  2000/10/17 12:53:19  mk
  - einige Funktionen auf Sysutils umgestellt

  Revision 1.19  2000/10/17 10:05:58  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.18  2000/08/19 09:41:36  mk
  - Code aufgeraeumt

  Revision 1.17  2000/08/04 09:02:49  mk
  - Bug in NLFilename nach Stringlistumestellung behoben

  Revision 1.16  2000/08/01 08:40:41  mk
  - einige String-Parameter auf const geaendert

  Revision 1.15  2000/07/12 15:27:01  hd
  - Ansistring

  Revision 1.14  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.13  2000/07/05 13:55:02  hd
  - AnsiString

  Revision 1.12  2000/07/04 12:04:29  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.11  2000/07/03 13:31:44  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.10  2000/07/02 14:24:54  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.9  2000/06/29 13:01:01  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.8  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.7  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.


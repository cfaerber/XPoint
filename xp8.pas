{   $Id$

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

{ CrossPoint - 'maps & Fileserver }

{$I xpdefine.inc}

unit xp8;

interface

uses sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,datadef,database,lister, winxp,
  maske,maus2,resource,win2,xp0,xp1,xp1o2,xp1help,xp1input,xp2c,xp_iti,
  xpglobal,fidoglob;


procedure SendMaps(bef:string; var box,datei:string);
procedure MapsDelBrett(brett:string);
procedure MapsReadList;
procedure MapsReadFile;
procedure ReadPromafList(const fn:string; var bfile:string);
procedure MapsBrettliste(art:byte);
procedure MapsCommands(defcom:byte);   { 0=Auswahl, 1=Brettliste holen }
procedure GetSysfile;

procedure AddFileechos;
procedure RemoveFileechos;
procedure FilescanReadlist;
procedure FilescanReadFile;
procedure FilescanCommands(cmd:shortint);


function  IsServer(box:string; var fstype:byte):boolean;
procedure FS_ReadList(msg:boolean);
procedure FS_command(comm:string; request:byte);

function testmark(const s:string; block:boolean):boolean;
function BrettMark(const s:string; block:boolean):boolean;
function MapsListcolor(const s:string; line:longint):byte;
function UUsendTestSourcefile(var s:string):boolean;
function fileechomarkfunc(const s:string; block:boolean):boolean;
function fileechocolfunc(const s:string; line:longint):byte;


implementation  { ------------------------------------------------- }

uses xp1o,xp3,xp3o2,xp3ex,xp4,xpsendmessage,xpsendmessage_unsent,
{$IFDEF Sockets }
  xpncnntp,
{$ENDIF }
  xp9bp,xpconfigedit,xpnt, crc, classes, debug;

const mapsbox : string = '';

var mapsname : string;
    mapsnt   : byte;
    mapsart  : byte;


function mapstype(box:string):byte;  { 0=MAPS, 1=AREAFIX, 2=MAF, 3=Maus, 4=Q. }
var d  : DB;                         { 5=Fido, 6=G&S, 7=changesys, 8=Pronet,  }
    nt : byte;                       { 9=Turbobox, 10=ZQWK, 11=GUP, 12=AutoSys}
begin                                { 13=Feeder, 14=postmaster, 15=online, 16=client  }
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if not dbFound then
    mapstype:=0
  else begin
    mapsname:= dbReadStr(d,'nameomaps');     { mu· vor MAF-Test stehen !! }
    dbRead(d,'netztyp',nt);
    if ntMAF(nt) then
      mapstype:=2
    else if ntNude(nt) then
      mapstype:=3
    else if ntQuickMaps(nt) then
      mapstype:=4
    else if ntAreamgr(nt) then
      mapstype:=5
    else if nt in [nt_NNTP,nt_POP3,nt_IMAP,nt_Client] then
      mapstype := 16
    else if (nt=nt_UUCP) then begin
      ReadBoxpar(nt,box);
      case Boxpar^.BMtyp of
        bm_changesys : mapstype:=7;
        bm_GUP       : mapstype:=11;
        bm_Feeder    : mapstype:=13;
        bm_AutoSys   : mapstype:=12;
        else           mapstype:=14;  { postmaster }
      end;
      end
    else if (nt=nt_Pronet) then
      mapstype:=8
    else if nt=nt_QWK then
      mapstype:=10
    else if nt=nt_NNTP then
      mapstype:=15
    else
      if mapsname='AREAFIX' then mapstype:=1
      else if mapsname='SYSTEM' then mapstype:=6  { G&S }
      else mapstype:=0;
    end;
  dbClose(d);
end;

function BoxHasMaps(const box:string):boolean;
begin
  Result := not ntNoMaps(ntBoxNetztyp(box));
  if not Result then 
    rfehler(801);   { 'Diese Box unterst¸tzt keine Brettbestell-Funktionen.' }
end;

procedure SendMaps(bef:string; var box,datei:string);
var
    hf : string;
    mt : byte;
    nt : byte;

  procedure AreaBef;
  var t1,t2 : text;
      tn    : string;
      s     : string;
  begin
    if (bef='ADD') or (bef='DEL') then begin
      tn:=TempS(_filesize(datei)*2);
      assign(t1,datei); reset(t1);
      assign(t2,tn); rewrite(t2);
      while not eof(t1) do begin
        readln(t1,s);
        write(t2,bef,' ',s,#13#10);
        end;
      close(t1); erase(t1);
      close(t2); rename(t2,datei);
      end
    else begin
      assign(t1,datei);
      rewrite(t1);
      write(t1,bef,#13#10);
      close(t1);
      end;
    bef:='AREAFIX';
  end;

  procedure MafNude(maf,promaf:boolean);
  var t1,t2 : text;
      tn    : string;
      s     : string;
  begin
    ReadBoxpar(0,box);
    if (bef='ADD') or (bef='DEL') then begin
      tn:=TempS(_filesize(datei)*2);
      assign(t1,datei); reset(t1);
      assign(t2,tn); rewrite(t2);
      if maf then
        write(t2,'%',boxpar^.passwort,#13#10);
      while not eof(t1) do begin
        readln(t1,s);
        if trim(s)<>'' then begin
          if not (maf or promaf) then write(t2,'G');    { Maus-Gruppe }
          if bef='ADD' then write(t2,iifs(promaf,'','+'),s,#13#10)
          else write(t2,'-',s,#13#10);
          end;
        end;
      close(t1); erase(t1);
      close(t2); rename(t2,datei);
      if promaf then bef:='Bretter';
      end
    else if not promaf then begin
      assign(t1,datei);
      rewrite(t1);
      if maf then begin
        write(t1,'%',boxpar^.passwort,#13#10);
        write(t1,'%',bef,#13#10);
        end
      else
        write(t1,bef,#13#10);
      close(t1);
      end;
    if maf then bef:='BRETTER'
    else if not promaf then bef:='<Maus-Command>'
    else forceabs:='SYSOP';
  end;

  procedure ChangeSys;
  type bnodep = ^bnode;
       bnode  = record
                  l,r : bnodep;
                  c   : string;
                  del : boolean;
                end;
  var t       : text;
      root    : bnodep;
      sysfile : string;
      _brett  : string;
      s       : string;
      syspos  : byte;
      first   : boolean;
      d       : DB;

    procedure SetBrett(add:boolean; var bn:bnodep);

      {procedure setbretts;
      begin bn^.c:=s; end;}

      function smaller:boolean;
      begin smaller:=(s<bn^.c); end;

      function found:boolean;
      begin found:=(s=bn^.c); end;

    begin
      if bn=nil then
        if add then begin   { ADD - Brett hinzufÅgen }
          new(bn);
          bn^.l:=nil; bn^.r:=nil; bn^.del:=false;
          bn^.c:=s;
          {setbretts;}
          end
        else                { DEL - Brett nicht vorhanden }
      else
        if found then
          if add then         { ADD - Brett schon vorhanden }
          else bn^.del:=true  { Brett lîschen }
        else
          if smaller then SetBrett(add,bn^.l)   { links suchen }
          else SetBrett(add,bn^.r);             { rechts suchen }
    end;

    procedure syswrite(bn:bnodep);
    begin
      if bn<>nil then begin
        syswrite(bn^.l);
        if not bn^.del then
          if syspos=0 then write(t,bn^.c,#13#10)
          else begin
            if not first then write(t,',')
            else first:=false;
            if syspos+length(bn^.c)>77 then begin
              write(t,'\',#13#10); syspos:=1; end;
            write(t,bn^.c);
            inc(syspos,length(bn^.c)+1);
            end;
        syswrite(bn^.r);
        end;
    end;

    procedure freelist(bn:bnodep);
    begin
      if bn<>nil then begin
        freelist(bn^.l);
        freelist(bn^.r);
        dispose(bn);
        end;
    end;

  begin
    sysfile:=GetServerFilename(box, extBbl);
    root:=nil;
    if FileExists(sysfile) then begin         { alte .BBL-Datei einlesen }
      assign(t,sysfile);
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        if (s<>'') and (s[1]<>'#') then SetBrett(true,root);
        end;
      close(t);
      end;
    assign(t,datei);                     { Brett(ab)bestellungen einlesen }
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if (s<>'') and (s[1]>='a') then
        if bef='ADD' then SetBrett(true,root)
        else SetBrett(false,root);
      end;
    close(t);
    assign(t,sysfile);                   { neue .BBL-Datei schreiben }
    rewrite(t); syspos:=0; first:=false;
    syswrite(root);
    close(t);
    assign(t,datei);                     { neue (Ab)Bestellung schreiben }
    ReadBoxPar(nt_UUCP,box);
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,UpperCase(box));
    rewrite(t);
    write(t,'system: ',boxpar^.pointname,
                       iifs(boxpar^.BMdomain,dbReadStr(d,'domain'),''),#13#10);
    write(t,'passwd: ',boxpar^.AreaPW,#13#10);
    write(t,'sysentry: '); syspos:=30; first:=true;
    dbClose(d);
    syswrite(root);
    write(t,#13#10);
    close(t);
    freelist(root);
    bef:='setsys';
    s:=mapsname+'@'+box+ntServerDomain(box);  { evtl. alten setsys-Befehl lîschen }
    dbSeek(ubase,uiName,UpperCase(s));
    if dbFound then begin
      _brett:=mbrettd('U',ubase);
      dbSeek(mbase,miBrett,_brett+dup(4,#255));
      if dbEOF(mbase) then dbGoEnd(mbase)
      else dbSkip(mbase,-1);
      while not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
            odd(dbReadInt(mbase,'unversandt')) do begin
        if dbReadStrN(mbase,mb_betreff)='setsys' then
          Unversandt(false,false);
        dbSkip(mbase,-1);
        end;
      end;
  end;

  procedure Guppie(typ:byte);   { 1=GUP, 2=AutoSys, 3=Feeder }
  var tn    : string;
      t1,t2 : text;
      s     : string;
      d     : DB;
      domain: string;
  begin
    ReadBoxpar(0,box);
    tn:=TempS(_filesize(datei)*2);
    assign(t1,datei); reset(t1);
    assign(t2,tn); rewrite(t2);
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,UpperCase(box));
    if boxpar^.BMdomain then domain:=dbReadStr(d,'domain')
    else domain:='';
    case typ of
      1 : write(t2,'site ',boxpar^.pointname,domain,' ',boxpar^.AreaPW,#13#10);
      2 : write(t2,'host ',boxpar^.pointname,domain,' ',boxpar^.AreaPW,#13#10);
      3 : write(t2,'@id ',boxpar^.pointname,domain,' ',boxpar^.AreaPW,#13#10);
    end;
    dbClose(d);
    write(t2,#13#10);
    if (bef='ADD') or (bef='DEL') then begin
      if typ=3 then write(t2,'@append',#13#10);
      while not eof(t1) do begin
        readln(t1,s);
        if (trim(s)<>'') and (s[1]<>'#') and (s[1]<>'-') then
          if (bef='ADD') then
            case typ of
              1 : write(t2,'include ',s,#13#10);
              2 : write(t2,'add ',s,#13#10);
              3 : write(t2,s,#13#10);
            end
          else
            case typ of
              1 : write(t2,'delete ',s,#13#10);
              2 : write(t2,'rem ',s,#13#10);
              3 : write(t2,'!',s,#13#10);
            end;
        end;
      if typ=3 then write(t2,'@end',#13#10);
      end
    else
      write(t2,bef,#13#10);
    if (bef<>'help') and (typ<>3) then write(t2,'quit',#13#10);
    close(t1); erase(t1);
    close(t2); rename(t2,datei);
  end;

begin
  mt:=mapstype(box);
  nt:=ntBoxNetztyp(box);
  case mt of
    1 : AreaBef;
    2 : MafNude(true,false);
    3 : MafNude(false,false);
    5 : begin
          ReadBoxpar(nt,box);
          if (bef='ADD') or (bef='DEL') or (bef='') then
            bef:=boxpar^.AreaPW
          else
            bef:=boxpar^.AreaPW+' '+bef;
        end;
    6 : if bef='ADD' then bef:='BRETT +'
        else if bef='DEL' then bef:='BRETT -';
    7 : if (bef='ADD') or (bef='DEL') or (bef='setsys') then
          ChangeSys;
    8 : MafNude(false,true);   { ProNet }
    9 : begin
          mapsname:='SYSTEM';
          ControlMsg:=true;
        end;
   10 : begin
          mapsname:='ZQWK';
          bef:='CONFIG';
        end;
   11 : Guppie(1);
   12 : Guppie(2);
   13 : Guppie(3);
   14 : ;   { Postmaster }
   15 : ; { NNTP }
  end;
  hf:='';
  _sendmaps:=true;
  forcebox:=box;
  if DoSend(true,datei,false,false,mapsname+'@'+box+ntServerDomain(box),bef,
            false,false,false,false,false,nil,hf,0) then;
  _sendmaps:=false;
end;


procedure wr_btext(var t:text; del,news:boolean);
var bretter : string;
begin
  write(t,'##  ',getres2(800,1),#13#10);   { 'Lieber Systemverwalter,' }
  write(t,'##',#13#10);
  bretter:=getres2(800,iif(news,2,3));
  write(t,'##  ',getreps2(800,iif(del,4,5),bretter),#13#10);
  write(t,'##',#13#10);
  write(t,'##  ',getres2(800,6),#13#10);   { 'mit virtuellen GrÅ·en' }
  write(t,'##     '+xp_xp+' ',verstr,#13#10);
  write(t,#13#10);
  write(t,dup(40,'-'),#13#10);
  write(t,#13#10);
  freeres;
end;

function Get_BL_Name(const box:string):string;
var
  s1: string;
begin
  ReadBox(0,box,boxpar);
  s1:= GetServerFilename(box, '');
  if not FileExists(s1+extBl) then                     { Brettliste im XP-Verzeichnis }
    s1 := Boxpar^.ClientPath+s1;
  Get_BL_Name:=FileUpperCase(s1+iifs(FileExists(s1+extBl),extBl, extGr));   { oder .BL/.GR im Client-Verz. }
end;

{ RFC/Client: RC-File anhand der im Lister markierten Bretter manipulieren }
{ Im Lister muss ein durch Read_RC_File erzeugtes BL-File sein.            }

function MakeRC(bestellen: boolean; const box: string; List: TLister):boolean;
var fRCFile       : text;
    fNewRCFile    : text;
    fBLFile       : file of char;
    rcfile,blfile : string;
    line          : string;
    Line2         : string;
    Articles      : String;
    fileofs       : Longint;
    x,y           : Integer;
    c             : char;
    brk           : boolean;
label makercend;

begin
  moment;
  MakeRc:=true;
  ReadBox(0, GetServerFilename(Box, ''),boxpar);
  rcfile:=FileUpperCase(BoxPar^.ClientPath) + GetServerFilename(box, extRc);
  blfile:=Get_BL_Name(box);
  if not FileExists(blfile) then
  begin
    rfehler(807);
    exit;
    end;
  Assign(fRCFile,rcfile);
  if not (FileExists(rcfile)) then
  begin
    Rewrite(fRCFile);
    Close(fRCFile);
    end;
  Assign(fBLFile,blfile);
  Reset(fBLFile);

  if bestellen then                        { Neue Bretter an RC-File anhaengen }
  begin
    c:='*';
    Articles := '10';
    dialog(length(getres2(10800,40))+9,3,getres2(10800,39),x,y);  { 'Newsgroups bestellen' }
    maddstring(2,2,getres2(10800,40), Articles,4,4,'1234567890');  { 'Anzahl der Artikel'   }
    mhnr(11910);
    readmask(brk);
    enddialog;
    if brk then
    begin
      MakeRc:=false;
      goto MakeRCEnd;
      end;
    Append(fRCFile);
    line := List.FirstMarked;                  { Im Lister sind markierte neue Bretter...}
    articles:=' -'+Articles;
    while line<>#0 do
    begin
      if line[1]='*' then List.UnMarkLine      { Bereits bestellte Bretter entmarkieren }
      else begin
        writeln(fRCFile, TrimRight(copy(Line,3,78)),Articles);    { ansonsten an .RC anhaengen }
        if ntBoxNetztyp(box)=nt_Client then begin
          fileofs:=ival(mid(line,80));         { Offset ins BL-File wurde von READ_BL_FILE }
          seek(fBLFile,fileofs);               { an die Listerzeile angehaengt. Jetzt wird }
          write(fBLFile,c);                    { an den Zeilenanfang ein "*" geschrieben,  }
          end;                                 { der ab jetzt das Bestellt-Flag darstellt  }
        end;
      line := List.NextMarked;
      end;
    Close(fRCFile);
    close(fBLFile);
    end

  else begin                               { Bestellte Bretter aus RC entfernen }
    line:= List.FirstLine;
    c:=' ';
    MakeRc:=false;
    Assign(fNewRCFile,TempFile(''));
    ReWrite(fNewRCFile);
    reset(fRCFile);
    while not eof(fRCFile) do
    begin
      readln(fRCFile,line2);                   { Zeile aus RC lesen }
      brk:=true;
      line := List.FirstMarked;                { Im Lister sind Bretter zum  }
      while line<>#0 do                        { Abbestellen markiert...     }
      begin
        line:= TrimRight(copy(Line,3,78));
        if line=LeftStr(line2,cposx(' ',line2)-1)
        then begin
          if ntBoxNetztyp(box)=nt_Client then begin
            fileofs:=ival(mid(line,80));       { Offset aus READ_BL_FILE (s.o.) }
            seek(fBLFile,fileofs);             { Abzubestellendes Brett gefunden... }
            write(fBLFile,c);                  { im BL-File "Bestellt"-'*' loeschen }
            end;
          List.UnMarkLine;                     { abbestelltes Brett demarkieren     }
          brk:=false;                          { und nicht in RC-Kopie uebernehmen  }
          end;
        line:= List.NextMarked;
        end;
      if brk then writeln(fNewRCFile,line2);   { Nicht abbestellte Zeilen kopieren  }
      end;
    close(fBLFile);
    Close(fRCFile);
    Close(fNewRCFile);
    Erase(fRCFile);
    Rename(fNewRCFile,rcfile);
    end;

makercend:
  List.Free;
  {$IFNDEF Delphi }
  InOutRes:=0;
  {$ENDIF }
  closebox;
end;



{ RFC/Client: Brettliste in Lister laden, Vorbereitung fuer MakeRC-Aktionen }

procedure Read_BL_File(s:string;bestellen:boolean; List: TLister);
var t1,t2    : text;
    s1: ShortString;
    tname : string;
    i        : longint;
    n,m      : byte;

  //!! ToDo: In Pascal umschreiben
  { Brettlisten-Zeilen im UKA* Format vor der        }
  { Uebergabe an den Lister ins XP-Format bringen    }
  { und Offsetanpassung fuer Bestellt-Flag ermitteln }
  Function Reformat_UKA_Brett(Var s:Shortstring):byte;
{$IFDEF NOASM }
begin
end;
{$ELSE }
assembler; asm
        push ebx
        push ecx
        push esi
        push edi
        mov esi,s
        xor eax,eax
        cmp byte ptr [esi+2],' '
        je @end                  { Abbruch wenn's eine Liste im XP-Format ist... }
        push esi
        lodsb
        mov ecx, eax
        mov ebx, eax
        mov dx,' *'
  @1:   lodsb                    { Brettnamenende suchen }
        cmp al,dh
        je @2
        cmp al,'˚'               { v und * werden als Bestellt-Flag akzeptiert }
        je @3
        cmp al,dl
        je @3
        inc ebx
        loop @1
  @2:   mov dl,' '
  @3:   pop esi
        mov cl,bl
        add bl,2
        push ebx                 { Offset zum Flag in UKA-Brettliste sichern }
        mov byte ptr [esi],bl
        add esi,ecx
        lea edi,[esi+2]
        std
        rep movsb                { s:='* '+s }
        dec edi
        mov eax, edx
        stosw
        cld
        pop eax
  @end: pop edi
        pop esi
        pop ecx
        pop ebx
  end;
{$ENDIF }

begin
  moment;      
  assign(t1,s);
  reset(t1);
  tname:=TempFile('');
  assign(t2,tname);
  Rewrite(t2);
  i:=0;
  readln(t1,s1);
  if FirstChar(s1)='!' then i:=length(s1)
   else reset(t1);
  while not eof(t1) do
  begin
    FillChar(s1[1], 255, ' ');
    readln(t1,s1);
    if(s1[2]<>' ')then
      // reformat NNTP style list to Client style list (quick hack)
      if RightStr(s1,2)=' *' then
        s1:='* '+LeftStr(s1,Length(s1)-2)
      else
        s1:='  '+s1;
    m:=length(s1)+2;
//    n:=reformat_UKA_Brett(s1);
    if bestellen or (s1[1]='*') then       { File-Offset des Strings wird angehaengt,  }
     writeln(t2,forms(s1,80)+strs(i+n));   { damit MakeRC schnellen Zugriff hat, um im }
    inc(i,m);                              { BL-File den '*' zu setzen bzw zu loeschen }
    end;
  close(t1);
  close(t2);
  List.ReadFromFile(tname,0, false);
  List.HeaderText := s;
  _era(tname);
  closebox;
end;

procedure MapsKeys(list:TLister;var t:taste); forward;

{ RFC/Client: Bretter anhand eines Files abbestellen (Brettfenster) }
procedure File_abbestellen(const box,GroupsToUnsubscribeFilename:string);
var fGroupsToUnsubscribe: Text;
    sBLFileName,s1,s2, rcfile: string;
    rc: TStringList;
    i: Integer;
begin
  sBLFileName:=Get_BL_Name(box);
  if not fileExists(sBLFileName) then
  begin
    rfehler(807);
    exit;
  end;
  rc := TStringlist.Create;
  rc.LineBreak := #$0D#$0A;
  try
    rcfile:= ChangeFileExt(sBLFilename, extRc);
    rc.LoadFromFile(rcfile);
    assign(fGroupsToUnsubscribe,GroupsToUnsubscribeFilename);
    reset(fGroupsToUnsubscribe);
    while not eof(fGroupsToUnsubscribe) do
    begin
      readln(fGroupsToUnsubscribe,s2);
      for i := 0 to rc.Count -1 do
      begin
        s1 :=LeftStr(rc[i],cposx(' ',rc[i])-1);
        if LeftStr(rc[i],cposx(' ',rc[i])-1) = s2 then
        begin
          rc.Delete(i);
          break;
        end;
      end;
    end;
    rc.SaveToFile(rcfile);
  finally
    rc.Free;
  end;
end;


Procedure ClientBl_Abgleich(box:string);
var t1    : text;
    f1    : file of char;
    c     : char;
    s1,s2 : string;
    brk   : boolean;
    fileofs : longint;
    List: TLister;
begin

  ReadBox(0,box,boxpar);
  s1:=FileUpperCase(BoxPar^.ClientPath) + GetServerFilename(box, extRc);
  Assign(t1,s1);       { BOX.RC }
  if not (FileExists(s1)) then
  begin
    Rewrite(t1);                           { T1 = RC-FILE (Text)         }
    Close(t1);
    end;
  s1:=get_BL_Name(box);
  if not FileExists(s1) then
  begin
    rfehler(807);
    exit;
    end;
  if not ReadJN(getreps2(810,92, UpperCase(s1)),true) then exit;  { 'Abgleich RC-Datei mit %s' }

  moment;
  List := TLister.CreateWithOptions(1,80,4,4,-1,'/M/SB/S/');        { Dummy-Lister }
  read_BL_File(s1,true, List);             { Bestellt-Liste in Lister laden }
  pushkey(^A);                             { Ctrl+A = Alles markieren  }
  pushkey(keyesc);                         { Esc    = Lister verlassen }
  List.Show;                               { Dummy-Lister starten      }
  assign(f1,s1);
  reset(f1);
  s1:= List.FirstMarked;                   { Brettliste mit .RC vergleichen }
  while s1<>#0 do
  begin
    brk:=false;
    reset(t1);
    while not eof(t1) do
    begin
      readln(t1,s2);
      if LeftStr(s2,cposx(' ',s2)-1)= TrimRight(copy(s1,3,78))
        then brk:=true;
      end;
    close(t1);
    if (s1[1]='*') xor brk
    then begin
      fileofs:=ival(mid(s1,80));
      c:=iifc(brk,'*',' ');
      seek(f1,fileofs);
      write(f1,c);
      end;
    s1:= List.NextMarked;
    end;
  close(f1);
  List.Free;
  closebox;
  aufbau:=true;
end;

Procedure ClientBL_Sort(box:string);
var
  blfile: String;
  List: TStringList;
begin
  blfile:=get_BL_Name(Box);
  if not FileExists(blfile) then
  begin
    rfehler(807);         { Keine Brettliste fÅr diese Box vorhanden! }
    exit;
  end;
  moment;
  List := TStringList.Create;
  List.LineBreak := #$0D#$0A;
  try
    try
      List.LoadFromFile(blfile);
      List.Sort;
      List.SaveToFile(blFile);
    except
      rfehler(836);      { Sortierung der Newsgroup-Liste ist fehlgeschlagen! }
    end;
  finally
    List.Free;
  end;
  Closebox;
end;

Procedure  ClientBL_Del(const box:string);
var
  Filename: string;
begin
  Filename := get_BL_Name(Box);
  if ReadJN(getreps2(810,93, LeftStr(Filename, 60)),false) then _era(Filename);  { '%s wirklich lîschen' }
end;


{ bbase-aktuelles Brett abbstellen   }
{ brett='' -> markierte Bretter abb. }

procedure MapsDelBrett(brett:string);
const maxmaggi = 500;

type maggibrett  = record
                     code  : string;
                     name  : string;
                   end;
    ma           = array[1..maxmaggi] of maggibrett;
var t     : text;
    fn    : string;
    box   : string;
    bfile : string;
    i,nr  : integer;
    d     : DB;
    topen : boolean;
    maf   : boolean;
    maus  : boolean;
    quick : boolean;
    fido  : boolean;
    gs    : boolean;
    uucp  : boolean;
    rfc   : Boolean;
    postmaster : boolean;
    pronet: boolean;
    qwk   : boolean;
    map   : ^ma;
    mm    : integer;

  { s. auch MAGGI.loadbretter! }

  procedure ReadBrettliste;
  var t : text;
      s : string;
  begin
    mm:=0;
    assign(t,bfile+extBl);
    reset(t);
    if ioresult=0 then begin
      message(getreps(801,UpperCase(box)));   { 'Brettliste fÅr %s laden...' }
      while (mm<maxmaggi) and not eof(t) do begin
        readln(t,s);
        if maf then begin
          inc(mm);
          if s[41]<>' ' then
            map^[mm].code:=copy(s,41,4)
          else
            map^[mm].code:=trim(copy(s,41,6));
          s:=trim(LeftStr(s,40));
          if FirstChar(s)<>'/' then s:='/'+s;
          while cpos(' ',s)>0 do s[cpos(' ',s)]:='_';
          map^[mm].name:=UpperCase(s);
          end
        else   { ProNet }
          if FirstChar(s)<>';' then begin
            inc(mm);
            map^[mm].code:=LeftStr(s,4);
            map^[mm].name:=trim(mid(s,32));
            end;
        end;
      close(t);
      closebox;
      end;
    if ioresult<>0 then;
  end;

  function brettcode(brett:string):string;
  var i : integer;
  begin
    UpString(brett);
    i:=1;
    while (i<=mm) and (brett<>map^[i].name) do inc(i);
    if i<=mm then brettcode:=map^[i].code
    else brettcode:='';
  end;

  procedure GetDel(const txt:string);
  var width,x,y : Integer;
      t         : taste;
  begin
    width:=max(33,length(txt)+5);
    diabox(width,5,'',x,y);
    mwrt(x+2,y+1,txt+'?');
    t:='';
    nr:=readbutton(x+2,y+3,2,getres(802),1,true,t);   { '  ^Ja  , ^Nein , ^Liste ' }
    closebox;
  end;

  function newsgroup(s:string):string;
  var i : integer;
  begin
    delete(s,1,2);
    for i:=1 to length(s) do
      if s[i]='/' then s[i]:='.';
    newsgroup:=s;
  end;

  function qwkbrett(brett:string):string;
  var t : text;
      s : string;
  begin
    brett:=UpperCase(mid(brett,length(boxpar^.magicbrett)+2));
    assign(t,bfile+extBl);
    qwkbrett:='';
    if existf(t) then begin
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        if pos(brett,UpperCase(s))>0 then
          qwkbrett:=LeftStr(s,3);
        end;
      close(t);
      end;
  end;

begin
  if brett='' then
    if bmarkanz=0 then
      nr:=3
    else
      GetDel(getreps(803,strs(bmarkanz)))   { '%s markierte Bretter abbestellen' }
  else
    if FirstChar(brett)<>'A' then
      nr:=3
    else
      GetDel(getreps(804,copy(brett,2,40)));   { '%s abbestellen' }
  if nr=3 then
    MapsBrettliste(1)
  else if nr=1 then begin
    if brett<>'' then begin
      box:= dbReadStrN(bbase,bb_pollbox);
      if box='' then begin
        rfehler(802);   { 'Dieses Brett hat keine Serverbox!' }
        exit; end;
      if not IsBox(box) then begin
         rfehler1(803,box);   { 'Unbekannte Serverbox: %s' }
        exit; end;
      if not BoxHasMaps(box) then exit;
      end;
    new(map);
    fn:=TempS(10000);
    assign(t,fn);
    if brett<>'' then begin
      maf:=false; maus:=false; quick:=false; fido:=false; gs:=false;
      uucp:=false; pronet:=false; qwk:=false; rfc := false;
      postmaster:=false;
      case mapstype(box) of
        2 : maf:=true;
        3 : maus:=true;
        4 : quick:=true;
        5 : fido:=true;
        6 : gs:=true;
        7 : uucp:=true;
        8 : pronet:=true;
       10 : qwk:=true;
       11..13 : uucp:=true;
       14: begin uucp:=true; postmaster:=true; end;
       16: rfc:= true;
      end;
      rewrite(t);
      if quick or (uucp and postmaster) then
        wr_btext(t,true,uucp);
      if maus or fido or qwk then
      begin
        ReadBoxPar(0,box);
        if copy(UpperCase(brett),2,length(boxpar^.magicbrett))=UpperCase(boxpar^.magicbrett)
        then
          if qwk then begin
            bfile:=GetServerFilename(box, '');
            write(t,'DROP ',qwkbrett(brett),#13#10);
            end
          else begin
            if fido then write(t,'-');
            write(t,mid(brett,length(boxpar^.magicbrett)+2),#13#10);
            end;
        end
      else if not maf then
        if gs then write(t,copy(brett,3,brettlen),#13#10)
        else if uucp then write(t,newsgroup(brett),#13#10)
        else write(t,copy(brett,2,brettlen),#13#10)
      else begin
        dbOpen(d,OwnPath+BoxenFile,1);
        dbSeek(d,boiName,UpperCase(box));
        if dbFound then begin
          bfile:= dbReadStr(d,'dateiname');
          ReadBrettliste;
          write(t,brettcode(copy(brett,2,40)),#13#10);
          end;
        dbClose(d);
        end;
      if fido then
        write(t,'---',#13#10);
      close(t);
      if not rfc then 
        SendMaps('DEL',box,fn)
      else 
      begin
        rewrite(t);
        write(t,newsgroup(brett),#13#10);
        close(t);
        File_Abbestellen(box,fn);
      end;
    end
    else begin   { mehrere markierte Bretter }
      dbOpen(d,OwnPath+BoxenFile,1);
      while not dbEOF(d) do begin
        topen:=false;
        box:=dbReadStr(d,'boxname');
        maf:=ntMAF(dbReadInt(d,'netztyp'));
        quick:=ntQuickMaps(dbReadInt(d,'netztyp'));
        maus:=ntNude(dbReadInt(d,'netztyp'));
        fido:=ntAreaMgr(dbReadInt(d,'netztyp'));
        gs:=(dbReadInt(d,'netztyp')=nt_GS);
        uucp:=(dbReadInt(d,'netztyp')=nt_UUCP);
        rfc:=(dbReadInt(d,'netztyp') in netsRFC)and(not uucp);
        pronet:=(dbReadInt(d,'netztyp')=nt_Pronet);
        qwk:=(dbReadInt(d,'netztyp')=nt_QWK);
        bfile:= dbReadStr(d,'dateiname');
        if maus or fido or qwk or uucp then
          ReadBox(0,bfile,boxpar);
        for i:=0 to bmarkanz-1 do begin
          dbGo(bbase,bmarked^[i]);
          if UpperCase(dbReadStrN(bbase,bb_pollbox))=UpperCase(box) then begin
            if not topen then begin
              rewrite(t);
              if quick or (uucp and (boxpar^.BMtyp=bm_postmaster)) then
                wr_btext(t,true,uucp);
              if maf or pronet then ReadBrettliste;
              end;
            Brett := dbReadNStr(bbase,bb_brettname);
            if maus or fido or qwk then begin
              if copy(UpperCase(brett),2,length(boxpar^.magicbrett))=
                 UpperCase(boxpar^.magicbrett)
              then
                if qwk then write(t,'DROP ',qwkbrett(brett),#13#10)
                else begin
                  if fido then write(t,'-');
                  write(t,mid(brett,length(boxpar^.magicbrett)+2),#13#10);
                  end;
              end
            else
              if not (maf or pronet) then
                if gs then write(t,copy(brett,3,brettlen),#13#10)
                else if uucp or rfc then write(t,newsgroup(brett),#13#10)
                else write(t,copy(brett,2,BrettLen),#13#10)
              else write(t,brettcode(mid(brett,2)),#13#10);
            topen:=true;
            end;
          end;
        if topen then begin
          if fido then
            write(t,'---',#13#10);
          close(t);
          if not rfc then SendMaps('DEL',box,fn)
            else File_Abbestellen(box,fn);
          topen:=false;
          end;
        dbSkip(d,1);
        end;
      dbClose(d);
      end;
    if existf(t) then erase(t);
    dispose(map);
    end;
end;


{ Unterprozeduren fÅr MapsReadList und MapsReadFile }

function ReadMafList(fn:string; var bfile:string):boolean;
var t1,t2 : text;
    s     : string;
    ss    : string;

  function zok:boolean;
  begin
    zok:=pos('bestellt',LowerCase(s))>40;
  end;

begin
  assign(t1,fn); reset(t1);
  s:='';
  while not eof(t1) and not zok do readln(t1,s);
  if eof(t1) then begin
    closebox;
    fehler('Nachricht ist keine MAF-Brettliste!');
    ReadMaflist:=false;
    end
  else begin
    assign(t2,FileUpperCase(bfile+extBl)); rewrite(t2);
    repeat
      if zok then begin
        ss:=trim(LeftStr(s,40));
        while cpos(' ',ss)>0 do ss[cpos(' ',ss)]:='_';
        writeln(t2,forms(boxpar^.MagicBrett+ss,40),mid(s,41));
        end;
      readln(t1,s);
    until eof(t1);
    close(t2);
    closebox;
    ReadMaflist:=true;
    end;
  close(t1);
end;

procedure ReadPromafList(const fn:string; var bfile:string);
var t1,t2 : text;
    s     : string;
begin
  assign(t1,fn); reset(t1);
  s:='';
  assign(t2,FileUpperCase(bfile+extBl)); rewrite(t2);
  repeat
    readln(t1,s);
    if (s[1]=';') or (s[32]<>'/') then
      writeln(t2,s)
    else
      writeln(t2,LeftStr(s,31),LeftStr(BoxPar^.MagicBrett,length(Boxpar^.Magicbrett)-1),
                 mid(s,32));
  until eof(t1);
  close(t1);
  close(t2);
  closebox;
end;

procedure MapsReadList;
var
    absender : string;
    box      : string;
    betreff  : string;
    bfile    : string;
    d        : DB;
    fido     : boolean;
    turbo    : boolean;
    uucp     : boolean;
    fn       : string;
    bpsik    : BoxPtr;
label ende;
begin
  absender:= dbReadNStr(mbase,mb_absender);
  betreff:= dbReadNStr(mbase,mb_betreff);
  box:=systemname(absender);
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then
    bfile:= dbReadStr(d,'dateiname')
  else
    if automessaging then begin
      trfehler1(804,box,20);   { 'unbekannte Box (%s) - Brettliste wurde nicht eingelesen' }
      dbClose(d);
      exit;
      end
    else begin
      box:=UniSel(1,false,'');
      if box='' then begin
        dbClose(d);
        exit;
        end
      else
        bfile:=GetServerFilename(box, '');
      end;

  dbClose(d);
  fido:=(mapstype(box)=5);
  turbo:=(mapstype(box)=9);
  uucp:=(mapstype(box) in [7,11]);
  bpsik:=boxpar;
  getmem(boxpar,sizeof(BoxRec));
  fillchar(boxpar^,sizeof(BoxRec),0);
  ReadBox(0,bfile,boxpar);
  if mapstype(box) in [2,8] then begin
    message('Brettliste fÅr '+UpperCase(box)+' wird eingelesen ...');
    fn:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(0,'',fn,false,0);
    case mapstype(box) of
      2 : if ReadMaflist(fn,bfile) then;
      8 : ReadPromafList(fn,bfile);
    end;
    _era(fn);
    end
  else begin
    if (pos('BRETT',UpperCase(betreff))=0) and (betreff<>'Gruppenliste') and
       (pos('list',LowerCase(betreff))=0) and
      not (fido or turbo or uucp)
      and not ReadJN(getres(805),true) then   { 'Sind Sie sicher, da· das eine Brettliste ist' }
      goto ende;
    if cPos('@',absender)=0 then
      trfehler(805,60)    { 'UngÅltige Absenderangabe' }
    else begin
      message(getreps(806,UpperCase(box)));   { 'Brettliste fÅr %s wird eingelesen ...' }
      makebak(bfile+extBl,ExtBak);
      fn:=TempS(dbReadInt(mbase,'msgsize'));
      extract_msg(xTractMsg,'',fn,false,0);
      ExpandTabs(fn,FileUpperCase(bfile+extBl));
      _era(fn);
      wkey(1,false);
      closebox;
      end;
    end;
ende:
  freemem(boxpar);
  boxpar:=bpsik;
end;


{ Brettliste aus Datei nach .BL einlesen }

procedure MapsReadFile;
var
    box     : string;
    bfile   : string;
    fn      : string;
    useclip : boolean;
    d       : DB;
    maggi   : boolean;
    promaf  : boolean;
begin
  box:=UniSel(1,false,DefaultBox);
  if box='' then exit;   { brk }
  fn:=Wildcard;
  useclip:=true;
  if not ReadFilename(getres(821),fn,true,useclip) then exit;  { 'Brettliste einlesen }
  maggi:=(mapstype(box)=2);    { MagicNet }
  promaf:=(mapstype(box)=8);
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  bfile:= dbReadStr(d,'dateiname');
  dbClose(d);
  ReadBox(0,bfile,boxpar);
  message(getreps(806,UpperCase(box)));   { 'Brettliste fÅr %s wird eingelesen ...' }
  if maggi then
    if not ReadMafList(fn,bfile) then exit
    else
  else if promaf then
    ReadPromafList(fn,bfile)
  else begin
    ExpandTabs(fn,FileUpperCase(bfile+extBl));
    closebox;
    end;
  if useclip or ReadJN(getreps(817,fn),false) then    { '%s lîschen' }
    _era(fn);
end;


function BrettMark(const s:string; block:boolean):boolean;
begin
  BrettMark:=false;
  if (trim(s)='') or
     ((mapsnt=nt_Pronet) and ((s[8]='-') or (s[1] in [';','-']) or (LeftStr(s,4)='CODE')))
  then begin
    if not block then errsound;
    end
  else if (mapsnt=nt_ZConnect) and (mapsart=0) and (firstchar(s)='-') then
    rfehler(826)     { 'Dieses Brett kann nicht bestellt werden.' }
  else if (mapsnt=nt_ZConnect) and (mapsart=1) and (firstchar(s)='!') then
    rfehler(827)     { 'Dieses Brett kann nicht abbestellt werden.' }
  else
    BrettMark:=true;
end;


var LcolType : byte;  { 0=nix, 1=Z, 2=Maus, 3=ProNet, 4=Fido }

function MapsListcolor(const s:string; line:longint):byte;
begin
  MapsListcolor:=col.collisttext;
  case LColType of
    1 :   if (length(s)>2) and ((s[1]='J') or (s[1] in ['+','!'])) then
            MapsListcolor:=col.ColMapsBest;
    2,4 : if (length(s)>2) and (s[1]='+') then
            MapsListcolor:=col.colMapsBest;
    3 :   if (s[1]<>';') and (s[8]='X') then
            MapsListcolor:=col.colMapsBest;
  end;
end;

procedure MapsKeys(list:TLister;var t:taste);
begin

  if t=keytab then t:=keyctab
  else if (t=keyctab) or (t=keystab) then t:=keytab;
(*
  if t=^S then t:='s'
  else if t='s' then
  begin
    t:='';
    if Suche(getres(438),'#','') then
    begin
      ListShowSeek:=true;
      t:=keyctab;
      end;
    end;
*)
{  if t=^S then if Suche(getres(438),'#','') then begin
    ListShowSeek:=true;
    pushkey(keyctab);
    end; }

  if uppercase(t)='E' then ListShowSeek:=not Listshowseek;

  if t[1]=^H then begin
    pushkey('S');
    pushkey('* ');
    pushkey(keycr);
  end;

end;

{ art: 0=bestellen, 1=abbestellen, 2=Bretter anlegen, 3=Inhalt (EM), 4=Rescan }

procedure MapsBrettliste(art:byte);
var d      : DB;
    box    : string;
    ask    : string;
    bretter: string;
    lfile  : string;
    fn     : string;
    brk    : boolean;
    t      : text;
    s      : string;
    anz    : longint;
    netztyp: byte;
    maf    : boolean;
    promaf : boolean;
    quick  : boolean;
    maus   : boolean;
    fido   : boolean;
    gs     : boolean;
    uucp, nntp: boolean;
    changesys  : boolean;
    postmaster : boolean;
    qwk    : boolean;
    verbose: boolean;
    List: TLister;

label again;

  function fidobrett(s:string):string;
  var p : byte;
  begin
    s:=trim(s);
    p:=cpos(' ',s);
    if p=2 then begin
      s:=trim(mid(s,3)); p:=cpos(' ',s); end;
    if p=0 then p:=cpos(#9,s);
    if p>0 then s:=LeftStr(s,p-1);
    TrimFirstChar(s, '+');
    TrimFirstChar(s, '*');
    p:=pos('....',s);         { direkt angehÑngten Kommentar abschneiden }
    if p>0 then truncstr(s,p-1);
    fidobrett:=s;
  end;

  procedure writeform;
  var p  : byte;
      gr : string;
  begin
    if maf then
      if (length(s)>40) and (s[41]<>' ') then
        writeln(t,copy(s,41,4))
      else
        writeln(t,trim(copy(s,41,6)))
    else if promaf then
      writeln(t,LeftStr(s,4))
    else if maus then begin
      p:=13;
      while (p<length(s)) and (s[p]<>' ') do inc(p);
      gr:=trim(copy(s,3,p-2));
      TrimFirstChar(gr, '?'); { geheime Gruppe }
      if (length(gr)=11) and (gr[10]=' ') then
        gr:=trim(LeftStr(gr,length(gr)-1));
      writeln(t,gr);
      end
    else if fido then
    begin
      if (art=0) and boxpar^.AreaPlus then
        write(t,'+');
      if art=1 then write(t,'-');
      s:=fidobrett(s);
{      if (art=4) and not BoxPar^.areabetreff then }
{        s:=s+' -R';} {'%Rescan '+s;}     { rescan }
      write(t,s,#13#10);
    end
    else if qwk then begin
      if art=0 then write(t,'ADD ')
      else write(t,'DROP ');
      writeln(t,LeftStr(s,3));
      end
    else begin
      s:=trim(s);
      if length(s)<2 then exit;
      p:=cPos(' ',s);
      if p=0 then p:=pos(#9,s);
      if p>0 then
        if uucp then
          s:=LeftStr(s,p-1)
        else begin
          if p<5 then
            s:=copy(s,p+1,80);
          if cPos(' ',s)>0 then
            s:=copy(s,1,cPos(' ',s)-1);
          end;
      if s='' then exit;
      if not (quick or gs or uucp) and (s[1]<>'/') then
        write(t,'/');  { Euromail }
      writeln(t,s);
      end;
  end;

  procedure BretterAnlegen;
  var x,y : Integer;
      n   : longint;
      s   : string;
      i   : integer;
  begin
    msgbox(30,5,'',x,y);
    mwrt(x+3,y+2,getres2(807,10));   { 'Bretter anlegen ...' }
    n:=0;
    s:=List.FirstMarked;
    while s<>#0 do begin
      if quick then
        for i:=1 to length(s) do
          if s[i]='/' then s[i]:='\'
          else if s[i]='\' then s[i]:='/';
      if uucp then
        for i:=1 to length(s) do
          if s[i]='.' then s[i]:='/';
      if maf then makebrett(trim(LeftStr(s,40)),n,box,netztyp,true) else
      if promaf then makebrett(trim(mid(s,32)),n,box,netztyp,true) else
      if maus then begin
        s:=trim(mid(s,3));
        TrimFirstChar(s, '?'); { geheime Gruppen }
        makebrett(boxpar^.MagicBrett+s,n,box,netztyp,true);
        end else
      if fido then
        makebrett(boxpar^.MagicBrett+fidobrett(s),n,box,netztyp,true)
      else if qwk then
        makebrett(boxpar^.MagicBrett+trim(copy(s,4,50)),n,box,netztyp,true)
      else makebrett(s,n,box,netztyp,true);
      moff;
      gotoxy(x+22,y+2); Wrt2(Format('%5d', [n]));
      mon;
      s:=List.NextMarked;
      end;
    closebox;
    dbFlushClose(bbase);
    aufbau:=true;
  end;

  procedure BretterAnlegen2;
  var x,y : Integer;
      n   : longint;
      s   : string;
      i   : integer;
      t1: text;
  begin
    msgbox(30,5,'',x,y);
    mwrt(x+3,y+2,getres2(807,10));   { 'Bretter anlegen ...' }
    n:=0;
    assign(t1, fn);
    reset(t1);
    while not eof(t1) do
    begin
      readln(t1, s);
      for i:=1 to length(s) do
        if s[i]='.' then s[i]:='/';
      if s[1]='*' then s[1]:=' ';
      makebrett(trim(s),n,box,netztyp,true);
      moff;
      gotoxy(x+22,y+2); write(n:5);
      mon;
    end;
    Close(t1);
    closebox;
    dbFlushClose(bbase);
  end;

  procedure HandleNNTP;
  var
    RCList: TStringList;
    s, s1: String;
    RCFilename: String;
    Index: Integer;
    Unmarklist: TStringlist;
    Articles: Integer;
    x, y: Integer;
    HeaderOnly: Boolean;
    brk: Boolean;
  begin
    if Art = 0 then
    begin
      Articles := 10;
      HeaderOnly := false;
      Dialog(30, 5, getres2(810,83),x,y);                        { 'Newsgroups bestellen' }
      MAddbool(2, 2, Getres2(810, 84), HeaderOnly);              { 'Nur Header bestellen'}
      MAddInt(2, 4, Getres2(810, 85), Articles,4, 6, 0, 30000);  { 'Anzahl der Artikel'   }
      MAppsel(False, Getres2(810, 86));
      ReadMask(brk);
      EndDialog;
    end;

    Moment;
    RCList := TStringList.Create;
    RCList.LineBreak := #$0D#$0A;
    if Art = 0 then
      RCFilename := FileUppercase(fn + extRc)
    else
      RCFilename := FileUppercase(fn + extBl);
    try
      if FileExists(RCFilename) then
      begin
        RCList.Capacity := _FileSize(RCFilename) div 25;
        RCList.LoadFromFile(RCFilename);
      end;

      Debug.DebugLog('xp8','Using RC file:'+ RCFilename + 'with filesize' + IntToStr(_FileSize(RCFilename)), DLDebug);

      Unmarklist := TStringlist.Create;
      Unmarklist.LineBreak := #$0D#$0A;
      try
        s:=List.FirstMarked;
        while s<>#0 do
        begin
          if cPos(' ', s) > 0 then s1 := Copy(s, 1, cPos(' ', s)-1) else s1 := s;

          try
            case Art of
              0: begin // subscribe
                   Index := 0;
                   while Index < RCList.Count do
                   begin
                     if LeftStr(RCList[Index],Length(s1))=s1 then
                     begin
                       rfehler1(831,s1);     { 'Newsgroup %s wurde bereits bestellt' }
                       break;
                     end else
                       Inc(Index);
                   end;
                   if Index = RCList.Count then // no dupe found
                   begin
                     RCList.Add(s1 + ' -' + IntToStr(Articles) + iifs(HeaderOnly, ' HdrOnly', ''));
                     List.Lines[List.Lines.IndexOf(s)] := Trim(s) + ' *';
                   end;
                 end;
              1: begin // unsubscribe
                   // remember unsubscribed groups; we can not delete at
                   // this place, because of marked-handling in TLister
                   UnmarkList.Add(s);
                   Index := RCList.IndexOf(s1 + ' *');
                   if Index = -1 then Index := RCList.IndexOf(s1);
                   if Index <> -1 then RCList[Index] := s1;
                 end;
            end;
          except on e: Exception do
            Debug.DebugLogException(e);
          end;

          s := List.NextMarked;
        end;
        // delete unsubcribed groups finally
        while UnmarkList.Count > 0 do
        begin
          List.Lines.Delete(List.Lines.IndexOf(UnmarkList[0]));
          UnmarkList.Delete(0);
        end;
      finally
        UnmarkList.Free;
      end;

      RCList.Sort;
      RCList.SaveToFile(RCFilename);
      if art = 0 then
        List.Lines.SaveToFile(FileUppercase(fn + extBl))
      else
        List.Lines.SaveToFile(FileUppercase(fn + extRc));
    finally
      RCList.Free;
    end;
    CloseBox;
  end;

begin
  if mapsbox='' then begin
    box:=UniSel(1,false,DefaultBox);
    if box='' then exit;   { brk }
    end
  else
    box:=mapsbox;
  if not BoxHasMaps(box) then exit;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then 
  begin
    fn:= dbReadStr(d,'dateiname');
    netztyp:=dbReadInt(d,'netztyp');
    mapsname:= dbReadStr(d,'nameomaps');
    maf:=ntMAF(netztyp);
    promaf:=ntProMAF(netztyp);
    quick:=ntQuickMaps(netztyp);
    maus:=ntNude(netztyp);
    fido:=ntAreamgr(netztyp);
    gs:=(netztyp=nt_GS);
    uucp:=(netztyp=nt_UUCP);
    nntp := netztyp in [nt_NNTP, nt_Client];
    if uucp then begin
      ReadBoxpar(netztyp,box);
      changesys:=(boxpar^.BMtyp=bm_changesys);
      postmaster:=(boxpar^.BMtyp=bm_postmaster);
    end;
    qwk:=(netztyp=nt_QWK);
    end
  else begin
    fn:='';
    maf:=false; quick:=false; maus:=false; fido:=false; gs:=false;
    uucp:=false; promaf:=false; qwk:=false; postmaster:=false; 
    netztyp:=0;
    end;
  dbClose(d);
  if fn='' then
    rfehler(806)      { 'BOXEN.IX1 ist defekt - bitte lîschen!' }
  else begin
    if (art=1) and FileExists(fn +  extBbl) and changesys then
      lfile:=fn+ extBbl else
    if (art=1) and NNTP and FileExists(fn+ extRc) then
      lfile:=fn+extRc
    else lfile:=fn+extBl;
    lfile := FileUpperCase(lfile);
    if not FileExists(lfile) then
      rfehler(807)    { 'Keine Brettliste fÅr diese Box vorhanden!' }
    else begin
      if fido or maus or qwk then
        ReadBoxpar(netztyp,box);
      List := TLister.CreateWithOptions(1,iif(_maus and ListScroller,ScreenWidth-1,ScreenWidth),4,screenlines-fnkeylines-1,-1,'/NS/M/SB/S/'+
                 'APGD/'+iifs(_maus and listScroller,'VSC/',''));
      // preallocate ram for stringlist do speed up loading
      List.Lines.Capacity := _FileSize(lfile) div 25;
      List.Lines.LoadFromFile(lfile);
      case art of
        0 : showkeys(9);
        1 : showkeys(-9);
        2 : showkeys(12);
        3 : showkeys(9);
        4 : showkeys(12);
      end;
    again:
      List.OnTestMark := BrettMark;
      mapsnt:=netztyp; mapsart:=art;
      if NNTP then mapsnt:=nt_Client;
      if maus then LColType:=2 else
      if fido then lcoltype:=4 else
      if maf or quick then LColType:=0 else
      if promaf then lcoltype:=3
      else LColType:=1;
      if NNTP and (art=0) then lcoltype:=5 else
        LColType:=1;
      List.OnColor := MapsListcolor;
      brk := List.Show;
      if not brk then begin
        anz:=List.SelCount;
        if anz=0 then anz:=1;
        if (mapsnt=nt_ZConnect) and (anz=1) then begin
          if (art=0) and (firstchar(List.FirstMarked)='-') then begin
            rfehler(826);   { 'Dieses Brett kann nicht bestellt werden.' }
            goto again;
            end;
          if (art=1) and (firstchar(List.FirstMarked)='!') then begin
            rfehler(827);   { 'Dieses Brett kann nicht abbestellt werden.' }
            goto again;
            end;
          end;
        if NNTP and (anz=1) and (art=0) and
          (Firstchar(List.FirstMarked)='!') then
        begin
          rfehler(826);   { 'Dieses Brett kann nicht bestellt werden.' }
          goto again;
        end;
        bretter:=getres2(807,iif(anz=1,1,2));
        case art of
            0 : ask:=reps(reps(getreps2(807,3,strs(anz)),bretter),box);
            1 : ask:=reps(reps(getreps2(807,4,strs(anz)),bretter),box);
            2 : ask:=reps(getreps2(807,5,strs(anz)),bretter);
          3,4 : ask:=getres2(807,6);   { 'Inhalt der gewÑhlten Bretter anfordern' }
        end;
        if not ReadJN(ask,true) then
          goto again;
        if art in [0,1,3,4] then
        begin
          if NNTP then
            HandleNNTP
          else
          begin
            fn:=TempS(10000);
            assign(t,fn);
            rewrite(t);
            if quick or (uucp and postmaster) then
              wr_btext(t,art<>0,uucp);
            s:=List.FirstMarked;
            if fido and (art=4) and not Boxpar^.areabetreff then
              write(t,'%Rescan',#13#10);
            while s<>#0 do begin
              writeform;
              s:=List.NextMarked;
              end;
            if fido then write(t,'---',#13#10);
            close(t);
            if (not NNTP) and (art=0) and (uucp or (netztyp=nt_ZCONNECT)) then
              BretterAnlegen;
            List.Free;
            if art=3 then
              verbose:=ReadJN(getres2(810,20),false);  { 'ausfÅhrliche Liste' }
            if not NNTP then
              case art of
                0 : sendmaps('ADD',box,fn);
                1 : sendmaps('DEL',box,fn);
                3 : sendmaps('INHALT'+iifs(verbose,' VERBOSE',''),box,fn);
                4 : sendmaps(iifs(BoxPar^.AreaBetreff,'-r',''),box,fn);
              end;
            if NNTP and (art in [0,1]) then
              if MakeRC(art=0,box, List) then
                BretterAnlegen2;

            erase(t);
          end;
        end else
        begin
          BretterAnlegen;
          List.Free;
        end
      end
      else List.Free;
      freeres;
      aufbau:=true;
    end;
  end;
end;


procedure MapsCommands(defcom:byte);   { 0=Auswahl, 1=Brettliste holen }
var brk     : boolean;
    comm    : string;
    box     : string;
    domain  : string;
    t       : text;
    fn      : string;
    d       : DB;
    area    : boolean;
    request : boolean;
    nt      : byte;
    maf     : boolean;
    maus    : boolean;
    nntp    : boolean;
    ppp     : Boolean;
    info    : MausInfAP;
    infos   : integer;
    fido    : boolean;
    gs      : boolean;
    uucp,gup: boolean;
    autosys : boolean;
    feeder  : boolean;
    postmaster : boolean;
    promaf  : boolean;
    lines   : byte;
    i,j     : integer;
    List: TLister;
    x, y: Integer;

  procedure app(s1,s2:string);
  begin
    List.AddLine(' '+forms(s1,iif(maus,8,iif(fido or (uucp and not gup),15,20)))+s2);
  end;

  procedure rdsystem;
  var x,y : Integer;
      sys : string;
      brk : boolean;
  begin
    diabox(28,5,getres(808),x,y);   { 'Info <System>' }
    sys:='';
    readstring(x+3,y+2,getres(809),sys,8,8,'>',brk);   { 'Systemname' }
    closebox;
    if brk then comm:=''
    else comm:='INFO '+sys;
  end;

  procedure rdthema;
  begin
    case minisel(33,10+(screenlines-25)div 2,'Thema',
                 '^HILFE,^INFO,^LIST,^SCRIPT,^STATUS,^VERSION,^ADD,^DEL',1) of
      1 : comm:='HILFE HILFE';
      2 : comm:='HILFE INFO';
      3 : comm:='HILFE LIST';
      4 : comm:='HILFE SCRIPT';
      5 : comm:='HILFE STATUS';
      6 : comm:='HILFE VERSION';
      7 : comm:='HILFE ADD';
      8 : comm:='HILFE DEL';
    else
      comm:='';
    end;
  end;

  procedure gruppenuser;
  var
      gruppe : string;
      user   : string;
      x,y,p  : Integer;
      brk    : boolean;
      aufnehm: boolean;
  begin
    aufnehm:=cpos('>',comm)>0;
    gruppe:=''; user:='';
    case aktdispmode of
      -1,0 : if not dbEOF(bbase) then
               gruppe:= dbReadNStr(bbase,bb_brettname);
      1..4 : if not dbEOF(ubase) then begin
               user:= dbReadNStr(ubase,ub_username);
               end;
    10..12 : if not dbEOF(mbase) then begin
               if not aufnehm then
                 user:= dbReadNStr(mbase,mb_absender);
               dbSeek(bbase,biIntnr,copy(dbReadStrN(mbase,mb_brett),2,4));
               if dbFound then
                 gruppe:= dbReadNStr(bbase,bb_brettname);
               end;
    end;
    if user<>'' then begin
      p:=cpos('@',user);
      if p>0 then user:=trim(LeftStr(user,p-1))+' @ '+trim(mid(user,p+1));
      end;
    if gruppe<>'' then begin
      delete(gruppe,1,1);
      ReadBoxPar(0,box);
      with BoxPar^ do
        if LeftStr(UpperCase(gruppe),length(MagicBrett))<>UpperCase(MagicBrett) then
          gruppe:=''
        else
          delete(gruppe,1,length(magicbrett));
      end;
    dialog(44,5,'User '+iifs(aufnehm,'aufnehmen','ausschlie·en'),x,y);
    maddstring(3,2,'Gruppe ',gruppe,30,eBrettLen,''); mhnr(670);
    maddstring(3,4,'User   ',user,30,eAdrLen,'');
    readmask(brk);
    enddialog;
    if brk then
      comm:=''
    else
      comm:='GU'+user+LastChar(comm)+gruppe;
  end;

  function MausCRC(comm:string):boolean;
  var i : integer;
  begin
    i:=infos;
    while (i>0) and (comm<>info^[i].ID) do dec(i);
    MausCRC:=((i=0) and (firstchar(comm)='I')) or info^[i].crcflag;
  end;

begin
  box:=UniSel(1,false,DefaultBox);
  if box='' then exit;
  if not BoxHasMaps(box) then exit;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  mapsname:= dbReadStr(d,'nameomaps');
  dbRead(d,'netztyp',nt);
  domain:= dbReadStr(d,'domain');
  dbClose(d);
  maf:=ntMAF(nt);
  ntQuickMaps(nt);
  maus:=ntNude(nt);
  fido:=ntAreamgr(nt);
  gs:=(nt=nt_GS);
  uucp:=(nt=nt_UUCP);
  nntp:=(nt=nt_NNTP);
  ppp := (nt=nt_Client);
  if (nntp) or (uucp) then ReadBoxPar(nt,box);
  if uucp then begin
    gup:=(boxpar^.BMtyp=bm_gup);
    autosys:=(boxpar^.BMtyp=bm_autosys);
    feeder:=(boxpar^.BMtyp=bm_feeder);
    postmaster:=(boxpar^.BMtyp=bm_postmaster);
  end;
  promaf:=ntProMaf(nt);
  case defcom of
    0 : if (not ntMapsOthers(nt) or ((nt=nt_UUCP) and postmaster)) then begin
          rfehler(818);     { 'Bei dieser Box nicht mîglich.' }
          exit;
          end;
    1 : if ppp then
        begin
          msgbox(63,8,_hinweis_,x,y);
          for j := 2 to 5 do
            { 'Netztyp RFC/Client: Zum Anfordern einer neuen Newsgroup-'  }
            { 'Liste mu· die entsprechende Funktion beim externen Client' }
            { 'aktiviert sein (siehe auch "Newsgroup-Liste pflegen" bei'  }
            { '/Edit/Boxen/Edit/Mail-/News-Server) und die bisherige'     }
            { 'Newsgroup-Liste gelîscht werden (siehe /Nachricht/Brett-'  }
            { 'manager/Sonstiges/Lîschen).'                               }
            mwrt(x+3,y+j,getres2(10800,30+j));
          errsound;
          wait(curoff);
          closebox;
          freeres;
          exit;
        end else
          if not ntMapsBrettliste(nt) then
          begin
            rfehler(818);
            exit;
          end;
  end;
{$IFDEF Sockets }
  if nntp then begin
    if not GetNNTPList(box,boxpar) then
      rfehler(840); { 'Gruppenliste konnte nicht uebertragen werden' }
    exit;
  end;
{$ENDIF }
  area:=(mapsname='AREAFIX');
  request:=(mapsname='REQUEST');
  if maf then lines:=4
  else if maus then lines:=17
  else if fido then lines:=5
  else if area then lines:=7
  else if request then lines:=6
  else if gs then lines:=4
  else if uucp then
    if gup then lines:=3
    else if autosys then lines:=5
    else if feeder then lines:=5
    else lines:=4
  else lines:=18;
  if maus then begin
    new(info);
    MausReadITI(box,info,infos);
    end;
  if defcom=1 then begin
    if fido then comm:='List' else
    if area then comm:='LIST ALL BRETTER' else
    if request then comm:='LIST BRETTER' else
    if maf  then comm:='LIST ALLE' else
    if maus then comm:='ITG' else
    if gs   then comm:='BESTELLBARE BRETTER' else
    if uucp then if gup then comm:='newsgroups *'
                 else if autosys then comm:='newsgroups'
                 else if feeder then comm:='@active'
                 else comm:='getgroups' else
    if promaf then comm:='REQUEST'
    else comm:='LIST VERBOSE BRETTER *';
    brk:=false;
    end
  else
  begin
    List := listbox(iif(maus,45,57),lines,getres2(810,0)+mapsname+   { 'Nachricht an ' }
      iifs((mapsname='MAPS') and (random<0.07),'-o-MAT','') + ' @ ' + box);
    if fido then begin
      app('List',getres2(810,40));      { 'Liste der verfÅgbaren Bretter' }
      app('Query',getres2(810,41));     { 'Liste der bestellten Bretter' }
      app('Unlinked',getres2(810,42));  { 'Liste der nicht bestellten Bretter' }
      app('Help',getres2(810,43));      { 'Hilfe zu den Areafix-Befehlen' }
      app('Rescan',getres2(810,44));    { 'Brettinhalt anfordern' }
      end
    else if area then begin
      app('HILFE',getres2(810,50));     { 'Hilfe zu den AREAFIX-Befehlen' }
      app('LIST ALL BRETTER',getres2(810,51));   { 'komplette Brettliste' }
      end
    else if request then begin
      app('HILFE',getres2(810,55));     { 'Hilfe zu den REQUEST-Befehlen' }
      app('LIST BRETTER',getres2(810,56));  { 'Brettliste' }
      app('LIST OTHER BRETTER',getres2(810,57));   { 'nicht bestellte Bretter' }
      end
    else if maf then begin
      app('HILFE','Hilfe zu dem MAF-Befehlen');
      app('LIST ALLE','Brettliste');
      app('LIST BESTELLT','bestellte Bretter');
      app('LIST UNBESTELLT','unbestellte Bretter');
      end
    else if maus then begin
      for i:=1 to infos do
        app(info^[i].ID,info^[i].text);
      app('GU>','Gruppenmitglied aufnehmen');
      app('GU<','Gruppenmitglied ausschlie·en');
      end
    else if gs then begin
      app('BESTELLBARE BRETTER','Liste der bestellbaren Bretter');
      app('BESTELLTE BRETTER','Liste der bestellten Bretter');
      app('BRETT +','Bretter bestellen');
      app('BRETT -','Bretter abbestellen');
      end
    else if uucp then
      if gup then begin
        app('help',getres2(810,70));      { 'Hilfe zu Gup anfordern' }
        app('list',getres2(810,71));      { 'Liste der bestellten Bretter' }
        app('newsgroups *',getres2(810,72));   { 'Liste der verfÅgbaren Bretter' }
        end
      else if autosys then begin
        app('help',getres2(810,75));        { 'Hilfe zu AutoSys anfordern' }
        app('newsgroups',getres2(810,72));  { 'Liste der verfÅgbaren Bretter' }
        app('active',getres2(810,77));      { 'Traffic-öbersicht' }
        app('perms',getres2(810,76));       { 'Brett-Zugriffsrechte abfragen' }
        app('show',getres2(810,71));        { 'Liste der bestellten Bretter' }
        end
      else if feeder then begin
        app('@help',getres2(810,80));        { 'Hilfe zu Feeder anfordern' }
        app('@active',getres2(810,72));      { 'Liste der verfÅgbaren Bretter' }
        app('@get',getres2(810,71));         { 'Liste der bestellten Bretter' }
        app('@suspend',getres2(810,81));     { 'alle Bretter vorÅbergehend abbestellen' }
        app('@resume',getres2(810,82));      { 'alle Bretter reaktivieren' }
        end
      else begin
        app('getgroups',getres2(810,60));
        app('getsys',getres2(810,62));
        app('setsys',getres2(810,63));
        app('help',getres2(810,61));
        end
    else begin
      app('HILFE *',getres2(810,1));          { 'Hilfe zu allen MAPS-Befehlen' }
      app('HILFE <Thema>',getres2(810,2));    { 'Hilfe zu einem Befehl' }
      app('HILFE THEMEN',getres2(810,3));     { 'Themen-öbersicht' }
      app('HOLD ON',getres2(810,21));         { 'Bretter vorÅbergehend abbestellen (Urlaub)' }
      app('HOLD OFF',getres2(810,22));        { 'Bretter wieder aktivieren' }
      app('INHALT',getres2(810,4));           { 'Brettinhalt' }
      app('INFO',getres2(810,5));             { 'Infos zum eigenen System' }
      app('INFO *',getres2(810,6));           { 'Infos zu allen Systemen' }
      app('INFO <System>',getres2(810,7));    { 'Infos zu einem bestimmten System' }
      app('LIST ALL',getres2(810,8));         { 'User-, Brett- und Systemliste' }
      app('LIST BRETTER',getres2(810,9));     { 'Brettliste' }
      end;
    if not (maf or maus or fido or gs or uucp) then begin
      if not request then app('LIST USER',getres2(810,11));   { 'Userliste' }
      app('LIST MY BRETTER',getres2(810,12));   { 'bestellte Bretter' }
      if not request then app('LIST OTHER BRETTER',getres2(810,13));   { 'nicht bestellte Bretter' }
      if not (area or request) then begin
        app('PM LOESCHEN',getres2(810,14));   { 'Postfachinhalt in *Mailbox* lîschen' }
        app('STATUS',getres2(810,18));   { 'Eigenen Userstatus abfragen' }
        end;
      app('ADD',getres2(810,16));        { 'Bretter bestellen' }
      app('DEL',getres2(810,17));        { 'Bretter abbestellen' }
      end;
    freeres;
    brk := List.Show;
    closebox;
    if not brk then
      comm:=trim(LeftStr(List.FirstMarked,iif(maus,9,iif(fido or uucp,16,21))));
    end;
  if not brk then begin
    if comm='BRETT +' then comm:='ADD'
    else if comm='BRETT -' then comm:='DEL';
    brk:=false;
    if not (area or request or maf or fido)
       and ((LeftStr(comm,4)='LIST') and (comm<>'LIST SYSTEME')) and (defcom=0)
    then begin
      pushhp(69);
      if ReadJNesc(getres2(810,20),true,brk) then begin   { 'ausfÅhrliche Liste' }
        insert(' VERBOSE',comm,cPos(' ',comm));
        comm:=comm+' *';
        end;
      pophp;
      end;
    if not brk then begin
      if Fido or uucp then begin
        ReadBoxPar(0,box);
        if comm='Rescan' then begin
          mapsbox:=box;
          MapsBrettliste(4);
          mapsbox:='';
          exit;
          end;
        end;
      fn:=TempS(2000);
      if comm='INFO <System>' then rdsystem;
      if comm='HILFE <Thema>' then rdthema;
      if (comm='GU>') or (comm='GU<') then
        gruppenuser
      else
        if maus and MausCRC(comm) then
          comm:=comm+' -1';    { CRC -1 -> Maus-Infofile anfordern }
      if comm='INHALT' then begin
        mapsbox:=box;
        mapsbrettliste(3);
        mapsbox:='';
        end
      else if (comm<>'ADD') and (comm<>'DEL') and (comm<>'SCRIPT') then begin
        assign(t,fn);
        rewrite(t);
        if fido then begin
          if not boxpar^.AreaBetreff then write(t,'%',comm,#13#10);
          write(t,'---',#13#10);
          end
        else if uucp then begin
          write(t,'system: ',boxpar^.pointname,
                  iifs(boxpar^.BMdomain,domain,''),#13#10);
          write(t,'passwd: ',boxpar^.areapw,#13#10);
          write(t,comm,#13#10);
          end
        else if promaf then begin
          write(t,#13#10);
          write(t,#13#10);
          write(t,'BRETTER',#13#10);
          end
        else
          write(t,#13#10);
        close(t);
        end
      else begin
        EditFile(fn,false,false,false,0,false);
        if _filesize(fn)<=2 then begin
          rfehler(808);   { 'leere Nachricht - nicht abgeschickt' }
          comm:='';
          end;
        end;
      if FileExists(fn) and (comm<>'') then
        if fido then begin
          if boxpar^.AreaBetreff then
            if comm='Query' then comm:='-q'
            else if comm='List' then comm:='-l'
            else if comm='Unlinked' then comm:='-u'
            else comm:='-h'
          else
            comm:='';
          SendMaps(comm,box,fn);
          end
        else
          SendMaps(comm,box,fn);
      SafeDeleteFile(fn);
      end;
    end;
  if maus then
    dispose(info);
  if defcom=0 then
    List.Free;
end;


{ Usenet-Sysfile aus akt. Nachricht auslesen   }
{ BoxPar der betreffenden Box mu· geladen sein }

{ This is your latest sys file entry

  xpoint/xpoint.sh.sub.org:shlink.dfue,shlink.general,shlink.ibm,shlink.general,\
          shlink.test/!local,all:f:   }

procedure GetSysfile;
var fn   : string;
    t,t2 : text;
    s    : string;
    p    : byte;

  procedure WriteStr;
  var p : byte;
  begin
    repeat
      p:=cpos(',',s);
      if p>0 then begin
        writeln(t2,LeftStr(s,p-1));
        delete(s,1,p);
        end;
    until p=0;
  end;

begin
  rmessage(820);      { 'Lese Sysfile-Eintrag ...' }
  fn:=TempS(dbReadInt(mbase,'groesse'));
  XRead(fn,false);
  assign(t,fn);
  reset(t);
  s:='';
  with BoxPar^ do begin
    while not eof(t) and (LeftStr(s,length(pointname))<>pointname) do
      readln(t,s);
    s:=trim(s); p:=cpos(':',s);
    if not eof(t) and (p>0) then begin
      assign(t2, GetServerFilename(boxpar^.boxname, extBbl));
      rewrite(t2);
      delete(s,1,p);
      while LastChar(s)='\' do
      begin
        DeleteLastChar(s);
        WriteStr;
        readln(t,s);
        s:=trim(s);
        end;
      p:=cpos('/',s); if p=0 then p:=cpos(':',s);
      if p>0 then begin
        s:=LeftStr(s,p-1)+',';
        WriteStr;
        end;
      close(t2);
      end;
    end;
  close(t);
  _era(fn);
  closebox;
end;


{----- Fileserver --------------------------------------------------}

function IsServer(box:string; var fstype:byte):boolean;
var d     : DB;
    flags : smallword;
begin
  dbOpen(d,SystemFile,1);
  dbSeek(d,siName,UpperCase(box));
  if dbFound then begin
    dbRead(d,'flags',flags);
    dbRead(d,'fs-typ',fstype);
    end;
  dbClose(d);
  IsServer:=dbFound and (flags and 1<>0);
end;

{ Erstellt aus einem Boxnamen (bis 20 Zeichen) einen Filenamen, der nur
  8+3 Zeichen lang ist }

function MangleBoxName(const s: String): String;
begin
  if Length(s) <= 8 then
    MangleBoxName := s
  else
    MangleBoxName := LeftStr(s, 4) + Hex(CRC32Str(UpperCase(s)), 4);
  Result := FileUpperCase(Result);
end;


{ msg => aktuelle Nachricht wird eingelesen }

procedure FS_ReadList(msg:boolean);
const
      tbufs = 2048;
var
    absender : string;
    box      : string;   { das ist auf jeden Fall eine Zerberus-Box.. }
    convert  : string;
    x,y,p,p2 : Integer;
    f        : file;
    fn       : string;
    t1,t2    : text;
    s,s2     : string;
    useclip  : boolean;
    fstype   : byte;
    tbuf     : pointer;

  procedure wrl;
  begin
    writeln(t2,s[1],'   ',trim(Mid(s,2)));
  end;

  procedure WriteFST(typ:byte);
  var d : DB;
  begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,UpperCase(box));
    if dbFound then
      dbWrite(d,'fs-typ',typ);
    dbClose(d);
  end;

  procedure GetConvert;
  var d : DB;
  begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,UpperCase(box));
    convert:= dbReadStr(d,'ZBV1');
    dbClose(d);
  end;

begin
  if msg then begin
    if (LeftStr(UpperCase(dbReadStrN(mbase,mb_betreff)),5)<>'FILES') and
       not ReadJN(getres(811),true) then   { 'Sind Sie sicher, da· das eine Fileliste ist' }
      exit;
    absender:= dbReadStrN(mbase,mb_absender);
    p:=cpos('@',absender);
    p2:=p+cPos('.',copy(absender,p+1,20));
    if (p=0) then begin { or (p2 = 0) rausgenomme!!  MK 12/99 }
      trfehler(809,60);   { 'fehlerhafter Absender!?' }
      exit;
      end;
    box:=copy(absender,p+1,p2-p-1);
    if not IsServer(box,fstype) then begin
      trfehler1(810,box,60);   { 'Das System %s ist nicht als Fileserver eingetragen.' }
      exit;
      end;
    end
  else begin
    fn:=FilePath+WildCard;
    useclip:=false;
    if not readfilename(getres(812),fn,true,useclip) then exit;   { 'Fileserver-Liste' }
    if not FileExists(fn) then begin
      rfehler(811);   { 'Datei ist nicht vorhanden.' }
      exit;
      end;
    box:=UniSel(3,false,'');
    if box='' then exit;
    if not IsServer(box,fstype) then begin
      trfehler1(812,UpperCase(box),60);   { '%s ist kein Fileserver.' }
      exit;
      end;
    end;

  msgbox(48,3,'',x,y);
  mwrt(x+3,y+1,getreps(813,UpperCase(box)));   { 'File-Liste fÅr %s wird eingelesen ...' }
  if msg then begin
    fn:=TempS(dbReadInt(mbase,'groesse')+5000);
    assign(f,fn);
    rewrite(f,1);
    XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
    close(f);
    end;
  getmem(tbuf,tbufs);
  assign(t1,fn);
  settextbuf(t1,tbuf^,tbufs);
  reset(t1);
  if fstype<3 then begin
    s:=''; s2:='';
    while not eof(t1) and
          ((pos('Typ',s)=0) or (pos('Dateiname',s)=0)) and
          ((pos('Name',s)=0) or (pos('Beschreibung',s)=0)) and
          (pos('Õ file description Õ',LowerCase(s))=0) do begin
      s2:=s;
      readln(t1,s);
      end;
    if eof(t1) then begin
      closebox;
      close(t1);
      freemem(tbuf,tbufs);
      if msg then _era(fn);
      trfehler(813,60);   { 'unbekanntes Listenformat :-(' }
      exit;
      end;
    fstype:=iif(pos('Beschreibung',s)>0,1,iif(pos('description',LowerCase(s))>0,2,0));
    WriteFST(fstype);
    end;
  GetServerName(box); { Gro·- und Kleinschreibung korrigieren }
  makebak(MangleBoxName(box)+ extFl, ExtBak);
  case fstype of
    0 : begin      { SendZMsg }
          assign(t2,MangleBoxName(box)+ extFl);
          rewrite(t2);
          readln(t1,s);
          repeat
            if copy(s,1,1)='%' then begin           { Kommentarzeile }
              writeln(t2);
              wrl;
              writeln(t2);
              readln(t1,s);
              end
            else
              if (Length(s) >= 2) and (s[1]<>' ') and (s[2]=' ') then begin
                repeat
                  if eof(t1) then s2:=''
                  else readln(t1,s2);
                  if (s2<>'') and (LeftStr(s2,5)='     ') then
                    s:=s+' '+trim(s2);
                until (s2='') or (LeftStr(s2,5)<>'     ');
                wrl;
                s:=s2;
                if (s='') then readln(t1,s);
                end
              else
                readln(t1,s);
          until eof(t1);
          close(t2);
        end;

    1 : begin      { iMLS-Fileserver }
          assign(t2, MangleBoxName(box)+extFl);
          rewrite(t2);
          writeln(t2,s2);
          writeln(t2,s);
          while not eof(t1) do begin
            readln(t1,s); writeln(t2,s);
            end;
          close(t2);
        end;

    2 : begin      { NCB-Mail-Fileserver }
          close(t1); reset(t1);
          assign(t2, MangleBoxName(box)+ extFl);
          rewrite(t2);
          while not eof(t1) do begin
            readln(t1,s); writeln(t2,s);
            end;
          close(t2);
        end;

    3 : begin      { UUCP-Fileserver }
          GetConvert;
          if pos('$INFILE',convert)=0 then
            rfehler(824)    { 'UngÅltiger Konvertierer-Eintrag: $INFILE fehlt' }
          else if pos('$OUTFILE',convert)=0 then
            rfehler(825)    { 'UngÅltiger Konvertierer-Eintrag: $OUTFILE fehlt' }
          else begin
            exchange(convert,'$INFILE',fn);
            exchange(convert,'$OUTFILE', MangleBoxName(box)+ extFl);
            shell(convert,300,3);
            if errorlevel=1 then rfehler(821);
            end;
        end;

  end;
  close(t1);
  if msg then erase(t1);
  freemem(tbuf,tbufs);
  closebox;
end;


var fstyp : byte;   { 0=SendZMsg, 1=iMLS }

function testmark(const s:string; block:boolean):boolean;
begin
  if (s<>'') and
     (((fstyp=0) and (FirstChar(s)<>'%') and (copy(s,2,1)=' ')) or
      ((fstyp=1) and (LeftStr(s,5)<>'Name-') and (FirstChar(s)<>' ')) or
      ((fstyp=2) and (s<>'') and (s[1]>' ') and (s[1]<'∞')) or
      ((fstyp=3) and (trim(s)<>''))) then
    testmark:=true
  else begin
    if not block then errsound;
    testmark:=false;
    end;
end;


function UUsendTestSourcefile(var s:string):boolean;
var
  name : string;

   procedure SetDestfile;
   begin
     if getfield(fieldpos+1)='' then
       setfield(fieldpos+1,LowerCase(extractfilename(s)));
   end;

begin
  s:=ExpandFileName(s);
  if FileExists(s) and not isPath(s) then begin
    SetDestfile;
    UUSendTestSourcefile:=true;
  end else begin                        { Datei fehlt }
    if not multipos('*?',s) then
    begin
      if (LastChar(s)<>DirSepa) {and (rc<>0)} then begin
        rfehler(823);               { 'Datei nicht gefunden.' }
        UUsendTestSourcefile:=false;
        exit;
      end;
      s:= AddDirSepa(s)+WildCard;
    end;
    selcol;
    name:=fsbox(screenlines div 2 - 5,s,'','',true,false,false);
    if name='' then
      UUsendTestSourcefile:=false
    else begin
      s:=name;
      SetDestfile;
      UUsendTestSourcefile:=true;
      end;
    end;
end;


{ comm:    '' / 'FILES' / 'HILFE'     }
{ request: 0=nein, 1=SEND, 2=TRANSFER }

procedure FS_command(comm:string; request:byte);
var d     : DB;
    fs    : string;
    fname : string;
    fpass : string;
    hd    : string;
    w     : smallword;
    fn    : string;
    t     : text;
    brk   : boolean;
    s     : string;
    p     : byte;
    enterfiles : boolean;
    List: TLister;

  procedure GetFilelist;
  var dateien : string;
      anz     : longint;
      s       : string;
  label again;
  begin
    showkeys(10);
    List := TLister.CreateWithOptions(1,ScreenWidth,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/');
    List.ReadFromFile(MangleBoxName(fs)+ extFl,0, false);
    List.OnTestMark := testmark;
  again:
    brk := List.Show;
    if not brk then
    begin
      anz:= List.SelCount;
      s:= List.FirstMarked;
      if (anz=0) and not (testmark(s,false)) then
        goto again;
      if anz=0 then anz:=1;
      dateien:=getres2(814,iif(anz<>1,2,1));
      if not ReadJN(reps(reps(getreps2(814,3,strs(anz)),dateien),fs),true)   { '%s %s bei %s bestellen' }
        then goto again;
      freeres;
      end;
    aufbau:=true;
  end;

  procedure GetTransCeiver;
  var adr : string;
  begin
    select(3);
    if selpos=0 then brk:=true
    else begin
      dbGo(ubase,selpos);
      adr:= dbReadNStr(ubase,ub_username);
      if FirstChar(adr)=vert_char then 
      begin
        rfehler(814);    { 'Verteiler sind hier nicht erlaubt.' }
        brk:=true;
        end
      else begin
        rewrite(t);
        writeln(t,'%',adr);
        close(t);
        end;
      end;
  end;

  procedure readservice;
  var s   : string;
      x,y : Integer;
  begin
    diabox(49,5,getres(815),x,y);   { 'Service-Befehl' }
    s:='';
    readstring(x+3,y+2,getres(816),s,32,32,'',brk);    { 'Befehl: ' }
    if not brk then comm:=comm+' '+s;
    closebox;
  end;

  procedure fscomm(comm:string);
  var domain : string;
  begin
    if isbox(fs) then domain:=ntServerDomain(fs)
    else domain:='.ZER';
    if DoSend(true,fn,false,false,fname+'@'+fs+domain,comm,
              false,false,false,false,false,nil,hd,0) then;
  end;

  procedure uucomm(comm:string);
  begin
    forcebox:=fs;
    GetServerName(fs);      { korrekte Schreibweise ermitteln }
    if DoSend(true,fn,false,false,fname+'@'+fs+ntServerDomain(fs),comm,
              false,false,false,false,false,nil,hd,0) then;
  end;

  procedure UUsendfile;
  var x,y    : Integer;
      brk    : boolean;
      source,
      dest   : string;
  begin
    dialog(ival(getres2(818,0)),5,getres2(818,1),x,y);
    source:=WildCard; dest:='';
    maddstring(3,2,getres2(818,2),source,41,70,'>'); mhnr(890);
    msetvfunc(UUsendTestSourcefile);
    maddstring(3,4,getres2(818,3),dest,41,79,'');
    readmask(brk);
    enddialog;
    if FileExists(source) and (dest<>'') then begin
      rewrite(t);
      writeln(t,dest);
      close(t);
      xpsendmessage.EditAttach:=false; xpsendmessage.noCrash:=true;
      GetServerName(fs);      { korrekte Schreibweise ermitteln }
      if DoSend(true,fn,false,false,fname+'@'+fs+ntServerDomain(fs),
                expandfilename(source),
                false,true,false,false,false,nil,hd,0) then;
      end;
  end;

  procedure ReadFiles;
  var x,y : Integer;
  begin
    dialog(ival(getres2(818,10)),3,getres2(818,11),x,y);   { 'UUCP-Filerequest' / 'Dateien ' }
    s:='';
    maddstring(3,2,getres2(818,12),s,43,250,''); mhnr(895);
    readmask(brk);
    if s='' then brk:=true;
    enddialog;
  end;

  function UU_directory:string;
  var s : string;
      p : byte;
  begin
    s:= List.PrevLine;
    while (s<>#0) and (LeftStr(LowerCase(s),10)<>'directory ') do
      s:=List.PrevLine;
    if s=#0 then s:=''
    else begin
      s:=trim(mid(s,11));
      TrimFirstChar(s, '"');
      p:=blankpos(s);
      if p>0 then truncstr(s,p-1);
      if LastChar(s)='"' then DeleteLastChar(s);
      s:=trim(s);
      if s<>'' then begin
        if (LastChar(s)=':') and (cpos('/',s)>0) then
          DeleteLastChar(s);
        if not (LastChar(s) in [':','/']) then
          s:=s+'/';
        end;
      end;
    UU_directory:=s;
  end;

  procedure AskStart;
  begin
    if ReadJN(getres(819),true) then
      AutoCrash:='*'+fs;
  end;

begin
  fs:=UniSel(3,false,'');
  if fs<>'' then begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,UpperCase(fs));
    fname:=dbReadStr(d,'fs-name');
    fpass:=dbReadStr(d,'fs-passwd');
    dbRead(d,'flags',w);
    dbRead(d,'fs-typ',fstyp);
    dbClose(d);
    enterfiles:=not FileExists(MangleBoxName(fs)+extFl);
    if w and 1=0 then
      rfehler(815)      { 'Das gewÑhlte System ist kein Fileserver!' }
    else if (request>0) and (fstyp<>3) and enterfiles then
      rfehler1(816,fs)  { 'keine Fileliste fÅr %s vorhanden' }
    else if (comm='SERVICE') and (fpass='') then
      rfehler(817)      { 'Pa·wort erforderlich - bitte unter /Edit/Systeme eintragen!' }
    else if (comm='SENDEN') and (fstyp<>3) then
      rfehler(822)      { 'Senden ist nur bei UUCP-Fileservern mîglich!' }
    else begin
      fn:=TempS(1000);
      assign(t,fn);
      hd:='';
      if fstyp=3 then begin      { UUCP-Fileserver }
        if not isBox(fs) then
          rfehler(820)
        else if comm='FILES' then begin
          GetServerName(fs);
          rewrite(t);
          writeln(t,fpass);
          close(t);
          uucomm('Request');
          end
        else if (comm='') and (request=1) then begin
          if enterfiles then
            ReadFiles
          else
            GetFileList;
          if not brk then begin
            rewrite(t);
            if enterfiles then begin
              s:=s+' ';
              repeat
                p:=blankpos(s);
                writeln(t,LeftStr(s,p-1));
                s:=trimleft(mid(s,p));
              until s='';
              end
            else begin
              FlushClose;
              s:=trim(List.FirstMarked);
              while s<>#0 do begin
                p:=blankpos(s);
                if p>0 then truncstr(s,p-1);
                if multipos(_MPMask,s) then writeln(t,s)
                else writeln(t,UU_directory+s);
                s:=List.NextMarked;
                end;
              List.Free;
              end;
            close(t);
            uucomm('Request');
            AskStart;   { sofort anrufen? }
            end
          else
            if not enterfiles then List.Free;
          end
        else if comm='SENDEN' then
          UUSendfile
        else                     { HILFE, TRANSFER, SERVICE }
          rfehler(819);          { 'Bei UUCP-Fileservern nicht mîglich.' }
        end
      else begin                 { SendZMsg/iMLS/NCB-Mail-Fileserver }
        rewrite(t);
        if comm='SERVICE' then writeln(t,'%',fpass)
        else writeln(t);
        close(t);
        if comm='SERVICE' then readservice
        else brk:=false;
        if not brk then
          if request=0 then
            fscomm(comm)
          else begin
            GetFileList;
            if not brk then begin
              if request=2 then    { Transfer }
                GetTransCeiver;
              if not brk then begin
                s:=List.FirstMarked;
                while s<>#0 do begin
                  if fstyp=0 then
                    s:=trim(Mid(s,2));
                  s:=LeftStr(s,cPos(' ',s)-1);
                  fscomm(iifs(request=1,'SEND ','TRANSFER ')+s);
                  s:=List.NextMarked;
                  end;
                AskStart;   { sofort anrufen? }
              end;
            end;
            List.Free;
          end;
        end;
      if existf(t) then
        erase(t);
      end;
    end;
end;


{ Fido FileScan }

procedure SendFilescan(var fn:string);
var leer : string;
begin
  leer:='';
  forcebox:=boxpar^.boxname;
  if DoSend(true,fn,false,false,BoxPar^.filescanner+'@'+boxpar^.boxname,
            boxpar^.FilescanPW,false,false,false,false,false,
            nil,leer,0) then;
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


function fileechomarkfunc(const s:string; block:boolean):boolean;
begin
  if trim(s)='' then begin
    if not block then errsound;
    fileechomarkfunc:=false;
    end
  else
    fileechomarkfunc:=true;
end;

function fileechocolfunc(const s:string; line:longint):byte;
begin
  if (s<>'') and (s[1]='*') then
    fileechocolfunc:=col.ColMapsBest
  else
    fileechocolfunc:=0;
end;

function echoname(s:string):string;
begin
  s:=trim(s);
  while FirstChar(s)<'0' do DeleteFirstChar(s);
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
  List := TLister.CreateWithOptions(1,iif(_maus and ListScroller,Screenwidth-1,screenwidth),4,screenlines-fnkeylines-1,-1,'/NS/M/SB/S/'+
             'APGD/'+iifs(_maus and ListScroller,'VSC/',''));
  List.ReadFromFile(fl,0, false);
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
      write(t,iifc(art=1,'+','-'),echoname(s),#13#10);
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
      if FirstChar(echo) ='A' then 
        DeleteFirstChar(echo)
      else 
        echo:='';
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
          DeleteFirstChar(echo);
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
    write(t,'-',echo,#13#10);
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
          DeleteFirstChar(echo);
          if (UpperCase(dbReadStrN(bbase,bb_pollbox))=box) and
             (LeftStr(UpperCase(echo),length(boxpar^.magicbrett))=UpperCase(boxpar^.magicbrett))
             and (pos('/files/',LowerCase(echo))>0)
          then begin
            if n=0 then rewrite(t);
            delete(echo,1,length(boxpar^.magicbrett));
            if LowerCase(LeftStr(echo,6))='files/' then delete(echo,1,6);
            write(t,'-',echo,#13#10);
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
    SafeDeleteFile(fn);
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
    write(t,'%',comm,#13#10);
    close(t);
    SendFilescan(fn);
    _era(fn);
    end;
  freeres;
end;
end.


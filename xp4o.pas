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

{ CrossPoint - Overlayroutinen, die von XP4 aufgerufen werden }

{$I xpdefine.inc }

{.$DEFINE sDebug}

unit xp4o;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,maske,datadef,database,
  lister,archive,maus2,winxp,printerx,resource,xpglobal,osdepend,
  xp0,xp1,xp1o2,xp1help,xp1input;


var  such_brett  : string;    { fuer Suche im gewaehlten Brett }
     FMsgReqnode : string;    { F3 - Request - Nodenr. }

const max_arc = 3;    { maximale verschachtelte Archivdateien }
      suchlen = 160;  { Maximallaenge der Suchbegriffe }
      histmax = 14;   { Anzahl Eintraege in Suchbegriff-History}
      opthmax = 4;    { Anzahl Eintraege in Optionen-History }
      suchmax = 20;   { Anzahl AND/OR Teilstrings im Suchbegriff }

var seeklen   : array[0..suchmax-1] of byte;
    seekstart : array[0..suchmax-1] of byte;
    seeknot   : array[0..suchmax-1] of boolean;
    suchand   : boolean;
    suchanz   : Integer;
    sst       : string; // evtl. UpString von suchstring
    igcase    : boolean;
    umlaut    : boolean;

Const
    historyFile = 'SEEK.TXT';
    libraryFile = 'SEEKLIB.TXT';
    optionsFile = 'OPTIONS.TXT';
    Suchergebnis : boolean = false;

procedure msg_info;          { interpretierten Header anzeigen }
procedure ShowHeader;        { Original-Header anzeigen        }

function  Suche(anztxt,suchfeld,autosuche:string):boolean;
procedure betreffsuche;
procedure SucheWiedervorlage;
procedure BU_reorg(user,adrbuch:boolean);
procedure MsgReorgScan(_del,repair:boolean; var brk:boolean);
procedure MsgReorg;
procedure ImportBrettliste;
procedure ImportUserliste;
procedure ExportUB(user:boolean);

procedure ModiEmpfDatum;
procedure ModiBetreff;
procedure ModiText;
procedure ModiRot13;
procedure ModiTyp;
procedure ModiGelesen;
procedure ModiHighlite;

procedure zeige_unversandt;
function  ViewArchive(var fn:string; typ:shortint):shortint;
procedure FileArcViewer(fn:string);

procedure ShowArch(const fn:string);
function  a_getfilename(nr,nn:byte):string;
procedure ArcSpecial(LSelf: TLister; var t:taste);

procedure DupeKill(autodupekill:boolean);
procedure print_msg(initpr:boolean);
function  UserMarkSuche(allmode:boolean):boolean;
procedure BrettInfo;
procedure ntinfo;
procedure do_bseek(fwd:boolean);
procedure FidoMsgRequest(var nnode:string);
function  _killit(ask:boolean):boolean;
function  testbrettscope(var s:string):boolean;
procedure seek_cutspace(var s:string);

procedure seekmenu(var s:string);
procedure OldSEEK_ed(LSelf: TLister; var t:taste); {Lister-Tastenabfrage fuer Seek-Menue}

function Bool_BrettGruppe(var s:string):boolean;
function Bool_Brettindex(var s:string):boolean;
Procedure Brettmarksuche;

implementation  {-----------------------------------------------------}

uses xpkeys,xpnt,xp1o,xp4,xp3,xp3o,xp3o2,xp3ex,xpfido,xpmaus,xpheader,
     xpmakeheader,xp_pgp,debug,viewer,xpconfigedit,classes,xp9bp,mime, 
     regexpr, xprope, xpspam;

type arcbuf = record
                arcer_typ : shortint;
                arcname   : string;
              end;

const arcbufp : byte = 0;
      suchopt : string = '*';  { Flag fÅr erste Suche seit Programmstart }

    history : array[0..histmax] of String[Suchlen]=
     ('','','','','','','','','','','','','','','');
    history_changed   : boolean = false;

var  reobuf : array[0..ablagen-1] of boolean;
     { HJT: 22.03.2006, sonst Arithmetic overflow bei zu grossen MPUFFERn }
     { bufsiz : array[0..ablagen-1] of longint; } { Groesse nach Reorg }
     bufsiz : array[0..ablagen-1] of Int64;
     abuf   : array[1..max_arc+1] of arcbuf;
     exdir  : string;
     arctyp_save : shortint;
     mid_bretter       : byte;
     Mid_teilstring    : boolean;

function testbrettscope(var s:string):boolean;
var i : integer;
begin
  if (length(s)=1) and (lastkey<>keybs) then begin
    for i:=4 downto 0 do
      if upcase(FirstChar(s))=UpperCase(FirstChar(getres2(442,i))) then
        s:=getres2(442,i);
    freeres;
    if length(s)>1 then _keyboard(keyend);
    end;
  testbrettscope:=true;
end;

procedure seek_cutspace(var s:string);
begin
  if s=' ' then s:='';
end;

function mid_suchoption(var s:string):boolean;
begin
  if aktdispmode <> 11 then setfieldenable(mid_bretter,s='J');
  mid_suchoption:=true;
end;

procedure OldSEEK_ed(LSelf: TLister; var t:taste); {Lister-Tastenabfrage fuer Seek-Menue}
begin
  if (UpperCase(t)='E') then begin
    EditFile(libraryFile,false,false,false,0,false);
    t:=keyesc;
    pushkey(keysf2);
    end;
end;

procedure seekmenu(var s:string);

const height = 10;
      width  = 70;
  var t    : text;
      brk  : boolean;
  x,y  : Integer;
  List: TLister;
begin
      assign(t,libraryFile);
      reset(t);
      s:='';
      if ioresult<>0 then exit;
      selbox(width+2,height+2,getres2(441,21),x,y,true);
      List := TLister.CreateWithOptions(x+1,x+width,y+1,y+height,0,'/NS/SB/DM/S/M/');
      ListboxCol(List);
      List.SetArrows(x+width+1,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
      while not eof(t) do
      begin
        readln(t,s);
        List.AddLine(LeftStr(s,suchlen));
        end;
      List.OnKeyPressed := oldseek_ed;
      brk := List.Show;
      if List.SelCount =0 then
      begin
        s := List.GetSelection;
        if FirstChar(s)=' ' then brk:=true;
        end
      else if not brk then begin
        s:= List.FirstMarked;
        x:=0;
        repeat
          if (s<>#0) and (s<>'') and (s[1]<>' ')
          then inc(x);
          s:= List.NextMarked;
        until (s=#0) or (x=histmax);
        for y:=histmax downto x do history[y]:=history[y-x];
        s:= List.FirstMarked;
        y:=0;
        repeat
          if (s<>#0) and (s<>'') and (s[1]<>' ')
          then begin
            history[y]:=s;
            inc(y);
            end;
          s:= List.NextMarked;
        until (y>x) or (s=#0);
        s:=history[0];
        history_changed:=true;
        pushkey(keyctcr);
        end;
      List.Free;
      closebox;
      close(t);
      if brk then s:='';
end;


{ Suchfeld:  '' (Volltext), '*' (Umiversal), 'Betreff', 'Absender', 'MsgID' }

function Suche(anztxt,suchfeld,autosuche:string):boolean;
type  suchrec    = record
                     betr,user,txt : string;
                     fidoempf,mid  : string;
                     nbetr,nuser   : Boolean;
                     nfidoempf     : Boolean;
                     or_betr       : Boolean;
                     or_user       : Boolean;
                     or_fidoempf   : Boolean;
                     vondat,bisdat : string;
                     vonkb,biskb   : longint;
                     status        : string;
                     typ           : string;
                   end;

const srec       : ^suchrec = nil;
      opthist : array[0..opthmax] of String[8]=('','','','','');
      history0   : string='';
      history1   : string='';
      history2   : string='';

var x,y   : Integer;
    brk   : boolean;
    n,nf  : longint;
    p     : pointer;
    psize : Integer;
    spez  : boolean;
    i     : integer;
    brett : string;
    me,uu : boolean;
    hdp   : Theader;
    hds   : longint;
    bretter : string;
    t               : text;

    suchstring      : string;
    typc          : char;
    statb           : byte;
    _vondat,_bisdat : longint;
    minsize,maxsize : longint;
    regex           : boolean;          // regexpressions zulassen?
    bereich         : shortint;
    _brett          : string;
    mi,add          : byte;
    bera            : array[0..4] of string;
    stata           : array[0..5] of string;
    typa            : array[0..4] of string;
    RegExpr: TRegExpr;

    seek            : string;
    found           : boolean;
    markedback      : marklistp;
    markanzback     : integer;
    check4date      : boolean;
    headersuche     : byte;
    andmask,ormask  : byte;
    holdmarked      : boolean;

label ende, restart;


{ Check_Seekmode:

  Wenn bei Normalsuche die Suchoptionen "ua&" fuer UND oder "o|" fuer ODER angegeben wurden,
  werden in den Arrays die Anfangs und End Offsets von bis zu 10 Teilsuchstrings gespeichert,
  und Suchanz auf die Anzahl der (durch Leerzeichen getrennten, bzw in Anfuehrungszeichen
  eingefassten) Teilsuchstrings gesetzt. Suchand ist "true" bei UND Verknuepfung und "false"
  bei ODER Verknuepfung.  Wurden keine Verknuepfungsoptionen angegeben, wird eine OR
  Verknuepfung durchgefuehrt, mit einem einzigen "Teilsuchstring", der die Ganze Laenge
  des Suchstring abdeckt
}
  procedure check_seekmode;
  var
  m,n,i   : Integer;
  quotes  : boolean;

{$IFDEF sDebug}                      { Zum Debuggen der Suchstringerkennung}

  Procedure Show_Seekstrings;
  var n,x,y: byte;
  const width=75; height=20;
  begin
    selbox(width+2,height+2,'Suchstring-Check',x,y,true);
    openlist(x+1,x+width,y+1,y+height,0,'/NS/CR/');
    ListboxCol;
    listarrows(width+3,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
    app_L('');
    app_L(' Benutzte Teilstrings: '+StrS(suchanz)+iifs(suchand,'  AND','  OR')+
      '   Igcase='+iifs(igcase,'1','0')+'   Umlaut='+iifs(umlaut,'1','0')+
      iifs(spez,'    SPEZIAL',''));
    app_L('');
    app_l(' Suchstring: '+chr($af)+iifs(spez,srec^.txt,suchstring)+chr($ae));
    app_l(' sst:        '+chr($af)+sst+chr($ae));
    app_l('');
    for n:=0 to iif(suchanz<10,10,suchanz-1) do
    begin
      app_l(' String'+rforms(strs(n),2)+': '+rforms(strs(seekstart[n]),3)+','+
        rforms(strs(seeklen[n]),3)+
        iifs(seeknot[n],' NOT ','     ')+chr($af)+
        left(mid(sst,seekstart[n]),seeklen[n])+chr($ae));
    end;
    app_l(''); 
    app_l(' Length(sst)='+strs(length(sst))+'  i='+(strs(i)));
    if spez then with srec^do
    begin
      app_l('');
      app_l(dup(30,'-'));
      app_l(' AND-Maske: '+bin(andmask,3)+'   OR-Maske: '+bin(ormask,3));
      app_l('');
      app_l(' User:     '+iifs(or_user,' OR','   ')
           +iifs(nuser,' NOT ','     ')+chr($af)+user+chr($ae));
      app_l(' Betr:     '+iifs(or_betr,' OR','   ')
           +iifs(nbetr,' NOT ','     ')+chr($af)+betr+chr($ae));
      app_l(' Fidoempf: '+iifs(or_fidoempf,' OR','   ')
           +iifs(nfidoempf,' NOT ','     ')+chr($af)+fidoempf+chr($ae));
    end;
    list(brk);
    closelist;
    closebox;
   end;
{$ENDIF}

  begin
{$IFDEF sDebug}
      for n:=0 to suchmax-1 do
      begin
        seekstart[n]:=0;
        seeklen[n]:=0;
        seeknot[n]:=false;
        end;
{$ENDIF}
    suchand:=cpos('o', LowerCase(suchopt))=0;             { OR }
    if not suchand or (cpos('a', LowerCase(suchopt))>0)   { oder AND ?}
     and not (trim(sst)='') then                    { und nicht Leertext (Suche-Spezial) }
    begin
      n:=0;
      seek:=trim(sst);                                 { Leerzeichen vorne und hinten, }
      i:=length(seek);
      while (i <> 0) and (seek[i]='"') do dec(i);      { Und Ausrufezeichen hinten abschneiden }
      truncstr(seek,i);
      if seek<>'' then begin
        i:=1;
        sst:=seek+'"';  quotes:=false;
        while (i<length(sst)) and (n<suchmax) do
        begin
          while sst[i]=' ' do inc(i);                  { Leerzeichen ueberspringen }
          if not quotes then
          begin
            seeknot[n]:=sst[i]='~';                    { NOT Flag setzen }
            while ((sst[i]='~') or (sst[i]=' '))
              do inc(i);                               { und evtl weitere ^ ueberspringen }
            end;
          quotes:=sst[i]='"';                          { Evtl. "- Modus aktivieren....}
          while sst[i]='"' do inc(i);                  { weitere " ueberspringen }
          seekstart[n]:=i;
          while (i<length(sst)) and not                { Weiterzaehlen bis Stringende      }
           ((not quotes and (sst[i]=' ')) or           { oder Space das nicht in " ist     }
            (sst[i]='"')) do inc(i);                   { oder das naechste " gefunden wird }
          seeklen[n]:=i-seekstart[n];
          quotes:=not quotes and (sst[i]='"');         { -"- Modus umschalten }
          if (not quotes) then inc(i);
          inc(n);
        end;
        if seeklen[n-1]=0 then dec(n);                 { Falls String mit > "< Endete... }
        suchanz:=n;
      end;

      if suchanz=1 then suchand:=true;
      m:=0;
      for n:=0 to suchanz-1 do        { Teilstrings Umsortieren: NOT zuerst }
      begin
        if (seeknot[n]=true) and (seeklen[n]<>0) then
        begin
          i:=seekstart[m];    seekstart[m]:=seekstart[n];  seekstart[n]:=i;
          i:=seeklen[m];      seeklen[m]:=seeklen[n];      seeklen[n]:=i;
          quotes:=seeknot[m]; seeknot[m]:=seeknot[n];      seeknot[n]:=quotes;
          inc(m);
          end;
        end;
      end

    else begin
      suchand:=true;
      suchanz:=1;
      seekstart[0]:=1;
      seeklen[0]:=length(sst);
      seeknot[0]:=false;
      end;

{$IFDEF sDebug}
    if cpos('c',lstr(suchopt))>0 then show_seekstrings; { "Writeln ist der beste Debugger..." }
{$ENDIF}

  end;


  function InText(const key:string):boolean;
  var size : longint;
      ofs  : longint;
      wsize: Integer;
      s: String;
  begin
    dbReadN(mbase,mb_msgsize,size);
    if size=0 then begin   { leerer Datensatz - vermutlich durch RuntimeError }
      dbDelete(mbase);
      InText:=false;
      end
    else begin
      wsize:=min(size,psize);
      ofs:=dbReadIntN(mbase, mb_msgsize)-dbReadIntN(mbase,mb_groesse);

      if headersuche=1 then begin           { nur Header durchsuchen }
        wsize:=ofs;
        ofs:=0;
        end
      else if headersuche=2 then ofs:=0;    { Header und Text durchsuchen }

      if (ofs>=0) and (ofs<wsize+1+length(key)) then
      begin
        dec(wsize,ofs);
        XmemRead(ofs,wsize,p^);
        if RegEx then
        begin
          RegExpr.Expression := key;
          SetString(s, PChar(p), WSize);
          InText := RegExpr.Exec(s);
        end else
        begin
          TxtSeekKey := Key;
          Intext := TxtSeek(p,wsize,igcase,umlaut);
          // hack to allow searching in iso charset
          if (result = false) and (wsize > 0) then
          begin
            SetString(s,pChar(p), wsize);
            s := isotoibm(s);
            InText := TxtSeek(@s[1], Length(s), igcase, umlaut);
          end;
        end;
      end else
        Intext:=false;
      end;
  end;


  function DateFit:boolean;
  var d : longint;
  begin
    dbReadN(mbase,mb_origdatum,d);
    DateFit:=not smdl(d,_vondat) and not smdl(_bisdat,d);
  end;

  function Sizefit:boolean;
  var s : longint;
  begin
    dbReadN(mbase,mb_groesse,s);
    sizefit:=(s>=minsize) and (s<=maxsize);
  end;

  function TypeFit:boolean;
  var t     : char;
      nt    : longint;
      flags : longint;
  begin
    if typc=' ' then typefit:=true
    else begin
      dbReadN(mbase,mb_typ,t);
      dbReadN(mbase,mb_netztyp,nt);
      dbReadN(mbase,mb_flags,flags);
      TypeFit:=((typc='F') and (nt and $200<>0)) or
               ((typc='M') and (flags and 4<>0)) or
               (t=typc);
      end;
  end;

  function StatOk:boolean;
  var flags : byte;
  begin
    dbReadN(mbase,mb_halteflags,flags);
    case statb of
     1,2 : StatOK:=(statb=flags);
       3 : StatOK:=(flags=1) or (flags=2);
       4 : StatOK:=(dbReadInt(mbase,'gelesen')=0);
       5 : StatOK:=(dbReadInt(mbase,'gelesen')<>0);
     else
       {0 : }StatOk:=true;
    end;
  end;


  { Leerzeichen Links und rechts loschen, Tilden links ebenfalls }
  { boolean setzen, wenn Tilde gefunden wurde }

  procedure Scantilde(var s:String; var suchnot:boolean);
  begin
    trim(s);
    if s='' then
     suchnot:=false
    else
    begin
      suchnot:=s[1]='~';
      i:=1;
      while ((s[i]='~') or (s[i]=' ')) do inc(i);
      s:=mid(s,i);
    end;
  end;


{--Einzelne Nachricht mit Sucheingaben vergleichen--}

  procedure TestMsg;
  var betr2 : string;
      user2 : string;
      realn : string;
      such  : string;
          j : byte;
          d : Longint;
          b : byte;
      found_not : boolean;
      foundmask : byte;

  label msg_ok;

{   Volltextcheck:

    Seekstart und Seeklen sind Zeiger auf Anfang und Ende der Teilsuchstrings
    innerhalb des Gesamtsuchstrings SST. Suchand ist "true" bei UND-Suche,
    und "false" bei ODER-Suche Der Textinhalt wird mit den  Teilsuchstrings verglichen,
    solange Suchand=1 (UND) und Found=0, bzw bis Suchand=0 (OR) und Found=1,
    wurde ein Teilsuchstring gefunden, obwol SeekNOT fuer ihn definiert ist,
    wird die Suche beendet und Found nachtraeglich auf 0 gesetzt (Suche gescheitert).
    NOT-Suchstrings werden dabei aus der UND-Verknuepfung ausgeklammert.
}
    procedure Volltextcheck;
    begin
      j:=0;
      repeat
        seek:=LeftStr(mid(sst,seekstart[j]),seeklen[j]);
        found:=Intext(seek);
        found_not:=found and seeknot[j];
        if suchand and not found and seeknot[j] then found:=true;
        inc(j);
      until (j=suchanz) or (suchand xor found) or found_not;
    if found_not then found:=false;
    end;


  begin
    inc(n);
    if (n mod 30)=0 then
    begin
      moff;
      FWrt(x+9, WhereY, Format('%7d', [n]));
      FWrt(x+26, WhereY, Format('%5d', [nf]));
      mon;
    end;

{--Spezialsuche--}
    if spez then with srec^ do
    begin
      if DateFit and SizeFit and TypeFit and StatOk then begin
        Betr2 := dbReadNStr(mbase,mb_betreff);
        if (betr<>'') and (length(betr2)=40) then begin
          ReadHeader(hdp,hds,false);
          if length(hdp.betreff)>40 then
            betr2:=hdp.betreff;
          end;
        user2 := dbReadNStr(mbase,mb_absender);
        if not ntEditBrettEmpf(mbnetztyp) then begin   { <> Fido, QWK }
          realn:= dbReadNStr(mbase,mb_name);
          end
        else
          realn:=#0;
        if fidoempf<>'' then
          if not ntBrettEmpf(mbnetztyp) then
            hdp.fido_to:=''
          else begin
            ReadHeader(hdp,hds,false);
            end;
        if umlaut then begin                    { Umlaute anpassen}
          UkonvStr(betr2,Length(betr2));
          UkonvStr(user2,Length(user2));
          UkonvStr(realn,Length(realn));
          UkonvStr(hdp.fido_to,Length(hdp.fido_to));
          end;
        if igcase then begin                    { Ignore Case}
          UpString(betr2);
          UpString(user2);
          UpString(realn);
          UpString(hdp.fido_to);
          end;

        if andmask<>0 then begin
          foundmask:=0;
          if ((betr='') or (pos(betr,betr2)>0) xor nbetr)
            then inc(foundmask,2);
          if ((user='') or ((pos(user,user2)>0) or (pos(user,realn)>0)) xor nuser)
            then inc(foundmask,4);
          if ((fidoempf='') or (pos(fidoempf,hdp.fido_to)>0) xor nfidoempf)
            then inc(foundmask);
          if foundmask and ormask <> 0 then goto msg_ok;
          if (foundmask and andmask) <> (andmask and not ormask) then exit;
          end;

        if txt<>'' then begin
          volltextcheck;
          if not found then exit;
          end;

msg_ok: MsgAddmark;
        inc(nf);
        end
    end

    else begin

      if check4date and (readmode >0) then 
      begin                                     { Suchen im akt. Lesemodus }
        if readmode=1 then begin
          dbReadN(mbase,mb_gelesen,b);    
          if b>0 then exit;
          end
        else if aktdispmode <> 10 then begin
          dbReadN(mbase,mb_empfdatum,d);
          if smdl(d,readdate) then exit; 
          end;
        end;
                                                { Headereintrag-Suche }
      if suchfeld<>'' then
      begin
        such := dbReadStr(mbase,suchfeld);
 
        if (suchfeld='Absender') and not ntEditBrettEmpf(mbnetztyp)
        then begin
          seek := dbReadNStr(mbase,mb_name);          { Bei Usersuche auch Realname ansehen... }
          such:=such+seek;
        end;

        if stricmp(suchfeld,'betreff') and (length(such)=40)
        then begin
          ReadHeader(hdp,hds,false);
          if length(hdp.betreff)>40 then
            such:=hdp.betreff;
          end;
         if suchfeld='MsgID' then begin
          ReadHeader(hdp,hds,false);
          such:=hdp.msgid;
          end;
        if umlaut then UkonvStr(such,Length(such));

        j:=0;
        repeat
          seek:=LeftStr(mid(sst,seekstart[j]),seeklen[j]);      { Erklaerung siehe Volltextcheck }
          found:=((igcase and (pos(seek,UpperCase(such))>0)) or
           (not igcase and (pos(seek,such)>0)));
          found_not:=found and seeknot[j];
          if suchand and not found and seeknot[j] then found:=true;
          inc(j);
        until (j=suchanz) or (suchand xor found) or found_not;
        if found_not then found:=false;

        if Found then Begin
          MsgAddmark;
          inc(nf);
          end;
      end

      else begin                           { Volltextsuche }
        volltextcheck;
        if found then Begin
          MsgAddmark;
          inc(nf);
          end;
        end;
      end;
  end;



  procedure TestBrett(const _brett:string);
  begin
    if check4date {and (aktdispmode<=10) } and (readmode>0)
      then if Readmode>1 then dbSeek(mbase,miBrett,_brett+dbLongStr(readdate))
                         else dbSeek(mbase,miGelesen,_brett+#0)
    else dbSeek(mbase,miBrett,_brett);
    while not dbEof(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and not brk do
    begin
      TestMsg;
      dbNext(mbase);
      testbrk(brk);
      end;
  end;

  function userform(const s:string):string;
  var p : Integer;
  begin
    p:=cpos('@',s);
    if p=0 then userform:=s
    else userform:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1));
  end;


  procedure InitHistory;        { Such-History beim Programmstart aus Datei laden }
  var i : byte;
  var t : text;
  begin
    assign(t,historyFile);
    reset(t);
    if ioresult<>0 then exit;
    for i:=0 to histmax do readln(t,history[i]);
    close(t);
    assign(t,optionsFile);
    reset(t);
    if ioresult<>0 then exit;
    for i:=0 to opthmax do readln(t,opthist[i]);
    close(t);
  end;

  procedure CheckHistory;       { Such-History Aktualisieren und in Datei speichern }
  var i,h: byte;
  var t  : text;
  begin
    if (suchstring='') or history_changed then exit;
    h:=histmax;
    for i:=0 to histmax do if history[i]=suchstring then h:=i;
    for i:=h downto 1 do history[i]:=history[i-1];
    history[0]:=suchstring;

    h:=opthmax;
    for i:=0 to opthmax do if opthist[i]=suchopt then h:=i;
    for i:=h downto 1 do opthist[i]:=opthist[i-1];
    opthist[0]:=suchopt;

    assign(t,historyFile);
    rewrite(t);
    for i:=0 to histmax do writeln(t,history[i]);
    close(t);

    assign(t,optionsFile);
    rewrite(t);
    for i:=0 to opthmax do writeln(t,opthist[i]);
    close(t);
  end;

  // adds new message id to boxname.mid
  procedure AddMsgId;
  var
    Boxname, Filename: String;
    IDList: TStringList;
  begin
    if ReadJN('Soll die Message-ID online gesucht werden', true) then
    begin
      BoxName := UniSel(1, false, DefaultBox);
      if BoxName <> '' then
      begin
        ReadboxPar(0, Boxname); 
        Filename := OwnPath + BoxPar.ClientPath + GetServerFilename(Boxname, extMid);
        IDLIst := TStringList.Create;
        try
          with IDList do
          begin
            if FileExists(Filename) then
            begin
              LoadFromFile(Filename);
              Sort;
            end;
            Sorted := true;
            Duplicates := dupIgnore;
            Add(Suchstring);
            SaveToFile(Filename);
          end;
        finally
          IDList.Free;
        end;
      end;
    end;
  end;

{--# Suche #--}

begin
  RegExpr := TRegExpr.Create;
  for i:=0 to 4 do bera[i]:=getres2(442,i);
  for i:=0 to 5 do stata[i]:=getres2(442,10+i);
  for i:=0 to 4 do typa[i]:=getres2(442,20+i);
  if FirstChar(suchopt)='*' then
  begin                                       { Erste Suche seit Programmstart? }
    suchopt:='au';
    InitHistory;
  end;

  if srec=nil then begin
    new(srec);
    fillchar(srec^,sizeof(srec^),0);
    with srec^ do begin
      vondat:='01.01.80'; bisdat:='31.12.69';
      vonkb:=0; biskb:=maxlongint div 2048;
      typ:=typa[0]; status:=stata[0];
      end;
    end;
  spez:=(suchfeld='*');
  case aktdispmode of
    -1,0 : bretter:=bera[iif(bmarkanz>0,3,1)];
    1..4 : bretter:=bera[iif(bmarkanz>0,3,2)];
    10   : bretter:=bera[4];
  else     bretter:=bera[0];
  end;
  i:=0;
  while (i<=4) and (bretter<>bera[i]) do inc(i);
  if i>4 then bretter:=bera[0];


{-- Eingabemaske Normalsuche --}

  MaskShiftF2(seekmenu,534);

restart:

  MaskSeekMenu:=iif(spez,4,1);
  if not spez then begin
    add:=0;
(*  if autosuche='' then begin *)
      dialog(51,7,getreps2(441,1,anztxt),x,y);         { '%s-Suche' }
      if autosuche<>'' then suchstring:=autosuche
      else if suchfeld='Betreff' then suchstring:=srec^.betr
      else if suchfeld='Absender' then suchstring:=srec^.user
      else if suchfeld='MsgID' then suchstring:=srec^.mid       { MID Suche aus Menue  }

      else suchstring:=srec^.txt;
      maddstring(3,2,getres2(441,2),suchstring,32,SuchLen,range(' ',#255));
      mnotrim;
      if history[0] <> '' then  { Bei Leerer Suchhistory kein Auswahlpfeil... }
        for i:=0 to histmax do mappsel(false,history[i]);
      mset3proc(seek_cutspace);
      mhnr(530);                                       { 'Suchbegriff ' }
      maddstring(3,4,getres2(441,3),suchopt,8,8,'');   { 'Optionen    ' }
      if opthist[0] <>'' then
        for i:=0 to opthmax do mappsel(false,opthist[i]);
      maddstring(31,4,getres2(441,4),bretter,8,8,'');  { 'Bretter '     }
      mid_bretter:=fieldpos;
      if (aktdispmode=11) or (suchfeld='#') then
        MDisable
      else begin
        for i:=0 to 4 do
          mappsel(true,bera[i]);    { Alle / Netz / User / markiert / gewÑhlt }
        mset1func(testbrettscope);
        end;
      if autosuche<>'' then _keyboard(keypgdn);

      if suchfeld='MsgID' then
      Begin
        Mid_teilstring:=false;
        Maddbool(3,6,getres2(442,25),Mid_teilstring);
        MSet1func(Mid_suchoption);
        if mid_suchoption(suchfeld) then;
      end;

      readmask(brk);
      MaskSeekMenu:=0;
      closemask;
      CheckHistory;

      if suchfeld='Betreff' then begin
        i:=ReCount(suchstring);         // Re's wegschneiden
        srec^.betr:=suchstring
        end

      else if suchfeld='Absender' then begin
        suchstring:=userform(suchstring);
        srec^.user:=suchstring;
        end

      else if suchfeld='MsgID' then srec^.mid:=suchstring   {JG: 22.01.00}

      else srec^.txt:=suchstring;
      if suchstring='' then goto ende;
      dec(x); inc(y);
    end


{--Eingabemaske Spezialsuche--}

  else with srec^ do begin
                                     { Spezial: NOT-Flags wieder an Suchstrings setzen }
    if nbetr and (betr[1]<>'~') then betr:='~'+betr; 
    if nuser and (user[1]<>'~') then user:='~'+user;
    if nfidoempf and (fidoempf[1]<>'~') then fidoempf:='~'+fidoempf;
  
    add:=iif(ntBrettEmpfUsed,1,0);
    dialog(53,12+add,getreps2(441,1,anztxt),x,y);
    i:=4;
    while (i>0) and (UpperCase(typ)<>UpperCase(typa[i])) do dec(i);
    typ:=typa[i];
    i:=5;
    while (i>0) and (UpperCase(status)<>UpperCase(stata[i])) do dec(i);
    status:=stata[i];
    maddstring(3,2,getres2(441,6),user,30,SuchLen,'');  mhnr(630);   { 'Absender  ' }
    maddstring(3,3,getres2(441,7),betr,30,SuchLen,'');    { 'Betreff   ' }
    mnotrim;
    mset3proc(seek_cutspace);
    if ntBrettEmpfUsed then
      maddstring(3,4,getres2(441,9),fidoempf,30,SuchLen,'');    { 'Fido-Empf.' }
    maddstring(3,4+add,getres2(441,8),txt,35,SuchLen,'');     { 'Text      ' }
    if history[0] <> '' then  { Bei leerer Suchhistory kein Auswahlpfeil... }
      for i:=0 to histmax do mappsel(false,history[i]);
    mnotrim;
    mset3proc(seek_cutspace);
    maddtext(48,1,'OR',0);
    maddbool(46,2,'',or_user);mhnr(640); 
    maddbool(46,3,'',or_betr);
    if ntBrettEmpfUsed then maddbool(46,4,'',or_fidoempf);  
    madddate(3,6+add,getres2(441,10),vondat,false,false); mhnr(634); { 'von Datum ' }
    madddate(3,7+add,getres2(441,11),bisdat,false,false); mhnr(634); { 'bis Datum ' }
    maddint(30,6+add,getres2(441,19),vonkb,6,5,0,99999);  mhnr(635); { 'von ' }
      maddtext(45,6+add,getres(14),0);   { 'KBytes' }
    biskb:=min(biskb,99999);
    maddint(30,7+add,getres2(441,20),biskb,6,5,0,99999);  mhnr(635); { 'bis ' }
      maddtext(45,7+add,getres(14),0);   { 'KBytes' }
    maddstring(3,9+add,getres2(441,12),typ,8,9,'');         { 'Typ       ' }
    for i:=0 to 4 do
      mappsel(true,typa[i]);
    maddstring(3,10+add,getres2(441,13),status,8,8,'');     { 'Status    ' }
    for i:=0 to 5 do
      mappsel(true,stata[i]);
    maddstring(30,9+add,getres2(441,14),bretter,8,8,'');    { 'Bretter   ' }
    if aktdispmode=11 then
      MDisable
    else begin
      for i:=0 to 4 do
        mappsel(true,bera[i]);
      mset1func(testbrettscope);
      end;
    maddstring(30,10+add,getres2(441,15),suchopt,8,8,'');   { 'Optionen  ' }
    if opthist[0] <>'' then
      for i:=0 to opthmax do mappsel(false,opthist[i]);
    readmask(brk);
    MaskSeekMenu:=0;
    closemask;
    dec(x);
    suchstring:=txt;
    CheckHistory;
    end;

{--Eingaben auswerten--}

  if not brk then with srec^ do begin

    if spez then begin
      andmask:=0; ormask:=0;
      if user='' then or_user:=false else andmask:=4;
      if betr='' then or_betr:=false else inc(andmask,2);
      if fidoempf='' then or_fidoempf:=false else inc(andmask); 
      if or_user then ormask:=4;
      if or_betr then inc(ormask,2);
      if or_fidoempf then inc(ormask);   
      if txt='' then  ;
      {$IFNDEF NOASM }
        asm
             mov al,ormask  { verhindern, dass alle Suchbegriffe auf OR stehen }
             or al,al
             je @2
             cmp al,andmask
             jne @2
             mov cl,0
         @1: inc cx
             shr al,1
             jnc @1
             shl al,cl
             mov ormask,al
         @2:
        end;
      {$ENDIF }
      end;

    sst:=suchstring;
    igcase:=multipos('iu', LowerCase(suchopt));
    umlaut:=multipos('ÑîÅu', LowerCase(suchopt)); { Umlautschalter}
    regex := pos('r', LowerCase(suchopt)) > 0 ;
    if umlaut and not igcase then 
    begin
      suchopt:=suchopt+'i';
      igcase:=true;
      end;
    check4date:=cpos('l', LowerCase(suchopt))>0;  { Suchen ab aktuellem Lesedatum }
    HoldMarked:=cpos('m', LowerCase(suchopt))>0;  { Alte Markierungen beibehalten } 

    i:=cpos('s', LowerCase(suchopt));             { Such-History loeschen }
    if i>0  then                   
    begin 
      delete(suchopt,i,1); 
      for i:=1 to histmax do history[i]:='';
      CheckHistory;
      closebox;
      goto restart;
      end;

    i:=cpos('k', LowerCase(suchopt));             { Such-History loeschen }
    if i>0  then                   
    begin
      delete(suchopt,i,1); 
      assign(t,libraryFile);
      append(t);
      if ioresult<>0 then rewrite(t);
      if trim(suchstring)<>'' then writeln(t,suchstring);
      close(t);
      closebox;
      goto restart;
      end; 

    headersuche:=0;                                     { Volltextsuche }
    if cpos('h', LowerCase(Suchopt))>0 then headersuche:=1;   { Headersuche   }
    if cpos('g', LowerCase(suchopt))>0 then headersuche:=2;   { Volltext+Headersuche } 

    bereich:=0;
    for i:=1 to 4 do
      if UpperCase(bretter)=UpperCase(bera[i]) then bereich:=i;
    statb:=0;
    for i:=1 to 5 do
      if UpperCase(status)=UpperCase(stata[i]) then statb:=i;
    me:=true;
    attrtxt(col.coldialog);

    if spez then with srec^ do begin
      sst:=txt;
      user:=userform(user);
      if umlaut then begin                              { Umlaute konvertieren }
        UkonvStr(betr, Length(betr)); UkonvStr(user, Length(user));
       { UkonvStr(txt,high(txt));} UkonvStr(fidoempf,Length(fidoempf));
        end;
      if igcase then begin
        UpString(betr); UpString(user); {UpString(txt);} UpString(fidoempf);
        end;
      scantilde(betr,nbetr); scantilde(user,nuser);
      scantilde(fidoempf,nfidoempf);
      if UpperCase(typ)=UpperCase(typa[1]) then typc:='T'
      else if UpperCase(typ)=UpperCase(typa[2]) then typc:='B'
      else if UpperCase(typ)=UpperCase(typa[3]) then typc:='F'
      else if UpperCase(typ)=UpperCase(typa[4]) then typc:='M'
      else typc:=' ';
      _vondat:=ixdat(copy(vondat,7,2)+copy(vondat,4,2)+copy(vondat,1,2)+'0000');
      _bisdat:=ixdat(copy(bisdat,7,2)+copy(bisdat,4,2)+copy(bisdat,1,2)+'2359');
      if biskb=99999 then biskb:=maxlongint div 2048;
      minsize:=vonkb*1024;
      maxsize:=biskb*1024+1023;
      end;
   { else begin}
      if umlaut then UkonvStr(sst, Length(sst));                        { 15.02.00}
      if igcase then UpString(sst);
    {  end;}

{--Start der Suche--}

    markanzback:=markanz;

    if suchfeld='#' then begin   {Lister-Dummysuche}
      check_seekmode; 
      CloseBox;
      exit;
      end;

    if (suchfeld='MsgID') and NOT MID_teilstring then begin      {-- Suche: Message-ID  --}
      suche:=false;
      if not brk then begin
        if not holdmarked then markanz:=0;
        check_seekmode; 
        for i:=0 to suchanz-1 do
        begin
          seek:=copy(suchstring,seekstart[i],seeklen[i]);
          n:=GetBezug(seek);
          if n<>0 then begin
            dbGo(mbase,n);
            MsgAddmark;
            end;
          end;
        end;
      end
                                             { Anzeige fuer alle anderen Suchvarianten }
    else begin
      {if spez then sst:=txt;  } { Bei Spezialsuche nur im Volltext... }
      if brk then goto ende;
      
      if history_changed then begin
        history_changed:=false;
        closebox;
        goto restart;
        end;

      check_seekmode;          { Vorbereiten fuer verknuepfte Suche}
      if brk then begin
        closebox;
        goto restart;
        end;         

      mwrt(x+3,y+iif(spez,11+add,4),getres2(441,16)); { 'Suche:         passend:' }
      if (aktdispmode<>11) and not holdmarked then markanz:=0;
      n:=0; nf:=0;
      hdp := THeader.Create;
      attrtxt(col.coldiahigh);
      psize:=65536;
      getmem(p,psize);
      brk:=false;

      if aktdispmode=11 then
      begin                       {-- Suche markiert (Weiter suchen) --}
        getmem(markedback,maxmark * sizeof(markrec));
        for i:=0 to markanz do markedback^[i]:=marked^[i];
        markanzback:=markanz;
        i:=0;
        while i<markanz do begin
          dbGo(mbase,marked^[i].recno);
          msgunmark;
          TestMsg;
          if MsgMarked then inc(i);
          end;
        aufbau:=true;

        if (markanz=0) and (markanzback<>0) then  
        begin
          hinweis(getres2(441,18));   { 'keine passenden Nachrichten gefunden' }
          markanz:=markanzback;
          for i:=0 to markanz do marked^[i]:=markedback^[i];
          end;
        if markanzback<>0 then freemem(markedback,maxmark * sizeof(markrec));    
        end

      else if bereich<3 then begin                       {-- Suche: Alle/Netz/User --}
        mi:=dbGetIndex(mbase);
        dbSetIndex(mbase,0);
        dbGoTop(mbase);
        brk:=false; i := 0;
        while not dbEOF(mbase) and (markanz<maxmark) and not brk do begin
          _brett := dbReadNStr(mbase,mb_brett);
          if (bereich=0) or ((bereich=1) and (FirstChar(_brett)='A')) or
                            ((bereich=2) and (FirstChar(_brett)='U')) then
            TestMsg;
          if not dbEOF(mbase) then    { kann passieren, wenn fehlerhafter }
            dbNext(mbase);            { Satz gelîscht wurde               }
          Inc(i);
          if i mod 50 = 0 then testbrk(brk);
        end;
        dbSetIndex(mbase,mi);
        end

      else begin                                         {-- Suche: aktuelles Brett --}
        mi:=dbGetIndex(mbase);
        dbSetIndex(mbase,miBrett);
        if bereich=3 then begin                          { bzw. markierte Bretter  }
          if aktdispmode<11 then begin
            i:=0;
            uu:=((aktdispmode>0) and (aktdispmode<10));
            while (i<bmarkanz) and not brk do begin
              if uu then begin
                dbGo(ubase,bmarked^[i]);
                TestBrett(mbrettd('U',ubase));
                end
              else begin
                dbGo(bbase,bmarked^[i]);
                brett := dbReadNStr(bbase,bb_brettname);
                TestBrett(mbrettd(FirstChar(brett),bbase));
                end;
              inc(i);
              end;
            end;
          end
        else
          case aktdispmode of
            -1..0 : begin
                      brett := dbReadNStr(bbase,bb_brettname);
                      TestBrett(mbrettd(FirstChar(brett),bbase));
                    end;
             1..4 : TestBrett(mbrettd('U',ubase));
               10 : TestBrett(such_brett);
          else      begin
                      hinweis(getres2(441,17));   { 'kein Brett gewÑhlt' }
                      me:=false;
                    end;
          end;
        dbSetIndex(mbase,mi);
        end;

      freemem(p,psize);
      CloseBox;
      hdp.Free;
      end;

{--Suche beendet--}

    if (markanz=0) or (holdmarked and (markanz=markanzback))   { Nichts gefunden }
    then begin 
      if me then
      begin
        if Suchfeld = 'MsgID' then
          AddMsgId
        else
          hinweis(getres2(441,18));   { 'keine passenden Nachrichten gefunden' }
        aufbau:=true;               { wg. gelîschter Markierung! }
      end;
      goto ende;                    { Fenster wiedeherstellen...}
      end
      
    else begin
      Suchergebnis:=true;
      suche:=true;                  { Suche erfolgreich }
      signal;
      CloseBox;
      end;

    end { of NOT Brk }

  else begin   { brk }
ende:                               { Suche gescheitert/abgebrochen }
    suche:=false;
    CloseBox;
    end;
  freeres;
end;
{ R+}



{ Betreff-Direktsuche }

procedure betreffsuche;
var betr,betr2   : string;
    brett,_Brett : string;
begin
  moment;
  betr := dbReadNStr(mbase,mb_betreff);
  ReCount(betr);  { schneidet Re's weg }
  betr:=trim(betr);
  UkonvStr(betr, Length(betr));
  brett := dbReadNStr(mbase,mb_brett);
  dbSetIndex(mbase,miBrett);
  dbSeek(mbase,miBrett,brett);
  markanz:=0;
  repeat
    betr2 := dbReadNStr(mbase,mb_betreff);
    ReCount(betr2);
    betr2:=trim(betr2);
    UkonvStr(betr2, Length(betr2));
 (*  ll:=min(length(betr),length(betr2));
    if (ll>0) and (UpperCase(left(betr,ll))=UpperCase(left(betr2,ll))) then *)
   if UpperCase(betr)=UpperCase(betr2) then
      MsgAddmark;
    dbSkip(mbase,1);
    if not dbEOF(mbase) then
      _brett := dbReadNStr(mbase,mb_brett);
  until dbEOF(mbase) or (_brett<>brett);
  closebox;
  signal;
  if markanz>0 then select(11);
  aufbau:=true;
end;

//Nachricht/Suchen/Wiedervorlage
procedure SucheWiedervorlage;
var x,y,xx : Integer;
    brk    : boolean;
    _brett : string;
    mbrett : string;
    dat    : string;
    n,nn   : longint;
    bi     : shortint;

  procedure testbase(xbase:pointer);
  begin
    bi:=dbGetIndex(xbase);
    if xbase=bbase then begin
      dbSetIndex(xbase,bibrett);
      dbGoTop(xbase);
      end
    else begin
      dbsetindex(xbase,uiadrbuch);
      dbseek(xbase,uiadrbuch,#1);
      end;
    dat:=dbLongStr(ixDat('2712310000'));

    brk:=false;
    dbSetIndex(mbase,miBrett);
    while not dbEOF(xbase) and not brk do
    begin
      inc(n);
      attrtxt(col.colmboxhigh);
      FWrt(xx, y+2, Format('%3d', [n*100 div nn]));
      if (xbase=ubase) or (not smdl(dbReadInt(xbase,'ldatum'),ixDat('2712310000')))
      then begin
        if xbase=ubase then _brett:='U' else
        _brett:=copy(dbReadStr(xbase,'brettname'),1,1);
        _brett:=_brett+dbLongStr(dbReadInt(xbase,'int_nr'));
        dbSeek(mbase,miBrett,_brett+dat);
        mbrett:=_brett;
        while not dbEOF(mbase) and (mbrett=_brett) do begin
          mbrett := dbReadNStr(mbase,mb_brett);
          if mbrett=_brett then MsgAddmark;
          dbSkip(mbase,1);
          end;
        end;
      dbSkip(xbase,1);
      testbrk(brk);
      end;
    dbSetIndex(xbase,bi);
  end;

begin
  markanz:=0;
  msgbox(33,5,'',x,y);
  wrt(x+3,y+2,getres(443));   { 'Einen Moment bitte...     %' }
  xx:=wherex-5;
  n:=0;
  nn:=dbRecCount(bbase)+dbreccount(ubase);

  testbase(ubase);
  if not brk then
    testbase(bbase);
  CloseBox;
  if not brk then
    if markanz=0 then
      hinweis(getres(444))   { 'keine Wiedervorlage-Nachrichten gefunden' }
    else begin
      signal;
      select(11);
    end;
end;


{ XP4O - Reorganisation }


procedure BU_reorg(user,adrbuch:boolean);
var x,y,xx  : Integer;
    brk,ask : boolean;
    typ     : string;
    d       : DB;
    brett   : string[BrettLen];
    brettc  : char;
    _brett  : string;
    _mbrett : string;
    nfeld   : integer;
    loesch  : string;
    n,gel   : longint;
    leer    : boolean;
    next    : longint;
    null    : byte;

  procedure wrstat;
  begin
    attrtxt(col.coldiahigh);
    moff;
    Wrt(xx,y+1, Format('%5d', [n]));
    Wrt(xx,y+2, Format('%5d', [gel]));
    mon;
  end;

begin
  typ:=getres2(445,iif(user,1,2));     { 'User' / 'Bretter' }
  if not adrbuch then
    ask:=ReadJNesc(getres2(445,iif(user,3,4)),true,brk)   { 'Beim Lîschen von '+typ+'n nachfragen' }
  else
    ask:=ReadJNesc(getres2(445,5),false,brk);   { 'Beim Austragen von Usern nachfragen' }
  if not brk then begin
    if user then begin
      d:=ubase; nfeld:=ub_username; end
    else begin
      d:=bbase; nfeld:=bb_brettname; end;
    n:=0; gel:=0;
    msgbox(25,4,'',x,y);
    if not adrbuch then begin
      mwrt(x+3,y+1,forms(typ,8)+':');
      mwrt(x+3,y+2,getres2(445,6));    { 'gelîscht:' }
      xx:=x+13;
      end
    else begin
      mwrt(x+3,y+1,getres2(445,7));    { 'User ..... :' }
      mwrt(x+3,y+2,getres2(445,8));    { 'ausgetragen:' }
      xx:=x+16;
      end;
    if adrbuch then begin
      dbSetindex(d,uiAdrbuch);
      dbSeek(d,uiAdrbuch,#1);
      end
    else begin
      dbSetindex(d,1);
      dbGoTop(d);
      end;
    if user then brettc:='U';
    null:=0;
    while not (dbEOF(d) or brk) do 
    begin
      inc(n);
      if not user then 
      begin
        brett := dbReadNStr(bbase,bb_brettname);
        brettc:= FirstChar(brett);
      end else
        brett := dbReadNStr(ubase,ub_username);
      _brett:=mbrettd(brettc,d);
      dbSeek(mbase,miBrett,_brett);
      leer:=dbEOF(mbase);
      if not leer then begin
        _Mbrett := dbReadNStr(mbase,mb_brett);
        leer:=_mbrett<>_brett;
        end;

      if leer and ((user and (LeftStr(brett,4)<>#0+'$/T')) or
        (not user and (LeftStr(brett,3)<>'$/T'))) then
      begin
        loesch := dbReadNStr(d,nfeld);
        if not user then loesch:=copy(loesch,2,80);
        if adrbuch then
          if (dbReadInt(ubase,'userflags') and 4=0) and
             (dbXsize(ubase,'adresse')=0) then
            if not ask or (ReadJNesc(getreps2(445,9,LeftStr(loesch,55)),true,brk)and not brk)
            then begin                                    { '%s austragen' }
              dbSkip(d,1);
              next:=dbRecno(d);
              if dbEOF(d) then dbGoEnd(d)
              else dbSkip(d,-1);
              dbWriteN(d,ub_adrbuch,null);
              dbGo(d,next);
              inc(gel);
              end
            else
              dbNext(d)
          else
            dbNext(d);
        if not adrbuch then
          if (not user or
             ((dbReadInt(d,'adrbuch')=0) and (dbReadInt(d,'userflags') and 5=1)))
            and (not ask or (ReadJNesc(getreps2(445,10,LeftStr(loesch,60)),true,brk)and not brk))
          then begin                                      { '%s lîschen' }
            if (user and (aktdispmode in [1..4])) or
               (not user and (aktdispmode<=0)) then
              UBunmark(dbRecno(d));
            dbDelete(d);
            inc(gel);
            end
          else
            dbNext(d);
        end   { not leer }
      else dbNext(d);
      wrstat;
      if (n mod 16=0) and not brk then
      begin
        testbrk(brk);
        if brk and not ReadJN(getres(446),true) then   { 'Reorganisation abbrechen' }
          brk:=false;
        end;
      end;
    closebox;
    end;
  freeres;
end;


procedure MsgReorgScan(_del,repair:boolean; var brk:boolean);
var x,y,wdt: Integer;
    n,ndel,
    nbesch : Int64;
    bt,dbt,
    { HJT 11.03.2006 sonst Abbruch bei mehr als 2 GB }
    { bbt    : longint; }
    bbt    : Int64;
    disp   : string;
    hzeit  : integer16;
    hzahl  : boolean;
    dat    : TDateTime;
    bi     : shortint;

  procedure display;
  begin
    attrtxt(col.colmboxhigh);
    moff;
    Wrt(x+wdt+3,y+4, Format('%7d', [n]));
    Wrt(x+wdt+3,y+5, Format('%7d', [ndel]));
    Wrt(x+wdt+3,y+6, Format('%7d', [nbesch]));
    Wrt(x+wdt+12,y+4, Format('%7d', [bt div 1024]));
    Wrt(x+wdt+12,y+5, Format('%7d', [dbt div 1024]));
    Wrt(x+wdt+12,y+6, Format('%7d', [bbt div 1024]));
    mon;
  end;

  procedure testdel(const _brett:string);
  var _mbrett : string;
      haltedat: longint;
      edat    : longint;
      msize   : longint;
      groesse : longint;
      hflags  : byte;
      uvs,b   : byte;
      ablage  : byte;
      defekt  : boolean;
      hdp     : THeader;
      hds     : longint;
      nzahl   : longint;
      typ     : char;
      defekt_reason : integer;
      hd_read       : boolean;
      inr_deb       : longint;
      brett_deb     : string;
      edat_deb      : longint;
      dat_deb       : longint;

    function htimeout:boolean;
    begin
      htimeout:=(hzahl and (hzeit>0) and (nzahl>hzeit)) or
                (not hzahl and smdl(edat,haltedat));
    end;

  begin
    Debug.DebugLog('xp4o','testdel, Brett-Start',dlDebug);
    hd_read:=false;

    if hzahl then begin
      dbSeek(mbase,miBrett,_brett+#$ff#$ff);  { Brettende suchen }
      if dbEOF(mbase) then dbGoEnd(mbase)
      else dbSkip(mbase,-1);
      if dbBOF(mbase) then exit;
      while not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
            (dbReadIntN(mbase,mb_unversandt) and 8<>0) do
        dbSkip(mbase,-1);
      if dbBOF(mbase) then exit;
      end
    else begin
      dbSeek(mbase,miBrett,_brett);           { Brettanfang suchen }
      if dbEOF(mbase) then exit;
      end;

    if hzahl or (hzeit=0) then
      haltedat:=0
    else
      haltedat:=ixdat(FormatDateTime('yymmdd', Dat-hzeit+1) + '0000');
    nzahl:=1;

    brk := false;
    hdp := THeader.Create;
    repeat
      if n mod 100 = 0 then testbrk(brk);
      if brk then
        brk:=ReadJN(getres(iif(_del,446,447)),true);   { (Reorganisation) 'abbrechen' }
      _mbrett := dbReadNStr(mbase,mb_brett);
      if _mbrett=_brett then begin
        inc(n);
        dbReadN(mbase,mb_msgsize,msize);
        inc(bt,msize);
        dbReadN(mbase,mb_groesse,groesse);
        dbReadN(mbase,mb_ablage,ablage);
        dbReadN(mbase,mb_typ,typ);
        dbReadN(mbase,mb_halteflags,hflags);
        defekt:=(groesse<0) or (msize<0) or (groesse+14>msize) or (ablage>=ablagen) or
                (msize-groesse>iif(ntZCablage(ablage),1000000,8000)) or
                (dbReadIntN(mbase, mb_adresse)+msize>ablsize[ablage]) or
                ((typ<>'T') and (typ<>'B') and (typ<>'M')) or (hflags>2) or
                (dbReadIntN(mbase, mb_adresse)<0) or
                (dbReadIntN(mbase, mb_netztyp)<0);    { empfanz > 127 ? }
        if defekt then { HJT 23.03.2006, Infos zur Nachricht ins Log }
        begin
          defekt_reason := 1;
        end;

        if repair and not defekt then begin
          dbReadN(mbase,mb_gelesen,b);
          if b>1 then begin
            b:=0;
            dbWriteN(mbase,mb_gelesen,b);
            end;
          ReadHeader(hdp,hds,false);
          defekt:=(hds=1) or (hds<>msize-groesse) or (groesse<>hdp.groesse);
          if defekt then { HJT 23.03.2006, Infos zur Nachricht ins Log }
          begin  
            defekt_reason := 2;
            Debug.DebugLog('xp4o','testdel, Nachricht defekt wg.: '+
                          '(hds=1) or (hds<>msize-groesse) or (groesse<>hdp.groesse)'
                          ,DLWarning);
          end;
          end;

          if defekt then    { HJT 23.03.2006, Infos zur Nachricht ins Log }
          begin
            Debug.DebugLog('xp4o','Nachricht defekt, reason: '+IntToStr(defekt_reason),dlInform);
            Debug.DebugLog('xp4o','   Werte des aktuellen MSGS.DB1-Satzes', DLWarning);
            Debug.DebugLog('xp4o','   groesse        : '+IntToStr(groesse), DLWarning);
            Debug.DebugLog('xp4o','   msize          : '+IntToStr(msize), DLWarning);
            Debug.DebugLog('xp4o','   ablage         : '+IntToStr(ablage), DLWarning);
            Debug.DebugLog('xp4o','   ablagen        : '+IntToStr(ablagen), DLWarning);
            Debug.DebugLog('xp4o','   mb_adresse     : '+IntToStr(dbReadIntN(mbase, mb_adresse)), DLWarning);
            Debug.DebugLog('xp4o','   ablsize[ablage]: '+IntToStr(ablsize[ablage]), DLWarning);
            Debug.DebugLog('xp4o','   typ            : <'+typ+'>', DLWarning);
            Debug.DebugLog('xp4o','   hflags         : '+IntToStr(hflags), DLWarning);
            Debug.DebugLog('xp4o','   mb_netztyp     : '+IntToStr(dbReadIntN(mbase, mb_netztyp)), DLWarning);
            Debug.DebugLog('xp4o','   ntZCablage     : '+iifs(ntZCablage(ablage), 'True', 'False'), DLWarning);
            if hd_read then 
            begin
              if hds <> 1 then 
              begin
                Debug.DebugLog('xp4o','    hdp.groesse: '+IntToStr(hdp.groesse),DLWarning);
              end else begin
                Debug.DebugLog('xp4o','    Header aus MPUFFER konnte nicht gelesen werden',DLWarning);
              end;
            end;
            // duerfen wir das? Erstes Byte ist String-Laenge, und moeglicherweise korrupt
            Debug.DebugLog('xp4o','   absender       : '+dbReadNStr(mbase,mb_absender), DLWarning);
            dbRead(mbase,'INT_NR',inr_deb);
            Debug.DebugLog('xp4o','   INT_NR         : '+IntToStr(inr_deb), DLWarning);
            brett_deb := dbReadNStr(mbase,mb_brett);
            Debug.DebugLog('xp4o','   brett          : '+brett_deb, DLWarning);
            dbReadN(mbase,mb_EmpfDatum,edat_deb);
            Debug.DebugLog('xp4o','   edat           : '+fdat(longdat(edat)), DLWarning);
            dbReadN(mbase,mb_OrigDatum,dat_deb);
            Debug.DebugLog('xp4o','   dat            : '+fdat(longdat(dat_deb)), DLWarning);
            Debug.DebugLog('xp4o','   MID(intern)    : '+dbReadNStr(mbase,mb_msgid), DLWarning);
        end;

        if defekt then begin
          hflags:=2;        { Nachricht defekt }
          Debug.DebugLog('xp4o','setze hflags:=2', DLWarning);
          dbWriteN(mbase,mb_halteflags,hflags);
          if repair then msgaddmark;
          inc(nbesch); inc(bbt,msize);
        end;
        if typ='M' then begin
          typ := 'T';
          dbWriteN(mbase,mb_typ,typ);
        end;
        
        dbReadN(mbase,mb_empfdatum,edat);
        dbReadN(mbase,mb_unversandt,uvs);
        if (msize=0) or   { nur zur Sicherheit - sollte nicht vorkommen }
           ((uvs and 1=0) and ((hflags=2) or ((hflags<>1) and htimeout)))
        then begin
          inc(ndel);
          inc(dbt,msize);
          if _del and (hflags<>2) then begin
            hflags:=2;
            dbWriteN(mbase,mb_halteflags,hflags);
            end;
          if ablage<ablagen then reobuf[ablage]:=true;
          end
        else
          if ablage<ablagen then inc(bufsiz[ablage],msize);
        dbSkip(mbase,iif(hzahl,-1,1));
        inc(nzahl);
        if n mod 20=0 then display;
        end;
    until brk or (_mbrett<>_brett) or dbEOF(mbase) or dbBOF(mbase);
    Hdp.Free;
  end;

begin
  if dbRecCount(mbase)=0 then begin
    rfehler(420);   { 'keine Nachrichten vorhanden!' }
    brk:=true;
    exit;
    end;
  if not repair then MausInfoReorg;
  wdt:=length(getres2(448,4));
  msgbox(max(45,wdt+33),iif(_del,9,10),getres2(448,iif(_del,1,iif(repair,2,3))),x,y);
  mwrt(x+3,y+4,getres2(448,4)+'        /        KB');   { 'Nachrichten:' }
  mwrt(x+3,y+5,getres2(448,5)+'        /        KB');   { 'auf Lîschen:' }
  mwrt(x+3,y+6,getres2(448,6)+'        /        KB');   { 'fehlerhaft: ' }
  n:=0; ndel:=0; nbesch:=0;
  bt:=0; dbt:=0; bbt:=0;
  getablsizes;
  Dat := Now;
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biBrett);
  dbGoTop(bbase);
  dbSetIndex(mbase,miBrett);
  brk:=false;
  fillchar(reobuf,sizeof(reobuf),false);
  fillchar(bufsiz,sizeof(bufsiz),0);
  if repair then markanz:=0;
  while not brk and not dbEOF(bbase) do begin
    dbReadN(bbase,bb_haltezeit,hzeit);
    hzahl:=odd(dbReadInt(bbase,'flags'));
    disp := dbReadNStr(bbase,bb_brettname);
    if (disp <> '') and (LeftStr(disp,3)<>'$/T') then
    begin
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(mid(disp,2),40));
      testdel(mbrettd(disp[1],bbase));
    end;
    dbSkip(bbase,1);
    end;
  dbSetIndex(ubase,uiAdrBuch);
  dbSeek(ubase,uiAdrbuch,#1);
  while not brk and not dbEOF(ubase) do begin
    if dbReadInt(ubase,'userflags') and 4=0 then begin   { keine Verteiler }
      dbReadN(ubase,ub_haltezeit,hzeit);
      hzahl:=false;
      disp := dbReadNStr(ubase,ub_username);
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(disp,40));
      testdel(mbrettd('U',ubase));
      end;
    dbSkip(ubase,1);
    end;
  dbSetIndex(bbase,bi);
  if repair then begin
    closebox;
    if markanz=0 then
      hinweis(getres2(448,7))   { 'keine fehlerhaften Nachrichten gefunden' }
    else
      select(11);
    aufbau:=true;
    end
  else begin
    if not brk then
      if _del then
        wkey(1,false)
      else begin
        signal;
        attrtxt(col.colmbox);
        mwrt(x+2,y+8,' '+getres(12){+' '#8});
        wait(curon);
        end;
    closebox;
    end;
  freeres;
  aufbau:=true;
end;


{ Alle Nachrichten mit halteflags=2 lîschen; Puffer Åberarbeiten }
{ Nachrichten mit defekter Grî·e werden auf 'lîschen' gesetzt    }

procedure MsgReorg;
const tmp     = 'REORG.$$$';
      maxbufs = 20;
var x,y,yy  : Integer;
    lastproz: byte;
    abl     : byte;
    ablage  : byte;
    n,count : longint;
    f1,f2   : file;
    f1s     : longint;
    hdfree  : int64;
    hflags  : byte;
    uflags  : byte;
    p       : pointer;
    bsize   : Integer;
    newadr  : longint;
    reo     : boolean;
    bufa    : array[1..maxbufs] of record
                                     bp   : pointer;
                                     size : word;
                                   end;
    bufs    : byte;
    break   : boolean;
    mi      : word;
    voll    : boolean;        { kein Platz fÅr eine ode mehrere Abl. }
    errflag : boolean;

  procedure test_killed;
  var f : file of boolean;
      b : boolean;
      i : byte;
  begin
    assign(f,killedDat);
    if existf(f) then begin
      reset(f);
      i:=0;
      while not eof(f) and (i<ablagen) do
      begin
        read(f,b);
        if b then reobuf[i]:=true;
        inc(i);
        end;
      close(f);
      end;
  end;

  procedure flushbufs;
  var i : byte;
  begin
    for i:=1 to bufs do
      with bufa[i] do begin
        blockwrite(f2,bp^,size);
        freemem(bp,size);
        end;
    bufs:=0;
  end;

  procedure movemsg;
  var adr,size : longint;
      rr       : Integer;
      mid      : string;
      domove   : boolean;
      rec : longint;
      b   : byte;
      mpos     : longint;

    function MsgOK:boolean;  { Test, ob Xgepostete Msg schon verschoben wurde }
    var fs  : longint;
        hdp : THeader;
        hds : longint;
        ok  : boolean;
        enr : shortint;
    begin
      flushbufs;
      fs:=filesize(f2);
      if adr+size>fs then
        MsgOK:=false
      else begin
        seek(f2,adr);
        enr:=dbReadInt(mbase,'netztyp') shr 24;
        hdp := THeader.Create;
        MakeHeader(true,f2,enr,hds,hdp,ok,true, true);
        MsgOK:=ok and (size=hds+hdp.groesse) and (hdp.Empfaenger.Count>=min(2,enr))
                  and (dbReadInt(mbase,'groesse')=hdp.groesse);
        Hdp.Free;
        seek(f2,fs);
        end;
    end;

  begin
    dbReadN(mbase,mb_adresse,adr);
    dbReadN(mbase,mb_msgsize,size);
    if (size<0) or (adr<0) or (MaxInt-adr<size) or (adr+size>f1s) then begin        { Nachricht defekt }
      dbDelete(mbase);
      exit;
      end;
    dbReadN(mbase,mb_gelesen,b);
    if b>1 then begin           { fehlerhaftes gelesen-Flag korrigieren }
      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      end;
    domove:=true;
    if dbReadInt(mbase,'netztyp') shr 24>0 then begin   { CrossPosting }
      rec:=dbRecno(mbase);
      mid:=LeftStr(dbReadStrN(mbase,mb_msgid),4);
      domove:=not MsgOk;
      if domove then begin
        dbSeek(bezbase,beiMsgid,mid);
        if dbFound then begin
          while not dbEOF(bezbase) and (dbLongStr(dbReadIntN(bezbase,bezb_msgid))=mid)
          do begin
            mpos:=dbReadIntN(bezbase,bezb_msgpos);
            if (mpos<>rec) and not dbDeleted(mbase,mpos) then begin
              dbGo(mbase,mpos);
              if (dbReadInt(mbase,'adresse')=adr) and
                 (dbReadInt(mbase,'ablage')=abl) and
                 (dbReadInt(mbase,'msgsize')=size) then
                dbWriteN(mbase,mb_adresse,newadr)
              end;
            dbNext(bezbase);
            end;
          dbGo(mbase,rec);
          end;
        end;   { of domove }
      end;   { of CrossPosting }

    if domove then begin
      dbWriteN(mbase,mb_adresse,newadr);
      seek(f1,adr);
      if size>bsize then
      begin
        flushbufs;
        while size>0 do begin
          blockread(f1,p^,min(bsize,size),rr);
          blockwrite(f2,p^,rr);
          dec(size,rr);
          end
        end
      else begin
        inc(bufs);                       { Nachricht in nÑchsten Puffer lesen }
        bufa[bufs].size:=size;
        getmem(bufa[bufs].bp,size);
        blockread(f1,bufa[bufs].bp^,size,rr);
        end;
      if bufs=maxbufs then
        flushbufs;
      inc(newadr,dbReadInt(mbase,'msgsize'));
      end;
    dbSkip(mbase,1);
  end;

  procedure show;
  var
    proz : byte;
  begin
    attrtxt(col.colmbox);
    proz:=n*100 div count;
    if proz<>lastproz then
    begin
      moff;
      FWrt(WhereX, WhereY, Format('%3d', [min(proz,100)]));
      mon;
      lastproz:=proz;
    end;
    if not break then begin
      testbrk(break);
      if break then begin
        savecursor;
        break:=ReadJN(getres(449),true);   { 'Abbruch nach Ablagenende' }
        if break then begin
          attrtxt(col.colmbox);
          mwrt(x+2,y+14,getres(450));      { ' Abbruch bei Ablagenende.' }
          end;
        restcursor;
        end;
      end;
  end;

  procedure clkilled(ablage:byte);
  var f   : file of boolean;
      b: boolean;
  begin
    if FileExists(KilledDat) then begin
      assign(f,KilledDat);
      reset(f);
      while filesize(f)<ablage do begin
        b:=false;
        seek(f,filesize(f));
        write(f,b);
      end;
      seek(f,ablage);
      b:=false;
      write(f,b);
      close(f);
      end;
  end;

  procedure clw;
  begin
    clwin(x+1,x+48,y+1,y+13);
    yy:=y+1;
  end;

  function reobufs:boolean;
  var i : integer;
      b : boolean;
  begin
    b:=reobuf[0];
    for i:=1 to ablagen-1 do
      b:=b or reobuf[i];
    reobufs:=b;
  end;

  { Zugriff fÅr /T/S/XPOINT und /Z/ALT/S/XPOINT setzen }

  procedure Rerout(znetz:boolean; brett:string; sperre:boolean);
  var b        : byte;
      newbrett : string;
  begin
    if znetz then
      dbSeek(bbase,biBrett,'A/Z-NETZ/ALT/SUPPORT/XPOINT/'+UpperCase(brett))
    else
      dbSeek(bbase,biBrett,'A/T-NETZ/SUPPORT/XPOINT/'+UpperCase(brett));
    if dbFound then begin
      if sperre then begin
        b:=dbReadInt(bbase,'flags') and (not 8)+8;
        dbWriteN(bbase,bb_flags,b);
        end;
      if znetz then
        newbrett:='/Z-NETZ/ALT/SUPPORT/XPOINT/ALLGEMEINES'
      else
        newbrett:='/T-NETZ/SUPPORT/XPOINT/ALLGEMEINES';
      dbWriteNStr(bbase,bb_adresse,newbrett);
      end;
  end;

begin
  CloseAblage;
  msgbox(50,15,getres2(451,1),x,y);    { 'Reorganisation' }
  bsize:=65536;
  getmem(p,bsize);
  test_killed;
  break:=false;
  DisableDos:=true;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,0);     { ohne Indexreihenfolge! }
  repeat
    hdfree:=DiskFree(0);
    clw;
    abl:=0;
    bufs:=0;
    voll:=false;
    reo:=false;
    while (abl<ablagen) and not break do begin
      if reobuf[abl] then begin
        inc(yy);
        attrtxt(col.colmbox);
        mwrt(x+3,yy,getreps2(451,2,strs(abl)));   { 'Ablage Nr. %s }
        if hdfree<bufsiz[abl]+50000 then begin
          moff;
          write(getres2(451,3));    { ' - kein Platz!!!' }
          mon;
          errsound;
          logerror(getreps2(451,4,strs(abl)));   { 'zu wenig Platz, um Ablage '+strs(abl)+' zu reorganisieren' }
          voll:=true;
          end
        else begin
          assign(f1,aFile(abl));
          if not FileExists(aFile(abl)) then begin
            savecursor;
            trfehler1(421,UpperCase(aFile(abl)),30);   { 'Warnung: Ablagendatei %s fehlt!' }
            restcursor;
            rewrite(f1,1);
            close(f1);
            end;
          moff;
          Wrt2(getres2(451,5));
          GotoXY(WhereX-5, WhereY); { ' packen ...     %' }
          mon;
          lastproz:=101;
          reset(f1,1);
          f1s:=filesize(f1);
          assign(f2,FileUpperCase(tmp));  { mu· wg. rename jedesmal neu assigned werden! }
          rewrite(f2,1);
          newadr:=0;
          dbGoTop(mbase);
          count:=dbRecCount(mbase);
          n:=1;
          errflag:=false;
          while not dbEOF(mbase) do
          begin
            if n>count then
              errflag:=true;
            if n mod 10=0 then show;
            dbReadN(mbase,mb_ablage,ablage);
            if ablage=abl then
            begin
              dbReadN(mbase,mb_halteflags,hflags);
              dbReadN(mbase,mb_unversandt,uflags);
              if (hflags<>2) or odd(uflags) then
                movemsg           { impliziert Skip oder Delete }
              else begin
                DelBezug;
                dbDelete(mbase);
              end;
            end
            else
              if ablage>=ablagen then
                dbDelete(mbase)
              else
                dbSkip(mbase,1);
            inc(n);
          end;
          if n<=count then errflag:=true;
          flushbufs;
          if count>0 then show;
          close(f2);
          close(f1); erase(f1);
          rename(f2,aFile(abl));
          FlushClose;
          if errflag then begin
            trfehler(445,30);   { 'Nachrichtendatenbank fehlerhaft - verwenden Sie /Wartung/Packen!' }
            break:=true;
            end;
          clkilled(abl);
          reobuf[abl]:=false;
          reo:=true;
          end;
        end;
      inc(abl);
      if abl=10 then clw;
      end;
  until break or not (voll and reo and reobufs);
  dbSetIndex(mbase,mi);
  if reo then
  begin
    Wrt(x+3,yy+2,getres2(451,6));
    GotoXY(WhereX-5, WhereY);   { 'Einen Moment noch...     %' }
    brettdatumsetzen(true);
  end else
    mwrt(x+3,yy+2,getres2(451,7));   { 'nix zu lîschen' }
  DisableDOS:=false;
  signal;
  Rerout(false,'MELDUNGEN',true);
  Rerout(false,'UPDATES',true);
  Rerout(true,'MELDUNGEN',true);
  Rerout(true,'UPDATES',true);
  SysDelay(500);
  closebox;
  freeres;
  wrtiming('REORG');
  freemem(p,bsize);
  markanz:=0;
  aufbau:=true; xaufbau:=true;
end;

procedure ModiEmpfDatum;
var d   : datetimest;
    brk : boolean;
   getdate: boolean;
    l   : longint;
begin
  getdate:=true;
  d:=longdat(dbReadInt(mbase,'empfdatum'));
  EditDate(15,11+(screenlines-25)div 2,getres2(452,1),d,getdate,brk);   { 'neues Empfangsdatum:' }
  if not brk then begin
    if getdate then d:=longdat(dbreadint(mbase,'origdatum'));
    l:=ixdat(d);
    dbWriteN(mbase,mb_empfdatum,l);
    aufbau:=true;
    end;
end;


function testuvs(const txt:atext):boolean;
var uvs : byte;
begin
  dbReadN(mbase,mb_unversandt,uvs);
  if uvs and 1<>0 then rfehler1(422,txt);  { 'Bitte verwenden Sie Nachricht/Unversandt/%s' }
  testuvs:=(uvs and 1<>0);
end;


procedure ModiBetreff;
var brk  : boolean;
    hdp  : THeader;
    hds  : longint;
    x,y  : Integer;
    fn   : string;
    f    : file;
begin
  if testuvs(getres(453)) then exit;   { 'Aendern' }
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  if hds>1 then begin
    diabox(63,5,'',x,y);
    readstring(x+3,y+2,getres(454),hdp.betreff,40,BetreffLen,'',brk);  { 'neuer Betreff:' }
    closebox;
    if not brk then begin
      PGP_BeginSavekey;
      fn:=TempS(dbReadInt(mbase,'msgsize')+100);
      assign(f,fn);
      rewrite(f,1);
      { ClearPGPflags(hdp); }
      hdp.orgdate:=true;
      WriteHeader(hdp,f);
      XreadF(hds,f);   { den Nachrichtentext anhaengen ... }
      close(f);
      Xwrite(fn);
      erase(f);
      wrkilled;
      TruncStr(hdp.betreff,40);
      dbWriteNStr(mbase,mb_betreff,hdp.betreff);
      PGP_EndSavekey;
      aufbau:=true;
      xaufbau:=true;
      end;
    end;
  Hdp.Free;
end;


procedure ModiHighlite;
var l : longint;
begin
  dbReadN(mbase,mb_netztyp,l);
  l:=l xor $1000;
  dbWriteN(mbase,mb_netztyp,l);
  aufbau:=true;
end;


procedure ModiText;
var fn   : string;
    fn2  : string;
    f,f2 : file;
    hdp  : Theader;
    hds  : longint;
    typ  : char;
    l    : longint;
begin
  dbReadN(mbase,mb_typ,typ);
  if (typ='B') or (typ='M') then begin
    rfehler(423);   { 'Bei Binaerdateien nicht moeglich' }
    exit;
  end;
  if testuvs(getres(455)) then exit;   { 'Edit' }
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);           { Heder einlesen }
  if hds>1 then begin
    PGP_BeginSavekey;
    fn:=TempS(dbReadInt(mbase,'msgsize'));
    assign(f,fn);
    rewrite(f,1);
    XReadIsoDecode:=true;
    XreadF(hds,f);                { Nachrichtentext in Tempfile.. }
    close(f);
    editfile(fn,true,false,false,0,false);    { ..editieren.. }
    fn2:=TempS(_filesize(fn)+2000);
    assign(f2,fn2);
    rewrite(f2,1);
    hdp.groesse:=_filesize(fn);
    dbWriteN(mbase,mb_groesse,hdp.groesse);
    hdp.charset:='';
    { ClearPGPflags(hdp); }
    hdp.orgdate:=true;
    WriteHeader(hdp,f2);          { ..Header in neues Tempfile.. }
    reset(f,1);
    fmove(f,f2);                  { ..den Text dranhaengen.. }
    close(f); erase(f);
    close(f2);
    dbReadN(mbase,mb_netztyp,l);
    l:=l and (not $2000);         { ISO-Codierung abschalten }
    dbWriteN(mbase,mb_netztyp,l);
    Xwrite(fn2);                  { ..und ab in die Datenbank. }
    erase(f2);
    wrkilled;
    PGP_EndSavekey;
    aufbau:=true;                 { wg. geaenderter Groesse }
    end;
   Hdp.Free;
end;


procedure ModiRot13;
var ablg   : byte;
    adr    : longint;
    f      : file;
    l,size : longint;
    p      : pointer;
    ps  : Integer;
    rr: Integer;
    typ    : char;
begin
  dbReadN(mbase,mb_typ,typ);
  if typ='B' then begin
    rfehler(423);   { 'Bei Binaerdateien nicht moeglich' }
    exit;
    end;
  if testuvs(getres(453)) then exit;   { 'Aendern' }
  dbReadN(mbase,mb_ablage,ablg);
  dbReadN(mbase,mb_adresse,adr);
  dbReadN(mbase,mb_groesse,size);
  assign(f,aFile(ablg));
  reset(f,1);
  if (size=0) or (adr+size>filesize(f)) then begin
    rfehler1(424,strs(ablg));   { 'Nachricht ist beschaedigt  (Ablage %s)' }
    close(f);
    end
  else
  begin
    ps:=32768;
    getmem(p,ps);
    seek(f,adr+dbReadInt(mbase,'msgsize')-size);
    repeat
      l:=filepos(f);
      blockread(f,p^,min(ps,size),rr);
      Rot13(p^,rr);
      seek(f,l);
      blockwrite(f,p^,rr);
      dec(size,rr);
    until size=0;
    close(f);
    freemem(p,ps);
    end;
end;


procedure ModiTyp;
var c   : char;
    uvs : byte;
    flags:longint;
begin
  dbReadN(mbase,mb_unversandt,uvs);
  if uvs and 1<>0 then
    rfehler(425)   { 'Bei unversandten Nachrichten leider nicht moeglich.' }
  else begin
    dbReadN(mbase,mb_typ,c);
    dbReadN(mbase,mb_flags,flags);

    if (c='T') and ((flags and 4)<>0) then c:='M';

    case c of
      'T': c:='B';
      'B': c:='M';
      else c:='T';
    end;

    if c='M' then flags := flags or 4 else
                  flags := flags and not 4;

    if c='M' then c:='T';
                  
    dbWriteN(mbase,mb_flags,flags);
    dbWriteN(mbase,mb_typ,c);
    aufbau:=true;
    end;
end;

procedure ModiGelesen;                    {Nachricht-Gelesen status aendern}
var b     : byte;
    brett : string;
begin
  if not dbBOF(mbase) then
  begin                                   {Nur Wenn ueberhaupt ne Nachricht gewaehlt ist...}
    dbReadN(mbase,mb_gelesen,b);
    if b=1 then b:=0 else b:=1;
    dbWriteN(mbase,mb_gelesen,b);
    Brett := dbReadNStr(mbase,mb_brett);
    if b=1 then begin
      dbSeek(mbase,miGelesen,brett+#0);
      if dbEOF(mbase) or (dbReadStrN(mbase,mb_brett)<>brett) or (dbReadInt(mbase,'gelesen')<>0)
      then b:=0   { keine ungelesenen Nachrichten mehr im Brett vorhanden }
      else b:=2;
      end
    else
      b:=2;        { noch ungelesene Nachrichten im Brett vorhanden }
    dbSeek(bbase,biIntnr,mid(brett,2));
    if dbFound then begin
      b:=dbReadInt(bbase,'flags') and (not 2) + b;
      dbWriteN(bbase,bb_flags,b);
      end;
    aufbau:=true;
    end;
end;


{ Brettliste importieren }

procedure ImportBrettliste;
var fn  : string;
    s   : string;
    t   : text;
    x,y : Integer;
    n   : longint;
    useclip: boolean;
begin
  fn:= WildCard;
  useclip:=true;
  if ReadFilename(getres2(456,1),fn,true,useclip) then   { 'Brettliste einlesen' }
    if not FileExists(fn) then
      fehler(getres2(456,2))   { 'Datei nicht vorhanden!' }
    else begin
      msgbox(30,5,'',x,y);
      wrt(x+3,y+2,getres2(456,3));   { 'Bretter anlegen ...' }
      n:=0;
      assign(t,fn);
      reset(t);
      while not eof(t) do
      begin
        readln(t,s);
        makebrett(s,n,DefaultBox,ntBoxNetztyp(DefaultBox),true);
        Wrt(x+22,y+2, Format('%5d', [n]));
      end;
      close(t);
      closebox;
      aufbau:=true;
      dbFlushClose(bbase);
      if useclip then _era(fn);
      end;
  freeres;
end;


{ Userliste importieren }

procedure ImportUserliste;
var fn  : string;
    adrb: boolean;
    brk : boolean;
    s   : string;
    t   : text;
    x,y : Integer;
    n   : longint;
    useclip: boolean;
    b   : byte;
begin
  fn:= WildCard;
  useclip:=true;
  if ReadFilename(getres2(456,11),fn,true,useclip) then   { 'Userliste einlesen' }
    if not FileExists(fn) then
      fehler(getres2(456,2))   { 'Datei nicht vorhanden!' }
    else begin
      dialog(38,3,'',x,y);
      adrb:=true;
      maddbool(3,2,getres2(456,12),adrb);   { 'User in Adressbuch eintragen' }
      readmask(brk);
      enddialog;
      if brk then exit;
      msgbox(34,5,'',x,y);
      wrt(x+3,y+2,getres2(456,13));   { 'Userbretter anlegen ...' }
      n:=0;
      assign(t,fn);
      reset(t);
      b:=0;
      while not eof(t) do begin
        readln(t,s);
        s:=trim(s);
        if cpos('@',s)>0 then begin
          dbSeek(ubase,uiName,UpperCase(s));
          if not dbFound then begin
            inc(n);
            AddNewUser(s,DefaultBox);
            if not adrb then dbWriteN(ubase,ub_adrbuch,b);
            Wrt(x+26, y+2, Format('%5d', [n]));
          end;
        end;
      end;
      close(t);
      wkey(1,false);
      closebox;
      aufbau:=true;
      dbFlushClose(ubase);
      if useclip then _era(fn);
      end;
  freeres;
end;


{ User-/Brettliste exportieren }

function Bool_BrettGruppe(var s:string):boolean;
begin
  if s=_jn_[2] then exit;
  setfield(2,_jn_[2]);
end;

function Bool_Brettindex(var s:string):boolean;
begin
  if s=_jn_[2] then exit;
  setfield(1,_jn_[2]);
end;

procedure ExportUB(user:boolean);
var fname   : String;
    t       : text;
    d       : DB;
    x,y,xx  : Integer;
    cnt,n   : longint;
    exkom   : boolean;
    brk     : boolean;
    useclip : boolean;
    onlyadress : boolean;
    sortadress : boolean;
    sortbox    : boolean;
    sort      : byte;
    ab,ab1     : longint;
    dbindex    : word;
    s          : string[80];
    sa,sa1     : string [20];

label ende;

  function komform(d:DB; s:string):string;
  var kom : string;
  begin
    kom:= dbReadStr(d,'kommentar');
    if exkom and (kom<>'') then
      komform:=forms(s,80)+kom
    else
      komform:=s;
  end;


  procedure getbrettinfos;
  var x1,y1 : Integer;
  begin
    sortadress:=false; sortbox:=Usersortbox;  exkom:=falsE;
    dialog(50,5,getres2(457,2),x1,y1);        { 'Brettliste erzeugen' }
    maddbool(2,2,getres2(457,7),SortAdress);  { 'nach Gruppen sortieren'}
    MSet1Func(bool_brettgruppe);
    maddbool(2,3,getres2(457,10),SortBox);    { 'Sortierung aus Bret¸bersicht beibehalten'}
    MSet1Func(bool_brettindex);
    maddbool(2,5,getres2(457,4),exkom);       { 'auch Kommentare exportieren' }
    readmask(brk);
    enddialog;
  end;

  procedure getuserinfos;
  var x1,y1 : Integer;
  begin
    sortadress:=true; sortbox:=Usersortbox; Onlyadress:=true; exkom:=falsE;
    dialog(43,6,getres2(457,1),x1,y1);        { 'Userliste erzeugen' }
    maddbool(2,2,getres2(457,7),SortAdress);  { 'nach Adressbuchgruppen sortieren'}
    maddbool(2,3,getres2(457,8),SortBox);     { 'nach Serverbox sortieren'}
    maddbool(2,4,getres2(457,9),OnlyAdress);  { 'Nur User im Adressbuch exportieren' }
    maddbool(2,6,getres2(457,4),exkom);       { 'auch Kommentare exportieren' }
    readmask(brk);
    enddialog;
  end;
  
begin
  fname:='';
  useclip:=true;
  if ReadFilename(getres2(457,iif(user,1,2)),fname,true,useclip)
  then
    if not ValidFileName(fname) then
      fehler(getres2(457,3))   { 'ungueltiger Dateiname' }
    else begin
      if user then getuserinfos 
        else getbrettinfos;
      if brk then begin
        if useclip then _era(fname);
        goto ende;
      end;   
      sort:=0;
      if sortadress then sort:=sort or 2;
      if sortbox then sort:=sort or 1;
      if user then begin
        dbindex:=dbgetindex(ubase);
        case Sort of
          1 : dbSetIndex(ubase,uiBoxName);
          2 : dbSetIndex(ubase,uiAdrbuch);
          3 : dbSetIndex(ubase,uiBoxAdrbuch);
         else dbSetIndex(ubase,uiName);
         end;
        d:=ubase;
        dbGoTop(ubase);
      end
      else begin
        dbindex:=dbgetindex(mbase);
        case sort of
          2 : begin
                dbsetindex(bbase,biGruppe);
                dbgotop(bbase);
                end;
          1 : begin
                dbsetindex(bbase,biIndex);
                dbgotop(bbase);
                end;
         else begin
                dbsetindex(bbase,biBrett);
                dbSeek(bbase,biBrett,'A');
                end;
         end;
        d:=bbase;
        end;
      msgbox(34,5,'',x,y);
      wrt(x+3,y+2,getres2(457,iif(user,5,6)));  { 'erzeuge User/Brettliste...     %' }
      xx:=wherex-5;
      if not multipos(':\',fname) then fname:=ExtractPath+fname;
      assign(t,fname);
      rewrite(t);
      cnt:=dbRecCount(d); n:=0; ab1:=-1; sa1:='';
      while not dbEOF(d) do
      begin
        attrtxt(col.colmboxhigh);

        Wrt(xx, y+2, Format('%3d', [n*100 div cnt]));
        if user then
        begin
          s:=dbReadStrN(ubase,ub_username);
          ab:=dbreadint(ubase,'adrbuch');
          sa:=dbReadStrN(ubase,ub_pollbox);
          if (dbReadInt(ubase,'userflags') and 4=0) AND       { keine Verteiler }
           (LeftStr(s,4)<>#0+'$/T') AND                          { keine Trennzeile }
          not (onlyadress and (ab=0))                         { Evtl. nur Adressbuch-User }
          then begin
            if sortadress and (ab<>ab1) then begin
              ab1:=ab;
              writeln(t,sp(60),'('+Getres2(2715,11),ab,')');  {Gruppe}
              end;
            if (sort=1) and (sa<>sa1) then begin
              sa1:=sa;
              writeln(t,sp(60),sa);
              end;
            writeln(t,komform(ubase,s))
            end
          end
        else begin
          if sort=2 then ab:=dbreadint(bbase,'gruppe')
          else ab:=-1;
          s:=dbReadStrN(bbase,bb_brettname);
          if LeftStr(s,3)='$/T'           { keine Trennzeile }
          then begin
           if sort=1 then writeln(t,dup(40,s[4]));
           end
          else begin
            if ab1<>ab then begin
              ab1:=ab;
              writeln(t,sp(60),'('+Getres2(2715,11),ab,')');  {Gruppe}
              end;
            writeln(t,komform(bbase,copy(s,2,80)));
            end;
          end;
        dbNext(d);
        inc(n);
        end;
      close(t);
      dbsetindex(d,dbindex); 
      if useclip then WriteClipfile(fname);
      closebox;
      end;
ende:
  freeres;
end;


// N/U/Z
procedure zeige_unversandt;
var _brett   : string;
    _mbrett  : string;
    sr       : tsearchrec;
    rc       : integer;
    f        : file;
    hdp      : Theader;
    hds      : longint;
    ok       : boolean;
    adr,fsize: longint;
    box      : string;
    uvf      : boolean;
    uvs      : byte;
    mtyp     : char;
    ntyp     : longint;
    zconnect : boolean;
    crashs   : boolean;

begin
  if uvs_active then exit;
  crashs:=false;
  // temporary debug log
  Debug.DebugLog('xp4o','Searching for '+GetCurrentDir+'/*'+extBoxFile,dlDebug);
  rc:= findfirst('*'+extBoxFile,faArchive,sr);
  if rc<>0 then
  begin
    FindClose(sr);
    rc:= findfirst('*.cp',faArchive,sr);
    crashs:=true;
  end;
  markanz:=0;
  moment;
  hdp := THeader.Create;
  while rc=0 do begin
    if crashs then begin
      box:=strs(hexval(LeftStr(sr.name,4)))+'/'+strs(hexval(copy(sr.name,5,4)));
      { ^^^ nur fuer Anzeige bei fehlerhaftem CP }
      zconnect:=true;
      end
    else begin
      box:=file_box(nil,LeftStr(sr.name,cPos('.',sr.name)-1));
      zconnect:=ntZConnect(ntBoxNetztyp(box));
      end;
    dbSetIndex(mbase,miBrett);
    assign(f,ownpath+sr.name);
    reset(f,1);
    adr:=0;
    fsize:=filesize(f);
    ok:=true;
    while ok and (adr<fsize) do begin
      seek(f,adr);
      makeheader(zconnect,f,1,hds,hdp,ok,false, true);
      if not ok then
        rfehler1(427,box)   { 'fehlerhaftes Pollpaket:  %s' }
      else with hdp do begin
        _brett:='';
        if (cpos('@',Firstempfaenger)=0) and
           ((netztyp<>nt_Netcall) or (FirstChar(FirstEmpfaenger)='/'))
        then begin
          dbSeek(bbase,biBrett,'A'+UpperCase(Firstempfaenger));
          if not dbFound then rfehler(426)   { 'Nachricht ist nicht mehr in der Datenbank vorhanden!' }
          else _brett:='A'+dbLongStr(dbReadInt(bbase,'int_nr'));
          end
        else begin
          dbSeek(ubase,uiName,UpperCase(Firstempfaenger+
                 iifs(cPos('@',Firstempfaenger)>0,'','@'+box+'.ZER')));
          if not dbFound then rfehler(426)   { 'Nachricht ist nicht mehr in der Datenbank vorhanden!' }
          else _brett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
          end;
        if _brett<>'' then begin
          dbSeek(mbase,miBrett,_brett+#255);
          uvf:=false;
          if dbEOF(mbase) then dbGoEnd(mbase)
          else dbSkip(mbase,-1);
          if not dbEOF(mbase) and not dbBOF(mbase) then
            repeat
              _MBrett := dbReadNStr(mbase,mb_brett);
              if _mbrett=_brett then begin
                dbReadN(mbase,mb_unversandt,uvs);
                dbReadN(mbase,mb_typ,mtyp);
                dbReadN(mbase,mb_netztyp,ntyp);
                if (uvs and 1=1) and EQ_betreff(betreff)  and ((mtyp='B') or
                   ((uvs and 4 <> 0) or (ntyp and $4000<>0) or  { codiert / signiert }
                   (groesse=dbReadInt(mbase,'groesse'))))
                   and (FormMsgid(msgid)=dbReadStrN(mbase,mb_msgid))
                   and not msgmarked then
                begin
                  MsgAddmark;
                  uvf:=true;
                  end;
                end;
              dbSkip(mbase,-1);
            until uvf or dbBOF(mbase) or (_brett<>_mbrett);
          if not uvf then
            rfehler(426);   { 'Nachricht ist nicht mehr in der Datenbank vorhanden!' }
          end;
        inc(adr,groesse+hds);
        end;
      end;
    close(f);
    rc:= findnext(sr);
    if (rc<>0) and not crashs then
    begin
      FindClose(sr);
      rc:= findfirst('*.cp',faArchive,sr);
      crashs:=true;
    end;
  end;
  FindClose(sr);
  Hdp.Free;
  closebox;
  if markanz=0 then
    hinweis(getres(458))   { 'Keine unversandten Nachrichten vorhanden!' }
  else begin
    MarkUnversandt:=true;
    uvs_active:=true;
    select(11);
    uvs_active:=false;
    end;
  aufbau:=true;
end;


procedure msg_info;     { Zerberus-Header anzeigen }
var hdp   : Theader;
    hds   : longint;
    i     : integer;
    x,y  : Integer;
    dat   : datetimest;
    anz   : byte;
    xxs   : array[1..20] of string;
    netz  : string;
    p     : byte;
    elist : boolean;    { mehrere Empfaenger }
    rlist : boolean;    { mehrere References }
    spami : boolean;    { hat SPAM-Informationen }
    t     : taste;
    s     : atext;

  procedure apps(nr:word; s:string);
  begin
    inc(anz);
    xxs[anz]:=getres2(459,nr)+' '+LeftStr(s,53);
  end;

  function ddat:string;
  begin
    with hdp do
      if ddatum='' then
        ddat:=''
      else
        ddat:=', '+copy(ddatum,7,2)+'.'+copy(ddatum,5,2)+'.'+LeftStr(ddatum,4)+
              ', '+copy(ddatum,9,2)+':'+copy(ddatum,11,2)+':'+copy(ddatum,13,2);
  end;

  procedure empfliste;   { Fenster mit Empfaengerliste }
  var ml  : byte;
      i,j : integer;
      x,y : Integer;
  begin
    ml:=length(getres2(459,30))+8;
    with hdp do begin
      for i:=1 to Empfaenger.Count do
      begin
        ReadHeadEmpf:=i;
        ReadHeader(hdp,hds,false);
        ml:=max(ml,length(Firstempfaenger)+6);
      end;
      ml:=min(ml,72);
      i:=min(Empfaenger.Count,screenlines-8);
      msgbox(ml,i+4,getres2(459,30),x,y);   { 'Empfaengerliste' }
      for j:=1 to i do begin
        ReadHeadEmpf:=j;
        ReadHeader(hdp,hds,false);
        mwrt(x+3,y+1+j,LeftStr(Firstempfaenger,72));
        end;
      wait(curoff);
      if rlist and (UpperCase(lastkey)='R') then keyboard('R');
{$IFDEF Debug}
      if (UpperCase(lastkey)='D') then keyboard('D');
{$ENDIF}
      closebox;
      end;
  end;

  procedure refliste;   { Fenster mit Referenzliste }
  var
    ml, i: integer;
    x, y: Integer;
  begin
    ml:=length(getres2(459,31))+8;
    with hdp do
    begin
      for i := 0 to References.Count - 1 do
        ml:=max(ml,length(References[i])+6);
      ml:=min(ml,72);
      i:=min(References.Count,screenlines-8);
      msgbox(ml,i+4,getres2(459,31),x,y);   { 'Empfaengerliste' }
      for i := 0 to References.Count - 1 do
        wrt(x+3,y+2+i,LeftStr(References[i],72));
      wait(curoff);
      if elist and (UpperCase(lastkey)='E') then keyboard('E');
{$IFDEF Debug}
      if (UpperCase(lastkey)='D') then keyboard('D');
{$ENDIF}
      closebox;
      end;
  end;

  procedure spam_info;
  var
      i    : integer;
      x,y  : Integer;
      s    : TRopeStream;
      stat : TSpamStats;
  begin
    msgbox(34,5+
      (High(stat.MostInteresting) - Low(stat.MostInteresting))+1+
      (High(stat.LeastInteresting) - Low(stat.LeastInteresting))+1,
      GetRes2(459,70),x,y);
    
    s := TRopeStream.Create;
    try
      XReadS(0,s);
      s.Seek(0,soFromBeginning);
      calc_message_spamicity(s,stat);      
    finally
      s.Free;
    end;

    inc(x,2);
    inc(y);
    
    wrt(x,y,Format(GetRes2(459,72),[Stat.Spamicity*100]));
    
    inc(y,2);
    for i:=Low(stat.MostInteresting) to High(stat.MostInteresting) do
    begin
      wrt(x,y,Format(GetRes2(459,73),[
        stat.MostInteresting[i].Word,
        stat.MostInteresting[i].GoodCount,
        stat.MostInteresting[i].BadCount,
        (1-stat.MostInteresting[i].Prob)*100]));
      inc(y);
    end;

    inc(y,1);
    for i:=Low(stat.LeastInteresting) to High(stat.LeastInteresting) do
    begin
      wrt(x,y,Format(GetRes2(459,73),[
        stat.LeastInteresting[i].Word,
        stat.LeastInteresting[i].GoodCount,
        stat.LeastInteresting[i].BadCount,
        (1-stat.LeastInteresting[i].Prob)*100]));
      inc(y);
    end;

    wait(curoff);
    closebox;
  end;

{$IFDEF Debug}
  procedure msgs_tuple;   { Fenster mit Datenbankinfo }
  var 
    x,y: Integer;

    procedure _(id:integer;s:string);
    begin
      attrtxt(col.colmboxhigh); wrt(x,y,getres2(459,id));  { 'Empfangsdatum: ' }
      attrtxt(col.colmbox);     wrt(x+12,y,LeftStr(s,70-15));
      inc(y);
    end;

    function brett(s:string):string;
    begin
      case s[1] of
        '$': result:=GetRes2(459,64);
        '1': result:=GetRes2(459,65);
        'A': result:=GetRes2(459,66);
        'U': result:=GetRes2(459,66);
        else result:=hex(Ord(s[1]),2);
      end;
      result:= StrS((Ord(s[2]) shl 0) or (Ord(s[3]) shl 8) or
        (Ord(s[4]) shl 16) or (Ord(s[5]) shl 24)) + result;
    end;

  begin
    msgbox(70,23,getres2(459,32),x,y);   { 'Empfaengerliste' }
    inc(x,2); inc(y,2);

    _(40,brett(dbReadStrN(mbase,mb_brett)));    { 'Brett-Nr. :' }
    _(41,dbReadStrN(mbase,mb_betreff));         { 'Betreff   :' }
    _(42,dbReadStrN(mbase,mb_absender));        { 'Absender  :' }
    _(43,StrS(dbReadIntN(mbase,mb_origdatum))); { 'OrigDatum :' }
    _(44,StrS(dbReadIntN(mbase,mb_empfdatum))); { 'EmpfDatum :' }
    _(45,StrS(dbReadIntN(mbase,mb_groesse)));   { 'Grî·e     :' }
    _(46,Chr(dbReadIntN(mbase,mb_typ)));        { 'Typ       :' }
    _(47,StrS(dbReadIntN(mbase,mb_HalteFlags)));{ 'HalteFlags:' }
    _(48,StrS(dbReadIntN(mbase,mb_gelesen)));   { 'gelesen   :' }
    _(49,hex(dbReadIntN(mbase,mb_unversandt),4));{'unversandt:' }
    _(50,StrS(dbReadIntN(mbase,mb_ablage)));    { 'Ablage    :' }
    _(51,StrS(dbReadIntN(mbase,mb_adresse)));   { 'Adresse   :' }
    _(52,StrS(dbReadIntN(mbase,mb_MsgSize)));   { 'MsgSize   :' }
    _(53,StrS(dbReadIntN(mbase,mb_WVdatum)));   { 'WVdatum   :' }
    _(54,dbReadStrN(mbase,mb_MsgID));           { 'MsgID     :' }
    _(55,hex(dbReadIntN(mbase,mb_netztyp),4));  { 'Netztyp   :' }
    _(57,dbReadStrN(mbase,mb_Name));            { 'Name      :' }
    _(58,hex(dbReadIntN(mbase,mb_flags),4));    { 'Flags     :' }
    _(59,dbReadStrN(mbase,mb_MimeTyp));         { 'Mimetyp   :' }

    wait(curoff);
    if rlist and (UpperCase(lastkey)='R') then keyboard('R');
    if elist and (UpperCase(lastkey)='E') then keyboard('E');
    closebox;
  end;
{$ENDIF}

  function typstr(typ,mimetyp:string):string;
  begin
    if mimetyp<>'' then
      typstr:=extmimetyp(mimetyp)
    else begin
      UpString(typ);
      if typ='T' then typstr:=typ+getres2(459,1) else   { '  (Text)' }
      if typ='B' then typstr:=typ+getres2(459,2) else   { '  (binaer)' }
      typstr:=typ;
      end;
  end;

begin
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  anz:=0;
  with hdp do begin
    apps(3,Firstempfaenger);
    if fido_to<>'' then apps(4,fido_to);
    apps(5,betreff);
    apps(6,LeftStr(absender,53));
    if realname<>'' then apps(7,realname);
    if organisation<>'' then apps(8,LeftStr(organisation,53));
    if Replyto <> '' then apps(9,LeftStr(replyto,53));
    apps(10,iifs(ntZDatum(netztyp),zdatum,datum)+
         iifs(datum<>'','  ('+fdat(datum)+', '+ftime(datum)+
         iifs(ntSec(netztyp),':'+copy(zdatum,13,2),'')+')',''));
    apps(11,LeftStr(pfad,53));
    repeat
      pfad:=mid(pfad,54);
      if pfad<>'' then apps(12,LeftStr(pfad,53));
    until pfad='';
    if msgid<>''    then apps(13,msgid);
    if References.Count > 0 then apps(14, References[References.Count-1]);
    if pm_bstat<>'' then apps(15,pm_bstat);
    apps(16,typstr(typ,mime.ctype));
    if programm<>'' then apps(17,programm);
    if datei<>''    then apps(18,datei+ddat);
    apps(19,strs(groesse)+getres(13));
    if komlen>0 then apps(21,strs(komlen)+getres(13));
    if attrib<>0 then
      apps(22,hex(attrib,4)+' - '+
           iifs(attrib and attrCrash<>0,'Crash ','')+
           iifs(attrib and attrFile<>0,'File ','')+
           iifs(attrib and attrReqEB<>0,'Req-EB ','')+
           iifs(attrib and attrIsEB<>0,'EB ','')+
           iifs(attrib and attrPmReply<>0,'PM-Reply ','')+
           iifs(attrib and attrQuoteTo<>0,'QuoteTo ','')+
           iifs(attrib and attrControl<>0,'Control ',''));
    if netztyp in netsRFC then netz:=' / RFC'
    else begin
      netz:=ntName(netztyp);
      if netz='???' then netz:=''
      else netz:=' / '+netz;
      end;
    msgbox(70,anz+7,getres2(459,23)+' ('+            { 'Nachrichtenkopf' }
                    getres2(459,iif(ntZConnect(netztyp),24,25))+netz+')',x,y);
    moff;
    for i:=1 to anz do begin
      if FirstChar(xxs[i])=' ' then p:=0
      else p:=cpos(':',xxs[i]);
      if p>0 then begin
        attrtxt(col.colmboxhigh);
        wrt(x+3,y+i+1,LeftStr(xxs[i],p));
        end;
      attrtxt(col.colmbox);
      wrt(x+3+p,y+i+1,mid(xxs[i],p+1));
      end;
    attrtxt(col.colmboxhigh); wrt(x+3,y+anz+3,getres2(459,26));  { 'Groesse des Kopfes: ' }
    attrtxt(col.colmbox);     wrt(x+3+length(getres2(459,26)),y+anz+3,IntToStr(hds)+getres(13));
    dat:=longdat(dbReadInt(mbase,'empfdatum'));
    if smdl(IxDat('2712300000'),IxDat(dat)) then
      dat:=longdat(dbReadInt(mbase,'wvdatum'));
    attrtxt(col.colmboxhigh); wrt(x+40,y+anz+2,getres2(459,27));  { 'Empfangsdatum: ' }
    attrtxt(col.colmbox);     wrt(x+40+length(getres2(459,27)),y+anz+2,fdat(dat));
    attrtxt(col.colmboxhigh); wrt(x+40,y+anz+3,getres2(459,28));  { 'Ablagedatei  :' }
    attrtxt(col.colmbox);     wrt(x+40+length(getres2(459,28)),y+anz+3,
                                  FileUpperCase('mpuffer.')+IntToStr(dbReadInt(mbase,'ablage')));
    elist:=(Empfaenger.Count>1);
    rlist:=(References.Count>1);
    spami:=(FirstChar(dbReadStrN(mbase,mb_brett)) in ['1']) or
      (dbReadStrN(bbase,bb_brettname) = '$/ØSpam');
    if elist then s:=' (E='+getres2(459,30)
    else s:='';
    if rlist then begin
      if s<>'' then s:=s+', '
      else s:=' (';
      s:=s+'R='+getres2(459,31);
      end;

    if spami then begin
      if s<>'' then s:=s+', '
      else s:=' (';
      s:=s+'S='+GetRes2(459,70);
      end;

{$IFDEF Debug}
    if s<>'' then s:=s+', '
    else s:=' (';
    s:=s+'D='+getres2(459,32);
{$ENDIF}
    if s<>'' then s:=s+')';
    wrt(x+3,y+anz+5,getres2(459,29)+s+' ...');    { Taste druecken / E=Empfaengerliste / R=Referenzliste }
    mon;
    x:=wherex; y:=wherey;
    repeat
      gotoxy(x,y);
      repeat
        get(t,curon);
      until (t<mausfirstkey) or (t>mauslastkey) or (t=mausleft) or (t=mausright);
      if elist and (UpperCase(t)='E') then empfliste;
      if rlist and (UpperCase(t)='R') then refliste;
      if spami and (UpperCase(t)='S') then spam_info;
{$IFDEF Debug}
      if (UpperCase(t)='D') then msgs_tuple;
{$ENDIF}
    until {$IFDEF Debug} (UpperCase(t)<>'D') and {$ENDIF}
      (not elist or (UpperCase(t)<>'E')) and (not rlist or (UpperCase(t)<>'R'));
    end;
  closebox;
  freeres;
  Hdp.Free;
end;


procedure ShowHeader;            { Header direkt so anzeigen wie er im PUFFER steht }
var fn  : string;
    f   : file;
    hdp : Theader;
    hds : longint;
    lm  : Byte;                  { Makrozwischenspeicher... }
begin
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  if hds>1 then begin
    fn:=TempS(dbReadInt(mbase,'msgsize')+1000);
    assign(f,fn);
    rewrite(f,1);
    XreadF(0,f);
    seek(f,hds);
    truncate(f);
    close(f);
    lm:=listmakros;                                   { Aktuelle Makros merken,       }
    listmakros:=16;                                   { Archivviewermakros aktivieren }
    ListFile(fn,getres(460),true,false,false,0);      { 'Nachrichten-Header' }
    listmakros:=lm;                                   { wieder alte Makros benutzen   }
    _era(fn);
    end;
  Hdp.Free;
end;


{ Es wird nicht im Temp-, sondern im XP-Verzeichnis entpackt! }
{ exdir='' -> Lister/ArcViewer;  exdir<>'' -> Xtrakt          }
{ Fehler -> exdir:=''                                         }

procedure ShowArch(const fn:string);
var decomp : string;
    p      : byte;
    datei  : string;
    newarc : longint;
    atyp   : shortint;
    spath  : string;
    ats    : shortint;
  MessageViewer: TMessageViewer;
begin
  MessageViewer := TMessageViewer.Create;
  ats:=arctyp_save;
  atyp:=abuf[arcbufp].arcer_typ;
  if atyp>arctypes then exit;  { ??? }
  if not getDecomp(atyp,decomp) then
    exdir:=''
  else begin
    p:=pos('$DATEI',UpperCase(decomp));
    datei:=trim(mid(fn, 80));
    if (exdir='') and ((temppath='') or (UpperCase(temppath)=ownpath))
      and FileExists(datei) then begin
        rfehler(428);   { 'extrahieren nicht moeglich - bitte Temp-Verzeichnis angeben!' }
        exit;
        end
    else if ((exdir<>'') and FileExists(exdir+datei)) or
            ((exdir='') and FileExists(temppath+datei)) then
      if exdir=ownpath then begin
        rfehler(429);  { 'Datei schon vorhanden - bitte Extrakt-Verzeichnis angeben!' }
        exit;
        end
      else
        if not ReadJN(getreps(461,fitpath(exdir+datei,40)),false)  { '%s existiert schon. ueberschreiben' }
        then exit
        else
          _era(iifs(exdir<>'',exdir,temppath)+datei);
    spath:=ShellPath;
    if exdir<>'' then SetCurrentDir(exdir)
    else SetCurrentDir(temppath);
    decomp:=copy(decomp,1,p-1)+datei+copy(decomp,p+6,127);
    p:=pos('$ARCHIV',UpperCase(decomp));
    decomp:=copy(decomp,1,p-1)+'"'+abuf[arcbufp].arcname+'" "' + copy(decomp,p+8,127)+'"';
    shell(decomp,400,3);
    if exdir='' then begin
      { !?! GoDir(temppath); }    { wurde durch Shell zurueckgesetzt }
      if not FileExists(temppath+datei) then
        rfehler(430)       { 'Datei wurde nicht korrekt entpackt.' }
      else begin
        newarc:=ArcType(TempPath+datei);
        if ArcRestricted(newarc) then newarc:=0;
        if newarc=0 then
        begin
         MessageViewer.GetFromExtension(ExtractFileExt(datei));
//          if MessageViewer.IsInternal then TestGifLbmEtc(datei,false,viewer);
          if MessageViewer.IsInternal then
            ListFile(TempPath+datei,datei,true,false,false,0)
          else
            MessageViewer.ViewFile(TempPath+datei,false);
          end
        else
          if arcbufp=max_arc then
            rfehler(432)   { 'Maximal 3 verschachtelte Archive moeglich!' }
          else begin
            decomp:=TempPath+datei;  { Stack sparen ... }
            if ViewArchive(decomp,newarc)<>0 then; 
        end;
        SafeDeleteFile(temppath+datei);
        end;
      { GoDir(OwnPath); }
      end;
    ShellPath:=spath;
    end;
  arctyp_save:=ats;
  attrtxt(col.colarcstat);
  wrt(77,4,arcname[ats]);
  keyboard(keydown);
  MessageViewer.Free;
end;


function a_getfilename(nr,nn:byte):string;
var fn   : string;
    sex  : string;
begin
//!!  fn:=trim(copy(get_selection,2,12));
  sex:=exdir; exdir:=TempPath;
  ShowArch(fn);
  exdir:=sex;
  a_getfilename:=TempPath+fn;
end;


procedure ArcSpecial(LSelf: TLister; var t:taste);
var s   : string;
    dp  : string;
    x,y : Integer;
    brk : boolean;
    fk  : string;
    sex : string;
    dd  : string;
begin
  if UpperCase(t)='X' then begin
    dp:=ExtractPath;
    dd:=getres(463);
    dialog(47+length(dd),3,getres(462),x,y);   { 'Extrakt' }
    maddstring(3,2,dd,dp,40,79,'');
    readmask(brk);
    enddialog;
    if brk then exit;
    UpString(dp);
    dp := IncludeTrailingPathDelimiter(dp);
    if not validfilename(dp+'test.$$1') then
      rfehler(433)   { 'ungueltiges Verzeichnis' }
    else begin
      sex:=exdir;
      exdir:=dp;
      s:=LSelf.FirstMarked;
      while (s<>#0) and (exdir<>'') do begin
        ShowArch(s);
        s:=LSelf.NextMarked;
        end;
      exdir:=sex;
      end;
    end
  else begin
    getfilename:=a_getfilename;
    fk:=forwardkeys; forwardkeys:='';
    test_fkeys(t);
    keyboard(fk);
    xp1o.listext(LSelf, t);
    end;
end;


{ 0=Esc, 1=minus, 2=plus }

function ViewArchive(var fn:string; typ:shortint):shortint;
var
  List: TLister;
  ar   : ArchRec;
  lm   : byte;

  function dt(d,t:word):string;
  begin
    dt:=formi(d and 31,2)+'.'+formi((d shr 5) and 15,2)+'.'+
        formi((d shr 9+80)mod 100,2)+'  '+
        formi(t shr 11,2)+':'+formi((t shr 5)and $3f,2)+':'+formi((t and $1f)*2,2);
  end;

  function prozent:string;
  begin
    with ar do
      if (OrgSize>0) and (OrgSize >= CompSize) then
        prozent:=strsrn(ar.CompSize/ar.OrgSize*100.0,3,1)
      else
        prozent:='     ';
  end;

  procedure renameDWC;
  var f  : file;
  begin
    assign(f,fn);
    fn:=ExtractFilePath(fn) +'temp$$.dwc';
    rename(f,fn);
  end;

begin
  if abs(typ)=ArcDWC then
    renameDWC;
  List := TLister.CreateWithOptions(1,ScreenWidth,5,screenlines-fnkeylines-1,1,'/NS/SB/M/NLR/');
  OpenArchive(fn,typ,ar);
  List.OnEnter := ShowArch;
  List.OnKeypressed := ArcSpecial;
  showkeys(11);
  attrtxt(col.colarcstat);
  mwrt(1,4,forms(getres(464), ScreenWidth));   { ' Name            OrgGroesse CompGroesse    %    Methode    Datum    Uhrzeit' }
  inc(arcbufp);
  with ar do begin
    arctyp_save:=arctyp;
    abuf[arcbufp].arcer_typ:=arctyp;
    abuf[arcbufp].arcname:=fn;
    mwrt(77,4,arcname[arctyp]);
    while not ende do begin
      if (name<>'') or (path='') then
        List.AddLine(iifc(path<>'','*',' ')+forms(name,12)+strsn(orgsize,11)+
              strsn(compsize,11)+'   '+ prozent+'  '+forms(method,10)+
              dt(datum,uhrzeit)+'       ' + name)
      else
        List.AddLine(forms('*'+path+name,80)+path+name);
      ArcNext(ar);
      end;
    end;
  CloseArchive(ar);

  exdir:='';
  llh:=true; listexit:=0;
  lm:=ListMakros; ListMakros:=16;
  pushhp(67);
  List.Show;
  pophp;
  ListMakros:=lm;
  dec(arcbufp);
  List.Free;
  attrtxt(col.colkeys);
  mwrt(1,2,sp(ScreenWidth));
  showlastkeys;
  if abs(typ)=ArcDWC then
    _era(fn);
  aufbau:=true;
  ViewArchive:=listexit;
end;

procedure FileArcViewer(fn:string);
var useclip : boolean;
    arc     : shortint;
    lm      : byte;
    ende    : boolean;
begin
  if (fn='') or multipos('?*',fn) then begin
    if fn='' then fn:=WildCard;
    useclip:=false;
    if not ReadFilename(getres(465),fn,true,useclip) then   { 'Archivdatei' }
      exit;
    fn:=ExpandFileName(fn);
    end;
  if FileExists(fn) then begin
    arc:=ArcType(fn);
    if ArcRestricted(arc) then arc:=0;
    if arc=0 then begin                                 { Wenns kein Archiv war...      }
      lm:=listmakros;
      listmakros:=16;                                   { Archivviewermacros benutzen!  }
      repeat
        if listfile(fn,fn,true,false,false, 0) = -4 then ende:=false
        else ende:=true;                                { und File einfach nur anzeigen }
      until ende;
      listmakros:=lm;
      end
      { rfehler(434)  } { 'keine Archivdatei' }
    else
      if ViewArchive(fn,arc)=0 then;
    end
  else
    rfehler(22);     { 'Datei ist nicht vorhanden!' }
end;


procedure DupeKill(autodupekill:boolean);
var d     : DB;
    f1,f2 : file;
    n,ll  : longint;
    x,y   : Integer;
    last,
    next  : string;
    flags : byte;
    log   : text;
    rec,rec2 : longint;

  procedure show;
  begin
    wrt(x+22,y+3,strsn(n,7));
    wrt(x+22,y+4,strsn(ll,7));
  end;

  procedure log_it;
  var _brett : string;
  begin
    _brett := dbReadStr(d,'brett');
    write(log,fdat(longdat(dbReadInt(d,'origdatum'))),' ');
    if FirstChar(_brett)='U' then
      write(log,forms(dbReadStr(d,'absender'),32))
    else begin
      dbSeek(bbase,biIntnr,copy(_brett,2,4));
      if dbFound then write(log,forms(copy(dbReadStrN(bbase,bb_brettname),2,40),32));
      end;
    writeln(log,' ',LeftStr(dbReadStr(d,'betreff'),37));
  end;

begin
  if diskfree(0)<_filesize(MsgFile+dbExt)*2 then begin
    rfehler(435);   { 'zu wenig Festplatten-Platz' }
    exit;
    end;
  message(getres2(466,1));   { 'Nachrichtendatei kopieren...' }
  dbTempClose(mbase);
  assign(f1,MsgFile+dbExt);  reset(f1,1);
  assign(f2,DupeFile+dbExt); rewrite(f2,1);
  fmove(f1,f2);
  close(f1);
  close(f2);
  SafeDeleteFile(DupeFile+dbIxExt);
  closebox;
  dbOpen(d,DupeFile,1);   { indizieren }
  n:=1; ll:=0;
  msgbox(32,8,getres2(466,2),x,y);   { 'DupeKill' }
  wrt(x+3,y+2,getres2(466,3));       { 'Nachrichten gesamt:' }
  wrt(x+3,y+3,getres2(466,4));       { '        bearbeitet:' }
  wrt(x+3,y+4,getres2(466,5));       { '         geloescht:' }
  attrtxt(col.colmboxhigh);
  wrt(x+22,y+2,strsn(dbRecCount(d),7));
  assign(log,logpath+DupeLogfile);
  if existf(log) then append(log)
  else rewrite(log);
  writeln(log,getres2(466,6)+date+getres2(466,7)+time);   { 'DupeKill gestartet am ' / ' um ' }
  last:='';
  dbGoTop(d);
  while not dbEOF(d) and (dbReadInt(d,'halteflags')=0) do begin
    show;
    repeat
      inc(n);
      next:=dbReadStr(d,'brett')+dbLongStr(dbReadInt(d,'origdatum'))+dbReadStr(d,'msgid');
      if (length(next)>10) and (next=last) and (dbReadInt(d,'unversandt') and 1=0)
      then begin
        dbRead(d,'HalteFlags',flags);
        rec:=dbRecno(d);
        dbSkip(d,1); rec2:=dbRecno(d);
        dbGo(d,rec);
        flags:=2;
        dbWrite(d,'HalteFlags',flags);
        log_it;
        dbGo(d,rec2);
        inc(ll);
        end
      else
        dbSkip(d,1);
    until (next<>last) or dbEOF(d);
    last:=next;
    end;
  dbClose(d);
  DeleteFile(MsgFile+dbExt);
  assign(f1,DupeFile+dbExt); rename(f1,MsgFile+dbExt);
  DeleteFile(DupeFile+dbIxExt);
  writeln(log);
  close(log);
  dbTempOpen(mbase);
  if not autodupekill then
  begin
    signal;
    attrtxt(col.colmbox);
    wrt(x+2,y+6,' '+getres(12){+' '#8});
    wait(curon);
  end;
  closebox;
  freeres;
  aufbau:=true; xaufbau:=true;
end;


procedure print_msg(initpr:boolean);
var t  : text;
    fn : string;
    s  : string;
begin
  if dbReadInt(mbase,'typ')=ord('B') then
    rfehler(436)   { 'Drucken nicht moeglich - Binaernachricht' }
  else begin
    fn:=TempS(dbReadInt(mbase,'groesse')+1000);
    extract_msg(1,'',fn,false,1);
    assign(t,fn);
    if existf(t) then begin
      if initpr then begin
        rmessage(119);
        initprinter;
        end;
      if checklst then begin
        reset(t);
        while not eof(t) do begin
          readln(t,s);
          printline(s);
          end;
        close(t);
        erase(t);
        end;
      if initpr then begin
        exitprinter;
        mdelay(200);
        closebox;
        end;
      end;
    end;
end;


Procedure Brettmarksuche;
var   x,y     : Integer;
      brk     : boolean;
      nn,n,nf : longint;
      suchst, bname   : string;
      spos    : longint;
      rec     : longint;
      m1,m2,j : longint;
      found, found_not : boolean;
begin
  SuchSt := '';
  rec:=dbRecno(bbase);
  if not Suche(getres2(467,5),'#','') then exit;
  diabox(52,7,getres2(467,5),x,y);   { 'Brett-(markier)-Suche' }
  brk:=false;
  m1:=maxlongint;
  attrtxt(col.coldialog);
  wrt(x+3,y+4,getres2(467,3));   { 'Suchen...     %        gefunden:' }
  nn:=dbRecCount(bbase); n:=0; nf:=0;
  dbGoTop(bbase);
  attrtxt(col.coldiahigh);
  while not dbEOF(bbase) and not brk do
  begin
    inc(n);
    FWrt(x+13, y+4, Format('%3d', [n*100 div nn]));
    FWrt(x+35, y+4, Format('%4d', [nf]));
    bname := dbReadNStr(bbase,bb_brettname);
    j:=0;
    repeat
      suchst:= LeftStr(copy(sst,seekstart[j],seeklen[j]),40);
      found:=((igcase and (pos(suchst,UpperCase(bname))>0)) or
       (not igcase and (pos(suchst,bname)>0)));
      found_not:=found and seeknot[j];
      if suchand and not found and seeknot[j] then found:=true;
      inc(j);
    until (j=suchanz) or (suchand xor found) or found_not;
    if found_not then found:=false;
    if found then
    begin
      UBAddmark(dbRecno(bbase));
      dbreadN(bbase,bb_index,m2);
      if m2<m1 then
      begin
        m1:=m2;
        spos:=dbRecno(bbase);
      end;
      inc(nf);
    end;
    dbNext(bbase);
    if n mod 16=0 then testbrk(brk);
  end;
  if m1=maxlongint then
    dbGo(bbase,rec)
  else
    dbGo(bbase,spos);
  aufbau:=true;
  closebox;
  if not brk and (nf=0) then fehler(getres2(467,6));  { 'keine passenden User gefunden' }
  freeres;
end;




{ Ausgabe true -> UserMode umschalten }

function UserMarkSuche(allmode:boolean):boolean;
const suchst  : string = '';
var   x,y     : Integer;
      brk     : boolean;
      nn,n,nf : longint;
      uname,
      sname   : string;
      spos    : longint;
      rec     : longint;
      mi,j    : shortint;
      found, found_not : boolean;
begin
  UserMarkSuche:=false;
  rec:=dbRecno(ubase);
  if not Suche(getres2(467,1),'#','') then exit;
  diabox(52,7,getres2(467,1),x,y);   { 'User-(markier)-Suche' }
  brk:=false;
  sname:=#255;
  attrtxt(col.coldialog);
  wrt(x+3,y+4,getres2(467,3));   { 'Suchen...     %        gefunden:' }
  nn:=dbRecCount(ubase); n:=0; nf:=0;
  mi:=dbGetIndex(ubase); dbSetIndex(ubase,0);
  dbGoTop(ubase);
  attrtxt(col.coldiahigh);
  while not dbEOF(ubase) and not brk do
  begin
    inc(n);
    FWrt(x+13, y+4, Format('%3d', [n*100 div nn]));
    FWrt(x+35, y+4, Format('%4d', [nf]));
    UName := dbReadNStr(ubase,ub_username);
    j:=0;
    repeat
      suchst:= LeftStr(copy(sst,seekstart[j],seeklen[j]),40);
      found:=((igcase and (pos(suchst,UpperCase(uname))>0)) or
       (not igcase and (pos(suchst,uname)>0)));
      found_not:=found and seeknot[j];
      if suchand and not found and seeknot[j] then found:=true;
      inc(j);
    until (j=suchanz) or (suchand xor found) or found_not;
    if found_not then found:=false;
    if found then
    begin
      UBAddmark(dbRecno(ubase));
      if not allmode and (dbReadInt(ubase,'adrbuch')=0) then
        UserMarkSuche:=true;
      if uname<sname then
      begin
        sname:=uname;
        spos:=dbRecno(ubase);
      end;
      inc(nf);
    end;
    dbNext(ubase);
    if n mod 16=0 then testbrk(brk);
  end;
  dbSetIndex(ubase,mi);
  if sname<>#255 then  // irgnore warning, sname is correctly initialized
    dbGo(ubase,spos)
  else
    dbGo(ubase,rec);
  aufbau:=true;
  closebox;
  if not brk and (nf=0) then fehler(getres2(467,4));  { 'keine passenden User gefunden' }
  freeres;
end;


procedure BrettInfo;
var i   : longint;
    x,y : Integer;
    brk : boolean;
begin
  dbReadN(bbase,bb_index,i);
  message(getres(468)+': '+strs(i));   { 'Brettindex-Nr.' }
  wait(curoff);
  closebox;
  if lastkey=' ' then begin
    dialog(30,1,'',x,y);
    maddint(3,1,getres(468)+' ',i,6,8,0,99999999);
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWriteN(bbase,bb_index,i);
      aufbau:=true;
      end;
    end;
end;


procedure NtInfo;
var mnt : longint;
    nts : string;
begin
  dbReadN(mbase,mb_netztyp,mnt);
  nts:=' ('+ntName(mnt and $ff)+')';
  message(getres(469)+strs(mnt and $ff)+nts);   { 'Netztyp: ' }
  wait(curoff);
  closebox;
end;


procedure do_bseek(fwd:boolean);
begin
  repeat
    if fwd then dbSkip(bbase,1)
    else dbSkip(bbase,-1);
  until dbBOF(bbase) or dbEOF(bbase) or brettok(false);
end;


procedure FidoMsgRequest(var nnode:string);
var files    : string;
    ic,id,
    k,p      : byte;
    p1,s,s1,t,u : string;
    v        : char;
    node     : string;
    secondtry,
    mark,
    lMagics  : boolean;
    dir, name, ext: string;
begin
  nnode := '';
  if not LastLister.Selbar and (LastLister.SelCount=0) then begin
    rfehler(438);   { 'keine Dateien markiert' }
    exit
  end;
  if not TestNodelist or not TestDefbox then exit;
  s := FMsgReqnode;
  p := cpos('.',s);
  if (p>0) then node:=LeftStr(s,p-1)
    else node:=s;
  files := '';
  u := ''; t := '';
  lMagics := Magics;
  secondtry:=false;
  s := LastLister.FirstMarked;
  repeat
    { Von Anfang an leer oder Liste komplett durchlaufen und nichts gefunden,
      dann probieren wir's nochmal mit MAGICS }
    if (s=#0) then begin
      secondtry:=true;
      s := LastLister.FirstMarked;
      lMagics:=true;
    end;
    while (s<>#0) do begin
      { --- komplett neu:oh (aus MultiReq uebernommen) --->> }
{     if (s='') then lMagics:=false; }

      { Usernamen vor Quotes killen }
      k:=cPos('>',s);
      if (k>0) then if (k<6) then delete(s,1,k);

      k:=0;
      if (s<>'') then
      while (k<byte(FirstChar(s))) do begin
        t:=''; v:=#0;
        { Nach dem ersten erlaubten Zeichen suchen }
        while (Length(s)>0)
        and not (FirstChar(s) in ['a'..'z','A'..'Z','0'..'9','@','!','$','^']) do begin
            v:=s[1];
            delete(s,1,1);
          continue
        end;
        { Vor dem Dateinamen muss ein Trennzeichen stehen }
        if (v<>#0) then if not (v in [#32,'"','<','>','Ø','Æ','(','[','{',',',';',':','_','*']) then begin
          while (Length(s)>0)
          and not (s[1] in [#32,'"','<','>','Ø','Æ','(','[','{','_','*']) do begin
            delete(s,1,1);
            continue
          end;
          continue
        end;
        mark:=false;
        if (v<>#0) then if (v='*') or (v='_') then mark:=true;
        k:=1; { erstes Zeichen ist schon ok, also Rest testen }
        while (k<Length(s))
        and (s[k+1] in ['a'..'z','A'..'Z','0'..'9','_','@','.','!','/','?','*',
                        '$','%','-']) do inc(k);
        t:=copy(s,1,k);
        u:=UpperCase(t);
        delete(s,1,Length(t));
        { Auf den Dateinamen muss ein Trennzeichen folgen }
        if (Length(s)>0) then if not (s[1] in [#32,'"','<','>','Ø','Æ',')',']','}',',',';',':','_','*']) then continue;

        if (mark and (LastChar(t) in ['_','*'])) then DeleteLastChar(t);

        while LastChar(t) in ['.','!','?','/'] do DeleteLastChar(t);
        if (Length(t)<2) then continue;
        k:=0;
        for ic:=1 to Length(t) do if t[ic]='.' then inc(k);
        if (k>1) then continue;
        if (pos('**',t)>0) then continue;
        if not lMagics then if (cPos('.',t)<3) and (Length(t)<5) then continue;

        { Passwort suchen, erkennen und speichern }
        p1:='';
        ic:=cPos('/',t); if (ic>0) then begin
          p1:=copy(t,ic,20); delete(t,ic,99)
        end;

        u:=UpperCase(t);
        if (length(u)<2) then continue;
        s1:=u;
        if (s1='S0') or (s1='XP') then continue;
        if (s1='ZMH') or (s1='NMH') then continue;
        if (s1='V.FC') or (s1='VFC') then continue;
        if (s1='ISDN') or (s1='USR') then continue;
        if (s1='FQDN') or (s1='INC') then continue;
        if (s1='IMHO') or (s1='YMMV') then continue;
        if (s1='ORG') or (s1='PGP') then continue;
        if (s1='FAQ') or (s1='OS') then continue;
        if (s1='DOS') then continue;
        if (length(s1)=3) then if (copy(s1,1,2)='RC') and (s1[3] in ['0'..'9']) then continue;
        ic:=cPos('@',t); if (ic>1) and (ic<>Length(t)) then continue;

        { Auf Beschreibungs-Datei testen }
        FSplit(u,dir,name,ext);
        if (ext='.DIZ') then continue;
        if (name='FILES') or (name='FILE_ID') or (name='00GLOBAL')
          or (name='DESCRIPT') then continue;

        { Ist der String eine Versionsnummer? V42.3, 1.0, X75, V34B etc. }
        if (Length(t)<8) then begin
          u:=t;
          if (UpperCase(copy(u,1,3))='VOL') then delete(u,1,3);
          if (UpCase(u[1]) in ['V','X']) then delete(u,1,1);
          id:=0;
          for ic:=1 to length(u) do if not (UpCase(u[ic]) in ['0'..'9','.','A','B','G']) then id:=1;
          if (id=0) then continue
        end;

        { Ist der String eine Baudrate oder Dateigroesse? xx.xK, x in  [0..9] }
        if (length(t)<10) then begin
          u:=Uppercase(t);
          { Zahl }
          while ((u<>'') and (u[1] in ['0'..'9'])) do delete(u,1,1);
          { . }
          if (u<>'') then if (u[1]='.') then begin
            delete(u,1,1);
            { Zahl }
            while (u<>'') and (u[1] in ['0'..'9']) do delete(u,1,1);
          end;
          if (u='K') or (u='KB')
            or (u='M') or (u='MB')
            or (u='B') or (u='BYTES')
          then continue;
        end;

        { Telefonnummern ausblenden }
        id:=0;
        for ic:=1 to length(u) do if not (u[ic] in ['0'..'9','-','/','+','(',')']) then id:=1;
        if (id=0) then continue;

        if (lMagics) then if (Uppercase(t)=t) then begin
          files:=files+' '+t;
          continue
        end;
        u := Uppercase(t);
        ic := cPos('.',u); if not (ic in [2..9]) then continue;
        if (length(u)<4) then continue;
        if (length(u)-ic>3) then continue;
        if (p1<>'') then u:=u+p1; p1:='';
        files:=files+' '+u;
        continue
      end; { while (k<byte(s[0])) }
      { <<--- komplett neu:oh (aus MultiReq uebernommen) --- }
      s:=LastLister.NextMarked;
    end; { while (s<>#0) do begin }
    files:=trim(files);
    { Abbrechen, wenn was gefunden, oder zweiter Durchlauf oder schon beim
      ersten mal MAGICS an waren und ein zweiter Durchlauf unnoetig ist }
  until ((files<>'') or secondtry or lmagics);
  if (files='') then
    rfehler(438)    { 'keine Dateien markiert' }
  else
    nnode:=FidoRequest(node,files);
end;


function _killit(ask:boolean):boolean;
var uv     : byte;
    _brett : string;
begin
  _killit:=false;
  dbReadN(mbase,mb_unversandt,uv);
  if uv and 1<>0 then
    rfehler(439)   { 'Unversandte Nachricht mit "Nachricht/Unversandt/Loeschen" loeschen!' }
  else
    if not ask or ReadJN(getres(470)+   { 'Nachricht loeschen' }
      iifs(KK and HasRef,getres(471),''),true) then
    begin                            { ' (unterbricht Bezugsverkettung)' }
      if msgmarked then
        msgUnmark;
      wrkilled;
      _brett := dbReadStrN(mbase,mb_brett);
      DelBezug;
      dbDelete(mbase);
      if FirstChar(_brett)<>'U' then RereadBrettdatum(_brett);
      _killit:=true;
      aufbau:=true; xaufbau:=true;
      setbrettgelesen(_brett);
      end;
end;

end.

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

{ CrossPoint - Overlayroutinen, die von XP4 aufgerufen werden }

{$I XPDEFINE.INC }
{$O+,F+}

{.$DEFINE sDebug}

unit xp4o;

interface

uses
  crt,dos,dosx,typeform,fileio,inout,keys,montage,maske,datadef,database,
  lister,archive,maus2,winxp,printerx,resource,xpglobal,
  xp0,xp1,xp1o2,xp1help,xp1input,lfn;

var  such_brett  : string[5];    { fÅr Suche im gewÑhlten Brett }
     FMsgReqnode : string[BoxNameLen];    { F3 - Request - Nodenr. }

const max_arc = 3;    { maximale verschachtelte Archivdateien }
      suchlen = 160;  { Maximallaenge der Suchbegriffe }
      histmax = 14;   { Anzahl Eintraege in Suchbegriff-History} 
      opthmax = 4;    { Anzahl Eintraege in Optionen-History } 
      suchmax = 20;   { Anzahl AND/OR Teilstrings im Suchbegriff }

var seeklen   : array[0..suchmax-1] of byte;
    seekstart : array[0..suchmax-1] of byte;
    seeknot   : array[0..suchmax-1] of boolean;
    suchand   : boolean;
    suchanz   : byte;
    sst       : string[suchlen];
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
function  ViewArchive(var fn:pathstr; typ:shortint):shortint;
procedure FileArcViewer(fn:pathstr);

procedure ShowArch(var fn:string);
function  a_getfilename(nr,nn:byte):pathstr;
procedure ArcSpecial(var t:taste);

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
procedure OldSEEK_ed(var t:taste); {Lister-Tastenabfrage fuer Seek-Menue}

function Bool_BrettGruppe(var s:string):boolean;
function Bool_Brettindex(var s:string):boolean;
Procedure Brettmarksuche;


implementation  {-----------------------------------------------------}

uses xpkeys,xpnt,xp1o,xp4,xp3,xp3o,xp3o2,xp3ex,xpfido,xpmaus,xpview,
     xp_pgp,xpovl;

type arcbuf = record
                arcer_typ : shortint;
                arcname   : pathstr;
              end;
     arcbp  = ^arcbuf;

const arcbufp : byte = 0;
      suchopt : string[8] = '*';  { Flag fÅr erste Suche seit Programmstart }

    history : array[0..histmax] of String[Suchlen]=
     ('','','','','','','','','','','','','','','');
    history_changed   : boolean = false;

var  reobuf : array[0..ablagen-1] of boolean;
     bufsiz : array[0..ablagen-1] of longint;  { Grî·e nach Reorg }
     abuf   : array[1..max_arc+1] of arcbp;
     exdir  : pathstr;
     arctyp_save : shortint;
     mid_bretter       : byte;
     Mid_teilstring    : boolean;


function testbrettscope(var s:string):boolean;
var i : integer;
begin
  if (length(s)=1) and (lastkey<>keybs) then begin
    for i:=4 downto 0 do
      if upcase(s[1])=ustr(left(getres2(442,i),1)) then
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

procedure OldSEEK_ed(var t:taste); {Lister-Tastenabfrage fuer Seek-Menue}
begin
  if (ustr(t)='E') then begin
    EditFile(libraryFile,false,false,false,0,false);
    t:=keyesc;
    pushkey(keysf2);
    end;
end;

procedure seekmenu(var s:string);
  var t    : text;
      brk  : boolean;
      x,y  : byte;

const height = 10;
      width  = 70;
begin
      assign(t,libraryFile);
      reset(t);  
      s:=''; 
      if ioresult<>0 then exit;
      selbox(width+2,height+2,getres2(441,21),x,y,true);
      openlist(x+1,x+width,y+1,y+height,0,'/NS/SB/DM/S/M/');
      ListboxCol;
      listarrows(x+width+1,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
      while not eof(t) do
      begin
        readln(t,s);
        app_L(left(s,suchlen));
        end;
      listtp(oldseek_ed);
      list(brk);
      if list_markanz=0 then begin
        s:=get_selection;
        if s[1]=' ' then brk:=true;
        end
      else if not brk then begin
        s:=first_marked;
        x:=0;  
        repeat
          if (s<>#0) and (s<>'') and (s[1]<>' ')
          then inc(x);
          s:=next_marked;
        until (s=#0) or (x=histmax);
        for y:=histmax downto x do history[y]:=history[y-x];
        s:=first_marked;
        y:=0;
        repeat
          if (s<>#0) and (s<>'') and (s[1]<>' ')
          then begin
            history[y]:=s;
            inc(y);
            end;
          s:=next_marked;
        until (y>x) or (s=#0);        
        s:=history[0];
        history_changed:=true;
        pushkey(keyctcr);
        end;
      closelist;
      closebox;
      close(t);
      if brk then s:='';
end;


{ Suchfeld:  '' (Volltext), '*' (Umiversal), 'Betreff', 'Absender', 'MsgID' }

function Suche(anztxt,suchfeld,autosuche:string):boolean;
type  suchrec    = record
                     betr,user,txt : string[SuchLen];
                     fidoempf,mid  : string[SuchLen];
                     nbetr,nuser   : Boolean;
                     nfidoempf     : Boolean;
                     or_betr       : Boolean;
                     or_user       : Boolean;
                     or_fidoempf   : Boolean;
                     vondat,bisdat : datetimest;
                     vonkb,biskb   : longint;
                     status        : string[10];
                     typ           : string[10];
                   end;

const srec    : ^suchrec = nil;
      opthist : array[0..opthmax] of String[8]=('','','','','');

var x,y             : byte;
    brk             : boolean;
    n,nf            : longint;
    p               : pointer;
    psize           : word;
    spez            : boolean;
    i               : integer;
    brett           : string[AdrLen];
    me,uu           : boolean;
    hdp             : headerp;
    hds             : longint;
    bretter         : string[8];
    t               : text;

    suchstring      : string[SuchLen];
    typc            : char;
    statb           : byte;
    _vondat,_bisdat : longint;
    minsize,maxsize : longint;
    bereich         : shortint;
    _brett          : string[5];
    mi,add          : byte;
    bera            : array[0..4] of string[10];
    stata           : array[0..5] of string[10];
    typa            : array[0..4] of string[10];

    seek            : string[suchlen];
    found           : boolean;
    markedback      : marklistp;
    markanzback     : integer;
    check4date      : boolean;  
    headersuche     : byte; 
    andmask,ormask  : byte;
    holdmarked      : boolean;    

label ende,restart;


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
  m,n,i   : byte;
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
    suchand:=cpos('o',lstr(suchopt))=0;             { OR }
    if not suchand or (cpos('a',lstr(suchopt))>0)   { oder AND ?}
     and not (trim(sst)='') then                    { und nicht Leertext (Suche-Spezial) }
    begin
      n:=0;
      seek:=trim(sst);                              { Leerzeichen vorne und hinten, }
      i:=length(seek);
      while (seek[i]='"') and (i<>0) do dec(i);     { und Ausrufezeichen hinten abschneiden }
      truncstr(seek,i);
      if seek<>'' then begin
        i:=1;
        sst:=seek+'"';  quotes:=false;
        while (i<length(sst)) and (n<suchmax) do
        begin
          while sst[i]=' ' do inc(i);               { Leerzeichen ueberspringen }
          if not quotes then
          begin
            seeknot[n]:=sst[i]='~';                 { NOT Flag setzen }
            while ((sst[i]='~') or (sst[i]=' '))
              do inc(i);                            { und evtl. weitere ^ ueberspringen }
            end;
          quotes:=sst[i]='"';                       { evtl. "- Modus aktivieren....}
          while sst[i]='"' do inc(i);               { weitere " ueberspringen }
          seekstart[n]:=i;
          while (i<length(sst)) and not             { weiterzaehlen bis Stringende      }
           ((not quotes and (sst[i]=' ')) or        { oder Space das nicht in " ist     }
            (sst[i]='"')) do inc(i);                { oder das naechste " gefunden wird }
          seeklen[n]:=i-seekstart[n];
          quotes:=not quotes and (sst[i]='"');      { -"- Modus umschalten }
          if (not quotes) then inc(i);
          inc(n);
          end;
        if seeklen[n-1]=0 then dec(n);              { Falls String mit > "< endete... }
        suchanz:=n;
        end;

      if suchanz=1 then suchand:=true;
      m:=0;
      for n:=0 to suchanz-1 do                      { Teilstrings umsortieren: NOT zuerst }
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


  function InText(var key:string):boolean;
  var size : longint;
      ofs  : longint;
      wsize: word;
  begin
    dbReadN(mbase,mb_msgsize,size);
    if size=0 then begin   { leerer Datensatz - vermutlich durch RuntimeError }
      dbDelete(mbase);
      InText:=false;
      end
    else begin
      wsize:=min(size,psize);
      ofs:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');

      if headersuche=1 then begin           { nur Header durchsuchen }
        wsize:=ofs;
        ofs:=0;
        end
      else if headersuche=2 then ofs:=0;    { Header und Text durchsuchen }

      if (ofs>=0) and (ofs<wsize+1+length(key)) then begin
        dec(wsize,ofs);
        XmemRead(ofs,wsize,p^);
       (* if umlaut then upstring(key); *)      { Umlaut-Suche automatisch case-insensitiv }
        Intext:=TxtSeek(p,wsize,key,igcase,umlaut);
        end
      else
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
       0 : StatOk:=true;
     1,2 : StatOK:=(statb=flags);
       3 : StatOK:=(flags=1) or (flags=2);
       4 : StatOK:=(dbReadInt(mbase,'gelesen')=0);
       5 : StatOK:=(dbReadInt(mbase,'gelesen')<>0);
    end;
  end;


  { Leerzeichen Links und rechts loschen, Tilden links ebenfalls }
  { boolean setzen, wenn Tilde gefunden wurde }

  procedure Scantilde(var s:String; var suchnot:boolean);
  begin
    trim(s);
    if s='' then suchnot:=false
    else begin
      suchnot:=s[1]='~';
      i:=1;
      while ((s[i]='~') or (s[i]=' ')) do inc(i);
      s:=mid(s,i);
    end;
  end;


{--Einzelne Nachricht mit Sucheingaben vergleichen--}

  procedure TestMsg;

  var betr2 : string[BetreffLen];
      user2 : string[AdrLen];
      realn : string[40];
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
        seek:=left(mid(sst,seekstart[j]),seeklen[j]);
        found:=Intext(seek);
        found_not:=found and seeknot[j];
        if suchand and not found and seeknot[j] then found:=true;
        inc(j);
      until (j=suchanz) or (suchand xor found) or found_not;
    if found_not then found:=false;
    end;


  begin
    inc(n);
    if (n mod 10)=0 then begin
      moff;
      gotoxy(x+9,wherey); write(n:7);
      gotoxy(x+26,wherey); write(nf:5);
      mon;
      end;

{--Spezialsuche--}
    if spez then
    with srec^ do begin
      if DateFit and SizeFit and TypeFit and StatOk then begin
        dbReadN(mbase,mb_betreff,betr2);
        if (betr<>'') and (length(betr2)=40) then begin
          ReadHeader(hdp^,hds,false);
          if length(hdp^.betreff)>40 then
            betr2:=hdp^.betreff;
          end;
        dbReadN(mbase,mb_absender,user2);
        if not ntEditBrettEmpf(mbnetztyp) then begin   { <> Fido, QWK }
          dbReadN(mbase,mb_name,realn);
          end
        else
          realn:=#0;
        if fidoempf<>'' then
          if not ntBrettEmpf(mbnetztyp) then
            hdp^.fido_to:=''
          else begin
            ReadHeader(hdp^,hds,false);
            end;
        if umlaut then begin                    { Umlaute anpassen}
          UkonvStr(betr2,high(betr2));
          UkonvStr(user2,high(user2));
          UkonvStr(realn,high(realn));
          UkonvStr(hdp^.fido_to,high(hdp^.fido_to));
          end;
        if igcase then begin                    { Ignore Case}
          UpString(betr2);
          UpString(user2);
          UpString(realn);
          UpString(hdp^.fido_to);
          end;

        if andmask<>0 then begin
          foundmask:=0;
          if ((betr='') or (pos(betr,betr2)>0) xor nbetr) 
            then inc(foundmask,2);
          if ((user='') or ((pos(user,user2)>0) or (pos(user,realn)>0)) xor nuser)
            then inc(foundmask,4);
          if ((fidoempf='') or (pos(fidoempf,hdp^.fido_to)>0) xor nfidoempf)
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

{--Normale Suche--}

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
      if suchfeld<>'' then begin
        dbRead(mbase,suchfeld,such);
 
        if (suchfeld='Absender') and not ntEditBrettEmpf(mbnetztyp)
        then begin
          dbReadN(mbase,mb_name,seek);          { Bei Usersuche auch Realname ansehen... }
          such:=such+seek;
          end;
 
        if stricmp(suchfeld,'betreff') and (length(such)=40) 
        then begin
          ReadHeader(hdp^,hds,false);
          if length(hdp^.betreff)>40 then
            such:=hdp^.betreff;
          end;
         if suchfeld='MsgID' then begin
          ReadHeader(hdp^,hds,false);
          such:=hdp^.msgid;
          end;
        if umlaut then UkonvStr(such,high(such));

        j:=0;
        repeat
          seek:=left(mid(sst,seekstart[j]),seeklen[j]);      { Erklaerung siehe Volltextcheck }
          found:=((igcase and (pos(seek,UStr(such))>0)) or
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


  procedure TestBrett(_brett:string);
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

  function userform(s:string):string;
  var p : byte;
  begin
    p:=cpos('@',s);
    if p=0 then userform:=s
    else userform:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
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

{--# Suche #--}

begin
  for i:=0 to 4 do bera[i]:=getres2(442,i);
  for i:=0 to 5 do stata[i]:=getres2(442,10+i);
  for i:=0 to 4 do typa[i]:=getres2(442,20+i);
  if suchopt[1]='*' then
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
      { seek:=suchstring; }
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
        i:=ReCount(suchstring);         { JG:15.02.00 Re's wegschneiden }
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
    while (i>0) and (ustr(typ)<>ustr(typa[i])) do dec(i);
    typ:=typa[i];
    i:=5;
    while (i>0) and (ustr(status)<>ustr(stata[i])) do dec(i);
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
      if txt='' then
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
      end;

    sst:=suchstring;
    igcase:=multipos('iu',lstr(suchopt));
    umlaut:=multipos('ÑîÅu',lstr(suchopt)); {JG: 15.02.00 Umlautschalter}
    if umlaut and not igcase then 
    begin
      suchopt:=suchopt+'i';
      igcase:=true;
      end;
    check4date:=cpos('l',lstr(suchopt))>0;  { Suchen ab aktuellem Lesedatum }
    HoldMarked:=cpos('m',lstr(suchopt))>0;  { Alte Markierungen beibehalten } 

    i:=cpos('s',lstr(suchopt));             { Such-History loeschen }
    if i>0  then                   
    begin 
      delete(suchopt,i,1); 
      for i:=1 to histmax do history[i]:='';
      CheckHistory;
      closebox;
      goto restart;
      end;

    i:=cpos('k',lstr(suchopt));             { Such-History loeschen }
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
    if cpos('h',lstr(Suchopt))>0 then headersuche:=1;   { Headersuche   }       
    if cpos('g',lstr(suchopt))>0 then headersuche:=2;   { Volltext+Headersuche } 

    bereich:=0;
    for i:=1 to 4 do
      if ustr(bretter)=ustr(bera[i]) then bereich:=i;
    statb:=0;
    for i:=1 to 5 do
      if ustr(status)=ustr(stata[i]) then statb:=i;
    me:=true;
    attrtxt(col.coldialog);

    if spez then with srec^ do begin
      sst:=txt;
      user:=userform(user);
      if umlaut then begin                              { JG: 15.02.00 Umlaute konvertieren }
        UkonvStr(betr,high(betr)); UkonvStr(user,high(user));
       { UkonvStr(txt,high(txt));} UkonvStr(fidoempf,high(fidoempf));
        end;                                            { /JG }
      if igcase then begin
        UpString(betr); UpString(user); {UpString(txt);} UpString(fidoempf);
        end;
      scantilde(betr,nbetr); scantilde(user,nuser);
      scantilde(fidoempf,nfidoempf);
      if ustr(typ)=ustr(typa[1]) then typc:='T'
      else if ustr(typ)=ustr(typa[2]) then typc:='B'
      else if ustr(typ)=ustr(typa[3]) then typc:='F'
      else if ustr(typ)=ustr(typa[4]) then typc:='M'
      else typc:=' ';
      _vondat:=ixdat(copy(vondat,7,2)+copy(vondat,4,2)+copy(vondat,1,2)+'0000');
      _bisdat:=ixdat(copy(bisdat,7,2)+copy(bisdat,4,2)+copy(bisdat,1,2)+'2359');
      if biskb=99999 then biskb:=maxlongint div 2048;
      minsize:=vonkb*1024;
      maxsize:=biskb*1024+1023;
      end;
   { else begin}
      if umlaut then UkonvStr(sst,high(sst));                        {JG:15.02.00}
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
      new(hdp);
      attrtxt(col.coldiahigh);
      psize:=min(maxavail-10000,60000);
      getmem(p,psize);
      brk:=false;

      if aktdispmode=11 then begin                       {-- Suche markiert (Weiter suchen) --}
        markanzback:=0; 
        if maxavail>maxmark * sizeof(markrec) then           { Wenn genug Speicher da ist }
        begin                                                { Markierte Nachrichten merken }
          getmem(markedback,maxmark * sizeof(markrec));      
          for i:=0 to markanz do markedback^[i]:=marked^[i];
          markanzback:=markanz;
          end;

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
        brk:=false;
        while not dbEOF(mbase) and (markanz<maxmark) and not brk do begin
          dbReadN(mbase,mb_brett,_brett);
          if (bereich=0) or ((bereich=1) and (_brett[1]='A')) or
                            ((bereich=2) and (_brett[1]='U')) then
            TestMsg;
          if not dbEOF(mbase) then    { kann passieren, wenn fehlerhafter }
            dbNext(mbase);            { Satz gelîscht wurde               }
          testbrk(brk);
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
                dbReadN(bbase,bb_brettname,brett);
                TestBrett(mbrettd(brett[1],bbase));
                end;
              inc(i);
              end;
            end;
          end
        else
          case aktdispmode of
            -1..0 : begin
                      dbReadN(bbase,bb_brettname,brett);
                      TestBrett(mbrettd(brett[1],bbase));
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
      dispose(hdp);
      end;

{--Suche beendet--}

    if (markanz=0) or (holdmarked and (markanz=markanzback))   { Nichts gefunden }
    then begin 
      if me then begin
        hinweis(getres2(441,18));   { 'keine passenden Nachrichten gefunden' }
        aufbau:=true;               { wg. gelîschter Markierung! }
        end; 
      goto ende;                    { Fenster wiedeherstellen...} 
      end
      
    else begin
      Suchergebnis:=true;
      suche:=true;                  { Suche erfolgreich }
      signal;
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
    brett,_Brett : string[5];
 (*       ll     : integer; *) 

begin
  moment;
  dbReadN(mbase,mb_betreff,betr);
  ReCount(betr);  { schneidet Re's weg }
  betr:=trim(betr);
  UkonvStr(betr,high(betr));
  dbReadN(mbase,mb_brett,brett);
  dbSetIndex(mbase,miBrett);
  dbSeek(mbase,miBrett,brett);
  markanz:=0;
  repeat
    dbReadN(mbase,mb_betreff,betr2);
    ReCount(betr2);
    betr2:=trim(betr2);
    UkonvStr(betr2,high(betr2));
 (*  ll:=min(length(betr),length(betr2));
    if (ll>0) and (ustr(left(betr,ll))=ustr(left(betr2,ll))) then *)
   if ustr(betr)=ustr(betr2) then 
      MsgAddmark;
    dbSkip(mbase,1);
    if not dbEOF(mbase) then
      dbReadN(mbase,mb_brett,_brett);
  until dbEOF(mbase) or (_brett<>brett);
  closebox;
  signal;
  if markanz>0 then select(11);
  aufbau:=true;
end;


procedure SucheWiedervorlage;
var x,y,xx : byte;
    brk    : boolean;
    _brett : string[5];
    mbrett : string[5];
    dat    : string[4];
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
    while not dbEOF(xbase) and not brk do begin
      inc(n);
      gotoxy(xx,y+2); attrtxt(col.colmboxhigh);
      write(n*100 div nn:3);
      if (xbase=ubase) or (not smdl(dbReadInt(xbase,'ldatum'),ixDat('2712310000')))
      then begin
        if xbase=ubase then _brett:='U' else 
        _brett:=copy(dbReadStr(xbase,'brettname'),1,1);
        _brett:=_brett+dbLongStr(dbReadInt(xbase,'int_nr'));
        dbSeek(mbase,miBrett,_brett+dat);
        mbrett:=_brett;
        while not dbEOF(mbase) and (mbrett=_brett) do begin
          dbReadN(mbase,mb_brett,mbrett);
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
  closebox;
  if not brk then
    if markanz=0 then
      hinweis(getres(444))   { 'keine Wiedervorlage-Nachrichten gefunden' }
    else begin
      signal;
      select(11);
      end;
end;


{$I XP4O.INC}     { Reorg }


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


function testuvs(txt:atext):boolean;
var uvs : byte;
begin
  dbReadN(mbase,mb_unversandt,uvs);
  if uvs and 1<>0 then rfehler1(422,txt);  { 'Bitte verwenden Sie Nachricht/Unversandt/%s' }
  testuvs:=(uvs and 1<>0);
end;


procedure ModiBetreff;
var brk  : boolean;
    hdp  : headerp;
    hds  : longint;
    x,y  : byte;
    fn   : pathstr;
    f    : file;
begin
  if testuvs(getres(453)) then exit;   { 'éndern' }
  new(hdp);
  ReadHeader(hdp^,hds,true);
  if hds>1 then begin
    diabox(63,5,'',x,y);
    readstring(x+3,y+2,getres(454),hdp^.betreff,40,BetreffLen,'',brk);  { 'neuer Betreff:' }
    closebox;
    if not brk then begin
      PGP_BeginSavekey;
      fn:=TempS(dbReadInt(mbase,'msgsize')+100);
      assign(f,fn);
      rewrite(f,1);
      { ClearPGPflags(hdp); }
      hdp^.orgdate:=true;
      WriteHeader(hdp^,f,reflist);
      XreadF(hds,f);   { den Nachrichtentext anhÑngen ... }
      close(f);
      Xwrite(fn);
      erase(f);
      wrkilled;
      TruncStr(hdp^.betreff,40);
      dbWriteN(mbase,mb_betreff,hdp^.betreff);
      PGP_EndSavekey;
      aufbau:=true;
      xaufbau:=true;
      end;
    end;
  dispose(hdp);
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
var fn   : pathstr;
    fn2  : pathstr;
    f,f2 : file;
    hdp  : headerp;
    hds  : longint;
    typ  : char;
    l    : longint;
begin
  dbReadN(mbase,mb_typ,typ);
  if typ='B' then
  begin
    rfehler(423);   { 'Bei BinÑrdateien nicht mîglich' }
    exit;
  end
  else if ((typ='M') or (dbReadInt(mbase,'flags') and 4<>0)) then
  begin
    rfehler(449);   { 'Bei MIME-Multipart-Nachrichten nicht mîglich.' }
    exit;
  end
  else if dbReadInt(mbase,'netztyp') and $8000<>0 then
  begin
    rfehler(451);   { 'Bei Nachrichten mit Kommentar nicht mîglich.' }
    exit;
  end;
  if testuvs(getres(455)) then exit;   { 'Edit' }
  new(hdp);
  ReadHeader(hdp^,hds,true);           { Header einlesen }
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
    hdp^.groesse:=_filesize(fn);
    dbWriteN(mbase,mb_groesse,hdp^.groesse);
    hdp^.charset:='';
    { ClearPGPflags(hdp); }
    hdp^.orgdate:=true;
    WriteHeader(hdp^,f2,reflist); { ..Header in neues Tempfile.. }
    reset(f,1);
    fmove(f,f2);                  { ..den Text dranhÑngen.. }
    close(f); erase(f);
    close(f2);
    dbReadN(mbase,mb_netztyp,l);
    l:=l and (not $2000);         { ISO-Codierung abschalten }
    dbWriteN(mbase,mb_netztyp,l);
    Xwrite(fn2);                  { ..und ab in die Datenbank. }
    erase(f2);
    wrkilled;
    PGP_EndSavekey;
    aufbau:=true;                 { wg. geÑnderter Grî·e }
    end;
  dispose(hdp);
end;


procedure ModiRot13;
var ablg   : byte;
    adr    : longint;
    f      : file;
    l,size : longint;
    p      : pointer;
    ps  : word;
    rr: word;
    typ    : char;
begin
  dbReadN(mbase,mb_typ,typ);
  if typ in ['B','M'] then begin
    rfehler(423);   { 'Bei BinÑrdateien nicht mîglich' }
    exit;
    end;
  if testuvs(getres(453)) then exit;   { 'éndern' }
  dbReadN(mbase,mb_ablage,ablg);
  dbReadN(mbase,mb_adresse,adr);
  dbReadN(mbase,mb_groesse,size);
  assign(f,aFile(ablg));
  reset(f,1);
  if (size=0) or (adr+size>filesize(f)) then begin
    rfehler1(424,strs(ablg));   { 'Nachricht ist beschÑdigt  (Ablage %s)' }
    close(f);
    end
  else
  begin
    ps:=min(maxavail,32768);
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
    rfehler(425)   { 'Bei unversandten Nachrichten leider nicht mîglich.' }
  else begin
    dbReadN(mbase,mb_typ,c);
    dbReadN(mbase,mb_flags,flags);

    if (c='M') or ((flags and 4)<>0) then
    begin                           { MIME -> Text }
      c:='T';
      flags:=flags and not 4;
    end else
    if c='T' then
      c:='B'                        { Text -> Bin  }
    else 
    begin			    { Bin -> MIME  }
      flags:=flags or 4;            
      c:='M';
    end;

    dbWriteN(mbase,mb_flags,flags);
    dbWriteN(mbase,mb_typ,c);
    aufbau:=true;
    end;
end;

procedure ModiGelesen;                    {Nachricht-Gelesen status aendern}
var b     : byte;
    brett : string[5];
begin
  if not dbBOF(mbase) then
  begin                                   {Nur Wenn ueberhaupt ne Nachricht gewaehlt ist...}
    dbReadN(mbase,mb_gelesen,b);
    if b=1 then b:=0 else b:=1;
    dbWriteN(mbase,mb_gelesen,b);
    dbReadN(mbase,mb_brett,brett);
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
var fn  : pathstr;
    s   : string;
    t   : text;
    x,y : byte;
    n   : longint;
    useclip: boolean;
begin
  fn:='*.*';
  useclip:=true;
  if ReadFilename(getres2(456,1),fn,true,useclip) then   { 'Brettliste einlesen' }
    if not exist(fn) then
      fehler(getres2(456,2))   { 'Datei nicht vorhanden!' }
    else begin
      msgbox(30,5,'',x,y);
      wrt(x+3,y+2,getres2(456,3));   { 'Bretter anlegen...' }
      n:=0;
      assign(t,fn);
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        makebrett(s,n,DefaultBox,ntBoxNetztyp(DefaultBox),true);
        gotoxy(x+22,y+2); write(n:5);
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
var fn  : pathstr;
    adrb: boolean;
    brk : boolean;
    s   : string;
    t   : text;
    x,y : byte;
    n   : longint;
    useclip: boolean;
    b   : byte;
begin
  fn:='*.*';
  useclip:=true;
  if ReadFilename(getres2(456,11),fn,true,useclip) then   { 'Userliste einlesen' }
    if not exist(fn) then
      fehler(getres2(456,2))   { 'Datei nicht vorhanden!' }
    else begin
      dialog(38,3,'',x,y);
      adrb:=true;
      maddbool(3,2,getres2(456,12),adrb);   { 'User in Adre·buch eintragen' }
      readmask(brk);
      enddialog;
      if brk then exit;
      msgbox(34,5,'',x,y);
      wrt(x+3,y+2,getres2(456,13));   { 'Userbretter anlegen...' }
      n:=0;
      assign(t,fn);
      reset(t);
      b:=0;
      while not eof(t) do begin
        readln(t,s);
        s:=trim(s);
        if cpos('@',s)>0 then begin
          dbSeek(ubase,uiName,ustr(s));
          if not dbFound then begin
            inc(n);
            makeuser(s,DefaultBox);
            if not adrb then dbWriteN(ubase,ub_adrbuch,b);
            gotoxy(x+26,y+2); write(n:5);
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
var fname   : pathstr;
    t       : text;
    d       : DB;
    x,y,xx  : byte;
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
  var kom : string[30];
  begin
    dbRead(d,'kommentar',kom);
    if exkom and (kom<>'') then
      komform:=forms(s,80)+kom
    else
      komform:=s;
  end;
 
  procedure getbrettinfos;
  var x1,y1 : byte;
  begin
    sortadress:=false; sortbox:=Usersortbox;  exkom:=false;
                                              { 'Brettliste erzeugen' }
    dialog(max(max(length(getres2(457,7)),length(getres2(457,10))),
                   length(getres2(457,4)))+9,6,getres2(457,2),x1,y1);
    maddbool(2,2,getres2(457,7),SortAdress);  { 'nach Gruppen ordnen'}
    MSet1Func(bool_brettgruppe);
    maddbool(2,3,getres2(457,10),SortBox);    { 'Sortierung aus BrettÅbersicht beibehalten'}
    MSet1Func(bool_brettindex);    
    maddbool(2,5,getres2(457,4),exkom);       { 'auch Kommentare exportieren' }
    readmask(brk);
    enddialog;
  end;
 
  procedure getuserinfos;
  var x1,y1 : byte;
  begin
    sortadress:=true; sortbox:=Usersortbox; Onlyadress:=true; exkom:=false;
    dialog(max(max(max(length(getres2(457,7)),length(getres2(457,8))),
                       length(getres2(457,9))),length(getres2(457,4)))+9,
           7,getres2(457,1),x1,y1);           { 'Userliste erzeugen' }
    maddbool(2,2,getres2(457,7),SortAdress);  { 'nach Gruppen ordnen'}
    maddbool(2,3,getres2(457,8),SortBox);     { 'nach Serverbox sortieren'}
    maddbool(2,4,getres2(457,9),OnlyAdress);  { 'nur User im Adre·buch exportieren' }
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
      fehler(getres2(457,3))   { 'ungÅltiger Dateiname' }
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
      wrt(x+3,y+2,getres2(457,iif(user,5,6)));  { 'erzeuge User-/Brettliste...     %' }
      xx:=wherex-5;
      if not multipos(':\',fname) then fname:=ExtractPath+fname;
      assign(t,fname);
      rewrite(t);
      cnt:=dbRecCount(d); n:=0; ab1:=-1; sa1:='';
      while not dbEOF(d) do begin 
        attrtxt(col.colmboxhigh);
        gotoxy(xx,y+2); write(n*100 div cnt:3);
        if user then
        begin
          s:=dbReadStrN(ubase,ub_username);
          ab:=dbreadint(ubase,'adrbuch');
          sa:=dbReadStrN(ubase,ub_pollbox);
          if (dbReadInt(ubase,'userflags') and 4=0) AND       { keine Verteiler }
           (left(s,4)<>#0+'$/T') AND                          { keine Trennzeile }
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
          if left(s,3)='$/T'           { keine Trennzeile }
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


procedure zeige_unversandt;
var _brett   : string[5];
    _mbrett  : string[5];
    sr       : searchrec;
    f        : file;
    hdp      : headerp;
    hds      : longint;
    ok       : boolean;
    adr,fsize: longint;
    box      : string[BoxNameLen];
    uvf      : boolean;
    uvs      : byte;
    mtyp     : char;
    ntyp     : longint;
    zconnect : boolean;
    crashs   : boolean;

  procedure fehlt;
  begin
    rfehler(426);   { 'Nachricht ist nicht mehr in der Datenbank vorhanden!' }
  end;

begin
  if uvs_active then exit;
  crashs:=false;
  findfirst('*.pp',dos.Archive,sr);
  if doserror<>0 then
  begin
    FindClose(sr);
    findfirst('*.cp',dos.Archive,sr);
    crashs:=true;
  end;
  markanz:=0;
  moment;
  new(hdp);
  while doserror=0 do begin
    if crashs then begin
      box:=strs(hexval(left(sr.name,4)))+'/'+strs(hexval(copy(sr.name,5,4)));
      { ^^^ nur fÅr Anzeige bei fehlerhaftem CP }
      zconnect:=true;
      end
    else begin
      box:=file_box(nil,left(sr.name,cpos('.',sr.name)-1));
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
      makeheader(zconnect,f,1,0,hds,hdp^,ok,false);
      if not ok then
        rfehler1(427,box)   { 'fehlerhaftes Pollpaket:  %s' }
      else with hdp^ do begin
        _brett:='';
        if (cpos('@',empfaenger)=0) and
           ((netztyp<>nt_Netcall) or (left(empfaenger,1)='/'))
        then begin
          dbSeek(bbase,biBrett,'A'+ustr(empfaenger));
          if not dbFound then fehlt
          else _brett:='A'+dbLongStr(dbReadInt(bbase,'int_nr'));
          end
        else begin
          dbSeek(ubase,uiName,ustr(empfaenger+
                 iifs(cpos('@',empfaenger)>0,'','@'+box+'.ZER')));
          if not dbFound then fehlt
          else _brett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
          end;
        if _brett<>'' then begin
          dbSeek(mbase,miBrett,_brett+#255);
          uvf:=false;
          if dbEOF(mbase) then dbGoEnd(mbase)
          else dbSkip(mbase,-1);
          if not dbEOF(mbase) and not dbBOF(mbase) then
            repeat
              dbReadN(mbase,mb_brett,_mbrett);
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
            fehlt;
          end;
        inc(adr,groesse+hds);
        end;
      end;
    close(f);
    findnext(sr);
    if (doserror<>0) and not crashs then
    begin
      FindClose(sr);
      findfirst('*.cp',dos.Archive,sr);
      crashs:=true;
    end;
  end;
  FindClose(sr);
  dispose(hdp);
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
var hdp   : headerp;
    hds   : longint;
    i     : integer;
    x,y  : byte;
    dat   : datetimest;
    anz   : byte;
    xxs   : array[1..20] of string[65];
    netz  : string[20];
    p     : byte;
    elist : boolean;    { mehrere EmpfÑnger }
    rlist : boolean;    { mehrere References }
    t     : taste;
    s     : atext;

  procedure apps(nr:word; s:string);
  begin
    inc(anz);
    xxs[anz]:=getres2(459,nr)+' '+left(s,53);
  end;

  function ddat:string;
  begin
    with hdp^ do
      if ddatum='' then
        ddat:=''
      else
        ddat:=', '+copy(ddatum,7,2)+'.'+copy(ddatum,5,2)+'.'+left(ddatum,4)+
              ', '+copy(ddatum,9,2)+':'+copy(ddatum,11,2)+':'+copy(ddatum,13,2);
  end;

  procedure empfliste;   { Fenster mit EmpfÑngerliste }
  var ml  : byte;
      i,j : integer;
      x,y : byte;
  begin
    ml:=length(getres2(459,30))+8;
    with hdp^ do begin
      for i:=1 to empfanz do begin
        ReadHeadEmpf:=i;
        ReadHeader(hdp^,hds,false);
        ml:=max(ml,length(empfaenger)+6);
        end;
      ml:=min(ml,72);
      i:=min(empfanz,screenlines-8);
      msgbox(ml,i+4,getres2(459,30),x,y);   { 'EmpfÑngerliste' }
      for j:=1 to i do begin
        ReadHeadEmpf:=j;
        ReadHeader(hdp^,hds,false);
        mwrt(x+3,y+1+j,left(empfaenger,72));
        end;
      wait(curoff);
      if rlist and (ustr(lastkey)='R') then keyboard('R');
      closebox;
      end;
  end;

  procedure refliste;   { Fenster mit Referenzliste }
  var ml  : byte;
      i,j : integer;
      x,y : byte;
      p   : refnodep;
    procedure writeref(p:refnodep);
    begin
      wrt(x+3,j,left(p^.ref,72));
      inc(j);
    end;
    procedure showrefs(list:refnodep);
    begin
      if list<>nil then begin
        showrefs(list^.next);
        writeref(list);
        end;
    end;
  begin
    ml:=length(getres2(459,31))+8;
    with hdp^ do begin
      p:=reflist;
      while p<>nil do begin
        ml:=max(ml,length(p^.ref)+6);
        p:=p^.next;
        end;
      ml:=max(ml,length(ref)+6);
      ml:=min(ml,72);
      i:=min(refanz,screenlines-8);
      msgbox(ml,i+4,getres2(459,31),x,y);   { 'EmpfÑngerliste' }
      j:=y+2;
      showrefs(reflist);
      wrt(x+3,y+i+1,left(ref,72));
      wait(curoff);
      if elist and (ustr(lastkey)='E') then keyboard('E');
      closebox;
      end;
  end;

  function typstr(typ,mimetyp:string):string;
  begin
    if mimetyp<>'' then
      typstr:=extmimetyp(mimetyp)
    else begin
      UpString(typ);
      if typ='T' then typstr:=typ+getres2(459,1) else   { '  (Text)' }
      if typ='B' then typstr:=typ+getres2(459,2) else   { '  (binÑr)' }
      typstr:=typ;
      end;
  end;

begin
  new(hdp);
  ReadHeader(hdp^,hds,true);
  anz:=0;
  with hdp^ do begin
    apps(3,empfaenger);
    if fido_to<>'' then apps(4,fido_to);
    apps(5,betreff);
    apps(6,left(absender,53));
    if realname<>'' then apps(7,realname);
    if organisation<>'' then apps(8,left(organisation,53));
    if PmReplyTo<>'' then apps(9,left(PmReplyTo,53));
    apps(10,iifs(ntZDatum(netztyp),zdatum,datum)+
         iifs(datum<>'','  ('+fdat(datum)+', '+ftime(datum)+
         iifs(ntSec(netztyp),':'+copy(zdatum,13,2),'')+')',''));
    apps(11,left(pfad,53));
    repeat
      pfad:=mid(pfad,54);
      if pfad<>'' then apps(12,left(pfad,53));
    until pfad='';
    if msgid<>''    then apps(13,msgid);
    if ref<>''      then apps(14,ref);
    if pm_bstat<>'' then apps(15,pm_bstat);
    apps(16,typstr(typ,mimetyp));
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
    if netztyp in [nt_UUCP,nt_Client] then netz:=' / RFC'
    else begin
      netz:=ntName(netztyp);
      if netz='???' then netz:=''
      else netz:=' / '+netz;
      end;
    msgbox(70,anz+7,getres2(459,23)+' ('+            { 'Nachrichtenkopf' }
                    getres2(459,iif(ntZConnect(netztyp),24,25))+netz+')',x,y);
    moff;
    for i:=1 to anz do begin
      if left(xxs[i],1)=' ' then p:=0
      else p:=cpos(':',xxs[i]);
      if p>0 then begin
        attrtxt(col.colmboxhigh);
        wrt(x+3,y+i+1,left(xxs[i],p));
        end;
      attrtxt(col.colmbox);
      wrt(x+3+p,y+i+1,mid(xxs[i],p+1));
      end;
    attrtxt(col.colmboxhigh); wrt(x+3,y+anz+3,getres2(459,26));  { 'Grî·e des Kopfes: ' }
    attrtxt(col.colmbox);     write(hds,getres(13));
    dat:=longdat(dbReadInt(mbase,'empfdatum'));
    if smdl(IxDat('2712300000'),IxDat(dat)) then
      dat:=longdat(dbReadInt(mbase,'wvdatum'));
    attrtxt(col.colmboxhigh); wrt(x+40,y+anz+2,getres2(459,27));  { 'Empfangsdatum: ' }
    attrtxt(col.colmbox);     write(fdat(dat));
    attrtxt(col.colmboxhigh); wrt(x+40,y+anz+3,getres2(459,28));  { 'Ablagedatei  :' }
    attrtxt(col.colmbox);     write('MPUFFER.',dbReadInt(mbase,'ablage'));
    elist:=(empfanz>1);
    rlist:=(refanz>1);
    if elist then s:=' (E='+getres2(459,30)
    else s:='';
    if rlist then begin
      if s<>'' then s:=s+', '
      else s:=' (';
      s:=s+'R='+getres2(459,31);
      end;
    if s<>'' then s:=s+')';
    wrt(x+3,y+anz+5,getres2(459,29)+s+' ...');    { Taste drÅcken / E=EmpfÑngerliste / R=Referenzliste }
    mon;
    x:=wherex; y:=wherey;
    repeat
      gotoxy(x,y);
      repeat
        get(t,curon);
      until (t<mausfirstkey) or (t>mauslastkey) or (t=mausleft) or (t=mausright);
      if elist and (ustr(t)='E') then empfliste;
      if rlist and (ustr(t)='R') then refliste;
    until (not elist or (ustr(t)<>'E')) and (not rlist or (ustr(t)<>'R'));
    end;
  closebox;
  freeres;
  dispose(hdp);
end;


procedure ShowHeader;            { Header direkt so anzeigen wie er im PUFFER steht }
var fn  : pathstr;
    f   : file;
    hdp : headerp;
    hds : longint;
    lm  : Byte;                  { Makrozwischenspeicher... }
begin
  new(hdp);
  ReadHeader(hdp^,hds,true);
  if hds>1 then begin
    fn:=TempS(dbReadInt(mbase,'msgsize')+1000);
    assign(f,fn);
    rewrite(f,1);
    XreadF(0,f);
    seek(f,hds);
    truncate(f);
    close(f);
    lm:=listmakros;                         { Aktuelle Makros merken,       }
    listmakros:=16;                         { Archivviewermakros aktivieren }
    ListFile(fn,getres(460),true,false,0);  { 'Nachrichten-Header'          }
    listmakros:=lm;                         { wieder alte Makros benutzen   }
    _era(fn);
    end;
  dispose(hdp);
end;


{ Es wird nicht im Temp-, sondern im XP-Verzeichnis entpackt! }
{ exdir='' -> Lister/ArcViewer;  exdir<>'' -> Xtrakt          }
{ Fehler -> exdir:=''                                         }

procedure ShowArch(var fn:string);   { 'var' wegen Stackplatz }
var decomp    : string;
    p         : byte;
    datei     : string;
    newarc    : longint;
    atyp      : shortint;
    spath     : pathstr;
    ats       : shortint;
    viewer    : viewinfo;
    wasLFN,
    extractOK : boolean;
label ende;
begin
  wasLFN:=LFNEnabled; extractOK:=false;
  ats:=arctyp_save;
  atyp:=abuf[arcbufp]^.arcer_typ;
  if atyp>arctypes then exit;  { ??? }
  if not getDecomp(atyp,decomp) then
    exdir:=''
  else begin
    p:=pos('$DATEI',ustr(decomp));
    datei:=trim(mid(fn,80));
    if (exdir='') and ((temppath='') or (ustr(temppath)=ownpath))
      and exist(datei) then begin
        rfehler(428);   { 'Entpacken nicht mîglich - bitte Temp-Verzeichnis angeben!' }
        exit;
        end
    else if ((exdir<>'') and exist(exdir+datei)) or
            ((exdir='') and exist(temppath+datei)) then
      if exdir=ownpath then begin
        rfehler(429);  { 'Datei schon vorhanden - bitte Extrakt-Verzeichnis angeben!' }
        exit;
        end
      else                            { '%s existiert schon. öberschreiben' }
        if not ReadJN(getreps(461,'"'+fitpath(iifs(exdir<>'',exdir,temppath)+datei,38)+'"'),false)
        then exit
        else
          _era(iifs(exdir<>'',exdir,temppath)+datei);
    decomp:=copy(decomp,1,p-1)+'"'+datei+'"'+mid(decomp,p+6);
    p:=pos('$ARCHIV',ustr(decomp));
    decomp:=copy(decomp,1,p-1)+abuf[arcbufp]^.arcname+mid(decomp,p+7);
    if length(decomp) > 127 then
    begin
      rfehler(452);  { 'Entpacken nicht mîglich - Entpacker-Aufruf lÑnger als 127 Zeichen.' }
      goto ende;
    end;
    spath:=ShellPath;
    if exdir<>'' then GoDir(exdir)
    else GoDir(temppath);
    shell(decomp,400,3);
    if exdir='' then
    begin
      datei:=mid(datei,rightpos('\',datei)+1);
      { !?! GoDir(temppath);   { wurde durch Shell zurÅckgesetzt }
      if exist(temppath+datei) then
        extractOK:=true
      else                     { Entpacker evtl. nicht LFN-fÑhig?   }
        if wasLFN then
        begin
          DisableLFN;          { dann ohne LFN-Support versuchen... }
          if exist(temppath+datei) then extractOK:=true;
        end;
      if not extractOK then rfehler(430)  { 'Datei wurde nicht korrekt entpackt.' }
      else begin
        newarc:=ArcType(TempPath+datei);
        if ArcRestricted(newarc) then newarc:=0;
        if newarc=0 then begin
          GetExtViewer(datei,viewer);
          if viewer.prog='' then TestGifLbmEtc(datei,false,viewer);
          if (viewer.prog<>'') and (viewer.prog<>'*intern*') then
            ViewFile(TempPath+datei,viewer,false)
          else
            ListFile(TempPath+datei,fitpath(datei,iif(listuhr,33,40)),true,false,0);
          end
        else
          if memavail<20000 then
            rfehler(431)   { 'zu wenig Speicher!' }
          else if arcbufp=max_arc then
            rfehler(432)   { 'Maximal 3 verschachtelte Archive mîglich!' }
          else begin
            decomp:=TempPath+datei;  { Stack sparen ... }
            if ViewArchive(decomp,newarc)<>0 then;
            end;
        if exist(temppath+datei) then
          _era(temppath+datei);
        end;
      { GoDir(OwnPath); }
      end;
    if (not LFNEnabled) and wasLFN then EnableLFN;
    ShellPath:=spath;
    end;
  ende:
  arctyp_save:=ats;
  attrtxt(col.colarcstat);
  wrt(77,4,arcname[ats]);
  keyboard(keydown);
end;


function a_getfilename(nr,nn:byte):pathstr;
var fn   : pathstr;
    sex  : pathstr;
begin
  fn:=trim(mid(get_selection,80));
  sex:=exdir; exdir:=TempPath;
  ShowArch(fn);
  exdir:=sex;
  a_getfilename:=TempPath+fn;
end;


procedure ArcSpecial(var t:taste);
var s   : string;
    dp  : pathstr;
    x,y : byte;
    brk : boolean;
    fk  : string[32];
    sex : pathstr;
    dd  : string[30];
begin
  if ustr(t)='X' then begin
    dp:=ExtractPath;
    dd:=getres(463);
    dialog(47+length(dd),3,getres(462),x,y);   { 'Extrakt' }
    maddstring(3,2,dd,dp,40,79,'');
    readmask(brk);
    enddialog;
    if brk then exit;
    UpString(dp);
    if (dp<>'') and (right(dp,1)<>':') and (right(dp,1)<>'\') then
      dp:=dp+'\';
    if not validfilename(dp+'test.$$1') then
      rfehler(433)   { 'ungÅltiges Verzeichnis' }
    else begin
      sex:=exdir;
      exdir:=dp;
      s:=first_marked;
      while (s<>#0) and (exdir<>'') do begin
        ShowArch(s);
        s:=next_marked;
        end;
      exdir:=sex;
      end;
    end
  else begin
    getfilename:=a_getfilename;
    fk:=forwardkeys; forwardkeys:='';
    if test_fkeys(t) then;
    keyboard(fk);
    xp1o.listext(t);
    end;
end;


{ 0=Esc, 1=minus, 2=plus }

function ViewArchive(var fn:pathstr; typ:shortint):shortint;
var ar   : ArchRec;
    brk  : boolean;
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
        prozent:=strsrn(CompSize/OrgSize*100,3,1)
      else
        prozent:='     ';
  end;

  procedure renameDWC;
  var f  : file;
      _d : dirstr;
      _n : namestr;
      _e : extstr;
  begin
    assign(f,fn);
    fsplit(fn,_d,_n,_e);
    fn:=_d+'temp$$.dwc';
    rename(f,fn);
  end;

begin
  if abs(typ)=ArcDWC then
    renameDWC;
  OpenList(1,screenwidth,5,screenlines-fnkeylines-1,1,'/NS/SB/M/{NLR/}');
  OpenArchive(fn,typ,ar);
  listcrp(ShowArch);
  listtp(ArcSpecial);
  showkeys(11);
  attrtxt(col.colarcstat);
  mwrt(1,4,forms(getres(464),80));   { ' Name            OrgGrî·e  CompGrî·e    %    Methode    Datum    Uhrzeit' }
  inc(arcbufp);
  new(abuf[arcbufp]);
  with ar do begin
    arctyp_save:=arctyp;
    abuf[arcbufp]^.arcer_typ:=arctyp;
    abuf[arcbufp]^.arcname:=fn;
    mwrt(77,4,arcname[arctyp]);
    while not ende do begin
      if (name<>'') or (path='') then begin
        lm:=rightpos('.',name); if lm=0 then lm:=length(name);
        app_l(forms(iifc(path<>'','*',' ')+forms(left(name,min(8,lm-1))+forms(mid(name,lm),4),12)
              +strsn(orgsize,11)+strsn(compsize,11)+'   '+ prozent+'  '+forms(method,10)+
              dt(datum,uhrzeit),80)+path+name)
      
      end else

        app_l('*'+path);
      ArcNext(ar);
      end;
    end;
  CloseArchive(ar);
  exdir:='';
  llh:=true; ListCtrlWdisabled:=true; listexit:=0;
  lm:=ListMakros; ListMakros:=16;
  pushhp(67);
  list(brk);
  pophp;
  ListMakros:=lm;
  ListCtrlWdisabled:=false;
  dispose(abuf[arcbufp]);
  dec(arcbufp);
  CloseList;
  attrtxt(col.colkeys);
  mwrt(1,2,sp(80));
  showlastkeys;
  if abs(typ)=ArcDWC then
    _era(fn);
  aufbau:=true;
  ViewArchive:=listexit;
end;

procedure FileArcViewer(fn:pathstr);
var useclip : boolean;
    arc     : shortint;
    lm      : byte;
begin
  if (fn='') or multipos('?*',fn) then begin
    if fn='' then fn:='*.*';
    useclip:=false;
    if not ReadFilename(getres(465),fn,true,useclip) then   { 'Archivdatei' }
      exit;
    fn:=FExpand(fn);
    end;
  if exist(fn) then begin
    arc:=ArcType(fn);
    if ArcRestricted(arc) then arc:=0;
    if arc=0 then begin                     { Wenn's kein Archiv war...     }
      lm:=listmakros;
      listmakros:=16;                       { Archivviewer-Makros benutzen! }
      listfile(fn,fn,true,false,0);         { und File einfach nur anzeigen }
      listmakros:=lm;
      end
      { rfehler(434)   { 'keine Archivdatei' }
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
    x,y   : byte;
    last,
    next  : string[30];
    flags : byte;
    log   : text;
    rec,rec2 : longint;

  procedure show;
  begin
    wrt(x+22,y+3,strsn(n,7));
    wrt(x+22,y+4,strsn(ll,7));
  end;

  procedure log_it;
  var _brett : string[5];
  begin
    dbRead(d,'brett',_brett);
    write(log,fdat(longdat(dbReadInt(d,'origdatum'))),' ');
    if _brett[1]='U' then
      write(log,forms(dbReadStr(d,'absender'),32))
    else begin
      dbSeek(bbase,biIntnr,copy(_brett,2,4));
      if dbFound then write(log,forms(copy(dbReadStrN(bbase,bb_brettname),2,40),32));
      end;
    writeln(log,' ',left(dbReadStr(d,'betreff'),37));
  end;

begin
  if fileio.diskfree(0)<_filesize(MsgFile+dbExt)*2 then begin
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
  if exist(DupeFile+dbIxExt) then
    _era(DupeFile+dbIxExt);
  closebox;
  dbOpen(d,DupeFile,1);   { indizieren }
  n:=1; ll:=0;
  msgbox(32,8,getres2(466,2),x,y);   { 'DupeKill' }
  wrt(x+3,y+2,getres2(466,3));       { 'Nachrichten gesamt:' }
  wrt(x+3,y+3,getres2(466,4));       { '        bearbeitet:' }
  wrt(x+3,y+4,getres2(466,5));       { '          gelîscht:' }
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
  era(MsgFile+dbExt);
  assign(f1,DupeFile+dbExt); rename(f1,MsgFile+dbExt);
  era(DupeFile+dbIxExt);
  writeln(log);
  close(log);
{$IFDEF BP }
  FlushSmartdrive(true);
{$ENDIF }
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
    fn : pathstr;
    s  : string;
begin
  if dbReadInt(mbase,'typ')=ord('B') then
    rfehler(436)   { 'Drucken nicht mîglich - BinÑrnachricht' }
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


{ Ausgabe true -> UserMode umschalten }

Procedure Brettmarksuche;
const suchst  : string[40] = '';
var   x,y     : byte;
      brk     : boolean;
      nn,n,nf : longint;
      bname   : string[BrettLen];
      spos    : longint;
      rec     : longint;
      m1,m2,j : longint;
      found, found_not : boolean;
begin
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
  while not dbEOF(bbase) and not brk do begin
    inc(n);
    gotoxy(x+13,y+4); write(n*100 div nn:3);
    gotoxy(x+35,y+4); write(nf:4);
    dbReadN(bbase,bb_brettname,bname);
    j:=0;
    repeat
      suchst:=left(copy(sst,seekstart[j],seeklen[j]),40);
      found:=((igcase and (pos(suchst,ustr(bname))>0)) or
       (not igcase and (pos(suchst,bname)>0)));
      found_not:=found and seeknot[j];
      if suchand and not found and seeknot[j] then found:=true;
      inc(j);
    until (j=suchanz) or (suchand xor found) or found_not;
    if found_not then found:=false;
    if found then begin
      UBAddmark(dbRecno(bbase));
      dbreadN(bbase,bb_index,m2);
      if m2<m1 then begin
        m1:=m2;
        spos:=dbRecno(bbase);
        end;
      inc(nf);
      end;
    dbNext(bbase);
    if n mod 16=0 then testbrk(brk);
    end;
  if m1=maxlongint then dbGo(bbase,rec)
    else dbGo(bbase,spos);    
  aufbau:=true;
  closebox;
  if not brk and (nf=0) then fehler(getres2(467,6));  { 'keine passenden User gefunden' }
  freeres;
end;


function UserMarkSuche(allmode:boolean):boolean;
const suchst  : string[40] = '';
var   x,y     : byte;
      brk     : boolean;
      nn,n,nf : longint;
      uname,
      sname   : string[AdrLen];
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
  begin
    attrtxt(col.coldialog);
    wrt(x+3,y+4,getres2(467,3));   { 'Suchen...     %        gefunden:' }
    nn:=dbRecCount(ubase); n:=0; nf:=0;
    mi:=dbGetIndex(ubase); dbSetIndex(ubase,0);
    dbGoTop(ubase);
    attrtxt(col.coldiahigh);
    while not dbEOF(ubase) and not brk do begin
      inc(n);
      gotoxy(x+13,y+4); write(n*100 div nn:3);
      gotoxy(x+35,y+4); write(nf:4);
      dbReadN(ubase,ub_username,uname);
      j:=0;
      repeat
        suchst:=left(copy(sst,seekstart[j],seeklen[j]),40);
        found:=((igcase and (pos(suchst,ustr(uname))>0)) or
         (not igcase and (pos(suchst,uname)>0)));
        found_not:=found and seeknot[j];
        if suchand and not found and seeknot[j] then found:=true;
        inc(j);
      until (j=suchanz) or (suchand xor found) or found_not;
      if found_not then found:=false;
      if found then begin
        UBAddmark(dbRecno(ubase));
        if not allmode and (dbReadInt(ubase,'adrbuch')=0) then
          UserMarkSuche:=true;
        if uname<sname then begin
          sname:=uname;
          spos:=dbRecno(ubase);
          end;
        inc(nf);
        end;
      dbNext(ubase);
      if n mod 16=0 then testbrk(brk);
      end;
    dbSetIndex(ubase,mi);
    if sname<>#255 then dbGo(ubase,spos)
    else dbGo(ubase,rec);
    aufbau:=true;
    end;
  closebox;
  if not brk and (nf=0) then fehler(getres2(467,4));  { 'keine passenden User gefunden' }
  freeres;
end;


procedure BrettInfo;
var i   : longint;
    x,y : byte;
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
    nts : string[20];
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
    p1,s,s1,t,u : string[80];
    v        : char;
    node     : string[20];
    secondtry,
    mark,
    lMagics  : boolean;
    dir      : dirstr;
    name     : namestr;
    ext      : extstr;

begin
  nnode := '';
  if not list_selbar and (list_markanz=0) then begin
    rfehler(438);   { 'keine Dateien markiert' }
    exit
  end;
  if not TestNodelist or not TestDefbox then exit;
  s := FMsgReqnode;
  p := cpos('.',s);
  if (p>0) then node:=left(s,p-1)
    else node:=s;
  files := '';
  u := ''; t := '';
  lMagics := Magics;
  secondtry:=false;
  s := first_marked;
  repeat
    { Von Anfang an leer oder Liste komplett durchlaufen und nichts gefunden,
      dann probieren wir's nochmal mit MAGICS }
    if (s=#0) then begin
      secondtry:=true;
      s := first_marked;
      lMagics:=true;
    end;
    while (s<>#0) do begin
      { --- komplett neu:oh (aus MultiReq uebernommen) --->> }
{     if (s='') then lMagics:=false; }

      { Usernamen vor Quotes killen }
      k:=cpos('>',s);
      if (k>0) then if (k<6) then delete(s,1,k);

      k:=0;
      if (s<>'') then
      while (k<byte(s[0])) do begin
        t:=''; v:=#0;
        { Nach dem ersten erlaubten Zeichen suchen }
        while (byte(s[0])>0)
        and not (s[1] in ['a'..'z','A'..'Z','0'..'9','@','!','$','^']) do begin
            v:=s[1];
            delete(s,1,1);
          continue
        end;
        { Vor dem Dateinamen mu· ein Trennzeichen stehen }
        if (v<>#0) then if not (v in [#32,'"','<','>','Ø','Æ','(','[','{',',',';',':','_','*']) then begin
          while (byte(s[0])>0)
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
        u:=UStr(t);
        delete(s,1,byte(t[0]));
        { Auf den Dateinamen mu· ein Trennzeichen folgen }
        if (byte(s[0])>0) then if not (s[1] in [#32,'"','<','>','Ø','Æ',')',']','}',',',';',':','_','*']) then continue;

        if (mark and (t[byte(t[0])] in ['_','*'])) then dec(byte(t[0]));

        while (byte(t[0])>0) and (t[byte(t[0])] in ['.','!','?','/']) do dec(byte(t[0]));
        if (byte(t[0])<2) then continue;
        k:=0;
        for ic:=1 to byte(t[0]) do if t[ic]='.' then inc(k);
        if (k>1) then continue;
        if (pos('**',t)>0) then continue;
        if not lMagics then if (cpos('.',t)<3) and (byte(t[0])<5) then continue;

        { Passwort suchen, erkennen und speichern }
        p1:='';
        ic:=cpos('/',t); if (ic>0) then begin
          p1:=copy(t,ic,20); delete(t,ic,99)
        end;

        u:=UStr(t);
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
        ic:=cpos('@',t); if (ic>1) and (ic<>byte(t[0])) then continue;

        { Auf Beschreibungs-Datei testen }
        FSplit(u,dir,name,ext);
        if (ext='.DIZ') then continue;
        if (name='FILES') or (name='FILE_ID') or (name='00GLOBAL')
          or (name='DESCRIPT') then continue;

        { Ist der String eine Versionsnummer? V42.3, 1.0, X75, V34B etc. }
        if (byte(t[0])<8) then begin
          u:=t;
          if (UStr(copy(u,1,3))='VOL') then delete(u,1,3);
          if (UpCase(u[1]) in ['V','X']) then delete(u,1,1);
          id:=0;
          for ic:=1 to length(u) do if not (UpCase(u[ic]) in ['0'..'9','.','A','B','G']) then id:=1;
          if (id=0) then continue
        end;

        { Ist der String eine Baudrate oder Dateigroesse? xx.xK, x in  [0..9] }
        if (length(t)<10) then begin
          u:=UStr(t);
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

        if (lMagics) then if (UStr(t)=t) then begin
          files:=files+' '+t;
          continue
        end;
        u := UStr(t);
        ic := cpos('.',u); if not (ic in [2..9]) then continue;
        if (length(u)<4) then continue;
        if (length(u)-ic>3) then continue;
        if (p1<>'') then u:=u+p1; p1:='';
        files:=files+' '+u;
        continue
      end; { while (k<byte(s[0])) }
      { <<--- komplett neu:oh (aus MultiReq uebernommen) --- }
      s:=next_marked;
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
    _brett : string[5];
begin
  _killit:=false;
  dbReadN(mbase,mb_unversandt,uv);
  if uv and 1<>0 then
    rfehler(439)   { 'Unversandte Nachricht mit "Nachricht/Unversandt/Lîschen" lîschen!' }
  else
    if not ask or ReadJN(getres(470)+   { 'Nachricht lîschen' }
      iifs(KK and HasRef,getres(471),''),true) then
    begin                            { ' (unterbricht Bezugsverkettung)' }
      if msgmarked then
        msgUnmark;
      wrkilled;
      dbReadN(mbase,mb_brett,_brett);
      DelBezug;
      dbDelete(mbase);
      if left(_brett,1)<>'U' then RereadBrettdatum(_brett);
      _killit:=true;
      aufbau:=true; xaufbau:=true;
      setbrettgelesen(_brett);
      end;
end;


end.
{
  $Log$
  Revision 1.47.2.44  2002/04/13 17:29:40  my
  MY:- Ressourcen fÅr erweiterte User- und Brett-Exportfunktionen (die
       offenbar schon seit lÑngerem implementiert waren) ergÑnzt und
       Breite der Dialogboxen an Ressourcen angepa·t.

  Revision 1.47.2.43  2002/04/12 14:34:16  my
  JG+MY:- Wortumbruch-Umschaltung im Lister (<Ctrl-W>) intern komplett
          umgebaut: Die Repeat-Schleife wird jetzt direkt in xp1s.listfile
          durchlaufen statt explizit bei jedem Routinenaufruf von
          listfile angegeben werden zu mÅssen.

  Revision 1.47.2.42  2002/04/09 21:08:47  my
  JG:- Fix Archiv-Viewer: Bei Dateinamen, die keinen Punkt enthielten,
       wurden innerhalb eines Archivs nur die ersten vier Stellen
       angezeigt.

  Revision 1.47.2.41  2002/03/27 19:47:06  my
  MY:- Fix Archiv-Viewer: Lange Dateinamen wurden nicht korrekt an den
       Entpacker Åbergeben und der Entpacker konnte daher die Datei im
       Archiv nicht finden (Code von JG am 09.03.2002 nicht vollstÑndig
       eingebaut).

  MY:- Fix Archiv-Viewer: Wenn XP unter einem LFN-fÑhigen Betriebssystem
       lief und die zu entpackende Datei einen langen Dateinamen hatte,
       jedoch ein nicht LFN-fÑhiger Entpacker wie PKUNZIP v2.04g verwendet
       wurde, dann wurde die Datei zwar (mit einem kurzen Dateinamen)
       entpackt, aber von XP nicht angezeigt ("Datei wurde nicht korrekt
       entpackt"). XP hatte ausschlie·lich nach dem langen Dateinamen
       gesucht und konnte daher die mit dem kurzen Dateinamen entpackte
       Datei nicht finden.

  MY:- Fix Archiv-Viewer: Entpacker-Aufrufe, die aufgrund der LÑnge des
       Dateinamens der zu entpackenden Datei lÑnger als 127 Zeichen
       wurden, werden jetzt abgefangen (statt erst den Entpacker
       aufzurufen und anschlie·end je eine Fehlermeldung vom Entpacker und
       von XP zu kassieren).

  Revision 1.47.2.40  2002/03/13 23:11:12  my
  SV[+MY]:- Bei Nachrichten mit KOM-Header wird ein éndern des Textes via
            N/é/T jetzt verhindert.

  Revision 1.47.2.39  2002/03/10 15:59:59  my
  JG:- Rechts-/Links-Scrolling im Archiv-Viewer aktiviert. Dadurch kann
       bei Dateien, die in einem Unterverzeichnis des Archivs liegen
       und/oder die einen langen Dateinamen haben, jetzt der vollstÑndige
       Pfad- und Dateiname betrachtet werden.

  Revision 1.47.2.38  2002/03/10 13:52:50  my
  JG+MY:- Kleine Korrektur bei der Anzeige (bzw. dem Entpacken) von
          Dateien mit langen Dateinamen (Entpacker-Parameter nicht mehr in
          AnfÅhrungszeichen einschlie·en).

  Revision 1.47.2.37  2002/03/09 21:52:20  my
  JG:- Einige kleinere Korrekturen bei der Anzeige von LFN-Dateinamen in
       Archiven vorgenommen und die Anzeige von Dateien, die sich in einem
       Unterverzeichnis des Archivs befinden, implementiert.

  Revision 1.47.2.36  2002/03/08 23:03:22  my
  MK:- Kleine Codeoptimierung/Variableneinsparung.

  MY:- Fix: Umschaltung des Wortumbruchs im Lister mit <Ctrl-W>
       funktioniert jetzt auch in der Anzeige des Nachrichtenkopfs ("o"),
       in Nachrichten mit KOM-Header und bei der Anzeige von Dateien in
       Archiven. Funktion bei der Anzeige des Archivinhalts deaktiviert
       (weil dort vîllig ÅberflÅssig und nur hinderlich).

  MY:- Fix: Nachricht/éndern/Text wird jetzt auch bei "alten" MIME-
       Multipart-Nachrichten vom Typ 'T' verhindert. Die entsprechende
       Fehlermeldung hat jetzt einen zutreffenden Text.

  Revision 1.47.2.35  2002/01/30 17:44:37  mk
  - const-parameter fuer dbReadXX verwenden

  Revision 1.47.2.34  2001/12/20 15:22:14  my
  MY+MK:- Umstellung "RFC/Client" auf neue Netztypnummer 41 und in der
          Folge umfangreiche Code-Anpassungen. Alte RFC/Client-Boxen
          mÅssen einmal manuell von RFC/UUCP wieder auf RFC/Client
          umgeschaltet werden.

  Revision 1.47.2.33  2001/12/07 17:57:03  my
  MY:- Fix Suchbegriffs-Bibliothek: Wenn Suchbegriffe markiert sind und
       <Esc> gedrÅckt wird, wird a) wieder zum Suche-Dialog zurÅckgekehrt
       und b) die markierten Begriffe werden nicht mehr in die
       Suchbegriff-History Åbernommen.

  JG:- Fix Suchbegriffs-Bibliothek: Der erste markierte Suchbegriff wird
       nicht mehr doppelt in die Suchbegriff-History Åbernommen.

  Revision 1.47.2.32  2001/12/05 19:29:05  my
  MY:- Wortumbruch kann jetzt auch im Archiv-Viewer mit <Ctrl-W>
       umgeschaltet werden (Zusatz/Archiv-Viewer)

  Revision 1.47.2.31  2001/11/20 23:17:52  my
  MY:- Variablen 'historyFile', 'libraryFile' und 'optionsFile'
       => Konstanten

  Revision 1.47.2.30  2001/10/22 23:04:18  my
  MY:- Option "Parken" beim Editieren von Nachrichten erscheint nur noch,
       wenn es sich auch um eine zu versendende Nachricht handelt (also
       nicht bei N/é/T z.B.)

  Revision 1.47.2.29  2001/09/16 20:27:02  my
  JG+MY:- Markierung der bei der letzten Nachrichten-Suche verwendeten
          Suchbegriffe im Lister (inkl. Umlaut- und Wildcardbehandlung):
          Nach Suche automatisch aktiv, ansonsten durch "E" schaltbar. Mit
          <Tab> springt der Cursorbalken die nÑchste Zeile mit einem
          markierten Suchbegriff an.

  JG+MY:- Neue Nachrichten-Suchfunktionen:

          - Suchbegriffs-Bibliothek (SEEKLIB.TXT): Hier kînnen oft
            benutzte Suchbegriffe abgelegt und mit <Shift-F2> ausgewÑhlt
            werden.

          - Suchbegriff-History: Mit <F2> werden die letzten 15 benutzten
            Suchbegriffe angezeigt und stehen beim nÑchsten Programmstart
            wieder zur VerfÅgung (SEEK.TXT)

          - Optionen-History: Mit <F2> werden die letzten 5 benutzten
            Options-Kombinationen angezeigt und stehen beim nÑchsten
            Programmstart wieder zur VerfÅgung (OPTIONS.TXT)

          - Neue Such-Optionen:
            l = sucht nur in Nachrichten, die dem aktuellen Lesemodus
                entsprechen
            m = hÑngt die gefundenen Nachrichten an die Liste bereits
                markierter Nachrichten an, statt diese vorher zu ent-
                markieren
            h = Volltextsuche nur im Header
            g = Volltextsuche in Header und Text
            s = lîscht die EintrÑge in der Suchbegriff-History
            k = kopiert den aktuellen Suchbegriff in die Suchbegriffs-
                Bibliothek

          - Spezial-Suche: Optionale ODER-VerknÅpfung von Absender,
            Betreff, Fido-EmpfÑnger und Text. <F2>-History fÅr Feld "Text"
            und "Optionen" eingebaut.

          - Betreffsuche (<Alt-B>) markiert jetzt nur noch Nachrichten mit
            gleichem Betreff und nicht mehr die Nachrichten, bei denen im
            Vergleichspaar der kÅrzere Betreff mit dem Anfang des lÑngeren
            Åbereinstimmt (z.B. "toll" und "toller Betreff")

          - Nachricht/Suchen/Wiedervorlage durchsucht auch User-Bretter

          - Message-ID-Suche: Suchoptionen sind wieder verfÅgbar

          - max. Anzahl der Teil-Suchbegriffe auf 20 erhîht

          - max. LÑnge des Suchbegriffs auf 160 Zeichen erhîht

  JG+MY:- Brett-(markier)-Suche ("U") analog zu User-Markiersuche
          implementiert.

  JG+MY:- User-Markiersuche ("U") benutzt jetzt Standard-Suchstring-
          eingabe, dadurch mehrere Suchbegriffe, <F2>-Auswahl, AND/OR/NOT-
          VerknÅpfung und UmlautunabhÑngigkeit mîglich

  JG+MY:- Bei Nachricht/éndern/Empfangsdatum jetzt Eingabe von Datum und
          Uhrzeit sowie öbernahme des Erstelldatums der markierten
          Nachricht mîglich.

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.47.2.28  2001/09/11 12:07:32  cl
  - small fixes/adaptions for MIME support (esp. 3.70 compatibility).

  Revision 1.47.2.27  2001/08/12 11:20:33  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.47.2.26  2001/08/11 22:17:58  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.47.2.25  2001/08/05 11:45:35  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.47.2.24  2001/07/10 07:59:16  mk
  JG:- added search Option "u"

  Revision 1.47.2.23  2001/01/12 07:42:49  mk
  JG:- Suche aus einem Brett heraus in vorher markierten Nachrichten (MK)

  Revision 1.47.2.22  2001/01/04 09:57:23  mk
  - Suchlaenge auf 120 Zeichen erhoeht

  Revision 1.47.2.21  2001/01/01 20:17:01  mo
  -Spezialsuche in markierten Brettern -lter Satnd wieder hergesetllt

  Revision 1.47.2.20  2000/12/31 14:23:56  mo
  -Spezialsuche in markierten Brettern auch aus der
   Nachrichten-/User-öbersicht

  Revision 1.47.2.19  2000/12/31 11:53:19  mk
  JG:- MsgId-Suche mit mehreren Strings

  Revision 1.47.2.18  2000/12/31 11:35:55  mk
  - fileio.disksize statt lfn.disksize benutzen

  Revision 1.47.2.17  2000/12/25 14:32:32  mk
  JG:- bugs bei der Suche behoben

  Revision 1.47.2.16  2000/12/15 21:23:46  mk
  - Findclose-Fix

  Revision 1.47.2.15  2000/12/12 14:46:05  mk
  - Sucheoption 'l' von JG wieder entfernt

  Revision 1.47.2.14  2000/12/12 11:30:28  mk
  - FindClose hinzugefuegt

  Revision 1.47.2.13  2000/12/01 09:55:37  mk
  - fix fuer letzten Commit

  Revision 1.47.2.12  2000/11/30 20:33:30  mk
  JG:- Suchmodus l hinzugefuegt

  Revision 1.47.2.11  2000/11/20 20:43:14  mk
  - Suchlaenge auf 73 reduziert

  Revision 1.47.2.10  2000/10/26 13:05:29  mk
  - Fixed Bug #112798: Lange Dateinamen in Archiven

  Revision 1.47.2.9  2000/10/17 00:16:44  mk
  - LFN Unit hinzugefuegt (Bug #112966)

  Revision 1.47.2.8  2000/10/15 09:28:07  mk
  - LFN fixes

  Revision 1.47.2.7  2000/08/28 23:35:54  mk
  - LFN in uses hinzugefuegt

  Revision 1.47.2.6  2000/08/27 20:45:52  mk
  OH:- F3-Request, ist die automatische Magic-Erkennung abgeschaltet,
    werden sie trotzdem erkannt, und zwar dann, wenn keine
    normalen Dateinamen gefunden wurden

  Revision 1.47.2.5  2000/08/22 19:46:09  mk
  - unnoetige Umlautkonvertierung entfernt

  Revision 1.47.2.4  2000/08/09 12:07:57  jg
  - Ungelesen Bug beim (K)illen von Nachrichten aus der
    Markiert-Liste behoben.
    Evtl. Allround-Fix fuer Ungelesen-Probleme in dieser Ecke.

  Revision 1.47.2.3  2000/08/08 09:24:18  mk
  - Bugfixes fuer Suche

  Revision 1.47.2.2  2000/07/22 22:01:02  mk
  - Zugriff auf nicht initialisierten String beseitigt

  Revision 1.47.2.1  2000/07/05 15:10:46  jg
  - Weitersuchen bei Markierten Nachrichten: bei fehlgeschlagener Suche
    bleibt alte  Markierung erhalten (falls genug Speicher frei ist)

  Revision 1.47  2000/06/17 06:18:35  jg
  - Bugfix: erfolglose Suche: Fensterhintergrund wurde nicht
    wiederhergestellt

  Revision 1.46  2000/06/05 16:38:51  jg
  Fix: (Suche) Stringvariable wurden vor initialisierung verwendet.

  Revision 1.45  2000/06/05 16:16:22  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.44  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.43  2000/05/22 15:52:55  jg
  - File-Kleinschreibungs-Bugfix: Suchoptionen

  Revision 1.42  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.41  2000/05/14 10:00:43  hd
  - Fix: SysUtils doppelt

  Revision 1.40  2000/05/14 07:22:21  mk
  - weiterer Fix fuer Debug-Modus

  Revision 1.39  2000/05/13 23:17:54  mk
  - jetzt mit FPC und Debug-Modus compilierbar

  Revision 1.38  2000/05/07 10:28:03  hd
  - Fix: (check_seekmode): wrt2 verlangt einen string, kein byte!

  Revision 1.37  2000/05/06 17:29:22  mk
  - DOS DPMI32 Portierung

  Revision 1.36  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.35  2000/04/27 16:36:09  jg
  - Nachricht/Aendern/Typ schaltet jetzt um zwischen Text,Bin und Mime

  Revision 1.34  2000/04/23 07:58:53  mk
  - OS/2-Portierung

  Revision 1.33  2000/04/22 08:32:47  jg
  - Bugfix: NOT - Verknuepfte Usersuche

  Revision 1.32  2000/04/15 18:21:33  mk
  - FindFirst-Fixes

  Revision 1.31  2000/04/13 12:48:37  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.30  2000/04/10 00:43:04  oh
  - F3-Request: Magicerkennung ein/ausschaltbar (C/O/e/V/Fido)

  Revision 1.29  2000/03/22 05:06:37  jg
  - Bugfix: Suchen-Spezial ohne Volltext aber mit Option "o" oder "a"
    Vorbereitung der Such-Teilstrings fuehrte zu nem RTE 201.

  Revision 1.28  2000/03/21 15:22:10  jg
  - Suche: Pfeil fuer Historyauswahl kommt nur noch
    wenn auch was gewaehlt werden kann.

  Revision 1.27  2000/03/18 10:39:06  jg
  - Suche-MessageID Wahlmoeglichkeit:  schnelle Bezugs-DB Suche
    oder langsamere Msg-Base Suche mit Teilstrings und Suchoptionen

  Revision 1.26  2000/03/14 15:15:40  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.25  2000/03/13 18:55:18  jg
  - xp4o+typeform: Ukonv in UkonvStr umbenannt
  - xp4o: Compilerschalter "History" entfernt,
          "Debugsuche" durch "Debug" ersetzt

  Revision 1.24  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.23  2000/03/08 22:36:33  mk
  - Bugfixes fÅr die 32 Bit-Version und neue ASM-Routinen

  Revision 1.22  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.21  2000/03/04 18:34:18  jg
  - Externe Viewer: zum Ansehen von Fileattaches wird keine Temp-Kopie
    mehr erstellt, und nicht mehr gewartet, da kein Loeschen noetig ist

  Revision 1.20  2000/03/02 20:09:31  jg
  - NOT Operator (~) fuer Suchstrings und Such-History eingebaut

  Revision 1.19  2000/03/01 13:17:41  jg
  - Ukonv Aufrufe benutzen jetzt High() fuer Maxlaenge
  - STRG + INS funktioniert in Texteingabefeldern wie STRG+C

  Revision 1.18  2000/03/01 08:04:23  jg
  - UND/ODER Suche mit Suchoptionen "o" + "u"
    Debug-Checkfenster mit Suchoption "c"
  - Umlautkonvertierungen beruecksichtigen
    jetzt Maximalstringlaenge

  Revision 1.17  2000/02/29 17:50:40  mk
  OH: - Erkennung der Magics verbessert

  Revision 1.16  2000/02/29 12:59:16  jg
  - Bugfix: Umlautkonvertierung beachtet jetzt Originalstringlaenge
    (Wurde akut bei Spezialsuche-Betreff)

  Revision 1.15  2000/02/29 10:46:28  jg
  -Bugfix Spezialsuche - Betreff

  Revision 1.14  2000/02/23 19:11:04  jg
  -Suchfunktionen im Lister benutzen Autosuche,
   "Global_Suchstring" und dessen auswertung entfernt.
  -!Todo.txt aktualisiiert

  Revision 1.13  2000/02/22 15:51:20  jg
  Bugfix fÅr "O" im Lister/Archivviewer
  Fix fÅr Zusatz/Archivviewer - Achivviewer-Macros jetzt aktiv
  O, I,  ALT+M, ALT+U, ALT+V, ALT+B nur noch im Lister gÅltig.
  Archivviewer-Macros gÅltig im MIME-Popup

  Revision 1.12  2000/02/19 18:00:24  jg
  Bugfix zu Rev 1.9+: Suchoptionen werden nicht mehr reseted
  Umlautunabhaengige Suche kennt jetzt "Ç"
  Mailadressen mit "!" und "=" werden ebenfalls erkannt

  Revision 1.11  2000/02/19 10:12:13  jg
  Bugfix Gelesenstatus aendern per F4 im ungelesen Modus

  Revision 1.10  2000/02/18 17:28:08  mk
  AF: Kommandozeilenoption Dupekill hinzugefuegt

  Revision 1.9  2000/02/18 15:54:52  jg
  Suchoptionen-Laenderabfrage verbessert

  Revision 1.8  2000/02/18 09:13:27  mk
  JG: * Volltextsuche jettz Sprachabhaengig gestaltet
      * XP3.ASM in XP3.PAS aufgenommen

  Revision 1.7  2000/02/16 15:25:32  mk
  OH: Schalter magics in FidoMsgRequest auf true gesetzt

  Revision 1.6  2000/02/15 21:19:24  mk
  JG: * Umlautkonvertierung von XP4O.Betreffsuche in Typeform verlagert
      * wenn man eine markierte Nachricht liest, wird beim Verlassen
        der Headeranzeige nicht gleich auch der Lister verlassen
      * Die Suchfunktionen "Absender/User", "Betreff" und "FidoempfÑnger"
        kînnen jetzt UmlautunabhÑngig geschalten werden

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

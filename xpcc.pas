{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Verteiler }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpcc;

interface

uses  typeform,fileio,inout,maske,datadef,database,stack,resource,
      xp0,xp1,xp1input, xpglobal;

const maxcc = 126;
      ccte_nobrett : boolean = false;

type  ccl   = array[1..maxcc] of AdrStr;
      ccp   = ^ccl;


procedure SortCCs(cc:ccp; cc_anz:integer);
procedure edit_cc(var cc:ccp; var cc_anz:integer16; var brk:boolean);
procedure read_verteiler(name:string; var cc:ccp; var cc_anz:integer16);
procedure write_verteiler(var name:string; var cc:ccp; cc_anz:integer);
procedure edit_verteiler(name:string; var anz:integer16; var brk:boolean);
procedure del_verteiler(name:string);

function  cc_test1(var s:string):boolean;
function  cc_testempf(var s:string):boolean;


implementation  { ---------------------------------------------------- }

uses xp3,xp3o2,xp3o,xp4e,xpnt, 
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  winxp;

const CCtemp = 'verteil.$$$';

var ccused   : array[1..maxcc] of boolean;

function is_vname(var s:string):boolean;
begin
  is_vname:=(left(s,1)='[') and (right(s,1)=']');
end;

procedure set_cce;
var i,j  : shortint;
    used : boolean;
begin
  used:=true;
  i:=1;
  while used and (i<=maxcc-1) do begin
    used:=(i=1) or ccused[i] or ccused[i-1];
    j:=i+1;
    while not used and (j<=maxcc-1) do begin
      used:=used or ccused[j];
      inc(j);
      end;
    setfieldenable(i,used);
    inc(i);
    end;
  while i<=maxcc-1 do begin
    setfieldenable(i,false);
    inc(i);
    end;
end;

function cc_test1(var s:string):boolean;
begin
  ccused[fieldpos]:=(trim(s)<>'');
  set_cce;
  cc_test1:=true;
end;

function cc_testempf(var s:string):boolean;
var p,p2 : byte;
    n    : longint;
    d    : DB;
    s2   : String;
begin
  if trim(s)='' then begin
    if ccte_nobrett then errsound;
    cc_testempf:=not ccte_nobrett;
    end
  else
    if is_vname(s) and not sel_verteiler
    then begin
      rfehler(2250);     { 'Verteiler sind hier nicht erlaubt.' }
      cc_testempf:=false;
      end
    else begin
     if is_vname(s) then s:=vert_char+s+'@V';
      n:=0;
      p:=cpos('@',s);
      if p>0 then
        s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1))
      else begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,ustr(s));
        if dbFound then begin
          dbRead(d,'Langname',s);
          p:=cpos('@',s);
          end
        else begin
          p2:=cpos(':',s);
          if (s[1]='+') and (p2>2) and IsBox(copy(s,2,p2-2)) then begin
            cc_testempf:=true;     { Crossposting-Empf„nger mit '+Server:' }
            dbClose(d);
            exit;
            end
          else
            if left(s,1)<>'/' then
              s:='/'+s;
          end;
        dbClose(d);
        end;
      if ntZonly and (p>0) and (pos('.',mid(s,p+1))=0) then
        s:=s+'.ZER';
      if p=0 then
      begin
        if ccte_nobrett then begin
          rfehler(2251);    { 'ungltige Adresse' }
          cc_testempf:=false;
          exit;
          end
        else dbSeek(bbase,biBrett,'A'+ustr(s));
        if not dbfound then
        begin
          s2:=s;
          repeat
            p:=cpos('.',s2);
            if p>0 then s2[p]:='/';
          until p=0;
          dbSeek(bbase,biBrett,'A'+ustr(s2));
           if dbfound then s:=s2;
          end;
        end
      else
        dbSeek(ubase,uiName,ustr(s));
      testmailstring_nt:=255;  { Hier alle Netztypen erlauben }
      if dbFound then begin
        cc_testempf:=true;
        if p=0 then s:=mid(dbReadStr(bbase,'brettname'),2)
        else dbReadN(ubase,ub_username,s);
        if left(s,1)=vert_char
          then s:=copy(s,2,length(s)-3);
        end
      else
        if (p>0) and not testmailstring(s) then
        begin
          cc_testempf:=false;
          if left(s,1)=vert_char
            then s:=copy(s,2,length(s)-3);
          exit;
          end
      else
        if ReadJN(getres2(2202,iif(p=0,2,1))+': '+left(s,33)+ { 'unbekannter User' / 'unbekanntes Brett' }
                  iifs(length(s)>33,'..','')+' - '+getres2(2202,3),true)
        then begin                                           { 'neu anlegen' }
          cc_testempf:=true;
          if p=0 then begin
            MakeBrett(mid(s,2),n,DefaultBox,ntBoxNetztyp(DefaultBox),false);
            if not modibrett then
            begin
              dbseek(bbase,bibrett,'A'+ustr(s));
              if dbfound then dbDelete(bbase);
              cc_testempf:=false;
              end;
            end
          else begin
            MakeUser(s,DefaultBox);
            if not modiuser(false) then
            begin
              dbseek(ubase,uiname,ustr(s));
              if dbfound then dbDelete(ubase);
              cc_testempf:=false;
              end;
            end;
          aufbau:=true;
          end
        else
          cc_testempf:=false;
      end;
  freeres;
end;

procedure SortCCs(cc:ccp; cc_anz:integer);
var i,j  : shortint;
    xchg : boolean;
    s    : string[80];

  function ccsmaller(cc1,cc2:string):boolean;
  begin
    if cc1[1]='+' then cc1[1]:=#255;
    if cc2[1]='+' then cc2[1]:=#255;
    ccsmaller:=(cc1<cc2);
  end;

begin
  j:=cc_anz-1;                     { Bubble-Sort }
  repeat
    xchg:=false;
    for i:=1 to j do
      if ccsmaller(ustr(cc^[i+1]),ustr(cc^[i])) then begin
        s:=cc^[i]; cc^[i]:=cc^[i+1]; cc^[i+1]:=s;
        xchg:=true;
        end;
    dec(j);
  until not xchg or (j=1);
end;

procedure edit_cc(var cc:ccp; var cc_anz:integer16; var brk:boolean);
var x,y   : byte;
    i     : integer {shortint};
    h     : byte;
    small : string[1];
    t     : text;
    s     : string;
begin
  h:=minmax(cc_anz+2,6,screenlines-13);
  diabox(62,h+4,getres(2201),x,y);    { 'Kopien an:' }
  inc(x); inc(y);
  openmask(x,x+59,y+1,y+h,false);
{ SortCCs(cc,cc_anz); }
  small:=iifs(ntZonly and not smallnames,'>','');
  for i:=1 to maxcc - 1 do begin
    maddstring(2,i,strsn(i,3)+'.',cc^[i],50,eAdrLen,small);
    mappcustomsel(auto_empfsel,false);
    mset1func(cc_test1);
    msetvfunc(cc_testempf);
    ccused[i]:=(cc^[i]<>'');
    end;
  maskdontclear;
  for i:=cc_anz+2 to maxcc - 1 do
    setfieldenable(i,false);
  wrt(x+53,y+h+2,' [F2] ');
  pushhp(600);
  spush(auto_empfsel_default,sizeof(auto_empfsel_default));
  spush(autoe_showscr,sizeof(autoe_showscr));
  auto_empfsel_default:=2; autoe_showscr:=true;
  readmask(brk);
  spop(autoe_showscr);
  spop(auto_empfsel_default);
  pophp;
  closemask;
  closebox;
  if not brk then begin
    cc_anz:=0;
    for i:=1 to maxcc do             { leere entfernen }
      if ccused[i] then begin
        inc(cc_anz);
        cc^[cc_anz]:=cc^[i];
        end;

    if cc_anz>0 then                 { wenn CCs da sind Verteilernamen suchen und aufloesen }
    begin
      i:=0;
      repeat
      inc(i);
      if is_vname(cc^[i]) then
      begin                                                    { nach Verteilernamen suchen }
        assign(t,CCfile);
        reset(t);
        if ioresult=0 then
        begin
          repeat
            readln(t,s)
          until eof(t) or (ustr(s)=ustr(cc^[i]));
          if not eof(t) then                                   { wenn gefunden... }                            
          begin                      
            repeat
              readln(t,s);                                     { auslesen und anhaengen }
              if (trim(s)<>'') and not is_vname(s) then
              begin       
                inc(cc_anz);
                cc^[cc_anz]:=left(s,79);
                end;
            until eof(t) or is_vname(s) or (cc_anz>=maxcc-1);
            cc^[i]:=cc^[cc_anz];                               { Verteilernamen durch }
            dec(cc_anz);                                       { letzten Eintrag ersetzen }
            end; 
          close(t);
          end;
        end;
      until i=cc_anz;
      end;
 
    for i:=cc_anz+1 to maxcc do
      cc^[i]:='';
    SortCCs(cc,cc_anz);
    end;
end;


{ Verteiler-Liste einlesen; Name hat Format '[..]' }

procedure read_verteiler(name:string; var cc:ccp; var cc_anz:integer16);
var t : text;
    s : string;
begin
  cc_anz:=0;
  fillchar(cc^,sizeof(cc^),0);
  assign(t,CCfile);
  reset(t);
  if ioresult=0 then begin
    UpString(name);
    repeat
      readln(t,s)
    until eof(t) or (ustr(s)=name);
    if not eof(t) then
      repeat
        readln(t,s);
        if (trim(s)<>'') and not is_vname(s) then begin
          inc(cc_anz);
          cc^[cc_anz]:=left(s,79);
          end;
      until eof(t) or is_vname(s);
    close(t);
    end;
  if ioresult<>0 then;
end;


procedure del_verteiler(name:string);
var t1,t2 : text;
    s     : string;
    same  : boolean;
begin
  assign(t1,CCfile);
  assign(t2,CCtemp); rewrite(t2);
  if existf(t1) then begin
    reset(t1);
    if not eof(t1) then begin
      repeat                       { vorhergehende Verteiler kopieren }
        readln(t1,s);
        same:=(ustr(s)=ustr(name));
        if not same then
          writeln(t2,s);
      until eof(t1) or same;
      if same then begin
        s:='';                     { alten (gleichen) Verteiler entfernen }
        while not eof(t1) and not is_vname(s) do
          readln(t1,s);
        if s<>'' then writeln(t2,s);
        while not eof(t1) do begin    { Rest kopieren }
          readln(t1,s);
          writeln(t2,s);
          end;
        end;
      end;
    close(t1);
    erase(t1);
    end;
  close(t2);
  rename(t2,CCfile);
end;


procedure write_verteiler(var name:string; var cc:ccp; cc_anz:integer);
var t2 : text;
    i  : integer;
begin
  del_verteiler(name);          { alten Eintrag l”schen, falls vorhanden }
  assign(t2,CCfile);
  append(t2);
  writeln(t2,name);             { neuen Eintrag anh„ngen }
  for i:=1 to cc_anz do
    writeln(t2,cc^[i]);
  writeln(t2);
  close(t2);
end;


procedure edit_verteiler(name:string; var anz:integer16; var brk:boolean);
var cc  : ccp;
begin
  new(cc);
  read_verteiler(name,cc,anz);
  edit_cc(cc,anz,brk);
  if not brk then
    write_verteiler(name,cc,anz);
  dispose(cc);
end;


end.
{
  $Log$
  Revision 1.15.2.3  2001/07/09 22:17:37  my
  JG:- Fix: Cancelling the automatic creation (e.g. of an Reply-To)
       user with <Esc> does *not* create the user anymore :-)

  Revision 1.15.2.2  2001/04/28 15:47:36  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.15.2.1  2000/07/30 12:51:08  jg
  - Maximale Anzahl Crossposting-Empfaenger auf 126 gesetzt
  - Darstellungsbug beim Crossposting an Fido Bretter behoben
  - 80K mehr Speicher im Editor (3 grosse Arrays im XMS zwischengelagert)

  Revision 1.15  2000/05/13 09:14:41  jg
  - Ueberpruefung der Adresseingaben jetzt auch Fido und Maus kompatibel

  Revision 1.14  2000/05/07 18:16:04  hd
  Kleine Linux-Anpassungen

  Revision 1.13  2000/05/05 18:08:50  jg
  - Sendefenster: Verteiler im "Kopien an" Dialog erlaubt
  - Empfaenger aendern Loescht alte "Kopien an" Eintraege

  Revision 1.12  2000/04/29 19:11:52  jg
  - Ueberpruefung der Usernameneingabe bei Nachricht/Direkt, Verteilern
    und "Kopien an" + "Empfaenger aendern" im Sendefenster

  Revision 1.11  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.10  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.8  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.7  2000/02/29 09:30:17  jg
  -Bugfix Brettnameneingaben mit "." bei Empfaenger und Kopien im Sendefenster

  Revision 1.6  2000/02/20 09:51:39  jg
  - auto_empfsel von XP4E.PAS nach XP3O.PAS verlegt
    und verbunden mit selbrett/seluser
  - Bei Brettvertreteradresse (Spezial..zUgriff) kann man jetzt
    mit F2 auch User direkt waehlen. Und Kurznamen eingeben.

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Hotkey-Anzeige und Online-Hilfe }

{$I XPDEFINE.INC }

unit xp1help;

interface

uses
  {$IFDEF NCRT} xpcurses,{$ELSE}crt,{$ENDIF}
  typeform,fileio,inout,keys,resource,maus2,help,winxp,printerx,sysutils,
  maske,xp0,xpglobal;

const inithlp : boolean = false;
      maxhelpst = 20;
      helpstp : shortint = 1;

var   helpst  : array[1..maxhelpst] of word;

procedure showkeys(nr:integer);
procedure showlastkeys;
procedure hilfe;
procedure hilfealt;
procedure dispfunctionkeys(editor:boolean);


implementation  { --------------------------------------------------- }

uses xp1;

var lastkeys : integer;


procedure showkeys(nr:integer);
const kss : byte = 2;

  procedure ks(s:string);
  var p : byte;
  begin
    p:=pos('^',s);
    delete(s,p,1);
    inc(shortkeys);
    if shortkeys>maxskeys then
      interr('Shortkey Overflow');
    with shortkey[shortkeys] do begin
      keypos:=wherex;
      keylen:=length(s);
      keyspot:=p;
      key:=LoCase(s[p]);
      end;
    attrtxt(col.colkeys);
    Wrt2(LeftStr(s,p-1));
    attrtxt(col.colkeyshigh);
    Wrt2(s[p]);
    attrtxt(col.colkeys);
    if kss = 2 then
      Wrt2(copy(s,p+1,30) + '  ')
    else
      Wrt2(copy(s,p+1,30) + ' ')
  end;

  procedure AddSK(pos,len,spot: integer; _key:taste);
  begin
    inc(shortkeys);
    if shortkeys>maxskeys then
      interr('Shortkey Overflow');
    with shortkey[shortkeys] do begin
      keypos:=pos;
      keylen:=len;
      keyspot:=spot;
      key:=_key;
      end;
  end;

  procedure ende(s1,s2:string);
  begin
    Wrt2(sp(ScreenWidth-length(s1)-length(s2)-wherex-1));
    attrtxt(col.colkeyshigh);
    Wrt2(s1);
    attrtxt(col.colkeys);
    Wrt2(s2 + '  ');
  end;

  procedure ksesc;
  begin
    ende('Esc','');
    AddSK(ScreenWidth-4,3,-3,keyesc);
  end;

  procedure plusminus;
  begin
    AddSK(wherex,1,1,'+');
    attrtxt(col.colkeyshigh);
    Wrt2('+');
    attrtxt(col.colkeys);
    Wrt2('/');
    AddSK(wherex,1,1,'-');
    attrtxt(col.colkeyshigh);
    Wrt2('-');
    attrtxt(col.colkeys);
  end;

  procedure tabkey;
  begin
    attrtxt(col.colkeys);
    Wrt2(sp(ScreenWidth-11-wherex));
    addsk(wherex,3,-3,keytab);
    attrtxt(col.colkeyshigh);
    Wrt2('Tab');
    attrtxt(col.colkeys);
    Wrt2(' / ');
    addsk(wherex,4,1,'q');
    ende('Q','uit');
  end;

  procedure hitxt(s:string);
  begin
    attrtxt(col.colkeyshigh);
    Wrt2(s);
  end;

  procedure ksmark;
  begin
    AddSK(wherex,21,-11,' ');
    hitxt('Space/F7/F');
    ks('^8-'+getres2(20,0));    { 'markieren' }
    dec(shortkeys);
  end;

  procedure kscr(txt:string);
  begin
    AddSK(wherex,length(txt)+4,-3,keycr);
    hitxt(#17'ƒ');
    ks('^Ÿ-'+txt);
    dec(shortkeys);
  end;

  procedure kstr(nr:word);
  var s : string;
      p : byte;
  begin
    s:=getres2(20,nr)+' ';
    repeat
      p:=cpos(' ',s);
      if FirstChar(s)='~' then begin      { Ctrl-Zeichen }
        hitxt('^');
        s[1]:='^';
        ks(LeftStr(s,p-1));
        with shortkey[shortkeys] do begin
          if length(s)>=2 then
            key:=chr(ord(upcase(s[2]))-64)
          else
            key:=#0;
          dec(keypos);
          inc(keylen);
          keyspot:=-keyspot-1;
          end;
        end
      else
        ks(LeftStr(s,p-1));
      delete(s,1,p);
    until s='';
  end;

begin
  if not keydisp then exit;
  shortkeys:=0;
  gotoxy(1,2);
  attrtxt(col.colkeys);
  FWrt(1,2,sp(screenwidth)); { Q&D fuer durchgehende Menuzeile }
  moff;
  Wrt2('  ');
  case abs(nr) of
    0 : Wrt2(sp(screenwidth-2));
    1 : begin       { Brettfenster }
          kstr(1);  { ^Alle ^Brief T^extfile B^inÑr ^Spezial ^Lesen: }
          lesemodepos:=wherex-1;
          gotoxy(wherex+10,wherey);     { gegen flackernden Lesemode }
          tabkey;
        end;
    2 : begin       { User-Fenster }
          kstr(2);  { ^Alle ^Brief T^extfile B^inÑr ^Spezial S^uche Ad^re·buch ^Pa·wort }
          tabkey;
        end;
    3 : begin       { Edit-Brettfenster }
          if length(getres2(20,3))>=58 then
            kss:=1;
          kstr(3);  { ^Hinzuf. ^Lîschen ^Edit ^VerknÅpfen ^Pos. ~Trenn. ^Spezial }
          plusminus;
          kss:=2;
          tabkey;
        end;
    4 : begin       { Edit-Userfenster }
          kstr(4);  { ^Alle ^Hinzuf. ^Vert. ^Lîschen ^Edit ^Spezial Ad^re·buch ^PW }
          plusminus;
          tabkey;
        end;
    5 : begin       { Msg-Fenster / User-Msg-Fenster }
          if nr>0 then kstr(5)   { ^Alle ^Halten ^Lîschen ^Kill Bezu^g ^BrettBrief ^PM ^User ^Info ^Sonst. }
          else kstr(6);          { ^Alle ^Halten ^Lîschen ^Kill Bezu^g ^PM ^Info D^ruck ^Sonstiges }
          ksesc;
        end;
    6 : begin             { markierte Nachrichten / Kommentarbaum }
          if nr>0 then kstr(7)   { ^Halten ^Lîschen ^Kill ^BrettBrief ^PM ^Info ^Absender ^Sonstige }
          else kstr(8);          { ^Adresse ^Halten ^Lîschen ^Kill ^BrettBrief ^PM ^Info ^Sonstige  }
          ksesc;
        end;
    7 : begin             { Brett-Weiterleitfenster }
          kscr(getres2(20,9));   { 'bestÑtigen' }
          ksesc;
        end;
    8 : begin             { User-Weiterleitfenster }
          ks(getres2(20,10));    { '^Alle' }
          ks(getres2(20,22));    { 'S^uche' }
          kscr(getres2(20,9));   { 'bestÑtigen' }
          ksesc;
        end;
    9 : begin             { Maps - Brettliste }
          ksmark;
          kscr(getres2(20,iif(nr<0,11,12)));  { 'abbestellen' / 'bestellen' }
          ksesc;
        end;
   10 : begin             { Fileserver - Dateiliste }
          ksmark;
          kscr(getres2(20,21));   { 'bestellen' }
          ksesc;
        end;
   11 : begin             { Archiv-Viewer }
          ksmark;
          kstr(13);       { 'E^xtrakt' }
          kscr(getres2(20,14));    { 'anzeigen' }
          ksesc;
        end;
   12 : begin             { Brettliste - hinzufÅgen }
          ksmark;
          kscr(getres2(20,15));   { 'Bretter anlegen' }
          ksesc;
        end;
   13 : begin             { Auto-Netcall }
          hitxt('Spac');
          ks('^e-'+getres2(20,16));   { 'Netcall direkt starten' }
          ksesc;
        end;
   14 : begin       { Autoversand-Liste }
          kstr(17); { ^Aktiv ^HinzufÅgen ^Kopie ^Lîschen ^Edit ^TextEdit ^Info ^Senden }
          AddSK(wherex,3,-3,keycr);
          hitxt(#17'ƒŸ');
          ksesc;
        end;
   15 : begin             { Netcall - Anwahl }
          plusminus;
          Wrt2(getres2(20,18));   { ' Zeit' }
          ksesc;
        end;
   16 : begin             { Netcall - Warten }
          hitxt('Spac');
          ks('^e-'+getres2(20,19));    { 'Netcall starten' }
          ksesc;
        end;
   17 : ksesc;            { Esc = Abbruch }
   18 : begin             { Online-Anruf - Warten }
          hitxt('Spac');
          ks('^e-'+getres2(20,20));    { 'Anruf starten' }
          ksesc;
        end;
  end;
  mon;
  freeres;
  lastkeys:=nr;
end;

procedure showlastkeys;
begin
  showkeys(lastkeys);
end;

procedure do_help(n:word);
var
      x,y  : byte;
      hlp  : string;
      mh   : boolean;
begin
  if not inithlp then
    if not inithelp(DocDir+helpfile,
                    1,1,HInvers,HKeinBlocksatz,HHeadNotHigh) then
    begin
      rfehler1(1,helpfile);   { Die Hilfsdatei XP.HLP fehlt }
      if ioresult<>0 then;
      end
    else
      inithlp:=true;

  if inithlp then
  begin
    hlp:='';
    setrahmen(2);
    openbox(58,18+(ScreenLines-25)div 2,hlp,x,y,col.colHelp,col.colHelp);
    sethelppos(x+3,y+1,16+(screenlines-25)div 2);
    setrahmen(1);
    mh:=hotkeys;
    hotkeys:=false;
    IHS(n);
    hotkeys:=mh;
    closebox;
  end;
end;


procedure hprint;
begin
  help_printable(^D,printstring(druckinit),printstring(druckexit));
end;

procedure hilfe;
begin
  savecursor;
  hprint;
  if readmask_active and (mask_helpnr>0) then
    do_help(mask_helpnr)
  else
    do_help(helpst[helpstp]);
  restcursor;
end;


procedure hilfealt;
begin
  hprint;
  do_help(0);
end;


{ F-TastenkÅrzel in letzter Zeile anzeigen }

procedure dispfunctionkeys(editor:boolean);
const fs : array[1..3] of char = 'SCA';
var fks,fkn : integer;
    i,j,spc : integer;
    hilfe,
    makros  : string[10];

  procedure wf(s:string);
  begin
    attrtxt(col.colkeyshigh);
    Wrt2(LeftStr(s,pos('-',s)-1));
    attrtxt(col.colkeys);
    Wrt2(copy(s,pos('-',s),60) + sp(spc));
  end;

begin
  fks:=0; fkn:=0;
  if not editor then
    for i:=1 to 3 do                     { benîtigten Platz berechnen }
      for j:=1 to 10 do                  { ohne Spaces                }
        with fkeys[i]^[j] do
          if menue<>'' then begin
            inc(fks,length(menue)+3);
            inc(fkn);
            end;
  spc:=iif(fks+2*fkn<42,2,1);
  inc(fks,spc*fkn);

  hilfe:=getres(100);
  makros:=getres(101);
  moff;
  gotoxy(1,screenlines);
  if fks<ScreenWidth-10-length(hilfe) then wf('F1-'+hilfe);
  if fks<ScreenWidth-23-length(hilfe)-length(makros) then wf('F6-'+makros);
  if fks<ScreenWidth-18-length(hilfe) then wf('F9-DOS');
  inc(windmax,$100);
  if editor then
    wf('F10-'+getres(133))
  else
    for i:=1 to 3 do
      for j:=1 to 10 do
        with fkeys[i]^[j] do
          if menue<>'' then
            if (wherex+length(menue)+3<=screenwidth+1) and (wherey=screenlines) then
              wf(fs[i]+strs(j)+'-'+menue);
  attrtxt(col.colkeys);
  XPdisplayed:=(wherey=screenlines) and (wherex<=screenwidth-10);
  if XPdisplayed then
    Wrt2(sp(screenwidth+1-length(xp_xp)-wherex) + xp_xp)   { 'CrossPoint' }
  else
    if wherey=screenlines then Wrt2(sp(screenwidth+1-wherex));
  mon;
  dec(windmax,$100);
  fnkeylines:=1;
end;


end.

{
  $Log$
  Revision 1.21  2001/02/22 11:48:13  ma
  - fixed crashes occurring with ScreenWidth>128

  Revision 1.20  2001/01/20 15:25:00  ml
  - helpfix

  Revision 1.19  2001/01/11 11:38:56  ma
  - some other screen adjustment fixes

  Revision 1.18  2001/01/09 20:31:04  ma
  - ' Tab / Quit' now justified correctly in all screen modes
  - shortened CVS logs

  Revision 1.17  2000/12/08 01:24:04  mk
  MH:- Usersuche bei Auswahl ueber F2 moeglich

  Revision 1.16  2000/11/11 19:26:48  ml
  - changed libdirs for rpm
}

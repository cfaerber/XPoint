{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ --- Hilfe -------------------------------------------------- }

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
    write(left(s,p-1));
    attrtxt(col.colkeyshigh);
    write(s[p]);
    attrtxt(col.colkeys);
    write(copy(s,p+1,30));
    if kss=2 then write('  ')
    else write(' ');
  end;

  procedure AddSK(pos,len,spot:shortint; _key:taste);
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
    write(sp(79-length(s1)-length(s2)-wherex));
    attrtxt(col.colkeyshigh);
    write(s1);
    attrtxt(col.colkeys);
    write(s2,'  ');
  end;

  procedure ksesc;
  begin
    ende('Esc','');
    AddSK(76,3,-3,keyesc);
  end;

  procedure plusminus;
  begin
    AddSK(wherex,1,1,'+');
    attrtxt(col.colkeyshigh);
    write('+');
    attrtxt(col.colkeys);
    write('/');
    AddSK(wherex,1,1,'-');
    attrtxt(col.colkeyshigh);
    write('-');
    attrtxt(col.colkeys);
  end;

  procedure tabkey;
  begin
    attrtxt(col.colkeys);
    write(sp(69-wherex));
    addsk(wherex,3,-3,keytab);
    attrtxt(col.colkeyshigh);
    write('Tab');
    attrtxt(col.colkeys);
    write(' / ');
    addsk(wherex,4,1,'q');
    ende('Q','uit');
  end;

  procedure hitxt(s:string);
  begin
    attrtxt(col.colkeyshigh);
    write(s);
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
    hitxt(#17'Ä');
    ks('^Ù-'+txt);
    dec(shortkeys);
  end;

  procedure kstr(nr:word);
  var s : string[80];
      p : byte;
  begin
    s:=getres2(20,nr)+' ';
    repeat
      p:=cpos(' ',s);
      if s[1]='~' then begin      { Ctrl-Zeichen }
        hitxt('^');
        s[1]:='^';
        ks(left(s,p-1));
        with shortkey[shortkeys] do begin
          key:=chr(ord(upcase(s[2]))-64);
          dec(keypos);
          inc(keylen);
          keyspot:=-keyspot-1;
          end;
        end
      else
        ks(left(s,p-1));
      delete(s,1,p);
    until s='';
  end;

begin
  if not keydisp then exit;
  shortkeys:=0;
  gotoxy(1,2);
  attrtxt(col.colkeys);
  moff;
  write('  ');
  case abs(nr) of
    0 : write(sp(78));
    1 : begin       { Brettfenster }
          kstr(1);  { ^Alle ^Brief T^extfile B^in„r ^Spezial ^Lesen: }
          lesemodepos:=wherex-1;
          gotoxy(wherex+10,wherey);     { gegen flackernden Lesemode }
          tabkey;
        end;
    2 : begin       { User-Fenster }
          kstr(2);  { ^Alle ^Brief T^extfile B^in„r ^Spezial S^uche Ad^reábuch ^Paáwort }
          tabkey;
        end;
    3 : begin       { Edit-Brettfenster }
          if length(getres2(20,3))>=58 then
            kss:=1;
          kstr(3);  { ^Hinzuf. ^L”schen ^Edit ^Verknpfen ^Pos. ~Trenn. ^Spezial }
          plusminus;
          kss:=2;
          tabkey;
        end;
    4 : begin       { Edit-Userfenster }
          kstr(4);  { ^Alle ^Hinzuf. ^Vert. ^L”schen ^Edit ^Spezial Ad^reábuch ^PW }
          plusminus;
          tabkey;
        end;
    5 : begin       { Msg-Fenster / User-Msg-Fenster }
          if nr>0 then kstr(5)   { ^Alle ^Halten ^L”schen ^Kill Bezu^g ^BrettBrief ^PM ^User ^Info ^Sonst. }
          else kstr(6);          { ^Alle ^Halten ^L”schen ^Kill Bezu^g ^PM ^Info D^ruck ^Sonstiges }
          ksesc;
        end;
    6 : begin             { markierte Nachrichten / Kommentarbaum }
          if nr>0 then kstr(7)   { ^Halten ^L”schen ^Kill ^BrettBrief ^PM ^Info ^Absender ^Sonstige }
          else kstr(8);          { ^Adresse ^Halten ^L”schen ^Kill ^BrettBrief ^PM ^Info ^Sonstige  }
          ksesc;
        end;
    7 : begin             { Brett-Weiterleitfenster }
          kscr(getres2(20,9));   { 'best„tigen' }
          ksesc;
        end;
    8 : begin             { User-Weiterleitfenster }
          ks(getres2(20,10));    { '^Alle' }
          kscr(getres2(20,9));   { 'best„tigen' }
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
   12 : begin             { Brettliste - hinzufgen }
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
          kstr(17); { ^Aktiv ^Hinzufgen ^Kopie ^L”schen ^Edit ^TextEdit ^Info ^Senden }
          AddSK(wherex,3,-3,keycr);
          hitxt(#17'ÄÙ');
          ksesc;
        end;
   15 : begin             { Netcall - Anwahl }
          plusminus;
          write(getres2(20,18));   { ' Zeit' }
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


procedure hlp(nr:word);
begin
  helpst[helpstp]:=nr;
end;

procedure pushhp(nr:word);
begin
  if helpstp>=maxhelpst then
    interr('PushHP: Overflow')
  else begin
    inc(helpstp);
    helpst[helpstp]:=nr;
    end;
end;

procedure pophp;
begin
  if helpstp=1 then
    interr('PopHP: Underflow')
  else
    dec(helpstp);
end;




{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

procedure SavePhonezones;
var t     : text;
    i,j,k : integer;
    first : boolean;
begin
  assign(t,ParGebdat);          { GEBUER.DAT speichern }
  rewrite(t);
  writeln(t,'## ',getres(1002));     { 'Telefon-Tarifzonen' }
  writeln(t);
  writeln(t,'Dummy=');
  writeln(t);
  for i:=1 to anzahl do
    with phones^[i] do begin
      writeln(t,'Zone=',komment);
      for j:=1 to (anz+7) div 8 do begin
        for k:=1 to 8 do
          if (j-1)*8+k<=anz then
            write(t,ph^[(j-1)*8+k],' ');
        writeln(t);
        end;
      writeln(t);
      end;
  close(t);
  assign(t,ParGebdat2);         { TARIFE.DAT speichern }
  rewrite(t);
  writeln(t,'## ',getres(1021));    { 'Telefon-Tarife' }
  writeln(t);
  for i:=1 to tables do with tarif^[i] do begin
    write(t,'[');                    { [Mo,Di,Mi,...] schreiben }
    first:=true;
    for j:=1 to maxwotage do
      if wochentag[j] then begin
        if not first then write(t,',');
        first:=false;
        write(t,wofeiertag[j]);
        end;
    writeln(t,']');
    for j:=1 to zeitbereiche do with zeitbereich[j] do begin
      write(t,von,'-',bis);
      for k:=1 to xp10.anzahl do with tarif[k] do begin
        write(t,' ');
        if round(sekunden,2)=round(sekunden,0) then
          write(t,strsr(sekunden,0))
        else
          write(t,strsr(sekunden,3));
        write(t,'/',pfennig);
        if anwahl>0 then write(t,'/',anwahl);
        end;
      writeln(t);
      end;
    writeln(t);
    end;
  close(t);
end;


procedure LoadPhonezones;
var pa    : phoneap;
    t     : text;
    s     : string;
    ss    : string[20];
    i     : integer;
    ppos  : integer;
    p     : byte;
    loadt : boolean;    { TARIFE.DAT laden }

  procedure AddP(s:string; _anz:integer);
  begin
    inc(anzahl);
    with phones^[anzahl] do begin
      komment:=s;
      anz:=_anz;
      if anz>0 then
        getmem(ph,anz*sizeof(phone1));
      end;
    ppos:=0;
  end;

  procedure x(nr:integer);
  begin
    with phones^[anzahl] do begin
      inc(ppos);
      ph^[ppos]:=strs(nr)+'-';
      end;
  end;

  procedure xs(s:string);
  begin
    with phones^[anzahl] do begin
      inc(ppos);
      ph^[ppos]:=s;
      end;
  end;

  procedure x7(nr:word);
  begin
    xs('7-'+strs(nr));
  end;

  procedure bereich(tnr,bereichnr:integer; _von,_bis:string;
                    g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11:real);
  var i : integer;
  begin
    with tarif^[tnr].zeitbereich[bereichnr] do begin
      von:=_von;
      bis:=_bis;
      tarif[1].sekunden:=g4;    tarif[9].sekunden:=16.7;
      tarif[2].sekunden:=g11;   tarif[10].sekunden:=10;
      tarif[3].sekunden:=g1;    tarif[11].sekunden:=g5;
      tarif[4].sekunden:=g2;    tarif[12].sekunden:=g6;
      tarif[5].sekunden:=g3;    tarif[13].sekunden:=g7;
      tarif[6].sekunden:=g3;    tarif[14].sekunden:=g8;
      tarif[7].sekunden:=g3;    tarif[15].sekunden:=g9;
      tarif[8].sekunden:=30;    tarif[16].sekunden:=g10;
      for i:=1 to 16 do tarif[i].pfennig:=12;
      for i:=1 to 16 do tarif[i].anwahl:=0;
      end;
  end;

begin
  new(phones);
  assign(t,ParGebdat);
  anzahl:=0;
  if existf(t) then begin
    new(pa);
    reset(t);
    while not eof(t) do begin
      repeat                         { Kopf Åberlesen }
        readln(t,s);
        if left(s,1)<>'#' then p:=cpos('=',s)
        else p:=0;
      until eof(t) or (p>0);
      while p>0 do begin             { einmaligen Datenblock einlesen }
      { if lstr(left(s,p-1))='waehrung' then
          Waehrung:=trim(mid(s,p+1)); }
        if eof(t) then s:=''
        else readln(t,s);
        p:=cpos('=',s);
        end;
      while not eof(t) and (anzahl<maxzones) do begin
        repeat
          readln(t,s); p:=cpos('=',s)
        until eof(t) or (p>0);
        if not eof(t) then begin
          inc(anzahl);               { Zonendaten einlesen }
          fillchar(phones^[anzahl],sizeof(phones^[anzahl]),0);
          with phones^[anzahl] do begin
            repeat
              if lstr(left(s,p-1))='zone' then komment:=trim(mid(s,p+1));
              readln(t,s);
              p:=cpos('=',s);
            until p=0;
            if not eof(t) then
              while trim(s)<>'' do begin     { Nummern einlesen }
                s:=left(trim(s),254)+' ';
                while s<>'' do begin
                  p:=cpos(' ',s);
                  if anz<maxphone then begin
                    inc(anz);
                    pa^[anz]:=left(s,p-1);
                    end;
                  s:=trimleft(mid(s,p+1));
                  end;
                if eof(t) then s:=''
                else readln(t,s);
                end;
            if memavail<sizeof(pa^)+anz*sizeof(phone1) then
              anz:=0;
            if anz>0 then begin
              getmem(ph,anz*sizeof(phone1));
              Move(pa^,ph^,anz*sizeof(phone1));
              end;
            end;
          end;
        end;
      end;
    close(t);
    dispose(pa);
    loadt:=exist(ParGebdat2);
    end

  else begin   { not exist(GebuehrDat) }
    AddP('Fernzone',0);
    AddP('Welt 4',0);
    AddP('City',0);
    AddP('Region 50',0);
    AddP('Region 200',0);
    AddP('Region 200',0);
    AddP('Region 200',0);
    AddP('Grenzb. Vis-Ö-vis 1',0);
    AddP('Grenzb. Vis-Ö-vis 2',0);
    AddP('Grenzb. Vis-Ö-vis 3',0);
    AddP('Euro 1 (8-18/18-8)',17);
    x(298); x(31); x(32); x(33); x(352); x(353); x(354); x(358);
    x(376); x(41); x(42); x(43); x(44); x(45); x(46); x(47);
    x(48);
    AddP('Euro 1 (8-20/20-8)',5);
    x(30); x(34); x(351); x(378); x(39);
    AddP('Euro 2',41);
    x(20); x(212); x(213); x(216); x(218); x(350); x(355); x(356);
    x(357); x(359); x(36); x(370); x(371); x(372); x(373); x(375);
    x(380); x(381); x(385); x(386); x(387); x(389); x(40);
    xs('7-01'); xs('7-07'); xs('7-08'); xs('7-095'); xs('7-096');
    x7(811); x7(812); x7(815); x7(816); x7(820); x7(861); x7(862); x7(863);
    x(90); x(961); x(962); x(963); x(972);
    AddP('Welt 1',1);
    x(1);
    AddP('Welt 2',7);
    x(61); x(64); x(65); xs('672-3-'); x(81); x(82); x(852);
    AddP('Welt 3',14);
    xs('1-809'); x(27); x(54); x(55); x(56); x(57); x(599); x(63);
    x7(31); x7(32); x(886); x(966); x(971); x(98);
    loadt:=false;
    end;
  new(tarif);
  fillchar(tarif^,sizeof(tarif^),0);
  tables:=0;
  if loadt then begin
    assign(t,ParGebdat2);
    reset(t);
    while not eof(t) do begin
      repeat
        readln(t,s)
      until (firstchar(s)='[') or eof(t);
      if firstchar(s)='[' then begin
        delfirst(s); dellast(s);
        inc(tables);
        with tarif^[tables] do begin
          while s<>'' do begin   { '[Mo,Di,Mi,...] parsen }
            ss:=GetToken(s,',');
            for i:=1 to maxwotage do
              if stricmp(ss,wofeiertag[i]) then wochentag[i]:=true;
            end;
          zeitbereiche:=0;
          repeat
            readln(t,s);
            s:=trim(s);
            if (s<>'') and (s[3]=':') and (s[6]='-') and (s[9]=':') then begin
              inc(zeitbereiche);
              with zeitbereich[zeitbereiche] do begin
                ss:=GetToken(s,' ');
                von:=left(ss,5);
                bis:=right(ss,5);
                i:=0;
                while (s<>'') and (i<xp10.anzahl) do begin
                  inc(i);
                  ss:=GetToken(s,' ');    { Tarifeinheit parsen }
                  tarif[i].sekunden:=minmaxr(rval(GetToken(ss,'/')),0,9999);
                  tarif[i].pfennig:=minmax(ival(GetToken(ss,'/')),0,9999);
                  tarif[i].anwahl:=minmax(ival(GetToken(ss,'/')),0,9999);
                  end;                 { -> lÑ·t Platz fÅr Erweiterungen }
                end;
              s:='*';
              end;
          until s='';
          end;   { with table }
        end;   { [... }
      end;   { while not eof() }
    close(t);
    end

  else begin   { not loadt }
    tables:=3;
    for i:=1 to maxwotage do
      tarif^[1].wochentag[i]:=(i<=5);
    tarif^[1].zeitbereiche:=10;
    bereich(1,1,'05:00','07:59',150,45,21.5,20,9,9,5.63,5.46,3,2.6,2.31);
    bereich(1,2,'08:00','08:59',150,45,21.5,20,7.2,7.2,5.63,5.46,3,2.6,2.31);
    bereich(1,3,'09:00','11:59',90,26,12,11.5,7.2,7.2,5.63,5.46,3,2.6,2.31);
    bereich(1,4,'12:00','13:59',90,30,13.5,12.5,7.2,7.2,5.63,5.46,3,2.6,2.31);
    bereich(1,5,'14:00','17:59',90,30,13.5,12.5,7.2,7.2,5.63,5,3,2.6,2.31);
    bereich(1,6,'18:00','19:59',150,45,21.5,20,9,7.2,5.63,5,3,2.6,2.31);
    bereich(1,7,'20:00','20:59',150,45,21.5,20,9,9,5.63,5,3,2.6,2.31);
    bereich(1,8,'21:00','01:59',240,60,30,25,9,9,5.63,5,3,2.6,2.31);
    bereich(1,9,'02:00','02:59',240,120,120,120,9,9,5.63,5,3,2.6,2.31);
    bereich(1,10,'03:00','04:59',240,120,120,120,9,9,5.63,5.46,3,2.6,2.31);
    for i:=1 to maxwotage do
      tarif^[2].wochentag[i]:=(i in [6,7]);
    tarif^[2].zeitbereiche:=4;
    bereich(2,1,'05:00','13:59',150,45,21.5,20,9,9,5.63,5.46,3,2.6,2.31);
    bereich(2,2,'14:00','20:59',150,45,21.5,20,9,9,5.63,5,3,2.6,2.31);
    bereich(2,3,'21:00','02:59',240,60,30,25,9,9,5.63,5,3,2.6,2.31);
    bereich(2,4,'03:00','04:59',240,60,30,25,9,9,5.63,5.46,3,2.6,2.31);
    for i:=1 to maxwotage do
      tarif^[3].wochentag[i]:=(i=8);
    tarif^[3].zeitbereiche:=7;
    bereich(3,1,'05:00','07:59',150,45,21.5,20,9,9,5.63,5.46,3,2.6,2.31);
    bereich(3,2,'08:00','13:59',150,45,21.5,20,7.2,7.2,5.63,5.46,3,2.6,2.31);
    bereich(3,3,'14:00','17:59',150,45,21.5,20,7.2,7.2,5.63,5,3,2.6,2.31);
    bereich(3,4,'18:00','19:59',150,45,21.5,20,9,7.2,5.63,5,3,2.6,2.31);
    bereich(3,5,'20:00','20:59',150,45,21.5,20,9,9,5.63,5,3,2.6,2.31);
    bereich(3,6,'21:00','02:59',240,60,30,25,9,9,5.63,5,3,2.6,2.31);
    bereich(3,7,'03:00','04:59',240,60,30,25,9,9,5.63,5.46,3,2.6,2.31);
    end;
  if not loadt then SavePhonezones;
end;


procedure FreePhoneZones;
var i : integer;
begin
  for i:=1 to anzahl do
    if phones^[i].anz>0 then
      freemem(phones^[i].ph,phones^[i].anz*sizeof(phone1));
  dispose(phones);
  anzahl:=0;
  dispose(tarif);
end;
{
  $Log$
  Revision 1.5  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.4  2000/07/02 14:24:52  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.3  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2002 OpenXP-Team, http://www.openxp.de                 }
{ (c) 2002-2003 OpenXP/16, http://www.openxp16.de                 }
{ See list of contributors in authors.txt                         }
{                                                                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ OpenXP ist eine eingetragene Marke von Markus Kaemmerer.        }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

procedure SavePhonezones;
var t     : text;
    i,j,k : integer;
    first : boolean;
begin
  assign(t,ParGebdat);               { GEBUEHR.DAT speichern }
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
  assign(t,ParGebdat2);              { TARIFE.DAT speichern }
  rewrite(t);
  writeln(t,'## ',getres(1021));     { 'Telefon-Tarife' }
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
          write(t,strsr(sekunden,2));
        if round(pfennig,2)=round(pfennig,0) then begin
           write(t,'/');
           write(t,strsr(pfennig,0))
          end
        else begin
          write(t,'/');
          write(t,strsr(pfennig,2));
        end;
        if anwahl>0 then begin
          if round(anwahl,2)=round(anwahl,0) then begin
            write(t,'/');
            write(t,strsr(anwahl,0));
          end
          else begin
             write(t,'/');
             write(t,strsr(anwahl,2));
            end;
          end;
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
                    g1,p1,g2,p2,g3,p3,g4,p4,g5,p5,g6,p6,g7,p7,g8,p8,g9,p9,g10,p10,g11,p11,g12,p12:real);
  var i : integer;
  begin
    with tarif^[tnr].zeitbereich[bereichnr] do begin
      von:=_von;
      bis:=_bis;
      tarif[1].sekunden:=g1; tarif[1].pfennig:=p1;
      tarif[2].sekunden:=g2; tarif[2].pfennig:=p2;
      tarif[3].sekunden:=g3; tarif[3].pfennig:=p3;
      tarif[4].sekunden:=g4; tarif[4].pfennig:=p4;
      tarif[5].sekunden:=g5; tarif[5].pfennig:=p5;
      tarif[6].sekunden:=g6; tarif[6].pfennig:=p6;
      tarif[7].sekunden:=g7; tarif[7].pfennig:=p7;
      tarif[8].sekunden:=g8; tarif[8].pfennig:=p8;
      tarif[9].sekunden:=g9; tarif[9].pfennig:=p9;
      tarif[10].sekunden:=g10; tarif[10].pfennig:=p10;
      tarif[11].sekunden:=g11; tarif[11].pfennig:=p11;
      tarif[12].sekunden:=g12; tarif[12].pfennig:=p12;
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
                  s:=ltrim(mid(s,p+1));
                  end;
                if eof(t) then s:=''
                else readln(t,s);
                end;
            if memavail<sizeof(pa^)+anz*sizeof(phone1) then
              anz:=0;
            if anz>0 then begin
              getmem(ph,anz*sizeof(phone1));
              FastMove(pa^,ph^,anz*sizeof(phone1));
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
    AddP('DTAG T-NET City',0);
    AddP('DTAG T-NET Fern',0);
    AddP('DTAG T-NET100 City',0);
    AddP('DTAG T-NET100 Fern',0);
    AddP('DTAG T-ISDN City',0);
    AddP('DTAG T-ISDN Fern',0);
    AddP('DTAG Aktiv+ City',0);
    AddP('DTAG Aktiv+ Fern',0);
    AddP('DTAG XXL City',0);
    AddP('DTAG XXL Fern',0);
    AddP('Freecall (0800)',2);
    xs('0800'); x(800);
    AddP('3U Normaltarif',0);
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
                  tarif[i].pfennig:=minmaxr(rval(GetToken(ss,'/')),0,9999);
                  tarif[i].anwahl:=minmaxr(rval(GetToken(ss,'/')),0,9999);
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
    tarif^[1].zeitbereiche:=7;
    if not newgeb then begin
       bereich(1,1,'00:00','04:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(1,2,'05:00','06:59',150,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(1,3,'07:00','07:59',150,6,60,12.3,60,3.1,60,4.6,90,6,60,9.2,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(1,4,'08:00','08:59',150,6,60,12.3,60,3.1,60,4.6,90,6,60,9.2,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.08);
       bereich(1,5,'09:00','17:59',90,6,60,12.3,60,3.1,60,4.6,90,6,60,9.2,60,3.1,60,4.6,60,3.1,60,4.6,0,0,1,0.08);
       bereich(1,6,'18:00','20:59',150,6,60,6.2,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(1,7,'21:00','23:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
    end else begin
       bereich(1,1,'00:00','04:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(1,2,'05:00','06:59',150,2.4,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(1,3,'07:00','07:59',150,2.4,60,12.3,60,3.1,60,4.6,90,4,60,9.2,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(1,4,'08:00','08:59',150,2.4,60,12.3,60,3.1,60,4.6,90,4,60,9.2,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,4.6);
       bereich(1,5,'09:00','17:59',90,4,60,12.3,60,3.1,60,4.6,90,4,60,9.2,60,3.1,60,4.6,60,3.1,60,4.6,0,0,1,4.6);
       bereich(1,6,'18:00','20:59',150,2.4,60,6.2,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(1,7,'21:00','23:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
    end;
    for i:=1 to maxwotage do
      tarif^[2].wochentag[i]:=(i=6);
    tarif^[2].zeitbereiche:=3;
    if not newgeb then begin
       bereich(2,1,'00:00','06:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(2,2,'07:00','20:59',150,6,60,4.6,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
       bereich(2,3,'21:00','23:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,0.04);
    end else begin
       bereich(2,1,'00:00','06:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(2,2,'07:00','20:59',150,2.4,60,4.6,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
       bereich(2,3,'21:00','23:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,60,1.6,60,2.6,0,0,1,2.5);
    end;
    for i:=1 to maxwotage do
      tarif^[3].wochentag[i]:=(i in [7,8]);
    tarif^[3].zeitbereiche:=3;
    if not newgeb then begin
       bereich(3,1,'00:00','06:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,0.04);
       bereich(3,2,'07:00','20:59',150,6,60,4.6,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,0.04);
       bereich(3,3,'21:00','23:59',240,6,60,3.1,60,1.6,60,2.6,240,6,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,0.04);
    end else begin
       bereich(3,1,'00:00','06:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,2.5);
       bereich(3,2,'07:00','20:59',150,2.4,60,4.6,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,2.5);
       bereich(3,3,'21:00','23:59',240,1.5,60,3.1,60,1.6,60,2.6,240,1.5,60,3.1,60,1.6,60,2.6,0,0,0,0,0,0,1,2.5);
    end;
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
  Revision 1.3.4.3  2003/05/01 14:22:40  mk
  - updated copyright headers

  Revision 1.3.4.2  2003/01/10 21:55:37  my
  MY:- Log- und Kommentarkosmetik

  Revision 1.3.4.1  2003/01/10 14:04:22  mw
  MW:- Gro·es GebÅhren-Update:
       1. Es kînnen jetzt auch Bruchteile von 1/100 WÑhrungseinheiten
          sowie ein zusÑtzliches festes Verbindungsentgelt eingegeben und bei
          der Berechnung berÅcksichtigt werden.
       2. Neues GebÅhrenmodell: Bei aktivem Schalter unter Config/Optionen/
                                GebÅhren/Sonstiges werden GeldbetrÑge als
                                rechnerische Minutenpreise gewertet und nicht
                                mehr als Preis einer Tarifeinheit. Bei
                                inaktivem Schalter gelten GeldbetrÑge
                                weiterhin als Preis einer Tarifeinheit.
       3. Neue Default-WÑhrung fuer die GebÅhrenberechnung ist nun EUR.
       4. Neue Tarife: Die Default-Tariftabelle enthÑlt nun alle aktuellen
          nationalen Privatkunden-Tarife der DTAG fÅr Festnetz zu Festnetz
          sowie den Tarif "Normaltarif" von 3U.
       5. Wenn die Default-Tariftabelle geschrieben wird, wird dabei das
          verwendete GebÅhrenmodell berÅcksichtigt.
       6. XPOINT.LOG enthÑlt jetzt alle Kostenangaben mit 4 Nachkommastellen.

  Revision 1.3  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}

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

{ XP4.PAS - Anzeigeroutinen }


{ --- Zeile einlesen und anzeigen --- }

procedure brettform(var s:string; flags:byte; user:boolean);
var i : byte;
begin
  case brettanzeige of
    1 : s:=TopAllStr(s);
    2 : for i:=iif(user,1,2) to length(s) do
          s[i]:=LoCase(s[i]);
  end;
  if not user and (cpos('/',s)>0)  and newsgroupdisp
     and (((s[1]='A') and (flags and 16<>0)) or newsgroupdispall)
  then begin
    if newsgroupdispall and (s[2]='/') then delete(s,2,1);
    if (s[1]='1') or (cpos(':',s)>0) then begin
      i:=cpos('/',s);
      if (i>2) and (cpos('@',s)=0) then s[i]:='@';
    end else
      for i:=3 to length(s) do if (s[i]='/') and not
        (((i>6) and (UpperCase(copy(s,i-6,6))='OPENXP')) and
         ((i<length(s)-1) and ((copy(s,i+1,2)='16') or
          (copy(s,i+1,2)='32')))) then
        s[i]:='.';
    end;
end;

procedure write_disp_line(y,p:shortint; show:boolean);
const
  uv : array[0..13] of char = ' !*'#19'z23456789#';
  ge : array[0..1] of char = '> ';
  wv : array[0..5] of char = ' cwwss';

var
    hf      : array[0..4] of char;
    taus    : array[1..FelderMax] of string;
    tausc   : 0..FelderMax;
    s,s0    : string;
    dat,edat: longint;
    sdat    : string;
    c       : string;
    s1      : string;
    s2      : string;
    size    : longint;
    typ     : char;
    hzeit   : integer16;  { Haltezeit }
    pbox    : string;
    komm    : string;
    dadr,pw : char;
    ab,flags: byte;
    abc     : string;
    grnr    : longint;
    uflags  : byte;
    markc   : char;
    hd      : Theader;
    hds     : longint;
    pp      : byte;
    wvl     : byte;
    vert    : boolean;
    mnt     : longint;
    netztyp : byte;
    _brett  : string;
    t,m,j   : smallword;
    tempd: integer;
    fileatt : boolean;
    dflags  : string;
    fidoqwk : boolean;
    uv_anz  : integer;
    helpbyte,h1,h2,h3: byte;  

    hflags,gelesen,unvers : byte;
    ulen,blen             : byte;

    over80  : integer;

    Short_Realnames : boolean;  { bei <Ctrl-N> Realname kÅrzen }

  function siz:string;
  begin
    if size<10000 then siz:=strsn(size,5)
    else
      if size<1024*1024 then
        siz:=strsn(size div 1024,4)+'k'
      else
        if size<1024*1024*1024 then
          siz:=strsn(size div (1024*1024),4)+'M'
        else
          siz:=strsn(size div (1024*1024*1024),4)+'G';
  end;

  function hz(i:integer):string;
  begin
    hz:=iifs(i=0,'   Ï',strsn(i,4));
  end;

  procedure msgtausch(c:char; const s:string);
  begin
    if cpos(c,MsgFeldTausch)=0 then exit;
    taus[cpos(c,MsgFeldTausch)] := s;
  end;

  procedure usrtausch(c:char; const s:string);
  begin
    if cpos(c,UsrFeldTausch)=0 then exit;
    taus[cpos(c,UsrFeldTausch)] := s;
  end;

begin  { __ Suchmarke }
  over80:= ScreenWidth-80;                                      { Bildschirm > Spalten }
  case aktdispmode of
   -1,0 : begin
            s:= dbReadStrN(bbase,bb_brettname);
            komm:= dbReadStr(bbase,'kommentar');
            if LeftStr(s,3)='$/T' then begin                       { Trennzeile ? }
              if komm='' then
                dispbuf[y]:='   '+dup(ScreenWidth-4,s[4])+' '   { ohne Kommentar }
              else
                case trennkomm of
                  1 : dispbuf[y]:='   '+komm+' '+dup(ScreenWidth-5-length(komm),s[4])+' '; { links }
                  2 : begin                                                                { mitte }
                        dispbuf[y]:='   '+dup((ScreenWidth-6-length(komm))div 2,s[4])+' '+
                                     komm+' ';
                        dispbuf[y]:=dispbuf[y]+dup(ScreenWidth-1-length(dispbuf[y]),s[4])+' ';
                      end;
                  3 : dispbuf[y]:='   '+dup(ScreenWidth-5-length(komm),s[4])+' '+komm+' ';  { rechts }
                end;
              markflag[y]:=2;
              end
            else begin                                           { keine Trennzeile }
              dbReadN(bbase,bb_LDatum,dat);
              dbReadN(bbase,bb_flags,flags);
              pbox:= dbReadNStr(bbase,bb_pollbox);
              markflag[y]:=iif(UBmarked(dbRecno(bbase)),1,0);
              markc:=iifc(markflag[y]<>0,suchch,' ');
              brettform(s,flags,false);
              if (not NewsgroupDispall) and (not UserSlash) and (s[1]<'A') then delete(s,2,1);
              if not dispext then begin                         { erweiterte Anzeige s ? }
                case readmode of     { s. auch XP4.BRETTOK() }
                  0 : c:=markc+iifs(showungelesen and (flags and 2<>0),#$af' ','  ');
                  1 : c:=markc+iifs(flags and 2<>0,#$af' ','  ');
                else
                  if showungelesen and (flags and 2<>0)
                    then c:=markc+iifs(not smdl(dat,readdate),#$af' ',#$1a' ')
                    else c:=markc+iifs(not smdl(dat,readdate),#16' ','  ');
                end;
                if length(s)>43 then                    { BrettÅbersicht in Normal mode }
                  if ( length(komm) > 1 ) then          { Kommentar vorhanden ?}
                    dispbuf[y]:=c                                           {   4  }
                                + forms(mid(s,2), 60 + (over80+1)div 2)+' ' {+ 60!! }
                                + forms( komm , 16 + over80 div 2)          {+ 16 }
                  else
                     dispbuf[y]:=c+forms(copy(s,2,76+over80),77+over80)
                else                                            { length(s)>43 then }
                             { 3             + 44                + 33 = 80 }
                  dispbuf[y]:=c+forms(copy(s,2,43),44)+forms(komm,over80+33);


                end {if not dispext then}

              else begin                                        { erweiterte Anzeige }
                dbReadN(bbase,bb_haltezeit,hzeit);
                dbReadN(bbase,bb_gruppe,grnr);
                dispbuf[y]:=markc+                                      {  1}
                            iifc(dat<>0,' ','-')                        { +1}
                            +iifc(flags and 8<>0,'X',' ')               { +1}
                            +iifc(dbReadStrN(bbase,bb_adresse)<>'',
                                  iifc(flags and 32=0,'*','O'),' ')     { +1}
                            +iifc(odd(flags),'N',' ')                   { +1}
                            +hz(hzeit)                                  { +4}
                            +'  '                                       { +1}
                            +forms(pbox,9)                              { +9}
                            +strsn(grnr,4)                              { +4}
                            +' '                                        { +1}
                            +forms(copy(s,2,37+(over80 div 2)),38+(over80+1)div 2){+38}
                            +forms(komm,17+ over80 div 2);              {+17}
                end;
              end;
          end;
    1,2,
    3,4 : begin { USERLISTE }

           { LÑngen:
             FLAGS     4
             ADRBUCH   2
             PBOX      9
             HZEIT     4
             ADRESSE   44,32,24
             KOMMENTAR 30,22
            }

            for tausc := 1 to UsrFelderMax do taus[tausc]:='';
            s:= dbReadNStr(ubase,ub_username);
            komm:= dbReadNStr(ubase,ub_kommentar);
                                                        { Trennzeilenerkennung }
            if LeftStr(s,4)=#0+'$/T' then
            begin
              if komm='' then                           { Trennzeile ohne Kommentar }
                dispbuf[y]:='     '+dup(74+over80,s[5])+' '
              else
                case trennkomm of
                  1 : dispbuf[y]:='     '+komm+' '+dup(73-length(komm)+over80,s[5])+' '; { links -ungetestet }
                  2 : begin                                                              { mitte -ungetestet}
                        dispbuf[y]:='     '+dup((72-length(komm)+over80)div 2,s[5])+' '+
                                     komm+' ';
                        dispbuf[y]:=dispbuf[y]+dup(79+over80-length(dispbuf[y]),s[5])+' ';
                      end;
                  3 : dispbuf[y]:='     '+dup(73-length(komm)+over80,s[5])+' '+komm+' '; { rechts }
                end;
              markflag[y]:=2;
              end
            else begin                          { User Åbersicht keine Trennzeile }

            dbReadN(ubase,ub_adrbuch,ab);
            dbReadN(ubase,ub_userflags,uflags);
            abc:=iifc(ab<>0,'˚',' ')+iifc(odd(uflags),' ','#');
            dadr:=iifc(dbXsize(ubase,'adresse')>0,'*',' ');
            pw:=iifc((dbXsize(ubase,'passwort')>0) or
                     (dbReadInt(ubase,'codierer') in [8,9]),'P',' ');
            vert:=(uflags and 4<>0);
            dbReadN(ubase,ub_haltezeit,hzeit);


            if vert then    { Verteiler }
            begin
              s := vert_name(s);
              abc:='V ';
            end;
            usrtausch('F',abc+pw+dadr);      { (F)lags }

            markflag[y]:=iif(UBmarked(dbRecno(ubase)),1,0);
            brettform(s,0,true);

            if dispext then begin     { Anzeige/Spezial }
              pbox:= dbReadNStr(ubase,ub_pollbox);
              brettform(s,0,true);
              usrtausch('H','  '+hz(hzeit)); { (H)altezeit }
              usrtausch('B',forms(pbox,9));  { (B)OX }
              usrtausch('G',strsn(ab,2));    { Adressbuch-(G)ruppe }
            end;

            Helpbyte:=cpos('A',Usrfeldtausch);
            h3:=cpos('K',Usrfeldtausch);
            if (komm='') or (h3=0) then h1:=75 else h1:=44;
            h2:=30;
            if dispext and (h3<>0) then  { Falls (S)pezial EIN, dann Resttext kÅrzen }
              if h3 > helpbyte then
                dec(h1,12)            { Adresse nach Kommentar: nur Adresse kÅrzen }
              else begin
                h1:=44-20;
                h2:=30-8;             { Kommentar nur kÅrzen, wenn er hinter der Adresse steht }
                end;
            usrtausch('A',forms(s,h1));              { Verteilername bzw. (A)dresse }
            usrtausch('K',forms(komm,h2));           { (K)ommentar }

            tausc:=0; dispbuf[y]:='';                     { Felder zusammenkopieren }
            while (tausc<UsrFelderMax) do begin
              inc(tausc);
              if (taus[tausc]='') then continue;           { Leere Felder Åbergehen }
              abc:=' ';
              if (dispbuf[y]='') then abc:='';
              dispbuf[y]:=dispbuf[y]+abc+taus[tausc];
            end;
            dispbuf[y]:=forms(dispbuf[y], ScreenWidth);            { Sicherheitshalber nochmal kÅrzen }

            asm                  { Bit 3-5 codieren Prioritaet... Farbflag setzen: }
                mov al,uflags    { 000=normal   -> 0     100=niedrigste -> 5       }
                shr al,5         { 001=hoechste -> 2     101=    ?      -> 6       }
                and al,7         { 010=hoch     -> 3     110=    ?      -> 7       }
                je @1            { 011=niedrig  -> 4     111=    ?      -> 8       }
                inc al
            @1: mov helpbyte,al
            end;
            userflag[y]:=helpbyte;
          end;
        end;
  10..12: begin { NACHRICHTENLISTE }
            if auswahlcursor and blind then hf := ' +-A.'
            else hf := ' +-A˘';

            for tausc := 1 to MsgFelderMax do taus[tausc]:='';

            _brett := dbReadNStr(mbase,mb_brett);
            s0:= dbReadNStr(mbase,mb_absender);
            dbReadN(mbase,mb_netztyp,mnt);
            fileatt:=(mnt and $200<>0);
            netztyp:=mnt and $ff;
            uv_anz:=(mnt shr 16) and $ff;
            pp:=cpos('@',s0);
            fidoqwk:=(netztyp=nt_Fido) or (netztyp=nt_QWK);
            Short_Realnames:=Showrealos and not ((netztyp=nt_Maus) or fidoqwk);

            {-------}
            h2:=0; h3:=0;
            if cpos('A',MsgFeldTausch)=0 then inc(h2,19) else inc(h3);
            if cpos('E',MsgFeldTausch)=0 then inc(h2,19) else inc(h3);
            if cpos('B',MsgFeldTausch)=0 then inc(h2,21) else inc(h3);
            if cpos('D',MsgFeldTausch)=0 then inc(h2,6);
            if cpos('G',MsgFeldTausch)=0 then inc(h2,6);
            if cpos('F',MsgFeldTausch)=0 then inc(h2,7);
            if not (fidoqwk and (dispmode=10) and dispfto and (_brett[1]='A'){})
            then begin
              if cpos('E',MsgFeldTausch)=0 then dec(h2,19)
              else dec(h3);
              end;
            if h3>0 then h3:=h2 div h3;
            {-------}

            if ((netztyp=nt_Maus) or fidoqwk) and (pp>0) then
              s:=LeftStr(s0,pp-1)+' @ '+mid(s0,pp+1)
            else
              if showrealos and (dbReadStrN(mbase,mb_name)<>'') and
                 ((dispmode<>11) or not markunversandt) then
                s:= dbReadNStr(mbase,mb_name)
              else begin
                s:=s0;
                if sabsender<>0 then begin
                  if RightStr(s,4)='.ZER' then delete(s, length(s)+1-4, 4); {dec(byte(s[0]),4);}
                  case sabsender of
                    1 : if pp=0 then s:=TopAllStr(s)
                        else s:=TopAllStr(LeftStr(s,pp-1))+'@'+copy(s,pp+1,length(s)-pp);
                    2 : if pp=0 then s:=TopAllStr(s)
                        else s:=TopAllStr(LeftStr(s,pp-1))+' @ '+copy(s,pp+1,length(s)-pp);
                    3 : if pp>0 then s:=LeftStr(s,pp-1);
                    4 : s:=TopAllStr(LeftStr(s,iif(pp>0,pp-1,length(s))));
                    5 : if pp>0 then s:=forms(LeftStr(s,pp-1),16)+' '+copy(s,pp+1,length(s)-pp);
                    6 : if pp=0 then s:=TopAllStr(s)
                        else s:=TopAllStr(forms(LeftStr(s,pp-1),16))+' '+copy(s,pp+1,length(s)-pp);
                  end;
                end;
              end;

            s1:= dbReadNStr(mbase,mb_betreff);

            if dispmode<>12 then
            begin
(*           if (netztyp=nt_Maus) and (mnt and $100<>0) then
                s1:=LeftStr('-'+s1,40) else
             if ntKomkette(netztyp) and (mnt and $100<>0) and
                 (LowerCase(LeftStr(s1,3))<>'re:') and (LeftStr(s1,3)<>'Re^') then
                s1:=LeftStr('-'+s1,40);
*)
             if ((netztyp=nt_Maus) and (mnt and $100<>0)) or
                (ntKomkette(netztyp) and (mnt and $100<>0) and
                 (LowerCase(leftStr(s1,3))<>'re:') and (LeftStr(s1,3)<>'Re^'))
               then s1:='-'+s1
             end;

            dbReadN(mbase,mb_OrigDatum,dat);
            if dispmode=10 then                    { 10=Nachrichten in DispBrett (auch To-Brett!)}
              dbReadN(mbase,mb_EmpfDatum,edat)
            else
              edat:=ixdat('6912310000');
            { Datum anzeigen }
            if showmsgdatum then begin
              s2:=fdat(longdat(dat));
              decodedate(now,j,m,t);

              tempd := ival(copy(s2,7,2));
              if tempd < 70 then inc(tempd, 2000) else inc(tempd, 1900);
              if abs(tempd*12+ival(copy(s2,4,2))-1-(j*12+m-1)) > 11 then
                sdat:=copy(s2,4,2)+'/'+copy(s2,7,2)
              else
                sdat:=LeftStr(s2,5);
              ulen:=iif(over80>2,27,25);    { wenn Platz fÅr vollen Username  }
              blen:=iif(over80>6,40,36);    { auch noch Platz fÅr den Betreff }
            { kein Datum anzeigen }
            end
            else begin
              sdat:='';
              ulen:=27;
              blen:=40;
            end;
            if (dispmode=11) and MarkUnversandt then
            begin      {11=markierte Nachrichten}
              inc(ulen,5); dec(blen,5);
              hd := THeader.Create;
              readheader(hd,hds,true);
              if hd.wab<>'' then s:=hd.wab;
              pp:=cpos('@',s);
              s:=copy(s,pp+1,length(s)-pp);
              pp:=cpos('.',s);
              if pp>0 then s:=LeftStr(s,pp-1);
              if hd.real_box<>'' then s:=hd.real_box;
              if LeftStr(hd.Firstempfaenger,length(TO_ID))=TO_ID then
                s:=forms(mid(hd.Firstempfaenger,length(TO_ID)+1),ulen-length(s)-1)+' '+s
              else
                s:=forms(hd.Firstempfaenger,ulen-length(s)-1)+' '+s;
              Hd.Free;
            end;
            _brett:= dbReadNStr(mbase,mb_brett);
            pp:=iif(netztyp=nt_Maus,35,iif(fidoqwk,35,0));
            if (pp<>0) and (_brett[1]<>'$') and (blen>pp) then
            begin
              inc(ulen,blen-pp);
              blen:=pp;
            end;
            dbReadN(mbase,mb_Groesse,size);
            dbReadN(mbase,mb_unversandt,unvers);
            dbReadN(mbase,mb_Typ,typ);
            dbReadN(mbase,mb_flags,flags);
            if typ='T' then typ:=iifc(fileatt,'F',iifc(flags and 4<>0,'M',' '));
            wvl:=(unvers and 12) div 4;

            if (wvl=0) and (mnt and $4000<>0) then
              wvl:=5;

            dbReadN(mbase,mb_Halteflags,hflags);
            dbReadN(mbase,mb_gelesen,gelesen);
            if unvers and 32<>0 then
              unvers:=4
            else
              unvers:=unvers and 1 + (unvers and 16) div 8;
            if (unvers=1) and (uv_anz>1) then unvers:=min(uv_anz,10)+3;
            if (hflags=0) and (unvers=0) and (abhdatum<>0) and
               smdl(edat,abhdatum)
            then
              hflags:=4;
            if dispmode=11 then
              markflag[y]:=1
            else
              markflag[y]:=iif(msgmarked,1,0);
            dflags:=iifc(markflag[y]<>0,suchch,' ')+ge[gelesen]+hf[hflags]+
                    uv[unvers]+wv[wvl]+typ;

            if fidoqwk and (dispmode=10) and
               dispfto and (FirstChar(_brett)='A') then
            begin
              if cpos('@',s)>0 then SetLength(s, cpos('@',s)-1); {s[0]:=chr(cpos('@',s)-1);}
              s0:= dbReadNStr(mbase,mb_name);

              ulen:=19; {Absender-laenge}
              msgtausch('E',forms(s0,19+h3));                      { (E)mpfaenger }
              msgtausch('B',forms(s1,iif(showmsgdatum,21,27)+h3+
                iif(Short_Realnames and not (MarkUnversandt and (Dispmode=11)),5,0))); { (B)etreff }
              userflag[y]:=IIf((HighlightName=UpperCase(s0)) or (mnt and $1000<>0),1,0);
            end
            else { Nicht-Fido bzw. Dispmode 11,12 }
            begin
              if dispmode=12 then                 { Kommentarbaum }
              begin
                s:=' '+BaumBlatt(ReplyTreeOfs,bezpos,s,s1);
                if (sdat<>'') then sdat:=' '+sdat;
                dispbuf[y]:=forms(dflags+siz+sdat+' '+s,ScreenWidth);

              end
              else msgtausch('B',forms(s1,blen-1+h3+
                iif(Short_Realnames and not (MarkUnversandt and (Dispmode=11)),5,0))); { (B)etreff }

              asm                  { Bit 3-5 codieren Prioritaet... Farbflag setzen: }
                  mov al,flags     { 000=normal   -> 0     100=niedrigste -> 5       }
                  shr al,3         { 001=hoechste -> 2     101=    ?      -> 6       }
                  and al,7         { 010=hoch     -> 3     110=    ?      -> 7       }
                  je @1            { 011=niedrig  -> 4     111=    ?      -> 8       }
                  inc al
              @1: mov helpbyte,al
              end;
              userflag[y]:=helpbyte;

              if (fidoqwk and (_brett[1]='A') and
                  (HighlightName=UpperCase(dbReadStr(mbase,'name'))))
                OR
                 ((cpos('@',dispspec)>0) and
                  (UpperCase(leftStr(s0,40))=UpperCase(copy(dispspec,2,min(40,length(s0))))))
                OR
                 (mnt and $1000<>0)
                  then userflag[y]:=1

            end;     {-*- Ab hier wieder alle Netze/Dispmodes -*-}

            msgtausch('F',dflags);                     { (F)lags }
            msgtausch('G',siz);                        { (G)roesse }
            msgtausch('D',iifs(showmsgdatum,sdat,'')); { (D)atum }
            msgtausch('A',forms(s,ulen+h3-
              iif(Short_Realnames and not (MarkUnversandt and (Dispmode=11)),5,0))); { (A)bsender }

            if (dispmode<>12) then begin            { Kommentarbaum-Ausgabe ist fertig }
              tausc:=0; dispbuf[y] :='';            { Ansonsten Felder zusammenkopieren }
              while (tausc<MsgFelderMax) do begin
                inc(tausc);
                if (taus[tausc]='') then continue; { Leere Felder Åbergehen }
                if (dispbuf[y]<>'') then dispbuf[y]:=dispbuf[y]+' ';
                dispbuf[y]:=dispbuf[y]+taus[tausc];
              end;
              // Sicherheitshalber nochmal auf ScreenWidth Zeichen kÅrzen
              dispbuf[y]:=forms(dispbuf[y],ScreenWidth);
            end;
          end;
     20 : dispbuf[y]:=AutoShow;
  end;

  if show then begin
    lcol(y,p);
    moff;
    fwrt(1,3+y+ya,dispbuf[y]);
    mon;
  end;
  disprec[y]:=dbRecNo(dispdat);
end;

procedure RedispLine;
begin
  write_disp_line(p,p,true);
end;


{$IFNDEF FPC }
  {$B+}
{$ENDIF }
procedure display(p:shortint);
var i,j : integer;
    mi  : shortint;
begin
  i:=1;
  fillchar(disprec,sizeof(disprec),0);
  mi:=dbGetIndex(mbase);
  if (dispmode=11) or (dispmode=12) then dbSetIndex(mbase,0);
  repeat
    write_disp_line(i,p,true);
    inc(i);
  until (i>gl) or not Forth;
  dbSetIndex(mbase,mi);
  if i<=gl then begin
    if dispmode<10 then attrtxt(col.colbretter)
    else attrtxt(col.colmsgs);
    clwin(1,screenwidth,3+i+ya,3+ya+gl);
    for j:=i to gl do dispbuf[j]:='';
    end;
  aufbau:=false;
  if dispmode=12 then xaufbau:=false;
  mdisplay:=false;
end;
{$IFNDEF FPC }
  {$B-}
{$ENDIF }

procedure redisplay(p:shortint);
var i,bp : integer;
begin
  i:=1;
  bp:=bezpos;
  while (i<=gl) and (disprec[i]<>0) do begin
    dbGo(mbase,disprec[i]);
    bezpos:=i;
    write_disp_line(i,p,true);
    inc(i);
    end;
  bezpos:=bp;
  if i<=gl then begin
    if dispmode<10 then attrtxt(col.colbretter)
    else attrtxt(col.colmsgs);
    clwin(1,screenwidth,3+i+ya,3+ya+gl);
    end;
  mdisplay:=false;
end;


procedure scrollup(show:boolean);
var
    i : integer;
    s : string;
begin
  s:=dispbuf[1];
  for i:= 2 to maxgl do
    dispbuf[i-1]:= dispbuf[i];
  dispbuf[maxgl]:= s;
  Move(disprec[2],disprec[1],sizeof(disprec)-4);
  disprec[maxgl]:=0;
  Move(markflag[2],markflag[1],sizeof(markflag)-1);
  Move(userflag[2],userflag[1],sizeof(userflag)-1);
  if show then begin
    moff;
    for i:=1 to gl-1 do begin
      lcol(i,0);
      fwrt(1,i+3+ya,dispbuf[i]);
      end;
    mon;
    end;
end;

procedure scrolldown(show:boolean);
var
  i : integer;
  s : string;
begin
  s:= dispbuf[maxgl];
  for i:= maxgl downto 2 do
    dispbuf[i]:= dispbuf[i-1];
  dispbuf[1]:= s;

  Move(disprec[1],disprec[2],sizeof(disprec)-4);
  Move(markflag[1],markflag[2],sizeof(markflag)-1);
  Move(userflag[1],userflag[2],sizeof(userflag)-1);
  if show then begin
    moff;
    for i:=2 to gl do begin
      lcol(i,0);
      fwrt(1,i+3+ya,dispbuf[i]);
      end;
    mon;
    end;
end;

{
  $Log$
  Revision 1.56  2002/03/25 22:03:09  mk
  MY:- Anzeige der Stammbox-Adresse unterhalb der MenÅleiste korrigiert
       und Åberarbeitet (bei aktivierter Option "C/A/D/Stammbox-Adresse
       anzeigen"):
       - VollstÑndige Adresse (statt nur Feld "Username") inkl. Domain
         wird angezeigt;
       - Alias-Points werden berÅcksichtigt (RFC/UUCP und ZConnect);
       - Realname wird in Klammern angezeigt (falls es sich um einen
         Netztyp mit Realnames handelt) und ggf. automatisch gekÅrzt, wenn
         die GesamtlÑnge von Adresse und Realname grî·er als 76 Zeichen
         ist;
       - Bei einem Wechsel des Netztyps der Stammbox wird die Anzeige
         der Absenderadresse unterhalb der MenÅleiste unmittelbar nach dem
         Wechsel aktualisiert.

  Revision 1.55  2002/01/30 17:29:52  mk
  - fixed dbReadN with AnsiString

  Revision 1.54  2002/01/22 19:15:29  mk
  - after 3.40 merge fixes

  Revision 1.53  2002/01/13 15:15:51  mk
  - new "empfaenger"-handling

  Revision 1.52  2002/01/13 15:07:29  mk
  - Big 3.40 Update Part I

  Revision 1.51  2001/10/14 21:53:38  ma
  - fixed: Long area names did not display properly

  Revision 1.50  2001/09/08 14:30:35  cl
  - adaptions/fixes for PGP/MIME support

  Revision 1.49  2001/08/23 11:15:02  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.48  2001/08/12 11:50:39  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.47  2001/08/11 23:06:31  mk
  - changed Pos() to cPos() when possible

  Revision 1.46  2001/07/31 13:10:33  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.45  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.44  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.43  2000/12/03 12:38:22  mk
  - Header-Record is no an Object

  Revision 1.42  2000/11/17 00:15:47  mk
  - Virtual Pascal compatibility updates

  Revision 1.41  2000/11/16 21:31:06  hd
  - DOS Unit entfernt

  Revision 1.40  2000/11/12 11:34:05  mk
  - removed some limits in Reply Tree
  - implementet moving the tree with cursor keys (RB)
  - optimized display of the tree

  Revision 1.39  2000/10/22 21:58:58  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.38  2000/10/17 10:05:50  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.37  2000/08/15 11:12:24  mk
  MO: Bugfixes und Anpassungen fuer > 80 Spalten

  Revision 1.36  2000/08/09 21:08:18  mk
  MO: -Anzeige > 80 Spalten Anzeigefehler behoben

  Revision 1.35  2000/08/03 15:42:07  mk
  MO: weitere Anpassungen fuer mehr als 80 Spalten

  Revision 1.34  2000/08/03 10:42:30  mk
  MO: - Anpassungen fuer mehr als 80 Spalten

}

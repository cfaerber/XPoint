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

{ --- Timing-Listen-Interpreter --------------------------------------- }

{ Es wird pro Tag eine Timing-Liste zusammengestellt, sortiert nach     }
{ Anfangszeit. Der erste Eintrag wird jeweils als nÑchstes ausgefÅhrt.  }
{ Nicht erfolgreiche Netcalls werden zurÅckgestellt. Alle EintrÑge mit  }
{ von<=time<=bis sind *aktiv*. Sind unter den aktiven EintrÑgen mehrere }
{ Netcalls, dann werden diese immer vor anderen Aktionen ausgefÅhrt.    }
{ öber nxtime wird bei Ende eines Netcalls festgelegt, wann er frÅhe-   }
{ stens wiederholt werden darf (time+RedialWait).                       }
{                                                                       }
{ Achtung: 'active' hat hier eine andere Bedeutung als im Timinglisten- }
{ Editor!                                                               }
{                                                                       }
{ callall=true ->  tnr=0 -> Alle-Anruf mit Auswahl                      }
{                  tnr<0 -> Alle-Anruf ohne Auswahl                     }
{                                                                       }
{ crashall=true -> tnr=0 -> alle Crashs/Requests                        }
{                  tnr=1 -> nur Crashs/Requests aus CRASH.TMP           }


procedure AutoTiming(tnr:integer; callall,crashall,special:boolean; datLine:byte);
var brk     : boolean;
    tl      : array[1..maxentries] of TRP;
    anz,i   : integer;
    ldate   : string;
    x,y,gl  : Integer;
    anzeige : boolean;
    ende    : boolean;
    endtime : string;

    startdat: string;
    netcalls: boolean;
    lastbusy: array[1..MaxCom] of string;
    _anz    : integer;
    lsec    : string;
    f       : file;

const NetcallAlleFlag = 'NETCALL.ALL';  { N/A-Semaphore fÅr Batchauswertung bei RFC/Client }
      NetcallEndeFlag = 'NETCALL.END';  { N/A-Semaphore (letzte Box) fÅr Batchauswertung bei RFC/Client }

  { testen, ob tr.action am tag dat zwischen von und bis }
  { ausgefÅhrt wurde.                                    }

  function intime(dat,von,bis:string; var tr:TimeRec):boolean;
  var t   : text;
      s   : string;
      p   : byte;
      it  : boolean;
      buf : array[0..1023] of byte;
  begin
    dat:=LeftStr(dat,6)+RightStr(dat,2);
    it:=false;
    assign(t,TimingDat);
    if existf(t) then begin
      settextbuf(t,buf);
      reset(t);
      while not eof(t) and not it do begin
        readln(t,s);
        UpString(s);
        p:=cpos('=',s);
        if (p>0) and (LeftStr(s,p-1)=UpperCase(tr.action)) and (copy(s,p+1,8)=dat) and
           (copy(s,p+10,5)>=von) and (copy(s,p+10,5)<=bis) then
            it:=true;
        end;
      close(t);
      end;
    intime:=it;
  end;

  procedure parse_liste;
  var i,j     : integer;
      tr,tr2  : TimeRec;
      dat,tim : datetimest;
      t       : TRP;
      p       : byte;
      s       : string;
      usebox  : string;
      lastbox : string;

    function tf(s:string):string;
    begin
      tf:=copy(s,4,2)+LeftStr(s,2);
    end;

  begin
    anz:=0;
    i:=0;
    dat:=tf(date); tim:=LeftStr(time,5);
    lastbox:='';
    while i<e.Count do
    begin   { -1 wegen Splitting }
      Str2Time(e[i],tr);
      with tr do
        if active and (tf(vond)<=dat) and (tf(bisd)>=dat) and ((bis>=tim) or (von>bis))
           and wotag[dow(date)]
        then begin
          s:=trim(action);
          p:=cpos(' ',s);
          if p=0 then box:=''                  { Boxname isolieren }
          else box:=trim(mid(s,p));
          if p>0 then s:=trim(LeftStr(s,p));
          comm:=0;
          for j:=1 to comms do                   { Befehls-Nummer ermitteln }
            if UpperCase(s)=comstr[j] then comm:=j;
          if ((comm=5) or (comm=6)) and (box<>'') then begin
            if (ival(box)>=0) and (ival(box)<=255) then
              qerrlevel:=ival(box)
            else
              qerrlevel:=0;
            box:='';
            end;
          if (comm=0) and (UpperCase(s)='CRASH') then begin
            crash:=true; comm:=1;
            crashtime:=false;
            UseBox:=DefFidoBox;
            if cpos(' ',box)>0 then begin  { ZEIT-Option }
              box:=LeftStr(box,cpos(' ',box)-1);
              if (pos(' ZEIT',UpperCase(action))>0) or (pos(' TIME',UpperCase(action))>0)
              then
                crashtime:=true;
              end;
            end
          else begin
            crash:=false; crashtime:=false;
            usebox:=box;
            end;
          if ((comm=1) and IsBox(usebox)) or (comm>1) then begin
            nxtime:='';
            if (comm=1) and (usebox<>lastbox) then begin
              ReadBoxPar(0,usebox);
              ncconn:=boxpar^.connectmax;
              lastbox:=usebox;
              comport:=boxpar^.bport;
              redialwait:=boxpar^.redialwait;
              end;
            tr2:=tr;
            if bis<von then tr2.bis:='23.59';
            if not (intime(date,tr2.von,tr2.bis,tr) or
                    ((bis<von) and intime(prevd(date),von,'23:59',tr)))
            then
            begin
              inc(anz);
              new(tl[anz]);
              fillchar(tl[anz]^, SizeOf(tl[anz]^), 0);
              tl[anz]^:=tr2;
            end;
            if (bis<von) and (bis>=tim) then
              if not intime(date,'00:00',bis,tr) then begin
                inc(anz);
                new(tl[anz]);
                fillchar(tl[anz]^, SizeOf(tl[anz]^), 0);
                tr.von:='00:00';
                tl[anz]^:=tr;
                end;
            end;
          end;
      inc(i);
      end;
    for i:=anz-1 downto 1 do   { Bubble Sort nach Uhrzeit }
      for j:=1 to i do
        if tl[j]^.von+tl[j]^.bis > tl[j+1]^.von+tl[j+1]^.bis then begin
          t:=tl[j]; tl[j]:=tl[j+1]; tl[j+1]:=t;
          end;
  end;

  procedure MakeAllListe(var brk:boolean; auto:boolean);
  var d          : DB;
      fn         : String;
      t          : text;
      ti         : string;
      all        : string;
      clientbox  : string;
      currentbox : string;
      box        : string;
      x,y        : integer;
      i,p,nt     : byte;
      datDa      : boolean;

    procedure InitNetcallSpecial;  { NETCALL.DAT beim Aufruf von Netcall/Spezial laden bzw. erstellen }
    var i          : byte;
        netcalldat : text;
    begin
      if not FileExists(ownpath+NetcallSpecialDat) then
      begin
        datDa:=false;
        if auto then
        begin
          trfehler(1016,60);   { 'Datei NETCALL.DAT nicht vorhanden' }
          exit;
        end;
        if not ReadJN(getres2(11000,16)+' '+getres2(11000,17),true) then exit;
                               { 'Datei NETCALL.DAT nicht vorhanden - neu anlegen' }
        EditNetcallDat;
        exit;
      end;
      datDa:=true;
      ReadNetcallSpecialData;
    end;

  begin
    all:='';
    clientbox:='';
    currentbox:='';
    if special then
    begin
      InitNetcallSpecial;
      if not datDa then exit;
    end;
    dbOpen(d,BoxenFile,1);
    if special then
    begin
      if auto then   { /nsp:1..20 (automatisch) }
      begin
        i:=datLine;
        if trim(NetcallSpecialList[i]) = '' then
        begin
          trfehler1(1019,strs(i),60);  { 'NETCALL.DAT enthÑlt keinen gÅltigen Eintrag in Zeile %s!' }
          dbClose(d);
          exit;
        end;
      end
      else begin     { Netcall/Spezial (manuell) }
      { ersten nicht-leeren Eintrag fÅr Anzeige im Eingabefeld ermitteln }
        i:=1;
        while (trim(NetcallSpecialList[i]) = '') and (i <= NetcallSpecialMax) do
          inc(i);
        if trim(NetcallSpecialList[i]) = '' then
        begin
          rfehler(1018);  { 'NETCALL.DAT enthÑlt keine gÅltigen EintrÑge (Zeilen 1-20)!' }
          dbClose(d);
          exit;
        end;
      end;
      all:=iifs(i>9,'',' ')+strs(i)+':  '+trim(NetcallSpecialList[i]);
    end
    else
      while not dbEOF(d) do
      begin
        if dbReadInt(d,'script') and 2=0 then
        begin
          currentbox := dbReadStr(d,'boxname');
          dbRead(d,'Netztyp',nt);
          ReadBox(nt,dbReadStr(d,'dateiname'),boxpar);
          if nt=nt_Client then          { RFC/Client? }
          begin
            clientbox := clientbox+' '+currentbox;
            currentbox := '';
          end;
          if currentbox <> '' then all:=all+' '+currentbox;
        end;
        dbNext(d);
      end;

    if not special then all:=trim(all + clientbox);
    if all='' then brk:=true
    else begin
      if auto then
        brk:=false
      else begin
        if special then
          dialog(72,3,getres2(1024,1),x,y)     { 'Spezial-Netcall bei:' }
        else
         dialog(72,3,getres2(1016,1),x,y);    { 'Netcall bei:' }
        maddstring(3,2,'',all,66,255,'');
        if special then
          for i:=1 to NetcallSpecialMax do
            if trim(NetcallSpecialList[i]) <> '' then  { nur EintrÑge anzeigen, die nicht leer sind }
              mappsel(false,iifs(i>9,'',' ')+strs(i)+':  '+
                      trim(NetcallSpecialList[i]));
        readmask(brk);
        enddialog;
        end;
      if special and (cpos(':',all)=3) then
        all:=uppercase(trim(Mid(all,4)))+' '
      else
        all:=uppercase(trim(all))+' ';

      if not brk then begin
        fn:=TempS(1000+dbRecCount(d)*200);
        assign(t,fn);
        rewrite(t);
        ti:=LeftStr(time,5);
        p:=cpos(' ',all);
        while p>0 do begin
          box:=LeftStr(all,p-1);
          dbSeek(d,boiName,UpperCase(box));
          if dbFound then
            writeln(t,'+ '+ti+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ NETCALL ',box);
          delete(all,1,p);
          while FirstChar(all)=' ' do DeleteFirstChar(all);
          p:=cpos(' ',all);
          end;
        close(t);
        loadfile(1,fn);
        erase(t);
        end;
      end;
    dbClose(d);
  end;

  procedure ResolveCrashs;   { s. auch XP7F.GetCrashbox! }
  var i   : integer;
      t   : text;
      ss  : string;
      sc  : string;
      adr : string;
      ni  : NodeInfo;
      c,f : boolean;
      crash: boolean;
      d   : DB;
  begin
    i:=0;                               //auch bei der Timingliste beginnen wir bei 0
    while (i< e.Count) do
      if (copy(e[i],37,6)='CRASHS') or (copy(e[i],37,8)='REQUESTS') then begin { liegen crashes oder request an}
        crash:=(copy(e[i],37,6)='CRASHS');  { es sollen offene crashs erledigt werden }
        ss:=LeftStr(e.Strings[i],36);
        if i<e.Count then e.Delete(i);      { crash aus der Liste entfernen }
        assign(t,ReqDat);
        if existf(t) then begin
          reset(t);
          KeepNodeindexOpen;
          dbOpen(d,BoxenFile,1);
          while not eof(t) do begin
            readln(t,adr);
            c:=false; f:=false;
            repeat
              readln(t,sc);
              if sc=CrashID then c:=true
              else if (sc<>'') and (sc[1]<>'>') then f:=true;
            until sc='';
            getNodeinfo(adr,ni,2);
            if ((not crash and f) or (crash and not f and c)) and  { aktuellen crash gefunden ?}
               ni.found and (e.Count<maxentries) then begin
              dbSeek(d,boiName,adr);
              if not dbFound then begin     { keine eingetragene Pollbox }
                sc:=ss+'CRASH '+adr;
                e.Insert(i, sc);
                inc(i);
                end;
              end;
            end;
          dbClose(d);
          KeepNodeindexClosed;
          close(t);
          end;
        end
      else
        inc(i);
  end;

  procedure MakeCrashListe;
  var fn   : string;
      t,t2 : text;
      s    : string[30];
  begin
    fn:=TempS(1000);
    assign(t,fn);
    rewrite(t);
    assign(t2,CrashTemp);
    if (tnr=0) or not existf(t2) then begin
      writeln(t,'+ '+LeftStr(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ CRASHS');
      writeln(t,'+ '+LeftStr(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ REQUESTS');
      end
    else begin
      reset(t2);
      while not eof(t2) do begin
        readln(t2,s);
        writeln(t,'+ '+LeftStr(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ CRASH '+s);
        end;
      close(t2);
      _era(CrashTemp);
      end;
    close(t);
    loadfile(1,fn);
    resolvecrashs;
    erase(t);
  end;


  procedure show_active;
  const ltc : string = '';
  var i      : integer;
      tc     : string;
  begin
    tc:=iifs(ticker mod 26<13,' '#4,#4' ');
    if tc<>ltc then begin
      ltc:=tc;
      attrtxt(col.colmbox);
      moff;
      for i:=1 to min(gl,anz) do
        wrt(x+2,y+i+1,iifs(tl[i]^.active,tc,'  '));
      mon;
      end;
  end;

  procedure display;
  var i : integer;
  begin
    attrtxt(col.colmbox);
    attrtxt(col.colmboxhigh);
    if anz=0 then begin
      clwin(x+1,x+58,y+1,y+gl+2);
      mwrt(x+10,y+3,getres2(1016,2));  { '-- keine weiteren EintrÑge fÅr heute --' }
      end
    else begin
      moff;
      for i:=1 to gl do
        if i<=anz then
          with tl[i]^ do
            wrt(x+2,y+i+1,'   '+von+'-'+bis+'  '+forms(action,41))
        else
          wrt(x+2,y+i+1,sp(57));
      mon;
      end;
    if anz>gl then mwrt(x+5,y+gl+2,'...')
    else mwrt(x+5,y+gl+2,'   ');
    anzeige:=false;
    show_active;
  end;

  procedure disprest;
  const lt : longint = 999;
  var t : longint;
      s : string;
  begin
    t:=TimeDiff(endtime,time);
    if t<>lt then begin
      lt:=t;
      s:=formi(t div 3600,2)+':'+formi((t div 60)mod 60,2)+':'+formi(t mod 60,2);
      attrtxt(col.colmbox);
      mwrt(x+49,y+gl+2,s);
      end;
  end;

  { evtl. noch hinzufÅgen: untenstehende, aktive Netcalls }
  { "nach oben schwimmen" lassen                          }

  procedure set_active;
  var i : integer;
  begin
    i:=1;                               { zuerst mal die alten rauswerfen.. }
    while (i<=anz) do
      if tl[i]^.bis+':59'<time then begin
        if i<anz then
          Move(tl[i+1],tl[i],(anz-i)*4);
        dec(anz);
        anzeige:=true;
        end
      else
        inc(i);
    for i:=1 to anz do                  { und dann die aktiven ermitteln }
      with tl[i]^ do
        active:=(von<=time) and (time<=bis+':59');
  end;

  function addtime(t:datetimest; sec:word):datetimest;
  var l : longint;
  begin
    l:=ival(LeftStr(t,2))*3600+ival(copy(t,4,2))*60+ival(RightStr(t,2))+sec;
    addtime:=formi(l div 3600,2)+':'+formi((l div 60)mod 60,2)+':'+
             formi(l mod 60,2);
  end;

  { tl[1]^ ausfÅhren }
  procedure execute1;
  var ok,brk : boolean;
      i      : integer;
      t      : TRP;
      p      : scrptr;
      rwait  : integer;
      nt     : string;
  begin
    ok:=true;
    with tl[1]^ do
      case comm of
        1 : begin               { NETCALL <Box> }
              sichern(p);
              rwait:=boxpar^.RedialWait;
              CrashGettime:=crashtime;
              nt:=time;
              ok:=netcall(true,box,true,false,crash);
              CrashGettime:=false;
              if Netcall_connect then begin
                netcalls:=true;
                if not ok then begin
                  dec(ncconn);
                  if ncconn=0 then ok:=true;
                  end;
                end
              else
                lastbusy[boxpar^.bport]:=nt;
              holen(p);
              nxtime:=addtime(time,rwait);
            end;
        2 : begin                { REORG }
              MsgReorgScan(true,false,brk);
              if not brk then
                MsgReorg;
            end;
        3 : PackAll(false);      { PACK }
        4 : begin                { EXEC <Cmd> }
              shell(trim(mid(action,6)),600,1);   { Bild komplett lîschen }
              wrtiming(action);
            end;
      5,6 : begin                { QUIT [n] }
              ende:=true; quit:=true;
              if comm=6 then WrTiming(action);
              errlevel:=qerrlevel;
            end;
        7 : AutoExec(false);
       10 : ende:=true;          { END }
       11 : if DoDiffs(FilePath+'*.*',true)=0 then;     { NODEDIFFS }
      end;
    if ok or (tl[1]^.bis<leftStr(time,5)) then begin
      dispose(tl[1]);
      dec(anz);
      if anz>0 then Move(tl[2],tl[1],anz*4);
      end
    else begin       { Netcall nach unten rotieren }
      i:=1;
      t:=tl[1];
      while (i<anz) and (tl[i+1]^.active) and (tl[i+1]^.comm=1) and
            (tl[i+1]^.nxtime<=t^.nxtime) do begin
        tl[i]:=tl[i+1];
        inc(i);
        end;
      tl[i]:=t;
      end;
    if anz>0 then
      with tl[1]^ do begin
        if active then
          if comm=1 then
            endtime:=iifs(nxtime='',time,nxtime)
          else
            endtime:=time
        else
          endtime:=von+':00';
        if (comm=1) and (comn[comport].postsperre) then
          endtime:=maxs(endtime,addtime(lastbusy[comport],redialwait));
        end;
    anzeige:=true;
  end;

  procedure addendtime(n:shortint);
  var h,m,s : integer;
  begin
    h:=ival(LeftStr(endtime,2));
    m:=ival(copy(endtime,4,2));
    s:=ival(RightStr(endtime,2));
    inc(s,n);
    if s<0 then begin
      s:=59; dec(m);
      if m<0 then begin
        m:=59; dec(h);
        end;
      end;
    if s>59 then begin
      s:=0; inc(m);
      if m>59 then begin
        m:=0; inc(h);
        end;
      end;
    endtime:=formi(h,2)+':'+formi(m,2)+':'+formi(s,2);
  end;

begin    { procedure AutoTiming(tnr:integer; callall,crashall:boolean);}
  filewidth:=timingwidth;
  if crashall then begin
    MakeCrashliste;
    _anz:=anzahl;
    end
  else if not callall then begin
    if tnr=0 then tnr:=ReadTimingNr(brk)     { hole nr der Timingliste }
    else brk:=false;
    if brk then exit;
    loadfile(1,TimingFile+strs(tnr));
    _anz:=anzahl;
    resolvecrashs;
    end
  else begin
    MakeAllListe(brk,tnr<0);
    if brk then exit;
    tnr:=0;
    _anz:=anzahl;                          { Anzahl der EintÑge merken }
    end;
  if _anz=0 then begin
    trfehler(iif(callall,1005,1006),60);   { 'keine zutreffenden Boxen' / 'leere Timing-Liste' }
    exit;
    end;
  if callall then MakeFile(ownpath+NetcallAlleFlag);
  gl:=screenlines-fnkeylines-12;
  startdat:=ZDate;
  netcalls:=false;
  for i:=1 to MaxCom do
    lastbusy[i]:='00:00:00';

  ende:=false;
  repeat
    ldate:=date;
    moment;
    parse_liste;
    if anz=0 then endtime:='24:00:00'
    else endtime:=iifs(tl[1]^.von<=LeftStr(time,5),time,tl[1]^.von+':00');
    closebox;
    msgbox(60,gl+4,getres2(1016,3)+iifs(callall,'',' / #'+strs(tnr)),x,y);   { 'Netcall-Automatik' }
    mwrt(x+48,y,' '+LeftStr(ldate,6)+RightStr(ldate,2)+' ');
    anzeige:=true;
    initscs;
    lsec:=RightStr(time,2);

    repeat                              { Die Gro·e Schleife, Warten auf das nÑchste Ereignis }
      if RightStr(time,2)<>lsec then begin { eine neue Sekunde angebrochen }
        lsec:=RightStr(time,2);            { lsec erneuern }
        dec(scsavecnt);                 { SreenSaveCounter }
        if scsavecnt=0 then begin
          if timediff(endtime,time)>10 then begin     { ist die Zeit bis zum nÑhsten Ereignis > 10 sec }
            addendtime(-2); TimedScsaver(endtime); addendtime(2);  {screensaver einschlten }
            end;
          initscs;                      { screensave counter neu initialisieren }
          end;
        end;
      set_active;
      if anzeige then display           { Anzeige erneueren }
      else show_active;
      multi2;
      if endtime>=time then disprest;
      if (anz>0) and (time>=endtime) then begin
        if (anz=1) and (callall) and (Fileexists(ownpath+NetcallAlleFlag)) then
          RenameFile(ownpath+NetcallAlleFlag,ownpath+NetcallEndeFlag);
        execute1;
        initscs;
        end;
      ende:=ende or (callall and (anz=0));
      while keypressed do                                       { wurde Taste gedrÅckt }
        case readkey of
          #27 : ende:=true;                                     { esc   = timing abbrechen }
          ' ' : endtime:=time;                                  { space = sofort ausfÅhren }
          '+' : if endtime<'23:59:59' then addendtime(1);       { +     = Zeitspanne erhîhen }
          '-' : if endtime>time then addendtime(-1);            { +     = Zeitspanne ernidrigen}
        end;
      if not ende then XpIdle;
    until ende or (date<>ldate);

    initscs;
    if netcalls then write_lastcall(startdat);
    closebox;
    for i:=1 to anz do
      dispose(tl[i]);
    if not ende then begin
      AutoSend;
      AutoExec(false);
      end;
  until ende;
  freeres;
  releaseliste;
  if callall then begin
    Deletefile(ownpath+NetcallAlleFlag);
    Deletefile(ownpath+NetcallEndeFlag);
  end;
end;


{ nr:  1 = Bretter, 2 = User, 3 = Msgs, 4=Lister, 5=ArcViewer, 6=Editor,
       7 = Terminal }

procedure MakSelKeys(LSelf: TLister; var t:taste);
begin
  if t=keyf6 then t:=keyesc;
end;

procedure Makroliste(nr:byte);
var
  List: TLister;
  x,y  : Integer;
    brk  : boolean;
    anz  : integer;
    t    : text;
    s,s2 : string;
    ta   : tap;
begin
  if _filesize(keydeffile)>0 then
  begin
    List := TLister.CreateWithOptions(15,65,10,11,-1,'/NS/SB/NLR/DM/');   { Koordinaten beliebig }
    assign(t,KeydefFile);
    reset(t);
    s:=''; anz:=0;
    while not eof(t) do begin
      readln(t,s2);
      if (FirstChar(s2)='!') and (s<>'') then
        s:=forms(s,13)+mid(s2,2);
      if (s2<>'') and (FirstChar(s2)<>'!') then begin
        if s<>'' then begin
          List.AddLine(' '+s); s:=''; inc(anz);
          end;
        if s2[15+nr]='*' then
          case s2[1] of
            '^' : s:='<Ctrl '+trim(copy(s2,2,10))+'>';
            '_' : s:=trim(copy(s2,2,10));
            else  s:=trim(LeftStr(s2,13));
          end;
        end;
      end;
    if s<>'' then begin
      List.AddLine(' '+s); inc(anz);
    end;
    close(t);
    if anz=0 then
      hinweis(getres2(1017,1))   { 'keine Tastenmakros fÅr dieses Fenster definiert' }
    else begin
      selbox(41,min(anz+2,screenlines-6),getres2(1017,2),x,y,true);   { 'Makro wÑhlen ...' }
      List.SetSize(x+1,x+39,y+1,y+min(anz+2,screenlines-6)-2);
      List.OnKeypressed := MakSelKeys;
      listboxcol(list);
      pushhp(84);
      brk := List.Show;
      pophp;
      closebox;
      if not brk then begin
        settap(ta);
        keyboard(getmacro(trim(LeftStr(List.GetSelection,12)),ta));
        dispose(ta);
        end;
      end;
    List.Free;
  end
  else
    hinweis(getres2(1017,3));   { 'keine Tastenmakros definiert' }
  freeres;
end;
{
  $Log$
  Revision 1.21  2002/01/22 18:08:33  cl
  - the never-ending after 3.40 merge story

  Revision 1.20  2002/01/22 13:59:21  mk
  - after 3.40 merge fixes

  Revision 1.19  2002/01/13 15:07:25  mk
  - Big 3.40 Update Part I

  Revision 1.18  2001/09/26 23:34:19  mk
  - fixed FPC compile error with newest snapshot:
    Error: Self can only be an explicit parameter in message handlers or class methods

  Revision 1.17  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.16  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.15  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.14  2000/12/25 14:02:40  mk
  - converted Lister to class TLister

  Revision 1.13  2000/10/17 10:05:45  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.12  2000/08/15 17:22:40  mk
  - Bereitschaft funktionsfaehig, Kommentare hinzugefuegt

  Revision 1.11  2000/08/14 14:45:14  mk
  MO: Umfangreiche Aenderung fuer Null basierende Stringlisten

  Revision 1.10  2000/08/09 19:51:34  mk
  - verschiedene Fixes fuer Timeingliste
  - Netcall/Alle funktioniert jetzt

  Revision 1.9  2000/08/03 20:08:18  mk
  - e[] auf TStringlist umgestellt

  Revision 1.8  2000/07/13 17:15:15  mk
  - noch einige ^String-spezifische Probleme beseitigt

  Revision 1.7  2000/07/13 10:23:45  mk
  - Zeiger auf Strings entfernt

  Revision 1.6  2000/07/05 17:58:26  hd
  - Ansistring

  Revision 1.5  2000/07/04 12:04:19  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.4  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.3  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

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


procedure AutoTiming(tnr:integer; callall,crashall:boolean);
var brk     : boolean;
    tl      : array[1..maxentries] of TRP;
    anz,i   : integer;
    ldate   : datetimest;
    x,y,gl  : byte;
    anzeige : boolean;
    ende    : boolean;
    endtime : datetimest;

    startdat: string[DateLen];
    netcalls: boolean;
    lastbusy: array[1..MaxCom] of datetimest;
    _anz    : integer;
    lsec    : string[2];

  { testen, ob tr.action am tag dat zwischen von und bis }
  { ausgefÅhrt wurde.                                    }

  function intime(dat,von,bis:datetimest; var tr:TimeRec):boolean;
  var t   : text;
      s   : string;
      p   : byte;
      it  : boolean;
      buf : array[0..1023] of byte;
  begin
    dat:=left(dat,6)+right(dat,2);
    it:=false;
    assign(t,TimingDat);
    if existf(t) then begin
      settextbuf(t,buf);
      reset(t);
      while not eof(t) and not it do begin
        readln(t,s);
        UpString(s);
        p:=cpos('=',s);
        if (p>0) and (left(s,p-1)=ustr(tr.action)) and (copy(s,p+1,8)=dat) and
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
      s       : string[127];
      usebox  : string[BoxNameLen];
      lastbox : string[BoxNameLen];

    function tf(s:string):string;
    begin
      tf:=copy(s,4,2)+left(s,2);
    end;

  begin
    anz:=0;
    i:=1;
    dat:=tf(date); tim:=left(time,5);
    lastbox:='';
    while (i<=anzahl) and (anz<maxentries-1) do begin   { -1 wegen Splitting }
      Str2Time(e[i]^,tr);
      with tr do
        if active and (tf(vond)<=dat) and (tf(bisd)>=dat) and ((bis>=tim) or (von>bis))
           and wotag[dow(date)]
        then begin
          s:=trim(action);
          p:=cpos(' ',s);
          if p=0 then box:=''                  { Boxname isolieren }
          else box:=trim(mid(s,p));
          if p>0 then s:=trim(left(s,p));
          comm:=0;
          for j:=1 to comms do                   { Befehls-Nummer ermitteln }
            if ustr(s)=comstr[j] then comm:=j;
          if ((comm=5) or (comm=6)) and (box<>'') then begin
            if (ival(box)>=0) and (ival(box)<=255) then
              qerrlevel:=ival(box)
            else
              qerrlevel:=0;
            box:='';
            end;
          if (comm=0) and (ustr(s)='CRASH') then begin
            crash:=true; comm:=1;
            crashtime:=false;
            UseBox:=DefFidoBox;
            if cpos(' ',box)>0 then begin  { ZEIT-Option }
              box:=left(box,cpos(' ',box)-1);
              if (pos(' ZEIT',ustr(action))>0) or (pos(' TIME',ustr(action))>0)
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
            then begin
              inc(anz);
              new(tl[anz]);
              tl[anz]^:=tr2;
              end;
            if (bis<von) and (bis>=tim) then
              if not intime(date,'00:00',bis,tr) then begin
                inc(anz);
                new(tl[anz]);
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
  var d   : DB;
      fn  : pathstr;
      t   : text;
      ti  : string[5];
      all : string;
      x,y : byte;
      p   : byte;
      box : string[BoxNameLen];
  begin
    all:='';
    dbOpen(d,BoxenFile,1);
    while not dbEOF(d) do begin
      if dbReadInt(d,'script') and 2=0 then
        all:=all+' '+dbReadStr(d,'boxname');
      dbNext(d);
      end;
    all:=trim(all);
    if all='' then brk:=true
    else begin
      if auto then
        brk:=false
      else begin
        dialog(72,3,getres2(1016,1),x,y);    { 'Netcall bei:' }
        maddstring(3,2,'',all,66,255,'>');
        readmask(brk);
        enddialog;
        end;
      all:=ustr(trim(all))+' ';

      if not brk then begin
        fn:=TempS(1000+dbRecCount(d)*200);
        assign(t,fn);
        rewrite(t);
        ti:=left(time,5);
        p:=cpos(' ',all);
        while p>0 do begin
          box:=left(all,p-1);
          dbSeek(d,boiName,ustr(box));
          if dbFound then
            writeln(t,'+ '+ti+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ NETCALL ',box);
          delete(all,1,p);
          while left(all,1)=' ' do delete(all,1,1);
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
      ss  : string[36];
      sc  : string[60];
      adr : string[25];
      ni  : NodeInfo;
      c,f : boolean;
      crash: boolean;
      d   : DB;
  begin
    i:=1;
    while (i<=anzahl) do
      if (copy(e[i]^,37,6)='CRASHS') or (copy(e[i]^,37,8)='REQUESTS') then begin
        crash:=(copy(e[i]^,37,6)='CRASHS');
        ss:=left(e[i]^,36);
        freemem(e[i],filewidth+1);
        if i<anzahl then Move(e[i+1],e[i],(anzahl-i)*4);
        dec(anzahl);
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
            if ((not crash and f) or (crash and not f and c)) and
               ni.found and (anzahl<maxentries) then begin
              dbSeek(d,boiName,adr);
              if not dbFound then begin     { keine eingetragene Pollbox }
                inc(anzahl);
                if anzahl>i then Move(e[i],e[i+1],(anzahl-i)*4);
                sc:=ss+'CRASH '+adr;
                getmem(e[i],filewidth+1);
                e[i]^:=sc;
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
  var fn   : pathstr;
      t,t2 : text;
      s    : string[30];
  begin
    fn:=TempS(1000);
    assign(t,fn);
    rewrite(t);
    assign(t2,CrashTemp);
    if (tnr=0) or not existf(t2) then begin
      writeln(t,'+ '+left(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ CRASHS');
      writeln(t,'+ '+left(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ REQUESTS');
      end
    else begin
      reset(t2);
      while not eof(t2) do begin
        readln(t2,s);
        writeln(t,'+ '+left(time,5)+' 23:59 01.01. 31.12. ˛˛˛˛˛˛˛ CRASH '+s);
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
  const ltc : string[2] = '';
  var i      : integer;
      tc     : string[2];
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
      s : datetimest;
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
    l:=ival(left(t,2))*3600+ival(copy(t,4,2))*60+ival(right(t,2))+sec;
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
      nt     : datetimest;
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
    if ok or (tl[1]^.bis<left(time,5)) then begin
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
    h:=ival(left(endtime,2));
    m:=ival(copy(endtime,4,2));
    s:=ival(right(endtime,2));
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

begin
  filewidth:=timingwidth;
  if crashall then begin
    MakeCrashliste;
    _anz:=anzahl;
    end
  else if not callall then begin
    if tnr=0 then tnr:=ReadTimingNr(brk)
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
    _anz:=anzahl;
    end;
  if _anz=0 then begin
    trfehler(iif(callall,1005,1006),60);   { 'keine zutreffenden Boxen' / 'leere Timing-Liste' }
    exit;
    end;
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
    else endtime:=iifs(tl[1]^.von<=left(time,5),time,tl[1]^.von+':00');
    closebox;
    msgbox(60,gl+4,getres2(1016,3)+iifs(callall,'',' / #'+strs(tnr)),x,y);   { 'Netcall-Automatik' }
    mwrt(x+48,y,' '+left(ldate,6)+right(ldate,2)+' ');
    anzeige:=true;
    initscs;
    lsec:=right(time,2);

    repeat
      if right(time,2)<>lsec then begin
        lsec:=right(time,2);
        dec(scsavecnt);
        if scsavecnt=0 then begin
          if timediff(endtime,time)>10 then begin
            addendtime(-2); TimedScsaver(endtime); addendtime(2);
            end;
          initscs;
          end;
        end;
      set_active;
      if anzeige then display
      else show_active;
      multi2(curoff);
      if endtime>=time then disprest;
      if (anz>0) and (time>=endtime) then begin
        execute1;
        initscs;
        end;
      ende:=ende or (callall and (anz=0));
      while keypressed do
        case readkey of
          #27 : ende:=true;
          ' ' : endtime:=time;
          '+' : if endtime<'23:59:59' then addendtime(1);
          '-' : if endtime>time then addendtime(-1);
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
end;


{ nr:  1 = Bretter, 2 = User, 3 = Msgs, 4=Lister, 5=ArcViewer, 6=Editor,
       7 = Terminal }

procedure MakSelKeys(var t:taste);
begin
  if t=keyf6 then t:=keyesc;
end;

procedure Makroliste(nr:byte);
var x,y  : byte;
    brk  : boolean;
    anz  : integer;
    t    : text;
    s,s2 : string[50];
    ta   : tap;
begin
  if _filesize(keydeffile)>0 then begin
{$IFNDEF WIN32}
    openlist(15,65,10,11,-1,'/NS/SB/NLR/DM/');   { Koordinaten beliebig }
{$ENDIF}
    assign(t,KeydefFile);
    reset(t);
    s:=''; anz:=0;
    while not eof(t) do begin
      readln(t,s2);
      if (left(s2,1)='!') and (s<>'') then
        s:=forms(s,13)+mid(s2,2);
      if (s2<>'') and (left(s2,1)<>'!') then begin
        if s<>'' then begin
{$IFNDEF WIN32}
          app_l(' '+s); s:=''; inc(anz);
{$ENDIF}
          end;
        if s2[15+nr]='*' then
          case s2[1] of
            '^' : s:='<Ctrl '+trim(copy(s2,2,10))+'>';
            '_' : s:=trim(copy(s2,2,10));
            else  s:=trim(left(s2,13));
          end;
        end;
      end;
    if s<>'' then begin
{$IFNDEF WIN32}
      app_l(' '+s); inc(anz);
{$ENDIF}
 end;
    close(t);
    if anz=0 then
      hinweis(getres2(1017,1))   { 'keine Tastenmakros fÅr dieses Fenster definiert' }
    else begin
      selbox(41,min(anz+2,screenlines-6),getres2(1017,2),x,y,true);   { 'Makro wÑhlen ...' }
{$IFNDEF WIN32}
      SetListSize(x+1,x+39,y+1,y+min(anz+2,screenlines-6)-2);
      ListTp(MakSelKeys);
      listboxcol;
      pushhp(84);
      list(brk);
      pophp;
      closebox;
{$ENDIF}
      if not brk then begin
        settap(ta);
{$IFNDEF WIN32}
        keyboard(getmacro(trim(left(get_selection,12)),ta));
{$ENDIF}
        dispose(ta);
        end;
      end;
{$IFNDEF WIN32}
    closelist;
{$ENDIF}
    end
  else
    hinweis(getres2(1017,3));   { 'keine Tastenmakros definiert' }
  freeres;
end;


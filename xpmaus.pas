{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - MausNet }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpmaus;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
     sysutils,dos,typeform,fileio,keys,inout,maus2,datadef,database,stack,maske,
     xp0,xp1,xp1input,crc,xp_iti, xpglobal;


procedure MausLogFiles(art:byte; delfile:boolean; var box:string);
procedure MausInfoReorg;
function  MausBestPM:boolean;     { gelesene Maus-PM bestÑtigen }
procedure MausImportITG(box:string);
procedure MausEditInfos;
procedure MausPMs_bestaetigen(box:string);
procedure MausGetInfs(box, logfile:string);


implementation  { ---------------------------------------------------- }

uses xp1o,xp3,xp3o2,xpnt,xp6,xp6o,xp9, winxp;


{ art=0 : ZurÅckgelieferte Maus-MessageIDs aus Logfile in  }
{         Datenbank und Verkettungsdatei einlesen.         }
{ art=1 : ZurÅckgelieferte PM-Status in Datenbank einlesen }
{ art=2 : Fehlermeldungs-Nachricht in PM senden            }

procedure MausLogFiles(art:byte; delfile:boolean; var box:string);
var t,t2 : text;
{$ifdef hasHugeString}
    fn, tfn, s, anew, old, msgid, empf : string;
{$else}
    fn,
    tfn  : pathstr;
    s    : string[80];
    anew  : string[80];
    old  : string[4];
    msgid: string[20];
    empf : string[AdrLen];
{$endif}
    stop : boolean;
    l    : longint;
    hdp  : headerp;
    hds  : longint;
    f    : file;
    mi   : shortint;
    p,p2 : byte;
    x,y  : byte;
    n    : longint;
    fehlerflag : boolean;
    rec  : longint;
    mdm  : shortint;

  function mausname(s:string):string;
  var p : byte;
  begin
    p:=cpos('@',s);
    if (p=0) or ((hdp^.netztyp<>nt_Maus) and (hdp^.netztyp<>nt_Fido)) then
      mausname:=s
    else
      mausname:=trim(left(s,p-1))+' @ '+trim(mid(s,p+1));
  end;

begin
  case art of
    0 : assign(t,MausLogfile);
    1 : assign(t,MausStLog);
    2 : assign(t,MausLogfile);
  end;
  if not existf(t) then exit;
  case art of
    0 : message('Message-IDs einlesen...    ');
    1 : message('PM-Status einlesen...    ');
    2 : message('öberprÅfe Logfile auf Fehlermeldungen...   ');
  end;
  if art<>0 then begin
    tfn:=TempS(20000);
    assign(t2,tfn);
    rewrite(t2);
    writeln(t2);
    if art=1 then begin
      writeln(t2,'Datum      EmpfÑnger              Betreff                         Status');
      writeln(t2,dup(78,'-'));
      end
    else begin
      writeln(t2,'Die folgenden Nachrichten wurden wegen eines Fehlers von der');
      writeln(t2,'Maus abgewiesen:');
      writeln(t2,dup(60,'-'));
      writeln(t2);
      end;
    end
  else
    tfn:='';
  x:=wherex-3; y:=wherey;
  reset(t);
{$IFNDEF Ver32 }
  system.new(hdp);
{$ENDIF }
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiMsgID);

  fehlerflag:=false;
  n:=0;
  anew:='';
  while not eof(t) do begin
    if left(anew,1)='#' then begin
      s:=anew; anew:='';
      end
    else begin
      repeat
        readln(t,s);
        if fehlerflag and (left(s,1)='?') then
          writeln(t2,'Fehler   : ',mid(s,2));
      until eof(t) or (left(s,1)<>'?');
      if fehlerflag then begin
        writeln(t2);
        fehlerflag:=false;
        end;
      s:=trim(s);
      end;
    if (left(s,1)='#') and ((art=1) or (length(s)=11)) and not eof(t)
    then begin
      anew:='!';
      while not eof(t) and (left(anew,1)='!') do   { Kommentare Åberlesen }
        readln(t,anew);
      anew:=trim(anew);   { dies ist entweder eine ID oder ein Status }
      if ((art<>2) and (left(anew,1)='=')) or ((art=2) and (left(anew,1)='?'))
      then begin
        delete(s,1,1);
        delete(anew,1,1);
        if art<2 then begin
          p:=cpos('@',anew);
          if p>0 then begin
            p2:=pos('.',mid(anew,p));   { mit Domain? }
            if p2=0 then
              s:=s+mid(anew,p)
            else
              s:=s+UpperCase(copy(anew,p,p2-1));   { Domain abschneiden }
            end;
          end
        else
          s:=s+'@'+box;
        old:=dbLongStr(MsgidIndex(s));
        dbSeek(bezbase,beiMsgID,old);

        if dbFound then begin
          stop:=false;
          repeat          { zugehîrigen mbase-Datensatz ermitteln }
            dbReadN(bezbase,bezb_msgpos,l);
            if dbDeleted(mbase,l) then stop:=true
            else begin
              dbGo(mbase,l);
              ReadHeader(hdp^,hds,false);
              if hdp^.msgid<>s then begin
                dbNext(bezbase);
                stop:=dbEOF(bezbase) or
                      (dbReadInt(bezbase,'msgid')<>MsgidIndex(s));
                end;
              end;
          until stop or (hdp^.msgid=s);

          if not stop and ((art<>1) or ((hdp^.pm_bstat<>anew) and (anew[1]<>'N')))
          then begin
            inc(n);                         { gefunden / Status geÑndert }
            attrtxt(col.colmboxhigh);
            mwrt(x,y,strsn(n,3));
            if art<2 then begin
              if art=0 then begin
                hdp^.msgid:=anew;
                hdp^.pm_bstat:='';
                end
              else
                hdp^.pm_bstat:=anew;
              fn:=TempS(hdp^.groesse+hds+5000);
              assign(f,fn);
              rewrite(f,1);          { neue Nachricht erzeugen }
              WriteHeader(hdp^,f,nil);
              XreadF(hds,f);
              close(f);
              Xwrite(fn);            { .. und in die Ablage einlesen }
              erase(f);
              end;

            case art of
              0 : begin
                    msgid:=FormMsgid(hdp^.msgid);
                    dbWriteN(mbase,mb_msgid,msgid);   { neue MsgID in die Datenbank }
                    l:=MsgidIndex(anew);
                    dbWriteN(bezbase,bezb_msgid,l);   { neue MsgID in die BezBase }
                    repeat                            { BezBase-References updaten }
                      dbSeek(bezbase,beiRef,old);
                      if dbFound then
                        dbWriteN(bezbase,bezb_ref,l);
                    until not dbFound;
                  end;
              1 : with hdp^ do begin
                    if cpos('@',empfaenger)>0 then
                      empfaenger:=left(empfaenger,cpos('@',empfaenger)-1);
                    write(t2,copy(datum,5,2),'.',copy(datum,3,2),'.',left(datum,2),
                             '   ',forms(empfaenger,21),'  ',forms(betreff,30),'  ');
                    case pm_bstat[1] of
                      'Z' : writeln(t2,'zurÅckgest.');
                      'B' : writeln(t2,'beantwortet');
                      'G' : writeln(t2,'gelesen');
                      'W' : writeln(t2,'weitergel.');
                      'M' : writeln(t2,'im Netz');
                      'A' : writeln(t2,'angekommen');
                      'Y' : writeln(t2,'beim Gateway');
                      'T' : writeln(t2,'im Tausch');
                      'N' : writeln(t2,'nicht gel.');
                    else    writeln(t2,pm_bstat[1],' - ???');
                    end;
                  end;
              2 : with hdp^ do begin
                    writeln(t2,'EmpfÑnger: ',mausname(empfaenger));
                    writeln(t2,'Betreff  : ',betreff);
                    writeln(t2,'Datum    : ',fdat(datum),' ',ftime(datum));
                    writeln(t2,'MessageID: ',msgid);
                    writeln(t2,'Fehler   : ',anew);
                    fehlerflag:=true;
                    if (left(LowerCase(anew),10)='mitteilung') and
                       (pos('nicht gefunden',LowerCase(anew))>0) then begin
                      rec:=GetBezug(msgid);
                      if rec>0 then begin
                        dbGo(mbase,rec);
                        mdm:=aktdispmode; aktdispmode:=10;
                        Weiterleit(4,false);
                        aktdispmode:=mdm;
                        writeln(t2,'Hinweis  : Nachricht wurde automatisch neu verschickt');
                        end;
                      end;
                  end;
            end;
            end;
          end;  { BezBase.found }

        end;
      end;
    end;   { while not eof(t) }

  dbSetIndex(bezbase,mi);
  FlushClose;
  close(t);
  if delfile then erase(t);
  if n>0 then mdelay(500);
  closebox;
  if tfn<>'' then begin
    close(t2);
    s:='';
    if n>0 then
      if art=2 then begin
        if SendPMmessage('MausTausch-Fehlerbericht',tfn,box) then;
        end
      else begin
        InternBox:=box;
        empf:='$/ØMausstatus';
        if DoSend(false,tfn,empf,'PM-Status vom '+date,
                false,false,false,false,false,nil,s,s,sendIntern+sendShow) then
          SetUngelesen;
        end;
    _era(tfn);
    end;
  dispose(hdp);
end;


procedure MausInfoReorg;
var brett  : string[5];
    mi     : shortint;
    x,y    : byte;
    rec    : longint;
    maus   : string[BoxNameLen];
    betreff: string[BetreffLen];
    abs    : string[AdrLen];
    loesch : byte;
    n      : longint;
    info   : MausInfAP;
    infos  : integer;

  function NetInfoFile:boolean;
  var i : integer;
  begin
    if info=nil then
      NetInfofile:=false
    else begin
      i:=infos;
      while (i>0) and not stricmp(betreff,info^[i].text) do
        dec(i);
      NetInfoFile:=(i>0) and (info^[i].netflag='N');
      end;
  end;

begin
  info:=nil;
  dbSeek(bbase,biBrett,UpperCase(MausInfoBrett));
  if not dbFound then exit;
  brett:=mbrettd('$',bbase);
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  msgbox(52,6,'Maus-Infofiles reorganisieren',x,y);
  mwrt(x+3,y+2,'Infofile:');
  mwrt(x+3,y+3,'gelîscht:');
  n:=0;
  dbSeek(mbase,miBrett,brett+#255);
  loesch:=2;
  if not dbBOF(mbase) then
    dbSkip(mbase,-1);
  while not dbBOF(mbase) and (dbReadStr(mbase,'brett')=brett) do begin
    if dbReadInt(mbase,'halteflags')<>2 then begin
      rec:=dbRecno(mbase);
      dbReadN(mbase,mb_absender,abs);
      maus:=trim(mid(abs,cpos('@',abs)+1));
      if (info=nil) and exist(maus+'.iti') then begin
        new(info);
        MausReadITI(maus,info,infos);
        end;
      dbReadN(mbase,mb_betreff,betreff);
      attrtxt(col.colmboxhigh);
      moff;
      wrt(x+14,y+2,maus+': '+forms(betreff,30));
      mon;
      dbSkip(mbase,-1);
      while not dbBOF(mbase) and (dbReadStr(mbase,'brett')=brett) do begin
        if (dbReadInt(mbase,'halteflags')=0) and
           (dbReadStr(mbase,'betreff')=betreff) then begin
          dbReadN(mbase,mb_absender,abs);
          if (maus=trim(mid(abs,cpos('@',abs)+1))) or NetInfofile then begin
            inc(n);
            mwrt(x+14,y+3,strs(n));
            dbWriteN(mbase,mb_halteflags,loesch);
            end;
          end;
        dbSkip(mbase,-1);
        end;
      dbGo(mbase,rec);
      end;
    dbSkip(mbase,-1);
    end;
  if n>0 then mdelay(1000);
  closebox;
  dbSetIndex(mbase,mi);
  if info<>nil then dispose(info);
end;


{ true -> 'z'-Flag geÑndert }

function MausBestPM:boolean;     { gelesene Maus-PM bestÑtigen }
var t   : text;
{$ifdef hasHugeString}
    fn  : string;
    leer: string;
{$else}
    fn  : pathstr;
    leer: string[12];
{$endif}
    hdp : headerp;
    hds : longint;
    nr  : shortint;
    x,y : byte;
    gel : byte;
    ta  : taste;
    b   : byte;
    rec : longint;
begin
  if dbReadInt(mbase,'unversandt') and 64<>0 then begin
    MausBestPM:=true;       { interne Nachricht }
    exit;
    end;
  MausBestPM:=false;
  new(hdp);
  ReadHeader(hdp^,hds,false);
  if hdp^.pfad='' then           { Pfad='' -> eigene Nachricht }
    MausBestPM:=true
  else begin
    msgbox(54,7,'',x,y);
    wrt(x+3,y+2,'alter Nachrichtenstatus:  ');
    attrtxt(col.colmboxhigh);
    dbReadN(mbase,mb_gelesen,gel);
    if gel=0 then write('ungelesen')
    else write('zurÅckgestellt');
    attrtxt(col.colmbox);
    wrt(x+3,y+4,'neuer Status:');
    ta:='';
    spush(m2t,1);
    m2t:=false;
    pushhp(91);
    nr:=ReadButton(x+19,y+4,2,' ^gelesen , ^zurÅckgestellt ',1,true,ta);
    pophp;
    closebox;
    if (nr=0) or ((gel=1) and (nr=2)) then
      spop(m2t)
    else begin
      MausBestPM:=true;
      b:=dbReadInt(mbase,'unversandt') and (not 32);
      if nr=2 then inc(b,32);
      dbWriteN(mbase,mb_unversandt,b);
      if hds>1 then begin
        fn:=TempS(1024);
        assign(t,fn);
        rewrite(t);
        writeln(t,'#',hdp^.msgid);
        if nr=1 then writeln(t,'BG')
        else writeln(t,'BZ');
        close(t);
        leer:='';
        rec:=dbRecno(mbase);
        if DoSend(true,fn,'MAUS@'+hdp^.pfad,'<Maus-Direct-Command>',
                  false,false,false,false,false,nil,leer,leer,0) then;
        dbGo(mbase,rec);
        erase(t);
        end;
      spop(m2t);
      end;
    end;
  dispose(hdp);
end;


procedure MausImportITG(box:string);
const bufs = 2048;
var t,t2  : text;
    s     : string;
    sg    : string[70];
    sb    : string;
    sfl   : string[30];
    buf   : pointer;
    b,wf  : byte;
begin
  if exist(box+'.ITG') then begin
    message('Maus-Gruppenliste wird eingelesen ...');
    getmem(buf,bufs);
    assign(t,box+'.ITG');
    settextbuf(t,buf^,bufs);
    reset(t);
    assign(t2,box+'.BL');
    rewrite(t2);
    while not eof(t) do begin
      sg:=''; sb:=''; sfl:='';
      repeat
        readln(t,s);
        case s[1] of
          'G' : sg:=mid(s,2);
          'U' : sb:=mid(s,2);
          'F' : sfl:=mid(s,2);
        end;
      until eof(t) or ((sg<>'') and (sb<>'') and (sfl<>'') and
            (pos('V=',sfl)=0));
      if not eof(t) then begin
        if pos('L+',sfl)>0 then write(t2,'+ ')
        else write(t2,'  ');
        if length(sg)<=30 then write(t2,forms(sg,32))
        else write(t2,sg,'  ');
        writeln(t2,sb);
        if pos('L+',sfl)>0 then begin
          wf:=iif(pos('S-',sfl)>0,8,0);     { Schreibzugriff gesperrt? }
          if wf=8 then begin   { automatisches Lîschen des Schreibschutzes }
                               { ist problematisch ...                     }
            dbSeek(bbase,biBrett,'A'+UpperCase(BoxPar^.MagicBrett+sg));
            if dbFound and (dbReadInt(bbase,'flags')and 8<>wf) then begin
              dbReadN(bbase,bb_flags,b);
              b:=(b and not 8) + wf;
              dbWriteN(bbase,bb_flags,b);
              end;
            end;
          end;
        end;
      end;
    close(t);
    close(t2);
    freemem(buf,bufs);
    closebox;
    end;
end;


procedure MausEditInfos;
var  box    : string[BoxNameLen];
     info   : MausInfAP;
     infos  : integer;
     brk    : boolean;

  procedure TruncText;
  var i : integer;
      p : byte;
  begin
    for i:=1 to infos do begin
      p:=pos('Auslastungsstatistik',info^[i].text);
      if p>0 then begin
        delete(info^[i].text,p+15,5);
        insert('.',info^[i].text,p+15);
        end;
      end;
  end;

  procedure SetDefaultTimes;
    procedure _set(ID:string; intervall:shortint);
    var i : integer;
    begin
      i:=infos;
      while (i>0) and (info^[i].id<>id) do
        dec(i);
      if i>0 then info^[i].intervall:=intervall;
    end;
  begin
    _set('IIE',30);    { EinfÅhrung in die Maus }
    _set('IIB',30);    { Bedienungsanleitung    }
    _set('IIA',30);    { MenÅ-Kurzanleitung     }
    _set('IIG',30);    { Gruppen-Anleitung      }
    _set('IIH',30);    { Hardware der MAUS      }
    _set('III',30);    { Technische Informationen }
    _set('IIM',7);     { MAUS-Beitrag           }
    _set('IIL',14);    { Login-Zeiten           }
    _set('IIT',30);    { MausTausch-Anleitung   }
    _set('IIK',0);     { Kommerzielles          }
    _set('IIP',30);    { PM-Manifest            }
    _set('IIN',14);    { Nutzungsbedingungen    }
    _set('INA',30);    { MausNet-Anleitung      }
    _set('INK',14);    { Kurze Boxen-Liste      }
    _set('INL',30);    { Lange Boxen-Liste      }
    _set('ING',0);     { Netzgruppen-Liste      }
    _set('INP',0);     { Netzplan               }
    _set('IGT',30);    { Box-Vorspann           }
    _set('IGE',0);     { Box-Abspann            }
    _set('IGS',0);     { Spruch des Tages       }
    _set('IGK',0);     { Kurze Gruppenliste     }
    _set('IGL',30);    { Lange Gruppenliste     }
    _set('ITB',0);     { Maschinenlesbare Boxenliste }
    _set('ITG',1);     { Maschinenlesbare Gruppenliste }
    _set('ITI',1);     { Maschinenlesbare Infoliste }
  end;

  procedure ReadINF;
  var t   : text;
{$ifdef hasHugeString}
      s   : string;
{$else}
      s   : string[80];
{$endif}
      p,i : integer;
  begin
    assign(t,box+'.inf');
    if existf(t) then begin            { .INF-File einlesen }
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        s:=trim(s);
        p:=blankpos(s);
        if p>0 then begin
          i:=1;
          while (i<=infos) and (UpperCase(left(s,p-1))<>info^[i].ID) do
            inc(i);
          if i<=infos then begin
            s:=trim(mid(s,p));
            p:=blankpos(s);
            if p>0 then begin
              info^[i].intervall:=ival(left(s,p));
              s:=trim(mid(s,p));
              p:=blankpos(s);
              info^[i].lastdate:=left(s,10);
              if p>0 then
                info^[i].crc:=ival(mid(s,p));
              end;
            end;
          end;
        end;
      close(t);
      end;
  end;

  procedure WriteINF;
  var i : integer;
      t : text;
  begin
    assign(t,box+'.inf');
    rewrite(t);
    for i:=1 to infos do with info^[i] do
      writeln(t,id,' ',intervall,' ',lastdate,' ',crc);
    close(t);
  end;

  procedure edit(var brk:boolean);
  var i,ml  : integer;
      h,x,y : byte;
      spflag: boolean;
      ipos  : byte;

    procedure addnr(nr:byte);
    begin
      if not info^[nr].edflag then begin
        maddint(3,ipos,forms(info^[nr].text,ml+1),info^[nr].intervall,2,2,0,99);
        info^[nr].edflag:=true;
        inc(ipos);
        spflag:=false;
        end;
    end;

    procedure add(id:string);
    var i : integer;
    begin
      i:=infos;
      while (i>0) and (info^[i].id<>id) do dec(i);
      if i>0 then addnr(i);
    end;

    procedure space;
    begin
      if not spflag then begin
        inc(ipos);
        spflag:=true;
        end;
    end;

  begin      { edit }
    ml:=29;
    for i:=1 to infos do begin
      ml:=max(ml,length(info^[i].text));
      info^[i].edflag:=false;
      end;
    h:=minmax(infos+2,6,screenlines-11);
    diabox(ml+12,h+4,'Infofile-Bestellintervalle in Tagen',x,y);
    inc(x); inc(y);
    openmask(x,x+ml+8,y+1,y+h,false);
    maskupdownarrows(x+ml+10,y+1,x+ml+10,y+h,'≥',col.coldiarahmen);
    ipos:=1;
    add('IGK'); add('IGL'); add('ISG'); add('ING'); add('ITG'); space;
    add('INK'); add('INL'); add('ISB'); add('INP'); add('ITB'); space;
    add('IIE'); add('IIA'); add('IIB'); add('IIT'); add('INA');
      add('IIG'); add('III'); space;
    add('IIL'); add('IIM'); add('IIH'); add('IIN'); add('IIP'); space;
    add('IGT'); add('IGS'); add('IGE'); add('IIK'); space;
    for i:=1 to infos do
      if (info^[i].id<>'ITI') and (firstchar(info^[i].id)<>'J') then
        addnr(i);
    space;
    for i:=1 to infos do
      if firstchar(info^[i].id)='J' then
        addnr(i);
    readmask(brk);
    enddialog;
  end;

begin     { MausEditInfos }
  box:=UniSel(1,false,DefaultBox);
  if box='' then exit;
  if ntBoxNetztyp(box)<>nt_Maus then begin
    fehler(box+' ist keine MausNet-Box.');
    exit;
    end;
  new(info);
  MausReadITI(box,info,infos);       { box.ITI einlesen oder Defaults setzen }
  TruncText;
  SetDefaultTimes;                   { Default-Bestellintervalle setzen }
  ReadINF;                           { Daten aus box.INF einlesen       }
  Edit(brk);                         { Daten editieren                  }
  if not brk then WriteINF;          { box.INF schreiben                }
  dispose(info);
end;


procedure MausPMs_bestaetigen(box:string);
var t1,t2 : text;
{$ifdef hasHugeString}
    fn    : string;
    leer  : string;
{$else}
    fn    : pathstr;
    leer  : string[12];
{$endif}
    s     : string;
begin
  if exist(mauspmlog) then begin
    fn:=TempS(_filesize(mauspmlog)*3);
    assign(t1,mauspmlog); reset(t1);
    assign(t2,fn); rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s);
      if trim(s)<>'' then begin
        writeln(t2,'#',s);
        writeln(t2,'BG');     { Bearbeitungsstatus: gelesen }
        end;
      end;
    close(t1); close(t2);
    if _filesize(fn)>0 then begin
      leer:='';
      forcebox:=box;
      if DoSend(true,fn,'MAUS@'+box,'<Maus-Direct-Command>',
                false,false,false,false,false,nil,leer,leer,0) then;
      end;
    _era(fn);
    end;
end;


procedure MausGetInfs(box, logfile: string);
type  infrec = record
                 inf       : string[5];
                 intervall : shortint;
                 crc       : longint;
                 lastdate  : datetimest;
               end;
      ia     = array[1..MaxMausInfos] of infrec;
var   info   : ^ia;
      infos  : integer;
      _info  : MausInfAP;
      _infos : integer;

  procedure getinfofiles;
  var t1   : text;
      s    : string[60];

    function gets:string;
    var p : byte;
    begin
      p:=cpos(' ',s);
      if p=0 then p:=length(s)+1;
      gets:=trim(left(s,p));
      s:=trim(mid(s,p));
    end;

  begin
    infos:=0;
    assign(t1,box+'.inf');
    reset(t1);
    if ioresult=0 then begin
      while not eof(t1) and (infos<MaxMausInfos) do begin
        inc(infos);
        with info^[infos] do begin
          readln(t1,s);
          inf:=UpperCase(gets);
          intervall:=minmax(ival(gets),0,99);
          lastdate:=gets;
          if lastdate='' then lastdate:=date;
          crc:=ival(gets);
          end;
        end;
      close(t1);
      end;
  end;

  procedure readlogfile;
  var t     : text;
      s     : string[20];
      p1,p2 : byte;
      i     : integer;
  begin
    assign(t,logfile);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if left(s,1)='$' then begin
        p1:=cpos('=',s);
        p2:=cpos(' ',s);
        if (p1>4) and (p2>p1+1) then begin   { CRC vorhanden }
          i:=1;
          while (i<=infos) and (info^[i].inf<>UpperCase(copy(s,2,p1-2))) do inc(i);
          if (i>infos) and (infos<MaxMausInfos) then begin
            inc(infos);
            info^[infos].inf:=UpperCase(copy(s,2,p1-2));
            info^[infos].intervall:=30;
            end;
          if i<=infos then begin
            info^[i].crc:=ival(copy(s,p1+1,p2-p1-1));
            info^[i].lastdate:=date;
            end;
          end
        else if pos('(generiert)',s)>0 then begin   { keine CRC vorhanden }
          i:=1;
          while (i<=infos) and (info^[i].inf<>UpperCase(copy(s,2,p2-2))) do inc(i);
          if i<=infos then info^[i].lastdate:=date;
          end;
        end;
      end;
    close(t);
  end;

{  procedure InfosOhneCRC;
  var i,j : integer;
  begin
    for i:=1 to _infos do
      if not _info^[i].crcflag then begin
        j:=infos;
        while (j>0) and (info^[j].inf<>_info^[i].ID) do
          dec(j);
        if j>0 then
          info^[j].lastdate:=date;
        end;
  end;       }

  procedure writeinfofiles;
  var t : text;
      i : integer;
  begin
    assign(t,box+'.inf');
    rewrite(t);
    for i:=1 to infos do
      with info^[i] do
        writeln(t,inf,' ',intervall,' ',lastdate,' ',crc);
    close(t);
  end;

begin
  if exist(logfile) then begin
    new(info);
    new(_info);
    MausReadITI(box,_info,_infos);   { Maus.ITI -> _info^ }
    getinfofiles;                    { Maus.INF -> info^  }
    readlogfile;                     { Maus.LOG -> info^  }
  { InfosOhneCRC;                    { info^ <- Datum von J## setzen }
    writeinfofiles;                  { info^ -> Maus.INF  }
    dispose(_info);
    dispose(info);
    end;
end;


end.
{
  $Log$
  Revision 1.11  2000/07/05 12:47:28  hd
  - AnsiString

  Revision 1.10  2000/07/04 12:04:30  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.9  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.8  2000/06/19 20:22:32  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.7  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

}
{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC }

{ Yuppie Mailbase -> Fido-PKT }

uses xpglobal, dos, typeform, fileio, dbase;


const NetmailDB = 'NET-MAIL';
      AreaDB    = 'AREABASE';

type  pheader =  record                       { Fido - Packet-header }
                   OrgNode    : word;
                   DestNode   : word;
                   Year       : word;         { Datum der Packet-Erzeugung }
                   Month      : word;         { 0..11 }
                   Day        : word;         { 1..31 }
                   Hour       : word;
                   Min        : word;
                   Sec        : word;
                   Baud       : word;         { = 0 }
                   PktVer     : word;         { = 2 }
                   OrgNet     : word;
                   DestNet    : word;
                   PrdCodL    : byte;         { Lo(ProductCode) }
                   HiVersion  : byte;         { Haupt-Versionsnummer }
                   Password   : array[0..7] of char;   { -> = 0 }
                   QOrgZone   : word;         { fÅr einige Fido-Mailer.. }
                   QDestZone  : word;
                   fill       : word;         { = 0 }
                   CapValid   : word;         { = $100 }
                   PrdCodH    : byte;         { Hi(ProductCode) }
                   LoVersion  : byte;         { Unter-Versionsnummer (.1=10) }
                   CapWord    : word;         { = 1 }
                   OrgZone    : word;
                   DestZone   : word;
                   OrgPoint   : word;
                   DestPoint  : word;
                   fill2      : longint;      { -> = 0 }
                 end;

      mheader  = record                       { Fido - Nachrichtenheader }
                   mPktVer    : word;
                   origNode   : word;
                   destNode   : word;
                   origNet    : word;
                   destNet    : word;
                   mAttrib    : word;
                   cost       : word;
                   datetime   : array[0..19] of char;
                 end;

      mhd2     = record                       { Header incl. flexiblem Teil }
                   mhd1       : mheader;
                   datum      : string[20];
                   from,_to   : string[36];
                   betreff    : string[72];
                   area       : string[32];
                   origZone   : word;
                   destZone   : word;
                   origPoint  : word;
                   destPoint  : word;
                 end;

       FidoAdr = record
                   username   : string[36];
                   zone,net   : word;
                   node,point : word;
                   ispoint    : boolean;
                 end;


var   YupDir,Outfile : pathstr;
      pkt            : file;
      Address        : FidoAdr;
      msgs           : longint;


procedure logo;
begin
  writeln;
  writeln('Yuppie-Mailbase -> PKT - Konvertierer, Peter Mandrella, 12/92');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;

procedure helppage;
begin
  writeln('YUP2PKT <Yuppie-Verzeichnis> <Ausgabedatei> <Zone:Net/Node>');
  halt(1);
end;

procedure error(txt:string);
begin
  writeln('Fehler: ',txt);
  halt(1);
end;

procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);
var p1,p2,p3 : byte;
    res      : integer;
    l        : longint;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos('@',adr);
    if p1>0 then begin
      username:=trim(left(adr,p1-1));
      delete(adr,1,p1);
      end;
    adr:=trim(adr);
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if p3=0 then p3:=cpos(',',adr);
    if p1+p2=0 then begin
      zone:=DefaultZone;
      net:=243;
      if p3>0 then begin
        if p3>1 then
          node:=ival(left(adr,p3-1))
        else
          node:=0;
        point:=minmax(ival(mid(adr,p3+1)),0,65535);
        ispoint:=(point>0);
        end
      else
        node:=minmax(ival(adr),0,65535);
      end
    else
      if (p2<>0) and (p1<p2) and ((p3=0) or (p3>p2)) then begin
        if p1=0 then
          zone:=DefaultZone
        else
          zone:=minmax(ival(left(adr,p1-1)),0,65535);
        net:=minmax(ival(copy(adr,p1+1,p2-p1-1)),0,65535);
        ispoint:=(p3>0);
        if ispoint then begin
          point:=minmax(ival(mid(adr,p3+1)),0,65535);
          if point=0 then ispoint:=false;
          end
        else
          p3:=length(adr)+1;
        node:=minmax(ival(copy(adr,p2+1,p3-p2-1)),0,65535);
        end;
    end;
end;

procedure getpar;
begin
  if paramcount<>3 then helppage;
  Yupdir:=ustr(paramstr(1));
  if right(YupDir,1)<>'\' then YupDir:=YupDir+'\';
  if not ispath(YupDir) then
    error('ungÅltiges Verzeichnis: '+yupdir);
  if exist(YupDir+'MAILBASE\'+NetmailDB+'.DBF') then
    YupDir:=YupDir+'MAILBASE\';
  if not exist(YupDir+NetmailDB+'.DBF') then
    error('Keine Yuppie-Mailbase im angegebenen Verzeichnis gefunden.');
  if not exist(YupDir+AreaDB+'.DBF') then
    error('Keine AREABASE vorhanden.');
  outfile:=ustr(paramstr(2));
  if not validfilename(outfile) then
    error('ungÅltige Ausgabedatei: '+outfile);
  splitfido(paramstr(3),address,2);
end;


{ -------------------------------------------------------------------- }

procedure RewritePKT;                { PKT-Header schreiben }
var phd       : pheader;
    y,m,d,dow : word;
    h,s,s100  : word;
begin
  assign(pkt,outfile);
  rewrite(pkt,1);
  fillchar(phd,sizeof(phd),0);
  with phd do begin
    OrgNode:=address.node; DestNode:=address.node;
    OrgNet:=address.net; Destnet:=address.net;
    getdate(y,m,d,dow);
    year:=y; month:=m; day:=d;
    gettime(h,m,s,s100);
    hour:=h; min:=m; sec:=s;
    PktVer:=2;
    PrdCodL:=$1a;   { d'Bridge }
    HiVersion:=2;
    QOrgZone:=address.zone; QDestZone:=address.zone;
    CapWord:=1; CapValid:=$100;
    OrgZone:=address.zone; DestZone:=address.zone;
    end;
  blockwrite(pkt,phd,sizeof(phd));
end;

procedure wrs0(s:string);            { nullterminierten String schreiben }
begin
  s:=s+#0;
  blockwrite(pkt,s[1],length(s));
end;

procedure wrs(s:string);             { CR/LF-terminierten String schreiben }
begin
  s:=s+#13#10;
  blockwrite(pkt,s[1],length(s));
end;


procedure WriteMheader(var mhd:mhd2);      { Nachrichtenheader schreiben }
begin
  with mhd do begin
    mhd1.mPktVer:=2;
    FastMove(datum[1],mhd1.datetime,20);
    blockwrite(pkt,mhd1,sizeof(mheader));
    wrs0(_to);
    wrs0(from);
    wrs0(betreff);
    end;
end;

procedure WriteMkludges(var mhd:mhd2);
begin
  with mhd,mhd.mhd1 do begin
    if area<>'' then wrs('AREA:'+area);
    if origPoint<>0 then wrs(^A'FMPT '+strs(origPoint));
    if destPoint<>0 then wrs(^A'TOPT '+strs(destPoint));
    if origZone<>destZone then
      wrs(^A'INTL '+strs(destZone)+':'+strs(destNet)+'/'+strs(destNode)+
                ' '+strs(origZone)+':'+strs(origNet)+'/'+strs(origNode));
    end;
end;

procedure CopyMsgBody(db:dbPointer);
var b : byte;
begin
  dbReadMemo(db,pkt,'MSGBODY');
  b:=0;
  blockwrite(pkt,b,1);
end;


procedure WrArea(txt:string);
begin
  write(forms(txt,26));
end;

procedure GetHd1(db:dbPointer; var mhd:mhd2);
var s : string[40];
  function monat(m:byte):string;
  begin
    monat:=copy('JanFebMarAprMayJunJulAugSepOctNovDec',(m-1)*3+1,3);
  end;
begin
  with mhd,mhd.mhd1 do begin
    from      := trim(dbRead(db,'FROM'));
    _to       := trim(dbRead(db,'TO'));
    betreff   := trim(dbRead(db,'SUBJECT'));
    s         := dbRead(db,'WRITEDATE')+dbRead(db,'WRITETIME');
    datum:=copy(s,7,2)+' '+monat(ival(copy(s,5,2)))+' '+copy(s,3,2)+'  '+
           copy(s,9,8);
    mattrib:=1;
    end;
end;

procedure ConvertArea(nr:word; fn,area:string);
var db    : DBpointer;
    mhd   : mhd2;
    n     : longint;
begin
  if nr=0 then
    wrarea(NetmailDB);
  n:=0;
  new(db);
  dbUse(db,YupDir+fn);
  while not dbEOF do begin
    if not dbDeleted(db) then begin
      inc(n); inc(msgs);
      write(#8#8#8#8#8#8,n:6);
      fillchar(mhd,sizeof(mhd),0);
      GetHD1(db,mhd);
      if nr=0 then
        with mhd,mhd.mhd1 do begin
          destZone  := ival(dbRead(db,'DESTZONE'));
          destNet   := ival(dbRead(db,'DESTNET'));
          destNode  := ival(dbRead(db,'DESTNODE'));
          destPoint := ival(dbRead(db,'DESTPOINT'));
          origZone  := ival(dbRead(db,'FROMZONE'));
          origNet   := ival(dbRead(db,'FROMNET'));
          origNode  := ival(dbRead(db,'FROMNODE'));
          origPoint := ival(dbRead(db,'FROMPOINT'));
          end;
      mhd.area:=area;
      WriteMheader(mhd);
      WriteMkludges(mhd);
      CopyMsgBody(db);
      end;
    dbSkip(db);
    end;
  dbClose(db);
  dispose(db);
  writeln;
end;


procedure ConvertAreas;
var db   : dbPointer;
    n    : longint;
    fn   : string[8];
    area : string[30];
begin
  new(db);
  dbUse(db,YupDir+AreaDB);
  n:=0;
  while not dbEOF do begin
    inc(n);
    area:=trim(dbRead(db,'AREANAME'));
    if not dbDeleted(db) and (area<>'NETMAIL') and (area<>'CLIPBOARD') then
    begin
      WrArea(area);
      fn:='AREA'+formi(n,4);
      if not exist(YupDir+fn+'.DBF') then
        writeln(#8#8#8#8#8,'fehlt!')
      else
        ConvertArea(n,fn,area);
      end;
    dbSkip(db);
    end;
  dbClose(db);
  dispose(db);
end;


procedure ClosePKT;
var w : word;
begin
  w:=0;
  blockwrite(pkt,w,2);
  close(pkt);
end;


begin
  logo;
  getpar;
  RewritePKT;
  msgs:=0;
  ConvertArea(0,NetmailDB,'');
  ConvertAreas;
  ClosePKT;
  writeln(dup(26,'-'));
  writeln('Nachrichten gesamt',msgs:8);
end.
{
  $Log$
  Revision 1.4  2000/06/22 19:53:33  mk
  - 16 Bit Teile ausgebaut

  Revision 1.3  2000/03/03 13:24:45  mk
  YUP2PKT compilierbar gemacht und in Distribution aufgenommen

}

{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ ZConnect <-> Magic/Quick - Konvertierer }
{ PM 04/92                                }

{ ERRORLEVEL:  0 = ok                  }
{              1 = keine Konvertierung }
{              2 = teilweise Konvert.  }

{ Namenpoints: ABS:      USER@POINT     Alias-Points: ABS:      USER@BOX  }
{              X-XP-BOX: BOX            bzw. Nodes:   X-XP-PNT: POINT     }
{              X-XP-PNT: POINT                                            }

{ Direction:   1=Magic->Z, 2=Z->Magic, 3=Magic->Netcall }
{              4=Quick->Z, 5=Z->Quick                   }
{              6=Maus->Z, 7=Z->Maus                     }
{              8=ProNet-Z, 9=Z->ProNet                  }

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

{$IFDEF BP }
  {$M 20000,50000,655360}
{$ENDIF }

uses  dos,
{$IFDEF BP }
  xms,
{$ENDIF }
typeform,fileio,montage,xpdatum,xp_iti, xpglobal;

const       nt_ZConnect=2;
      OrgLen    = 80;
      hderrlen  = 40;
      BetreffLen= 70;
      readempflist = false;
      readkoplist  = false;
      postadrlen= 80;
      telelen   = 60;
      adrlen    = 80;
      homepagelen = 90;
      { 01/2000 oh }
      custheadlen = 60;
      { /oh }
      realnlen= 40;

      direction : byte = 0;
      infile    : pathstr = '';
      outfile   : pathstr = '';
      brettfile : pathstr = '';
      netzname  : string[8] = '';
      maxbrett  = 2000;
      halferror : boolean = false;   { Teilkonvertierung }
      bretth    : string[25] = '';
      boxname   : string[20] = '';
      msgids    : boolean = false;   { MagicNET - MsgID's }
      mausinfos : boolean = false;   { Infofiles bestellen / BOX.INF }
      g_und_s   : boolean = false;   { keine Brettslashs Ñndern }
      maxmaus   : longint = 0;       { vorgegebene Maximalgrî·e }
      mkoutfile : boolean = false;   { OUTFILE statt INFILE erzeugen }
      mausOE    : boolean = true;
      mausPSA   : boolean = true;
      mausON    : boolean = true;    { îffentl. Nachrichten anfordern }
      mausKF    : boolean = true;    { fb: Filter f?r Kommentare }
      MausIT_files : boolean = false;   { ITI/ITG -> Box.IT* }

      { attrCrash   = $0002; }        { header.attrib: Crashmail   }
      { attrFile    = $0010; }        { File attached              }
      attrMPbin   = $0040;            { Multipart-Binary           }
      attrReqEB   = $1000;            { EB anfordern               }
      attrIsEB    = $2000;            { EB                         }
      AttrPmReply = $0100;            { PM-Reply auf AM (Maus)     }
      AttrQuoteTo = $0400;            { QuoteTo (Maus)             }
      AttrQPC     = $0001;            { QPC-codiert                }

type  charr       = array[0..65530] of char;
      charrp      = ^charr;

      empfnodep=^empfnode;
      empfnode= record
                  next   : empfnodep;
                  empf   : string[AdrLen];
                end;

      header = record
                 netztyp    : byte;
                 archive    : boolean;       { archivierte PM }
                 empfaenger : string[90];    { Brett / User / TO:User }
                 empfanz    : integer;
                 kopien     : empfnodep;
                 betreff    : string[BetreffLen];
                 absender   : string[80];
                 datum      : string[11];    { Netcall-Format }
                 ddatum     : string[14];
                 zdatum     : string[22];    { ZConnect-Format; nur auslesen }
                 pfad       : string;        { Netcall-Format }
                 msgid,ref  : string[120];   { ohne <> }
                 typ        : string[1];     { T / B }
                 crypttyp   : string[1];
                 groesse    : longint;
                 komlen     : longint;       { Kommentar-LÑnge }
                 ckomlen    : longint;
                 realname   : string[realnlen];
                 programm   : string[60];    { Mailer-Name }
                 datei      : string[40];    { Dateiname }
                 prio       : byte;          { 10=direkt, 20=Eilmail }
                 oem,oab,wab: string[90];
                 oar,war    : string[40];
                 real_box   : string[20];    { X-XP-BOX: Absendebox }
                 hd_point   : string[25];    { X-XP-PNT: Pointname  }
                 pm_bstat   : string[20];    { X-XP-BST: Bearb.-Status }
                 org_msgid  : string[120];   { X-XP-ORGMID }
                 org_xref   : string[120];   { X-XP-ORGREF }
                 attrib     : word;          { X-XP-ATT    }
                 filterattr : word;          { X-XP-F      }
                 fido_to    : string[36];    { X-XP-FTO    }
                 organisation  : string[OrgLen];
                 postanschrift : string[PostAdrlen];
                 telefon    : string[telelen];
                 homepage   : string[homepagelen];
                 PmReplyTo  : string[80];      { Reply-To }
                 AmReplyTo  : string[80];
                 amrepanz   : integer;
                 error      : string[hderrlen];
                 ReplyPath  : string[8];
                 ReplyGroup : string[40];
                 x_charset  : string[25];
                 keywords   : string[60];
                 summary    : string[200];
                 priority   : byte;           { Priority by MH }
                 distribution:string[40];
                 pm_reply   : boolean;
                 QuoteString: string[20];
                 empfbestto : string[adrlen];
                 charset    : string[7];
                 ccharset   : string[7];
                 vertreter  : string[80];
                 XPointCtl  : longint;
                 nokop      : boolean;
                 mimever    : string[20];    { MIME }
                 mimect     : string;
                 boundary   : string[70];
                 gate       : string[80];
                 mimetyp    : string[30];
                 xnoarchive: boolean; { MK 01/00 fÅr UUZ Fix von Robo }
                 { 01/2000 oh }
                 Cust1,Cust2: string[custheadlen];
                 { /oh }
               end;

      brett  = record
                 code  : string[4];
                 name  : string[40];
               end;

var   f1,f2     : file;
      bretter   : integer;
      brettp    : array[1..maxbrett] of ^brett;


{ 01/2000 oh : wird zum Einlesen der Customizable Headerlines benîtigt }

const
      mheadercustom : array[1..2] of string[custheadlen] = ('','');

procedure logo;
begin
  writeln;
  writeln('MAGGI - Magic/Quick/Maus/ZConnect - Konvertierer (c) P.Mandrella');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;

procedure helppage;
begin
  writeln('MagicNET:   MAGGI [-mz|zm|mn] -nNetz [-m] <Eingabe> <Ausgabe> <Bretter>');
  writeln('ProNET:     MAGGI [-pz|zp] -nNetz <Eingabe> <Ausgabe> <Bretter>');
  writeln('QuickMail:  MAGGI [-qz|zq] [-g] <Eingabe> <Ausgabe>');
  writeln('MausTausch: MAGGI [-sz|zs] -bBox -hBretthierarchie [-i] <Eingabe> <Ausgabe>');
  writeln;
  writeln('    -mz  =  MagicNET -> ZConnect       -sz  =  MausTausch -> ZConnect');
  writeln('    -zm  =  ZConnect -> MagicNET       -zs  =  ZConnect -> MausTausch');
  writeln('    -mn  =  MagicNET -> Netcall        -i   =  Infofiles bestellen');
  writeln('    -m   =  Message-IDs erzeugen       -o   =  Outfile erzeugen');
  writeln;
  writeln('    -qz  =  QuickNET -> ZConnect       -pz  =  ProNET -> ZConnect');
  writeln('    -zq  =  ZConnect -> QuickNet       -zp  =  ZConnect -> ProNet');
  writeln('    -g   =  GS-Mailbox');
  halt(1);
end;

procedure parerr;
begin
  writeln('UngÅltige(r) Parameter');
  writeln;
  helppage;
end;

procedure error(txt:atext);
var ticker : longint {$IFNDEF Ver32 } absolute $40:$6c {$ENDIF } ;
    t      : longint;
    i      : integer;
begin
  writeln('Fehler: ',txt);
  for i:=1 to 18 do begin
    t:=ticker;
    repeat until ticker<>t;
    end;
  halt(1);
end;


procedure getpar;
var i : integer;
    s : string[127];

  function _is(t:string):boolean;
  begin
    _is:=(s='-'+t) or (s='/'+t);
  end;

  function isl(t:string):boolean;
  begin
    isl:=(left(s,length(t)+1)='-'+t) or (left(s,length(t)+1)='/'+t);
  end;

begin
  for i:=1 to paramcount do begin
    s:=lstr(paramstr(i));
    if _is('mz') then direction:=1
    else if _is('zm') then direction:=2
    else if _is('mn') then direction:=3
    else if _is('qz') then direction:=4
    else if _is('zq') then direction:=5
    else if _is('sz') then direction:=6
    else if _is('zs') then direction:=7
    else if _is('pz') then direction:=8
    else if _is('zp') then direction:=9
    else if _is('h') or _is('?') then helppage
    else if isl('n') then netzname:=forms(mid(paramstr(i),3),8)
    else if isl('h') then bretth:=ustr(copy(s,3,25))
    else if isl('b') then boxname:=ustr(copy(s,3,8))
    else if _is('m')  then msgids:=true
    else if _is('i')  then mausinfos:=true
    else if _is('o')  then mkoutfile:=true
    else if _is('g')  then g_und_s:=true
    else if _is('mm') then maxmaus:=diskfree(0) div 3
    else if _is('oe') then mausOE:=false
    else if _is('psa')then mausPSA:=false
    else if _is('on') then MausON:=false
    else if _is('kf') then mausKF:=false
    else if _is('it') then MausIT_Files:=true
    else if (left(s,1)='/') or (left(s,1)='-') then parerr
    else if infile='' then infile:=s
    else if outfile='' then outfile:=s
    else if brettfile='' then brettfile:=s
    else parerr;
    end;
  if ((direction in [1,2,3,8,9]) and ({(brettfile='') or} (netzname=''))) or
     ((direction in [6,7]) and ((bretth='') or (boxname=''))) or
     (direction=0) or (outfile='') then helppage;
  writeln(ustr(infile),'  ØØ  ',ustr(outfile));
  writeln;
end;


{ s. auch XP8.readbrettliste! }

{$R-}
procedure loadbretter(pronet:boolean);
const s : string = '';
      p : byte = 0;
var t   : text;

  procedure berror(txt:atext);
  begin
    writeln; writeln;
    close(t);
    error(txt);
  end;

  procedure qsort(l,r:integer);
  var i,j : integer;
      x   : string[80];
      w   : pointer;
  begin
    i:=l; j:=r;
    x:=brettp[(l+r) div 2]^.name;
    repeat
      while brettp[i]^.name<x do inc(i);
      while brettp[j]^.name>x do dec(j);
      if i<=j then begin
        w:=brettp[i]; brettp[i]:=brettp[j]; brettp[j]:=w;
        inc(i); dec(j);
        end;
    until i>j;
    if l<j then qsort(l,j);
    if r>i then qsort(i,r);
  end;

begin
  write('lade Brettliste ...');
  assign(t,brettfile);
  reset(t);
  if ioresult<>0 then
    error('Bretterdatei fehlt')
  else begin
    bretter:=0;
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      if (s<>'') and (s[1]<>'#') and
         (not pronet or ((s[1]<>';') and (s[1]<>'-') and (left(s,4)<>'CODE')))
      then begin
        if cpos(' ',s)=0 then
          berror('Fehler in Brettdatei');
        inc(bretter);
        if bretter>maxbrett then
          berror('zu viele EintrÑge in Brettdatei!');
        if memavail<100 then
          berror('zu wenig Speicher');
        new(brettp[bretter]);
        if pronet then begin
          brettp[bretter]^.code:=left(s,4);
          brettp[bretter]^.name:=trim(mid(s,32));
          end
        else begin
          if s[41]<>' ' then
            brettp[bretter]^.code:=copy(s,41,4)
          else
            brettp[bretter]^.code:=trim(copy(s,41,6));
          p:=40;
          while (p>0) and (s[p]=' ') do dec(p);
          s[0]:=chr(p);
          { UpString(s); }
          if left(s,1)<>'/' then s:=left('/'+s,40);
          while cpos(' ',s)>0 do
            s[cpos(' ',s)]:='_';
          brettp[bretter]^.name:=s;
          end;
        end;
      end;
    close(t);
    end;
  qsort(1,bretter);
  writeln(' ok.');
  writeln;
end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }


procedure testfiles;
begin
  if not exist(infile) then error('Eingabedatei nicht vorhanden');
  if not validfilename(outfile) then error('ungÅltige Ausgabedatei');
end;


procedure Iso2Ibm(var s:string);
const ISO2IBMtab : array[128..255] of byte =
      (128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
       144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
        32,173,155,156,120,157,124, 21, 34, 67,166,174,170, 45, 82,223,
       248,241,253,252, 39,230,227,249, 44, 49,167,175,172,171, 47,168,
       133,160,131, 65,142,143,146,128,138,144,136,137,141,161,140,139,
        68,165,149,162,147,111,153,120,237,151,163,150,154,121, 80,225,
       133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
       100,164,149,162,147,111,148,246,237,151,163,150,129,121,112,152);
var i : integer;
begin
  {$R-}
  for i:=1 to length(s) do
    if (s[i]>=#128) then
      s[i]:=chr(iso2ibmtab[byte(s[i])]);
{$IFDEF Debug }
  {$R+}
{$ENDIF }
end;


{ --- Konvertierung -------------------------------------------------- }


procedure wrs(s:string);
begin
  s:=s+#13#10;
  blockwrite(f2,s[1],length(s));
end;

procedure fmove(size:longint);
const bs = 20000;
var p  : pointer;
    rr : word;
begin
  getmem(p,bs);
  repeat
    blockread(f1,p^,min(bs,size),rr);
    blockwrite(f2,p^,rr);
    dec(size,rr);
  until (size=0) or eof(f1);
  freemem(p,bs);
end;

function reverse(pfad:string):string;
var s : string;
    p : byte;
begin
  pfad:=trim(pfad);
  s:='';
  while pfad<>'' do begin
    p:=1;
    while (p<=length(pfad)) and (pfad[p]<>'!') do inc(p);
    if s<>'' then s:='!'+s;
    s:=left(pfad,p-1)+s;
    delete(pfad,1,p);
    end;
  reverse:=s;
end;

procedure ZtoZCdatum(var d1,d2:string);
begin
  if ival(left(d1,2))<70 then d2:='20'+d1+'00W+0'
  else d2:='19'+d1+'00W+0';
end;


procedure MZ(pronet:boolean);
const maxmlines = 10;   { max. $-Zeilen, die in Z-Text Åbernommen werden }
      readfirst = 2000;
type buf = array[0..readfirst-1] of byte;
var hd   : header;
    s    : string;
    p    : byte;
    mc   : string[12];
    adr0,
    adr1,
    adr2 : longint;
    ok   : boolean;
    i    : integer;
    nn   : longint;
    bp   : ^buf;
    bpos : word;
    bsize: word;
    ef1  : boolean;
    adds : longint;

    mlanz : integer;
    mline : array[1..maxmlines] of ^string;

  procedure bread;
  begin
    blockread(f1,bp^,readfirst,bsize);
    bpos:=0;
    ef1:=eof(f1);
  end;

  {$R-}
  procedure rdln;
  var l,b : byte;

    procedure getb;
    begin
      if bpos<bsize then begin
        b:=bp^[bpos]; inc(bpos);
        if (bpos=bsize) and not ef1 then begin
          inc(adr0,readfirst);
          bread;
          end;
        end;
    end;

  begin
    l:=0;
    if bpos<bsize then
      repeat
        getb;
        if (b<>13) and (b<>10) then begin   { LFs werden einfach ignoriert }
          inc(l); s[l]:=chr(b); end;
      until (b=13) or (bpos>=bsize);
    s[0]:=chr(l);
    { if b=13 then getb;     { LF Åberlesen }
  end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }

  function ff(msgid:string):string;
  begin
    msgid:=trim(msgid);
    if left(msgid,1)='<' then delfirst(msgid);
    if right(msgid,1)='>' then dellast(msgid);
    ff:=msgid;
  end;

  procedure wrml(s:string);
  var p : byte;
  begin
    s:=trim(s);
    p:=cpos(' ',s);
    if (p>=10) and (s[p-1]=':') then
      dec(p)
    else
      if p>0 then
        while (p<=length(s)) and (s[p]=' ') do
          delete(s,p,1);
    if s[p]=':' then
      wrs('Maggi-'+s);
  end;

begin
  new(bp);
  reset(f1,1); rewrite(f2,1);
  nn:=0;
  ok:=true;
  while not eof(f1) and ok do with hd do begin
    mlanz:=0;
    adr0:=filepos(f1);
    adr2:=adr0;
    bread;
    s:='';
    while (bpos<bsize) and (s<>^A) do rdln;
    if s<>^A then
      adr2:=filesize(f1)
    else begin
      fillchar(hd,sizeof(hd),0);
      rdln;
      if left(s,1)<>'-' then begin
        fido_to:=trim(mid(s,5));
        s:=ustr(left(s,4));
        if s='' then
          empfaenger:='/UNZUSTELLBAR'
        else begin
          i:=1;
          while (i<=bretter) and (brettp[i]^.code<>s) do inc(i);
          if i>bretter then empfaenger:='/'+s
          else empfaenger:=brettp[i]^.name;
          end;
        end
      else begin
        delete(s,1,1);
        p:=cpos(':',s);
        if p=0 then ok:=false
        else empfaenger:=trim(mid(s,p+1))+'@'+trim(left(s,p-1));
        end;
      if ok then begin
        rdln; absender:=trim(s);
        rdln; while pos('@',left(s,15))>0 do s[cpos('@',s)]:='#';  { @ -> # }
              absender:=trim(left(s,15))+'@'+absender;
              realname:=trim(mid(s,16));
              if pronet and ((length(realname)<3) or
                  ((length(realname)<10) and   { TopNET-Gate-Fehler beheben }
                 (realname[length(realname)] in ['0'..'9']))) then begin
                insert(realname,absender,16);
                realname:='';
                end;
        rdln; betreff:=trim(s);
        rdln; ok:=(s=^B);
        end;
      while ok and (s<>'') do begin
        rdln;
        if s[1]<>'$' then
          s:=''
        else begin
          mc:=ustr(left(s,11));
          if (left(mc,2)='$ ') and (pos('NET',copy(mc,3,8))>0) then begin
            programm:=trim(mid(s,12));
            repeat
              p:=cpos('[',programm);
              if p>0 then programm:=trim(copy(programm,p+1,length(programm)-p-1));
            until p=0;
            end else
          if mc='$ ROUTE   :' then begin
            pfad:=trim(mid(s,12));
            p:=cpos(' ',pfad);
            while p>0 do begin
              pfad[p]:='!';
              p:=cpos(' ',pfad);
              end;
            end else
          if mc='$ ORIGIN  :' then begin
            datum:=copy(s,21,2)+copy(s,16,2)+copy(s,13,2)+copy(s,24,2)+
                   copy(s,27,2);
            ZtoZCdatum(datum,zdatum);
            end else
          if mc='$ REALNAME:' then
            realname:=trim(mid(s,12)) else
          if (mc='$ MSG-ID  :') or (mc='$ MSGID   :') then
            msgid:=ff(copy(s,12,120)) else
          if (mc='$ REPLY-ID:') or (mc='$ X-REF   :') then
            ref:=ff(copy(s,12,120)) else
          if mc='$ ABSENDER:' then begin
            s:=trim(copy(s,12,79));
            p:=cpos('@',s);
            if p>0 then
              absender:=s
            else begin
              p:=cpos(':',s);
              if p>0 then
                absender:=mid(s,p+1)+'@'+left(s,p-1);
              end;
            end else
          if mc='$ BETREFF :' then
            betreff:=trim(mid(s,12)) else
          if mc='$ FLAGS   :' then begin
            if multipos('sl',mid(s,12)) then
              inc(attrib,attrReqEB);
            end
          else if (s<>'') and (mlanz<maxmlines) then begin   { unbekannte Zeile Åbernehmen }
            inc(mlanz);
            getmem(mline[mlanz],length(s)+1);
            mline[mlanz]^:=mid(s,3);
            end;
          end;
        end;
      if ok then begin
        adr1:=adr0+bpos;
        while (bpos<bsize) and (s<>^X) do rdln;
        if s<>^X then ok:=false;
        end;
      if ok then begin
        inc(nn); write(#13,'Nachrichten: ',nn);
        adr2:=adr0+bpos;
        groesse:=adr2-4-adr1+1;
        adds:=0;
     {  for i:=1 to mlanz do
          inc(adds,length(mline[i]^)+2);
        if mlanz>0 then inc(adds,2); }
        if (direction=1) or (direction=8) then begin    { M->Z }
          wrs('ABS: '+absender+iifs(realname<>'',' ('+realname+')',''));
          wrs('EMP: '+empfaenger);
          wrs('BET: '+betreff);
          wrs('ROT: '+reverse(pfad));
          wrs('MID: '+msgid);
          if ref<>'' then wrs('BEZ: '+ref);
          if zdatum<>'' then
            wrs('EDA: '+zdatum)
          else
            wrs('EDA: '+right(date,4)+copy(date,4,2)+left(date,2)+
                        left(time,2)+copy(time,4,2)+copy(time,7,2)+'W+0');
          wrs('LEN: '+strs(groesse+adds));
          if programm<>'' then
            wrs('MAILER: '+programm);
          if attrib and attrReqEB<>0 then
            wrs('EB:');
          for i:=1 to mlanz do
            wrml(mline[i]^);
          wrs('X-XP-NTP: '+iifs(pronet,'4','3'));
          if fido_to<>'' then
            wrs('X-XP-FTO: '+fido_to);
          wrs('');
          end
        else begin                   { M->N }
          wrs(empfaenger);
          wrs(betreff);
          wrs(absender);
          wrs(datum);
          wrs(pfad);
          wrs('!!');
          wrs('T');
          wrs(strs(groesse));
          end;
      { for i:=1 to mlanz do
          wrs(mline[i]^);
        if mlanz>0 then wrs(''); }
        seek(f1,adr1);
        fmove(groesse);
        end;
      for i:=1 to mlanz do
        freemem(mline[i],length(mline[i]^)+1);
      end;
    seek(f1,adr2);
    end;
  close(f1); close(f2);
  dispose(bp);
  if not ok then begin
    writeln('Fehler - Konvertierung abgebrochen');
    halt(2);
    end;
end;


function zdate:string;
begin
  zdate:=right(date,2)+copy(date,4,2)+copy(date,1,2)+
         left(time,2)+copy(time,4,2);
end;

procedure setzdate(var hd:header);
begin
  with hd do begin
    if datum='' then datum:=zdate;
    ZtoZCdatum(datum,zdatum);
    end;
end;


procedure addtoempflist(s:string);
begin
end;


function compmimetyp(typ:string):string;
begin
  if left(typ,12)='application/' then
    compmimetyp:=lstr(mid(typ,12))
  else
    compmimetyp:=lstr(typ);
end;


{$R-}
{$I xpmakehd.inc}     { MakeHeader() }
{$IFDEF Debug }
  {$R+}
{$ENDIF }

procedure cerror;
begin
  writeln;
  writeln('Fehler - Konvertierung abgebrochen');
  halt(2);
end;


procedure ZM(pronet:boolean);
var hd    : header;
    hds   : longint;
    i     : integer;
    ok,pm : boolean;
    berr  : boolean;
    adr   : longint;
    rr    : word;
    node  : string[15];
    fs    : longint;
    nn    : longint;
    alias : boolean;
{    SevenNet : boolean; }
    MagicNet : boolean;
    reqfile  : text;
    reqopen  : boolean;
    s     : string;

  procedure killEOF;
  var c : char;
  begin
    seek(f2,filesize(f2)-1);
    blockread(f2,c,1);
    if c=#26 then begin
      seek(f2,filesize(f2)-1);
      truncate(f2);
      end;
  end;

  procedure Leerzeile;
  var c : char;
  begin
    seek(f2,filesize(f2)-1);
    blockread(f2,c,1);
    if c<>#10 then wrs('');
  end;

begin
{  SevenNet:=(lstr(netzname)='sevennet'); }
  MagicNet:=(lstr(netzname)='magicnet');
  reset(f1,1);
  rewrite(f2,1);
  if filesize(f1)<16 then begin
    close(f1); close(f2);
    exit; end;               { leerer ZC-Puffer }
  adr:=0; nn:=0;
  reqopen:=false;
  fs:=filesize(f1);
  repeat
    seek(f1,adr);
    makeheader(true,f1,0,0,hds,hd,ok,false);
    if ok then with hd do begin
      UpString(empfaenger);
      UpString(absender);
      if (empfaenger='SYSTEM@'+real_box) and (betreff='REQUEST') then begin
        if not reqopen then begin
          assign(reqfile,real_box+'.REQ');
          rewrite(reqfile);
          reqopen:=true;
          end;
        seek(f1,adr+hds);
        blockread(f1,s[1],min(255,groesse),rr);
        s[0]:=chr(rr);
        write(reqfile,s);
        end
      else begin
        pm:=cpos('@',empfaenger)>0;
        if not pm then begin
          i:=1;
          while (i<=bretter) and (ustr(brettp[i]^.name)<>ustr(empfaenger)) do
            inc(i);
          berr:=(i>bretter);
          if berr then writeln('unbekanntes Brett:  ',empfaenger)
          else empfaenger:=brettp[i]^.code+fido_to;
          end
        else begin
          i:=cpos('@',empfaenger);
          while pos('#',left(empfaenger,i))>0 do
            empfaenger[cpos('#',empfaenger)]:='@';    { # -> @ }
          node:=mid(empfaenger,i+1);
          if cpos('.',node)>0 then
            node:=left(node,cpos('.',node)-1);
        { if (empfaenger='SYSTEM@'+node) then
            empfaenger:='-'+left(empfaenger,i-1)+sp(16-i)
          else }
            empfaenger:='-'+node+':'+left(empfaenger,i-1)+sp(16-i);
          berr:=false;
          end;
        if berr then halferror:=true
        else begin
          inc(nn);
          write(#13,'Nachrichten: ',nn);
          alias:=(real_box='');
          wrs(^A);                       { ^A            }
          wrs(empfaenger);               { EmpfÑnger     }
          i:=cpos('@',absender);
          node:=mid(absender,i+1);
          if cpos('.',node)>0 then
            node:=left(node,cpos('.',node)-1);
          if real_box='' then real_box:=node;
          if hd_point<>'' then node:=hd_point;
          wrs(node);                     { Absender-Node }
          wrs(forms(left(absender,i-1),15)+realname);
          if not magicnet or (length(betreff)<=25) then wrs(betreff)  { Betreff }
          else wrs(left(betreff,24)+'>');
          wrs(^B);                       { Header-Ende   }
          if cpos('[',programm)>0 then
            programm:=trim(left(programm,cpos('[',programm)-1));
          if pronet then begin
            wrs('$ '+forms(netzname,8)+': '+node+' ['+programm+']');
            wrs('$ Route   : '+pfad);     { Boxname }
            wrs('$ Origin  : '+copy(datum,5,2)+'-'+copy(datum,3,2)+'-'+
                left(zdatum,2)+left(datum,2)+' '+
                copy(datum,7,2)+':'+copy(datum,9,2)+':'+copy(zdatum,13,2));
            wrs('$ MsgID   : '+msgid);
            if ref<>'' then wrs('$ Reply-ID: '+ref);
            end
          else begin
            if not alias then
              wrs('$ '+forms(netzname,8)+': '+node+' ['+programm+']')  { Programmname }
            else
              wrs('$ '+forms(netzname,8)+': '+hd_point+' ['+programm+']');
            wrs('$ Route   : '+pfad);      { Route        }
            wrs('$ Origin  : '+copy(datum,5,2)+'-'+copy(datum,3,2)+'-'+
                left(zdatum,2)+left(datum,2)+' '+copy(datum,7,2)+':'+copy(datum,9,2));
            if realname<>'' then           { Realname      }
              wrs('$ Realname: '+realname);
            if msgids and (msgid<>'') then  { Message-ID   }
              wrs('$ Msg-ID  : <'+msgid+'>');
            if ref<>'' then
              wrs('$ Reply-ID: <'+ref+'>');
            if magicnet and (length(betreff)>25) then
              wrs('$ Betreff : '+betreff);
            if attrib and attrReqEB<>0 then
              wrs('$ Flags   : s');
            end;
          wrs('');
          seek(f1,adr+hds);
          fmove(groesse);                { Text          }
          killEOF;
          Leerzeile;
          wrs(^X);                       { Textende      }
          end;
        end;
      end;
    inc(adr,hd.groesse+hds);
  until (adr>=fs) or not ok;
  writeln;
  close(f1); close(f2);
  if reqopen then close(reqfile);
  if not ok then cerror;
end;


procedure ZQZ(zq:boolean);
var hd    : header;
    hds   : longint;
    i     : integer;
    ok,pm : boolean;
    adr   : longint;
    fs    : longint;
    nn    : longint;
    p1,p2 : byte;
begin
  reset(f1,1);
  rewrite(f2,1);
  if filesize(f1)<16 then begin
    close(f1); close(f2);
    exit; end;               { leerer ZC-Puffer }
  adr:=0; nn:=0;
  fs:=filesize(f1);
  repeat
    seek(f1,adr);
    makeheader(zq,f1,0,0,hds,hd,ok,false);
    if ok then with hd do begin
      inc(nn);
      write(#13,'Nachrichten: ',nn);
      p1:=cpos('@',empfaenger);
      pm:=(p1>0);
      if pm then
        if zq then begin
          p2:=pos('.',mid(empfaenger,p1+1));
          if p2>0 then empfaenger:=left(empfaenger,p1+p2-1);
          end
        else
      else begin
        if zq and (left(empfaenger,1)='/') then
          empfaenger:=mid(empfaenger,2);
        if not g_und_s then
          for i:=1 to length(empfaenger) do
            if empfaenger[i]='/' then empfaenger[i]:='\'
            else if empfaenger[i]='\' then empfaenger[i]:='/';
        if not zq and (left(empfaenger,1)<>'/') then
          empfaenger:='/'+empfaenger;
        end;
      if zq then begin
        wrs(empfaenger);
        wrs(betreff);
        wrs(absender);
        wrs(datum);
        repeat
          p1:=cpos('!',pfad);
          if p1>0 then pfad[p1]:=' ';
        until p1=0;
        if not g_und_s and (pfad<>'') and (right(pfad,1)<>' ') then
          pfad:=pfad+' ';
        wrs(pfad);
        wrs(msgid);
        wrs(typ);
        wrs(strs(groesse));
        end
      else begin
        wrs('EMP: '+empfaenger);
        wrs('ABS: '+absender);
        wrs('BET: '+betreff);
        pfad:=trim(pfad);
        repeat
          p1:=cpos(' ',pfad);
          if p1>0 then
            pfad:=left(pfad,p1-1)+'!'+trim(mid(pfad,p1+1));
        until p1=0;
        wrs('ROT: '+reverse(pfad));
        wrs('MID: '+msgid);
        setzdate(hd);
        wrs('EDA: '+zdatum);
        if typ='B' then wrs('BIN:');
        wrs('LEN: '+strs(groesse));
        wrs('X_C:');
        wrs('X-XP-NTP: 10');
        wrs('');
        end;
      seek(f1,adr+hds);
      fmove(groesse);
      end;
    inc(adr,hd.groesse+hds);
  until (adr>=fs) or not ok;
  writeln;
  close(f1); close(f2);
  if not ok then cerror;
end;




procedure ZMaus;
type  barr  = array[0..20000] of byte;
      barrp = ^barr;
var hd     : header;
    hds    : longint;
    ok     : boolean;
    adr    : longint;
    rr     : word;
    fs     : longint;
    nn     : longint;
    p1,p2  : byte;
    t      : text;
    s      : string;
    bufpos : word;
    buf    : barrp;
    size   : longint;
    pmb    : boolean;  { PM-BestÑtigung }
    tomaus : boolean;  { evtl. Steuernachricht }
    komzu  : boolean;
    umlaute: set of char;
    fch    : string[1];

  function getchar:char;
  begin
    if bufpos>=rr then
      if size=0 then rr:=0
      else begin
        blockread(f1,buf^,min(sizeof(buf^),size),rr);
        dec(size,rr);
        bufpos:=0;
        end;
    if bufpos<rr then begin
      getchar:=char(buf^[bufpos]);
      inc(bufpos);
      end;
  end;

  procedure getline;
  var b : byte;
      c : char;
  begin
    b:=0;
    c:=#0;
    while (c<>#10) and ((bufpos<rr) or (size>0)) and (b<253) do begin
      c:=getchar;
      if (c>=' ') or (c=#9) then begin
        inc(b); s[b]:=c;
        end;
      end;
    if b<253 then begin
      s[b+1]:=#13; s[b+2]:=#10;
      inc(b,2);
      end;
    s[0]:=chr(b);
  end;

  procedure get_infofiles;
  var t1   : text;
      s    : string[120];
      inf  : string[10];
      tage : shortint;
      today: datetimest;
      dat  : datetimest;
      crc  : longint;
      _d   : fdate;
      i    : integer;
      _iti : boolean;
      info : MausInfAP;
      infos: integer;

    function gets:string;
    var p : byte;
    begin
      p:=cpos(' ',s);
      if p=0 then p:=length(s)+1;
      gets:=trim(left(s,p));
      s:=trim(mid(s,p));
    end;

  begin
    new(info);
    MausReadITI(boxname,info,infos);
    _iti:=MausIT_files and not exist(boxname+'.iti');
    if _iti then writeln(t,':ITI -1');
    assign(t1,boxname+'.inf');
    reset(t1);
    if ioresult=0 then begin
      today:=right(date,4)+copy(date,4,2)+left(date,2);
      while not eof(t1) do begin
        readln(t1,s);
        inf:=gets;
        tage:=minmax(ival(gets),0,99);
        if tage>0 then begin
          dat:=gets;
          crc:=ival(gets);
          with _d do begin
            t:=ival(left(dat,2));
            m:=ival(copy(dat,4,2));
            j:=ival(right(dat,4));
            end;
          for i:=1 to tage do
            incd(_d);
          if (inf<>'ITI') or not _iti then
            if formi(_d.j,4)+formi(_d.m,2)+formi(_d.t,2)<=today then begin
              write(t,':',inf);
              i:=infos;
              while (i>0) and (info^[i].ID<>inf) do dec(i);
              if (i=0) or info^[i].crcflag then
                writeln(t,' ',crc)
              else
                writeln(t);
              end;
          end;
        end;
      close(t1);
      end;
    dispose(info);
  end;

  function UmlShort(s:string; len:byte):string;
  var i,n : byte;
  begin
    n:=0;
    for i:=1 to length(s) do
      if s[i] in umlaute then inc(n);
    UmlShort:=left(s,len-n);
  end;

  procedure uuencode;
  const llen = 45;
        map  : array[0..63] of char =
        '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  var line : array[0..llen-1] of byte;
      s    : string[61];
      i,j  : integer;
      rr   : word;
  begin
    writeln(t,':Content-Type: UU1');
    if hd.ddatum<>'' then
      writeln(t,':File-Date: ',left(hd.ddatum,14));
    writeln(t,':');
    writeln(t,':Diese Nachricht beinhaltet eine uu-codierte BinÑrdatei. Verwenden Sie');
    writeln(t,':das Programm uudecode zum Decodieren.');
    writeln(t,':');
    writeln(t,':This message contains an uu-encoded binary file. Use uudecode to obtain');
    writeln(t,':the original file.');
    writeln(t,':');
    writeln(t,':table');
    writeln(t,':',left(map,32));
    writeln(t,':',mid(map,33));
    writeln(t,':begin 644 ',iifs(hd.datei<>'',hd.datei,mid(zdate,3)+'.msg'));
    {$R-}
    while size>0 do begin
      blockread(f1,line,min(llen,size),rr);
      s[0]:=chr(1+((rr+2) div 3)*4);
      s[1]:=chr(rr+32);
      j:=2; i:=0;
      while i<rr do begin
        s[j]:=map[line[i] shr 2]; inc(j);
        s[j]:=map[(line[i]) and 3 shl 4 + line[i+1] shr 4]; inc(j);
        s[j]:=map[line[i+1] and $f shl 2 + line[i+2] shr 6]; inc(j);
        s[j]:=map[line[i+2] and $3f]; inc(j);
        inc(i,3);
        end;
      writeln(t,':',s);
      dec(size,rr);
      end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }
    writeln(t,':`');
    writeln(t,':end');
    writeln(t,':size ',hd.groesse);
  end;

  function GetBinType(fn:pathstr):string;    { vgl. UUZ.PAS }
  var mtype: string[30];
      p    : byte;
      ext  : string[6];
      t    : text;
      s    : string;
  begin
    mtype:='application/octet-stream';
    p:=rightpos('.',fn);
    if p>0 then begin
      ext:=mid(fn,p+1);
      assign(t,'mimetyp.cfg');
      reset(t);
      if ioresult=0 then begin
        while not eof(t) do begin
          readln(t,s);
          if (s<>'') and (firstchar(s)<>'#') and
             stricmp(ext,GetToken(s,'=')) then
            mtype:=s;
          end;
        close(t);
        end;
      end;
    GetBinType:=mtype;
  end;

  function month(m:string):string;
  begin
    month:=copy('Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ',
                ival(m)*4-3,4);
  end;

  function ZtoRFCdate(date,zdate:string):string;
  var p : byte;
  begin
    p:=cpos(':',zdate);
    if p=0 then p:=length(zdate)+1;
    ZtoRFCdate:=copy(date,5,2)+' '+month(copy(date,3,2))+left(zdate,2)+
         left(date,2)+' '+copy(date,7,2)+':'+copy(date,9,2)+':'+
         copy(zdate,13,2)+' '+zdate[16]+formi(ival(copy(zdate,17,p-17)),2)+
         formi(ival(mid(zdate,p+1)),2);
  end;

  procedure mimeencode;
  const llen = 54;
        map  : array[0..63] of char =
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  var line : array[0..llen-1] of byte;
      s    : string[61];
      i,j  : integer;
      rr   : word;
  begin
    writeln(t,':-');
    writeln(t,':---');
    writeln(t,':Content-Type: text/plain');
    writeln(t,':');
    writeln(t,':Diese Nachricht enthaelt eine MIME-codierte Binaerdatei. Falls Ihr');
    writeln(t,':Mailer die Datei nicht decodieren kann, verwenden Sie dafuer bitte');
    writeln(t,':ein Tool wie ''munpack'' oder ''udec''.');
    writeln(t,':');
    writeln(t,':This message contains a MIME encoded binary file. If your mailer');
    writeln(t,':cannot decode the file, please use a decoding tool like ''munpack''.');
    writeln(t,':');
    writeln(t,':---');
    write  (t,':Content-Type: ',GetBinType(hd.datei));
    if hd.datei<>'' then write(t,'; name="'+hd.datei+'"');
    if hd.ddatum<>'' then write(t,';'#13#10': x-date="'+ZtoRFCdate(hd.datum,hd.zdatum)+'"');
    writeln(t);
    writeln(t,':Content-Transfer-Encoding: base64');
    writeln(t,':');
    {$R-}
    while size>0 do begin
      blockread(f1,line,min(llen,size),rr);
      s[0]:=chr(((rr+2) div 3)*4);
      j:=1; i:=0;
      while i<rr do begin
        s[j]:=map[line[i] shr 2]; inc(j);
        s[j]:=map[(line[i]) and 3 shl 4 + line[i+1] shr 4]; inc(j);
        if i+1<rr then s[j]:=map[line[i+1] and $f shl 2 + line[i+2] shr 6]
        else s[j]:='=';
        inc(j);
        if i+2<rr then s[j]:=map[line[i+2] and $3f]
        else s[j]:='=';
        inc(j);
        inc(i,3);
        end;
      writeln(t,':',s);
      dec(size,rr);
      end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }
    writeln(t,':---');
  end;

begin
  new(buf);
  reset(f1,1);
  assign(t,outfile);
  rewrite(t);
  adr:=0; nn:=0;
  fs:=filesize(f1);
  umlaute:=['Ñ','î','Å','é','ô','ö','·'];
  writeln(t,'#CMD');
  if MausON then
    writeln(t,':ON');     { îffentliche Msgs abrufen }
  writeln(t,':PN');       { private Msgs abrufen     }
  if MausOE then
    writeln(t,':OE');     { eigene Nachrichten nicht zurÅcksenden }
  if MausPSA then
    writeln(t,':PSN');    { Bearbeitungsstati anfordern }
  if maxmaus>0 then
    writeln(t,':M ',maxmaus);     { maximale Outfile-Grî·e vorgeben }
{ writeln(t,':REN'); }
  if mausinfos then
    get_infofiles;
  if fs>=8 then
    repeat
      seek(f1,adr);
      makeheader(true,f1,0,0,hds,hd,ok,false);
      if ok then with hd do begin
        if not mkoutfile then begin
          p1:=cpos('@',msgid);
          if p1=0 then p1:=length(msgid)+1;
          msgid:=left(msgid,min(10,p1-1));
          end;
        p1:=cpos('@',empfaenger);
        if (p1=0) and
           not stricmp(left(ustr(empfaenger),length(bretth)),bretth) then
          writeln(#13#10,'unbekannte Mausgruppe: ',empfaenger)
        else begin
          inc(nn);
          write(#13,'Nachrichten: ',nn);
          pmb:=false;
          komzu:=(attrib and attrQuoteTo<>0) or
                 (not mkoutfile and (p1>0) and (replypath<>boxname));
            { !!: "Kommentar zu" bei PM-Kommentar an andere Pollbox }
          tomaus:=(ref='') and (ustr(left(empfaenger,5))='MAUS@');
          if tomaus and (betreff='<Maus-Command>') then
            writeln(t,'#CMD')
          else if tomaus and (betreff='<Maus-Direct-Command>') then
            pmb:=true
          else begin
            writeln(t,'#',msgid);           { Block-ID }
            if org_msgid<>'' then
              writeln(t,'I',org_msgid);
            if p1>0 then                  { PM }
              if (ref='') or (attrib and attrPmReply<>0) or komzu or mkoutfile
              then begin
                writeln(t,'A',trim(left(empfaenger,p1-1)),' @ ',
                          trim(mid(empfaenger,p1+1)));
                komzu:=(ref<>'');
                end
              else
            else
              { fb: G-Zeile in AM auf jeden Fall schreiben }
              { if komzu or (ref='') or mkoutfile then }
              { /fb}
                writeln(t,'G',mid(empfaenger,length(bretth)+1));
            if mkoutfile then begin
              p2:=cpos('@',absender);
              if p2<>0 then
                absender:=left(absender,p2-1)+' @ '+mid(absender,p2+1);
              writeln(t,'V',absender);
              end;
            writeln(t,'E',left(zdatum,2)+datum);
            writeln(t,'W',betreff {UmlShort(betreff,30)});
            if (typ='B') and (attrib and attrMPbin<>0) then
              writeln(t,'M1.0; Content-Type: multipart/mixed; boundary="-"');
            if p1=0 then    { AM }
              if stricmp(distribution,'mausnet') then
                writeln(t,'DM')
              else if stricmp(distribution,'lokal') then
                writeln(t,'DL')
              else
                writeln(t,'DN');
            if ref<>'' then begin
              if org_xref<>'' then
                writeln(t,'R',org_xref);
              if komzu then begin
                if org_xref='' then writeln(t,'R',ref);
                { fb: Wildwestverkettung nur bei G?K und PM. Ist nach
                Maus-Specs. nicht nîtig, aber der KompatiblitÑt wegen }
                if left(ustr(ReplyGroup),length(bretth))=bretth then
                {/fb}
                  writeln(t,':Kommentar zu ',ref,' in der Gruppe ',
                            ustr(mid(ReplyGroup,length(bretth)+1)));
                end
              else
                writeln(t,'-',ref);
              end;
            if AmReplyTo<>'' then
              writeln(t,':Followup-To: ',mid(AmReplyTo,length(bretth)+1));
            end;
          size:=groesse;              { Nachrichtentext kopieren }
          bufpos:=0; rr:=0;
          seek(f1,adr+hds);
          if typ='B' then
            if attrib and attrMPbin<>0 then
              mimeencode
            else
              uuencode
          else begin
            fch:=iifs(pmb,'',':');
            if size=0 then
              writeln(t,fch)
            else
              while (size>0) or (bufpos<rr) do begin
                getline;
                if (length(s)>2) and (s[length(s)]=#10) and
                   (s[length(s)-1]=#13) and (s[length(s)-2]=' ') then
                   delete(s,length(s)-2,1);
                if (length(s)>2) and (s[length(s)]=#10) and
                   (s[length(s)-1]=#13) and (s[length(s)-2]=' ') then
                   delete(s,length(s)-2,1);
                write(t,fch,s);
                if not pmb then
                  if s[length(s)]=#10 then fch:=':'
                  else fch:='';
                end;
            if not pmb and (fch='') then writeln(t);
            end;
          end;
        end;
      inc(adr,hd.groesse+hds);
    until (adr>=fs) or not ok
  else
    ok:=true;
  if mkoutfile then writeln(t,'#LOG');
  writeln(t,'#');
  writeln;
  close(f1); close(t);
  dispose(buf);
  if not ok then cerror;
end;


procedure MausZ;
const maxilines = 5000;
      maxxlines = 10;
      logfile   = 'MAUS.LOG';
      pmlogfile = 'MAUSPM.LOG';
      stlogfile = 'MAUSSTAT.LOG';
      bufsize   = 8192;
type  tb        = array[0..bufsize-1] of byte;
      tbufa     = array[1..maxilines] of record
                                           s  : ^string;
                                           lf : boolean;
                                         end;
var t1,log     : text;
    f2         : file;
    pmlog,stlog: text;
    hd         : header;
    s          : string[253];
    tbuf       : ^tbufa;
    lines,i,nn : longint;
    c          : char;
    pm         : byte;
    b1         : ^tb;
    firstline  : boolean;
    keinbetreff: boolean;
    killmsg    : boolean;
    info       : MausInfAP;
    infos      : integer;
    mime       : boolean;
    parname    : string[20];
    xline      : array[1..maxxlines] of ^string;
    xlines     : integer;
{$IFDEF BP }
    fsize      : longint;
    xms        : byte;    { 0 = nicht initialisiert, 1=initialisiert }
    xmshandle  : word;                    { 2 = nicht verfÅgbar }
    xmssize    : longint;
    xmsoffset  : longint;
{$ENDIF }

  function mausform(s:string):string;
  var p : byte;
  begin
    p:=cpos('@',s);
    if p=0 then
      mausform:=left(s,79)
    else
      mausform:=left(trim(left(s,p-1))+'@'+trim(mid(s,p+1)),79);
  end;

  procedure appline(s:string; crlf:boolean);
{$IFDEF BP }
  const cr_lf : array[0..1] of char = #13#10;
{$ENDIF }
  begin
    if hd.mimever<>'' then
      ISO2IBM(s);
{$IFDEF BP }
    if (xms=0) and (lines<maxilines) and
       (((lines mod 16)<>0) or (memavail>10000)) then
{$ENDIF }
    begin
      inc(lines);
      getmem(tbuf^[lines].s,length(s)+1);
      tbuf^[lines].s^:=s;
      tbuf^[lines].lf:=crlf;
      inc(hd.groesse,length(s)+iif(crlf,2,0));
    end
{$IFDEF BP }
    else
    begin
      if xms=0 then
        if XmsAvail > 0 then begin
          xmssize:=min(fsize div 1024+1,XmsAvail)*1024;
          xmshandle:=xmsalloc(xmssize div 1024);
          if xmsresult=0 then begin
            xms:=1; xmsoffset:=0;
            end
          else
            xms:=2;
          end
        else
          xms:=2;
      if (xms=1) and (xmsoffset+length(s)+2<xmssize) then begin
        XmsWrite(xmshandle,s[1],xmsoffset,length(s));
        inc(xmsoffset,length(s));
        inc(hd.groesse,length(s));
        if crlf then begin
          XmsWrite(xmshandle,cr_lf,xmsoffset,2);
          inc(xmsoffset,2);
          inc(hd.groesse,2);
          end;
        end;
      end;
{$ENDIF }
  end;

  function infofile(s:string):string;
  var i : integer;
  begin
    UpString(s);
    i:=infos;
    while (i>0) and (s<>info^[i].ID) do dec(i);
    if i>0 then
      infofile:=info^[i].text
    else
      infofile:='Info-File: '+s;
  end;

  procedure wrs(s:string);
  begin
    if length(s)>253 then s[0]:=#253;
    s[length(s)+1]:=#13;
    s[length(s)+2]:=#10;
    inc(byte(s[0]),2);
    blockwrite(f2,s[1],length(s));
  end;

  procedure TestUUbinary;
  var i : integer;
  begin
    i:=1;
    while (i<=lines) and (pos('begin ',tbuf^[i].s^)<>1) do inc(i);
    while (i<=lines) and (pos('end',tbuf^[i].s^)<>1) do inc(i);
    while (i<=lines) and (pos('size ',tbuf^[i].s^)<>1) do inc(i);
    if i>lines then hd.typ:='T';
  end;

  procedure UUdecode;
  var buf  : charrp;
      bufs : word;
      bufp : word;
      i    : integer;
      n,p  : byte;
      s    : string[80];
      b1,b2,
      b3,b4: byte;
  begin
    bufs:=min(maxavail-16,65500);
    getmem(buf,bufs);
    bufp:=0;
    i:=1;
    while (i<=lines) and (pos('begin ',tbuf^[i].s^)<>1) do inc(i);
    s:=tbuf^[i].s^;
    delete(s,1,6);
    s:=trim(mid(s,blankpos(s)));   { Unix-Filemode wegschneiden }
    if blankpos(s)>0 then truncstr(s,blankpos(s)-1);
    wrs('FILE: '+s);
    if hd.ddatum<>'' then wrs('DDA: '+hd.ddatum+'W+0');
    inc(i);
    { R-}
    while pos('end',tbuf^[i].s^)=0 do begin
      s:=tbuf^[i].s^;
      n:=(((ord(s[1])-32) and $3f + 2)div 3)*4;   { anzahl Original-Bytes }
      p:=2;
      while p<n do begin
        b1:=(ord(s[p])-32) and $3f; b2:=(ord(s[p+1])-32) and $3f;
        b3:=(ord(s[p+2])-32) and $3f; b4:=(ord(s[p+3])-32) and $3f;
        inc(p,4);
        buf^[bufp]:=chr(b1 shl 2 + b2 shr 4);
        buf^[bufp+1]:=chr((b2 and $f)shl 4 + b3 shr 2);
        buf^[bufp+2]:=chr((b3 and 3)shl 6 + b4);
        inc(bufp,3);
        end;
      inc(i);
      end;
    { R+}
    while (pos('size ',tbuf^[i].s^)<>1) do inc(i);
    s:=trim(mid(tbuf^[i].s^,5));
    bufp:=min(bufp,ival(s));   { echte Grî·e }
    wrs('LEN: '+strs(bufp));
    wrs('');
    blockwrite(f2,buf^,bufp);
  end;

  procedure WriteInfofile(fn:pathstr);
  var t : text;
      i : longint;
  begin
    assign(t,fn);
    rewrite(t);
    if ioresult=0 then begin
      for i:=1 to lines do             { Nachrichtentext schreiben }
        if tbuf^[i].lf then
          writeln(t,tbuf^[i].s^)
        else
          write(t,tbuf^[i].s^);
      close(t);
      end;
  end;

  procedure add_xline(s:string);
  begin
    if xlines<maxxlines then begin
      inc(xlines);
      xline[xlines]^:=s;
      end;
  end;

{$IFDEF BP }
  procedure CopyXms;
  var buf : pointer;
      ofs : longint;
      blk : word;
  begin
    getmem(buf,1024);
    ofs:=0;
    while (ofs<xmsoffset) do begin
      blk:=min(1024,xmsoffset-ofs);
      XmsRead(xmshandle,buf^,ofs,blk);
      blockwrite(f2,buf^,blk);
      inc(ofs,1024);
      end;
    freemem(buf,1024);
  end;
{$ENDIF }

begin
  new(b1);
  new(tbuf);
  new(info);
  for i:=1 to maxxlines do
    new(xline[i]);
  MausReadITI(boxname,info,infos);
{$IFDEF BP }
  fsize:=_filesize(infile);
{$ENDIF }
  assign(t1,infile); settextbuf(t1,b1^); reset(t1);
  assign(f2,outfile); rewrite(f2,1);
  assign(log,logfile); rewrite(log);
  assign(pmlog,pmlogfile); rewrite(pmlog);
  assign(stlog,stlogfile); rewrite(stlog);
  if not eof(t1) then readln(t1,s);
  nn:=0;
  while not eof(t1) do with hd do begin
{$IFDEF BP }
    xms:=0;
{$ENDIF }
    fillchar(hd,sizeof(hd),0);
    xlines:=0;
    typ:='T';
    lines:=0;
    keinbetreff:=true;
    while (left(s,1)<>'#') and not eof(t1) do
      readln(t1,s);
    if not eof(t1) then begin
      pm:=0;
      if ustr(s)='#LOG' then
        msgid:=ustr(s)
      else
        msgid:=copy(s,2,120);
      firstline:=true;
      mime:=false;
      repeat
        read(t1,s);
        if (s<>'') and (s[1]<>'#') then begin
          c:=s[1];
          delete(s,1,1);
          case c of
            ':' : if mime then
                    if (s='-') or (cpos(':',s)=0) then
                      mime:=false
                    else
                      add_xline(s)
                  else begin
                    appline(s,eoln(t1));
                    if firstline then begin
                      if left(lstr(s),13)='kommentar zu ' then begin
                        ref:=trim(mid(s,14));
                        if cpos(' ',ref)>0 then
                          ref:=left(ref,cpos(' ',ref)-1);
                        end
                      else if (left(s,1)='-') and (cpos('@',s)>0) and (ref='')
                      then begin
                        ref:=trim(mid(s,2));
                        if cpos(' ',ref)>0 then
                          ref:=left(ref,cpos(' ',ref)-1);
                        end
                      else if left(lstr(s),13)='followup-to: ' then
                        AmReplyTo:=bretth+trim(mid(s,14))
                      else if left(lstr(s),10)='reply-to: ' then
                        PmReplyTo:=trim(mid(s,11))
                      else if lstr(s)='content-type: uu1' then
                        typ:='B'
                      else if lstr(left(s,11))='file-date: ' then
                        ddatum:=trim(mid(s,12))
                      else
                        firstline:=false;
                      end;
                    while not eoln(t1) do begin
                      read(t1,s);
                      appline(s,eoln(t1));
                      end;
                  end;
            'A' : if empfaenger='' then empfaenger:=mausform(s);
            'B' : begin pm_bstat:=left(s,15); inc(pm); end;
            'E' : begin datum:=copy(s,3,10); ZtoZCdatum(datum,zdatum); end;
            'G' : empfaenger:=bretth+s;
            'O' : organisation:=s;
            'V' : absender:=mausform(s);
            'W' : begin betreff:=s; inc(pm); keinbetreff:=false; end;
            { fb: Name des Gateway aus der daf?r vorgesehenen Header-Zeile
              nehmen }
            'Y' : gate:=left(s,80);
            { /fb }
            'I' : org_msgid:=left(s,120);
            'R' : org_xref:=left(s,120);
            'N' : realname:=left(s,40);
            'D' : if s<>'' then case s[1] of
                    'M' : distribution:='MausNet';
                    'L' : distribution:='lokal';
                  end;
            'M' : begin
                    mimever:=GetToken(s,';');
                    mime:=true;
                    if lstr(GetToken(s,':'))='content-type' then
                      mimect:=s;
                  end;
            { redundante Kommentar-Zeilen aus Outfile tilgen }
            '>' : if (left(s,9)<>'boundary=') and (not mausKF) then begin
                    parname:=lstr(trim(left(s,cposx(':',s)-1)));
                    if (parname<>'mid') and (parname<>'rid') and
                       (parname<>'mime')
                       then appline(s,true);
                    if parname='gate' then begin
                       if GetToken(s,':')='' then;
                       gate:=s;
                       end;
                  end;
            {/fb}
             '-' : ref:=left(s,120);
          end;
          s:=c+s;
        end;
        readln(t1);
      until (left(s,1)='#') or eof(t1);
      if pm=2 then writeln(pmlog,msgid);
      if zdatum='' then begin
        datum:=zdate; setzdate(hd); end;
      if msgid='#LOG' then
        for i:=1 to lines do
          writeln(log,tbuf^[i].s^)
      else begin
        inc(nn);
        write(#13,'Nachrichten: ',nn);
        killmsg:=false;
        if (msgid<>'') and (cpos('@',msgid)=0) then begin   { Info-File }
          empfaenger:='/ØMausinfo';    { s. auch XP0.MausInfoBrett }
          absender:='MausInfo@'+boxname;
          if msgid='HEAD' then
            killmsg:=true            { internes Infofile lîschen }
          else
            if MausIT_files and (left(msgid,2)='IT') and (length(MsgID)=3)
            then begin
              WriteInfofile(boxname+'.'+msgid);    { ITI/ITG/ITB in Datei }
              killmsg:=true;                       { schreiben            }
              end
            else
              betreff:=infofile(msgid);
          msgid:='';
          end                                               { Statusinfo }
        else if (betreff='') and keinbetreff and (pm_bstat<>'') then begin
          writeln(stlog,'#',msgid);
          writeln(stlog,'=',pm_bstat);
          killmsg:=true;
          end;
        if not killmsg then begin
          pfad:=boxname;
          if absender='' then absender:='Absender fehlt?!@'+boxname;
          wrs('EMP: '+empfaenger);
          if cpos('@',absender)=0 then
            absender:=absender+'@'+boxname;  { fÅr lokale Quark-Nachrichten }
          if realname='' then
            wrs('ABS: '+absender)
          else
            wrs('ABS: '+absender+' ('+realname+')');
          wrs('BET: '+betreff);
          wrs('EDA: '+zdatum);
          wrs('ROT: '+reverse(pfad));
          wrs('MID: '+msgid);
          { fb: Organisation aus O-Zeile ?bertragen }
          if organisation<>'' then wrs('ORG: '+organisation);
          { /fb }
          if ref<>'' then wrs('BEZ: '+ref);
          if AmReplyTo<>'' then wrs('Diskussion-in: '+AmReplyTo);
          if PmReplyTo<>'' then wrs('Antwort-an: '+PmReplyTo);
          if programm<>''  then wrs('Mailer: '+programm);
          if gate<>''      then wrs('Gate: '+gate);
          if distribution<>'' then wrs('U-Distribution: '+distribution);
          if mimever<>'' then wrs('U-MIME-Version: '+mimever);
          if mimect<>'' then wrs('U-Content-Type: '+mimect);
          for i:=1 to xlines do
            wrs('U-'+xline[i]^);
          wrs('X_C:');
          wrs('X-XP-NTP: 20');
          if pm_bstat<>''  then wrs('X-XP-BST: '+pm_bstat);
          if org_msgid<>'' then wrs('X-XP-ORGMID: '+org_msgid);
          if org_xref<>''  then wrs('X-XP-ORGREF: '+org_xref);
          if typ='B' then TestUUbinary;
          if typ='B' then begin
            wrs('TYP: BIN');
            UUdecode;    { schreibt LEN, crlf + Inhalt }
            end
          else begin
            wrs('LEN: '+strs(groesse));
            wrs('');
            for i:=1 to lines do             { Nachrichtentext schreiben }
              if tbuf^[i].lf then
                wrs(tbuf^[i].s^)
              else
                blockwrite(f2,tbuf^[i].s^[1],length(tbuf^[i].s^));
{$IFDEF BP }
            if xms=1 then begin
              CopyXms;
              XmsFree(xmshandle);
              end;
{$ENDIF }
            end;
          end;
        end;
      for i:=lines downto 1 do
        freemem(tbuf^[i].s,length(tbuf^[i].s^)+1);
      end;
    end;
  for i:=1 to xlines do
    dispose(xline[i]);
  dispose(info);
  dispose(tbuf);
  close(t1); dispose(b1);
  close(f2);
  close(log);
  close(pmlog);
  close(stlog);
end;


begin
  test8086:=0;
  logo;
  getpar;
  if direction in [1,2,3,8,9] then loadbretter(direction in [8,9]);
  testfiles;
  assign(f1,infile);
  assign(f2,outfile);
  case direction of
    1,3 : MZ(false);       { Maggi -> ZC }
    2   : ZM(false);       { ZC -> Maggi }
    4   : ZQZ(false);      { Quick -> ZC }
    5   : ZQZ(true);       { ZC -> Quick }
    6   : MausZ;           { Maus -> ZC  }
    7   : ZMaus;           { ZC -> Maus  }
    8   : MZ(true);        { ProNet -> ZC }
    9   : ZM(true);        { ZC -> ProNET }
  end;
  if halferror then halt(2);
end.

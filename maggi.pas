{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{$I xpdefine.inc }

unit maggi;

interface

implementation

uses sysutils, classes, xpheader, xpmakeheader,
  typeform,fileio,montage,xpdatum,xp_iti, xpglobal;

const nt_ZConnect  = 2;
      nt_UUCP      = 40;
      readempflist = false;
      readkoplist  = false;

      direction : byte = 0;
      infile    : string = '';
      outfile   : string = '';
      brettfile : string = '';
      netzname  : string[8] = '';
      maxbrett  = 2000;
      halferror : boolean = false;   { Teilkonvertierung }
      bretth    : string[25] = '';
      boxname   : string[20] = '';
      msgids    : boolean = false;   { MagicNET - MsgID's }
      mausinfos : boolean = false;   { Infofiles bestellen / BOX.INF }
      g_und_s   : boolean = false;   { keine Brettslashs aendern }
      maxmaus   : Int64 = 0;         { vorgegebene Maximalgroesse }
      mkoutfile : boolean = false;   { OUTFILE statt INFILE erzeugen }
      mausOE    : boolean = true;
      mausPSA   : boolean = true;
      mausON    : boolean = true;    { oeffentl. Nachrichten anfordern }
      mausKF    : boolean = true;    { fb: Filter fuer Kommentare }
      MausIT_files : boolean = false;{ ITI/ITG -> Box.IT* }

      attrMPbin   = $0040;            { Multipart-Binary           }
      attrReqEB   = $1000;            { EB anfordern               }
      attrIsEB    = $2000;            { EB                         }
      AttrPmReply = $0100;            { PM-Reply auf AM (Maus)     }
      AttrQuoteTo = $0400;            { QuoteTo (Maus)             }
      AttrQPC     = $0001;            { QPC-codiert                }
      AttrControl = $0020;            { Cancel-Nachricht }


type  charr       = array[0..65530] of char;
      charrp      = ^charr;


      brett  = record
                 code  : string[4];
                 name  : string[40];
               end;

var   f1,f2     : file;
      bretter   : integer;
      brettp    : array[1..maxbrett] of ^brett;

      empflist,uline,xline,mail : tstringlist;
      hd : THeader;




const
  { wird zum Einlesen der Customizable Headerlines benoetigt }
      mheadercustom : array[1..2] of string = ('','');

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
  writeln('Ung�ltige(r) Parameter');
  writeln;
  helppage;
end;

procedure error(txt:atext);
begin
  writeln('Fehler: ',txt);
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
    isl:=(LeftStr(s,length(t)+1)='-'+t) or (LeftStr(s,length(t)+1)='/'+t);
  end;

begin
  for i:=1 to paramcount do begin
    s:=LowerCase(paramstr(i));
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
    else if isl('h') then bretth:=UpperCase(copy(s,3,25))
    else if isl('b') then boxname:=UpperCase(copy(s,3,8))
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
    else if (LeftStr(s,1)='/') or (LeftStr(s,1)='-') then parerr
    else if infile='' then infile:=s
    else if outfile='' then outfile:=s
    else if brettfile='' then brettfile:=s
    else parerr;
    end;
  if ((direction in [1,2,3,8,9]) and ({(brettfile='') or} (netzname=''))) or
     ((direction in [6,7]) and ((bretth='') or (boxname=''))) or
     (direction=0) or (outfile='') then helppage;
  writeln(UpperCase(infile),'  ��  ',UpperCase(outfile));
  writeln;
end;


{ s. auch XP8.readbrettliste! }

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
         (not pronet or ((s[1]<>';') and (s[1]<>'-') and (LeftStr(s,4)<>'CODE')))
      then begin
        if cpos(' ',s)=0 then
          berror('Fehler in Brettdatei');
        inc(bretter);
        if bretter>maxbrett then
          berror('zu viele Eintr�ge in Brettdatei!');
        new(brettp[bretter]);
        if pronet then begin
          brettp[bretter]^.code:=LeftStr(s,4);
          brettp[bretter]^.name:=trim(mid(s,32));
          end
        else begin
          if s[41]<>' ' then
            brettp[bretter]^.code:=copy(s,41,4)
          else
            brettp[bretter]^.code:=trim(copy(s,41,6));
          p:=40;
          while (p>0) and (s[p]=' ') do dec(p);
          SetLength(s, p);
          { UpString(s); }
          if LeftStr(s,1)<>'/' then s:=LeftStr('/'+s,40);
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

procedure testfiles;
begin
  if not fileexists(infile) then error('Eingabedatei nicht vorhanden');
  if not validfilename(outfile) then error('ung�ltige Ausgabedatei');
end;

{ --- Konvertierung -------------------------------------------------- }


procedure wrs(s:string);
begin
  s:=s+#13#10;
  blockwrite(f2,s[1],length(s));
end;

procedure fmove(size:longint);
const bs = 32768;
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
    s:=LeftStr(pfad,p-1)+s;
    delete(pfad,1,p);
    end;
  reverse:=s;
end;

procedure MZ(pronet:boolean);
const maxmlines = 10;   { max. $-Zeilen, die in Z-Text uebernommen werden }
      readfirst = 2000;
type buf = array[0..readfirst-1] of byte;
var hd   : THeader;
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
    mline : array[1..maxmlines] of string;

  procedure bread;
  begin
    blockread(f1,bp^,readfirst,bsize);
    bpos:=0;
    ef1:=eof(f1);
  end;

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
    SetLength(s, l);
    { if b=13 then getb; }    { LF ueberlesen }
  end;

  function ff(msgid:string):string;
  begin
    msgid:=trim(msgid);
    if LeftStr(msgid,1)='<' then DeleteFirstChar(msgid);
    if RightStr(msgid,1)='>' then DeleteLastChar(msgid);
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
      if LeftStr(s,1)<>'-' then begin
        fido_to:=trim(mid(s,5));
        s:=UpperCase(LeftStr(s,4));
        if s='' then
          Firstempfaenger:='/UNZUSTELLBAR'
        else begin
          i:=1;
          while (i<=bretter) and (brettp[i]^.code<>s) do inc(i);
          if i>bretter then FirstEmpfaenger:='/'+s
          else FirstEmpfaenger:=brettp[i]^.name;
          end;
        end
      else begin
        delete(s,1,1);
        p:=cpos(':',s);
        if p=0 then ok:=false
        else FirstEmpfaenger:=trim(mid(s,p+1))+'@'+trim(LeftStr(s,p-1));
        end;
      if ok then begin
        rdln; absender:=trim(s);
        rdln; while cPos('@',LeftStr(s,15))>0 do s[cpos('@',s)]:='#';  { @ -> # }
              absender:=trim(LeftStr(s,15))+'@'+absender;
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
          mc:=UpperCase(LeftStr(s,11));
          if (LeftStr(mc,2)='$ ') and (pos('NET',copy(mc,3,8))>0) then begin
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
            ZtoZCdatumNTZ(datum,zdatum);
            end else
          if mc='$ REALNAME:' then
            realname:=trim(mid(s,12)) else
          if (mc='$ MSG-ID  :') or (mc='$ MSGID   :') then
            msgid:=ff(copy(s,12,120)) else
          if (mc='$ REPLY-ID:') or (mc='$ X-REF   :') then
            hd.References.Add(ff(copy(s,12,120))) else
          if mc='$ ABSENDER:' then begin
            s:=trim(copy(s,12,79));
            p:=cpos('@',s);
            if p>0 then
              absender:=s
            else begin
              p:=cpos(':',s);
              if p>0 then
                absender:=mid(s,p+1)+'@'+LeftStr(s,p-1);
              end;
            end else
          if mc='$ BETREFF :' then
            betreff:=trim(mid(s,12)) else
          if mc='$ FLAGS   :' then begin
            if multipos('sl',mid(s,12)) then
              inc(attrib,attrReqEB);
            end
          else if (s<>'') and (mlanz<maxmlines) then begin   { unbekannte Zeile uebernehmen }
            inc(mlanz);
            mline[mlanz]:=mid(s,3);
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
          wrs('EMP: '+ FirstEmpfaenger);
          wrs('BET: '+betreff);
          wrs('ROT: '+reverse(pfad));
          wrs('MID: '+msgid);
          if References.Count <> 0 then wrs('BEZ: '+ References[0]);
          if zdatum<>'' then
            wrs('EDA: '+zdatum)
          else
            wrs('EDA: '+RightStr(date,4)+copy(date,4,2)+LeftStr(date,2)+
                        LeftStr(time,2)+copy(time,4,2)+copy(time,7,2)+'W+0');
          wrs('LEN: '+strs(groesse+adds));
          if programm<>'' then
            wrs('MAILER: '+programm);
          if attrib and attrReqEB<>0 then
            wrs('EB:');
          for i:=1 to mlanz do
            wrml(mline[i]);
          wrs('X-XP-NTP: '+iifs(pronet,'4','3'));
          if fido_to<>'' then
            wrs('F-TO: '+fido_to);
          wrs('');
          end
        else begin                   { M->N }
          wrs(FirstEmpfaenger);
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
  zdate:=RightStr(date,2)+copy(date,4,2)+copy(date,1,2)+
         LeftStr(time,2)+copy(time,4,2);
end;

procedure setzdate(hd: Theader);
begin
  with hd do begin
    if datum='' then datum:=zdate;
    ZtoZCdatumNTZ(datum,zdatum);
    end;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF FPC }
procedure addtoempflist(s:string);
begin
end;
{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF FPC }


function compmimetyp(typ:string):string;
begin
  if LeftStr(typ,12)='application/' then
    compmimetyp:=LowerCase(mid(typ,12))
  else
    compmimetyp:=LowerCase(typ);
end;


{$DEFINE uuzmime }

procedure cerror;
begin
  writeln;
  writeln('Fehler - Konvertierung abgebrochen');
  halt(2);
end;


procedure ZM(pronet:boolean);
var hd    : THeader;
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
  MagicNet:=(LowerCase(netzname)='magicnet');
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
    makeheader(true,f1,0,hds,hd,ok,false, false);
    if ok then with hd do begin
      FirstEmpfaenger := UpperCase(FirstEmpfaenger);
      UpString(absender);
      if (FirstEmpfaenger='SYSTEM@'+real_box) and (betreff='REQUEST') then begin
        if not reqopen then begin
          assign(reqfile,real_box+'.REQ');
          rewrite(reqfile);
          reqopen:=true;
          end;
        seek(f1,adr+hds);
        SetLength(s, groesse);
        blockread(f1,s[1],groesse,rr);
        SetLength(s, rr);
        write(reqfile,s);
        end
      else begin
        pm:=cpos('@', FirstEmpfaenger)>0;
        if not pm then begin
          i:=1;
          while (i<=bretter) and (UpperCase(brettp[i]^.name)<>UpperCase(FirstEmpfaenger)) do
            inc(i);
          berr:=(i>bretter);
          if berr then writeln('unbekanntes Brett:  ', FirstEmpfaenger)
          else Firstempfaenger:=brettp[i]^.code+fido_to;
          end
        else begin
          i:=cpos('@', Firstempfaenger);
          while cPos('#',LeftStr(Firstempfaenger,i))>0 do
            Firstempfaenger[cpos('#', Firstempfaenger)]:='@';    { # -> @ }
          node:=mid(Firstempfaenger,i+1);
          if cpos('.',node)>0 then
            node:=LeftStr(node,cpos('.',node)-1);
        { if (empfaenger='SYSTEM@'+node) then
            empfaenger:='-'+LeftStr(empfaenger,i-1)+sp(16-i)
          else }
            Firstempfaenger:='-'+node+':'+LeftStr(Firstempfaenger,i-1)+sp(16-i);
          berr:=false;
          end;
        if berr then halferror:=true
        else begin
          inc(nn);
          write(#13,'Nachrichten: ',nn);
          alias:=(real_box='');
          wrs(^A);                       { ^A            }
          wrs(Firstempfaenger);               { Empfaenger     }
          i:=cpos('@',absender);
          node:=mid(absender,i+1);
          if cpos('.',node)>0 then
            node:=LeftStr(node,cpos('.',node)-1);
          if real_box='' then real_box:=node;
          if hd_point<>'' then node:=hd_point;
          wrs(node);                     { Absender-Node }
          wrs(forms(LeftStr(absender,i-1),15)+realname);
          if not magicnet or (length(betreff)<=25) then wrs(betreff)  { Betreff }
          else wrs(LeftStr(betreff,24)+'>');
          wrs(^B);                       { Header-Ende   }
          if cpos('[',programm)>0 then
            programm:=trim(LeftStr(programm,cpos('[',programm)-1));
          if pronet then begin
            wrs('$ '+forms(netzname,8)+': '+node+' ['+programm+']');
            wrs('$ Route   : '+pfad);     { Boxname }
            wrs('$ Origin  : '+copy(datum,5,2)+'-'+copy(datum,3,2)+'-'+
                LeftStr(zdatum,2)+LeftStr(datum,2)+' '+
                copy(datum,7,2)+':'+copy(datum,9,2)+':'+copy(zdatum,13,2));
            wrs('$ MsgID   : '+msgid);
            if References.Count <> 0 then wrs('$ Reply-ID: '+ References[0]);
            end
          else begin
            if not alias then
              wrs('$ '+forms(netzname,8)+': '+node+' ['+programm+']')  { Programmname }
            else
              wrs('$ '+forms(netzname,8)+': '+hd_point+' ['+programm+']');
            wrs('$ Route   : '+pfad);      { Route        }
            wrs('$ Origin  : '+copy(datum,5,2)+'-'+copy(datum,3,2)+'-'+
                LeftStr(zdatum,2)+LeftStr(datum,2)+' '+copy(datum,7,2)+':'+copy(datum,9,2));
            if realname<>'' then           { Realname      }
              wrs('$ Realname: '+realname);
            if msgids and (msgid<>'') then  { Message-ID   }
              wrs('$ Msg-ID  : <'+msgid+'>');
            if References.Count <> 0 then
              wrs('$ Reply-ID: <'+ References[0] +'>');
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
var hd    : Theader;
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
    makeheader(zq,f1,0,hds,hd,ok,false, false);
    if ok then with hd do begin
      inc(nn);
      write(#13,'Nachrichten: ',nn);
      p1:=cpos('@', Firstempfaenger);
      pm:=(p1>0);
      if pm then
        if zq then begin
          p2:=cPos('.',mid(Firstempfaenger,p1+1));
          if p2>0 then Firstempfaenger:=LeftStr(Firstempfaenger,p1+p2-1);
          end
        else
      else begin
        if zq and (LeftStr(Firstempfaenger,1)='/') then
          Firstempfaenger:=mid(Firstempfaenger,2);
        if not g_und_s then
          for i:=1 to length(empfaenger) do
            if Firstempfaenger[i]='/' then Firstempfaenger[i]:='\'
            else if Firstempfaenger[i]='\' then Firstempfaenger[i]:='/';
        if not zq and (LeftStr(empfaenger,1)<>'/') then
          Firstempfaenger:='/'+Firstempfaenger;
        end;
      if zq then begin
        wrs(Firstempfaenger);
        wrs(betreff);
        wrs(absender);
        wrs(datum);
        repeat
          p1:=cpos('!',pfad);
          if p1>0 then pfad[p1]:=' ';
        until p1=0;
        if not g_und_s and (pfad<>'') and (RightStr(pfad,1)<>' ') then
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
            pfad:=LeftStr(pfad,p1-1)+'!'+trim(mid(pfad,p1+1));
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
    pmb    : boolean;  { PM-Bestaetigung }
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
  var
    c : char;
  begin
    c:=#0; s := ''; // Hier ist es besser, ein wenig RAM vorher zu allocieren
    while (c<>#10) and ((bufpos<rr) or (size>0)) do
    begin
      c:=getchar;
      if (c>=' ') or (c=#9) then
        s := s + c;
    end;
    s := s + #13#10;
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
      gets:=trim(LeftStr(s,p));
      s:=trim(mid(s,p));
    end;

  begin
    new(info);
    MausReadITI(boxname,info,infos);
    _iti:=MausIT_files and not fileexists(boxname+'.iti');
    if _iti then writeln(t,':ITI -1');
    assign(t1,boxname+'.inf');
    reset(t1);
    if ioresult=0 then begin
      today:=RightStr(date,4)+copy(date,4,2)+LeftStr(date,2);
      while not eof(t1) do begin
        readln(t1,s);
        inf:=gets;
        tage:=minmax(ival(gets),0,99);
        if tage>0 then begin
          dat:=gets;
          crc:=ival(gets);
          with _d do begin
            t:=ival(LeftStr(dat,2));
            m:=ival(copy(dat,4,2));
            j:=ival(RightStr(dat,4));
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
    UmlShort:=LeftStr(s,len-n);
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
      writeln(t,':File-Date: ',LeftStr(hd.ddatum,14));
    writeln(t,':');
    writeln(t,':Diese Nachricht beinhaltet eine uu-codierte Bin�rdatei. Verwenden Sie');
    writeln(t,':das Programm uudecode zum Decodieren.');
    writeln(t,':');
    writeln(t,':This message contains an uu-encoded binary file. Use uudecode to obtain');
    writeln(t,':the original file.');
    writeln(t,':');
    writeln(t,':table');
    writeln(t,':',LeftStr(map,32));
    writeln(t,':',mid(map,33));
    writeln(t,':begin 644 ',iifs(hd.datei<>'',hd.datei,mid(zdate,3)+'.msg'));
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
    writeln(t,':`');
    writeln(t,':end');
    writeln(t,':size ',hd.groesse);
  end;

  function GetBinType(fn:string):string;    { vgl. UUZ.PAS }
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
    ZtoRFCdate:=copy(date,5,2)+' '+month(copy(date,3,2))+LeftStr(zdate,2)+
         LeftStr(date,2)+' '+copy(date,7,2)+':'+copy(date,9,2)+':'+
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
    writeln(t,':---');
  end;

begin
  new(buf);
  reset(f1,1);
  assign(t,outfile);
  rewrite(t);
  adr:=0; nn:=0;
  fs:=filesize(f1);
  umlaute:=['�','�','�','�','�','�','�'];
  writeln(t,'#CMD');
  if MausON then
    writeln(t,':ON');     { oeffentliche Msgs abrufen }
  writeln(t,':PN');       { private Msgs abrufen     }
  if MausOE then
    writeln(t,':OE');     { eigene Nachrichten nicht zuruecksenden }
  if MausPSA then
    writeln(t,':PSN');    { Bearbeitungsstati anfordern }
  if maxmaus>0 then
    writeln(t,':M ',maxmaus);     { maximale Outfile-Groesse vorgeben }
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
          msgid:=LeftStr(msgid,min(10,p1-1));
          end;
        p1:=cpos('@',Firstempfaenger);
        if (p1=0) and
           not stricmp(LeftStr(UpperCase(Firstempfaenger),length(bretth)),bretth) then
          writeln(#13#10,'unbekannte Mausgruppe: ',Firstempfaenger)
        else begin
          inc(nn);
          write(#13,'Nachrichten: ',nn);
          pmb:=false;
          komzu:=(attrib and attrQuoteTo<>0) or
                 (not mkoutfile and (p1>0) and (replypath<>boxname));
            { !!: "Kommentar zu" bei PM-Kommentar an andere Pollbox }
          tomaus:=(ref='') and (UpperCase(LeftStr(Firstempfaenger,5))='MAUS@');
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
                writeln(t,'A',trim(LeftStr(Firstempfaenger,p1-1)),' @ ',
                          trim(mid(Firstempfaenger,p1+1)));
                komzu:=(ref<>'');
                end
              else
            else
              { fb: G-Zeile in AM auf jeden Fall schreiben }
              { if komzu or (ref='') or mkoutfile then }
              { /fb}
                writeln(t,'G',mid(Firstempfaenger,length(bretth)+1));
            if mkoutfile then begin
              p2:=cpos('@',absender);
              if p2<>0 then
                absender:=LeftStr(absender,p2-1)+' @ '+mid(absender,p2+1);
              writeln(t,'V',absender);
              end;
            writeln(t,'E',LeftStr(zdatum,2)+datum);
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
                Maus-Specs. nicht noetig, aber der Kompatiblitaet wegen }
                if LeftStr(UpperCase(ReplyGroup),length(bretth))=bretth then
                {/fb}
                  writeln(t,':Kommentar zu ',ref,' in der Gruppe ',
                            UpperCase(mid(ReplyGroup,length(bretth)+1)));
                end
              else
                writeln(t,'-',ref);
              end;
            { suboptimal }
            if followup.count>0 then
              writeln(t,':Followup-To: ',mid(followup[0],length(bretth)+1));
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
                if (length(s)>2) and (LastChar(s)=#10) and
                   (s[length(s)-1]=#13) and (s[length(s)-2]=' ') then
                   delete(s,length(s)-2,1);
                if (length(s)>2) and (LastChar(s)=#10) and
                   (s[length(s)-1]=#13) and (s[length(s)-2]=' ') then
                   delete(s,length(s)-2,1);
                write(t,fch,s);
                if not pmb then
                  if LastChar(s)=#10 then
                    fch:=':'
                  else
                    fch:='';
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
                                           s  : string;
                                           lf : boolean;
                                         end;
var t1,log     : text;
    f2         : file;
    pmlog,stlog: text;
    hd         : header;
    s          : string;
    tbuf       : tbufa;
    lines,i,nn : longint;
    c          : char;
    pm         : byte;
    b1         : tb;
    firstline  : boolean;
    keinbetreff: boolean;
    killmsg    : boolean;
    info       : MausInfAP;
    infos      : integer;
    UseMIME       : boolean;
    parname    : string[20];
    xline      : array[1..maxxlines] of string;
    xlines     : integer;

  function mausform(s:string):string;
  var p : byte;
  begin
    p:=cpos('@',s);
    if p=0 then
      mausform:=LeftStr(s,79)
    else
      mausform:=LeftStr(trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1)),79);
  end;

  procedure appline(s:string; crlf:boolean);
  begin
    if hd.mimever<>'' then
      s := ISOToIBM(s);
    begin
      inc(lines);
      tbuf[lines].s:=s;
      tbuf[lines].lf:=crlf;
      inc(hd.groesse,length(s)+iif(crlf,2,0));
    end
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
    s := s + #13#10;
    blockwrite(f2,s[1],length(s));
  end;

  procedure TestUUbinary;
  var i : integer;
  begin
    i:=1;
    while (i<=lines) and (pos('begin ',tbuf[i].s)<>1) do inc(i);
    while (i<=lines) and (pos('end',tbuf[i].s)<>1) do inc(i);
    while (i<=lines) and (pos('size ',tbuf[i].s)<>1) do inc(i);
    if i>lines then hd.typ:='T';
  end;

  procedure UUdecode;
  var buf  : charrp;
      bufs : word;
      bufp : word;
      i    : integer;
      n,p  : byte;
      s    : string;
      b1,b2,
      b3,b4: byte;
  begin
    bufs:=65536;
    getmem(buf,bufs);
    bufp:=0;
    i:=1;
    while (i<=lines) and (pos('begin ',tbuf[i].s)<>1) do inc(i);
    s:=tbuf[i].s;
    delete(s,1,6);
    s:=trim(mid(s,blankpos(s)));   { Unix-Filemode wegschneiden }
    if blankpos(s)>0 then truncstr(s,blankpos(s)-1);
    wrs('FILE: '+s);
    if hd.ddatum<>'' then wrs('DDA: '+hd.ddatum+'W+0');
    inc(i);
    { R-}
    while pos('end',tbuf[i].s)=0 do begin
      s:=tbuf[i].s;
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
    while (pos('size ',tbuf[i].s)<>1) do inc(i);
    s:=trim(mid(tbuf[i].s,5));
    bufp:=min(bufp,ival(s));   { echte Groesse }
    wrs('LEN: '+strs(bufp));
    wrs('');
    blockwrite(f2,buf^,bufp);
  end;

  procedure WriteInfofile(fn:string);
  var t : text;
      i : longint;
  begin
    assign(t,fn);
    rewrite(t);
    if ioresult=0 then begin
      for i:=1 to lines do             { Nachrichtentext schreiben }
        if tbuf[i].lf then
          writeln(t,tbuf[i].s)
        else
          write(t,tbuf[i].s);
      close(t);
      end;
  end;

  procedure add_xline(s:string);
  begin
    if xlines<maxxlines then begin
      inc(xlines);
      xline[xlines]:=s;
      end;
  end;

begin
  new(info);
  MausReadITI(boxname,info,infos);
  // !! fsize:=_filesize(infile);
  assign(t1,infile); settextbuf(t1,b1); reset(t1);
  assign(f2,outfile); rewrite(f2,1);
  assign(log,logfile); rewrite(log);
  assign(pmlog,pmlogfile); rewrite(pmlog);
  assign(stlog,stlogfile); rewrite(stlog);
  if not eof(t1) then readln(t1,s);
  nn:=0;
  while not eof(t1) do with hd do
  begin
    fillchar(hd,sizeof(hd),0);
    xlines:=0;
    typ:='T';
    lines:=0;
    keinbetreff:=true;
    while (LeftStr(s,1)<>'#') and not eof(t1) do
      readln(t1,s);
    if not eof(t1) then begin
      pm:=0;
      if UpperCase(s)='#LOG' then
        msgid:=UpperCase(s)
      else
        msgid:=copy(s,2,120);
      firstline:=true;
      UseMIME:=false;
      repeat
        read(t1,s);
        if (s<>'') and (s[1]<>'#') then begin
          c:=s[1];
          delete(s,1,1);
          case c of
            ':' : if UseMIME then
                    if (s='-') or (cpos(':',s)=0) then
                      UseMIME:=false
                    else
                      add_xline(s)
                  else begin
                    appline(s,eoln(t1));
                    if firstline then begin
                      if LeftStr(LowerCase(s),13)='kommentar zu ' then begin
                        ref:=trim(mid(s,14));
                        if cpos(' ',ref)>0 then
                          ref:=LeftStr(ref,cpos(' ',ref)-1);
                        end
                      else if (LeftStr(s,1)='-') and (cpos('@',s)>0) and (ref='')
                      then begin
                        ref:=trim(mid(s,2));
                        if cpos(' ',ref)>0 then
                          ref:=LeftStr(ref,cpos(' ',ref)-1);
                        end
                      else if LeftStr(LowerCase(s),13)='followup-to: ' then
                        followup.add(bretth+trim(mid(s,14)))
                      else if LeftStr(LowerCase(s),10)='reply-to: ' then
                        replyto.add(trim(mid(s,11)))
                      else if LowerCase(s)='content-type: uu1' then
                        typ:='B'
                      else if LowerCase(LeftStr(s,11))='file-date: ' then
                        ddatum:=trim(mid(s,12))
                      else
                        firstline:=false;
                      end;
                    while not eoln(t1) do begin
                      read(t1,s);
                      appline(s,eoln(t1));
                      end;
                  end;
            'A' : if Firstempfaenger='' then Firstempfaenger:=mausform(s);
            'B' : begin pm_bstat:=LeftStr(s,15); inc(pm); end;
            'E' : begin datum:=copy(s,3,10); ZtoZCdatumNTZ(datum,zdatum); end;
            'G' : Firstempfaenger:=bretth+s;
            'O' : organisation:=s;
            'V' : absender:=mausform(s);
            'W' : begin betreff:=s; inc(pm); keinbetreff:=false; end;
            { Name des Gateway aus der dafuer vorgesehenen Header-Zeile nehmen }
            'Y' : gate:=LeftStr(s,80);
            'I' : org_msgid:=LeftStr(s,120);
            'R' : org_xref:=LeftStr(s,120);
            'N' : realname:=LeftStr(s,40);
            'D' : if s<>'' then case s[1] of
                    'M' : distribution:='MausNet';
                    'L' : distribution:='lokal';
                  end;
            'M' : begin
                    mimever:=GetToken(s,';');
                    UseMIME:=true;
                    if LowerCase(GetToken(s,':'))='content-type' then
                      mimect:=s;
                  end;
            { redundante Kommentar-Zeilen aus Outfile tilgen }
            '>' : if (LeftStr(s,9)<>'boundary=') and (not mausKF) then begin
                    parname:=LowerCase(trim(LeftStr(s,cposx(':',s)-1)));
                    if (parname<>'mid') and (parname<>'rid') and
                       (parname<>'mime')
                       then appline(s,true);
                    if parname='gate' then begin
                       if GetToken(s,':')='' then;
                       gate:=s;
                       end;
                  end;
            {/fb}
             '-' : ref:=LeftStr(s,120);
          end;
          s:=c+s;
        end;
        readln(t1);
      until (LeftStr(s,1)='#') or eof(t1);
      if pm=2 then writeln(pmlog,msgid);
      if zdatum='' then begin
        datum:=zdate; setzdate(hd); end;
      if msgid='#LOG' then
        for i:=1 to lines do
          writeln(log,tbuf[i].s)
      else begin
        inc(nn);
        write(#13,'Nachrichten: ',nn);
        killmsg:=false;
        if (msgid<>'') and (cpos('@',msgid)=0) then begin   { Info-File }
          Firstempfaenger:='/�Mausinfo';    { s. auch XP0.MausInfoBrett }
          absender:='MausInfo@'+boxname;
          if msgid='HEAD' then
            killmsg:=true            { internes Infofile loeschen }
          else
            if MausIT_files and (LeftStr(msgid,2)='IT') and (length(MsgID)=3)
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
          wrs('EMP: '+Firstempfaenger);
          if cpos('@',absender)=0 then
            absender:=absender+'@'+boxname;  { fuer lokale Quark-Nachrichten }
          if realname='' then
            wrs('ABS: '+absender)
          else
            wrs('ABS: '+absender+' ('+realname+')');
          wrs('BET: '+betreff);
          wrs('EDA: '+zdatum);
          wrs('ROT: '+reverse(pfad));
          wrs('MID: '+msgid);
          if organisation<>'' then wrs('ORG: '+organisation);
          if ref<>'' then wrs('BEZ: '+ref);
          for i:=0 to replyto.count-1 do
            wrs('ANTWORT-AN: '+replyto[i]);
          for i:=0 to followup.count-1 do
            wrs('DISKUSSION-IN: '+followup[i]);
          if programm<>''  then wrs('Mailer: '+programm);
          if gate<>''      then wrs('Gate: '+gate);
          if distribution<>'' then wrs('U-Distribution: '+distribution);
          if mimever<>'' then wrs('U-MIME-Version: '+mimever);
          if mimect<>'' then wrs('U-Content-Type: '+mimect);
          for i:=1 to xlines do
            wrs('U-'+xline[i]);
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
              if tbuf[i].lf then
                wrs(tbuf[i].s)
              else
                blockwrite(f2,tbuf[i].s[1],length(tbuf[i].s));
            end;
          end;
        end;
      end;
    end;
  dispose(info);
  close(t1);
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
{
  $Log$
  Revision 1.36  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.35  2002/02/21 01:39:04  mk
  - made a litte bit more compilable :)

  Revision 1.34  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.33  2001/08/11 23:06:26  mk
  - changed Pos() to cPos() when possible

  Revision 1.32  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.31  2000/11/24 09:40:11  mk
  - fixed Franks suboptimal changes :(

  Revision 1.30  2000/11/23 22:33:22  fe
  Fixed some ugly bugs with followup and replyto.

  Revision 1.29  2000/11/18 00:04:44  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.28  2000/11/14 22:35:05  fe
  Replaced "exist()" by "fileexists()".

  Revision 1.27  2000/10/17 20:36:49  mk
  - Diskfree/Disksize von Longint auf Int64 umgestellt

  Revision 1.26  2000/10/17 10:05:41  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.25  2000/09/30 16:51:41  fe
  Notduerftig uebersetzbar gemacht.

  Revision 1.24  2000/09/06 21:31:01  fe
  /home/fe/foo

  Revision 1.23  2000/08/08 13:18:13  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.22  2000/07/23 10:00:59  mk
  - memavail wo moeglich rausgenommen

  Revision 1.21  2000/07/14 12:39:07  ma
  - jetzt wieder kompilierbar (Ansistrings)

  Revision 1.20  2000/07/13 10:23:44  mk
  - Zeiger auf Strings entfernt

  Revision 1.19  2000/07/09 08:35:12  mk
  - AnsiStrings Updates

  Revision 1.18  2000/07/04 12:04:17  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.17  2000/07/04 09:59:03  mk
  - Sysutils eingefuegt

  Revision 1.16  2000/06/23 15:59:12  mk
  - 16 Bit Teile entfernt

  Revision 1.15  2000/06/05 16:16:21  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.14  2000/06/04 18:10:28  sv
  - Maggi wieder kompilierbar

  Revision 1.13  2000/06/04 16:57:23  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.12  2000/05/07 19:25:16  mk
  - Anpassung an Pfad als HugeString

  Revision 1.11  2000/05/03 07:33:56  mk
  - unbenutze Variablen/Units rausgeworfen

  Revision 1.10  2000/04/30 21:03:35  mk
  - kein crt noetig

  Revision 1.9  2000/04/29 20:54:06  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.8  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/15 21:06:52  mk
  RB: [40] in [realnlen] geaendert

}

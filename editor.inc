{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Offset der Cursorposition in dl^[scy].absatz; kann grî·er }
{ als die LÑnge des Absatzes sein!                          }

function WorkPos:integer;
begin
  with e^ do
    WorkPos:=dl^[scy].offset+xoffset+scx-1;
end;

function LineLength:integer;
begin
  with dl^[e^.scy] do
    LineLength:=Advance(absatz,offset,e^.rrand)-offset;
end;

function ActAbs:absatzp;
begin
  ActAbs:=dl^[e^.scy].absatz;
end;

procedure GetPosition(var p:position);
begin
  with e^ do begin
    p.absatz:=dl^[scy].absatz;
    p.offset:=dl^[scy].offset+xoffset+scx-1;
    end;
end;

function AbsDelete(var ap:absatzp; from,len:integer; delentry,bkorr:boolean):absatzp; { .robo }
         forward;

procedure TruncAbs(ap:absatzp);
var
  p: word;
begin
  with ap^ do
  begin
    p:=size;
    { KH+MK 01/00 Signaturtrenner nicht erfassen }
    if (p=3) and (cont[0]='-') and (cont[1]='-') and (cont[2]=' ') then
    { Signaturtrenner, nicht anfassen }
    else begin
      while (p>0) and (cont[p-1]=' ') do
        dec(p);
      if p<size then
      begin
        if AbsDelete(ap,p,size-p,false,true)<>nil then; { .robo }
        aufbau:=true;
      end;
    end;
  end;
end;

{ ---------------------------------------------- Blockkorrektur }

procedure CheckBlockOrder;    { e^.blockinverse setzen }
var b1l,b2l : integer;        { vorauss.: disp[x]=2 fÅr mind. ein x }
begin
  with e^ do
    if block[1].disp<block[2].disp then blockinverse:=false
    else if block[1].disp>block[2].disp then blockinverse:=true
    else begin
      b1l:=1;
      while (b1l<=gl) and (dl^[b1l].absatz<>block[1].pos.absatz) do
        inc(b1l);
      b2l:=1;
      while (b2l<=gl) and (dl^[b2l].absatz<>block[2].pos.absatz) do
        inc(b2l);
      if b1l<b2l then blockinverse:=false
      else if b1l>b2l then blockinverse:=true
      else blockinverse:=(block[1].pos.offset>=block[2].pos.offset);
      end;
end;

procedure bskorr(n:byte; newdisp:byte);
var blp : byte;
begin
  with e^ do begin
    blp:=1;
    while (blp<=gl) and (dl^[blp].absatz<>block[n].pos.absatz) do
      inc(blp);
    if blp<=gl then
      block[n].disp:=2
    else
      if block[n].disp=2 then block[n].disp:=newdisp;
    end;
end;

procedure KorrBlockScrolled(up:boolean);   { up = Bild nach oben! }
begin                                  { Korrektur nach Scrolling }
  NoDisplay;
  bskorr(1,iif(up,1,3));
  bskorr(2,iif(up,1,3));
end;

{ Korrektur nach Lîschen innerhalb eines Absatzes }

function BlockAbsCut(oldabs,newabs:absatzp; from,len:integer):boolean;
var nxt,prv : absatzp;
    i       : byte;
begin
  BlockAbsCut:=false;
  with e^ do begin
    nxt:=oldabs^.next;
    prv:=oldabs^.prev;
    for i:=1 to 7 do
      with block[i] do
        if oldabs=pos.absatz then begin
          if assigned(newabs) then pos.absatz:=newabs
          else pos.absatz:=nxt;
          if from<pos.offset then begin
            pos.offset:=max(from,pos.offset-len);
            if (i=1) or (i=2) then BlockAbsCut:=true;
            if from>{=}oldabs^.size{-len} then
              case i of
                1,3..7 : if assigned(nxt) then begin
                           pos.absatz:=nxt; pos.offset:=0; end;
                2      : if assigned(prv) then begin
                           pos.absatz:=prv; pos.offset:=prv^.size; end;
              end;
            end;
          end;
    end;
end;

{ Korrektur nach EinfÅgen innerhalb eines Absatzes }

procedure BlockAbsInsert(oldabs,newabs:absatzp; from,len:integer);
var i : integer;
begin
  with e^ do
    for i:=1 to 7 do
      with block[i] do
        if oldabs=pos.absatz then begin
          pos.absatz:=newabs;
          if from<pos.offset then inc(pos.offset,len);
          end;
end;

{ Korrektur nach Aufspalten eines Absatzes }

procedure BlockAbsSplit(old,new1,new2:absatzp; split,insert_:integer);
var i : integer;
begin
  with e^ do
    for i:=1 to 7 do
      with block[i] do
        if old=pos.absatz then
          if pos.offset<split then
            pos.absatz:=new1
          else begin
            pos.absatz:=new2;
            dec(pos.offset,split);
            inc(pos.offset,insert_);
          end;
end;


{ ---------------------------------------------- Cursor bewegen }

procedure Zeilenanfang;
begin
  with e^ do begin
    scx:=1;
    if xoffset>0 then begin
      xoffset:=0; aufbau:=true;
      end
    end;
end;

procedure Zeilenende;
begin
  with e^ do begin
    scx:=Advance(ActAbs,dl^[scy].offset,rrand)-dl^[scy].offset-xoffset+1;
    if dl^[scy].zeile<alines(ActAbs) then dec(scx);
    if scx>w then begin
      inc(xoffset,scx-w); scx:=w;
      aufbau:=true;
      end
    else if scx<1 then begin
      if LineLength=0 then begin     { End-Taste in Leerzeile in Spalte >80 }
        scx:=1; xoffset:=0;
        end
      else begin
        scx:=2;
        xoffset:=LineLength-1;
        end;
      aufbau:=true;
      end;
    end;
end;

procedure SeiteOben(korrblock:boolean);
var i  : integer;
    ap : absatzp;
begin
  TruncAbs(ActAbs);
  with e^ do
    if (dl^[1].zeile=1) and (dl^[1].absatz^.prev=nil) then
      scy:=1
    else begin
      ap:=dl^[1].absatz;
      i:=gl-dl^[1].zeile;
      while (i>0) and assigned(ap^.prev) do begin
        ap:=ap^.prev;
        dec(i,alines(ap));
        end;
      dec(startline,gl-max(0,i)-1);
      firstpar:=ap;
      firstline:=max(1,1-i);
      if korrblock then KorrBlockScrolled(false);
      aufbau:=true;
      end;
end;

procedure Seitenende; forward;
procedure SeiteUnten;
var i : integer;
begin
  TruncAbs(ActAbs);
  with e^ do begin
    if (dl^[gl].absatz=nil) or
       ((dl^[gl].absatz^.next=nil) and (dl^[gl].zeile=alines(dl^[gl].absatz)))
    then Seitenende  { Wenn Ende des Textes bereits auf dem Bildschirm ist... }
    else begin
      i:=gl;
      while dl^[i].absatz=nil do dec(i);
      inc(startline,i-1);
      firstpar:=dl^[i].absatz;
      firstline:=dl^[i].zeile;
      KorrBlockScrolled(true);
      aufbau:=true;
      end;
    end;
end;

function ScrollUp:boolean;
begin
  with e^ do
    if assigned(dl^[2].absatz) then begin
      if scy=1 then TruncAbs(ActAbs);
      firstpar:=dl^[2].absatz;
      firstline:=dl^[2].zeile;
      inc(startline);
      KorrBlockScrolled(true);
      aufbau:=true;
      ScrollUp:=true;
      end
    else
      ScrollUp:=false;
end;

function ScrollDown:boolean;
var ap : pointer;
begin
  ScrollDown:=true;
  with e^ do begin
    if scy=gl then TruncAbs(ActAbs);
    if dl^[1].zeile>1 then begin
      dec(firstline); dec(startline);
      aufbau:=true;
      end
    else begin
      ap:=dl^[1].absatz^.prev;
      if assigned(ap) then begin
        firstpar:=ap;
        firstline:=alines(ap);
        dec(startline);
        KorrBlockScrolled(false);
        aufbau:=true;
        end
      else
        ScrollDown:=false;
      end;
    end;
end;

function ZeileOben:boolean;
begin
  ZeileOben:=true;
  TruncAbs(ActAbs);
  with e^ do
    if scy>1 then dec(scy)
    else ZeileOben:=ScrollDown;
end;

function ZeileUnten:boolean;
begin
  ZeileUnten:=true;
  with e^ do begin
    if Advance(ActAbs,dl^[scy].offset,rrand)>=ActAbs^.size then
      TruncAbs(ActAbs);    { bei Absatzwechsel Leerzeichen am Ende abschneiden }
    if scy=gl then ZeileUnten:=ScrollUp
    else
      if assigned(dl^[scy+1].absatz) then inc(scy)
      else ZeileUnten:=false;
    end;
end;

procedure Scroll_Up;
begin
  if ScrollUp then
    if e^.scy>1 then if ZeileOben then;
end;

procedure Scroll_Down;
begin
  if ScrollDown then
    if e^.scy<e^.gl then if ZeileUnten then;
end;


function ZeichenLinks:boolean;
begin
  ZeichenLinks:=true;
  with e^ do
    if scx>1 then dec(scx)
    else if xoffset>0 then begin
      dec(xoffset); aufbau:=true; end
    else
      if ZeileOben then Zeilenende
      else ZeichenLinks:=false;
end;

procedure ZeichenRechts(overline:boolean);
var ll,ladd : integer;
begin
  with e^ do begin
    ll:=LineLength;
    ladd:=iif(dl^[scy].offset+ll>=ActAbs^.size,1,0);
    if scx+xoffset<iif(overline,maxabslen+1,LineLength+ladd) then begin
      if scx<w then inc(scx)
      else begin
        inc(xoffset); aufbau:=true; end;
      end
    else if not overline then
      if ZeileUnten then Zeilenanfang;
    end;
end;

procedure CondZeichenRechts;
begin
  with e^ do
    ZeichenRechts(not ActAbs^.umbruch or
                  (dl^[scy].zeile=alines(ActAbs)));
end;

procedure Seitenanfang;
begin
  TruncAbs(ActAbs);
  e^.scy:=1;
end;

procedure Seitenende;
begin
  TruncAbs(ActAbs);
  with e^ do
    while (scy<gl) and assigned(dl^[scy+1].absatz) do
      inc(scy);
end;

procedure Textanfang;
begin
  TruncAbs(ActAbs);
  with e^ do begin
    firstpar:=root;
    firstline:=1;
    startline:=0;
    scy:=1;
    xoffset:=0; scx:=1;
    KorrBlockScrolled(false);
    if block[1].disp<>2 then block[1].disp:=3;
    if block[2].disp<>2 then block[2].disp:=3;
    aufbau:=true;
    end;
end;

procedure Textende;
var ap,p : absatzp;
begin
  TruncAbs(ActAbs);
  with e^ do
    if (dl^[gl].absatz=nil) or
       ((dl^[gl].absatz^.next=nil) and (dl^[gl].zeile=alines(dl^[gl].absatz)))
    then
      Seitenende
    else begin
      ap:=dl^[1].absatz;
      p:=ap^.next;
      while assigned(p) do begin
        inc(startline,alines(ap));
        ap:=p;
        p:=ap^.next;
        end;
      inc(startline,alines(ap)-1);
      firstpar:=ap;
      firstline:=alines(firstpar);
      KorrBlockScrolled(true);
      if block[1].disp<>2 then block[1].disp:=1;
      if block[2].disp<>2 then block[2].disp:=1;
      SeiteOben(true);
      scy:=gl;
      end;
  e^.xoffset:=0;
  NoDisplay;
  Zeilenende;
  aufbau:=true;
end;

function IsWhitespace:boolean;
var wo  : integer;
    abs : absatzp;
begin
  with e^ do begin
    wo:=WorkPos;
    abs:=ActAbs;
    IsWhitespace:=(wo>=abs^.size) or (abs^.cont[wo] in trennzeich);
    end;
end;

procedure WortLinks;
  function LStop:boolean;
  begin
    with e^ do
      LStop:=(scx+xoffset+scy+startline=2);
  end;
begin
  if ZeichenLinks then begin
    while IsWhitespace and not LStop do begin
      if ZeichenLinks then; if aufbau then NoDisplay; end;
    while not IsWhitespace and not LStop do begin
      if ZeichenLinks then; if aufbau then NoDisplay; end;
    if IsWhitespace then ZeichenRechts(false);
    end;
end;

procedure WortRechts;
  function RStop:boolean;
  begin
    with e^ do
      RStop:=(ActAbs^.next=nil) and (workpos>=ActAbs^.size);
  end;
begin
  while not IsWhitespace and not RStop do begin
    ZeichenRechts(false); if aufbau then NoDisplay; end;
  while IsWhitespace and not RStop do begin
    ZeichenRechts(false); if aufbau then NoDisplay; end;
end;

{ dir: 0=beide, -1=rÅckwÑrts, 1=vorwÑrts }
procedure GotoPos(p:position; dir:shortint);    { beliebige Position anspringen }
var p0      : position;
    i,n,add : integer;
    ap      : absatzp;
    l       : longint;
    b1,b2   : absatzp;
    touch1  : boolean;   { erste Blockmarkierung gesehen ... }
    touch2  : boolean;   { zweite Blockmarkierung gesehen    }
begin
  if p.absatz=nil then exit;
  TruncAbs(ActAbs);
  NoDisplay;
  with e^ do begin
    GetPosition(p0);
    i:=1;
    while (i<=gl) and (dl^[i].absatz<>p.absatz) do inc(i);
    if i<=gl then begin       { Absatz ist noch auf Bildschirm }
      if i=1 then
        while dl^[1].offset>p.offset do begin
          if ScrollDown then; NoDisplay;
          end;
      n:=dl^[i].zeile;
      while (n<alines(dl^[i].absatz)) and
            (Advance(dl^[i].absatz,dl^[i].offset,rrand)<=p.offset) do begin
        if i<gl then inc(i)
        else begin if ScrollUp then; NoDisplay; end;
        inc(n);
        end;
      scy:=i;
      end
    else begin
      if dir=1 then
        ap:=nil
      else begin
        b1:=block[1].pos.absatz;
        b2:=block[2].pos.absatz;
        ap:=dl^[1].absatz; l:=dl^[1].zeile-1;    { Abs. rÅckwÑrts suchen }
        touch1:=false; touch2:=false;
        while (ap<>p.absatz) and assigned(ap) do begin
          ap:=ap^.prev;
          if ap=b1 then touch1:=true;
          if ap=b2 then touch2:=true;
          if assigned(ap) then inc(l,alines(ap));
          end;
        end;
      if assigned(ap) then begin
        firstpar:=ap; firstline:=alines(ap); scy:=1;
        dec(startline,l);
        if touch1 then block[1].disp:=2;
        if touch2 then block[2].disp:=2;
        KorrBlockScrolled(false);
        NoDisplay;
        end
      else if (dl^[gl].absatz=nil) or (dl^[gl].absatz^.next=nil) then
        exit
      else begin                                  { .. vorwÑrts suchen }
        if dir=-1 then exit;
        l:=alines(dl^[gl].absatz)-dl^[gl].zeile;
        ap:=dl^[gl].absatz^.next;
        touch1:=false; touch2:=false;
        while (ap<>p.absatz) and assigned(ap) do begin
          inc(l,alines(ap));
          ap:=ap^.next;
          if ap=b1 then touch1:=true;
          if ap=b2 then touch2:=true;
          end;
        if ap=nil then exit;
        i:=Advance(ap,0,rrand); n:=1; add:=1;
        while (i<=p.offset) and (add<>0) do begin    { Zeile suchen }
          inc(l);
          add:=Advance(ap,i,rrand)-i;
          if add<>0 then inc(n);
          inc(i,add);
          end;
        inc(startline,l+gl-1);
        dl^[1].absatz:=ap; dl^[1].zeile:=n;
        SeiteOben(false);
        if touch1 then block[1].disp:=2;
        if touch2 then block[2].disp:=2;
        KorrBlockScrolled(true);
        NoDisplay;
        scy:=gl;
        end;
      end;
    scx:=p.offset-dl^[scy].offset+1-xoffset;
    if scx>w then begin
      xoffset:=scx-w; scx:=w; end
    else if scx<1 then begin
      inc(xoffset,(scx-1));
      scx:=1;
      end;
    lastpos:=p0;
    aufbau:=true;
    end;
end;

procedure SetMarker(n:byte);
begin
  GetPosition(e^.block[n+2].pos);
end;

procedure GotoMarker(n:byte);
begin
  GotoPos(e^.block[n+2].pos,0);
end;


{ -------------------------------------------------- Schalter }

procedure SetAbsatzmarke;
begin
  with e^ do
    if absatzende=' ' then absatzende:=Config.absatzendezeichen
    else absatzende:=' ';
  aufbau:=true;
end;


function InBlock:boolean; forward;

procedure UmbruchEin;
var ap    : absatzp;
    fpmet : boolean;
    wpos  : position;

  procedure uein;
  begin
    if not ap^.umbruch then begin
      if ap=e^.firstpar then fpmet:=true;
      ap^.umbruch:=true;
      if not fpmet then
        inc(e^.startline,alines(ap)-1);
      end;
  end;

begin
  with e^ do begin
    wpos.absatz:=ActAbs;
    wpos.offset:=min(workpos,ActAbs^.size);
    if blockinverse or blockhidden or not InBlock then
      ActAbs^.umbruch:=true
    else begin
      fpmet:=false;
      ap:=block[1].pos.absatz;
      uein;
      while ap<>block[2].pos.absatz do begin
        ap:=ap^.next;
        uein;
        end;
      end;
    NoDisplay;
    while (scy<=gl) and (dl^[scy].absatz<>wpos.absatz) do inc(scy);
    if dl^[scy].absatz<>wpos.absatz then
       GotoPos(wpos,0)
    else
      while (scy<gl) and (Advance(wpos.absatz,dl^[scy].offset,rrand)<wpos.offset) do
        inc(scy);
    end;
  Zeilenanfang;
  KorrBlockScrolled(false);
  aufbau:=true;
end;


procedure UmbruchAus;
var ap,da : absatzp;
    fpmet : boolean;
    ulines: integer;

  procedure uoff(ap:absatzp);
  begin
    if ap^.umbruch then begin
      ulines:=alines(ap)-1;
      ap^.umbruch:=false;
      end
    else
      ulines:=0;
    with e^ do
      if ap=firstpar then begin
        dec(startline,dl^[1].zeile-1);
        firstline:=1;
        fpmet:=true;
        end;
  end;

begin
  with e^ do
    if blockinverse or blockhidden or not InBlock then
      uoff(ActAbs)
    else begin
      fpmet:=false;
      da:=ActAbs;
      ap:=block[1].pos.absatz;
      uoff(ap);
      while ap<>block[2].pos.absatz do begin
        if not fpmet then dec(startline,ulines);
        ap:=ap^.next;
        uoff(ap);
        end;
      NoDisplay;
      while (scy>1) and (ActAbs<>da) do dec(scy);
      end;
  KorrBlockScrolled(true);
  aufbau:=true;
end;


{ ---------------------------------------------- Text editieren }

function EndSpaces:integer;    { Abstand zwischen Absatzende + Cursor }
begin
  EndSpaces:=max(0,workpos-dl^[e^.scy].absatz^.size);
end;

procedure copyflags(abs1,abs2:absatzp);
begin
  abs2^.umbruch:=abs1^.umbruch;
end;

procedure CorrectWorkpos;      { Position innerhalb Umbruchabsatz korr. }
begin
  with e^ do
    if (EndSpaces=0) and (scx+xoffset>LineLength) then
      ZeilenEnde;
end;

procedure absatzwechsel(old,anew:absatzp; setpointer:boolean);
begin
  with e^ do begin
    if setpointer then begin
      if assigned(old^.prev) then begin
        anew^.prev:=old^.prev;
        old^.prev^.next:=anew;
        end;
      if assigned(old^.next) then begin
        anew^.next:=old^.next;
        old^.next^.prev:=anew;
        end;
      end;
    if root=old then root:=anew;
    if firstpar=old then firstpar:=anew;
    end;
end;

function createacopy(ap1:absatzp; var ap2:absatzp; from,len:integer):boolean;
begin
  ap2:=AllocAbsatz(len);
  if ap2=nil then
    createacopy:=false
  else begin
    copyflags(ap1,ap2);
    Move(ap1^.cont[from],ap2^.cont,len);
    createacopy:=true;
    end;
end;

function AbsDelete(var ap:absatzp; from,len:integer; delentry,bkorr:boolean):absatzp; { .robo }
var apnew : absatzp;
begin
  AbsDelete:=ap;
  if len>0 then begin
    if delentry and (memavail-len-asize-16>minfree) then begin
      apnew:=AllocAbsatz(len);
      Move(ap^.cont[from],apnew^.cont,len);
      AddDelEntry(apnew);
    end;
    if ap^.size-len>=ap^.msize-15 then begin
      Move(ap^.cont[from+len],ap^.cont[from],ap^.size-from-len);
      dec(ap^.size,len);
      if blockabscut(ap,ap,from,len) then
        CheckBlockOrder;
    end
    else begin
      apnew:=AllocAbsatz(ap^.size-len);
      if assigned(apnew) then begin
        copyflags(ap,apnew);
        absatzwechsel(ap,apnew,true);
        Move(ap^.cont[0],apnew^.cont[0],from);
        Move(ap^.cont[from+len],apnew^.cont[from],ap^.size-from-len);
        if blockabscut(ap,apnew,from,len) then begin
          NoDisplay;
          CheckBlockOrder;
        end;
        freeabsatz(ap);
        AbsDelete:=apnew;
      end;
    end;
    if bkorr then KorrBlockScrolled(true);
    e^.modified:=true;
  end;
end;

procedure moveworkpos(newwp:integer; wpa:absatzp);
begin
  NoDisplay;
  if assigned(wpa) then begin
    if ActAbs<>wpa then TruncAbs(ActAbs);
    while (ActAbs<>wpa) and ZeichenLinks do
      if aufbau then Display;
    end;
  with e^ do
    if (scx+xoffset>linelength) and (linelength>1) then begin
      while (dl^[scy].offset>=ActAbs^.size) and ZeileOben do NoDisplay;
      ZeilenEnde;
      end;
  while workpos>newwp do
    if ZeichenLinks then if aufbau then Display;
  while workpos<newwp do begin
    CondZeichenRechts;
    if aufbau then Display;
    end;
end;

{ 10.02.2000 robo - aus xp3ex.pas entnommen und leicht verÑndert }
function GetQuote(ap:absatzp):string;
var p,q : byte;
    s   : string;
begin
  p:=min(ap^.size,80);
  SetLength(s,p);
  Move(ap^.cont,s[1],p);
  {s[0]:=char(p);}
  p:=cpos('>',s);
  if p>5 then p:=0
  else if p>0 then begin
    repeat        { korrektes Ende des (mehrfach-?)Quotezeichens }
      q:=p+1;     { ermitteln                                    }
      while (q<=length(s)) and (q-p<=4) and (s[q]<>'>') do
        inc(q);
      if (q<=length(s)) and (s[q]='>') then p:=q;
    until q>p;
    while (p<length(s)) and (s[p+1]='>') do inc(p);
    while (p<length(s)) and (s[p+1]=' ') do inc(p);
  end;
  s:=left(s,p);
  GetQuote:=s;
end;
{ /robo }

procedure NewLine;              { Enter - Absatz einfÅgen }
var ap,ap2   : absatzp;
    wp       : integer;
    copysize : integer;
    delsp    : integer;
    spaces   : integer;
    quotes   : integer;  { Anzahl Quotezeichen }
    quotec   : string;   { Quotezeichen }

  procedure su(ap:absatzp);    { Umbruch einstellen }
  begin
    with e^ do
      if ap^.size<rrand-10 then
        if na_umbruch=0 then ap^.umbruch:=false
        else if na_umbruch=2 then ap^.umbruch:=true;
  end;

begin
  with e^ do
    if insertmode then begin
      ap:=ActAbs;
      wp:=workpos;
      copysize:=max(0,ap^.size-wp);
      spaces:=0;
      delsp:=0;
      { Quote-Reflow }
      if config.QuoteReflow xor kb_shift then begin
        quotec:=GetQuote(ap);
        if quotec<>'' then begin
          delsp:=wp;
          while (delsp<ap^.size) and (ap^.cont[delsp]=' ') do inc(delsp);
          dec(delsp,wp);
        end;
      end
      else quotec:='';
      quotes:=length(quotec);
      if wp>=ap^.size
        then quotes:=0; { kein Reflow bei Zeilenende }
      if quotes>wp then quotes:=0;
      if config.autoindent and (quotes=0) then begin
        while (spaces<ap^.size) and (ap^.cont[spaces]=' ') do
          inc(spaces);
        if spaces>=wp then spaces:=0;
      end;
      if createacopy(ap,ap2,wp-spaces-quotes,copysize+spaces+quotes)
      then begin
        if spaces>0 then
          fillchar(ap2^.cont,spaces,' ')
        else if quotes>0 then
          Move(quotec[1],ap2^.cont[0],quotes);
        if assigned(ap^.next) then
          ap^.next^.prev:=ap2;
        ap2^.next:=ap^.next;
        ap^.next:=ap2;
        ap2^.prev:=ap;
        blockabssplit(ap,ap,ap2,wp,spaces+quotes-delsp);
        ap:=AbsDelete(ap,wp,copysize,false,false);
        su(ap);
        su(ap2);
        KorrBlockScrolled(false);
        if ActAbs<>ap2 then if ZeileUnten then;
        Zeilenanfang;
        moveworkpos(spaces,actabs);           { JG: 27.04.00 Cursor an Zeilenanfang }
        {moveworkpos(spaces+quotes,actabs);}  { und nicht ans Ende der Quotezeichen }
        modified:=true;
        aufbau:=true;
        end;
      end
    else begin
      Zeilenanfang;
      if ZeileUnten then;
      end;
end;

procedure DELchar;              { DEL - Zeichen lîschen }
var ap,ap2,apnew : absatzp;
    ss,size1,wp  : word;
    addspaces    : integer;
    wpa          : absatzp;
    quotes          : integer; { Anzahl Quotezeichen fuer Quote-Reflow }
    quotec1,quotec2 : string;  { Quotezeichen fuer Quote-Reflow }
begin
  with e^ do begin
    ap:=ActAbs;
    wp:=workpos;
    wpa:=ap;
    if wp<ap^.size then
      wpa:=AbsDelete(ap,workpos,1,false,true)
    else
      if ap^.next=nil then    { Textende }
        errsound
      else begin
        addspaces:=EndSpaces;
        if (addspaces=0) and (ap^.size>0) then addspaces:=1;
        ss:=ap^.size + ap^.next^.size + addspaces;
        if ss>maxabslen then
          error(2)    { 'Absatz zu gro·' }
        else begin   { AbsÑtze zusammenhÑngen }
          { Quote-Reflow }
          if config.QuoteReflow then begin
            quotec1:=getquote(ap);
            quotec2:=getquote(ap^.next);
            quotes:=length(quotec1);
            if (quotes>0) and (trim(quotec1)=trim(quotec2)) then begin
              ap2:=ap^.next;
              wpa:=AbsDelete(ap2,0,length(quotec2),false,true);
              dec(ss,length(quotec2));
            end;
          end;
          { /robo }
          apnew:=AllocAbsatz(ss);
          wpa:=apnew;
          copyflags(ap,apnew);
          ap2:=ap^.next;
          blockabsinsert(ap,apnew,maxint,maxint);
          blockabsinsert(ap2,apnew,0,ap^.size+addspaces);
          if assigned(ap^.prev) then begin
            apnew^.prev:=ap^.prev;
            ap^.prev^.next:=apnew;
            end;
          if assigned(ap2^.next) then begin
            apnew^.next:=ap2^.next;
            ap2^.next^.prev:=apnew;
            end;
          size1:=ap^.size;
          Move(ap^.cont,apnew^.cont,size1);
          fillchar(apnew^.cont[size1],addspaces,32);
          Move(ap2^.cont,apnew^.cont[size1+addspaces],ap2^.size);
          absatzwechsel(ap,apnew,false);
          FreeAbsatz(ap);
          FreeAbsatz(ap2);
          KorrBlockScrolled(true);
          modified:=true;
          end;
        end;
    MoveWorkpos(wp,wpa);
    aufbau:=true;
    end;
end;

procedure BackSpace;            { Backspace - Zeichen lîschen }
begin
  if EndSpaces>0 then
    if Zeichenlinks then else
  else
    if ZeichenLinks then begin
      if aufbau then NoDisplay;
      DELchar;
      end;
end;

function ADWhitespace(wpnew:integer):boolean;
begin
  ADWhitespace:=(dl^[e^.scy].absatz^.cont[wpnew] in [' ',#9]);
end;

procedure WortRechtsLoeschen;   { Wort rechts lîschen }
var wp,wpnew,size : integer;
    wpa,ap        : absatzp; { .robo }
begin
  size:=ActAbs^.size;
  if workpos>=size then
    DELchar
  else with e^ do begin
    wpa:=ActAbs;
    wp:=workpos;
    wpnew:=workpos;
    while (wpnew<size) and not (ActAbs^.cont[wpnew] in TrennZeich) do
      inc(wpnew);
    while (wpnew<size) and ADWhitespace(wpnew) do inc(wpnew);
    if wpnew=workpos then inc(wpnew);
    ap:=ActAbs; { .robo }
    wpa:=AbsDelete(ap,wp,wpnew-wp,true,true); { .robo }
    MoveWorkpos(wp,wpa);
    aufbau:=true;
  end;
end;

procedure WortLinksLoeschen;    { Wort links lîschen }
var wp,wpnew{,size }: word;
    wpa,ap        : absatzp; { .robo }
begin
  if workpos=0 then
    BackSpace
  else if workpos>ActAbs^.size then
    zeilenende
  else with e^ do begin
    wpa:=ActAbs;
{    size:=ActAbs^.size;    MK 06.02.00 Zuweisung unnîtig  }
    wp:=workpos;
    wpnew:=workpos;
    while (wpnew>0) and not (ActAbs^.cont[wpnew-1] in trennzeich) do dec(wpnew);
    while (wpnew>0) and ADWhiteSpace(wpnew-1) do dec(wpnew);
    if wpnew=workpos then dec(wpnew);
    ap:=ActAbs; { .robo }
    wpa:=AbsDelete(ap,wpnew,wp-wpnew,true,true); { .robo }
    MoveWorkPos(wpnew,wpa);
    aufbau:=true;
  end;
end;

procedure ZeileLoeschen;        { akt. Zeile lîschen }
var apd,ap : absatzp; { .robo }
    bc     : boolean;
begin
  with e^ do                         { 1. Fall: letzte Zeile im Text }
    if (ActAbs^.next=nil) and (dl^[scy].zeile=alines(ActAbs)) then begin
      ap:=ActAbs; { .robo }
      AbsDelete(ap,dl^[scy].offset,LineLength,true,true); { .robo }
    end
    else if alines(ActAbs)=1 then begin   { 2. Fall: kompletten Absatz lîschen }
      absatzwechsel(ActAbs,ActAbs^.next,false);
      bc:=BlockAbsCut(ActAbs,nil,0,ActAbs^.size);
      if assigned(ActAbs^.prev) then
        ActAbs^.prev^.next:=ActAbs^.next;
      ActAbs^.next^.prev:=ActAbs^.prev;
      if memavail-asize-16>minfree then begin    { Dellisten-Eintrag }
        apd:=AllocAbsatz(0);
        copyflags(ActAbs,apd);
        ActAbs^.next:=apd; ActAbs^.prev:=nil;  { leeren Absatz anhÑngen }
        apd^.prev:=ActAbs;
        AddDelEntry(ActAbs);
      end
      else begin
        ap:=ActAbs; { .robo }
        FreeAbsatz(ap); { .robo }
      end;
      KorrBlockScrolled(true);
      if bc then CheckBlockOrder;
      modified:=true;
    end
    else begin                     { 3. Fall: Zeile aus Absatz lîschen }
      ap:=ActAbs; { .robo }
      AbsDelete(ap,dl^[scy].offset,LineLength,true,true); { .robo }
    end;
  aufbau:=true;
end;

procedure AbsatzRechtsLoeschen;
var
  wp,size : integer;
  ap      : absatzp; { .robo }
begin
  size:=ActAbs^.size;
  if workpos<size then with e^ do begin
    wp:=workpos;
    ap:=ActAbs; { .robo }
    AbsDelete(ap,wp,size-wp,true,true); { .robo }
    aufbau:=true;
  end;
end;


procedure Juppheidi(ap:absatzp; wp:integer);   { Yuppie -> Oops! }
var s : string[10];
begin
  if wp>=6 then begin
    s[0]:=#6;
    Move(ap^.cont[wp-6],s[1],6);
    if s='Yuppie' then begin
      s:='Oops!';
      Move(s[1],ap^.cont[wp-6],5);
      if ap^.size>wp then
        Move(ap^.cont[wp],ap^.cont[wp-1],ap^.size-wp);
      dec(ap^.size);
      MoveWorkpos(wp-1,nil);
      end;
    end;
end;

procedure ZeichenEinfuegen(fast:boolean);     { Texteingabe }
const u1 : string[7] = 'ÑîÅéôö·';
      u2 = 'aouAOUs';
      u3 = 'eeeeeess';
var ap     : absatzp;
    wp     : integer;
    spaces : integer;
    apnew  : absatzp;
    p      : byte;
begin
  with e^ do begin
    p:=cpos(t[1],u1);
    if ukonv and (p>0) then begin
      t:=copy(u2,p,1);
      ZeichenEinfuegen(fast);
      t:=copy(u3,p,1);
      end;
    ap:=ActAbs;
    wp:=workpos;
    if not insertmode and (wp<ap^.size) then begin   { Overwrite }
      ap^.cont[wp]:=t[1];
      MoveWorkpos(wp+1,nil);
      end
    else begin                                            { Insert }
      spaces:=EndSpaces;
      if (spaces=0) and (ap^.msize>ap^.size) then begin
        blockabsinsert(ap,ap,wp,1);
        Move(ap^.cont[wp],ap^.cont[wp+1],ap^.size-wp); { noch Platz da... }
        ap^.cont[wp]:=t[1];
        inc(ap^.size);
        MoveWorkpos(wp+1,nil);
        if pointswitch and (t[1]='e') then Juppheidi(ap,wp+1);
        end
      else
          if ap^.size+spaces>=maxabslen then
            error(2)       { 'Absatz zu gro·' }
          else begin
            apnew:=AllocAbsatz(ap^.size+spaces+1);
            copyflags(ap,apnew);
            absatzwechsel(ap,apnew,true);
            blockabsinsert(ap,apnew,wp,1);
            Move(ap^.cont,apnew^.cont,wp-spaces);
            fillchar(apnew^.cont[wp-spaces],spaces,32);
            apnew^.cont[wp]:=t[1];
            if ap^.size>wp then
              Move(ap^.cont[wp],apnew^.cont[wp+1],ap^.size-wp);
            FreeAbsatz(ap);
            MoveWorkpos(wp+1,nil);
            if pointswitch and (t[1]='e') then Juppheidi(apnew,wp+1);
            end;
      if not fast then KorrBlockScrolled(false);
      end;
    modified:=true;
    aufbau:=true;
    end;
end;

procedure Steuerzeichen;        { ^P - Steuerzeicheneingabe }
begin
  t:=GetPrefixChar('P',false);
  ZeichenEinfuegen(false);
end;

procedure Insert(var blk:absatzp; var endpos:position); forward; { .robo }

procedure tabulator;
var n,i   : byte;
    ap    : absatzp;
    dummy : position;
    wp    : integer;
begin
  n:=8 - workpos mod 8;
  if not e^.insertmode then begin
    for i:=1 to n do CondZeichenRechts;
    end
  else begin
    wp:=workpos;
    ap:=AllocAbsatz(n);
    fillchar(ap^.cont,n,32);
    Insert(ap,dummy);
    MoveWorkpos(wp+n,nil);
  end;
end;

procedure Paragraph;
begin
  t:=^U;
  ZeichenEinfuegen(false);
end;


procedure ModiBlock(mproc:modiproc);
var ap,ap1,ap2 : absatzp;
    ofs1,ofs2  : integer;
begin
  with e^ do begin
    ap1:=block[1].pos.absatz;
    ap2:=block[2].pos.absatz;
    ofs1:=block[1].pos.offset;
    ofs2:=block[2].pos.offset;
    if ap1=ap2 then
      mproc(ap1^.cont[ofs1],ofs2-ofs1)
    else begin
      mproc(ap1^.cont[ofs1],ap1^.size-ofs1);
      ap:=ap1^.next;
      while assigned(ap) and (ap<>ap2) do begin
        mproc(ap^.cont,ap^.size);
        ap:=ap^.next;
        end;
      if assigned(ap) then
        mproc(ap2^.cont,ofs2);
      end;
    modified:=true;
    aufbau:=true;
    end;
end;

procedure BlockRot13;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else
      ModiBlock(Rot13);
end;

procedure CaseWechseln;
begin
  with e^ do
    if blockhidden or blockinverse then
      if workpos<ActAbs^.size then begin
        FlipCase(ActAbs^.cont[workpos],1);
        modified:=true;
        aufbau:=true;
        end
      else
        ErrSound
    else
      ModiBlock(FlipCase);
end;


{ -------------------------------------------- Blockbearbeitung }

procedure SetBlock(n:byte; abs:absatzp; ofs:integer; ndisp:byte);
begin
  with e^.block[n] do begin
    pos.absatz:=abs;
    pos.offset:=min(ofs,abs^.size);
    disp:=ndisp;
    end;
  CheckBlockOrder;
  e^.blockhidden:=false;
  aufbau:=true;
end;

procedure SetBlockmark(n:byte);
begin
  with e^ do
    SetBlock(n,ActAbs,workpos,2);
end;

procedure WortMarkieren;
var wp,sp,sp0 : integer;
  function IsTrennz(p:integer):boolean;
  begin
    IsTrennz:=(ActAbs^.cont[p] in trennzeich);
  end;
begin
  with e^ do begin
    wp:=workpos; sp:=-1;
    if (wp<ActAbs^.size) and not IsTrennz(wp) then
      sp:=wp
    else if (wp>0) and (wp<=ActAbs^.size) and not IsTrennz(wp-1) then
      sp:=wp-1;
    if sp>-1 then begin
      sp0:=sp;
      while (sp0>0) and not IsTrennz(sp0-1) do dec(sp0);
      while (sp<ActAbs^.size) and not IsTrennz(sp) do inc(sp);
      SetBlock(1,ActAbs,sp0,2);
      KorrBlockScrolled(true);
      SetBlock(2,ActAbs,sp,2);
      KorrBlockScrolled(false);
      end;
    end;
end;


procedure ZeileMarkieren;
begin
  with e^ do begin
    SetBlock(1,ActAbs,dl^[scy].offset,2);
    SetBlock(2,ActAbs,Advance(ActAbs,dl^[scy].offset,rrand),2);
    end;
end;

procedure AbsatzMarkieren;
begin
  with e^ do begin
    SetBlock(1,ActAbs,0,2);
    KorrBlockScrolled(true);
    if ActAbs^.next=nil then
      SetBlock(2,ActAbs,ActAbs^.size,2)
    else
      SetBlock(2,ActAbs^.next,0,2);
    KorrBlockScrolled(false);
    end;
end;

procedure KomplettMarkieren;
var ap : absatzp;
begin
  with e^ do begin
    SetBlock(1,root,0,2);
    ap:=ActAbs;
    while assigned(ap^.next) do ap:=ap^.next;
    SetBlock(2,ap,ap^.size,3);
    KorrBlockScrolled(true);
    aufbau:=true;
    end;
end;

{ Block an Cursorposition einfÅgen; Blockzeiger auf Ende zurÅckliefern }
{ blk^ wird freigegeben!                                               }

procedure Insert(var blk:absatzp; var endpos:position); { .robo }
var ap,ap2,ap3,apn : absatzp;
    ss             : word;
    wp             : integer;
    spaces         : integer;
begin
  endpos.absatz:=nil; { .robo }
  if blk=nil then exit;
  ap:=ActAbs;
  spaces:=EndSpaces;
  wp:=workpos;
  if blk^.next=nil then begin        { Absatzausschnitt einfÅgen }
    ss:=ap^.size+blk^.size+spaces;
    if ss>maxabslen then
      error(2)     { 'Absatz zu gro· }
    else if assigned(blk) { .robo }
    then begin
      ap2:=AllocAbsatz(ss);
      copyflags(ap,ap2);
      Move(ap^.cont,ap2^.cont,wp-spaces);
      fillchar(ap2^.cont[wp-spaces],spaces,32);
      ss:=blk^.size;
      Move(blk^.cont,ap2^.cont[wp],ss);
      if ap^.size>wp then
        Move(ap^.cont[wp],ap2^.cont[wp+ss],ap^.size-wp);
      endpos.absatz:=ap2;
      endpos.offset:=wp+ss;
      absatzwechsel(ap,ap2,true);
      blockabsinsert(ap,ap2,wp,ss);
      FreeAbsatz(ap);
      e^.modified:=true;
    end;
    FreeAbsatz(blk);
  end
  else begin                              { mehrere AbsÑtze einfÅgen }
    apn:=blk;
    while assigned(apn^.next) do      { letzten Absatz in Block suchen }
      apn:=apn^.next;
    if (ap^.size+blk^.size>maxabslen) or
       (apn^.size+ap^.size>maxabslen) then begin
      error(2);     { 'Absatz zu gro· }
      FreeBlock(blk);
    end
    else if assigned(blk) { .robo }
    then begin
      ap2:=AllocAbsatz(blk^.size+wp);        { 1. Teil des ActAbs }
      copyflags(ap,ap2);                          { am Blockanfang einfg. }
      Move(ap^.cont,ap2^.cont,wp-spaces);
      fillchar(ap2^.cont[wp-spaces],spaces,32);
      Move(blk^.cont,ap2^.cont[wp],blk^.size);
      absatzwechsel(blk,ap2,true);
      if assigned(ap^.prev) then begin      { Verkettung mit vorausgeh. }
        ap2^.prev:=ap^.prev;      { Text herstellen           }
        ap2^.prev^.next:=ap2;
      end;
      ss:=ap^.size+spaces-wp;              { Grî·e 2. Absatzteil }
      ap3:=AllocAbsatz(ss+apn^.size);
      copyflags(apn,ap3);
      Move(apn^.cont,ap3^.cont,apn^.size);
      Move(ap^.cont[wp],ap3^.cont[apn^.size],ss);
      absatzwechsel(apn,ap3,true);
      if assigned(ap^.next) then begin      { Verkettung mit nachfolg. }
        ap3^.next:=ap^.next;      { Text herstellen          }
        ap3^.next^.prev:=ap3;
      end;
      blockabssplit(ap,ap2,ap3,wp,0);
      absatzwechsel(ap,ap2,false);
      endpos.absatz:=ap3;
      endpos.offset:=apn^.size;
      FreeAbsatz(apn);
      FreeAbsatz(blk);
      FreeAbsatz(ap);
      e^.modified:=true;
    end
    else
      FreeBlock(blk);
  end;
  KorrBlockScrolled(false);
  aufbau:=true;
end;

procedure InsertWithMark(var ap:absatzp); { .robo }
var pos : position;
begin
  if assigned(ap) then begin
    Insert(ap,pos);
    if assigned(pos.absatz) then begin { .robo }
      SetBlock(1,ActAbs,workpos,2);
      SetBlock(2,pos.absatz,pos.offset,2);
      KorrBlockScrolled(false);
    end;
  end;
end;

function CopyBlock:absatzp;    { Kopie des markierten Blocks anlegen }
var tsize      : longint;
    ap         : absatzp;
    ap0,ap1,ap2: absatzp;
    ss         : integer;
begin
  with e^ do
    if blockinverse then
      CopyBlock:=nil
    else if block[1].pos.absatz=block[2].pos.absatz then begin
      ss:=block[2].pos.offset-block[1].pos.offset;    { Absatzausschnitt }
      ap:=AllocAbsatz(ss);
      if assigned(ap) then begin
        Copyflags(block[1].pos.absatz,ap);
        Move(block[1].pos.absatz^.cont[block[1].pos.offset],
             ap^.cont,ss);
        end;
      CopyBlock:=ap;
      end
    else begin
      tsize:=0;
      ap:=block[1].pos.absatz;
      repeat
        inc(tsize,ap^.size+asize+16);
        ap:=ap^.next;
      until (ap=block[2].pos.absatz) or (ap=nil);
      if assigned(ap) then begin
        ap:=block[1].pos.absatz;
        ss:=ap^.size-block[1].pos.offset;
        ap0:=AllocAbsatz(ss); ap1:=ap0;             { Startabsatz }
        copyflags(ap,ap0);
        Move(ap^.cont[block[1].pos.offset],ap0^.cont,ss);
        ap:=ap^.next;
        while ap<>block[2].pos.absatz do begin      { Body }
          ap2:=AllocAbsatz(ap^.size);
          copyflags(ap,ap2);
          Move(ap^.cont,ap2^.cont,ap^.size);
          ap1^.next:=ap2;
          ap2^.prev:=ap1;
          ap1:=ap2;
          ap:=ap^.next;
          end;
        ap2:=AllocAbsatz(block[2].pos.offset);      { Endabsatz }
        copyflags(ap,ap2);
        Move(ap^.cont,ap2^.cont,block[2].pos.offset);
        ap1^.next:=ap2;
        ap2^.prev:=ap1;
        CopyBlock:=ap0;
        end
      else
        CopyBlock:=nil;
      end;
end;

procedure Undelete;
var ap   : absatzp;
    endp : position;
begin
  ap:=GetDelEntry;
  if ap=nil then
    errsound
  else begin
    Insert(ap,endp);
    if assigned(endp.absatz) then begin { .robo }
      SetBlockMark(1);
      SetBlock(2,endp.absatz,endp.offset,2);
      KorrBlockScrolled(false);
    end;
  end;
end;

function InBlock:boolean;    { workpos innerhalb des mark. Blockes }
var b1,b2 : byte;    { 1=vor workpos, 2=gleich, 3=dahinter }
  function seek(n:byte):byte;
  var i : integer;
  begin
    with e^ do
      if (block[n].disp=1) or (block[n].disp=3) then
        seek:=block[n].disp
      else if block[n].pos.absatz=ActAbs then
        if block[n].pos.offset<workpos then seek:=1
        else if block[n].pos.offset=workpos then seek:=2
        else seek:=3
      else begin
        i:=1;
        while (i<=gl) and (block[n].pos.absatz<>dl^[i].absatz) do inc(i);
        if i<scy then seek:=1
        else seek:=3;
        end;
  end;
begin
  with e^ do
    if blockinverse or blockhidden then
      InBlock:=true
    else begin
      b1:=seek(1);
      b2:=seek(2);
      InBlock:=(b1<=2) and (b2>2);
      end;
end;

procedure RecountStartline;
var ap : absatzp;
begin
  with e^ do begin
    startline:=0; ap:=root;
    while (ap<>dl^[1].absatz) do begin
      inc(startline,alines(ap));
      ap:=ap^.next;
      end;
    inc(startline,firstline-1);
    end;
end;

function CutBlock:absatzp;      { markierten Block ausschneiden }
var ap1,ap2,ap    : absatzp;
    apnew,apl,apn : absatzp;
    ofs1,ofs2     : integer;
    ss,i          : integer;
    inblk,aflag   : boolean;
    wp            : position;
    recount       : boolean;    { startline neu berechnen }

  procedure RestoreWorkpos;
  begin
    KorrBlockScrolled(true);
    with e^ do
      if inblk then
        if block[1].disp=1 then begin
          SeitenAnfang; Zeilenanfang; end
        else
          GotoPos(block[1].pos,0)
      else if assigned(wp.absatz) then
        GotoPos(wp,0);
  end;

begin
  with e^ do begin
    ap1:=block[1].pos.absatz;
    ap2:=block[2].pos.absatz;
    ofs1:=block[1].pos.offset;
    ofs2:=block[2].pos.offset;
    wp.absatz:=nil;
    wp.offset:=workpos;
    inblk:=InBlock;
    recount:=false;
    CutBlock := nil; { MK 12/99 Sicherheitshalber immer ersteinmal nil }
    if blockinverse or blockhidden then begin
      errsound;
      end
    else if ap1=ap2 then begin    { Absatzteil ausschneiden }
      ss:=ofs2-ofs1;
      begin
        recount:=(block[1].disp<2);
        apnew:=AllocAbsatz(ss);
        copyflags(ap1,apnew);
        Move(ap1^.cont[ofs1],apnew^.cont,ss);
        aflag:=(ap1=ActAbs);
        ap:=AbsDelete(ap1,ofs1,ss,false,true);
        if aflag then begin
          wp.absatz:=ap;
          if ofs1<wp.offset then wp.offset:=max(ofs1,wp.offset-ss);
          end
        else wp.absatz:=ActAbs;
        RestoreWorkpos;
        CutBlock:=apnew;
        modified:=true;
        aufbau:=true;
        end;
      end
    else if longint(ofs1)+ap2^.size-ofs2>maxabslen then
      error(2)                         { mehrere AbsÑtze ausschneiden }
    else begin
      recount:=(block[1].disp<2);
      wp.absatz:=ActAbs;
      ss:=ap1^.size-ofs1;
      apnew:=AllocAbsatz(ss);       { Ende von 1. Absatz ausschneiden }
      copyflags(ap1,apnew);
      Move(ap1^.cont[ofs1],apnew^.cont,ss);
      if ap1^.next<>ap2 then begin     { dazwischenliegende AbsÑtze }
        apnew^.next:=ap1^.next;   { anhÑngen                   }
        apnew^.next^.prev:=apnew;
        apl:=apnew^.next;
        while apl^.next<>ap2 do apl:=apl^.next;
        end
      else
        apl:=apnew;
      apn:=AllocAbsatz(ofs2);       { Anfang vom letzten Absatz ausschn. }
      copyflags(ap2,apn);
      Move(ap2^.cont,apn^.cont,ofs2);
      apl^.next:=apn;             { .. an Cut-Block anhÑngen }
      apn^.prev:=apl;

      apn:=AllocAbsatz(ofs1+(ap2^.size-ofs2));   { Join ap1+ap2 }
      copyflags(ap1,apn);
      blockabsinsert(ap1,apn,maxint,maxint);
      if blockabscut(ap2,ap2,0,ofs2) then;
      blockabsinsert(ap2,apn,0,ofs1);
      if assigned(ap1^.prev) then begin
        apn^.prev:=ap1^.prev;
        apn^.prev^.next:=apn;
        end;
      if assigned(ap2^.next) then begin
        apn^.next:=ap2^.next;
        apn^.next^.prev:=apn;
        end;
      Move(ap1^.cont,apn^.cont,ofs1);
      Move(ap2^.cont[ofs2],apn^.cont[ofs1],ap2^.size-ofs2);
      absatzwechsel(ap1,apn,false);
      absatzwechsel(ap2,apn,false);
      if wp.absatz=ap2 then begin
        wp.absatz:=apn;
        inc(wp.offset,ofs1-ofs2);
        end;
      firstline:=min(firstline,alines(firstpar));

      block[2]:=block[1];           { markierter Block ist jetzt leer }
      blockinverse:=true;
      ap:=apnew^.next;
      while assigned(ap) do begin       { Pos.-Marker korrigieren }
        absatzwechsel(ap,apn,false);
        for i:=3 to 7 do
          if block[i].pos.absatz=ap then
            block[i]:=block[1];
        ap:=ap^.next;
        end;
      RestoreWorkpos;
      CutBlock:=apnew;
      modified:=true;
      aufbau:=true;
      end;

    if recount then RecountStartline;
    end;
end;

procedure BlockKopieren;     { Ctrl-K-C }
var ap : absatzp; { .robo }
begin
  ap:=CopyBLock; { .robo }
  InsertWithMark(ap); { .robo }
end;

procedure BlockEinAus;
begin
  e^.blockhidden:=not e^.blockhidden;
  aufbau:=true;
end;

procedure UmbruchKomplettEin;
begin
  KomplettMarkieren;
  UmbruchEin;
  BlockEinAus;
end;

procedure UmbruchKomplettAus;
begin
  KomplettMarkieren;
  UmbruchAus;
  BlockEinAus;
end;


procedure BlockLoeschen;
var ap : absatzp;
begin
  ap:=CutBlock;
  if assigned(ap) then AddDelEntry(ap);
end;

procedure RestLoeschen;     { ab Cursorposition bis Textende }
begin
  KomplettMarkieren;
  SetBlockMark(1);
  BlockLoeschen;
end;


procedure BlockVerschieben;
var ap : absatzp;
begin
  if InBlock then
    errsound
  else begin
    ap:=CutBlock;
    if assigned(ap) then InsertWithMark(ap);
    end;
end;


procedure BlockClpKopie(cut:boolean);
var ap : absatzp;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else
      if ClipAvailable then begin
        if SaveBlock(block[1].pos,block[2].pos,EdTempFile,rrand,false,true,false) then
        begin
          FileToClip(EdTempFile);
          { if exist(EdTempFile) then era(EdTempFile); }
          if cut then begin
            ap:=CutBlock; Freeblock(ap); end;
          end;
        end
      else begin
        FreeBlock(Clipboard);
        if cut then Clipboard:=CutBlock
        else Clipboard:=CopyBlock;
        end;
end;


procedure BlockClpEinfuegen;
var ap : absatzp; { .robo }
begin
  with e^ do
    if Clipavailable then begin
      ClipToFile(EdTempFile);
      ap :=LoadBlock(EdTempFile,false,0,rrand); { .robo }
      InsertWithMark(ap); { .robo }
      { if exist(EdTempFile) then era(EdTempFile); }
    end
    else begin
      InsertWithMark(Clipboard);
      Clipboard:=CopyBlock;
    end;
end;


procedure BlockEinlesen;
var fn : string;
    ap : absatzp;
begin
  with e^ do begin
    Procs.FileProc(e,fn,false,false);
    if fn<>'' then
      if not exist(fn) then
        error(5)     { 'Fehler: Datei nicht vorhanden' }
      else begin
        ap:=LoadBlock(fn,false,0,rrand);
        if assigned(ap) then InsertWithMark(ap);
      end;
  end;
end;

procedure BlockUUeEinlesen;
var fn : string;
    ap : absatzp;
begin
  with e^ do begin
    Procs.FileProc(e,fn,false,true);
    if fn<>'' then
      if not exist(fn) then
        error(5)     { 'Fehler: Datei nicht vorhanden' }
      else begin
        ap:=LoadUUeBlock(fn);
        if assigned(ap) then InsertWithMark(ap);
      end;
  end;
end;

procedure BlockSpeichern;
var fn : string;
begin
  with e^ do
    if blockinverse or blockhidden then
      errsound
    else begin
      Procs.FileProc(e,fn,true,false);
      if fn<>'' then begin
        if not exist(fn) then t:='J'
        else t:=Procs.Overwrite(e,fn);
        if (t='J') or (t='N') then
          if SaveBlock(block[1].pos,block[2].pos,fn,rrand,false,t='J',false) then;
      end;
    end;
end;


procedure BlockDrucken;
var ap,endap   : absatzp;
    ofs,endofs : word;
    nofs       : word;
    s          : string;
begin
  with e^ do begin
    attrtxt(col.colstatus);
    mwrt(x,y,forms(language^.drucken,w));
    if blockinverse or blockhidden then begin
      ap:=root; ofs:=0;
      endap:=nil; endofs:=0;
      end
    else begin
      ap:=block[1].pos.absatz;
      ofs:=block[1].pos.offset;
      endap:=block[2].pos.absatz;
      endofs:=block[2].pos.offset;
      end;
    checklst:=true;
    repeat
      nofs:=Advance(ap,ofs,rrand);
      if ap=endap then nofs:=min(nofs,endofs);
      SetLEngth(s, nofs-ofs); {s[0]:=chr(nofs-ofs);}
      Move(ap^.cont[ofs],s[1],length(s));
      writeln(lst,s);
      if nofs=ap^.size then begin
        ap:=ap^.next; ofs:=0; end
      else
        ofs:=nofs;
    until (ap=nil) or ((ap=endap) and (ofs>=endofs));
    writeln(lst);
    end;
end;


{ Block reformatieren }

procedure BlockFormatieren; { by Robo }
var ap,ap2    : absatzp;
    apnew     : absatzp;
    fp        : integer;
    copysize  : integer;
    ss,size1  : word;
    addspaces : integer;
    spaces    : integer;
    quotes    : integer;
    quotec1   : string;
    quotec2   : string;
    saveumbr  : boolean;

  procedure su(ap:absatzp);    { Umbruch einstellen }
  begin
    with e^ do
      if ap^.size<rrand-10 then
        if na_umbruch=0 then ap^.umbruch:=false
        else if na_umbruch=2 then ap^.umbruch:=true;
  end;

begin
  with e^ do begin
    if blockinverse or blockhidden then begin
      { kein Block markiert oder Blockmarkierung ausgeschaltet:
        ActAbs bis Leerzeile formatieren }
      if ActAbs^.size=0 then begin { Leerzeile -> abbrechen }
        errsound;
        exit;
      end;
      SetBlock(1,ActAbs,0,2);
      ap:=ActAbs;
      if config.QuoteReflow and assigned(ap^.next)
        then quotec1:=getquote(ap^.next)
        else quotec1:='';
      while assigned(ap^.next) and (ap^.next^.size>length(quotec1)) do begin
        ap:=ap^.next;
        if config.QuoteReflow and assigned(ap^.next)
          then quotec1:=getquote(ap^.next)
          else quotec1:='';
      end;
      SetBlock(2,ap,ap^.size,3);
    end
    else begin
      { Blockmarkierung auf Absatzanfang und Absatzende ausdehnen }
      with block[1].pos do
        if offset>0 then offset:=0;
      bskorr(1,1);
      with block[2].pos do
        if offset=0
          then SetBlock(2,absatz^.prev,absatz^.prev^.size,block[2].disp)
          else
            if offset<absatz^.size then offset:=absatz^.size;
      bskorr(2,3);
    end;
    { Cursor auf Blockanfang setzen }
    gotopos(block[1].pos,0);
    ap:=ActAbs;
    saveumbr:=ap^.umbruch;
    ap^.umbruch:=true;
    repeat
      fp:=advance(ap,0,rrand);
      if fp<ap^.size then begin  { Absatz umbrechen }
        spaces:=0;
        if config.QuoteReflow
          then quotec1:=GetQuote(ap)
          else quotec1:='';
        quotes:=length(quotec1);
        if quotes>rrand div 2 then begin
          quotec1:=trimright(quotec1);
          quotes:=length(quotec1);
          if quotes>rrand div 2 then quotes:=0;
        end;
        if config.autoindent and (quotes=0) then begin
          while (spaces<ap^.size) and (ap^.cont[spaces]=' ') do
            inc(spaces);
          if spaces>rrand div 2 then spaces:=0;
        end;
        if (fp<=quotes) or (fp<=spaces) then begin { Absatz kann nicht umgebrochen werden }
          if ap=block[1].pos.absatz then ap^.umbruch:=saveumbr;
          errsound;
          break;
        end;
        copysize:=max(0,ap^.size-fp);
        if createacopy(ap,ap2,fp-spaces-quotes,copysize+spaces+quotes)
        then begin
          if spaces>0 then
            fillchar(ap2^.cont,spaces,' ')
          else if quotes>0 then
            Move(quotec1[1],ap2^.cont[0],quotes);
          if assigned(ap^.next) then
            ap^.next^.prev:=ap2;
          ap2^.next:=ap^.next;
          ap^.next:=ap2;
          ap2^.prev:=ap;
          blockabssplit(ap,ap,ap2,fp,spaces+quotes);
          ap:=AbsDelete(ap,fp,copysize,false,false);
          su(ap);
          su(ap2);
          KorrBlockScrolled(false);
          TruncAbs(ap);
          ap:=ap2;
        end
        else begin
          if ap=block[1].pos.absatz then ap^.umbruch:=saveumbr;
          errsound;
          break;
        end;
      end
      else
      if assigned(ap^.next)
      and (ap<>block[2].pos.absatz)
      then begin  { AbsÑtze ggf. zusammenfÅgen }
        addspaces:=1;
        ss:=ap^.size + ap^.next^.size + addspaces;
        if ss>maxabslen then
          error(2)    { 'Absatz zu gro·' }
        else if true then begin
          spaces:=0;
          if config.QuoteReflow then begin
            quotec1:=getquote(ap);
            quotec2:=getquote(ap^.next);
            quotes:=length(quotec1);
          end
          else begin
            quotec1:='';
            quotec2:='';
            quotes:=0;
          end;
          if config.autoindent and (quotes=0) and (length(quotec2)=0) then
          begin
            while (spaces<ap^.next^.size) and (ap^.next^.cont[spaces]=' ') do
              inc(spaces);
            if spaces>rrand div 2 then spaces:=0;
          end;
          TruncAbs(ap);
          TruncAbs(ap^.next);
          if (ap^.size>length(quotec1))
          and (ap^.next^.size>length(quotec2))  { Leerzeile belassen }
          and (trim(quotec1)=trim(quotec2))  { nur bei ident. QuoteCh zusammenf. }
          then begin
            if (quotes>0) and (trim(quotec1)=trim(quotec2)) then begin
              ap2:=ap^.next;
              AbsDelete(ap2,0,length(quotec2),false,true);
              dec(ss,length(quotec2));
            end
            else if spaces>0 then begin
              ap2:=ap^.next;
              AbsDelete(ap2,0,spaces,false,true);
              dec(ss,spaces);
            end;
            apnew:=AllocAbsatz(ss);
            apnew^.umbruch:=true;
            ap2:=ap^.next;
            blockabsinsert(ap,apnew,maxint,maxint);
            blockabsinsert(ap2,apnew,0,ap^.size+addspaces);
            if assigned(ap^.prev) then begin
              apnew^.prev:=ap^.prev;
              ap^.prev^.next:=apnew;
            end;
            if assigned(ap2^.next) then begin
              apnew^.next:=ap2^.next;
              ap2^.next^.prev:=apnew;
            end;
            size1:=ap^.size;
            Move(ap^.cont,apnew^.cont,size1);
            fillchar(apnew^.cont[size1],addspaces,32);
            Move(ap2^.cont,apnew^.cont[size1+addspaces],ap2^.size);
            absatzwechsel(ap,apnew,false);
            FreeAbsatz(ap);
            FreeAbsatz(ap2);
            KorrBlockScrolled(true);
            ap:=apnew;
          end
          else ap:=ap^.next;
        end
        else begin
          if ap=block[1].pos.absatz then ap^.umbruch:=saveumbr;
          errsound;
          break;
        end;
      end
      else ap:=ap^.next;
    until ap=block[2].pos.absatz^.next;
    CheckBlockOrder;
    modified:=true;
    aufbau:=true;
  end;
end;

{ ---------------------------------------------- Suchfunktionen }

procedure Suchen(again,ersetzen:boolean);
const
      txt   : string = '';
      repby : string = '';
      igcase: boolean = true;
      lastop: byte    = 0;         { 1=suchen, 2=suchen/ersetzen }
var
      stxt  : string;
      spos  : integer;
      sofs  : word;
      ap    : absatzp;
      apd   : absatzp; { .robo }
      insap : absatzp;
      pos   : position;
      repall: boolean;
      brk   : boolean;
      t     : taste;
      count : longint;
begin
  if again and (lastop=0) then begin
    errsound;
    exit;
  end;
  if again then ersetzen:=(lastop=2)
  else lastop:=iif(ersetzen,2,1);
  insap:=nil;
  with e^ do
  if again or
     (ersetzen and Procs.ReplFunc(e,txt,repby,igcase)) or
     (not ersetzen and Procs.FindFunc(e,txt,igcase)) then begin
    if txt='' then exit;
    if igcase then stxt:=UpperCase(txt)
    else stxt:=txt;
    brk:=false; repall:=false;
    count:=0;
    repeat
      sofs:=min(workpos+1,ActAbs^.size-dl^[scy].offset);
      ap:=ActAbs;
      repeat
        spos:=SeekStr(ap^.cont[sofs],ap^.size-sofs,stxt,igcase);
        if spos=-1 then begin
          ap:=ap^.next;
          sofs:=0;
        end;
      until (ap=nil) or (spos>=0);
      if ap=nil then
        if count=0 then error(6) else     { 'Text wurde nicht gefunden' }
      else begin
        inc(count);
        pos.absatz:=ap;
        pos.offset:=spos+sofs;
        GotoPos(pos,1);
        if ersetzen then with language^ do begin
          if aufbau then Display;
          if repall then begin
            t:=replacechr[1];
            testbrk(brk);
          end
          else begin
            attrtxt(col.colstatus);
            wrt(x,y,forms(' '+askreplace,w));
            gotoxy(x+scx-1,y+scy);
            repeat
              get(t,curon); t := UpperCase(t);
            until (cpos(t[1],replacechr)>0) or (t=keyesc);
            if t[1]=replacechr[3] then begin
              repall:=true; t:=replacechr[1];
            end;
            brk:=(t=keyesc);
          end;
          if t[1]=replacechr[1] then begin  { ersetzen: Ja }
            insap:=AllocAbsatz(length(repby));  { EinfÅgeabsatz erzeugen }
            if insap=nil then brk:=true
            else begin
              Move(repby[1],insap^.cont,length(repby));
              apd:=ActAbs; { .robo }
              AbsDelete(apd,workpos,length(txt),false,true); { .robo }
              Insert(insap,pos);
              MoveWorkpos(workpos+length(repby),ActAbs);
            end;
          end;
        end
        else
          brk:=true;
      end;
    until (ap=nil) or brk;
    if repall then message:=strs(count)+language^.ersetzt;
  end;
end;


{ -------------------------------------------------------- MenÅ }

function LocalMenu:integer;
const lastp : byte = 1;
var mx,my,ml,i,p : byte;
    highp        : array[1..editmenumps] of byte;
    t            : taste;
    xx,yy,oldp   : integer;
    mausmenu     : boolean;
    lmcurtype    : curtype;

label sok;

  procedure display;
  var i : integer;
  begin
    moff;
    for i:=1 to editmenumps do with e^,language^ do
      if menue[i]<>'-' then begin
        if i=p then attrtxt(col.colmenuinv)
        else attrtxt(col.colmenu);
        wrt(mx+1,my+i,' '+left(menue[i],highp[i]-2));
        if i<>p then
          attrtxt(col.colmenuhi)
        else
          attrtxt(col.colmenuhiinv);
        Wrt2(menue[i,highp[i]]);
        if i<>p then
          attrtxt(col.colmenu)
        else
          attrtxt(col.colmenuinv);
        Wrt2(forms(mid(menue[i],highp[i]+1),ml-highp[i]+1));
      end;
    mon;
    with e^ do
      if EdSelcursor then begin
        gotoxy(mx+1,my+p);
        if p=0 then lmcurtype:=curoff
        else lmcurtype:=curon;
        end
      else gotoxy(x+scx-1,y+scy);
  end;

  procedure MausMenusel;
  var first : boolean;
  begin
    first:=true;
    repeat
      if p<>oldp then display;
      oldp:=p;
      if first then t:=mauslmoved
      else get(t,lmcurtype);
      if (t=mausrmoved) or (t=mauslmoved) then begin
        maus_gettext(xx,yy);
        if (yy<=my) or (yy>my+editmenumps) or (xx<=mx) or (xx>mx+ml+1) then
          p:=0
        else if (language^.menue[yy-my]<>'-') then
          p:=yy-my
        else if p=0 then
          p:=yy-my+1;
        end;
      first:=false;
    until maust=0;
  end;

begin
  with e^,language^ do begin
    ml:=length(menue[1]);
    for i:=1 to editmenumps do begin    { Achtung, ^ wird mitgerechnet }
      ml:=max(ml,length(menue[i]));
      highp[i]:=cpos('^',menue[i])+1;
      end;
    mausmenu:=(maust and 2<>0);
    if mausmenu then begin
      maus_gettext(xx,yy);
      mx:=min(xx,screenwidth-ml-3);
      my:=min(yy,y+h-editmenumps-3);
      end
    else begin
      mx:=min(x+scx-1,screenwidth-ml-3);
      my:=y+min(scy,h-editmenumps-3);
      end;
    attrtxt(col.colmenu);
    forcecolor:=true;
    wpushs(mx,mx+ml+2,my,my+editmenumps+1,'');
    forcecolor:=false;
    mwrt(mx+2,my,' '+menue[0]+' ');
    for i:=1 to editmenumps do
      if menue[i]='-' then wrt(mx,my+i,HBar(ml+3));
    if (scx+x-1<mx) or (scx+x-1>mx+ml+2) or (scy+y<my) or (scy+y>my+editmenumps+1)
    then if insertmode then lmcurtype:=curon
            else lmcurtype:=cureinf
    else lmcurtype:=curoff;
    p:=0; oldp:=-1;

    if mausmenu then
      MausMenusel
    else begin
      p:=lastp;
      if (p<1) or (p>editmenumps) or (menue[p]='-') then p:=1;
      repeat
        if p<>oldp then display;
        oldp:=p; lastp:=p;
        get(t,lmcurtype);
        if (t=mausleft) or (t=mausright) then begin
          MausMenusel;
          goto sok;
          end
        else begin
          if t=keyup then
            if p=1 then p:=editmenumps
            else repeat dec(p) until menue[p]<>'-';
          if t=keydown then
            if p=editmenumps then p:=1
            else repeat inc(p) until menue[p]<>'-';
          if t=keyhome then p:=1;
          if t=keyend then p:=editmenumps;
          if (t=keyesc) or (t=keyf10) then p:=0;
          t := UpperCase(t);
          if (t[1]>='A') and (t[1]<='Z') then
            for i:=1 to editmenumps do
              if t[1]=UpCase(menue[i,highp[i]]) then begin
                p:=i; t:=keycr; end;
          end;
      until (t=keycr) or (p=0);
      if t=keycr then lastp:=p;
      end;

sok:  case p of
        1 : LocalMenu:=EditfCCopyBlock;
        2 : LocalMenu:=EditfCutBlock;
        3 : LocalMenu:=EditfPasteBlock;
        4 : LocalMenu:=EditfReadBlock;
        5 : LocalMenu:=EditfReadUUeBlock;
        6 : LocalMenu:=EditfWriteBlock;
        8 : LocalMenu:=EditfWrapOff;
        9 : LocalMenu:=EditfWrapOn;
       11 : LocalMenu:=EditfSetup;
       { 12 : LocalMenu:=EditfSaveSetup; }
        { /robo }
      else  LocalMenu:=0;
      end;
    wpop;
    end;
end;


procedure EinstellungenSichern;
var t : text;
begin
  assign(t,EdConfigFile);
  rewrite(t);
  with e^.Config do begin
    writeln(t,'RechterRand=',rechter_rand);
    writeln(t,'AbsatzEnde=',absatzendezeichen);
    writeln(t,'AutoIndent=',iifc(AutoIndent,'J','N'));
    { 01/2000 oh }
    writeln(t,'PersistentBlocks=',iifc(PersistentBlocks,'J','N'));
    { /oh }
    { 10.02.2000 robo }
    writeln(t,'QuoteReflow=',iifc(QuoteReflow,'J','N'));
    { /robo }
    end;
  close(t);
end;


procedure Einstellungen;
var brk      : boolean;
    wp,o,nxo : word;
begin
  with e^ do
    if @Procs.CfgFunc=nil then
      errsound
    else begin
      Procs.CfgFunc(Config,brk);
      if not brk then begin
        if absatzende<>' ' then absatzende:=Config.absatzendezeichen;
        if (rrand<>Config.rechter_rand) and (ActAbs=dl^[1].absatz)
           and (firstline>1) then begin
          wp:=workpos;
          firstline:=1; nxo:=0;
          repeat
            o:=nxo;
            nxo:=Advance(ActAbs,o,Config.rechter_rand);
            if (nxo>o) and (nxo<=wp) then begin
              inc(firstline); end;
          until (nxo=o) or (nxo>wp);
          end;
        if rrand<>Config.rechter_rand then
          RecountStartline;
        rrand:=Config.rechter_rand;
        EinstellungenSichern;
        aufbau:=true;
        end;
      end;
end;
{
  $Log$
  Revision 1.27  2000/07/31 22:05:04  mk
  RB:- Quotereflow verfeinert

  Revision 1.26  2000/07/21 21:17:42  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.25  2000/07/14 11:35:41  mk
  - 16 Bit Ueberbleibsel und ungenutze Variablen beseitigt

  Revision 1.24  2000/07/05 17:10:53  mk
  - AnsiString Updates

  Revision 1.23  2000/07/05 09:50:12  hd
  - AnsiString-Anpassung

  Revision 1.22  2000/07/04 12:04:15  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.21  2000/07/03 16:20:02  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.20  2000/07/02 14:24:46  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.19  2000/05/06 17:29:19  mk
  - DOS DPMI32 Portierung

  Revision 1.18  2000/04/27 09:06:55  jg
  - Editor: Pgdn in der letzten Textseite springt ohne scrollen zum Textende
            beim Quote-Reflow wird der Cursor an den Zeilenanfang gesetzt,
            und nicht mehr hinter das Quotezeichen

  - Lister: "/" als Hervorhebezeichen erlaubt.

  Revision 1.17  2000/04/06 20:55:52  rb
  Bugfixes; ich wills schliesslich keinen Murks hinterlassen

  Revision 1.16  2000/03/27 22:21:59  rb
  Bugfix fÅr 'Block reformatieren'

  Revision 1.15  2000/03/21 22:08:20  rb
  kleiner Schînheitsbugfix fÅr 'Block reformatieren', Onlinehilfe angepasst

  Revision 1.14  2000/03/21 21:19:09  rb
  Bugfixes ('Block reformatieren' u. a.), 'Block reformatieren' jetzt auf <Ctrl-B>

  Revision 1.13  2000/03/21 01:01:55  rb
  'Block reformatieren' verbessert, weiterer kleiner Bugfix

  Revision 1.12  2000/03/17 23:10:27  rb
  'Block reformatieren' fertiggestellt

  Revision 1.11  2000/03/17 21:22:10  rb
  vActAbs entfernt, erster Teil von 'Blîcke reformatieren' (<Ctrl K><F>)

  Revision 1.10  2000/03/02 20:51:22  rb
  Wrapper-Funktionen vap und vap2 aus Editor entfernt

  Revision 1.9  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.8  2000/02/18 09:04:56  mk
  RB: * RTE im Editor behoben

  Revision 1.7  2000/02/17 08:34:51  mk
  RB: * es wird kein Reflow mehr gemacht, wenn bei Return nichts in die neue Zeile
        umgebrochen wird

  Revision 1.6  2000/02/15 22:22:04  mk
  RB: * Quote-Reflow jetzt vollstaendig implementiert

  Revision 1.5  2000/02/15 20:43:35  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

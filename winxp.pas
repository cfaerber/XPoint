{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                      UNIT windows                       *)
(*                                                         *)
(*            Window-Verwaltung & Datei-Auswahl            *)
(*                                                         *)
(***********************************************************)

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$F+}
{$ENDIF }

unit winxp;

{  ==========================  Interface-Teil  ==========================  }

INTERFACE

uses  xpglobal, crt,dos,keys,inout,maus2,typeform;

const maxpull = 30;
      maxpush = 20;

      crline  : byte = 25;      { zeile fÅr Alt-F10-Copyright }
      shadowcol: byte = 8;

type  selproc = procedure(var sel:slcttyp);

var   wpstack  : array[1..maxpush] of word;
      wpp      : byte;
      warrows  : boolean;     { Pfeile bei wslct anzeigen }
      warrcol  : byte;        { Farbe fÅr Pfeile          }
      selp     : selproc;


procedure normwin;
procedure clwin(l,r,o,u:word);

procedure rahmen1(li,re,ob,un:byte; txt:string);    { Rahmen ≥ zeichen       }
procedure rahmen2(li,re,ob,un:byte; txt:string);    { Rahmen ∫ zeichnen      }
procedure rahmen3(li,re,ob,un:byte; txt:string);    { Special-Rahmen         }
procedure rahmen1d(li,re,ob,m,un:byte; txt:string); { Doppelrahmen ≥ zeichen }
procedure rahmen2d(li,re,ob,m,un:byte; txt:string); { Doppelrahmen ∫ zeichnen}
procedure explode(l,r,o,u,typ,attr1,attr2:byte; msec:word; txt:string);
procedure wshadow(li,re,ob,un:word);                { 8-Schatten }

procedure setrahmen(n:shortint);                 { Rahmenart fÅr wpull setzen }
function  getrahmen:shortint;
procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
procedure wpull(x1,x2,y1,y2:byte; text:string; var handle:word);
procedure wrest(handle:word);
procedure wslct(anz:integer; ta:pntslcta; handle,pos:word; abs1:boolean;
                var n:word; var brk:boolean);
procedure seldummy(var sel:slcttyp);
procedure wpush(x1,x2,y1,y2:byte; text:string);
procedure wpushs(x1,x2,y1,y2:byte; text:string);
procedure wpop;

procedure fwrt(x,y:word; var s:string);
procedure w_copyrght;


{ ========================= Implementation-Teil =========================  }

IMPLEMENTATION

const rchar : array[1..3,1..6] of char =
              ('⁄ƒø≥¿Ÿ','…Õª∫»º','’Õ∏≥‘æ');

      shad  : byte = 0;  { Zusatz-Fensterbreite/hîhe }

type  { MK 01/2000 von $0fff in $1fff geÑndert }
  memarr     = array[0..$1fff] of byte;

var pullw   : array[1..maxpull] of record
                                     l,r,o,u,wi : byte;
                                     ashad      : byte;
                                     savemem    : ^memarr;
                                     free       : boolean;
                                   end;
    rahmen  : shortint;

procedure qrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean); assembler;
asm
{$IFDEF BP }
         cld
         mov   al,typ
         mov   ah,6
         mul   ah
         mov   bx,offset rchar - 6
         add   bx,ax                   { Offset der Zeichentabbelle [typ] }

         mov   es,base
         mov   al,byte ptr o
         dec   al
         mul   byte ptr zpz
         shl   ax,1                    { di  <-  (o-1) * zpz * 2 }
         mov   di,l
         dec   di
         shl   di,1
         add   di,ax                   { di  <-  di + (l-1) * 2  }
         mov   ax,r
         sub   ax,l
         dec   ax
         mov   dx,ax

         mov   ah,attr
         push  di
         mov   al,[bx+0]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+2]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         mov   si,u
         sub   si,o
         dec   si
         jz    @r2                      { Leerer Mittelteil }

@qrlp1:   push  di
         mov   al,[bx+3]
         stosw
         mov   al,' '
         mov   cx,dx
         cmp   clr,1
         jz    @docl
         add   di,cx
         add   di,cx
         jmp   @nocl
@docl:   rep   stosw
@nocl:   mov   al,[bx+3]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         sub   si,1
         jnz   @qrlp1

@r2:      mov   al,[bx+4]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+5]
         stosw
{$ELSE }
         cld
         xor   eax, eax
         mov   al,typ
         mov   ah,6
         mul   ah
         xor   ebx, ebx
         mov   bl, rchar - 6
         add   ebx,eax                   { Offset der Zeichentabbelle [typ] }

         mov   al,byte ptr o
         dec   al
         mul   byte ptr zpz
         shl   eax,1                    { di  <-  (o-1) * zpz * 2 }
         mov   edi,l
         add   edi, base
         dec   edi
         shl   edi,1
         add   edi, eax                   { di  <-  di + (l-1) * 2  }
         mov   eax,r
         sub   eax,l
         dec   eax
         mov   edx,eax

         mov   ah,attr
         push  edi
         mov   al,[ebx+0]
         stosw
         mov   al,[ebx+1]
         mov   ecx,edx
         rep   stosw
         mov   al,[ebx+2]
         stosw
         pop   edi
         add   edi,zpz
         add   edi,zpz
         mov   esi,u
         sub   esi,o
         dec   esi
         jz    @r2                      { Leerer Mittelteil }

@qrlp1:  push  edi
         mov   al,[ebx+3]
         stosw
         mov   al,' '
         mov   ecx,edx
         cmp   clr,1
         jz    @docl
         add   edi,ecx
         add   edi,ecx
         jmp   @nocl
@docl:   rep   stosw
@nocl:   mov   al,[ebx+3]
         stosw
         pop   edi
         add   edi,zpz
         add   edi,zpz
         sub   esi,1
         jnz   @qrlp1

@r2:     mov   al,[ebx+4]
         stosw
         mov   al,[ebx+1]
         mov   ecx,edx
         rep   stosw
         mov   al,[ebx+5]
         stosw
{$ENDIF }
end;

procedure wshadow(li,re,ob,un:word); assembler;
asm
{$IFDEF BP }
         call  moff
         mov   ax,un                   { Adresse untere linke Ecke berechnen }
         dec   ax
         mov   bx,zpz
         mul   bx
         shl   ax,1
         mov   di,li
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   es,base
         mov   cx,re
         cmp   cx,bx
         jbe   @c1ok
         mov   cx,bx
@c1ok:    sub   cx,li
         inc   cx
         mov   al,shadowcol

@usloop: stosb                         { unteren Schatten zeichnen }
         inc   di
         loop  @usloop

         mov   ax,ob                   { Adresse obere rechte Ecke berechnen }
         dec   ax
         mul   bx
         shl   ax,1
         mov   di,re
         cmp   di,bx                   { Schattenspalte > 80? }
         ja    @nors
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   cx,un
         sub   cx,ob
         mov   al,shadowcol
         dec   bx

@rsloop: stosb                         { rechten Schatten zeichnen }
         add   di,bx
         add   di,bx
         inc   di
         loop  @rsloop

@nors:   call  mon
{$ELSE }
         call  moff
         mov   eax,un                   { Adresse untere linke Ecke berechnen }
         dec   eax
         mov   ebx,zpz
         mul   ebx
         shl   eax,1
         mov   edi,li
         dec   edi
         shl   edi,1
         add   edi,eax
         inc   edi

         add   edi, base
         mov   ecx,re
         cmp   ecx,ebx
         jbe   @c1ok
         mov   ecx,ebx
@c1ok:   sub   ecx,li
         inc   ecx
         mov   al,shadowcol

@usloop: stosb                         { unteren Schatten zeichnen }
         inc   edi
         loop  @usloop

         mov   eax,ob                   { Adresse obere rechte Ecke berechnen }
         dec   eax
         mul   ebx
         shl   eax,1
         mov   edi,re
         cmp   edi,ebx                   { Schattenspalte > 80? }
         ja    @nors
         dec   edi
         shl   edi,1
         add   edi,eax
         inc   edi

         mov   ecx,un
         sub   ecx,ob
         mov   al,shadowcol
         dec   ebx
@rsloop: stosb                         { rechten Schatten zeichnen }
         add   edi,ebx
         add   edi,ebx
         inc   edi
         loop  @rsloop
@nors:   call  mon
{$ENDIF }
end;

procedure clwin(l,r,o,u:word); assembler;
asm
{$IFDEF BP }
         call   moff
         mov    si,zpz
         shl    si,1
         mov    cx,si
         mov    ax,o
         dec    ax
         mul    cx
         mov    dx,l
         dec    dx
         mov    bx,dx
         shl    dx,1
         add    dx,ax
         mov    di,dx                  { dx, di = Startadresse des Fensters }
         mov    cx,r
         sub    cx,bx
         mov    bx,u
         sub    bx,o
         inc    bx                     { bl = Fensterhîhe }
         mov    bh,cl                  { bh,cx = Fensterbreite }
         mov    es,base
         mov    al,' '
         mov    ah,textattr
@wclo:    or     bl,bl
         jz     @wcende                 { Fenster ist gelîscht }
         cld
         rep    stosw                  { Fensterbereich lîschen mit del }
         mov    cl,bh                  { Fensterbreite holen }
         add    dx,si                  { NÑchste Fensterzeile }
         mov    di,dx
         dec    bl
         jmp    @wclo
@wcende: call   mon
{$ELSE }
         call   moff
         mov    esi,zpz
         shl    esi,1
         mov    ecx,esi
         mov    eax,o
         dec    eax
         mul    ecx
         mov    edx,l
         dec    edx
         mov    ebx,edx
         shl    edx,1
         add    edx,eax
         mov    edi,edx                  { dx, di = Startadresse des Fensters }
         mov    ecx,r
         sub    ecx,ebx
         mov    ebx,u
         sub    ebx,o
         inc    ebx                     { bl = Fensterhîhe }
         mov    bh,cl                  { bh,cx = Fensterbreite }
         add    edi, base
         mov    al,' '
         mov    ah,textattr
@wclo:   or     bl,bl
         jz     @wcende                 { Fenster ist gelîscht }
         cld
         rep    stosw                  { Fensterbereich lîschen mit del }
         mov    cl,bh                  { Fensterbreite holen }
         add    edx,esi                  { NÑchste Fensterzeile }
         mov    edi,edx
         dec    bl
         jmp    @wclo
@wcende: call   mon
{$ENDIF }
end;

procedure fwrt(x,y:word; var s:string); assembler;
asm
{$IFDEF BP }
         push ds
         cld
         mov    es,base
         mov    ax, y
         dec    al
{        mov    cl,5
         shl    ax,cl
         mov    di,ax
         shl    ax,1
         shl    ax,1
         add    di,ax }
         mul    zpz
         shl    ax,1
         mov    di,ax
         add    di,x
         add    di,x
         sub    di,2
         mov    ah,textattr
         lds    si,s
         mov    ch,0
         lodsb
         mov    cl,al
         jcxz   @nowrt
@lp:     lodsb
         stosw
         loop   @lp
@nowrt:  pop ds
{$ELSE }
         cld
         mov    edi,base
         mov    eax, y
         dec    eax
         mul    zpz
         shl    eax,1
         mov    edi,eax
         add    edi,x
         add    edi,x
         sub    edi,2
         mov    ah,textattr
         mov    esi, s
         mov    ch,0
         lodsb
         mov    cl,al
         jcxz   @nowrt
@lp:     lodsb
         stosw
         loop   @lp
@nowrt:
{$ENDIF }
end;

{ attr1 = Rahmen/Background; attr2 = Kopf }

procedure explode(l,r,o,u,typ,attr1,attr2:byte; msec:word; txt:string);
var la           : byte;
    ls,rs,os,us,
    i,nx,ny,del  : byte;
begin
  if odd(r-l) then begin
    ls:=(r+l)div 2-1; rs:=ls+3; nx:=(r-l-3)div 2;
    end
  else begin
    ls:=(r+l)div 2-1; rs:=ls+2; nx:=(r-l-2)div 2;
    end;
  if odd(u-o) then begin
    os:=(u+o)div 2-1; us:=os+3; ny:=(u-o-3)div 2;
    end
  else begin
    os:=(u+o)div 2-1; us:=os+2; ny:=(u-o-2)div 2;
    end;
  del:=msec div max(nx,ny);
  if nx>ny then
    for i:=0 to nx do begin
      moff;
      qrahmen(ls-i,rs+i,os-i*ny div nx,us+i*ny div nx,typ,attr1,true);
      mon;
   delay(del);
      end
  else
    for i:=0 to ny do begin
      moff;
      qrahmen(ls-i*nx div ny,rs+i*nx div ny,os-i,us+i,typ,attr1,true);
      mon;
   delay(del);
      end;
  if txt<>'' then begin
    moff;
    la:=lastattr;
    attrtxt(attr1);
    wrt((r+l+1)div 2-length(txt)div 2-2,o,' ');
    attrtxt(attr2); write(' ',txt,' ');
    attrtxt(attr1); write(' ');
    attrtxt(la);
    mon;
    end;
end;


procedure wrt(x,y:byte; txt:string);
begin
  gotoxy(x,y);
  write(txt);
end;


procedure normwin;
begin
  window(1,1,80,25);
end;


procedure rahmen1(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,1,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then
  begin
    wrt((re+li+1)div 2 - length(txt) div 2 - 2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
  end;
  mon;
end;


procedure rahmen2(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,2,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
    end;
  mon;
end;


procedure rahmen3(li,re,ob,un:byte; txt:string);
begin
  normtxt;
  moff;
  qrahmen(li,re,ob,un,3,iif(forcecolor,lastattr,normattr),false);
  if txt<>'' then begin
    wrt((re+li+1)div 2-length(txt)div 2-2,ob,' ');
    invtxt; write(' ',txt,' '); normtxt; write(' ');
    end;
  mon;
end;


Procedure rahmen1d(li,re,ob,m,un:byte; txt:string);
begin
  rahmen1(li,re,ob,un,txt);
  mwrt(li,m,hbar(re-li+1));
end;


Procedure rahmen2d(li,re,ob,m,un:byte; txt:string);
begin
  rahmen2(li,re,ob,un,txt);
  mwrt(li,m,'Ã'+dup(re-li-1,'Õ')+'π');
end;


procedure setrahmen(n:shortint);
begin
  rahmen:=n;
end;

function getrahmen:shortint;
begin
  getrahmen:=rahmen;
end;


Procedure wpull(x1,x2,y1,y2:byte; text:string; var handle:word);
var i : byte;
  j: Integer;
begin
  if (x2-x1<1) or (y2-y1<1) then begin
    writeln('WPULL error');
    halt(1);
    end;
  savecursor;
  cursor(curoff);
  normwin;
  i:=1;
  while not pullw[i].free do
    inc(i);
  handle:=i;
  with pullw[i] do begin
    free:=false;
    l:=x1; r:=x2; o:=y1; u:=y2;
    ashad:=shad;
    wi:=(r-l+1+shad)*2;
    getmem(savemem,wi*(u-o+ashad+1));
    moff;
{$IFNDEF ver32}
   for j:=o-1 to u-1+ashad do
    Fastmove(mem[base:j*zpz*2+(l-1)*2],savemem^[(1+j-o)*wi],wi);
{$ENDIF}
    mon;
    if rahmen=1 then rahmen1(l,r,o,u,text);
    if rahmen=2 then rahmen2(l,r,o,u,text);
    if rahmen>0 then clwin(l+1,r-1,o+1,u-1);
    if rahmen<0 then explode(l,r,o,u,abs(rahmen),normattr,invattr,100,text);
    end;
  restcursor;
end;


Procedure wrest(handle:word);
var i : byte;
begin
  normwin;
  with pullw[handle] do begin
    moff;
{$IFNDEF ver32}
    for i:=o-1 to u-1+ashad do
      Fastmove(savemem^[(i-o+1)*wi],mem[base:i*zpz*2+(l-1)*2],wi);
{$ENDIF}
    mon;
    freemem(savemem,wi*(u-o+ashad+1));
    free:=true;
    end;
end;


procedure sort_list(pa:pointer; anz:integer);    { Liste nach 'el' sortieren }
var i,j : word;
    xch : boolean;
    sa  : slcttyp;
    l   : pntslcta;
begin
  l:=pntslcta(pa);
  j:=anz-1;
  repeat
    xch:=false;
    for i:=1 to j do
      if UStr(l^[i].el)>UStr(l^[i+1].el) then begin
        sa:=l^[i];
        l^[i]:=l^[i+1];
        l^[i+1]:=sa;
        xch:=true;
        end;
  until not xch;
end;


Procedure wslct(anz:integer; ta:pntslcta; handle,pos:word; abs1:boolean;
                var n:word; var brk:boolean);

var z          : taste;
    i,po,pon   : integer;
    wsize      : word;
    pa,pan     : integer;
    ende       : boolean;
    ox         : integer;

Procedure dispage;
var i:integer;
begin
  moff;
  with pullw[handle] do begin
    for i:=1 to wsize do
      if i+pa<=anz then
        with ta^[i+pa] do begin
          if zu then normtxt else hightxt;
          wrt(l+2,ox+i,el);
          normtxt;
          end
      else
        wrt(l+2,ox+i,sp(r-l-3));
    if warrows then begin
      attrtxt(warrcol);
      wrt(l,o+1,iifc(pa>0,#30,#179));
      wrt(l,u-1,iifc(pa+wsize<anz,#31,#179));
      normtxt;
      end;
    end;
  mon;
end;

Procedure godown;
begin
  if (pan+pon<anz) then begin
    inc(pon);
    if pon>wsize then begin
      dec(pon); inc(pan);
      end;
    end;
end;

Procedure goup;
begin
  if pon+pan>1 then begin
    dec(pon);
    if pon=0 then begin
      dec(pan); pon:=1;
      end;
    end;
end;


begin    { of wslct }
  if anz=0 then begin
    brk:=true;
    exit;
    end;
  pos:=min(pos,anz);
  savecursor;
{$IFNDEF ver32}
  cursor(curoff);
{$ENDIF}
  normwin;
  ende:=false;
  with pullw[handle] do begin
    for i:=1 to anz do
      ta^[i].el:=forms(ta^[i].el,r-l-3);
    ox:=iif(abs1,o+1,o);
    wsize:=u-ox-1;
    if pos<=anz then begin
      pa:=0; po:=pos;
      end
    else begin
      pa:=pos-1; po:=1;
      end;
    if po>wsize then begin
      inc(pa,po-wsize);
      po:=wsize;
      end;
    dispage;
    mausiniti;
    repeat
      mauszuo:=(pa+po>1);
      mauszuu:=(pa+po<anz);
      invtxt;
      mwrt(l+2,ox+po,ta^[pa+po].el);
      selp(ta^[pa+po]);
      get(z,curoff);
      pan:=pa; pon:=po;
      if (z=keydown) or (z=keytab) or (z[1]='2') then
        godown
      else if (z=keyup) or (z=keystab) or (z[1]='8') then
        goup
      else if z=keyesc then begin
        brk:=true;
        ende:=true;
        end
      else if (z=keyhome) or (z[1]='7') then begin
        pon:=1;
        if not ta^[pan+pon].zu then godown;
        end
      else if (z=keyend) or (z[1]='1') then begin
        pon:=min(wsize,anz-pan);
        if not ta^[pan+pon].zu then goup;
        end
      else if (z=keypgup) or (z[1]='9') then begin
        if pan=0 then pon:=1
        else pan:=max(0,pan-wsize);
        end
      else if (z=keypgdn) or (z[1]='3') then begin
        if pan+wsize>=anz then pon:=anz-pan
        else pan:=min(anz-pon,pan+wsize);
        end
      else if (z=keycpgu) or (z=keychom) then begin
        pan:=0; pon:=1;
        end
      else if (z=keycpgd) or (z=keycend) then begin
        pan:=max(0,anz-wsize);
        pon:=anz-pan;
        end
      else if z=keycr then begin
        brk:=false;
        ende:=true;
        end;
      if pa<>pan then begin
        pa:=pan;
        po:=pon;
        dispage;
        end
      else begin
        normtxt;
        wrt(l+2,ox+po,ta^[pa+po].el);
        po:=pon;
        end;
    until ende;
    n:=po+pa;
    end;
  restcursor;
end;


procedure seldummy(var sel:slcttyp);
begin
end;


procedure w_copyrght;
var z     : taste;
    sd,st : boolean;
    buf   : array[0..159] of byte;
begin
  moff;
{$IFNDEF ver32}
  Fastmove(mem[base:(crline-1)*160],buf,160);
{$ENDIF}
  mon;
  disphard(1,crline,dup(16,'∞')+' (c) by '+pm+' ∞∞∞∞∞∞ Tel. 02632/48651 '+
           dup(16,'∞'));
  sd:=m2d; st:=m2t;
  if datey=25 then m2d:=false;
  if timey=25 then m2t:=false;
  get(z,curoff);
  m2d:=sd; m2t:=st;
  Disp_DT;
  moff;
{$IFNDEF ver32}
  Fastmove(buf,mem[base:(crline-1)*160],160);
{$ENDIF}
  mon;
end;


procedure wpush(x1,x2,y1,y2:byte; text:string);
var r   : byte;
    tx1 : char;
begin
  if wpp=maxpush then writeln('WPUSH error')
  else begin
    r:=rahmen;
    if (text='*') or (text='-') then begin
      setrahmen(0); text:='';
      tx1:=text[1];
      end
    else
      tx1:=' ';
    inc(wpp);
    wpull(x1,x2,y1,y2,text,wpstack[wpp]);
    if tx1='*' then clwin(x1,x2,y1,y2);
    setrahmen(r);
    end;
end;


procedure wpushs(x1,x2,y1,y2:byte; text:string);
begin
  shad:=1;
  wpush(x1,x2,y1,y2,text);
{  rahmen1(x1,x2,y1,y2,text);}
{  clwin(x1+1,x2-1,y1+1,y2-1); }
  wshadow(x1+1,x2+1,y1+1,y2+1);
  shad:=0;
end;

procedure wpop;
begin
  if wpp=0 then
{$IFDEF Debug }
  writeln('WPOP error')
{$ENDIF }
  else begin
    wrest(wpstack[wpp]);
    dec(wpp);
    Disp_DT;
    end;
end;


var i : byte;

begin
  for i:=1 to maxpull do
    pullw[i].free:=true;
  rahmen:=1;
  fnproc[3,10]:=w_copyrght;
  wpp:=0;
  warrows:=false;
  warrcol:=7;
  selp:=seldummy;
end.
{
  $Log$
  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
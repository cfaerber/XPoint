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

{ mhelpnr=0 -> keine Hilfsseiten }

procedure readHmask(mhelpnr:word; var brk:boolean);
var ax,p,myp  : integer;
    afld      : feldp;
    helpsave  : word;          { zum Sichern von help_page }
    t         : taste;
    newfld    : boolean;
    redisplay : boolean;
    replace   : boolean;
    doreplace : boolean;
    s         : string;
    valchar   : boolean;
    i,x       : integer;
    mzu,mzo   : boolean;
    mzl,mzr   : boolean;
    crec      : ^customrec;
    fwd       : boolean;
    maussel   : boolean;
    poutside  : boolean;
    { adiff     : integer; }
    moretext  : boolean;
    lasttexty : byte;
    gl        : byte;

  procedure ClearFields;
  var i : integer;
      s : string;
  begin
    with amaskp^ do begin
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if disnodisp and not enabled then begin
            attrtxt(stat.col.ColFeldName);
            wrt(xx,yy,dup(length(txt)+len+3,' '));
          end;
      mon;
      end;
    normtxt;
  end;

  procedure ShowFldNames;
  var i : integer;
      s : string[80];
  begin
    with amaskp^ do begin
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if enabled or (not disnodisp) then begin
            s:=txt;
            if enabled then
              if owncol then attrtxt(ownattr)
              else attrtxt(stat.col.ColFeldName)
            else begin
              attrtxt(stat.col.ColDisabled);
              if stat.col.ColDisabled shr 4 = stat.col.ColDisabled and $f then
                s:=sp(length(s));  { einige Herc-Bildschirme kînnen gleiches }
              end;                 { Vorder- und Hintergrundattribut nicht   }
            if (yy0>=a+1) and (yy<=a+un) then                  { darstellen  }
              wrt(xx,yy-a,txt);
            end;
      mon;
      end;
    normtxt;
  end;

  procedure FldDisplay;
  var i   : integer;
      l   : string[1];
      ua  : boolean;    { Scrollpfeil nach oben }
      da  : boolean;    { Scrollpfeil nach unten }
  begin
    with amaskp^ do begin
      attrtxt(stat.col.ColFeldInput);
      l:=iifs(stat.arrowspace,' ','');
      ua:=false; da:=false;
      moff;
      for i:=1 to felder do
        with fld[i]^ do
          if (yy0>=a+1) and (yy<=a+un) then begin
            if enabled or (not disnodisp) then begin
              if not enabled then { not enabled }
	      begin
                attrtxt(stat.col.ColBack);
                wrt(xx2,yy-a,l+sp(len)+l);
                if stat.mausarrows and hassel then 
		  Wrt2(' ')
                else 
		if checkbutt then 
		  Wrt2('  ');
                attrtxt(stat.col.ColFeldInput);
              end { not enabled }
              else if checkbutt then begin { enabled, checkbutt }
                attrtxt(stat.col.colbuttons);
                wrt(xx2,yy-a,' ['+iifc(cont=yesno[1],'x',' ')+'] ');
              end { enabled, checkbutt }
              else begin { enabled, not checkbutt }
                attrtxt(stat.col.colfeldinput);
                wrt(xx2,yy-a,l+forms(cont,len));
                if stat.arrowspace and enabled then 
		begin
                  if length(cont)>len then 
		  begin
                    attrtxt(stat.col.ColArrows);
                    Wrt2(#16);
                  end
                  else
                    Wrt2(' ');
                  if stat.mausarrows and hassel then 
		  begin
                    attrtxt(stat.col.colback);
                    Wrt2(#25);
                  end;
                end; { stat.arrowspace and enabled }
              end; { enabled, not checkbutt }
            end; { enabled or (not dispnodisp) } 
          end { (yy0>=a+1) and (yy<=a+un) }
          else
            if yy0<a+1 then ua:=true
            else da:=true;
      attrtxt(uda.color);
      if uda.x1>0 then wrt(uda.x1,uda.y1,iifc(ua,#30,uda.fillc));
      if uda.x2>0 then wrt(uda.x2,uda.y2,iifc(da,#31,uda.fillc));
      mon;
      end;
    normtxt;
  end;

  procedure textdisplay;
  var p : textnodep;
  begin
    with amaskp^ do begin
      moretext:=false;
      lasttexty:=0;
      p:=mtxt;
      moff;
      while p<>nil do begin
        with p^ do begin
          lasttexty:=max(lasttexty,yy);
          if yy>a+un-ob+1 then
            moretext:=true
          else if yy>=a+1 then begin
            attrtxt(attr);
            wrt(xx,yy+ob-1-a,txt);
            end;
          end;
        p:=p^.next;
        end;
      normtxt;
      mon;
      end;
  end;

  procedure setcur;
  begin
    with afld^ do
      gotoxy(xx2+p-iif(amaskp^.stat.arrowspace,0,1)+iif(checkbutt,1,0),
             yy-amaskp^.a);
  end;

  function valid(feld:feldp; entered:boolean):boolean;
  var l   : longint;
      res : integer;
      s   : string[40];
      r   : real;
      p   : selnodep;
      v   : boolean;

    function testdate:boolean;
    var t,m,j : word;
    begin
      with feld^ do
        if pempty and (cont=mask) then
          testdate:=true
        else begin
          if len=8 then
          begin
            { MK 13.02.2000 Y2K Fix }
            j :=ival(copy(cont,7,2));
            if j < 70 then inc(j, 2000) else inc(j, 1900);
          end else
            j:=ival(copy(cont,7,4));
          schalt(j);
          t:=ival(copy(cont,1,2));
          m:=ival(copy(cont,4,2));
          testdate:=(m>=1) and (m<=12) and
                    (t>=1) and (t<=monat[m].zahl);
          end;
    end;

    function testtime(len:byte; t:string):boolean;
    var h,m,s : word;
    begin
      h:=ival(copy(t,1,2));
      m:=ival(copy(t,4,2));
      if len>5 then s:=ival(copy(t,7,2))
      else s:=1;
      testtime:=(h<=23) and (m<=59) and (s<=59);
    end;

  begin
    v:=true;
    with feld^ do begin
      if typ=1 then
        case autotrim of
          1 : cont:=trimright(cont);
          2 : cont:=trim(cont);
        end;
      if (typ>=2) and (typ<=7) then begin
        s:=trim(cont);
        if pos(' ',s)>0 then s:=copy(s,1,pos(' ',s)-1);
        if typ<7 then begin
          val(s,l,res);
          if not entered then begin
            v:=v and (l>=_min) and (l<=_max);
            end
          else
            if (l<_min) or (l>_max) then begin
              l:=max(l,_min);
              l:=min(l,_max);
              amaskp^.modified:=true;
              end;
          str(l:maxlen,cont);
          end
        else begin
          val(s,r,res);
          if not entered then begin
            v:=v and (r>=_rmin) and (r<=_rmax);
            end
          else
            if (r<_rmin) or (r>_rmax) then begin
              r:=maxr(r,_rmin);
              r:=minr(r,_rmax);
              amaskp^.modified:=true;
              end;
          str(r:maxlen:nk,cont);
          end;
        {cont:=cont+' ';}
        end;
      if (selliste<>nil) and forcesll then begin
        p:=selliste;
        while (p<>nil) and (UpperCase(cont)<>UpperCase(p^.el)) do
          p:=p^.next;
        if (p=nil) and (cont<>'') then begin    { Ñhnlichen Eintrag suchen }
          p:=selliste;
          while (p<>nil) and (LeftStr(UpperCase(p^.el),length(cont))<>UpperCase(cont)) do
            p:=p^.next;
          if p<>nil then begin
            cont:=p^.el;
            amaskp^.modified:=true;
            end;
          end;
        v:=v and (p<>nil);
        end;
      case typ of
        8 : v:=v and testdate;
        9 : v:=v and testtime(len,cont);
      end;
      valid:=v and test2(cont);
      end;
  end;

  procedure showfield;
  begin
    with amaskp^ do
      with afld^ do begin
        gotoxy(xx2,yy-a);
        moff;
        if checkbutt then begin
          attrtxt(stat.col.colbuttons);
          Wrt2(' [' + iifc(cont=yesno[1],'x',' ') + '] ');
          end
        else begin
          if stat.arrowspace then begin
            attrtxt(stat.col.colarrows);
            Wrt2(iifc(ax>0,#17,' '));
            end;
          attrtxt(stat.col.ColFeldActive);
          if replace then attrtxt(stat.col.ColFeldMarked);
          Wrt2(copy(cont,ax+1,len));
          attrtxt(stat.col.ColFeldActive);
          Wrt2(dup(len-length(cont)+ax,stat.fillchar));
          if stat.arrowspace then begin
            attrtxt(stat.col.colarrows);
            Wrt2(iifc(length(cont)-ax>len,#16,' '));
            end;
          if stat.mausarrows and hassel then begin
            attrtxt(stat.col.colback);
            Wrt2(#25);
            end;
          end;
        mon;
        normtxt;
        end;
  end;

  procedure select(var brk,fwd:boolean);     { sll = gl ! }
  var p1     : selnodep;
      lines  : integer;
      width  : byte;
      i,p,sa : integer;
      t      : taste;
      na     : byte;
      insmaus: boolean;   { Taste wurde 'inside' gedrÅckt }

    procedure display;
    var i : integer;
    begin
      with amaskp^,afld^ do begin
        p1:=selliste;
        for i:=1 to sa do       { sa *mu·* < lines sein!! }
          p1:=p1^.next;
        moff;
        for i:=1 to sll do begin
          if i=p then attrtxt(stat.col.ColSelBar);
          if p1=nil then wrt(slx+1,sly+i,sp(width+2))
          else wrt(slx+1,sly+i,' '+forms(p1^.el,width+1));
          normtxt;
          if p1<>nil then p1:=p1^.next;
          end;
        wrt(slx,sly+1,iifc(sa>0,#30,'≥'));
        wrt(slx,sly+sll,iifc(sa+sll<lines,#31,'≥'));
        mon;
        end;
    end;

    procedure oben;
    begin
      if p>1 then dec(p) else
      if sa>0 then dec(sa);
    end;

    procedure unten;
    begin
      if (sa+p<lines) then
        if p<afld^.sll then inc(p)
        else inc(sa);
    end;

    procedure maus_bearbeiten;
    var mx,my  : integer;
        inside : boolean;
    begin
      with afld^,amaskp^ do begin
        maus_gettext(mx,my);
        inside:=(mx>slx) and (mx<slx+width+3) and (my>sly) and (my<sly+sll+1);
        dec(my,sly);
        if (t=mausleft) or (t=mausright) then
          insmaus:=inside;
        if inside or insmaus then
          if (t=mausunleft) or (t=mausunright) then t:=^J;
        if inside then
          if (t=mausleft) or (t=mauslmoved) or (t=mausrmoved) or (t=mausright)
            then p:=minmax(my,1,min(sll,lines-sa));
        if not inside then begin
          if t=mausleft then t:=^J
          else if t=mausright then t:=keyesc;
          end;
        end;
    end;

  begin { procedure select }
    brk:=false;
    with amaskp^,afld^ do begin
      width:=4; lines:=0;       { Minimale Breite: 4 }
      p1:=selliste;
      p:=1; sa:=0;
      insmaus:=maussel;
      while p1<>nil do begin    { Breite und Zeilenzahl bestimmen }
        inc(lines);
        width:=max(width,min(length(p1^.el), ScreenWidth-10));
        if UpperCase(cont)=UpperCase(p1^.el) then p:=lines;
        p1:=p1^.next;
        end;

      if noslpos then             { Position wird jeweils neu bestimmt }
        if maussel then begin
          slx:=min(xx2,75-width);
          sly:=max(3,yy-p-a);
          sll:=min(lines,ScreenLines-sly-1);
          end
        else
          if (yy-a<=ScreenLines-3-slmin) or (yy-a+lines+2<=ScreenLines-1) then begin
            slx:=min(xx2,75-width); sly:=yy-a+1; sll:=min(lines,ScreenLines-3-yy+a);
            end
          else begin
            i:=lines div 2;
            slx:=min(75-width,xx2+7);
            sly:=max(3,min(yy-a-i+1,ScreenLines-2-lines));
            sll:=min(ScreenLines-2-sly,lines);
            end;
      if p>sll then begin
        sa:=p-sll; p:=sll; end;

      na:=normattr; normattr:=stat.col.ColSelBox;
      normtxt;
      wpushs(slx,slx+width+3,sly,sly+sll+1,'');
      maus_pushinside(slx+1,slx+width+2,sly+1,sly+sll-1);
      repeat
        display;
        if stat.selcursor then begin
          gotoxy(slx+1,sly+p);
          get(t,curon);
          end
        else
          get(t,curoff);
        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten;
        if t=keyup then oben;
        if t=keydown then unten;
        if t=keyhome then begin
          p:=1; sa:=0;
          end;
        if t=keyend then begin
          p:=min(lines,sll);
          sa:=lines-p;
          end;
        if t=keypgup then
          for i:=1 to sll-1 do oben;
        if t=keypgdn then
          for i:=1 to sll-1 do unten;
        if t=stat.selboxkey then t:=keyesc;
      until (t=keycr) or (t=^J) or (t=keyesc);
      maus_popinside;
      normattr:=na;
      normtxt;
      if t<>keyesc then begin
        p1:=selliste;
        for i:=1 to p+sa-1 do p1:=p1^.next;
        if length(p1^.el)>maxlen then
          error('Eingabefeld zu kurz!');
        cont:=p1^.el;
        modified:=true;
        end;
      end;
    wpop;
    brk:=(t=keyesc);
    fwd:=(t=keycr);
    maussel:=false;
  end;

  { Achtung! vor jeder Modifikation an cont mu· xtest1() aufgerufen werden! }

  function xtest1(var s:string):boolean;
  var x1 : boolean;
  begin
    if afld^.nonedit then xtest1:=false
    else begin
      if afld^.topcase then
      begin
        s := LowerCase(s);
        if s<> '' then s[1] := UpCase(s[1]);
      end;
      x1:=afld^.test1(s);
      if x1 then amaskp^.modified:=true;
      xtest1:=x1;
    end;
  end;

  { offs Eingabefelder weiterspringen }

  procedure movefield(offs:integer);
  var newyp : integer;
      s     : string;
  begin
    with amaskp^ do begin
      p:=1; ax:=0;
      with afld^ do begin
        if autotrim<>0 then begin               { altes Feld verlassen ... }
          s:=cont;
          if autotrim=1 then
            s:= TrimRight(s)
          else
            s:=trim(s);
          if (s<>cont) and (xtest1(s)) then;
          cont:=s;
          end;
        if (offs=0) or valid(afld,true) then begin
          afld^.test3(afld^.cont);
          FldDisplay;

          newyp:=max(1,min(felder,yp+offs));    { neues Feld suchen ... }
          while (newyp<=felder) and not fld[newyp]^.enabled do begin
            if abs(offs)>1 then offs:=-sgn(offs);
            inc(newyp,sgn(offs));
            if newyp<1 then
              case stat.wrapmode of
                dont_wrap,endonlast : begin offs:=-offs; newyp:=1; end;
                do_wrap             : newyp:=felder;
              end
            else if newyp>felder then
              case stat.wrapmode of
                dont_wrap  : begin offs:=-offs; newyp:=felder; end;
                do_wrap    : newyp:=1;
                endonlast  : if t=keycr then t:=^J
                             else begin
                               offs:=-offs; newyp:=felder;
                               end;
              end;
            end;
          if newyp<=felder then yp:=newyp;

          while a+1>fld[yp]^.yy0 do begin       { a anpassen ... }
            dec(a); redisplay:=true;
            end;
          while a+(un-ob+1)<fld[yp]^.yy0 do begin
            if t=keycr then
              inc(a,min(stat.autojump,maxyy0-(a+un-ob+1)))
            else inc(a);
            redisplay:=true;
            end;
          end;
        end;
      newfld:=true;     { Kennzeichen: zur Zeit kein aktives Feld; }
                        {              Feld yp ist zu aktivieren   }
      end;
  end;

  procedure links;
  begin
    repeat
      if p>1 then dec(p)
      else if ax>0 then dec(ax);
    until (afld^.mask='') or (afld^.mask[p+ax]=' ');
    replace:=false;
  end;

  procedure rechts;
  begin
    with afld^ do begin
      if (p+ax<=length(cont)) and
         not ((p=len) and (len=maxlen) and (p+ax=length(cont))) then
        repeat
          if p<len then inc(p)
          else inc(ax);
        until (mask='') or ((mask[ax+p]=' ') or (ax+p>length(mask)));
      if (mask<>'') and (ax+p>length(mask)) then
        repeat
          if ax>0 then dec(ax)
          else dec(p)
        until mask[ax+p]=' ';
      end;
    replace:=false;
  end;

  procedure returnfields;
  var i : integer;
      l : longint;
      r : real;
      b : boolean;
  begin
    with amaskp^ do
      for i:=1 to felder do
        with fld[i]^ do
          if variable<>nil then begin
            if (typ>=2) and (typ<=6) then l:=ival(cont);
            case typ of
              1,8,9 : AnsiString(variable^):= cont; {Move(cont,variable^,length(cont)+1);}
              2,3   : Move(l,variable^,1);
              4,5   : Move(l,variable^,2);
              6     : Move(l,variable^,4);
              7     : begin
                        r:=rval(cont);
                        Move(r,variable^,6);
                      end;
              10    : begin
                        b:=(cont=yesno[1]);
                        Move(b,variable^,1);
                      end;
            end;
          end;
  end;

  procedure fillfnhelp;
  begin
    with amaskp^.stat,afld^ do begin
      attrtxt(col.ColFnFill);
      mwrt(fnix,fniy,dup(length(fnkeyinfo),fnkeyfill));
      end;
  end;

  procedure exitmask;
  begin
    if not newfld then
      with amaskp^.stat,afld^ do begin
        test3(cont);
        replace:=false;
        showfield;
        if hpline<>'' then
        begin
          attrtxt(col.ColHelpTxt);
          wrt(hpx,hpy,sp(hpl));
        end;
        if (fnix<>0) and (selliste<>nil) then
          fillfnhelp;
        normtxt;
        end;
  end;

  procedure _count_;
  var dat : fdate;
      s   : datetimest;
      h,m : byte;
  begin
    with afld^ do begin
      s:=cont;
      if cpos(' ',s)>0 then exit;
      if counter=1 then begin
        dat.t:=ival(LeftStr(s,2));
        dat.m:=ival(copy(s,4,2));
        if length(s)=8 then begin
          dat.j:=ival(RightStr(s,2));
          if dat.j>=50 then inc(dat.j,1900)
          else inc(dat.j,2000);
          end
        else
          dat.j:=ival(RightStr(s,4));
        if t='+' then incd(dat)
        else decd(dat);
        cont:=formi(dat.t,2)+'.'+formi(dat.m,2)+'.'+
               iifs(length(s)=8,formi(dat.j mod 100,2),formi(dat.j,4));
        end
      else begin
        h:=ival(LeftStr(s,2));
        m:=ival(RightStr(s,2));
        if t='+' then begin
          inc(m);
          if m>59 then begin
            m:=0; inc(h);
            if h>23 then h:=0;
            end;
          end
        else
          if m>0 then dec(m)
          else begin
            m:=59;
            if h>0 then dec(h)
            else h:=23;
            end;
        cont:=formi(h,2)+':'+formi(m,2);
        end;
      end;
  end;

  procedure testfndef;
  var fnkn : byte;
  begin
    if ((t>=keyf1) and (t<=keyf10)) or
       ((t>=keysf1) and (t<=keysf10)) then begin
      if t<=keyf10 then
        fnkn:=ord(t[2])-58
      else
        fnkn:=ord(t[2])-73;
      if fndef[fnkn]<>'' then
        if RightStr(fndef[fnkn],1)=';' then
          keyboard(LeftStr(fndef[fnkn],length(fndef[fnkn])-1)+#13)
        else
          keyboard(fndef[fnkn]);
      end;
  end;

  procedure maus_bearbeiten;
  var inside  : boolean;
      mx,my,i : integer;
      arrows  : integer;
      ok      : boolean;
      infield : boolean;
      s       : string;
  begin
    with amaskp^ do begin
      maus_gettext(mx,my);
      if ((t=mausleft) or (t=mausldouble)) and (mx=uda.x1) and (my=uda.y1) then
        t:=keypgup
      else if ((t=mausleft) or (t=mausldouble)) and (mx=uda.x2) and (my=uda.y2) then
        t:=keypgdn
      else begin
        inside:=(mx>=li-1) and (mx<=re+1) and (my>=ob-1) and (my<=un+1);
        if inside then begin
          arrows:=iif(stat.arrowspace,2,0);
          if (t=mausleft) or (t=mausldouble) or (t=mausright) then begin
            i:=1;
            ok:=false;
            while (i<=felder) and not ok do begin
              with fld[i]^ do
                ok:=(mx>=min(xx,xx2)-1) and
                    (mx<=max(xx2+len+arrows+iif(hassel,1,0),xx+length(txt))) and
                    (my=yy-a) and enabled;
              if not ok then inc(i);
              end;
            if ok then with fld[i]^ do begin
              movefield(i-yp);
              if yp=i then begin
                afld:=fld[yp];
                infield:=(mx>=xx2) and (mx<xx2+len+arrows+iif(checkbutt,2,0));
                if hassel and (mx>=xx2+len+arrows-1) and (mx<=xx2+len+arrows+1) then
                  _keyboard(stat.selboxkey)
                else begin
                  if infield then
                    if stat.arrowspace and (length(cont)-ax>len) and (mx=xx2+len+1)
                    then begin
                      ax:=length(cont)-len+1;
                      p:=length(cont)-ax+1;
                      replace:=false;
                      doreplace:=false;
                      end
                    else if t<>mausldouble then begin
                      p:=minmax(mx-xx2-iif(stat.arrowspace,0,1),1,
                                min(length(cont)+1,len));
                      replace:=false;
                      doreplace:=false;
                      end
                    else begin
                      p:=1; ax:=0;
                      end;
                  if checkbutt and ((t=mausleft) or (t=mausldouble)) then begin
                    if cont=yesno[1] then s:=yesno[2]
                    else s:=yesno[1];
                    if xtest1(s) then cont:=s;
                    end;
                  if t=mausright then
                    if infield and hassel then
                      maussel:=true;
                  end;
                end;
              end;
            end;
          if (t=mausunright) or (t=mausunleft) then
            poutside:=false;
          end
        else begin    { not inside }
          if (t=mausright) or (t=mausleft) then poutside:=true else
          if (t=mausunleft) and poutside then t:=^J else
          if (t=mausunright) and poutside then t:=keyesc;
          end;
        end;
      end;
  end;

begin { procedure readHmask(mhelpnr:word; var brk:boolean); }
  newfld:=true; redisplay:=true;
  mzu:=mauszuu; mzo:=mauszuo;
  mzl:=mauszul; mzr:=mauszur;
  mauszul:=true; mauszur:=true;
  insert_mode:=true;
  helpsave:=help_page;

  with amaskp^,amaskp^.stat do begin
    if felder=0 then error('no fields!');
    editing:=true;
    a:=0; ax:=0; p:=1;
    modified:=false;
    doreplace:=true;
    yp:=1;
    while not fld[yp]^.enabled do inc(yp);
    if dopush then
      if rahmentyp=0 then
        wpush(li,re+1,ob,un+1,'-')
      else
        wpush(rl,rr+1,ro,ru+1,'-');

    attrtxt(col.ColBack);
    forcecolor:=true;
    case rahmentyp of
      1   : rahmen1(rl,rr,ro,ru,'');
      2,4 : rahmen2(rl,rr,ro,ru,'');
      3   : rahmen3(rl,rr,ro,ru,'');
    end;
    if rahmentyp>0 then wshadow(rl+1,rr+1,ro+1,ru+1);
    forcecolor:=false;
    if rahmentyp>0 then clwin(rl+1,rr-1,ro+1,ru-1);

    maussel:=false;
    poutside:=false;
    repeat { main loop }

      if redisplay then 
      begin
        attrtxt(col.ColBack);
        if rahmentyp=4 then 
	begin
          moff;
          if a>0 then wrt(rl,ro,'÷'+dup(rr-rl-1,'ƒ')+'∑')
          else wrt(rl,ro,'…'+dup(rr-rl-1,'Õ')+'ª');
          if maxyy0-a>un-ob+1 then wrt(rl,ru,'”'+dup(rr-rl-1,'ƒ')+'Ω')
          else wrt(rl,ru,'»'+dup(rr-rl-1,'Õ')+'º');
          mon;
        end;
        if not dontclear then  
	  clwin(li,re,ob,un);
        redisplay:=false;
        redispfields:=true;
        redisptext:=true;
      end;
	
      if redispfields then 
      begin
        ClearFields;
        showfldnames;
        FldDisplay;
        if @userdisp<>nil then 
	  userdisp;
        redispfields:=false;
      end;
      
      if redisptext then 
      begin
        textdisplay;
        redisptext:=false;
      end;

      afld:=fld[yp];
      with afld^ do begin
        setcur;                          {----- Feldwechsel: neues Feld -----}
        help_page:=mhelpnr+helpnr;
        if newfld then begin
          replace:=autohigh and doreplace;
          doreplace:=true;
          test0(cont);
          if autoup then UpString(cont)
          else if autodown then LoString(cont);
          showfield;
          newfld:=false;
          if hpx<>0 then begin           { Hilfszeile anzeigen }
            s:=hpline;
            if hcenter then s:=center(s,hpl)
            else s:=forms(s,hpl);
            attrtxt(col.ColHelpTxt);
            mwrt(hpx,hpy,s);
            normtxt;
            end;
          if fnix<>0 then begin          { SelKey-Info anzeigen }
            if (selliste<>nil) or (@custom<>nil) then begin
              attrtxt(col.ColFnInfo);
              mwrt(fnix,fniy,fnkeyinfo);
              end
            else
              fillfnhelp;
            normtxt;
            end;
          end                            {-----------------------------------}
        else
          showfield;
        setcur;
        mauszuo:=yp>1;
        mauszuu:=yp<felder;
        if maussel then
          t:=selboxkey
        else
          if insert_mode then
            get(t,curon)
          else
            get(t,cureinf);

        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten;

        if ((t=keyins) and not (kb_shift or kb_ctrl)) then insert_mode:=not insert_mode;
        if (t=keyup) or (t=keystab) then
          if yp>1 then movefield(-1)
          else
            if wrapmode=do_wrap then t:=keycpgd;
        if (t=keydown) or (t=keytab) or (t=keycr) then
          if yp<felder then movefield(+1)
          else if wrapmode=do_wrap then t:=keycpgu
          else if (wrapmode=endonlast) and (t=keycr) then t:=^J;
        if t=keyleft then links;
        if t=keyrght then rechts;
        if (t=keyclft) and (ax+p>1) then
          repeat
            links
          until (ax+p=1) or ((cont[ax+p]<>' ') and (cont[ax+p-1]=' '));
        if (t=keycrgt) and (ax+p<=length(cont)) then
          repeat
            rechts
          until (ax+p>length(cont)) or
                ((ax+p=length(cont)) and (length(cont)=maxlen)) or
                ((cont[ax+p]<>' ') and (cont[ax+p-1]=' '));
        if t=keyhome then begin
          p:=1; ax:=0;
          replace:=false;
          end;
        if t=keyend then begin
          if len=maxlen then p:=min(length(cont)+1,len)
          else begin
            ax:=max(0,length(cont)-len+1);
            p:=length(cont)-ax+1;
            end;
          replace:=false;
          end;
        if (t=keybs) and (p+ax>1) then
          if mask<>'' then links
          else
            if replace then
              t:=^Y
            else begin
              s:=cont;
              delete(s,p+ax-1,1);
              if xtest1(s) then cont:=s;
              links;
              end;
        if (t=keydel) and (p+ax<=length(cont)) and (mask='') then
          if replace then t:=^Y
          else begin
            s:=cont;
            delete(s,p+ax,1);
            if xtest1(s) then cont:=s;
            end;
        if ((t=^T) or (t=#127)) and (mask='') then begin
          i:=p+ax;
          if t=^T then begin
            while (i<=length(cont)) and (cont[i]<>' ') do inc(i);
            while (i<=length(cont)) and (cont[i]=' ') do inc(i);
            end
          else begin
            while (p+ax>1) and (cont[p+ax-1]<>' ') do links;
            while (p+ax>1) and (cont[p+ax-1]=' ') do links;
            end;
          s:=cont;
          delete(s,p+ax,i-(p+ax));
          if xtest1(s) then cont:=s;
          replace:=false;
          end;
        if typ=1 then
          if (t=^G) and not (autodown or topcase) then begin
            UpString(cont); replace:=false; end
            else
          if (t=^K) and not (autoup or topcase) then begin
            cont:=LowerCase(cont); replace:=false; end
            else
          if (t=^X) and not (autoup or autodown) then begin
            cont:=TopAllStr(UpperCase(cont)); replace:=false; end
            else
                                                              { JG: Clipboard fuer Menues }

          if (t=^C) or (t=keycins) then string2clip(cont);   { STRG+C / STRG+INS = kopieren }

          if (t=^V) or ((t=keyins) and kb_shift) then
          begin   { STRG+V / SHIFT+INS }
            s:= Clip2String;
            i := length(s);
            if replace then
            begin                                             { Ersetzen }
              p:=1; ax:=0; replace:=false;
            end else
              s:=LeftStr(cont,p+ax-1)+s+mid(cont,p+ax);

            p:=p+i;                                           {Cursorposition aktualisieren}
            if p>=len then
            begin
              ax:=ax+p-len;
              p:=len;
              end;
            if xtest1(s) then cont:=s;
            end;

        if (t=keypgup) then
          if yp>1 then begin
            i:=yp; x:=fld[yp]^.yy;
            while (i>1) and (x-fld[i]^.yy<un-ob) do dec(i);
            { adiff:=(un-ob+1)-(x-fld[i]^.yy); }
            movefield(i-yp);
            {if (a>0) and (adiff>0) then begin
              a:=max(0,a-adiff);
              redisplay:=true;
              end;}
            end
          else
            if a>0 then begin
              a:=0;
              redisplay:=true;
              end;
        if (t=keypgdn) and ((yp<felder) or moretext) then begin
          gl:=un-ob+1;
          i:=yp; x:=fld[yp]^.yy;
          while (i<felder) and (fld[i]^.yy-x<gl-1) do inc(i);
          if i<>yp then
            movefield(i-yp);
          if (lasttexty>a+gl) and (fld[i]^.yy-x<gl-1) then begin
            inc(a,min(lasttexty-a-gl,gl-1-(fld[i]^.yy-x)));
            redisplay:=true;
            end;
          end;
        if t=keycpgu then movefield(1-yp);
        if t=keycpgd then movefield(felder-yp);

        if t=selboxkey then begin
          if selhelpnr<>0 then
            help_page:=selhelpnr;
          if selliste<>nil then begin
            select(brk,fwd);
            if not brk then begin
              if xtest1(cont) then;   { Select-Strings sind immer ok! }
              movefield(iif(fwd,+1,0));
              end;
            end
          else if @custom<>nil then
          begin
            getmem(crec,sizeof(customrec));
            system.fillchar(crec^,sizeof(customrec),0);
            with crec^ do begin
              acol:=col;
              x:=xx2; y:=yy;
              fpos:=yp;
              s:=cont;
              brk:=false;
              custom(crec^);
              if not brk then begin
                cont:=s;
                if xtest1(cont) then;
                movefield(+1);
                end;
              end;
            freemem(crec,sizeof(customrec));
            end;
          help_page:=mhelpnr+helpnr;
          maussel:=false;
          end
        else
          testfndef;

        if (counter>0) and ((t='-') or (t='+')) then
          _count_
        else if t>=' ' then
        begin
          if replace then
            s:=mask
          else
            s:=cont;
          if autoup then t[1]:=UpCase(t[1]);
          if autodown then t[1]:=LoCase(t[1]);
          if convcolon and (t[1]=',') then t[1]:='.';
          if checkbutt and (t=' ') then
          begin
            if cont=yesno[1] then
              s:=yesno[2]
            else
              s:=yesno[1];
            if xtest1(s) then cont:=s;
          end
          else
          begin
            if allowed='' then
              valchar:=(pos(t,allchar)>0)
            else
              valchar:=(pos(t,allowed)>0);
            if valchar then
            begin
              if (mask='') and (insert_mode or (p+ax>length(s))) then
              begin
                insert(t,s,p+ax);
                s:=copy(s,1,maxlen);
              end else
                s[p+ax]:=t[1];
              if xtest1(s) then begin
                cont:=s;
                rechts;
                end;
              if replace then replace:=false;
              end;
            end;
          end;

        if (t=^Y) and not nonedit then begin
          cont:=mask;
          if xtest1(cont) then;      { leer ist immer zulÑssig! }
          p:=1; ax:=0;
          replace:=false;
          end;

        if (t=^J) and not quitfn(false,modified) then t:='';
        if (t=keyesc) and not quitfn(true,modified) then t:='';

        if ((t=^J) or ((t=keyesc) and keeponesc)) then begin
          myp:=yp;
          yp:=1;
          while (yp<=felder) and
                (not fld[yp]^.enabled or valid(fld[yp],false)) do inc(yp);
          i:=yp; yp:=myp;
          if i<=felder then begin
            t:='';
            movefield(i-yp);
            end;
          end;
        end;
    until (t=keyesc) or (t=^J); { main loop }
    exitmask;

    if dopush then wpop;
    mauszuu:=mzu; mauszuo:=mzo;
    mauszul:=mzl; mauszur:=mzr;

    brk:=(t=keyesc);
    if not brk or keeponesc then
      returnfields;
    editing:=false;
    end;
  help_page:=helpsave;
end;   { readHmask }
{
  $Log$
  Revision 1.34  2001/03/19 12:43:04  cl
  - fixed missing ,

  Revision 1.33  2001/03/19 12:17:02  cl
  - changed #$9E back to 'x' due to charset problems

  Revision 1.32  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.31  2001/03/04 23:15:34  cl
  - reformatted some code and added comments
  - fixed bug: quitfn not called under certain circumstances

  Revision 1.30  2001/01/04 09:59:11  mk
  - limited length of line to ScreenWidth - 10

  Revision 1.29  2000/12/29 22:29:04  mk
  CL:- fixes for UUCP config dialogue

  Revision 1.28  2000/10/17 10:05:41  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.27  2000/08/08 13:18:13  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.26  2000/08/06 10:17:27  mk
  - AnsiString fix

  Revision 1.25  2000/07/27 10:12:58  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.24  2000/07/21 21:17:43  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.23  2000/07/21 13:14:09  hd
  - Fix: Strings in der Maske
  - Fix: Einige Datenbankzugriffe wegen AnsiString

  Revision 1.22  2000/07/19 13:42:48  hd
  - new/dispose durch get-/freemem ersetzt

  Revision 1.21  2000/07/13 17:15:15  mk
  - noch einige ^String-spezifische Probleme beseitigt

  Revision 1.20  2000/07/13 10:23:44  mk
  - Zeiger auf Strings entfernt

  Revision 1.19  2000/07/05 09:27:08  hd
  - AnsiString-Anpassung

  Revision 1.18  2000/07/04 12:04:17  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.17  2000/07/03 16:20:02  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.16  2000/07/02 14:24:49  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.15  2000/06/24 14:10:26  mk
  - 32 Bit Teile entfernt

  Revision 1.14  2000/06/23 15:59:12  mk
  - 16 Bit Teile entfernt

  Revision 1.13  2000/05/06 17:29:20  mk
  - DOS DPMI32 Portierung

  Revision 1.12  2000/04/13 12:48:32  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/03/02 18:32:23  mk
  - Code ein wenig aufgeraeumt

  Revision 1.10  2000/03/01 13:17:41  jg
  - Ukonv Aufrufe benutzen jetzt High() fuer Maxlaenge
  - STRG + INS funktioniert in Texteingabefeldern wie STRG+C

  Revision 1.9  2000/02/26 11:00:24  jg
  - Strg+C (Kopieren ins Clipboard) in Texteingabefenstern
  - OLH aktualisiert.

  Revision 1.8  2000/02/25 18:30:20  jg
  - Clip2string sauberer gemacht
  - Menues: STRG+A entfernt, STRG+V kann jetzt auch einfuegen

  Revision 1.7  2000/02/24 20:27:53  jg
  -Schoenheitsfix: neuerstellte Eintrae in xp9.unisel-Boxen
   Eintraege am Anfang der Liste werden sofort angezeigt
  -MiniBugfix: STRG+U in Eingabeboxen umgelegt auf STRG+A
   (STRG+U entsprach SHIFT+3)

  Revision 1.6  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}

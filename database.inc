{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ B-Tree-Routinen von DATABASE.PAS    }
{ werden $Included, wegen Platzmangel }


{ Falls der gesuchte SchlÅssel nicht im Knoten bf^ enthalten ist, }
{ liefert searchpage (in x) die Nummer des nÑchstkleineren Keys.  }
{ Das kann auch die Nummer 0 sein!                                }

procedure searchpage(bf:inodep; var searchkey:string; searchrec:longint;
                     var x:integer);
var r,l : integer;
    ke  : boolean;
begin
  l:=0;
  r:=succ(bf^.anzahl);
  found:=false;
  while (l+1<r) and not found do begin
    x:=(l+r) div 2;
    ke:=bf^.key[x].keystr=searchkey;
    if ke and ((searchrec=0) or (bf^.key[x].data=searchrec)) then
      found:=true
    else
      if (searchkey<bf^.key[x].keystr) or (ke and (searchrec<bf^.key[x].data))
      then r:=x
      else l:=x;
  end;
  if not found then
    x:=l
  else
    if searchrec=0 then
      while (x>1) and (bf^.key[x-1].keystr=searchkey) do dec(x);
end;


{ Key zusammensetzen }

procedure getkey(dbp:DB; indnr:word; old:boolean; var key:string);
var i,j : byte;
    s   : string;
    r   : real;
    rb  : barrp;
begin
  with dp(dbp)^ do
    with index^[indnr] do
      if feldanz and $80<>0 then
        if old then begin
          rb:=recbuf; recbuf:=orecbuf;
          key:=ifunc(dbp);
          recbuf:=rb;
          end
        else
          key:=ifunc(dbp)
      else begin
        key:='';
        if old then rb:=orecbuf
        else rb:=recbuf;
        for i:=1 to feldanz do
          with feldp^.feld[ifeldnr[i] and $fff] do
            case ftyp of
              1 : begin
                    FastMove(rb^[fofs],s,rb^[fofs]+1);
                    if length(s)+1>fsize then s[0]:=chr(fsize-1);
                    if ifeldnr[i] and $8000<>0 then s:=UStr(s);
                    if feldanz=i then key:=key+s
                    else key:=key+forms(s,fsize-1);
                  end;
              2 : for j:=1 to fsize do begin
                    inc(byte(key[0]));
                    key[length(key)]:=char(rb^[fofs+fsize-j]);
                  end;
              3 : begin
                    FastMove(rb^[fofs],r,6);
                    str(r:20:3,s);
                    key:=key+s;
                  end;
              4 : begin
                    for j:=1 to 4 do
                      key[length(key)+j]:=char(rb^[fofs+4-j]);
                    inc(byte(key[0]),4);
                  end;
            end;
        end;
end;


{ Index-SchlÅssel 'key' in Index Nr, 'indnr' von Datenbank 'dbp' einfÅgen }

procedure insertkey(dbp:DB; indnr:word; var key:string);

var bf        : inodep;
    risen     : boolean;
    rootsplit : boolean;
    rootitem  : inodekey;
    newroot   : inodep;

    srecno    : longint;      { gesuchte Adresse (data) }

  procedure split(bf:inodep; var item:inodekey; x:integer);
  var splititem : inodekey;
      splitbf   : inodep;
      z,n       : integer;
  begin
    AllocNode(dbp,indnr,splitbf);
    allocateIrec(dbp,indnr,splitbf^.filepos);
    with dp(dbp)^.index^[indnr] do begin
      n:=nn div 2;
      if x<n then begin
        splititem:=bf^.key[n];
        for z:=n-1 downto x+1 do
          bf^.key[z+1]:=bf^.key[z];
        bf^.key[x+1]:=item;
        end
      else
        if x>n then begin
          splititem:=bf^.key[n+1];
          for z:=n+2 to x do
            bf^.key[z-1]:=bf^.key[z];
          bf^.key[x]:=item;
          end
        else
          splititem:=item;
      splitbf^.key[0].ref:=splititem.ref;
      splititem.ref:=splitbf^.filepos;
      item:=splititem;
      for z:=n+1 to nn do
        splitbf^.key[z-n]:=bf^.key[z];
      bf^.anzahl:=n;
      splitbf^.anzahl:=nn-n;
      end;
    writenode(splitbf);
    freenode(splitbf);
  end;

  procedure update(node:longint; var rise:boolean; var risenitem:inodekey);
  var x,z   : integer;
  begin
    if node=0 then begin
      rise:=true;
      risenitem.keystr:=key;
      risenitem.data:=srecno;
      risenitem.ref:=0;
      end
    else begin
      readnode(node,bf);
      searchpage(bf,key,srecno,x);
      risen:=false;
      update(bf^.key[x].ref,risen,risenitem);
      if risen then begin
        readnode(node,bf);
        if bf^.anzahl<dp(dbp)^.index^[indnr].nn then
          with bf^ do begin
            inc(anzahl);
            for z:=anzahl-1 downto x+1 do
              key[z+1]:=key[z];
            key[x+1]:=risenitem;
            rise:=false;
            end
        else begin
          split(bf,risenitem,x);
          rise:=true;
          end;
        writenode(bf);
        end;
      end;
  end;

begin   { insertkey }
  with dp(dbp)^ do
    with index^[indnr] do begin
      srecno:=recno;
      allocnode(dbp,indnr,bf);
      allocnode(dbp,indnr,newroot);

      rootsplit:=false;
      update(rootrec,rootsplit,rootitem);
      if rootsplit then begin
        allocateIrec(dbp,indnr,newroot^.filepos);
        newroot^.anzahl:=1;
        newroot^.key[0].ref:=rootrec;
        newroot^.key[1]:=rootitem;
        writenode(newroot);
        rootrec:=newroot^.filepos;
        writeindf(dbp,indnr);
        end;

    if indnr=actindex then tiefe:=0;
    end;
  freenode(newroot);
  freenode(bf);
end;


{ Index-SchlÅssel 'key' aus Index Nr, 'indnr' von Datenbank 'dbp' lîschen }

procedure deletekey(dbp:DB; indnr:word; var key:string);
var z         : longint;
    underflow : boolean;
    bf        : inodep;
    delrec    : longint;     { Datensatz-Nr. }
    n         : word;

  procedure del(node:longint; var underflow:boolean);
  var x,z : integer;
      y   : longint;

    procedure compensate(precedent,node:longint; path:integer;
                         var underflow:boolean);
    var neighbour     : longint;
        numbf2,numbf3 : integer;
        x,z           : integer;
        bf1,bf2,bf3   : inodep;
    begin
      allocnode(dbp,indnr,bf1);
      allocnode(dbp,indnr,bf2);
      allocnode(dbp,indnr,bf3);
      readnode(node,bf1);
      readnode(precedent,bf3);
      numbf3:=bf3^.anzahl;
      if path<numbf3 then begin
        inc(path);
        neighbour:=bf3^.key[path].ref;
        readnode(neighbour,bf2);
        numbf2:=bf2^.anzahl;
        x:=(succ(numbf2)-n) div 2;
        bf1^.key[n]:=bf3^.key[path];
        bf1^.key[n].ref:=bf2^.key[0].ref;
        if x>0 then begin
          for z:=1 to x-1 do
            bf1^.key[z+n]:=bf2^.key[z];
          bf3^.key[path]:=bf2^.key[x];
          bf3^.key[path].ref:=neighbour;
          bf2^.key[0].ref:=bf2^.key[x].ref;
          numbf2:=numbf2-x;
          for z:=1 to numbf2 do
            bf2^.key[z]:=bf2^.key[z+x];
          bf2^.anzahl:=numbf2;
          bf1^.anzahl:=n-1+x;
          writenode(bf1);
          writenode(bf2);
          writenode(bf3);
          underflow:=false
          end
        else begin
          for z:=1 to n do
            bf1^.key[z+n]:=bf2^.key[z];
          for z:=path to numbf3-1 do
            bf3^.key[z]:=bf3^.key[z+1];
          bf1^.anzahl:=dp(dbp)^.index^[indnr].nn;
          bf3^.anzahl:=pred(numbf3);
          underflow:=numbf3<=n;
          writenode(bf1);
          writenode(bf3);
          releaseIrec(dbp,indnr,neighbour);
          end
        end
      else begin
        neighbour:=bf3^.key[pred(path)].ref;
        readnode(neighbour,bf2);
        numbf2:=succ(bf2^.anzahl);
        x:=(numbf2-n) div 2;
        if x>0 then begin
          for z:=n-1 downto 1 do
            bf1^.key[z+x]:=bf1^.key[z];
          bf1^.key[x]:=bf3^.key[path];
          bf1^.key[x].ref:=bf1^.key[0].ref;
          numbf2:=numbf2-x;
          for z:=x-1 downto 1 do
            bf1^.key[z]:=bf2^.key[z+numbf2];
          bf1^.key[0].ref:=bf2^.key[numbf2].ref;
          bf3^.key[path]:=bf2^.key[numbf2];
          bf3^.key[path].ref:=node;
          bf2^.anzahl:=pred(numbf2);
          bf1^.anzahl:=n-1+x;
          writenode(bf1);
          writenode(bf2);
          writenode(bf3);
          underflow:=false;
          end
        else begin
          bf2^.key[numbf2]:=bf3^.key[path];
          bf2^.key[numbf2].ref:=bf1^.key[0].ref;
          for z:=1 to n-1 do
            bf2^.key[z+numbf2]:=bf1^.key[z];
          bf2^.anzahl:=dp(dbp)^.index^[indnr].nn;
          bf3^.anzahl:=pred(numbf3);
          underflow:=numbf3<=n;
          writenode(bf2);
          writenode(bf3);
          releaseIrec(dbp,indnr,node);
          end;
        end;
      freenode(bf3); freenode(bf2); freenode(bf1);
    end;

    procedure findgreatest(node1:longint; var underflow:boolean);
    var node2 : longint;
        numbf : integer;
        bf1   : inodep;
    begin
      allocnode(dbp,indnr,bf1);
      readnode(node1,bf1);
      numbf:=bf1^.anzahl;
      node2:=bf1^.key[numbf].ref;
      if node2<>0 then begin
        findgreatest(node2,underflow);
        if underflow then
          compensate(node1,node2,numbf,underflow);
        end
      else begin
        bf^.key[x].keystr:=bf1^.key[numbf].keystr;
        bf^.key[x].data:=bf1^.key[numbf].data;
        numbf:=pred(numbf);
        underflow:=numbf<n;
        bf1^.anzahl:=numbf;
        writenode(bf1);
        writenode(bf);
        end;
      freenode(bf1);
    end;

  begin    { del }
    if node=0 then
      underflow:=false
    else begin
      readnode(node,bf);
      searchpage(bf,key,delrec,x);
      if found then begin
        y:=bf^.key[x-1].ref;
        if y=0 then begin
          dec(bf^.anzahl);
          underflow:=bf^.anzahl<n;
          for z:=x to bf^.anzahl do
            bf^.key[z]:=bf^.key[z+1];
          writenode(bf);
          end
        else begin
          findgreatest(y,underflow);
          if underflow then
            compensate(node,y,x-1,underflow);
          end;
        end
      else begin
        y:=bf^.key[x].ref;
        del(y,underflow);
        if underflow then
          compensate(node,y,x,underflow);
        end
      end
  end;

begin    { deletekey }
  allocnode(dbp,indnr,bf);
  with dp(dbp)^ do
    with index^[indnr] do begin
      n:=nn div 2;
      delrec:=recno;
      del(rootrec,underflow);
      readnode(rootrec,bf);
      if underflow and (bf^.anzahl=0) then begin
        z:=rootrec;
        if bf^.key[0].ref<>0 then begin
          readnode(bf^.key[0].ref,bf);
          rootrec:=bf^.filepos;
          end
        else
          rootrec:=0;
        releaseIrec(dbp,indnr,z);
        end;
      writeindf(dbp,indnr);
      if indnr=actindex then tiefe:=0;
      end;
  freenode(bf);
end;


{ rekursiv im Index 'indnr' von Datenbank 'dbp' nach dem SchlÅssel }
{ 'searchkey' suchen; falls rec=true, so wird zusÑtzlich nach der  }
{ Satznummer 'data' gesucht.                                       }
{ Falls gefunden, ist found=true und data=Satznummer.              }

procedure findkey(dbp:DB; indnr:word; searchkey:string; rec:boolean;
                  var data:longint);

var bf   : inodep;
    x,i  : integer;
    srec : longint;
    nf   : boolean;
    rr   : longint;

  procedure searchbtree(y:longint);
  begin
    with dp(dbp)^ do
      if y=0 then
        found:=false
      else begin
        readnode(y,bf);
        searchpage(bf,searchkey,srec,x);
        with dp(dbp)^ do begin
          inc(tiefe);
          vpos[tiefe]:=y;
          if x=bf^.anzahl then vx[tiefe]:=-x
          else vx[tiefe]:=x;
          if not found then
            searchbtree(bf^.key[x].ref)
          else
            data:=bf^.key[x].data;
          end;
        end;
  end;

  { Weitersuchen, ob im linken Teilbaum des gefundenen Nodes }
  { noch gleiche SchlÅssel existieren.                       }

  procedure searchequal;
  var ok    : boolean;
      tmark : integer;
  begin
    with dp(dbp)^ do begin
      ok:=bf^.key[x-1].ref<>0;
      tmark:=tiefe;
      while ok do begin
        if found then
          dec(vx[tiefe]);
        readnode(bf^.key[vx[tiefe]].ref,bf);
        searchpage(bf,searchkey,srec,x);
        ok:=(found and (bf^.key[x-1].ref<>0)) or
            (not found and (x=bf^.anzahl) and (bf^.key[x].ref<>0));
        inc(tiefe);
        vpos[tiefe]:=bf^.filepos;
        vx[tiefe]:=x;
        if found then begin
          tmark:=tiefe;
          data:=bf^.key[x].data;
          end;
        end;
      if tiefe>tmark then begin
        inc(vx[tmark]);
        tiefe:=tmark;
        end;
      end;
  end;

begin
  allocnode(dbp,indnr,bf);
  with dp(dbp)^ do begin
    tiefe:=0;
    if rec then srec:=data else srec:=0;
    found:=false;
    rr:=dp(dbp)^.index^[indnr].rootrec;
    if rr=0 then begin
      dBOF:=true; dEOF:=true;
      end
    else begin
      dBOF:=false; dEOF:=false;
      searchbtree(rr);

      if not found then begin            { die Tiefen-Liste auf den  }
        nf:=(vx[tiefe]<0);               { nÑchstgrîsseren SchlÅssel }
        for i:=tiefe downto 1 do         { positionieren ...         }
          if vx[i]<0 then
            if nf then dec(tiefe)
            else vx[i]:=-vx[i]
          else
            nf:=false;
        if tiefe=0 then dEOF:=true
        else begin
          inc(vx[tiefe]);
          readnode(vpos[tiefe],bf);
          data:=bf^.key[vx[tiefe]].data;
          end;
        end
      else
        for i:=1 to tiefe do
          vx[i]:=abs(vx[i]);

      if found and not rec then begin
        searchequal;
        found:=true;
        end;
      end;
    end;
  freenode(bf);
end;
{
  $Log$
  Revision 1.3  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

}
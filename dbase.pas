{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                       UNIT dbase                        *)
(*          Schnittstelle Turbo Pascal <-> dBASE           *)
(*               (c) P.Mandrella - 11/09/89                *)
(*                   Memo-Felder - 12/12/92                *)
(***********************************************************)

{$I XPDEFINE.INC }

unit dbase;

interface  {------------------------------------------------}


uses dos, typeform, xpglobal;

const  MaxFelder  = 50;                   { max Felder pro Datei          }
       memoblock  = 512;

type   FeldStr    = string[10];
       memobuf    = array[0..memoblock-1] of char;
       DbFeld     = packed record
                      name : FeldStr;     { Feldname (Gro·buchstaben!)  }
                      typ  : char;        { Feldtyp: C=String, N=Zahl   }
      { FeldlÑnge }   size : byte;        {  D=Datum, L=Boolean, M=Memo }
                      nk   : byte;        { Nachkommastellen bei Zahlen }
                      off  : smallword;
                    end;
       DbStruktur = record
                      felder : word;      { Anzahl Fehler         }
                      feld   : array[1..MaxFelder] of DbFeld;
                                          { Felderliste           }
                      datei  : file;      { I/O-Datei             }
                      memodat: file;      { DBT-Datei             }
                      hasmemo: boolean;
                      j,m,t  : byte;      { Datum d. letzten énd. }
                      recs,               { DatensÑtze            }
                      FPos   : longint;   { Datei-Zeiger          }
                      hdsize,             { Header-Grî·e + 1      }
                      rsize  : word;      { Datensatz-Grî·e + 1   }
                      buff   : pointer;   { I/O-Puffer            }
                      modi   : boolean;   { Flag f. "geÑndert"    }
                    end;

       DbPointer  = ^DbStruktur;
       PathStr    = string[79];

                                 { Result: > 0 = I/O-Fehler, s. Handbuch  }
var    DbResult   : integer;     {           0 = o.k.                     }
       DbEOF      : boolean;     {          -1 = ungÅltige Satznummer     }
                                 {          -2 = ungÅltiger Feldname      }

procedure DbUse(f:DbPointer; name:string);              { Datei îffnen    }
procedure DbClose(f:DbPointer);                         { Datei schlie·en }
procedure DbGo(f:DbPointer; p:longint);                 { Zeiger bewegen  }
procedure DbSkip(f:DbPointer);                          { 1 Satz weiter   }
function  DbRead(f:DbPointer; fname:FeldStr):string;    { Feld auslesen   }
procedure DbReadT(f:DbPointer; fname:FeldStr; var x);   { dto., TP-Format }
procedure DbReadMemo(f:DbPointer; var mf:file; fname:FeldStr); { Memo-Feld }

procedure DbCreate(f:DbPointer; name:string);           { Datei anlegen   }
procedure DbAppend(f:DbPointer);                        { Satz anhÑngen   }
procedure DbReplace(f:DbPointer; fname:FeldStr; s:string); { F. schreiben }
procedure DbReplT(f:DbPointer; fname:FeldStr; var x);   { dto., TP-Format }

procedure DbDelete(f:DbPointer);                        { Satz lîschen     }
procedure DbRecover(f:DbPointer);                       { Satz wiederherst.}
function  DbDeleted(f:DbPointer):boolean;               { Satz gelîscht ?  }

implementation  {-------------------------------------------}


type   xa     = array[0..$ff00] of byte;

var    Header : packed record
                  ID,j,m,t     : byte;
                  recs         : longint;
                  hdsize,rsize : smallword;
                  dummy        : array[1..20] of byte;
                end;

       XFeld  : packed record
                  name         : array[1..10] of char;
                  dummy1       : byte;
                  typ          : char;
                  dummy2       : array[1..4] of byte;
                  size,nk      : byte;
                  dummy3       : array[1..14] of byte;
                end;

       FeldNr : word;


procedure FeldNummer(f:DbPointer; fname:string);
var i : word;
begin
  FeldNr:=1;
  while (FeldNr<=f^.felder) and (f^.feld[FeldNr].name<>fname) do inc(FeldNr);
  if FeldNr>f^.felder then
    DbResult:=-2
  else
    DbResult:=0;
end;


{- Datei îffnen;  f   : Dateizeiger    }
{-                name: Name der Datei }

procedure DbUse(f:DbPointer; name:string);

var i : word;
    l : byte;
    o : word;

begin
  fillchar(f^,sizeof(f^),0);
  if pos('.',name)=0 then name:=name+'.dbf';
  assign(f^.datei,name);
  reset(f^.datei,1);
  DbResult:=IOResult;
  if DbResult=0 then begin
    with f^ do begin
      blockread(datei,Header,SizeOf(Header));
      j:=header.j; m:=header.m; t:=header.t;
      recs:=header.recs; rsize:=header.rsize;
      felder:=(header.hdsize-$21) div $20;
      hdsize:=header.hdsize+1;
      o:=0;
      for i:=1 to felder do begin
        blockread(datei,XFeld,SizeOf(XFeld));
        l:=1;
        while (l<=10) and (XFeld.name[l]<>#0) do inc(l);
        Move(XFeld.name[1],feld[i].name[1],l-1);
        feld[i].name[0]:=chr(l-1);
        feld[i].typ:=XFeld.typ;
        feld[i].size:=XFeld.size;
        feld[i].nk:=Xfeld.nk;
        feld[i].off:=o;
        inc(o,XFeld.size);
        end;
      modi:=false;
      DbEOF:=(recs=0);
      getmem(buff,rsize+1);
      if not DbEOF then DbGo(f,1);
      FPos:=1;
      assign(memodat,copy(name,1,length(name)-4)+'.DBT');
      reset(memodat,1);
      hasmemo:=(ioresult=0);
      end;
    DbResult:=IOResult;
    end;
end;


{- Datei schlie·en;  f : Dateizeiger }

procedure DbClose(f:DbPointer);

var ja,mo,ta,wt : rtlword;

begin
  with f^ do begin
    if modi then begin
      seek(datei,0);
      blockread(datei,Header,SizeOf(Header));
      GetDate(ja,mo,ta,wt);
      Header.j:=ja-1900; Header.m:=mo; Header.t:=ta;
      Header.recs:=recs;
      seek(datei,0);
      blockwrite(datei,header,SizeOf(Header));
      end;
    close(datei);
    if hasmemo then
      close(memodat);
    freemem(buff,rsize+1);
    end;
  DbResult:=IOResult;
end;


{- Satzzeiger bewegen;  f : Dateizeiger   }
{-                      p : neue Position }

procedure DbGo(f:DbPointer; p:longint);
begin
  with f^ do
    if (p>recs) or (p<1) then
      DbResult:=-1
    else begin
      seek(datei,hdsize+(p-1)*rsize);
      blockread(datei,buff^,rsize);
      DbResult:=IOResult;
      FPos:=p;
      end;
end;


{- Satzzeiger um 1 weiterbewegen;  f : Dateizeiger }

procedure DbSkip(f:DbPointer);
begin
  with f^ do
    if FPos>=recs then
      DbEOF:=true
    else begin
      inc(FPos);
      seek(datei,hdsize+(FPos-1)*rsize);
      blockread(datei,buff^,rsize);
      dbEOF:=false;
      end;
  DbResult:=IOResult;
end;


{- Datensatz auslesen;  f     : Dateizeiger }
{-                      fname : Feldname    }

function DbRead(f:DbPointer; fname:FeldStr):string;

var  h  : string;

begin
  with f^ do
    if FPos>recs then
      DbRead:=''
    else begin
      FeldNummer(f,fname);
      if DbResult<0 then DbRead:=''
      else begin
        Move(xa(buff^)[feld[FeldNr].off],h[1],feld[FeldNr].size);
        setlength(h,feld[FeldNr].size);
        DbRead:=h;
        DbResult:=0;
        end;
      end;
end;


{- Datensatz auslesen; RÅckgabe im Turbo-Pascal-Format }
{-   f     : Dateizeiger                               }
{-   fname : Feldname                                  }
{-   x     : RÅckgabe-Variable                         }

procedure DbReadT(f:DbPointer; fname:FeldStr; var x);
var h   : string;
    r   : real;
    res : integer;
begin
  h:=DbRead(f,fname);
  with f^ do
    if h<>'' then
      case feld[FeldNr].typ of
        'C' : begin
                while (h<>'') and (h[length(h)]=' ') do delete(h,length(h),1);
                string(x):=h;
              end;
        'N' : begin val(h,r,res); real(x):=r; end;
        'D' : string(x):=copy(h,7,2)+'.'+copy(h,5,2)+'.'+copy(h,1,4);
        'L' : boolean(x):=(h='J') or (h='T');
      end;
end;

{- Memo-Feld auslesen;  f    : Dateizeiger      }
{-                      mf   : Zieldatei, offen }
{-                      fname: Feldname         }

procedure DbReadMemo(f:DbPointer; var mf:file; fname:FeldStr);
var memonr  : string[10];
    memopos : longint;
    res,i   : integer;
    buf     : memobuf;
    rr      : word;
begin
  with f^ do
    if hasmemo and (FPos<=recs) then begin
      FeldNummer(f,fname);
      if (DbResult=0) and (feld[FeldNr].typ='M') then begin
        memonr:=dbRead(f,fname);
        while (memonr<>'') and (memonr[1]=' ') do delete(memonr,1,1);
        val(memonr,memopos,res);
        if (res=0) and (memopos*512<filesize(memodat)) then begin
          seek(memodat,memopos*512);
          repeat
            blockread(memodat,buf,memoblock,rr);
            i:=0;
            while (i<memoblock) and (buf[i]<>#26) do inc(i);
            if i>0 then
              blockwrite(mf,buf,i);
          until eof(memodat) or (i<memoblock);
          end;
        end;
      end;
end;


{- Datei anlegen;  f    : Dateizeiger }
{-                 fname: Feldname    }

procedure DbCreate(f:DbPointer; name:string);

var i : word;
    o : word;
    x : array[1..2] of char;

    ta,mo,ja,wt : rtlword;

begin
  if pos('.',name)=0 then name:=name+'.dbf';
  assign(f^.datei,name);
  rewrite(f^.datei,1);
  DbResult:=IOResult;
  if DbResult=0 then begin
    with f^ do begin
      fillchar(Header,sizeof(Header),0);
      Header.ID:=3;
      getdate(ja,mo,ta,wt);
      Header.j:=ja-1900; Header.m:=mo; Header.t:=ta;
      Header.hdsize:=felder*$20+$21;
      rsize:=1;
      for i:=1 to felder do begin
        if feld[i].typ='D' then feld[i].size:=8;
        if feld[i].typ='L' then feld[i].size:=1;
        inc(rsize,feld[i].size);
        end;
      Header.rsize:=rsize;
      blockwrite(datei,Header,SizeOf(Header));
      hdsize:=header.hdsize+1;
      o:=0;
      for i:=1 to felder do begin
        fillchar(XFeld,sizeof(XFeld),0);
        Move(feld[i].name[1],XFeld.name,length(feld[i].name));
        XFeld.typ:=feld[i].typ;
        XFeld.size:=feld[i].size;
        if feld[i].typ='N' then XFeld.nk:=feld[i].nk;
        blockwrite(datei,XFeld,SizeOf(XFeld));
        feld[i].off:=o;
        inc(o,feld[i].size);
        end;
      modi:=false;
      DbEOF:=true;
      getmem(buff,rsize+1);
      recs:=0; FPos:=1;
      x[1]:=^M; x[2]:=^Z;
      blockwrite(datei,x,2);
      end;
    DbResult:=IOResult;
    end;
end;


{- leeren Satz anhÑngen;  f : Dateizeiger }

procedure DbAppend(f:DbPointer);
begin
  with f^ do begin
    inc(recs);
    fillchar(buff^,rsize,' ');
    xa(buff^)[rsize]:=$1a;
    seek(datei,hdsize+(recs-1)*rsize-1);
    blockwrite(datei,buff^,rsize+1);
    FPos:=recs;
    modi:=true;
    end;
  DbResult:=IOResult;
end;


{- Feld schreiben;  f     : Dateizeiger  }
{-                  fname : Feldname     }
{-                  s     : neuer Inhalt }

procedure DbReplace(f:DbPointer; fname:FeldStr; s:string);
begin
  with f^ do
    if (FPos>recs) or (FPos<1) then
      DbResult:=-3
    else begin
      FeldNummer(f,fname);
      if DbResult=0 then begin
        fillchar(buff^,rsize,' ');
        Move(s[1],buff^,length(copy(s,1,feld[FeldNr].size)));
        seek(datei,hdsize+(FPos-1)*rsize+feld[FeldNr].off);
        blockwrite(datei,buff^,feld[FeldNr].size);
        modi:=true;
        DbResult:=IOResult;
        end;
      end;
end;


{- Feld schreiben; öbergabe im Turbo-Pascal-Format }
{-   f     : Dateizeiger                           }
{-   fname : Feldname                              }
{-   x     : zu schreibende Variable               }

procedure DbReplT(f:DbPointer; fname:FeldStr; var x);   { dto., TP-Format }
var h : string;
begin
  FeldNummer(f,fname);
  if DbResult=0 then
    with f^ do begin
      case feld[FeldNr].typ of
        'C' : h:=string(x);
        'N' : str(real(x):feld[FeldNr].size:feld[FeldNr].nk,h);
        'D' : begin
                h:=string(x); h:=copy(h,7,4)+copy(h,4,2)+copy(h,1,2);
              end;
        'L' : if boolean(x) then h:='T' else h:='F';
      end;
      DbReplace(f,fname,h);
      end;
end;


procedure SetDelFlag(f:DbPointer; c:char);
begin
  with f^ do begin
    if (FPos<1) or (FPos>recs) then exit;
    seek(datei,hdsize+(FPos-1)*rsize-1);
    blockwrite(datei,c,1);
    DbResult:=IOResult;
    end;
end;


{- Satz lîschen;  f : Dateizeiger }

procedure DbDelete(f:DbPointer);
begin
  SetDelFlag(f,'*');
end;


{- Satz wiederherstellen;  f : Dateizeiger }

procedure DbRecover(f:DbPointer);
begin
  SetDelFlag(f,' ');
end;


{- ist Datensatz gelîscht?  f: Dateizeiger }

function DbDeleted(f:DbPointer):boolean;
var c : char;
begin
  with f^ do
    if (FPos<1) or (FPos>recs) then DbDeleted:=false
    else begin
      seek(datei,hdsize+(FPos-1)*rsize-1);
      blockread(datei,c,1);
      DbResult:=IOResult;
      DbDeleted:=(c='*');
      end;
end;

end.
{
  $Log$
  Revision 1.8  2000/09/19 22:46:40  fe
  Ansistring-Fix

  Revision 1.7  2000/09/09 22:30:39  fe
  rtlword-Fixes

  Revision 1.6  2000/07/02 14:24:45  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.5  2000/03/17 13:12:09  mk
  - Anpassung der Records an 32 Bit

  Revision 1.4  2000/03/03 13:24:45  mk
  YUP2PKT compilierbar gemacht und in Distribution aufgenommen

  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}

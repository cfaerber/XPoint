{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - Bin„rfile-Viewer }

{$O+,B-,V-,F+}

unit xpview;

interface

uses dos,dosx,typeform,fileio,inout,database,xp0,xp1;


function ViewFile(fn:pathstr; betreffname:boolean):boolean;


implementation  { ---------------------------------------------------- }

uses xp3;


function ViewFile(fn:pathstr; betreffname:boolean):boolean;
var f    : file;
    id,s : string[80];
    rr   : word;
    prog : string[127];
    p    : byte;
    oldn : string[12];
    ext  : string[3];
    hdp  : headerp;
    hds  : longint;
    datei: string[20];
    i    : integer;

  function HasExt(ext:string):boolean;
  begin
    if right(ustr(fn),length(ext)+1)='.'+ext then
      HasExt:=true
    else
      if right(ustr(datei),length(ext)+1)='.'+ext then
        HasExt:=true
      else
        if betreffname and not dbEOF(mbase) and not dbBOF(mbase) then
          HasExt:=pos('.'+ext,ustr(dbReadStr(mbase,'betreff')))>0
        else
          HasExt:=false;
  end;

begin
  ViewFile:=false;
  UpString(fn);
  assign(f,fn);
  if existf(f) then begin
    resetfm(f,0);
    blockread(f,id[1],80,rr);
    id[0]:=chr(rr);
    close(f);
    prog:=''; ext:='';
    if betreffname and not dbEOF(mbase) and not dbBOF(mbase) then begin
      new(hdp);
      ReadHeader(hdp^,hds,false);
      datei:=ustr(hdp^.datei);
      dispose(hdp);
      end
    else
      datei:='';
    if (left(id,3)='GIF') and (viewers^[1].prog<>'') then begin
      prog:=viewers^[1].prog; ext:='GIF'; end
    else if (pos('ILBM',id) in [1..20]) and (viewers^[2].prog<>'') then begin
      prog:=viewers^[2].prog; ext:='LBM'; end
    else
      for i:=3 to maxviewers do
        if (viewers^[i].ext<>'') and (viewers^[i].prog<>'') and
           HasExt(viewers^[i].ext) then begin
          prog:=viewers^[i].prog; ext:=viewers^[i].ext;
          end;
    if prog<>'' then begin
      p:=cpos(' ',prog);
      if p=0 then s:=prog
      else s:=left(prog,p-1);
      if not exist(s) then
        s:=FSearch(s,getEnv('PATH'));
      if s<>'' then begin
        oldn:=GetFileName(fn);
        s:=fn;
        if right(fn,length(ext)+1)='.'+ext then ext:='';
        if ext<>'' then begin
          repeat
            s:=GetFileDir(s)+'pict'+formi(random(10000),4)+'.'+ext;
          until not exist(s);
          rename(f,s);
          end;
        p:=pos('$FILE',ustr(prog));
        if p=0 then prog:=prog+' '+s
        else prog:=left(prog,p-1)+s+mid(prog,p+5);
        shell(prog,500,1);
        ViewFile:=true;
        if (ext<>'') and existf(f) then  { k”nnte gel”scht worden sein... }
          rename(f,GetFileDir(fn)+oldn);
        end;
      end;
    end;
end;

end.


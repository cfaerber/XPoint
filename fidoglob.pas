{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on December, 27st 2000 by Michael Koppel <mo@openxp.de>

   This software is part of the OpenXP project (www.openxp.de).

   Global definitions, types and constants
}

{$I XPDEFINE.INC}


unit fidoglob;

interface

uses
  typeform,sysutils,fileio,xpglobal,classes;

const
{$IFDEF UnixFS }
        FidoDir_    = 'fido';
        FidoDir     = FidoDir_+DirSepa;

        FidoCfg     = 'fido.cfg';
        FidoLog     = 'xpfido.log';             // XP-FM-Logfile

        FileLists   = FidoDir+'filelist.cfg';   //
        NodelistCfg = FidoDir+'nodelst.cfg';
        NodeindexF  = FidoDir+'nodelist.idx';
        UserindexF  = FidoDir+'nodeuser.idx';
{$ELSE}
        FidoDir_    = 'FIDO';
        FidoDir     = FidoDir_+DirSepa;

        FidoCfg     = 'FIDO.CFG';
        FidoLog     = 'XPFIDO.LOG';             //XP-FM-Logfile

        FileLists   = FidoDir+'FILELIST.CFG';
        NodelistCfg = FidoDir+'NODELST.CFG';
        NodeindexF  = FidoDir+'NODELIST.IDX';
        UserindexF  = FidoDir+'NODEUSER.IDX';
{$ENDIF}
        nlNodelist=1;           //normale Nodelist
        nlPoints24=2;           //Pointliste im P24-Format
        nlNode=3;               //Pointlist fr einen Node
        nl4DPointlist=4;        //4D-Pointliste
        nlFDpointlist=5;        //FrontDoor-Pointliste
type
        FidoAdr = record
                username   : string;  { darf aber nach FTS nicht > 36 sein (incl #0) }
                zone,net   : word;
                node,point : word;
                ispoint    : boolean;
                end;

        PNodeListItem = ^TNodeListItem;
        ///////////////////////////////////////////////////////////////////////
        TNodeListItem  = class
        public
                listfile   : string;    { Nodelisten-Datei      }
                number     : integer;   { akt. Nummer           }
                updatefile : string;    { Diff/Update-Datei     }
                updatearc  : string;    { gepackte Update-Datei }
                processor  : string;    { externer Bearbeiter   }
                DoDiff     : boolean;
                DelUpdate  : boolean;   { Diff loeschen }
                format     : byte; { NL, P24, 3=PVT, 4=4D, 5=FD }
                zone,net,node : word;
                sort       : longint;   { Temporaerfeld }
                end;

        ///////////////////////////////////////////////////////////////////////
        //Nodelisten Verwaltung
        TNodeList  = class
        public
                mEntrys         :TList;                 //die einzelnen Listen
                mOpen           :boolean;
                ///////////////////////////////////////////
                constructor     Create;
                procedure       LoadConfigFromFile;
                procedure       SaveConfigToFile;               // NODELST.CFG speichern
                procedure       AddEntry( PNLItem :PNodeListItem);
                function        GetMainNodelist: integer;
                function        GetFileName(n:integer):string;
                end;
        ///////////////////////////////////////////////////////////////////////

procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);

var
        DefaultZone : word;           { Fido - eigene Zone }
        DefaultNet  : word;           {      - eigenes Net }
        DefaultNode : word;           {      - eigener Node}


implementation
///////////////////////////////////////////////////////////////////////////////
// begin TNodList

constructor TNodeList.Create;
begin
        mEntrys:=TList.Create;
        mOpen:=false;               // Nodelist(en) vorhanden & geoeffnet
end;

procedure TNodeList.LoadConfigFromFile;       { NODELST.CFG laden }
var t  : text;
    s  : string;
    ss : string[20];
    p  : byte;
    fa : fidoadr;
    Item: PNodeListItem;
begin
  create;                               // call first constructor
  assign(t,NodelistCfg);
  if existf(t) then begin
    reset(t);
    while not eof(t) do
    begin
      new(Item);
      item^:=TNodeListItem.Create;
      mEntrys.Add(Item);
      with Item^ do begin
        repeat
          readln(t,s);
          p:=cpos('=',s);
          if p>0 then begin
            ss:=LowerCase(LeftStr(s,p-1));
            s:=mid(s,p+1);
            if ss='listfile'       then listfile:=s else
            if ss='number'         then number:=minmax(ival(s),0,999) else
            if ss='updatefile'     then updatefile:=s else
            if ss='delupdate'      then delupdate:=(UpperCase(s)='J') else
            if ss='updatearchive'  then updatearc:=s else
            if ss='process-by'     then Processor :=s else
            if ss='dodiff'         then dodiff:=(UpperCase(s)='J') else
            if ss='format'         then format:=minmax(ival(s),0,6) else
            if ss='zone'           then zone:=minmax(ival(s),0,32767) else
            if ss='address'        then begin
              SplitFido(s,fa,2);
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              end;
            end;
        until eof(t) or (s='');
        if (format<1) or (format>5) then
        begin
          mEntrys.Remove(Item);
          Dispose(Item);
        end;
      end;  { with }
    end;  { while }
    close(t);
  end;
end;

procedure TNodeList.SaveConfigToFile;                    { NODELST.CFG speichern }
var t : text;
    i : integer;
begin
  assign(t,NodelistCfg);
  rewrite(t);
  for i:=0 to mEntrys.Count - 1 do
  with PNodeListItem(mEntrys[i])^ do
  begin
    writeln(t,'Listfile=',listfile);
    if pos('###',listfile)>0 then
      writeln(t,'Number=',number);
    if updatefile<>'' then writeln(t,'UpdateFile=',updatefile);
    if updatearc<>''  then writeln(t,'UpdateArchive=',updatearc);
    if updatefile<>'' then writeln(t,'DelUpdate=',iifc(delupdate,'J','N'));
    if processor<>'' then writeln(t,'process-by=',processor);
    writeln(t,'DoDiff=',iifc(dodiff,'J','N'));
    writeln(t,'Format=',byte(format));
    case format of
      nlNodelist     : if zone>0 then writeln(t,'zone=',zone);
      nlPoints24,
      nl4DPointlist  : writeln(t,'zone=',zone);
      nlNode         : writeln(t,'address=',zone,':',net,'/',node);
    end;
    writeln(t);
    end;
  close(t);
end;

function TNodeList.GetMainNodelist: integer;
begin
  Result:=mEntrys.Count-1;
  while (Result>=0) and (PNodeListItem(mEntrys[Result])^.listfile <>'NODELIST.###') do
    dec(Result);
end;

function  TNodeList.GetFileName(n:integer):string;
var p : byte;
begin
  if n>=mEntrys.Count then
    result:=''
  else
    with PNodeListItem(mEntrys[n])^ do
    begin
      p:=pos('###',listfile);
      if p=0 then
        result:=listfile
      else
        result:=LeftStr(listfile,p-1)+formi(number,3)+mid(listfile,p+3);
    end;
end;

procedure TNodeList.AddEntry(PNLItem : PNodeListItem);       //
var
  i,j : integer;
begin

  mEntrys.Add(PNLItem);                         // merge entry

  for i:=0 to mEntrys.Count - 1 do              // and sort Dateigr”áe sortieren
    PNodeListItem(mEntrys[i])^.sort:=_filesize(FidoDir+ GetFilename(i));
  for i:=0 to mEntrys.Count - 1 do
    for j:=mEntrys.Count - 1 downto 1 do
      if PNodeListItem(mEntrys[j])^.sort>PNodeListItem(mEntrys[j-1])^.sort then
        mEntrys.Exchange(j, j-1);
end;

//end TNodeList
///////////////////////////////////////////////////////////////////////////////
//

procedure splitfido(adr:string; var frec:fidoadr; defaultzone:word);
var p1,p2,p3 : byte;
    res      : integer;
    l        : longint;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos('@',adr);
    if p1>0 then begin
      username:=trim(LeftStr(adr,p1-1));
      delete(adr,1,p1);
      end;
    adr:=trim(adr);
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if p3=0 then p3:=cpos(',',adr);
    if p1+p2=0 then begin
      zone:=DefaultZone;
      net:=2;
      if p3>0 then begin
        if p3>1 then
          node:=ival(LeftStr(adr,p3-1))
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
          zone:=minmax(ival(LeftStr(adr,p1-1)),0,65535);
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

end.

{
  $Log$
  Revision 1.7  2000/12/29 16:44:25  mo
  - class TNodeList, new procedure AddEntry

  Revision 1.6  2000/12/29 11:08:17  mo
  -nodelist.cfg rerenamed in nodlst.cfg

  Revision 1.5  2000/12/28 23:12:04  mo
  - class TNodeList ergänzt

  Revision 1.4  2000/12/28 10:59:13  mk
  - added GPL and CVS Info

}

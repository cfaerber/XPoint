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

{$I xpdefine.inc}


unit fidoglob;

interface

uses
  classes,
  addresses,
  xpglobal;

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
        nlNode=3;               //Pointlist fuer einen Node
        nl4DPointlist=4;        //4D-Pointliste
        nlFDpointlist=5;        //FrontDoor-Pointliste
type
//      FidoAdr = record
//              username   : string;  { darf aber nach FTS nicht > 36 sein (incl #0) }
//              zone,net   : xpWord;
//              node,point : xpWord;
//              ispoint    : boolean;
//              end;

        ///////////////////////////////////////////////////////////////////////
        // Nodelistenverwaltung
        TNodeListItem  = class
        protected
                fListfile   : string;    { Nodelisten-Datei      }
                fnumber     : integer;   { akt. Nummer           }
                fzone,fnet,fnode : xpWord;
                fsort       : longint;  // Temporaerfeld

        public
                fformat     : byte;     // NL, P24, 3=PVT, 4=4D, 5=FD }
                fDelUpdate  : boolean;   { Diff loeschen }
                fupdatefile : string;    { Diff/Update-Datei     }
                fprocessor  : string;    { externer Bearbeiter   }
                fupdatearc  : string;    { gepackte Update-Datei }
                fDoDiff     : boolean;
        public
                property  Listfile    :string   read fListfile   write fListfile;
                property  number      :integer  read fnumber     write fnumber;
                property  format      :byte     read fformat     write fformat;
                property  zone        :xpWord     read fzone       write fzone;
                property  net         :xpWord     read fnet        write fnet;
                property  node        :xpWord     read fnode       write fnode;
                property  updatefile  : string  read fupdatefile write fupdatefile;
                property  processor   : string  read fprocessor  write fprocessor;
                property  updatearc   : string  read fupdatearc  write fupdatearc;
                property  DoDiff      : boolean read fDoDiff     write fDoDiff;


          end;
        ///////////////////////////////////////////////////////////////////////
        //Nodelisten Verwaltung
        TNodeList  = class(TList)
        private
                fOpen           :boolean;
        protected

        public
                constructor     Create;
                procedure       LoadConfigFromFile;
                procedure       SaveConfigToFile;               // NODELST.CFG speichern
                procedure       Add(NLItem :TNodeListItem);     // fuege NL hinzu und sortiere
                function        GetMainNodelist: integer;
                function        GetFileName(n:integer):string;
                function        GetItem(Index: integer): TNodeListItem;
                procedure       ClearItems;


                property        Open  : boolean read fOpen   write fOpen;

              end;
        ///////////////////////////////////////////////////////////////////////

// procedure splitfido(adr:string; var frec:fidoadr; defaultzone:xpWord);

function FTNParse(addr: string; var d: string; var z,n,f,p: integer): boolean; overload;
function FTNParse(addr: string; var d: string; var z,n,f,p: integer; DefaultZone: integer): boolean; overload;
function FTNForm(const Domain: string; Zone,Net,Node,Point: integer): string;
function FTNNormaliseAddress(const addr: string):string; overload;
function FTNNormaliseAddress(const addr: string; DefaultZone: Integer):string; overload;
function FTNIsValidAddress(const addr: string): boolean;

type
  TFTNAddress = class(TEmailAddress)
  private
    F4DOk:      boolean;
    FUserName:  string;
    FDomain:    string;
    FZone:      integer;
    FNode:      integer;
    FNet:       integer;
    FPoint:     integer;

    FZC:        string;

  private
    procedure _internal_Mk4d;
    procedure _internal_MkZC;
    
  protected  
    function GetZCAddress: string; override;
    function GetXPAddress: string; override;
    function GetRFCAddress: string; override;

    function GetUsername: string; override;
    procedure SetUsername(const NewValue: string); override;
    function GetDomain: string; procedure SetDomain(NewValue:string);
    function GetZone: integer; procedure SetZone(NewValue:integer);
    function GetNode: integer; procedure SetNode(NewValue:integer);
    function GetNet:  integer; procedure SetNet (NewValue:integer);
    function GetPoint:integer; procedure SetPoint(NewValue:integer);
    function GetFidoAddr:string; procedure SetFidoAddr(NewValue:string);

    function GetIsPoint: boolean; procedure SetIsPoint(NewValue:boolean);
    function GetIsValid: boolean;
    
  private
    constructor _Create(const addr: string); overload;
    constructor _Create(CopyFrom: TFTNAddress); overload;
  public
    constructor Create(const addr: string; DefaultZone: Integer); overload;
    class function Create(const addr: string):TFTNAddress; overload;
    class function Create(CopyFrom: TFTNAddress):TFTNAddress; overload;
    constructor Create(const user,d: string; z,n,f,p: integer); overload;

    procedure Clear;

    property Domain: string read GetDomain write SetDomain;
    property Zone:  integer read GetZone write SetZone;
    property Net:   integer read GetNet  write SetNet;
    property Node:  integer read GetNode write SetNode;
    property Point: integer read GetPoint write SetPoint;

    property FidoAddr: string read GetFidoAddr write SetFidoAddr;
    property IsPoint: boolean read GetIsPoint write SetIsPoint;    
    property Valid: boolean read GetIsValid;
  end;

var
        DefaultZone : Integer;           { Fido - eigene Zone }
        DefaultNet  : Integer;           {      - eigenes Net }
        DefaultNode : Integer;           {      - eigener Node}


implementation

uses
  sysutils,
  rfc2822,
  typeform,fileio,xp0;

{ TNodeList }

///////////////////////////////////////////////////////////////////////////////
// begin TNodList
constructor TNodeList.Create;
begin
        inherited Create;;
        fOpen:=false;                   // Nodelist(en) vorhanden & geoeffnet
end;
procedure TNodeList.LoadConfigFromFile;       { NODELST.CFG laden }
var t     : text;
    s,dd  : string;
    dp    : integer;
    ss    : string[20];
    p     : byte;
    faddr : TFTNAddress;
    NlItem: TNodeListItem;
begin
  assignFile(t, NodelistCfg);
  if existf(t) then begin
    reset(t);
    while not eof(t) do
    begin
      NlItem := TNodeListItem.Create;
      with NlItem do
      begin
        repeat
          readln(t,s);
          p:=cpos('=',s);
          if p>0 then begin
            ss:=LowerCase(LeftStr(s,p-1));
            s:=mid(s,p+1);
            if ss='listfile'       then flistfile:=s else
            if ss='number'         then fnumber:=minmax(ival(s),0,999) else
            if ss='updatefile'     then fupdatefile:=s else
            if ss='delupdate'      then fdelupdate:=(UpperCase(s)='J') else
            if ss='updatearchive'  then fupdatearc:=s else
            if ss='process-by'     then fProcessor :=s else
            if ss='dodiff'         then fdodiff:=(UpperCase(s)='J') else
            if ss='format'         then fformat:=minmax(ival(s),0,6) else
            if ss='zone'           then fzone:=minmax(ival(s),0,32767) else
            if ss='address'        then begin
              FTNParse(s,dd,fzone,fnet,fnode,dp);
              end;
            end;
        until eof(t) or (s='');
        if (fformat<1) or (fformat>5) then  // ungueltiges Format
          NlItem.Free                       // wieder freigeben
        else
          Add(NlItem);
      end;
    end;  { while }
    close(t);
  end;
end;

procedure TNodeList.SaveConfigToFile;           // NODELST.CFG speichern
var t : text;
    i : integer;
begin
  assignFile(t,NodelistCfg);
  rewrite(t);
  for i:=0 to Count - 1 do
  with TNodeListItem(Items[i]) do
  begin
    writeln(t,'Listfile=',flistfile);
    if pos('###',flistfile)>0 then
      writeln(t,'Number=',fnumber);
    if fupdatefile<>'' then writeln(t,'UpdateFile=',fupdatefile);
    if fupdatearc<>''  then writeln(t,'UpdateArchive=',fupdatearc);
    if fupdatefile<>'' then writeln(t,'DelUpdate=',iifc(fdelupdate,'J','N'));
    if fprocessor<>'' then writeln(t,'process-by=',fprocessor);
    writeln(t,'DoDiff=',iifc(fdodiff,'J','N'));
    writeln(t,'Format=',byte(fformat));
    case fformat of
      nlNodelist     : if fzone>0 then writeln(t,'zone=',fzone);
      nlPoints24,
      nl4DPointlist  : writeln(t,'zone=',fzone);
      nlNode         : writeln(t,'address=',fzone,':',fnet,'/',fnode);
    end;
    writeln(t);
    end;
  close(t);
end;

function TNodeList.GetMainNodelist: integer;
begin
  Result:=Count-1;
  while (Result>=0) and ( TNodeListItem(Items[Result]).flistfile <>'NODELIST.###') do
    dec(Result);
end;

function  TNodeList.GetFileName(n:integer):string;
var p : byte;
begin
  if n>=Count then
    result:=''
  else
    with TNodeListItem(Items[n]) do
    begin
      p:=pos('###',flistfile);
      if p=0 then
        result:=flistfile
      else
        result:=LeftStr(flistfile,p-1)+formi(fnumber,3)+mid(flistfile,p+3);
    end;
end;

//Fuege neuen Eintrag hinzu und sortiere nach Groesse
procedure TNodeList.Add(NLItem : TNodeListItem);
var
  i,j : integer;
begin

  inherited Add(NLItem);

  for i:=0 to Count - 1 do              // and sort Dateigroesse sortieren
    TNodeListItem(Items[i]).fsort:=_filesize(FidoPath+ GetFilename(i));
  for i:=0 to Count - 1 do
    for j:=Count - 1 downto 1 do
      if TNodeListItem(Items[j]).fsort>TNodeListItem(Items[j-1]).fsort then
        Exchange(j, j-1);
end;

function  TNodeList.GetItem(Index: integer): TNodeListItem;
begin
  result:=Items[index];
end;

procedure TNodeList.ClearItems;
begin
  while Count > 0 do
  begin
    TNodeListItem(Items[0]).Free;
    Delete(0);
  end;
end;

//end TNodeList
///////////////////////////////////////////////////////////////////////////////
//

(*
procedure splitfido(adr:string; var frec:fidoadr; defaultzone:xpWord);
var
  p1,p2,p3 : Integer;
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
*)

function FTNParse(addr: string; var d: string; var z,n,f,p: integer): boolean; overload;
begin
  Result := FTNParse(addr,d,z,n,f,p,0);
end;

function FTNParse(addr: string; var d: string; var z,n,f,p: integer; DefaultZone: integer): boolean; overload;
var i: integer;
    s: integer;
    dd: string;
    zz,nn,ff,pp: integer;
begin
  result := false;

  s  := 9;

  dd := '';
  zz := 0;
  nn := 0;
  ff := 0;
  pp := 0;

  addr := Trim(addr);

  if (FirstChar(addr)='[') and (FirstChar(addr)=']') then
    addr := Copy(addr,2,length(addr)-2);

  for i:=Length(addr) downto 1 do
    case addr[i] of
      '0'..'9': begin {noop} end;
      'A'..'Z','a'..'z','-': if not (s in [9,4]) then exit;

      '@':      if s in [9] then
                begin
                  dd := Trim(Mid(addr,i+1));
                  s := 0;
                  SetLength(addr,i-1);
                end;

      '.':      if s in [9,0] then
                begin
                  pp := IVal(Mid(addr,i+1));
                  s:=1;
                  SetLength(addr,i-1);
                end else
                  exit;
                
      '/':      if s in [9,0,1] then
                begin
                  ff := IVal(Mid(addr,i+1));
                  s:=2;
                  SetLength(addr,i-1);
                end else
                  exit;
        
      ':':      if s in [2] then
                begin
                  nn := IVal(Mid(addr,i+1));
                  s:=3;
                  SetLength(addr,i-1);
                end else
                  exit;
      '#':      if(s in [3])and(dd='')then
                begin
                  zz := IVal(Mid(addr,i+1));
                  s:=4;
                  SetLength(addr,i-1);
                end;
      else 
        exit;
  end;

  if s=2 then
    nn := Ival(addr)
  else
  if s=3 then
    zz := IVal(addr)
  else
  if s=4 then
    dd := Trim(addr)
  else 
    exit;

  d := dd;

  z := zz;
  n := nn;
  f := ff;
  p := pp;

  result := true;
end;

function FTNForm(const Domain: string; Zone,Net,Node,Point: integer): string;
begin
  if (Net=0) and (Node=0) then
    result := ''
  else
    result :=  
      iifs(domain<>'',Domain+'#','')+
      iifs(Zone>0,StrS(Zone)+':','')+
      StrS(Net)+'/'+
      StrS(Node)+
      iifs(Point>0,'.'+StrS(Point),'');
end;

function FTNNormaliseAddress(const addr: string):string; overload;
begin
  Result := FTNNormaliseAddress(addr,0);
end;

function FTNNormaliseAddress(const addr: string; DefaultZone:Integer):string;
var I:  Integer;
    Domain: string;
    Zone,Net,Node,Point: Integer;

begin
  I := CPos('@',Addr);

  // Username@Domain#Zone:Net/Node.Point
  // Username@Zone:Net/Node.Point@DOMAIN
  // Username@Zone:Net/Node.Point
  // Zone:Net/Node.Point  
  if FTNParse(Mid(Addr,i+1),Domain,Zone,Net,Node,Point,DefaultZone) then
    Result := LeftStr(Addr,Min(36,I))+'@'+FTNForm(Domain,Zone,Net,Node,Point)
  else

  // Zone:Net/Node.Point@DOMAIN
  if FTNParse(Addr,Domain,Zone,Net,Node,Point,DefaultZone) then
    Result := FTNForm(Domain,Zone,Net,Node,Point)
  else

    Result := '';
end;

function FTNIsValidAddress(const addr: string): boolean;
var I:  Integer;
    Domain: string;
    Zone,Net,Node,Point: Integer;
begin
  I := CPos('@',Addr);
  result := FTNParse(Mid(Addr,i+1),Domain,Zone,Net,Node,Point,DefaultZone) or
            FTNParse(Addr,Domain,Zone,Net,Node,Point,DefaultZone);
end;

// -- TFTNAddress -------------------------------------------------

procedure TFTNAddress._internal_Mk4D;
var i: integer;
begin
  if F4DOk then exit;
  i := CPos('@',FZC);
  if not FTNParse(Mid(FZC,i+1),fdomain,fzone,fnet,fnode,fpoint) then
  begin
    fdomain := '';    
    fzone := 0;
    fnet  := 0;
    fnode := 0;
    fpoint:= 0;
  end; 
  FUsername := LeftStr(FZC,i-1);
  F4dOk := true;
end;

procedure TFTNAddress._internal_MkZC;
begin
  if FZC<>'' then exit;
  FZC := FUsername+'@'+GetFidoAddr;
end;

function TFTNAddress.GetUsername: string;
begin
  _internal_Mk4d;
  result := FUserName;
end;
    
procedure TFTNAddress.SetUsername(const NewValue: string);
begin
  _internal_Mk4d;
  FUsername := NewValue;
  FZC := '';
end;

function TFTNAddress.GetDomain: string; 
begin
  _internal_Mk4d;
  result := FDomain;
end;

procedure TFTNAddress.SetDomain(NewValue: string);
begin
  _internal_Mk4d;
  FDomain := NewValue;
  FZC := '';
end;

function TFTNAddress.GetZone: integer; 
begin
  _internal_Mk4d;
  result := FZone;
end;

procedure TFTNAddress.SetZone(NewValue:integer);
begin
  _internal_Mk4d;
  FZone := NewValue;
  FZC := '';
end;

function TFTNAddress.GetNode: integer;
begin
  _internal_Mk4d;
  result := FNode;
end;

procedure TFTNAddress.SetNode(NewValue:integer);
begin
  _internal_Mk4d;
  FNode := NewValue;
  FZC := '';
end;

function TFTNAddress.GetNet:  integer;
begin
  _internal_Mk4d;
  result := FNet;
end;

procedure TFTNAddress.SetNet (NewValue:integer);
begin
  _internal_Mk4d;
  FNet := NewValue;
  FZC := '';
end;

function TFTNAddress.GetPoint:integer;
begin
  _internal_Mk4d;
  result := FPoint;
end;

procedure TFTNAddress.SetPoint(NewValue:integer);
begin
  _internal_Mk4d;
  FPoint := NewValue;
  FZC := '';
end;

function TFTNAddress.GetFidoAddr:string; 
begin
  _internal_Mk4D;
  Result := FTNForm(FDomain,FZone,FNet,FNode,FPoint);
end;

procedure TFTNAddress.SetFidoAddr(NewValue:string);
begin
  if F4DOk then
  begin
    FZC := FUserName+'@'+NewValue;
    F4DOk := false;
  end else
  begin
    FZC := LeftStr(FZC,CPosX('@',FZC)-1)+'@'+NewValue;
  end;
end;

function TFTNAddress.GetIsPoint: boolean; 
begin
  _internal_Mk4d;
  Result := FPoint<>0;
end;

procedure TFTNAddress.SetIsPoint(NewValue:boolean);
begin
  if NewValue = false then
    SetPoint(0);
end;

function TFTNAddress.GetIsValid: boolean; 
begin
  if FZC<>'' then _internal_Mk4D;
  result := F4Dok and((FNet<>0)or(FNode<>0))
end;

function TFTNAddress.GetZCAddress: string;
begin
  _internal_MkZC;
  result := FZC;
end;

function TFTNAddress.GetXPAddress: string;
begin
  result := GetZCAddress;
end;

function TFTNAddress.GetRFCAddress: string;
begin
  _internal_Mk4d;
  result := RFCNormalizeAddress('"'+FUsername+'"@['+GetFidoAddr+']','');
end;

constructor TFTNAddress.Create(const addr: string; DefaultZone: Integer);
begin
  FZC := addr;
  _internal_Mk4d;
  if FZone=0 then begin
    FZone := DefaultZone;
    FZC := '';
  end;
end;

class function TFTNAddress.Create(const addr: string): TFTNAddress;
begin 
  result := TFTNAddress._Create(addr); 
end;

constructor TFTNAddress._Create(const addr: string);
begin
  FZC := addr;
end;

class function TFTNAddress.Create(CopyFrom: TFTNAddress): TFTNAddress;
begin 
  result := TFTNAddress._Create(CopyFrom); 
end;

constructor TFTNAddress._Create(CopyFrom: TFTNAddress);
begin
  FUsername:=CopyFrom.FUserName;
  FDomain := CopyFrom.FDomain;
  FZone := CopyFrom.FZone;
  FNet  := CopyFrom.FNet;
  FNode := CopyFrom.FNode;
  FPoint:= CopyFrom.FPoint;
  F4dOK := CopyFrom.F4dOk;

  FZC   := CopyFrom.FZC;
end;

constructor TFTNAddress.Create(const user,d: string; z,n,f,p: integer);
begin
  FUsername := user;

  FDomain := d;
  FZone := z;
  FNet  := n;
  FNode := f;
  FPoint:= p;

  F4dOK := true;
end;

procedure TFTNAddress.Clear;
begin
  FZC := '';
  F4DOK := false;
end;

{
  $Log$
  Revision 1.22  2003/01/13 22:05:19  cl
  - send window rewrite - Fido adaptions
  - new address handling - Fido adaptions and cleanups

  Revision 1.21  2002/12/21 05:37:49  dodi
  - removed questionable references to Word type

  Revision 1.20  2002/12/06 14:27:26  dodi
  - updated uses, comments and todos

  Revision 1.19  2002/12/04 16:56:57  dodi
  - updated uses, comments and todos

  Revision 1.18  2002/07/22 10:10:32  mk
  - fixed TNodeList.LoadConfigFromFile
    add was called before NlItem: TNodeListItem; was filled,
    but add uses this information internally to sort nodelist entries

  Revision 1.17  2002/04/14 11:01:54  mk
  - fixed memory leaks

  Revision 1.16  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.15  2001/08/10 20:57:56  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.14  2001/07/31 16:18:38  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.13  2001/07/31 13:10:31  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.12  2001/01/08 07:37:10  mo
  -Add inherited,  delete wrong create -TnodeList(Tlist)

  Revision 1.11  2001/01/07 20:03:02  mo
  no message

  Revision 1.10  2001/01/07 12:34:37  mo
  - einig  Änderungen an TNodeList

  Revision 1.9  2001/01/06 21:13:35  mo
  - Änderung an TnodeListItem

  Revision 1.8  2001/01/06 17:18:07  mk
  - fixed some TNodeListItem-Bugs

  Revision 1.7  2000/12/29 16:44:25  mo
  - class TNodeList, new procedure AddEntry

  Revision 1.6  2000/12/29 11:08:17  mo
  -nodelist.cfg rerenamed in nodlst.cfg

  Revision 1.5  2000/12/28 23:12:04  mo
  - class TNodeList ergänzt

  Revision 1.4  2000/12/28 10:59:13  mk
  - added GPL and CVS Info

}
end.


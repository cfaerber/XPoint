{   $Id$

    Copyright (C) 2000-2001 OpenXP team <http://www.openxp.de>
    Copyright (C) 2001 Claus F"arber <http://www.faerber.muc.de>

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

{$I XPDEFINE.INC }

unit xpextractmessage_mime;

{ --------------------------- } interface { -------------------------- }

uses xpglobal,classes,mime,xpheader,contnrs;

{
  TContentPart
  \-- TContentPartNode
}

type 
  TContentPart = class
  private
    FHeaders:TStringList;
    FData:   TStream;

    FParent: TContentPart;
    
    FCtype:  TMimeContentType;
    FEncod:  TMimeEncoding;
    FDispo:  TMimeDisposition;
    FDescr:  String;

  protected
    constructor Create;
  
  public
    class function CreateFromMessage(hd:THeader;data:TStream):TContentPart;
    class function CreateFromMIME(const mime:mimedata;data:TStream):TContentPart;
    
    destructor Destroy; override;

    property Parent:  TContentPart read FParent;
    property Data:    TStream read FData;

    property ContentType: TMimeContentType read FCType;
    property ContentEncoding: TMimeEncoding read FEncod write FEncod;
    property ContentDisposition: TMimeDisposition read FDispo;    
    property ContentDescription: string read FDescr write FDescr;
  end;

  TContentVorspannNachspann = class(TContentPart)
  public
    constructor Create(description: string;data: TStream); 
  end;

  TContentVorspann = class(TContentVorspannNachspann)
  public
    constructor Create(data:TStream);  
  end;

  TContentNachspann = class(TContentVorspannNachspann)
  public
    constructor Create(data:TStream);  
  end;

  TContentNode = class(TContentPart)
  private
    FList: TObjectList;

    function GetPart(Index:Integer):TContentPart;
    function GetCount: Integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function Add(Item: TContentPart): Integer;    
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: TContentPart;
    procedure Insert(Index: Integer; Item: TContentPart);
    function Last: TContentPart;
    procedure Move(CurIndex, NewIndex: Integer);

    property Items[Index: Integer]: TContentPart read GetPart;
    property Count: Integer read GetCount;
  end;

  TContentMessage = class(TContentNode)
  private
    FHeader: THeader;
  public
    constructor Create;
    destructor Destroy; override;
  
    property Header:THeader read FHeader;
  end;

function SelectMessagePart(select,selectmore:boolean; index:integer;
  root:TContentPart):TContentPart;

procedure mimedecode;    { Nachricht/Extract/MIME-Decode }

{ ------------------------ } implementation { ------------------------ }

uses 
  sysutils,
  typeform,
  rfc2822,
  xpstreams,
  xp0,
  xp1,
  lister,
  keys,
  xpkeys,
  resource;

constructor TContentPart.Create;
begin
  FHeaders := TStringList.Create;
  FCtype   := TMimeContentType.Create('');
  FDispo   := TMimeDisposition.Create('');
end;

class function TContentPart.CreateFromMIME(const mime:mimedata;data:TStream):TContentPart;
var main,sub: string;
    os: TStream;
    oldpos: Longint;

  function Multipart: TContentNode;
  var boundary, t, t2: string;
      vstart,start,stop: Longint;
      i,nr: integer;
      hdp: TRFCHeaderParser;

      NewMime: MimeData;
      MimeVorspann: boolean;
      TestMimeVorspann: boolean;
  begin
    result := TContentNode.Create;
    result.FData                        := Data;
    result.ContentType.AsString         := mime.ctype;
    result.ContentDisposition.AsString  := mime.disposition;
    result.ContentEncoding              := mime.encoding;
    result.ContentDescription           := mime.description;

    // Decode a multipart message if necessary 
    // NB: this means that the message is actually invalid!
    if Mime.Encoding in [MimeEncodingQuotedPrintable,MimeEncodingBase64] then
    begin
      ConnectStream(data,MimeCreateDecoder(Mime.Encoding));
      os := TTemporaryFileStream.Create;
      CopyStream(data,os);
      data.Free;
      Result.FData := os;
      result.ContentEncoding := mimeencodingbinary;
    end; 
 
    boundary := result.contenttype.boundary;

    start := data.Position;
    MimeVorspann := true;
    TestMimeVorspann := true;
    nr := 0;
   try
    repeat
      stop:= data.Position-2;
      t   := readln_s(data);
      t2  := Trim(t);

      if TestMimeVorspann and (t2<>'') then 
      begin
        MimeVorspann := (t2='This is a multi-part message in MIME format.') or           { diverse }
           (t2='This is a multipart message in MIME format') or             { InterScan NT }
           (t2='Dies ist eine mehrteilige Nachricht im MIME-Format.') or    { Netscape dt. }
           (t2='This is a MIME-encapsulated message') or                    { Unix..? }
           (t2='This is a MIME encoded message.') or                        { ? }
           (t2='This message is in MIME format. Since your mail reader does not understand') or { MS Exchange }
           (t2='This message is in MIME format.  The first part should be readable text,');   { elm }
        TestMimeVorspann := false;
      end;

      if (Boundary='') and (LeftStr(t2,2)='--') then
        Boundary:=Mid(t2,3);      

      if ((Length(t2)-Length(Boundary)) in [2,4]) and // this is only
         (t2[1]='-') and (t2[2]='-') and              // optimization
         ( (t2 = '--' + Boundary) or             // and this is the
           (t2 = '--' + Boundary + '--') ) then  // real test
      begin
        case nr of
          0: if not MimeVorspann then
               result.add(TContentVorspann.Create(
                 TPartialStream.CreateWithStream(data,start,stop)));
        else result.add(TContentPart.CreateFromMIME(NewMIME,
               TPartialStream.CreateWithStream(data,start,stop)));
        end;

        if t2 = '--' + Boundary + '--' then
        // end boundary
        begin
          if data.size-data.position>5 then
            result.add(TContentNachspann.Create(
              TPartialStream.CreateWithStream(data,data.position,data.size)));
          break;
        end else 
        // new part
        begin
          NewMime.CType := iifs(sub='DIGEST','message/rfc822','text/plain');
          NewMime.Description := '';
          NewMime.Disposition := '';
          NewMime.Encoding := MimeEncoding7Bit;
          NewMime.Cid := '';

          hdp := TRFCHeaderParser.Create(data);
         try
          with hdp do 
            while NextLine do
              if NameUC = 'CONTENT-TYPE' then
                NewMime.CType := RFCRemoveComments(Content) else
              if NameUC = 'CONTENT-DISPOSITION' then
                NewMime.Disposition := RFCRemoveComments(Content) else
              if NameUC = 'CONTENT-TRANSFER-ENCODING' then
                NewMime.Encoding := MimeGetEncodingFromName(RFCRemoveComments(Content)) else
              if NameUC = 'CONTENT-DESCRIPTION' then begin
                NewMime.Description := Content;
                RFC2047_Decode(NewMime.Description,csCP437);
              end;
         finally
          hdp.Free;
         end;
        end;
      end;
    until false;

    try data.Seek(soFromBeginning,vstart); except end;
        
   except
    on EReadError do begin end;
   end;

    for i:= 0 to result.Count-1 do
      Result.Items[i].FParent := Result;   
  end;
    
begin
  MimeContentTypeSplit(mime.ctype,main,sub);
  main:=UpperCase(main);
  sub :=UpperCase(sub);

  OldPos := data.Position;
  try
    Data.Seek(0,soFromBeginning);
  

    if main='MULTIPART' then 
      result := Multipart
      
(*
    else if main='MESSAGE' then
  
*)
    else begin
      result:=TContentPart.Create;
      result.FData                        := Data;
      result.ContentType.AsString         := mime.ctype;
      result.ContentDisposition.AsString  := mime.disposition;
      result.ContentEncoding              := mime.encoding;
      result.ContentDescription           := mime.description;
    end; 

  finally
    data.Seek(OldPos,soFromBeginning); 
  end;
  
end;
 
class function TContentPart.CreateFromMessage(hd:THeader;data:TStream):TContentPart;
var h2: THeader;
    m2: mimedata;
(*
  function Decode:boolean;
  begin
    hd.typ     := hd.crypt.typ;    hd.crypt.typ:='';
    hd.komlen  := hd.crypt.komlen; hd.crypt.komlen:=0;
    hd.charset := hd.crypt.charset;hd.crypt.charset:='';
    hd.crypt.method:='';
  end;
*)
  function NormalPart(data:TStream):TContentPart;
  var m2:MimeData;
      ctype:TMimeContentType;
      mdisp: TMimeDisposition;
  begin
    ctype := TMimeContentType.Create(hd.mime.ctype);
   try
    if hd.charset<>'' then ctype.Charset:=ZCCharsetToMime(hd.charset);
    if hd.boundary<>'' then ctype.Boundary:=hd.boundary;

    mdisp  := TMimeDisposition.Create(hd.mime.disposition);
   try
    if hd.datei<>'' then mdisp.ParamValues['filename'] := hd.datei;
    if hd.ddatum<>'' then mdisp.ParamValues['modification-date'] := ZToRFCDate('',hd.ddatum);

    m2.mversion := ''; 
    m2.ctype    := ctype.AsString;
    m2.encoding := MimeEncodingBinary;
    m2.disposition := mdisp.AsString;
    m2.description := hd.mime.description;
    m2.cid      := hd.mime.cid;

   finally
    mdisp.Free;
   end;
   finally
    ctype.Free;
   end;

    result := TContentPart.CreateFromMIME(m2,data);
  end;

  function KomPart:TContentNode;
  var m2: MimeData;
  begin
    result := TContentNode.Create;

    m2.mversion := ''; 
    m2.ctype    := 'text/plain; charset=IBM437';
    m2.encoding := MimeEncodingBinary;
    m2.disposition := '';
    m2.description := '';
    m2.cid      := '';

    result.Add(TContentPart.CreateFromMIME(m2,
      TPartialStream.CreateWithStream(data,data.Position,data.Position+hd.komlen)));
    result.Add(NormalPart(
      TPartialStream.CreateWithStream(data,data.Position+hd.komlen,data.Position+hd.groesse)));

    result.Items[0].FParent := result;
    result.Items[1].FParent := result;
  end;
    
begin
  // The message is encoded: decode
(* if Decode then
  begin
  end else
*)

  // The message has a comment: create a node with two children
  if hd.komlen>0 then 
    result := KomPart else
    
  // MIME-encoded Message
  if hd.typ='M' then
    result := CreateFromMime(hd.mime,data) else
    
  // other message
    result := NormalPart(data);

  
    
end;

destructor TContentPart.Destroy;
begin
  FData.Free;
  FHeaders.Free;
  FCtype.Free;
  FDispo.Free;
end;

{ --- TContentVorspannNachspann -------------------------------------- }

constructor TContentVorspannNachspann.Create(description: string;data: TStream);  
begin
  inherited Create;

  FData := data;
  
  FCtype.AsString:= 'text/plain; charset=x-unknown';
  FEncod:= MimeEncodingBinary;
  FDescr:= Description;
end;

constructor TContentVorspann.Create(data:TStream);  
begin
  inherited Create(GetRes2(2440,1),data);
end;

constructor TContentNachspann.Create(data:TStream);  
begin
  inherited Create(GetRes2(2440,2),data);
end;

{ --- TContentPartNode ----------------------------------------------- }

constructor TContentNode.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TContentNode.Destroy; 
begin
  FList.Free;
  inherited Destroy;
end;

function TContentNode.GetPart(Index:Integer):TContentPart;
begin 
  result := FList[Index] as TContentPart 
end;

function TContentNode.GetCount: Integer;
begin
  result := FList.Count;
end;

function TContentNode.Add(Item: TContentPart): Integer;
begin
  result := FList.Add(Item);
end;

procedure TContentNode.Clear; 
begin
  FList.Clear;
end;

procedure TContentNode.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TContentNode.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1,Index2);
end;

function TContentNode.First: TContentPart;
begin
  result:=FList.First as TContentPart;
end;

procedure TContentNode.Insert(Index: Integer; Item: TContentPart);
begin
  FList.Insert(Index,Item);
end;

function TContentNode.Last: TContentPart;
begin
  result:=FList.Last as TContentPart;
end;

procedure TContentNode.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex,NewIndex);
end;

{ --- TContentPartMessage -------------------------------------------- }

constructor TContentMessage.Create;
begin
  FHeader:=THeader.Create;
end;

destructor TContentMessage.Destroy;
begin
  FHeader.Free;
  inherited Destroy;
end;

{$IFDEF __undefined__}
  
{ lokale Variablen von SelectMultiPart() und SMP_Keys }

const maxparts = 100;    { max. Teile in einer Nachricht }

type
  TMimeMFArray= array[1..maxparts] of multi_part;
var
  mf: TMimeMFArray;


function typname(typ,subtyp:string):string;
var s : string;
begin
  if typ='text' then s:=getres2(2440,3)         { 'Text'   }
  else if typ='image' then s:=getres2(2440,4)   { 'Grafik' }
  else if typ='video' then s:=getres2(2440,5)   { 'Video'  }
  else if typ='audio' then s:=getres2(2440,6)   { 'Audio'  }
  else if typ='application' then s:=getres2(2440,7)  { 'Datei' }
  else s:=typ;
  if subtyp='octet-stream' then subtyp:='';
  if (subtyp<>'') and (subtyp<>'plain') and (subtyp<>'octet-stream') then
    typname:=s+' ('+subtyp+')'
  else
    typname:=s;
end;

procedure m_extrakt(var mpdata:multi_part);
var fn      : string;
    useclip : boolean;
    brk,o   : boolean;
begin
  fn:=mpdata.fname;
  useclip:=true;                          { 'Nachrichtenteil extrahieren' }
  if ReadFilename(getres(2441),fn,true,useclip) then
  begin
    if not multipos(':\',fn) then fn:=ExtractPath+fn;
    if not UseClip then
    begin
      if FileExists(fn) then
      begin
        if mpdata.typ='text'then o:=false else o:=true;   {Falls vorhanden... Text: "anhaengen"}
        o:=overwrite(fn,o,brk);                           {Rest: "ueberschreiben"}
      end else
      o:=true;
    end else
      o := true; { fÅr Clipboard immer Åberschreiben }
    if not FileExists(fn) or not brk or UseClip then
      ExtractMultiPart(mpdata,fn,not o);
    if UseClip then
      WriteClipfile(fn);
  end;
end;
{$ENDIF __undefined__}

procedure SMP_Keys(Self: TLister; var t:taste);
begin
  Xmakro(t,16);                           { Macros des Archivviewer fuer das Popup benutzen }
//  if UpperCase(t)='X' then
//    m_extrakt(mf[ival(mid(Self.getselection,57))]);
end;

{$IFDEF __undefined__}

// select keys for SINGLE-PART MIME
procedure SSP_Keys(Self: TLister; var t:taste);
var OldET : byte;
begin
  Xmakro(t,16);                           { Macros des Archivviewer fuer das Popup benutzen }
  if UpperCase(t)='X' then
  begin
    OldET:=ExtraktTyp;
    ExtraktTyp:=0;                        { Als Text ohne Kopf extrahieren... }
    extrakt(1,aktdispmode,0);
    ExtraktTyp:=OldET;
    end;
end;


{ Datumsformate:         11 Jan 92 01:02 +nnnn
                    Mon, 11 Jan 1992 01:02:03 +nnnn
                    Mon Jan 11, 1992 01:02:03 +nnnn  }
{$ENDIF __undefined__}

(*
  PART
  /--+- PART
  |  +--+- PART
  |  |  \- PART
  |  +- PART
  |  \- PART
  |- PART
  \- PART
  PART
  /- PART
  \--+- PART
     \- PART
  PART
*)


(*                    Level  IsFirst  IsLast
  PART                  1       1       0
                        2       1       0
  /--+- PART            3       1       0
  |  +--+- PART         4       1       0
  |  |  \- PART         4       0       1
  |  +- PART            3       0       0
  |  \- PART            3       0       1
  |- PART               2       0       0
  \- PART               2       0       1
  PART                  1       0       0
  /- PART               2       1       0
  \--+- PART            3       1       1
     \- PART            3       0       1
  PART                  1       0       1
*)

type TSelectMessagePart = class
  public
    Part: TContentPart;
    Selectable: Boolean;
  end;

{ select:     Auswahlliste                                             }
{ selectmore: Laengere Auswahlliste                                    }

function SelectMessagePart(select,selectmore:boolean; index:integer;
  root:TContentPart):TContentPart;
 
var 
  list: TStringList;
  pa: TSelectMessagePart;
 
  Prefix: string;
  LastLevel:Integer;

  i: integer;

const 
  width=72;

  procedure MakeList(node:TContentPart;Level:Integer;IsFirst,IsLast,Signed,Encrypted: boolean);
  var i: integer;
      s,s2,s3: string;
  begin
    if Max(1,Level)<LastLevel then
    begin
      Prefix:=LeftStr(Prefix,Length(Prefix)-2);
      dec(LastLevel);
    end;

    for i:=0 to Length(Prefix)-2 do
      if Prefix[i] in [#$C0,#$C3] then
        Prefix[i]:=#$B3
      else
        Prefix[i]:=' ';

    if Length(Prefix)>=2 then
      Prefix:=LeftStr(Prefix,Length(Prefix)-2)+
        iifc(IsLast,#$c0,#$c3)+#$c4;
    
    if Max(1,Level)>LastLevel then
    begin
      Prefix:=Prefix+iifc(IsFirst,iifc(Prefix='',#$da,#$c2),iifc(IsLast,#$C0,#$C3))+#$c4;
      inc(LastLevel);
    end;

    if node is TContentNode then
      with TContentNode(node) do
        case Count of
          0: exit;
          1: MakeList(Items[0],Level,IsFirst,IsLast,Signed,Encrypted);
        else begin
               s := UpperCase(node.ContentType.SubType);
               if s='ENCRYPTED' then
                 for i:=1 to Count-1 do
                   MakeList(Items[i],Level,IsFirst and(i=1),IsLast and(i=Count-1),Signed,true) else
               if s='SIGNED' then
                 for i:=1 to Count-1 do
                   MakeList(Items[i],Level,IsFirst and(i=1),IsLast and(i=Count-1),true,Encrypted) else
               if ((s='APPLEDOUBLE') or (s='HEADER-SET')) and not selectmore then
                 MakeList(Last,Level,IsFirst,IsLast,Signed,Encrypted) else
               if (s='ALTERNATIVE') and not selectmore then 
                 MakeList(First,Level,IsFirst,IsLast,Signed,Encrypted) else
               for i:=0 to Count-1 do
                 MakeList(Items[i],Level+1,(i=0),(i=Count-1),Signed,Encrypted);          
            end;
         end
    else if (not (node is TContentVorSpannNachSpann)) or selectmore then
    begin
      s:=Prefix+' ';

      pa := TSelectMessagePart.Create;

      pa.Part       := Node;
      pa.Selectable := true;

      if node is TContentVorspannNachspann then
        s:=s+node.ContentDescription else
        
      begin
        s3:=Node.ContentType.Verb;
        s3:=Trim(LeftStr(s3,PosX(';',s3)));

        if Length(s3)>20 then
        begin
          if UpperCase(LeftStr(s3,10))='MULTIPART/'   then s3:='mul./'+Mid(s3,11) else
          if UpperCase(LeftStr(s3,12))='APPLICATION/' then s3:='app./'+Mid(s3,13) else
          if UpperCase(LeftStr(s3,6 ))='AUDIO/'       then s3:='au./' +Mid(s3, 7) else
          if UpperCase(LeftStr(s3,6 ))='IMAGE/'       then s3:='img./'+Mid(s3, 7) else
          if UpperCase(LeftStr(s3,8 ))='MESSAGE/'     then s3:='msg./'+Mid(s3, 9) else
          if UpperCase(LeftStr(s3,6 ))='MODEL/'       then s3:='mod./'+Mid(s3, 7) else
          if UpperCase(LeftStr(s3,6 ))='VIDEO/'       then s3:='vid./'+Mid(s3, 7);

        if Length(s3)>20 then
          s3:=LeftStr(s3,20-3)+'...';
        end;

        case node.ContentEncoding of
          MIMEEncoding7bit:            s3:=s3+' 7bit';
          MIMEEncoding8bit:            s3:=s3+' 8bit';
          MIMEEncodingQuotedPrintable: s3:=s3+'   QP';
          else                         s3:=s3+'  B64';
        end;

        s:=s+iifc(node.ContentDisposition.DispoType<>MimeDispositionInline,'A','I')+' ';

        if 0<Length(node.ContentDescription) then
          s2:=node.ContentDescription
        else if 0<Length(node.ContentDisposition.ParamValues['filename']) then
          s2:=fitpath(node.ContentDisposition.ParamValues['filename'],width-Length(s)-Length(s3)-2)
        else
          s2:=GetRes2(624,10); { '(intern)' }

        if Length(s2)>(width-2-Length(s3)-Length(s)-1) then
          s2:=LeftStr(s2,width-2-Length(s3)-Length(s)-1)
        else if Length(s2)<(width-2-Length(s3)-Length(s)-1) then
          s2:=s2+sp(width-2-Length(s3)-Length(s)-1-Length(s2));

        s:=s+s2+' '+s3;
      end;

      list.AddObject(s,pa);        
    end;
  end;

var
  LBox: TLister;
  
begin
  Prefix:='';
  LastLevel:=1;

  list := TStringList.Create;
 try
  MakeList(root,0,true,true,false,false);

  case List.Count of
    0: result:=nil; // WTF?
    1: result:=TSelectMessagePart(List.Objects[0]).Part;
  else 
    begin
      LBox := ListBox(Width,min(screenlines-4,List.Count),'');   { 'mehrteilige Nachricht' }
     try
      for i:=0 to List.Count-1 do 
        LBox.AddLine(List[i]);
      LBox.OnKeypressed := SMP_Keys;
      LBox.Startpos := index-1;

      if LBox.Show then
        result := nil
      else
        result:= TSelectMessagePart(List.Objects[Lbox.SelLine]).Part;

     finally
      LBox.Free;
      closebox;
     end;
    end;
  end; // case List.Count

 finally
  if assigned(list) then
    for i:=list.count-1 downto 0 do
      list.objects[i].Free;
  list.Free;
 end;
end;

{$IFDEF __undefined__}

{ Liste der Teile einer Multipart-Nachricht erzeugen; }
{ Teil aus Liste auswÑhlen                            }

procedure SelectMultiPart(select:boolean; index:integer; forceselect:boolean;
                          var mpdata:multi_part; var brk:boolean);
var   hdp      : THeader;
      hds      : longint;
      anzahl   : integer;     { Anzahl der Nachrichtenteile }
      anzahl0  : integer;     { Anzahl Nachrichtenteile ohne Gesamtnachricht }
      alter    : boolean;
      List: TLister;

  procedure MakePartlist;
  const maxlevel = 25;    { max. verschachtelte Multiparts }
        bufsize  = 2048;
  var   t      : text;
        tmp    : string;
        buf    : pointer;
        bstack : array[1..maxlevel] of string;    { Boundaries }
        bptr   : integer;
        s         : string;
        bufline   : string;
        s2        : string;
        folded    : boolean;
        firstline : string;
        _encoding   : string;
        filename    : string;
        filedate    : string;
        CharSetName: String;
        subboundary : string;
        hdline      : string;
        ctype,subtype: string;    { content type }
        bound    : string;
        parname  : string;
        parvalue : string;
        vorspann : boolean;
        n,_start : longint;
        isbound  : boolean;
        endbound : boolean;
        last     : integer;
        endhd    : boolean;
        stackwarn: boolean;

    label ende;

    procedure push(boundary:string);
    begin
      if bptr=maxlevel then begin
        if not stackwarn then
          rfehler(2405);   { 'zu viele verschachtelte Nachrichtenteile' }
        stackwarn:=true;
        end
      else begin
        inc(bptr);
        bstack[bptr]:=boundary;
        end;
    end;

    procedure pop;
    begin
      if bptr>0 then
        dec(bptr);
    end;

    procedure reset_var;
    begin
      filename:='';
      filedate:='';
      CharsetName := '';
      _encoding:='';
      ctype:='';
      subtype:='';
      subboundary:='';
    end;

    procedure GetParam;   { Content-Type-Parameter parsen }
    var p : byte;
    begin
      parname:=LowerCase(GetToken(s,'='));
      parvalue:='';
      if firstchar(s)='"' then DeleteFirstChar(s);
      p:=1;
      while (p<=length(s)) and (s[p]<>';') do begin
        if s[p]='\' then
          delete(s,p,1);     { Quote auflîsen }
        inc(p);
        end;
      parvalue:=trim(LeftStr(s,p-1));
      if lastchar(parvalue)='"' then DeleteLastChar(parvalue);
      s:=trim(mid(s,p+1));
    end;

    function MimeVorspann:boolean;
    begin
      MimeVorspann:=(firstline='This is a multi-part message in MIME format.') or           { diverse }
                    (firstline='This is a multipart message in MIME format') or             { InterScan NT }
                    (firstline='Dies ist eine mehrteilige Nachricht im MIME-Format.') or    { Netscape dt. }
                    (firstline='This is a MIME-encapsulated message') or                    { Unix..? }
                    (firstline='This is a MIME encoded message.') or                        { ? }
                    (firstline='This message is in MIME format. Since your mail reader does not understand') or { MS Exchange }
                    (firstline='  This message is in MIME format.  The first part should be readable text,');   { elm }
    end;

  begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(0,'',tmp,false,0);
    assign(t,tmp);
    getmem(buf,bufsize);
    settextbuf(t,buf^,bufsize);
    reset(t);
    anzahl:=0;
    stackwarn:=false;

    if hdp.boundary='' then begin     { Boundary erraten ... }
      n:=0; s:=''; bound:='';
      while not eof(t) and (n<100) and
         ((LowerCase(LeftStr(s,13))<>'content-type:') or (LeftStr(bound,2)<>'--')) do begin
        bound:=s;
        readln(t,s);
        inc(n);
        end;
      if bound='' then goto ende;
      hdp.boundary:=mid(bound,3);
      close(t);
      reset(t);
      end;

    bptr:=0;
    push('--' + hdp.boundary);
    n:=0;     { Zeilennummer }
    vorspann:=true;
    reset_var;
    last:=0;
    bufline:='';
    firstline := '';

    while not eof(t) and (anzahl<maxparts) do begin
      _start:=n+1;
      if bptr=0 then bound:=#0     { Nachspann }
      else bound:=bstack[bptr];
      repeat
        if bufline<>'' then begin
          s:=bufline; bufline:='';
          dec(_start);
          end
        else begin
          readln(t,s);
          inc(n);
          if (n>=1) and (Firstline = '') then firstline:=s;
          end;
        endbound:=(s=bound+'--');
        isbound:=endbound or (s=bound);
        if (ctype='') and (s<>'') and not isbound then
          if vorspann then ctype:=getres2(2440,1)     { 'Vorspann' }
          else ctype:=getres2(2440,2);                { 'Nachspann' }
      until isbound or eof(t);
      { Letzte Zeile im letzen Part wird sonst unterschlagen }
      if not isbound then inc(n);
      vorspann:=false;

      if not eof(t) and (ctype=getres2(2440,2)) then begin  { 'Nachspann' }
        { das war kein Nachspann, sondern ein text/plain ohne Subheader ... }
        ctype:='text'; subtype:='plain';
        end;

      if (ctype=getres2(2440,1)) and MimeVorspann then
        ctype:='';

      if ctype<>'' then begin
        inc(anzahl);
        with mf[anzahl] do begin
          level:=bptr+last;
          typ:=ctype;
          subtyp:=subtype;
          code:=MimeGetEncodingFromName(_encoding);
          fname:=filename;
          ddatum:=filedate;
          charset := MimeGetCharsetFromName(CharsetName);
          startline:=_start;
          lines:=n-startline;
          part:=anzahl;
 {         parts := anzahl; MK 01/00 Bitte pr¸fen, ob ok, wenn das reingenommen wird!!! }
          end;
        end;
      last:=0;

      if endbound then begin
        pop;
        s:='';
        last:=1;
        end;

      reset_var;
      if not eof(t) and not endbound then begin
        s2:='';
        repeat                       { Subheader auswerten }
          if s2<>'' then
            s:=iifs(s2=#0,'',s2)
          else begin
            readln(t,s); inc(n);
            end;
          if not eof(t) and (cpos(':',s)>0) then
            repeat                { Test auf Folding }
              readln(t,s2);
              inc(n);
              folded:=(firstchar(s2) in [' ',#9]);
              if folded then s:=s+' '+trim(s2)
              else if s2='' then s2:=#0;
            until not folded or eof(t);
          endhd:=cpos(':',s)=0;
          if endhd and (s<>'') then bufline:=s;
          hdline:=LowerCase(GetToken(s,':'));
          if hdline='content-transfer-encoding' then
            _encoding:=LowerCase(s)
          else
          if hdline='content-type' then
          begin
            ctype:=LowerCase(GetToken(s,'/'));
            subtype:=LowerCase(GetToken(s,';'));
            while s<>'' do
            begin
              GetParam;
              if (ctype='multipart') and (parname='boundary') then
                subboundary:=parvalue
              else if (parname='name') or (parname='filename') then
                filename:=parvalue
              else if (parname='x-date') then
                filedate:=RFC2Zdate(parvalue)
              else if (parname='charset') then
                CharsetName := parvalue;
            end;
          end else
            { Manchmal ist der Dateiname nur im disposition-Teil enthalten }
            if (hdline='content-disposition') and (filename = '') then
            begin
              parname:=LowerCase(GetToken(s,'='));
              if firstchar(s)='"' then DeleteFirstChar(s);
              if lastchar(s)='"' then DeleteLastChar(s);
              if (pos('name', parname) >0) then filename:=s;
            end;
        until endhd or eof(t);

        if subboundary<>'' then begin
          push('--'+subboundary);
          reset_var;
          vorspann:=true;
          end;

        end;
      end;

    pop;

    anzahl0:=anzahl;
    if anzahl>1 then begin
      inc(anzahl);
      with mf[anzahl] do begin
        level:=1;
        typ:=getres2(2440,10);    { 'gesamte Nachricht' }
        subtyp:='';
        code:=MimeEncodingBinary;
        fname:='';
        startline:=1;
        lines:=n;
        part:=0;
        end;
      end;

  ende:
    close(t);
    _era(tmp);
    freemem(buf,bufsize);
  end;

  function fnform(fname:string; len:integer):string;
  begin
    if length(fname)<len then
      fnform:=rforms(fname,len)
    else if length(fname)>len then
      fnform:=LeftStr(fname,len-3)+'...'
    else
      fnform:=fname;
  end;


var i : integer;

begin                         { SelectMultiPart }
  brk:=false;
  fillchar(mpdata,sizeof(mpdata),0);
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  MakePartlist;
  if not forceselect and (anzahl=3) and (mf[2].typ='text')
     and (mf[1].typ='text') and (mf[1].subtyp='plain')
     and (((hdp.mime.ctype='multipart/alternative')      { Text+HTML Messis }
            and (mf[2].subtyp='html'))
         or (mf[2].subtyp='x-vcard'))                 { oder Text mit VCard }
  then begin
    index:=1;
    select:=false;                         { Standardmaessig Nur Text zeigen }
    alter:=true;
    end
  else
    alter:=false;

  if (index=0) and (anzahl>anzahl0) then
    index:=anzahl
  else
    index:=minmax(index,1,anzahl0);

  if anzahl>0 then
    if not select or (anzahl=1) then begin
      if (anzahl>1) or (mf[index].typ <> getres2(2440,1)) then begin { 'Vorspann' }
        mpdata:=mf[index];
        mpdata.parts:=max(1,anzahl0);
        mpdata.alternative:=alter;
        end
      end
    else begin
      List := listbox(56,min(screenlines-4,anzahl),getres2(2440,9));   { 'mehrteilige Nachricht' }
      for i:=1 to anzahl do
        with mf[i] do
          List.AddLine(forms(sp((level-1)*2+1)+typname(typ,subtyp),25)+strsn(lines,6)+
                ' ' + fnform(fname,23) + ' ' + strs(i));
      List.OnKeypressed := SMP_Keys;
      List.Startpos := index-1;
      brk := List.Show;
      if not brk then
      begin
        mpdata:=mf[List.SelLine+1];
        if (mpdata.typ=getres2(2440,1)) or (mpdata.typ=getres2(2440,2)) or
           (mpdata.typ=getres2(2440,10)) then begin
          mpdata.typ:='text';
          mpdata.subtyp:='plain';
          end;
        mpdata.parts:=anzahl0;
        mpdata.alternative:=false;
        end;
      List.Free;
      closebox;
    end;
  Hdp.Free;
end;


{ Teil einer Multipart-Nachricht decodieren und extrahieren }

procedure ExtractMultiPart(var mpdata:multi_part; fn:string; append:boolean);
const bufsize = 2048;

var   input,t : text;
      tmp     : string;
      f       : file;
      buf     : pointer;
      i       : longint; { Integer->LongInt, wegen gro·en MIME-Mails }
      s       : string;
      softbreak: boolean;

  procedure QP_decode;       { s quoted-printable-decodieren }
  var
    p : integer;
  begin
    if s = '' then exit;
    p:=1;
    while p<length(s)-1 do
    begin
      while (p<length(s)-1) and (s[p]<>'=') do
        inc(p);
      if p<length(s)-1 then
      begin
        s[p]:=chr(hexval(copy(s,p+1,2)));
        delete(s,p+1,2);
      end;
      inc(p);
    end;
  end;

begin
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  extract_msg(0,'',tmp,false,0);
  assign(input,tmp);
  getmem(buf,bufsize);
  settextbuf(input,buf^,bufsize);
  reset(input);

  with mpdata do
  begin
    // if Charset is unkown, assume ISO 8859-1 is used
    if Charset = csUnknown then CHarset := csISO8859_1;
    for i:=1 to startline-1 do
      readln(input);

    if code<>MimeEncodingBase64 then begin     { plain / quoted-printable }
      assign(t,fn);
      if append then system.append(t)
      else rewrite(t);
      for i:=1 to lines do begin
        readln(input,s);
        if code=MimeEncodingQuotedPrintable then begin
          softbreak:=(lastchar(s)='=');
          QP_decode;
        end
        else
          softbreak:=false;

        if code in [MimeEncodingBinary, MimeEncoding7Bit, MimeEncoding8Bit] then
        begin
          // convert s to Unicode (UTF-8)
          if Charset <> csUTF8 then
            s := Convert8BitToUTF(s, Charset);

          // convert s (now UTF-8) back in the used Codepage
          s := ConvertUTFTo8Bit(s, SysGetConsoleCodepage);
        end;

        if softbreak then
        begin
          SetLength(s, Length(s)-1);
          write(t,s);
        end else
          writeln(t,s);
        end;
      close(t);
      end

    else begin                          { base64 }
      assign(f,fn);
      if append then begin
        reset(f,1);
        seek(f,filesize(f));
        end
      else
        rewrite(f,1);

      if lines>500 then { Auf 500 Zeilen angepasst }
        rmessage(2442);    { 'decodiere BinÑrdatei ...' }

      for i:=1 to lines do
      begin
        readln(input,s);
        s:=DecodeBase64(s);
        if s <> '' then blockwrite(f,s[1],length(s));
      end;

      if lines>500 then closebox;

      close(f);
      if ddatum<>'' then
        SetZCftime(fn,ddatum);
    end;
  end;
  close(input);
  _era(tmp);
  freemem(buf,bufsize);
end;

procedure mimedecode;    { Nachricht/Extract/MIME-Decode }
var mpdata : multi_part;
    brk    : boolean;
begin
  mpdata.startline:=0;
  SelectMultiPart(true,1,true,mpdata,brk);
  if not brk then
    if mpdata.startline>0 then
      m_extrakt(mpdata)
    else
      rfehler(2440);    { 'keine mehrteilige MIME-Nachricht' }
  Freeres;
end;
{$ENDIF __undefined__}

procedure mimedecode;    { Nachricht/Extract/MIME-Decode }
begin
  tfehler('not implemented',20);
end;

//
//  $Log$
//  Revision 1.1  2001/09/30 19:37:49  cl
//  - moved from main directory
//

{ --------------------------------------------------------------- } end.

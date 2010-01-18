{$I xpdefine.inc }

unit rfc_streams;

interface

uses
  xpstreams_codec,
  typeform,
  classes;

{ -------------------------- Unix line ends -------------------------- }

type TCRLFtoLFStream = class(TCodecStream)
  private
    BytesWritten: LongInt;
    LastCharWasCR: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ------------------- RFC 2821/976/977 Dot Escaping ------------------ }

type TDotEscapeStream = class(TCodecStream)
  private
    BytesWritten: LongInt;
    LastWasCRLF,LastWasCR: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

implementation

const
  cr: char = #13;

{ -------------------------- Unix line ends -------------------------- }

constructor TCRLFtoLFStream.Create;
begin
  inherited Create;
  BytesWritten:=0;
  LastCharWasCR:=false;
end;

function TCRLFtoLFStream.Write(const Buffer; Count: Longint): Longint;
var I:Longint;
begin
  Result := 0;

  if Count<=0 then
    exit;

  if LastCharWasCR and (PChar(@Buffer)^<>#10) then
    FOtherStream.WriteBuffer(cr,1);

  for i:=1 to Count-1 do
    if ((PChar(@Buffer)+i-1)^=#13) and
       ((PChar(@Buffer)+i  )^=#10) then
    begin
      if i-Result-1>0 then
        Inc(Result,FOtherStream.Write((PChar(@Buffer)+Result)^,i-Result-1));
      Inc(Result,1);
      if Result<>i then
        exit;
    end;

  LastCharWasCR := (PChar(@Buffer)+Count-1)^=#13;

  if Count-Result>0 then
    Inc(Result,FOtherStream.Write((PChar(@Buffer)+Result)^,Count-Result-iif(LastCharWasCR,1,0)));

  if LastCharWasCR then
    Inc(Result);

  Inc(BytesWritten,Result);
end;

function TCRLFtoLFStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  Result := BytesWritten;
  if not ((((Origin = soFromCurrent) or (Origin = soFromEnd)) and (Offset = 0))
     or ((Origin = soFromBeginning) and (Offset = Result))) then
    raise EStreamError.Create('Invalid stream operation');
end;

destructor TCRLFtoLFStream.Destroy;
begin
  if LastCharWasCR then
    FOtherStream.WriteBuffer(cr,1);
end;

{ ------------------- RFC 2821/976/977 Dot Escaping ------------------ }

constructor TDotEscapeStream.Create;
begin
  inherited Create;

  BytesWritten:=0;
  LastWasCRLF:=false;
  LastWasCR:=false;
end;

destructor TDotEscapeStream.Destroy;
const delim: array[0..4] of char = #13#10'.'#13#10;
var o:integer;
begin
  o:=iif(LastWasCRLF,2,0);
  FOtherStream.WriteBuffer(delim[o],sizeof(delim)-o);
  inherited Destroy;
end;

function TDotEscapeStream.Write(const Buffer; Count: Longint): Longint;
var I,Beg:Longint;
begin
  Result := 0;
  Beg := 0;

  for i:=0 to Count-1 do
  begin
    if LastWasCRLF and ((PChar(@Buffer)+i)^='.') then
    begin
      Inc(Beg,FOtherStream.Write((PChar(@Buffer)+Beg)^,i-beg+1)-1);
      if Beg<>i then exit; // write error
    end;

    LastWasCRLF:=((PChar(@Buffer)+i)^=#10) and LastWasCR;
    LastWasCR  :=((PChar(@Buffer)+i)^=#13);
  end;

  if Count-Beg>0 then
    Inc(Beg,FOtherStream.Write((PChar(@Buffer)+Beg)^,Count-beg));

  Result:=Beg;
  Inc(BytesWritten,Result);
end;

function TDotEscapeStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  Result := BytesWritten;
  if not ((((Origin = soFromCurrent) or (Origin = soFromEnd)) and (Offset = 0))
     or ((Origin = soFromBeginning) and (Offset = Result))) then
    raise EStreamError.Create('Invalid stream operation');
end;

end.

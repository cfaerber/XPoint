{   $Id$

    TRopeStream - fast memory-based stream class based on the algorithms
    presented in "Ropes: an Alternative to Strings" by Hans-J. Boehm,
    Russ Atkinson and Michael Plass 
    
    References:
      http://www.sgi.com/tech/stl/ropeimpl.html
      http://www.cs.ubc.ca/local/reading/proceedings/spe91-95/spe/vol25/issue12/spe986.pdf

    This file Copyright (C) 2003 Claus F"arber <cl@openxp.de>

    This file is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this library; see the file COPYING.  If not, write to the
    Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.

    As a special exception, you may use this file as part of a free
    software library without restriction. Specifically, if other files
    instantiate templates or use macros or inline functions from this
    file, or you compile this file and link it with other files to
    produce an executable, this file does not by itself cause the
    resulting executable to be covered by the GNU General Public
    License.  This exception does not however invalidate any other
    reasons why the executable file might be covered by the GNU General
    Public License.
}

{$DEFINE DEBUG}

{$IFDEF FPC}
  {$MODE Delphi}
  {$IFNDEF ver1_0}
    {$DEFINE SEEK64}
  {$ELSE}
    {$UNDEF SEEK64}
  {$ENDIF}
{$ELSE}
  {$DEFINE SEEK64}
{$ENDIF}

unit xprope;

{ ---------------------------} interface { --------------------------- }

uses classes;

type
  TRopeStream = class(TStream)
  private
    FRootNode: pointer;
    FPosition: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF};
    FLastNode: pointer;

  protected 
    procedure Optimise;
    procedure SetSize({$IFNDEF FPC}const{$ENDIF} NewSize: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}); override;
  
  public
    constructor Create;
    destructor Destroy; override;
    
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek({$IFNDEF FPC}const{$ENDIF} Offset: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}; Origin: {$IFDEF FPC}Word{$ELSE}TSeekOrigin{$ENDIF}): {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}; override;

    procedure Assign(otherRope: TRopeStream);
  end;

{ ------------------------} implementation { ------------------------- }

uses sysutils, math {$IFDEF DEBUG}, debug{$ENDIF};

type
  TRopeNodeType = (
    rntEmpty,
    rntLeaf,
    rntConcat (* ,
    rntRopeFunction *) ,
    rntSubstring,
    rntFill  );

  TRopeNodeP = ^TRopeNode;  
  TRopeNode  = packed record
    ReferenceCount: Integer;
    Size:       {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF};
    Depth:      Integer;
    case NodeType: TRopeNodeType of
      rntLeaf: ( 
        MaxSize: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF};
        Data: PChar; );
      rntConcat: ( 
        Left, Right: TRopeNodeP; );
      rntSubstring: ( 
        Source: TRopeNodeP; 
        StartPos: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}; );
      rntFill: (
        FillValue: Byte; );
  end;

const
  ROPE_MIN_ALLOC   = $40000;    // allocation unit
  ROPE_MAX_COMBINE = $10000;    // max total size of nodes to combine into single leaf node
  ROPE_MAX_MOVE    = $10000;    // max size of byte to move within a node to combine with other node

function alloc_node: TRopeNodeP;
begin
  result := AllocMem(SizeOf(TRopeNode));
  result^.NodeType := rntEmpty;
  result^.ReferenceCount := 1;
  result^.Size     := 0;
  result^.Depth    := 0;
end;

function alloc_fill_node(Size: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}; FillValue: Byte): TRopeNodeP;
begin
  result := alloc_node;
  result^.NodeType := rntFill;
  result^.Size := Size;
  result^.FillValue := FillValue;
end;

function alloc_concat_node(Left, Right: TRopeNodeP): TRopeNodeP;
begin
  result := alloc_node;
  result^.NodeType := rntConcat;
  result^.Left := Left;
  result^.Right := Right;
  result^.Size := Left^.Size + Right^.Size;
  result^.Depth := Max(Left^.Depth,Right^.Depth);
end;

function alloc_substr_node(Source: TRopeNodeP; StartPos: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}): TRopeNodeP; overload;
begin
  result := alloc_node;
  result^.NodeType := rntSubstring;
  result^.Source := Source;
  result^.StartPos := StartPos;
  result^.Size := Source^.Size - StartPos;
  result^.Depth := Source^.Depth +1;
end;

function alloc_substr_node(Source: TRopeNodeP; StartPos, Count: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}): TRopeNodeP; overload;
begin
  result := alloc_substr_node(Source,StartPos);
  result^.Size := Min(result^.Size,Count);
end;

  
procedure addref_node(node: TRopeNodeP);
begin
  Inc(node^.ReferenceCount);
end;

procedure release_node(node: TRopeNodeP);
begin
  if not assigned(node) then exit;

  Dec(node^.ReferenceCount);
  if (node^.ReferenceCount) <= 0 then begin
    with node^ do case NodeType of
      rntLeaf:   if assigned(Data) then FreeMem(data,Size);
      rntConcat: begin release_node(Left); release_node(Right); end;
      rntSubstring: release_node(Source);
    end;
    FreeMem(node,sizeof(TRopeNode));
  end;
end;

{$IFDEF DEBUG}
procedure debug_out_node(const prefix1,prefix2,prefix3: string; node: TRopeNodeP);
begin
  case node^.NodeType of
  rntConcat:
    begin
      DebugLog('xprope',prefix1+'concat node: '+IntToStr(node^.Left^.Size)+' + '+IntToStr(node^.Right^.Size)+' bytes',dlTrace);
      debug_out_node(prefix2+' +- ',prefix2+ ' |   ',prefix2+ ' \- ', node^.Left);
      debug_out_node(prefix2+' \- ',prefix2+ '     ',prefix2+ '    ', node^.Right);
    end;
  rntEmpty:
    DebugLog('xprope',prefix1+'empty node',dlTrace);
  rntLeaf:
    DebugLog('xprope',prefix1+'leaf node: '+IntToStr(node^.Size)+' bytes',dlTrace);
  rntFill:
    DebugLog('xprope',prefix1+'fill node: '+IntToStr(node^.Size)+' bytes',dlTrace);
  rntSubstring:
    begin
      DebugLog('xprope',prefix1+'substring node: '+IntToStr(node^.Size)+' bytes',dlTrace);
      debug_out_node(prefix2+' \- ',prefix2+ '     ',prefix2+ '    ', node^.Source);
    end;
  end;
end;
{$ENDIF}

function read_node(Node: TRopeNodeP; StartPos: Longint; var Buffer; Count: Longint): Longint;
var tmp: Longint;
begin
  case Node^.NodeType of
    rntEmpty:
      result := 0;
    
    rntLeaf:   
      begin
        if Count >= (Node^.Size - StartPos) then
          Count := (Node^.Size - StartPos);
        if Count > 0 then
          Move((PChar(Node^.Data)+StartPos)^, Buffer, Count );
        Result := Count;
      end;
                           
    rntConcat:
      begin
        Result := 0;
        if Node^.Left^.Size > StartPos then
        begin
          tmp := Node^.Left.Size - StartPos;
          if tmp > Count then tmp := Count;
          Inc(Result,Read_Node(Node^.Left,StartPos,Buffer,tmp));
        end;

        if Node^.Left^.Size < StartPos+Count then
        begin
          Inc(Result,Read_Node(Node^.Right,StartPos-Node^.Left^.Size+Result,
            ((PChar(@Buffer))+Result)^,Count-Result));
        end
      end;
       
    rntSubstring:
      begin
        Result := Read_Node(Node^.Source,StartPos+Node^.StartPos,
          Buffer,Count);
      end;

    rntFill:
      begin
        Result := Count;
        FillChar(Buffer,Count,Node^.FillValue);
      end;
  end;
end;

procedure optimise_node(var node: TRopeNodeP; full_opt:boolean);
var tmp,tmp2,tmp1: TRopeNodeP;
begin
  if node^.ReferenceCount > 1 then 
    exit;

  case node^.NodeType of
  rntConcat:
    if full_opt then
    begin
    // -- at least one child is empty ----------------------------------
      if (node^.Left^.Size = 0) and
         (node^.Right^.Size = 0) then
      begin
        release_node(node^.Left);
        release_node(node^.Right);
        node^.Size := 0;
        node^.Depth := 0;
        node^.NodeType := rntEmpty;
      end else
      
      if (node^.Left^.Size = 0) then
      begin
        tmp := node^.Right;
        release_node(node^.Left);
        release_node(node);
        node := tmp;
      end else

      if (node^.Right^.Size = 0) then
      begin
        tmp := node^.Left;
        release_node(node^.Right);
        release_node(node);
        node := tmp;
      end else

      if (node^.Left^.NodeType = rntFill) and
         (node^.Right^.NodeType = rntFill) and
         (node^.Left^.FillValue = node^.Right^.FillValue) then
      begin
        tmp := node^.Left;
        release_node(node^.Right);
        node^.NodeType := rntFill;
        node^.FillValue := tmp^.FillValue;
        release_node(tmp);
        node^.Depth := 0;
      end else

    // -- right child fits into left child -----------------------------
      if (node^.Left^.NodeType = rntLeaf) and
         (node^.Left^.MaxSize - node^.Left^.Size >= node^.Right^.Size) then
      begin
        tmp := node^.Left;
        read_node(node^.Right,0,((tmp^.Data)+(tmp^.Size))^,node^.Right^.Size);
        inc(tmp^.Size,node^.Right^.Size);
        release_node(node^.Right);
        release_node(node);
        node := tmp;
      end else

    // -- left child fits into right child -----------------------------
      if (node^.Right^.NodeType = rntLeaf) and
         (node^.Right^.MaxSize - node^.Right^.Size >= node^.Left^.Size) and
         (node^.Right^.Size <= ROPE_MAX_MOVE) then
      begin
        tmp := node^.Right;
        Move((tmp^.Data)^,((tmp^.Data)+(node^.Left^.Size))^,tmp^.Size);        
        read_node(node^.Left,0,(tmp^.Data)^,node^.Left^.Size);
        inc(tmp^.Size,node^.Left^.Size);
        release_node(node^.Left);
        release_node(node);
        node := tmp;
      end else

    // -- children small enough to reallocate --------------------------
      if node^.Left^.Size + node^.Right^.Size <= ROPE_MAX_COMBINE then
      begin
        tmp := node^.Left;
        tmp2 := node^.Right;

        node^.NodeType := rntLeaf;
        node^.Depth := 0;
        node^.Size := tmp^.Size + tmp2^.Size;
        node^.MaxSize := node^.Size;
        node^.Data := AllocMem(node^.MaxSize);

        read_node(tmp, 0,(node^.Data)^, tmp^.Size);
        read_node(tmp2,0,((node^.Data)+(tmp^.Size))^,tmp2^.Size);

        release_node(tmp);
        release_node(tmp2);
      end else

    // -- balance tree -------------------------------------------------
      begin
      // -- optimise children ------------------------------------------
        optimise_node(node^.Left,full_opt);
        optimise_node(node^.Right,full_opt);
    
        if(node^.Left^.ReferenceCount <= 1) and
          (node^.Right^.ReferenceCount <= 1) then
        begin
          if node^.Left^.Depth >= node^.Right^.Depth + 2 then
          begin

          end else
          if node^.Left^.Depth <= node^.Right^.Depth - 2 then
          begin

          end;
        end;
        node^.Depth := max(node^.Left^.Depth,node^.Right^.Depth);
      end;
    end;
    
  rntSubString:
    begin
    // -- fill node ----------------------------------------------------
      if node^.Source^.NodeType = rntFill then
      begin
        tmp := node^.Source;
        node^.NodeType := rntFill;
        node^.FillValue := tmp^.FillValue;        
        node^.Depth := 0;
        release_node(tmp);
      end else
    
    // -- small enough to reallocate -----------------------------------
      if node^.Size <= ROPE_MAX_COMBINE then
      begin
        tmp := alloc_node;
        try
          tmp^.NodeType := rntLeaf;
          tmp^.Size := node^.Size;
          tmp^.MaxSize := tmp^.Size;
          tmp^.Data := AllocMem(tmp^.MaxSize);
          read_node(node, 0,(tmp^.Data)^, node^.Size);
        except
          release_node(tmp);
          raise;
        end;
        release_node(node);
        node := tmp;          
      end else

    // -- concat node --------------------------------------------------
      if (node^.Source^.NodeType = rntConcat) and
         (node^.Source^.ReferenceCount <= 1) then 
      begin
      // -- left no longer necessary -----------------------------------
        if node^.Source^.Left^.Size <= node^.StartPos then
        begin
          tmp := node^.Source;
          node^.Source := tmp^.Right; addref_node(tmp^.Right);
          node^.StartPos := node^.StartPos + tmp^.Left^.Size;
          release_node(tmp);
          optimise_node(node,full_opt);
        end else

      // -- right no longer necessary ----------------------------------
        if node^.Source^.Left^.Size >= node^.StartPos + node^.Size then
        begin
          tmp := node^.Source;
          node^.Source := tmp^.Left; addref_node(tmp^.Left);
          release_node(tmp);
          optimise_node(node,full_opt);
        end else

      // -- right and left needed => move substr nodes down ------------
        begin
          tmp1 := nil;
          tmp2 := nil;

          try
            tmp1 := alloc_node;
            tmp2 := alloc_node;

            tmp1^.NodeType := rntSubstring;
            tmp1^.Source := node^.left;
            tmp1^.Size := node^.Left.Size - node^.StartPos;
            tmp1^.StartPos := node^.StartPos;
            tmp1^.Depth := node^.Left.Depth +1;

            tmp2^.NodeType := rntSubstring;
            tmp2^.Source := node^.Right;
            tmp2^.Size := node^.Size-node^.Left^.Size-node^.StartPos;
            tmp2^.StartPos := 0;
            tmp2^.Depth := node^.Right.Depth +1;
          except
            release_node(tmp1);
            release_node(tmp2);
            raise;
          end;
            tmp := node;
            node := tmp^.Source;
            release_node(tmp);
            
            node^.Left := tmp1;
            node^.Right := tmp2;
            tmp^.NodeType := rntEmpty;

            if full_opt then
              optimise_node(node,full_opt)
            else begin
              optimise_node(node^.Left,full_opt);
              optimise_node(node^.Right,full_opt);
            end;
        end;
      end;
    end;
    
  end;
end;

procedure write_node(var node: TRopeNodeP; StartPos: Longint; const Buffer; Count: Longint; insert: boolean);

  function can_add_to(dest: TRopeNodeP): boolean;
  begin
    result := (dest^.ReferenceCount<=1) and
      (dest^.NodeType = rntLeaf) and
      (dest^.Maxsize - dest^.Size >= Count);
  end;

  procedure add_to(dest: TRopeNodeP);
  begin
    assert(dest^.NodeType = rntLeaf);
    Move(Buffer,((dest^.Data)+(dest^.Size))^,Count);
    Inc(dest^.Size,Count);
  end;

  function alloc_data_node: TRopeNodeP;
  begin
    result := alloc_node;
    result^.NodeType := rntLeaf;
    result^.MaxSize := Max(ROPE_MIN_ALLOC,Count);
    result^.Size := Count;
    result^.Data := AllocMem(result^.MaxSize);
    Move(Buffer,result^.Data^,Count);
  end;

begin
// -- empty node => replace --------------------------------------------
  if (node^.NodeType = rntEmpty) or (node^.Size <= 0) then
  begin
    release_node(node);
    if StartPos <= 0 then
      node := alloc_data_node
    else
      node := alloc_concat_node(
        alloc_fill_node(StartPos,0),
        alloc_data_node);
  end else

// -- write at start ---------------------------------------------------
  if StartPos = 0 then
  begin
    if insert then
      node := alloc_concat_node(
        alloc_data_node,
        node)
    else begin
      node := alloc_concat_node(
        alloc_data_node,
        alloc_substr_node(node,Count));
      optimise_node(node^.Right,false);
    end;
  end else

// -- write at end -----------------------------------------------------
  if(StartPos = node^.Size) then
  begin
    if can_add_to(node) then
      add_to(node)
    else

    if(node^.ReferenceCount <= 1) and
      (node^.NodeType = rntConcat) and
       can_add_to(node^.Right) then
    begin
      add_to(node^.Right);
      inc(node^.Size,Count);
    end
    else
      node := alloc_concat_node(
        node,
        alloc_data_node);
  end else

// -- write in the middle ----------------------------------------------
  begin
    assert(false);
  end;
end;

constructor TRopeStream.Create;
begin
  FRootNode := AllocMem(sizeof(TRopeNode));
  with TRopeNodeP(FRootNode)^ do begin
    ReferenceCount := 1;
    Size := 0;
    Depth := 0;
    NodeType := rntEmpty;
  end;
  FPosition := 0;
end;

destructor TRopeStream.Destroy;
begin
  release_node(TRopeNodeP(FRootNode));
end;

procedure TRopeStream.Optimise;
begin
{$IFDEF DEBUG}
  DebugLog('xprope','optimising tree',dlDebug);
  DebugLog('xprope','before:',dlTrace);
  debug_out_node('- ','  ','  ',TRopeNodeP(FRootNode));
{$ENDIF}
    
  optimise_node(TRopeNodeP(FRootNode),true);    

{$IFDEF DEBUG}
  DebugLog('xprope','after:',dlTrace);
  debug_out_node('- ','  ','  ',TRopeNodeP(FRootNode));
{$ENDIF}
end;

function TRopeStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Read_Node(TRopeNodeP(FRootNode),FPosition,Buffer,Count);
  Inc(FPosition,Result);
end;

function TRopeStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  if Count <= 0 then exit;
  write_node(TRopeNodeP(FRootNode),FPosition,Buffer,Count,false);
  Inc(FPosition, Result)
end;

function TRopeStream.Seek({$IFNDEF FPC}const{$ENDIF} Offset: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF}; Origin: {$IFDEF FPC}Word{$ELSE}TSeekOrigin{$ENDIF}): {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF};
var mySize: Longint;
begin
  mySize := TRopeNodeP(FRootNode)^.Size;

  case ord(Origin) of
    soFromBeginning:    Result := Offset;
    soFromCurrent:      Result := FPosition + Offset;
    soFromEnd:          Result := mySize - Offset;
  end;

  if Result < 0      then Result := 0;
  if Result > mySize then Result := mySize;

  if FPosition<>Result then Optimise;
  FPosition := Result;
end;

procedure TRopeStream.SetSize({$IFNDEF FPC}const{$ENDIF} NewSize: {$IFDEF SEEK64}Int64{$ELSE}Longint{$ENDIF});
var mySize: Longint;
begin
  mySize := TRopeNodeP(FRootNode)^.Size;

  if NewSize < mySize then
  begin
    FRootNode := alloc_substr_node(
      TRopeNodeP(FRootNode),
      NewSize);
    optimise_node(TRopeNodeP(FRootNode),false);
  end else
  if NewSize > mySize then
    FRootNode := alloc_concat_node(
      TRopeNodeP(FRootNode),
      alloc_fill_node(NewSize-mySize,0));
end;

procedure TRopeStream.Assign(otherRope: TRopeStream);
begin
  addref_node(TRopeNodeP(otherRope.FRootNode));
  release_node(TRopeNodeP(FRootNode));
  FRootNode := otherRope.FRootNode;
  FPosition := otherRope.FPosition;
end;

end.

// $Log$
// Revision 1.2  2003/01/13 23:31:34  cl
// - FPC compile fix
//
// Revision 1.1  2003/01/13 22:48:51  cl
// - enabled TRopeStream
//
// Revision 1.1  2003/01/11 19:48:39  cl
// - new class TRopeStream to replace TMemoryStream
//
//

{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by Michael Van Canneyt, member of the 
    Free Pascal development team

    Implements a memory manager that uses the C memory management.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit compheap;

{$mode objfpc}

interface

implementation

const
  blocksize    = 16;  { at least size of freerecord }
  blockshr     = 4;   { shr value for blocksize=2^blockshr}
  maxblocksize = 512+blocksize; { 1024+8 needed for heaprecord }
  maxblock     = maxblocksize div blocksize;
  maxreusebigger = 8; { max reuse bigger tries }

  usedmask = 1;            { flag if the block is used or not }
  beforeheapendmask = 2;   { flag if the block is just before a heapptr }
  sizemask = not(blocksize-1);

type
  pfreerecord  = ^tfreerecord;
  tfreerecord  = record
    size  : longint;
    next,
    prev  : pfreerecord;
  end; { 12 bytes }


  pheaprecord = ^theaprecord;
  theaprecord = record
  { this should overlap with tfreerecord }
    size  : longint;
  end; { 4 bytes }


{$IFDEF Win32 }
Function Malloc (Size : Longint) : Pointer;cdecl; external 'msvcrt' name 'malloc';
Procedure Free (P : pointer); cdecl; external 'msvcrt' name 'free';
Procedure FreeMem (P : Pointer); cdecl; external 'msvcrt' name 'free';
function ReAlloc (P : Pointer; Size : longint) : pointer; cdecl; external 'msvcrt' name 'realloc';
Function CAlloc (unitSize,UnitCount : Longint) : pointer;cdecl;external 'msvcrt' name 'calloc';
{$ELSE }
Function Malloc (Size : Longint) : Pointer;cdecl; external 'c' name 'malloc';
Procedure Free (P : pointer); cdecl; external 'c' name 'free';
Procedure FreeMem (P : Pointer); cdecl; external 'c' name 'free';
function ReAlloc (P : Pointer; Size : longint) : pointer; cdecl; external 'c' name 'realloc';
Function CAlloc (unitSize,UnitCount : Longint) : pointer;cdecl;external 'c' name 'calloc';
{$ENDIF }

Var 
  OldMemoryManager : TMemoryManager;


Function CompGetMem  (Size : Longint) : Pointer;
var
  s: cardinal;
  newsize: longint;
begin
  newsize := size;
  if newsize = 0 then
    newsize := 1;
  newsize:=(newsize+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
  s := newsize shr blockshr;
  if s > maxblock then
    begin
      CompGetMem:=malloc(newsize);
      pheaprecord(CompGetMem)^.size := newsize;
      inc(CompGetMem,sizeof(theaprecord));
    end
  else
    CompGetMem := OldMemoryManager.GetMem(size);
end;

Function CompFreeMemSize(var p:pointer;Size:Longint):Longint;
var
  pcurr: pfreerecord;
  pcurrsize,s: longint;
begin
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  pcurrsize:=pcurr^.size and sizemask;
  s:=pcurrsize shr blockshr;
  if s>maxblock then
    begin
      size:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
      if size<>pcurrsize then
        RunError(204);
      dec(p,sizeof(theaprecord));
      free(p);
      CompFreeMemSize := pcurrsize;
    end
  else
    CompFreeMemSize := OldMemoryManager.FreeMemSize(p,size);
end;

Function CompFreeMem (Var P : pointer) : Longint;
var
  pcurr: pfreerecord;
  pcurrsize, s,size: longint;
begin
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  pcurrsize:=pcurr^.size and sizemask;
  s:=pcurrsize shr blockshr;
  if s>maxblock then
    begin
      size:=(pcurrsize+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
      dec(p,sizeof(theaprecord));
      free(p);
      p := nil;
      CompFreeMem := pcurrsize;
    end
  else
    CompFreeMem := OldMemoryManager.FreeMem(p);
end;

Function CompReAllocMem (var p:pointer;Size:longint):Pointer;
var
  pcurr: pfreerecord;
  pcurrsize,newsize: longint;
  newp: pointer;
begin
  if p = nil then
    begin
      CompReAllocMem := CompGetMem(size);
      p := result;
      exit;
    end;
  pcurr:=pfreerecord(pointer(p)-sizeof(theaprecord));
  pcurrsize:=pcurr^.size and sizemask;
  newsize:=(size+sizeof(theaprecord)+(blocksize-1)) and (not (blocksize-1));
  if pcurrsize <= maxblocksize then
    if newsize <= maxblocksize then
      OldMemoryManager.ReallocMem(p,size)
    else
      begin
        newp := CompGetMem(size);
        move(p^,newp^,MemSize(p));
        CompFreeMem(p);
        p := newp
      end 
  else
    if newsize <= maxblocksize then
      begin
        newp := CompGetMem(size);
        move(p^,newp^,size);
        CompFreeMem(p);
        p := newp;
      end
    else
      begin
        dec(p,sizeof(theaprecord));
        p := Realloc(p,newsize);
        pheaprecord(p)^.size := newsize;
        inc(p,sizeof(theaprecord));
      end;
  CompReallocMem := p;
end;

Function CMemSize (p:pointer): Longint;

begin
  Result:=OldMemoryManager.MemSize(p);
end;

Function CMemAvail : Longint;

begin
  Result:=0;
end;

Function CMaxAvail: Longint;

begin
  Result:=0;
end;

Function CHeapSize : Longint;

begin
  Result:=0;
end;
                    
Const
 CMemoryManager : TMemoryManager =
    (
      GetMem : {$ifdef fpc}@{$endif}CompGetmem;
      FreeMem : {$ifdef fpc}@{$endif}CompFreeMem;
      FreememSize : {$ifdef fpc}@{$endif}CompFreememSize;
      AllocMem : {$ifdef fpc}@{$endif}CompGetMem;
      ReallocMem : {$ifdef fpc}@{$endif}CompReAllocMem;
      MemSize : {$ifdef fpc}@{$endif}CMemSize;
      MemAvail : {$ifdef fpc}@{$endif fpc}CMemAvail;
      MaxAvail : {$ifdef fpc}@{$endif}MaxAvail;
      HeapSize : {$ifdef fpc}@{$endif}CHeapSize;
    );
  

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);
  
Finalization
  SetMemoryManager (OldMemoryManager);
end.

{   
 $Log: compheap.pas,v $
 Revision 1.1  2002/06/06 20:05:37  mk
 - added compheap as memory manager (disabled)

 Revision 1.4  2001/06/07 16:34:41  jonas
   * added ifdef fpc round @ for procvars

 Revision 1.3  2001/06/07 16:14:48  marco
  * Fixed @ procvar

    Revision 1.2  2000/07/13 11:33:10  michael
     + removed logs
 
}

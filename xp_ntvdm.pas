{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Claus Färber, http://www.openxp.de       }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ --------------------------------------------------------------- }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$            }

Library xp_ntvdm;

uses windows,sysutils,strings;

{ --- Imports from ntvdm.exe ------------------------------------ }

procedure setEAX(para:ULONG);  external 'ntvdm.exe'; function getEAX:ULONG; external 'ntvdm.exe';
procedure setAX(para:USHORT);  external 'ntvdm.exe'; function getAX:USHORT; external 'ntvdm.exe';
procedure setAL(para:UCHAR);   external 'ntvdm.exe'; function getAL:UCHAR;  external 'ntvdm.exe';
procedure setAH(para:UCHAR);   external 'ntvdm.exe'; function getAH:UCHAR;  external 'ntvdm.exe';

procedure setEBX(para:ULONG);  external 'ntvdm.exe'; function getEBX:ULONG; external 'ntvdm.exe';
procedure setBX(para:USHORT);  external 'ntvdm.exe'; function getBX:USHORT; external 'ntvdm.exe';
procedure setBL(para:UCHAR);   external 'ntvdm.exe'; function getBL:UCHAR;  external 'ntvdm.exe';
procedure setBH(para:UCHAR);   external 'ntvdm.exe'; function getBH:UCHAR;  external 'ntvdm.exe';

procedure setECX(para:ULONG);  external 'ntvdm.exe'; function getECX:ULONG; external 'ntvdm.exe';
procedure setCX(para:USHORT);  external 'ntvdm.exe'; function getCX:USHORT; external 'ntvdm.exe';
procedure setCL(para:UCHAR);   external 'ntvdm.exe'; function getCL:UCHAR;  external 'ntvdm.exe';
procedure setCH(para:UCHAR);   external 'ntvdm.exe'; function getCH:UCHAR;  external 'ntvdm.exe';

procedure setEDX(para:ULONG);  external 'ntvdm.exe'; function getEDX:ULONG; external 'ntvdm.exe';
procedure setDX(para:USHORT);  external 'ntvdm.exe'; function getDX:USHORT; external 'ntvdm.exe';
procedure setDH(para:UCHAR);   external 'ntvdm.exe'; function getDH:UCHAR;  external 'ntvdm.exe';
procedure setDL(para:UCHAR);   external 'ntvdm.exe'; function getDL:UCHAR;  external 'ntvdm.exe';

procedure setESP(para:ULONG);  external 'ntvdm.exe'; function getESP:ULONG; external 'ntvdm.exe';
procedure setSP(para:USHORT);  external 'ntvdm.exe'; function getSP:USHORT; external 'ntvdm.exe';

procedure setEBP(para:ULONG);  external 'ntvdm.exe'; function getEBP:ULONG; external 'ntvdm.exe';
procedure setBP(para:USHORT);  external 'ntvdm.exe'; function getBP:USHORT; external 'ntvdm.exe';

procedure setESI(para:ULONG);  external 'ntvdm.exe'; function getESI:ULONG; external 'ntvdm.exe';
procedure setSI(para:USHORT);  external 'ntvdm.exe'; function getSI:USHORT; external 'ntvdm.exe';

procedure setEDI(para:ULONG);  external 'ntvdm.exe'; function getEDI:ULONG; external 'ntvdm.exe';
procedure setDI(para:USHORT);  external 'ntvdm.exe'; function getDI:USHORT; external 'ntvdm.exe';

procedure setEIP(para:ULONG);  external 'ntvdm.exe'; function getEIP:ULONG; external 'ntvdm.exe';
procedure setIP(para:USHORT);  external 'ntvdm.exe'; function getIP:USHORT; external 'ntvdm.exe';

procedure setCS(para:USHORT);  external 'ntvdm.exe'; function getCS:USHORT; external 'ntvdm.exe';
procedure setSS(para:USHORT);  external 'ntvdm.exe'; function getSS:USHORT; external 'ntvdm.exe';
procedure setDS(para:USHORT);  external 'ntvdm.exe'; function getDS:USHORT; external 'ntvdm.exe';
procedure setES(para:USHORT);  external 'ntvdm.exe'; function getES:USHORT; external 'ntvdm.exe';
procedure setFS(para:USHORT);  external 'ntvdm.exe'; function getFS:USHORT; external 'ntvdm.exe';
procedure setGS(para:USHORT);  external 'ntvdm.exe'; function getGS:USHORT; external 'ntvdm.exe';

procedure setCF(para:ULONG);   external 'ntvdm.exe'; function getCF:ULONG;  external 'ntvdm.exe';
procedure setPF(para:ULONG);   external 'ntvdm.exe'; function getPF:ULONG;  external 'ntvdm.exe';
procedure setAF(para:ULONG);   external 'ntvdm.exe'; function getAF:ULONG;  external 'ntvdm.exe';
procedure setZF(para:ULONG);   external 'ntvdm.exe'; function getZF:ULONG;  external 'ntvdm.exe';
procedure setSF(para:ULONG);   external 'ntvdm.exe'; function getSF:ULONG;  external 'ntvdm.exe';
procedure setIF(para:ULONG);   external 'ntvdm.exe'; function getIF:ULONG;  external 'ntvdm.exe';

procedure setDF(para:ULONG);   external 'ntvdm.exe'; 
procedure setOF(para:ULONG);   external 'ntvdm.exe'; 
procedure setMSW(para:USHORT); external 'ntvdm.exe'; 

function  GetVDMPointer(Address,Size:ULONG; ProtectedMode:BOOL):Pointer; begin GetVDMPointer:=Pointer(DWORD(Hi(Address)*16+Lo(Address))); end;
function  FreeVDMPointer(Address:ULONG; Size:USHORT; Buffer:Pointer; ProtectedMode:BOOL):BOOL; begin FreeVDMPointer:=true; end;

{ --- Exact Windows Version ------------------------------------- }

procedure get_windows_version;
begin
  setEAX(GetVersion);
end;

{ --- Clipboard functions --------------------------------------- }

procedure clip_to_string;
var maxlen:  integer;
    len:     integer;
    i:	     integer;
    oneline: boolean;
    sp:      ^shortstring;
    ch:	     HANDLE;
    cp:	     PChar;
begin
  maxlen := getCL;
  oneline:= getCH<>0;
  sp := GetVDMPointer(GetEDI,maxlen,false);
  setCF(1);

  OpenClipboard(0);
  ch := GetClipboardData(CF_OEMTEXT); 
  if ch<> 0 then
  begin
    cp := GlobalLock(ch); 
    if cp <> nil then
    begin
      len := StrLen(cp);
      if len>255    then len:=255; 
      if len>maxlen then len:=maxlen;
      MoveMemory(PChar(Pointer(sp))+1,cp,len);
      sp^[0]:=Char(Byte(len));
    end;
    if oneline then 
      for i:=1 to len do
        if sp^[i]<#32 then
          sp^[i]:=#32;
    setCF(0);	  
    
    GlobalUnlock(ch);
  end;
  CloseClipboard;

  FreeVDMPointer(GetEDI,maxlen,sp,false);
end;

procedure mem_to_clip;
var 	cp: PChar;
	cl: ULONG;
	hm: HANDLE;
	pm: PChar;
begin
  cl := GetECX;
  cp := GetVDMPointer(GetESI,cl,false);

  SetCF(1);

  if OpenClipboard(0) then 
  begin
    hm := GlobalAlloc(cl+1,GMEM_MOVEABLE);
    if hm <> 0 then
    begin
      pm := GlobalLock(hm);
      if pm <> nil then
      begin
        MoveMemory(pm,cp,cl);
        (PChar(pm)+cl)^ := #0;
	GlobalUnLock(hm);

	EmptyClipboard;
        SetClipboardData(CF_OEMTEXT,hm);
	SetCF(0);
      end;
    end;
    CloseClipboard;
  end;
  
  FreeVDMPointer(GetESI,cl,cp,false);
end;

procedure clip_to_file;
var	fn: PChar;
	fh: Handle;
	ch: Handle;
	cp: LPTSTR;
	wr: DWORD;
begin
  fn:=GetVDMPointer(GetESI,$10000,false);
  setCF(1);

  OpenClipboard(0);
  ch := GetClipboardData(CF_OEMTEXT); 
  if ch<> 0 then
  begin
    cp := GlobalLock(ch); 
    if cp <> nil then
    begin
      fh:=CreateFile(fn,GENERIC_WRITE,0,0,CREATE_ALWAYS,
        FILE_FLAG_SEQUENTIAL_SCAN,0);
      if fh<>INVALID_HANDLE_VALUE then 
      begin
        WriteFile(fh,cp^,StrLen(cp),wr,0);
	CloseHandle(fh);
	setCF(0);
      end;
      GlobalUnlock(ch); 
    end;
  end;
  CloseClipboard;

  FreeVDMPointer(GetESI,0,fn,false);
end;

procedure file_to_clip;
var	fn: PChar;
	fh: Handle;
	ln: DWORD;
	mh: HANDLE;
	mp: PChar;
begin
  fn:=GetVDMPointer(GetESI,$10000,false);
  setCF(1);

  fh:=CreateFile(fn,GENERIC_READ,0,0,OPEN_EXISTING,
    FILE_FLAG_SEQUENTIAL_SCAN,0);
  if fh<>INVALID_HANDLE_VALUE then 
  begin
    ln := GetFileSize(fh,nil);
    mh := GlobalAlloc(GMEM_MOVEABLE,ln+1);
    if mh <> 0 then 
    begin
      mp := GlobalLock(mh);
      if mp <> nil then
      begin
        ReadFile(fh,mp^,ln,ln,nil);
        (PChar(mp)+ln)^ := #0;
	GlobalUnlock(mh);
        
	if OpenClipboard(0) then 
	begin
	  EmptyClipboard;
  	  SetClipboardData(CF_OEMTEXT,mh);
          CloseClipboard;
	end;
        setCF(0);
      end else
        GlobalFree(mh);
    CloseHandle(fh);
    end;
  end;

  FreeVDMPointer(GetESI,0,fn,false);
end;

{ --- VDD calls ------------------------------------------------- }

procedure OpenXP_Call; export;
begin
  case getDX of
    $0000: get_windows_version;
    $0101: clip_to_string;
    $0102: mem_to_clip;
    $0103: clip_to_file;
    $0104: file_to_clip;
  end;
end;  

procedure OpenXP_Init; export;
begin
end;

{ --- DLL exports ----------------------------------------------- }

exports OpenXP_Init;
exports OpenXP_Call;

end.

{ 
  $Log$
  Revision 1.1.2.1  2001/07/02 20:43:04  mk
  - NTVDM Interface

}  
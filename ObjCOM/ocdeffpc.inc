(*
**
** ObjCOM include file to make FPC more Delphi compatible
** See files "LICENSE.TXT" and "CREDITS.TXT"
**
*)

{$IFDEF FPC}
type DCB = record
          DCBlength : DWORD;
          BaudRate : DWORD;
          flags : longint;
          wReserved : WORD;
          XonLim : WORD;
          XoffLim : WORD;
          ByteSize : BYTE;
          Parity : BYTE;
          StopBits : BYTE;
          XonChar : char;
          XoffChar : char;
          ErrorChar : char;
          EofChar : char;
          EvtChar : char;
          wReserved1 : WORD;
       end;

  TDcb     = DCB;
  pInteger = ^Integer;

  function GetCommState(hFile:HANDLE; var lpDCB:TDCB):WINBOOL; external 'kernel32' name 'GetCommState';
  function SetCommState(hFile:HANDLE; var lpDCB:TDCB):WINBOOL; external 'kernel32' name 'SetCommState';
  function WaitForMultipleObjects(nCount:DWORD; lpHandles:Pointer; bWaitAll:WINBOOL; dwMilliseconds:DWORD):DWORD; external 'kernel32' name 'WaitForMultipleObjects';

{$ENDIF}

{
  $Log$
  Revision 1.2  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}

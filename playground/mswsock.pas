{ $Id: xprope.pas,v 1.7 2003/08/23 23:02:38 mk Exp $

  Copyright (C) 2004 Claus Faerber

  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, and/or sell copies of the
  Software, and to permit persons to whom the Software is furnished to do so,
  provided that the above copyright notice(s) and this permission notice appear
  in all copies of the Software and that both the above copyright notice(s) and
  this permission notice appear in supporting documentation.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE
  LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY
  DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

  Except as contained in this notice, the name of a copyright holder shall not
  be used in advertising or otherwise to promote the sale, use or other dealings
  in this Software without prior written authorization of the copyright holder.
}

unit mswsock;

interface

uses windows, winsock2;

function AcceptEx(sListenSocket,sAcceptSocket: SOCKET; lpOutputBuffer: PVOID;
  dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
  lpwdBytesReceived: LPDWORD; lpOverlapped: LPWSAOVERLAPPED): BOOL; external 'mswsock.dll';
function ConnectEx(s: SOCKET; name: psockaddr; namelen: integer;
  lpSendBuffer: PVOID; dwSendDataLength: DWORD; lpdwBytesSent: LPDWORD;
  lpOverlapped: LPWSAOVERLAPPED): BOOL; external 'mswsock.dll';
function DisconnectEx(s: SOCKET; lpOverlapped: LPWSAOVERLAPPED; dwFlags,
  dwReserved: DWORD); external 'mswsock.dll';

procedure GetAcceptExSockaddrs(lpOutputBuffer: LPVOID; dwReceiveDataLength,
  dwLocalAddressLength, dwRemoteAddressLength: DWORD; var localSockAddr: LPSOCKADDR;
  var LocalSockAddrLen: integer; var RemoteSockAddr: LPSOCKADDR; var
  RemoteSockAddrLen: integer); external 'mswsock.dll';


implementation



end.

{  $Id$

   Copyright (c) 2000 by the OpenXP development team
   Copyright (c) 1999-2000 by the Free Pascal development team

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

   Created on December, 27th 2000 by Claus Faerber <claus@faerber.muc.de>

   This software is part of the OpenXP project (www.openxp.de).

   Based on sockets.pp, part of the Free Pascal run time library.
   Based on socketsh.inc, part of the Free Pascal run time library.
}

{
  Enthält das Interface für einen OS2-TCP/IP-Stack.
  Derzeit nur Stubs.
}

Unit OS2Sock;

interface

uses sysutils;

const
  { Socket Types }
  SOCK_STREAM     = 1;  { stream (connection) socket   }
  SOCK_DGRAM      = 2;  { datagram (conn.less) socket  }
  SOCK_RAW        = 3;  { raw socket                   }
  SOCK_RDM        = 4;  { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;  { sequential packet socket     }

  {  Address Families }
  AF_UNSPEC       = 0;
  AF_UNIX         = 1;  { Unix domain sockets          }
  AF_INET         = 2;  { Internet IP Protocol         }
  AF_INET6        = 3;  { IPv6 }
  AF_MAX          = 4;

  {  Protocol Families }
  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

  SOCK_MAXADDRLEN =255;             { longest possible addresses }

  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

  type
  TUnixSockAddr = packed Record
    family:word; { was byte, fixed }
    path:array[0..108] of char;
  end;

  TSockAddr = packed Record
    {$ifdef BSD}
     len : byte;
     family:byte;
    {$ELSE}
     family:word;  { was byte, fixed }
    {$ENDIF}
    data  :array [0..13] of char;
    end;

  TInetSockAddr = packed Record
    family:Word;
    port  :Word;
    addr  :Cardinal;
    pad   :array [1..8] of byte; { to get to the size of sockaddr... }
    end;

  TSockArray = Array[1..2] of Longint;

  THostEnt = record
    h_Name     : pchar;   { Official name }
    h_Aliases  : ppchar;  { Null-terminated list of aliases}
    h_Addrtype : longint; { Host address type }
    h_Length   : longint; { Length of address }
    h_Addr_list: ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

Var
  SocketError:Longint;

Function Connect(Sock:Longint;Var Addr;Addrlen:Longint):boolean;
Function Recv(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
Function Send(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function Socket(Domain,SocketType,Protocol:Longint):Longint;

function gethostbyname(name:pchar):PHostEnt;

//Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:File):Boolean;
//Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:text):Boolean;
//Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean;
//Function Accept(Sock:longint;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
//Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
//Function Bind(Sock:longint;const addr:string):boolean;
//Function Bind(Sock:Longint;Var Addr;AddrLen:Longint):Boolean;
//Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:file):Boolean;
//Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:text):Boolean;
//Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean;
//Function Connect(Sock:longint;const addr:TInetSockAddr;var SockIn,SockOut:text):Boolean;
//Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
//Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
//Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
//Function Listen (Sock,MaxConnect:Longint):Boolean;
//Function SetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
//Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;

//Procedure Sock2Text(Sock:Longint;Var SockIn,SockOut:Text);
//Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
//Procedure Sock2File(Sock:Longint;Var SockIn,SockOut:File);

implementation

uses doserrno;

Function Connect(Sock:Longint;Var Addr;Addrlen:Longint):boolean;
begin
  SocketError:=Sys_ENETDOWN;
  Connect:=false;
end;

Function Recv(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
begin
  SocketError:=Sys_ENETDOWN;
  recv:=0;
end;

Function Send(Sock:Longint;Var Addr;AddrLen,Flags:Longint):Longint;
begin
  SocketError:=Sys_ENETDOWN;
  send:=0;
end;

Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  SocketError:=Sys_ENETDOWN;
  shutdown:=0;
end;

Function Socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  SocketError:=Sys_ENETDOWN;
  Socket:=0;
end;

//function gethostbyaddr(addr:pchar; len:tOS_INT; adrtype:tOS_INT): PHostEnt;stdcall;
//begin
//  SocketError:=Sys_ENETDOWN;
//  gethostbyaddr:=nil;
//end;

function gethostbyname(name:pchar):PHostEnt;
begin
  SocketError:=Sys_ENETDOWN;
  gethostbyname:=nil;
end;

end.

{
  $Log$
  Revision 1.1  2001/05/16 01:59:16  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.1  2000/12/28 09:17:25  mk
  CL:- added DOS32 socket stubs

}


(*
 * ws2tcpip.h : TCP/IP specific extensions in Windows Sockets 2
 * (this version taken from Mingw and translated to Pascal)
 *
 * Portions Copyright (c) 1980, 1983, 1988, 1993
 * The Regents of the University of California.  All rights reserved.
 *
 *)

{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}

unit ws2tcpip;

interface

uses windows, winsock2;

const
  IP_OPTIONS =  1;
  IP_HDRINCL = 2;

(*
 * These are also be defined in winsock.h,
 * but values have changed for WinSock2 interface
 *)
  IP_TOS = 3; (* old (winsock 1.1) value 8 *)
  IP_TTL = 4; (* old value 7 *)
  IP_MULTICAST_IF = 9; (* old value 2 *)
  IP_MULTICAST_TTL = 10; (* old value 3 *)
  IP_MULTICAST_LOOP = 11; (* old value 4 *)
  IP_ADD_MEMBERSHIP = 12; (* old value 5 *)
  IP_DROP_MEMBERSHIP = 13; (* old value 6 *)
  IP_DONTFRAGMENT = 14; (* old value 9 *)
  IP_ADD_SOURCE_MEMBERSHIP = 15;
  IP_DROP_SOURCE_MEMBERSHIP = 16;
  IP_BLOCK_SOURCE = 17;
  IP_UNBLOCK_SOURCE = 18;
  IP_PKTINFO = 19;

(*
 * As with BSD implementation, IPPROTO_IPV6 level socket options have
 * same values as IPv4 counterparts.
 *)
  IPV6_UNICAST_HOPS = 4;
  IPV6_MULTICAST_IF = 9;
  IPV6_MULTICAST_HOPS = 10;
  IPV6_MULTICAST_LOOP = 11;
  IPV6_ADD_MEMBERSHIP = 12;
  IPV6_DROP_MEMBERSHIP = 13;
  IPV6_JOIN_GROUP = IPV6_ADD_MEMBERSHIP;
  IPV6_LEAVE_GROUP = IPV6_DROP_MEMBERSHIP;
  IPV6_PKTINFO = 19;

  IP_DEFAULT_MULTICAST_TTL = 1 ;
  IP_DEFAULT_MULTICAST_LOOP = 1 ;
  IP_MAX_MEMBERSHIPS = 20 ;

  TCP_EXPEDITED_1122 = 2;

  UDP_NOCHECKSUM = 1;

(* INTERFACE_INFO iiFlags *)
  IFF_UP = 1;
  IFF_BROADCAST = 2;
  IFF_LOOPBACK = 4;
  IFF_POINTTOPOINT = 8;
  IFF_MULTICAST = 16;

  SIO_GET_INTERFACE_LIST = IOC_OUT or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('t') shl 8) or 127;

  INET_ADDRSTRLEN = 16;
  INET6_ADDRSTRLEN = 46;

(* getnameinfo constants *) 
  NI_MAXHOST = 1025;
  NI_MAXSERV = 32;

  NI_NOFQDN = 1;
  NI_NUMERICHOST = 2;
  NI_NAMEREQD = 4;
  NI_NUMERICSERV = 8;
  NI_DGRAM = $10;

(* getaddrinfo constants *)
  AI_PASSIVE = 1;
  AI_CANONNAME = 2;
  AI_NUMERICHOST = 4;

(* getaddrinfo error codes *)
  EAI_AGAIN = WSATRY_AGAIN;
  EAI_BADFLAGS = WSAEINVAL;
  EAI_FAIL = WSANO_RECOVERY;
  EAI_FAMILY = WSAEAFNOSUPPORT;
  EAI_MEMORY = WSA_NOT_ENOUGH_MEMORY;
  EAI_NODATA = WSANO_DATA;
  EAI_NONAME = WSAHOST_NOT_FOUND;
  EAI_SERVICE = WSATYPE_NOT_FOUND;
  EAI_SOCKTYPE = WSAESOCKTNOSUPPORT;

(*
 *   ip_mreq also in winsock.h for WinSock1.1,
 *   but online msdn docs say it is defined here for WinSock2.
 *) 

type 
in_pktinfo = packed record
  ipi_addr: in_addr;
  ipi_ifiindex: UINT;
end;

ip_mreq = packed record
  imr_multiaddr, imr_interface: in_addr;
end;

ip_mreq_source = packed record
  imr_multiaddr, imr_sourceaddr, imr_interface: in_addr;
end;

ip_msfilter = packed record
  imsf_multiaddr, imsf_interface: in_addr;
  imsf_fmode, imfs_numsrc: u_long;
  imsf_slist: array [0..0] of in_addr;
end;

(* ipv6 *) 
(* These require XP or .NET Server or use of add-on IPv6 stacks on NT 4
  or higher *)

(* This is based on the example given in RFC 2553 with stdint types
   changed to BSD types.  For now, use these  field names until there
   is some consistency in MS docs. In this file, we only use the
   in6_addr structure start address, with casts to get the right offsets
   when testing addresses *)

type
in6_addr = Winsock2.in6_addr;
sockaddr_in6 = Winsock2.sockaddr_in6;

const
in6addr_any: in6_addr = ( Byte: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) );
in6addr_loopback: in6_addr = ( Byte: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1) );

(* Described in RFC 2292, but not in 2553 *)
(* int IN6_ARE_ADDR_EQUAL(const struct in6_addr * a, const struct in6_addr * b) *)
function IN6_ARE_ADDR_EQUAL(const a, b: in6_addr): boolean;

(* Address Testing Macros 

 These macro functions all take const struct in6_addr* as arg.
 Static inlines would allow type checking, but RFC 2553 says they
 macros.	 
 NB: These are written specifically for little endian host *)

function IN6_IS_ADDR_UNSPECIFIED(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_LOOPBACK(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MULTICAST(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_LINKLOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_SITELOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_V4MAPPED(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_V4COMPAT(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MC_NODELOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MC_LINKLOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MC_SITELOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MC_ORGLOCAL(const _addr: in6_addr): boolean;
function IN6_IS_ADDR_MC_GLOBAL(const _addr: in6_addr): boolean;

type
socklen_t = Winsock2.socklen_t;

ipv6_mreq = packed record
  ipv6mr_multiaddr, ipv6mr_interface: in6_addr;
end;

in6_pktinfo = packed record
  ipi6_addr: in6_addr;
  ipi6_ifiindex: UINT;
end;

paddrinfo = ^addrinfo;
addrinfo = packed record
  ai_flags, ai_family, ai_sockettype, ai_protocol: integer;
  ai_addrlen: size_t;
  ai_canonname: pchar;
  ai_addr: psockaddr;
  ai_next: paddrinfo;
end;
TPAddrInfoArray = array [0..32767] of addrinfo;
PPAddrInfo = ^ TPAddrInfoArray;

procedure freeaddrinfo(si: paddrinfo); stdcall; external 'ws2_32.dll';

function getaddrinfo(nodename, servname: PChar; hints: paddrinfo; res: ppaddrinfo): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getaddrinfo(nodename, servname: PChar; hints: paddrinfo; var res: paddrinfo): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getaddrinfo(nodename, servname: PChar; const hints: addrinfo; res: ppaddrinfo): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getaddrinfo(nodename, servname: PChar; const hints: addrinfo; var res: paddrinfo): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

{
char* WSAAPI gai_strerrorA(int);
WCHAR* WSAAPI gai_strerrorW(int);
#ifdef UNICODE
#define gai_strerror   gai_strerrorW
#else
#define gai_strerror   gai_strerrorA
#endif  (* UNICODE *)
}

function getnameinfo(sa: psockaddr; salen: socklen_t; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} external 'ws2_32.dll';
function getnameinfo(var sa: sockaddr; salen: socklen_t; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getnameinfo(var sa: sockaddr_in; salen: socklen_t; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getnameinfo(var sa: sockaddr_in6; salen: socklen_t; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

(* Some older IPv4/IPv6 compatability stuff *)
(* This struct lacks sin6_scope_id; retained for use in sockaddr_gen *)
type sockaddr_in6_old = Winsock2.sockaddr_in6;

implementation

function IN6_ARE_ADDR_EQUAL(const a, b: in6_addr): boolean;
begin
  result := (a.Int64[0] = b.Int64[0]) and (a.Int64[1] = b.Int64[1]);
end;

function IN6_IS_ADDR_UNSPECIFIED(const _addr: in6_addr): boolean;
begin
  result := (_addr.Int64[0] = 0) and (_addr.Int64[1] = 0);
end;

function IN6_IS_ADDR_LOOPBACK(const _addr: in6_addr): boolean;
begin
  result := (_addr.Int64[0] = 0) and
    (_addr.Int64[1] = (Int64($01) shl 56));
end;

function IN6_IS_ADDR_MULTICAST(const _addr: in6_addr): boolean;
begin
  result := _addr.Byte[0] = $FF;
end;

function IN6_IS_ADDR_LINKLOCAL(const _addr: in6_addr): boolean;
begin
  result := (_addr.Word[0] and $C0FF) = $80FE;
end;

function IN6_IS_ADDR_SITELOCAL(const _addr: in6_addr): boolean;
begin
  result := (_addr.Word[0] and $C0FF) = $C0FE;
end;

function IN6_IS_ADDR_V4MAPPED(const _addr: in6_addr): boolean;
begin
  result := (_addr.Int64[0] = 0) and (_addr.Long[2] = $FFFF0000);
end;

function IN6_IS_ADDR_V4COMPAT(const _addr: in6_addr): boolean;
begin
  result := (_addr.Int64[0] = 0) and (_addr.Long[2] = 0) and
    (_addr.Long[3] <> $01000000);
end;

function IN6_IS_ADDR_MC_NODELOCAL(const _addr: in6_addr): boolean;
begin
  result := _addr.Word[0] and $0FFF = $01FF;
end;

function IN6_IS_ADDR_MC_LINKLOCAL(const _addr: in6_addr): boolean;
begin
  result := _addr.Word[0] and $0FFF = $02FF;
end;

function IN6_IS_ADDR_MC_SITELOCAL(const _addr: in6_addr): boolean;
begin
  result := _addr.Word[0] and $0FFF = $04FF;
end;

function IN6_IS_ADDR_MC_ORGLOCAL(const _addr: in6_addr): boolean;
begin
  result := _addr.Word[0] and $0FFF = $08FF;
end;

function IN6_IS_ADDR_MC_GLOBAL(const _addr: in6_addr): boolean;
begin
  result := _addr.Word[0] and $0FFF = $0eFF;
end;

end.

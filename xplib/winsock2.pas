{ $Id: winsock2.pas,v 1.3 2004/02/09 02:49:09 cl Exp $

  Copyright (C) 2004 Claus Faerber
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

{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}

unit winsock2;

interface

uses windows;

{ types missing from Windows unit }
type
HANDLE = Cardinal;
PHANDLE = ^HANDLE;
LPHANDLE = PHANDLE;
USHORT = System.Word;
ULONGULONG = Int64;
PVOID = pointer;
LPVOID = PVOID;
LPGUID = PGUID;
LPINT = PINT;
DWORD_PTR = ^DWORD;
ULONG_PTR = ^ULONG;
TCHAR = char;

{ C-compatible types }
type
u_int = Windows.UINT;
u_char = Windows.UCHAR;
u_short = USHORT;
u_long = Windows.ULONG;
short = Windows.Short;
long = System.LongInt;
size_t = Integer;
socklen_t = size_t;

pu_long = ^u_long;
pu_short = ^u_short;

ppchar = ^pchar;

{ Winsock2 simple types }
type
WSAEVENT = type HANDLE;
TSOCKET = type Integer;
GROUP = Cardinal;

PWSAEVENT = ^WSAEVENT;
PGROUP = ^GROUP;

{ Winsock2 enums }
{$IFDEF Delphi}
type
eWINDOW_ADVANCE_METHOD = (
  E_WINDOW_ADVANCE_BY_TIME = 1,
  E_WINDOW_USE_AS_DATA_CACHE );

WSACOMPARATOR = (
  COMP_EQUAL = 0,
  COMP_NOTLESS );
{$ELSE}
type eWINDOW_ADVANCE_METHOD = type integer;
const E_WINDOW_ADVANCE_BY_TIME: eWINDOW_ADVANCE_METHOD = 1;
const E_WINDOW_USE_AS_DATA_CACHE: eWINDOW_ADVANCE_METHOD = 2;

type WSACOMPARATOR = type integer;
const COMP_EQUAL: WSACOMPARATOR = 0;
const COMP_NOTLESS: WSACOMPARATOR = 1;
{$ENDIF}

type
WSAESETSERVICEOP = (
  RNRSERVICE_REGISTER,
  RNRSERVICE_DEREGISTER,
  RNRSERVICE_DELETE );

{ Winsock constants }
const
  FD_SETSIZE = 64;

  WSADESCRIPTION_LEN = 256;
  WSASYS_STATUS_LEN = 128;

  MAX_PROTOCOL_CHAIN = 7;
  WSAPROTOCOL_LEN = 255;

  INVALID_SOCKET = TSocket(not 0);
  SOCKET_ERROR = (-1);

{ shutdown() how types }
  SD_RECEIVE      = $00;
  SD_SEND         = $01;
  SD_BOTH         = $02;

{ ioctlsocket() }
  IOCPARM_MASK = $7f;
  IOC_VOID  = $20000000;
  IOC_OUT   = $40000000;
  IOC_IN    = $80000000;
  IOC_INOUT = $C0000000;

  FIONBIO   = IOC_IN  or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 126;
  FIONREAD  = IOC_OUT or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 127;
  FIOASYNC  = IOC_IN  or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('f') shl 8) or 125;

  SIOCSHIWAT = IOC_IN  or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 0;
  SIOCGHIWAT = IOC_OUT or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 1;
  SIOCSLOWAT = IOC_IN  or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 2;
  SIOCGLOWAT = IOC_OUT or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 3;
  SIOCATMARK = IOC_OUT or (Long(Sizeof(u_long) and IOCPARM_MASK) shl 16) or (Ord('s') shl 8) or 7;

{ IP protocols }
  IPPROTO_IP = 0;
  IPPROTO_ICMP = 1;
  IPPROTO_IGMP = 2;
  IPPROTO_GGP = 3;
  IPPROTO_TCP = 6;
  IPPROTO_PUP = 12;
  IPPROTO_UDP = 17;
  IPPROTO_IDP = 22;
  IPPROTO_ND = 77;
  IPPROTO_RAW = 255;
  IPPROTO_MAX = 256;
  IPPROTO_HOPOPTS = 0;  { IPv6 Hop-by-Hop options }
  IPPROTO_IPV6 = 41; { IPv6 header }
  IPPROTO_ROUTING = 43; { IPv6 Routing header }
  IPPROTO_FRAGMENT = 44; { IPv6 fragmentation header }
  IPPROTO_ESP = 50; { encapsulating security payload }
  IPPROTO_AH = 51; { authentication header }
  IPPROTO_ICMPV6 = 58; { ICMPv6 }
  IPPROTO_NONE = 59; { IPv6 no next header }
  IPPROTO_DSTOPTS = 60; { IPv6 Destination options }

{ IP services }
  IPPORT_ECHO = 7;
  IPPORT_DISCARD = 9;
  IPPORT_SYSTAT = 11;
  IPPORT_DAYTIME = 13;
  IPPORT_NETSTAT = 15;
  IPPORT_FTP = 21;
  IPPORT_TELNET = 23;
  IPPORT_SMTP = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS = 43;
  IPPORT_MTP = 57;
  IPPORT_TFTP = 69;
  IPPORT_RJE = 77;
  IPPORT_FINGER = 79;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP = 95;
  IPPORT_EXECSERVER = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER = 514;
  IPPORT_EFSSERVER = 520;
  IPPORT_BIFFUDP = 512;
  IPPORT_WHOSERVER = 513;
  IPPORT_ROUTESERVER = 520;
  IPPORT_RESERVED = 1024;

  IMPLINK_IP = 155;
  IMPLINK_LOWEXPER = 156;
  IMPLINK_HIGHEXPER = 158;

  INADDR_ANY = 0;
  INADDR_LOOPBACK: LongWord = $7f000000;
  INADDR_BROADCAST: LongWord = LongWord(not 0);
  INADDR_NONE: LongWord = LongWord(not 0);

  ADDR_ANY = INADDR_ANY;

  IP_OPTIONS = 1;
  TCP_NODELAY = $0001;

  SO_DEBUG = 1;
  SO_ACCEPTCONN = 2;
  SO_REUSEADDR = 4;
  SO_KEEPALIVE = 8;
  SO_DONTROUTE = 16;
  SO_BROADCAST = 32;
  SO_USELOOPBACK = 64;
  SO_LINGER = 128;
  SO_OOBINLINE = 256;
  SO_DONTLINGER = $FFFF and not SO_LINGER;

  SO_SNDBUF = $1001;
  SO_RCVBUF = $1002;
  SO_SNDLOWAT = $1003;
  SO_RCVLOWAT = $1004;
  SO_SNDTIMEO = $1005;
  SO_RCVTIMEO = $1006;
  SO_ERROR = $1007;
  SO_TYPE = $1008;

  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;
  SOCK_RAW = 3;
  SOCK_RDM = 4;
  SOCK_SEQPACKET = 5;

  AF_UNSPEC = 0;
  AF_UNIX = 1;
  AF_INET = 2;
  AF_IMPLINK = 3;
  AF_PUP = 4;
  AF_CHAOS = 5;
  AF_IPX = 6;
  AF_NS = 6;
  AF_ISO = 7;
  AF_OSI = AF_ISO;
  AF_ECMA = 8;
  AF_DATAKIT = 9;
  AF_CCITT = 10;
  AF_SNA = 11;
  AF_DECnet = 12;
  AF_DLI = 13;
  AF_LAT = 14;
  AF_HYLINK = 15;
  AF_APPLETALK = 16;
  AF_NETBIOS = 17;
  AF_VOICEVIEW = 18;
  AF_FIREFOX = 19;
  AF_UNKNOWN1 = 20;
  AF_BAN = 21;
  AF_ATM = 22;
  AF_INET6 = 23;
  AF_CLUSTER = 24;
  AF_12844 = 25;
  AF_IRDA = 26;
  AF_NETDES = 28;
  AF_MAX = 29;                     

  PF_UNSPEC = AF_UNSPEC;
  PF_UNIX = AF_UNIX;
  PF_INET = AF_INET;
  PF_IMPLINK = AF_IMPLINK;
  PF_PUP = AF_PUP;
  PF_CHAOS = AF_CHAOS;
  PF_NS = AF_NS;
  PF_IPX = AF_IPX;
  PF_ISO = AF_ISO;
  PF_OSI = AF_OSI;
  PF_ECMA = AF_ECMA;
  PF_DATAKIT = AF_DATAKIT;
  PF_CCITT = AF_CCITT;
  PF_SNA = AF_SNA;
  PF_DECnet = AF_DECnet;
  PF_DLI = AF_DLI;
  PF_LAT = AF_LAT;
  PF_HYLINK = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_VOICEVIEW = AF_VOICEVIEW;
  PF_FIREFOX = AF_FIREFOX;
  PF_UNKNOWN1 = AF_UNKNOWN1;
  PF_BAN = AF_BAN;
  PF_ATM = AF_ATM;
  PF_INET6 = AF_INET6;
  PF_MAX = AF_MAX;

  SOL_SOCKET = $ffff;

  MSG_OOB = 1;
  MSG_PEEK = 2;
  MSG_DONTROUTE = 4;
  MSG_MAXIOVLEN = 16;
  MSG_INTERRUPT = $10;
  MSG_PARTIAL = $8000;
  MAXGETHOSTSTRUCT = 1024;

  FD_READ_BIT      = 0;
  FD_READ          = 1 shl FD_READ_BIT;
  FD_WRITE_BIT     = 1;
  FD_WRITE         = 1 shl FD_WRITE_BIT;
  FD_OOB_BIT       = 2;
  FD_OOB           = 1 shl FD_OOB_BIT;
  FD_ACCEPT_BIT    = 3;
  FD_ACCEPT        = 1 shl FD_ACCEPT_BIT;
  FD_CONNECT_BIT   = 4;
  FD_CONNECT       = 1 shl FD_CONNECT_BIT;
  FD_CLOSE_BIT     = 5;
  FD_CLOSE         = 1 shl FD_CLOSE_BIT;
  FD_QOS_BIT       = 6;
  FD_QOS           = 1 shl FD_QOS_BIT;
  FD_GROUP_QOS_BIT = 7;
  FD_GROUP_QOS     = 1 shl FD_GROUP_QOS_BIT;
  FD_ROUTING_INTERFACE_CHANGE_BIT = 8;
  FD_ROUTING_INTERFACE_CHANGE     = 1 shl FD_ROUTING_INTERFACE_CHANGE_BIT;
  FD_ADDRESS_LIST_CHANGE_BIT = 9;
  FD_ADDRESS_LIST_CHANGE     = 1 shl FD_ADDRESS_LIST_CHANGE_BIT;
  FD_MAX_EVENTS    = 10;
  FD_ALL_EVENTS    = (1 shl FD_MAX_EVENTS) - 1;

{ error codes }
  WSABASEERR = 10000;
  WSA_E_CANCELLED = WSABASEERR + 111;
  WSA_E_NO_MORE = WSABASEERR + 110;
  WSA_INVALID_HANDLE = Windows.ERROR_INVALID_HANDLE;
  WSA_INVALID_PARAMETER = Windows.ERROR_INVALID_PARAMETER;
  WSA_IO_INCOMPLETE = Windows.ERROR_IO_INCOMPLETE;
  WSA_IO_PENDING = Windows.ERROR_IO_PENDING;
  WSA_NOT_ENOUGH_MEMORY = Windows.ERROR_NOT_ENOUGH_MEMORY;
  WSA_OPERATION_ABORTED = Windows.ERROR_OPERATION_ABORTED;
  WSA_QOS_ADMISSION_FAILURE = WSABASEERR + 1010;
  WSA_QOS_BAD_OBJECT = WSABASEERR + 1013;
  WSA_QOS_BAD_STYLE = WSABASEERR + 1012;
  WSA_QOS_EFILTERCOUNT = WSABASEERR + 1021;
  WSA_QOS_EFILTERSTYLE = WSABASEERR + 1019;
  WSA_QOS_EFILTERTYPE = WSABASEERR + 1020;
  WSA_QOS_EFLOWCOUNT = WSABASEERR + 1023;
  WSA_QOS_EFLOWDESC = WSABASEERR + 1026;
  WSA_QOS_EFLOWSPEC = WSABASEERR + 1017;
  WSA_QOS_EOBJLENGTH = WSABASEERR + 1022;
  WSA_QOS_EPOLICYOBJ = WSABASEERR + 1025;
  WSA_QOS_EPROVSPECBUF = WSABASEERR + 1018;
  WSA_QOS_EPSFILTERSPEC = WSABASEERR + 1028;
  WSA_QOS_EPSFLOWSPEC = WSABASEERR + 1027;
  WSA_QOS_ESDMODEOBJ = WSABASEERR + 1029;
  WSA_QOS_ESERVICETYPE = WSABASEERR + 1016;
  WSA_QOS_ESHAPERATEOBJ = WSABASEERR + 1030;
  WSA_QOS_EUNKOWNPSOBJ = WSABASEERR + 1024;
  WSA_QOS_GENERIC_ERROR = WSABASEERR + 1015;
  WSA_QOS_NO_RECEIVERS = WSABASEERR + 1008;
  WSA_QOS_NO_SENDERS = WSABASEERR + 1007;
  WSA_QOS_POLICY_FAILURE = WSABASEERR + 1011;
  WSA_QOS_RECEIVERS = WSABASEERR + 1005;
  WSA_QOS_REQUEST_CONFIRMED = WSABASEERR + 1009;
  WSA_QOS_RESERVED_PETYPE = WSABASEERR + 1031;
  WSA_QOS_SENDERS = WSABASEERR + 1006;
  WSA_QOS_TRAFFIC_CTRL_ERROR = WSABASEERR + 1014;
  WSAEACCES = WSABASEERR + 13;
  WSAEADDRINUSE = WSABASEERR + 48;
  WSAEADDRNOTAVAIL = WSABASEERR + 49;
  WSAEAFNOSUPPORT = WSABASEERR + 47;
  WSAEALREADY = WSABASEERR + 37;
  WSAEBADF = WSABASEERR + 9;
  WSAECANCELLED = WSABASEERR + 103;
  WSAECONNABORTED = WSABASEERR + 53;
  WSAECONNREFUSED = WSABASEERR + 61;
  WSAECONNRESET = WSABASEERR + 54;
  WSAEDESTADDRREQ = WSABASEERR + 39;
  WSAEDISCON = WSABASEERR + 101;
  WSAEDQUOT = WSABASEERR + 69;
  WSAEFAULT = WSABASEERR + 14;
  WSAEHOSTDOWN = WSABASEERR + 64;
  WSAEHOSTUNREACH = WSABASEERR + 65;
  WSAEINPROGRESS = WSABASEERR + 36; { deprecated on WinSock2 } 
  WSAEINTR = WSABASEERR + 4;
  WSAEINVAL = WSABASEERR + 22;
  WSAEINVALIDPROCTABLE = WSABASEERR + 104;
  WSAEINVALIDPROVIDER = WSABASEERR + 105;
  WSAEISCONN = WSABASEERR + 56;
  WSAELOOP = WSABASEERR + 62;
  WSAEMFILE = WSABASEERR + 24;
  WSAEMSGSIZE = WSABASEERR + 40;
  WSAENAMETOOLONG = WSABASEERR + 63;
  WSAENETDOWN = WSABASEERR + 50;
  WSAENETRESET = WSABASEERR + 52;
  WSAENETUNREACH = WSABASEERR + 51;
  WSAENOBUFS = WSABASEERR + 55;
  WSAENOMORE = WSABASEERR + 102;
  WSAENOPROTOOPT = WSABASEERR + 42;
  WSAENOTCONN = WSABASEERR + 57;
  WSAENOTEMPTY = WSABASEERR + 66;
  WSAENOTSOCK = WSABASEERR + 38;
  WSAEOPNOTSUPP = WSABASEERR + 45;
  WSAEPFNOSUPPORT = WSABASEERR + 46;
  WSAEPROCLIM = WSABASEERR + 67;
  WSAEPROTONOSUPPORT = WSABASEERR + 43;
  WSAEPROTOTYPE = WSABASEERR + 41;
  WSAEPROVIDERFAILEDINIT = WSABASEERR + 106;
  WSAEREFUSED = WSABASEERR + 112;
  WSAEREMOTE = WSABASEERR + 71;
  WSAESHUTDOWN = WSABASEERR + 58;
  WSAESOCKTNOSUPPORT = WSABASEERR + 44;
  WSAESTALE = WSABASEERR + 70;
  WSAETIMEDOUT = WSABASEERR + 60;
  WSAETOOMANYREFS = WSABASEERR + 59;
  WSAEUSERS = WSABASEERR + 68;
  WSAEWOULDBLOCK = WSABASEERR + 35;
  WSAHOST_NOT_FOUND = WSABASEERR + 1001;
  WSANO_DATA = WSABASEERR + 1004;
  WSANO_RECOVERY = WSABASEERR + 1003;
  WSANOTINITIALISED = WSABASEERR + 93;
  WSASERVICE_NOT_FOUND = WSABASEERR + 108;
  WSASYSCALLFAILURE = WSABASEERR + 107;
  WSASYSNOTREADY = WSABASEERR + 91;
  WSATRY_AGAIN = WSABASEERR + 1002;
  WSATYPE_NOT_FOUND = WSABASEERR + 109;
  WSAVERNOTSUPPORTED = WSABASEERR + 92;

  FROM_PROTOCOL_INFO = -1;

  SO_GROUP_ID = $2001;
  SO_GROUP_PRIORITY = $2002;
  SO_MAX_MSG_SIZE = $2003;
  SO_PROTOCOL_INFOA = $2004;
  SO_PROTOCOL_INFOW = $2005;
  SO_PROTOCOL_INFO = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001;

  WSA_INVALID_EVENT = 0;
  WSA_MAXIMUM_WAIT_EVENTS = Windows.MAXIMUM_WAIT_OBJECTS;
  WSA_WAIT_FAILED = Windows.WAIT_FAILED;
  WSA_WAIT_EVENT_0 = Windows.WAIT_OBJECT_0;
  WSA_WAIT_IO_COMPLETION = Windows.WAIT_IO_COMPLETION;
  WSA_WAIT_TIMEOUT = Windows.WAIT_TIMEOUT;
  WSA_INFINITE = Windows.INFINITE;

  CF_ACCEPT = $0000;
  CF_REJECT = $0001;
  CF_DEFER = $0002;

  BASE_PROTOCOL      = 1;
  LAYERED_PROTOCOL   = 0;

  PFL_MULTIPLE_PROTO_ENTRIES          = $00000001;
  PFL_RECOMMENDED_PROTO_ENTRY         = $00000002;
  PFL_HIDDEN                          = $00000004;
  PFL_MATCHES_PROTOCOL_ZERO           = $00000008;
  XP1_CONNECTIONLESS                  = $00000001;
  XP1_GUARANTEED_DELIVERY             = $00000002;
  XP1_GUARANTEED_ORDER                = $00000004;
  XP1_MESSAGE_ORIENTED                = $00000008;
  XP1_PSEUDO_STREAM                   = $00000010;
  XP1_GRACEFUL_CLOSE                  = $00000020;
  XP1_EXPEDITED_DATA                  = $00000040;
  XP1_CONNECT_DATA                    = $00000080;
  XP1_DISCONNECT_DATA                 = $00000100;
  XP1_SUPPORT_BROADCAST               = $00000200;
  XP1_SUPPORT_MULTIPOINT              = $00000400;
  XP1_MULTIPOINT_CONTROL_PLANE        = $00000800;
  XP1_MULTIPOINT_DATA_PLANE           = $00001000;
  XP1_QOS_SUPPORTED                   = $00002000;
  XP1_INTERRUPT                       = $00004000;
  XP1_UNI_SEND                        = $00008000;
  XP1_UNI_RECV                        = $00010000;
  XP1_IFS_HANDLES                     = $00020000;
  XP1_PARTIAL_MESSAGE                 = $00040000;

  BIGENDIAN                           = $0000;
  LITTLEENDIAN                        = $0001;

  SECURITY_PROTOCOL_NONE              = $0000;
  JL_SENDER_ONLY    = $01;
  JL_RECEIVER_ONLY  = $02;
  JL_BOTH           = $04;
  WSA_FLAG_OVERLAPPED           = $01;
  WSA_FLAG_MULTIPOINT_C_ROOT    = $02;
  WSA_FLAG_MULTIPOINT_C_LEAF    = $04;
  WSA_FLAG_MULTIPOINT_D_ROOT    = $08;
  WSA_FLAG_MULTIPOINT_D_LEAF    = $10;
  IOC_UNIX                      = $00000000;
  IOC_WS2                       = $08000000;
  IOC_PROTOCOL                  = $10000000;
  IOC_VENDOR                    = $18000000;

  SIO_ASSOCIATE_HANDLE          = IOC_IN    or IOC_WS2 or 1;
  SIO_ENABLE_CIRCULAR_QUEUEING  = IOC_VOID  or IOC_WS2 or 2;
  SIO_FIND_ROUTE                = IOC_OUT   or IOC_WS2 or 3;
  SIO_FLUSH                     = IOC_VOID  or IOC_WS2 or 4;
  SIO_GET_BROADCAST_ADDRESS     = IOC_OUT   or IOC_WS2 or 5;
  SIO_GET_EXTENSION_FUNCTION_POINTER
   				= IOC_INOUT or IOC_WS2 or 6;
  SIO_GET_QOS                   = IOC_INOUT or IOC_WS2 or 7;
  SIO_GET_GROUP_QOS             = IOC_INOUT or IOC_WS2 or 8;
  SIO_MULTIPOINT_LOOPBACK       = IOC_IN    or IOC_WS2 or 9;
  SIO_MULTICAST_SCOPE           = IOC_IN    or IOC_WS2 or 10;
  SIO_SET_QOS                   = IOC_IN    or IOC_WS2 or 11;
  SIO_SET_GROUP_QOS             = IOC_IN    or IOC_WS2 or 12;
  SIO_TRANSLATE_HANDLE          = IOC_INOUT or IOC_WS2 or 13;
  SIO_ROUTING_INTERFACE_QUERY   = IOC_INOUT or IOC_WS2 or 20;
  SIO_ROUTING_INTERFACE_CHANGE  = IOC_IN    or IOC_WS2 or 21;
  SIO_ADDRESS_LIST_QUERY        = IOC_OUT   or IOC_WS2 or 22;
  SIO_ADDRESS_LIST_CHANGE       = IOC_VOID  or IOC_WS2 or 23;
  SIO_QUERY_TARGET_PNP_HANDLE   = IOC_OUT   or IOC_WS2 or 24;
  SIO_NSP_NOTIFY_CHANGE         = IOC_IN    or IOC_WS2 or 25;

  TH_NETDEV = $00000001;
  TH_TAPI = $00000002;

{ Winsock2 structures and complex types }
type
in_addr = packed record
  case integer of
    0: (S_un: packed record
          case integer of
            1: (S_un_b: packed record s_b1, s_b2, s_b3, s_b4: u_char; end);
            2: (S_un_w: packed record s_w1, s_w2: u_short; end);
            3: (S_addr: u_long);
            4: (s_b1, s_b2, s_b3, s_b4: u_char);
            5: (s_w1, s_w2: u_short);
        end);
    1: (S_un_b: packed record s_b1, s_b2, s_b3, s_b4: u_char; end);
    2: (S_un_w: packed record s_w1, s_w2: u_short; end);
    3: (S_addr: u_long);
    4: (s_b1, s_b2, s_b3, s_b4: u_char);
    5: (s_w1, s_w2: u_short);
end;

in6_addr = packed record
  case integer of
    0: (u: packed record
         case integer of
           1: (Byte: array [0..15] of u_char);
           2: (Word: array [0..7] of u_short);
           3: (Long: array[0..3] of u_long);
           4: (Int64: array[0..1] of Int64);
        end);
    1: (Byte: array [0..15] of u_char);
    2: (Word: array [0..8] of u_short);
    3: (Long: array[0..3] of u_long);
    4: (Int64: array[0..1] of Int64);
end;

sockaddr_in = packed record
  sin_family: short;
  sin_port: u_short;
  sin_addr: in_addr;
  sin_zero: array [0..7] of char;
end;
psockaddr_in = ^sockaddr_in;

sockaddr_in6 = packed record
  sin6_family: short;
  sin6_port: u_short;
  sin6_flowinfo: u_long;
  sin6_addr: in6_addr;
  sin6_scope_id: u_long;
end;
psockaddr_in6 = ^sockaddr_in6;

sockaddr_in6_old = packed record
  sin6_family: short;
  sin6_port: u_short;
  sin6_flowinfo: u_Long;
  sin6_addr: in_addr;
end;
psockaddr_in6_old = ^sockaddr_in6_old;

SOCKADDR_IRDA = packed record
  irdaAddressFamily: u_short;
  irdaDeviceId: array [0..3] of u_char;
  irdaServiceName: array[0..25] of Char;
end;
psockaddr_irda = ^sockaddr_irda;

sockaddr = packed record
  case integer of
    0: (ss_family: short);
    1: (sin_family: short;
        sin_port: u_short;
        sin_addr: in_addr;
        sin_zero: array [0..7] of char);
    2: (sin6_family: short;
        sin6_port: u_short;
        sin6_flowinfo: u_long;
        sin6_addr: in6_addr;
        sin6_scope_id: u_long);
end;
PSockAddr = ^sockaddr;
LPSockAddr = PSockAddr;

{ SOCKADDR_STORAGE }
const
  _SS_PAD1SIZE = SizeOf(Int64) - SizeOf(SmallInt);
  _SS_PAD2SIZE = SizeOf(Sockaddr) - 2 * SizeOf(Int64);

type
SOCKADDR_STORAGE = packed record
  ss_family: short;
  __ss_pad1: array [0.._SS_PAD1SIZE-1] of char;
  __ss_align: Int64;
  __ss_pad2: array[0.._SS_PAD2SIZE-1] of char;
end;

SOCKET_ADDRESS = packed record
  lpSockAddr: LPSOCKADDR;
  iSockAddrLength: Integer;
end;

type
AFPROTOCOLS = packed record
  iAddressFamily, iProtocol: integer;
end;
PAFPROTOCOLS = ^AFPROTOCOLS;
LPAFPROTOCOLS = PAFPROTOCOLS;

BLOB = packed record
  cbSize: Windows.ULONG;
  pBlobData: Windows.PByte;
end;
PBLOB = ^BLOB;
LPBLOB = PBLOB;

CSADDR_INFO = packed record
  LocalAddress, RemoteAddress: SOCKET_ADDRESS;
  iSocketType, iProtocol: Integer;
end;
PCSADDR_INFO = ^CSADDR_INFO;
LPCSADDR_INFO = PCSADDR_INFO;

TFD_SET = packed record
  fd_count: u_int;
  fd_array: array [0..FD_SETSIZE-1] of TSOCKET;
end;
pfd_set = ^TFD_SET;

hostent = packed record
  h_name: PChar;
  h_aliases: PPChar;
  h_addrtype, h_length: Windows.short;
  h_addr_list: PPChar;
end;
phostent = ^hostent;

linger = packed record
  l_onoff, l_linger: u_short;
end;

PROTOCOL_INFO = packed record
  dwServiceFlags: DWORD;
  iAddressFamily, iMaxSocketAddr, iMinSocketAddr, iSocketType, iProtocol: Integer;
  dwMessageSize: DWORD;
  lpProtocol: LPTSTR;
end;

protoent = packed record
  p_name: PChar;
  p_aliases: PPChar;
  p_proto: short;
end;
pprotoent = ^protoent;

RM_FEC_INFO = packed record
  FECBlockSize, FECProActivePackets: USHORT;
  FECGroupSize: UCHAR;
  fFECOnDemandParityEnabled: BOOL;
end;

RM_RECEIVER_STATS = packed record
  NumODataPacketsReceived, NumRDataPacketsReceived, NumDuplicateDataPackets,
  DataBytesReceived, TotalBytesReceived, RateKBitsPerSecOverall,
  RateKBitsPerSecLast, TrailingEdgeSeqId, LeadingEdgeSeqId,
  AverageSequencesInWindow, MinSequencesInWindow, MaxSequencesInWindow,
  FirstNakSequenceNumber, NumPendingNaks, NumOutstandingNaks,
  NumDataPacketsBuffered, TotalSelectiveNaksSent, TotalParityNaksSent: ULONGULONG;
end;

RM_SEND_WINDOW = packed record
  RateKBitsPerSec, WindowSizeInMSec, WindowSizeInBytes: ULONG;
end;

RM_SENDER_STATS = packed record
  DataBytesSent, TotalBytesSent, NaksReceived, NaksReceivedTooLate,
  NumOutstandingNaks, NumNaksAfterRData, RepairPacketsSent,
  BufferSpaceAvailable, TrailingEdgeSeqId, LeadingEdgeSeqId,
  RateKBitsPerSecOverall, RateKBitsPerSecLast, TotalODataPacketsSent: ULONGULONG; 
end;

servent = packed record
  s_name: PChar;
  s_aliases: PPChar;
  s_port: Short;
  s_proto: PChar;
end;
pservent = ^servent;

SERVICE_ADDRESS = packed record
  dwAddressType, dwAddressFlags, dwAddressLength, dwPrincipalLength: DWORD;
  lpAddress, lpPrincipal: PByte;
end;

SERVICE_ADDRESSES = array[0..32767] of SERVICE_ADDRESS; 
PSERVICE_ADDRESSES = ^SERVICE_ADDRESSES;
LPSERVICE_ADDRESSES = PSERVICE_ADDRESSES;

SERVICE_INFO = packed record
  lpServiceType: LPGUID;
  lpServiceName, lpComment, lpLocale: LPTSTR;
  dwDisplayHint, dwVersion, dwTime: DWORD;
  lpMachineName: LPTSTR;
  lpServiceAddresses: LPSERVICE_ADDRESSES;
  ServiceSpecificInfo: BLOB;
end;

SERVICE_TYPE_VALUE_ABS = packed record
  dwNameSpace, dwValueType, dwValueSize: DWORD;
  lpValueName: LPTSTR;
  lpValue: PVOID;
end;

SERVICE_TYPE_INFO_ABS = packed record
  lpTypeName: LPTSTR;
  dwValueCount: DWORD;
  Values: array [0..0] of SERVICE_TYPE_VALUE_ABS;
end;

NS_SERVICE_INFO = packed record
  dwNameSpace: DWORD;
  ServiceInfo: SERVICE_INFO;
end;

timeval = packed record
  tv_sec, tv_usec: long;
end;
ptimeval = ^timeval;

TRANSMIT_FILE_BUFFERS = packed record
  Head: PVOID;
  HeadLength: DWORD;
  Tail: PVOID;
  TailLength: DWORD;
end;

TRANSMIT_PACKETS_ELEMENT = packed record
  dwElFlags,cLength: ULONG;
  case integer of
    0: (nFileOffset: LARGE_INTEGER; hFile: HANDLE);
    1: (pBuffer: PVOID);
end;

WSABUF = packed record
  len: u_long;
  buf: PChar;
end;
PWSABUF = ^WSABUF;
LPWSABUF = PWSABUF;

WSAOVERLAPPED = packed record
  Internal, InternalHigh, Offset, OffsetHigh: DWORD;
  hEvent: WSAEVENT;
end;
PWSAOVERLAPPED = ^WSAOVERLAPPED;
LPWSAOVERLAPPED = PWSAOVERLAPPED;

WSAOVERLAPPED_COMPLETION_ROUTINE = procedure(dwError, cbTransferred: DWORD;
  lpOverlapped: LPWSAOVERLAPPED; dwFlags: DWORD);
PWSAOVERLAPPED_COMPLETION_ROUTINE = WSAOVERLAPPED_COMPLETION_ROUTINE;
LPWSAOVERLAPPED_COMPLETION_ROUTINE = PWSAOVERLAPPED_COMPLETION_ROUTINE;

WSACOMPLETIONTYPE = Cardinal;
WSACOMPLETION = packed record
  CompletionType: WSACOMPLETIONTYPE;
  case integer of
    1: (WindowMessage: packed record hWnd: HWND; uMsg: UINT; context: WPARAM end);
    2: (Event: packed record lpOverlapped: LPWSAOVERLAPPED end);
    3: (Apc: packed record lpOperlapped: LPWSAOVERLAPPED; lpfnCompletionProc: LPWSAOVERLAPPED_COMPLETION_ROUTINE; end);
    4: (Port: packed record lpOperlapped: LPWSAOVERLAPPED; hPort: HANDLE; Key: ULONG_PTR; end);

    5: (hWnd: HWND; uMsg: UINT; context: WPARAM);
    6: (lpOverlapped: LPWSAOVERLAPPED);
    7: (__3_lpOperlapped: LPWSAOVERLAPPED; lpfnCompletionProc: LPWSAOVERLAPPED_COMPLETION_ROUTINE);
    8: (__2_lpOperlapped: LPWSAOVERLAPPED; hPort: HANDLE; Key: ULONG_PTR);

    0: (Parameters: packed record
         case integer of
           1: (WindowMessage: packed record hWnd: HWND; uMsg: UINT; context: WPARAM end);
           2: (Event: packed record lpOverlapped: LPWSAOVERLAPPED end);
           3: (Apc: packed record lpOperlapped: LPWSAOVERLAPPED; lpfnCompletionProc: LPWSAOVERLAPPED_COMPLETION_ROUTINE; end);
           4: (Port: packed record lpOperlapped: LPWSAOVERLAPPED; hPort: HANDLE; Key: ULONG_PTR; end);

           5: (hWnd: HWND; uMsg: UINT; context: WPARAM);
           6: (lpOverlapped: LPWSAOVERLAPPED);
           7: (__3_lpOperlapped: LPWSAOVERLAPPED; lpfnCompletionProc: LPWSAOVERLAPPED_COMPLETION_ROUTINE);
           8: (__2_lpOperlapped: LPWSAOVERLAPPED; hPort: HANDLE; Key: ULONG_PTR);
       end);
end;
PWSACOMPLETION = ^WSACOMPLETION;
LPWSACOMPLETION = PWSACOMPLETION;

WSADATA = packed record
  wVersion, wHighVersion: WORD;
  szDescription: array [0..WSADESCRIPTION_LEN] of char;
  szSystemStatus: array [0..WSASYS_STATUS_LEN] of char;
  iMaxSockets, iMaxUdpDg: u_short;
  lpVendorInfo: PChar;
end;
PWSADATA = ^WSADATA;
LPWSADATA = PWSADATA;

WSAMSG = packed record
  name: LPSOCKADDR;
  namelen: integer;
  lpBuffers: LPWSABUF;
  dwBufferCount: DWORD;
  Control: WSABUF;
  dwFlags: DWORD;
end;
PWSAMSG = ^WSAMSG;
LPWSAMSG = PWSAMSG;

WSANAMESPACE_INFO = packed record
  NsProviderId: TGUID;
  dwNameSpace: DWORD;
  fActive: BOOL;
  dwVersion: DWORD;
  lpszIdentifier: LPTSTR;
end;
PWSANAMESPACE_INFO = ^WSANAMESPACE_INFO;
LPWSANAMESPACE_INFO = PWSANAMESPACE_INFO;

WSANETWORKEVENTS = packed record
  lNetworkEvents: long;
  iErrorCode: array [0..FD_MAX_EVENTS-1] of integer;
end;
PWSANETWORKEVENTS = ^WSANETWORKEVENTS;
LPWSANETWORKEVENTS = PWSANETWORKEVENTS;

WSANSCLASSINFO = packed record
  lpszName: LPSTR;
  dwNameSpace, dwValueType, dwValueSize: DWORD;
  lpValue: LPVOID;
end;
PWSANSCLASSINFO = ^WSANSCLASSINFO;
LPWSANSCLASSINFO = PWSANSCLASSINFO;

WSAPROTOCOLCHAIN = packed record
  ChainLen: integer;
  ChainEntries: array[0..MAX_PROTOCOL_CHAIN-1] of DWORD;
end;

WSAPROTOCOL_INFO = packed record
  dwServiceFlags1, dwServiceFlags2, dwServiceFlags3, dwServiceFlags4: DWORD;
  dwProciderFlags: DWORD;
  ProviderID: TGUID;
  dbCatalogEntryId: DWORD;
  ProtocolChain: WSAPROTOCOLCHAIN;
  iVersion, iAddressFamily, iMaxSockAddr, iMinSockAddr, iSocketType, iProtocol,
  iProtocolMaxOffset, iNetworkByteOrder, iSecurityScheme: integer;
  dwMessageSize, dwProviderReserved: DWORD;
  szProtocol: array [0..WSAPROTOCOL_LEN] of TCHAR;
end;
PWSAPROTOCOL_INFO = ^WSAPROTOCOL_INFO;
LPWSAPROTOCOL_INFO = PWSAPROTOCOL_INFO;

WSAVERSION = packed record
  dwVersion: DWORD;
  ecHow: WSACOMPARATOR;
end;
PWSAVERSION =  ^WSAVERSION;
LPWSAVERSION = PWSAVERSION;

WSAQUERYSET = packed record
  dwSize: DWORD;
  lpszServiceInstanceName: LPTSTR;
  lpServiceClassId: LPGUID;
  lpVersion: LPWSAVERSION;
  lpszComment: LPTSTR;
  dwNameSpace: DWORD;
  lpNsProviderId: LPGUID;
  lpszContext: LPTSTR;
  dwNumberOfProtocols: DWORD;
  lpafpProtocols: LPAFPROTOCOLS;
  lpszQueryString: LPTSTR;
  dwNumberOfCsAddrs: DWORD;
  lpcsaBuffer: LPCSADDR_INFO;
  dwOutputFlags: DWORD;
  lpBlob: LPBLOB;
end;
PWSAQUERYSET = ^WSAQUERYSET;
LPWSAQUERYSET = PWSAQUERYSET;

WSASERVICECLASSINFO = packed record
  lpServiceClassId: LPGUID;
  lpszServiceClassName: LPTSTR;
  dwCount: DWORD;
  lpClassInfos: LPWSANSCLASSINFO;
end;
PWSASERVICECLASSINFO = ^WSASERVICECLASSINFO;
LPWSASERVICECLASSINFO = PWSASERVICECLASSINFO;

{ Winsock2 functions }
type
LPSERVICE_ASYNC_INFO = pointer;

function accept(s: TSocket; addr: psockaddr; addrlen: PInteger): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr; addrlen: PInteger): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_in; addrlen: PInteger): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_in6; addrlen: PInteger): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_irda; addrlen: PInteger): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; addr: psockaddr; var addrlen: Integer): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr; var addrlen: Integer): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_in; var addrlen: Integer): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_in6; var addrlen: Integer): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';
function accept(s: TSocket; var addr: sockaddr_irda; var addrlen: Integer): TSocket; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'accept';

function bind(s: TSocket; name: psockaddr; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'bind';
function bind(s: TSocket; const name: sockaddr; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'bind';
function bind(s: TSocket; const name: sockaddr_in; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'bind';
function bind(s: TSocket; const name: sockaddr_in6; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'bind';
function bind(s: TSocket; const name: sockaddr_irda; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'bind';

function closesocket(s: TSocket): integer; stdcall; external 'ws2_32.dll' name 'closesocket';

function connect(s: TSocket; name: psockaddr; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'connect';
function connect(s: TSocket; const name: sockaddr; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'connect';
function connect(s: TSocket; const name: sockaddr_in; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'connect';
function connect(s: TSocket; const name: sockaddr_in6; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'connect';
function connect(s: TSocket; const name: sockaddr_irda; namelen: Integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'connect';

// function gai_strerror(ecode: integer): PChar; stdcall; external 'ws2_32.dll';
// function GetAddressByName(dwNameSpace: DWORD; lpServiceType: LPGUID; lpServiceName: LPTSTR; lpiProtocols: LPINT;
//   dwResulution: DWORD; lpServiceAsyncInfo: LPSERVICE_ASYNC_INFO; lpCsaddrBuffer: LPVOID; lpdwBufferLength: LPDWORD;
//   lpAliasBuffer: LPTSTR; lpdwAliasBufferLength: LPDWORD): integer; stdcall; external 'ws2_32.dll';

function gethostbyaddr(addr: pchar; len, addrtype: integer): phostent; stdcall; external 'ws2_32.dll';
function gethostbyname(addr: pchar): phostent; stdcall; external 'ws2_32.dll';
function gethostname(name: pchar; namelen: integer): integer; stdcall; external 'ws2_32.dll';
// function GetNameByType(lpServiceType: LPGUID; lpServiceName: LPTSTR; dwNameLength: DWORD): integer;

function getpeername(s: TSocket; name: psockaddr; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_in; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_in6; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_irda; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; name: psockaddr; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_in; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_in6; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getpeername(s: TSocket; var name: sockaddr_irda; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

function getprotobyname(name: pchar): pprotoent; stdcall; external 'ws2_32.dll';
function getprotobynumber(number: integer): pprotoent; stdcall; external 'ws2_32.dll';
function getservbyname(name, proto: pchar): pservent; stdcall; external 'ws2_32.dll';
function getservbyport(port: integer; proto: pchar): pservent; stdcall; external 'ws2_32.dll';

// function GetService(dwNameSpace: DWORD; lpGuid: LPGUID; lpServiceName: LPTSTR; dwProperties: DWORD; lpBuffer: LPVOID; lpdwBufferSize: LPDWORD; lpServiceAsyncInfo: LPSERVICE_ASYNC_INFO): integer; stdcall; external 'ws2_32.dll';

function getsockname(s: TSocket; name: psockaddr; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_in; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_in6; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_irda; namelen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; name: psockaddr; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_in; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_in6; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockname(s: TSocket; var name: sockaddr_irda; var namelen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

// function getsockopt(s: TSocket; level, optname: integer; optval: pchar; optlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function getsockopt(s: TSocket; level, optname: integer; var optval; optlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

// function GetTypeByName(lpServiceName LPTSTR; lpServiceType: PGUID): integer;
function htonl(hostlong: u_long): u_long; stdcall; external 'ws2_32.dll';
function htons(hostshort: u_short): u_short; stdcall; external 'ws2_32.dll';
function inet_addr(cp: pchar): u_long; stdcall; external 'ws2_32.dll';
function inet_ntoa(ina: in_addr): PChar; stdcall; external 'ws2_32.dll';

// function ioctlsocket(s: TSocket; cmd: long; argp: pointer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function ioctlsocket(s: TSocket; cmd: long; var arg): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

function listen(s: TSocket; backlog: integer): integer; stdcall; external 'ws2_32.dll';

function ntohl(netlong: u_long): u_long; stdcall; external 'ws2_32.dll';
function ntohs(netshort: u_short): u_short; stdcall; external 'ws2_32.dll';

function recv(s: TSocket; var buf; len, flags: integer): integer; stdcall; external 'ws2_32.dll';

function recvfrom(s: TSocket; var buf; len, flags: integer; from: psockaddr; fromlen: pinteger): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function recvfrom(s: TSocket; var buf; len, flags: integer; var from: sockaddr; var fromlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function recvfrom(s: TSocket; var buf; len, flags: integer; var from: sockaddr_in; var fromlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function recvfrom(s: TSocket; var buf; len, flags: integer; var from: sockaddr_in6; var fromlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function recvfrom(s: TSocket; var buf; len, flags: integer; var from: sockaddr_irda; var fromlen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

function select(nfds: integer; readfds, writefds, exceptfds: pfd_set; timeout: ptimeval): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function select(nfds: integer; const readfds, writefds, exceptfds: tfd_set; timeout: ptimeval): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function select(nfds: integer; readfds, writefds, exceptfds: pfd_set; const timeout: timeval): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function select(nfds: integer; const readfds, writefds, exceptfds: tfd_set; const timeout: timeval): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

function send(s: TSocket; const buf; len, flags: integer): integer; stdcall; external 'ws2_32.dll';

function sendto(s: TSocket; const buf; len, flags: integer; to_: psockaddr; tolen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function sendto(s: TSocket; const buf; len, flags: integer; const to_: sockaddr; tolen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function sendto(s: TSocket; const buf; len, flags: integer; const to_: sockaddr_in; tolen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function sendto(s: TSocket; const buf; len, flags: integer; const to_: sockaddr_in6; tolen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';
function sendto(s: TSocket; const buf; len, flags: integer; const to_: sockaddr_irda; tolen: integer): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll';

// function SetService(dwNameSpace, dwOperation, dwFlags: DWORD; lpServiceInfo: LPSERVICE_INFO; lpServiceAsyncInfo: LPSERVIE_ASYNC_INFO; lpdwStatusFlags: LPDWORD): integer;

function setsockopt(s: TSocket; level, optname: integer; const optval; optlen: integer): integer; stdcall; external 'ws2_32.dll';

function shutdown(s: TSocket; how: integer): integer; stdcall; external 'ws2_32.dll';
function socket(af, type_, protocol: integer): TSocket; stdcall; external 'ws2_32.dll';

type
  LPQOS = pointer;
  LPCONDITIONPROC = function(lpCallerId, lpCallerData: LPWSABUF; lpSQOS, lpGQOS: LPQOS; lpCalleeId, lpCalleeData: LPWSABUF; g: PGROUP; dwCallbackData: DWORD_PTR): integer;

function WSAAccept(s: TSocket; addr: psockaddr; addrlen: LPINT; lpfnCondition: LPCONDITIONPROC; dwCallbackData: DWORD): TSocket; stdcall; external 'ws2_32.dll';
function WSAAddressToString(lpsaAddress: LPSOCKADDR; dwAddressLength: DWORD; lpProtocolInfo: LPWSAPROTOCOL_INFO; lpszAddressString: LPTSTR; lpdwAddressStringLength: LPDWORD): integer; stdcall; external 'ws2_32.dll' name 'WSAAddressToStringA';
function WSAAsyncGetHostByAddr(hWnd: HWND; wMsg: UINT; addr: PChar; len, type_: integer; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSAAsyncGetHostByName(hWnd: HWND; wMsg: UINT; name: PChar; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSAAsyncGetProtoByName(hWnd: HWND; wMsg: UINT; name: PChar; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSAAsyncGetProtoByNumber(hWnd: HWND; wMsg: UINT; number: integer; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSAAsyncGetServByName(hWnd: HWND; wMsg: UINT; name, proto: PChar; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSAAsyncGetServByPort(hWnd: HWND; wMsg: UINT; port: integer; proto: PChar; buf: PChar; buflen: integer): HANDLE; stdcall; external 'ws2_32.dll';
function WSACancelAsyncRequest(hAsyncTaskHandle: HANDLE): integer; stdcall; external 'ws2_32.dll';
function WSACleanup: integer; stdcall; external 'ws2_32.dll';
function WSACloseEvent(hEvent: WSAEVENT): BOOL; stdcall; external 'ws2_32.dll';
function WSAConnect(s: TSocket; name: psockaddr; namelen: integer; lpCallerData, lpCalleeData: LPWSABUF; lpSQOS, lpGQOS: LPQOS): integer; stdcall; external 'ws2_32.dll';
function WSACreateEvent: WSAEVENT; stdcall; external 'ws2_32.dll';
function WSADuplicateSocket(s: TSocket; dwProcessId: DWORD; lpProtocolInfo: LPWSAPROTOCOL_INFO): integer; stdcall; external 'ws2_32.dll' name 'WSADuplicateSocketA';
function WSAEnumNameSpaceProviders(lpdwBufferLength: LPDWORD; lpnspBuffer: LPWSANAMESPACE_INFO): integer; stdcall; external 'ws2_32.dll' name 'WSAEnumNameSpaceProvidersA';
function WSAEnumNetworkEvents(lpdwBufferLength: LPDWORD; hEventObject: WSAEVENT; lpNetworkEvents: LPWSANETWORKEVENTS): integer; stdcall; external 'ws2_32.dll';
function WSAEnumProtocols(lpiProtocols: LPINT; lpProtocolBuffer: LPWSAPROTOCOL_INFO; lpdwBufferLength: LPDWORD): integer; stdcall; external 'ws2_32.dll' name 'WSAEnumProtocolsA';
function WSAEventSelect(s: TSocket; hEventObject: WSAEVENT; lNetworkEvents: long): integer; stdcall; external 'ws2_32.dll';
function __WSAFDIsSet(fd: TSocket; set_: pfd_set): boolean; stdcall; external 'ws2_32.dll';
function WSAGetLastError: integer; stdcall; external 'ws2_32.dll';
function WSAGetOverlappedResult(s: TSocket; lpOverlapped: LPWSAOVERLAPPED; lpobTransfer: LPDWORD; fWait: BOOL; lpdwFlags: LPDWORD): BOOL; stdcall; external 'ws2_32.dll';
function WSAGetQOSByName(s: TSocket; lpQOSName: LPWSABUF; lpQos: LPQOS): BOOL; stdcall; external 'ws2_32.dll';
function WSAGetServiceClassInfo(lpProviderId, lpServiceClassId: LPGUID; lpdwBufferLength: LPDWORD; lpServiceClassInfo: LPWSASERVICECLASSINFO): integer; stdcall; external 'ws2_32.dll' name 'WSAGetServiceClassInfoA';
function WSAGetServiceClassNameByClassId(lpServiceClassId: LPGUID; lpszServiceClassName: LPTSTR; lpdwBufferLength: LPDWORD): integer; stdcall; external 'ws2_32.dll' name 'WSAGetServiceClassNameByClassIdA';
function WSAHtonl(s: TSocket; hostlong: u_long; lpnetlong: pu_long): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAHtonl';
function WSAHtonl(s: TSocket; hostlong: u_long; var netlong: u_long): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAHtonl';
function WSAHtons(s: TSocket; hostshort: u_short; lpnetshort: pu_short): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAHtons';
function WSAHTons(s: TSocket; hostshort: u_short; var netshort: u_short): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAHtons';
function WSAInstallServiceClass(lpServiceClassInfo: LPWSASERVICECLASSINFO): Integer; stdcall; external 'ws2_32.dll' name 'WSAInstallServiceClassA';
function WSAIoctl(s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: LPVOID; cbInBuffer: DWORD; lpvOutBuffer: LPVOID; cbOutBuffer: DWORD; lpcbBytesReturned: LPDWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSAJoinLeaf(s: TSocket; name: psockaddr; namelen: integer; lpCallerData, lpCalleeData: LPWSABUF; lpSQOS, lpGQOS: LPQOS; dwFlags: DWORD): TSocket; stdcall; external 'ws2_32.dll';
function WSALookupServiceBegin(lpqsRestrictions: LPWSAQUERYSET; dwControlFlags: DWORD; lphLookup: LPHANDLE): integer; stdcall; external 'ws2_32.dll' name 'WSALookupServiceBeginA';
function WSALookupServiceEnd(hLookup: HANDLE): integer; stdcall; external 'ws2_32.dll';
function WSALookupServiceNext(hLookup: HANDLE; dwControlFlags: DWORD; lpdwBufferLength: LPDWORD; lpqsRequest: LPWSAQUERYSET): integer; stdcall; external 'ws2_32.dll' name 'WSALookupServiceNextA';
function WSANSPIoctl(hLookup: HANDLE; dwControlCode: DWORD; lpvInBuffer: LPVOID; cbInBuffer: DWORD; lpvOutBuffer: LPVOID; cbOutBufffer: DWORD; lpcbBytesReturned: LPDWORD; lpCompletion: LPWSACOMPLETION): integer; stdcall; external 'ws2_32.dll';
function WSANtohl(s: TSocket; netlong: u_long; lphostlong: pu_long): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSANtohl';
function WSANtohl(s: TSocket; netlong: u_long; var hostlong: u_long): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSANtohl';
function WSANtohs(s: TSocket; netshort: u_short; lphostshort: pu_short): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSANtohs';
function WSANTohs(s: TSocket; netshort: u_short; var hostshort: u_short): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSANtohs';
function WSAProviderConfigChange(lpNotificationHandle: LPHANDLE; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSARecv(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesRecvd, lpFlags: LPDWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSARecvDisconnect(s: TSocket; lpInboundDisconnetData: LPWSABUF): integer; stdcall; external 'ws2_32.dll';
// mswsock: function WSARecvEx(s: TSocket; buf: Pchar; len: integer; flags: pinteger): integer; stdcall; external 'ws2_32.dll';
function WSARecvFrom(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberOfBytesRecvd, lpFlags: LPDWORD; lpFrom: psockaddr; lpFromLen: LPINT; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
// mswsock: function WSARecvMsg(s: TSocket; lpMsg: LPWSAMSG; lpNumberOfBytesRecvd, lpFlags: LPDWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSARemoveServiceClass(lpServiceClassId: LPGUID): integer; stdcall; external 'ws2_32.dll';
function WSAResetEvent(hEvent: WSAEVENT): BOOL; stdcall; external 'ws2_32.dll';
function WSASend(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberofBytesSent: LPDWORD; dwFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSASendDisconnect(s: TSocket; lpOutboundDisconnetData: LPWSABUF): integer; stdcall; external 'ws2_32.dll';
function WSASendTo(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; lpNumberofBytesSent: LPDWORD; dwFlags: DWORD; lpTo: psockaddr; lpToLen: LPINT; lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): integer; stdcall; external 'ws2_32.dll';
function WSASetEvent(hEvent: WSAEVENT): BOOL; stdcall; external 'ws2_32.dll';
procedure WSASetLastError(iError: integer); stdcall; external 'ws2_32.dll';
function WSASetService(lpqsRegInfo: LPWSAQUERYSET; essOperation: WSAESETSERVICEOP; dwControlFlags: DWORD): integer; stdcall; external 'ws2_32.dll' name 'WSASetServiceA';
function WSASocket(af, type_, protocol: integer; lpProtocolInfo: LPWSAPROTOCOL_INFO; g: GROUP; dwFlags: DWORD): TSocket; stdcall; external 'ws2_32.dll' name 'WSASocketA';
function WSAStartup(wVersionRequested: WORD; lpWSAData: LPWSADATA): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAStartup';
function WSAStartup(wVersionRequested: WORD; var WSAData: WSADATA): integer; {$IFNDEF FPC} overload; {$ENDIF} stdcall; external 'ws2_32.dll' name 'WSAStartup';
function WSAStringToAddress(AddressString: LPTSTR; AddressFamily: integer; lpProtocolInfo: LPWSAPROTOCOL_INFO; lpAddress: LPSOCKADDR; lpAddressLength: LPINT): integer; stdcall; external 'ws2_32.dll' name 'WSAStringToAddressA';
function WSAWaitForMultipleEvents(cEvents: DWORD; lphEvents: PWSAEVENT; fWaitAll: BOOL; dwTimeout: DWORD; fAlertable: BOOL): DWORD; stdcall; external 'ws2_32.dll';

{ FD_* functions}
procedure FD_CLR(s: TSocket; var Set_: TFD_SET);
function FD_ISSET(s: TSocket; var Set_: TFD_SET): boolean; stdcall; external 'ws2_32.dll' name '__WSAFDIsSet';
procedure FD_SET(s: TSocket; var Set_: TFD_SET); // renamed due to conflict with TFD_SET (above)
procedure FD_ZERO(var set_: TFD_SET);

{ Delphi Winsock1 compatibiliby }
type
TSockAddr = sockaddr;
TSockAddrIn = sockaddr_in;
TWSAData = WSAData;
TFDSet = TFD_SET;
TTimeVal = timeval;

{ FPC Winsock1 compatibiliby }
type
TInetSockAddr = sockaddr_in;

implementation

procedure FD_CLR(s: TSocket; var Set_: TFD_SET);
var i: integer;
begin
  if Set_.fd_count > 0 then
    for i := 0 to Set_.fd_count - 1 do
      if Set_.fd_array[i] = s then begin
        if i < set_.fd_count - 1 then
          Move(Set_.fd_array[i+1],Set_.fd_array[i],
           (Set_.fd_count - i - 1) * sizeof(set_.fd_array[0]));
        dec(set_.fd_count);
        exit;
      end;
end;
(*
function FD_ISSET(s: TSocket; var Set_: TFD_SET): Boolean;
var i: integer;
begin
  if Set_.fd_count > 0 then
    for i := 0 to Set_.fd_count -1 do
      if Set_.fd_array[i] = s then begin
        result := true;
        exit;
      end;
  result := false;
end;
*)

procedure FD_SET(s: TSocket; var Set_: TFD_SET);
var i: integer;
begin
  if Set_.fd_count > 0 then
    for i := 0 to Set_.fd_count - 1 do
      if Set_.fd_array[i] = s then
        exit;
  if Set_.fd_count <= High(Set_.fd_array) - Low(Set_.fd_array) then begin
    Set_.fd_array[Set_.fd_count] := s;
    Inc(Set_.fd_count);
  end;
end;

procedure FD_ZERO(var set_: TFD_SET);
begin
  FillChar(set_, sizeof(set_), 0);
end;

end.

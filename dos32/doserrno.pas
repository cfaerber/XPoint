{  $Id: doserrno.pas,v 1.1 2000/12/28 09:17:25 mk Exp $

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

   Based on errno.inc, part of the Free Pascal run time library.
   Based on errors.pp, part of the Free Pascal run time library.
}

Unit DOSErrNo;

Interface

uses strings;

const
  sys_errn=125;

  Sys_EPERM	= 1;	{ Operation not permitted }
  Sys_ENOENT	= 2;	{ No such file or directory }
  Sys_ESRCH	= 3;	{ No such process }
  Sys_EINTR	= 4;	{ Interrupted system call }
  Sys_EIO	= 5;	{ I/O error }
  Sys_ENXIO	= 6;	{ No such device or address }
  Sys_E2BIG	= 7;	{ Arg list too long }
  Sys_ENOEXEC	= 8;	{ Exec format error }
  Sys_EBADF	= 9;	{ Bad file number }
  Sys_ECHILD	= 10;	{ No child processes }
  Sys_EAGAIN	= 11;	{ Try again }
  Sys_ENOMEM	= 12;	{ Out of memory }
  Sys_EACCES	= 13;	{ Permission denied }
  Sys_EFAULT	= 14;	{ Bad address }
  Sys_ENOTBLK	= 15;	{ Block device required }
  Sys_EBUSY	= 16;	{ Device or resource busy }
  Sys_EEXIST	= 17;	{ File exists }
  Sys_EXDEV	= 18;	{ Cross-device link }
  Sys_ENODEV	= 19;	{ No such device }
  Sys_ENOTDIR	= 20;	{ Not a directory }
  Sys_EISDIR	= 21;	{ Is a directory }
  Sys_EINVAL	= 22;	{ Invalid argument }
  Sys_ENFILE	= 23;	{ File table overflow }
  Sys_EMFILE	= 24;	{ Too many open files }
  Sys_ENOTTY	= 25;	{ Not a typewriter }
  Sys_ETXTBSY	= 26;	{ Text file busy }
  Sys_EFBIG	= 27;	{ File too large }
  Sys_ENOSPC	= 28;	{ No space left on device }
  Sys_ESPIPE	= 29;	{ Illegal seek }
  Sys_EROFS	= 30;	{ Read-only file system }
  Sys_EMLINK	= 31;	{ Too many links }
  Sys_EPIPE	= 32;	{ Broken pipe }
  Sys_EDOM	= 33;	{ Math argument out of domain of func }
  Sys_ERANGE	= 34;	{ Math result not representable }
  Sys_EDEADLK	= 35;	{ Resource deadlock would occur }
  Sys_ENAMETOOLONG= 36;	{ File name too long }
  Sys_ENOLCK	= 37;	{ No record locks available }
  Sys_ENOSYS	= 38;	{ Function not implemented }
  Sys_ENOTEMPTY= 39;	{ Directory not empty }
  Sys_ELOOP	= 40;	{ Too many symbolic links encountered }
  Sys_EWOULDBLOCK	= Sys_EAGAIN;	{ Operation would block }
  Sys_ENOMSG	= 42;	{ No message of desired type }
  Sys_EIDRM	= 43;	{ Identifier removed }
  Sys_ECHRNG	= 44;	{ Channel number out of range }
  Sys_EL2NSYNC= 45;	{ Level 2 not synchronized }
  Sys_EL3HLT	= 46;	{ Level 3 halted }
  Sys_EL3RST	= 47;	{ Level 3 reset }
  Sys_ELNRNG	= 48;	{ Link number out of range }
  Sys_EUNATCH	= 49;	{ Protocol driver not attached }
  Sys_ENOCSI	= 50;	{ No CSI structure available }
  Sys_EL2HLT	= 51;	{ Level 2 halted }
  Sys_EBADE	= 52;	{ Invalid exchange }
  Sys_EBADR	= 53;	{ Invalid request descriptor }
  Sys_EXFULL	= 54;	{ Exchange full }
  Sys_ENOANO	= 55;	{ No anode }
  Sys_EBADRQC	= 56;	{ Invalid request code }
  Sys_EBADSLT	= 57;	{ Invalid slot }
  Sys_EDEADLOCK= 58;	{ File locking deadlock error }
  Sys_EBFONT	= 59;	{ Bad font file format }
  Sys_ENOSTR	= 60;	{ Device not a stream }
  Sys_ENODATA	= 61;	{ No data available }
  Sys_ETIME	= 62;	{ Timer expired }
  Sys_ENOSR	= 63;	{ Out of streams resources }
  Sys_ENONET	= 64;	{ Machine is not on the network }
  Sys_ENOPKG	= 65;	{ Package not installed }
  Sys_EREMOTE	= 66;	{ Object is remote }
  Sys_ENOLINK	= 67;	{ Link has been severed }
  Sys_EADV	= 68;	{ Advertise error }
  Sys_ESRMNT	= 69;	{ Srmount error }
  Sys_ECOMM	= 70;	{ Communication error on send }
  Sys_EPROTO	= 71;	{ Protocol error }
  Sys_EMULTIHOP= 72;	{ Multihop attempted }
  Sys_EDOTDOT	= 73;	{ RFS specific error }
  Sys_EBADMSG	= 74;	{ Not a data message }
  Sys_EOVERFLOW= 75;	{ Value too large for defined data type }
  Sys_ENOTUNIQ= 76;	{ Name not unique on network }
  Sys_EBADFD	= 77;	{ File descriptor in bad state }
  Sys_EREMCHG	= 78;	{ Remote address changed }
  Sys_ELIBACC	= 79;	{ Can not access a needed shared library }
  Sys_ELIBBAD	= 80;	{ Accessing a corrupted shared library }
  Sys_ELIBSCN	= 81;	{ .lib section in a.out corrupted }
  Sys_ELIBMAX	= 82;	{ Attempting to link in too many shared libraries }
  Sys_ELIBEXEC= 83;	{ Cannot exec a shared library directly }
  Sys_EILSEQ	= 84;	{ Illegal byte sequence }
  Sys_ERESTART= 85;	{ Interrupted system call should be restarted }
  Sys_ESTRPIPE= 86;	{ Streams pipe error }
  Sys_EUSERS	= 87;	{ Too many users }
  Sys_ENOTSOCK= 88;	{ Socket operation on non-socket }
  Sys_EDESTADDRREQ= 89;	{ Destination address required }
  Sys_EMSGSIZE= 90;	{ Message too long }
  Sys_EPROTOTYPE= 91;	{ Protocol wrong type for socket }
  Sys_ENOPROTOOPT= 92;	{ Protocol not available }
  Sys_EPROTONOSUPPORT= 93;	{ Protocol not supported }
  Sys_ESOCKTNOSUPPORT= 94;	{ Socket type not supported }
  Sys_EOPNOTSUPP= 95;	{ Operation not supported on transport endpoint }
  Sys_EPFNOSUPPORT= 96;	{ Protocol family not supported }
  Sys_EAFNOSUPPORT= 97;	{ Address family not supported by protocol }
  Sys_EADDRINUSE= 98;	{ Address already in use }
  Sys_EADDRNOTAVAIL= 99;	{ Cannot assign requested address }
  Sys_ENETDOWN= 100;	{ Network is down }
  Sys_ENETUNREACH= 101;	{ Network is unreachable }
  Sys_ENETRESET= 102;	{ Network dropped connection because of reset }
  Sys_ECONNABORTED= 103;	{ Software caused connection abort }
  Sys_ECONNRESET= 104;	{ Connection reset by peer }
  Sys_ENOBUFS	= 105;	{ No buffer space available }
  Sys_EISCONN	= 106;	{ Transport endpoint is already connected }
  Sys_ENOTCONN= 107;	{ Transport endpoint is not connected }
  Sys_ESHUTDOWN= 108;	{ Cannot send after transport endpoint shutdown }
  Sys_ETOOMANYREFS= 109;	{ Too many references: cannot splice }
  Sys_ETIMEDOUT= 110;	{ Connection timed out }
  Sys_ECONNREFUSED= 111;	{ Connection refused }
  Sys_EHOSTDOWN= 112;	{ Host is down }
  Sys_EHOSTUNREACH= 113;	{ No route to host }
  Sys_EALREADY= 114;	{ Operation already in progress }
  Sys_EINPROGRESS= 115;	{ Operation now in progress }
  Sys_ESTALE	= 116;	{ Stale NFS file handle }
  Sys_EUCLEAN	= 117;	{ Structure needs cleaning }
  Sys_ENOTNAM	= 118;	{ Not a XENIX named type file }
  Sys_ENAVAIL	= 119;	{ No XENIX semaphores available }
  Sys_EISNAM	= 120;	{ Is a named type file }
  Sys_EREMOTEIO= 121;	{ Remote I/O error }
  Sys_EDQUOT	= 122;	{ Quota exceeded }

//Function  StrError(err:longint):string;

Implementation

end.

{
  $Log: doserrno.pas,v $
  Revision 1.1  2000/12/28 09:17:25  mk
  CL:- added DOS32 socket stubs

}

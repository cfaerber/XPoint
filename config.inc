//   config.h.  Generated automatically by configure.
//   $Id$
//
//   config.h.in -- OpenXP autoconf system
//
//   This software is part of the OpenXP project (www.openxp.de).
//   Copyright (c) 2000 by the OpenXP Team and Claus F"arber.
//
//   This is free software; you can redistribute it and/or modify it
//   under the terms of the Lesser GNU General Public License (LGPL) as
//   published by the Free Software Foundation; either version 2,
//   or (at your option) any later version.
//
//   The software is distributed in the hope that it will be useful, but
//   WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LGPL
//   for more details.
//
//   You should have received a copy of the LGPL along with this
//   software; see the file lgpl.txt. If not, write to the
//   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//

//   NCURSES
{$define HAVE_LIBNCURSES 1}

//   GNUTLS <http://www.gnu.org/software/gnutls/>
//   #undef HAVE_LIBGNUTLS

//   OpenSSL <http://www.openssl.org/>
{$define HAVE_LIBSSL 1}

//
//   $Log$
//   Revision 1.1  2002/01/13 22:03:42  cl
//   - these files can't be built without autoconf and a Unix-like environment
//
//   Revision 1.1  2002/01/13 21:52:16  cl
//   - added a start for autoconf configuration
//

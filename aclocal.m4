# $Id$
# This file extends Autoconf.                          -*- Autoconf -*-
# Programming language support for the PASCAL language
#
# Copyright 2003 Claus Faerber
# based on Autoconf Copyright 2001 Free Software Foundation
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# As a special exception, the copyright holders give unlimited
# permission to copy, distribute and modify the configure scripts that
# are the output of Autoconf.  You need not follow the terms of the GNU
# General Public License when using or distributing such scripts, even
# though portions of the text of Autoconf appear in them.  The GNU
# General Public License (GPL) does govern all other use of the material
# that constitutes the Autoconf program.
#
# Certain portions of the Autoconf source text are designed to be copied
# (in certain cases, depending on the input) into the output of
# Autoconf.  We call these the "data" portions.  The rest of the
# Autoconf source text consists of comments plus executable code that
# decides which of the data portions to output in any given case.  We
# call these comments and executable code the "non-data" portions.
# Autoconf never copies any of the non-data portions into its output.
#
# This special exception to the GPL applies to versions of Autoconf
# released by the current copyright holders.  When you make and
# distribute a modified version of Autoconf, you may extend this special
# exception to the GPL to apply to your modified version as well,
# *unless* your modified version has the potential to copy into its
# output some of the text that was the non-data portion of the version
# that you started with.  (In other words, unless your change moves or
# copies text from the non-data portions to the data portions.) If your
# modification has such potential, you must delete any notice of this
# special exception to the GPL from your modified version.
#
# Written by Claus Faerber for OpenXP <http://www.openxp.de>

AC_PREREQ(2.53)
AC_COPYRIGHT(Autoconf for PASCAL - (PASCAL)Copyright 2003 Claus Faerber)

## ----------------------------------------------------------------- ##
## 1. The PASCAL Language                                            ##
## ----------------------------------------------------------------- ##

m4_define([AC_LANG(PASCAL)],
[ac_ext=pp
ac_ppc='$PPC $PPCFLAGS'
ac_compile='$PPC $PPCFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_link='$PPC $PPCFLAGS $LDFLAGS $LIBS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_compiler_gnu=$ac_cv_ppc_compiler_gnu
])

AU_DEFUN([AC_LANG_PASCAL], [AC_LANG(PASCAL)])

m4_define([_AC_LANG_ABBREV(PASCAL)], [pas])

## ----------------------------------------------------------------- ##
## 2. Producing programs.  					     ##
## ----------------------------------------------------------------- ##

# AC_LANG_SOURCE(PASCAL)(BODY)
# -----------------------
m4_define([AC_LANG_SOURCE(PASCAL)],
[(*@S|@include confdefs.inc *)
$1])

# AC_LANG_PROGRAM(PASCAL)([PROLOGUE], [BODY])
# --------------------------------------
m4_define([AC_LANG_PROGRAM(PASCAL)],
[program conftest;
$1
begin
$2
halt(0);
end.])

# AC_LANG_CALL(PASCAL)(PROLOGUE, FUNCTION)
# -----------------------------------
# Avoid conflicting decl of main.
m4_define([AC_LANG_CALL(PASCAL)],
[AC_LANG_PROGRAM([$1, [$2;])])])


# AC_LANG_FUNC_LINK_TRY(PASCAL)(FUNCTION)
# ----------------------------------
m4_define([AC_LANG_FUNC_LINK_TRY(PASCAL)],
[AC_LANG_PROGRAM([var f: pointer;],[
f := @$1;
])])

# AC_LANG_BOOL_COMPILE_TRY(PASCAL)(PROLOGUE, EXPRESSION)
# -------------------------------------------------
# Be sure to use this array to avoid `unused' warnings, which are even
# errors with `-W error'.
m4_define([AC_LANG_BOOL_COMPILE_TRY(PASCAL)],
[AC_LANG_PROGRAM([$1
var test_array = array @<:@0..(1-2*ord(not($2)))@:>@;],[
test_array @<:@0@:>@ := 0
])])

# AC_LANG_INT_SAVE(PASCAL)(PROLOGUE, EXPRESSION)
# -----------------------------------------
# We need assign/close from Borland Pascal/Delphi, also supported by
# Free Pascal and GPC.
# It might be a good idea to alternatively use the standardised method
# (bind) based on previous Autoconf tests
m4_define([AC_LANG_INT_SAVE(PASCAL)],
[AC_LANG_PROGRAM([$1
  function integer_val: integer; 
  begin 
    integer_val := ($2); 
  end;

  var f: text;
],
[
  assign(f,'conftest.val');
  rewrite(f);
  writeln(f,($2));
  close(f);
])])

## ----------------------------------------------------------------- ##
## 3. Looking for Compilers and Preprocessors.  		     ##
## ----------------------------------------------------------------- ##

# AC_LANG_COMPILER(PASCAL)
# -------------------
# Find the PASCAL compiler.  Must be AC_DEFUN'd to be AC_REQUIRE'able.
AC_DEFUN([AC_LANG_COMPILER(PASCAL)],
[AC_REQUIRE([AC_PROG_PPC])])

# AC_PROG_PPC([COMPILER ...])
# --------------------------
# COMPILER ... is a space separated list of PASCAL compilers to search for.
# This just gives the user an opportunity to specify an alternative
# search list for the PASCAL compiler.
AC_DEFUN([AC_PROG_PPC],
[AC_LANG_PUSH(PASCAL)dnl
AC_ARG_VAR([PPC],      [PASCAL compiler command])dnl
AC_ARG_VAR([PPCFLAGS], [PASCAL compiler flags])dnl
dnl _AC_ARG_VAR_LDFLAGS()dnl
dnl _AC_ARG_VAR_CPPFLAGS()dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(PPC, [$1])],
[AC_CHECK_TOOLS(PPC, gpc ppc386 dcc)
])
test -z "$PPC" && AC_MSG_ERROR([no acceptable PASCAL compiler found in \$PATH])

# Provide some information about the compiler.
echo "$as_me:$LINENO:" \
     "checking for _AC_LANG compiler version" >&AS_MESSAGE_LOG_FD
ac_compiler=`set X $ac_compile; echo $[2]`
_AC_EVAL([$ac_compiler --version </dev/null >&AS_MESSAGE_LOG_FD])
_AC_EVAL([$ac_compiler -v </dev/null >&AS_MESSAGE_LOG_FD])
_AC_EVAL([$ac_compiler -V </dev/null >&AS_MESSAGE_LOG_FD])

m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
_AC_LANG_COMPILER_GNU
GPC=`test $ac_compiler_gnu = yes && echo yes`
AC_LANG_POP(PASCAL)dnl
])# AC_PROG_PPC

## ----------------------------------------------------------------- ##
## 4. Compilers' characteristics.  				     ##
## ----------------------------------------------------------------- ##

# $Id$

all: linebreak.inc

%.inc: %.pl %.txt
	perl $<

download:
	perl download.pl

#
# $Log$
# Revision 1.2  2003/02/15 21:47:40  cl
# - FPC/Linux compile fixes (FPC has problems with mixed-case filenames)
#
# Revision 1.1  2003/02/13 14:27:12  cl
# - Unicode support library:
#   . character width
#   . character line breaking properties/line breaking library
#   . UTF8 functions
#

#
# OpenXP ObjCOM Makefile
# <http://www.openxp.de/>
#
# $Id$
#
# Fuer GNU make <http://www.gnu.org/software/make/>
#
# DOS/32: <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/mak3791b.zip>
# OS/2: <ftp://ftp-os2.cdrom.com/pub/os2/unix/gnumake.zip>
# Windows 95/98/NT:
# <ftp://sources.redhat.com/pub/cygwin/latest/make/make-3.79.1-1.tar.gz>

# Das Betriebssystem, fuer das OpenXP uebersetzt werden soll
# (MUSS gesetzt werden.)
#
# dos32		DOS 32 Bit
# linux		Linux
# os2		OS/2
# win32		Windows
#
#OS = dos32

# Ihr Compiler (nur fpc)
# (MUSS gesetzt werden.)
#
#COMPILER = fpc

# Ihre CPU (386, 486, 586, 686)
# (KANN gesetzt werden.)
#
#CPU = 386

# Compiler-Flags
# (KANN gesetzt werden.)
#
#PFLAGS =

# Soll die Debug-Version erstellt werden?
# (KANN gesetzt werden.)
#
#DEBUG = yes

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Ab hier bitte keine Aenderungen mehr durchfuehren
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SHELL = /bin/sh

CPU ?= 386

ifneq (,$(findstring $(OS),dos32 os2 win32))

EXE_EXT = .exe
# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)

RM ?= rm

endif

ifeq ($(OS),linux)

EXE_EXT =
SEP = /

RM ?= rm

endif

ifeq ($(COMPILER),fpc)
PC = ppc386$(EXE_EXT)

ifeq ($(DEBUG),yes)
PFLAGS += -Ct -dDEBUG -Sg -pg
else
PFLAGS += -CX -O2 -Sg
endif

PF_dos32 = -TGO32V2
PF_os2 = -TOS2
PF_win = -TWIN32
PF_linux = -TLINUX
PF_386 = -Op1
PF_486 = -Op1
PF_586 = -Op2
PF_686 = -Op3

PFLAGS += -Fu..

endif

PFLAGS += $(PF_$(OS))
PFLAGS += $(PF_$(CPU))

all: objcom.o

objcom.o: objcom.pas ocfdos.inc ocfdosh.inc ocsdos.inc ocsdosh.inc \
	ocsos2.inc ocswin.inc ocswinh.inc octhread.o octwin.inc \
	octwinh.inc ringbuff.o
	$(PC) $(PFLAGS) $<

octhread.o: octhread.pas
	$(PC) $(PFLAGS) $<

ringbuff.o: ringbuff.pas
	$(PC) $(PFLAGS) $<

example$(EXE_EXT): example.pas objcom.o
	$(PC) $(PFLAGS) $<

clean:
	-$(RM) *.o
	-$(RM) *.a
	-$(RM) *.ppu
	-$(RM) example$(EXE_EXT)

#
# $Log$
# Revision 1.2  2000/09/27 21:22:49  fe
# kleinere Korrekturen
#
# Revision 1.1  2000/09/26 14:01:57  fe
# Makefile fuer GNU make
#
#

#
# OpenXP ObjCOM Makefile
# <http://www.openxp.de/>
#
# $Id: Makefile,v 1.14 2000/11/22 20:58:22 fe Exp $
#
# fuer GNU make <http://www.gnu.org/software/make/>
#
# DOS/32: <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/mak3791b.zip>

# Das Betriebssystem, fuer das OpenXP uebersetzt werden soll
# (MUSS gesetzt werden.)
#
# dos32		DOS 32 Bit
# freebsd       FreeBSD
# linux		Linux
# os2		OS/2
# win32		Windows
#
#OS = 

# Ihre CPU (386, 486, 586, 686)
# (KANN gesetzt werden.)
#
#CPU = 386

# Ihr Pascal-Compiler, mit dem der OpenXP-Quellcode uebersetzt werden
# soll.  (MUSS gesetzt werden.)
#
# fpc           Free Pascal <http://www.freepascal.org/>
# vpc           Virtual Pascal <http://www.vpascal.com/>
#
#COMPILER = 

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

ifneq (,$(findstring $(OS),freebsd linux))
SHELL = /bin/sh
endif

# Ueberpruefen, ob die Variablen richtig gesetzt sind

ifeq (,$(findstring $(OS),dos32 freebsd linux os2 win32))
$(error Variable "OS" muss auf "dos32", "freebsd", "linux", "os2" oder "win32" \
gesetzt werden)
endif

CPU ?= 386

ifeq (,$(findstring $(COMPILER),fpc vpc))
$(error Variable "COMPILER" muss auf "fpc" oder "vpc" gesetzt werden)
endif

ifneq (,$(findstring $(OS),dos32 os2 win32))

EXEEXT = .exe
# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)

MAKE ?= make
RM ?= rm

endif

ifeq ($(OS),freebsd)

EXEEXT =
SEP = /

MAKE ?= gmake
RM ?= rm

endif

ifeq ($(OS),linux)

EXEEXT =
SEP = /

MAKE ?= make
RM ?= rm

endif

ifeq ($(COMPILER),fpc)
PC = ppc386

ifeq ($(DEBUG),yes)
PFLAGS += -Ct -Cr- -dDEBUG -Sg -pg -Fu..
else
PFLAGS += -Cr- -O2 -Sg -Fu..
endif

ifeq ($(OS),dos32)

UNITEXT = .ppu
OBJEXT = .o
LNKEXT = .a
LIBPREF = 
LIBEXT = .a

endif

ifeq ($(OS),freebsd)

UNITEXT = .ppu
OBJEXT = .o
LNKEXT = .a
LIBPREF = lib
LIBEXT = .a

endif

ifeq ($(OS),linux)

UNITEXT = .ppu
OBJEXT = .o
LNKEXT = .a
LIBPREF = lib
LIBEXT = .a

endif

ifeq ($(OS),os2)

UNITEXT = .ppu
OBJEXT = .o
LNKEXT = .a
LIBPREF = lib
LIBEXT = .a

endif

ifeq ($(OS),win32)

UNITEXT = .ppw
OBJEXT = .ow
LNKEXT = .aw
LIBPREF = lib
LIBEXT = .aw

endif

PF_dos32 = -TGO32V2
PF_freebsd = -TFREEBSD
PF_linux = -TLINUX -dUnixDevelop
PF_os2 = -TOS2
PF_win = -TWIN32
PF_386 = -Op1
PF_486 = -Op1
PF_586 = -Op2
PF_686 = -Op3

endif

ifeq ($(COMPILER),vpc)
PC = vpc

vpprefix ?= \vp21

ifeq ($(DEBUG),yes)
PFLAGS += -DDEBUG -$D+ -$Q- -$R- -$S- -$V+ -M -U"$(vpprefix)$(SEP)units.%p;.." -L"$(vpprefix)$(SEP)lib.%p;$(vpprefix)$(SEP)units.%p" -R"$(vpprefix)$(SEP)res.%p"
else
PFLAGS += -$$SmartLink+ -M -U"$(vpprefix)$(SEP)units.%p;.." -L"$(vpprefix)$(SEP)lib.%p;$(vpprefix)$(SEP)units.%p" -R"$(vpprefix)$(SEP)res.%p"
endif

UNITEXT = .vpi
OBJEXT = .obj
LNKEXT = .lnk
LIBPREF =
LIBEXT = .lib

PF_dos32 = 
PF_freebsd = 
PF_linux = 
PF_os2 = -CO
PF_win32 = -CW
PF_386 = -$$G3+
PF_486 = -$$G4-
PF_586 = -$$G5-
PF_686 = -$$G5-

endif

export PC
export OS
export CPU
export COMPILER
export DEBUG
export MAKE
export RM

BIN = example
UNITS = objcom octhread ringbuff

PFLAGS += $(PF_$(OS))
PFLAGS += $(PF_$(CPU))

all: objcom$(UNITEXT)

ifeq ($(OS),dos32)

objcom$(UNITEXT): objcom.pas ocfdos.inc ocfdosh.inc ocsdos.inc \
	ocsdosh.inc ringbuff$(UNITEXT) root-debug root-timer
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),freebsd)

objcom$(UNITEXT): objcom.pas ocslin.inc ocslinh.inc ringbuff$(UNITEXT) \
	root-debug root-timer
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),linux)

objcom$(UNITEXT): objcom.pas ocslin.inc ocslinh.inc ringbuff$(UNITEXT) \
	root-debug root-timer
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),os2)

objcom$(UNITEXT): objcom.pas ocsos2.inc octhread$(UNITEXT) \
	ringbuff$(UNITEXT) root-debug root-timer
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),win32)

objcom$(UNITEXT): objcom.pas ocswin.inc ocswinh.inc octhread$(UNITEXT) \
	octwin.inc octwinh.inc ringbuff$(UNITEXT) root-debug root-timer
	$(PC) $(PFLAGS) $<

endif

octhread$(UNITEXT): octhread.pas
	$(PC) $(PFLAGS) $<

ringbuff$(UNITEXT): ringbuff.pas
	$(PC) $(PFLAGS) $<

example$(EXEEXT): example.pas objcom$(UNITEXT)
	$(PC) $(PFLAGS) $<

# root-Units

root-debug:
	$(MAKE) -C .. debug$(UNITEXT)

root-timer:
	$(MAKE) -C .. timer$(UNITEXT)

clean: $(patsubst %,clean_%,$(BIN)) $(patsubst %,clean_%,$(UNITS))

$(patsubst %,clean_%,$(BIN)): clean_%:
	-$(RM) $*$(EXEEXT)
	-$(RM) $*$(OBJEXT)
	-$(RM) $*$(LNKEXT)

$(patsubst %,clean_%,$(UNITS)): clean_%:
	-$(RM) $*$(UNITEXT)
	-$(RM) $*$(OBJEXT)
	-$(RM) $(LIBPREF)$*$(LIBEXT)

distclean: clean

mostlyclean: clean

maintainer-clean: clean

#
# $Log: Makefile,v $
# Revision 1.14  2000/11/22 20:58:22  fe
# Fixed.
#
# Revision 1.13  2000/11/14 18:45:20  fe
# Dependecies fixed.
#
# Revision 1.12  2000/10/23 17:16:53  fe
# FreeBSD-Anpassungen
#
# Revision 1.11  2000/10/20 16:12:25  fe
# Fehler in den Abhaengigkeiten korrigiert.
#
# Revision 1.10  2000/10/19 18:16:09  fe
# Fuer FPC SmartLinking und RangeChecking ausgestellt.
# Anpassungen der FPC-Linux-Aufrufparameter.
# SHELL-Variable wird nur noch bei Linux gesetzt.  (wegen Cygwin make)
#
# Revision 1.9  2000/10/16 11:54:20  fe
# serial.pas ergaenzt.
#
# Revision 1.8  2000/10/12 20:44:46  fe
# Pfade geaendert, v.a. fuer Linux (/opt/openxp/).
#
# Revision 1.7  2000/10/11 21:50:46  fe
# Korrekturen fuer Virtual Pascal.
#
# Revision 1.6  2000/10/08 18:06:06  fe
# Targets distclean, mostlyclean und maintainer-clean ergaenzt.
#
# Revision 1.5  2000/10/05 19:56:45  fe
# Korrekturen, v.a. fuer FPC/Win32.
#
# Revision 1.4  2000/09/30 16:54:17  fe
# Abhaengigkeiten angepasst.
#
# Revision 1.3  2000/09/28 14:22:08  fe
# Ueberpruefung der Variablen eingebaut.
# kleinere Korrekturen
#
# Revision 1.2  2000/09/27 21:22:49  fe
# kleinere Korrekturen
#
# Revision 1.1  2000/09/26 14:01:57  fe
# Makefile fuer GNU make
#

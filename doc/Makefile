#
# OpenXP Makefile
# <http://www.openxp.de/>
#
# $Id$
#
# fuer GNU make <http://www.gnu.org/software/make/>
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

# Ihre CPU (386, 486, 586, 686)
# (KANN gesetzt werden.)
#
#CPU = 386

# Ihr Compiler (nur fpc)
# (MUSS gesetzt werden.)
#
#COMPILER = fpc

# Verzeichnis, in dem OpenXP installiert werden soll
# (KANN gesetzt werden.)
#
#prefix = \OXP370

# Compiler-Flags
# (KANN gesetzt werden.)
#
#PFLAGS =

# Soll die Debug-Version erstellt werden?
# (KANN gesetzt werden.)
#
#DEBUG = yes

# Verzeichnis, in dem notwendige Dateien liegen, die nicht Bestandteil
# des Quellcode sind
# (MUSS gesetzt werden.)
#
#contribdir = \CONTRIB

# Flags fuer docform
# (KANN gesetzt werden.)
#
#DOCFORMFLAGS = 72 1 N

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Ab hier bitte keine Aenderungen mehr durchfuehren
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SHELL = /bin/sh

# Ueberpruefen, ob die Variablen richtig gesetzt sind

ifeq (,$(findstring $(OS),dos32 linux os2 win32))
$(error Variable "OS" muss auf "dos32", "linux", "os2" oder "win32" \
gesetzt werden)
endif

CPU ?= 386

ifneq ($(COMPILER),fpc)
$(error Variable "COMPILER" muss auf "fpc" gesetzt werden)
endif

ifeq (,$(contribdir))
$(error Variable "contribdir" muss gesetzt werden)
endif

DOCFORMFLAGS ?= 72 1 N

ifneq (,$(findstring $(OS),dos32 os2 win32))

prefix ?= \OPENXP
datadir = $(prefix)
docdir = $(prefix)\DOC

# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)
MAKE ?= make.exe
RM ?= rm
RMDIR ?= rmdir
INSTALLDIR ?= mkdir
INSTALL_DATA ?= xcopy
DOCFORM ?= ..\docform.exe
IHS ?= ..\ihs.exe

endif

ifeq ($(OS),linux)

prefix ?= /usr/local/xp
datadir = $(prefix)
docdir = $(prefix)/doc

SEP = /
MAKE ?= make
RM ?= rm
RMDIR ?= rmdir
INSTALLDIR ?= install -m 755 -d
INSTALL_DATA ?= install -b -V existing -p -m 644
DOCFORM ?= ../docform
IHS ?= ../ihs

endif

HELP = xp xp-e
OLDDOC = fido uucp
TXT = authors.txt dbform devtodo.txt files.txt gpl.txt make.txt \
	planung.txt readme.txt slizenz.txt tools.txt versions.txt
CONTRIBDOC = bestell.doc lizenz.doc xpoint.log xpoint.txt

ifeq ($(OS),linux)
TXT += readme.lnx
endif

HELPFILES = $(patsubst %,%.hlp,$(HELP))
OLDDOCFILES = $(patsubst %,%.txt,$(OLDDOC))
TXT += $(OLDDOCFILES)

all: $(HELPFILES) $(OLDDOCFILES)

$(HELPFILES): %.hlp: %.ihq
	$(IHS) $*

$(OLDDOCFILES): %.txt: %.dq
	$(DOCFORM) $< $@ $(DOCFORMFLAGS)

install: install_datadir $(patsubst %,install_%,$(HELPFILES)) \
	install_docdir $(patsubst %,install_%,$(TXT)) \
	$(patsubst %,install_%,$(CONTRIBDOC))

install_datadir:
	-$(INSTALLDIR) $(datadir)

$(patsubst %,install_%,$(HELPFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)

install_docdir:
	-$(INSTALLDIR) $(docdir)

$(patsubst %,install_%,$(TXT)): install_%: %
	$(INSTALL_DATA) $* $(docdir)

$(patsubst %,install_%,$(CONTRIBDOC)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)doc$(SEP)$* $(docdir)

uninstall: $(patsubst %,uninstall_%,$(TXT)) \
	$(patsubst %,install_%,$(HELPFILES))
	-$(RMDIR) $(docdir)
	-$(RMDIR) $(datadir)

$(patsubst %,uninstall_%,$(TXT)): uninstall_%: %
	-$(RM) $(docdir)$(SEP)$*

$(patsubst %,uninstall_%,$(HELPFILES)): uninstall_%: %
	-$(RM) $(datadir)$(SEP)$*

clean:  $(patsubst %,clean_%,$(OLDDOCFILES))
	-$(RM) *.hlp

$(patsubst %,clean_%,$(OLDDOCFILES)): clean_%:
	-$(RM) $*

#
# $Log$
# Revision 1.3  2000/09/28 14:21:29  fe
# Ueberpruefung der Variablen eingebaut.
# kleinere Korrekturen
#
# Revision 1.2  2000/09/27 21:22:25  fe
# Zmodem-Anleitung (zm.txt) wird nicht mehr installiert, da zm.exe und
# zconfig.exe nicht mehr installiert werden.
#
# Revision 1.1  2000/09/26 14:00:37  fe
# Makefile fuer GNU make
#

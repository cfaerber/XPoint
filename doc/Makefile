#
# OpenXP doc Makefile
# <http://www.openxp.de/>
#
# $Id$
#
# fuer GNU make <http://www.gnu.org/software/make/>
#
# DOS/32: <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/mak3791b.zip>

# Das Betriebssystem, fuer das OpenXP uebersetzt werden soll
# (MUSS gesetzt werden.)
#
# dos32		DOS 32 Bit
# linux		Linux
# os2		OS/2
# win32		Windows
#
#OS = 

# Ihre CPU (386, 486, 586, 686)
# (KANN gesetzt werden.)
#
#CPU = 386

# Verzeichnis, in dem OpenXP installiert werden soll
# (KANN gesetzt werden.)
#
#prefix = C:\Programme\OpenXP

# Soll das Handbuch aus DocBook gebaut werden?
# (KANN gesetzt werden.)
#
#MAKEHB = 

# Verzeichnis, in dem die DTD von DocBook 4.1 (SGML) liegt.
# <http://www.oasis-open.org/docbook/sgml/4.1/docbk41.zip>
# (MUSS gesetzt werden, falls MAKEHB auf yes gesetzt ist.)
#
#dbdir = 

# Verzeichnis, in dem die ISO 8879 Entities liegen.
# <http://fallout.camputview.indiana.edu/ports/distfiles/isoENTS.zip>
# (MUSS gesetzt werden, falls MAKEHB auf yes gesetzt ist.)
#
#entdir = 

# Verzeichnis, in dem die DSSSL-Dateien von OpenJade liegen.
# <http://www.netfolder.com/DSSSL/>
# (MUSS gesetzt werden, falls MAKEHB auf yes gesetzt ist.)
#
#jadedir = C:\Programme\OpenJade-1.3\dsssl

# Verzeichnis, in dem die Modular DSSSL-Stylesheets liegen.
# <http://nwalsh.com/docbook/dsssl/db157.zip>
# (MUSS gesetzt werden, falls MAKEHB auf yes gesetzt ist.)
#
#moddir = 

# Verzeichnis, in dem notwendige Dateien liegen, die nicht Bestandteil
# des Quellcode sind.
# <ftp://ftp.openxp.de/openxp/snapshot/oxpcontr.rar>
# (MUSS gesetzt werden, falls "make install" aufgerufen wird.)
#
#contribdir = 

# Sollen unter Linux bei "make install" Softlinks nach /usr/bin und
# /usr/lib gesetzt werden?
# (KANN gesetzt werden.  Voreingestellt auf yes.)
#
#SETLINKS = yes

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Ab hier bitte keine Aenderungen mehr durchfuehren
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ifeq ($(OS),linux)
SHELL = /bin/sh
endif

# Ueberpruefen, ob die Variablen richtig gesetzt sind

ifeq (,$(findstring $(OS),dos32 linux os2 win32))
$(error Variable "OS" muss auf "dos32", "linux", "os2" oder "win32" \
gesetzt werden)
endif

CPU ?= 386
SETLINKS ?= yes

ifeq ($(MAKEHB),yes)

ifeq (,$(dbdir))
$(error Variable "dbdir" muss gesetzt werden)
endif

ifeq (,$(entdir))
$(error Variable "entdir" muss gesetzt werden)
endif

ifeq (,$(jadedir))
$(error Variable "jadedir" muss gesetzt werden)
endif

ifeq (,$(moddir))
$(error Variable "moddir" muss gesetzt werden)
endif

endif

DOCFORMFLAGS ?= 72 1 N
TIDYFLAGS ?= -q -f tidyerr.tmp -m -asxml --doctype strict --show-warnings no --add-xml-decl yes

ifneq (,$(findstring $(OS),dos32 os2 win32))

ifeq ($(OS),win32)
prefix ?= C:\Programme\OpenXP
else
prefix ?= \OPENXP
endif

datadir ?= $(prefix)
docdir ?= $(prefix)$(SEP)DOC

# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)
MAKE ?= make.exe
RM ?= rm
RMDIR ?= rmdir
SED ?= sed
RECODE ?= recode
JADE ?= openjade
TIDY ?= tidy
H2T ?= w3m
TEX ?= tex
PDFTEX ?= pdftex
DVIPS ?= dvips
INSTALLDIR ?= mkdir
INSTALL_DATA ?= xcopy
DOCFORM ?= ..\docform.exe
IHS ?= ..\ihs.exe

endif

ifeq ($(OS),linux)

prefix ?= /opt/openxp
datadir ?= $(prefix)/lib
datalinkdir ?= /usr/lib
docdir ?= $(prefix)/doc

SEP = /
MAKE ?= make
RM ?= rm
RMDIR ?= rmdir
SED ?= sed
RECODE ?= recode
JADE ?= openjade
TIDY ?= tidy
H2T ?= w3m
TEX ?= tex
PDFTEX ?= pdftex
DVIPS ?= dvips
INSTALLDIR ?= install -m 755 -d
INSTALL_DATA ?= install -b -V existing -p -m 644
DOCFORM ?= ../docform
IHS ?= ../ihs

endif

ifeq ($(H2T),links)
H2TFLAGS ?= -dump
endif
ifeq ($(H2T),lynx)
H2TFLAGS ?= -dump -width=72
endif
ifeq ($(H2T),w3m)
H2TFLAGS ?= -dump -cols 72
endif

HELP = xp xp-e
OLDDOC = fido uucp

# make.txt wird nicht installiert, da es nur beim Quellcode Sinn macht.

TXT = authors.txt dbform.txt devtodo.txt files.txt gpl.txt planung.txt \
	readme.txt slizenz.txt tools.txt update.txt versions.txt
CONTRIBDOC = bestell.txt lizenz.doc xpoint.log
HBFILES = xpoint.htm xpoint.rtf xpoint.tex xpoint.dvi xpoint.ps \
	xpoint.pdf catalog xpoint.tmp xpoint.tm1 xpoint.tm2 tidyerr.tmp

ifeq ($(OS),linux)
TXT += linux.txt
endif

HELPFILES = $(patsubst %,%.hlp,$(HELP))
OLDDOCFILES = $(patsubst %,%.txt,$(OLDDOC))
TXT += $(OLDDOCFILES) xpoint.txt

# "/" und "\" fuer sed quoten und $(SEP) bzw. $(SEP)docbook$(SEP) anhaengen

dbdirq = $(subst @QUOTE@,\,$(subst \,@QUOTE@\,$(subst /,@QUOTE@/,$(dbdir)$(SEP))))
entdirq = $(subst @QUOTE@,\,$(subst \,@QUOTE@\,$(subst /,@QUOTE@/,$(entdir)$(SEP))))
jadedirq = $(subst @QUOTE@,\,$(subst \,@QUOTE@\,$(subst /,@QUOTE@/,$(jadedir)$(SEP))))
moddirq = $(subst @QUOTE@,\,$(subst \,@QUOTE@\,$(subst /,@QUOTE@/,$(moddir)$(SEP)docbook$(SEP))))

ifeq ($(MAKEHB),yes)
all: xpoint.txt $(HELPFILES) $(OLDDOCFILES)
else
all: $(HELPFILES) $(OLDDOCFILES)
endif

fulldoc: xpoint.txt xpoint.htm xpoint.rtf xpoint.tex xpoint.dvi \
	xpoint.ps xpoint.pdf $(HELPFILES) $(OLDDOCFILES)

$(HELPFILES): %.hlp: %.ihq
	$(IHS) $*

xpoint.txt: xpoint.htm
	$(H2T) $(H2TFLAGS) xpoint.htm > xpoint.tmp
	-$(RECODE) -f latin1..ibmpc < xpoint.tmp > xpoint.txt

xpoint.htm: xpoint.sgm xpoint.dsl catalog
	$(JADE) -c catalog -d xpoint.dsl#html -t sgml xpoint.sgm > xpoint.tmp
	$(SED) 's/&#13;//g' xpoint.tmp > xpoint.htm
	-$(TIDY) $(TIDYFLAGS) xpoint.htm

catalog: xpoint.cat
	$(SED) 's/@DBDIR@/$(dbdirq)/' xpoint.cat > xpoint.tmp
	$(SED) 's/@ENTDIR@/$(entdirq)/' xpoint.tmp > xpoint.tm1
	$(SED) 's/@JADEDIR@/$(jadedirq)/' xpoint.tm1 > xpoint.tm2
	$(SED) 's/@MODDIR@/$(moddirq)/' xpoint.tm2 > catalog

xpoint.rtf: xpoint.sgm xpoint.dsl catalog
	$(JADE) -c catalog -d xpoint.dsl#print -t rtf xpoint.sgm > xpoint.rtf

xpoint.tex: xpoint.sgm xpoint.dsl catalog
	$(JADE) -c catalog -d xpoint.dsl#print -t tex xpoint.sgm > xpoint.tex

xpoint.dvi: xpoint.tex
	$(TEX) "&jadetex" xpoint.tex
	$(TEX) "&jadetex" xpoint.tex

xpoint.ps: xpoint.dvi
	$(DVIPS) -o xpoint.ps xpoint.dvi

xpoint.pdf: xpoint.tex
	$(PDFTEX) "&pdfjadetex" xpoint.tex
	$(PDFTEX) "&pdfjadetex" xpoint.tex

$(OLDDOCFILES): %.txt: %.dq
	$(DOCFORM) $< $@ $(DOCFORMFLAGS)

ifeq (,$(contribdir))
install:
	$(error Variable "contribdir" muss gesetzt werden)
else
install: install_datadir $(patsubst %,install_%,$(HELPFILES)) \
	install_docdir $(patsubst %,install_%,$(TXT)) \
	$(patsubst %,install_%,$(CONTRIBDOC))
endif

installdirs: install_datadir install_docdir

install_datadir:
	-$(INSTALLDIR) $(datadir)

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,install_%,$(HELPFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
	$(LN) $(datadir)$(SEP)$* $(datalinkdir)$(SEP)$*
else
$(patsubst %,install_%,$(HELPFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif
else
$(patsubst %,install_%,$(HELPFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif

install_docdir:
	-$(INSTALLDIR) $(docdir)

ifeq ($(OS),linux)
$(patsubst %,install_%,$(TXT)): install_%: %
	-$(RECODE) -f ibmpc..latin1 $* > tmp
	$(INSTALL_DATA) tmp $(docdir)$(SEP)$*
else
$(patsubst %,install_%,$(TXT)): install_%: %
	$(INSTALL_DATA) $* $(docdir)
endif

$(patsubst %,install_%,$(CONTRIBDOC)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)doc$(SEP)$* $(docdir)

uninstall: $(patsubst %,uninstall_%,$(TXT)) \
	$(patsubst %,install_%,$(HELPFILES))
	-$(RMDIR) $(docdir)
	-$(RMDIR) $(datadir)

$(patsubst %,uninstall_%,$(TXT)): uninstall_%: %
	-$(RM) $(docdir)$(SEP)$*

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,uninstall_%,$(HELPFILES)): uninstall_%: %
	-$(RM) $(datalinkdir)$(SEP)$*
	-$(RM) $(datadir)$(SEP)$*
else
$(patsubst %,uninstall_%,$(HELPFILES)): uninstall_%: %
	-$(RM) $(datadir)$(SEP)$*
endif
else
$(patsubst %,uninstall_%,$(HELPFILES)): uninstall_%: %
	-$(RM) $(datadir)$(SEP)$*
endif

clean:  $(patsubst %,clean_%,$(HBFILES)) \
	$(patsubst %,clean_%,$(OLDDOCFILES)) \
	$(patsubst %,clean_%,$(HELPFILES))
	-$(RM) tmp

$(patsubst %,clean_%,$(HBFILES)): clean_%:
	-$(RM) $*

$(patsubst %,clean_%,$(HELPFILES)): clean_%:
	-$(RM) $*

$(patsubst %,clean_%,$(OLDDOCFILES)): clean_%:
	-$(RM) $*

distclean: clean

mostlyclean: clean

maintainer-clean: clean
	-$(RM) xpoint.txt

dvi: xpoint.dvi

#
# $Log$
# Revision 1.13  2000/10/19 19:37:57  fe
# Parameter fuer links und lynx eingetragen.
#
# Revision 1.12  2000/10/19 18:17:12  fe
# SHELL-Variable wird nur noch bei Linux gesetzt.  (wegen Cygwin make)
# Variablen W3M und W3MPARMS in H2T und H2TPARMS umbenannt.
#
# Revision 1.11  2000/10/17 15:00:26  fe
# Zu installierende Textdateien aktualisiert.
#
# Revision 1.10  2000/10/12 20:44:31  fe
# Pfade geaendert, v.a. fuer Linux (/opt/openxp/).
#
# Revision 1.9  2000/10/10 20:01:46  fe
# Restliche GNU-Targets ergaenzt.
#
# Revision 1.8  2000/10/10 17:46:37  fe
# Handbuch-Formate TeX, RTF, DVI, PS und PDF ergaenzt.
#
# Revision 1.7  2000/10/09 22:10:02  fe
# Ergaenzungen fuer Handbuch, v.a. ISO 8879 Entities.
#
# Revision 1.6  2000/10/08 18:14:16  fe
# Uebersetzung des Handbuchs ergaenzt.
#
# Revision 1.5  2000/10/06 13:58:10  fe
# bestell.doc in bestell.txt umbenannt.
#
# Revision 1.4  2000/10/05 19:57:20  fe
# Korrekturen
#
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

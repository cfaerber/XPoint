#
# OpenXP Makefile
# <http://www.openxp.de/>
#
# $Id$
#
# fuer GNU make <http://www.gnu.org/software/make/>
#
# DOS/32: <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/mak3791b.zip>

# Fuer dieses Makefile muessen ein aktuelles GNU make (s.o.), der Free
# Pascal Compiler <http://www.freepascal.org/> und die GNU fileutils
# <http://www.gnu.org/software/fileutils/> (DOS/32:
# <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/fil316b.zip>)
# installiert sein.
#
# Zum Installieren von OpenXP wird ausserdem ein Archiv mit
# verschiedenen Dateien, die nicht im OpenXP-Quellcode enthalten sind,
# benoetigt.  (Z.Z. noch nicht verfuegbar.)
#
# Fuer das Erstellen eines Quellcodearchiv wird die
# Archivierungssoftware rar <http://www.rarsoft.com/> gebraucht.
#
# Zum Uebersetzen des Handbuchs werden OpenJade, sed, w3m, tidy,
# die DocBook DTD und die modular Stylesheets benoetigt.

# Unten muessen einige Variablen gesetzt werden.

# make                   uebersetzt das ganze Programm
# make install           installiert das Programm
# make uninstall         deinstalliert das Programm
# make clean             raeumt das Verzeichnis auf
# make distclean         raeumt das Verzeichnis auf
# make mostlyclean       raeumt das Verzeichnis auf
# make maintainer-clean  raeumt das Verzeichnis auf inkl. Plaintext-Handbuch
# make dist              erstellt Quellcodearchiv


# Das Betriebssystem, fuer das OpenXP uebersetzt werden soll.
# (MUSS gesetzt werden.)
#
# dos32         DOS 32 Bit
# linux         Linux
# os2           OS/2
# win32         Windows 95/98/NT
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

# Verzeichnis, in dem OpenXP installiert werden soll
# (KANN gesetzt werden.)
#
#prefix = \OPENXP

# Compiler-Flags
# (KANN gesetzt werden.)
#
#PFLAGS =

# Soll die Debug-Version erstellt werden?
# (KANN gesetzt werden.)
#
#DEBUG = no

# Soll das Handbuch aus DocBook gebaut werden?
# (KANN gesetzt werden.)
#
#MAKEHB = 

# Verzeichnis, in dem die DTD von DocBook 4.1 (SGML) liegt.
# <http://www.oasis-open.org/docbook/sgml/4.1/docbk41.zip>
# (MUSS gesetzt werden, falls MAKEHB gesetzt ist.)
#
#DBDIR = 

# Verzeichnis, in dem die DSSSL-Dateien von OpenJade liegen.
# <http://www.netfolder.com/DSSSL/>
# (MUSS gesetzt werden, falls MAKEHB gesetzt ist.)
#
#JADEDIR = C:\Programme\OpenJade-1.3\dsssl

# Verzeichnis, in dem die Modular DSSSL-Stylesheets liegen.
# <http://nwalsh.com/docbook/dsssl/>
# (MUSS gesetzt werden, falls MAKEHB gesetzt ist.)
#
#MODDIR = 

# Verzeichnis, in dem notwendige Dateien liegen, die nicht Bestandteil
# des Quellcode sind
# (MUSS gesetzt werden.)
#
#contribdir = 

# Name des Quellcode-Archivs
# (MUSS gesetzt werden.)
#
#DISTFILE = oxp370_s.rar

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

ifeq (,$(findstring $(COMPILER),fpc vpc))
$(error Variable "COMPILER" muss auf "fpc" oder "vpc" gesetzt werden)
endif

ifeq (,$(contribdir))
$(error Variable "contribdir" muss gesetzt werden)
endif

ifeq (,$(DISTFILE))
$(error Variable "DISTFILE" muss gesetzt werden)
endif

ifneq (,$(findstring $(OS),dos32 os2 win32))

prefix ?= \OPENXP
bindir = $(prefix)
datadir = $(prefix)
exampledir = $(prefix)\BEISPIEL

EXEEXT = .exe
# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)
RM ?= rm
RMDIR ?= rmdir
INSTALLDIR ?= mkdir
INSTALL_PROGRAM ?= xcopy
INSTALL_DATA ?= xcopy
RC = rc
RAR ?= rar

CONTRIBBIN = compress.exe freeze.exe gzip.exe tar.exe uucico.exe
CONTRIBDATA = fido.pc

endif

ifeq ($(OS),linux)

prefix ?= /usr/local/xp
bindir = $(prefix)
linkdir ?= /usr/bin
datadir = $(prefix)
exampledir = $(prefix)/beispiel

EXEEXT =
SEP = /
RM ?= rm
RMDIR ?= rmdir
INSTALLDIR ?= install -m 755 -d
INSTALL_PROGRAM ?= install -b -V t -s -p -m 755
INSTALL_DATA ?= install -b -V existing -p -m 644
LN ?= ln -s
RC = ./rc
RAR = rar

CONTRIBBIN =
CONTRIBDATA = fido.pc

endif

ifeq ($(COMPILER),fpc)
PC = ppc386

ifeq ($(DEBUG),yes)
PFLAGS += -Ct -dDEBUG -Sg -pg -FuObjCOM
else
PFLAGS += -CX -O2 -Sg -FuObjCOM
endif

ifeq ($(OS),dos32)

UNITEXT = .ppu
OBJEXT = .o
LIBPREF = 
LIBEXT = .a

endif

ifeq ($(OS),linux)

UNITEXT = .ppu
OBJEXT = .o
LIBPREF = lib
LIBEXT = .a

endif

ifeq ($(OS),os2)

UNITEXT = .ppu
OBJEXT = .o
LIBPREF = lib
LIBEXT = .a

endif

ifeq ($(OS),win32)

UNITEXT = .ppw
OBJEXT = .ow
LIBPREF = lib
LIBEXT = .aw

endif

PF_dos32 = -TGO32V2
PF_os2 = -TOS2
PF_win32 = -TWIN32
PF_linux = -TLINUX
PF_386 = -Op1
PF_486 = -Op1
PF_586 = -Op2
PF_686 = -Op3

uuzext$(EXEEXT): PFLAGS += -S2

endif

ifeq ($(COMPILER),vpc)
PC = vpc

ifeq ($(DEBUG),yes)
PFLAGS += -DDEBUG -UObjCOM
else
PFLAGS += -UObjCOM
endif

UNITEXT = .ppu
OBJEXT = .o
LIBPREF = lib
LIBEXT = .a

PF_dos32 = 
PF_os2 = -CO
PF_win32 = -CW
PF_linux = 
PF_386 = 
PF_486 = 
PF_586 = 
PF_686 = 

endif

export PC
export OS
export CPU
export prefix
export COMPILER
export DEBUG
export MAKEHB
export DBDIR
export JADEDIR
export MODDIR
export contribdir
export RM

# uucico uebersetzt nicht.

#BIN = maggi ndiff pmconv uucp-fl1 uucico uuzext xp xp-fm xpme yup2pkt \
#	zfido zpr
BIN = maggi ndiff pmconv uucp-fl1 uuzext xp xp-fm xpme yup2pkt zfido zpr
COMPBIN = $(BIN) docform ihs rc
UNITS = archive clip crc database databaso datadef datadef1 dbase \
	debug dosx eddef editor encoder exxec feiertag fileio gpltools \
	help inout ipaddr ipcclass keys lister maske maus2 modem \
	montage mouse ncnntp ncpop3 ncsmtp ncsocket ncurses netcall \
	printerx regexpr resource stack timer typeform uart uuz win2 \
	winxp xp0 xp1 xp10 xp1help xp1input xp1o xp1o2 xp2 xp2c xp2db \
	xp2f xp3 xp3ex xp3o xp3o2 xp4 xp4e xp4o xp4o2 xp4o3 xp5 xp6 \
	xp6l xp6o xp7 xp7f xp7l xp7o xp8 xp9 xp9bp xp_des xp_iti \
	xp_pgp xp_uue xpauto xpcc xpcfg xpcurses xpdatum xpdiff \
	xpdos32 xpe xpeasy xpf2 xpfido xpfidonl xpftnadr xpglobal \
	xpimpexp xpipc xpkeys xplinux xpmaus xpmime xpnntp xpnodes \
	xpnt xpos2 xpreg xpstat xpterm xpuu xpview xpwin32 xpx zmodem
RES = xp-d xp-e xpfm-d xpfm-e xpuu-d xpuu-e
EXAMPLES = gsbox.scr madness.scr magic.scr maus.scr o-magic.scr \
	oz-netz.scr pcsysop.scr privhead.xps qbrett.xps qpmpriv.xps \
	qpriv.xps quark.scr quoteto.xps uucp.scr z-netz.scr

ifeq ($(OS),win32)
RST = ipaddr ncnntp ncpop3 ncsmtp
else
RST =
endif

BINFILES = $(patsubst %,%$(EXEEXT),$(BIN))
COMPBINFILES = $(patsubst %,%$(EXEEXT),$(COMPBIN))
INSTALLBINFILES = $(BINFILES) $(CONTRIBBIN)
RESFILES = $(patsubst %,%.res,$(RES))
RSTFILES = $(patsubst %,%.rst,$(RST))
DATAFILES = $(RESFILES) $(RSTFILES) icons.res $(CONTRIBDATA)
CONTRIB = $(CONTRIBBIN) $(CONTRIBDATA)

PFLAGS += $(PF_$(OS))
PFLAGS += $(PF_$(CPU))

RARFLAGS = a -m5 -zfile_id.diz

all: $(COMPBINFILES) $(RESFILES) documentation

# Programme

docform$(EXEEXT): docform.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

ihs$(EXEEXT): ihs.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

ihs$(EXEEXT): ihs.pas fileio$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

maggi$(EXEEXT): maggi.pas fileio$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) xp_iti$(UNITEXT) xpdatum$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpheader.inc xpmakehd.inc
	$(PC) $(PFLAGS) $<

ndiff$(EXEEXT): ndiff.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

pmconv$(EXEEXT): pmconv.pas typeform$(UNITEXT) xpdatum$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpheader.inc xpmakehd.inc \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

rc$(EXEEXT): rc.pas fileio$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

uucico$(EXEEXT): uucico.pas fileio$(UNITEXT) inout$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) uart$(UNITEXT) uucp-g.inc \
	xpfiles.inc
	$(PC) $(PFLAGS) $<

uucp-fl1$(EXEEXT): uucp-fl1.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

uuzext$(EXEEXT): uuzext.pas uuz$(UNITEXT) xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp$(EXEEXT): xp.pas archive$(UNITEXT) clip$(UNITEXT) crc$(UNITEXT) \
	database$(UNITEXT) databaso$(UNITEXT) datadef$(UNITEXT) \
	eddef$(UNITEXT) editor$(UNITEXT) feiertag$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) uuz$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2db$(UNITEXT) xp2f$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp5$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) \
	xp7$(UNITEXT) xp7f$(UNITEXT) xp7o$(UNITEXT) xp8$(UNITEXT) \
	xp9$(UNITEXT) xp_des$(UNITEXT) xp_iti$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpcurses$(UNITEXT) xpdatum$(UNITEXT) \
	xpdefine.inc xpdiff$(UNITEXT) xpe$(UNITEXT) xpf2$(UNITEXT) \
	xpfido$(UNITEXT) xpfidonl$(UNITEXT) xpglobal$(UNITEXT) \
	xpimpexp$(UNITEXT) xpkeys$(UNITEXT) xplinux$(UNITEXT) \
	xpmaus$(UNITEXT) xpmime$(UNITEXT) xpnt$(UNITEXT) \
	xpreg$(UNITEXT) xpstat$(UNITEXT) xpterm$(UNITEXT) \
	xpuu$(UNITEXT) xpview$(UNITEXT) xpx$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp$(EXEEXT): xp.pas archive$(UNITEXT) clip$(UNITEXT) crc$(UNITEXT) \
	database$(UNITEXT) databaso$(UNITEXT) datadef$(UNITEXT) \
	eddef$(UNITEXT) editor$(UNITEXT) feiertag$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) uuz$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2db$(UNITEXT) xp2f$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp5$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) \
	xp7$(UNITEXT) xp7f$(UNITEXT) xp7o$(UNITEXT) xp8$(UNITEXT) \
	xp9$(UNITEXT) xp_des$(UNITEXT) xp_iti$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc xpdiff$(UNITEXT) \
	xpe$(UNITEXT) xpf2$(UNITEXT) xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) xpimpexp$(UNITEXT) \
	xpkeys$(UNITEXT) xpmaus$(UNITEXT) xpmime$(UNITEXT) \
	xpnt$(UNITEXT) xpreg$(UNITEXT) xpstat$(UNITEXT) \
	xpterm$(UNITEXT) xpuu$(UNITEXT) xpview$(UNITEXT) xpx$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp-fm$(EXEEXT): xp-fm.pas crc$(UNITEXT) debug$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) modem$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) timer$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp-fm.inc \
	xpcurses$(UNITEXT) xpdefine.inc xpdiff$(UNITEXT) \
	xpglobal$(UNITEXT) zmodem$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp-fm$(EXEEXT): xp-fm.pas crc$(UNITEXT) debug$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) modem$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) timer$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp-fm.inc xpdefine.inc \
	xpdiff$(UNITEXT) xpglobal$(UNITEXT) zmodem$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpme$(EXEEXT): xpme.pas fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xplinux$(UNITEXT) xpmecol.inc
	$(PC) $(PFLAGS) $<

else

xpme$(EXEEXT): xpme.pas fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpmecol.inc
	$(PC) $(PFLAGS) $<

endif

yup2pkt$(EXEEXT): yup2pkt.pas dbase$(UNITEXT) fileio$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

zfido$(EXEEXT): zfido.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpcurses$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpdiff$(UNITEXT) xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

zfido$(EXEEXT): zfido.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpdiff$(UNITEXT) \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

zpr$(EXEEXT): zpr.pas dosx$(UNITEXT) typeform$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

zpr$(EXEEXT): zpr.pas dosx$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

# Units

archive$(UNITEXT): archive.pas montage$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

clip$(UNITEXT): clip.pas fileio$(UNITEXT) xp0$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

clip$(UNITEXT): clip.pas fileio$(UNITEXT) xp0$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

crc$(UNITEXT): crc.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

database$(UNITEXT): database.pas databas1.inc databas2.inc \
	database.inc datadef$(UNITEXT) datadef1$(UNITEXT) \
	inout$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

database$(UNITEXT): database.pas databas1.inc databas2.inc \
	database.inc datadef$(UNITEXT) datadef1$(UNITEXT) \
	inout$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

databaso$(UNITEXT): databaso.pas database$(UNITEXT) datadef$(UNITEXT) \
	datadef1$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

databaso$(UNITEXT): databaso.pas database$(UNITEXT) datadef$(UNITEXT) \
	datadef1$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

datadef$(UNITEXT): datadef.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

datadef1$(UNITEXT): datadef1.pas datadef$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

dbase$(UNITEXT): dbase.pas typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

debug$(UNITEXT): debug.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

dosx$(UNITEXT): dosx.pas linux/dosx.inc linux/dosxh.inc xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

dosx$(UNITEXT): dosx.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

eddef$(UNITEXT): eddef.pas keys$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

editor$(UNITEXT): editor.pas clip$(UNITEXT) eddef$(UNITEXT) editor.inc \
	encoder$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	printerx$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

editor$(UNITEXT): editor.pas clip$(UNITEXT) eddef$(UNITEXT) editor.inc \
	encoder$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	printerx$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

encoder$(UNITEXT): encoder.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

exxec$(UNITEXT): exxec.pas debug$(UNITEXT) fileio$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

feiertag$(UNITEXT): feiertag.pas montage$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

fileio$(UNITEXT): fileio.pas fileio.inc linux/fileio.inc \
	linux/fileioh1.inc typeform$(UNITEXT) xp0$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

fileio$(UNITEXT): fileio.pas fileio.inc typeform$(UNITEXT) \
	xp0$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

gpltools$(UNITEXT): gpltools.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

help$(UNITEXT): help.pas fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	printerx$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

help$(UNITEXT): help.pas fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	printerx$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

inout$(UNITEXT): inout.pas keys$(UNITEXT) maus2$(UNITEXT) \
	mouse$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

inout$(UNITEXT): inout.pas keys$(UNITEXT) maus2$(UNITEXT) \
	mouse$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ipaddr$(UNITEXT): ipaddr.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ipcclass$(UNITEXT): ipcclass.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

keys$(UNITEXT): keys.pas typeform$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

keys$(UNITEXT): keys.pas typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

lister$(UNITEXT): lister.pas fileio$(UNITEXT) gpltools$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

lister$(UNITEXT): lister.pas fileio$(UNITEXT) gpltools$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

maske$(UNITEXT): maske.pas clip$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske.inc maus2$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

maske$(UNITEXT): maske.pas clip$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske.inc maus2$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

maus2$(UNITEXT): maus2.pas inout$(UNITEXT) keys$(UNITEXT) \
	mouse$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

maus2$(UNITEXT): maus2.pas inout$(UNITEXT) keys$(UNITEXT) \
	mouse$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

modem$(UNITEXT): modem.pas debug$(UNITEXT) timer$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

montage$(UNITEXT): montage.pas typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

mouse$(UNITEXT): mouse.pas maus2$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ncnntp$(UNITEXT): ncnntp.pas ipcclass$(UNITEXT) ncsocket$(UNITEXT) \
	netcall$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ncpop3$(UNITEXT): ncpop3.pas ipcclass$(UNITEXT) ncsocket$(UNITEXT) \
	netcall$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ncsmtp$(UNITEXT): ncsmtp.pas ipcclass$(UNITEXT) ncsocket$(UNITEXT) \
	netcall$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ncsocket$(UNITEXT): ncsocket.pas ipaddr$(UNITEXT) netcall$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ncurses$(UNITEXT): ncurses.pas 
	$(PC) $(PFLAGS) $<

netcall$(UNITEXT): netcall.pas ipcclass$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

printerx$(UNITEXT): printerx.pas inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

printerx$(UNITEXT): printerx.pas inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

regexpr$(UNITEXT): regexpr.pas 
	$(PC) $(PFLAGS) $<

resource$(UNITEXT): resource.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

stack$(UNITEXT): stack.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

timer$(UNITEXT): timer.pas debug$(UNITEXT)
	$(PC) $(PFLAGS) $<

typeform$(UNITEXT): typeform.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

uart$(UNITEXT): uart.pas dosx$(UNITEXT) inout$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

uuz$(UNITEXT): uuz.pas fileio$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) xpcurses$(UNITEXT) xpdatum$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpheader.inc xplinux$(UNITEXT) \
	xpmakehd.inc
	$(PC) $(PFLAGS) $<

else

uuz$(UNITEXT): uuz.pas fileio$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpheader.inc xpmakehd.inc
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

win2$(UNITEXT): win2.pas dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

win2$(UNITEXT): win2.pas dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

winxp$(UNITEXT): winxp.pas inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

winxp$(UNITEXT): winxp.pas inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp0$(UNITEXT): xp0.pas keys$(UNITEXT) typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpheader.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),dos32)

xp1$(UNITEXT): xp1.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) dosx$(UNITEXT) \
	exxec$(UNITEXT) fileio$(UNITEXT) help$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	mouse$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1cm.inc xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1menu.inc xp1o$(UNITEXT) xp1o2$(UNITEXT) xp1s.inc \
	xp2$(UNITEXT) xpdefine.inc xpdos32$(UNITEXT) xpe$(UNITEXT) \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),linux)

xp1$(UNITEXT): xp1.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) dosx$(UNITEXT) \
	exxec$(UNITEXT) fileio$(UNITEXT) help$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	mouse$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1cm.inc xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1menu.inc xp1o$(UNITEXT) xp1o2$(UNITEXT) xp1s.inc \
	xp2$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc xpe$(UNITEXT) \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),os2)

xp1$(UNITEXT): xp1.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) dosx$(UNITEXT) \
	exxec$(UNITEXT) fileio$(UNITEXT) help$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	mouse$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1cm.inc xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1menu.inc xp1o$(UNITEXT) xp1o2$(UNITEXT) xp1s.inc \
	xp2$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpglobal$(UNITEXT) \
	xpnt$(UNITEXT) xpos2$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),win32)

xp1$(UNITEXT): xp1.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) dosx$(UNITEXT) \
	exxec$(UNITEXT) fileio$(UNITEXT) help$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	mouse$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1cm.inc xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1menu.inc xp1o$(UNITEXT) xp1o2$(UNITEXT) xp1s.inc \
	xp2$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpglobal$(UNITEXT) \
	xpnt$(UNITEXT) xpwin32$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp10$(UNITEXT): xp10.pas database$(UNITEXT) datadef$(UNITEXT) \
	feiertag$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10.inc xp10p.inc \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp4o$(UNITEXT) \
	xp4o2$(UNITEXT) xp5$(UNITEXT) xp7$(UNITEXT) xp9bp$(UNITEXT) \
	xpauto$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpfido$(UNITEXT) xpfidonl$(UNITEXT) xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp10$(UNITEXT): xp10.pas database$(UNITEXT) datadef$(UNITEXT) \
	feiertag$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10.inc xp10p.inc \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp4o$(UNITEXT) \
	xp4o2$(UNITEXT) xp5$(UNITEXT) xp7$(UNITEXT) xp9bp$(UNITEXT) \
	xpauto$(UNITEXT) xpdefine.inc xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp1help$(UNITEXT): xp1help.pas help$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp1help$(UNITEXT): xp1help.pas help$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp1input$(UNITEXT): xp1input.pas inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp1input$(UNITEXT): xp1input.pas inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp1o$(UNITEXT): xp1o.pas archive$(UNITEXT) clip$(UNITEXT) \
	crc$(UNITEXT) database$(UNITEXT) datadef$(UNITEXT) \
	dosx$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1input$(UNITEXT) \
	xp1o2$(UNITEXT) xp4$(UNITEXT) xp4o$(UNITEXT) xp_uue$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp1o$(UNITEXT): xp1o.pas archive$(UNITEXT) clip$(UNITEXT) \
	crc$(UNITEXT) database$(UNITEXT) datadef$(UNITEXT) \
	dosx$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1input$(UNITEXT) \
	xp1o2$(UNITEXT) xp4$(UNITEXT) xp4o$(UNITEXT) xp_uue$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpkeys$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp1o2$(UNITEXT): xp1o2.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp1o2$(UNITEXT): xp1o2.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),dos32)

xp2$(UNITEXT): xp2.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2cfg.inc xp3$(UNITEXT) xp5$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcfg$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpdos32$(UNITEXT) xpe$(UNITEXT) \
	xpeasy$(UNITEXT) xpfido$(UNITEXT) xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT) xpnt$(UNITEXT) xpreg$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),linux)

xp2$(UNITEXT): xp2.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2cfg.inc xp3$(UNITEXT) xp5$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcfg$(UNITEXT) \
	xpcurses$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpe$(UNITEXT) xpeasy$(UNITEXT) xpfido$(UNITEXT) \
	xpglobal$(UNITEXT) xpkeys$(UNITEXT) xplinux$(UNITEXT) \
	xpnt$(UNITEXT) xpreg$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),os2)

xp2$(UNITEXT): xp2.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2cfg.inc xp3$(UNITEXT) xp5$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcfg$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpeasy$(UNITEXT) \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpkeys$(UNITEXT) \
	xpnt$(UNITEXT) xpos2$(UNITEXT) xpreg$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),win32)

xp2$(UNITEXT): xp2.pas clip$(UNITEXT) crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2cfg.inc xp3$(UNITEXT) xp5$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcfg$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpeasy$(UNITEXT) \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpkeys$(UNITEXT) \
	xpnt$(UNITEXT) xpreg$(UNITEXT) xpwin32$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp2c$(UNITEXT): xp2c.pas database$(UNITEXT) datadef$(UNITEXT) \
	editor$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) uart$(UNITEXT) \
	win2$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp2$(UNITEXT) \
	xp4o2$(UNITEXT) xp9bp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp2c$(UNITEXT): xp2c.pas database$(UNITEXT) datadef$(UNITEXT) \
	editor$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) mouse$(UNITEXT) printerx$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) uart$(UNITEXT) \
	win2$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp2$(UNITEXT) \
	xp4o2$(UNITEXT) xp9bp$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp2db$(UNITEXT): xp2db.pas database$(UNITEXT) databaso$(UNITEXT) \
	datadef$(UNITEXT) datadef1$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp4o2$(UNITEXT) \
	xp5$(UNITEXT) xp9bp$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp2db$(UNITEXT): xp2db.pas database$(UNITEXT) databaso$(UNITEXT) \
	datadef$(UNITEXT) datadef1$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp4o2$(UNITEXT) \
	xp5$(UNITEXT) xp9bp$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp2f$(UNITEXT): xp2f.pas inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp2$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp2f$(UNITEXT): xp2f.pas inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp2$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp3$(UNITEXT): xp3.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1input$(UNITEXT) xp3ex$(UNITEXT) \
	xp3o$(UNITEXT) xp_des$(UNITEXT) xp_pgp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpmakehd.inc xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp3$(UNITEXT): xp3.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1input$(UNITEXT) xp3ex$(UNITEXT) \
	xp3o$(UNITEXT) xp_des$(UNITEXT) xp_pgp$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) xpmakehd.inc \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp3ex$(UNITEXT): xp3ex.pas database$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) resource$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) \
	xp3$(UNITEXT) xp_des$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpmime$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp3ex$(UNITEXT): xp3ex.pas database$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) resource$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) \
	xp3$(UNITEXT) xp_des$(UNITEXT) xpdefine.inc xpfido$(UNITEXT) \
	xpglobal$(UNITEXT) xpmime$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp3o$(UNITEXT): xp3o.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o.inc xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4o$(UNITEXT) xp6$(UNITEXT) xp8$(UNITEXT) \
	xp9bp$(UNITEXT) xp_pgp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp3o$(UNITEXT): xp3o.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o.inc xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4o$(UNITEXT) xp6$(UNITEXT) xp8$(UNITEXT) \
	xp9bp$(UNITEXT) xp_pgp$(UNITEXT) xpdatum$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp3o2$(UNITEXT): xp3o2.pas database$(UNITEXT) datadef$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp6$(UNITEXT) \
	xp_pgp$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),dos32)

xp4$(UNITEXT): xp4.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2f$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4.inc \
	xp4d.inc xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp4w.inc xp5$(UNITEXT) xp6$(UNITEXT) \
	xp6o$(UNITEXT) xp7$(UNITEXT) xp8$(UNITEXT) xp9$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpdefine.inc xpdos32$(UNITEXT) xpe$(UNITEXT) \
	xpfido$(UNITEXT) xpfidonl$(UNITEXT) xpglobal$(UNITEXT) \
	xpimpexp$(UNITEXT) xpkeys$(UNITEXT) xpmaus$(UNITEXT) \
	xpmime$(UNITEXT) xpnt$(UNITEXT) xpreg$(UNITEXT) \
	xpstat$(UNITEXT) xpterm$(UNITEXT) xpview$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),linux)

xp4$(UNITEXT): xp4.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2f$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4.inc \
	xp4d.inc xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp4w.inc xp5$(UNITEXT) xp6$(UNITEXT) \
	xp6o$(UNITEXT) xp7$(UNITEXT) xp8$(UNITEXT) xp9$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc xpe$(UNITEXT) \
	xpfido$(UNITEXT) xpfidonl$(UNITEXT) xpglobal$(UNITEXT) \
	xpimpexp$(UNITEXT) xpkeys$(UNITEXT) xpmaus$(UNITEXT) \
	xpmime$(UNITEXT) xpnt$(UNITEXT) xpreg$(UNITEXT) \
	xpstat$(UNITEXT) xpterm$(UNITEXT) xpview$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),os2)

xp4$(UNITEXT): xp4.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2f$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4.inc \
	xp4d.inc xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp4w.inc xp5$(UNITEXT) xp6$(UNITEXT) \
	xp6o$(UNITEXT) xp7$(UNITEXT) xp8$(UNITEXT) xp9$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) xpimpexp$(UNITEXT) \
	xpkeys$(UNITEXT) xpmaus$(UNITEXT) xpmime$(UNITEXT) \
	xpnt$(UNITEXT) xpos2$(UNITEXT) xpreg$(UNITEXT) \
	xpstat$(UNITEXT) xpterm$(UNITEXT) xpview$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),win32)

xp4$(UNITEXT): xp4.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1help$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp2f$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4.inc \
	xp4d.inc xp4e$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) \
	xp4o3$(UNITEXT) xp4w.inc xp5$(UNITEXT) xp6$(UNITEXT) \
	xp6o$(UNITEXT) xp7$(UNITEXT) xp8$(UNITEXT) xp9$(UNITEXT) \
	xp_pgp$(UNITEXT) xp_uue$(UNITEXT) xpauto$(UNITEXT) \
	xpcc$(UNITEXT) xpdefine.inc xpe$(UNITEXT) xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) xpimpexp$(UNITEXT) \
	xpkeys$(UNITEXT) xpmaus$(UNITEXT) xpmime$(UNITEXT) \
	xpnt$(UNITEXT) xpreg$(UNITEXT) xpstat$(UNITEXT) \
	xpterm$(UNITEXT) xpview$(UNITEXT) xpwin32$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp4e$(UNITEXT): xp4e.pas database$(UNITEXT) datadef$(UNITEXT) \
	dosx$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4$(UNITEXT) xp6$(UNITEXT) xp9$(UNITEXT) \
	xp9bp$(UNITEXT) xpauto$(UNITEXT) xpcc$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpfido$(UNITEXT) \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp4e$(UNITEXT): xp4e.pas database$(UNITEXT) datadef$(UNITEXT) \
	dosx$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) win2$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4$(UNITEXT) xp6$(UNITEXT) xp9$(UNITEXT) \
	xp9bp$(UNITEXT) xpauto$(UNITEXT) xpcc$(UNITEXT) xpdefine.inc \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp4o$(UNITEXT): xp4o.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4o.inc xp_pgp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpfido$(UNITEXT) xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT) xpmaus$(UNITEXT) xpnt$(UNITEXT) \
	xpview$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp4o$(UNITEXT): xp4o.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	printerx$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp4o.inc xp_pgp$(UNITEXT) xpdefine.inc \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpkeys$(UNITEXT) \
	xpmaus$(UNITEXT) xpnt$(UNITEXT) xpview$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp4o2$(UNITEXT): xp4o2.pas crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) fileio$(UNITEXT) \
	help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp4o2$(UNITEXT): xp4o2.pas crc$(UNITEXT) database$(UNITEXT) \
	databaso$(UNITEXT) datadef$(UNITEXT) fileio$(UNITEXT) \
	help$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp4o3$(UNITEXT): xp4o3.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1input$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) \
	xp4$(UNITEXT) xp6$(UNITEXT) xpcc$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpkeys$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp5$(UNITEXT): xp5.pas clip$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) feiertag$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp1o2$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp5$(UNITEXT): xp5.pas clip$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) feiertag$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp1o2$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp6$(UNITEXT): xp6.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4e$(UNITEXT) xp6l$(UNITEXT) xp6s.inc \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xp_des$(UNITEXT) \
	xp_pgp$(UNITEXT) xpcc$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpe$(UNITEXT) xpfido$(UNITEXT) xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp6$(UNITEXT): xp6.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4e$(UNITEXT) xp6l$(UNITEXT) xp6s.inc \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xp_des$(UNITEXT) \
	xp_pgp$(UNITEXT) xpcc$(UNITEXT) xpdefine.inc xpe$(UNITEXT) \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp6l$(UNITEXT): xp6l.pas xp0$(UNITEXT) xpcc$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp6o$(UNITEXT): xp6o.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4$(UNITEXT) xp4e$(UNITEXT) xp6$(UNITEXT) \
	xp6l$(UNITEXT) xp_des$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpe$(UNITEXT) xpfido$(UNITEXT) xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp6o$(UNITEXT): xp6o.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	stack$(UNITEXT) typeform$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xp4$(UNITEXT) xp4e$(UNITEXT) xp6$(UNITEXT) \
	xp6l$(UNITEXT) xp_des$(UNITEXT) xpdefine.inc xpe$(UNITEXT) \
	xpfido$(UNITEXT) xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp7$(UNITEXT): xp7.pas database$(UNITEXT) datadef$(UNITEXT) \
	debug$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) uuz$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp10$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp2c$(UNITEXT) xp3$(UNITEXT) \
	xp3o$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) xp5$(UNITEXT) \
	xp7.inc xp7f$(UNITEXT) xp7l$(UNITEXT) xp7o$(UNITEXT) xp7u.inc \
	xp8$(UNITEXT) xp9$(UNITEXT) xp9bp$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpdiff$(UNITEXT) xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) xpmaus$(UNITEXT) \
	xpnt$(UNITEXT) xpterm$(UNITEXT) xpuu$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp7$(UNITEXT): xp7.pas database$(UNITEXT) datadef$(UNITEXT) \
	debug$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) uuz$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp10$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp2c$(UNITEXT) xp3$(UNITEXT) \
	xp3o$(UNITEXT) xp4o$(UNITEXT) xp4o2$(UNITEXT) xp5$(UNITEXT) \
	xp7.inc xp7f$(UNITEXT) xp7l$(UNITEXT) xp7o$(UNITEXT) xp7u.inc \
	xp8$(UNITEXT) xp9$(UNITEXT) xp9bp$(UNITEXT) xpdefine.inc \
	xpdiff$(UNITEXT) xpfido$(UNITEXT) xpfidonl$(UNITEXT) \
	xpglobal$(UNITEXT) xpmaus$(UNITEXT) xpnt$(UNITEXT) \
	xpterm$(UNITEXT) xpuu$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp7f$(UNITEXT): xp7f.pas debug$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp3$(UNITEXT) \
	xp3o$(UNITEXT) xp7$(UNITEXT) xp7l$(UNITEXT) xp7o$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpdiff$(UNITEXT) \
	xpf2$(UNITEXT) xpfido$(UNITEXT) xpfidonl$(UNITEXT) \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp7f$(UNITEXT): xp7f.pas debug$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) xp3$(UNITEXT) \
	xp3o$(UNITEXT) xp7$(UNITEXT) xp7l$(UNITEXT) xp7o$(UNITEXT) \
	xpdefine.inc xpdiff$(UNITEXT) xpf2$(UNITEXT) xpfido$(UNITEXT) \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) 
	$(PC) $(PFLAGS) $<

endif

xp7l$(UNITEXT): xp7l.pas typeform$(UNITEXT) xp0$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp7o$(UNITEXT): xp7o.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) uart$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1o$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp6$(UNITEXT) xp7$(UNITEXT) xp7l$(UNITEXT) xp9bp$(UNITEXT) \
	xp_iti$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp7o$(UNITEXT): xp7o.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) debug$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) uart$(UNITEXT) winxp$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) xp1o$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp6$(UNITEXT) xp7$(UNITEXT) xp7l$(UNITEXT) xp9bp$(UNITEXT) \
	xp_iti$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp8$(UNITEXT): xp8.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) xp8.inc xp8fs.inc \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xp_iti$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnntp$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifeq ($(OS),dos32)

xp8$(UNITEXT): xp8.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) xp8.inc xp8fs.inc \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xp_iti$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif
ifneq (,$(findstring $(OS),os2 win32))

xp8$(UNITEXT): xp8.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1help$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2c$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) xp3o2$(UNITEXT) \
	xp4$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) xp8.inc xp8fs.inc \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xp_iti$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnntp$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xp9$(UNITEXT): xp9.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp9.inc xp9bp$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT) xpnt$(UNITEXT) \
	xpterm$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp9$(UNITEXT): xp9.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp10$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp2$(UNITEXT) xp2c$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp9.inc xp9bp$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT) xpterm$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp9bp$(UNITEXT): xp9bp.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp2$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp_des$(UNITEXT): xp_des.pas fileio$(UNITEXT) inout$(UNITEXT) \
	maus2$(UNITEXT) xp0$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp_des$(UNITEXT): xp_des.pas fileio$(UNITEXT) inout$(UNITEXT) \
	maus2$(UNITEXT) xp0$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xp_iti$(UNITEXT): xp_iti.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xp_pgp$(UNITEXT): xp_pgp.pas database$(UNITEXT) fileio$(UNITEXT) \
	maske$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) \
	xp3o$(UNITEXT) xp3o2$(UNITEXT) xp6$(UNITEXT) xpcc$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xp_uue$(UNITEXT): xp_uue.pas database$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xp_uue$(UNITEXT): xp_uue.pas database$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp3$(UNITEXT) xp3ex$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpauto$(UNITEXT): xpauto.pas database$(UNITEXT) datadef$(UNITEXT) \
	dosx$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3o$(UNITEXT) xp6$(UNITEXT) xp9bp$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpmaus$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpcc$(UNITEXT): xpcc.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) maske$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4e$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpcc$(UNITEXT): xpcc.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) maske$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp4e$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpcfg$(UNITEXT): xpcfg.pas fileio$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpcfg$(UNITEXT): xpcfg.pas fileio$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpcurses$(UNITEXT): xpcurses.pas inout$(UNITEXT) ncurses$(UNITEXT) \
	typeform$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdatum$(UNITEXT): xpdatum.pas montage$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdiff$(UNITEXT): xpdiff.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdos32$(UNITEXT): xpdos32.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpe$(UNITEXT): xpe.pas dosx$(UNITEXT) eddef$(UNITEXT) editor$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp10$(UNITEXT) xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp5$(UNITEXT) xp6$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpe$(UNITEXT): xpe.pas dosx$(UNITEXT) eddef$(UNITEXT) editor$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp10$(UNITEXT) xp1help$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp5$(UNITEXT) xp6$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpeasy$(UNITEXT): xpeasy.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp1o2$(UNITEXT) xp2c$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpeasy$(UNITEXT): xpeasy.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) mouse$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) win2$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp1o2$(UNITEXT) xp2c$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpf2$(UNITEXT): xpf2.pas archive$(UNITEXT) fileio$(UNITEXT) \
	montage$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpf2$(UNITEXT): xpf2.pas archive$(UNITEXT) fileio$(UNITEXT) \
	montage$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) \
	xp3o2$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpfido$(UNITEXT): xpfido.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp2$(UNITEXT) xp3$(UNITEXT) xp4e$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpf1.inc xpfidonl$(UNITEXT) \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpfido$(UNITEXT): xpfido.pas archive$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) keys$(UNITEXT) lister$(UNITEXT) \
	maske$(UNITEXT) maus2$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) stack$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xp1o$(UNITEXT) xp2$(UNITEXT) xp3$(UNITEXT) xp4e$(UNITEXT) \
	xpdefine.inc xpf1.inc xpfidonl$(UNITEXT) xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpfidonl$(UNITEXT): xpfidonl.pas archive$(UNITEXT) fileio$(UNITEXT) \
	maske$(UNITEXT) montage$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) \
	xp3$(UNITEXT) xpdefine.inc xpfido$(UNITEXT) xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpftnadr$(UNITEXT): xpftnadr.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpglobal$(UNITEXT): xpglobal.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpimpexp$(UNITEXT): xpimpexp.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT) xpmaus$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpimpexp$(UNITEXT): xpimpexp.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) maske$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpmaus$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpipc$(UNITEXT): xpipc.pas ipcclass$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) 
	$(PC) $(PFLAGS) $<

else

xpipc$(UNITEXT): xpipc.pas ipcclass$(UNITEXT) maus2$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpkeys$(UNITEXT): xpkeys.pas fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp4o$(UNITEXT) xp7$(UNITEXT) \
	xp9$(UNITEXT) xpauto$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xplinux$(UNITEXT): xplinux.pas typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpmaus$(UNITEXT): xpmaus.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3o2$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) xp9$(UNITEXT) \
	xp_iti$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpmaus$(UNITEXT): xpmaus.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) stack$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3o2$(UNITEXT) xp6$(UNITEXT) xp6o$(UNITEXT) xp9$(UNITEXT) \
	xp_iti$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpmime$(UNITEXT): xpmime.pas database$(UNITEXT) fileio$(UNITEXT) \
	keys$(UNITEXT) lister$(UNITEXT) montage$(UNITEXT) \
	resource$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xp1$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) xp3ex$(UNITEXT) \
	xp3o$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpnntp$(UNITEXT): xpnntp.pas inout$(UNITEXT) ipaddr$(UNITEXT) \
	ipcclass$(UNITEXT) maus2$(UNITEXT) ncnntp$(UNITEXT) \
	ncsocket$(UNITEXT) netcall$(UNITEXT) resource$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpipc$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpnntp$(UNITEXT): xpnntp.pas inout$(UNITEXT) ipaddr$(UNITEXT) \
	ipcclass$(UNITEXT) maus2$(UNITEXT) ncnntp$(UNITEXT) \
	ncsocket$(UNITEXT) netcall$(UNITEXT) resource$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp1input$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpipc$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpnodes$(UNITEXT): xpnodes.pas typeform$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

xpnt$(UNITEXT): xpnt.pas crc$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) typeform$(UNITEXT) xp0$(UNITEXT) \
	xpdefine.inc
	$(PC) $(PFLAGS) $<

xpos2$(UNITEXT): xpos2.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpreg$(UNITEXT): xpreg.pas clip$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp6$(UNITEXT) xp9bp$(UNITEXT) xpauto$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpreg$(UNITEXT): xpreg.pas clip$(UNITEXT) database$(UNITEXT) \
	datadef$(UNITEXT) fileio$(UNITEXT) inout$(UNITEXT) \
	keys$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) printerx$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp1input$(UNITEXT) xp1o$(UNITEXT) xp1o2$(UNITEXT) \
	xp6$(UNITEXT) xp9bp$(UNITEXT) xpauto$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpstat$(UNITEXT): xpstat.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp2$(UNITEXT) \
	xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp6$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpfidonl$(UNITEXT) xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpstat$(UNITEXT): xpstat.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	lister$(UNITEXT) maske$(UNITEXT) maus2$(UNITEXT) \
	montage$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xp2$(UNITEXT) \
	xp3$(UNITEXT) xp3o$(UNITEXT) xp3o2$(UNITEXT) xp6$(UNITEXT) \
	xp9$(UNITEXT) xp9bp$(UNITEXT) xpdefine.inc xpfidonl$(UNITEXT) \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpterm$(UNITEXT): xpterm.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp10$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2$(UNITEXT) xp2c$(UNITEXT) xp9bp$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpterm$(UNITEXT): xpterm.pas database$(UNITEXT) datadef$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) keys$(UNITEXT) \
	maus2$(UNITEXT) resource$(UNITEXT) typeform$(UNITEXT) \
	uart$(UNITEXT) winxp$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xp10$(UNITEXT) xp1input$(UNITEXT) xp1o$(UNITEXT) \
	xp1o2$(UNITEXT) xp2$(UNITEXT) xp2c$(UNITEXT) xp9bp$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT) xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifeq ($(OS),linux)

xpuu$(UNITEXT): xpuu.pas fileio$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) \
	xpcurses$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpuu$(UNITEXT): xpuu.pas fileio$(UNITEXT) resource$(UNITEXT) \
	typeform$(UNITEXT) xp0$(UNITEXT) xp1$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

xpview$(UNITEXT): xpview.pas database$(UNITEXT) dosx$(UNITEXT) \
	fileio$(UNITEXT) inout$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpnt$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpwin32$(UNITEXT): xpwin32.pas typeform$(UNITEXT) winxp$(UNITEXT) \
	xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)

xpx$(UNITEXT): xpx.pas crc$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) mouse$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xpcurses$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

xpx$(UNITEXT): xpx.pas crc$(UNITEXT) dosx$(UNITEXT) fileio$(UNITEXT) \
	inout$(UNITEXT) mouse$(UNITEXT) typeform$(UNITEXT) \
	xp0$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

zmodem$(UNITEXT): zmodem.pas crc$(UNITEXT) debug$(UNITEXT) timer$(UNITEXT)
	$(PC) $(PFLAGS) $<

# Sprachmodule

$(RESFILES): %.res: %.rq rc$(EXEEXT)
	$(RC) $<

# Dokumentation

documentation:
	$(MAKE) -C doc

# Installation

install: install_bindir $(patsubst %,install_%,$(BINFILES)) \
	install_datadir $(patsubst %,install_%,$(RESFILES)) \
	$(patsubst %,install_%,$(RSTFILES)) \
	install_exampledir $(patsubst %,install_%,$(EXAMPLES)) \
	$(patsubst %,install_%,$(CONTRIB))
	$(INSTALL_DATA) icons.res $(datadir)
	$(MAKE) -C doc install

install_bindir:
	-$(INSTALLDIR) $(bindir)

# Installiert Binaries.  Linux bekommt Softlinks nach /usr/bin.

ifeq ($(OS),linux)
$(patsubst %,install_%,$(BINFILES)): install_%: %
	$(INSTALL_PROGRAM) $* $(bindir)
	$(LN) $(bindir)$(SEP)$* $(linkdir)$(SEP)$*
else
$(patsubst %,install_%,$(BINFILES)): install_%: %
	$(INSTALL_PROGRAM) $* $(bindir)
endif

install_datadir:
	-$(INSTALLDIR) $(datadir)

$(patsubst %,install_%,$(RESFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)

$(patsubst %,install_%,$(RSTFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)

$(patsubst %,install_%,$(EXAMPLES)): install_%:
	$(INSTALL_DATA) beispiel$(SEP)$* $(exampledir)

install_exampledir:
	-$(INSTALLDIR) $(exampledir)

$(patsubst %,install_%,$(CONTRIBBIN)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)$(OS)$(SEP)$* $(bindir)

$(patsubst %,install_%,$(CONTRIBDATA)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)data$(SEP)$* $(datadir)

# Deinstallation

uninstall: $(patsubst %,uninstall_%,$(CONTRIBEXAMPLE)) \
	$(patsubst %,uninstall_%,$(DATAFILES)) \
	$(patsubst %,uninstall_%,$(INSTALLBINFILES))
	-$(RMDIR) $(bindir)
	-$(RMDIR) $(datadir)
	-$(RMDIR) $(exampledir)
	$(MAKE) -C doc uninstall
	-$(RMDIR) $(prefix)

$(patsubst %,uninstall_%,$(CONTRIBEXAMPLE)): uninstall_%:
	-$(RM) $(exampledir)$(SEP)$*

$(patsubst %,uninstall_%,$(DATAFILES)): uninstall_%:
	-$(RM) $(datadir)$(SEP)$*

ifeq ($(OS),linux)
$(patsubst %,uninstall_%,$(INSTALLBINFILES)): uninstall_%:
	-$(RM) $(linkdir)$(SEP)$*
	-$(RM) $(bindir)$(SEP)$*
else
$(patsubst %,uninstall_%,$(INSTALLBINFILES)): uninstall_%:
	-$(RM) $(bindir)$(SEP)$*
endif

# Aufraeumen

clean: localclean
	$(MAKE) -C ObjCOM clean
	$(MAKE) -C doc clean

localclean: $(patsubst %,clean_%,$(COMPBINFILES)) \
	$(patsubst %,clean_%,$(UNITS)) \
	$(patsubst %,clean_%,$(RESFILES)) \
	$(patsubst %,clean_%,$(RSTFILES))
	-$(RM) $(DISTFILE)

$(patsubst %,clean_%,$(COMPBINFILES)): clean_%$(EXEEXT):
	-$(RM) $*$(EXEEXT)
	-$(RM) $*$(OBJEXT)
	-$(RM) $*$(LIBEXT)

$(patsubst %,clean_%,$(UNITS)): clean_%:
	-$(RM) $*$(UNITEXT)
	-$(RM) $*$(OBJEXT)
	-$(RM) $(LIBPREF)$*$(LIBEXT)

$(patsubst %,clean_%,$(RESFILES)): clean_%:
	-$(RM) $*

$(patsubst %,clean_%,$(RSTFILES)): clean_%:
	-$(RM) $*

distclean: localclean
	$(MAKE) -C ObjCOM distclean
	$(MAKE) -C doc distclean

mostlyclean: localclean
	$(MAKE) -C ObjCOM mostlyclean
	$(MAKE) -C doc mostlyclean

maintainer-clean: localclean
	$(MAKE) -C ObjCOM maintainer-clean
	$(MAKE) -C doc maintainer-clean

# Erstellen eines Quellcodearchivs

# Etwas umstaendlich wegen der maximalen Zeilenlaenge von COMMAND.COM.

dist:
	-$(RM) $(DISTFILE)
	$(RAR) $(RARFLAGS) $(DISTFILE) file_id.diz Makefile icons.res \
	*.inc *.pas *.rq *.bat
	$(RAR) $(RARFLAGS) $(DISTFILE) beispiel$(SEP)*.scr \
	beispiel$(SEP)*.xps
	$(RAR) $(RARFLAGS) $(DISTFILE) doc$(SEP)Makefile doc$(SEP)*.txt \
	doc$(SEP)*.dq doc$(SEP)*.ihq doc$(SEP)xpoint.xml \
	doc$(SEP)xpoint.dsl doc$(SEP)xpoint.cat doc$(SEP)dbform \
	doc$(SEP)readme.lnx
	$(RAR) $(RARFLAGS) $(DISTFILE) linux$(SEP)*.inc
	$(RAR) $(RARFLAGS) $(DISTFILE) ObjCOM$(SEP)Makefile \
	ObjCOM$(SEP)*.inc ObjCOM$(SEP)*.pas ObjCOM$(SEP)*.txt

#
# $Log$
# Revision 1.16  2000/10/08 18:15:12  fe
# Uebersetzung des Handbuchs ergaenzt.
# Neue Unit gpltools ergaenzt.
#
# Revision 1.15  2000/10/05 22:06:18  fe
# Unterstuetzung fuer Windows-RessourceStringTables ergaenzt.
#
# Revision 1.14  2000/10/05 19:55:43  fe
# Korrekturen, v.a. fuer FPC/Win32.
#
# Revision 1.13  2000/10/02 17:31:02  fe
# icons.res wird nicht mehr geloescht.  Statt xpicons.dll wird jetzt
# icons.res installiert.
#
# Revision 1.12  2000/10/02 13:38:10  fe
# Vergessenes nachgetragen.
#
# Revision 1.11  2000/10/02 13:07:30  fe
# uucico-Dateien eingetragen.
# ungetestete VPC-Unterstuetzung eingebaut.
# Link zu OS/2-make-Binary geloescht (zu alt).
#
# Revision 1.10  2000/10/01 07:50:52  fe
# Abhaengigkeiten von pmconv aktualisiert.
#
# Revision 1.9  2000/09/30 16:52:38  fe
# maggi.pas ist jetzt uebersetzbar.
#
# Revision 1.8  2000/09/30 14:49:22  fe
# pmconv.pas jetzt uebersetzbar.
#
# Revision 1.7  2000/09/28 14:21:02  fe
# Ueberpruefung der Variablen eingebaut.
# kleinere Korrekturen
#
# Revision 1.6  2000/09/27 21:21:08  fe
# Login-Scripts und Beispielschablonen sind jetzt im Repository.
# zm.exe und zconfig.exe werden nicht mehr installiert.
#
# Revision 1.5  2000/09/27 12:20:37  fe
# kleinere Fehlerkorrekturen
#
# Revision 1.4  2000/09/26 14:02:48  fe
# komlett ueberarbeitet
#
# Revision 1.3  2000/08/07 10:23:56  hd
# - Installation unter Linux angepasst
#
# Revision 1.2  2000/08/07 10:08:23  hd
# - Makefile angepasst
#
# Revision 1.1  2000/08/06 20:10:32  mk
# - erster Versuch eines Makefiles
#

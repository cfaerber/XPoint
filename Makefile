#
# OpenXP Makefile
# <http://www.openxp.de/>
#
# $Id$
#
# fuer GNU make <http://www.gnu.org/software/make/>
#
# DOS/32: <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/mak3791b.zip>

# Damit Sie dieses Makefile benutzen koennen, muessen ein aktuelles GNU
# make (s.o.), der Free Pascal Compiler <http://www.freepascal.org/>
# oder der Virtual Pascal Compiler <http://www.vpascal.com/> und die GNU
# fileutils <http://www.gnu.org/software/fileutils/> (DOS/32:
# <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/fil316b.zip>) auf
# ihrem System installiert sein.
#
# Wenn Sie mit Hilfe dieses Makefiles OpenXP installieren wollen,
# benoetigen Sie ausserdem ein Archiv mit verschiedenen Dateien, die
# nicht im OpenXP-Quellcode enthalten sind:
# <ftp://ftp.openxp.de/openxp/snapshot/oxpcontr.rar>
#
# Wenn Sie ein Quellcodearchiv erstellen wollen, muss die
# Archivierungssoftware rar <http://www.rarsoft.com/> auf ihrem System
# installiert sein.
#
# Wenn Sie aus dem DocBook-Quellcode des Handbuchs die verschiedenen
# Darstellungsformate (Plaintext, HTML etc.) erstellen wollen, muessen
# OpenJade <http://www.netfolder.com/DSSSL/>, sed
# <http://www.gnu.org/software/sed/> (DOS/32:
# <ftp://ftp.simtel.net/pub/simtelnet/gnu/djgpp/v2gnu/sed302b.zip>), w3m
# <http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/eng/>, tidy
# <http://www.w3c.org/People/Ragget/tidy/>, die DocBook DTD
# <http://www.oasis-open.org/docbook/sgml/4.1/docbk41.zip>, die ISO 8879
# Entities
# <http://fallout.camputview.indiana.edu/ports/distfiles/isoENTS.zip>
# und die Modular Stylesheets
# <http://nwalsh.com/docbook/dsssl/db157.zip> auf ihrem System
# installiert sein.
#
# Wenn Sie die Darstellungsformate DVI, PS und PDF der Dokumentation
# erstellen wollen, benoetigen Sie darueberhinaus ein aktuelles
# TeX-Paket und jadetex <http://www.tug.org/applications/jadetex/>.
#
# Unten muessen Sie einige Variablen setzen.

# make                   uebersetzt das ganze Programm
# make install           installiert das Programm
# make uninstall         deinstalliert das Programm
# make clean             raeumt das Verzeichnis auf
# make maintainer-clean  raeumt das Verzeichnis auf inkl. Plaintext-Handbuch
# make fulldoc           erstellt Dokumentation in allen moegl. Formaten
# make dist              erstellt Quellcodearchiv
# make docdist           erstellt Quellcodearchiv der Dokumentation
# make TAGS              erstellt TAGS-Datei fuer Emacs-Editor

# Das Betriebssystem, fuer das OpenXP uebersetzt werden soll.
# (MUSS gesetzt werden.)
#
# dos32         DOS 32 Bit
# freebsd       FreeBSD
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
#prefix = C:\Programme\OpenXP

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

# Name des Quellcode-Archivs
# (MUSS gesetzt werden, falls "make dist" aufgerufen wird.)
#
#DISTFILE = oxp370_s

# Name des Archivs mit der vollstaendigen Dokumentation
# (MUSS gesetzt werden, falls "make docdist" aufgerufen wird.)
#
#DOCDISTFILE = oxp370_d

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
SETLINKS ?= yes

ifeq (,$(findstring $(COMPILER),fpc vpc))
$(error Variable "COMPILER" muss auf "fpc" oder "vpc" gesetzt werden)
endif

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

ifneq (,$(findstring $(OS),dos32 os2 win32))

ifeq ($(OS),win32)
prefix ?= C:\Programme\OpenXP
else
prefix ?= \OPENXP
endif

bindir ?= $(prefix)
datadir ?= $(prefix)
docdir ?= $(prefix)\DOC
exampledir ?= $(prefix)\BEISPIEL

EXEEXT = .exe
# Weil ein Backslash am Ende einer Zeile eine Sonderbedeutung hat,
# muss hier getrickst werden
SEP = $(subst ,,\)
MAKE ?= make
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

ifeq ($(OS),freebsd)

prefix ?= /usr/local
bindir ?= $(prefix)/bin
datadir ?= $(prefix)/share/openxp
docdir ?= $(prefix)/share/doc/openxp
exampledir ?= $(prefix)/share/examples/openxp

EXEEXT =
SEP = /
MAKE ?= gmake
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

ifeq ($(OS),linux)

prefix ?= /opt/openxp
bindir ?= $(prefix)/bin
binlinkdir ?= /usr/bin
datadir ?= $(prefix)/lib
datalinkdir ?= /usr/lib
docdir ?= $(prefix)/doc
exampledir ?= $(prefix)/beispiel

EXEEXT =
SEP = /
MAKE ?= make
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
PFLAGS += -Ct -Cr- -dDEBUG -Sg -pg -FuObjCOM
else
PFLAGS += -Cr- -O2 -Sg -FuObjCOM
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
PF_freebsd = -TFREEBSD -dUnixDevelop
PF_linux = -TLINUX -dUnixDevelop
PF_os2 = -TOS2
PF_win32 = -TWIN32
PF_386 = -Op1
PF_486 = -Op1
PF_586 = -Op2
PF_686 = -Op3

uuzext$(EXEEXT): PFLAGS += -S2

endif

ifeq ($(COMPILER),vpc)
PC = vpc

vpprefix ?= \vp21

ifeq ($(DEBUG),yes)
PFLAGS += -DDEBUG -$D+ -$Q- -$R- -$S- -$V+ -M -U"$(vpprefix)$(SEP)units.%p;ObjCOM" -L"$(vpprefix)$(SEP)lib.%p;$(vpprefix)$(SEP)units.%p" -R"$(vpprefix)$(SEP)res.%p"
else
PFLAGS += -$$SmartLink+ -M -U"$(vpprefix)$(SEP)units.%p;ObjCOM" -L"$(vpprefix)$(SEP)lib.%p;$(vpprefix)$(SEP)units.%p" -R"$(vpprefix)$(SEP)res.%p"
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
PF_486 = -$$G4+
PF_586 = -$$G5+
PF_686 = -$$G5+

endif

export PC
export OS
export CPU
export prefix
export bindir
export binlinkdir
export datadir
export datalinkdir
export docdir
export exampledir
export COMPILER
export DEBUG
export MAKEHB
export dbdir
export entdir
export jadedir
export moddir
export contribdir
export MAKE
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
	printerx regexpr resource stack timer typeform uart unicode \
	utftools uuz win2 winxp xp0 xp1 xp10 xp1help xp1input xp1o \
	xp1o2 xp2 xp2c xp2db xp2f xp3 xp3ex xp3o xp3o2 xp4 xp4e xp4o \
	xp4o2 xp4o3 xp5 xp6 xp6l xp6o xp7 xp7f xp7l xp7o xp8 xp9 xp9bp \
	xp_des xp_iti xp_pgp xp_uue xpauto xpcc xpcfg xpcurses xpdatum \
	xpdiff xpdos32 xpe xpeasy xpf2 xpfido xpfidonl xpftnadr \
	xpglobal xpimpexp xpipc xpkeys xplinux xpmaus xpmime xpnntp \
	xpnodes xpnt xpos2 xpreg xpstat xpterm xpuu xpview xpwin32 xpx \
	zmodem
RES = xp-d xp-e xpfm-d xpfm-e xpuu-d xpuu-e
EXAMPLES = gsbox.scr madness.scr magic.scr maus.scr o-magic.scr \
	oz-netz.scr pcsysop.scr privhead.xps qbrett.xps qpmpriv.xps \
	qpriv.xps quark.scr quoteto.xps uucp.scr z-netz.scr \
	glossary.cfg

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

all: objcomunit $(COMPBINFILES) $(RESFILES) documentation

# Programme

docform$(EXEEXT): docform.pas fileio$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

uuzext$(EXEEXT): uuzext.pas uuz$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

keys$(UNITEXT): keys.pas typeform$(UNITEXT) xpcurses$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

else

keys$(UNITEXT): keys.pas typeform$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

endif

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

unicode$(UNITEXT): unicode.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

utftools$(UNITEXT): utftools.pas charsets$(SEP)8859_1.inc \
	charsets$(SEP)8859_10.inc charsets$(SEP)8859_13.inc \
	charsets$(SEP)8859_14.inc charsets$(SEP)8859_15.inc \
	charsets$(SEP)8859_2.inc charsets$(SEP)8859_3.inc \
	charsets$(SEP)8859_4.inc charsets$(SEP)8859_5.inc \
	charsets$(SEP)8859_6.inc charsets$(SEP)8859_7.inc \
	charsets$(SEP)8859_8.inc charsets$(SEP)8859_9.inc \
	charsets$(SEP)cp1251.inc charsets$(SEP)cp1252.inc \
	charsets$(SEP)cp1255.inc charsets$(SEP)cp437.inc \
	charsets$(SEP)cp866.inc xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

uuz$(UNITEXT): uuz.pas fileio$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) unicode$(UNITEXT) utftools$(UNITEXT) \
	xpcurses$(UNITEXT) xpdatum$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xpheader.inc xplinux$(UNITEXT) \
	xpmakehd.inc
	$(PC) $(PFLAGS) $<

else

uuz$(UNITEXT): uuz.pas fileio$(UNITEXT) montage$(UNITEXT) \
	typeform$(UNITEXT) unicode$(UNITEXT) utftools$(UNITEXT) \
	xpdatum$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpheader.inc xpmakehd.inc
	$(PC) $(PFLAGS) $<

endif

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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
ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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
ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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
ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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
	typeform$(UNITEXT) utftools$(UNITEXT) xpdefine.inc \
	xpglobal$(UNITEXT) xplinux$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdatum$(UNITEXT): xpdatum.pas montage$(UNITEXT) typeform$(UNITEXT) \
	xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdiff$(UNITEXT): xpdiff.pas xpdefine.inc xpglobal$(UNITEXT)
	$(PC) $(PFLAGS) $<

xpdos32$(UNITEXT): xpdos32.pas utftools$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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
	resource$(UNITEXT) typeform$(UNITEXT) utftools$(UNITEXT) \
	xp0$(UNITEXT) xp1$(UNITEXT) xp1o$(UNITEXT) xp3$(UNITEXT) \
	xp3ex$(UNITEXT) xp3o$(UNITEXT) xpdefine.inc xpglobal$(UNITEXT) \
	xpkeys$(UNITEXT)
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

xpos2$(UNITEXT): xpos2.pas utftools$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

ifneq (,$(findstring $(OS),freebsd linux))

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

xpwin32$(UNITEXT): xpwin32.pas typeform$(UNITEXT) utftools$(UNITEXT) \
	winxp$(UNITEXT) xpdefine.inc
	$(PC) $(PFLAGS) $<

ifneq (,$(findstring $(OS),freebsd linux))

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

# ObjCOM-Unit

objcomunit: debug$(UNITEXT) timer$(UNITEXT)
	$(MAKE) -C ObjCOM

# Sprachmodule

$(RESFILES): %.res: %.rq rc$(EXEEXT)
	$(RC) $<

# Dokumentation

documentation:
	$(MAKE) -C doc

fulldoc:
	$(MAKE) -C doc fulldoc

# Installation

ifeq (,$(contribdir))
install:
	$(error Variable "contribdir" muss gesetzt werden)
else
install: install_bindir $(patsubst %,install_%,$(BINFILES)) \
	install_datadir $(patsubst %,install_%,$(RESFILES)) \
	$(patsubst %,install_%,$(RSTFILES)) \
	install_exampledir $(patsubst %,install_%,$(EXAMPLES)) \
	$(patsubst %,install_%,$(CONTRIB))
	$(INSTALL_DATA) icons.res $(datadir)
	$(MAKE) -C doc install
endif

installdirs: install_bindir install_datadir install_exampledir
	$(MAKE) -C doc installdirs

install_bindir:
	-$(INSTALLDIR) $(bindir)

# Installiert Binaries.  Linux bekommt Softlinks nach /usr/bin.

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,install_%,$(BINFILES)): install_%: %
	$(INSTALL_PROGRAM) $* $(bindir)
	$(LN) $(bindir)$(SEP)$* $(binlinkdir)$(SEP)$*
else
$(patsubst %,install_%,$(BINFILES)): install_%: %
	$(INSTALL_PROGRAM) $* $(bindir)
endif
else
$(patsubst %,install_%,$(BINFILES)): install_%: %
	$(INSTALL_PROGRAM) $* $(bindir)
endif

install_datadir:
	-$(INSTALLDIR) $(datadir)

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,install_%,$(RESFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
	$(LN) $(datadir)$(SEP)$* $(datalinkdir)$(SEP)$*
else
$(patsubst %,install_%,$(RESFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif
else
$(patsubst %,install_%,$(RESFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,install_%,$(RSTFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
	$(LN) $(datadir)$(SEP)$* $(datalinkdir)$(SEP)$*
else
$(patsubst %,install_%,$(RSTFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif
else
$(patsubst %,install_%,$(RSTFILES)): install_%: %
	$(INSTALL_DATA) $* $(datadir)
endif

$(patsubst %,install_%,$(EXAMPLES)): install_%:
	$(INSTALL_DATA) beispiel$(SEP)$* $(exampledir)

install_exampledir:
	-$(INSTALLDIR) $(exampledir)

$(patsubst %,install_%,$(CONTRIBBIN)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)$(OS)$(SEP)$* $(bindir)

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,install_%,$(CONTRIBDATA)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)data$(SEP)$* $(datadir)
	$(LN) $(datadir)$(SEP)$* $(datalinkdir)$(SEP)$*
else
$(patsubst %,install_%,$(CONTRIBDATA)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)data$(SEP)$* $(datadir)
endif
else
$(patsubst %,install_%,$(CONTRIBDATA)): install_%:
	$(INSTALL_DATA) $(contribdir)$(SEP)data$(SEP)$* $(datadir)
endif

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

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,uninstall_%,$(DATAFILES)): uninstall_%:
	-$(RM) $(datalinkdir)$(SEP)$*
	-$(RM) $(datadir)$(SEP)$*
else
$(patsubst %,uninstall_%,$(DATAFILES)): uninstall_%:
	-$(RM) $(datadir)$(SEP)$*
endif
else
$(patsubst %,uninstall_%,$(DATAFILES)): uninstall_%:
	-$(RM) $(datadir)$(SEP)$*
endif

ifeq ($(OS),linux)
ifeq ($(SETLINKS),yes)
$(patsubst %,uninstall_%,$(INSTALLBINFILES)): uninstall_%:
	-$(RM) $(binlinkdir)$(SEP)$*
	-$(RM) $(bindir)$(SEP)$*
else
$(patsubst %,uninstall_%,$(INSTALLBINFILES)): uninstall_%:
	-$(RM) $(bindir)$(SEP)$*
endif
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
	-$(RM) $(DISTFILE).rar
	-$(RM) $(DOCDISTFILE).rar
	-$(RM) TAGS

$(patsubst %,clean_%,$(COMPBINFILES)): clean_%$(EXEEXT):
	-$(RM) $*$(EXEEXT)
	-$(RM) $*$(OBJEXT)
	-$(RM) $*$(LNKEXT)

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

ifeq (,$(DISTFILE))
dist:
	$(error Variable "DISTFILE" muss gesetzt werden)
else
dist:
	-$(RM) $(DISTFILE).rar
	$(RAR) $(RARFLAGS) $(DISTFILE).rar file_id.diz Makefile icons.res *.inc *.pas *.rq *.bat
	$(RAR) $(RARFLAGS) $(DISTFILE).rar beispiel$(SEP)*.scr beispiel$(SEP)*.xps beispiel$(SEP)*.cfg
	$(RAR) $(RARFLAGS) $(DISTFILE).rar charsets$(SEP)*.inc
	$(RAR) $(RARFLAGS) $(DISTFILE).rar doc$(SEP)Makefile doc$(SEP)*.txt doc$(SEP)*.dq doc$(SEP)*.ihq doc$(SEP)xpoint.sgm doc$(SEP)xpoint.dsl doc$(SEP)xpoint.cat
	$(RAR) $(RARFLAGS) $(DISTFILE).rar linux$(SEP)*.inc
	$(RAR) $(RARFLAGS) $(DISTFILE).rar ObjCOM$(SEP)Makefile ObjCOM$(SEP)*.inc ObjCOM$(SEP)*.pas ObjCOM$(SEP)*.txt
endif

ifeq (,$(DOCDISTFILE))
docdist:
	$(error Variable "DOCDISTFILE" muss gesetzt werden)
else
docdist: fulldoc
	-$(RM) $(DOCDISTFILE).rar
	$(RAR) $(RARFLAGS) $(DOCDISTFILE).rar doc$(SEP)*.txt doc$(SEP)xpoint.sgm doc$(SEP)xpoint.htm doc$(SEP)xpoint.rtf doc$(SEP)xpoint.tex doc$(SEP)xpoint.dvi doc$(SEP)xpoint.ps doc$(SEP)xpoint.pdf
endif

TAGS:
	etags -l pascal *.inc *.pas charsets$(SEP)*.inc linux$(SEP)*.inc ObjCOM$(SEP)*.pas ObjCOM$(SEP)*.inc

install-strip: install

info:

dvi:
	$(MAKE) -C doc dvi

check: all

installcheck: install

#
# $Log$
# Revision 1.26  2000/10/23 17:15:39  fe
# FreeBSD-Anpassungen
#
# Revision 1.25  2000/10/19 19:39:44  fe
# Neue SETLINKS-Variable bestimmt, ob bei Linux bei "make install"
# Softlinks nach /usr/bin und /usr/lib gesetzt werden.
#
# Revision 1.24  2000/10/19 18:15:35  fe
# Fuer FPC SmartLinking und RangeChecking ausgestellt.
# Anpassungen der FPC-Linux-Aufrufparameter.
# SHELL-Variable wird nur noch bei Linux gesetzt.  (wegen Cygwin make)
#
# Revision 1.23  2000/10/17 15:01:18  fe
# dbform und readme.lnx heissen jetzt dbform.txt und linux.txt.
#
# Revision 1.22  2000/10/15 11:12:15  fe
# beispiel/glossary.cfg eingetragen.
#
# Revision 1.21  2000/10/12 20:44:07  fe
# Pfade geaendert, v.a. fuer Linux (/opt/openxp/).
#
# Revision 1.20  2000/10/11 21:51:50  fe
# Korrekturen fuer Virtual Pascal.
#
# Revision 1.19  2000/10/10 20:00:39  fe
# Restliche GNU-Targets ergaenzt.
#
# Revision 1.18  2000/10/10 17:47:34  fe
# Neue Dateien inkl. Abhaengigkeitsaenderungen ergaenzt.
# Ergaenzungen fuer Dokumentation.
#
# Revision 1.17  2000/10/09 22:11:22  fe
# Ergaenzungen fuer Handbuch.
# URL von oxpcontr.rar ergaenzt.
# Korrekturen und Ergaenzungen.
#
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

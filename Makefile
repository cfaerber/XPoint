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

# Bemerkungen:
#
# - Ist im Moment nur fuer Free Pascal <http://www.freepascal.org/>
#   angepasst.
# - Unter DOS, Windows und OS/2 muessen z.Z. noch rm, rmdir und mkdir
#   installiert werden.
# - Es wird ein Verzeichnis mit verschiedenen Dateien, die nicht im
#   OpenXP-Quellcode enthalten sind, benoetigt.
# - Unten muessen einige Variablen gesetzt werden.

# make             uebersetzt das ganze Programm
# make install     installiert das Programm
# make uninstall   deinstalliert das Programm
# make clean       raeumt das Verzeichnis auf
# make dist        erstellt Quellcodearchiv

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
#
COMPILER = fpc

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

ifneq ($(COMPILER),fpc)
$(error Variable "COMPILER" muss auf "fpc" gesetzt werden)
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

EXE_EXT = .exe
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
CONTRIBDATA = fido.pc xpicons.dll xpuu-d.res

endif

ifeq ($(OS),linux)

prefix ?= /usr/local/xp
bindir = $(prefix)
linkdir ?= /usr/bin
datadir = $(prefix)
exampledir = $(prefix)/beispiel

EXE_EXT =
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
CONTRIBDATA = fido.pc xpicons.dll

endif

ifeq ($(COMPILER),fpc)
PC = ppc386$(EXE_EXT)

ifeq ($(DEBUG),yes)
PFLAGS += -Ct -dDEBUG -Sg -pg -FuObjCOM
else
PFLAGS += -CX -O2 -Sg -FuObjCOM
endif

PF_dos32 = -TGO32V2
PF_os2 = -TOS2
PF_win = -TWIN32
PF_linux = -TLINUX
PF_386 = -Op1
PF_486 = -Op1
PF_586 = -Op2
PF_686 = -Op3

uuzext$(EXE_EXT): PFLAGS += -S2

endif

export PC
export OS
export CPU
export prefix
export COMPILER
export DEBUG
export contribdir
export RM

BIN = maggi ndiff pmconv uucp-fl1 uuzext xp xp-fm xpme yup2pkt zfido zpr
COMPBIN = $(BIN) docform ihs rc
RES = xp-d xp-e xpfm-d xpfm-e
EXAMPLES = gsbox.scr madness.scr magic.scr maus.scr o-magic.scr \
	oz-netz.scr pcsysop.scr privhead.xps qbrett.xps qpmpriv.xps \
	qpriv.xps quark.scr quoteto.xps uucp.scr z-netz.scr

BINFILES = $(patsubst %,%$(EXE_EXT),$(BIN))
COMPBINFILES = $(patsubst %,%$(EXE_EXT),$(COMPBIN))
INSTALLBINFILES = $(BINFILES) $(CONTRIBBIN)
RESFILES = $(patsubst %,%.res,$(RES))
DATAFILES = $(RESFILES) $(CONTRIBDATA)
CONTRIB = $(CONTRIBBIN) $(CONTRIBDATA)

PFLAGS += $(PF_$(OS))
PFLAGS += $(PF_$(CPU))

RARFLAGS = a -m5 -zfile_id.diz

all: $(COMPBINFILES) $(RESFILES) documentation

# Programme

docform$(EXE_EXT): docform.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
ihs$(EXE_EXT): ihs.pas fileio.o typeform.o xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
ihs$(EXE_EXT): ihs.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

maggi$(EXE_EXT): maggi.pas fileio.o montage.o typeform.o xp_iti.o xpdatum.o \
	xpdefine.inc xpglobal.o xpheader.inc xpmakehd.inc
	$(PC) $(PFLAGS) $<

ndiff$(EXE_EXT): ndiff.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

pmconv$(EXE_EXT): pmconv.pas typeform.o xpdatum.o xpdefine.inc \
	xpglobal.o xpmakehd.inc
	$(PC) $(PFLAGS) $<

rc$(EXE_EXT): rc.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

uucp-fl1$(EXE_EXT): uucp-fl1.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

uuzext$(EXE_EXT): uuzext.pas uuz.o xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp$(EXE_EXT): xp.pas archive.o clip.o crc.o database.o databaso.o datadef.o \
	eddef.o editor.o feiertag.o fileio.o help.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o mouse.o printerx.o \
	resource.o stack.o typeform.o uart.o uuz.o win2.o winxp.o \
	xp0.o xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2.o \
	xp2c.o xp2db.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o xp4.o \
	xp4e.o xp4o.o xp4o2.o xp4o3.o xp5.o xp6.o xp6o.o xp7.o xp7f.o \
	xp7o.o xp8.o xp9.o xp_des.o xp_iti.o xp_pgp.o xp_uue.o \
	xpauto.o xpcc.o xpcurses.o xpdatum.o xpdefine.inc xpdiff.o \
	xpe.o xpf2.o xpfido.o xpfidonl.o xpglobal.o xpimpexp.o \
	xpkeys.o xplinux.o xpmaus.o xpmime.o xpnt.o xpreg.o xpstat.o \
	xpterm.o xpuu.o xpview.o xpx.o
	$(PC) $(PFLAGS) $<
else
xp$(EXE_EXT): xp.pas archive.o clip.o crc.o database.o databaso.o datadef.o \
	eddef.o editor.o feiertag.o fileio.o help.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o mouse.o printerx.o \
	resource.o stack.o typeform.o uart.o uuz.o win2.o winxp.o \
	xp0.o xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2.o \
	xp2c.o xp2db.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o xp4.o \
	xp4e.o xp4o.o xp4o2.o xp4o3.o xp5.o xp6.o xp6o.o xp7.o xp7f.o \
	xp7o.o xp8.o xp9.o xp_des.o xp_iti.o xp_pgp.o xp_uue.o \
	xpauto.o xpcc.o xpdatum.o xpdefine.inc xpdiff.o xpe.o xpf2.o \
	xpfido.o xpfidonl.o xpglobal.o xpimpexp.o xpkeys.o xpmaus.o \
	xpmime.o xpnt.o xpreg.o xpstat.o xpterm.o xpuu.o xpview.o xpx.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp-fm$(EXE_EXT): xp-fm.pas crc.o debug.o fileio.o inout.o modem.o \
	montage.o resource.o timer.o typeform.o winxp.o \
	xp-fm.inc xpcurses.o xpdefine.inc xpdiff.o xpglobal.o zmodem.o
	$(PC) $(PFLAGS) $<
else
xp-fm$(EXE_EXT): xp-fm.pas crc.o debug.o fileio.o inout.o modem.o \
	montage.o resource.o timer.o typeform.o winxp.o \
	xp-fm.inc xpdefine.inc xpdiff.o xpglobal.o zmodem.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpme$(EXE_EXT): xpme.pas fileio.o inout.o keys.o maus2.o resource.o \
	typeform.o winxp.o xpcurses.o xpdefine.inc xpglobal.o xplinux.o \
	xpmecol.inc
	$(PC) $(PFLAGS) $<
else
xpme$(EXE_EXT): xpme.pas fileio.o inout.o keys.o maus2.o resource.o \
	typeform.o winxp.o xpdefine.inc xpglobal.o xpmecol.inc
	$(PC) $(PFLAGS) $<
endif

yup2pkt$(EXE_EXT): yup2pkt.pas dbase.o fileio.o typeform.o xpdefine.inc \
	xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
zfido$(EXE_EXT): zfido.pas fileio.o typeform.o xpcurses.o xpdatum.o \
	xpdefine.inc xpdiff.o xpglobal.o
	$(PC) $(PFLAGS) $<
else
zfido$(EXE_EXT): zfido.pas fileio.o typeform.o xpdatum.o xpdefine.inc \
	xpdiff.o xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
zpr$(EXE_EXT): zpr.pas dosx.o typeform.o xpcurses.o xpdefine.inc \
	xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
zpr$(EXE_EXT): zpr.pas dosx.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

# Units

archive.o: archive.pas montage.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
clip.o: clip.pas fileio.o xp0.o xpdefine.inc xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
clip.o: clip.pas fileio.o xp0.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

crc.o: crc.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
database.o: database.pas databas1.inc databas2.inc database.inc \
	datadef.o datadef1.o inout.o typeform.o xpdefine.inc \
	xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
database.o: database.pas databas1.inc databas2.inc database.inc \
	datadef.o datadef1.o inout.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
databaso.o: databaso.pas database.o datadef.o datadef1.o typeform.o \
	xpdefine.inc xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
databaso.o: databaso.pas database.o datadef.o datadef1.o typeform.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

datadef.o: datadef.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

datadef1.o: datadef1.pas datadef.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

dbase.o: dbase.pas typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

debug.o: debug.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
dosx.o: dosx.pas linux/dosx.inc linux/dosxh.inc xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
dosx.o: dosx.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

eddef.o: eddef.pas keys.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
editor.o: editor.pas clip.o eddef.o editor.inc encoder.o fileio.o \
	inout.o keys.o maus2.o mouse.o printerx.o typeform.o winxp.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
editor.o: editor.pas clip.o eddef.o editor.inc encoder.o fileio.o \
	inout.o keys.o maus2.o mouse.o printerx.o typeform.o winxp.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

encoder.o: encoder.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

exxec.o: exxec.pas debug.o fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

feiertag.o: feiertag.pas montage.o typeform.o xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
fileio.o: fileio.pas fileio.inc linux/fileio.inc linux/fileioh1.inc \
	typeform.o xp0.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
fileio.o: fileio.pas fileio.inc typeform.o xp0.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
help.o: help.pas fileio.o inout.o keys.o maus2.o mouse.o printerx.o \
	typeform.o winxp.o xpcurses.o xpdefine.inc xpglobal.o \
	xplinux.o
	$(PC) $(PFLAGS) $<
else
help.o: help.pas fileio.o inout.o keys.o maus2.o mouse.o printerx.o \
	typeform.o winxp.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
inout.o: inout.pas keys.o maus2.o mouse.o typeform.o winxp.o xp0.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
inout.o: inout.pas keys.o maus2.o mouse.o typeform.o winxp.o xp0.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ipaddr.o: ipaddr.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ipcclass.o: ipcclass.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
keys.o: keys.pas typeform.o xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
keys.o: keys.pas typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
lister.o: lister.pas fileio.o inout.o keys.o maus2.o typeform.o winxp.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
lister.o: lister.pas fileio.o inout.o keys.o maus2.o typeform.o winxp.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
maske.o: maske.pas clip.o inout.o keys.o maske.inc maus2.o montage.o \
	typeform.o winxp.o xp0.o xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
maske.o: maske.pas clip.o inout.o keys.o maske.inc maus2.o montage.o \
	typeform.o winxp.o xp0.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
maus2.o: maus2.pas inout.o keys.o mouse.o typeform.o winxp.o \
	xpcurses.o xpdefine.inc xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
maus2.o: maus2.pas inout.o keys.o mouse.o typeform.o winxp.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

modem.o: modem.pas debug.o timer.o typeform.o xpdefine.inc
	$(PC) $(PFLAGS) $<

montage.o: montage.pas typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

mouse.o: mouse.pas maus2.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ncnntp.o: ncnntp.pas ipcclass.o ncsocket.o netcall.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ncpop3.o: ncpop3.pas ipcclass.o ncsocket.o netcall.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ncsmtp.o: ncsmtp.pas ipcclass.o ncsocket.o netcall.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ncsocket.o: ncsocket.pas ipaddr.o netcall.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ncurses.o: ncurses.pas 
	$(PC) $(PFLAGS) $<

netcall.o: netcall.pas ipcclass.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
printerx.o: printerx.pas inout.o keys.o maus2.o typeform.o winxp.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
printerx.o: printerx.pas inout.o keys.o maus2.o typeform.o winxp.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

regexpr.o: regexpr.pas 
	$(PC) $(PFLAGS) $<

resource.o: resource.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

stack.o: stack.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

timer.o: timer.pas debug.o
	$(PC) $(PFLAGS) $<

typeform.o: typeform.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

uart.o: uart.pas dosx.o inout.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
uuz.o: uuz.pas fileio.o montage.o typeform.o xpcurses.o xpdatum.o \
	xpdefine.inc xpglobal.o xpheader.inc xplinux.o xpmakehd.inc
	$(PC) $(PFLAGS) $<
else
uuz.o: uuz.pas fileio.o montage.o typeform.o xpdatum.o xpdefine.inc \
	xpglobal.o xpheader.inc xpmakehd.inc
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
win2.o: win2.pas dosx.o fileio.o inout.o keys.o maus2.o typeform.o winxp.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
win2.o: win2.pas dosx.o fileio.o inout.o keys.o maus2.o typeform.o winxp.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
winxp.o: winxp.pas inout.o keys.o maus2.o typeform.o xp0.o xpcurses.o \
	xpdefine.inc xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
winxp.o: winxp.pas inout.o keys.o maus2.o typeform.o xp0.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xp0.o: xp0.pas keys.o typeform.o xpdefine.inc xpglobal.o xpheader.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),dos32)
xp1.o: xp1.pas clip.o crc.o database.o datadef.o debug.o dosx.o exxec.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o \
	montage.o mouse.o printerx.o resource.o typeform.o win2.o \
	winxp.o xp0.o xp1cm.inc xp1help.o xp1input.o xp1menu.inc \
	xp1o.o xp1o2.o xp1s.inc xp2.o xpdefine.inc xpdos32.o xpe.o \
	xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),linux)
xp1.o: xp1.pas clip.o crc.o database.o datadef.o debug.o dosx.o exxec.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o \
	montage.o mouse.o printerx.o resource.o typeform.o win2.o \
	winxp.o xp0.o xp1cm.inc xp1help.o xp1input.o xp1menu.inc \
	xp1o.o xp1o2.o xp1s.inc xp2.o xpcurses.o xpdefine.inc \
	xpe.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),os2)
xp1.o: xp1.pas clip.o crc.o database.o datadef.o debug.o dosx.o exxec.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o \
	montage.o mouse.o printerx.o resource.o typeform.o win2.o \
	winxp.o xp0.o xp1cm.inc xp1help.o xp1input.o xp1menu.inc \
	xp1o.o xp1o2.o xp1s.inc xp2.o xpdefine.inc xpe.o xpglobal.o \
	xpnt.o xpos2.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),win32)
xp1.o: xp1.pas clip.o crc.o database.o datadef.o debug.o dosx.o exxec.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o \
	montage.o mouse.o printerx.o resource.o typeform.o win2.o \
	winxp.o xp0.o xp1cm.inc xp1help.o xp1input.o xp1menu.inc \
	xp1o.o xp1o2.o xp1s.inc xp2.o xpdefine.inc xpe.o xpglobal.o \
	xpnt.o xpwin32.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp10.o: xp10.pas database.o datadef.o feiertag.o fileio.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.inc xp10p.inc xp1help.o \
	xp1input.o xp1o2.o xp2.o xp3.o xp3o.o xp4o.o xp4o2.o xp5.o \
	xp7.o xp9bp.o xpauto.o xpcurses.o xpdefine.inc xpfido.o \
	xpfidonl.o xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp10.o: xp10.pas database.o datadef.o feiertag.o fileio.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.inc xp10p.inc xp1help.o \
	xp1input.o xp1o2.o xp2.o xp3.o xp3o.o xp4o.o xp4o2.o xp5.o \
	xp7.o xp9bp.o xpauto.o xpdefine.inc xpfido.o xpfidonl.o \
	xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp1help.o: xp1help.pas help.o inout.o keys.o maske.o maus2.o printerx.o \
	resource.o typeform.o winxp.o xp0.o xp1.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp1help.o: xp1help.pas help.o inout.o keys.o maske.o maus2.o printerx.o \
	resource.o typeform.o winxp.o xp0.o xp1.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp1input.o: xp1input.pas inout.o keys.o maske.o maus2.o resource.o \
	typeform.o winxp.o xp0.o xp1.o xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp1input.o: xp1input.pas inout.o keys.o maske.o maus2.o resource.o \
	typeform.o winxp.o xp0.o xp1.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp1o.o: xp1o.pas archive.o clip.o crc.o database.o datadef.o dosx.o \
	fileio.o inout.o keys.o lister.o maske.o maus2.o printerx.o \
	resource.o typeform.o xp0.o xp1.o xp10.o xp1input.o xp1o2.o \
	xp4.o xp4o.o xp_uue.o xpcurses.o xpdefine.inc xpglobal.o \
	xpkeys.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp1o.o: xp1o.pas archive.o clip.o crc.o database.o datadef.o dosx.o \
	fileio.o inout.o keys.o lister.o maske.o maus2.o printerx.o \
	resource.o typeform.o xp0.o xp1.o xp10.o xp1input.o xp1o2.o \
	xp4.o xp4o.o xp_uue.o xpdefine.inc xpglobal.o xpkeys.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp1o2.o: xp1o2.pas database.o datadef.o fileio.o inout.o keys.o maus2.o \
	resource.o stack.o typeform.o xp0.o xp1.o xp1input.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp1o2.o: xp1o2.pas database.o datadef.o fileio.o inout.o keys.o maus2.o \
	resource.o stack.o typeform.o xp0.o xp1.o xp1input.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),dos32)
xp2.o: xp2.pas clip.o crc.o database.o databaso.o datadef.o dosx.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o montage.o \
	mouse.o printerx.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2cfg.inc \
	xp3.o xp5.o xp9.o xp9bp.o xpcfg.o xpdatum.o xpdefine.inc \
	xpdos32.o xpe.o xpeasy.o xpfido.o xpglobal.o xpkeys.o xpnt.o \
	xpreg.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),linux)
xp2.o: xp2.pas clip.o crc.o database.o databaso.o datadef.o dosx.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o montage.o \
	mouse.o printerx.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2cfg.inc \
	xp3.o xp5.o xp9.o xp9bp.o xpcfg.o xpcurses.o xpdatum.o \
	xpdefine.inc xpe.o xpeasy.o xpfido.o xpglobal.o xpkeys.o \
	xplinux.o xpnt.o xpreg.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),os2)
xp2.o: xp2.pas clip.o crc.o database.o databaso.o datadef.o dosx.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o montage.o \
	mouse.o printerx.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2cfg.inc \
	xp3.o xp5.o xp9.o xp9bp.o xpcfg.o xpdatum.o xpdefine.inc \
	xpe.o xpeasy.o xpfido.o xpglobal.o xpkeys.o xpnt.o xpos2.o \
	xpreg.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),win32)
xp2.o: xp2.pas clip.o crc.o database.o databaso.o datadef.o dosx.o \
	fileio.o help.o inout.o keys.o lister.o maske.o maus2.o montage.o \
	mouse.o printerx.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1help.o xp1input.o xp1o.o xp1o2.o xp2cfg.inc \
	xp3.o xp5.o xp9.o xp9bp.o xpcfg.o xpdatum.o xpdefine.inc \
	xpe.o xpeasy.o xpfido.o xpglobal.o xpkeys.o xpnt.o xpreg.o \
	xpwin32.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp2c.o: xp2c.pas database.o datadef.o editor.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o mouse.o printerx.o resource.o \
	typeform.o uart.o win2.o winxp.o xp0.o xp1.o xp1input.o xp1o.o \
	xp2.o xp4o2.o xp9bp.o xpcurses.o xpdatum.o xpdefine.inc \
	xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp2c.o: xp2c.pas database.o datadef.o editor.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o mouse.o printerx.o resource.o \
	typeform.o uart.o win2.o winxp.o xp0.o xp1.o xp1input.o xp1o.o \
	xp2.o xp4o2.o xp9bp.o xpdatum.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp2db.o: xp2db.pas database.o databaso.o datadef.o datadef1.o fileio.o \
	inout.o keys.o maus2.o resource.o typeform.o winxp.o xp0.o \
	xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3o.o xp4o2.o xp5.o \
	xp9bp.o xpcurses.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp2db.o: xp2db.pas database.o databaso.o datadef.o datadef1.o fileio.o \
	inout.o keys.o maus2.o resource.o typeform.o winxp.o xp0.o \
	xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3o.o xp4o2.o xp5.o \
	xp9bp.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp2f.o: xp2f.pas inout.o keys.o maske.o maus2.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1help.o xp1input.o xp2.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp2f.o: xp2f.pas inout.o keys.o maske.o maus2.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1help.o xp1input.o xp2.o xpdefine.inc \
	xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp3.o: xp3.pas database.o datadef.o fileio.o inout.o montage.o resource.o \
	typeform.o xp0.o xp1.o xp1input.o xp3ex.o xp3o.o xp_des.o \
	xp_pgp.o xpcurses.o xpdatum.o xpdefine.inc xpglobal.o \
	xpmakehd.inc xpnt.o
	$(PC) $(PFLAGS) $<
else
xp3.o: xp3.pas database.o datadef.o fileio.o inout.o montage.o resource.o \
	typeform.o xp0.o xp1.o xp1input.o xp3ex.o xp3o.o xp_des.o \
	xp_pgp.o xpdatum.o xpdefine.inc xpglobal.o xpmakehd.inc xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp3ex.o: xp3ex.pas database.o fileio.o inout.o resource.o stack.o \
	typeform.o xp0.o xp1.o xp1o.o xp3.o xp_des.o xpcurses.o \
	xpdefine.inc xpfido.o xpglobal.o xpmime.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp3ex.o: xp3ex.pas database.o fileio.o inout.o resource.o stack.o \
	typeform.o xp0.o xp1.o xp1o.o xp3.o xp_des.o xpdefine.inc \
	xpfido.o xpglobal.o xpmime.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp3o.o: xp3o.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o montage.o printerx.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3ex.o \
	xp3o.inc xp3o2.o xp4.o xp4o.o xp6.o xp8.o xp9bp.o xp_pgp.o \
	xpcurses.o xpdatum.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp3o.o: xp3o.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o montage.o printerx.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3ex.o \
	xp3o.inc xp3o2.o xp4.o xp4o.o xp6.o xp8.o xp9bp.o xp_pgp.o \
	xpdatum.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

xp3o2.o: xp3o2.pas database.o datadef.o resource.o typeform.o xp0.o xp1.o \
	xp3.o xp3o.o xp6.o xp_pgp.o xpdatum.o xpdefine.inc xpglobal.o \
	xpnt.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),dos32)
xp4.o: xp4.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.o xp1help.o xp1input.o \
	xp1o.o xp2.o xp2c.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o \
	xp4.inc xp4d.inc xp4e.o xp4o.o xp4o2.o xp4o3.o xp4w.inc xp5.o \
	xp6.o xp6o.o xp7.o xp8.o xp9.o xp_pgp.o xp_uue.o xpauto.o \
	xpcc.o xpdefine.inc xpdos32.o xpe.o xpfido.o xpfidonl.o \
	xpglobal.o xpimpexp.o xpkeys.o xpmaus.o xpmime.o xpnt.o \
	xpreg.o xpstat.o xpterm.o xpview.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),linux)
xp4.o: xp4.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.o xp1help.o xp1input.o \
	xp1o.o xp2.o xp2c.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o \
	xp4.inc xp4d.inc xp4e.o xp4o.o xp4o2.o xp4o3.o xp4w.inc xp5.o \
	xp6.o xp6o.o xp7.o xp8.o xp9.o xp_pgp.o xp_uue.o xpauto.o \
	xpcc.o xpcurses.o xpdefine.inc xpe.o xpfido.o xpfidonl.o \
	xpglobal.o xpimpexp.o xpkeys.o xpmaus.o xpmime.o xpnt.o \
	xpreg.o xpstat.o xpterm.o xpview.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),os2)
xp4.o: xp4.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.o xp1help.o xp1input.o \
	xp1o.o xp2.o xp2c.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o \
	xp4.inc xp4d.inc xp4e.o xp4o.o xp4o2.o xp4o3.o xp4w.inc xp5.o \
	xp6.o xp6o.o xp7.o xp8.o xp9.o xp_pgp.o xp_uue.o xpauto.o \
	xpcc.o xpdefine.inc xpe.o xpfido.o xpfidonl.o xpglobal.o \
	xpimpexp.o xpkeys.o xpmaus.o xpmime.o xpnt.o xpos2.o xpreg.o \
	xpstat.o xpterm.o xpview.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),win32)
xp4.o: xp4.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp10.o xp1help.o xp1input.o \
	xp1o.o xp2.o xp2c.o xp2f.o xp3.o xp3ex.o xp3o.o xp3o2.o \
	xp4.inc xp4d.inc xp4e.o xp4o.o xp4o2.o xp4o3.o xp4w.inc xp5.o \
	xp6.o xp6o.o xp7.o xp8.o xp9.o xp_pgp.o xp_uue.o xpauto.o \
	xpcc.o xpdefine.inc xpe.o xpfido.o xpfidonl.o xpglobal.o \
	xpimpexp.o xpkeys.o xpmaus.o xpmime.o xpnt.o xpreg.o xpstat.o \
	xpterm.o xpview.o xpwin32.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp4e.o: xp4e.pas database.o datadef.o dosx.o fileio.o inout.o keys.o \
	maske.o maus2.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp1input.o xp1o.o xp1o2.o xp2.o xp3.o xp3o.o xp3o2.o \
	xp4.o xp6.o xp9.o xp9bp.o xpauto.o xpcc.o xpcurses.o \
	xpdefine.inc xpfido.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp4e.o: xp4e.pas database.o datadef.o dosx.o fileio.o inout.o keys.o \
	maske.o maus2.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp1input.o xp1o.o xp1o2.o xp2.o xp3.o xp3o.o xp3o2.o \
	xp4.o xp6.o xp9.o xp9bp.o xpauto.o xpcc.o xpdefine.inc \
	xpfido.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp4o.o: xp4o.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o printerx.o \
	resource.o typeform.o winxp.o xp0.o xp1.o xp1help.o xp1input.o \
	xp1o.o xp1o2.o xp3.o xp3ex.o xp3o.o xp3o2.o xp4.o xp4o.inc \
	xp_pgp.o xpcurses.o xpdefine.inc xpfido.o xpglobal.o xpkeys.o \
	xpmaus.o xpnt.o xpview.o
	$(PC) $(PFLAGS) $<
else
xp4o.o: xp4o.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o printerx.o \
	resource.o typeform.o winxp.o xp0.o xp1.o xp1help.o xp1input.o \
	xp1o.o xp1o2.o xp3.o xp3ex.o xp3o.o xp3o2.o xp4.o xp4o.inc \
	xp_pgp.o xpdefine.inc xpfido.o xpglobal.o xpkeys.o xpmaus.o \
	xpnt.o xpview.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp4o2.o: xp4o2.pas crc.o database.o databaso.o datadef.o fileio.o help.o \
	inout.o keys.o maus2.o resource.o typeform.o xp0.o xp1.o \
	xp1input.o xp1o.o xp3.o xp3ex.o xp3o.o xpcurses.o xpdefine.inc \
	xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp4o2.o: xp4o2.pas crc.o database.o databaso.o datadef.o fileio.o help.o \
	inout.o keys.o maus2.o resource.o typeform.o xp0.o xp1.o \
	xp1input.o xp1o.o xp3.o xp3ex.o xp3o.o xpdefine.inc xpglobal.o \
	xpnt.o
	$(PC) $(PFLAGS) $<
endif

xp4o3.o: xp4o3.pas database.o datadef.o fileio.o inout.o keys.o resource.o \
	typeform.o xp0.o xp1.o xp1input.o xp3.o xp3ex.o xp4.o xp6.o \
	xpcc.o xpdefine.inc xpglobal.o xpkeys.o xpnt.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp5.o: xp5.pas clip.o database.o datadef.o feiertag.o fileio.o inout.o \
	keys.o maske.o maus2.o montage.o resource.o typeform.o winxp.o \
	xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xpcurses.o xpdefine.inc \
	xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
xp5.o: xp5.pas clip.o database.o datadef.o feiertag.o fileio.o inout.o \
	keys.o maske.o maus2.o montage.o resource.o typeform.o winxp.o \
	xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp6.o: xp6.pas crc.o database.o datadef.o fileio.o inout.o keys.o
	lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2c.o xp3.o \
	xp3ex.o xp3o.o xp3o2.o xp4e.o xp6l.o xp6s.inc xp9.o xp9bp.o \
	xp_des.o xp_pgp.o xpcc.o xpcurses.o xpdefine.inc xpe.o xpfido.o \
	xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp6.o: xp6.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2c.o xp3.o \
	xp3ex.o xp3o.o xp3o2.o xp4e.o xp6l.o xp6s.inc xp9.o xp9bp.o \
	xp_des.o xp_pgp.o xpcc.o xpdefine.inc xpe.o xpfido.o xpglobal.o \
	xpnt.o
	$(PC) $(PFLAGS) $<
endif

xp6l.o: xp6l.pas xp0.o xpcc.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp6o.o: xp6o.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2c.o xp3.o \
	xp3ex.o xp3o.o xp3o2.o xp4.o xp4e.o xp6.o xp6l.o xp_des.o \
	xpcurses.o xpdefine.inc xpe.o xpfido.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp6o.o: xp6o.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	lister.o maske.o maus2.o montage.o resource.o stack.o typeform.o \
	winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2c.o xp3.o xp3ex.o \
	xp3o.o xp3o2.o xp4.o xp4e.o xp6.o xp6l.o xp_des.o xpdefine.inc \
	xpe.o xpfido.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp7.o: xp7.pas database.o datadef.o debug.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o uart.o uuz.o winxp.o xp0.o xp1.o xp10.o xp1help.o \
	xp1input.o xp1o.o xp2c.o xp3.o xp3o.o xp4o.o xp4o2.o xp5.o \
	xp7.inc xp7f.o xp7l.o xp7o.o xp7u.inc xp8.o xp9.o xp9bp.o \
	xpcurses.o xpdefine.inc xpdiff.o xpfido.o xpfidonl.o \
	xpglobal.o xpmaus.o xpnt.o xpterm.o xpuu.o
	$(PC) $(PFLAGS) $<
else
xp7.o: xp7.pas database.o datadef.o debug.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o uart.o uuz.o winxp.o xp0.o xp1.o xp10.o xp1help.o \
	xp1input.o xp1o.o xp2c.o xp3.o xp3o.o xp4o.o xp4o2.o xp5.o \
	xp7.inc xp7f.o xp7l.o xp7o.o xp7u.inc xp8.o xp9.o xp9bp.o \
	xpdefine.inc xpdiff.o xpfido.o xpfidonl.o xpglobal.o xpmaus.o \
	xpnt.o xpterm.o xpuu.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp7f.o: xp7f.pas debug.o dosx.o fileio.o inout.o keys.o lister.o maske.o \
	maus2.o montage.o resource.o typeform.o xp0.o xp1.o xp1input.o \
	xp3.o xp3o.o xp7.o xp7l.o xp7o.o xpcurses.o xpdefine.inc \
	xpdiff.o xpf2.o xpfido.o xpfidonl.o xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp7f.o: xp7f.pas debug.o dosx.o fileio.o inout.o keys.o lister.o maske.o \
	maus2.o montage.o resource.o typeform.o xp0.o xp1.o xp1input.o \
	xp3.o xp3o.o xp7.o xp7l.o xp7o.o xpdefine.inc xpdiff.o xpf2.o \
	xpfido.o xpfidonl.o xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xp7l.o: xp7l.pas typeform.o xp0.o xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp7o.o: xp7o.pas archive.o database.o datadef.o debug.o fileio.o inout.o \
	maus2.o resource.o typeform.o uart.o winxp.o xp0.o xp1.o xp10.o \
	xp1o.o xp3.o xp3ex.o xp3o.o xp3o2.o xp6.o xp7.o xp7l.o xp9bp.o \
	xp_iti.o xpcurses.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xp7o.o: xp7o.pas archive.o database.o datadef.o debug.o fileio.o inout.o \
	maus2.o resource.o typeform.o uart.o winxp.o xp0.o xp1.o xp10.o \
	xp1o.o xp3.o xp3ex.o xp3o.o xp3o2.o xp6.o xp7.o xp7l.o xp9bp.o \
	xp_iti.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp8.o: xp8.pas database.o datadef.o fileio.o inout.o keys.o lister.o \
	maske.o maus2.o resource.o typeform.o win2.o xp0.o xp1.o \
	xp1help.o xp1input.o xp1o.o xp1o2.o xp2c.o xp3.o xp3ex.o \
	xp3o2.o xp4.o xp6.o xp6o.o xp8.inc xp8fs.inc xp9.o xp9bp.o \
	xp_iti.o xpcurses.o xpdefine.inc xpglobal.o xpnntp.o xpnt.o
	$(PC) $(PFLAGS) $<
endif
ifeq ($(OS),dos32)
xp8.o: xp8.pas database.o datadef.o fileio.o inout.o keys.o lister.o \
	maske.o maus2.o resource.o typeform.o win2.o xp0.o xp1.o \
	xp1help.o xp1input.o xp1o.o xp1o2.o xp2c.o xp3.o xp3ex.o \
	xp3o2.o xp4.o xp6.o xp6o.o xp8.inc xp8fs.inc xp9.o xp9bp.o \
	xp_iti.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif
ifneq (,$(findstring $(OS),os2 win32))
xp8.o: xp8.pas database.o datadef.o fileio.o inout.o keys.o lister.o \
	maske.o maus2.o resource.o typeform.o win2.o xp0.o xp1.o \
	xp1help.o xp1input.o xp1o.o xp1o2.o xp2c.o xp3.o xp3ex.o \
	xp3o2.o xp4.o xp6.o xp6o.o xp8.inc xp8fs.inc xp9.o xp9bp.o \
	xp_iti.o xpdefine.inc xpglobal.o xpnntp.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xp9.o: xp9.pas database.o datadef.o fileio.o inout.o keys.o maske.o \
	maus2.o mouse.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1input.o xp1o.o xp1o2.o xp2.o xp2c.o xp3.o \
	xp3o.o xp9.inc xp9bp.o xpcurses.o xpdefine.inc xpglobal.o \
	xplinux.o xpnt.o xpterm.o
	$(PC) $(PFLAGS) $<
else
xp9.o: xp9.pas database.o datadef.o fileio.o inout.o keys.o maske.o \
	maus2.o mouse.o resource.o typeform.o win2.o winxp.o xp0.o \
	xp1.o xp10.o xp1input.o xp1o.o xp1o2.o xp2.o xp2c.o xp3.o \
	xp3o.o xp9.inc xp9bp.o xpdefine.inc xpglobal.o xpnt.o xpterm.o
	$(PC) $(PFLAGS) $<
endif

xp9bp.o: xp9bp.pas database.o datadef.o fileio.o typeform.o xp0.o \
	xp1.o xp2.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp_des.o: xp_des.pas fileio.o inout.o maus2.o xp0.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp_des.o: xp_des.pas fileio.o inout.o maus2.o xp0.o xpdefine.inc \
	xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xp_iti.o: xp_iti.pas fileio.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

xp_pgp.o: xp_pgp.pas database.o fileio.o maske.o resource.o typeform.o \
	xp0.o xp1.o xp3.o xp3ex.o xp3o.o xp3o2.o xp6.o xpcc.o \
	xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xp_uue.o: xp_uue.pas database.o fileio.o inout.o maus2.o resource.o \
	typeform.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3ex.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xp_uue.o: xp_uue.pas database.o fileio.o inout.o maus2.o resource.o \
	typeform.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp3.o xp3ex.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xpauto.o: xpauto.pas database.o datadef.o dosx.o fileio.o inout.o \
	montage.o resource.o typeform.o xp0.o xp1.o xp1o.o xp3.o \
	xp3o.o xp6.o xp9bp.o xpdefine.inc xpglobal.o xpmaus.o xpnt.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpcc.o: xpcc.pas database.o datadef.o fileio.o inout.o maske.o resource.o \
	stack.o typeform.o winxp.o xp0.o xp1.o xp1input.o xp3.o xp3o.o \
	xp3o2.o xp4e.o xpcurses.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xpcc.o: xpcc.pas database.o datadef.o fileio.o inout.o maske.o resource.o \
	stack.o typeform.o winxp.o xp0.o xp1.o xp1input.o xp3.o xp3o.o \
	xp3o2.o xp4e.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpcfg.o: xpcfg.pas fileio.o resource.o typeform.o xpdefine.inc \
	xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
xpcfg.o: xpcfg.pas fileio.o resource.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xpcurses.o: xpcurses.pas inout.o ncurses.o typeform.o xpdefine.inc \
	xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<

xpdatum.o: xpdatum.pas montage.o typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

xpdiff.o: xpdiff.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

xpdos32.o: xpdos32.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpe.o: xpe.pas dosx.o eddef.o editor.o fileio.o inout.o keys.o maske.o \
	maus2.o resource.o typeform.o winxp.o xp0.o xp1.o xp10.o \
	xp1help.o xp1input.o xp1o.o xp5.o xp6.o xpcurses.o \
	xpdefine.inc xpglobal.o xpkeys.o
	$(PC) $(PFLAGS) $<
else
xpe.o: xpe.pas dosx.o eddef.o editor.o fileio.o inout.o keys.o maske.o \
	maus2.o resource.o typeform.o winxp.o xp0.o xp1.o xp10.o \
	xp1help.o xp1input.o xp1o.o xp5.o xp6.o xpdefine.inc \
	xpglobal.o xpkeys.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpeasy.o: xpeasy.pas database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o mouse.o resource.o typeform.o win2.o winxp.o \
	xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp2c.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xpeasy.o: xpeasy.pas database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o mouse.o resource.o typeform.o win2.o winxp.o \
	xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp2c.o xpdefine.inc \
	xpglobal.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpf2.o: xpf2.pas archive.o fileio.o montage.o typeform.o xp0.o xp1.o \
	xp1o.o xp3.o xp3o.o xp3o2.o xpcurses.o xpdefine.inc xpglobal.o \
	xpnt.o
	$(PC) $(PFLAGS) $<
else
xpf2.o: xpf2.pas archive.o fileio.o montage.o typeform.o xp0.o xp1.o \
	xp1o.o xp3.o xp3o.o xp3o2.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpfido.o: xpfido.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2.o xp3.o \
	xp4e.o xpcurses.o xpdefine.inc xpf1.inc xpfidonl.o xpglobal.o \
	xpnt.o
	$(PC) $(PFLAGS) $<
else
xpfido.o: xpfido.pas archive.o database.o datadef.o dosx.o fileio.o inout.o \
	keys.o lister.o maske.o maus2.o montage.o resource.o stack.o \
	typeform.o winxp.o xp0.o xp1.o xp1input.o xp1o.o xp2.o xp3.o \
	xp4e.o xpdefine.inc xpf1.inc xpfidonl.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

xpfidonl.o: xpfidonl.pas archive.o fileio.o maske.o montage.o resource.o \
	typeform.o xp0.o xp1.o xp1o.o xp3.o xpdefine.inc xpfido.o \
	xpglobal.o
	$(PC) $(PFLAGS) $<

xpftnadr.o: xpftnadr.pas xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

xpglobal.o: xpglobal.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpimpexp.o: xpimpexp.pas database.o datadef.o fileio.o inout.o maske.o \
	maus2.o resource.o typeform.o winxp.o xp0.o xp1.o xp1o.o \
	xp1o2.o xp3.o xp3o.o xp3o2.o xp9.o xp9bp.o xpcurses.o \
	xpdefine.inc xpglobal.o xplinux.o xpmaus.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xpimpexp.o: xpimpexp.pas database.o datadef.o fileio.o inout.o maske.o \
	maus2.o resource.o typeform.o winxp.o xp0.o xp1.o xp1o.o \
	xp1o2.o xp3.o xp3o.o xp3o2.o xp9.o xp9bp.o xpdefine.inc \
	xpglobal.o xpmaus.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpipc.o: xpipc.pas ipcclass.o maus2.o typeform.o winxp.o xp0.o \
	xpcurses.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xpipc.o: xpipc.pas ipcclass.o maus2.o typeform.o winxp.o xp0.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xpkeys.o: xpkeys.pas fileio.o inout.o keys.o resource.o typeform.o \
	xp0.o xp1.o xp4o.o xp7.o xp9.o xpauto.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

xplinux.o: xplinux.pas typeform.o xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpmaus.o: xpmaus.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o stack.o typeform.o winxp.o xp0.o xp1.o \
	xp1input.o xp1o.o xp3.o xp3o2.o xp6.o xp6o.o xp9.o xp_iti.o \
	xpcurses.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xpmaus.o: xpmaus.pas crc.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o stack.o typeform.o winxp.o xp0.o xp1.o \
	xp1input.o xp1o.o xp3.o xp3o2.o xp6.o xp6o.o xp9.o xp_iti.o \
	xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

xpmime.o: xpmime.pas database.o fileio.o keys.o lister.o montage.o \
	resource.o typeform.o xp0.o xp1.o xp1o.o xp3.o xp3ex.o xp3o.o \
	xpdefine.inc xpglobal.o xpkeys.o
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpnntp.o: xpnntp.pas inout.o ipaddr.o ipcclass.o maus2.o ncnntp.o \
	ncsocket.o netcall.o resource.o winxp.o xp0.o xp1.o xp1input.o \
	xpcurses.o xpdefine.inc xpglobal.o xpipc.o
	$(PC) $(PFLAGS) $<
else
xpnntp.o: xpnntp.pas inout.o ipaddr.o ipcclass.o maus2.o ncnntp.o \
	ncsocket.o netcall.o resource.o winxp.o xp0.o xp1.o xp1input.o \
	xpdefine.inc xpglobal.o xpipc.o
	$(PC) $(PFLAGS) $<
endif

xpnodes.o: xpnodes.pas typeform.o xpdefine.inc
	$(PC) $(PFLAGS) $<

xpnt.o: xpnt.pas crc.o database.o datadef.o typeform.o xp0.o xpdefine.inc
	$(PC) $(PFLAGS) $<

xpos2.o: xpos2.pas xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpreg.o: xpreg.pas clip.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o montage.o printerx.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp6.o xp9bp.o \
	xpauto.o xpcurses.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xpreg.o: xpreg.pas clip.o database.o datadef.o fileio.o inout.o keys.o \
	maske.o maus2.o montage.o printerx.o resource.o typeform.o \
	winxp.o xp0.o xp1.o xp1input.o xp1o.o xp1o2.o xp6.o xp9bp.o \
	xpauto.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpstat.o: xpstat.pas database.o datadef.o fileio.o inout.o keys.o lister.o \
	maske.o maus2.o montage.o resource.o typeform.o winxp.o xp0.o \
	xp1.o xp2.o xp3.o xp3o.o xp3o2.o xp6.o xp9.o xp9bp.o \
	xpcurses.o xpdefine.inc xpfidonl.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
else
xpstat.o: xpstat.pas database.o datadef.o fileio.o inout.o keys.o lister.o \
	maske.o maus2.o montage.o resource.o typeform.o winxp.o xp0.o \
	xp1.o xp2.o xp3.o xp3o.o xp3o2.o xp6.o xp9.o xp9bp.o \
	xpdefine.inc xpfidonl.o xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpterm.o: xpterm.pas database.o datadef.o fileio.o inout.o keys.o \
	maus2.o resource.o typeform.o uart.o winxp.o xp0.o xp1.o \
	xp10.o xp1input.o xp1o.o xp1o2.o xp2.o xp2c.o xp9bp.o \
	xpcurses.o xpdefine.inc xpglobal.o xpkeys.o
	$(PC) $(PFLAGS) $<
else
xpterm.o: xpterm.pas database.o datadef.o fileio.o inout.o keys.o \
	maus2.o resource.o typeform.o uart.o winxp.o xp0.o xp1.o \
	xp10.o xp1input.o xp1o.o xp1o2.o xp2.o xp2c.o xp9bp.o \
	xpdefine.inc xpglobal.o xpkeys.o
	$(PC) $(PFLAGS) $<
endif

ifeq ($(OS),linux)
xpuu.o: xpuu.pas fileio.o resource.o typeform.o xp0.o xp1.o xpcurses.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
else
xpuu.o: xpuu.pas fileio.o resource.o typeform.o xp0.o xp1.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

xpview.o: xpview.pas database.o dosx.o fileio.o inout.o typeform.o \
	xp0.o xp1.o xp1o.o xpdefine.inc xpglobal.o xpnt.o
	$(PC) $(PFLAGS) $<

xpwin32.o: xpwin32.pas typeform.o winxp.o xpdefine.inc
	$(PC) $(PFLAGS) $<

ifeq ($(OS),linux)
xpx.o: xpx.pas crc.o dosx.o fileio.o inout.o mouse.o typeform.o xp0.o \
	xpcurses.o xpdefine.inc xpglobal.o xplinux.o
	$(PC) $(PFLAGS) $<
else
xpx.o: xpx.pas crc.o dosx.o fileio.o inout.o mouse.o typeform.o xp0.o \
	xpdefine.inc xpglobal.o
	$(PC) $(PFLAGS) $<
endif

zmodem.o: zmodem.pas crc.o debug.o timer.o
	$(PC) $(PFLAGS) $<

# Sprachmodule

$(RESFILES): %.res: %.rq
	$(RC) $<

# Dokumentation

documentation:
	$(MAKE) -C doc

# Installation

install: install_bindir $(patsubst %,install_%,$(BINFILES)) \
	install_datadir $(patsubst %,install_%,$(RESFILES)) \
	install_exampledir $(patsubst %,install_%,$(EXAMPLES)) \
	$(patsubst %,install_%,$(CONTRIB))
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

clean: $(patsubst %,clean_%,$(COMPBINFILES))
	-$(RM) *.res
	-$(RM) *.o
	-$(RM) *.a
	-$(RM) *.ppu
	-$(RM) $(DISTFILE)
	$(MAKE) -C ObjCOM clean
	$(MAKE) -C doc clean

$(patsubst %,clean_%,$(COMPBINFILES)): clean_%:
	-$(RM) $*

# Erstellen eines Quellcodearchivs

# Etwas umstaendlich wegen der maximalen Zeilenlaenge von COMMAND.COM.

dist:
	-$(RM) $(DISTFILE)
	$(RAR) $(RARFLAGS) $(DISTFILE) file_id.diz Makefile *.inc \
	*.pas *.rq *.bat
	$(RAR) $(RARFLAGS) $(DISTFILE) beispiel$(SEP)*.scr \
	beispiel$(SEP)*.xps
	$(RAR) $(RARFLAGS) $(DISTFILE) doc$(SEP)Makefile doc$(SEP)*.txt \
	doc$(SEP)*.dq doc$(SEP)*.ihq doc$(SEP)xpoint.xml \
	doc$(SEP)xpoint.dsl doc$(SEP)dbform doc$(SEP)readme.lnx 
	$(RAR) $(RARFLAGS) $(DISTFILE) linux$(SEP)*.inc
	$(RAR) $(RARFLAGS) $(DISTFILE) ObjCOM$(SEP)Makefile \
	ObjCOM$(SEP)*.inc ObjCOM$(SEP)*.pas ObjCOM$(SEP)*.txt

#
# $Log$
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

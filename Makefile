#
# OpenXP Makefile
#
# $Id$
#
# Funktioniert nur mit Borland Make oder unter Linux

# HINWEIS: Kopieren Sie zunaechst eine der gewuenschten Dateien
#          Makefile.dos, Makefile.lnx, Makefile.os2 oder Makefile.win
#          nach Makefile.inc
#
#	   Nehmen Sie nach Moeglichkeit in dieser Datei keine Aenderungen
#          vor, sondern nur in der Makefile.inc.

include Makefile.inc

ifdef INLINUX
EXE_EXT=
MAKE=make
MOVE=cp
COPY=cp
REMOVE=rm
MKDIR=install -m 755 -d
RMDIR=rmdir
ECHO=echo
INSTALL_EXE=install -b -V t -s -p -m 755
INSTALL_DAT=install -b -V existing -p -m 644
INSTALL_DIR=/usr/local/xp/
BIN_DIR=/usr/bin/
LINK=ln -s
TEMP_RC=./rc
else
EXE_EXT=.exe
MAKE=make.exe
COPY=copy
MOVE=move
REMOVE=del
MKDIR=mkdir
RMDIR=rmdir
ECHO=echo
INSTALL_EXE=$(MOVE)
INSTALL_DAT=$(MOVE)
INSTALL_DIR=bin/
BIN_DIR=bin/
LINK=REM
TEMP_RC=RC.EXE
endif

ifdef FPC
CC=ppc386$(EXE_EXT)

CF_DEBUG=-Ct -dDEBUG -Sg -pg
CF_RELEASE=-CX -O2 -Sg
CF_DOS=-TGO32V2
CF_LINUX=-TLINUX
CF_OS2=-TOS2
CF_WIN=-TWIN32
CF_386=-Op1
CF_586=-Op2
CF_686=-Op3

else
ifdef VPC
CC=vpc$(EXE_EXT)

CF_DEBUG=
CF_RELEASE=
CF_DOS=
CF_LINUX=
CF_OS2=
CF_WIN=
CF_386=
CF_586=
CF_686=


endif
endif

RC=rc$(EXE_EXT)
XP=xp$(EXE_EXT)

flags=$(CF_$(VERSION)) $(CF_$(OS))

openxp: $(XP)

$(XP):	xp.pas res
	$(CC) $(flags) xp.pas

$(RC):	rc.pas
	$(CC) $(flags) rc.pas

res: 	xp-d.rq xp-e.rq $(RC)
	$(TEMP_RC) xp-d.rq
	$(TEMP_RC) xp-e.rq

clean:
	-$(REMOVE) *.res
	-$(REMOVE) *.o
	-$(REMOVE) *.ow
	-$(REMOVE) *.ppu
	-$(REMOVE) *.ppw
	-$(REMOVE) $(XP)
	-$(REMOVE) $(RC)

dist-clean:
	-$(MAKE) clean
	-$(REMOVE) $(INSTALL_DIR)$(XP)
	-$(REMOVE) $(INSTALL_DIR)*.res

install: $(XP)
	-$(MKDIR) $(INSTALL_DIR)
	$(INSTALL_EXE) $(XP) $(INSTALL_DIR)
	-$(LINK) $(INSTALL_DIR)$(XP) $(BIN_DIR)$(XP)
	$(INSTALL_DAT) xp-d.res $(INSTALL_DIR)
	$(INSTALL_DAT) xp-e.res $(INSTALL_DIR)

#
# $Log$
# Revision 1.3  2000/08/07 10:23:56  hd
# - Installation unter Linux angepasst
#
# Revision 1.2  2000/08/07 10:08:23  hd
# - Makefile angepasst
#
# Revision 1.1  2000/08/06 20:10:32  mk
# - erster Versuch eines Makefiles
#
#

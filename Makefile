#
# OpenXP Makefile
#
# $Id$
#
# Funktioniert nur mit Borland Make

flags=-Sg

openxp: xp 

xp:	xp.exe res

xp.exe:	xp.pas
	ppc386.exe $(flags) xp.pas

rc.exe:	rc.pas
	ppc386.exe $(flags) rc.pas

res: 	xp-d.rq xp-e.rq rc.exe
	rc.exe xp-d.rq
	rc.exe xp-e.rq	

clean:
	del *.exe *.res

install: xp
	mkdir bin
	move xp.exe bin
	move xp-d.res bin
	move xp-e.res bin

#
# $Log$
# Revision 1.1  2000/08/06 20:10:32  mk
# - erster Versuch eines Makefiles
#
#

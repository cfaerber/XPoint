# Makefile - makefile for nomarch

CC=gcc
CFLAGS=-O2 -Wall

# Set BINDIR to directory for binary,
# MANDIR to directory for man page.
# Usually it will be simpler to just set PREFIX.
#
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
MANDIR=$(PREFIX)/man/man1

# You shouldn't need to edit below this line.
#--------------------------------------------------------

OBJ=main.o readrle.o readhuff.o readlzw.o

all: nomarch

nomarch: $(OBJ)
	$(CC) $(CFLAGS) -o nomarch $(OBJ)

installdirs:
	/bin/sh ./mkinstalldirs $(BINDIR) $(MANDIR)

install: nomarch installdirs
	install -m 755 nomarch $(BINDIR)
	install -m 644 nomarch.1 $(MANDIR)

uninstall:
	$(RM) $(BINDIR)/nomarch $(MANDIR)/nomarch.1

clean:
	$(RM) *.o *~ nomarch


# stuff to make distribution tgz

VERS=1.4

tgz: ../nomarch-$(VERS).tar.gz
  
../nomarch-$(VERS).tar.gz: clean
	@cd ..;ln -s nomarch nomarch-$(VERS)
	cd ..;tar zchvf nomarch-$(VERS).tar.gz nomarch-$(VERS)
	@cd ..;$(RM) nomarch-$(VERS)

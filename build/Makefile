# $Id$ 

# ---------------------------------------------------------------------

include Makefile.inc

default :
	@echo "Possible targets:"
	@echo ""
	@echo "  source ............. all of these:"
	@echo "    source-tgz ......... gzip'ed tarball"
	@echo "    source-bz2 ......... bzip2'ed tarball"
	@echo "    source-zip ......... zip source archive"
	@echo ""
	@echo "  debian ............. all of these:"
	@echo "    debian-src ......... Debian source package"
	@echo "    debian-bin ......... Debian binary package"
	@echo "  debian-upload ........ Complete upload for Debian"
	@echo ""
	@echo "  rpm ................ all of these:"
	@echo "    rpm-src ............ RPM source package"
	@echo "    rpm-bin ............ RPM binary package"
	@echo ""
	@echo "  all ................ everything (except debian-upload)"
	@echo ""

all : source debian-upload rpm

# -- Source Distribution ----------------------------------------------

source : source-tgz source-bz2 source-zip
source-tgz : $(SOURCE_FBASE).tar.gz
source-bz2 : $(SOURCE_FBASE).tar.bz2
source-zip : $(SOURCE_FBASE).zip

tmp/$(SOURCE_FBASE).tar : tmp/stamp-src
	cd tmp ; tar -c -f '$(SOURCE_FBASE).tar' '$(SOURCE_BASE)/'

$(SOURCE_FBASE).tar.gz : tmp/$(SOURCE_FBASE).tar
	gzip -c -9 tmp/$(SOURCE_FBASE).tar > $(SOURCE_FBASE).tar.gz

$(SOURCE_FBASE).tar.bz2 : tmp/$(SOURCE_FBASE).tar 
	bzip2 -c -9 tmp/$(SOURCE_FBASE).tar > $(SOURCE_FBASE).tar.bz2

$(SOURCE_FBASE).zip : tmp/stamp-src
	@echo "cd tmp; zip -DX9q[l] ../$(SOURCE_FBASE).zip $(SOURCE_BASE)/[...]" >&2
	@rm -f $(SOURCE_FBASE).zip
	@cd tmp ; \
	 zip -DX9ql ../$(SOURCE_FBASE).zip $(patsubst %,$(SOURCE_BASE)/%,$(SOURCE_FILES_TXT)) ; \
	 zip -DX9q  ../$(SOURCE_FBASE).zip $(patsubst %,$(SOURCE_BASE)/%,$(SOURCE_FILES_BIN))

# -- Debian Packages --------------------------------------------------

debian : debian-src debian-bin

debian-upload : debian-src debian-bin
	cd tmp/$(SOURCE_BASE).build/; dpkg-genchanges -u../..
	@ln -f tmp/$(DEB_SOURCE)_*.changes .
	@for file in $(DEB_SOURCE)_$(DEB_VERSION)*.changes $(DEB_SOURCE)_$(DEB_VERSION)*.dsc ; do \
	  gpg --local-user "$(DEB_MAINTNR)" --clearsign --armor --textmode < $$file > $$file ; \
	done
	
debian-src : tmp/stamp-debian-src
debian-bin : tmp/stamp-debian-bin

tmp/stamp-debian-src : $(SOURCE_FBASE).tar.gz tmp/stamp-src tmp/stamp-debian
	@ln -f $(SOURCE_FBASE).tar.gz tmp/$(SOURCE_FBASE).tar.gz 
	cd tmp; dpkg-source -b $(SOURCE_BASE) $(SOURCE_FBASE).tar.gz
	@grep -v '$(DEB_SOURCE)_$(DEB_VERSION)-$(DEB_REVISION).diff' \
	 < 'tmp/$(DEB_SOURCE)_$(DEB_VERSION)-$(DEB_REVISION).dsc' \
	 > '$(DEB_SOURCE)_$(DEB_VERSION)-$(DEB_REVISION).dsc'
	@touch tmp/stamp-debian-src

tmp/stamp-debian-bin : tmp/stamp-build tmp/stamp-debian
	cd tmp/$(SOURCE_BASE).build/; dpkg-buildpackage -uc -nc -b -rfakeroot
	@ln -f tmp/$(DEB_SOURCE)*.deb .
	@touch tmp/stamp-debian-bin

# -- RPM packages -----------------------------------------------------

RPM_FLAGS = --define '_topdir $(shell pwd)/tmp/rpm-work/' \
  --define 'buildroot $(shell pwd)/tmp/rpm-root'
RPM_MOVE = sed -ne "s/\(.*\/lsb-\([^-]*\.[^-]*-\)\?\(.*\)\)/ln -f '\1' '\3'/ip" | sh

rpm : rpm-bin rpm-src

rpm-src : tmp/stamp-src tmp/stamp-rpm-work
	rpm -bs $(RPM_FLAGS) tmp/$(SOURCE_BASE)/build/lsb-openxp.spec
	@find tmp/rpm-work/SRPMS -name "*openxp*.rpm" | $(RPM_MOVE)

rpm-bin : tmp/stamp-build tmp/stamp-rpm-work
	rpm -v -bb $(RPM_FLAGS) tmp/$(SOURCE_BASE)/build/lsb-openxp.spec
	@find tmp/rpm-work/RPMS -name "*openxp*.rpm" | $(RPM_MOVE)

tmp/stamp-rpm-work : tmp/stamp-src $(SOURCE_FBASE).tar.gz
	@echo -n Setting up RPM build environment...
	-@rm -rf tmp/rpm-root tmp/rpm-work tmp/rpmrc
	@mkdir tmp/rpm-root tmp/rpm-work tmp/rpm-work/BUILD tmp/rpm-work/RPMS \
	       tmp/rpm-work/RPMS/i386 tmp/rpm-work/SOURCES tmp/rpm-work/SPECS tmp/rpm-work/SRPMS
	@ln $(SOURCE_FBASE).tar.gz tmp/rpm-work/SOURCES
	@touch tmp/stamp-rpm-work
	@echo " Done."

# -- Source tree ------------------------------------------------------

tmp/stamp : tmp/stamp-src tmp/stamp-build
	@touch tmp/stamp

tmp/stamp-src : tmp/stamp-date tmp/stamp-debian tmp/stamp-rpm
	@touch tmp/stamp-src

tmp/stamp-date : tmp/stamp-src-tree prepare_snap.pl
	@perl prepare_snap.pl 'tmp/$(SOURCE_BASE)/snapdate.inc' '$(SOURCE_VERSION)'
	@touch tmp/stamp-date

tmp/stamp-debian : tmp/stamp-src-tree prepare_debian.pl ../debian/changelog
	@perl prepare_debian.pl 'tmp/$(SOURCE_BASE)/debian/changelog' \
	 '$(DEB_VERSION)-$(DEB_REVISION)' '$(DEB_SOURCE)' '$(DEB_MAINTNR)'
	@touch tmp/stamp-debian
	
tmp/stamp-rpm : tmp/stamp-src-tree prepare_rpm.pl lsb-openxp.spec
	@perl prepare_rpm.pl 'tmp/$(SOURCE_BASE)/build/lsb-openxp.spec' \
	  '$(RPM_VERSION)' '$(RPM_REVISION)' '$(SOURCE_FBASE).tar.gz'
	@touch tmp/stamp-rpm
	
tmp/stamp-src-tree : $(patsubst %,../%,$(SOURCE_FILES))
	@echo -n "Createing clean source tree..."
	@rm -rf tmp/
	@mkdir tmp/ 'tmp/$(SOURCE_BASE)/' $(patsubst %,tmp/$(SOURCE_BASE)/%,$(SOURCE_DIRS));
	@for a in $(SOURCE_FILES) ; do ln ../$$a tmp/$(SOURCE_BASE)/$$a ; done
	@touch tmp/stamp-src-tree
	@echo " Done."

tmp/stamp-build : tmp/stamp-src
	@echo -n "Createing separate build tree tree..."
	@rm -rf 'tmp/$(SOURCE_BASE).build/'
	@mkdir 'tmp/$(SOURCE_BASE).build/' $(patsubst %,tmp/$(SOURCE_BASE).build/%,$(SOURCE_DIRS));
	@for a in $(SOURCE_FILES) ; do ln tmp/$(SOURCE_BASE)/$$a tmp/$(SOURCE_BASE).build/$$a; done
	@touch tmp/stamp-build
	@echo " Done."
	
../snapdate.inc : snapdate.inc
	ln -f snapdate.inc ../snapdate.inc

# -- clean ------------------------------------------------------------

clean :
	-rm -rf tmp/ *.rpm *.spm *.tar.gz *.tar.bz2 *.dsc *.deb *.zip *.changes

# -- Makefile.inc -----------------------------------------------------

Makefile.inc : Makefile.inc.pl $(patsubst %,../%,$(SOURCE_FILES))
	@perl Makefile.inc.pl

# ---------------------------------------------------------------------

# Openxp.spec  first version created by Matthias Leonhardt <ml@mleo.net>
# 07.11.2000   some changes to get it running
# 11.11.2000   some more changes to get more running
# 15.10.2001   fix for symlinks-kill
# 21.04.2002   adapted by Christian Boettger <chritain.boettger@web.de>
Summary: openxp - The Open-Source Project (from Crosspoint by Peter Mandrella)
Name: openxp
%define version 3.7.9
#%define ppcopts -gl -FuObjCOM -Funetcall -dDEBUG -CX -XX -Or
#%define ppcopts -gl -FuObjCOM -Funetcall -dDEBUG
%define ppcopts -FU. -FuObjCOM -Funetcall
#%define helpdir /home/boettger/openxp/openxp/contrib
##%define filelist /home/boettger/openxp/openxptools/filelist.lst
%define filelist /tmp/filelist.lst
%define Prefix /usr/local/lib/openxp
Version: %{version}
Release: 1
Group: Applications/Mail
Copyright: (C) 2000 by OpenXP-Team
Source: /usr/src/packages/SOURCES/openxp-%{version}.tar.gz
BuildRoot: /tmp/openxp-root
# Following are optional fields
URL: http://www.openxp.de
Distribution: none
#Patch: openxp.patch
Prefix: %{Prefix}
BuildArchitectures: i386
Requires: ncurses
#Obsoletes:
%description
OpenXP/32 - the mail- and newsreader for fido, uucp, rfc, zconnect and other networks

%prep

%setup -c -n openxp

#%patch

%build
#CFLAGS="$RPM_OPT_FLAGS" ./configure --Prefix=/usr
#make openxp
#patch netcall/zmodem.pas < /home/leo/openxptools/zmodem.diff
#patch < /home/leo/openxptools/xp4.pas.diff
#ppc386 %{ppcopts} openxp
ppc386 -B %{ppcopts} openxp
ppc386 %{ppcopts} rc
ppc386 %{ppcopts} ihs
./rc openxp-d.rq
./rc openxp-e.rq
./ihs doc/openxp-d
./ihs doc/openxp-e
#/home/leo/openxp/rc openxp-d.rq
#/home/leo/openxp/rc openxp-e.rq
#/home/leo/openxp/ihs doc/openxp-d
#/home/leo/openxp/ihs doc/openxp-e

%install
# echo install >> /tmp/rpm.log
rm -fr $RPM_BUILD_ROOT
# directory structure
mkdir -p $RPM_BUILD_ROOT%{Prefix}
mkdir $RPM_BUILD_ROOT%{Prefix}/bin
mkdir $RPM_BUILD_ROOT%{Prefix}/lib
mkdir $RPM_BUILD_ROOT%{Prefix}/doc

# copy bins
cp openxp $RPM_BUILD_ROOT%{Prefix}/bin
cp rc $RPM_BUILD_ROOT%{Prefix}/bin
cp ihs $RPM_BUILD_ROOT%{Prefix}/bin

# copy resources
cp openxp-d.res openxp-e.res $RPM_BUILD_ROOT%{Prefix}/lib

# copy Helpfiles
#pushd .
#cd %{helpdir}
rm -rf doc/CVS
cp doc/* $RPM_BUILD_ROOT%{Prefix}/doc
#popd
#cp doc/*.hlp $RPM_BUILD_ROOT%{Prefix}/doc
cp file_id.diz $RPM_BUILD_ROOT%{Prefix}/doc

pushd .
cd $RPM_BUILD_ROOT
find -type f | sed s/[.]// > %{filelist}
popd

%pre
# echo pre >> /tmp/rpm.log
%define Prefix /usr/local/lib/openxp
/bin/ln -sf %{Prefix}/bin/openxp /usr/local/bin/xp
/bin/ln -sf %{Prefix}/bin/openxp /usr/local/bin/openxp
/bin/ln -sf %{Prefix}/bin/rc /usr/local/bin/rc
/bin/ln -sf %{Prefix}/bin/ihs /usr/local/bin/ihs
/bin/ln -sf %{Prefix}/doc/openxp-d.hlp %{Prefix}/doc/openxp.hlp
# echo preend >> /tmp/rpm.log

%post
# echo post >> /tmp/rpm.log

%preun
# echo preun >> /tmp/rpm.log

%postun
# echo postun >> /tmp/rpm.log
%define Prefix /usr/local/lib/openxp
#rm -f /usr/local/bin/xp
#rm -f /usr/local/bin/openxp
#rm -f %{Prefix}/doc/openxp.hlp
#rm -f /usr/local/bin/uuz
#rm -f /usr/local/bin/zpr
#rm -f /usr/local/bin/zfido
#rm -f /usr/local/bin/ndiff

%clean
# echo clean >> /tmp/rpm.log
rm -rf $RPM_BUILD_ROOT

%files -f %{filelist}

%changelog
* Sun Jan 6 2002 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- compilerupdate
- range-check-fixes

* Sun Dec 23 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- compilerupdate
- range-check-fixes

* Sun Dec 9 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- range-check-fixes

* Sat Dec 8 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- multiple range-check-fixes

* Wed Nov 7 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- some range-check-fixes

* Fri Oct 26 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- some range-check-fixes
- don't delete symlinks while uninstalling (buggy rpm)

* Sun Oct 21 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- pop3-connect-bugfix
- save/restore terminal-state while calling external programs

* Fri Oct 19 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- range check errors-fixes
- pop3-connect-bugfix

* Thu Oct 18 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- Umlaut bugfixes
- range check errors-fixes

* Thu Oct 17 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- Umlaut bugfixes
- external Editor Crash-fix

* Mon Oct 15 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- multiple bugfixes
- compilable (but not fully functional) with Kylix

* Thu Sep 27 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- bugfixes

* Sun Sep 9 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- multiple bugfixes und features
- first source-changes for kylix

* Mon Sep 3 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- fix for Editor-key-bug

* Thu Aug 28 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- bugfixing
- removed crt unit for compiling with delphi (kylix follows soon)

* Mon Jun 18 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- fix 4 dbbug with role-feature

* Mon Jun 11 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- UIDL
- important bugfixes (db-crashes)

* Wed May 30 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- zmodem - verbose debugmessages
- smtp-auth
- bugfixing

* Sun May 20 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- INet-Protocoll extensions + fixes
- bugfixing

* Wed May 1 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- bugfixing

* Fri Apr 20 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- keyboardhandling redesign

* Mon Apr 16 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- fixes for pop3/smtp
- couple of uuz changes
- APOP support
- extended POP3 options

* Wed Apr 11 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- hotfixe debugfile-RuntimeError

* Tue Apr 10 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- hotfixes for pop3/smtp/nntp
- completely rewritten keyboardtranslation for ncurses
- blinkbit disabled

* Fri Apr 6 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- pop3/smtp/nntp support implemented
- bugfixes

* Wed Mar 28 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- completely new netcall-routines
- bugfixes

* Sat Mar 3 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- bugfixes

* Sun Jan 21 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- help bugfixes
- telnetroutines extended but not yet ready

* Fri Jan 19 2001 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- a couple of bugfixes
- netcall

* Sat Dec 30 2000 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- a couple of bugfixes
- first pop3-implementation
- uuz removed - You should use the internal version

* Sat Dec 23 2000 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- a couple of bugfixes

* Tue Dec 05 2000 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- killed uuzext, update for bugfixes

* Sun Nov 13 2000 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- added some binaries to distribution and recompile *.hlp while building

* Sat Nov 11 2000 Matthias Leonhardt <i7lema@rz.uni-jena.de>
- first version of openxp-rpm

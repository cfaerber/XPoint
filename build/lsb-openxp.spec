# $Id$
Name: lsb-openxp.de-openxp
Summary: Text-based mail and news reader
Version: 3.9.7
Release: 1
Group: Applications/Mail
Copyright: GPL
Source: openxp_3.9-7.tar.gz
URL: http://www.openxp.de
Distribution: none
BuildRoot: $RPM_BUILD_ROOT
BuildArchitectures: i386
Requires: lsb
AutoReqProv: no
%description
OpenXP is a text-based messaging user agent. Features include:
 * POP3, SMTP and NNTP support.
 * UUCP support (internal UUCICO, dialup or TCP/IP).
 * FidoNet support (including BinkP, dialup or TCP/IP).
 * MausNet support (planned)
 * ZConnect support (dialup or TCP/IP).
 * Threading across multiple mail accounts and newsgroups.
 * MIME support.
 * PGP/GnuPG support, including PGP/MIME (planned).
 * Full Unicode support (planned).
OpenXP is a descendant from CrossPoint (XP) by Peter Mandrella.
%prep
%setup -n openxp-$RPM_PACKAGE_VERSION

%build
./configure --prefix=/usr
make

%install
install -d -D $RPM_BUILD_ROOT/usr/bin
install -d -D $RPM_BUILD_ROOT/usr/share/openxp
make strip
make install prefix=$RPM_BUILD_ROOT/usr
install -D --mode=644 debian/openxp.1 $RPM_BUILD_ROOT/usr/share/man/man1/openxp.1
%define __os_install_post %{nil}

%clean

%files

%defattr (755, root, root) 
/usr/bin/openxp

%defattr (644, root, root)
/usr/share/openxp/*
/usr/share/man/man1/*.*

%changelog

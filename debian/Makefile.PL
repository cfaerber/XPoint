#!/usr/bin/perl
# $Id$
# 
# Copyright: (C) 2003 Claus Färber <cl@openxp.de>
# Copyright: (C) 2003 OpenXP team <dev@openxp.de>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with Debian system; see the file
# /usr/doc/copyright/GPL. If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

use Date::Format;

my @dist = (
  ['unstable',	    'unstable-snapshot'],
  ['experimental',  'snapshot'],
);

my $debian_changelog_version	= undef;
my $debian_changelog_version_e	= undef;
my $debian_changelog_version_d	= undef;
my $debian_changelog		= undef;

# --- read changelog.in -----------------------------------------------

open DEB_CHANGELOG, "<debian/changelog.in";
while(<DEB_CHANGELOG>) {
  if(!$debian_changelog_version && m/^([^ ]+) \(([0-9a-z\.\+-]+(:[0-9a-z\.\+:-]+)?)-([0-9a-z\.\+]+)\) ([^ ]+);/i) {
    $debian_changelog_package = $1;
    $debian_changelog_version = $2;
    $debian_changelog_version_d = $4;
    $debian_changelog_dist = $5;
    
    if($debian_changelog_version =~ m/([^:]*:)(.*)/) {
      $debian_changelog_version = $2;
      $debian_changelog_version_e = $1;
    }
  }
  $debian_changelog .= $_;
}
close DEB_CHANGELOG;

# --- read version from xpglobal.pas ----------------------------------

my $xpglobal_version	= undef;
my $xpglobal_distrib	= undef;

open XPGLOBAL, "<xpglobal.pas";
while(<XPGLOBAL>) {
  if(m/verstr[\t ]*=[\t ]*'v?([^']+)'/) {
    $xpglobal_version	= $1;
    $xpglobal_version =~ s/[^a-z0-9\.\+]+/-/;
    last;
  }
}
close XPGLOBAL;

my $debian_version = $debian_changelog_version;

# --- read snapshot status from xpdefine.inc --------------------------

my $snapshot = undef;

open XPDEFINE, "<xpdefine.inc";
while(<XPDEFINE>) {
  if(m/({|\(\*)\$DEFINE *Snapshot *(}|\*\))/i) {
    $snapshot++;
    last;
  }
}
close XPDEFINE;

my $openxp = 'openxp';

open RULES, "<debian/rules";
while(<RULES>) {
  if(m/^openxp\s*=\s*(.*\S)/) {
    $openxp = $1;
    last;
  }
}
close RULES;

if($debian_changelog_package ne $openxp) {
  $debian_changelog =~ s/^$debian_changelog_package/$openxp/meg;
  $debian_changelog_package = $openxp;
}

# --- write changelog -------------------------------------------------

open DEB_CHANGELOG, ">debian/changelog";
if ($xpglobal_version ne $debian_changelog_version || $snapshot) {
  my $new_version = $debian_changelog_e .
    $xpglobal_version . 
    ( $xpglobal_version ne $debian_changelog_version
      ? '-0'
      : '-'.$debian_changelog_version_d ) .
    ( $snapshot
      ? sprintf('snapshot%04d%02d%02d',
        ((localtime)[5]+1900),
	((localtime)[4]+1),
	((localtime)[3]) )
      : '' );

    if($xpglobal_version =~ m/^([0-9]+)\.([0-9]+)/) {
      $xpglobal_distrib = $dist[(($1 % 2)==0)?0:1]->[$snapshot?1:0];
    }
      
  printf DEB_CHANGELOG "%s (%s) %s; urgency=low\n", $debian_changelog_package, $new_version,
  	$xpglobal_distrib || $debian_changelog_dist;
  print  DEB_CHANGELOG "\n";
  printf DEB_CHANGELOG "  * New Upstream %s.\n",($snapshot?'Snapshot':"Version\n    (NB: This is a preliminary, autocreated package.)");
  print  DEB_CHANGELOG "\n";
  printf DEB_CHANGELOG " -- OpenXP Autobuild <dev\@openxp.de>  %s\n",
    time2str('%a, %d %b %Y %T %z', time() );
  print  DEB_CHANGELOG "\n";
  $debian_version = $new_version;
}
print DEB_CHANGELOG $debian_changelog;
close DEB_CHANGELOG;

# --- write Makefile.version ------------------------------------------

open DEB_MAKEFILE, ">debian/Makefile.version";
my %deb_arch = ();

# -- get architecture -------------------------------------------------

open DEB_ARCHITECTURE, 'dpkg-architecture |';
while(<DEB_ARCHITECTURE>) { 
  print DEB_MAKEFILE; 
  chomp; 
  m/([^=]*)=(.*)/; 
  $deb_arch{$1} = $2; 
}
close DEB_ARCHITECTURE;

# -- read control -----------------------------------------------------

my @packages = ();
open DEB_CONTROL, "<debian/control";
{
  my $package = undef;
  while(<DEB_CONTROL>) {
    chomp;
    $package = $1 				if m/^Package: *([^ ]+)/i;
    push @packages,(map { $_ = '../'.$package.'_'.$debian_version.'_'.$_.'.deb'; } 
    	grep { $_ eq $deb_arch{DEB_HOST_ARCH} || $_ eq 'all' } 
	split(/[ ,]+/, $1))	if m/^Architecture:(.*)/i;
  }
}
close DEB_CONTROL;
print DEB_MAKEFILE "DEB_VERSION=", $debian_version, "\n";
print DEB_MAKEFILE "DEB_TARGETS=", join(' ',@packages), "\n";
print DEB_MAKEFILE "DEB_UPSTREAM_VERSION=", $xpglobal_version, "\n";

close DEB_MAKEFILE;

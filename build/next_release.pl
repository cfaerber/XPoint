#!perl
# $Id$

use POSIX qw(strftime setlocale LC_ALL);
setlocale (LC_ALL, "C");

chdir '..' if (!-f 'xpglobal.pas') && -f '../xpglobal.pas';
die "Must be run from CVS directory" unless -f 'CVS/Entries';

# Get old version from xpglobal.pas, calculate new version number and update
# xpglobal.pas

open OLD, "<xpglobal.pas";
open NEW, ">xpglobal.pas.new";

my $version = undef;

while(<OLD>) {
  if(m/mainver\s*=\s*'v?([^']+)'/i) {
    $version	= $1;
    $version =~ s/[^a-z0-9\.\+]+/-/g;
    $version =~ s/\.([0-9]+)$/ '.'.($1+1) /e;
    s/(mainver\s*=\s+')[^']+(')/ $1.$version.$2 /ei;
  }
  print NEW $_;
}

close OLD;
close NEW;

unless($version)
{
  unlink "xpglobal.pas.new";
  die "Could not change version number";
}

$version =~ m/^[0-9]+\.([0-9]+)/;
my $odd_version = ($1 % 2) != 0;

my $cvs_version = $version;
$cvs_version =~ s/-*\+-*/-/g;
$cvs_version =~ s/\./_/g;

# update RPM spec file

open OLD, "<build/lsb-openxp.spec";
open NEW, ">build/lsb-openxp.spec.new";

while(<OLD>) {
  if(m/^Version:/) {
    printf NEW "Version: %s\n", $version;
    printf NEW "Release: 1\n";
    printf NEW "Source: openxp_%s.tar.gz\n", $version;
  }
  elsif(m/^Release:/) { }
  elsif(m/^Source:/) { }
  elsif(m/^%changelog/) {
    print  NEW "%changelog\n";
    printf NEW "* %s %s <%s>\n",(strftime "%a %b %d %Y", gmtime),
      'OpenXP', 'dev@openxp.de';
    printf NEW "- New upstream version %s (release 1)\n", $version;
    print  NEW "\n";
  }
  else
  {
    print NEW $_;
  }
}

# update Debian changelog

my $deb_maintainer = "OpenXP <dev\@openxp.de>";
my $deb_package = 'openxp';

open CTL, "<debian/control";
while (<CTL>) {
  if(m/^Maintainer: *(.*) *$/) {
    $deb_maintainer = $1;
  }
  if(m/^Source: *(.*) *$/) {
    $deb_package = $1;
  }
}

open OLD, "<debian/changelog";
open NEW, ">debian/changelog.new";

printf NEW "%s (%s-1) %s; urgency=low\n\n", $deb_package, $version, 
  ($odd_version ? 'experimental' : 'unstable');
printf NEW "  * New upstream version %s (revision 1)\n\n", $version;
printf NEW " -- %s  %s +0000\n\n", $deb_maintainer,
  (strftime "%a, %d %b %Y %T", gmtime);

while(<OLD>) { print NEW $_; };

# unset snapshot define

open OLD, "<xpdefine.inc";
open NEW, ">xpdefine.inc.new";

while(<OLD>) {
  if(m/^\s*(\{|\(\*)[^a-z]*(undef|define)\s+snapshot\s*(\}|\*\))\s*/i) {
    print NEW "{\$UNDEF Snapshot}\n";
  } else {
    print NEW $_;
  }
}

close OLD;
close NEW;

# Checkin the changes and tag the version

rename "xpglobal.pas.new", 	"xpglobal.pas";
rename "debian/changelog.new",  "debian/changelog";
rename "build/lsb-openxp.spec.new", "build/lsb-openxp.spec";
rename "xpdefine.inc.new",	"xpdefine.inc";

system 'cvs', 'commit', '-m', 'New version number: '.$version,
  "xpglobal.pas", "debian/changelog", "build/lsb-openxp.spec", "xpdefine.inc" || die;
system 'cvs', 'tag', '-F', 'OpenXP-'.$cvs_version || die;

# system 'cvs', 'tag', 'Debian-'.$cvs_version.'-1';
# system 'cvs', 'tag', '-b', 'Debian-'.$cvs_version;
# system 'cvs', 'tag', 'LSBRPM-'.$cvs_version.'-1';
# system 'cvs', 'tag', '-b', 'LSBRPM-'.$cvs_version;

# re-set snapshot define

open OLD, "<xpdefine.inc";
open NEW, ">xpdefine.inc.new";

while(<OLD>) {
  if(m/^\s*(\{|\(\*)[^a-z]*(undef|define)\s+snapshot\s*(\}|\*\))\s*/i) {
    print NEW "{\$DEFINE Snapshot}\n";
  } else {
    print NEW $_;
  }
}

close OLD;
close NEW;

rename "xpdefine.inc.new",	"xpdefine.inc";
system 'cvs', 'commit', '-m', 'Re-set $DEFINE Snapshot for CVS versions.', 'xpdefine.inc' || die;
system 'cvs', 'update', '-r', 'OpenXP-'.$cvs_version || die;

#!perl
# $Id$

use POSIX qw(strftime setlocale LC_ALL);
setlocale (LC_ALL, "C");
use File::Find;

chdir '..' if (!-f 'xpglobal.pas') && -f '../xpglobal.pas';
die "Must be run from CVS directory" unless -f 'CVS/Entries';

# -- read version number from xpglobal.pas ----------------------------
# 
open OLD, "<xpglobal.pas";
my $version = undef;
while(<OLD>) {
  if(m/mainver\s*=\s*'v?([^']+)'/i) {
    $version	= $1;
    last
  }
}
die "Could not determine version number" unless($version);

# -- read snapshot status from xpdefine.inc ---------------------------
#
my $is_snapshot;
open OLD, "<xpdefine.inc";
while(<OLD>) {
  if(m/\s*(\{|\(\*)\$define\s+snapshot\s*(\}|\*\))\s*/i) {
    $is_snapshot++;
    last;
  } 
}

# -- get list of files and last commit date ---------------------------
#
our %months = (
  'jan' => 1, 'feb' => 2, 'mar' => 3, 'apr' => 4,
  'may' => 5, 'jun' => 6, 'jul' => 7, 'aug' => 8,
  'sep' => 9, 'oct' =>10, 'nov' =>11, 'dec' =>12, );
my @files = ();
my @files_txt = ();
my @files_bin = ();
my @dirs = ();
my $snapdate = undef;
find(sub{
  return unless ($_ eq "Entries" && $File::Find::dir =~ m%\/CVS\/?$%);

  my $dir = $File::Find::dir;
  $dir =~ s/\/+CVS\/*$//;
  $dir =~ s/^(\.\/+)*(\.\/*)//;

  return if $dir =~ m/^(Internet|build|playground|CVSROOT)(?:\/.*)?$/i;

  push @dirs, $dir if $dir;

  open ENTRIES, "<$_";
  while(<ENTRIES>) {
    next unless m%^/([^/]*)/[^/]*/([^/]*)/([^/]*)/$%;
    my ($file,$date,$flags) = ($1,$2,$3);

    my $dir = $dir;
    $dir .= '/' if $dir;
    $dir .= "$file";
    push @files, $dir;
    $flags =~ m/-kb/ ? (push @files_bin, $dir) : (push @files_txt, $dir);
    
    next unless 
      $date =~ m%^[A-Z][a-z][a-z] +([A-Z][a-z][a-z]) +([0-9]+) +([0-9]+):([0-9]+):([0-9]+) +([0-9]{4,})$%;
    my ($month,$day,$hh,$mm,$ss,$year) = ($1,$2,$3,$4,$5,$6);
    my $new_date = sprintf("%04d%02d%02dT%02d%02d",
      $year, $months{lc $month}, $day, $hh, $mm, $ss);
    $snapdate = $new_date if $snapdate < $new_date || !defined $snapdate;
  }
  close ENTRIES;
}, '.');
@files = sort @files;
@dirs = sort @dirs;

# -- read Debian control information ----------------------------------
#
my $deb_source = undef;
my @deb_bin = ();
my $deb_maintainer = "OpenXP <dev\@openxp.de>";
open DEB, "<debian/control";
while(<DEB>) {
  if(m/^Source:\s*(\S+)/) {
     $deb_source = $1;
  }
  elsif(m/^Package:\s*(\S+)/) {
     push @deb_bin, $1;
  }
  elsif(m/^Maintainer:\s*(.*\S)/) {
    $deb_maintainer = $1;
  }
}

my $deb_version = undef;
my $deb_revision = undef;
my $deb_source_changelog = undef;
my $deb_snapshot = $is_snapshot;
open DEB, "<debian/changelog";
while(<DEB>) {
  if(m/(\S+)\s+\(([[:alnum:]\.\+\-:])-([[:alnum:]\.\+]+)\)\s/) {
    $deb_source_changelog = $1;
    $deb_version = $2;
    $deb_revision = $3;
  }
}

if($deb_version ne $version || $deb_source_changelog ne $deb_source) {
  $deb_version = $version;
  $deb_revision = '0';
  $deb_snapshot ||= 1;
}
else {
  $deb_revision =~ s/\s[0-9]+T[0-9]+$//;
}
$deb_revision .= 's'.$snapdate if $is_snapshot;

$deb_revision =~ m/^[0-9]+\.([0-9]+)/;
$deb_distribution =  ($1 % 1) ? 'experimental' : 'unstable';

# -- read RPM control files -------------------------------------------
#
my $rpm_package = undef;
my $rpm_maintainer = "OpenXP <dev\@openxp.de>";
my $rpm_version = undef;
my $rpm_release = undef;
my $rpm_snapshot = $is_snapshot;

open RPM, "<build/lsb-openxp.spec";
while(<RPM>) {
  if(m/^Version:\s+(\S+)/) {
    $rpm_version = $1;
  }
  elsif(m/^Release:\s+(\S+)/) {
    $rpm_release = $1;
  }
  elsif(m/^Name:\s+(\S+)/) {
    $rpm_package = $1;
  }
}

if($rpm_version ne $version) {
  $rpm_version = $version;
  $rpm_release = '0';
  $rpm_snapshot ||= 1;
}
else {
  $rpm_release =~ s/\s[0-9]+T[0-9]+$//;
}
$rpm_release .= 's'.$snapdate if $is_snapshot;

# -- CVS tags ---------------------------------------------------------
#
my $cvs_version = $version;
$cvs_version =~ s/-*\+-*/-/g;
$cvs_version =~ s/\./_/g;

if($is_snapshot) {
  push @files, 'snapdate.inc';
  push @files_txt, 'snapdate.inc';
}

push @files, 'build/lsb-openxp.spec';
push @files_txt, 'build/lsb-openxp.spec';
push @dirs, 'build';

# -- write Makefile.inc -----------------------------------------------
#
open MAKE, ">build/Makefile.inc";
printf MAKE "SOURCE_BASE=openxp-%s\n", 	$version;
printf MAKE "SOURCE_FBASE=openxp_%s\n",	($is_snapshot ? $version.'-'.$snapdate : $version);
printf MAKE "SOURCE_VERSION=%s\n",     	($is_snapshot ? $version.'-'.$snapdate : $version);
print  MAKE "\n";
print  MAKE "SOURCE_DIRS=@dirs\n";
print  MAKE "SOURCE_FILES=@files\n";
print  MAKE "SOURCE_FILES_BIN=@files_bin\n";
print  MAKE "SOURCE_FILES_TXT=@files_txt\n";
print  MAKE "\n";
printf MAKE "RPM_VERSION=%s\n",  $rpm_version;
printf MAKE "RPM_REVISION=%s\n", $rpm_release;
printf MAKE "RPM_PACKAGE=%s\n",  $rpm_package;
print  MAKE "\n";
printf MAKE "DEB_VERSION=%s\n",  $deb_version;
printf MAKE "DEB_REVISION=%s\n", $deb_revision;
printf MAKE "DEB_SOURCE=%s\n",   $deb_source;
printf MAKE "DEB_DISTRIB=%s\n",	 $deb_distribution;
printf MAKE "DEB_MAINTNR=%s\n",	 $deb_maintainer;
print  MAKE "\n";
printf MAKE "CVS_VERSION=%s\n",  $cvs_version;

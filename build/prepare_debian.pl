#!perl
# $Id$

use POSIX qw(strftime mktime setlocale LC_ALL);
setlocale (LC_ALL, "C");

chdir 'build' if (!-f '../xpglobal.pas') && -f 'build/xpglobal.pas';
die "Must be run from CVS directory" unless -f 'CVS/Entries';

my($file, $deb_version, $deb_package, $deb_maintainer) = @ARGV;

if($deb_version =~ m/s(
    ([0-9]{4,})
    ([0-9][0-9])
    ([0-9][0-9])
    T
    ([0-9][0-9])
    ([0-9][0-9])?
    ([0-9][0-9])?
  )$/x)
{
  my($date, $year, $mon, $mday, $hour, $min, $sec) = ($1, $2, $3, $4, $5, $6, $7);

  print STDERR "Changing $file...";

  open OLD, "<$file";
  open NEW, ">$file.new";

  printf NEW "%s (%s) %s; urgency=low\n\n", $deb_package, $deb_version, 'snapshot';
  print  NEW "  * New snapshot ($date).\n\n";
  printf NEW " -- %s  %s +0000\n\n", $deb_maintainer,
    (strftime "%a, %d %b %Y %T", $sec, $min, $hour, $mday, $mon-1, $year-1900);
  while(<OLD>) { print NEW $_; };  

  close OLD;
  close NEW;

  rename "$file.new", $file;

  print STDERR " Done.\n";
}

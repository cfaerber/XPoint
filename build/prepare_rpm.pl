#!perl
# $Id$

use POSIX qw(strftime mktime setlocale LC_ALL);
setlocale (LC_ALL, "C");

chdir 'build' if (!-f '../xpglobal.pas') && -f 'build/xpglobal.pas';
die "Must be run from CVS directory" unless -f 'CVS/Entries';

my($file, $rpm_version, $rpm_release, $source) = @ARGV;

if($rpm_release =~ m/s(
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

  while(<OLD>) {
    if(m/^Version:/) {
      print NEW "Version: $rpm_version\n";
      print NEW "Release: $rpm_release\n";
      print NEW "Source: $source\n";
    } 
    elsif(m/^(Release|Source):/) {
    	# noop
    }
    elsif(m/^%changelog/) {
      print  NEW "%changelog\n";
      printf NEW "* %s %s <%s>\n",
        (strftime "%a %b %d %Y", $sec, $min, $hour, $mday, $mon-1, $year-1900),
        'OpenXP', 'dev@openxp.de';
      printf NEW "- New snapshot (%s)\n", $date;
      print  NEW "\n";
    }
    else {
      print NEW $_;  
    }
  }

  close OLD;
  close NEW;

  rename "$file.new", $file;

  print STDERR " Done.\n";
}

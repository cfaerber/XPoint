#!/usr/bin/perl

use File::Find;

our %months = (
  'jan' => 1, 'feb' => 2, 'mar' => 3, 'apr' => 4,
  'may' => 5, 'jun' => 6, 'jul' => 7, 'aug' => 8,
  'sep' => 9, 'oct' =>10, 'nov' =>11, 'dec' =>12, );

chdir ".." if (!-x 'xpglobal.pas') && (-x '../xpglobal.pas');

my $date = undef;

find(sub{
  next unless $_ eq "Entries";
  next unless $File::Find::dir =~ m%\/CVS\/?$%;

  print STDERR "$File::Find::dir\n";

  open ENTRIES, "<$_";
  while(<ENTRIES>) {
    next unless m%^/[^/]*/[^/]*/([^/]*)/[^/]*/$%;
    next unless $1 =~ m%^[A-Z][a-z][a-z] +([A-Z][a-z][a-z]) +([0-9]+) +([0-9]+):([0-9]+):([0-9]+) +([0-9]{4,})$%;
    my ($month,$day,$hh,$mm,$ss,$year) = ($1,$2,$3,$4,$5,$6);
    my $new_date = sprintf("%04d%02d%02d%02d%02d%02d",
      $year, $months{lc $month}, $day, $hh, $mm, $ss);
    $date = $new_date if $date < $new_date || !defined $date;
  }
  close ENTRIES;
}, '.');

open VERSION, ">version.inc";
print VERSION "builddate = '$date';\n";

#!perl
# $Id$

chdir 'build' if (!-f '../xpglobal.pas') && -f 'build/xpglobal.pas';
die "Must be run from CVS directory" unless -f 'CVS/Entries';

my ($file,$version) = @ARGV;

if($version =~ m/-([0-9]{6,}T[0-9]{4,})$/)
{
  my $date = $1;
  my $file_base = $file; $file_base =~ s%.*/%%;
    
  print STDERR "Changing $file...";

  open FILE, ">$file.new";
  print FILE "(*\nCVS conflict to prevent accidental commit:\n<<<<<<< $file_base\n=======\n*)\n\n";
  print FILE "  snapdate = '$date';\n\n";
  print FILE "(*\n>>>>>>> 1.1\n*)\n";
  close FILE;
  rename "$file.new", $file;

  print STDERR " Done.\n";
}

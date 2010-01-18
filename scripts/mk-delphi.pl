#!perl

use strict;
use utf8;

my $xp_name = lc shift @ARGV;
$xp_name ||= 'crosspoint';

my @inc_files = ();
my @pas_files = ();

open GITLS, '-|', 'git', 'ls-files';

while(<GITLS>) {
  s(\/)(\\)g;
  next unless m/\.(?:pas|pp|(inc))$/i;
  chomp; 
  push @inc_files, $_ if defined($1);
  push @pas_files, $_ if !defined($1);
};

open GITVER, '>', $xp_name.'.dpr';

printf GITVER "(* DO NOT EDIT - This file is generated automatically by %  - DO NOT EDIT *)\n\n", $0;
print  GITVER "{\$I xpdefine.inc}\n\n";
printf GITVER "program %s;\n\n", $xp_name;

print GITVER "{%File '$_'}\n" foreach(@inc_files);
print GITVER "\n" if @inc_files;

print  GITVER "uses ", join(",\n  ", map {
    m/([^\\\/]*)\.[^\\\/\.]*$/;
    lc($1)." in '".$_."'";
  } @pas_files), ";\n\n" if @pas_files;

printf GITVER "{\$R *.res}\n\n", $xp_name;
print  GITVER "begin\n";
print  GITVER "  StartOpenXP;\n";
print  GITVER "end.\n";

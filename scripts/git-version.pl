#!perl

use strict;
use utf8;

open GITLOG, '-|', 'git', 'log', '-n', 1, '--pretty=%H,%ci,%cN,%cE';
binmode GITLOG, ':utf8';
my @git = split /[,\r\n]/s, scalar <GITLOG>;

open GITLOG, '-|', 'git', 'describe', '--tags', '--dirty=+';
binmode GITLOG, ':utf8';
push @git, map { chomp;$_; } scalar <GITLOG>;

open GITVER, '>', 'git-version.inc';
binmode GITVER, ':encoding(IBM437)';

print GITVER <<HEAD;
(* DO NOT EDIT - This file is generated automatically by git hooks - DO NOT EDIT *)

HEAD

foreach(('hash','date','name', 'email', 'desc')) {
  printf GITVER "  git_%s = '%s';\n", $_, shift @git;
}

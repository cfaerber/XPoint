#!perl

use strict;
use utf8;

open GITLOG, '-|', 'git', 'log', '-n', 1, '--pretty=%H,%ci,%cN,%cE';
binmode GITLOG, ':utf8';

my @git = split /[,\r\n]/s, scalar <GITLOG>;

open GITVER, '>', 'git-version.inc';
binmode GITVER, ':encoding(IBM437)';

foreach(('hash','date','name', 'email')) {
  printf GITVER "  git_%s = '%s';\n", $_, shift @git;
}





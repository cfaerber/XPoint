#!/usr/bin/perl -w

# use with eval $(get_build_nr.pl)
# get version number from xpglobal.pas and version.inc and build .spec-file

  $BUILD = "unkown";

  open(InFile, "../version.inc");
  while (<InFile>) {
    if (/version_build.*=.*\'.*\s(.*)\s.*\'/ig ) { $BUILD = $1;  }
  }
  close(InFile);

print 'svn propset version "'.$BUILD.'" version.inc'."\n";
print 'svn ci version.inc "-m touch for version update"'."\n";


#!/usr/bin/perl -w

# increments build version in ../version.inc
 
  $BUILD = "unkown";
  
  open(InFile, "../version.inc");
  while (<InFile>) {
    if (/buildver.*=.*\'(.*)\'/ig ) { $BUILD = $1;  }
  }
  close(InFile);

  open(OutFile, ">../version.inc");
  $_ = "buildver = \'".++$BUILD."\';";
  print(OutFile);
  close(OutFile);

print "New build version: $BUILD\n";

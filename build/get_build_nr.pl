#!/usr/bin/perl -w

# use with eval $(get_build_nr.pl)
# get version number from xpglobal.pas and version.inc and build .spec-file
 
  $MAINVER = "unkown";
  $SUBVER = "unkown";
  $BUILD = "unkown";
  

  open(InFile, "../xpglobal.pas");
  while (<InFile>) {
    if (/mainver.*=.*\'(.*)\.(.*)\'/ig ) { $MAINVER = $1; $SUBVER=$2 }  
    if (/subver.*=.*\'(.*)\'/ig ) { $SUBVER = $1;  }
  }
  close(InFile);

  open(InFile, "../version.inc");
  while (<InFile>) {
    if (/buildver.*=.*\'(.*)\'/ig ) { $BUILD = $1;  }
  }
  close(InFile);


  open(InFile, "openxp.spec");
  open(OutFile, ">openxp-$MAINVER.$SUBVER-$BUILD.spec");

  while (<InFile>) {

    if (s/\%version\%/$MAINVER\.$SUBVER/ig) {  }
    if (s/\%release\%/$BUILD/ig) {  }

    print OutFile;
  }

  close(InFile);
  close(OutFile);

print "export OPENXP_MAINVER=$MAINVER\n";
print "export OPENXP_SUBVER=$SUBVER\n";
print "export OPENXP_BUILD=$BUILD\n";

$VERZ = $ENV{'TEMP'};
if (!$VERZ) { $VERZ = '/tmp' } 
open(OutFile, ">$VERZ/openxp_ver.txt");
$_ = "- new version $MAINVER.$SUBVER-$BUILD";
print OutFile;
close(OutFile);

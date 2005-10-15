#!/usr/bin/perl -w

# use with eval $(get_build_nr.pl)
# get version number from xpglobal.pas and version.inc and build .spec-file

  $MAINVER = "unknown";
  $SUBVER = "unknown";
  $BUILD = "unknown";
  $RELEASE = "1";


  open(InFile, "../xpglobal.pas");
  while (<InFile>) {
    if (/mainver.*=.*\'(.*)\.(.*)\'/ig ) { $MAINVER = $1; $SUBVER=$2 }
  }
  close(InFile);

  open(InFile, "version.svn");
  while (<InFile>) {
    if (/Revision (.*)\./ig ) { $BUILD = $1;  }
  }
  close(InFile);
  unlink 'version.svn';

  open(OutFile, ">\.\./version.inc");
  print OutFile "version_build = ".$BUILD.";\n";
  close(OutFile);
  printf("version.inc written\n");

  open(InFile, "../xpdefine.inc");
  while (<InFile>) {
    if (/\{\$DEFINE Snapshot\}/ig ) { $RELEASE = "0" }
  }
  close(InFile);

  if ($RELEASE eq "1") {
    $OPTS = "-gl -CX -OG3p3r";
  } else
  {
    $OPTS = "-gl -OG3p3";
  }


  open(InFile, "openxp.spec");
  open(OutFile, ">openxp-$MAINVER.$SUBVER-$BUILD.spec");

  while (<InFile>) {

    s/\%version\%/$MAINVER\.$SUBVER/ig;
    s/\%release\%/$BUILD/ig;
    s/\%compopts\%/$OPTS/ig;

    print OutFile;
  }

  close(InFile);
  close(OutFile);

if ($^O eq "MSWin32") {
  print "SET OXP_VER=$MAINVER.$SUBVER.$BUILD\n";
  print "SET OXP_OPTS=$OPTS\n";
} else
{
  print "export OPENXP_MAINVER=$MAINVER\n";
  print "export OPENXP_SUBVER=$SUBVER\n";
  print "export OPENXP_BUILD=$BUILD\n";
  print "export OPENXP_RELEASE=$RELEASE\n";
  print "export OXP_VER=$MAINVER.$SUBVER.$BUILD\n";
  print "export OXP_OPTS='$OPTS'\n";
}

  open(InFile, "file_id.win.in");
  open(OutFile, ">file_id.win");

  while (<InFile>) {

    if (s/\%version\%/$MAINVER\.$SUBVER.$BUILD/ig) {  }

    print OutFile;
  }

  close(InFile);
  close(OutFile);

  open(InFile, "file_id.dos.in");
  open(OutFile, ">file_id.dos");

  while (<InFile>) {

    if (s/\%version\%/$MAINVER\.$SUBVER.$BUILD/ig) {  }

    print OutFile;
  }

  close(InFile);
  close(OutFile);

  open(InFile, "file_id.source.in");
  open(OutFile, ">file_id.source");

  while (<InFile>) {

    if (s/\%version\%/$MAINVER\.$SUBVER.$BUILD/ig) {  }

    print OutFile;
  }

  close(InFile);
  close(OutFile);

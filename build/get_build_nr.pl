#!/usr/bin/perl -w

# use with eval $(get_build_nr.pl)

  open(InFile, "../xpglobal.pas");
  while (<InFile>) {
    if (/mainver.*=.*\'(.*)\'/ig ) { $ENV{'OPENXP_MAINVER'} = $1; }  
    if (/subver.*=.*\'(.*)\'/ig ) { $ENV{'OPENXP_SUBVER'} = $1;  }
  }
  close(InFile);

  open(InFile, "../version.inc");
  while (<InFile>) {
    if (/buildver.*=.*\'(.*)\'/ig ) { $BUILD = $1;  }
  }
  close(InFile);

  open(OutFile, ">../version.inc");
  $_ = "buildver = \'".++$BUILD."\'";
  print(OutFile);
  close(OutFile);

#print $ENV{'OPENXP_MAINVER'}."\n";
#print $ENV{'OPENXP_SUB'}."\n";
#print $ENV{'OPENXP_BUILD'}."\n";

print "export OPENXP_MAINVER=$ENV{'OPENXP_MAINVER'}\n";
print "export OPENXP_SUBVER=$ENV{'OPENXP_SUBVER'}\n";
print "export OPENXP_BUILD=$BUILD\n";

# openxp_ver_hi für 3.8, openxp_ver_lo für 13 und openxp_ver_build für 17

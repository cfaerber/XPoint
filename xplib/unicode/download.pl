#!perl
#
# $Id$
# Download parts of the Unicode database

use LWP::UserAgent;
use HTTP::Request;

@files = (
  # 'ArabicShaping.txt',
  # 'BidiMirroring.txt',
  # 'Blocks.txt',
  # 'CaseFolding.txt',
  # 'CompositionExclusions.txt',
  # 'DerivedAge.txt',
  # 'DerivedCoreProperties.txt',
  # 'DerivedNormalizationProps.txt',
  # 'EastAsianWidth.txt',
  # 'Index.txt',
  # 'Jamo.txt',
  'LineBreak.txt',
  # 'NamesList.txt',
  # 'NormalizationCorrections.txt',
  # 'NormalizationTest.txt',
  # 'PropList.txt',
  # 'PropertyAliases.txt',
  # 'PropertyValueAliases.txt',
  # 'Scripts.txt',
  # 'SpecialCasing.txt',
  # 'UnicodeData.txt',
  # 'Unihan.txt',
);

my $UserAgent = LWP::UserAgent->new;
foreach (@files) {
  print STDERR "Downloading $_... ";
  my $Request = HTTP::Request->new('GET', 'http://www.unicode.org/Public/UNIDATA/'.$_);
  $UserAgent->request($Request,$_);
  print STDERR "Done.\n";
}

#
# $Log$
# Revision 1.1  2003/02/13 14:27:12  cl
# - Unicode support library:
#   . character width
#   . character line breaking properties/line breaking library
#   . UTF8 functions
#

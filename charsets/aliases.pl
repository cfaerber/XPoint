#!perl 
# $Id$
#
# aliases.pl - Converts IANA charset list to PASCAL aliases.inc
# (C) 2001 Claus Färber <claus@faerber.muc.de>
#
# This file is provided as-is by the author.  No claims are made
# as to fitness for any particular purpose.  No warranties of any
# kind are expressed or implied.  The recipient agrees to
# determine applicability of information provided.
#
# The author hereby grants the right to freely use the information
# supplied in this file in the creation of products supporting the
# Unicode Standard, and to make copies of this file in any form
# for internal or external distribution as long as this notice
# remains attached.

$name=undef;
@alias=();

use LWP::UserAgent;
use HTTP::Request;

$data = undef;
$started = undef;

print STDERR "Connecting www.iana.org:80...\n";

$UserAgent = LWP::UserAgent->new;
$Request = HTTP::Request->new('GET', 'http://www.iana.org/assignments/character-sets');
$UserAgent->request($Request,sub {

  unless ($started){
    print STDERR "Retrieving data...\n";
    open STDOUT,">aliases.inc";
    print "(* \$Id$ *)\n";
    print "(* generated from IANA charset list -- do not edit *)\n\n";
    print "function MimeCharsetCanonicalName(Name: String): String;\nbegin\n";
    print "  Name:=UpperCase(Name);";
    $started=1;
  }

  $data.=shift;
  while($data=~s/^([^\r\n]*)\r?\n//s) {
    $_=$1;
  
    if (/^Name: *([^ ]*)/) 
    {
      printf STDERR $name.(($#alias>0)?(" (".($#alias)." aliases)\n"):"\n");
      print "\n";
      foreach (@alias) {
        printf "  if name=%-63s else\n",
          sprintf "%-25s then result :=%s","'".uc($_)."'","'$name'";
      };
  
      $name=$1;
      @alias=($name);
    } elsif(/^Alias: *([^ ]+) \(.*preferred MIME.*\)/) {
      $name=$1;
      push @alias,$name;
    } elsif(/^Alias: *([^ ]+)/) {
      push @alias,$1 unless lc($1) eq "none";
    } elsif(/^REFERENCES[ \t]*$/) {
      print "\n  result:=name;\nend;\n";
    }
   
    s/\(\*/( */g; 
    s/\*\)/* )/g;
    1 while s/\t+/' ' x (length($&) * 8 - length($`) % 8)/e;
    
    if(/^ *$/) {
      print "\n";
    } else {
      printf "\n(* %-74s *)",$_ unless /^ *$/;
    }
  }
} );

# $Log$
# Revision 1.3  2001/09/10 18:48:18  cl
# - retrieve charset list directly from iana web server
#
# Revision 1.2  2001/09/08 14:55:27  cl
# - More uniform naming of MIME functions/types/consts
# - MimeCharsetCanonicalName now does also canonicalize case
#
# Revision 1.1  2001/04/09 13:18:15  cl
# - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
# - zcrfc.pas: regognition of all known charsets for news and smtp batches
# - typeform.pas: Changed DecodeBase64 from var-procedure to function.
# - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
# - utftools.pas: Optimized Charset recoders
# - utftools.pas: added charset aliases from IANA database
#

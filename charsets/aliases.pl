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

print "(* \$ID: \$ *)\n";
print "(* generated from IANA charset list -- do not edit *)\n\n";

print "function ResolveCharsetAlias(Name: String): String;\nbegin\n";
print "  Name:=LowerCase(Name);\n";

while(<>) {
  chomp;
  if(/^Name: *([^ ]*)/) {
    print "\n";
    printf "  if name=%-63s else\n",
      sprintf "%-25s then result :=%s","'$_'","'$name'" 
      foreach @alias;
    
    $name=$1;
    @alias=();
  } elsif(/^Alias: *([^ ]+) \(.*preferred MIME.*\)/) {
    push @alias,lc($name);
    $name=$1;
  } elsif(/^Alias: *([^ ]+)/) {
    push @alias,lc($1) unless lc($1) eq "none";
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

# $Log$
# Revision 1.1  2001/04/09 13:18:15  cl
# - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
# - zcrfc.pas: regognition of all known charsets for news and smtp batches
# - typeform.pas: Changed DecodeBase64 from var-procedure to function.
# - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
# - utftools.pas: Optimized Charset recoders
# - utftools.pas: added charset aliases from IANA database
#
#

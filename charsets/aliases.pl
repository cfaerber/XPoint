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

my $name=undef;
my $name_pref=0;
my @alias=();

use LWP::UserAgent;
use HTTP::Request;

my $data = undef;
my $started = undef;

my $MODE = 'MIME';

my %more_aliases=();
my %preferred_aliases=();

my @saved=();

open MORE_ALIASES, "<more_aliases.txt";
while(<MORE_ALIASES>) {
  next if /^#/;
  unless(m/^([^\s]+)\s+([^\s]+)\s*$/) { print STDERR "WARNING: malformated input $_"; next; }
  my ($cs,$al)=($1,uc($2));
  if(exists $more_aliases{$al}) { print STDERR "WARNING: re-definition of $al ignored.\n"; next; }
  $more_aliases{$al}=$cs;
}

open PREFERRED_ALIASES, "<preferred_aliases.txt";
while(<PREFERRED_ALIASES>) {
  next if /^#/;
  unless(m/^([^\s]+)\s+([^\s]+)\s*$/) { print STDERR "WARNING: malformated input $_"; next; }
  my ($sc,$re)=(uc($1),$2);
  push @{$preferred_aliases{$sc}}, $re;
}

sub add_name {
  my ($cs) = @_;
  next if $cs =~ m/^none$/i;

  my @pa = (@{$preferred_aliases{uc $MODE}}, @{$preferred_aliases{'ALL'}});
  
  for(my $p=0; ($p<=$#pa) && ((!defined $name_pref)||($p<$name_pref)); $p++) {
    if($cs =~ m/^$pa[$p]$/i) {
      $name = $cs;
      $name_pref = $p;
      last;
    }
  }
  $name ||= $cs;
  push @alias, $cs;
}

sub write_name {
  printf STDERR $name.(($#alias>0)?(" (".($#alias)." aliases)\n"):"\n");
  my $nl = "\n";

  foreach my $alias (keys %more_aliases) {
    unless (grep { uc $alias eq uc $_ } @alias ) {
      if (grep { uc $more_aliases{$alias} eq uc $_ } @alias ) {
        printf "$nl  (* additional alias: %-51s *)\n", $alias;
	add_name($alias);
	$nl = '';
      }
    }
  }	 

  printf "$nl  (* overridden preference for %-44s *)\n", 
    sprintf "%s: %s", $MODE, $name 
    if (defined $name_pref) && $name_pref>=0;

  print  "\n";
  foreach (@alias) {
    printf "  if UName=%-63s else\n",
      sprintf "%-25s then result :=%s","'".uc($_)."'","'$name'";
  };

  @alias = ();
  $name = undef;
  $name_pref = undef;
}	

$UserAgent = LWP::UserAgent->new;
$Request = HTTP::Request->new('GET', 'http://www.iana.org/assignments/character-sets');
$UserAgent->request($Request,sub {

  unless ($started){
    print STDERR "Retrieving data...\n";
    open STDOUT,">aliases.inc";
    print "(* \$Id$ *)\n";
    print "(* generated from IANA charset list -- do not edit *)\n\n";
    print "function MimeCharsetCanonicalName(Const Name: String): String;\nvar UName: String;\nbegin\n";
    print "  UName:=UpperCase(Name);";
    $started=1;
  }

  $data.=shift;
  while($data=~s/^([^\r\n]*)\r?\n//s) {
    $_=$1;
  
    if (/^Name: *([^ ]*)/) 
    {
      write_name();
      add_name($1);
    } elsif(/^Alias: *([^ ]+) +\(.*preferred MIME.*\)/) {
      $name=$1;
      $name_pref = -1 if $MODE eq 'MIME';
      push @alias,$name;
    } elsif(/^Alias: *([^ ]+)/) {
      add_name($1);
    } elsif(/^REFERENCES[ \t]*$/) {
      write_name();
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
# Revision 1.7  2003/09/27 00:43:00  cl
# Fixed aliases for Latin-7..10/ISO-8859-13..16
#
# Revision 1.6  2003/08/24 19:11:51  cl
# - better aliases for charsets
# - additional aliases for charsets not registered w/ IANA (e.g. x-mac-roman)
#
# Revision 1.5  2003/01/07 00:22:28  cl
# - made parameter const
#
# Revision 1.4  2002/03/25 18:42:24  cl
# - fixed detection of preferred MIME charsets
#
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

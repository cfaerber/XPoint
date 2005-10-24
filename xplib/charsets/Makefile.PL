#!perl 
# $Id: Makefile.PL 6397 2003-09-29 20:47:21Z cl $
#
# aliases.pl - Converts IANA charset list to PASCAL aliases.inc
#              and mimenum.inc
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
my $mib = undef;
my $name_var = undef;
my @alias=();

use LWP::UserAgent;
use HTTP::Request;

my $data = undef;
my $started = undef;

my $MODE = 'MIME';

my %more_aliases=();
my %more_mibenum=();
my %preferred_aliases=();

my @saved=();

open MORE_ALIASES, "<more_aliases.txt";
while(<MORE_ALIASES>) {
  next if /^#/ || /^\s+$/;
  unless(m/^([^\s]+)\s+([^\s]+)\s*$/) { print STDERR "WARNING: malformated input $_"; next; }
  my ($cs,$al)=($1,uc($2));
  if(exists $more_aliases{$al}) { print STDERR "WARNING: re-definition of $al ignored.\n"; next; }
  $more_aliases{$al}=$cs;
}

open MORE_MIBENUM, "<more_mibenum.txt";
while(<MORE_MIBENUM>) {
  next if /^#/ || /^\s+$/;
  unless(m/^([^\s]+)\s+([^\s]+)\s*$/) { print STDERR "WARNING: malformated input $_"; next; }
  my ($cs,$al)=($1,uc($2));
  if(exists $more_mibenum{$al}) { print STDERR "WARNING: re-definition of $al ignored.\n"; next; }
  $more_mibenum{$al}=$cs;
}


open PREFERRED_ALIASES, "<preferred_aliases.txt";
while(<PREFERRED_ALIASES>) {
  next if /^#/ || /^\s+$/;
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

  if(!$mib) {
    foreach my $mib_add (keys %more_mibenum) {
      if (grep { uc $more_mibenum{$mib_add} eq uc $_ } @alias ) {
	$mib = $mib_add;
  } } }

  printf "$nl  (* overridden preference for %-44s *)\n", 
    sprintf "%s: %s", $MODE, $name 
    if (defined $name_pref) && $name_pref>=0;
  print  "\n";

  foreach(@alias) { if(m/^cs/) { $name_var = $_; last; } };
  $name_var = 'cs'.$name unless $name_var;

  $name_var =~ s/[^[:alnum:]_]+/_/g;
  
  foreach (@alias) {
    printf ALIAS "  if UName=%-63s else\n",
      sprintf "%-25s then result :=%s","'".uc($_)."'","'$name'";
    printf MIBENUM "  if UName=%-63s else\n",
      sprintf "%-25s then result := %s","'".uc($_)."'",$name_var || $mib;
  };

  my $len = length($name_var) + length($mib)*2;
  printf MIBCONST "  %-27s{\$IFDEF FPC} : TMimeCharset = %4d; {\$ELSE} = %4d,{\$ENDIF}\n",
    $name_var, 
    $mib, $mib if $name;

  @alias = ();
  $name = undef;
  $name_pref = undef;
  $name_var = undef;
  $mib = undef;
}

sub aliases::tie::TIEHANDLE { bless {}, shift; }
sub aliases::tie::PRINT { shift; print ALIAS (@_); print MIBENUM (@_); }
sub aliases::tie::PRINTF { shift->PRINT(sprintf(shift,@_)) };

$UserAgent = LWP::UserAgent->new;
$Request = HTTP::Request->new('GET', 'http://www.iana.org/assignments/character-sets');
$UserAgent->request($Request,sub {

  unless ($started){
    print STDERR "Retrieving data...\n";

    open ALIAS,">aliases.inc";
    open MIBENUM,">mibenum.inc";
    open MIBCONST,">mibconst.inc";

    untie *STDOUT;
    tie *STDOUT, 'aliases::tie';

    print "(* \$Id: Makefile.PL 6397 2003-09-29 20:47:21Z cl $ *)\n";
    print "(* generated from IANA charset list -- do not edit *)\n\n";
    
    print MIBCONST "(* \$Id: Makefile.PL 6397 2003-09-29 20:47:21Z cl $ *)\n";
    print MIBCONST "(* generated from IANA charset list -- do not edit *)\n\n";
    print MIBCONST "type\n  TMimeCharset = {\$IFDEF FPC} 0..2999; const {\$ELSE} ( {\$ENDIF}\n";

    print ALIAS   "function MIMECharsetCanonicalName(Const Name: String): String;\nvar UName: String;\nbegin\n";
    print MIBENUM "function GetCharsetFromName(Const Name: String): TMIMECharset;\nvar UName: String;\nbegin\n";
    
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
    } elsif(/^MIBEnum: *([[:digit:]]+)/i) {
      $mib = $1;
    } elsif(/^Alias: *([^ ]+) +\(.*preferred MIME.*\)/) {
      $name=$1;
      $name_pref = -1 if $MODE eq 'MIME';
      push @alias,$name;
    } elsif(/^Alias: *([^ ]+)/) {
      add_name($1);
    } elsif(/^REFERENCES[ \t]*$/) {
      write_name();
      print ALIAS "\n  result:=name;\nend;\n";
      print MIBENUM "\n  result:=name;\nend;\n";

      print MIBCONST "  csUnknown                  {\$IFDEF FPC} : TMimeCharset =    0; {\$ELSE} =  0 );{\$ENDIF}\n"
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


# $Log: Makefile.PL,v $
# Revision 1.1  2003/09/29 20:47:19  cl
# - moved charset handling/conversion code to xplib
#
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

#!perl

open LBRK, "<linebreak.txt";

my $rclass = undef;
my $rstart = 0;
my $rstop = 0;

%class_map = (
  'AI' => 'AL',	# no East Asian Width
  'XX' => '',   # don't have in table
  'SG' => '',
  'CB' => '',
  'SA' => '',	# Treat as unknown
);

while(<LBRK>) {
  m/([0-9A-F]+)(\.\.([0-9A-F]+))?;([A-Z0-9]*)/;
  my $start = $1;
  my $stop = $3 || $start;
  my $class = $4; $class = $class_map{$class} if exists $class_map{$class};

  if($class ne $rclass) {
    push @lines,sprintf "  (start: \$%s; stop: \$%s; line_break_type: UNICODE_BREAK_%s)",
      $rstart, $rstop,$rclass if $rclass;
    $rstart = $start;
    $classes{'UNICODE_BREAK_'.$rclass}++ if $rclass;
  }
  $rstop = $stop;
  $rclass = $class;
}

open STDOUT,">linebreak.inc";

print "(* \$"."Id: LineBreak.inc \$ *)\n";
print "(* generated from Unicode Database -- do not edit *)\n\n";

# print  "type TUnicodeLineBreakType = (\n  ";
# print  join ",\n  ", (sort keys %classes), (map {$_ = 'UNICODE_BREAK_'.$_; } @class_add);
# print  " );\n\n";

printf "const UnicodeLineBreakTypes: packed array[0..%d] of packed record\n".
       "  start,stop: TUnicodeChar;\n".
       "  line_break_type: TUnicodeLineBreakType;\n".
       "end = (\n", $#lines;
print join ",\n",@lines;
print  " );\n\n";

# Runs mpts.run for files 001 > 100
$dir = "C:/Research/iw";


open (TEMPLATE, "<$dir/tntscript/template.run") or warn "ERROR: can't find template file at $dir/tntscript/template.run";
@template = <TEMPLATE>;
close TEMPLATE;

foreach my $i (1..100) {
  $fileno = sprintf("%03d", $i);
  print "\n processing $fileno...";
  $scriptfile = "$dir/tntscript/run$fileno.run";
  open (SCRIPT, ">", $scriptfile) or warn "Can't open script file";
  for (@template) {
    $line = $_;
    $line =~ s/001\./$fileno\./g;
    print SCRIPT $line;
  }
  close SCRIPT;
  system("tnt $scriptfile");
}
do "tnt2nex.pl";

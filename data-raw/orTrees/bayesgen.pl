# Runs mpts.run for files 001 > 100
$dir = "C:/Bayes64/iwor";
open (MBB, "<$dir/mrbayesblock.nex") or warn "ERROR: can't find template file at $dir/mrbayesblock.nex";
@template = <MBB>;
close MBB;

foreach my $k (1..10) {
  foreach my $j (1..100) {
    foreach my $i (100, 350, 1000) {
      $fileno = $i . '_' . $j . '_' . $k;
      $consensus = "$dir/$fileno.mb.nex.con.tre";
      if (-e $consensus) {
        print "\n Output already exists at $consensus";
        if (!(-e "$dir/$fileno.mb.con.tre.nex")) {
          print " but hasn't been converted to R-readable format";
          open (CONSENSUS, "<$consensus") or warn ("Can't open consensus trees at $consensus");
          @lines = <CONSENSUS>;
          close CONSENSUS;
          open (CONSOUT, ">$dir/$fileno.mb.con.tre.nex") or warn ("Can't create new consensus tree file");
          for (@lines) {
            s/:[\d\.e\-]+//g;
            s/\[[^\]]+\]//g;
            print CONSOUT;
          }
          close CONSOUT;
        }
      } else {
        print "\n " . localtime . ": Processing $fileno...";
        open (MATRIX, "<$dir/$fileno.NEX") or warn "Can't open sourcefile $dir/$fileno.NEX";
        @matrix = <MATRIX>;
        close (MATRIX);
        $splicefile = "$dir/$fileno.mb.nex";
        open (SPLICE, ">", $splicefile) or warn "Can't open output file $dir/$fileno.mb.nex";
        for (@matrix) {
          print SPLICE;
        }
        for (@template) {
          print SPLICE;
        }
        close SPLICE;
        system("C:/Bayes64/MrBayes/mrbayes_x64.exe $splicefile");
        
        open (CONSENSUS, "<$consensus") or warn ("Can't open consensus trees");
        @lines = <CONSENSUS>;
        close CONSENSUS;
        open (CONSOUT, ">$dir/$fileno.mb.con.tre.nex") or warn ("Can't create new consensus tree file");
        for (@lines) {
          s/:[\d\.e\-]+//g;
          s/\[[^\]]+\]//g;
          print CONSOUT;
        }
        close CONSOUT;
      }
    }
  }
}
do "t2nex.pl"; # Convert files so R can read them

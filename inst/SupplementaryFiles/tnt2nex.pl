use File::Find;
# Converts the .tre outputs produced by TNT runs to nexus files
$dir = "C:/Research/iw/Trees";


@taxa = ();
print " Reading directory ". $dir;
find (\&tnt2nex, $dir . "/");
@lines = <NEXSRC>;
close NEXSRC;
print "\nDone.";

sub tnt2nex() {
	if (-f and /\.tre|\.tnttree$/) {
    $infile = $_;
    $outfile = $_;
    $outfile =~ s/\.\w+$/.nex/i;
    $i = 0;
    undef(@taxa);
    print "\n- Processing $infile ... ";
    
    open (FILE, "<$infile") or warn " ERROR: Can't find TNT trees list $dir/$infile.\n";
    @lines = <FILE>;
    close FILE;
    
    shift(@lines);
    pop(@lines);
    
    open (OUTPUT, ">$outfile")  or warn "!! Can't open $outfile: $!\n";
    print OUTPUT "#NEXUS\nbegin taxa;\n\tdimensions ntax=22;\n\ttaxlabels";
    for (1..22) {
      print OUTPUT "\n\t\t" . $_;
    }
    print OUTPUT "\n\t;\nend;\nbegin trees;\n";
    for (@lines) {
      ++$i;
      # Replace taxon numbers with taxon names
      # Oddly, the numbers are not given in numerical order in the source files (!)
      s/(?<!\d)0(?![\d~])/1~/g;
      s/(?<!\d)1(?![\d~])/2~/g;
      s/(?<!\d)2(?![\d~])/3~/g;
      s/(?<!\d)3(?![\d~])/4~/g;
      s/(?<!\d)4(?![\d~])/5~/g;
      s/(?<!\d)5(?![\d~])/6~/g;
      s/(?<!\d)6(?![\d~])/7~/g;
      s/(?<!\d)7(?![\d~])/8~/g;
      s/(?<!\d)8(?![\d~])/9~/g;
      s/(?<!\d)9(?![\d~])/10~/g;
      s/(?<!\d)10(?![\d~])/11~/g;
      s/(?<!\d)11(?![\d~])/12~/g;
      s/(?<!\d)12(?![\d~])/19~/g;
      s/(?<!\d)13(?![\d~])/18~/g;
      s/(?<!\d)14(?![\d~])/17~/g;
      s/(?<!\d)15(?![\d~])/15~/g;
      s/(?<!\d)16(?![\d~])/16~/g;
      s/(?<!\d)17(?![\d~])/13~/g;
      s/(?<!\d)18(?![\d~])/14~/g;
      s/(?<!\d)19(?![\d~])/20~/g;
      s/(?<!\d)20(?![\d~])/21~/g;
      s/(?<!\d)21(?![\d~])/22~/g;
      s/~//g;
      
      s/(\d+) /\1, /g;
      s/,? +\)/\)/g;
      # Annotations
      s/(=\S*)\//$1;/g;
      s/=(\S+)/[&Annot="$1"]/g;
      s/;/; /g;
      # Place commas between clades
      s/\)\(/),(/g;
      s/\]\s*\(/], (/g;
      # Separate multiple trees
      s/[\*; ]+;?$/;/;
      # Name trees
      s/^(.*?)([\d\w_]+)/tree tree$i = [&U] $1$2/;
      print OUTPUT "\t" . $_;
    }
    print OUTPUT "\nend;";
    close (OUTPUT);
    print " " . 22 . " taxa numbered.";
  }
}
use File::Find;
use File::stat;
use Time::localtime;
print "\r\nt2nex.pl: Converts the .tre outputs produced by MrBayes runs to nexus files\r\n";
#$dir = "C:/Research/iw/Trees"; # TODO REPLACE
$dir = "C:/Bayes64/iw";
@taxa = ();
print " Reading directory ". $dir;
find (\&t2nex, $dir . "/");
@lines = <NEXSRC>;
close NEXSRC;
print "\nDone.";

sub t2nex() {
	if (-f and /\.run\d+\.t$/) {
    $infile = $_;
    $outfile = $_;
    $outfile =~ s/\.t$/.nex/i;
    $outfile = "nexTrees/" . $outfile;
    $i = 0;
    if (1 || !(-e $outfile) || -C $infile < -C $outfile) {
      print "\n- Processing $infile: ";
      print "[+] " if (!(-e $outfile));
      open (FILE, "<$infile") or warn " ERROR: Can't find MrBayes trees list $dir/$infile.\n";
      @lines = <FILE>;
      close FILE;
      open (OUTPUT, ">$outfile")  or warn "!! Can't open $outfile: $!\n";
      for (@lines) {
        ++$i;
        if ($i < 5) {
          print OUTPUT; 
        } elsif ($i > 428) {
          s/:[^,\(\)]+//g;
          # Replace taxon numbers with taxon names
          s/(?<!\d)1(?![\d~])/1~/g;
          s/(?<!\d)2(?![\d~])/2~/g;
          s/(?<!\d)3(?![\d~])/3~/g;
          s/(?<!\d)4(?![\d~])/4~/g;
          s/(?<!\d)5(?![\d~])/5~/g;
          s/(?<!\d)6(?![\d~])/6~/g;
          s/(?<!\d)7(?![\d~])/7~/g;
          s/(?<!\d)8(?![\d~])/8~/g;
          s/(?<!\d)9(?![\d~])/9~/g;
          s/(?<!\d)10(?![\d~])/10~/g;
          s/(?<!\d)11(?![\d~])/11~/g;
          s/(?<!\d)12(?![\d~])/12~/g;
          s/(?<!\d)13(?![\d~])/19~/g;
          s/(?<!\d)14(?![\d~])/18~/g;
          s/(?<!\d)15(?![\d~])/17~/g;
          s/(?<!\d)16(?![\d~])/15~/g;
          s/(?<!\d)17(?![\d~])/16~/g;
          s/(?<!\d)18(?![\d~])/13~/g;
          s/(?<!\d)19(?![\d~])/14~/g;
          s/(?<!\d)20(?![\d~])/20~/g;
          s/(?<!\d)21(?![\d~])/21~/g;
          s/(?<!\d)22(?![\d~])/22~/g;
          s/~//g;
          
          s/(\d+) /\1, /g;
          s/,? +\)/\)/g;
          ## Annotations
          #s/(=\S*)\//$1;/g;
          #s/=(\S+)/[&Annot="$1"]/g;
          #s/;/; /g;
          #
          ## Place commas between clades
          #s/\)\(/),(/g;
          #s/\]\s*\(/], (/g;
          ## Separate multiple trees
          #s/[\*; ]+;?$/;/;
          # Name trees
          # s/^(.*?)([\d\w_]+)/tree tree$i = [&U] $1$2/;
          s/(gen.\d+),/\1/g;
          print OUTPUT "\t" . $_;
        }
      }
      print OUTPUT "\nend;";
      close (OUTPUT);
    } else {
      print "\nSkipping existing files, at $infile" if (rand(1) < 0.001);
    }
  }
}
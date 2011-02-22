#!/usr/bin/perl

my @sets = ([200, 0, 2, 0, 15, 0, 0.03, 0.04],
	 [100, 0, 10, 50, 10, 50, 0.03, 0.05],
	 [400, 2.5, 10, 20, 5, 25, 0.04, 0.03],
	 [100, 0, 6, 10, 10, 50, 0.03, 0.05],
	 [100, 12.5, 0, 30, 0, 50, 0.04, 0.02],
	 [0, 0, 6, 20, 25, 0, 0.02, 0.05],
	 [500, 2.5, 8, 0, 15, 75, 0.05, 0]);

$num = $ARGV[0];

for($f=0; $f<7; $f++){
    for($s=$f+1; $s<7; $s++){
	open(FILE, ">$f-to-$s.params");
	my @first = @{$sets[$f]};
	my @second = @{$sets[$s]};

	for($i=0;$i<$num;$i++){
	    my $line = "";
	    for($j=0;$j<@first;$j++){
		my $val = $i*($second[$j] - $first[$j])/$num + $first[$j];
		$line = "$line$val ";
	    }
	    chop($line);
	    print FILE "$line\n";
	}
	close(FILE);
    }
}

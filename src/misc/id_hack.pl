#! /usr/bin/perl
# Generated automatically from id_hack.pl.in by configure.
$mod="";
while(<>) {
    if (m/#define _(\w+)_/) {
	$mod = $1;
    } elsif (m/^#/) {
    } elsif(m/${mod}_(\w+)_s/g) { # ignore _s
    } elsif(m/${mod}_(\w+)/g && $mod) {
         $data{"${mod}_$1"} = $1;
    }
}

foreach $i (sort(keys %data)) {
    print("#define $data{$i} $i\n");
}

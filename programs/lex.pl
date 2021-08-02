#!/usr/bin/perl
use strict;
use warnings;

my %hash;
while (my $line = <>) {
   while ($line =~ s/\w+//) {
      ++$hash{$&}
   }
}

for my $key (sort keys %hash) {
   print "$key $hash{$key}\n";
}

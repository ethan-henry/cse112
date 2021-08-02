#!/usr/bin/perl
use strict;
use warnings;

my $lines = 0;
my $words = 0;
my $chars = 0;

while (defined (my $line = <>)) {
   ++$lines;
   $chars += length $line;
   while ($line =~ s/\S+//) { ++$words }
}

print "$lines $words $chars\n";

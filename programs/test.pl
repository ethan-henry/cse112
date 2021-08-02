#!/usr/bin/perl

# $RCSfile: xref-words.perl,v $$Revision: 1.1 $

map { $hash{lc $_} .= " $." } m/(\w+)/g while <>;
map { print "$_$hash{$_}\n" } sort keys %hash;      

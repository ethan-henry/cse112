#!/usr/bin/perl -w

use strict;
use warnings;

my $filedir = "/private/";


opendir(my $dir, $filedir) or die "Could not load directory";
my @read_dir = sort grep {!-d}readdir($dir);
foreach my $fileInDir(@read_dir)
{

    my $currentDestination = "$filedir/$fileInDir";
    my $filesize = -s $currentDestination;

    if(-z $currentDestination)
    {
        print "Zero -  Filename: $fileInDir Size: $filesize\n";
    }
    else
    {
        print "Non - Zero Filename: $fileInDir Size: $filesize\n";
    }
}

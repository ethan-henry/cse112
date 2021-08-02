#!/usr/bin/perl

#NON-TAIL RECURSIVE
sub fac {
    my $num = $_[0];
    if ($num <= 1 ) {
        return 1;
    } else {
        return $num * fac($num - 1);
    }
}

#TAIL RECURSIVE
sub factorial {
    my ($num) = @_;
    sub f {
        my ($num, $acc) = @_;
        if ($num <= 1) {
            return $acc;
        } else {
            f( ($num - 1), ($acc * $num) );
        }
    }
    my $output = f($num, 1);
    print "$output\n";
    return($output);
}

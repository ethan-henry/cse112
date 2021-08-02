#!/usr/bin/perl

sub binarysearch {
    my ($less, $num, @arr) = @_;
    if ((@arr[2] == undef) or (@arr[3] == undef)) {
        return(undef);
    } elsif ($less ($arr[0] $num)) {
        binarysearch($less, $num, @{@arr[3]}); 
    } elsif ($less ($num $arr[0])) {
        binarysearch($less, $num, @{@arr[2]});
    } elsif ($arr[0] == $num) {
        return(@arr[1]);
}

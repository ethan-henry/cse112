sub findpred {
    my ($fun, @arr) = @_;
    for my $i (@arr) {
        my @elem = $i;
        if ($fun (@elem[0])) {
            return($elem[1]);
        }
    }
    return undef; 
}

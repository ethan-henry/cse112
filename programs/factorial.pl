sub factorial {
    my $acc = 1;
    for (my $x = @_; $x > 0; $x = $x + 1) {
        $acc = $acc * $x;
    }
    return($acc);
}

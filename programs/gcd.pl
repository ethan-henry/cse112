sub gcd ($$) {
   my ($x, $y) = @_;
   while ($x != $y) {
      if ($x > $y) {$x -= $y} else {$y -= $x}
   }
   return $x
}

#IFFY
sub gcd {
    my ($x, $y) = @_;
    my $big = max($x, $y);
    my $small = min($x, $y);
    while ($big % $small != 0) {
        my $temp = $big - $small;
        $big = max($temp, $small);
        $small = min($temp, $small);
    }
    return $small;
}

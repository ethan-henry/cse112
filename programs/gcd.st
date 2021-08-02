Integer extend [
   gcd: other [
      |x y|
      x := self.
      y := other.
      [x ~= y] whileTrue: [
         x > y ifTrue: [x := x - y]
               ifFalse: [y := y - x]
      ].
      ^ x
   ]
].


"IFFY"
gcd := [:a :b|
    |max min|
    max := a max: b.
    min := a min: b.
    
    [(max mod: min) = 0] whileFalse: [ |temp|
                         temp := max - min.
                         max := temp max: min.
                         min := temp min: min. ].
    ^ min].

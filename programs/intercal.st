Array extend [
   innerprod: other [
      self size = other size
      ifFalse: [
         ^ self error: 'innerprod different sizes'
      ]
      ifTrue: [
         |sum|
         sum := 0.
         1 to: self size do: [:i|
            sum := sum + (self at: i) * (other at: i) "ACTUAL CALCULATION PART"
         ].
         ^ sum
      ]
   ]
]

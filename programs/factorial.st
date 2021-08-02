|acc|
acc := 1.

self to: 0 do: [ :i|
    acc := acc * i.
]

^ acc.

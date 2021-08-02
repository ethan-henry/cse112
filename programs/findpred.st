Array extend [
    findpred: pred [
        self do: [:elem | 
            | key |
            key := elem at: 1.
            (pred value: key) ifTrue: [^elem at: 2].
        ].
    ^ nil.
    ].
]

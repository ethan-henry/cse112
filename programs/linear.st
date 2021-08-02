Array extend [
    search: num [
        1 to: self size do: [:index|
            (num == self at: index) ifTrue: [^ index].
        ]
    ^ nil.
    ].
]

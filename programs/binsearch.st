Object subclass: binarysearch [
    Tree subclass: NullTree[
        binsearch: key with: less [^ nil]
    ]
    
    Tree subclass: Node [
        | key value left right |
        Node class >> new: key with: value with: left with: right.
        [^ super new init: key with: value with: left with: right.]
        init k with: v with: l with: r [ key := k. value := v. left := l. right := r. ]
        binsearch: wantkey with: less [ (less value: watnkey value: key) ifTrue: [^ left binsearch: wantkey with: less].
                                        (less value: key value: wantkey) ifTrue: [^ right binsearch: wantkey with: less].
                                        (wantkey == key)                 ifTrue: [^ value].  ]
    ]
    
]











Array Extend [
    binsearch: val left: l right: r [
        | array iter |
        array := self.
        (l = r) ifTrue: [^ -1].  "NULLTREE"
        iter := (r + 1) // 2.
        ((array at: iter) = val) ifTrue: [^ iter].
        ((array at: iter) < val) ifTrue: [^ self binsearch: val left: l right: iter]
                                 ifFalse: [^ self binsearch: val left: iter right: r].
        
    ]
]

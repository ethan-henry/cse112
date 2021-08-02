#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: mbint.st,v 1.16 2021-02-11 19:21:43-08 - - $"

Array extend [
    filter [ |i x|
        i := 1.
        x := Array new: self size.
        1 to: self size do: [ :index|
            ((self at: index) > 1) ifTrue: [x at: i put: (self at: index). i printNl. i := i + 1].
    ].
    ^ x.
]
]
array := {1. 5. 10. 15. 2}.

stdout << array filter << Character nl.

#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f

"ANOTHER FACTORIAL"
  
    Integer extend 
        [factorial [ ^ (self <= 1)
        ifTrue: [1]
        ifFalse: [self * (self - 1) factorial]]
        ].


    (10 factorial) printNl.

#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr


(define (fac num) 
(define (factorial num acc)
    (if (<= num 0) acc
    (factorial (- num 1) (* acc num))))
    (factorial num 0))

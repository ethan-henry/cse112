#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr

(define (max_ a b) (if (> a b) a b))

(define (max a num)
(if (empty? a)
(#f)
(max (cdr a) (max_ (car a) num))))

(define (main)
(printf "~a~n" (max '(1 4 5 2 5 6) 1) ))

(main)

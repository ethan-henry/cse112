#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr

(define (filter ok? lst)
    (if (null? lst) '()
        (let ((a (car lst))
              (d (cdr lst)))
             (if (ok? a) (cons a (filter ok? d))
                         (filter ok? d)))))

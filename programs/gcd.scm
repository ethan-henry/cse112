(define (gcd x y)
    (cond ((> x y) (gcd (- x y) y))
          ((< x y) (gcd x (- y x)))
          (else x)))


; IFFY
(define (gcd x y)
    (define (foo a b)
        (if (= (mod a b) 0) b
            (let ((temp (- a b)))
                 (foo (max temp b) (min temp b)))))
    (foo (max x y) (min x y)))

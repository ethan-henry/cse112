(define (find cmp key list)
    (cond ((null? list) #f)
          ((cmp key (caar list)) (cadar list))
          (else (find cmp key (cdr list)))))

#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: mbir.scm,v 1.9 2021-01-12 11:57:59-08 - - $
;;
;;BY:
;;    Ethan Henry (efhenry)
;;    Hansel Rahardjo (hrahardj)
;;
;; NAME
;;    mbir.scm filename.mbir
;;
;; SYNOPSIS
;;    mbir.scm - mini basic interper
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an mbir
;;    program, which is the executed.  Currently it is only printed.
;;

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))

(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))
(define *var-table*      (make-hash))
(define *array-table*    (make-hash))
(define *label-table*    (make-hash))

(for-each (lambda (var) (hash-set! *var-table* (car var) (cadr var)))
   `(
        (e    ,(exp 1.0))
        (eof  0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    ))

(for-each (lambda (var) 
    (hash-set! *function-table* (car var) (cadr var)))
        `( 
        (+ ,+)
        (- ,-)
        (* ,*)
        (/ ,/)
        (^ ,expt)
        (sqr ,sqr) 
        (abs ,abs)
        (acos ,acos)
        (asin ,asin)
        (atan ,atan)
        (ceil ,ceiling)
        (cos ,cos)
        (exp ,exp)
        (floor ,floor)
        (log ,log)
        (log10 ,log 10)
        (round ,round)
        (sin ,sin)
        (sqrt ,sqrt)
        (tan ,tan)
        (trunc ,truncate)
        (= ,=)
        (<= ,<=)
        (< ,<)
        (>= ,>=)
        (> ,>)
        (!= ,(lambda (x y) (not (= x y))))
        (asub ,(lambda (x y) (vector-ref 
            (hash-ref *array-table* (exact-round x)) y)))
        ))

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

(define NAN (/ 0.0 0.0))

(define (eval-expr expr)
    (cond ((number? expr) (+ expr 0.0))
          ((and (pair? expr) (equal? (car expr) 'asub))
                (vector-ref (hash-ref *array-table* (cadr expr)) 
                 (eval-expr (caddr expr))))
          ((symbol? expr) (hash-ref *var-table* expr 0.0))
          ((pair? expr) 
              (let ((func (hash-ref *function-table* (car expr) #f))
                    (opnds (map eval-expr (cdr expr))))
                   (if (not func) NAN
                       (apply func opnds) )))
          (else (die '("SOMETHING WENT WRONG WITH THE EXPRESSION")))))

(define (interp-dim args continuation)
    (hash-set! *array-table* (cadar args) 
        (make-vector (exact-round (eval-expr (caddar args))) 0.0))
    (interp-program continuation))

(define (interp-let args continuation)
        (cond
            ((pair? (car args))
                (if (hash-has-key? *array-table* (cadar args))
                    (vector-set! (hash-ref *array-table* (cadar args)) 
                    (eval-expr (caddar args)) 
                    (eval-expr (cadr args)))
                (die '("NO ARRAY WITH NAME FOUND"))))
            ((pair? args)
                (hash-set! *var-table*  (car args) 
                (eval-expr (cadr args)))))
    (interp-program continuation))

(define (interp-goto args continuation)
        (cond
            ((hash-has-key? *label-table* (car args))
            (interp-program (hash-ref *label-table* (car args))))
        (else (die '("LABEL NOT FOUND")))))

(define (interp-if args continuation)
        (let ([ans (eval-expr (car args))]) 
            (if ans 
                (interp-goto (list (cadr args)) continuation) 
                (interp-program continuation) )))


(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

(define (interp-input args continuation)
        (if (> (length args) 0)
        (let ([object (read)])
            (cond
                ((eof-object? object) 
                    (begin (hash-set! *var-table* 'eof 1) 
                    (eof_handle args continuation)))
                ((number? object) 
                    (begin (hash-set! *var-table* (car args) object) 
                        (interp-input (cdr args) continuation)))
                (else 
                    (begin (hash-set! *var-table* (car args) 
                    (/ 0.0 0.0))
                        (interp-input (cdr args) continuation)))))
        (interp-program continuation)))

(define (eof_handle args continuation)
        (if (> (length args) 0)
            (begin (hash-set! *var-table* (car args) (/ 0.0 0.0)) 
            (eof_handle (cdr args) continuation))
        (interp-program continuation)))

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))

(define (interp-program program)
    (when (not (null? program))
          (let ((line (line-stmt (car program)))
                (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))

(define (scan-for-labels program)
        (when (not (null? program))
            (let ((line (car program)) 
            (cont (cdr program)))
            (when (not (null? (cdr line))) 
            (when (not (pair? (cadr line)))  
            (hash-set! *label-table* (cadr line) 
            (cons (car program) cont))))
            (scan-for-labels cont) )))

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                       (scan-for-labels program)
                       (interp-program program))))))

(main *ARG-LIST*)


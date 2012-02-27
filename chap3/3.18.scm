(define (has-cycle? x)
  (define (go-on? p1 p2)
    (if (or (null? p2)
            (null? (cdr p2))
            (null? (cddr p2)))
        #f
        #t))
  (define (test p1 p2)
    (if (eq? p1 p2)
        #t
        (if (go-on? p1 p2)
            (test (cdr p1) (cddr p2))
            #f)))
  (if (or (null? x)
          (null? (cdr x))
          (null? (cddr x)))
      #f
      (test x (cddr x))))
  
(define (make-cycle x)
  (define (last-pair s)
    (if (null? (cdr s))
        s
        (last-pair (cdr s))))
  (set-cdr! (last-pair x) x)
  x)
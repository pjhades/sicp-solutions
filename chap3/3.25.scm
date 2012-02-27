; just do the one-dimensional table part

(define (make-table)
  (list '*table*))

(define (lookup keys table)
  (let ((record (assoc keys (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert! keys value table)
  (let ((record (assoc keys (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons keys value)
                              (cdr table)))))
  'ok)

(define (print t)
  (define (pk ptr)
    (if (null? ptr)
        (display " --> ")
        (begin (display (car ptr))
               (display " ")
               (pk (cdr ptr)))))
  (define (p ptr)
    (if (null? ptr)
        'ok
        (begin (pk (caar ptr))
               (display (cdar ptr))
               (newline)
               (p (cdr ptr)))))
  (p (cdr t)))
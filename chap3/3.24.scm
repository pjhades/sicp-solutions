(define (make-table same-key?)
  (define (assoc key records)
        (cond ((null? records) #f)
              ((same-key? key (caar records)) (car records))
              (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    ;--------------------------------------
    (define (lookup key-1 key2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (_assoc key-2 (cdr subtable))))
              (if record
                  (cddr record)
                  #f))
            #f)))
    ;--------------------------------------
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    ;--------------------------------------
    (define (print)
      (define (inner ptr)
        (if (null? ptr) 'ok (begin (display "    ")
                                   (display (caar ptr))
                                   (display ": ")
                                   (display (cdar ptr))
                                   (newline)
                                   (inner (cdr ptr)))))
      (define (outer ptr)
        (if (null? ptr) 'ok (begin (display (caar ptr))
                                   (display ":")
                                   (newline)
                                   (inner (cdar ptr))
                                   (outer (cdr ptr)))))
      (outer (cdr local-table)))
    ;--------------------------------------
    (define (dispatch tag)
      (cond ((eq? tag 'lookup) lookup)
            ((eq? tag 'insert!) insert!)
            ((eq? tag 'print) (print))
            (else "wrong tag")))
    dispatch))

(define t (make-table (lambda (x y) (or (= x 1) (>= y 5)))))
((t 'insert!) 1 4 35)
((t 'insert!) 1 5 47)
((t 'insert!) 1 6 103)
((t 'insert!) 2 7 -4)
((t 'insert!) 2 8 21)
(t 'print)
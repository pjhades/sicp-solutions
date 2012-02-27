; deque implemented with double-linked list
(define (make-deque) (cons '() '()))
(define (front-ptr d) (car d))
(define (rear-ptr d) (cdr d))
(define (set-front-ptr! d value) (set-car! d value))
(define (set-rear-ptr! d value) (set-cdr! d value))

; predicate
(define (empty-deque? d)
  (and (null? (front-ptr d))
       (null? (rear-ptr d))))

; selectors
(define (front-deque d)
  (car (front-ptr d)))

(define (rear-deque d)
  (car (rear-ptr d)))

; mutators
(define (front-insert-deque! d item)
  (let ((new (cons item (cons '() '()))))
    (if (empty-deque? d)
        (begin (set-front-ptr! d new)
               (set-rear-ptr! d new))
        (begin (set-cdr! (cdr new) (front-ptr d))
               (set-car! (cdr (front-ptr d)) new)
               (set-front-ptr! d new))))
  d)

(define (rear-insert-deque! d item)
  (let ((new (cons item (cons '() '()))))
    (if (empty-deque? d)
        (begin (set-front-ptr! d new)
               (set-rear-ptr! d new))
        (begin (set-car! (cdr new) (rear-ptr d))
               (set-cdr! (cdr (rear-ptr d)) new)
               (set-rear-ptr! d new))))
  d)

(define (front-delete-deque! d)
  (if (empty-deque? d)
      "empty deque"
      (begin (set-front-ptr! d (cddr (front-ptr d)))
             (if (null? (front-ptr d))
                 (set-cdr! d '())
                 (set-car! (cdr (front-ptr d)) '()))
             d)))
      
(define (rear-delete-deque! d)
  (if (empty-deque? d)
      "empty deque"
      (begin (set-rear-ptr! d (cadr (rear-ptr d)))
             (if (null? (rear-ptr d))
                 (set-car! d '())
                 (set-cdr! (cdr (rear-ptr d)) '()))
             d)))
      
; printer
(define (print-deque d)
  (define (print ptr)
    (if (null? ptr)
        (newline)
        (begin (display (car ptr))
               (display " ")
               (print (cddr ptr)))))
  (if (empty-deque? d)
      (newline)
      (print (front-ptr d))))
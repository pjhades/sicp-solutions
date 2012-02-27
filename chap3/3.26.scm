; organize things in a table by trees
(define (make-tree record left right)
  (list record left right))
; record
(define (entry tree) (car tree))
; left branch
(define (lb tree) (cadr tree))
; right branch
(define (rb tree) (caddr tree))

; table constructor: return an empty one
(define (make-table) (list '() '() '()))

; similar to assoc
; here we need some procedure to make comparisons
; between keys: less?
(define (locate key tree less?)
  (cond ((null? tree) #f)
        ((equal? key (car (entry tree))) (entry tree))
        ((less? key (car (entry tree))) (locate key (lb tree) less?))
        (else (locate key (rb tree) less?))))

(define (lookup key tree less?)
  (let ((record (locate key tree less?)))
    (if record
        (cdr record)
        #f)))

(define (insert! key value tree less?)
  (define (seek pre p)
    (cond ((null? p) pre)
          ((equal? key (car (entry p))) p)
          ((less? key (car (entry p))) (seek p (lb p)))
          (else (seek p (rb p)))))
  (if (null? (entry tree))
      ; empty tree
      (set-car! tree (cons key value))
      ; find inserting place
      (let ((ptr (seek '() tree))
            (new (cons key value)))
        (cond ((equal? key (car (entry ptr)))
               (set-cdr! (entry ptr) value))
              ((less? key (car (entry ptr)))
               (set-car! (cdr ptr) (make-tree new '() '())))
              (else
               (set-car! (cddr ptr) (make-tree new '() '()))))))
  'ok)

; test
(define t (make-table))
(insert! 2 20 t <)
t
(insert! 1 10 t <)
(insert! 3 30 t <)
t
(insert! 4 99 t <)
t
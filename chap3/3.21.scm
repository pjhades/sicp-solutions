(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      "empty queue"
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new (cons item '())))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new)
               (set-rear-ptr! queue new))
        (begin (set-cdr! (rear-ptr queue) new)
               (set-rear-ptr! queue new)))
    queue))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      "empty queue"
      (set-front-ptr! queue (cdr (front-ptr queue))))
  queue)

(define (print-queue queue)
  (define (print ptr)
    (if (null? ptr)
        (newline)
        (begin (display (car ptr))
               (display " ")
               (print (cdr ptr)))))
  (print (front-ptr queue)))
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) 
      (null? front-ptr))   
    (define (front-queue)
      (if (empty-queue?)
          "empty queue"
          (car front-ptr)))    
    (define (insert-queue! item)
      (let ((new (cons item '())))
        (if (empty-queue?)
            (begin (set! front-ptr new)
                   (set! rear-ptr new))
            (begin (set-cdr! rear-ptr new)
                   (set! rear-ptr new)))))
    (define (delete-queue!)
      (if (empty-queue?)
          "empty-queue"
          (set! front-ptr (cdr front-ptr))))    
    (define (print-queue)
      (define (print ptr)
        (if (null? ptr)
            (newline)
            (begin (display (car ptr))
                   (display " ")
                   (print (cdr ptr)))))
      (print front-ptr))
    (define (dispatch tag)
      (cond ((eq? tag 'empty-queue?) empty-queue?)
            ((eq? tag 'front-queue) front-queue)
            ((eq? tag 'insert-queue!) insert-queue!)
            ((eq? tag 'delete-queue!) delete-queue!)
            ((eq? tag 'print-queue) print-queue)
            (else "error tag")))
    dispatch))

; Pay attention here:
; since the dispatched procedure has no parameter,
; we need to call them, otherwise the procedure object
; will be returned
(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))
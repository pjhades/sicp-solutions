; ripple-carry adder
; input: A[], B[], c-in
; output: S[], c-out
(define (ripple-carry-adder A B S C)
  (let ((cin (make-wire)))
    (if (null? (cdr A))
        (set-signal! cin 0)
        (ripple-carry-adder (cdr A) (cdr B) (cdr S) cin))
    (full-adder (car A) (car B) cin (car S) C)
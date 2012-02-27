; Here we maintain a list which recording
; the pointers we have seen. We check to
; see if the pointer to the current pair
; is already in the list to detect repetition
(define (count-pairs x)
  (let ((visited '()))
    (define (count s)
      (if (member s visited)
          0
          (begin 
            (set! visited (cons s visited))
            (if (not (pair? s))
                0
                (+ (count (car s))
                   (count (cdr s))
                   1)))))
    (count x)))

(define (count-pairs-wrong x)
  (if (not (pair? x))
      0
      (+ (count-pairs-wrong (car x))
         (count-pairs-wrong (cdr x))
         1)))
; (+ (f 0) (f 1))
; returns 0 if arguments to + are evalutated ->
; returns 1 if arguments to + are evalutated <-

(define f
  (let ((state -1))
    (lambda (x)
      (cond ((= x 0) (set! state x) 0)
            ((= state 0) 0)
            (else x)))))
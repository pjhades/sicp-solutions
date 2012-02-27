; the original rand procedure
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

; modified version
(define (rand method)
  (let ((x random-init))
    (cond ((eq? method 'generate)
           (begin (set! x (rand-update x))
                  x))
          ((eq? method 'reset)
           (lambda (value)
             (begin (set! x value)
                    x)))
          (else "unknown method"))))
(define (avg a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (multiplier x c y)
    (constant 2 x)
    (adder a b y)
    'ok))
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(avg a b c)
(probe 'a-value a)
(probe 'b-value b)
(probe 'c-value c)
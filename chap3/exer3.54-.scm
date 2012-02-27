(require racket/stream)

(define (s-map proc . streams)
  (if (stream-empty? (car streams))
      '()
      (stream-cons
       (apply proc (map stream-first streams))
       (apply s-map 
              (cons proc (map stream-rest streams))))))

(define (add-streams s1 s2)
  (s-map + s1 s2))

(define (mul-streams s1 s2)
  (s-map * s1 s2))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams ones integers)))

(define s (stream-cons 1 (add-streams s s)))

(define (show-first-n s n)
  (if (or (= n 0) (stream-empty? s))
      (newline)
      (begin (display (stream-first s))
             (newline)
             (show-first-n (stream-rest s) (- n 1)))))


;; exercise 3.54
(define factorials (stream-cons 1
                                (mul-streams (add-streams ones integers)
                                             factorials)))

;; exercise 3.55
(define (partial-sums s)
  (define (iter sum stm)
    (if (stream-empty? stm)
        '()
        (stream-cons (+ sum (stream-first stm))
                     (iter (+ sum (stream-first stm))
                           (stream-rest stm)))))
              
  (iter 0 s))


(define (stream-scale s k)
  (stream-map (lambda (x) (* x k)) s))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car
                               (merge (stream-rest s1) s2)))
                 ((< s2car s1car)
                  (stream-cons s2car
                               (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))

;; exercise 3.56
(define S (stream-cons 1 (merge (stream-scale S 2)
                                (merge (stream-scale S 3)
                                       (stream-scale S 5)))))


(define fib
  (stream-cons 0
               (stream-cons 1
                            (add-streams fib (stream-rest fib)))))

(define (expan num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expan (remainder (* num radix) den) den radix)))

;; exercise 3.59 (part a)
(define (integrate-series s)
  (define (iter factor stm)
    (if (stream-empty? stm)
        '()
        (stream-cons (/ (stream-first stm) factor)
                     (iter (+ factor 1) (stream-rest stm)))))
  (iter 1 s))

(define exp-series
  (stream-cons 1
               (integrate-series exp-series)))

;; exercise 3.59 (part b)
;; see how these two streams depend on each other
(define cosine-series
  (stream-cons 1
               (stream-scale (integrate-series sine-series) -1)))

(define sine-series
  (stream-cons 0
               (integrate-series cosine-series)))


;; exercise 3.60
;  
; This problem suddenly becomes easy if you notice a fact.
; Suppose we're calculating (mul-series ones integers), then:
;      x^0  x^1  x^2  x^3 ...
; s1:  1    1    1    1   ...
; s2:  1    2    3    4   ...
;
; The we scale s2 by each element of s1 like:
;      x^0  x^1  x^2  x^3 ...
;      1    2    3    4   ...
;           1    2    3   ...
;                1    2   ...
;                     1   ...
; Looking at the first two lines, the resulting stream is 
; constructed by the first element 1 and the sum of two
; streams, the lowest power of both are x^1.
; 
; The element 1 comes from multiplying the stream-first of two streams,
; while the sum is performed on the rest of that stream and mul-streams
; of the remaining stream and the other stream.
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (add-streams (stream-rest (stream-scale s2 (stream-first s1)))
                            (mul-series (stream-rest s1) s2))))
                
; this should print 1 0 0 ...
;(show-first-n (add-streams (mul-series sine-series sine-series)
;                           (mul-series cosine-series cosine-series))
;              10)


;; exercise 3.61
(define (invert-unit-series s)
  (stream-cons 1
               (mul-series (stream-scale (stream-rest s) -1)
                           (invert-unit-series s))))

; test exercise 3.61
;(show-first-n exp-series 10)
;(define inv (invert-unit-series exp-series))
;(show-first-n inv 10)
;(show-first-n (mul-series inv exp-series) 10)

;; exercise 3.62
(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
      (begin (display "error")
             '())
      (mul-series s1 (invert-unit-series s2))))



(define (improve-guess guess x)
  (define (average a b)
    (/ (+ a b) 2))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (g)
                               (improve-guess g x))
                             guesses)))
  guesses)

;; exercise 3.64
(define (stream-limit s limit)
  (cond ((stream-empty? (stream-rest s)) #f)
        ((< (abs (- (stream-first s)
                    (stream-first (stream-rest s))))
            limit)
         (stream-first (stream-rest s)))
        (else
         (stream-limit (stream-rest s) limit))))

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; exercise 3.65
(define summands
  (stream-map (lambda (x)
                (if (= (remainder x 2) 0)
                    (- (/ 1.0 x))
                    (/ 1.0 x)))
              integers))

(define log2-stream
  (partial-sums summands))

; Extracting the first 10000 terms bounds the result
; between about 0.69309 and 0.69319
;(show-first-n log2-stream 10000)
;(log 2)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (sqr (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

; This time, with Euler transform, extracting the first 
; 1000 terms bounds the result between about 0.6931471804 
; and 0.6931471806, which is much faster and more accurate.
;(show-first-n (euler-transform log2-stream) 1000)
;(log 2)


(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

; (show-first-n (pairs integers integers) 100)

;; exercise 3.66
(define (get-index i j)
  (if (= i j)
      (- (expt 2 i) 1)
      (+ (* (expt 2 i)
            (- j i))
         (- (expt 2 (- i 1)) 1))))


;; exercise 3.67
(define (my-interleave s1 s2 s3)
  (cond ((stream-empty? s1) (interleave s2 s3))
        ((stream-empty? s2) (interleave s1 s3))
        ((stream-empty? s3) (interleave s1 s2))
        (else (stream-cons (stream-first s1)
                           (stream-cons (stream-first s2)
                                        (stream-cons (stream-first s3)
                                                     (my-interleave (stream-rest s1)
                                                                    (stream-rest s2)
                                                                    (stream-rest s3))))))))
(define (pairs-all s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (my-interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (stream-map (lambda (x) (list x (stream-first s)))
                (stream-rest s))
    (pairs (stream-rest s) (stream-rest t)))))

;(show-first-n (pairs-all integers integers) 50)


;; exercise 3.68
; 
; Don't mix lazy evaluation with lazy language. According to Scheme's evaluation
; strategy, parameters are evaluated before evaluating the procedure. So in this
; implementation, the two arguments given to interleave are evaluated first. 
;
; However, with respect to the fact that streams are lazy lists, stream-map won't 
; compute any value since nothing needs it (we haven't begun the evaluation of 
; how-first-n). Unfortunately, the second argument is a recursive call to pairs 
; itself, which will lead to an infinite loop.
;
; The original version works because it throws out a stream whose stream-first is
; the value we need so that the program won't get stuck in recursive call.
(define (pairs-wrong s t)
  (interleave
   (stream-map (lambda (x) (list (stream-first s) x))
               t)
   (pairs-wrong (stream-rest s) (stream-rest t))))

;(show-first-n (pairs integers integers) 10)
;(show-first-n (pairs-another integers integers) 10)


;; exercise 3.69
(define (triples s t u)
  (stream-cons (list (stream-first s)
                     (stream-first t)
                     (stream-first u))
               (interleave (stream-map (lambda (p)
                                         (cons (stream-first s) p))
                                       (pairs t (stream-rest u)))
                           (triples (stream-rest s)
                                    (stream-rest t)
                                    (stream-rest u)))))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (let ((a (car triple))
                         (b (cadr triple))
                         (c (caddr triple)))
                     (= (+ (sqr a) (sqr b)) (sqr c))))
                 (triples integers integers integers)))


;; exercise 3.70
;
(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((w1 (weight (stream-first s1)))
               (w2 (weight (stream-first s2))))
           (cond ((<= w1 w2)
                  (stream-cons (stream-first s1)
                               (merge-weighted (stream-rest s1) s2 weight)))
                 (else
                  (stream-cons (stream-first s2)
                               (merge-weighted s1 (stream-rest s2) weight))))))))

(define (weighted-pairs s t weight)
  (stream-cons (list (stream-first s) (stream-first t))
               (merge-weighted
                (stream-map (lambda (x) (list (stream-first s) x)) 
                            (stream-rest t))
                (weighted-pairs (stream-rest s)
                                (stream-rest t)
                                weight)
                weight)))

;; a)
(define solution-a (weighted-pairs integers integers (lambda (p) (+ (car p) (cadr p)))))
;; b)
(define not-divisible-by-235
  (stream-filter (lambda (x) (not (or (= (remainder x 2) 0)
                                      (= (remainder x 3) 0)
                                      (= (remainder x 5) 0))))
                 integers))
(define solution-b (weighted-pairs not-divisible-by-235
                                   not-divisible-by-235
                                   (lambda (p)
                                     (+ (* 2 (car p))
                                        (* 3 (cadr p))
                                        (* 5 (car p) (cadr p))))))


;; exercise 3.71
;
; This requires that the merge-weighted procedure shouldn't
; rule out the repetitions.
(define (add-cubic p)
  (+ (expt (car p) 3) (expt (cadr p) 3)))

(define cubic-sums
  (weighted-pairs integers integers add-cubic))

(define ramanujan-numbers
  (stream-map car
              (stream-filter (lambda (p) (= (car p) (cdr p)))
                             (s-map cons 
                                    (stream-map add-cubic cubic-sums)
                                    (stream-map add-cubic (stream-rest cubic-sums))))))

;; exercise 3.72
;
(define (add-square p)
  (+ (sqr (car p)) (sqr (cadr p))))

(define square-sums
  (weighted-pairs integers integers add-square))

(define sqr-sum-numbers
  (stream-map (lambda (p)
                (list (add-square (car p)) (car p) (cadr p) (caddr p)))
              (stream-filter (lambda (p)
                               (and (= (add-square (car p)) (add-square (cadr p)))
                                    (= (add-square (car p)) (add-square (caddr p)))))
                             (s-map list
                                    square-sums
                                    (stream-rest square-sums)
                                    (stream-rest (stream-rest square-sums))))))

;; exercise 3.73
(define (RC R C dt)
  (lambda (current v0)
    (add-streams (stream-scale current R)
                 (integral (stream-scale current (/ 1.0 C)) v0 dt))))

(define RC1 (RC 5 1 0.5))
(define resp (RC1 integers 0))

;; exercise 3.74
(define zero-crossings
  (s-map sign-change-detector sense-data (stream-cons 0 sense-data)))
 
;; exercise 3.75
(define (make-zero-crossings-tweak input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-first input-stream) last-value) 2)))
    (stream-cons (sign-change-detector avpt last-avpt)
                 (make-zero-crossings-tweak (stream-rest input-stream)
                                            (stream-first input-stream)
                                            avpt))))

;; exercise 3.76
(define (smooth s)
  (s-map (lambda (x y) (/ (+ x y) 2))
         (stream-rest s)
         s))

(define zero-crossings-modular
  (make-zero-crossings (smooth sense-data) 0))

;; exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (stream-cons initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-empty? integrand)
                     '()
                     (integral (delay (stream-rest integrand))
                               (+ (* dt (stream-first integrand))
                                  initial-value)
                               dt)))))

;; exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (stream-scale dy a)
                           (stream-scale y b)))
  y)

;; exercise 3.79
(define (solve-2nd-gen f a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; exercise 3.80
(define (RLC R L C dt)
  (lambda (v_C0 i_L0)
    (define v_C (integral (delay dv_C) v_C0 dt))
    (define i_L (integral (delay di_L) i_L0 dt))
    (define dv_C (stream-scale i_L (/ -1.0 C)))
    (define di_L (add-streams (stream-scale v_C (/ 1.0 L))
                              (stream-scale i_L (/ (- R) L))))
    (stream-map (lambda (v i) (cons v i))
                v_C i_L)))

;; exercise 3.81
(define (make-LCG a c m)
  (lambda (x)
    (remainder (+ (* a x) c) m)))
  
(define rand-update
  (make-LCG 22695477 1 (expt 2 32)))

(define (decide req value)
  (if (and (pair? req) (eq? (car req) 'reset))
      (cdr req)
      (rand-update value)))

(define (rand-gen requests rand-now)
  (stream-cons rand-now
               (rand-gen (stream-rest requests)
                         (decide (stream-first requests) rand-now))))
 
(define reqs
  (stream-cons 'generate
  (stream-cons 'generate
  (stream-cons (cons 'reset 4)
  (stream-cons 'generate '())))))

(define s (rand-gen reqs 127))
;(show-first-n s 5)


;; exercise 3.82
(define rand1
  (stream-cons 127
               (stream-map rand-update rand1)))
(define rand2
  (stream-cons 1023
               (stream-map rand-update rand2)))

(define rand-max 22695477)

(define (monte-carlo exp-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo (stream-rest exp-stream) passed failed)))
  (if (stream-first exp-stream)
      (next (+ 1 passed) failed)
      (next passed (+ 1 failed))))

(define (random-points-in-range low high)
  (define (convert x)
    (+ (* (- high low)
          (/ (remainder x rand-max) rand-max))
       low))
  (s-map (lambda (x y)
           (cons (convert x) (convert y)))
         rand1 rand2))

(define result-stream
  (stream-map (lambda (point)
                (<= (+ (sqr (car point)) (sqr (cdr point))) 1))
              (random-points-in-range -1 1)))

(define pi-stream
  (stream-map (lambda (x) (* 4.0 x))
              (monte-carlo result-stream 0 0)))

;(show-first-n pi-stream 1000)
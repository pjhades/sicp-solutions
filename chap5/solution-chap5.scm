;; 5.2
(controller
 (assign counter (const 1))
 (assign product (const 1))
 test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label fact-done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))
 fact-done)


;; 5.3
;; not all the same with the data path diagram
;; I omitted some intermediate registers
(controller
 (assign guess (const 1.0))
 good-enough
     (assign t1 (op *) (reg guess) (reg guess))
     (assign t2 (op -) (reg t1) (reg x))
     (test (op >) (reg t2) (const 0))
     (branch (label positive-value))
     (assign t3 (op -) (reg t2))
 positive-value
     (test (op <) (reg t3) (const 0.001))
     (branch (label so-good))
     (assign t4 (op /) (reg x) (reg guess))
     (assign t5 (op +) (reg t4) (reg guess))
     (assign guess (op /) (reg t5) (const 2))
     (goto (label good-enough))
 so-good)

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


;; 5.4
;; only the instruction sequence
;; a)
(controller
	(assign continue (label expt-done))
	expt-loop
		(test (op =) (reg n) (const 0))
		(branch (label base-case))
		(save continue)
		(assign continue (label after-recursion))
		(assign n (op -) (reg n) (const 1))
		(goto (label expt-loop))
	after-recursion
		(restore continue)
		(assign val (op *) (reg b) (reg val))
		(goto (reg continue))
	base-case
		(assign val (const 1))
		(goto (reg continue))
	expt-done)

;; b)
(controller
	(assign counter (reg n))
	(assign produce (const 1))
	(goto (label expt-loop)) ;; I'll put a call here, though not necessary
	expt-loop
		(test (op =) (reg counter) (const 0))
		(branch (label give-answer))
		(assign (op -) (reg counter) (const 1))
		(assign (op *) (reg product) (reg b))
		(goto (label expt-loop))
	give-answer
		(assign val (reg product))
		(goto (label expt-done))
	expt-done)

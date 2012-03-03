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


;; 5.5
;; I have done this when I was learning assembly...-_-b


;; 5.6
;; In the block denoted by label "afterfib-n-1", the (restart continue) and
;; (save continue) can both be removed since the address stored in continue
;; register points to the outer level which will only be used as a return
;; address after this whole Fib(n-1)+Fib(n-2) is computed.


;; 5.8
;; Register 'a' will hold 3 since the labels are searched
;; from A to Z. The new version of 'extract-labels' is:
(define (extract-labels text receive)
    ;; 我擦! 这不是CPS嘛!
    (if (null? text)
        (receive '() '())
        (extract-lables (cdr text)
                        (lambda (insts labels)
                            (let ((next-inst (car text)))
                                (if (symbol? next-inst)
                                    (if (assoc next-inst labels)
                                        (error "duplicate labels found -- assemble" next-inst)
                                        (receive insts
                                                 (cons (make-label-entry next-inst
                                                                         insts)
                                                       labels)))
                                    (receive (cons (make-instruction next-inst)
                                                   insts)
                                             labels)))))))

;; 5.9
(define (make-operation-exp exp machine labels operations)
    (let ((op (lookup-prim (operation-exp-op exp) operations))
          (aprocs
           (map (lambda (e)
                       (if (label-exp? e)
                        (error "can only operate registers and constants -- assemble")
                        (make-primitive-exp e machine labels)))
                (operation-exp-operands exp))))
        (lambda ()
            (apply op (map (lambda (p) (p)) aprocs)))))

;; 5.11
;; a)
;; After label 'afterfib-n-2', these two instructions:
(assign n (reg val))
(restore val)
;; intend to move the old value of 'val' to 'n' then
;; save a new value into 'val'. This can be simplified by
(restore n)

;; b)
;; note: changing the 'make-xxx' will leave the 
;;       stack unmodified
(define (make-save inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (push stack (cons (stack-inst-reg-name inst)
                              (get-contents reg)))
            (advance-pc pc))))

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (let ((top (pop stack)))
                (if (eq? (car top)
                         (stack-inst-reg-name inst))
                    (begin
                        (set-contents! reg (pop stack))
                        (advance-pc pc))
                    (error "register doesn't match -- assemble" inst))))))

;; c)
;; add a stack to each of the register on register table
(let ((the-ops
          (list (list 'initialize-stack
                    (lambda () 
                        (for-each (lambda (r)
                            ((caddr r) 'initialize))
                            register-table)))))
      (register-table
          ;; don't forget pc and flag
          (list (list 'pc pc (make-stack)) 
                (list 'flag flag (make-stack)))))

    ;; ...
)

(define (allocate-register name)
    (if (assoc name register-table)
        (error "multiply defined register:" name)
        (set! register-table
              (cons (list name 
                          (make-register name)
                          (make-stack))
                      register-table)))
    'register-allocated)

;; fetch the stack associated with each register
(define (make-save inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (push (caddr reg) (get-contents reg))
            (advance-pc pc))))

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (set-contents! reg (pop (caddr reg)))
            (advance-pc pc))))


;; 5.12
(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (the-instruction-sequence '())

          ;; <<< exer 5.12
          (all-insts '())
          (goto-regs '())
          (stack-regs '())
          (reg-assign-src '()))
          ;; >>> exer 5.12

        (let ((the-ops
                  (list (list 'initialize-stack
                              (lambda () (stack 'initialze)))))
              (register-table
                  (list (list 'pc pc) (list 'flag flag))))

            ;; <<< exer 5.12
            (define (record-inst inst)
                (let ((item (assoc (car inst) all-insts)))
                    (if item
                        (if (not (member inst (cdr item)))
                            (set-cdr! item (cons inst (cdr item))))
                        (set! all-insts
                              (cons (list (car inst) inst)
                                    all-insts)))))

            (define (record-goto-reg inst)
                (if (not (memq (cadadr inst) goto-regs))
                    (set! goto-regs
                          (cons (cadadr inst) goto-regs))))

            (define (record-stack-reg inst)
                (if (not (memq (cadr inst) stack-regs))
                    (set! stack-regs
                          (cons (cadr inst) stack-regs))))

            (define (record-reg-assign-src inst)
                (let ((item (assoc (cadr inst) reg-assign-src)))
                    (if item 
                        (if (not (member (caddr inst) (cdr item)))
                            (set-cdr! item
                                      (cons (caddr inst) (cdr item))))
                        (set! reg-assign-src
                              (cons (list (cadr inst)
                                          (caddr inst))
                                    reg-assign-src)))))
            ;; >>> exer 5.12

            ;; ... as before

            (define (dispatch message)
                (cond 
                      ;; ... as before

                      ;; >>> exer 5.12
                      ((eq? message 'all-insts) all-insts)
                      ((eq? message 'goto-regs) goto-regs)
                      ((eq? message 'stack-regs) stack-regs)
                      ((eq? message 'assign-src) reg-assign-src)

                      ((eq? message 'record-inst) record-inst)
                      ((eq? message 'record-goto-reg) record-goto-reg)
                      ((eq? message 'record-stack-reg) record-stack-reg)
                      ((eq? message 'record-reg-assign-src) record-reg-assign-src)
                      ;; <<< exer 5.12

                      (else (error "unknown request -- machine" message))))

            dispatch)))

;; modify the corresponding machine procedures
(define (make-assign inst machine labels operations pc)
    (let ((target (get-register machine (assign-reg-name inst)))
          (value-exp (assign-value-exp inst)))
        (let ((value-proc (if (operation-exp? value-exp)
                              (make-operation-exp value-exp machine labels operations)
                              (make-primitive-exp (car value-exp) machine labels))))
            ;; >>> exer 5.12
            ((machine 'record-reg-assign-src) inst)
            ;; <<< exer 5.12
            (lambda ()
                (set-contents! target (value-proc))
                (advance-pc pc)))))

(define (make-goto inst machine labels pc)
    (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
               (let ((insts
                     (lookup-label labels (label-exp-label dest))))
                   (lambda () (set-contents! pc insts))))
               ((register-exp? dest)
               (let ((reg
                     (get-register machine (register-exp-reg dest))))
                   ;; >>> exer 5.12
                   ((machine 'record-goto-reg) inst)
                   ;; <<< exer 5.12
                   (lambda () (set-contents! pc (get-contents reg)))))
              (else (error "bad goto instruction -- assemble" inst)))))

(define (goto-dest goto-instruction)
    (cadr goto-instruction))

(define (make-save inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        ;; >>> exer 5.12
        ((machine 'record-stack-reg) inst)
        ;; <<< exer 5.12
        (lambda ()
            (push stack (get-contents reg))
            (advance-pc pc))))

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        ;; >>> exer 5.12
        ((machine 'record-stack-reg) inst)
        ;; <<< exer 5.12
        (lambda ()
            (set-contents! reg (pop stack))
            (advance-pc pc))))


;; 5.13
;; delete the initialization in make-machine
(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence)
         (assemble controller-text machine))
        machine))

;; add a message to expose the register-table
(define (make-new-machine)
    (let
        ;; ... as before
        (let 
            ;; ... as before
            (define (dispatch message)
                (cond 
                      ;; >>> exer 5.13
                      ((eq? message 'register-table) register-table)
                      ;; <<< exer 5.13
                      (else (error "unknown request -- machine" message))))
            dispatch)))

;; allocate registers in each make-xxx function, that is,
;; wherever 'get-register' is called
(define (make-assign inst machine labels operations pc)
    ;; >>> exer 5.13
    (if (not (assoc (assign-reg-name inst) (machine 'register-table)))
        ((machine 'allocate-register) (assign-reg-name inst)))
    ;; <<< exer 5.13
    (let 
        ;; ... as before
        ))

(define (make-goto inst machine labels pc)
    (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
               ;; ... as before
               ((register-exp? dest)
               ;; >>> exer 5.13
               (if (not (assoc (register-exp-reg dest) (machine 'register-table)))
                   ((machine 'allocate-register) (register-exp-reg dest)))
               ;; <<< exer 5.13
               (let 
                   ;; ... as before
                   ))
              (else (error "bad goto instruction -- assemble" inst)))))

(define (make-save inst machine stack pc)
    ;; >>> exer 5.13
    (if (not (assoc (stack-inst-reg-name inst) (machine 'register-table)))
        ((machine 'allocate-register) (stack-inst-reg-name inst)))
    ;; <<< exer 5.13
    (let 
        ;; ... as before
        ))

(define (make-restore inst machine stack pc)
    ;; >>> exer 5.13
    (if (not (assoc (stack-inst-reg-name inst) (machine 'register-table)))
        ((machine 'allocate-register) (stack-inst-reg-name inst)))
    ;; <<< exer 5.13
    (let 
        ;; ... as before
        ))

(define (make-primitive-exp exp machine labels)
    (cond 
           ;; ... as before
          ((register-exp? exp)
           ;; >>> exer 5.13
           (if (not (assoc (register-exp-reg exp) (machine 'register-table)))
               ((machine 'allocate-register) (register-exp-reg exp)))
           ;; <<< exer 5.13
           (let 
               ;; ... as before
               )
          (else (error "unknown expression type -- assemble" exp))))


;; 5.14
(define fact-machine
    (make-machine
        '(val n continue)
        (list (list '= =) (list '- -) (list '* *))
        '(
                (perform (op initialize-stack))
                ;; ... as before
            fact-done
                (perform (op print-stack-statistics)))))

(for-each (lambda (n)
              (printf "computing ~a!\n" n)
              (set-register-contents! fact-machine 'n n)
              (start fact-machine)
              (printf "result: ~a\n\n" (get-register-contents fact-machine 'val)))
          '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

;; Note that the nunber of pushes equals the maximum depth.
;; Let f(n) be the number of pushes to compute n!.
;; Then we have f(n) = f(n-1) + 2 for pushing n and continue.
;; Solving this we obtain f(n) = 2n - 2



;; 5.15
(define (make-new-machine)
    (let (
          ;; ... 
          ;; >>> exer 5.15
          (inst-count 0)
          ;; <<< exer 5.15
          )

        (let 
            ;; ... as before
            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            ((instruction-execution-proc (car insts)))
                            ;; >>> exer 5.15
                            (set! inst-count (+ inst-count 1))
                            ;; <<< exer 5.15
                            (execute)))))

            ;; >>> exer 5.15
            (define (print-inst-count)
                (printf "~a instructions executed\n" inst-count))

            (define (reset-inst-count)
                (set! inst-count 0))
            ;; <<< exer 5.15

            (define (dispatch message)
                (cond 
                      ;; ...
                      ;; >>> exer 5.15
                      ((eq? message 'print-inst-count) (print-inst-count))
                      ((eq? message 'reset-inst-count) (reset-inst-count))
                      ;; <<< exer 5.15

                      (else (error "unknown request -- machine" message))))

            dispatch)))


;; 5.16
(define (make-new-machine)
    (let (
          ;; >>> exer 5.16
          (enable-trace #f)
          ;; <<< exer 5.16
          )
        (let
            ;; ... 
            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            ;; >>> exer 5.16
                            (if enable-trace
                                (printf "executing --> ~a\n" (caar insts)))
                            ;; <<< exer 5.16
                            ((instruction-execution-proc (car insts)))
                            (set! inst-count (+ inst-count 1))
                            (execute)))))
            (define (dispatch message)
                (cond ;; ...
                      ;; >>> exer 5.16
                      ((eq? message 'trace-on) (set! enable-trace #t))
                      ((eq? message 'trace-off) (set! enable-trace #f))
                      ;; <<< exer 5.16
                      ;; ...
                      ))
            dispatch)))

;; turn off the option before running the program
;; since racket prints mpairs with curly braces
(print-mpair-curly-braces #f)



;; 5.17
;; If we see a label, we attach it to all the instructions
;; after it, until we reach the next label, i.e. an instruction
;; which has already been attached a label.
(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
                        (lambda (insts labels)
                            (let ((next-inst (car text)))
                                (if (symbol? next-inst)
                                    ;; >>> exer 5.17
                                    (begin
                                        (for-each (lambda (ins)
                                                      (if (eq? (cadr ins) '*n/a*)
                                                          (set-car! (cdr ins) next-inst)))
                                                  insts)
                                        (receive insts
                                                 (cons (make-label-entry next-inst
                                                                         insts)
                                                       labels)))
                                    ;; <<< exer 5.17
                                    (receive (cons (make-instruction next-inst '*n/a*)
                                                   insts)
                                             labels)))))))

;; and modify the syntax procedures
;; >>> exer 5.17
(define (make-instruction text label)
    (cons text (cons label '())))

(define (instruction-text inst)
    (car inst))

(define (instruction-execution-proc inst)
    (cddr inst))

(define (set-instruction-execution-proc! inst proc)
    (set-cdr! (cdr inst) proc))
;; <<< exer 5.17

;; and we print the label when executing it
(define (execute)
    (let ((insts (get-contents pc)))
        (if (null? insts)
            'done
            (begin
                (if enable-trace
                    ;; >>> exer 5.17
                    (printf "executing --> ~a: ~a\n" (cadar insts) (caar insts)))
                    ;; <<< exer 5.17
                ((instruction-execution-proc (car insts)))
                (set! inst-count (+ inst-count 1))
                (execute)))))



;; 5.18
;; modify each register
(define (make-register name)
    (let ((contents '*unassigned*)
          ;; >>> exer 5.18
          (enable-trace #f))
          ;; <<< exer 5.18
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                  ((eq? message 'set)
                   (lambda (value) 
                       (if enable-trace
                           ;; >>> exer 5.18
                           (printf "[trace] assign register ~a: ~a --> ~a\n" 
                                   name contents value))
                           ;; <<< exer 5.18
                       (set! contents value)))
                  ;; >>> exer 5.18
                  ((eq? message 'trace-on)  (set! enable-trace #t))
                  ((eq? message 'trace-off)  (set! enable-trace #f))
                  ;; <<< exer 5.18
                  (else
                   (error "unknown request -- register" message))))
        dispatch))

;; add new interfaces
(define (make-new-machine)
    (let 
        ;; ... as before
        (let 
            ;; ... as before    
            ;; >>> exer 5.18
            (define (trace-reg-on name)
                ((lookup-register name) 'trace-on))
            (define (trace-reg-off name)
                ((lookup-register name) 'trace-off))
            ;; <<< exer 5.18
            (define (dispatch message)
                (cond ;; ...
                      ;; >>> exer 5.18
                      ((eq? message 'trace-reg-on) trace-reg-on)
                      ((eq? message 'trace-reg-off) trace-reg-off)
                      ;; <<< exer 5.18
                      (else (error "unknown request -- machine" message))))
            dispatch)))

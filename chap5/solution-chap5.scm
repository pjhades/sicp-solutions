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

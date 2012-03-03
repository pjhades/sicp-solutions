#lang racket/base
(require r5rs/r5rs)

(define (p seq)
    (if (null? seq)
        (printf "\n")
        (begin (printf "~a\n" (car seq))
               (p (cdr seq)))))


(define (error msg . objs)
    (define (p lst)
        (if (null? lst)
            (newline)
            (begin
                (display " ")
                (display (car lst))
                (p (cdr lst)))))
    (display msg)
    (p objs))

(define (make-machine register-names ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                    ((machine 'allocate-register) register-name))
                  register-names)
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence)
         (assemble controller-text machine))
        (p (machine 'inst-sequence))
        machine))

(define (make-register name)
    (let ((contents '*unassigned*))
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                  ((eq? message 'set)
                   (lambda (value) (set! contents value)))
                  (else
                   (error "unknown request -- register" message))))
        dispatch))

(define (get-contents register)
    (register 'get))

(define (set-contents! register value)
    ((register 'set) value))

(define (make-stack)
    (let ((s '())
          (number-pushes 0)
          (max-depth 0)
          (current-depth 0))

        (define (push x)
            (set! s (cons x s))
            (set! number-pushes (+ number-pushes 1))
            (set! current-depth (+ current-depth 1))
            (set! max-depth (max max-depth current-depth)))

        (define (pop)
            (if (null? s)
                (error "empty stack -- pop")
                (let ((top (car s)))
                    (set! s (cdr s))
                    (set! current-depth (- current-depth 1))
                     top)))

        (define (initialize)
            (set! s '())
            (set! number-pushes 0)
            (set! max-depth 0)
            (set! current-depth 0)
            'done)

        (define (print-statistics)
            (printf "total-pushes=~a max-depth=~a\n" 
                    number-pushes max-depth))

        (define (dispatch message)
            (cond ((eq? message 'push) push)
                  ((eq? message 'pop) (pop))
                  ((eq? message 'initialze) (initialize))
                  ((eq? message 'print-statistics) (print-statistics))
                  (else (error "unknown request -- stack" message))))

        dispatch))

(define (pop stack)
    (stack 'pop))

(define (push stack value)
    ((stack 'push) value))

;; make a new machine, common to all register machines
(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (inst-count 0)
          (enable-trace #f)
          (the-instruction-sequence '()))

        (let ((the-ops
                  (list (list 'initialize-stack
                              (lambda () (stack 'initialze)))
                        (list 'print-stack-statistics
                              (lambda () (stack 'print-statistics)))))
              (register-table
                  (list (list 'pc pc) (list 'flag flag))))

            (define (allocate-register name)
                (if (assoc name register-table)
                    (error "multiply defined register:" name)
                    (set! register-table
                          (cons (list name (make-register name)) 
                                register-table)))
                'register-allocated)

            (define (lookup-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        (cadr val)
                        (error "unknown register:" name))))

            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            (if enable-trace
                                (printf "executing --> ~a: ~a\n" (cadar insts) (caar insts)))
                            ((instruction-execution-proc (car insts)))
                            (set! inst-count (+ inst-count 1))
                            (execute)))))

            (define (print-inst-count)
                (printf "~a instructions executed\n" inst-count))

            (define (reset-inst-count)
                (set! inst-count 0))

            (define (dispatch message)
                (cond ((eq? message 'start)
                       (set-contents! pc the-instruction-sequence)
                       (execute))
                      ((eq? message 'install-instruction-sequence)
                       (lambda (seq) (set! the-instruction-sequence seq)))
                      ((eq? message 'allocate-register) allocate-register)
                      ((eq? message 'get-register) lookup-register)
                      ((eq? message 'install-operations)
                       (lambda (ops) (set! the-ops (append the-ops ops))))
                      ((eq? message 'stack) stack)
                      ((eq? message 'operations) the-ops)
                      ((eq? message 'print-inst-count) (print-inst-count))
                      ((eq? message 'reset-inst-count) (reset-inst-count))
                      ((eq? message 'trace-on) (set! enable-trace #t))
                      ((eq? message 'trace-off) (set! enable-trace #f))
                      ((eq? message 'inst-sequence) the-instruction-sequence)
                      (else (error "unknown request -- machine" message))))

            dispatch)))

;; interfaces of the machine
(define (start machine)
    (machine 'start))

(define (get-register-contents machine register-name)
    (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
    (set-contents! (get-register machine register-name) value)
    'done)

(define (get-register machine reg-name)
    ((machine 'get-register) reg-name))

;; assembler
(define (assemble controller-text machine)
    (extract-labels controller-text
                    (lambda (insts labels)
                            (update-insts! insts labels machine)
                            insts)))

(define (extract-labels text receive)
    ;; 我擦! CPS有木有!
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
                                    ;(receive insts
                                    ;         (cons (make-label-entry next-inst
                                    ;                                 insts)
                                    ;               labels))
                                    (receive (cons (make-instruction next-inst '*n/a*)
                                                   insts)
                                             labels)))))))

(define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
          (flag (get-register machine 'flag))
          (stack (machine 'stack))
          (ops (machine 'operations)))
        (for-each
            (lambda (inst)
                (set-instruction-execution-proc! 
                    inst
                    (make-execution-procedure
                        (instruction-text inst) labels machine
                        pc flag stack ops)))
            insts)))

;; >>> exer 5.17
(define (make-instruction text label)
    (cons text (cons label '())))
;; <<< exer 5.17
;(define (make-instruction text)
;    (cons text '()))

;; >>> exer 5.17
(define (instruction-text inst)
    (car inst))
;; <<< exer 5.17
;(define (instruction-text inst)
;    (car inst))

;; >>> exer 5.17
(define (instruction-execution-proc inst)
    (cddr inst))
;; <<< exer 5.17
;(define (instruction-execution-proc inst)
;    (cdr inst))

;; >>> exer 5.17
(define (set-instruction-execution-proc! inst proc)
    (set-cdr! (cdr inst) proc))
;; <<< exer 5.17
;(define (set-instruction-execution-proc! inst proc)
;    (set-cdr! inst proc))

(define (make-label-entry label-name inst)
    (cons label-name inst))

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "undefined label -- assemble" label-name))))

;; return a function for each instruction type
(define (make-execution-procedure inst labels machine pc flag stack ops)
    (cond ((eq? (car inst) 'assign)
           (make-assign inst machine labels ops pc))
          ((eq? (car inst) 'test)
           (make-test inst machine labels ops flag pc))
          ((eq? (car inst) 'branch)
           (make-branch inst machine labels flag pc))
          ((eq? (car inst) 'goto)
           (make-goto inst machine labels pc))
          ((eq? (car inst) 'save)
           (make-save inst machine stack pc))
          ((eq? (car inst) 'restore)
           (make-restore inst machine stack pc))
          ((eq? (car inst) 'perform)
           (make-perform inst machine labels ops pc))
          (else
           (error "unknown instruction type -- assemble" inst))))

(define (advance-pc pc)
    (set-contents! pc (cdr (get-contents pc))))


;; machine instructions
(define (make-assign inst machine labels operations pc)
    (let ((target (get-register machine (assign-reg-name inst)))
          (value-exp (assign-value-exp inst)))
        (let ((value-proc (if (operation-exp? value-exp)
                              (make-operation-exp value-exp machine labels operations)
                              (make-primitive-exp (car value-exp) machine labels))))
            (lambda ()
                (set-contents! target (value-proc))
                (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
    (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
    (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
    (let ((condition (test-condition inst)))
        (if (operation-exp? condition)
            (let ((condition-proc
                  (make-operation-exp condition machine labels operations)))
                (lambda ()
                    (set-contents! flag (condition-proc))
                    (advance-pc pc)))
            (error "bad test instruction -- assamble" inst))))

(define (test-condition test-instruction)
    (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
    (let ((dest (branch-dest inst)))
        (if (label-exp? dest)
            (let ((insts
                  (lookup-label labels (label-exp-label dest))))
                (lambda ()
                    (if (get-contents flag)
                        (set-contents! pc insts)
                        (advance-pc pc))))
            (error "bad branch instruction -- assemble" inst))))

(define (branch-dest branch-instruction)
    (cadr branch-instruction))

(define (make-goto inst machine labels pc)
    (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
               (let ((insts
                     (lookup-label labels (label-exp-label dest))))
                   (lambda () (set-contents! pc insts))))
               ((register-exp? dest)
               (let ((reg
                     (get-register machine (register-exp-reg dest))))
                   (lambda () (set-contents! pc (get-contents reg)))))
              (else (error "bad goto instruction -- assemble" inst)))))

(define (goto-dest goto-instruction)
    (cadr goto-instruction))

(define (make-save inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (push stack (get-contents reg))
            (advance-pc pc))))

(define (make-restore inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
        (lambda ()
            (set-contents! reg (pop stack))
            (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
    (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
    (let ((action (perform-action inst)))
        (if (operation-exp? action)
            (let ((action-proc
                  (make-operation-exp action machine labels operations)))
                (lambda ()
                    (action-proc)
                    (advance-pc pc)))
            (error "bad perform instruction -- assemble" inst))))

(define (perform-action inst)
    (cdr inst))

(define (make-primitive-exp exp machine labels)
    (cond ((constant-exp? exp)
           (let ((c (constant-exp-value exp)))
               (lambda () c)))
          ((label-exp? exp)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label exp))))
               (lambda () insts)))
          ((register-exp? exp)
           (let ((r (get-register machine
                                  (register-exp-reg exp))))
               (lambda () (get-contents r))))
          (else (error "unknown expression type -- assemble" exp))))

(define (tagged-list? s tag)
    (and (pair? s) (eq? (car s) tag)))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

;; the same functionality as apply
(define (make-operation-exp exp machine labels operations)
    (let ((op (lookup-prim (operation-exp-op exp) operations))
          (aprocs
           (map (lambda (e)
                    (make-primitive-exp e machine labels))
                (operation-exp-operands exp))))
        (lambda ()
            (apply op (map (lambda (p) (p)) aprocs)))))
            
(define (operation-exp? exp)
    (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
    (cadar operation-exp))

(define (operation-exp-operands operation-exp)
    (cdr operation-exp))

(define (lookup-prim symbol operations)
    (let ((val (assoc symbol operations)))
        (if val
            (cadr val)
            (error "unknown operation -- assemble" symbol))))


;; test programs
(print-mpair-curly-braces #f)

(define gcd-machine
    (make-machine
        '(a b t)
        (list (list 'rem remainder) (list '= =))
        '(test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
          gcd-done)))

(define fib-machine 
    (make-machine 
        '(val n continue) 
        (list (list '+ +) (list '- -) (list '< <))
        '(  
                (assign continue (label fib-done)) 
            fib-loop 
                (test (op <) (reg n) (const 2)) 
                (branch (label immediate-answer)) 
                (save continue) 
                (assign continue (label afterfib-n-1)) 
                (save n) 
                (assign n (op -) (reg n) (const 1)) 
                (goto (label fib-loop)) 
            afterfib-n-1 
                (restore n) 
                (restore continue) 
                (assign n (op -) (reg n) (const 2)) 
                (save continue) 
                (assign continue (label afterfib-n-2)) 
                (save val) 
                (goto (label fib-loop)) 
            afterfib-n-2 
                (assign n (reg val)) 
                (restore val) 
                (restore continue) 
                (assign val (op +) (reg val) (reg n))
                (goto (reg continue))
            immediate-answer
                (assign val (reg n))
                (goto (reg continue))
            fib-done)))

(define fact-machine
    (make-machine
        '(val n continue)
        (list (list '= =) (list '- -) (list '* *))
        '(
                (perform (op initialize-stack))
                (assign continue (label fact-done))
            fact-loop
                (test (op =) (reg n) (const 1))
                (branch (label base-case))
                (save continue)
                (save n)
                (assign n (op -) (reg n) (const 1))
                (assign continue (label after-fact))
                (goto (label fact-loop))
            after-fact
                (restore n)
                (restore continue)
                (assign val (op *) (reg n) (reg val))
                (goto (reg continue))
            base-case
                (assign val (const 1))
                (goto (reg continue))
            fact-done)))


(set-register-contents! fact-machine 'n 10)
(fact-machine 'trace-on)
(start fact-machine)
(get-register-contents fact-machine 'val)
(fact-machine 'print-inst-count)


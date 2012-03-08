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
        machine))

(define (make-register name)
    (let ((contents '*unassigned*)
          (enable-trace #f))
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                  ((eq? message 'set)
                   (lambda (value) 
                       (if enable-trace
                           (printf "[trace] assign register ~a: ~a --> ~a\n" 
                                   name contents value))
                       (set! contents value)))
                  ((eq? message 'trace-on)  (set! enable-trace #t))
                  ((eq? message 'trace-off)  (set! enable-trace #f))
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
            (printf "total pushes: ~a\nmax depth: ~a\n" 
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

            (define (execute resume?)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            (cond ((and (not resume?) (instruction-break (car insts)))
                                   (printf "[break] ~a: ~a\n"
                                           (instruction-label (car insts))
                                           (instruction-text (car insts))))
                                  (else 
                                   (if enable-trace
                                       (begin (printf "[trace] execute --> ~a: ~a" 
                                                      (instruction-label (car insts)) 
                                                      (instruction-text (car insts)))
                                              (if (instruction-break (car insts))
                                                  (printf " <B>\n")
                                                  (printf "\n"))))
                                   ((instruction-execution-proc (car insts)))
                                   (set! inst-count (+ inst-count 1))
                                   (execute #f)))))))

            (define (print-inst-count)
                (printf "~a instructions executed\n" inst-count))

            (define (reset-inst-count)
                (set! inst-count 0))
 
            (define (trace-reg-on name)
                ((lookup-register name) 'trace-on))

            (define (trace-reg-off name)
                ((lookup-register name) 'trace-off))

            (define (set-breakpoint label n)
                (define (iter insts k)
                    (cond ((null? insts)
                           (error (format "no such instruction to set breakpoint on -- machine ~a:~a"
                                          label n)))
                          ((eq? (instruction-label (car insts))
                                label)
                           (if (= (+ k 1) n)
                               (set-instruction-break! (car insts) #t)
                               (iter (cdr insts) (+ k 1))))
                          (else (iter (cdr insts) k))))
                (iter the-instruction-sequence 0))

            (define (cancel-breakpoint label n)
                (define (iter insts k)
                    (cond ((null? insts)
                           (error (format "no such breakpoint to cancel -- machine ~a:~a"
                                          label n)))
                          ((eq? (instruction-label (car insts))
                                label)
                           (if (= (+ k 1) n)
                               (set-instruction-break! (car insts) #f)
                               (iter (cdr insts) (+ k 1))))
                          (else (iter (cdr insts) k))))
                (iter the-instruction-sequence 0))

            (define (cancel-all-breakpoints)
                (define (iter insts)
                    (if (null? insts)
                        'done
                        (begin
                            (if (instruction-break (car insts))
                                (set-instruction-break! (car insts) #f))
                            (iter (cdr insts)))))
                (iter the-instruction-sequence))

            (define (dispatch message)
                (cond ((eq? message 'start)
                       (set-contents! pc the-instruction-sequence)
                       (execute #f))
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
                      ((eq? message 'trace-reg-on) trace-reg-on)
                      ((eq? message 'trace-reg-off) trace-reg-off)
                      ((eq? message 'set-breakpoint) set-breakpoint)
                      ((eq? message 'cancel-breakpoint) cancel-breakpoint)
                      ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
                      ((eq? message 'proceed) (execute #t))
                      ((eq? message 'inst-seq) the-instruction-sequence)
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
        (extract-labels 
            (cdr text)
            (lambda (insts labels)
                (let ((next-inst (car text)))
                    (if (symbol? next-inst)
                        (begin
                            (for-each (lambda (ins)
                                          (if (eq? (instruction-label ins) '*n/a*)
                                              (set-instruction-label! ins next-inst)))
                                      insts)
                            (receive insts
                                     (cons (make-label-entry next-inst
                                                             insts)
                                           labels)))
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

(define (make-instruction text label)
    (list text label '() #f))

(define (instruction-text inst)
    (car inst))

(define (instruction-label inst)
    (cadr inst))

(define (instruction-break inst)
    (cadddr inst))

(define (instruction-execution-proc inst)
    (caddr inst))

(define (set-instruction-execution-proc! inst proc)
    (set-car! (cddr inst) proc))

(define (set-instruction-label! inst label)
    (set-car! (cdr inst) label))

(define (set-instruction-break! inst break)
    (set-car! (cdddr inst) break))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Evaluate a list of expressions, return the list
; of results
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (EVAL (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (EVAL (if-predicate exp) env))
      (EVAL (if-consequent exp) env)
      (EVAL (if-alternative exp) env)))

; Evaluate a list of expressions, like for-each
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (EVAL (first-exp exps) env))
        (else (EVAL (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (EVAL (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (EVAL (definition-value exp) env)
                    env)
  'ok)


; Representing expressions, building data abstraction that decouples the
; representation and operation of the data.

; Numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))


; Variables
(define (variable? exp) (symbol? exp))


; Quotes
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))



; Assignments
; (set! var val)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


; Definitions
; (define var val)
; (define (var arg1 arg2) body)  --> syntactic sugar
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


; Lambda expressions
; (lambda (arg1 arg2) body)
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; if
; (if (pred) (consequent) (alternative))
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; begin code block
; (begin (exp1) (exp2))
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))
 
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


; procedure application
; (proc arg1 arg2)
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (last-operand? ops) (null? (cdr ops)))


; let
; (let ((var1 exp1)
;       (var2 exp2)
;       (var3 exp3))
;    body)
(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (binding-variable binding) (car binding))

(define (binding-exp binding) (cadr binding))

(define (let-variables bindings)
  (if (null? bindings)
      '()
      (cons (binding-variable (car bindings))
            (let-variables (cdr bindings)))))

(define (let-exps bindings)
  (if (null? bindings)
      '()
      (cons (binding-exp (car bindings))
            (let-exps (cdr bindings)))))


; let*
; (let* ((var1 exp1)
;        (var2 exp2)
;        (var3 exp3))
;    body)
(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-bindings exp) (cadr exp))

(define (let*-body exp) (cddr exp))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (expand-bindings bindings body)
  (if (null? (cdr bindings))
      (make-let (list (car bindings)) body)
      (make-let (list (car bindings))
                (list (expand-bindings (cdr bindings) body)))))

(define (let*->nested-lets exp)
  (expand-bindings (let*-bindings exp) (let*-body exp)))


(define (named-let? exp) (not (pair? (cadr exp))))

(define (named-let-name exp) (cadr exp))

(define (named-let-body exp) (cdddr exp))

(define (named-let-bindings exp) (caddr exp))

(define (named-let-params bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings)
            (named-let-params (cdr bindings)))))


; letrec
; (letrec ((var1 exp1)
;          (var2 exp2)
;          (var3 exp3))
;    body)
(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec-set-exps exp)
  (define (iter seq)
    (if (null? seq)
        '()
        (cons (cons 'set! (car seq))
              (iter (cdr seq)))))
  (iter (cadr exp)))

(define (letrec-let-bindings exp)
  (define (iter seq)
    (if (null? seq)
        '()
        (cons (list (caar seq) ''*unassigned*)
              (iter (cdr seq)))))
  (iter (cadr exp)))

(define (letrec->let exp)
  (make-let (letrec-let-bindings exp)
            (append (letrec-set-exps exp)
                    (cddr exp))))


; Named let
; (let foo ((arg1 exp1)
;           (arg2 exp2)
;           (arg3 exp3))
;    exp4
;    (foo 1 2 3))
(define (named-let-values bindings)
  (if (null? bindings)
      '()
      (cons (cadar bindings)
            (named-let-values (cdr bindings)))))

; Using Y-combinator to bind the name fib-iter within 
; the scope of the named-let
(define (Y f)
  (let ((g (lambda (h)
             (lambda args
               (apply (f (h h)) args)))))
    (g g)))

(define (make-fixed-point exp)
  (list 'Y
        (make-lambda (list (named-let-name exp))
                     (list (make-lambda (named-let-params (named-let-bindings exp))
                                        (named-let-body exp))))))

(define (let->combination exp)
  (if (named-let? exp)
      (cons (make-fixed-point exp)
            (named-let-values (named-let-bindings exp)))
      (cons (make-lambda (let-variables (let-bindings exp))
                         (let-body exp))
            (let-exps (let-bindings exp)))))


; cond
; (cond ((pred1) (exp1)) (else (exp2)))
; or
; (cond (<test> => <proc>) // if <test> is true call <proc> on it
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (=>clause? clause) (memq '=> clause))

(define (=>clause-test clause) (car clause))

(define (=>clause-recipient clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (list '*error* "COND->IF: ELSE clause isn't last")))
              ((=>clause? first)
               (make-if (=>clause-test first)
                        (list (=>clause-recipient first)
                              (=>clause-test first))
                        (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))


; and
; (and exp1 exp2 exp3)
(define (and? exp) (tagged-list? exp 'and))

(define (and-exps exp) (cdr exp))

(define (test-and-exps exps env)
  (cond ((null? exps) 'true)
        ((eq? (EVAL (first-exp exps) env) 'false) 'false)
        (else (test-and-exps (rest-exps exps) env))))

(define (eval-and exps env)
  (if (null? exps)
      'true
      (test-and-exps exps env)))


; or
; (or exp1 exp2 exp3)
(define (or? exp) (tagged-list? exp 'or))

(define (or-exps exp) (cdr exp))

(define (test-or-exps exps env)
  (cond ((null? exps) 'false)
        ((eq? (EVAL (first-exp exps) env) 'true) 'true)
        (else (test-or-exps (rest-exps exps) env))))

(define (eval-or exps env)
  (if (null? exps)
      'false
      (test-or-exps exps env)))


; apply
; (apply (lambda (x) x) '(5))
; or
; (apply (lambda (x y z) (+ x y z)) '(1 2 3))
(define (apply? exp) (tagged-list? exp 'apply))

(define (apply-procedure exp) (cadr exp))

(define (apply-arguments exp) (caddr exp))

(define (eval-apply exp env)
  (APPLY (EVAL (apply-procedure exp) env)
         (EVAL (apply-arguments exp) env)))


; Evaluator data structures: true/false, environment, 
; representation of procedures ...
; i.e. the internal data structures that we have to 
; maintain in the evaluator.


; true/false test
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))


; Procedure object
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


; Environment
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(define (extend-environment vars vals base-env)
  (if (not (pair? vars))
      (cons (make-frame (list vars) (list vals)) base-env)
      (if (= (length vars) (length vals))
          (cons (make-frame vars vals) base-env)
          (if (< (length vars) (length vals))
              (begin (list '*error* "Too many arguments supplied"))
              (begin (list '*error* "Too few arguments supplied"))))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (list '*error* "LOOKUP-VARIABLE-VALUE: Unbound variable:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (list '*error* "SET-VARIABLE-VALUE: Unbound variable:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


; List of primitive procedures. Add more here to enhance
; the evaluator.
(define primitive-procedures
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; Initial bindings
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (EVAL '(define (Y f)
             (let ((g (lambda (h)
                        (lambda args
                          (apply (f (h h)) args)))))
               (g g))) initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

; Prompts
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

; Driver loop: read-eval-print loop
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (EVAL input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
    (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


; Main eval and apply procedures
(define (EVAL exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (EVAL (cond->if exp) env))
        ((let? exp) (EVAL (let->combination exp) env))
        ((let*? exp) (EVAL (let*->nested-lets exp) env))
        ((letrec? exp) (EVAL (letrec->let exp) env))
        ((and? exp) (eval-and (and-exps exp) env))
        ((or? exp) (eval-or (or-exps exp) env))
        ((apply? exp) (eval-apply exp env))
        ((application? exp)
         (APPLY (EVAL (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (list '*error* "EVAL: Unknown expression type: " exp))))

(define (APPLY procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (display procedure)
         (list '*error* "APPLY: Unknown procedure type:" procedure))))



(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (list '*error* "EXECUTE-APPLICATION: Unknown procedure type" proc))))



(define the-global-environment (setup-environment))

(define (get-global-environment)
    the-global-environment)

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
    (append arglist (list arg)))

; Run the metacircular evaluator
;(driver-loop)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define eceval-operations
    `(
      (self-evaluating? ,self-evaluating?)
      (prompt-for-input ,prompt-for-input)
      (read ,read)
      (get-global-environment ,get-global-environment)
      (announce-output ,announce-output)
      (self-evaluating? ,self-evaluating?)
      (variable? ,variable?)
      (quoted? ,quoted?)
      (assignment? ,assignment?)
      (definition? ,definition?)
      (if? ,if?)
      (lambda? ,lambda?)
      (begin? ,begin?)
      (application? ,application?)
      (lookup-variable-value ,lookup-variable-value)
      (text-of-quotation ,text-of-quotation)
      (lambda-parameters ,lambda-parameters)
      (lambda-body ,lambda-body)
      (make-procedure ,make-procedure)
      (operands)
      (operator)
      (empty-arglist)
      (first-operand)
      (last-operand?)
      (rest-operands)
      (primitive-procedure?)
      (compound-procedure?)
      (apply-primitive-procedure)
      (procedure-parameters)
      (procedure-environment)
      (extend-environment)
      (procedure-body)
      (begin-actions)
      (first-exp)
      (last-exp?)
      (rest-exps)
      (true?)
      (if-predicate)
      (if-alternative)
      (if-consequent)
      (assignment-variable)
      (assignment-value)
      (set-variable-value!)
      (definition-variable)
      (definition-value)
      (define-variable!)
      ))


;; explicit-control evaluator
(define eceval
    (make-machine
        '(exp env val proc argl continue unev)
        eceval-operations
        '(
            ;; REPL
            read-eval-print-loop
                (perform (op initialize-stack))
                (perform (op prompt-for-input) (const ";;; EC-Eval input: "))
                (assign exp (op read))
                (assign env (op get-global-environment))
                (assign continue (label print-result))
                (goto (label eval-dispatch))
            print-result
                (perform (op announce-output) (const ";;; EC-Eval value: "))
                (perform (op user-print) (reg val))
                (goto (label read-eval-print-loop))

            ;; dispatch expressions
            eval-dispatch
                (test (op self-evaluating?) (reg exp))
                (branch (label ev-self-eval))
                (test (op variable?) (reg exp))
                (branch (label ev-variable))
                (test (op quoted?) (reg exp))
                (branch (label ev-quoted))
                (test (op assignment?) (reg exp))
                (branch (label ev-assignment))
                (test (op definition?) (reg exp))
                (branch (label ev-definition))
                (test (op if?) (reg exp))
                (branch (label ev-if))
                (test (op lambda?) (reg exp))
                (branch (label ev-lambda))
                (test (op begin?) (reg exp))
                (branch (label ev-begin))
                (test (op application?) (reg exp))
                (branch (label ev-application))
                (goto (label unknown-expression-type))

            ;; evaluating procedures
            ev-self-eval
                (assign val (reg exp))
                (goto (reg continue))
            ev-variable
                (assign val (op lookup-variable-value) (reg exp) (reg env))
                (goto (reg continue))
            ev-quoted
                (assign val (op text-of-quotation) (reg exp))
                (goto (reg continue))
            ev-lambda
                (assign unev (op lambda-parameters) (reg exp))
                (assign exp (op lambda-body) (reg exp))
                (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
                (goto (reg continue))

            ev-application
                (save continue)
                (save env)
                (assign unev (op operands) (reg exp))
                (save unev)
                (assign exp (op operator) (reg exp))
                (assign continue (label ev-appl-did-operator))
                (goto (label eval-dispatch))
            ev-appl-did-operator
                (restore unev)
                (restore env)
                (assign argl (op empty-arglist))
                (assign proc (reg val))
                (test (op no-operands?) (reg unev))
                (branch (label apply-dispatch))
                (save proc)
            ev-appl-operand-loop
                (save argl)
                (assign exp (op first-operand) (reg unev))
                (test (op last-operand?) (reg unev))
                (branch (label ev-appl-last-arg))
                (save env)
                (save unev)
                (assign continue (label ev-appl-accumulate-arg))
                (goto (label eval-dispatch))
            ev-appl-accumulate-arg
                (restore unev)
                (restore env)
                (restore argl)
                (assign argl (op adjoin-arg) (reg val) (reg argl))
                (assign unev (op rest-operands) (reg unev))
                (goto (label ev-appl-operand-loop))
            ev-appl-last-arg
                (assign continue (label ev-appl-accum-last-arg))
                (goto (label eval-dispatch))
            ev-appl-accum-last-arg
                (restore argl)
                (assign argl (op adjoin-arg) (reg val) (reg argl))
                (restore proc)
                (goto (label apply-dispatch))

            apply-dispatch
                (test (op primitive-procedure?) (reg proc))
                (branch (label primitive-apply))
                (test (op compound-procedure?) (reg proc))
                (branch (label compound-apply))
                (goto (label unknown-procedure-type))
            primitive-apply
                (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
                (restore continue)
                (goto (reg continue))
            compound-apply
                (assign unev (op procedure-parameters) (reg proc))
                (assign env (op procedure-environment) (reg proc))
                (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
                (assign unev (op procedure-body) (reg proc))
                (goto (label ev-sequence))

            ev-begin
                (assign unev (op begin-actions) (reg exp))
                (save continue)
                (goto (label ev-sequence))

            ev-sequence
                (assign exp (op first-exp) (reg unev))
                (test (op last-exp?) (reg unev))
                (branch (label ev-sequence-last-exp))
                (save unev)
                (save env)
                (assign continue (label ev-sequence-continue))
                (goto (label eval-dispatch))
            ev-sequence-continue
                (restore env)
                (restore unev)
                (assign unev (op rest-exps) (reg unev))
                (goto (label ev-sequence))
            ev-sequence-last-exp
                (restore continue)
                (goto (label eval-dispatch))

            ev-if
                (save exp)
                (save env)
                (save continue)
                (assign continue (label ev-if-decide))
                (assign exp (op if-predicate) (reg exp))
                (goto (label eval-dispatch))
            ev-if-decide
                (restore continue)
                (restore env)
                (restore exp)
                (test (op true?) (reg val))
                (branch (label ev-if-consequent))
            ev-if-alternative
                (assign exp (op if-alternative) (reg exp))
                (goto (label eval-dispatch))
            ev-if-consequent
                (assign exp (op if-consequent) (reg exp))
                (goto (label eval-dispatch))

            ev-assignment
                (assign unev (op assignment-variable) (reg exp))
                (save unev)
                (assign exp (op assignment-value) (reg exp))
                (save env)
                (save continue)
                (assign continue (label ev-assignment-1))
                (goto (label eval-dispatch))
            ev-assignment-1
                (restore continue)
                (restore env)
                (restore unev)
                (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
                (assign val (const ok))
                (goto (reg continue))

            ev-definition
                (assign unev (op definition-variable) (reg exp))
                (save unev)
                (assign exp (op definition-value) (reg exp))
                (save env)
                (save continue)
                (assign continue (label ev-definition-1))
                (goto (label eval-dispatch))
            ev-definition-1
                (restore continue)
                (restore env)
                (restore unev)
                (perform (op define-variable!) (reg unev) (reg val) (reg env))
                (assign val (const ok))
                (goto (reg continue))

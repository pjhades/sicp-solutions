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
        ((boolean? exp) #t)
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

(define (cond-first-clause clauses) (car clauses))

(define (cond-rest-clauses clauses) (cdr clauses))

(define (cond-last-clause? clauses) (null? (cdr clauses)))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Compiler 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile exp target linkage)
    (cond ((self-evaluating? exp)
           (compile-self-evaluating exp target linkage))
          ((quoted? exp)
           (compile-quoted exp target linkage))
          ((variable? exp)
           (compile-variable exp target linkage))
          ((assignment? exp)
           (compile-assignment exp target linkage))
          ((definition? exp)
           (compile-definition exp target linkage))
          ((if? exp)
           (compile-if exp target linkage))
          ((lambda? exp)
           (compile-lambda exp target linkage))
          ((begin? exp)
           (compile-sequence (begin-actions exp) target linkage))
          ((cond? exp)
           (compile (cond->if exp) target linkage))
          ((application? exp)
           (compile-application exp target linkage))
          (else
           (error "unknown expression type -- compile" exp))))

;; 对每个代码序列，附加它需要的寄存器、修改的寄存器
(define (make-instruction-sequence needs modifies statements)
    (list needs modifies statements))

(define (empty-instruction-sequence)
    (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
           (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
          ((eq? linkage 'next)
           (empty-instruction-sequence))
          (else
           (make-instruction-sequence '() '() `((goto (label ,linkage)))))))

;; 连接两个代码序列，保护regs指定的寄存器
;; 在seq1周围加上对regs中寄存器的save和restore
;; 逐个处理regs里面的寄存器。对每个reg，若reg
;; 被seq1而seq2需要，则要保护，加上save和restore，
;; 生成一个新的seq，这个seq的need是seq1的need加上这个reg，
;; modify是seq1的modify除去这一个reg（因为reg被保护）。
;; 然后将这个辅助的seq递归和seq2进行preserving。
(define (preserving regs seq1 seq2)
    (if (null? regs)
        (append-instruction-sequences seq1 seq2)
        (let ((first-reg (car regs)))
            (if (and (needs-register? seq2 first-reg)
                     (modifies-register? seq1 first-reg))
                (preserving (cdr regs)
                            (make-instruction-sequence
                                (list-union (list first-reg)
                                            (registers-needed seq1))
                                (list-difference (registers-modified seq1)
                                                 (list first-reg))
                                (append `((save ,first-reg))
                                        (statements seq1)
                                        `((restore ,first-reg))))
                            seq2)
                (preserving (cdr regs) seq1 seq2)))))

(define (end-with-linkage linkage instruction-sequence)
    (preserving '(continue) instruction-sequence (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
    (end-with-linkage linkage 
                      (make-instruction-sequence '() (list target)
                                                 `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
    (end-with-linkage linkage
                      (make-instruction-sequence '() (list target)
                                                 `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
    (end-with-linkage linkage
                      (make-instruction-sequence '(env) (list target)
                                                 `((assign ,target 
                                                           (op lookup-variable-value) 
                                                           (const ,exp)
                                                           (reg env))))))

(define (compile-assignment exp target linkage)
    (let ((var (assignment-variable exp))
          (get-value-code (compile (assignment-value exp) 'val 'next)))
        (end-with-linkage linkage
                          (preserving '(env)
                                      get-value-code
                                      (make-instruction-sequence '(env val) (list target)
                                                                 `((perform (op set-variable-value!)
                                                                            (const ,var)
                                                                            (reg val)
                                                                            (reg env))
                                                                   (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
    (let ((var (definition-variable exp))
          (get-value-code (compile (definition-value exp) 'val 'next)))
        (end-with-linkage linkage
                          (preserving '(env)
                                      get-value-code
                                      (make-instruction-sequence '(env val) (list target)
                                                                 `((perform (op define-variable!)
                                                                            (const ,var)
                                                                            (reg val)
                                                                            (reg env))
                                                                   (assign ,target (const ok))))))))

(define label-counter 0)

(define (new-label-number)
    (set! label-counter (+ label-counter 1))
    label-counter)

(define (make-label name)
    (string->symbol 
        (string-append (symbol->string name)
                       (number->string (new-label-number)))))

(define (registers-needed s)
    (if (symbol? s) '() (car s)))

(define (registers-modified s)
    (if (symbol? s) '() (cadr s)))

(define (statements s)
    (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
    (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
    (memq reg (registers-modified seq)))

(define (list-union s1 s2)
    (cond ((null? s1) s2)
          ((memq (car s1) s2)
           (list-union (cdr s1) s2))
          (else
           (cons (car s1)
                 (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
    (cond ((null? s1) '())
          ((memq (car s1) s2)
           (list-difference (cdr s1) s2))
          (else
           (cons (car s1)
                 (list-difference (cdr s1) s2)))))

;; 连接多个代码序列
;; 每次连接两个序列，计算出need和modify的寄存器列表
;; 对seq1和seq2，结果序列的modify是两者modify的并,
;; 而need表示的是结果序列对外的需要，所以是seq1的需要
;; 和只被seq2需要的寄存器列表的并（不算那些seq2需要seq1
;; 写结果的寄存器）
(define (append-instruction-sequences . seqs)
    (define (append-2-sequences seq1 seq2)
        (make-instruction-sequence
            (list-union (registers-needed seq1)
                        (list-difference (registers-needed seq2)
                                         (registers-modified seq1)))
            (list-union (registers-modified seq1)
                        (registers-modified seq2))
            (append (statements seq1)
                    (statements seq2))))
    (define (append-seq-list seqs)
        (if (null? seqs)
            (empty-instruction-sequence)
            (append-2-sequences (car seqs)
                                (append-instruction-sequences (cdr seqs)))))
    (append-seq-list seqs))

;; 为if分支连接两个代码序列
;; 只会有一个分支被执行，但是整体语句的need和modify
;; 仍是两者列表的并
(define (parallel-instruction-sequences seq1 seq2)
    (make-instruction-sequence
        (list-union (registers-needed seq1)
                    (registers-needed seq2))
        (list-union (registers-modified seq1)
                    (registers-modified seq2))
        (append (statements seq1) (statements seq2))))

(define (compile-if exp target linkage)
    (let ((t-branch (make-label 'true-branch))
          (f-branch (make-label 'false-branch))
          (after-if (make-label 'after-if)))
        (let ((consequent-linkage
               (if (eq? linkage 'next) after-if linkage)))
            (let ((p-code (compile (if-predicate exp) 'val 'next))
                  (c-code (compile (if-consequent exp) target consequent-linkage))
                  (a-code (compile (if-alternative exp) target linkage)))
                (preserving '(env continue)
                            p-code
                            (append-instruction-sequences
                                (make-instruction-sequence '(val) '()
                                    `((test (op false?) (reg val))
                                      (branch (label ,f-branch))))
                                (parallel-instruction-sequences
                                    (append-instruction-sequences t-branch c-code)
                                    (append-instruction-sequences f-branch a-code))
                                after-if))))))

(define (compile-sequence seq target linkage)
    (if (last-exp? seq)
        (compile (first-exp seq) target linkage)
        (preserving '(env continue)
                    (compile (first-exp seq) target 'next)
                    (compile-sequence (rest-exps seq) target linkage))))

(define (make-compiled-procedure entry env)
    (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
    (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc)
    (cadr c-proc))

(define (compiled-procedure-env c-proc)
    (caddr c-proc))

;; 将一个函数体连接到代码序列
;; 函数不会立即执行，所以need和modify按seq算
(define (tack-on-instruction-sequence seq body-seq)
    (make-instruction-sequence
        (registers-needed seq)
        (registers-modified seq)
        (append (statements seq) (statements body-seq))))

;; 编译一个lambda语句
;; 即将函数体的入口点赋给target，并设置linkage。这一赋值语句之后
;; 即lambda函数的函数体，不会立即执行，只有一个label，以供后续调用。
;; 若linkage指定为next，直接跳转到after-lambda
(define (compile-lambda exp target linkage)
    (let ((proc-entry (make-label 'entry))
          (after-lambda (make-label 'after-lambda)))
        (let ((lambda-linkage
              (if (eq? linkage 'next) after-lambda linkage)))
            (append-instruction-sequences
                (tack-on-instruction-sequence
                    (end-with-linkage lambda-linkage
                        ;; 给target赋值为一个编译过的函数体对象
                        (make-instruction-sequence '(env) (list target)
                                                   `((assign ,target
                                                             (op make-compiled-procedure)
                                                             (label ,proc-entry)
                                                             (reg env)))))
                    (compile-lambda-body exp proc-entry))
                after-lambda))))

;; 编译一个lambda函数体
;; 即生成一个不立即执行的指令序列，按照执行时需要的寄存器来写need和modify
;; 即proc放过程体，argl放实参，env放环境，进入proc-entry时先用形参和实参
;; 扩展环境，后面跟上过程体的语句序列
(define (compile-lambda-body exp proc-entry)
    (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
            (make-instruction-sequence '(env proc argl) '(env)
                                       `(,proc-entry
                                            (assign env (op compiled-procedure-env) (reg proc))
                                            (assign env (op extend-environment)
                                                        (const ,formals)
                                                        (reg argl)
                                                        (reg env))))
            ;; 函数体的返回值统一放val、执行完后统一return
            (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
    (let ((proc-code (compile (operator exp) 'proc 'next))
          (operand-codes
           (map (lambda (operand) (compile operand 'val 'next))
                (operands exp))))
        (preserving '(env continue)
                    proc-code
                    (preserving '(proc continue)
                                (construct-arglist operand-codes)
                                (compile-procedure-call target linkage)))))

;; 构造argl实参列表
;; operand-codes是编译过的所有实参的列表
;; 若其为空，则直接返回给argl赋值为空列表的语句
;; 否则看operand-codes是否只有一个元素，若是，则
;; 在其代码（最终会把结果赋给val，见compile-application中的map）后面加上
;; 给argl赋值为单元素列表的语句
;; 否则将code-to-get-last-arg这一语句和后面处理的结果连接起来（code-get-rest-args的结果）
(define (construct-arglist operand-codes)
    ;; 先初始化argl为最后一个编译后的实参，然后再逐步倒着往前cons编译后的实参到argl
    (let ((operand-codes (reverse operand-codes)))
        (if (null? operand-codes)
            (make-instruction-sequence '() '(argl)
                                       `((assign argl (const ()))))
            (let ((code-to-get-last-arg
                  (append-instruction-sequences
                    (car operand-codes)
                    (make-instruction-sequence '(val) '(argl)
                                               `((assign argl (op list) (reg val)))))))
                (if (null? (cdr operand-codes))
                    code-to-get-last-arg
                    (preserving '(env)
                                code-to-get-last-arg
                                (code-to-get-rest-args
                                    (cdr operand-codes))))))))

;; 更新argl，构造参数列表，被construct-arglist调用
;; 对于operand-codes中的每一个实参的代码序列，逐步生成代码，
;; 即实参的代码，后面跟上对argl的更新（用cons）
(define (code-to-get-rest-args operand-codes)
    (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 `((assign argl
                                                           (op cons)
                                                           (reg val)
                                                           (reg argl)))))))
        ;; 与construct-arglist后半段类似
        (if (null? (cdr operand-codes))
            code-for-next-arg
            (preserving '(env)
                        code-for-next-arg
                        (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
    (let ((primitive-branch (make-label 'primitive-branch))
          (compiled-branch (make-label 'compiled-branch))
          (after-call (make-label 'after-call)))
        (let ((compiled-linkage
              (if (eq? linkage 'next) after-call linkage)))
            (append-instruction-sequences
                ;; 这段指令，测试proc中的过程体对象是不是primitive
                (make-instruction-sequence 
                    '(proc) '()
                    `((test (op primitive-procedure?)
                            (reg proc))
                      (branch (label ,primitive-branch))))
                ;; 两种测试结果，只有一个执行，类似if
                (parallel-instruction-sequences
                    ;; 应用一个compiled procedure
                    (append-instruction-sequences
                        compiled-branch
                        ;; 生成应用该函数的代码序列
                        (compile-proc-appl target compiled-linkage))
                    ;; 应用一个primitive函数
                    (append-instruction-sequences
                        primitive-branch
                        (end-with-linkage 
                            linkage
                            (make-instruction-sequence 
                                '(proc argl)
                                (list target)
                                `((assign ,target
                                          (op apply-primitive-procedure)
                                          (reg proc)
                                          (reg argl)))))))
                after-call))))

(define all-regs '(env proc val argl continue))
                   
;; 编译一个compiled procedure的调用
;; target和linkage分别是caller调用callee之后的赋值目标
;; 和continuation
;; 要求proc保存函数体对象，然后获取其代码入口点到val
(define (compile-proc-appl target linkage)
    (cond 
          ;; caller返回值存入val，linkage是个label
          ;; caller调用完callee之后直接跳到label，val已经是返回值
          ((and (eq? target 'val) (not (eq? linkage 'return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,linkage))
                                        (assign val (op compiled-procedure-entry)
                                                    (reg proc))
                                        (goto (reg val)))))

          ;; caller返回值存入其它寄存器
          ;; 即调用完callee之后跳转到一个label
          ;; 要求caller调用完callee后返回到一个中间proc-return，设置
          ;; 返回值到target，然后再跳转到linkage
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
           (let ((proc-return (make-label 'proc-return)))
            (make-instruction-sequence '(proc) all-regs
                                       `((assign continue (label ,proc-return))
                                         (assign val (op compiled-procedure-entry)
                                                     (reg proc))
                                         (goto (reg val))
                                         ,proc-return
                                         ;; caller返回值存入target
                                         (assign ,target (reg val))
                                         (goto (label ,linkage))))))

          ;; caller返回值存入val，且是return
          ((and (eq? target 'val) (eq? linkage 'return))
           (make-instruction-sequence '(proc continue) all-regs
                                      `((assign val (op compiled-procedure-entry)
                                                    (reg proc))
                                        (goto (reg val)))))
          ;; caller返回值存入其它寄存器，且return
          ;; 有问题：caller要求直接return，即(goto (reg continue))
          ;; callee默认的是返回值存val，linkage是return
          ;; 所以这里需要先停下来将返回值从val给到其它寄存器
          ;; 于是就要求callee里面默认的return返回到一个临时的label处，
          ;; 于是就需要设置continue
          ((and (not (eq? target 'val)) (eq? linkage 'return))
           (error "return linkage, target not val -- compile" target))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(compile 
    '(define (fact n)
        (if (= n 1)
            1
            (* (fact (- n 1)) n)))
    'val
    'next)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explicit-control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eceval-operations
    `(
      (eq? ,eq?)
      (cons ,cons)
      (list ,list)
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
      (cond? ,cond?)
      (lambda? ,lambda?)
      (begin? ,begin?)
      (application? ,application?)
      (lookup-variable-value ,lookup-variable-value)
      (text-of-quotation ,text-of-quotation)
      (lambda-parameters ,lambda-parameters)
      (lambda-body ,lambda-body)
      (make-procedure ,make-procedure)
      (operands ,operands)
      (operator ,operator)
      (no-operands? ,no-operands?)
      (empty-arglist ,empty-arglist)
      (first-operand ,first-operand)
      (last-operand? ,last-operand?)
      (rest-operands ,rest-operands)
      (primitive-procedure? ,primitive-procedure?)
      (compound-procedure? ,compound-procedure?)
      (apply-primitive-procedure ,apply-primitive-procedure)
      (procedure-parameters ,procedure-parameters)
      (procedure-environment ,procedure-environment)
      (extend-environment ,extend-environment)
      (adjoin-arg ,adjoin-arg)
      (procedure-body ,procedure-body)
      (begin-actions ,begin-actions)
      (first-exp ,first-exp)
      (last-exp? ,last-exp?)
      (rest-exps ,rest-exps)
      (true? ,true?)
      (false? ,false?)
      (if-predicate ,if-predicate)
      (if-alternative ,if-alternative)
      (if-consequent ,if-consequent)
      (assignment-variable ,assignment-variable)
      (assignment-value ,assignment-value)
      (set-variable-value! ,set-variable-value!)
      (definition-variable ,definition-variable)
      (definition-value ,definition-value)
      (define-variable! ,define-variable!)
      (user-print ,user-print)
      (cond->if ,cond->if)
      (cond-clauses ,cond-clauses)
      (cond-predicate ,cond-predicate)
      (cond-actions ,cond-actions)
      (cond-first-clause ,cond-first-clause)
      (cond-rest-clauses ,cond-rest-clauses)
      (cond-last-clause? ,cond-last-clause?)
      (compiled-procedure-entry ,compiled-procedure-entry)
      (compiled-procedure-env ,compiled-procedure-env)
      (apply-primitive-procedure ,apply-primitive-procedure)
      (make-compiled-procedure ,make-compiled-procedure)
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
                (test (op cond?) (reg exp))
                (branch (label ev-cond))
                (test (op lambda?) (reg exp))
                (branch (label ev-lambda))
                (test (op begin?) (reg exp))
                (branch (label ev-begin))
                (test (op application?) (reg exp))
                (branch (label ev-application))
                (perform (op user-print) (reg exp))
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

            ev-cond
                ;;(assign exp (op cond->if) (reg exp))
                ;;(goto (label ev-if))

                ;; unev: all clauses
                (assign unev (op cond-clauses) (reg exp))
                ;; exp: first clause
            ev-cond-clauses
                (assign exp (op cond-first-clause) (reg unev))
                (test (op cond-last-clause?) (reg unev))
                (branch (label ev-cond-last-clause))
                (save exp)
                (save env)
                (save unev)
                (save continue)
                (assign exp (op cond-predicate) (reg exp))
                (assign continue (label ev-cond-decide))
                (goto (label eval-dispatch))
            ev-cond-last-clause
                (assign val (op cond-predicate) (reg exp))
                (test (op eq?) (reg val) (const else))
                (branch (label ev-cond-actions))
                (save exp)
                (save env)
                (save unev)
                (save continue)
                (assign exp (op cond-predicate) (reg exp))
                (assign continue (label ev-cond-last-decide))
                (goto (label eval-dispatch))
            ev-cond-decide
                (restore continue)
                (restore unev)
                (restore env)
                (restore exp)
                (test (op true?) (reg val))
                (branch (label ev-cond-actions))
                (assign unev (op cond-rest-clauses) (reg unev))
                (goto (label ev-cond-clauses))
            ev-cond-last-decide
                (restore continue)
                (restore unev)
                (restore env)
                (restore exp)
                (test (op true?) (reg val))
                (branch (label ev-cond-actions))
                (assign val (const #f))
                (goto (reg continue))
            ev-cond-actions
                (assign unev (op cond-actions) (reg exp))
                ;; get back to continue after sequence is done
                (save continue)
                (goto (label ev-sequence))

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

            unknown-expression-type
                (assign val (const unknown-expression-type-error))
                (goto (label signal-error))
            unknown-procedure-type
                (restore continue)
                (assign val (const unknown-procedure-type-error))
                (goto (label signal-error))
            signal-error
                (perform (op user-print) (reg val))
                (goto (label read-eval-print-loop))
        )))

;; (start eceval)


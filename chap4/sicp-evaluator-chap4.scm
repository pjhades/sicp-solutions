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

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


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
  (show-prompt input-prompt)
  (let ((input (read)))
    (let ((output (EVAL input the-global-environment)))
      (if (and (pair? output)
               (eq? (car output) '*error*))
          (begin (for-each (lambda (x) (display x) (display " ")) (cdr output))
                 (newline))
          (begin (show-prompt output-prompt)
                 (user-print output)))))
  (driver-loop))

(define (show-prompt string)
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


; Split syntax analysis from execution
;(define (EVAL exp env)
;  ((analyze exp) env))

;(define (analyze exp)
;  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
;        ((quoted? exp) (analyze-quoted exp))
;        ((variable? exp) (analyze-variable exp))
;        ((assignment? exp) (analyze-assignment exp))
;        ((definition? exp) (analyze-definition exp))
;        ((if? exp) (analyze-if exp))
;        ((let? exp) (analyze-application (let->combination exp)))
;        ((lambda? exp) (analyze-lambda exp))
;        ((begin? exp) (analyze-sequence (begin-actions exp)))
;        ((cond? exp) (analyze (cond->if exp)))
;        ((application? exp) (analyze-application exp))
;        (else
;         (list '*error* "ANALYZE: Unknown expression type:" exp))))

; syntax analysis
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (list '*error* "ANALYZE-SEQUENCE: Empty sequence")
        (lambda (env) (execute-sequence procs env)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

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



; Run the evaluator
(define the-global-environment (setup-environment))
(driver-loop)

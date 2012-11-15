;; exercise 4.1
(define (list-of-values-L2R exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (EVAL (first-operand exps) env)))
        (cons first-value
              (list-of-values-L2R (rest-operands exps) env)))))

(define (list-of-values-R2L exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values-R2L (rest-operands exps) env)))
        (cons (EVAL (first-operand exps) env)
              rest-values))))

;; -----------------------------------------------------------------------


;; exercise 4.2
; (call proc arg1 arg2)
(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))

;; -----------------------------------------------------------------------

;; exercise 4.3
(define (make-table)
  (let ((table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr table))))
        (if subtable
            (let ((item (assoc key2 (cdr subtable))))
              (if item
                  (cdr item)
                  #f))
            #f)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr table))))
        (if subtable
            (let ((item (assoc key2 (cdr subtable))))
              (if item
                  (set-cdr! item value)
                  (set-cdr! subtable
                            (cons (cons key2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key1
                                  (cons key2 value))
                            (cdr table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (display "Wrong message -- MAKE-CALL-TABLE"))))
    dispatch))

; Interfaces
(define (get table tag1 tag2)
  ((table 'lookup) tag1 tag2))

(define (put table tag1 tag2 proc)
  ((table 'insert!) tag1 tag2 proc))

; Make a new procedure table
(define call-table (make-table))

; Store table items
(put call-table 'eval 'variable lookup-variable-value)
(put call-table 'eval 'quoted (lambda (exp env)
                                (text-of-quotation exp)))
(put call-table 'eval 'assignment eval-assignment)
(put call-table 'eval 'definition? eval-definition)
(put call-table 'eval 'if eval-if)
(put call-table 'eval 'lambda (lambda (exp env)
                                (make-procedure (lambda-parameters exp)
                                                (lambda-body exp)
                                                env)))
(put call-table 'eval 'begin (lambda (exp env)
                               (eval-sequences (begin-actions exp) env)))
(put call-table 'eval 'cond (lambda (exp env)
                              (EVAL (cond->if exp) env)))
(put call-table 'eval 'application (lambda (exp env)
                                (APPLY (EVAL (operator exp) env)
                                       (list-of-values (operands exp) env))))

(define (get-type exp) (car exp))

(define (get-body exp) (cadr exp))

(define (EVAL exp env)
  (cond ((self-evaluating? exp) exp)
        ((get call-table 'eval '(get-type exp))
         ((get call-table 'eval '(get-type exp)) (get-body exp) env))
        (else
         (display "Uknown expression type -- EVAL"))))

;; -----------------------------------------------------------------------

;; exercise 4.4
; First, add new syntax procedures to EVAL
(define (EVAL exp env)
  ; ......
        ((and? exp) (eval-and (and-exps exp) env))
        ((or? exp) (eval-or (or-exps exp) env))
  ; ......
  )

;; and
; (and exp1 exp2 exp3)
(define (and? exp) (tagged-list? exp 'and))

(define (and-exps exp) (cdr exp))

(define (test-and-exps exps env)
  (cond ((null? exps) '|#t|)
        ((eq? (EVAL (first-exp exps) env) '|#f|) '|#f|)
        (else (test-and-exps (rest-exps exps) env))))

(define (eval-and exps env)
  (if (null? exps)
      '|#t|
      (test-and-exps exps env)))

;; or
; (or exp1 exp2 exp3)
(define (or? exp) (tagged-list? exp 'or))

(define (or-exps exp) (cdr exp))

(define (test-or-exps exps env)
  (cond ((null? exps) '|#f|)
        ((eq? (EVAL (first-exp exps) env) '|#t|) '|#t|)
        (else (test-or-exps (rest-exps exps) env))))

(define (eval-or exps env)
  (if (null? exps)
      '|#f|
      (test-or-exps exps env)))

; Second, evaluate and/or in terms of derived expressions: cond
(define (EVAL exp env)
  ; ......
        ((and? exp) (EVAL (and->cond exp) env))
        ((or? exp) (EVAL (or->cond exp) env))
  ; ......
  )

(define (and->cond exp)
  (if (null? (and-exps exp))
      '|#t|
      (cons 'cond (make-and-clauses (and-exps exp)))))

(define (make-and-clauses exps)
  (if (null? exps)
      (list (list 'else '|#t|))
      (cons (list (list 'not (first-exp exps)) '|#f|)
            (make-and-clauses (rest-exps exps)))))

(define (or->cond exp)
  (if (null? (or-exps exp))
      '|#f|
      (cons 'cond (make-or-clauses (or-exps exp)))))

(define (make-or-clauses exps)
  (if (null? exps)
      (list (list 'else '|#f|))
      (cons (list (first-exp exps) '|#t|)
            (make-or-clauses (rest-exps exps)))))

; Third, evaluate and/or in terms of derived expressions: if
(define (EVAL exp env)
  ; ......
        ((and? exp) (EVAL (and->if exp) env))
        ((or? exp) (EVAL (or->if exp) env))
  ; ......
  )
 
(define (and->if exp)
  (expand-and (and-exps exp)))

(define (expand-and exps)
  (if (null? exps)
      '|#t|
      (make-if (list 'not (first-exp exps))
               '|#f|
               (expand-and (rest-exps exps)))))

(define (or->if exp)
  (expand-or (or-exps exp)))

(define (expand-or exps)
  (if (null? exps)
      '|#f|
      (make-if (first-exp exps)
               '|#t|
               (expand-or (rest-exps exps)))))

;; -----------------------------------------------------------------------

;; exercise 4.5
; Only add these syntax procedures and modify expand-clauses
(define (=>clause? clause) (memq '=> clause))

(define (=>clause-test clause) (car clause))

(define (=>clause-recipient clause) (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      '|#f|
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (cons '*error* "ELSE clause isn't last -- COND->IF")))
              ((=>clause? first)
               (make-if (=>clause-test first)
                        (list (=>clause-recipient first)
                              (=>clause-test first))
                        (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

;; -----------------------------------------------------------------------

;; exercise 4.6
;
;; let
; (let ((var1 exp1)
;       (var2 exp2)
;       (var3 exp3))
;    body)
(define (EVAL exp env)
  ; ......
        ((let? exp) (EVAL (let->combination exp) env))
  ; ......
  )

(define (let? exp) (tagged-list exp 'let))

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

(define (let->combination exp)
  (cons (make-lambda (let-variables (let-bindings exp))
                     (let-body exp))
        (let-exps (let-bindings exp))))

;; -----------------------------------------------------------------------

;; exercise 4.7
;
; let*
; (let* ((var1 exp1)
;        (var2 exp2)
;        (var3 exp3))
;    body)
(define (EVAL exp env)
  ; ......
        ((let*? exp) (EVAL (let*->nested-lets exp) env))
  ; ......
  )

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

;; -----------------------------------------------------------------------

;; exercise 4.8
;
; Named let
; (let foo ((arg1 exp1)
;           (arg2 exp2)
;           (arg3 exp3))
;    exp4
;    (foo 1 2 3))
(define (named-let? exp) (not (pair? (cadr exp))))

(define (named-let-name exp) (cadr exp))

(define (named-let-body exp) (cdddr exp))

(define (named-let-bindings exp) (caddr exp))

(define (named-let-params bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings)
            (named-let-params (cdr bindings)))))

(define (named-let-values bindings)
  (if (null? bindings)
      '()
      (cons (cadar bindings)
            (named-let-values (cdr bindings)))))

(define (make-definition name params body)
  (cons 'define
        (cons (cons name params)
              body)))

(define (make-application name values)
  (cons name values))

(define (let->combination exp)
  (if (named-let? exp)
      (sequence->exp (list (make-definition (named-let-name exp)
                                            (named-let-params (named-let-bindings exp))
                                            (named-let-body exp))
                           (make-application (named-let-name exp)
                                             (named-let-values (named-let-bindings exp)))))
      (cons (make-lambda (let-variables (let-bindings exp))
                         (let-body exp))
            (let-exps (let-bindings exp)))))

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

;; -----------------------------------------------------------------------

;; exercise 4.11
; Just modify the frame operations
(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables)
                  (car values))
            (make-frame (cdr variables)
                        (cdr values)))))

(define (frame-variables frame)
  (if (null? frame)
      '()
      (cons (caar frame)
            (frame-variables (cdr frame)))))

(define (frame-values frame)
  (if (null? frame)
      '()
      (cons (cdar frame)
            (frame-values (cdr frame)))))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

; Or use lists instead
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame var-val-pairs)
  var-val-pairs)

(define (add-binding-to-frame! var val frame)
  (cons (list var val) frame))

(define (extend-environment var-val-pairs base-env)
      (cons (make-frame var-val-pairs) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (cadar frame))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (begin (display "Unbound variable -- LOOKUP-VARIABLE-VALUE") (newline))
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (set-car! frame (list var val)))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (begin (display "Unbound variable -- SET-VARIABLE-VALUE!") (newline))
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar frame))
             (set-car! frame (list var val)))
            (else
             (scan (cdr frame)))))
    (scan frame)))

;; -----------------------------------------------------------------------

;; exercise 4.12
; Extract the common framework of the procedures, do
; things according to the high-order procedures passed in.
(define (env-loop env var if-frame-null if-found)
  (define (scan vars vals)
    (cond ((null? vars) 
           (if-frame-null env))
          ((eq? var (car vars))
           (if-found vals))
          (else
           (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (begin (display "Unbound variable") (newline))
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value var env)
  (define (if-frame-null env)
    (env-loop (enclosing-environment env) var if-frame-null if-found))
  (define (if-found vals)
    (car vals))
  (env-loop env var if-frame-null if-found))

(define (set-variable-value! var val env)
  (define (if-frame-null env)
    (env-loop (enclosing-environment env) var if-frame-null if-found))
  (define (if-found vals)
    (set-car! vals val))
  (env-loop env var if-frame-null if-found))

(define (define-variable! var val env)
  (define (if-frame-null env)
    (add-binding-to-frame! var val (first-frame env)))
  (define (if-found vals)
    (set-car! vals val))
  (env-loop env var if-frame-null if-found))

;; -----------------------------------------------------------------------

;; exercise 4.13
(define (make-unbound! var env)
  (define (scan vars vals)
    (cond ((null? vars)
           (display "Unbound variable") (newline))
          ((eq? var (car vars))
           (set-car! vars '())
           (set-car! vals '()))
          (else
           (scan (cdr vars) (cdr vals)))))
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
          (frame-values frame))))

;; -----------------------------------------------------------------------

;; exercise 4.16

; a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (list '*error* "LOOKUP-VARIABLE-VALUE: Unassigned variable:" var)
                 (car vals)))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (list '*error* "LOOKUP-VARIABLE-VALUE: Unbound variable:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b)
(define (scan-out-defines body)
  (let ((let-bindings '())
        (set-exps '())
        (main-body '()))
    (let scan-iter ((exps body))
      (cond ((null? exps) 
             (make-let let-bindings
                       (append set-exps main-body)))
            ((definition? (car exps))
             (set! let-bindings
                   (cons (list (definition-variable (car exps))
                               ''*unassigned*)
                         let-bindings))
             (set! set-exps
                   (cons (list 'set!
                               (definition-variable (car exps))
                               (definition-value (car exps)))
                         set-exps))
             (scan-iter (cdr exps)))
            (else
             (set! main-body
                   (cons (car exps) main-body))
             (scan-iter (cdr exps)))))))

; c)
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;; exercise 4.17
(define (arrange-body-order body)
  (let ((define-exps '())
        (other-exps '()))
    (let arrange-iter ((exps body))
      (cond ((null? exps)
             (append define-exps other-exps))
            ((definition? (car exps))
             (set! define-exps
                   (cons (car exps) define-exps))
             (arrange-iter (cdr exps)))
            (else
             (set! other-exps
                   (cons (car exps) other-exps))
             (arrange-iter (cdr exps)))))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (arange-body-order body) env))


;; exercise 4.20

; a)
; letrec
; (letrec ((var1 exp1)
;          (var2 exp2)
;          (var3 exp3))
;    body)
(define (EVAL exp env)
  ; ......
        ((letrec? exp) (EVAL (letrec->let exp) env))
  ; ......
  )

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


;; exercise 4.21

; a)
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (f n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (f f (- n 1))
                     (f f (- n 2))))))))
 5)

; b)
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))


;; exercise 4.22
(define (analyze exp)
  (cond ...
        ((let? exp) (analyze-application (let->combination exp)))
        ...))


;; exercise 4.35
(define (an-integer-between i j)
  (require (<= i j))
  (amb i (an-integer-between (+ i 1) j)))


;; exercise 4.36
;; According to DFS, amb tries to search for an answer along the DFS tree.
;; So for a particular i and j, amb will spend its whole life on looking for
;; a solution by traversing all possible values of k, which is impossible.
;; We have to adjust the searching strategy.
(define (a-pythagorean-triple-starting-from low)
  (let* ((k (an-integer-starting-from low))
         (i (an-integer-between low k))
         (j (an-integer-between i k)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))


;; exercise 4.37
;; Yes, this modified version may do better.
;; In this version, we do not need to go over all values in [j, high] in order
;; to find k. We first test if i*i + j*j exceeds the range. If not, we test
;; if its square root is an integer. The complexity is reduced from O((high - low)^3)
;; to O((high - low)^2).

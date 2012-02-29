(define (make-machine register-name ops controller-text)
    (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                    ((machine 'allocate-register) register-name))
                  register-names)
        ((machine 'install-operations) ops)
        ((machine 'install-instruction-sequence)
         (assemble controller-text machine))
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
    (let ((s '()))
        (define (push x)
            (set! s (cons x s)))

        (define (pop)
            (if (null? s)
                (error "empty stack -- pop")
                (let ((top (car s)))
                    (set! s (cdr s))
                     top)))

        (define (initialize)
            (set! s '())
            'done)

        (define (dispatch message)
            (cond ((eq? message 'push) push)
                  ((eq? message 'pop) pop)
                  ((eq? message 'initialze) (initialize))
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
          (the-instruction-sequence '()))
        (let ((the-ops
                  (list (list 'initialize-stack
                            (lambda () (stack 'initialze)))))
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
                            ((instruction-execution-proc (car insts)))
                            (execute)))))

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
        (extract-lables (cdr text)
                        (lambda (insts labels)
                            (let ((next-inst (car text)))
                                (if (symbol? next-inst)
                                    (receive insts
                                             (cons (make-label-entry next-inst
                                                                     insts)
                                                   labels))
                                    (receive (cons (make-instruction next-inst)
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

(define (make-instruction text)
    (cons text '()))

(define (instruction-text inst)
    (car inst))

(define (instruction-execution-proc inst)
    (cdr inst))

(define (set-instruction-execution-proc! inst proc)
    (set-cdr! inst proc))

(define (make-label-entry label-name inst)
    (cons label-name inst))

(define (lookup-label labels label-name)
    (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "undefined label -- assemble" label-name))))

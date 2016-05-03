;;;; SICP Chapter 4.1
;;;;
;;;; Author @uents on twitter
;;;;

#lang racket

(require "../misc.scm")

;;;;--------------------
;;;; Data Types
;;;;--------------------

;;; number
(define (analyze-number-value exp)
  (define (eval-number-value env) exp)
  eval-number-value)

;;; string
(define (analyze-string-value exp)
  (define (eval-string-value env) exp)
  eval-string-value)

;;; variable
(define (analyze-variable exp)
  (define (eval-variable env)
	(lookup-variable-value exp env))
  eval-variable)


;;;;--------------------
;;;; Environment
;;;;--------------------

(define (extend-environment vars vals env)
  (with-handlers
	  ([exn:fail? (lambda (exn)
					(error "extend-environment: arguments error:"
						   vars vals))])
	(cons (make-frame vars vals) env)))

(define (make-frame vars vals)
  (let ((frame (make-hash)))
	(map (lambda (var val) (hash-set! frame var val))
		 vars vals)
	frame))

(define (lookup-variable-value var env)
  (define (traverse env)
	(if (eq? env nil)
		(error "lookup-variable-value: unbound variable:" var)
		(let ((frame (car env)))
		  (if (hash-has-key? frame var)
			  (hash-ref frame var)
			  (traverse (cdr env))))))
  (traverse env))

(define (define-variable! var val env)
  (let ((frame (car env)))
	(hash-set! frame var val)))

(define (set-variable-value! var val env)
  (define (traverse env)
	(if (eq? env nil)
		(error "set-variable-value!: unbound variable:" var)
		(let ((frame (car env)))
		  (if (hash-has-key? frame var)
			  (hash-set! frame var val)
			  (traverse (cdr env))))))
  (traverse env))


;;;;--------------------
;;;; Evaluator
;;;;--------------------

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  false))

;;; evaluation
(define (eval-proc proc env)
  (display (format "eval-proc: ~A ~%" proc))
  (proc env))

(define (eval-sequence exps env)
  (if (pair? exps)
	  (last (map (lambda (exp) (eval-proc exp env)) exps))
	  (eval-proc exps env)))

;;; application
(define (apply-proc proc arguments)
  (display (format "apply-proc: ~A ~A ~%" proc arguments))
  (cond ((tagged-list? proc 'primitive)
		 (apply (primitive-object-proc proc) arguments))
		((tagged-list? proc 'procedure)
		 (let ((params (cadr proc))
			   (body (caddr proc))
			   (env (cadddr proc)))
		   (eval-sequence body
						  (extend-environment params arguments env))))
		(else
		 (error "apply: unknown procedure: " proc))))

(define (analyze-application exp)
  (let ((proc (analyze (car exp)))
		(arguments (map analyze (cdr exp))))
	(define (eval-application env)
	  (apply-proc (eval-proc proc env)
				  (map (lambda (arg) (eval-proc arg env)) arguments)))
	eval-application))

;;; compound procedure
(define (make-procedure params body env)
  (list 'procedure params body env))


;;;;--------------------
;;;; Primitive Procedures
;;;;--------------------

(define primitive-procedures
  (list (list '= =)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '< <)
		(list '> >)		
		))

(define (primitive-procedure-names)
  (map car
	   primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
	   primitive-procedures))

(define (primitive-object-proc object)
  (cadr object))


;;;;--------------------
;;;; Special Forms
;;;;--------------------

;;; quotation
(define (analyze-quoted exp)
  (let ((val (cadr exp)))
	(define (eval-quoted env) val)
	eval-quoted))

;;; assignment
(define (analyze-assginment exp)
  (let ((var (cadr exp))
		(val (analyze (caddr exp))))
	(define (eval-assginment env)
	  (set-variable-value! var (val env) env))
	eval-assginment))

;;; definition
(define (analyze-definition exp)
  (let ((var (if (symbol? (cadr exp))
				 (cadr exp)
				 (caadr exp)))
		(val (analyze
			  (if (symbol? (cadr exp))
				  (caddr exp)
				  (cons 'lambda (cons (cdadr exp) (cddr exp)))))))
	(define (eval-definition env)
	  (define-variable! var (val env) env))
	eval-definition))

;;; condition
(define (analyze-if exp)
  (let ((predicate (analyze (cadr exp)))
		(consequent (analyze (caddr exp)))
		(alternative (analyze (cadddr exp))))
	(define (eval-if env)
	  (if (true? (eval-proc predicate env))
		  (eval-proc consequent env)
		  (eval-proc alternative env)))
	eval-if))

;;; lambda
(define (analyze-lambda exp)
  (let ((params (cadr exp))
		(body (analyze-sequence (cddr exp))))
	(define (eval-lambda env)
	  (make-procedure params body env))
	eval-lambda))

;;; begin
(define (analyze-begin exp)
  (analyze-sequence (cdr exp)))


;;;;--------------------
;;;; Derived Expressions
;;;;--------------------

(define (let->combination exp)
  (let ((variables (map car (cadr exp)))
		(expressions (map cadr (cadr exp)))
		(body (cddr exp)))
  (cons (cons 'lambda (cons variables body))
		expressions)))

;;;;--------------------
;;;; Parser
;;;;--------------------

(define (analyze exp)
  (display (format "analize: ~A ~%" exp))
  (cond ((number? exp) (analyze-number-value exp))
		((string? exp) (analyze-string-value exp))
		((symbol? exp) (analyze-variable exp))
		;; special forms
		((tagged-list? exp 'quote) (analyze-quoted exp))
		((tagged-list? exp 'set!) (analyze-assginment exp))
		((tagged-list? exp 'define) (analyze-definition exp))
		((tagged-list? exp 'if) (analyze-if exp))
		((tagged-list? exp 'lambda) (analyze-lambda exp))
		((tagged-list? exp 'begin) (analyze-begin exp))
		;; derived expressions
		((tagged-list? exp 'let) (analyze (let->combination exp)))
		;; application
		((pair? exp) (analyze-application exp))
		(else
		 (analyze-error exp))))

;;; error
(define (analyze-error exp)
  (error "anaylize: unknown expression: " exp))

(require racket/trace)
(trace analyze-error)

;;; sequence
(define-syntax analyze-sequence
  (syntax-rules ()
	((_ exps) (analyze-sequence-original exps))))

(define (analyze-sequence-original exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "analyze sequence: empty sequence")
		(loop (car procs) (cdr procs)))))

(define (analyze-sequence-by-alyssa exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "analyze sequence: empty sequence")
		(lambda (env) (execute-sequence procs env)))))


;;;;--------------------
;;;; REPL
;;;;--------------------

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (setup-environment)
  (let* ((frame (make-hash))
		 (env (extend-environment (primitive-procedure-names)
								  (primitive-procedure-objects)
								  (list frame))))
	(define-variable! 'true (lambda (env) true) env)
	(define-variable! 'false (lambda (env) false) env)
	env))

(define (driver-loop)
  (input-prompt)
  (let* ((input (read))
		 (output (eval-proc (analyze input)
							the-global-environment)))
	(output-prompt)
	(user-print output))
  (driver-loop))

(define (input-prompt)
  (newline)
  (display ";;; M-Eval input:")
  (newline))

(define (output-prompt)
  (newline)
  (display ";;; M-Eval value:")
  (newline))

(define (user-print object)
  (display object)
  (newline))


;;; run repl driver
(define the-global-environment (setup-environment))
(driver-loop)

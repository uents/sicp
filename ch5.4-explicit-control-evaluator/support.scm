;;;; SICP Chapter 5.4
;;;; The Explicit-Control Evaluator
;;;;
;;;; Author @uents on twitter
;;;;

#lang racket


;;;SECTION 4.1.2

(define (tagged-list? exp tag)
  (if (pair? exp)
	  (eq? (car exp) tag)
	  false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
		((string? exp) true)
		(else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

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

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
	  (cadddr exp)
	  'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))


(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))


(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
	  'false						  ; no else clause
	  (let ((first (car clauses))
			(rest (cdr clauses)))
		(if (cond-else-clause? first)
			(if (null? rest)
				(sequence->exp (cond-actions first))
				(error "ELSE clause isn't last -- COND->IF"
					   clauses))
			(make-if (cond-predicate first)
					 (sequence->exp (cond-actions first))
					 (expand-clauses rest))))))


;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define (make-frame vars vals)
  (let ((frame (make-hash)))
	(map (lambda (var val) (hash-set! frame var val))
		 vars vals)
	frame))

(define (extend-environment vars vals env)
  (with-handlers
	  ([exn:fail? (lambda (exn)
					(error "extend-environment: arguments error:"
						   vars vals))])
	(cons (make-frame vars vals) env)))

(define (lookup-variable-value var env)
  (define (traverse env)
	(if (eq? env '())
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
	(if (eq? env '())
		(error "set-variable-value!: unbound variable:" var)
		(let ((frame (car env)))
		  (if (hash-has-key? frame var)
			  (hash-set! frame var val)
			  (traverse (cdr env))))))
  (traverse env))


;;;SECTION 4.1.4

(define (setup-environment)
  (let* ((frame (make-hash))
		 (env (extend-environment (primitive-procedure-names)
								  (primitive-procedure-objects)
								  (list frame))))
	(define-variable! 'true (lambda (env) true) env)
	(define-variable! 'false (lambda (env) false) env)
	env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list '> >)
	(list '< <)
	))

(define (primitive-procedure-names)
  (map car
	   primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
	   primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


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


;;; Simulation of new machine operations needed by
;;;	 eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; Simulation of new machine operations needed for compiled code
;;;	 and eceval/compiler interface (not used by plain eceval machine)
;;; From section 5.5.2 footnote
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))


(provide (all-defined-out))

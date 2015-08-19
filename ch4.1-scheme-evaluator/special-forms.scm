
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


;;; quotation
(define (analyze-quoted exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; assignment
(define (analyze-assginment exp)
  (let ((var (cadr exp))
		(val (analyze (caddr exp))))
	(lambda (env)
	  (set-variable-value! var val env))))

;;; definition
(define (analyze-definition exp)
  (let ((var (if (symbol? (cadr exp))
				 (cadr exp) (caadr exp)))
		(val (analyze (if (symbol? (cadr exp))
						  (caddr exp)
						  (analyze-lambda (cdadr exp) (cddr exp))))))
	(lambda (env)
	  (define-variable! var val env))))

;;; condition
(define (analyze-if exp)
  (let ((predicate (cadr exp))
		(consequent (caddr exp))
		(alternative (cadddr exp)))
	(lambda (env)
	  (if (true? (eval-proc predicate env))
		  (eval-proc consequent env)
		  (eval-proc alternative env)))))

;;; lambda
(define (analyze-lambda exp)
  (let ((params (map analyze (cadr exp)))
		(body (map analyze (cddr exp))))
	(lambda (env)
	  (analyze-procedure params body env))))

;;; sequence
(define (analyze-sequence exp)
  (lambda (env)
	exp)) ;; @@@TODO


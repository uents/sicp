
;;; quotation
(define (make-quotation exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; assignment
(define (make-assginment exp)
  (let ((var (cadr exp))
		(val (parse (caddr exp))))
	(lambda (env)
	  (set-variable-value! var val env))))

;;; definition
(define (make-definition exp)
  (let ((var (if (symbol? (cadr exp))
				 (cadr exp) (caadr exp)))
		(val (parse (if (symbol? (cadr exp))
						(caddr exp)
						(make-lambda (cdadr exp) (cddr exp))))))
	(lambda (env)
	  (define-variable! var val env))))

;;; condition
(define (make-if exp)
  (let ((predicate (cadr exp))
		(consequent (caddr exp))
		(alternative (cadddr exp)))
	(lambda (env)
	  (if (true? (eval-proc predicate env))
		  (eval-proc consequent env)
		  (eval-proc alternative env)))))

;;; lambda
(define (make-lambda exp)
  (let ((params (map parse (cadr exp)))
		(body   (map parse (cddr exp))))
	(lambda (env)
	  (make-procedure params body env))))

;;; sequence
(define (make-sequence exp)
  (lambda (env)
	exp)) ;; @@@TODO


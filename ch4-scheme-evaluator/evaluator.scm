
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
	(lambda (env)
	  (apply-proc (eval-proc proc env)
				  (map (lambda (arg) (eval-proc arg env)) arguments)))))

;;; compound procedure
(define (make-procedure params body env)
  (list 'procedure params body env))


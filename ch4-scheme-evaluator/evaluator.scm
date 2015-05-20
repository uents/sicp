
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
  (let ((tag (car proc)))
	(cond ((eq? tag 'primitive)
		   nil) ;; @@@TODO
		  ((eq? tag 'procedure)
		   (let ((params (cadr proc))
				 (body   (caddr proc))
				 (env    (cadddr proc)))
			 (eval-sequence body
							(extend-environment params arguments env))))
		  (else
		   (error "apply: unknown procedure: " proc)))))

(define (make-application exp)
  (let ((proc (parse (car exp)))
		(arguments (map parse (cdr exp))))
	(lambda (env)
	  (apply-proc (eval-proc proc env)
				  (eval-sequence arguments env)))))

;;; compound procedure
(define (make-procedure params body env)
  (list 'procedure params body env))


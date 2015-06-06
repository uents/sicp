
;;; quotation
(define (analyze-quoted exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; assignment
(define (analyze-assginment exp)
  (let ((var (cadr exp))
		(val (analyze (caddr exp))))
	(lambda (env)
	  (set-variable-value! var (val env) env))))

;;; definition
(define (analyze-definition exp)
  (let ((var (if (symbol? (cadr exp))
				 (cadr exp)
				 (caadr exp)))
		(val (analyze
			  (if (symbol? (cadr exp))
				  (caddr exp)
				  (cons 'lambda (cons (cdadr exp) (cddr exp)))))))
	(lambda (env)
	  (define-variable! var (val env) env))))

;;; condition
(define (analyze-if exp)
  (let ((predicate (analyze (cadr exp)))
		(consequent (analyze (caddr exp)))
		(alternative (analyze (cadddr exp))))
	(lambda (env)
	  (if (true? (eval-proc predicate env))
		  (eval-proc consequent env)
		  (eval-proc alternative env)))))

;;; lambda
(define (analyze-lambda exp)
  (let ((params (cadr exp))
		(body (map analyze (cddr exp))))
	(lambda (env)
	  (make-procedure params body env))))

;;; sequence
(define (analyze-sequence exp)
   (analyze-sequence-original exp))
;  (analyze-sequence-by-alyssa exp))

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
        (error "Empty sequence -- ANALYZE")
		(loop (car procs) (cdr procs)))))

(define (analyze-sequence-by-alyssa exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
		(lambda (env) (execute-sequence procs env)))))

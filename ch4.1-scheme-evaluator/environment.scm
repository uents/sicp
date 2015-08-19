
(define (setup-environment handler)
  (let* ((frame (make-hash))
		 (env (list frame)))
	(if (eq? handler nil)
		false
		(handler env))
	env))

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

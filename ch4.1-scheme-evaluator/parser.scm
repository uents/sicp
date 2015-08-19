
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



;;;; Parser

(define (parse exp)
  (cond ((number? exp) (parse-number exp))
		((string? exp) (parse-string exp))
		((symbol? exp) (parse-variable exp))
		((pair? exp)
		 (let ((operator (car exp)))
		   (cond ((eq? operator 'quote)  (parse-quotation exp))
				 ((eq? operator 'set!)   (parse-assginment exp))
				 ((eq? operator 'define) (parse-definition exp))
				 ((eq? operator 'if)     (parse-if exp))
				 ((eq? operator 'lambda) (parse-lambda exp))
				 ((eq? operator 'begin)  (parse-sequence exp))
				 (else (parse-application exp)))))
		(else
		 (error "parse: unknown expression " exp))))


;;; Number
(define (parse-number exp)
  (lambda (env) exp))

;;; String
(define (parse-string exp)
  (lambda (env) exp))

;;; Variable
(define (parse-variable exp)
  (lambda (env)
	(lookup-variable-value exp env)))

;;; Quotation
(define (parse-quotation exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Assignment
(define (parse-assginment exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Definition
(define (parse-definition exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Condition
(define (parse-if exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Lambda
(define (parse-lambda exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Sequence
(define (parse-sequence exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; Application
(define (parse-application exp)
  (lambda (env)
	exp)) ;; @@@TODO




				 
				

			   



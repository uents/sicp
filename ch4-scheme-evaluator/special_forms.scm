
;;; quotation
(define (make-quotation exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; assignment
(define (make-assginment exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; definition
(define (make-definition exp)
  (lambda (env)
	exp)) ;; @@@TODO

;;; condition
(define (make-if exp)
  (lambda (env)
	exp)) ;; @@@TODO

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


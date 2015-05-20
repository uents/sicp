
;;; number
(define (make-number-value exp)
  (lambda (env) exp))

;;; string
(define (make-string-value exp)
  (lambda (env) exp))

;;; variable
(define (make-variable exp)
  (lambda (env)
	(lookup-variable-value exp env)))


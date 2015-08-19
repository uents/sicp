
;;; number
(define (analyze-number-value exp)
  (define (eval-number-value env) exp)
  eval-number-value)

;;; string
(define (analyze-string-value exp)
  (define (eval-string-value env) exp)
  eval-string-value)

;;; variable
(define (analyze-variable exp)
  (define (eval-variable env)
	(lookup-variable-value exp env))
  eval-variable)


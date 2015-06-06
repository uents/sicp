
;;; number
(define (analyze-number-value exp)
  (lambda (env) exp))

;;; string
(define (analyze-string-value exp)
  (lambda (env) exp))

;;; variable
(define (analyze-variable exp)
  (lambda (env)
	(lookup-variable-value exp env)))
;	((lookup-variable-value exp env) env)))

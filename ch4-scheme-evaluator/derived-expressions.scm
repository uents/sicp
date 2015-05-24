
(define (let->combination exp)
  (let ((variables (map car (cadr exp)))
		(expressios (map cadr (cadr exp))))
		(body (cddr exp)))
  (cons (cons 'lambda (cons variable body))
		expressions))




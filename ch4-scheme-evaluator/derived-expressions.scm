
(define (let->combination exp)
  (let ((variables (map car (cadr exp)))
		(expressions (map cadr (cadr exp))))
		(body (cddr exp)))
  (cons (cons 'lambda (cons variables body))
		expressions))




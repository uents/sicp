
(define escape false)

(call/cc (lambda (cont)
		   (set! escape cont)))

(define (finish)
  (escape (printf "; there are no more values ~%")))

(define fail finish)

(define (amb . values)
  (if (null? values)
	  (fail)
	  (let ((fail-former fail))
		(call/cc
		 (lambda (succeed)
		   (set! fail (lambda ()
						(printf "; fail: ~A ~%" (cdr values))
						(set! fail fail-former)
						(succeed (apply amb (cdr values)))))
		   (succeed (begin
					  (printf "; ok: ~A ~%" (car values))
					  (car values))))))))

(define-syntax try-again
  (syntax-rules ()
	((_) (fail))))

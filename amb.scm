
(define *paths* '())

(define (choose choices)
  (if (null? choices)
	  (fail)
	  (call/cc
	   (lambda (cc)
		 (set! *paths*
			   (cons (lambda ()
					   (cc (choose (cdr choices))))
					 *paths*))
		 (car choices)))))

(define fail false)

(call/cc
 (lambda (cc)
   (set! fail
		 (lambda ()
		   (if (null? *paths*)
			   (cc '(there are no more values))
			   (let ((proc (car *paths*)))
				 (set! *paths* (cdr *paths*))
				 (proc)))))))

(define (amb . choices)
  (choose choices))

(define (try-again)
  (choose '()))


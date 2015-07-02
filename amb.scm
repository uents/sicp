
(define *alternatives* '())

(define (choose choices)
  (if (null? choices)
	  (try-again)
	  (call/cc
	   (lambda (cc)
		 (define try-next
		   (lambda () (cc (choose (cdr choices)))))
		 (set! *alternatives*
			   (cons try-next *alternatives*))
		 (force (car choices))))))


(define try-again false)
	
(call/cc
 (lambda (cc)
   (set! try-again
		 (lambda ()
		   (if (null? *alternatives*)
			   (cc '(there are no more values))
			   (let ((next (car *alternatives*)))
				 (set! *alternatives* (cdr *alternatives*))
				 (next)))))))

(define (amb . choices)
  (choose choices))

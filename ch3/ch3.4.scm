
;;; ex 3.47

(define (mutex-semaphore counter)
  (let (mutex (make-mutex))
	(define (acquire)
	  (mutex 'acquire)
	  (if (> counter 0)
		  (begin (set! counter (- counter 1))
				 (mutex 'release))
		  (begin (mutex 'release)
				 (acquire))))
	(define (release)
	  (mutex 'acquire)
	  (set! counter (+ counter 1))
	  (mutex 'release))
	(define (dispatch m)
	  (cond ((eq? m 'acquire) acquire)
			((eq? m 'release) release)
			(else (error "unknown requeset -- make-semaphore"
						 m))))
	dispatch))

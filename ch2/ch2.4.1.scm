
;; constructors of complex arithmetric data

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular
			  (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
			  (cons (* r (cos a)) (* r (sin a)))))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
			  (cons (sqrt (+ (square x) (square y))) (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
			  (cons r a)))

;; accessors of complex arithmetric data

(define (real-part-rectangular z) (car z))
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-rectangular z) (cdr z))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))

; ....

(define (real-part z)
  (cond ((rectangular? z)
		 (real-part-rectangular (contents z)))
		((polar? z)
		 (real-part-polar (contents z)))
		(else (error "Unknown type -- REAL-PART " z))))
  



;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.4
;;;;
;;;; Author: @uents on twitter
;;;;

(load-relative "../misc.scm")
(load-relative "streams.scm")


(define (integral delayed-integrand initial-value dt)
  (define int
	(cons-stream initial-value
				 (let ((integrand (force delayed-integrand)))
				   (add-streams (scale-stream integrand dt)
								int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; racket@> (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;; => 2.716923932235896


;;; ex 3.77
(define (integral-ex delayed-integrand initial-value dt)
  (cons-stream initial-value
			   (let ((integrand (force delayed-integrand)))
				 (if (stream-null? integrand)
					 the-empty-stream
					 (integral-ex (stream-cdr integrand)
								  (+ initial-value
									 (* (stream-car integrand) dt))
								  dt)))))

(define (solve-ex f y0 dt)
  (define y (integral-ex (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; racket@> (stream-ref (solve-ex (lambda (y) y) 1 0.001) 1000)
;; => 2.716923932235896


;;; ex 3.78
(define (solve-2nd dy0 y0 a b dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral
			  (delay (add-streams
					  (scale-stream dy a)
					  (scale-stream y b)))
			  dy0 dt))
  y)

;; racket@> (stream-ref (solve-2nd 1 1 0 1 0.001) 1000)
;; => 2.716923932235896
;; racket@> (stream-ref (solve-2nd 1 1 2 -1 0.001) 1000)
;; => 2.716923932235896

;;; ex 3.79
(define (solve-2nd-ex f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)


;; racket@> (stream-ref (solve-2nd-ex (lambda (dy y) y) 1 1 0.001) 1000)
;; => 2.716923932235896
;; racket@> (stream-ref
;; 		  (solve-2nd-ex (lambda (dy y) (+ (* dy 2) (* y -1)))
;; 						1 1 0.001) 1000)
;; => 2.716923932235896

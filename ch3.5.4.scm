;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.4
;;;;
;;;; Author: @uents on twitter
;;;;
;;;; Usage:
;;;;
;;;; 0. Setup Geiser on Emacs
;;;;     M-x package-install geiser
;;;;
;;;; 1. Download source codes
;;;;     git clone https://github.com/uents/sicp.git
;;;;
;;;; 2. Start Emacs and Racket REPL (M-x run-racket)
;;;;
;;;; 3. Executes below commands on Racket REPL
;;;;
;;;;   (load "ch3.5.4.scm")
;;;;   ....
;;;;

(load "misc.scm")
(load "streams.scm")


(define (integral delayed-integrand initial-value dt)
  (define int
	(stream-cons initial-value
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
  (stream-cons initial-value
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
  (define ddy (high-stream-map f dy y))
  y)


;; racket@> (stream-ref (solve-2nd-ex (lambda (dy y) y) 1 1 0.001) 1000)
;; => 2.716923932235896
;; racket@> (stream-ref
;; 		  (solve-2nd-ex (lambda (dy y) (+ (* dy 2) (* y -1)))
;; 						1 1 0.001) 1000)
;; => 2.716923932235896

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



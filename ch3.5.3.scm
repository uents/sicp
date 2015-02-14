;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.3 
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
;;;;   (load "ch3.5.3.scm")
;;;;   ....
;;;;

(load "misc.scm")
(load "streams.scm")


(define (integral integrand initial-value dt)
  (define int
	(stream-cons initial-value
				 (add-streams (scale-stream integrand dt)
							  int)))
  int)


;; ex 3.73

(define (RC R C dt)
  (define (proc integrand v0)
	(add-streams
	 (scale-stream integrand R)
	 (integrand (scale-stream integrand (/ 1 C))
				v0 dt)))
  proc)


;; ex 3.74


;; ex 3.75


;; ex 3.76


;; ex 3.77


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


;;; ex 3.73

(define (RC R C dt)
  (define (proc integrand v0)
	(add-streams
	 (scale-stream integrand R)
	 (integrand (scale-stream integrand (/ 1 C))
				v0 dt)))
  proc)


;;; ex 3.74

(define (sign-change-detector x last)
  (cond ((and (< x 0) (> last 0)) -1)
		((and (> x 0) (< last 0)) 1)
		(else 0)))

(define sense-data
  (list->stream
   (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings
  (high-stream-map
   sign-change-detector sense-data (stream-cons 0 sense-data)))

;; racket@> (map (lambda (i) (stream-ref zero-crossings i))
;; 			  (enumerate-interval 0 12))
;; '(0 0 0 0 0 -1 0 0 0 0 1 0 0)


;;; ex 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
	(stream-cons (sign-change-detector avpt last-avpt)
				 (make-zero-crossings (stream-cdr input-stream)
									  (stream-car input-stream)
									  avpt))))
(define smooth-sense-data
  (make-zero-crossings sense-data 0 0))

;;; ex 3.76

(define (average x y)
  (/ (+ x y) 2))

(define (smooth input-stream)
  (high-stream-map average
				   input-stream
				   (stream-cons 0 input-stream)))

(define (make-zero-crossings input-stream)
  (high-stream-map sign-change-detector
				   input-stream
				   (stream-cons 0 input-stream)))

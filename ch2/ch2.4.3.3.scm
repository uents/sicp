;;;; SICP Chapter 2.4.3
;;;;  Data-Directed Programming and Additivity
;;;;
;;;; Author @uents on twitter
;;;;

#lang racket

(require "../misc.scm")


(define (make-from-real-imag x y)
  (define (dispatch op)
	(cond ((eq? op 'real-part) x)
		  ((eq? op 'imag-part) y)
		  ((eq? op 'magnitude-part)
		   (sqrt (+ (* x x) (* y y))))
		  ((eq? op 'angle-part)
		   (atan y x))
		  (else
		   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;; generic accessors
(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude-part z) (apply-generic 'magnitude-part z))
(define (angle-part z) (apply-generic 'angle-part z))

#|
(magnitude-part (make-from-real-imag 4 3))
;;=> 5
|#

;;;; ex 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
	(cond ((eq? op 'real-part)
		   (* r (cos a)))
		  ((eq? op 'imag-part)
		   (* r (sin a)))
		  ((eq? op 'magnitude-part) r)
		  ((eq? op 'angle-part) a)
		  (else
		   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

#|
(real-part (make-from-mag-ang 2 (/ pi 3)))
;;=> 1.0000000000000002

(imag-part (make-from-mag-ang 2 (/ pi 3)))
;;=> 1.7320508075688772
|#

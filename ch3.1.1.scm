
(load "misc.scm")


;;;; ex 3.1

(define (make-accumulator amount)
  (lambda (value)
	(begin
	  (set! amount (+ amount value))
	  amount)))

; racket@> (define A (make-accumulator 5))
; racket@> (A 10)
; 15
; racket@> (A 10)
; 25



;;;; ex 3.2

(define (make-monitored proc)
  (define counter 0)
  (define (dispatch m)
	(cond ((eq? m 'how-many-calls?)
		   counter)
		  (else
		   (begin (set! counter (+ counter 1))
				  (proc m)))))
  dispatch)

; racket@> (define s (make-monitored sqrt))
; racket@> (s 100)
; 10
; racket@> (s 'how-many-calls?)
; 1





					 

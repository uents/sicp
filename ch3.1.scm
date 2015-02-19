;;;; #lang racket
;;;;
;;;; SICP Chapter 3.1 Assignment and Local State
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
;;;;   (load "ch3.1.scm")
;;;;   ....
;;;;

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


;;;; ex 3.3

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-secure-account balance password)
  (let ((account (make-account balance)))
	(lambda (pw method)
	  (if (eq? pw password)
		  (account method)
		  "Incorrect password"))))


; racket@> (define acc (make-secure-account 100 'secret-password))
; 
; racket@> ((acc 'secret-password 'withdraw) 40)
; 60
; racket@> (acc 'secret-other-password 'deposit)
; "Incorrect password"


;;;; ex 3.4

(define (make-secure-account balance password call-the-cops)
  (let ((account (make-account balance))
		(mistake-counter 0))
	(lambda (pw method)
	  (if (eq? pw password)
		  (begin (set! mistake-counter 0)
				 (account method))
		  (begin (set! mistake-counter (+ mistake-counter 1))
				 (if (< mistake-counter 3)
					 "Incorrect password"
					 (call-the-cops)))))))

; racket@> (define acc (make-secure-account 100 'secret-password (lambda() "Oops!")))
; 
; racket@> ((acc 'secret-password 'withdraw) 40)
; 60
; racket@> (acc 'foo 'withdraw)
; "Incorrect password"
; racket@> (acc 'foo 'withdraw)
; "Incorrect password"
; racket@> (acc 'foo 'withdraw)
; "Oops!"


;;;; randの実装

(define (rand-update x)
  (modulo (+ (* 13 x) 47) 97))

(define random-init 7)

(define rand
  (let ((x random-init))
	(lambda ()
	  (set! x (rand-update x))
	  x)))



;;;; ex 3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ (* trials-passed 1.0) trials)) ;;小数で返すように1.0を掛ける
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (range low high x)
  (+ low (modulo x (+ 1 (- high low)))))

(define (random-in-range low high)
  (range low high (rand)))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (let ((area (* (- x2 x1) (- y2 y1)))
		(passed-ratio (monte-carlo
					   trials
					   (lambda ()
						 (let ((x (random-in-range x1 x2))
							   (y (random-in-range y1 y2)))
						   (predicate x y))))))
	(* area passed-ratio)))

; racket@> (estimate-integral
; 		  (lambda (x y)
; 			(<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
; 		  2 4 8 10 1000000)
; => 21.750012
;
; racket@> (* (square 3) pi)
; 28.274333882308138			 
;
; 精度がいまいちなのは小数の領域を拾っていないから

;;
;; monte-carlo を使わないベタ書き版。
;; 題意を汲み取れていないので、いまいちな解答
;;
;; (define (estimate-integral predicate x1 y1 x2 y2 trials)
;;   (define (iter remain passed)
;; 	(let ((x (random-in-range x1 x2))
;; 		  (y (random-in-range y1 y2)))
;; 	  (cond ((= remain 0)
;; 			 (/ (* (- x2 x1) (- y2 y1) passed 1.0) trials))
;; 			((predicate x y)
;; 			 (iter (- remain 1) (+ passed 1)))
;; 			(else
;; 			 (iter (- remain 1) passed)))))
;;   (iter trials 0))



;;; ex 3.6

(define rand-ex
  (let ((x random-init))
	(define (generate)
	  (set! x (rand-update x))
	  x)
	(define (reset)
	  (set! x random-init)
	  x)
	(define (dispatch m)
	  (cond ((eq? m 'generate) generate)
			((eq? m 'reset) reset)
			(else (error "Unknown request -- RAND" m))))
	dispatch))

;; racket@> ((rand-ex 'reset))
;; => 7
;; racket@> ((rand-ex 'generate))
;; => 41
;; racket@> ((rand-ex 'generate))
;; => 95
;; racket@> ((rand-ex 'generate))
;; => 21
;; racket@> ((rand-ex 'reset))
;; => 7
;; racket@> ((rand-ex 'generate))
;; => 41
;; racket@> ((rand-ex 'generate))
;; => 95
;; racket@> ((rand-ex 'generate))
;; => 21


;;;; ex 3.7

(define (make-joint another-account another-password password)
  (lambda (pw method)
	(if (eq? pw password)
		(another-account another-password method)
		"Incorrect password")))

; racket@> (define peter-acc (make-secure-account 100 'open-sesame))
; racket@> ((peter-acc 'open-sesame 'withdraw) 0)
; 100
; racket@> (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; racket@> ((paul-acc 'rosebud 'withdraw) 40)
; 60
; racket@> ((peter-acc 'open-sesame 'withdraw) 0)
; 60


;;;; ex 3.8

(define *zero-evaluated* false)

(define (f x)
  (cond ((= x 0)
		 (begin (set! *zero-evaluated* true)
				0))
		((= x 1)
		 (if (eq? *zero-evaluated* true) 1 0))
		(else
		 "Unexpected argument -- " x)))

		   
; racket@> (set! *zero-evaluated* false)
; racket@> (+ (f 0) (f 1))
; 1

; racket@> (set! *zero-evaluated* false)
; racket@> (+ (f 1) (f 0))
; 0		  


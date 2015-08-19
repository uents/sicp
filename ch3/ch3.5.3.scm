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

(load-relative "../misc.scm")
(load-relative "streams.scm")


;;;; 反復をストリームとして形式化する

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
	(cons-stream 1.0
				 (stream-map (lambda (guess)
							   (sqrt-improve guess x))
							 guesses)))
  guesses)

#|
racket@> (display-stream (sqrt-stream 2))
=> 1.0 1.5 1.4166666666666665 1.4142156862745097 ...
|#


;; from ex 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
			   (add-streams (partial-sums s) (stream-cdr s))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

#|
racket@> (display-stream pi-stream)
=> 4.0 2.666666666666667 3.466666666666667 ... 3.1420954187666665 ...
|#


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))    ; S_{n-1}
		(s1 (stream-ref s 1))    ; S_{n}
		(s2 (stream-ref s 2)))   ; S_{n+1}
	(cons-stream (- s2 (/ (square (- s2 s1))
						  (+ s0 (* -2 s1) s2)))
				 (euler-transform (stream-cdr s)))))

#|
racket@> (display-stream (euler-transform pi-stream))
=> 3.166666666666667 3.1333333333333337 3.1452380952380956 ...
|#

(define (make-tableau transform s)
  (cons-stream s
			   (make-tableau transform
							 (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
			  (make-tableau transform s)))

#|
racket@> (display-stream (accelerated-sequence euler-transform pi-stream))
=> 4.0 3.166666666666667 3.142105263157895 3.141599357319005 3.1415927140337785 3.1415926539752927 3.1415926535911765 3.141592653589778 3.1415926535897953 3.141592653589795 +nan.0 +nan.0 +nan.0 ....
|#


;;; ex 3.63

(define (sqrt-stream-1 x)
  (define guesses
	(cons-stream 1.0
				 (stream-map (lambda (guess)
							   (display (format "guess=~a ~%" guess))
							   (sqrt-improve guess x))
							 guesses)))
  guesses)

(define (sqrt-stream-2 x)
  (cons-stream 1.0
			   (stream-map (lambda (guess)
							 (display (format "guess=~a ~%" guess))
							 (sqrt-improve guess x))
						   (sqrt-stream-2 x))))

;;; ex 3.64

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (let ((s0 (stream-ref s 0))
		(s1 (stream-ref s 1)))
	(if (< (abs (- s0 s1)) tolerance)
		s1
		(stream-limit (stream-cdr s) tolerance))))

#|
racket@> (stream-limit (sqrt-stream 2) 0.01)
=> 1.4142156862745097
racket@> (stream-limit (sqrt-stream 2) 0.001)
=> 1.4142135623746899
|#


;;; ex 3.65


;;;; 対の無限ストリーム


;;; ex 3.66

;;; ex 3.67

;;; ex 3.68

;;; ex 3.69

;;; ex 3.70

;;; ex 3.71

;;; ex 3.72



;;;; 信号としてのストリーム

(define (integral integrand initial-value dt)
  (define int
	(cons-stream initial-value
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
  (stream-map sign-change-detector
			  sense-data
			  (cons-stream 0 sense-data)))

;; racket@> (map (lambda (i) (stream-ref zero-crossings i))
;; 			  (enumerate-interval 0 12))
;; '(0 0 0 0 0 -1 0 0 0 0 1 0 0)


;;; ex 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
	(cons-stream (sign-change-detector avpt last-avpt)
				 (make-zero-crossings (stream-cdr input-stream)
									  (stream-car input-stream)
									  avpt))))
(define smooth-sense-data
  (make-zero-crossings sense-data 0 0))

;;; ex 3.76

(define (smooth input-stream)
  (stream-map average
			  input-stream
			  (cons-stream 0 input-stream)))

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector
			  input-stream
			  (cons-stream 0 input-stream)))

;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.3 
;;;;
;;;; Author: @uents on twitter
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

;; 実行結果は以下の通り。
;; 後者のguessを使わない方は、繰り返しsqrt-streamが
;; 呼ばれてストリームが生成される。
;; ただしメモ化しないストリームでは、guessを使う場合でも
;; 繰り返しsqrt-streamが呼ばれてしまうため、効率に差はなくなる。


#|
racket@> (stream-ref (sqrt-stream-1 2) 5)
guess=1.0 
guess=1.5 
guess=1.4166666666666665 
guess=1.4142156862745097 
guess=1.4142135623746899 
1.414213562373095

racket@> (stream-ref (sqrt-stream-2 2) 5)
guess=1.0 
guess=1.0 
guess=1.5 
guess=1.0 
guess=1.5 
guess=1.4166666666666665 
guess=1.0 
guess=1.5 
guess=1.4166666666666665 
guess=1.4142156862745097 
guess=1.0 
guess=1.5 
guess=1.4166666666666665 
guess=1.4142156862745097 
guess=1.4142135623746899 
1.414213562373095
|#


;;; ex 3.64

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (define (iter s count)
	(let ((s0 (stream-ref s 0))
		  (s1 (stream-ref s 1)))
	  (if (< (abs (- s0 s1)) tolerance)
		  (cons s1 count)
		  (iter (stream-cdr s) (+ count 1)))))
  (iter s 0))

#|
racket@> (sqrt 2 0.01)
=> '(1.4142156862745097 . 2)
racket@> (stream-limit (sqrt-stream 2) 0.00001)
=> '(1.4142135623746899 . 3)

|#


;;; ex 3.65

(define (ln2-sum n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (ln2-sum (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-sum 1)))

#|
racket@> (log 2)
0.6931471805599453

racket@> (map (lambda (n)
				(stream-ref ln2-stream n))
			  (enumerate-interval 0 10))
'(1.0
  0.5
  0.8333333333333333
  0.5833333333333333
  0.7833333333333332
  0.6166666666666666
  0.7595238095238095
  0.6345238095238095
  0.7456349206349207
  0.6456349206349207
  0.7365440115440116)

racket@> (map (lambda (n)
				(stream-ref (euler-transform ln2-stream) n))
			  (enumerate-interval 0 10))
'(0.7
  0.6904761904761905
  0.6944444444444444
  0.6924242424242424
  0.6935897435897436
  0.6928571428571428
  0.6933473389355742
  0.6930033416875522
  0.6932539682539683
  0.6930657506744464
  0.6932106782106783)

racket@> (map (lambda (n)
				(stream-ref (accelerated-sequence euler-transform ln2-stream) n))
			  (enumerate-interval 0 10))
'(1.0
  0.7
  0.6932773109243697
  0.6931488693329254
  0.6931471960735491
  0.6931471806635636
  0.6931471805604039
  0.6931471805599445
  0.6931471805599427
  0.6931471805599454
  +nan.0)
|#


;;;; 対の無限ストリーム

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; from ch 3.5.2
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


;;; ex 3.66

#|
racket@> (map (lambda (k)
				(let ((p (stream-ref (pairs integers integers) k)))
				  (cons k (list p))))
			  (enumerate-interval 0 25))
'((0 (1 1))
  (1 (1 2))
  (2 (2 2))
  (3 (1 3))
  (4 (2 3))
  (5 (1 4))
  (6 (3 3))
  (7 (1 5))
  (8 (2 4))
  (9 (1 6))
  (10 (3 4))
  (11 (1 7))
  (12 (2 5))
  (13 (1 8))
  (14 (4 4))
  (15 (1 9))
  (16 (2 6))
  (17 (1 10))
  (18 (3 5))
  (19 (1 11))
  (20 (2 7))
  (21 (1 12))
  (22 (4 5))
  (23 (1 13))
  (24 (2 8))
  (25 (1 14)))

racket@> (filter
		  (lambda (x) (= (caadr x) 1))
		  (map (lambda (k)
				 (let ((p (stream-ref (pairs integers integers) k)))
				   (cons k (list p))))
			   (enumerate-interval 0 10)))
'((0 (1 1)) (1 (1 2)) (3 (1 3)) (5 (1 4)) (7 (1 5)) (9 (1 6)))

racket@> (filter
		  (lambda (x) (= (caadr x) 2))
		  (map (lambda (k)
				 (let ((p (stream-ref (pairs integers integers) k)))
				   (cons k (list p))))
			   (enumerate-interval 0 20)))
'((2 (2 2)) (4 (2 3)) (8 (2 4)) (12 (2 5)) (16 (2 6)) (20 (2 7)))

racket@> (filter
		  (lambda (x) (= (caadr x) 3))
		  (map (lambda (k)
				 (let ((p (stream-ref (pairs integers integers) k)))
				   (cons k (list p))))
			   (enumerate-interval 0 30)))
'((6 (3 3)) (10 (3 4)) (18 (3 5)) (26 (3 6)))

racket@> (filter
		  (lambda (x) (= (caadr x) 4))
		  (map (lambda (k)
				 (let ((p (stream-ref (pairs integers integers) k)))
				   (cons k (list p))))
			   (enumerate-interval 0 60)))
'((14 (4 4)) (22 (4 5)) (38 (4 6)) (54 (4 7)))
|#

a
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

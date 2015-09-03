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

(define (pairs-index i j)
  (letrec ((iter (lambda (i j)
				   (cond ((> i j) (error "unexpected index " i j))
						 ((and (= i 0) (= j 0)) 0)
						 ((= i j) (+ (iter (- i 1) (- j 1)) (expt 2 i)))
						 ((= i (- j 1)) (+ (iter i (- j 1)) (expt 2 i)))
						 (else (+ (iter i (- j 1)) (expt 2 (+ i 1))))))))
	(iter (- i 1) (- j 1))))

#|
racket@> (pairs-index 1 100)
197
racket@> (pairs-index 99 100)
950737950171172051122527404030
racket@> (pairs-index 100 100)
1267650600228229401496703205374

;; 答え合わせ

racket@> (stream-ref (pairs integers integers) 197)
=> '(1 100)
racket@> (stream-ref (pairs integers integers) 950737950171172051122527404030)
=> いくら待っても返らない...

|#


;;; ex 3.67

(define (pairs-ex s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
	(interleave
	 (stream-map (lambda (x) (list (stream-car s) x))
				 (stream-cdr t))
	 (stream-map (lambda (x) (list x (stream-car t)))
				 (stream-cdr s)))
	(pairs (stream-cdr s) (stream-cdr t)))))

#|
racket@> (map (lambda (k)
				(let ((p (stream-ref (pairs-ex integers integers) k)))
				  (cons k (list p))))
			  (enumerate-interval 0 25))
'((0 (1 1))
  (1 (1 2))
  (2 (2 2))
  (3 (2 1))
  (4 (2 3))
  (5 (1 3))
  (6 (3 3))
  (7 (3 1))
  (8 (2 4))
  (9 (1 4))
  (10 (3 4))
  (11 (4 1))
  (12 (2 5))
  (13 (1 5))
  (14 (4 4))
  (15 (5 1))
  (16 (2 6))
  (17 (1 6))
  (18 (3 5))
  (19 (6 1))
  (20 (2 7))
  (21 (1 7))
  (22 (4 5))
  (23 (7 1))
  (24 (2 8))
  (25 (1 8)))
|#


;;; ex 3.68

;; interleaveの呼び出しが無限に続き、処理が返らない


;;; ex 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
	(stream-map (lambda (x) (flatten (list (stream-car u) x)))
				(pairs (stream-cdr s) (stream-cdr t)))
	(triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

#|
racket@> (map (lambda (k)
				(let ((p (stream-ref (triples integers integers integers) k)))
				  (cons k (list p))))
			  (enumerate-interval 0 25))
'((0 (1 1 1))
  (1 (1 2 2))
  (2 (2 2 2))
  (3 (1 2 3))
  (4 (2 3 3))
  (5 (1 3 3))
  (6 (3 3 3))
  (7 (1 2 4))
  (8 (2 3 4))
  (9 (1 3 4))
  (10 (3 4 4))
  (11 (1 2 5))
  (12 (2 4 4))
  (13 (1 4 4))
  (14 (4 4 4))
  (15 (1 2 6))
  (16 (2 3 5))
  (17 (1 3 5))
  (18 (3 4 5))
  (19 (1 2 7))
  (20 (2 4 5))
  (21 (1 4 5))
  (22 (4 5 5))
  (23 (1 2 8))
  (24 (2 3 6))
  (25 (1 3 6)))
|#

(define pythagoras
  (stream-filter (lambda (triple)
				   (= (+ (expt (car triple) 2) (expt (cadr triple) 2))
					  (expt (caddr triple) 2)))
				 (triples integers integers integers)))

#|
racket@> (map (lambda (n)
			    (stream-ref pythagoras n))
			  (enumerate-interval 0 3))
'((3 4 5) (6 8 10) (5 12 13) (9 12 15))
|#

;; ただし (triples integers integers integers) が重いため
;; (stream-ref pythagoras 6) あたりから返ってこなくなる..


;;; ex 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		 (let* ((s1-car (stream-car s1))
				(s2-car (stream-car s2))
				(w1 (weight s1-car))
				(w2 (weight s2-car)))
		   (if (<= w1 w2)
			   (cons-stream s1-car
							(merge-weighted (stream-cdr s1) s2 weight))
			   (cons-stream s2-car
							(merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weight-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
	(stream-map (lambda (x) (list (stream-car s) x))
				(stream-cdr t))
	(weight-pairs (stream-cdr s) (stream-cdr t) weight)
	weight)))

;; a.
(define p1
  (weight-pairs integers integers
				(lambda (pair) (+ (car pair) (cadr pair)))))

#|
racket@> (map (lambda (n) (stream-ref p1 n))
			  (enumerate-interval 0 24))
'((1 1)
  (1 2)
  (1 3)
  (2 2)
  (1 4)
  (2 3)
  (1 5)
  (2 4)
  (3 3)
  (1 6)
  (2 5)
  (3 4)
  (1 7)
  (2 6)
  (3 5)
  (4 4)
  (1 8)
  (2 7)
  (3 6)
  (4 5)
  (1 9)
  (2 8)
  (3 7)
  (4 6)
  (5 5))
|#

;; b.
(define (divisible? n)
  (or (eq? (remainder n 2) 0)
	  (eq? (remainder n 3) 0)
	  (eq? (remainder n 5) 0)))

(define p2
  (stream-filter
   (lambda (pair)
	 (and (not (divisible? (car pair)))
		  (not (divisible? (cadr pair)))))
   (weight-pairs integers integers
				 (lambda (pair)
				   (let ((i (car pair))
						 (j (cadr pair)))
					 (+ (* 2 i) (* 3 j) (* 5 i j)))))))

#|
racket@> (map (lambda (n) (stream-ref p2 n))
			  (enumerate-interval 0 24))
'((1 1)
  (1 7)
  (1 11)
  (1 13)
  (1 17)
  (1 19)
  (1 23)
  (1 29)
  (1 31)
  (7 7)
  (1 37)
  (1 41)
  (1 43)
  (1 47)
  (1 49)
  (1 53)
  (7 11)
  (1 59)
  (1 61)
  (7 13)
  (1 67)
  (1 71)
  (1 73)
  (1 77)
  (1 79))
|#


;;; ex 3.71

(define (cube n) (* n n n))

(define (sum-of-cube pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define (ramanujan s)
  (let* ((s1 (stream-car s))
		 (s2 (stream-car (stream-cdr s)))
		 (w1 (sum-of-cube s1))
		 (w2 (sum-of-cube s2)))
	(if (= w1 w2)
		(cons-stream (list w1 s1 s2)
					 (ramanujan (stream-cdr s)))
		(ramanujan (stream-cdr s)))))

(define ramanujan-numbers
  (ramanujan (weight-pairs integers integers sum-of-cube)))


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

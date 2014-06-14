
;;;; 1.3 高階手続きによる抽象

;;; text code
(define (identify x) x)

(define (inc x) (+ x 1)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;;; 高階手続きとは？
;;;  - 引数に手続きをとり戻値で手続きを返す手続き

;;; ex. 1.30

(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

;; テスト
(map (lambda (x) (sum identify 1 inc x)) (range 1 11))
; => '(1 3 6 10 15 21 28 36 45 55)


;;; ex. 1.31

;; (a) 再帰的プロセス
(define (product term a next b)
  (if (> a b)
	  1
	  (* (term a) (product term (next a) next b))))

;; (b) 反復的プロセス
(define (product term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (* result (term a)))))
  (iter a 1))

;; テスト
(* (product (lambda (n) (/ (* n (+ n 2)) (* (+ n 1) (+ n 1))))
			2
			(lambda (n) (+ n 2))
			100) 4.0)
; => 3.15703...


;;; ex. 1.32

;; (a) 再帰的プロセス
(define (accumulate combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (combiner (term a)
				(accumulate combiner null-value term (next a) next b))))

;; (b) 反復的プロセス
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; テスト
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;;; ex. 1.33

;; 再帰的プロセス
(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
	  null-value
	  (combiner (if (predicate a) (term a) null-value)
				(filtered-accumulate combiner null-value term (next a) next b predicate))))

;; 反復的プロセス
(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (combiner result (if (predicate a) (term a) null-value)))))
  (iter a null-value))

;; (a) a-b区間の素数の二乗の和
(define (prime-square-sum a b)
  (filtered-accumulate + 0 square a inc b prime?)) 

(prime-square-sum 1 10) ; => 88

;; (b) 省略


;;; ex. 1.34

(define (f g) (g 2))

(f f)

; > (f f)
; > (f 2)
; > (2 2)
; となりエラー


;;; ex. 1.37

(define (cont-frac n d k) ; nとdは手続き
  (define (helper i)
	(if (> i k)
		0
		(/ (n i) (+ (d i) (helper (+ i 1))))))
  (helper 0))


;; 誤差をチェック。
;; 小数点以下4 桁の精度の近似を得るにはkが10より大きければよい。

(map (lambda (k) (list k (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
	 (range 20))

;; => 
;; '((0 1.0)
;;   (1 0.5)
;;   (2 0.6666666666666666)
;;   (3 0.6000000000000001)
;;   (4 0.625)
;;   (5 0.6153846153846154)
;;   (6 0.6190476190476191)
;;   (7 0.6176470588235294)
;;   (8 0.6181818181818182)
;;   (9 0.6179775280898876)
;;   (10 0.6180555555555556)
;;   (11 0.6180257510729613)
;;   (12 0.6180371352785146)
;;   (13 0.6180327868852459)
;;   (14 0.6180344478216819)
;;   (15 0.6180338134001252)
;;   (16 0.6180340557275542)
;;   (17 0.6180339631667064)
;;   (18 0.6180339985218034)
;;   (19 0.6180339850173578))


;; 反復的手続き
(define (cont-frac n d k)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


;;; ex. 1.41

; (((double (double double)) inc) 5) ; => 21

(define (double f)
  (lambda (x)
	(f (f x))))

(define (inc x) (+ x 1))


; i. (double inc) の置換モデルを考える
(double inc)

(lambda (x)
  (inc (inc x)))

; さらに5を適用する
((lambda (x)
   (inc (inc x)))
 5) ; => 7

; ii. (double double) の置換モデルを考える
(double double)

(lambda (x)
  (double (double x)))

; さらにincを適用する
((lambda (x)
   (double (double x)))
 inc)

(double (double inc))

(double (lambda (x)
		  (inc (inc x))))

(lambda (x)
  ((lambda (x)
	 (inc (inc x)))
   ((lambda (x)
	  (inc (inc x))) x)))

; さらに5を適用する
((lambda (x)
   ((lambda (x)
	  (inc (inc x)))
	((lambda (x)
	   (inc (inc x))) x)))
5)

((lambda (x)
   (inc (inc x)))
 ((lambda (x)
	(inc (inc x))) 5))

((lambda (x)
   (inc (inc x)))
 7) ; => 9


; iii. (double (double double)) の置換モデルを考える

(double (double double))

(double (lambda (x)
		  (double (double x))))

(lambda (x)
  ((lambda (x)
	 (double (double x)))
   ((lambda (x)
	  (double (double x))) x)))

; さらにincを適用する
((lambda (x)
  ((lambda (x)
	 (double (double x)))
   ((lambda (x)
	  (double (double x))) x)))
 inc)

((lambda (x)
   (double (double x)))
 ((lambda (x)
	(double (double x))) inc))

((lambda (x)
   (double (double x)))
 (double (double inc)))

((lambda (x)
   (double (double x)))
 (lambda (x)
   ((lambda (x)
	  (inc (inc x)))
	((lambda (x)
	   (inc (inc x))) x))))

(lambda (x)
  ((lambda (x)
	 ((lambda (x)
		((lambda (x)
		   (inc (inc x)))
		 ((lambda (x)
			(inc (inc x))) x)))
	  ((lambda (x)
		 ((lambda (x)
			(inc (inc x)))
		  ((lambda (x)
			 (inc (inc x))) x))) x)))
   ((lambda (x)
	  ((lambda (x)
		 ((lambda (x)
			(inc (inc x)))
		  ((lambda (x)
			 (inc (inc x))) x)))
	   ((lambda (x)
		  ((lambda (x)
			 (inc (inc x)))
		   ((lambda (x)
			  (inc (inc x))) x))) x))) x)))


; さらに5を適用する
((lambda (x)
   ((lambda (x)
	  ((lambda (x)
		 ((lambda (x)
			(inc (inc x)))
		  ((lambda (x)
			 (inc (inc x))) x)))
	   ((lambda (x)
		  ((lambda (x)
			 (inc (inc x)))
		   ((lambda (x)
			  (inc (inc x))) x))) x)))
	((lambda (x)
	   ((lambda (x)
		  ((lambda (x)
			 (inc (inc x)))
		   ((lambda (x)
			  (inc (inc x))) x)))
		((lambda (x)
		   ((lambda (x)
			  (inc (inc x)))
			((lambda (x)
			   (inc (inc x))) x))) x))) x)))
5) ; => 21
 

   
;;; ex. 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose inc square) 2) ; => 5
((compose square inc) 2) ; => 9

(define (compose . f)
  (define (helper funcs)
	(let ((func (car funcs))
		  (rest (cdr funcs)))
	  (if (null? rest)
		  func
		  (lambda (x) ((helper rest) (func x))))))
  (helper (reverse f)))

((compose inc square inc) 2) ; => 10
((compose square inc inc) 2) ; => 16


;;; ex. 1.43

;; 再帰的プロセス
(define (repeated f n)
  (if (= n 1)
	  f
	  (compose (repeated f (- n 1)) f)))

;; 反復的プロセス
(define (repeated f n)
  (define (iter count result)
	(if (= count n)
		result
		(iter (+ count 1) (compose f result))))
  (iter 1 f))

	
((repeated square 2) 5) ; => 625



;;; ex 1.5

(deifne (p) (p))

(define (test x y)
  (if (= x 0)
	  0
	  y))

(test 0 (p))


;;; ex 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;;; ex. 1.11

;; 再帰的プロセス
(define (f n)
  (if (< n 3)
	  n
	  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; 反復的プロセス
;; a <- a + 2b + 3c
;; b <- a
;; c <- b
;; と素直に実装
(define (f n)
  (define (iter a b c count)
	(cond ((= count 0) c)
		  ((= count 1) b)
		  ((= count 2) a)
		  (else (iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (iter 2 1 0 n))


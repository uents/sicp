
;;; call/ccで実装したambオペレータ
(load "amb.scm")

;;; SICP本文でのrequireはreqという名前で定義
(define (req p)
  (if (not p) (amb) false))

;;; ambオペレータを再帰的に呼ぶだけに、引数を遅延オブジェクトとする
;;; SICP本文のamb評価器は、遅延評価するため不要
(define (an-element-of items)
  (req (not (null? items)))
  (amb (car items) (delay (an-element-of (cdr items)))))

;;; ambオペレータは特殊形式ではなく手続きの場合、
;;; applyを使って以下のようにもかける
;; (define (an-element-of items)
;;   (apply amb items))


;;;; 4.3.1 ambと検索

(require math/number-theory)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
	(req (prime? (+ a b)))
    (list a b)))

;;(prime-sum-pair '(1 3 5 8) '(20 35 110))

(define (an-integer-starting-from n)
  (amb n (delay (an-integer-starting-from (+ n 1)))))


;;; ex 4.35

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (an-integer-between low high)
  (an-element-of (enumerate-interval low high)))



;;; ex 4.36

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between i high)))
	  (let ((k (an-integer-between j high)))
		(req (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))

(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
	(let ((j (an-integer-starting-from 1)))
	  (let ((k (an-integer-starting-from 1)))
		(req (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))
;; => kだけが増えていくので返ってこない

(define (a-pythagorean-triple)
  (let* ((k (an-integer-starting-from 1))
		 (j (an-integer-between 1 k))
		 (i (an-integer-between 1 j)))
	(req (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))


;;; ex 4.37

(define *try-count* 0)

(define (req p)
  (if (not p)
	  (begin (set! *try-count* (+ *try-count* 1))
			 (amb))
	  false))

(define (a-pythagorean-triple-between-ex low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (req (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (req (integer? k))
          (list i j k))))))

;; racket@> (a-pythagorean-triple-between-ex 1 20)
;; => '(3 4 5)
;; racket@> (try-again)
;; ...
;; racket@> (try-again)
;; => '(there are no more values)
;; racket@> *try-count*
;; => 225

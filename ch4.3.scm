
(define nil '())

(load "amb.scm")

;;; SICP本文でのrequireはnecessaryという名前で定義

(define (necessary p)
  (if (not p) (amb) false))

(define (an-element-of items)
  (necessary (not (null? items)))
  (apply amb items))


;;;; 4.3.1 ambと検索

(require math/number-theory)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
	(necessary (prime? (+ a b)))
    (list a b)))

;(define (an-integer-starting-from n)
;  (amb n (an-integer-starting-from (+ n 1))))



;;; ex 4.35

(define (enumerate-interval low high)
  (if (> low high)
	  nil
	  (cons low (enumerate-interval (+ low 1) high))))

(define (an-integer-between low high)
  (an-element-of (enumerate-interval low high)))


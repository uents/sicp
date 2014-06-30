#lang racket

;;;;--------------------------------------------------------
;;;; chapter 2.2
;;;; Hierarchical Data and the Closure Property
;;;;--------------------------------------------------------


;;;; text codes

(define (list-ref items n)
  (if (= n 0)
	  (car items)
	  (list-ref (cdr items) (- n 1))))

(define squares
  (map (lambda (x) (* x x)) (range 1 6)))

(list-ref squares 3) ;; => 16


(define (length items)
  (if (null? items)
	  0
	  (+ 1 (length (cdr items)))))

(define odds (filter odd? (range 1 8)))

(length odds) ;; => 4


;;; lengthの反復プロセス版
(define (length items)
  (define (iter rest count)
	(if (null? rest)
		count
		(iter (cdr rest) (+ 1 count))))
  (iter items 0))


;;; append
;;; リストをcdrダウンしつつconsアップ

(define (append list1 list2)
  (if (null? list1)
	  list2
	  (cons (car list1) (append (cdr list1) list2))))

(append squares odds) ;; => '(1 4 9 16 1 3 5 7)



;;;; ex. 2.17

(define (last-pair lst)
  (cond ((null? lst) nil)
		((null? (cdr lst)) (car lst))
		(else (last-pair (cdr lst)))))

;;;; ex. 2.18

(define (reverse lst)
  (define (iter lst result)
	(if (null? lst)
		result
		(iter (cdr lst) (cons (car lst) result))))
  (iter lst nil))

;;;; ex. 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

; racket@> (append x y)
; '(1 2 3 4 5 6)
; racket@> (cons x y)
; '((1 2 3) 4 5 6)
; racket@> (list x y)
; '((1 2 3) (4 5 6))


;;;; ex. 2.27

;;; reverseを改変。carの要素がリストの場合は
;;; さらに再帰呼び出しして展開していく

(define (deep-reverse lst)
  (define (iter lst result)
	(cond ((null? lst)
		   result)
		  ((pair? (car lst))
		   (iter (cdr lst) (cons (deep-reverse (car lst)) result)))
		  (else
		   (iter (cdr lst) (cons (car lst) result)))))
  (iter lst nil))

;;;; ex. 2.28

;;;; ex. 2.29

;;;; ex. 2.30

;;;; ex. 2.31

;;;; ex. 2.32

;;;; ex. 2.33

;;;; ex. 2.34

;;;; ex. 2.35

;;;; ex. 2.36

;;;; ex. 2.37

;;;; ex. 2.38

;;;; ex. 2.39

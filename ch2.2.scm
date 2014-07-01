;;; #lang racket

;;;;--------------------------------------------------------
;;;; chapter 2.2
;;;; Hierarchical Data and the Closure Property
;;;;--------------------------------------------------------

(define nil '())

;;;; text code

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

(define (length-iter items)
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

(define (fringe lst)
  (define (iter lst result)
	(cond ((null? lst)
		   result)
		  ((pair? (car lst))
		   (iter (cdr lst)
				 (append result (fringe (car lst)))))
		  (else
		   (iter (cdr lst)
				 (append result (list (car lst)))))))
  (iter lst nil))



;;;; ex. 2.29

;;; text code

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;; a.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;; テスト
(define m
  (make-mobile (make-branch 3
							(make-mobile (make-branch 3 1)
										 (make-branch 1 3)))
			   (make-branch 4
							(make-mobile (make-branch 1 2)
										 (make-branch 2 1)))))

(left-branch m)                      ;; => '(3 ((3 1) (1 3)))
(branch-length    (left-branch m))   ;; => 3
(branch-structure (left-branch m))   ;; => '((3 1) (1 3))

(right-branch m)                     ;; => '(4 ((1 2) (2 1)))
(branch-length    (right-branch m))  ;; => 4
(branch-structure (right-branch m))  ;; => '((1 2) (2 1))



;;; b.

; nil以外の値であればtrue
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

; 錘であればtrue、モービルであればfalse
(define (simple-weight? branch)
  (not (pair? (branch-structure branch))))

(define (total-weight mobile)
  (cond ((atom? mobile) mobile)
		((simple-weight? mobile) (branch-structure mobile))
		(else 
		 (+ (total-weight (branch-structure (left-branch mobile)))
			(total-weight (branch-structure (right-branch mobile)))))))

;; テスト
(total-weight (make-branch 1 3)) ;; => 3
(total-weight m) ;; => 7


;;; c.

(define (moment branch)
  (* (branch-length branch)
	 (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (cond ((atom? mobile) #t)
		((simple-weight? mobile) #t)
		(else
		 (let* ((l (left-branch mobile))
				(r (right-branch mobile)))
		   (and (= (moment l) (moment r))
				(balanced? (branch-structure l))
				(balanced? (branch-structure r)))))))

;; テスト
(moment (make-branch 1 3)) ;; => 3
(moment (left-branch m))   ;; => 12

(balanced? (make-branch 1 3)) ;; => true
(balanced? m) ;; => true


;;; d.

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; アクセサを入れ替えるだけでよい

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))


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

;;;; #lang racket

;;;;--------------------------------------------------------
;;;; chapter 2.2
;;;; Hierarchical Data and the Closure Property
;;;;--------------------------------------------------------

(load "./misc.scm")


;;;; text code

(define (list-ref items n)
  (if (= n 0)
	  (car items)
	  (list-ref (cdr items) (- n 1))))

(define squares
  (map square (range 1 6)))

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

(define (deep-reverse tree)
  (define (iter tree result)
	(cond ((null? tree)
		   result)
		  ((pair? (car tree))
		   (iter (cdr tree) (cons (deep-reverse (car tree)) result)))
		  (else
		   (iter (cdr tree) (cons (car tree) result)))))
  (iter tree nil))

;;; map,reverseを使ったバージョン。簡潔！

(define (deep-reverse tree)
  (if (not (pair? tree))
	  tree
	  (reverse (map deep-reverse tree))))


;;;; ex. 2.28

(define (fringe tree)
  (define (iter tree result)
	(cond ((null? tree)
		   result)
		  ((pair? (car tree))
		   (iter (cdr tree)
				 (append result (fringe (car tree)))))
		  (else
		   (iter (cdr tree)
				 (append result (list (car tree)))))))
  (iter tree nil))



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

;; テストする際は、mの再評価を忘れずに
(define m
  (make-mobile (make-branch 3
							(make-mobile (make-branch 3 1)
										 (make-branch 1 3)))
			   (make-branch 4
							(make-mobile (make-branch 1 2)
										 (make-branch 2 1)))))


;;;; ex. 2.30

(define (square-tree tree)
  (cond ((null? tree) tree)
		((not (pair? tree)) (square tree))
		(else
		 (cons (square-tree (car tree))
			   (square-tree (cdr tree))))))

;; テスト
(square-tree (list 1
				   (list 2 (list 3 4) 5)
				   (list 6 7)))			  
; => '(1 (4 (9 16) 25) (36 49))


;;;; ex. 2.31

(define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (cond ((null? tree) tree)
		((not (pair? tree)) (proc tree))
		(else
		 (cons (tree-map proc (car tree))
			   (tree-map proc (cdr tree))))))

;;;; ex. 2.32

;; <??> は (lambda (x) (cons (car s) x))

(define (subsets s)
  (if (null? s)
	  (list nil)
	  (let ((rest (subsets (cdr s))))
		(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 理由
;; 全ての部分集合は、ひとつの要素とそれ以外の残りの要素との全ての組み合わせから求まるため



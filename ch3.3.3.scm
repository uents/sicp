;;;; #lang racket
;;;;
;;;; SICP Chapter 3.3.3 Representing Tables
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
;;;;   (load "ch3.3.3.scm")
;;;;   ....
;;;;

(load "misc.scm")

;;; ミュータブルデータを使用するのに必要
(require r5rs)


;;;; ex 3.24

; 解答は省略。
; 
; make-table にテスト関数を引数で渡せるようにし、
; assoc-tree 内の equal? の代わりにそのテスト関数を使えばよいだけ。


;;;; ex 3.25

;; record
(define (make-record k v)
  (cons k v))
(define (key record)
  (car record))
(define (value record)
  (cdr record))
(define (set-key! record k)
  (set-car! record k))
(define (set-value! record v)
  (set-cdr! record v))

;; tree
(define (make-tree record next)
  (cons record next))
(define (record tree)
  (car tree))
(define (next tree)
  (cdr tree))
(define (set-record! tree record)
  (set-car! tree record))
(define (set-next! tree next)
  (set-cdr! tree next))

;; tree operations
(define (assoc-tree tree k)
  (cond ((null? tree)
		 false)
		((equal? k (key (record tree)))
		 tree)
		(else (assoc-tree (next tree) k))))

(define (adjoin-tree! tree key-list v)
  (define (make-deep-tree key-list)
	(if (null? (cdr key-list))
		(make-tree (make-record (car key-list) v)
				   (next tree))
		(make-tree (make-record (car key-list)
								(make-deep-tree (cdr key-list)))
				   (next tree))))
  (set-next! tree (make-deep-tree key-list)))

(define (make-table-tree)
  (make-tree (make-record '*table* nil) nil))

;; table
(define (make-table)
  (let ((the-tree (make-table-tree)))
	(define (lookup key-list)
	  (define (iter tree key-list)
;		(display (format "lookup t=~A k=~A ~%" tree key-list))
		(if (null? key-list)
			false
			(let ((t (assoc-tree tree (car key-list))))
			  (if t
				  (if (null? (cdr key-list))
					  (value (record t))
					  (iter (value (record t)) (cdr key-list)))
				  false))))
	  (iter the-tree key-list))
	(define (insert! key-list v)
	  (define (iter tree key-list)
;		(display (format "insert! t=~A k=~A v=~A ~%" tree key-list v))
		(if (null? key-list)
			false
			(let ((t (assoc-tree tree (car key-list))))
			  (if t
				  (if (null? (cdr key-list))
					  (set-value! (record t) v)
					  (iter (value (record t)) (cdr key-list)))
				  (adjoin-tree! tree key-list v)))))
	  (iter the-tree key-list))
	(define (print)
	  (begin
		(display the-tree (current-error-port))
		(newline (current-error-port))))

	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			((eq? m 'print-proc) print)
			(else (error "dispatch -- unknown operation" m))))
	dispatch))


(define (lookup-table table key)
  ((table 'lookup-proc) key))
(define (insert-table! table key value)
  ((table 'insert-proc!) key value))
(define (print-table table)
  ((table 'print-proc)))


;;; test

;; racket@> (define tbl (make-table))
;; racket@> (insert-table! tbl (list 1 2) "one two")
;; racket@> (insert-table! tbl (list 1 3) "one three")
;; racket@> (insert-table! tbl (list 2 3) "two three")
;; racket@> (insert-table! tbl (list 1 4) "one four")
;; racket@> (insert-table! tbl (list 2 4) "two four")
;; racket@> (print-table tbl)
;; ((*table*) (2 (3 . two three) (4 . two four)) (1 (2 . one two) (4 . one four) (3 . one three)))

;; racket@> (lookup-table tbl (list 1 2))
;; "one two"
;; racket@> (lookup-table tbl (list 1 1))
;; #f
;; racket@> (lookup-table tbl (list 2 1))
;; #f
;; racket@> (lookup-table tbl (list 2 3))
;; "two three"



;;;; ex 3.26

;; tree
(define (make-tree record left right)
  (list record left right))
(define (record tree)
  (car tree))
(define (left tree)
  (cadr tree))
(define (right tree)
  (caddr tree))
(define (set-record! tree record)
  (set-car! tree record))
(define (set-left! tree left)
  (set-car! (cdr tree) left))
(define (set-right! tree right)
  (set-car! (cddr tree) right))

;; tree operations
(define (assoc-tree tree k)
  (cond ((null? tree)
		 false)
		((equal? k (key (record tree)))
		 tree)
		((< k (key (record tree)))
		 (assoc-tree (left tree) k))
		(else
		 (assoc-tree (right tree) k))))

(define (adjoin-tree! tree key-list v)
  (define (make-deep-tree key-list)
	(if (null? (cdr key-list))
		(make-tree (make-record (car key-list) v)
				   nil nil)
		(make-tree (make-record (car key-list)
								(make-deep-tree (cdr key-list)))
				   nil nil)))
  (cond ((< (car key-list) (key (record tree)))
		 (if (null? (left tree))
			 (set-left! tree (make-deep-tree key-list))
			 (adjoin-tree! (left tree) key-list v)))
		((> (car key-list) (key (record tree)))
		 (if (null? (right tree))
			 (set-right! tree (make-deep-tree key-list))
			 (adjoin-tree! (right tree) key-list v)))
		(else
		 (error "adjoin-tree! -- tree key value" tree key-list v))))

(define (make-table-tree)
  (make-tree (make-record -inf.0 nil) nil nil))


;;; test

;; racket@> (define tbl (make-table))
;; racket@> (insert-table! tbl (list 1 2) "one two")
;; racket@> (insert-table! tbl (list 1 3) "one three")
;; racket@> (insert-table! tbl (list 2 3) "two three")
;; racket@> (insert-table! tbl (list 1 4) "one four")
;; racket@> (insert-table! tbl (list 2 4) "two four")
;; racket@> (print-table tbl)
;; ((-inf.0) () ((1 (2 . one two) () ((3 . one three) () ((4 . one four) () ()))) () ((2 (3 . two three) () ((4 . two four) () ())) () ())))

;; racket@> (lookup-table tbl (list 1 2))
;; "one two"
;; racket@> (lookup-table tbl (list 1 1))
;; #f
;; racket@> (lookup-table tbl (list 2 1))
;; #f
;; racket@> (lookup-table tbl (list 2 3))
;; "two three"



;;;; ex 3.27

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((memo-result (lookup-table table (list x))))
		(display (format "memo-ret = ~A ~%" memo-result))
        (or memo-result
            (let ((result (f x)))
              (insert-table! table (list x) result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; (define (fib n)
;;   (cond ((= n 0) 0)
;;         ((= n 1) 1)
;;         (else (+ (fib (- n 1))
;;                  (fib (- n 2))))))


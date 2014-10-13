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

;; record operations
(define (assoc-records records k)
  (define (assoc-tree tree)
	(cond ((null? tree)
		   false)
		  ((equal? k (key (record tree)))
		   (record tree))
		  (else (assoc-tree (next tree)))))
  (assoc-tree (value records)))

(define (adjoin-records! records key-list v)
  (define (make-deep-records key-list)
	(if (null? (cdr key-list))
		(make-record (car key-list) v)
		(make-record (car key-list)
					 (make-tree (make-deep-records (cdr key-list))
								nil))))
  (set-value! records
			  (make-tree (make-deep-records key-list)
						 (value records))))

;; table
(define (make-table)
  (let ((the-hash (make-record '*table* nil)))
	(define (lookup key-list)
	  (define (iter key-list records)
		(if (null? key-list)
			false
			(let ((record (assoc-records records (car key-list))))
			  (if record
				  (if (null? (cdr key-list))
					  (value record)
					  (iter (cdr key-list) record))
				  false))))
	  (iter key-list the-hash))
	(define (insert! key-list v)
	  (define (iter key-list records)
		(if (null? key-list)
			false
			(let ((record (assoc-records records (car key-list))))
			  (if record
				  (if (null? (cdr key-list))
					  (set-value! record v)
					  (iter (cdr key-list) record))
				  (adjoin-records! records key-list v)))))
	  (iter key-list the-hash))
	(define (print)
	  (begin
		(display the-hash (current-error-port))
		(newline (current-error-port))))

	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			((eq? m 'print-proc) print)
			(else (error "Unknown operation -- TABLE" m))))
	dispatch))


(define (lookup-table table key)
  ((table 'lookup-proc) key))
(define (insert-table! table key value)
  ((table 'insert-proc!) key value))
(define (print-table table)
  ((table 'print-proc)))


;; test
;; racket@> (define tbl (make-table))
;; racket@> (insert-table! tbl (list 'foo 'bar) 1)
;; racket@> (insert-table! tbl (list 'foo 'baz) 2)
;; racket@> (insert-table! tbl (list 'foo 'qux) 3)
;; racket@> (insert-table! tbl (list 'bar 'baz) 11)
;; racket@> (insert-table! tbl (list 'bar 'qux) 12)
;; racket@> (print-table tbl)
;; (*table* (bar (qux . 12) (baz . 11)) (foo (qux . 3) (baz . 2) (bar . 1)))

;; racket@> (lookup-table tbl (list 'foo 'baz))
;; 2
;; racket@> (lookup-table tbl (list 'foo 'foo))
;; #f
;; racket@> (lookup-table tbl (list 'bar 'foo))
;; #f
;; racket@> (lookup-table tbl (list 'bar 'baz))
;; 11



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

;; record operations
(define (assoc-records records k)
  (define (assoc-tree tree)
	(cond ((null? tree)
		   false)
		  ((equal? k (key (record tree)))
		   (record tree))
		  ((< k (key (record tree)))
		   (assoc-tree (left tree)))
		  (else
		   (assoc-tree (right tree)))))
  (assoc-tree (value records)))

(define (make-deep-tree key-list v)
  (if (null? (cdr key-list))
  	  (make-tree (make-record (car key-list) v)
				 nil nil)
	  (make-tree (make-record (car key-list)
							  (make-tree (make-deep-tree (cdr key-list) v)
										 nil nil))
				 nil nil)))

(define (adjoin-records! tree key-list v)
  (define (iter tree key-list)
	(cond ((null? tree)
		   (set! tree
				 (make-deep-tree key-list v)))
		  ((< (car key-list) (key (record (tree))))
		   (iter (left tree) (cdr key-list)))
		  (else
		   (iter (right tree) (cdr key-list)))))
  (iter tree key-list))


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


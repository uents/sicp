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

;; utility prodecures
(define (assoc-tree k tree)
  (cond ((null? tree)
		 false)
        ((equal? k (key (record tree)))
		 (record tree))
        (else (assoc-tree k (next tree)))))

(define (make-deep-record key-list v)
  (if (null? (cdr key-list))
	  (make-record (car key-list) v)
	  (make-record (car key-list)
				   (make-tree (make-deep-record (cdr key-list) v)
							  nil))))

(define (adjoin-record! records key-list v)
  (set-value! records
			  (make-tree (make-deep-record key-list v)
						 (value records))))

;; table
(define (make-table)
  (let ((the-hash (make-record '*table* nil)))
	(define (lookup key-list)
	  (define (iter key-list records)
		(if (null? key-list)
			false
			(let ((record (assoc-tree (car key-list) (value records))))
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
			(let ((record (assoc-tree (car key-list) (value records))))
			  (if record
				  (if (null? (cdr key-list))
					  (set-value! record v)
					  (iter (cdr key-list) record))
				  (adjoin-record! records key-list v)))))
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



;;;; ex 3.27

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((momoized-result (lookup-table table (list x))))
		(display (format "prev-ret = ~A ~%" momoized-result))
        (or momoized-result
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


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


;;;; ex 3.25

;; record
(define (make-record k v)
  (cons k v))
(define (key record)
  (car record))
(define (value record)
  (cdr record))
(define (key! record k)
  (set-car! record k))
(define (value! record v)
  (set-cdr! record v))

;; tree
(define (make-tree record next)
  (cons record next))
(define (record tree)
  (car tree))
(define (next tree)
  (cdr tree))
(define (record! tree record)
  (set-car! tree record))
(define (next! tree next)
  (set-cdr! tree next))

;; hash
(define (make-hash name tree)
  (cons name tree))
(define (name-hash hash)
  (car hash))
(define (tree-hash hash)
  (cdr hash))
(define (name-hash! hash name)
  (set-car! hash name))
(define (tree-hash! hash tree)
  (set-cdr! hash tree))

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
	  (make-hash (car key-list)
				 (make-tree (make-deep-record (cdr key-list) v)
							nil))))

(define (adjoin-hash! key-list v hash)
  (tree-hash! hash
			  (make-tree (make-deep-record key-list v)
						 (tree-hash hash))))

;; table
(define (make-table)
  (let ((the-hash (make-hash '*table* nil)))
	(define (lookup key-list)
	  (define (iter key-list hash)
		(if (null? key-list)
			false
			(let ((record (assoc-tree (car key-list) (tree-hash hash))))
			  (if record
				  (if (null? (cdr key-list))
					  (value record)
					  (iter (cdr key-list) record))
				  false))))
	  (iter key-list the-hash))
	(define (insert! key-list v)
	  (define (iter key-list hash)
		(if (null? key-list)
			false
			(let ((record (assoc-tree (car key-list) (tree-hash hash))))
			  (if record
				  (if (null? (cdr key-list))
					  (value! record v)
					  (iter (cdr key-list) record))
				  (adjoin-hash! key-list v hash)))))
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

; racket@> (define tbl (make-table))
; racket@> ((tbl 'insert-proc!) (list 'foo 'bar) 1)
; racket@> ((tbl 'insert-proc!) (list 'foo 'baz) 2)
; racket@> ((tbl 'insert-proc!) (list 'foo 'qux) 3)
; racket@> ((tbl 'insert-proc!) (list 'bar 'baz) 11)
; racket@> ((tbl 'insert-proc!) (list 'bar 'qux) 12)
;
; racket@> ((tbl 'print-proc))
; (*table* (bar (qux . 12) (baz . 11)) (foo (qux . 3) (baz . 2) (bar . 1)))
; 
; racket@> ((tbl 'lookup-proc) (list 'foo 'baz))
; 2
; racket@> ((tbl 'lookup-proc) (list 'bar 'foo))
; #f
; racket@> ((tbl 'lookup-proc) (list 'bar 'baz))
; 11



;;;; ex 3.26



;;;; ex 3.27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


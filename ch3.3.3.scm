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


;;; ex 3.24


;;; ex 3.25

(define (make-record key value)
  (cons key value))
(define (key-record record)
  (car record))
(define (value-record record)
  (cdr record))
(define (key-record! key record)
  (set-car! record key))
(define (value-record! value record)
  (set-cdr! record value))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (key-record (car records))) (car records))
        (else (assoc key (cdr records)))))

(define (adjoin! key-list value records)
  (define (new-record key-list value)
	(if (null? (cdr key-list))
		(make-record (car key-list) value)
		(list (car key-list)
			  (new-record (cdr key-list) value))))
  (set-cdr! records
			(cons (new-record key-list value) (cdr records))))

(define (make-table)
  (let ((the-table (list '*table*)))
	(define (lookup key-list)
	  (define (iter key-list table)
		(if (null? key-list)
			false
			(let ((record (assoc (car key-list) (cdr table))))
			  (if record
				  (if (null? (cdr key-list))
					  (value-record record)
					  (iter (cdr key-list) record))
				  false))))
	  (iter key-list the-table))
	(define (insert! key-list value)
	  (define (iter key-list table)
		(if (null? key-list)
			false
			(let ((record (assoc (car key-list) (cdr table))))
			  (if record
				  (if (null? (cdr key-list))
					  (value-record! value record)
					  (iter (cdr key-list) record))
				  (adjoin! key-list value table)))))
	  (iter key-list the-table))

	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (error "Unknown operation -- TABLE" m))))
	dispatch))

; racket@> (define tbl (make-table))
; racket@> ((tbl 'insert-proc!) (list 'foo 'bar) 1)
; racket@> ((tbl 'insert-proc!) (list 'foo 'baz) 2)
; racket@> ((tbl 'insert-proc!) (list 'foo 'qux) 3)
; racket@> ((tbl 'insert-proc!) (list 'bar 'baz) 11)
; racket@> ((tbl 'insert-proc!) (list 'bar 'qux) 12)
; 
; racket@> ((tbl 'lookup-proc) (list 'foo 'baz))
; 2
; racket@> ((tbl 'lookup-proc) (list 'bar 'foo))
; #f
; racket@> ((tbl 'lookup-proc) (list 'bar 'baz))
; 11

;;; ex 3.26


;;; ex 3.27

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

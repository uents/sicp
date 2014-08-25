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


;;; 1次元テーブル
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;;; 2次元テーブル
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
	(if subtable
		(let ((record (assoc key-2 (cdr subtable))))
		  (if (record
			   (cdr record))
			  false))
		false)))

(define (insert key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
	(if subtable
		(let ((record (assoc key-2 (cdr subtable))))
		  (if record
			  (set-cdr! record value)
			  (set-cdr! subtable
						(cons (cons key-2 value) (cdr subtable)))))
		(set-cdr! table
				  (cons (list key-1 (cons key-2 value))
						(cdr table)))))
  'ok)


;;; ローカルなテーブル
(define (make-table)
  (let ((local-table (list '*table*)))
	(define (lookup key-1 key-2)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if (record
				   (cdr record))
				  false))
			false)))
	(define (insert key-1 key-2 value)
	  (let ((subtable (assoc key-1 (cdr local-table))))
		(if subtable
			(let ((record (assoc key-2 (cdr subtable))))
			  (if record
				  (set-cdr! record value)
				  (set-cdr! subtable
							(cons (cons key-2 value) (cdr subtable)))))
			(set-cdr! local-table
					  (cons (list key-1 (cons key-2 value))
							(cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
			((eq? m 'insert-proc!) insert!)
			(else (error "Unknown operation -- TABLE" m))))
	disptach))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;;; ex 3.24


;;; ex 3.25


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

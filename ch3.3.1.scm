;;;; #lang racket
;;;;
;;;; SICP Chapter 3.3.1 Mutable List Structure
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
;;;;   (load "ch3.3.1.scm")
;;;;   ....
;;;;

(load "misc.scm")

;; ミュータブルなデータを扱う場合に必要
(require r5rs)

;; display procedure for mutable list
(define (disp msg)
  (begin
	(display msg (current-error-port))
	(newline (current-error-port))))


;;; ex 3.12

(define (my-last-pair x)
  (if (null? (cdr x))
	  x
	  (my-last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

;;; ex 3.13

(define (make-cycle x)
  (set-cdr! (my-last-pair x) x)
  x)

;;; ex 3.14

(define (mystery x)
  (define (loop x y)
	(if (null? x)
		y
		(let ((temp (cdr x)))
		  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

; racket@> v
; (mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))
; racket@> (disp v)
; (a b c d)
; 
; racket@> (define w (mystery v))
; racket@> w
; (mcons 'd (mcons 'c (mcons 'b (mcons 'a '()))))
; racket@> (disp w)
; (d c b a)
; racket@> v
; (mcons 'a '())
; racket@> (disp v)
; (a)


;;;; 共有と同一

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

; racket@> (eq? (car z1) (car z1))
; #t
; => 同じ格納場所を指しているため

; racket@> (eq? (car z2) (cdr z2))
; #f
; => (car z2) と (cdr z2) の値は同じだが、格納場所は異なるため


;;; ex 3.16

(define 3-pairs (cons 'a (cons 'b (cons 'c nil))))

(define p (cons 'b nil))
(define 4-pairs (cons 'a (cons p p)))

(define q (cons 'a nil))
(define r (cons q q))
(define 7-pairs (cons r r))

(define infinity-pairs
  (make-cycle (cons 'a (cons 'b (cons 'c nil)))))

(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))

; racket@> (count-pairs 3-pairs)
; 3
; racket@> (count-pairs 4-pairs)
; 4
; racket@> (count-pairs 7-pairs)
; 7
; racket@> (count-pairs infinity-pairs)
; => 返ってこない


;;; ex 3.17

(define (count-pairs-2 x)
  (let ((bred-crumbs nil))
	; チェック済みのペアかどうかを調べる
	; チェック済みでないペアの場合、その参照をパンくずリストに追加
	(define (checked-pair? x)
	  (define (iter crumbs)
		(if (null? crumbs)
			(begin
			  (set! bred-crumbs
					(append bred-crumbs (list x)))
			  false)
			(if (eq? x (car crumbs))
				true
				(iter (cdr crumbs)))))
	  (iter bred-crumbs))

	; ペアの数え上げ手続き。外部環境に
	; パンくずリストをおくために内部手続きとする
	(define (count-proc x)
	  (cond ((not (pair? x)) 0)
			((checked-pair? x) 0)
			(else
			 (+ (count-proc (car x))
				(count-proc (cdr x))
				1))))
	(count-proc x)))


;;; ex 3.18

(define (cycle-list? x)
  (let ((bred-crumbs nil))
	; チェック済みのペアかどうかを調べる
	; チェック済みでないペアの場合、その参照をパンくずリストに追加
	(define (checked-pair? x)
	  (define (iter crumbs)
		(if (null? crumbs)
			(begin
			  (set! bred-crumbs
					(append bred-crumbs (list x)))
			  false)
			(if (eq? x (car crumbs))
				true
				(iter (cdr crumbs)))))
	  (iter bred-crumbs))

	; チェック処理
	(define (check-proc x)
	  (cond ((not (pair? x)) false)
			((checked-pair? x) true)
			(else (check-proc (cdr x)))))
	(check-proc x)))

; racket@> (cycle-list? 3-pairs)
; #f
; racket@> (cycle-list? 4-pairs)
; #f
; racket@> (cycle-list? 7-pairs)
; #f
; racket@> (cycle-list? infinity-pairs)
; #t


;;; ex 3.19

;; ギブアップ...


;;; ex 3.20



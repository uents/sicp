

(+ 1
   (call/cc
	(lambda (cc)
	  2))
   3)
;; => 6

(define frozen false)

(+ 1
   (call/cc
	(lambda (cc)
	  (set! frozen cc)
	  2))
   3)
;; => 6

frozen
;; => #<continuation>


;;; frozen とは
;;; (lambda (x) (+ 1 x 3)) のような手続き
;;; ただし呼び出すと戻ってこない (前のコンテキストへジャンプする)

(frozen 10)
;; => 14

(+ 100 (frozen 10))
;; => 14

(+ (frozen 10) 100)
;; => 14


;;; false を放り込んでみる

;; racket@> (frozen false)
;; +: contract violation
;;   expected: number?
;;   given: #f
;;   argument position: 2nd
;;   other arguments...:
;;    1
;;    3

;;; => 引数は2番目ということが分かる


;;; 引数を2つ以上与えてみる

;; racket@> (frozen 10 20 30)
;; result arity mismatch;
;;  expected number of values not received
;;   expected: 1
;;   received: 3
;;   values...:
;;    10
;;    20
;;    30

;;; => 引数は1つしかムリ


;;; 変数の和を求める

(define a 1)
(define b 3)

(+ a
   (call/cc
	(lambda (cc)
	  (set! frozen cc)
	  2))
   b)

(frozen 10)
;; => 14

(set! a 100)
(set! b 200)

(frozen 10)
;; => 211

;;; つまりfrozenが実行する手続きは
;;; (lambda (x) (+ 1 x b)) となっている
;;;
;;; => frozen には call/cc が評価された瞬間の
;;;    スタックの状態がそのまま保存されていることがわかる


;;;; ccを呼んでみる

(set! frozen false)

(+ 1
   (call/cc
	(lambda (cc)
	  (set! frozen (cc 10))
	  2))
   3)
;; => 14

frozen
;; => #f
;;  call/ccの中でccを呼んでいるため
;;  set!は実行されていない

(set! frozen false)

(+ 1
   (call/cc
	(lambda (cc)
	  (set! frozen (lambda (x) (cc x)))
	  2))
   3)
;; => 6

(frozen 10)
;; => 14

(frozen 2)
;; => 6


(set! frozen false)
(define a 1)
(define y 3)

(+ a
   (call/cc
	(lambda (cc)
	  (set! frozen (lambda (x) (cc x)))
	  2))
   b)
;; => 6

(set! a 100)
(set! b 200)

(frozen 10)
;; => 211


;;; レキシカルな例

(define accumlator false)

(let ((x 0))
  (call/cc
   (lambda (cc)
	 (set! accumlator cc)))
  (set! x (+ x 1))
  x)
;; => 0

(accumlator)
;; => 2

(accumlator 100)
;; => 3
;; 引数があっても無視する






;;; 深さ優先探索の例

(define t1 '(a (b (d h) (c e (f i) g))))
(define t2 '(1 (2 (3 6 7) 4 5)))

(define (dft tree)
  (cond ((null? tree) 'done)
		((not (pair? tree))
		 (display (format "~A " tree)))
		(else (dft (car tree))
			  (dft (cdr tree)))))

;; racket@> (dft t1)
;; a b d h c e f i g 'done
;; racket@> (dft t2)
;; 1 2 3 6 7 4 5 'done

(define *saved* '())

(define (dft-node tree)
  (cond ((null? tree) (restart))
		((not (pair? tree)) tree)
		(else (call/cc
			   (lambda (cc)
				 (set! *saved*
					   (cons (lambda ()
							   (cc (dft-node (cdr tree))))
							 *saved*))
				 (dft-node (car tree)))))))

(define (restart)
  (if (null? *saved*)
	  'done
	  (let ((cont (car *saved*)))
		(set! *saved* (cdr *saved*))
		(cont))))

(define (dft2 tree)
  (set! *saved* '())
  (let ((node (dft-node tree)))
	(cond ((eq? node 'done) 'done)
		  (else (display (format "~A " node))
				(restart)))))
		  
;; racket@> (dft2 t1)
;; a b d h c e f i g 'done
;; racket@> (dft2 t2)
;; 1 2 3 6 7 4 5 'done
;;
;; racket@> (dft-node t1)
;; 'a
;; racket@> (restart)
;; 'b
;; racket@> (restart)
;; 'c
;; ...
;; racket@> (restart)
;; 'done


;;; ただし、この例は call-cc 使わなくても書ける
(define (dft-node2 tree)
  (cond ((null? tree) (restart))
		((not (pair? tree)) tree)
		(else (begin
				(set! *saved*
					  (cons (lambda () (dft-node2 (cdr tree)))
							*saved*))
				(dft-node2 (car tree))))))

;; racket@> (dft-node2 t1)
;; 'a
;; racket@> (restart)
;; 'b
;; racket@> (restart)
;; 'c
;; ...

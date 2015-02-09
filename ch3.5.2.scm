;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.2 Infinit streams
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
;;;;   (load "ch3.5.2.scm")
;;;;   ....
;;;;

(load "misc.scm")
(load "streams.scm")




(define ones
  (stream-cons 1 ones))

;; racket@> (time (stream-ref ones 0))
;; cpu time: 0 real time: 0 gc time: 0
;; 1
;; racket@> (time (stream-ref ones 1))
;; cpu time: 0 real time: 0 gc time: 0
;; 1
;; racket@> (time (stream-ref ones 10000))
;; cpu time: 3 real time: 3 gc time: 0
;; 1
;; racket@> (time (stream-ref ones 1000000))
;; cpu time: 211 real time: 210 gc time: 0
;; 1
;;
;; racket@> (stream->list ones)
;; => 返ってこない


;; racketのstream-mapは複数のストリームを引数に取れない
;; ex3.50で作成したhigh-stream-mapを使う
(define integers
  (stream-cons 1 (high-stream-map + ones integers)))


;; フィボナッチ数
;; a0 = 0, a1 = 1, a2 = a1 + a0 = 1, ...,
;; a_k = a_(k-1) + a_(k-2) なので

(define fibs
  (stream-cons 0
			   (stream-cons 1 (high-stream-map +
											   fibs
											   (stream-cdr fibs)))))

;; racket@> (map (lambda (x) (stream-ref fibs x))
;; 			  (enumerate-interval 0 10))
;; '(0 1 1 2 3 5 8 13 21 34 55)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

;; prime? を使うためにロード
(require math/number-theory)

(define primes
  (stream-cons
   2
   (stream-filter prime? (integers-starting-from 3))))


;;; ex 3.53
(define s (stream-cons 1 (add-stream s s)))


;;; ex 3.54

(define (mul-stream s1 s2)
  (stream-cons 

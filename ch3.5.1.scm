;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.1 Streams
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
;;;;   (load "ch3.5.1.scm")
;;;;   ....
;;;;

(load "misc.scm")
(load "streams.scm")

;; for using prime?
(require math/number-theory)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

;;; テスト
;; racket@> (define s (stream-enumerate-interval 10000 1000000))
;; racket@> (stream-car s)
;; 10000
;; racket@> (stream-car (stream-cdr s))
;; 10001
;; 
;; racket@> (define primes (stream-filter prime? s))
;; racket@> (stream-car primes)
;; 10007
;; racket@> (stream-car (stream-cdr primes))
;; 10009


;;; 速度を測ってみる

;; racket@> (time (list-ref (enumerate-interval 10000 10000000) 10000))
;; cpu time: 9206 real time: 10190 gc time: 6286
;; 20000
;; racket@> (time (stream-ref (stream-enumerate-interval 10000 10000000) 10000))
;; cpu time: 6 real time: 8 gc time: 0
;; 20000


;;;; ex3.50

;;; ソースコードは streams.scmを参照

;;; テスト
;; racket@> (mono-map (lambda (n) (+ 1 n)) (list 1 2 3))
;; '(2 3 4)
;; racket@> (high-map (lambda (n) (+ 1 n)) (list 1 2 3))
;; '(2 3 4)
;; racket@> (high-map + (list 1 2 3) (list 4 5 6))
;; '(5 7 9)
;; 
;; racket@> (stream->list
;; 		  (high-stream-map +
;; 						   (list->stream (list 1 2 3))
;; 						   (list->stream (list 4 5 6))))
;; '(5 7 9)



;;;; ex3.51

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
			  (stream-enumerate-interval 0 10)))

;; racket@> (stream->list x)
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; '(0 1 2 3 4 5 6 7 8 9 10)
;;
;; racket@> (stream->list x)
;; '(0 1 2 3 4 5 6 7 8 9 10)
;; →2回目の呼び出しはメモ化された結果が変える

;; 一度クリアしてから...
;;
;; racket@> (stream-ref x 5)
;; 5
;; 5
;; racket@> (stream-ref x 7)
;; 7
;; 7
;; →余計な評価は行わない

;; racket@> (stream->list x)
;; 0
;; 1
;; 2
;; 3
;; 4
;; 6
;; 8
;; 9
;; 10
;; '(0 1 2 3 4 5 6 7 8 9 10)
;; →5,7は評価済みなのでプリントされない


;;;; ex3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum
			  (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
				 seq))

;; このとき以下の実行結果はどうなるか
;; (stream-ref y 7)
;; (display-stream z)

;; racket@> (stream-ref y 7)
;; 136
;; racket@> (display-stream z)
;; 10 15 45 55 105 120 190 210


;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5 Streams
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
;;;;   (load "ch3.5.scm")
;;;;   ....
;;;;

(load "misc.scm");

;;; prime? を使うためにロード
(require math/number-theory)

;;; stream を使うためにロード
(require racket/stream)


;;; sicp の表記に合わせて再定義
(define stream-null? stream-empty?)
;;(define cons-stream stream-cons) ; 特殊形式なのでdefineで再定義できない
(define car-stream stream-first)
(define cdr-stream stream-rest)
(define the-empty-stream empty-stream)
  

;; @@@TODO 再帰降下に対応する
(define (display-stream s)
  (stream-for-each
   (lambda (x) (display (format "~a " x))) s)
  (newline))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

;;; テスト
;; racket@> (define s (stream-enumerate-interval 10000 1000000))
;; racket@> (car-stream s)
;; 10000
;; racket@> (car-stream (cdr-stream s))
;; 10001
;; 
;; racket@> (define primes (stream-filter prime? s))
;; racket@> (car-stream primes)
;; 10007
;; racket@> (car-stream (cdr-stream primes))
;; 10009


;;;; ex3.50

(define (mono-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (mono-map proc (cdr items)))))

(define (high-map proc . argitems)
  (if (null? (car argitems))
	  nil
	  (cons
	   (apply proc (mono-map car argitems))
	   (apply high-map
			  (cons proc (mono-map cdr argitems))))))

;;; テスト
;; racket@> (mono-map (lambda (n) (+ 1 n)) (list 1 2 3))
;; '(2 3 4)
;; racket@> (high-map (lambda (n) (+ 1 n)) (list 1 2 3))
;; '(2 3 4)
;; racket@> (high-map + (list 1 2 3) (list 4 5 6))
;; '(5 7 9)


;;; high-map を応用して stream-map を作る

(define (high-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-stream
	  (stream-cons
	   (apply proc (high-map car-stream argstreams))
	   (apply high-stream-map
			  (cons proc (high-map cdr-stream argstreams))))))

(define (list->stream sequence)
  (if (null? sequence)
	  nil
	  (stream-cons (car sequence)
				   (list->stream (cdr sequence)))))

(define (add-streams x y)
  (high-stream-map + x y))

;;; テスト
;; racket@> (stream->list
;; 		  (high-stream-map +
;; 						   (list->stream (list 1 2 3))
;; 						   (list->stream (list 4 5 6))))
;; '(5 7 9)

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
;; racket@> (stream->list ones)
;; => 返ってこない


(define integers
  (stream-cons 1 (add-streams ones integers)))


;; フィボナッチ数
;; a0 = 0, a1 = 1, a2 = a1 + a0 = 1, ...,
;; a_k = a_(k-1) + a_(k-2) なので

(define fibs
  (stream-cons 0
			   (stream-cons 1 (add-streams
							   fibs
							   (stream-cdr fibs)))))

;; racket@> (map (lambda (i) (stream-ref fibs i))
;; 			  (enumerate-interval 0 10))
;; => '(0 1 1 2 3 5 8 13 21 34 55)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

;; for using prime?
(require math/number-theory)

(define primes
  (stream-cons
   2
   (stream-filter prime? (integers-starting-from 3))))


;;; ex 3.53
(define s (stream-cons 1 (add-streams s s)))

;; racket@> (map (lambda (i) (stream-ref s i))
;;			  (enumerate-interval 0 10))
;; => '(1 2 4 8 16 32 64 128 256 512 1024)


;;; ex 3.54

;; see mul-streams in streams.scm

(define factorials
  (stream-cons 1 (mul-streams integers factorials)))

;; racket@> (map (lambda (i) (stream-ref factorials i))
;; 			  (enumerate-interval 0 10))
;; '(1 1 2 6 24 120 720 5040 40320 362880 3628800)


;;; ex 3.55
(define (practical-sum s)
  (stream-cons (stream-car s)
			   (add-streams (practical-sum s) (stream-cdr s))))

;; racket@> (map (lambda (i) (stream-ref (practical-sum integers) i))
;; 			  (enumerate-interval 0 10))
;; => '(1 3 6 10 15 21 28 36 45 55 66)


;;; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S
  (stream-cons 1
			   (merge (scale-stream S 2)
					  (merge (scale-stream S 3) (scale-stream S 5)))))

;; racket@> (map (lambda (i) (stream-ref S i))
;; 			  (enumerate-interval 0 20))
;; => '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40)


;;; ex 3.57

;; expandという名前はRacketの組み込みマクロと
;; 重複するので、別の名前で実装

(define (my-expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (my-expand (remainder (* num radix) den) den radix)))

 

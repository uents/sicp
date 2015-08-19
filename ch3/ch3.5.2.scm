;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.2 Infinit streams
;;;;
;;;; Author: @uents on twitter
;;;;

(load-relative "../misc.scm")
(load-relative "streams.scm")


(define ones
  (cons-stream 1 ones))

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
  (cons-stream 1 (add-streams ones integers)))


;; フィボナッチ数
;; a0 = 0, a1 = 1, a2 = a1 + a0 = 1, ...,
;; a_k = a_(k-1) + a_(k-2) なので

(define fibs
  (cons-stream 0
			   (cons-stream 1 (add-streams
							   fibs
							   (stream-cdr fibs)))))

;; racket@> (map (lambda (i) (stream-ref fibs i))
;; 			  (enumerate-interval 0 10))
;; => '(0 1 1 2 3 5 8 13 21 34 55)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; for using prime?
(require math/number-theory)

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))


;;; ex 3.53
(define s (cons-stream 1 (add-streams s s)))

;; racket@> (map (lambda (i) (stream-ref s i))
;;			  (enumerate-interval 0 10))
;; => '(1 2 4 8 16 32 64 128 256 512 1024)


;;; ex 3.54

;; see mul-streams in streams.scm

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

;; racket@> (map (lambda (i) (stream-ref factorials i))
;; 			  (enumerate-interval 0 10))
;; '(1 1 2 6 24 120 720 5040 40320 362880 3628800)


;;; ex 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
			   (add-streams (partial-sums s) (stream-cdr s))))

;; racket@> (map (lambda (i) (stream-ref (partial-sums integers) i))
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
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S
  (cons-stream 1
			   (merge (scale-stream S 2)
					  (merge (scale-stream S 3) (scale-stream S 5)))))

;; racket@> (map (lambda (i) (stream-ref S i))
;; 			  (enumerate-interval 0 20))
;; => '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40)


;;; ex 3.58

;; expandという名前はRacketの組み込みマクロと
;; 重複するので、別の名前で実装

(define (my-expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (my-expand (remainder (* num radix) den) den radix)))


;;; ex 3.59

(define (integrate-series s)
  (define (iter s k)
	(cons-stream (/ (stream-car s) k)
				 (iter (stream-cdr s) (+ k 1))))
  (iter s 1))

;; racket@> (define i (integrate-series ones))
;; racket@> (map (lambda (x) (stream-ref i x))
;;			  (enumerate-interval 0 5))
;; => '(1 1/2 1/3 1/4 1/5 1/6)

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; racket@> (map (lambda (x) (stream-ref exp-series x))
;; 			  (enumerate-interval 0 5))
;; => '(1 1 1/2 1/6 1/24 1/120)

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


;;; ex 3.60

;;(define (mul-series s1 s2)
;;  (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
			   (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
							(mul-series (stream-cdr s1) s2))))

;; racket@> (map (lambda (i) (stream-ref (mul-series integers integers) i))
;; 			  (enumerate-interval 0 5))
;; => '(1 4 10 20 35 56)


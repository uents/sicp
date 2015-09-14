#lang racket

(define nil '())

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(provide (all-defined-out))

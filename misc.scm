
(define nil '())

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (square x) (* x x))


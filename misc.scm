(module misc racket
  (provide (all-defined-out))

  (define nil '())

  (define (atom? x)
	(and (not (null? x)) (not (pair? x))))

  (define (square x) (* x x))

  (define (average x y)
	(/ (+ x y) 2))

  (define (enumerate-interval low high)
	(if (> low high)
		nil
		(cons low (enumerate-interval (+ low 1) high))))

  )

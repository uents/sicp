
(define nil '())

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (square x) (* x x))
  
(define (debug-print msg)
  (begin
	(display msg (current-error-port))
	(newline (current-error-port))))


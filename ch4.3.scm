

(define jump false)

(call/cc (lambda (cont)
		   (set! jump cont)))

(define (require p)
  (if (not p)
	  (jump (try-again))
	  false))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (product list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (list a b)))

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
	  nil
	  (cons low (enumerate-interval (+ low 1) high))))

(define (an-integer-between low high)
  (an-element-of (enumerate-interval low high)))

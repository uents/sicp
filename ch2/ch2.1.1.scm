;;;; #lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;: (define x (cons 1 2))
;: 
;: (car x)
;: (cdr x)

;: (define x (cons 1 2))
;: (define y (cons 3 4))
;: (define z (cons x y))
;: (car (car z))
;: (car (cdr z))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;;; ex. 2.1

(define (make-rat n d)
  (let ((sign (if (>= (* n d) 0) 1 -1)))
	(cons (* sign (abs n)) (abs d))))

;;; ex. 2.2

; print-pointはテキストに記載がある
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (make-segment (/ (+ (x-point (start-segment segment))
					  (x-point (end-segment segment))) 2)
				(/ (+ (y-point (start-segment segment))
					  (y-point (end-segment segment))) 2)))

; テスト
(define seg (make-segment (make-point 1 2) (make-point 4 6)))
(midpoint-segment seg) ; => '(5/2 . 4)


;;; ex. 2.3

(define (make-rect top-left-point bottom-right-point)
  (cons top-left-point bottom-right-point))

(define (top-left-point-rect rect) (car rect))
(define (bottom-right-point-rect rect) (cdr rect))

(define (width-rect rect)
  (abs (- (x-point (top-left-point-rect rect))
		  (x-point (bottom-right-point-rect rect)))))
(define (height-rect rect)
  (abs (- (y-point (top-left-point-rect rect))
		  (y-point (bottom-right-point-rect rect)))))
  
(define (perimeter rect)
  (+ (* 2 (width-rect rect)) (* 2 (height-rect rect))))

(define (area rect)
  (* (width-rect rect) (height-rect rect)))

; テスト
(define r (make-rect (make-point 1 2) (make-point 4 6)))
(perimeter r) ; => 14
(area r)      ; => 12


;; ex. 2.4

#|
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
|#

;; ex. 2.6

(define zero  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one   (lambda (f) (lambda (x) (f x))))
(define two   (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define inc (lambda (x) (+ x 1)))

((zero inc)  0) ; => 0
((one inc)   0) ; => 1
((two inc)   0) ; => 2
((three inc) 0) ; => 3

(define add (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))))
(define mul (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((n (m f)) x))))))

;(define (add m n)
;  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

((((add two) three) inc) 0) ; => 5
((((mul two) three) inc) 0) ; => 6


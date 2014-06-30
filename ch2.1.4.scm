#lang racket

;;;;--------------------------------------------------------
;;;; section 2.1.4
;;;; Extended Exercise : Interval Arithmetic
;;;;--------------------------------------------------------

;;; text

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-orig x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval-orig x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


;;; ex 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))

(define (lower-bound i) (min (car i) (cdr i)))

;;; ex 2.8

(define (sub-interval x y)
  (add-interval x (mul-interval (make-interval -1 -1) y)))

;; (sub-interval (make-interval 4 8) (make-interval 1 7))
;; => '(-3 . 8) 
;; これ合ってる？


;;; ex 2.9

;; 引数x,yの幅をx_w=((x_u-x_l)/2) y_w=((y_u-y_l)/2) とすると、
;; 加算の場合、w = x_w + y_w で求まる
;; 減算の場合も加算と同様
;; 乗算の場合、u = x_u * y_u, l = x_l * y_l のため
;;  w = (x_u * y_u - x_l * y_l) / 2  となり x_w, y_w の関数として成立できない
;; 除算の場合も乗算と同様


;;; ex 2.10

(define (positive-interval? i)
  (and (> (upper-bound i) 0) (> (lower-bound i) 0)))

(define (negative-interval? i)
  (and (< (upper-bound i) 0) (< (lower-bound i) 0)))

(define (zero-interval? i)
  (and (not (positive-interval? i)) (not (negative-interval? i))))

(define (div-interval x y)
  (if (zero-interval? y)
	  (error "divied by zero interval")
	  (mul-interval x 
					(make-interval (/ 1.0 (upper-bound y))
								   (/ 1.0 (lower-bound y))))))

;;; ex 2.11

(define (mul-interval x y)
  (cond ((positive-interval? x)
		 (cond ((positive-interval? y)
				(make-interval (* (lower-bound x) (lower-bound y))
							   (* (upper-bound x) (upper-bound y))))
			   ((negative-interval? y)
				(make-interval (* (upper-bound x) (lower-bound y))
							   (* (lower-bound x) (upper-bound y))))
			   ((zero-interval? y)
				(make-interval (* (upper-bound x) (lower-bound y))
							   (* (upper-bound x) (upper-bound y))))))
		((negative-interval? x)
		 (cond ((positive-interval? y)
				(make-interval (* (lower-bound x) (upper-bound y))
							   (* (upper-bound x) (lower-bound y))))
			   ((negative-interval? y)
				(make-interval (* (upper-bound x) (upper-bound y))
							   (* (lower-bound x) (lower-bound y))))
			   ((zero-interval? y)
				(make-interval (* (lower-bound x) (upper-bound y))
							   (* (lower-bound x) (lower-bound y))))))
		((zero-interval? x)
		 (cond ((positive-interval? y)
				(make-interval (* (lower-bound x) (upper-bound y))
							   (* (upper-bound x) (upper-bound y))))
			   ((negative-interval? y)
				(make-interval (* (upper-bound x) (lower-bound y))
							   (* (lower-bound x) (lower-bound y))))
			   ((zero-interval? y)
				(make-interval (min (* (lower-bound x) (upper-bound y))
									(* (upper-bound x) (lower-bound y)))
							   (max (* (lower-bound x) (lower-bound y))
									(* (upper-bound x) (upper-bound y)))))))
		))


;; test

(define (equal-interval x y)
  (and (= (lower-bound x) (lower-bound y))
	   (= (upper-bound x) (upper-bound y))))

(let ((z (make-interval -3 3))
	  (p (make-interval 2 4))
	  (n (make-interval -1 6)))
  (display "test01 => ")
  (display (equal-interval (mul-interval-orig p p) (mul-interval p p)))
  (newline)
  (display "test02 => ")
  (display (equal-interval (mul-interval-orig p n) (mul-interval p n)))
  (newline)
  (display "test03 => ")
  (display (equal-interval (mul-interval-orig p z) (mul-interval p z)))
  (newline)
  (display "test04 => ")
  (display (equal-interval (mul-interval-orig n p) (mul-interval n p)))
  (newline)
  (display "test05 => ")
  (display (equal-interval (mul-interval-orig n n) (mul-interval n n)))
  (newline)
  (display "test06 => ")
  (display (equal-interval (mul-interval-orig n z) (mul-interval n z)))
  (newline)
  (display "test07 => ")
  (display (equal-interval (mul-interval-orig z p) (mul-interval z p)))
  (newline)
  (display "test08 => ")
  (display (equal-interval (mul-interval-orig z n) (mul-interval z n)))
  (newline)
  (display "test09 => ")
  (display (equal-interval (mul-interval-orig z z) (mul-interval z z)))
  (newline))


;;; text

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))


;;; ex 2.12

(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
	(make-center-width c w)))

(define (percent i)
  (/ (* (width i) 100.0) (center i)))


;;; ex 2.13

(define (percent-mul-approx x y)
  (* (+ (/ (width x) (center x)) (/ (width y) (center y))) 100.0))

(define i1 (make-center-width 1000 1))
(define i2 (make-center-width 2000 1))

(percent (mul-interval i1 i2))
(percent-mul-approx i1 i2)
 

;;; text

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;; ex 2.14

;;; ex 2.15

;;; ex 2.16

(define (div-interval2 x y)
  (let ((l1 (/ (lower-bound x) (lower-bound y)))
		(l2 (/ (lower-bound x) (upper-bound y)))
		(u1 (/ (upper-bound x) (lower-bound y)))
		(u2 (/ (upper-bound x) (upper-bound y))))
	(make-interval (min (max l1 l2) (min u1 u2))
				   (max (max l1 l2) (min u1 u2)))))

(define (par3 r1 r2)
  (div-interval2 (mul-interval r1 r2)
				 (add-interval r1 r2)))

(define (par4 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval2 one
				   (add-interval (div-interval2 one r1)
								 (div-interval2 one r2)))))


			
			   

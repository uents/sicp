;;;; SICP Chapter 2.2.3
;;;;
;;;; Author @uents on twitter
;;;;

#lang racket

(require "../misc.scm")


;;;; text code

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (range 1 6)) ;; => 15

(accumulate * 1 (range 1 6)) ;; => 120

;;; foldl (fold-left)でも同じ結果が得られる
(foldl + 0 (range 1 6))
(foldl * 1 (range 1 6))


;;;; ex. 2.33

(define (my-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(my-map square (range 1 6))           ;; => '(1 4 9 16 25)
(my-append (range 1 4) (range 11 14)) ;; => '(1 2 3 11 12 13)
(my-length (range 11 14))             ;; => 3

;;;; ex. 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
				(+ this-coeff (* x higher-terms)))
			  0
			  coefficient-sequence))
				

;;;; ex. 2.35

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves tree)
  (accumulate +
			  0
			  (map (lambda (leave) 1)
				   (enumerate-tree tree))))

;;;; ex. 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  nil
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s) ;; => '(22 26 30)


;;;; ex. 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2) (list 3 4)) ;; => 11

(define (matrix-*-vector m v)
  (map (lambda (mcols) (dot-product mcols v)) m))

(matrix-*-vector (list (list 1 2) (list 3 4)) (list 3 4)) ;; => '(11 25)

(define (transpose m)
  (accumulate-n cons nil m))

(transpose (list (list 1 2) (list 3 4))) ;; => '((1 3) (2 4))

(define (matrix-*-matrix m n)
  (let ((ncols (transpose n)))
	(map (lambda (mcols) (matrix-*-vector ncols mcols)) m)))

(matrix-*-matrix (list (list 1 2) (list 3 4))
				 (list (list 1 2) (list 3 4))) ;; => '((7 10) (15 22))

;;;; ex. 2.38

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-left / 1 (list 1 2 3))
; => (/ (/ (/ 1 1) 2) 3)
; => 1/6

(fold-right / 1 (list 1 2 3))
; => (/ 1 (/ 2 (/ 3 1)))
; => 3/2

(fold-right list nil (list 1 2 3))
; => (list 1 (list 2 (list 3 nil)))
; => '(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
; => (list (list (list nil 1) 2) 3)
; => '(((() 1) 2) 3)


;;;; ex. 2.39

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (cons x nil))) nil sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))



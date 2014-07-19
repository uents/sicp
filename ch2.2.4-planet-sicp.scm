;;;; #lang racket
;;;;
;;;; SICP Chapter 2.2.4
;;;;  A Picture Language (using planet/sicp.ss)
;;;;
;;;; Author: @uents on twitter
;;;;
;;;; Usage:
;;;;
;;;; 0. Setup Geiser on Emacs
;;;;     M-x package-install geiser
;;;;
;;;; 1. Checkout my codes
;;;;     git clone https://github.com/uents/sicp.git
;;;;
;;;; 2. Start Emacs and Racket REPL (M-x run-racket)
;;;;
;;;; 3. Executes below commands on Racket REPL
;;;;
;;;;    (load "ch2.2.4.scm")
;;;;    ...
;;;;



(load "misc.scm")

;;; @NOTE: this package includes some text codes
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


;;; text code
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))



;;; ex. 2.44

;; ex. 2.45でさらに汎用化するため省略


;;; ex. 2.45

(define (split op1 op2)
  (define (proc painter n)
	(if (= n 0)
		painter
		(let ((smaller (proc painter (- n 1))))
		  (op1 painter (op2 smaller smaller)))))
  proc)

(define right-split (split beside below))
(define up-split (split below beside))

;;; ex. 2.46-48

;; planet/sicp.ssでは実装済み


;;; ex. 2.49

;; a.

(define outline
  (let* ((v0 (make-vect 0.0 0.0))
		 (v1 (make-vect 1.0 0.0))
		 (v2 (make-vect 0.0 1.0))
		 (v3 (make-vect 1.0 1.0)))
	(segments->painter (list (make-segment v0 v1)
							  (make-segment v1 v3)
							  (make-segment v3 v2)
							  (make-segment v2 v0)))))

;; b.

(define diagonal
  (let* ((v0 (make-vect 0.0 0.0))
		 (v1 (make-vect 1.0 0.0))
		 (v2 (make-vect 0.0 1.0))
		 (v3 (make-vect 1.0 1.0)))
	(segments->painter (list (make-segment v0 v3)
							 (make-segment v1 v2)))))

;; c.

(define diamond
  (let* ((m1 (make-vect 0.5 0.0))
		 (m2 (make-vect 0.0 0.5))
		 (m3 (make-vect 1.0 0.5))
		 (m4 (make-vect 0.5 1.0)))
	(segments->painter (list (make-segment m1 m3)
							 (make-segment m3 m4)
							 (make-segment m4 m2)
							 (make-segment m2 m1)))))

;; d.

(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))


;;; ex. 2.50-51

;; planet/sicp.ssでは実装済み


;;;; #lang racket
;;;;
;;;; SICP Chapter 2.5 Systems with Generic Operations
;;;;
;;;; Author: @uents on twitter
;;;;
;;;; Usage:
;;;;
;;;; 0. Setup Geiser on Emacs
;;;;     M-x package-install geiser
;;;;
;;;; 1. Download source codes
;;;;     git clone https://github.com/uents/sicp.git
;;;;
;;;; 2. Start Emacs and Racket REPL (M-x run-racket)
;;;;
;;;; 3. Executes below commands on Racket REPL
;;;;
;;;;   (load "ch2.5.1.scm")
;;;;   ....
;;;;


(load "misc.scm")

;;;; ---------------------------
;;;; operation/type table (using hash tables)
;;;;  http://docs.racket-lang.org/guide/hash-tables.html
;;;;  http://docs.racket-lang.org/reference/hashtables.html 
;;;; ---------------------------

(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
	  (hash-set! *op-table* op (make-hash))
	  true)
  (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
  (define (not-found . msg)
	(display msg (current-error-port))
	(display "\n")
	false)
  (if (hash-has-key? *op-table* op)
	  (if (hash-has-key? (hash-ref *op-table* op) type)
		  (hash-ref (hash-ref *op-table* op) type)
		  (not-found "Bad key -- TYPE" type))
	  (not-found "Bad key -- OPERATION" op)))


;;;; ---------------------------
;;;; type-tag system
;;;; ---------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


;;;; ---------------------------
;;;; rectangular and polar package
;;;; ---------------------------

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  '(installed rectangular package))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  '(installed poloar package))

;; constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; install this packages
(install-rectangular-package)
(install-polar-package)


;;;; ---------------------------
;;;; scheme number package
;;;; ---------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  ;; ex 2.79
  (put 'equ? '(scheme-number scheme-number)
	   (lambda (x y) (= x y)))

  ;; ex 2.80
  (put '=zero? '(scheme-number)
	   (lambda (x) (= x 0)))

  '(installed scheme number package))

;; constructor
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; install this package
(install-scheme-number-package)


;;;; ---------------------------
;;;; rational package
;;;; ---------------------------

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ;; ex 2.79
  (put 'equ? '(rational rational)
	   (lambda (x y) (= (* (numer x) (denom y))
						(* (numer y) (denom x)))))
  ;; ex 2.80
  (put '=zero? '(rational)
	   (lambda (x) (= (numer x) 0)))

  '(installed rational package))

;; constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; install this pakcage
(install-rational-package)


;;;; ---------------------------
;;;; complex package
;;;; ---------------------------

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; ex 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ;; ex 2.79
  (put 'equ? '(complex complex)
	   (lambda (x y) (and (= (magnitude x) (magnitude y))
						  (= (angle x) (angle y)))))

  ;; ex 2.80
  (put '=zero? '(complex)
	   (lambda (x) (= (magnitude x) 0)))

  '(installed complex package))

;; constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; install this package
(install-complex-package)


;;;; ---------------------------
;;;  exercise
;;;; ---------------------------

;;;; ex 2.77

;; before
;
; racket@> (define z (cons 'complex (cons 'rectangular (cons 3 4))))
; racket@> (magnitude z)
; No method for these types -- APPLY-GENERIC (magnitude ((complex)))
;   context...:
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7

;; added to complex package
;
; (put 'real-part '(complex) real-part)
; (put 'imag-part '(complex) imag-part)
; (put 'magnitude '(complex) magnitude)
; (put 'angle '(complex) angle)

;; after
;
; racket@> (magnitude z)
; >(apply-generic 'magnitude '(complex rectangular 3 . 4))
; >(apply-generic 'magnitude '(rectangular 3 . 4))
; <5
; 5


;;;; ex 2.78

;; before
;
; racket@> (make-scheme-number 3)
; '(scheme-number . 3)
; 
; racket@> (add (make-scheme-number 3) (make-scheme-number 4))
; '(scheme-number . 7)
;
; racket@> (add 3 4)
; Bad tagged datum -- TYPE-TAG 3
;   context...:
;    /Users/uents/work/sicp/ch2.5.scm:61:0: apply-generic
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7

(define (attach-tag type-tag contents)
  (if (number? contents)
	  contents
	  (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
		((pair? datum) (car datum))
		(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
		((pair? datum) (cdr datum))
		(else (error "Bad tagged datum -- CONTENTS" datum))))

;; after
;
; racket@> (make-scheme-number 3)
; 3
; 
; racket@> (add (make-scheme-number 3) (make-scheme-number 4))
; 7
; 
; racket@> (add 3 4)
; 7


;;;; ex 2.79

;; add to scheme-number package
;
; (put 'equ? '(scheme-number scheme-number)
; 	 (lambda (x y) (= x y)))

;; add to rational package
;
; (put 'equ? '(rational rational)
; 	 (lambda (x y) (= (* (numer x) (denom y))
;  					  (* (numer y) (denom x)))))

;; add to complex package
;
; (put 'equ? '(complex complex)
; 	 (lambda (x y) (and (= (magnitude x) (magnitude y))
; 						(= (angle x) (angle y)))))

(define (equ? x y) (apply-generic 'equ? x y))


; racket@> (equ? 3 3)
; #t
;
; racket@> (equ? (make-rational 2 3) (make-rational 6 9))
; #t
; 		 
; racket@> (equ? (make-complex-from-real-imag 0 1)
;			     (make-complex-from-mag-ang 1 (/ pi 2)))
; #t


;;;; ex 2.80

;; add to scheme number pakcage
; 
; (put '=zero? '(scheme-number)
; 	 (lambda (x) (= x 0)))

;; add to rational package
; 
; (put '=zero? '(rational)
; 	 (lambda (x) (= (numer x) 0)))

;; add to complex package
; 
; (put '=zero? '(complex)
; 	 (lambda (x) (= (magnitude x) 0)))

(define (=zero? x) (apply-generic '=zero? x))

; racket@> (=zero? 0)
; #t
; 
; racket@> (=zero? (make-rational 0 3))
; #t
; 
; racket@> (=zero? (make-complex-from-real-imag 0 0))
; #t
; 
; racket@> (=zero? (make-complex-from-mag-ang 0 pi))
; #t

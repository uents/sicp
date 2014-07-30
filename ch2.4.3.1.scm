;;;; #lang racket
;;;;
;;;; SICP Chapter 2.4.3 Data-Directed Programming and Additivity
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
;;;;   (load "ch2.4.3.1.scm")
;;;;   ....
;;;;


(load "misc.scm")

;;;; arithmetic operation for complex numbers

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


;;;; tagged data

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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;;;; operation/type table (using hash tables)
;;;;  http://docs.racket-lang.org/guide/hash-tables.html
;;;;  http://docs.racket-lang.org/reference/hashtables.html 

(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
	  (hash-set! *op-table* op (make-hash))
	  false)
  (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
  (if (hash-has-key? *op-table* op)
	  (if (hash-has-key? (hash-ref *op-table* op) type)
		  (hash-ref (hash-ref *op-table* op) type)
		  false)
	  false))


;;;; Complex-arithmetic package installers

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
  'done)

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
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
		  ; argsはリストで渡されるので
		  ; contents手続きをmapして、procをapplyで適用する
		  (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


;;;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;;;; Install packages

(install-rectangular-package)
(install-polar-package)


;;;; Test

; 例えば (real-part (make-from-real-imag 4 3)) は、
; 置き換えモデルを使うと、
; 
; => (apply (get 'real-part '(rectangular))
; 	      (map contents (list (make-from-real-imag 4 3))))
; => (apply (get 'real-part '(rectangular))
; 	      '((mcons 4 3)))
; => (mcar (mcons 4 3))
; => 4
; 
; となる



;;;; ex 2.73

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;;;; a.
;;;; 演算手続きはop-tableから取得できるが、
;;;; number? variable? は type に依存しないため、op-tableには吸収できないから


;;;; b.

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (install-deriv-package)
  (define (=number? exp num)
	(and (number? exp) (= exp num)))

  ;; add
  (define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2)) (+ a1 a2))
		  (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) 
	(if (= (length (cdr s)) 1)
		(cadr s)
		(cons '+ (cdr s))))

  ;; product
  (define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
		  ((=number? m1 1) m2)
		  ((=number? m2 1) m1)
		  ((and (number? m1) (number? m2)) (* m1 m2))
		  (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p)
	(if (= (length (cdr p)) 1)
		(cadr p)
		(cons '* (cdr p))))

  ;; interface
  (put 'deriv '+ 
	   (lambda (exp var)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var))))
  (put 'deriv '* 
	   (lambda (exp var)
		 (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp)))))
  'done)

; racket@> (install-deriv-package)
; 'done
; racket@> (deriv '(+ x y z) 'x)
; 1
; racket@> (deriv '(+ x y z) 'y)
; 1
; racket@> (deriv '(+ x y z) 'z)
; 1
; racket@> (deriv '(+ x y z) 'w)
; 0


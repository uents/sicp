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
;;;;   (load "ch2.5.2.scm")
;;;;   ....
;;;;


(load "misc.scm")

;;;; ---------------------------
;;;; procedure table (using hash tables)
;;;;  http://docs.racket-lang.org/guide/hash-tables.html
;;;;  http://docs.racket-lang.org/reference/hashtables.html 
;;;; ---------------------------

;;; 演算テーブルと型変換テーブルを別々で作成する

(define (put-table table key1 key2 item)
  (if (not (hash-has-key? table key1))
	  (hash-set! table key1 (make-hash))
	  true)
  (hash-set! (hash-ref table key1) key2 item))

(define (get-table table key1 key2)
  (define (not-found . msg)
;	(display msg (current-error-port))
;	(display "\n")
	false)
  (if (hash-has-key? table key1)
	  (if (hash-has-key? (hash-ref table key1) key2)
		  (hash-ref (hash-ref table key1) key2)
		  (not-found "Bad key -- KEY2" key2))
	  (not-found "Bad key -- KEY1" key1)))

(define *op-table* (make-hash))
(define (put op type item)
  (put-table *op-table* op type item))
(define (get op type)
  (get-table *op-table* op type))

(define *coercion-table* (make-hash))
(define (put-coercion type1 type2 item)
  (put-table *coercion-table* type1 type2 item))
(define (get-coercion type1 type2)
  (get-table *coercion-table* type1 type2))


;;;; ---------------------------
;;;; type-tag system
;;;; ---------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; modified on ex 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
		((pair? datum) (car datum))
		(else (error "Bad tagged datum -- TYPE-TAG" datum))))

;; modified on ex 2.78
(define (contents datum)
  (cond ((number? datum) datum)
		((pair? datum) (cdr datum))
		(else (error "Bad tagged datum -- CONTENTS" datum))))


;;;; -------------------------------------
;;;; generic operators
;;;; -------------------------------------

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude-part z) (apply-generic 'magnitude-part z))
(define (angle-part z) (apply-generic 'angle-part z))


;;;; ---------------------------
;;;; rectangular and polar package
;;;; ---------------------------

(define (install-rectangular-package)
  ;; internal
  (define (real-part z)
	(car z))
  (define (imag-part z)
	(cdr z))
  (define (magnitude-part z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle-part z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
	(cons x y))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude-part '(rectangular) magnitude-part)
  (put 'angle-part '(rectangular) angle-part)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

(define (install-polar-package)
  ;; internal
  (define (magnitude-part z) (car z))
  (define (angle-part z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude-part z) (cos (angle-part z))))
  (define (imag-part z)
    (* (magnitude-part z) (sin (angle-part z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude-part '(polar) magnitude-part)
  (put 'angle-part '(polar) angle-part)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(install-rectangular-package)
(install-polar-package)


;;;; ---------------------------
;;;; complex package
;;;; ---------------------------

(define (install-complex-package)
  ;; internal
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude-part z1) (magnitude-part z2))
                       (+ (angle-part z1) (angle-part z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude-part z1) (magnitude-part z2))
                       (- (angle-part z1) (angle-part z2))))

  ;; interface
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
  (put 'magnitude-part '(complex) magnitude-part)
  (put 'angle-part '(complex) angle-part)
  ;; ex 2.79
  (put 'equ? '(complex complex)
	   (lambda (x y) (and (= (magnitude-part x) (magnitude-part y))
						  (= (angle-part x) (angle-part y)))))
  ;; ex 2.80
  (put '=zero? '(complex)
	   (lambda (x) (= (magnitude-part x) 0)))

  'done)

;; constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)


;;;; ---------------------------
;;;; scheme number package
;;;; (real package)
;;;; ---------------------------

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
       (lambda (x)
		 (tag (if (eq? 'integer (type-tag x))
				  (contents x)
				  x))))

  ;; ex 2.79
  (put 'equ? '(scheme-number scheme-number)
	   (lambda (x y) (= x y)))
  ;; ex 2.80
  (put '=zero? '(scheme-number)
	   (lambda (x) (= x 0)))
  ;; ex 2.81
  (put 'exp '(scheme-number scheme-number)
	   (lambda (x y) (tag (expt x y))))
  ;; ex 2.83
  (put 'raise '(scheme-number)
	   (lambda (z) (make-complex-from-real-imag z 0)))

  'done)

;; constructor
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)


;;;; ---------------------------
;;;; rational package
;;;; ---------------------------

(define (install-rational-package)
  ;; internal
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

  ;; interface
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
  ;; ex 2.83
  (put 'raise '(rational)
	   (lambda (z) (make-scheme-number (/ (numer z) (denom z)))))

  'done)

;; constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)


;;;; ---------------------------
;;;; integer package
;;;; ---------------------------

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag (floor x))))

  ;; ex 2.79
  (put 'equ? '(integer integer)
	   (lambda (x y) (= x y)))
  ;; ex 2.80
  (put '=zero? '(integer)
	   (lambda (x) (= x 0)))
  ;; ex 2.83
  (put 'raise '(integer)
	   (lambda (z) (make-rational z 1)))

  'done)

;; constructor
(define (make-integer n)
  ((get 'make 'integer) n))

(install-integer-package)



;;;; -------------------------------------
;;;; exercise
;;;; -------------------------------------

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;;;; ex 2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
			  scheme-number->scheme-number)
(put-coercion 'complex 'complex
			  complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;; added to scheme number package
;
; (put 'exp '(scheme-number scheme-number)
; 	 (lambda (x y) (tag (expt x y))))


;;; a.

; racket@> (exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 4 3))
; (Bad key -- KEY2 (complex complex))
; (Bad key -- KEY2 (complex complex))
; (Bad key -- KEY2 (complex complex))
; (Bad key -- KEY2 (complex complex))
; (Bad key -- KEY2 (complex complex))
; ....

; 1. apply-genericの中で (get 'exp '(complex complex)) が実行され #f が返る
; 2. 1の返値はprocに格納されるため、procが#fとなる
;    よって、applyの実行ではなく型変換の方の処理に入る
; 3. type1 type2とも'complexのため、t1->t2に complex->complex が格納される
; 4. (apply-generic op (t1->t2 a1) a2) => (apply-generic op a1 a2)
;    となり、同じ条件で再度 apply-genericが実行される
; 5. 1に戻る


;;; b.

; Louisは間違っている。a.のように無限ループとなり正しく動作しない


;;; c.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
				(if (eq? type1 type2)                     ;; 
					(error "No method for these types"    ;; added for ex 2.83 c
						   (list op type-tags))           ;;
					(let ((t1->t2 (get-coercion type1 type2))
						  (t2->t1 (get-coercion type2 type1)))
					  (cond (t1->t2
							 (apply-generic op (t1->t2 a1) a2))
							(t2->t1
							 (apply-generic op a1 (t2->t1 a2)))
							(else
							 (error "No method for these types"
									(list op type-tags)))))))
			  (error "No method for these types"
					 (list op type-tags)))))))

; racket@> (exp (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 4 3))
; (Bad key -- KEY2 (complex complex))
; No method for these types (exp (complex complex))
;   context...:
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7


;;;; ex 2.83

;; add to integer package
;
;(put 'raise '(integer)
;	 (lambda (z) (make-rational z 1)))

;; add to rational package
;
;(put 'raise '(rational)
;	 (lambda (z) (make-scheme-number (/ (numer z) (denom z)))))

;; add to scheme number package
;
;(put 'raise '(scheme-number)
;	 (lambda (z) (make-complex-from-real-imag z 0)))


(define (raise z) (apply-generic 'raise z))


; racket@> (raise (make-integer 3))
; '(rational 3 . 1)
; 
; racket@> (raise (raise (make-integer 3)))
; '(scheme-number . 3)
; 
; racket@> (raise (raise (raise (make-integer 3))))
; '(complex rectangular 3 . 0)
;
; racket@> (raise (raise (raise (raise (make-integer 3)))))
; (Bad key -- KEY2 (complex))
; No method for these types (raise (complex))
;   context...:
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7


;;;; ex 2.84

(define type-tower '(complex scheme-number rational integer))

(define (higher-type? type1 type2)
  (define (iter tower)
	(if (null? tower)
		false
		(cond ((eq? type1 (car tower)) type1)
			  ((eq? type2 (car tower)) type2)
			  (else (iter (cdr tower))))))
  (iter type-tower))


; racket@> (higher-type? (type-tag 5)
; 					   (type-tag (make-complex-from-real-imag 4 3)))
; 'complex
; 
; racket@> (higher-type? (type-tag 5) (type-tag (make-integer 3)))
; 'scheme-number


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
				(if (eq? type1 type2)                   ;; 
					(error "No method for these types"  ;; added for ex 2.83 c
						   (list op type-tags))         ;;
					(let ((t1->t2 (get-coercion type1 type2))
						  (t2->t1 (get-coercion type2 type1)))
					  (cond ((eq? type1 (higher-type? type1 type2)) ;;
							 (apply-generic op a1 (t2->t1 a2)))     ;; modified for ex 2.84
							((eq? type2 (higher-type? type1 type2)) ;;
							 (apply-generic op (t1->t2 a1) a2))     ;;
							(else
							 (error "No method for these types"
									(list op type-tags)))))))
			  (error "No method for these types"
					 (list op type-tags)))))))


(put-coercion 'integer 'rational (lambda (n) (raise n)))
(put-coercion 'integer 'scheme-number (lambda (n) (raise (raise n))))
(put-coercion 'integer 'complex (lambda (n) (raise (raise (raise n)))))

(put-coercion 'rational 'scheme-number (lambda (n) (raise n)))
(put-coercion 'rational 'complex (lambda (n) (raise (raise n))))

(put-coercion 'scheme-number 'complex (lambda (n) (raise n)))


; racket@> (add (make-complex-from-real-imag 1 0)
; 			  (add (make-scheme-number 2)
; 				   (add (make-rational 3 1) (make-integer 4))))
; '(complex rectangular 10 . 0)
;
; racket@> (add (make-integer 1)
; 			  (add (make-rational 2 1)
; 				   (add (make-scheme-number 3) (make-complex-from-real-imag 4 0))))
; '(complex rectangular 10 . 0)


;;;; ex 2.85


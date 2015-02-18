;;;; #lang racket
;;;;
;;;; SICP Chapter 3.5.5
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
;;;;   (load "ch3.5.5.scm")
;;;;   ....
;;;;

(load "misc.scm")
(load "streams.scm")


(define random-init 7)

(define (rand-update x)
  (modulo (+ (* 13 x) 47) 97))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (stream-cons random-init
               (stream-map rand-update random-numbers)))

;; racket@> (map (lambda (i) (stream-ref random-numbers i))
;; 			  (enumerate-interval 0 20))
;; => '(7 41 95 21 29 36 30 49 5 15 48 89 40 82 46 63 90 53 57 12 9)
;;
;; random-initは定数なので何度やっても結果は同じになる


(define (map-successive-pairs f s)
  (stream-cons
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

;; racket@> (map (lambda (i) (stream-ref cesaro-stream i))
;; 			  (enumerate-interval 0 20))
;; => '(#t #t #t #t #f #t #f #t #t #f #t #f #f #f #t #f #f #t #t #t #t)


(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
   (stream-map (lambda (p) (sqrt (/ 6 p)))
               (monte-carlo cesaro-stream 0 0)))

;; racket@> (stream-ref pi 0)
;; => 2.449489742783178
;; racket@> (stream-ref pi 100)
;; => 3.1780497164141406
;; racket@> (stream-ref pi 1000)
;; => 3.2041639575194445
;; racket@> (stream-ref pi 10000)
;; => 3.2073868966203145
;; racket@> (stream-ref pi 100000)
;; => 3.2071601019111857
;; racket@> (stream-ref pi 1000000)
;; => 3.207137422841252


;;; ex 3.6
(define rand-ex
  (let ((x random-init))
	(define (generate)
	  (set! x (rand-update x))
	  x)
	(define (reset)
	  (set! x random-init)
	  x)
	(define (dispatch m)
	  (cond ((eq? m 'generate) generate)
			((eq? m 'reset) reset)
			(else (error "Unknown request -- RAND" m))))
	dispatch))

;; racket@> ((rand-ex 'reset))
;; => 7
;; racket@> ((rand-ex 'generate))
;; => 41
;; racket@> ((rand-ex 'generate))
;; => 95
;; racket@> ((rand-ex 'generate))
;; => 21
;; racket@> ((rand-ex 'reset))
;; => 7
;; racket@> ((rand-ex 'generate))
;; => 41
;; racket@> ((rand-ex 'generate))
;; => 95
;; racket@> ((rand-ex 'generate))
;; => 21


;;; ex 3.81
(define (rand-stream s)
  (let ((item (stream-car s)))
	(stream-cons
	 (if (eq? item 'generate)
		 ((rand-ex 'generate))
		 (begin (set! random-init item)
				((rand-ex 'reset))))
	 (rand-stream (stream-cdr s)))))

(define s
  (rand-stream
   (list->stream (list 100 'generate 'generate 'generate
					   100 'generate 'generate 'generate))))

;; racket@> (map (lambda (i) (stream-ref s i))
;; 			  (enumerate-interval 0 7))
;; => '(100 86 1 60 100 86 1 60)


;;; ex 3.82
(define (random-in-range low high)
  (let ((seed (+ (- high low) 1)))
	(+ low (random seed))))


(define (random-in-range-stream low high)
  (define ones (stream-cons 1 ones))
  (stream-map
   (lambda (i) (random-in-range low high)) ones))

;; racket@> (map (lambda (i) (stream-ref (random-in-range-stream 2 4) i))
;; 			  (enumerate-interval 0 20))
;; => '(2 3 3 4 2 3 4 3 2 4 3 3 2 4 2 2 2 4 4 4 4)


;(define (estimate-integral-ex predicate x1 y1 x2 y2)
;  (let ((x-stream (random-in-range-stream x1 x2))
;		(y-stream (random-in-range-stream y1 y2)))
;	))

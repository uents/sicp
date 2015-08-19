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

(load-relative "../misc.scm")
(load-relative "streams.scm")


;; §3.1から転用
(define (rand-update x)
  (modulo (+ (* 13 x) 47) 97))

(define random-init 7)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

;; racket@> (map (lambda (i) (stream-ref random-numbers i))
;; 			  (enumerate-interval 0 20))
;; => '(7 41 95 21 29 36 30 49 5 15 48 89 40 82 46 63 90 53 57 12 9)
;;
;; random-initは定数なので何度やっても結果は同じになる


(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

;; racket@> (map (lambda (i) (stream-ref cesaro-stream i))
;; 			  (enumerate-interval 0 20))
;; => '(#t #t #t #t #f #t #f #t #t #f #t #f #f #f #t #f #f #t #t #t #t)


(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (* (/ passed (+ passed failed) 1.0)) ;;小数にするため1.0を掛ける
     (monte-carlo-stream
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi-stream
   (stream-map (lambda (p) (sqrt (/ 6 p)))
               (monte-carlo-stream cesaro-stream 0 0)))

;; racket@> (stream-ref pi-stream 0)
;; => 2.449489742783178
;; racket@> (stream-ref pi-stream 100)
;; => 3.1780497164141406
;; racket@> (stream-ref pi-stream 1000)
;; => 3.2041639575194445
;; racket@> (stream-ref pi-stream 10000)
;; => 3.2073868966203145
;; racket@> (stream-ref pi-stream 100000)
;; => 3.2071601019111857
;; racket@> (stream-ref pi-stream 1000000)
;; => 3.207137422841252


;;; ex 3.81

;; rand-exは問題3.6を流用
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

(define (rand-stream s)
  (let ((item (stream-car s)))
	(cons-stream
	 (if (eq? item 'generate)
		 ((rand-ex 'generate))
		 (begin (set! random-init item)
				((rand-ex 'reset))))
	 (rand-stream (stream-cdr s)))))

;; racket@> (define s
;; 		   (rand-stream
;; 			(list->stream (list 100 'generate 'generate 'generate
;; 								100 'generate 'generate 'generate))))
;; racket@> (map (lambda (i) (stream-ref s i))
;; 			  (enumerate-interval 0 7))
;; => '(100 86 1 60 100 86 1 60)


;;; ex 3.82

(define (range low high x)
  (+ low (modulo x (+ 1 (- high low)))))

(define (random-in-range-stream low high)
  (stream-map (lambda (x) (range low high x))
			  random-numbers))

;; racket@> (map (lambda (i) (stream-ref (random-in-range-stream 2 8) i))
;; 			  (enumerate-interval 0 20))
;; => '(5 3 6 5 2 2 5 6 6 8 3 2 7 8 4 7 5 4 3 6 4)


(define (estimate-integral-stream predicate x1 y1 x2 y2)
  (let* ((area (* (- x2 x1) (- y2 y1)))
		 (x-stream (random-in-range-stream x1 x2))
		 (y-stream (random-in-range-stream y1 y2))
		 (passed-ratio-stream (monte-carlo-stream
							   (stream-map predicate
										   x-stream
										   y-stream)
							   0 0)))
	(scale-stream passed-ratio-stream area)))
	
;; racket@> (define s (estimate-integral-stream
;; 					(lambda (x y)
;; 					  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
;; 					2 4 8 10))
;; racket@> (stream-ref s 100)
;; => 25.306930693069305
;; racket@> (stream-ref s 1000)
;; => 25.858141858141856
;; racket@> (stream-ref s 10000)
;; => 25.867013298670134
;; racket@> (stream-ref s 100000)
;; => 25.875821241787584
;; racket@> (stream-ref s 1000000)
;; => 25.875082124917874

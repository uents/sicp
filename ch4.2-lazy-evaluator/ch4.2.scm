
(define (try a b)
  (if (= a 0) 1 b))
; => ok

(try 0 (/ 1 0))
; => 10


;;; ex 4.27

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;; > count => ???
;; > w     => ???
;; > count => ???


;;; ex 4.29

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))

;; > (square (id 10)) ;; => ???
;; > count            ;; => ???


;;; ex 4.30

;; original
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Cy's proposal
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; a.
(define (for-each proc items)
  (if (null? items)
	  'done
	  (begin (proc (car items))
			 (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
				  (list 57 321 88))

;; => 57
;; => 321
;; => 88


;; b.
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
	e
	x)
  (p (set! x (cons x '(2)))))

;; (p1 1) => ???
;; (p2 1) => ???

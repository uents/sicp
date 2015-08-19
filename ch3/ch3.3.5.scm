;;;; #lang racket
;;;;
;;;; SICP Chapter 3.3.5 XXXX
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
;;;;   (load "ch3.3.5.scm")
;;;;   ....
;;;;

(load-relative "../misc.scm")
(load-relative "mutable-lists.scm")


;;;; -----------------------------------
;;;; connector
;;;; -----------------------------------

;;; constractor
(define (make-connector)
  (let ((value false)        ; value
		(informant false)    ; object of value supplier
		(constraints '()))   ; constraints list
	;; set-value!
	(define (set-my-value newval setter)
	  (cond ((not (has-value? me))
			 (set! value newval)
			 (set! informant setter)
			 (for-each-except setter
							  inform-about-value
							  constraints))
			((not (= value newval))
			 (error "Contradiction" (list value newval)))
			(else 'ignored)))
	;; forget-value!
	(define (forget-my-value retractor)
	  (if (eq? retractor informant)
		  (begin (set! informant false)
				 (for-each-except retractor
								  inform-about-no-value
								  constraints))
		  'ignored))
	;; connect
	(define (connect new-constraint)
	  (if (not (memq new-constraint constraints))
		  (set! constraints
				(cons new-constraint constraints))
		  false)
	  (if (has-value? me)
		  (inform-about-value new-constraint)
		  false)
	  'done)

	;; dispatcher
	(define (me request)
	  (cond ((eq? request 'has-value?)
			 (if informant true false))
			((eq? request 'value)
			 value)
			((eq? request 'set-value!)
			 set-my-value)
			((eq? request 'forget)
			 forget-my-value)
			((eq? request 'connect)
			 connect)
			(else
			 (error "Unknown operation -- CONNECTOR" request))))
	me))

(define (for-each-except exception procedure lst)
  (define (loop items)
	(cond ((null? items)
		   'done)
		  ((eq? (car items) exception)
		   (loop (cdr items)))
		  (else
		   (procedure (car items))
		   (loop (cdr items)))))
  (loop lst))


;; interfaces
(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;;;; -----------------------------------
;;;; (primitive) constraints
;;;; -----------------------------------

;;; interface for connector
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;;; probe
(define (probe name connector)
  (define (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)
			
;;; adder
(define (adder a1 a2 sum)
  (define (process-new-value)
	(cond ((and (has-value? a1) (has-value? a2))
		   (set-value! sum
					   (+ (get-value a1) (get-value a2))
					   me))
		  ((and (has-value? a1) (has-value? sum))
		   (set-value! a2
					   (- (get-value sum) (get-value a1))
					   me))
		  ((and (has-value? a2) (has-value? sum))
		   (set-value! a1
					   (- (get-value sum) (get-value a2))
					   me))))
  (define (process-forget-value)
	(forget-value! sum me)
	(forget-value! a1 me)
	(forget-value! a2 me)
	(process-new-value))

  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


;;; テスト
;; racket@> (define a (make-connector))
;; (define b (make-connector))
;; (define s (make-connector))
;; (probe "a" a)
;; (probe "b" b)
;; (probe "sum" s)
;; (adder a b s)

;; racket@> (set-value! a 3 'user)
;; Probe: a = 3
;; 'done

;; racket@> (set-value! b 4 'user)
;; Probe: sum = 7
;; Probe: b = 4
;; 'done

;; racket@> (forget-value! b 'user)
;; Probe: sum = ?
;; Probe: b = ?
;; 'done

;; racket@> (set-value! s 2 'user)
;; Probe: b = -1
;; Probe: sum = 2
;; 'done

;;; constant
(define (constant value connector)
  (define (me request)
	(error "Unknown request  -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;;; multiplier
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)



;;;; -----------------------------------
;;;; excercise
;;;; -----------------------------------

;;;; ex 3.33
(define (averager a b c)
  (let ((u (make-connector))
		(v (make-connector))
		(w (make-connector)))
	(adder a b u)
	(multiplier c v u)
	(constant 2 v)
	'ok))


;;; test
;; racket@> (define a (make-connector))
;; (define b (make-connector))
;; (define c (make-connector))
;; (probe "a" a)
;; (probe "b" b)
;; (probe "c" c)
;; (averager a b c)
;; 'ok
;; racket@> (set-value! a 3 'user)
;; Probe: a = 3
;; 'done
;; racket@> (set-value! b 5 'user)
;; Probe: c = 4
;; Probe: b = 5
;; 'done
;; racket@> (forget-value! b 'user)
;; Probe: c = ?
;; Probe: b = ?
;; 'done
;; racket@> (set-value! c 1 'user)
;; Probe: b = -1
;; Probe: c = 1
;; 'done


;;;; ex 3.34
; bに値を設定しても２つのaの値が決定されるわけではないため動作しない

;;;; ex 3.35
(define (squarer a b)
  (define (process-new-value)
	(if (has-value? b)
		(if (< (get-value b) 0)
			(error "square less than 0 --- SQUARER" (get-value b))
			(set-value! a (sqrt (get-value b)) me))         ; <代替部1>
		(set-value! b (* (get-value a) (get-value a)) me))) ; <代替部2>
  (define (process-forget-value)
	(forget-value! a me)  ; <本体1>
	(forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  ; <本体2>
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- SQUARER" request))))
  (connect a me)   ; <定義の残り>
  (connect b me)
  me)

;;; test
;; racket@> (define x (make-connector))
;; (define y (make-connector))
;; (probe "x" x)
;; (probe "y" y)
;; (squarer x y)
;; racket@> (set-value! x 3 'user)
;; Probe: y = 9
;; Probe: x = 3
;; 'done
;; racket@> (forget-value! x 'user)
;; Probe: y = ?
;; Probe: x = ?
;; 'done
;; racket@> (set-value! y 3 'user)
;; Probe: x = 1.7320508075688772
;; Probe: y = 3
;; 'done


;;;; ex 3.36 
;; 略

;;;; ex 3.37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
	(multiplier x y z)
	z))

(define (c/ x y)
  (let ((z (make-connector)))
	(multiplier z y x)
	z))

(define (cv x)
  (let ((z (make-connector)))
	(constant x z)
	z))


;;; テスト
;; racket@> (define C (make-connector))
;; racket@> (define F (celsius-fahrenheit-converter C))
;; racket@> (probe "C" C)
;; #<procedure:me>
;; racket@> (probe "F" F)
;; #<procedure:me>
;; racket@> (set-value! C 30 'user)
;; Probe: C = 30
;; Probe: F = 86
;; 'done
;; racket@> (forget-value! C 'user)
;; Probe: C = ?
;; Probe: F = ?
;; 'done
;; racket@> (set-value! F 72 'user)
;; Probe: F = 72
;; Probe: C = 200/9
;; 'done


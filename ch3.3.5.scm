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

(load "misc.scm")

;;;; ミュータブルデータを使用するのに必要
(require r5rs)


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
				(cons new-constraint constraints)))
	  (if (has-value? me)
		  (inform-about-value new-constraint))
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


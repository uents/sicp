;;;; #lang racket
;;;;
;;;; SICP Chapter 3.3.2 Representing Queues
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
;;;;   (load "ch3.3.2.scm")
;;;;   ....
;;;;

(load "misc.scm")

;; ミュータブルなデータを使用するために必要
(require r5rs)


;;; FIFO
(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 


;;; ex 3.21

; racket@> (define q1 (make-queue))
; 
; racket@> (insert-queue! q1 'a)
; (mcons (mcons 'a '()) (mcons 'a '()))
; 
; racket@> (insert-queue! q1 'b)
; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))
; 
; racket@> (delete-queue! q1)
; (mcons (mcons 'b '()) (mcons 'b '()))
; 
; racket@> (delete-queue! q1)
; (mcons '() (mcons 'b '()))

(define (print-queue queue)
  (display (front-ptr queue) (current-error-port))
  (newline (current-error-port)))


;;; ex 3.22

(define (make-queue)
  (let ((front-ptr nil)
		(rear-ptr nil))
	(define (empty-queue?)
	  (null? front-ptr))
	(define (front-queue)
	  (if (empty-queue?)
		  (error "FRONT called with an empty queue")
		  (car front-ptr)))
	(define (insert-queue! item)
	  (let ((new-pair (cons item nil)))
		(if (empty-queue?)
			(begin
			  (set! front-ptr new-pair)
			  (set! rear-ptr new-pair))
			(begin
			  (set-cdr! rear-ptr new-pair)
			  (set! rear-ptr new-pair)))))
	(define (delete-queue!)
	  (if (empty-queue?)
		  (error "DELETE! called with an empty queue")
		  (set! front-ptr (cdr front-ptr))))
	(define (print-queue)
	  (display front-ptr (current-error-port))
	  (newline (current-error-port)))

	(define (dispatch m)
	  (cond ((eq? m 'empty-proc?) empty-queue?)
			((eq? m 'front-proc) front-queue)
			((eq? m 'insert-proc!) insert-queue!)
			((eq? m 'delete-proc!) delete-queue!)
			((eq? m 'print-proc) print-queue)
			(else (error "Unknown operation -- QUEUE" m))))
	dispatch))

(define (empty-queue? q)
  ((q 'empty-proc?)))
(define (front-queue q)
  ((q 'front-proc)))
(define (insert-queue! q item)
  ((q 'insert-proc!) item))
(define (delete-queue! q)
  ((q 'delete-proc!)))
(define (print-queue q)
  ((q 'print-proc)))


;; test

; racket@> (define q1 (make-queue))
; racket@> (insert-queue! q1 'a)
; racket@> (print-queue q1)
; (a)
; racket@> (insert-queue! q1 'b)
; racket@> (print-queue q1)
; (a b)
; racket@> (front-queue q1)
; 'a
; racket@> (delete-queue! q1)
; racket@> (print-queue q1)
; (b)
; racket@> (delete-queue! q1)
; racket@> (print-queue q1)
; ()
; racket@> (delete-queue! q1)
; DELETE! called with an empty queue
;   context...:
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7


;;; ex 3.23

(define (make-pair item)
  (cons item (cons nil nil)))
(define (item p)
  (car p))
(define (prev-ptr p)
  (cadr p))
(define (next-ptr p)
  (cddr p))
(define (set-prev-ptr! p q)
  (set-car! (cdr p) q))
(define (set-next-ptr! p q)
  (set-cdr! (cdr p) q))

(define (make-deque)
  (let ((front-ptr nil)
		(rear-ptr nil))
	(define (empty-deque?)
	  (null? front-ptr))
	(define (front-deque)
	  (if (empty-deque?)
		  (error "FRONT called with an empty deque")
		  (car front-ptr)))
	(define (rear-deque)
	  (if (empty-deque?)
		  (error "REAR called with an empty deque")
		  rear-ptr))
	(define (insert-deque! loc item)
	  (let ((new-pair (make-pair item)))
		(cond ((empty-deque?)
			   (set! front-ptr new-pair)
			   (set! rear-ptr new-pair))
			  ((eq? loc 'front)
			   (set-prev-ptr! front-ptr new-pair)
			   (set-next-ptr! new-pair front-ptr)
			   (set! front-ptr new-pair))
			  ((eq? loc 'rear)
			   (set-next-ptr! rear-ptr new-pair)
			   (set-prev-ptr! new-pair rear-ptr)
			   (set! rear-ptr new-pair))
			  (else
			   (error "Unknown location -- INSERT-DEQUE" loc)))))
	(define (delete-deque! loc)
	  (cond ((empty-deque?)
			 (error "DELETE! called with an empty deque"))
			((eq? front-ptr rear-ptr) ;; キューの要素が1つ
			 (set! front-ptr nil)
			 (set! rear-ptr nil))
			((eq? loc 'front)
			 (set! front-ptr (next-ptr front-ptr))
			 (set-prev-ptr! front-ptr nil))
			((eq? loc 'rear)
			 (set! rear-ptr (prev-ptr rear-ptr))
			 (set-next-ptr! rear-ptr nil))
			(else
			 (error "Unknown location -- INSERT-DEQUE" loc))))
	(define (print-deque)
	  (define (iter pair)
		(display (item pair) (current-error-port))
		(if (not (null? (next-ptr pair)))
			(begin
			  (display " " (current-error-port))
			  (iter (next-ptr pair)))))
	  (display "(" (current-error-port))
	  (if (not (empty-deque?))
		  (iter front-ptr))
	  (display ")" (current-error-port))
	  (newline (current-error-port)))

	(define (dispatch m)
	  (cond ((eq? m 'empty-proc?) empty-deque?)
			((eq? m 'front-proc) front-deque)
			((eq? m 'rear-proc) rear-deque)
			((eq? m 'front-insert-proc!)
			 (lambda (i) (insert-deque! 'front i)))
			((eq? m 'rear-insert-proc!)
			 (lambda (i) (insert-deque! 'rear i)))
			((eq? m 'front-delete-proc!)
			 (lambda () (delete-deque! 'front)))
			((eq? m 'rear-delete-proc!)
			 (lambda () (delete-deque! 'rear)))
			((eq? m 'print-proc) print-deque)
			(else (error "Unknown operation -- DEQUE" m))))
	dispatch))

(define (empty-deque? q)
  ((q 'empty-proc?)))
(define (front-deque q)
  ((q 'front-proc)))
(define (front-insert-deque! q item)
  ((q 'front-insert-proc!) item))
(define (rear-insert-deque! q item)
  ((q 'rear-insert-proc!) item))
(define (front-delete-deque! q)
  ((q 'front-delete-proc!)))
(define (rear-delete-deque! q)
  ((q 'rear-delete-proc!)))
(define (print-deque q)
  ((q 'print-proc)))

;; test

; racket@> (define q2 (make-deque))
; racket@> (front-insert-deque! q2 'x)
; racket@> (rear-insert-deque! q2 'z)
; racket@> (front-insert-deque! q2 'a)
; racket@> (print-deque q2)
; (a x z)
; racket@> (front-deque q2)
; 'a
; racket@> (rear-delete-deque! q2)
; racket@> (print-deque q2)
; (a x)
; racket@> (front-delete-deque! q2)
; racket@> (print-deque q2)
; (x)
; racket@> (front-delete-deque! q2)
; racket@> (print-deque q2)
; ()
; racket@> (front-delete-deque! q2)
; DELETE! called with an empty deque
;   context...:
;    /Applications/Racket6.0.1/collects/racket/private/misc.rkt:87:7


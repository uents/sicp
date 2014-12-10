;;;; #lang racket
;;;;
;;;; SICP Chapter 3.3.4 A Simulator for Digital Circuits
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
;;;;   (load "ch3.3.4.scm")
;;;;   ....
;;;;

(load "misc.scm")

;;;; ミュータブルデータを使用するのに必要
(require r5rs)


;;;; -----------------------------------
;;;; 回路の実装
;;;; -----------------------------------

(define (half-adder a b s c)
  (let ((d (make-wire))
		(e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))

(define (inverter input output)
  (define (invert-input)
	(let ((new-value (logical-not (get-signal input))))
	  (after-delay *inverter-delay*
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (if (= s 0)
	   1
	   0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
	(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
	  (after-delay *and-gate-delay*
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

;; 回路の作成
(define (make-wire)
  (let ((signal-value 0)
		(action-procedures '()))
	(define (call-each procedures)
	  (if (null? procedures)
		  'done
		  (begin ((car procedures))
				 (call-each (cdr procedures)))))
	(define (set-my-signal! new-value)
	  (if (not (= signal-value new-value))
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))
		  'done))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc))
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-my-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;;;; -----------------------------------
;;;; キューの実装 (from §3.3.2)
;;;; -----------------------------------

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


;;;; -----------------------------------
;;;; アジェンダの実装
;;;; -----------------------------------

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (cons 0 '()))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  ;; segmentsの終端かtimeの直前にsegmentがあればtrueを返す
  (define (belongs-before? segs)
;	(display (format "belongs-before? ~a ~%" segs))
	(or (null? segs)
		(< time (segment-time (car segs)))))
  ;; segmentを作成
  (define (make-new-time-segment)
;	(display (format "make-new-time-segments! ~%"))
	(let ((q (make-queue)))
	  (insert-queue! q action)
	  (make-time-segment time q)))
  ;; segmentを追加
  (define (add-to-segments! segs)
;	(display (format "add-to-segments! ~a ~%" segs))
	(if (= time (segment-time (car segs)))
		(insert-queue! (segment-queue (car segs))
					   action)
		(if (belongs-before? (cdr segs))
			(set-cdr! segs (cons (make-new-time-segment)
								 (cdr segs)))
			(add-to-segments! (cdr segs)))))
  (let ((segs (segments agenda)))
	(if (belongs-before? segs)
		(set-cdr! agenda (cons (make-new-time-segment)
							   segs))
		(add-to-segments! segs))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (car (segments agenda)))))
	(delete-queue! q)
	(if (empty-queue? q)
		(set-segments! agenda (cdr (segments agenda))))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
	  (error "Agenda is empty -- FIRST-AGENDA-ITEM")
	  (let ((seg (car (segments agenda))))
		(set-current-time! agenda (segment-time seg))
		(front-queue (segment-queue seg)))))


;;;; -----------------------------------
;;;; シミュレーションの実行
;;;; -----------------------------------

;; 遅延時間
(define *the-agenda* (make-agenda))
(define *inverter-delay* 2)
(define *and-gate-delay* 3)
(define *or-gate-delay* 5)

;; 手続きを遅延時間後に実行させる
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
				  action
				  *the-agenda*))

;; 回路の動作に値を出力させる
(define (probe name wire)
  (add-action! wire
			   (lambda ()
				 (display name)
				 (display " ")
				 (display (current-time *the-agenda*))
				 (display " New-value = ")
				 (display (get-signal wire))
				 (newline))))

;; アジェンダに登録されている手続きを全て実行する
(define (propagate)
  (if (empty-agenda? *the-agenda*)
	  'done
	  (begin ((first-agenda-item *the-agenda*))
			 (remove-first-agenda-item! *the-agenda*)
			 (propagate))))


;;;; -----------------------------------
;;;; 練習問題
;;;; -----------------------------------

;;; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
	  (after-delay *or-gate-delay*
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
	  1
	  0))

;; テスト
;(define in-1 (make-wire))
;(define in-2 (make-wire))
;(define out (make-wire))
;(probe 'in-1 in-1)
;(probe 'in-2 in-2)
;(probe 'out out)
;(or-gate in-1 in-2 out)
;(set-signal! in-1 1)
;(propagate)
;(set-signal! in-1 0)
;(propagate)
;(set-signal! in-2 1)
;(propagate)
;(set-signal! in-2 0)
;(propagate)

;; 結果
;racket@> (define in-1 (make-wire))
;racket@> (define in-2 (make-wire))
;racket@> (define out (make-wire))
;
;racket@> (probe 'in-1 in-1)
;in-1 0 New-value = 0
;racket@> (probe 'in-2 in-2)
;in-2 0 New-value = 0
;racket@> (probe 'out out)
;out 0 New-value = 0
;
;racket@> (or-gate in-1 in-2 out)
;racket@> (set-signal! in-1 1)
;racket@> (propagate)
;in-1 0 New-value = 1
;out 5 New-value = 1
;
;racket@> (set-signal! in-1 0)
;racket@> (propagate)
;in-1 5 New-value = 0
;out 10 New-value = 0
;
;racket@> (set-signal! in-2 1)
;racket@> (propagate)
;in-2 10 New-value = 1
;out 15 New-value = 1
;
;racket@> (set-signal! in-2 0)
;racket@> (propagate)
;in-2 15 New-value = 0
;out 20 New-value = 0


;;; ex 3.29
;;; orをnotとandで実装する
;;; 遅延は 2 * inverter-delay + and-gate-delay

(define (or-gate-ex a b output)
  (let ((c (make-wire))
		(d (make-wire))
		(e (make-wire)))
	(inverter a c)
	(inverter b d)
	(and-gate c d e)
	(inverter e output)
	'ok))

;; テスト
;; (define in-1 (make-wire))
;; (define in-2 (make-wire))
;; (define out (make-wire))
;; (probe 'in-1 in-1)
;; (probe 'in-2 in-2)
;; (probe 'out out)
;; (or-gate-ex in-1 in-2 out)
;; (set-signal! in-1 1)
;; (propagate)
;; (set-signal! in-1 0)
;; (propagate)
;; (set-signal! in-2 1)
;; (propagate)
;; (set-signal! in-2 0)
;; (propagate)

;; 結果
;racket@> (define in-1 (make-wire))
;racket@> (define in-2 (make-wire))
;racket@> (define out (make-wire))
;
;racket@> (probe 'in-1 in-1)
;in-1 0 New-value = 0
;racket@> (probe 'in-2 in-2)
;in-2 0 New-value = 0
;racket@> (probe 'out out)
;out 0 New-value = 0
;
;racket@> (or-gate in-1 in-2 out)
;racket@> (set-signal! in-1 1)
;racket@> (propagate)
;in-1 0 New-value = 1
;out 7 New-value = 1
;
;racket@> (set-signal! in-1 0)
;racket@> (propagate)
;in-1 7 New-value = 0
;out 14 New-value = 0
;
;racket@> (set-signal! in-2 1)
;racket@> (propagate)
;in-2 14 New-value = 1
;out 21 New-value = 1
;
;racket@> (set-signal! in-2 0)
;racket@> (propagate)
;in-2 21 New-value = 0
;out 28 New-value = 0


;;; ex 3.30

(define (ripple-carry-addr a-list b-list c-in sum-list)
  (let ((a-k (car a-list))
		(b-k (car b-list))
		(sum-k 0)
		(c-out 0))
	(full-adder a-k b-k c-in sum-k c-out)
	(set! sum-list (cons sum-k sum-list))
	(if (null? (cdr a-list))
		sum-list
		(ripple-carry-addr (cdr a-list) (cdr b-list) c-out sum-list))))


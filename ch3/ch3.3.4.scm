;;;; SICP Chapter 3.3.4
;;;;   A Simulator for Digital Circuits
;;;;
;;;; Author @uents on twitter
;;;;

#lang racket

;;; for mutable pairs and lists
(require r5rs)

(require "../misc.scm")


;;;; -----------------------------------
;;;; 回路の実装
;;;; -----------------------------------

;;; 回路の実装

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


;;; 回線の実装

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
;;	(define (accept-action-procedure! proc)
;;	  (set! action-procedures (cons proc action-procedures))
;;	  (proc))
	(define (accept-action-procedure! proc) ;; for ex 3.31
	  (set! action-procedures (cons proc action-procedures)))
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
;;;; シミュレーションの実装
;;;; -----------------------------------

;; 手続きを遅延時間後に実行させるようアジェンダに登録する
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
				  action
				  *the-agenda*))

;; アジェンダに登録されている手続きを全て実行させる
(define (propagate)
  (if (empty-agenda? *the-agenda*)
	  'done
	  (begin ((first-agenda-item *the-agenda*))
			 (remove-first-agenda-item! *the-agenda*)
			 (propagate))))

;; 回線の値の変更時に現在値をプリントさせる
(define (probe name wire)
  (add-action! wire
			   (lambda ()
				 (display name)
				 (display " ")
				 (display (current-time *the-agenda*))
				 (display " New-value = ")
				 (display (get-signal wire))
				 (newline))))


;;;; -----------------------------------
;;;; アジェンダの実装
;;;; -----------------------------------

;;; アジェンダ
(define (make-agenda) (cons 0 '()))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

;;; セグメント
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


;;; アジェンダの操作手続き

;; アジェンダが空かどうかチェック
(define (empty-agenda? agenda)
  (null? (segments agenda)))

;; アジェンダに手続きを追加
(define (add-to-agenda! time action agenda)
  ; segmentsの終端かtimeの直前にsegmentがあればtrueを返す
  (define (belongs-before? segs)
;	(display (format "belongs-before? ~a ~%" segs))
	(or (null? segs)
		(< time (segment-time (car segs)))))
  ; segmentを作成
  (define (make-new-time-segment)
;	(display (format "make-new-time-segments! ~%"))
	(let ((q (make-queue)))
	  (insert-queue! q action)
	  (make-time-segment time q)))
  ; segmentを追加
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

;; アジェンダの先頭セグメントを取り出す
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
	  (error "Agenda is empty -- FIRST-AGENDA-ITEM")
	  (let ((seg (car (segments agenda))))
		(set-current-time! agenda (segment-time seg))
		(front-queue (segment-queue seg)))))

;; アジェンダの先頭セグメントを削除
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (car (segments agenda)))))
	(delete-queue! q)
	(if (empty-queue? q)
		(set-segments! agenda (cdr (segments agenda)))
		false)))


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
;;;; シミュレーションの設定
;;;; -----------------------------------

;; 遅延時間の設定
(define *inverter-delay* 2)
(define *and-gate-delay* 3)
(define *or-gate-delay* 5)

;; アジェンダの作成
(define *the-agenda* (make-agenda))


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

#|
racket@> (define in-1 (make-wire))
(define in-2 (make-wire))
(define out (make-wire))

racket@> (probe 'in-1 in-1)
(probe 'in-2 in-2)
(probe 'out out)
in-1 0 New-value = 0
in-2 0 New-value = 0
out 0 New-value = 0

racket@> (or-gate in-1 in-2 out)
'ok

racket@> (set-signal! in-1 1)
'done
racket@> (propagate)
in-1 0 New-value = 1
out 5 New-value = 1

racket@> (set-signal! in-1 0)
'done
racket@> (propagate)
in-1 5 New-value = 0
out 10 New-value = 0

racket@> (set-signal! in-2 1)
'done
racket@> (propagate)
in-2 10 New-value = 1
out 15 New-value = 1

racket@> (set-signal! in-2 0)
'done
racket@> (propagate)
in-2 15 New-value = 0
out 20 New-value = 0
|#

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

#|
racket@> (define in-1 (make-wire))
(define in-2 (make-wire))
(define out (make-wire))

racket@> (probe 'in-1 in-1)
in-1 0 New-value = 0
racket@> (probe 'in-2 in-2)
in-2 0 New-value = 0
racket@> (probe 'out out)
out 0 New-value = 0

racket@> (or-gate in-1 in-2 out)
racket@> (set-signal! in-1 1)
racket@> (propagate)
in-1 0 New-value = 1
out 7 New-value = 1

racket@> (set-signal! in-1 0)
racket@> (propagate)
in-1 7 New-value = 0
out 14 New-value = 0

racket@> (set-signal! in-2 1)
racket@> (propagate)
in-2 14 New-value = 1
out 21 New-value = 1

racket@> (set-signal! in-2 0)
racket@> (propagate)
in-2 21 New-value = 0
out 28 New-value = 0
|#

;; ripple-carry-adderを実装する前にfull-adderのテスト

#|
racket@> (define a (make-wire))
'ok
racket@> (define b (make-wire))
'ok
racket@> (define c-in (make-wire))
'ok
racket@> (define sum (make-wire))
'ok
racket@> (define c-out (make-wire))
'ok

racket@> (probe 'a a)
a 0 New-value = 0
racket@> (probe 'b b)
b 0 New-value = 0
racket@> (probe 'c-in c-in)
c-in 0 New-value = 0
racket@> (probe 'sum sum)
sum 0 New-value = 0
racket@> (probe 'c-out c-out)
c-out 0 New-value = 0

racket@> (full-adder a b c-in sum c-out)
'ok

racket@> (set-signal! a 1)
a 0 New-value = 1
'done
racket@> (propagate)
sum 8 New-value = 1
'done

racket@> (set-signal! b 1)
b 8 New-value = 1
'done
racket@> (propagate)
c-out 24 New-value = 1
sum 24 New-value = 0
'done

racket@> (set-signal! c-in 1)
c-in 24 New-value = 1
'done
racket@> (propagate)
sum 40 New-value = 1
'done
|#

;;; ex 3.30
(define (ripple-carry-adder a-list b-list sum-list c-out)
  (define (iter a-list b-list sum-list c-in)
	(if (not (null? a-list))
		(let ((a-k (car a-list))
			  (b-k (car b-list))
			  (sum-k (car sum-list))
			  (c-out (make-wire)))
		  (full-adder a-k b-k c-in sum-k c-out)
		  (iter (cdr a-list) (cdr b-list) (cdr sum-list) c-out))
		'ok))
  (iter a-list b-list sum-list c-out))

;; テスト

#|
racket@> (define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))
(define a4 (make-wire))
(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))
(define b4 (make-wire))
(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))
(define s4 (make-wire))
(define a (list a1 a2 a3 a4))
(define b (list b1 b2 b3 b4))
(define s (list s1 s2 s3 s4))
(define c (make-wire))

racket@> (probe 's1 s1)
(probe 's2 s2)
(probe 's3 s3)
(probe 's4 s4)
(probe 'c c)
s1 0 New-value = 0
s2 0 New-value = 0
s3 0 New-value = 0
s4 0 New-value = 0
c 0 New-value = 0

racket@> (ripple-carry-adder a b s c)
'ok

racket@> (set-signal! a1 1)
'done
racket@> (propagate)
s1 8 New-value = 1
'done
racket@> (set-signal! b1 1)
'done
racket@> (propagate)
s1 24 New-value = 0
s2 40 New-value = 1
'done
racket@> (set-signal! a2 1)
'done
racket@> (propagate)
s2 48 New-value = 0
s3 64 New-value = 1
'done
|#

;;; ex 3.31

#|
(define in-1 (make-wire))
(define in-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder in-1 in-2 sum carry)

(set-signal! in-1 0)
(set-signal! in-2 1)
(propagate)
|#

;; 普通に実行した場合

#|
racket@> (define in-1 (make-wire))
(define in-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

racket@> (probe 'sum sum)
sum 0 New-value = 0

racket@> (probe 'carry carry)
carry 0 New-value = 0

racket@> (half-adder in-1 in-2 sum carry)
'ok
racket@> (set-signal! in-1 0)
'done
racket@> (set-signal! in-2 1)
'done

racket@> (propagate)
sum 8 New-value = 1
'done

racket@> (get-signal sum)
1
racket@> (get-signal carry)
0
|#

;; accept-action-procedure! を
;;	(define (accept-action-procedure! proc)
;;	  (set! action-procedures (cons proc action-procedures)))
;; とした場合、

#|
racket@> (define in-1 (make-wire))
(define in-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

racket@> (probe 'sum sum)
racket@> (probe 'carry carry)

racket@> (half-adder in-1 in-2 sum carry)
'ok
racket@> (set-signal! in-1 0)
'done
racket@> (set-signal! in-2 1)
'done
racket@> (propagate)
'done

racket@> (get-signal sum)
0
racket@> (get-signal carry)
0
|#

;; `sum`や`carry`の`probe`で初期値が出力されないのは、
;; `probe`での出力手続きが`accept-action-procedure!`の変更で呼ばれなくなったため、まあその通り。

;; では、`(set-sitnal! in-2 1)`で`sum`へ出力が伝播しなかった理由だが、これは`(half-adder in-1 in-2 sum carry)`で入出力を結線する際に`(and-gate d e s)`において`and-action-procedure`が実行されないため、この`and-gate`の出力`s`が変わることができずこのようなことになる。

;; よって、

;; > 論理回路 `inverter`、`and-gate`、`or-gate`はいずれも`add-action!`に渡す手続きは内部で`after-delay`を実行する。この`after-delay`は内部で`add-to-agenda!`を実行して、アジェンダにアクション手続きを登録する。

;; という処理が実行されなくなるため、`accept-action-procedure!`で与えられた手続きは即座に実行する必要がある。

;;; ex 3.32

;; 省略


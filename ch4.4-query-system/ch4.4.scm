;;;; #lang racket

;;; ex 4.55

;; a.
(supervisor ?who (Bitdiddle Ben))

;; b.
(job ?who (accounting . ?type))

;; c.
(address ?who (Slumerville . ?where))


;;; ex 4.56

;; a.
(and (supervisor ?person (Bitdiddle Ben))
	 (address ?person ?address))

;; b.
(and (salary (Bitdiddle Ben) ?ben-amount)
	 (and (salary ?person ?amount)
		  (lisp-value < ?amount ?ben-amount)))

;; c.
(and (supervisor ?staff-person ?boss)
	 (not (job ?boss (computer . ?type)))
	 (job ?boss ?job))


;;; ex 4.57

#| add rule to microshaft-data-base
(rule (replace ?person-1 ?person-2)
	  (and (job ?person-1 ?job-1)
		   (job ?person-2 ?job-2)
		   (or (same ?job-1 ?job-2)
			   (can-do-job ?job-1 ?job-2))
		   (not (same ?person-1 ?person-2))))
|#

;; a.
(replace ?who (Fect Cy D))

;; b.
(and (replace ?person-1 ?person-2)
	 (salary ?person-1 ?salary-1)
	 (salary ?person-2 ?salary-2)
	 (lisp-value > ?salary-1 ?salary-2))


;;; ex 4.58

#| add rule to microshaft-data-base
(rule (big-shot ?person)
	  (and (supervisor ?person ?boss)
		   (not (replace ?boss ?person))))
|#

(big-shot ?person)


;;; ex 4.59

#| add data to microshaft-data-base
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))
|#

;; a.
(meeting ?division (Friday ?time))

;; b.

#| add data to microshaft-data-base
(rule (meeting-time ?person ?day-and-time)
	  (or (and (job ?person (?division . ?type))
			   (meeting ?division ?day-and-time))
		  (meeting whole-company ?day-and-time)))
|#

;; c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))


;;; ex 4.69

#|
- 理由は、規則`lives-near`が重複チェックを`(not (same ?person-1 ?person-2))`でしかしていないため
- 重複チェックとしてさらに名前の比較チェックを追加すればよい。実装はパス
|#

;;; ex 4.74

#|
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))
|#

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
			  (stream-filter (lambda (s) (not (stream-null? s))) stream)))

;;; ex 4.75
(define (uniquely-query exps) (car exps))

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
   (lambda (frame)
	 (let ((result-stream (qeval (uniquely-query contents)
								 (singleton-stream frame))))
	   (if (and (not (stream-null? result-stream))
				(stream-null? (stream-cdr result-stream)))
		   result-stream
		   the-empty-stream)))
   frame-stream))


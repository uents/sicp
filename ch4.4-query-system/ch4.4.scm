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

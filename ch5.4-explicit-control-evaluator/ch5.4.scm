
;;;; test for tail-recuresive

(define (fact-iter n)
  (define (iter n val)
	(if (= n 1)
		val
		(iter (- n 1) (* n val))))
  (iter n 1))

#| test for tail-recursive version

;;; EC-Eval input:
(fact-iter 5)
'(total-pushes = 169 max-depth = 10 curr-depth = 0)

;;; EC-Eval value:
120

;;; EC-Eval input:
(fact-iter 10)
'(total-pushes = 344 max-depth = 10 curr-depth = 0)

;;; EC-Eval value:
3628800

|#


#| test for non-tail-recursive version

;;; EC-Eval input:
(fact-iter 5)
'(total-pushes = 181 max-depth = 26 curr-depth = 0)

;;; EC-Eval value:
120

;;; EC-Eval input:
(fact-iter 10)
'(total-pushes = 366 max-depth = 41 curr-depth = 0)

;;; EC-Eval value:
3628800

|#


;;;; ex 5.27

(define (factorial n)
  (define (iter product counter)
	(if (> counter n) product
		(iter (* counter product) (+ counter 1)))) (iter 1 1))

#|

| `n` | `n!`| `max-depth` | `total-pushes` |
|-----|-----|-------------|----------------|
| 1	 | 1	   | 10 | 64 |
| 2	 | 2	   | 10 | 99 |
| 3	 | 6	   | 10 | 134 |
| 5	 | 120	   | 10 | 204 |
| 10 | 3628800 | 10 | 379 |

|#


;;;; ex 5.28

(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n)))

#|

| `n` | `n!`| `max-depth` | `total-pushes` |
|-----|-----|-------------|----------------|
| 1	 | 1	   | 8	| 16  |
| 2	 | 2	   | 13 | 48  |
| 3	 | 6	   | 18 | 80  |
| 5	 | 120	   | 28 | 144 |
| 10 | 3628800 | 53 | 304 |

|#

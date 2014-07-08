;;;; #lang racket

;;;;--------------------------------------------------------
;;;; chapter 2.2.3
;;;; 
;;;;--------------------------------------------------------

(load "./misc.scm")


;;;; text code

;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))

;(define (filter predicate sequence)
;  (cond ((null? sequence) nil)
;        ((predicate (car sequence))
;         (cons (car sequence)
;               (filter predicate (cdr sequence))))
;        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))



(let ((n 6))
  (accumulate append
			  nil
			  (map (lambda (i)
					 (map (lambda (j) (list i j))
						  (enumerate-interval 1 (- i 1))))
				   (enumerate-interval 1 n))))

; => '((2 1)
;      (3 1) (3 2)
;      (4 1) (4 2) (4 3)
;      (5 1) (5 2) (5 3) (5 4)
;      (6 1) (6 2) (6 3) (6 4) (6 5))


;; 上記のコードをflatmapを使って書き直す
(let ((n 6))
  (flatmap 
   (lambda (i)
	 (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; prime?を借りるためにロード
(require math/number-theory)

(define (pair-sum pair)
  (+ (car pair) (cadr pair)))

(define (prime-sum? pair)
  (prime? (pair-sum pair)))

(define (make-pair-sum pair)
  (append pair (cons (pair-sum pair) nil)))


(define (prime-sum-pairs n)
  ;; (3) i,j,i+jのpairのリストを作成する
  (map make-pair-sum
	   ;; (2) i+jが素数のpairだけをリストに残す
	   (filter prime-sum?
			   ;; (1) ここでi,jの対のリストを作成
			   (flatmap
				(lambda (i)
				  (map (lambda (j) (list i j))
					   (enumerate-interval 1 (- i 1))))
				(enumerate-interval 1 n)))))

(prime-sum-pairs 6)
; => '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))


;;; ex. 2.40

(define (unique-pairs n)
  (flatmap
   (lambda (i)
	 (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (unique-pairs n))))


;;; ex. 2.41

(define (unique-trio n)
  (flatmap
   (lambda (k)
	 (flatmap
	  (lambda (j)
		(map (lambda (i) (list i j k))
			 (enumerate-interval 1 (- j 1))))
	  (enumerate-interval 1 (- k 1))))
   (enumerate-interval 1 n)))

(define (trio-sum trio)
  (+ (car trio) (cadr trio) (caddr trio)))

(define (equal-sum-trio n s)
  (filter (lambda (t) (= (trio-sum t) s))
		  (unique-trio n)))

;;; ex. 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
;  (display (format "n-row=~a k=~a r-queens=~a ~%" new-row k rest-of-queens)))
  (append rest-of-queens (list (list k new-row))))

;(define (safe? k positions)
;  (display (format "k=~a pos=~a ~%" k positions))
;  #t)

(define (safe? k positions)
  (safe-iter? (- k 1) k positions))

(define (safe-iter? i k positions)
  (if (= i 0)
	  #t
	  (let ((old-pos (list-ref positions (- i 1)))
			(new-pos (list-ref positions (- k 1))))
		(and (not (= (cadr old-pos) (cadr new-pos)))
			 (not (= (cadr old-pos) (- (cadr new-pos) (- k i))))
			 (not (= (cadr old-pos) (+ (cadr new-pos) (- k i))))
			 (safe-iter? (- i 1) k positions)))))


;;; ex 2.43

(define (queens-louis board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
				 (queen-cols (- k 1))))
		  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

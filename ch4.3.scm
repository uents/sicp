
;;; call/ccで実装したambオペレータ
(load "amb.scm")

;;; SICP本文でのrequireはreqという名前で定義
(define (req p)
  (if (not p) (amb) false))

;;; ambオペレータを再帰的に呼ぶだけに、引数を遅延オブジェクトとする
;;; SICP本文のamb評価器は、遅延評価するため不要
(define (an-element-of items)
  (req (not (null? items)))
  (amb (car items) (delay (an-element-of (cdr items)))))

;;; ambオペレータは特殊形式ではなく手続きの場合、
;;; applyを使って以下のようにもかける
;; (define (an-element-of items)
;;   (apply amb items))


;;;; 4.3.1 ambと検索

(require math/number-theory)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
	(req (prime? (+ a b)))
    (list a b)))

;;(prime-sum-pair '(1 3 5 8) '(20 35 110))

(define (an-integer-starting-from n)
  (amb n (delay (an-integer-starting-from (+ n 1)))))


;;; ex 4.35

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (an-integer-between low high)
  (an-element-of (enumerate-interval low high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
	(let ((j (an-integer-between i high)))
	  (let ((k (an-integer-between j high)))
		(req (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))


;;; ex 4.36

(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
	(let ((j (an-integer-starting-from 1)))
	  (let ((k (an-integer-starting-from 1)))
		(req (= (+ (* i i) (* j j)) (* k k)))
		(list i j k)))))
;; => kだけが増えていくので返ってこない

(define (a-pythagorean-triple)
  (let* ((k (an-integer-starting-from 1))
		 (j (an-integer-between 1 k))
		 (i (an-integer-between 1 j)))
	(req (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))


;;; ex 4.37

(define *backtrack-count* 0)

(define (req p)
  (if (not p)
	  (begin (set! *backtrack-count* (add1 *backtrack-count*))
			 (amb))
	  false))

(define (a-pythagorean-triple-between-ex low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (req (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (req (integer? k))
          (list i j k))))))

;; racket@> (a-pythagorean-triple-between-ex 1 20)
;; => '(3 4 5)
;; racket@> (try-again)
;; ...
;; racket@> (try-again)
;; => '(there are no more values)
;; racket@> *backtrack-count*
;; => 225


;;;; 4.3.2

;;; ex 4.39

;; - 解そのものには影響しない
;; - 解が出るまでの時間 (計算回数) には影響する
;;  => バックトラックの回数を数えればよい

;;; ex 4.40

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (req (distinct? (list baker cooper fletcher miller smith)))
    (req (not (= baker 5)))
    (req (not (= cooper 1)))
    (req (not (= fletcher 5)))
    (req (not (= fletcher 1)))
    (req (> miller cooper))
    (req (not (= (abs (- smith fletcher)) 1)))
    (req (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; racket@> (multiple-dwelling)
;; => '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; racket@> (try-again)
;; => '(there are no more values)
;; racket@> *backtrack-count*
;; => 3124

(define (multiple-dwelling-ex)
  (let ((fletcher (amb 1 2 3 4 5)))
    (req (not (= fletcher 5)))
    (req (not (= fletcher 1)))
	(let ((baker (amb 1 2 3 4 5)))
	  (req (not (= baker 5)))
	  (let ((cooper (amb 1 2 3 4 5)))
		(req (not (= cooper 1)))
		(let ((miller (amb 1 2 3 4 5)))
		  (req (> miller cooper))
		  (let ((smith (amb 1 2 3 4 5)))
			(req (not (= (abs (- smith fletcher)) 1)))
			(req (not (= (abs (- fletcher cooper)) 1)))
			(begin
			  (req (distinct? (list baker cooper fletcher miller smith)))
			  (list (list 'baker baker)
					(list 'cooper cooper)
					(list 'fletcher fletcher)
					(list 'miller miller)
					(list 'smith smith)))))))))

;; racket@> (multiple-dwelling-ex)
;; => '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; racket@> (try-again)
;; => '(there are no more values)
;; racket@> *backtrack-count*
;; => 544


;;; ex 4.42

(define (girls-standing)
  (let ((betty (amb 1 2 3 4 5))
		(ethel (amb 1 2 3 4 5))
		(joan (amb 1 2 3 4 5))
		(kitty (amb 1 2 3 4 5))
		(mary (amb 1 2 3 4 5)))
	(req (distinct? (list betty ethel joan kitty mary)))
	(req (xor (= kitty 2) (= betty 3))) ;; betty said
	(req (xor (= ethel 1) (= joan 2)))  ;; ehtel said
	(req (xor (= joan 3) (= ethel 5)))  ;; joan said
	(req (xor (= kitty 2) (= mary 4)))  ;; kitty said
	(req (xor (= mary 4) (= betty 1)))  ;; mary said
	(list (list 'betty betty)
		  (list 'ethel ethel)
		  (list 'joan joan)
		  (list 'kitty kitty)
		  (list 'mary mary))))

;; racket@> (girls-standing)
;; => '((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
;; racket@> (try-again)
;; => '(there are no more values)



;;; ex 4.43

(define (yacht-owner)
  (let (;; 「Mary Ann Mooreの父はヨットを持っており」 => 娘はMary
		;; 「MooreさんのはLorna」 => ヨットはLorna
		(moore (cons 'mary
					 'lorna))
		;; 娘は? => 残りの条件からRosalind, Gabrielle
		;; 「Downing大佐のMelissaはBarnacle卿の娘の名」 => ヨットはMelissa
		(downing (cons (amb 'lorna 'rosalind 'gabrielle)
					   'melissa))
		;; 娘は? => 残りの条件からLorna, Gabrielle
		;; 「HallさんのはRosalind」 => ヨットはGabrielle
		(hall (cons (amb 'lorna 'gabrielle)
					'rosalind))
		;; 「Downing大佐のMelissaはBarnacle卿の娘の名」 => 娘はMelissa
		;; 「Barnacle卿のヨットはGabrielle」 => ヨットはGabrielle
		(barnacle (cons 'melissa     
						'gabrielle))
		;; 「Gabrielleの父のヨットはDr.Parkerの娘から」
		;;  => ParkerはGabrielleの父ではない
		;;  => 娘はLorna, Rosalind
		;; Parker以外はヨットは決まっている
		;;  => ヨットは残りのMary
		(parker (cons (amb 'lorna 'rosalind) 'mary)))
					  
	(let ((fathers (list moore downing hall barnacle parker)))
	  ;; 娘は重複しない
	  (req (distinct? (map car fathers)))

	  ;; 残る条件は「Gabrielleの父のヨットはDr.Parkerの娘から」
	  (let ((gabrielle-father
			 (car (filter (lambda (owner) (equal? (car owner) 'gabrielle))
						  fathers))))
		(req (equal? (cdr gabrielle-father) (car parker)))
		
		(list (list 'moore moore)
			  (list 'downing downing)
			  (list 'hall hall)
			  (list 'barnacle barnacle)
			  (list 'parker parker))))))


;;; ex 4.44

(define (3-queens)
  (let ((q1 (amb 1 2 3)))
	(req (safe? 1 (list 1 q1)))
	(let ((q2 (amb 1 2 3)))
	  (req (safe? 2 (list (list 1 q1) (list 2 q2))))
	  (let ((q3 (amb 1 2 3)))
		(req (safe? 3 (list (list 1 q1)  (list 2 q2) (list 3 q3))))
		(try-again)))))
							
(define (safe? k positions)
  (display (format "k=~a pos=~a ~%" k positions))
  true)

;; racket@> (3-queens)
;; k=1 pos=(1 1) 
;; k=2 pos=((1 1) (2 1)) 
;; k=3 pos=((1 1) (2 1) (3 1)) 
;; k=3 pos=((1 1) (2 1) (3 2)) 
;; k=3 pos=((1 1) (2 1) (3 3)) 
;; k=2 pos=((1 1) (2 2)) 
;; k=3 pos=((1 1) (2 2) (3 1)) 
;; k=3 pos=((1 1) (2 2) (3 2)) 
;; k=3 pos=((1 1) (2 2) (3 3)) 
;; k=2 pos=((1 1) (2 3)) 
;; k=3 pos=((1 1) (2 3) (3 1)) 
;; k=3 pos=((1 1) (2 3) (3 2)) 
;; k=3 pos=((1 1) (2 3) (3 3)) 
;; k=1 pos=(1 2) 
;; k=2 pos=((1 2) (2 1)) 
;; k=3 pos=((1 2) (2 1) (3 1)) 
;; k=3 pos=((1 2) (2 1) (3 2)) 
;; k=3 pos=((1 2) (2 1) (3 3)) 
;; k=2 pos=((1 2) (2 2)) 
;; k=3 pos=((1 2) (2 2) (3 1)) 
;; k=3 pos=((1 2) (2 2) (3 2)) 
;; k=3 pos=((1 2) (2 2) (3 3)) 
;; k=2 pos=((1 2) (2 3)) 
;; k=3 pos=((1 2) (2 3) (3 1)) 
;; k=3 pos=((1 2) (2 3) (3 2)) 
;; k=3 pos=((1 2) (2 3) (3 3)) 
;; k=1 pos=(1 3) 
;; k=2 pos=((1 3) (2 1)) 
;; k=3 pos=((1 3) (2 1) (3 1)) 
;; k=3 pos=((1 3) (2 1) (3 2)) 
;; k=3 pos=((1 3) (2 1) (3 3)) 
;; k=2 pos=((1 3) (2 2)) 
;; k=3 pos=((1 3) (2 2) (3 1)) 
;; k=3 pos=((1 3) (2 2) (3 2)) 
;; k=3 pos=((1 3) (2 2) (3 3)) 
;; k=2 pos=((1 3) (2 3)) 
;; k=3 pos=((1 3) (2 3) (3 1)) 
;; k=3 pos=((1 3) (2 3) (3 2)) 
;; k=3 pos=((1 3) (2 3) (3 3)) 
;; '(there are no more values)



;;; 遅延オブジェクトの生成
(define (delay-it exp env)
  (list 'thunk exp env))

;;; 遅延オブジェクトの評価
;;; @note set-car!/set-cdr!を使うのでr5rsをrequireする必要がある
(define (force-it obj)
  (cond ((tagged-list? obj 'evaluated-thunk)
		 (cadr obj)) ;; its value
		((tagged-list? obj 'thunk)
		 ((let ((value (force-it (eval-proc exp env))))
			(set-car! obj 'evaluated-thunk)
			(set-car! (cdr obj) value) ;; replace expression with its value
			(set-cdr! (cdr obj) '())   ;; forget environment
			value)))
		(else obj))) ;; not delayed object


;;; RacketでSICPサンプルコードの遅延評価器を動かす
;;
;; (1) 以下からサンプルコードをダウンロードして展開
;;  https://mitpress.mit.edu/sicp/code/index.html から
;;  allcode.tar.gz をダウンロード
;;
;; (2) いつものようにemacs/geiserからrakcetを起動し以下を実行
;;  racket@> (require r5rs)
;;  racket@> (load "ch4-leval.scm")
;;  'LAZY-EVALUATOR-LOADED
;;  racket@> (define the-global-environment (setup-environment))
;;  racket@> (driver-loop)


(define (try a b)
  (if (= a 0) 1 b))
; => ok

(try 0 (/ 1 0))
; => 10


;;; ex 4.27

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;; > count => ???
;; > w     => ???
;; > count => ???


;;; ex 4.29

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))

;; > (square (id 10)) ;; => ???
;; > count            ;; => ???


;;; ex 4.30

;; Cy's proposal
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; a.
(define (for-each proc items)
  (if (null? items)
	  'done
	  (begin (proc (car items))
			 (for-each proc (cdr items)))))


(for-each (lambda (x) (newline) (display x))
				  (list 57 321 88))

;; => 57
;; => 321
;; => 88				   



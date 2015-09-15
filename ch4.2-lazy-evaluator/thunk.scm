#lang racket

;;; set-car!/set-cdr!を使うのでr5rsをrequireする必要がある
(require r5rs)

;;; 遅延オブジェクトの生成
(define (delay-it exp env)
  (list 'thunk exp env))

;;; 遅延オブジェクトの評価
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


(provide (all-defined-out))



;;; ex 4.1

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 左から右へ評価
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
	  (let ((first (eval (first-operand exps) env))
			(rest (list-of-values (rest-operands exps) env)))
		(cons first rest))))

;; 右から左へ評価
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
	  (let ((rest (list-of-values (rest-operands exps) env))
			(first (eval (first-operand exps) env)))
		(cons first rest))))

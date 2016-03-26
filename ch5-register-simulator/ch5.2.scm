
;;; example
(define gcd-machine
  (make-machine
   '(a b t) ;; register names
   (list (list 'rem remainder) ;; operations
		 (list '= =))
   '(test-b ;; controller instruction sequence
	 (test (op =) (reg b) (const 0))
	 (branch (label gcd-done))
	 (assign t (op rem) (reg a) (reg b))
	 (assign a (reg b))
	 (assign b (reg t))
	 (goto (label test-b))
	 gcd-done)))

#|
  #=> コントローラ命令列を出力させるとこんな感じ
  (list
	(mcons '(test (op =) (reg b) (const 0)) #<procedure:test-proc>)
	(mcons '(branch (label gcd-done)) #<procedure:branch-proc>)
	(mcons '(assign t (op rem) (reg a) (reg b)) #<procedure:assign-proc>)
	(mcons '(assign a (reg b)) #<procedure:assign-proc>)
	(mcons '(assign b (reg t)) #<procedure:assign-proc>)
	(mcons '(goto (label test-b)) #<procedure:goto-proc-1>))
|#

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a) #=> 2


;;; apply stack statistics to factorial machine
(define fact-machine
  (make-machine
   '(val n continue)
   (list (list '= =)
		 (list '- -)
		 (list '* *))
   '(controller
	   (perform (op initialize-stack))		 ;; add
	   (assign continue (label fact-done))
	 fact-loop
	   (test (op =) (reg n) (const 1))
	   (branch (label base-case))
	   (save continue)
	   (perform (op print-stack-statistics)) ;; add
	   (save n)
	   (perform (op print-stack-statistics)) ;; add
	   (assign n (op -) (reg n) (const 1))
	   (assign continue (label after-fact))
	   (goto (label fact-loop))
	 after-fact
	   (restore n)
	   (perform (op print-stack-statistics)) ;; add
	   (restore continue)
	   (perform (op print-stack-statistics)) ;; add
	   (assign val (op *) (reg n) (reg val))
	   (goto (reg continue))
	 base-case
	   (assign val (const 1))
	   (goto (reg continue))
	 fact-done)))

(set-register-contents! fact-machine 'n 3)
(start fact-machine)
(get-register-contents fact-machine 'val)


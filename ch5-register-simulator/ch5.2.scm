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


;;; ex 5.14
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
	   (save n)
	   (assign n (op -) (reg n) (const 1))
	   (assign continue (label after-fact))
	   (goto (label fact-loop))
	 after-fact
	   (restore n)
	   (restore continue)
	   (assign val (op *) (reg n) (reg val))
	   (goto (reg continue))
	 base-case
	   (perform (op print-stack-statistics)) ;; add
	   (assign val (const 1))
	   (goto (reg continue))
	 fact-done)))

(map (lambda (n)
	   (set-register-contents! fact-machine 'n n)
	   (start fact-machine))
	 '(1 2 3 4 5 6 7 8 9 10))

;; =>
;; '(total-pushes = 0 max-depth = 0 curr-depth = 0)
;; '(total-pushes = 2 max-depth = 2 curr-depth = 2)
;; '(total-pushes = 4 max-depth = 4 curr-depth = 4)
;; '(total-pushes = 6 max-depth = 6 curr-depth = 6)
;; '(total-pushes = 8 max-depth = 8 curr-depth = 8)
;; '(total-pushes = 10 max-depth = 10 curr-depth = 10)
;; '(total-pushes = 12 max-depth = 12 curr-depth = 12)
;; '(total-pushes = 14 max-depth = 14 curr-depth = 14)
;; '(total-pushes = 16 max-depth = 16 curr-depth = 16)
;; '(total-pushes = 18 max-depth = 18 curr-depth = 18)
;; '(done done done done done done done done done done)

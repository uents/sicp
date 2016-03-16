

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



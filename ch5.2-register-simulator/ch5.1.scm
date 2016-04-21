

;;; ex 5.2
(controller
 (assign product (const 1))
 (assign counter (const 1))
 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign product (op *) (reg product) (reg counter))
   (assign counter (op +) (reg counter) (const 1))
   (goto (label test-counter))
 factorial-done)


;;; ex 5.3
(controller
 (assign x (op read))
 (assign guess (const 1.0))
 test-good-enough?
   (test (op good-enough?) (reg guess) (reg x))
   (branch (label sqrt-done))
   (assign guess (op remove) (reg guess) (reg x))
   (goto (label test-good-enough?))
 sqrt-done)


;;; figure 5.11
(controller
   (assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n * (n-1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1!=1
   (goto (reg continue))                   ; return to caller
 fact-done)

;;; ex 5.4
;;; - 再帰プロセスは、スタックが必要
;;; - 反復プロセスは、末尾再帰ならスタック不要
(controller
 (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (goto (label expt-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign product (op *) (reg b) (reg product))
   (goto (reg continue))
 base-case
   (assign product (const 1))
   (goto (reg continue))
 expt-done)

(controller
 (assign continue (reg n))
 (assign product (const 1))
 expt-loop
   (test (op =) (reg counter) (const 0))
   (branch (label expt-done))
   (assign counter (op -) (reg counter) (const 1))
   (assign product (op *) (reg b) (reg product))
   (goto (label expt-loop))
 expt-done)

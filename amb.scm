
;; (define fail #f)

;;; initialize fail
;; (call/cc
;;  (lambda (cc)
;;    (set! fail
;;          (lambda ()
;;            (cc 'no-choise)))))

(define amb-cont
  (lambda ()
	"there are no more values"))

(define (amb . lst)
  (if (null? lst)
	  (amb-cont)
	  (let ((cont amb-cont))
		(call/cc
		 (lambda (cc)
		   (set! amb-cont
				 (lambda ()
				   (set! amb-cont cont)
				   (cc (apply amb (cdr lst))))) ;; 継続に残りの選択子を渡す
		   (cc (car lst)))))))                  ;; 継続の外に最初の選択肢を出す

;; (define-syntax amb
;;   (syntax-rules ()
;;     ((_) (amb-cont))
;;     ((_ a) a)
;;     ((_ a b ...)
;;      (let ((cont amb-cont))
;;        (call/cc
;; 		(lambda (cc)
;; 		  (set! amb-cont
;; 				(lambda ()
;; 				  (set! amb-cont cont)
;; 				  (cc (amb b ...))))
;; 		  (cc a)))))))

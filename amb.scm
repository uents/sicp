
;; (define fail #f)

;;; initialize fail
;; (call/cc
;;  (lambda (cc)
;;    (set! fail
;;          (lambda ()
;;            (cc 'no-choise)))))

(define amb-fail
  (lambda ()
	"there are no more values"))

(define (amb . lst)
  (if (null? lst)
	  (amb-fail)
	  (let ((fail amb-fail))
		(call/cc
		 (lambda (cc)
		   (set! amb-fail
				 (lambda ()
				   (set! amb-fail fail)
				   (cc (apply amb (cdr lst)))))
		   (cc (car lst)))))))

;; (define-syntax amb
;;   (syntax-rules ()
;;     ((_) (amb-fail))
;;     ((_ a) a)
;;     ((_ a b ...)
;;      (let ((fail amb-fail))
;;        (call/cc
;; 		(lambda (cc)
;; 		  (set! amb-fail
;; 				(lambda ()
;; 				  (set! amb-fail fail)
;; 				  (cc (amb b ...))))
;; 		  (cc a)))))))

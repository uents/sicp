
;; (define fail false)

;; (call/cc
;;  (lambda (cont)
;;    (set! fail
;; 		 (lambda ()
;; 		   "there are no more values"))))

(define fail-init
  (lambda ()
	"there are no more values"))

(define fail fail-init)

(define-syntax amb
  (syntax-rules ()
    ((_)
	 (begin (set! fail fail-init)
			"there is no current program"))
    ((_ first) first)
    ((_ first second ...)
     (let ((former-fail fail))
       (call/cc (lambda (cont)
				  (set! fail
						(lambda ()
						  (set! fail former-fail)
						  (cont (amb second ...))))
				  (cont first)))))))

(define-syntax try-again
  (syntax-rules ()
	((_) (fail))))

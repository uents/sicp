
;; (define next false)

;; (call/cc
;;  (lambda (cont)
;;    (set! next
;; 		 (lambda ()
;; 		   "there are no more values"))))

(define init
  (lambda ()
	"there is no current program"))

(define next init)

(define-syntax amb
  (syntax-rules ()
;    ((_)
;	 (begin (display "@@@") (next)))
;	 (escape (begin
;			   (set! next init)
;			   "there is no current program")))
    ((_ first) first)
    ((_ first second ...)
	 (begin
;	   (pretty-print "starting a new program")
;	   (set! next init)
	   (let ((former-next next))
		 (call/cc
		  (lambda (cont)
			(set! next
				  (lambda ()
					(set! next former-next)
					(cont (amb second ...))))
			(cont first))))))))

(define-syntax try-again
  (syntax-rules ()
	((_) (next))))


(define escape false)

(call/cc (lambda (cont)
		   (set! escape cont)))
		 
(define-syntax backtrack
  (syntax-rules ()
	((_)
	 (escape (next)))))
;	 (escape (begin (next)
;					(if (eq? next init)
;						"there are no more values"
;						false))))))

(define (require p)
  (if p true (backtrack)))

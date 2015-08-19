
(load-relative "../misc.scm")
(load-relative "data-types.scm")
(load-relative "environment.scm")
(load-relative "evaluator.scm")
(load-relative "primitive-procedures.scm")
(load-relative "special-forms.scm")
(load-relative "derived-expressions.scm")
(load-relative "parser.scm")

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (setup-environment)
  (let* ((frame (make-hash))
		 (env (extend-environment (primitive-procedure-names)
								  (primitive-procedure-objects)
								  (list frame))))
	(define-variable! 'true (lambda (env) true) env)
	(define-variable! 'false (lambda (env) false) env)
	env))

(define the-global-environment (setup-environment))

(define (driver-loop)
  (input-prompt)
  (let* ((input (read))
		 (output (eval-proc (analyze input)
							the-global-environment)))
	(output-prompt)
	(user-print output))
  (driver-loop))

(define (input-prompt)
  (newline)
  (display ";;; M-Eval input:")
  (newline))

(define (output-prompt)
  (newline)
  (display ";;; M-Eval value:")
  (newline))

(define (user-print object)
  (display object)
  (newline))

;; (if (compound-procedure? object)
  ;;     (display (list 'compound-procedure
  ;;                    (procedure-parameters object)
  ;;                    (procedure-body object)
  ;;                    '<procedure-env>))
  ;;     (display object)))


(define (analyze exp)
  (display (format "parse: ~A ~%" exp))
  (cond ((number? exp) (analyze-number-value exp))
		((string? exp) (analyze-string-value exp))
		((symbol? exp) (analyze-variable exp))
		;; special forms
		((tagged-list? exp 'quote) (analyze-quoted exp))
		((tagged-list? exp 'set!) (analyze-assginment exp))
		((tagged-list? exp 'define) (analyze-definition exp))
		((tagged-list? exp 'if) (analyze-if exp))
		((tagged-list? exp 'lambda) (analyze-lambda exp))
		((tagged-list? exp 'begin) (analyze-sequence exp))
		;; derived expressions
		((tagged-list? exp 'let) (analyze (let->combination exp)))
		;; application
		((pair? exp) (analyze-application exp))
		(else
		 (error "parse: unknown expression: " exp))))

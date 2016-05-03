### Requirements

- Racket 6.0+

### Run

1. Start Racket REPL.
2. Load and start evaluator on Racket REPL.

```scheme
racket@> ,enter "anylizing-mceval.scm" (or "mceval.scm")

;;; M-Eval input:

```

### Examples

```scheme
;;; M-Eval input:
(+ 1 2 3)

;;; M-Eval value:
6

;;; M-Eval input:
(define (fact n)
  (if (< n 1)
	  1
	  (* n (fact (- n 1)))))

;;; M-Eval value:
#<void>

;;; M-Eval input:
(fact 5)

;;; M-Eval value:
120
```

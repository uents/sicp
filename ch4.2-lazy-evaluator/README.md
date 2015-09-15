### Requirements

- Racket 6.0+

### Run

1. Start Racket REPL. (or DrRacket)
2. Load script on Racket REPL.

```scheme
racket@> ,enter "ch4-leval.scm"
'METACIRCULAR-EVALUATOR-LOADED
'LAZY-EVALUATOR-LOADED

;;; L-Eval input:

```

### Examples

```scheme
;;; L-Eval input:
(define (try a b)
  (if (= a 0) 1 b))

;;; L-Eval value:
ok

;;; L-Eval input:
(try 0 (/ 1 0))

;;; L-Eval value:
1
```

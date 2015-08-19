### Requirements

- Racket 6.0+
- SICP Complete Code
    + get `ch4-leval.scm` and `ch4-mceval.scm` from https://mitpress.mit.edu/sicp/code/index.html

### Run

(1). Start Racket REPL.

(2). Load start script on Racket REPL.

```scheme
racket@> (load "start.scm")

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

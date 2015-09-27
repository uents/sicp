### Requirements

- Racket 6.0+

### Run

1. Start Racket REPL
2. Load start script on Racket REPL

```scheme
racket@> ,enter "ch4-query.scm"
'done
racket@ch4-query.scm> (query-driver-loop)

;;; Query input:

```

### Exmaples

simple query.

```scheme
;;; Query input:
(job ?x ?y)

;;; Query results:
(job (Aull DeWitt) (administration secretary))
(job (Cratchet Robert) (accounting scrivener))
(job (Scrooge Eben) (accounting chief accountant))
(job (Warbucks Oliver) (administration big wheel))
(job (Reasoner Louis) (computer programmer trainee))
(job (Tweakit Lem E) (computer technician))
(job (Fect Cy D) (computer programmer))
(job (Hacker Alyssa P) (computer programmer))
(job (Bitdiddle Ben) (computer wizard))
```

compound query.

```scheme
;;; Query input:
(and (job ?who (computer programmer))
     (address ?who ?where))

;;; Query results:
(and (job (Fect Cy D) (computer programmer)) (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(and (job (Hacker Alyssa P) (computer programmer)) (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))

;;; Query input:
(or (supervisor ?who (Bitdiddle Ben))
	(supervisor ?who (Hacker Alyssa P)))

;;; Query results:
(or (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
(or (supervisor (Reasoner Louis) (Bitdiddle Ben)) (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(or (supervisor (Fect Cy D) (Bitdiddle Ben)) (supervisor (Fect Cy D) (Hacker Alyssa P)))
(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
```

filter.

```scheme
;;; Query input:
(and (supervisor ?who (Bitdiddle Ben))
	 (not (job ?who (computer programmer))))

;;; Query results:
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (not (job (Tweakit Lem E) (computer programmer))))
```

rule.

```scheme
;;; Query input:
(lives-near ?who (Bitdiddle Ben))

;;; Query results:
(lives-near (Aull DeWitt) (Bitdiddle Ben))
(lives-near (Reasoner Louis) (Bitdiddle Ben))
```



### Requirements

- Racket 6.0+

### Run

Start Racket REPL and load script.

```scheme
racket@> (load "ch2.x.scm")
```

### Examples

#### `ch2.2.4.scm` (Chapter 2.2.4 A Picture Language script)

```scheme
racket@> (load "ch2.2.4.scm")
racket@> (open-canvas)
racket@> (draw wave)
racket@> (clear-canvas)
racket@> (draw (corner-split wave 4))
racket@> (clear-canvas)
racket@> (draw (square-limit wave 4))

;; ...
```

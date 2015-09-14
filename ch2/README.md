### Requirements

- Racket 6.0+

### Run

- Start Racket REPL and load script.

```scheme
racket@> ,enter "ch2.x.scm"
```

#### `ch2.2.4.scm` (Chapter 2.2.4 A Picture Language)

1. Start Emacs and Racket REPL. (or DrRacket)
2. Load this script and type command lines as below.

```scheme
racket@> ,enter "ch2.2.4.scm"
racket@ch2.2.4.scm> (open-canvas)
racket@ch2.2.4.scm> (draw wave)
racket@ch2.2.4.scm> (clear-canvas)
racket@ch2.2.4.scm> (draw (corner-split wave 4))
racket@ch2.2.4.scm> (clear-canvas)
racket@ch2.2.4.scm> (draw (square-limit wave 4))

;; ...
```

#### `ch2.2.4-planet-sicp.scm` (using sicp.ss)

1. Start DrRacket and load this script.
2. Run it.
3. Type command lines as below on console.

```scheme
> (paint wave)
> (paint (corner-split wave 4))
> (paint (square-limit wave 4))
```

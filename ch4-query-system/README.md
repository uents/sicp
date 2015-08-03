
### 使い方

Racket Shellで以下のように入力する。

```scheme
racket@> (load "start.scm")
```

または、

```scheme
racket@> (require r5rs)
racket@> (load "../streams.scm")
racket@> (load "ch4-query.scm")
racket@> (initialize-data-base microshaft-data-base)
racket@> (query-driver-loop)
```

### 実行例

プログラマを全て見つける。

```scheme
;;; Query input:
(job ?who (computer programmer))

;;; Query results:
(job (Fect Cy D) (computer programmer))
(job (Hacker Alyssa P) (computer programmer))
```

全ての社員の住所をリストアップする。

```scheme
;;; Query input:
(address ?x ?y)

;;; Query results:
(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(address (Scrooge Eben) (Weston (Shady Lane) 10))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(address (Fect Cy D) (Cambridge (Ames Street) 3))
(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
```

コンピュータ部門の社員を見つける。

```scheme
;;; Query input:
(job ?who (computer ?type))

;;; Query results:
(job (Tweakit Lem E) (computer technician))
(job (Fect Cy D) (computer programmer))
(job (Hacker Alyssa P) (computer programmer))
(job (Bitdiddle Ben) (computer wizard))
```

さらに`.`をつけると複数のシンボルにもマッチする。

```scheme
;;; Query input:
(job ?who (computer . ?type))

;;; Query results:
(job (Reasoner Louis) (computer programmer trainee))
(job (Tweakit Lem E) (computer technician))
(job (Fect Cy D) (computer programmer))
(job (Hacker Alyssa P) (computer programmer))
(job (Bitdiddle Ben) (computer wizard))
```


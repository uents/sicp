### Requirements

- Ruby 2.2.2+
- Bundler 
- rbenv

### Setup & Run

(1). install ruby.

```sh
% rbenv install 2.2.2
% cd ch4.1-ruby-evaluator
% ruby local 2.2.2
```

(2). install needed packages with bundler.

```sh
% bundle install --path vendor/bundle
```

(3). start pry and load evaluator.

```sh
% bundle exec pry
[1] pry(main)> load "repl.rb"
>
```

`>` is REPL prompt. for example, input scheme code as below.

```scheme
> (+ 1 2 3)
=> 6
> (define (fact n)
    (if (< n 1)
	    1
		(* n (fact (- n 1)))))
> (fact 5)
=> 120
```

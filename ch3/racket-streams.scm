;;;; #lang racket

#|
racket/streamについて

- cons-stream :
  + stream-consが特殊形式のためマクロで再定義
- stream-car, stream-cdr, stream-null?, the-stream-empty :
  + SICPの名前に合わせて定義
- stream-map :
  + rakcet/stream版は引数に複数のストリームが取れないので
    ex 3.50から引用
- stream-ref, stream-filter など :
  + racket/streamに含まれている手続きをそのまま流用

|#

(module racket-streams racket
  (provide (all-defined-out))

  (define nil '())

  (require (prefix-in strm: racket/stream))

  (define-syntax cons-stream
	(syntax-rules ()
	  ((_ a b) (strm:stream-cons a b))))
  (define stream-car strm:stream-first)
  (define stream-cdr strm:stream-rest)
  (define stream-null? strm:stream-empty?)
  (define the-empty-stream strm:empty-stream)

  ;; form ex 3.50
  (define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
		 (apply proc (map stream-car argstreams))
		 (apply stream-map
				(cons proc (map stream-cdr argstreams))))))

  (define (list->stream sequence)
	(if (null? sequence)
		nil
		(cons-stream (car sequence)
					 (list->stream (cdr sequence)))))

  (define (scale-stream s factor)
	(stream-map (lambda (x) (* x factor)) s))

  (define (add-streams s1 s2)
	(stream-map + s1 s2))

  (define (mul-streams s1 s2)
	(stream-map * s1 s2))

  )

(require 'racket-streams)

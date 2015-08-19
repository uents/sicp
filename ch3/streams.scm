;;;; #lang racket

(module streams racket
  (provide (all-defined-out))

  (define nil '())

  (define (memo-proc proc)
	(let ((already-run? false)
		  (result false))
	  (lambda ()
		(if (not already-run?)
			(begin (set! result (proc))
				   (set! already-run? true)
				   result)
			result))))

;  (define-syntax cons-stream
;	(syntax-rules ()
;	  ((_ a b) (cons a (lambda () b)))))

  ;; memoized stream
  (define-syntax cons-stream
	(syntax-rules ()
	  ((_ a b) (cons a (memo-proc (lambda () b))))))

  (define (stream-car s) (car s))
  (define (stream-cdr s) ((cdr s)))
  (define (stream-null? s) (null? s))
  (define the-empty-stream '())

  (define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

  (define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream))
		   (cons-stream (stream-car stream)
						(stream-filter pred
									   (stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))

  (define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
		 low
		 (stream-enumerate-interval (+ low 1) high))))

  (define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
			   (stream-for-each proc (stream-cdr s)))))

  (define (display-stream s)
	(stream-for-each
	 (lambda (x) (display (format "~a " x))) s)
	(newline))

  ;; from ex 3.50
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

  (define (stream->list s)
	(if (stream-null? s)
		nil
		(cons (stream-car s)
			  (stream->list (stream-cdr s)))))

  (define (scale-stream s factor)
	(stream-map (lambda (x) (* x factor)) s))

  (define (add-streams s1 s2)
	(stream-map + s1 s2))

  (define (mul-streams s1 s2)
	(stream-map * s1 s2))

  )

(require 'streams)


;;; @NOTE reackt/streamを使う場合
;;; - cons-stream : stream-consが特殊形式のためマクロで再定義
;;; - stream-car, stream-cdr, stream-null?, the-stream-empty : SICPの名前に合わせて定義
;;; - stream-ref, stream-map, stream-filter など : racket/streamに含まれている

;; (require racket/stream)
;; (define-syntax cons-stream
;;   (syntax-rules ()
;; 	((cons-stream a b) (stream-cons a b))))
;; (define stream-car stream-first)
;; (define stream-cdr stream-rest)
;; (define stream-null? stream-empty?)
;; (define the-empty-stream empty-stream)

;;;; #lang racket

(require racket/stream)
(define-syntax cons-stream
  (syntax-rules ()
 	((_ a b) (stream-cons a b))))
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)
(define (list->stream sequence)
	(if (null? sequence)
		nil
		(cons-stream (car sequence)
					 (list->stream (cdr sequence)))))

(require r5rs)
(load "ch4-query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

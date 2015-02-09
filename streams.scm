
;;; streamsを使うためにロード
(require racket/stream)

;;; SICPの表記に合わせて再定義
;;; stream-consは特殊形式のためcons-streamに再定義できない
(define stream-null? stream-empty?)
;;(define cons-stream stream-cons) 
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define the-empty-stream empty-stream)

;; @@@TODO 再帰降下に対応する
(define (display-stream s)
  (stream-for-each
   (lambda (x) (display (format "~a " x))) s)
  (newline))

(define (list->stream sequence)
  (if (null? sequence)
	  nil
	  (stream-cons (car sequence)
				   (list->stream (cdr sequence)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-stream
	  (stream-cons
	   (apply proc (map stream-car argstreams))
	   (apply high-stream-map
			  (cons proc (map stream-cdr argstreams))))))

(define (add-stream s1 s2)
  (stream-map + s1 s2))


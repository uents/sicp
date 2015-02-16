
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

(define (mono-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (mono-map proc (cdr items)))))

(define (high-map proc . argitems)
  (if (null? (car argitems))
	  nil
	  (cons
	   (apply proc (mono-map car argitems))
	   (apply high-map
			  (cons proc (mono-map cdr argitems))))))

;;; ex 3.50
(define (high-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-stream
	  (stream-cons
	   (apply proc (map stream-car argstreams))
	   (apply high-stream-map
			  (cons proc (map stream-cdr argstreams))))))

(define (list->stream sequence)
  (if (null? sequence)
	  nil
	  (stream-cons (car sequence)
				   (list->stream (cdr sequence)))))

(define (scale-stream s factor)
  (high-stream-map (lambda (x) (* x factor)) s))

(define (add-streams s1 s2)
  (high-stream-map + s1 s2))

(define (mul-streams s1 s2)
  (high-stream-map * s1 s2))



(define nil '())

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (square x) (* x x))

(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))

(define (append! x y)
  (if (pair? x)
	  (begin
		(set-cdr! (my-last-pair x) y)
		x)
	  (error "Object is not pair" x)))
  
;; 引数xがnilの場合に対応したくても
;; 以下の実装では値渡しとなるため期待通りには動かない
;; (define (append! x y)
;;   (cond ((null? x) (set! x y))
;; 		((pair? x) (set-cdr! (my-last-pair x) y))
;; 		(else (error "Object is atom" x)))
;;   x)


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

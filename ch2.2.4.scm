;;;; #lang racket
;;;;
;;;; Chapter 2.4
;;;;  A Picture Language (using racket graphics legacy library)
;;;;
;;;; Author: @uents on twitter
;;;;
;;;; Usage:
;;;;
;;;; 0. Setup Geiser on Emacs 24
;;;;     see also my blog (in Japanese)
;;;;     http://uents.hatenablog.com/entry/2014/07/09/143208
;;;;
;;;; 1. Checkout my codes
;;;;     git clone https://github.com/uents/sicp.git
;;;;
;;;; 2. Start Emacs and Racket REPL (M-x run-racket)
;;;;
;;;; 3. Exec below commands on Racket REPL
;;;;
;;;;   (load "ch2.2.4.scm")
;;;;   (open-canvas)
;;;;   (draw wave)
;;;;   (clear-canvas)
;;;;   ....
;;;;

(load "misc.scm")


;;;; Vector (ex. 2.46)

(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
			 (+ (ycor-vect vec1) (ycor-vect vec2))))

(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
			 (- (ycor-vect vec1) (ycor-vect vec2))))

(define (scale-vect s vec)
  (make-vect (* s (xcor-vect vec))
			 (* s (ycor-vect vec))))


;;;; Frame (ex. 2.47)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;;;; Segment (ex. 2.48)

(define (make-segment start-vec end-vec) (cons start-vec end-vec))
(define start-segment car)
(define end-segment cdr)

;;;; Painter

;;; - ペインタとはフレームを引数にとり、そのフレームのなかに図形を描画する手続き
;;; - segment->painterは線分リストを引数にとり、
;;;   その線分を指定されるフレームないに描画する手続きを返す
;;; - segment->painterに与える線分リストは
;;;   単位方形 ([0.0,0.0] .. [1.0,1.0]) での座標を使って与えられる
;;; - SICPのテキスト内のdraw-lineという手続きは、Racket Graphics Legacy Libraryに
;;;   同名の手続きがあるため、ここではlineに変更する

(define (segments->painter segment-list)
  (lambda (frame)
	(for-each
	 (lambda (segment)
	   (line
		((frame-coord-map frame) (start-segment segment))
		((frame-coord-map frame) (end-segment segment))))
	 segment-list)))

;;; フレームの外形を描くペインタ (ex. 2.49a)

(define outline
  (let* ((v0 (make-vect 0.0 0.0))
		 (v1 (make-vect 1.0 0.0))
		 (v2 (make-vect 0.0 1.0))
		 (v3 (make-vect 1.0 1.0)))
	(segments->painter (list (make-segment v0 v1)
							  (make-segment v1 v3)
							  (make-segment v3 v2)
							  (make-segment v2 v0)))))

;;; フレームの向かい側の頂点を結んで "X" を描くペインタ (ex. 2.49b)

(define diagonal
  (let* ((v0 (make-vect 0.0 0.0))
		 (v1 (make-vect 1.0 0.0))
		 (v2 (make-vect 0.0 1.0))
		 (v3 (make-vect 1.0 1.0)))
	(segments->painter (list (make-segment v0 v3)
							 (make-segment v1 v2)))))

;;; フレームの辺の中点を結んで菱形を描くペインタ (ex. 2.49c)

(define diamond
  (let* ((m1 (make-vect 0.5 0.0))
		 (m2 (make-vect 0.0 0.5))
		 (m3 (make-vect 1.0 0.5))
		 (m4 (make-vect 0.5 1.0)))
	(segments->painter (list (make-segment m1 m3)
							 (make-segment m3 m4)
							 (make-segment m4 m2)
							 (make-segment m2 m1)))))

;;; waveペインタ (ex. 2.49d)

(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))


;;;; Painter Operations (ex 2.44-45)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; todo
(define (below painter1 painter2) nil)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (split op1 op2)
  (define (proc painter n)
	(if (= n 0)
		painter
		(let ((smaller (proc painter (- n 1))))
		  (op1 painter (op2 smaller smaller)))))
  proc)

(define right-split (split beside below))

(define up-split (split below beside))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


;;;; Canvas

(require graphics/graphics)

(define canvas-margin 4)
(define canvas-width  512)
(define canvas-height 512)

(define canvas-frame
  (make-frame (make-vect canvas-margin (+ canvas-margin canvas-height))
			  (make-vect canvas-width 0)
			  (make-vect 0 (* -1 canvas-height))))

(define vp nil) ;; view point

(define open-canvas
  (lambda ()
	(if (null? vp)
		(begin
		  (open-graphics)
		  (set! vp (open-viewport "The Picture Language"
								  (+ canvas-width  (* canvas-margin 2))
								  (+ canvas-height (* canvas-margin 2)))))
		nil)))


(define close-canvas
  (lambda ()
	(if (null? vp)
		nil
		(begin
		  (close-viewport vp)
		  (close-graphics)
		  (set! vp nil)))))

(define clear-canvas
  (lambda ()
	(if (null? vp)
		nil
		((clear-viewport vp)))))

;;; draw line on canvas
(define (line start-vec end-vec)
  (define (vect->posn vec)
	(make-posn (xcor-vect vec) (ycor-vect vec)))
  ((draw-line vp) (vect->posn start-vec)
                  (vect->posn end-vec)))

;;; draw painter on canvas
(define (draw painter)
  (painter canvas-frame))

   

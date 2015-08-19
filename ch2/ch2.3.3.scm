;;;; #lang racket
;;;;
;;;; SICP Chapter 2.3.3 Representing Sets
;;;;
;;;; Author: @uents on twitter
;;;; 

(load-relative "../misc.scm")


;;;; 2.3.3 集合の表現

;;;; unordered set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; ex 2.59

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
		((null? set1) set2)
		((null? set2) set1)
		((not (element-of-set? (car set1) set2))
		 (cons (car set1)
			   (union-set (cdr set1) set2)))
		(else (union-set (cdr set1) set2))))

;;; ex 2.60
;; element-of-set?とintersection-setは変更なし

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))


;;;; ordered set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; ex 2.61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		(else (cons (car set)
					(adjoin-set x (cdr set))))))

;;; ex 2.62

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
		((null? set1) set2)
		((null? set2) set1)
		((< (car set1) (car set2))
		 (cons (car set1)
			   (union-set (cdr set1) set2)))
		((> (car set1) (car set2))
		 (cons (car set2)
			   (union-set set1 (cdr set2))))
		(else (cons (car set1)
					(union-set (cdr set1) (cdr set2))))))


;;;; binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

  
;;; ex 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;;; ex 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
			 (left-result (partial-tree elts left-size))
			 (left-tree (car left-result))
			 (non-left-elts (cdr left-result))
			 (right-size (- n (+ left-size 1)))
			 (this-entry (car non-left-elts))
			 (right-result (partial-tree (cdr non-left-elts)
										 right-size))
			 (right-tree (car right-result))
			 (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
			  remaining-elts))))


;;; ex 2.65

(define (union-set-2 tree1 tree2)
  (list->tree
   (union-set (tree->list-2 tree1)
			  (tree->list-2 tree2))))

(define (intersection-set-2 tree1 tree2)
  (list->tree
   (intersection-set (tree->list-2 tree1)
					 (tree->list-2 tree2))))



;;;; information retrieval

(define (make-record key value) (cons key value))
(define (key record) (car record))
(define (value record) (cdr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define slecao
  (list
   (make-record 3  '(Thiago Silva))
   (make-record 4  '(David Louis))
   (make-record 5  '(Fernandinho))
   (make-record 6  '(Marcelo))
   (make-record 7  '(Hulk))
   (make-record 8  '(Paulinho))
   (make-record 9  '(Fred))
   (make-record 10 '(Neymar))
   (make-record 11 '(Oscar))
   (make-record 12 '(Julio Cesar))
   (make-record 23 '(Maicon))))


; (lookup 3  slecao)  ; => '(3 Thiago Silva)
; (lookup 10 slecao)  ; => '(10 Neymar)
; (lookup 13 slecao)  ; => #f


;;; ex 2.66

(define (lookup given-key tree)
  (cond ((null? tree) false)
		((equal? given-key (key (entry tree)))
		 (entry tree))
		((< given-key (key (entry tree)))
		 (lookup given-key (left-branch tree)))
		(else
		 (lookup given-key (right-branch tree)))))

(define slecao
  (list->tree
   (list
	(make-record 3  '(Thiago Silva))
	(make-record 4  '(David Louis))
	(make-record 5  '(Fernandinho))
	(make-record 6  '(Marcelo))
	(make-record 7  '(Hulk))
	(make-record 8  '(Paulinho))
	(make-record 9  '(Fred))
	(make-record 10 '(Neymar))
	(make-record 11 '(Oscar))
	(make-record 12 '(Julio Cesar))
	(make-record 23 '(Maicon)))))


; (lookup 3  slecao)  ; => '(3 Thiago Silva)
; (lookup 10 slecao)  ; => '(10 Neymar)
; (lookup 13 slecao)  ; => #f


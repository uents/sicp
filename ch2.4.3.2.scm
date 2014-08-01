;;;; #lang racket
;;;;
;;;; SICP Chapter 2.4.3 Data-Directed Programming and Additivity
;;;;
;;;; Author: @uents on twitter
;;;;
;;;; Usage:
;;;;
;;;; 0. Setup Geiser on Emacs
;;;;     M-x package-install geiser
;;;;
;;;; 1. Download source codes
;;;;     git clone https://github.com/uents/sicp.git
;;;;
;;;; 2. Start Emacs and Racket REPL (M-x run-racket)
;;;;
;;;; 3. Executes below commands on Racket REPL
;;;;
;;;;   (load "ch2.4.3.2.scm")
;;;;   ....
;;;;

(load "misc.scm")

;;;; -----------------------------------
;;;; operation/type table (using hash tables)
;;;;  http://docs.racket-lang.org/guide/hash-tables.html
;;;;  http://docs.racket-lang.org/reference/hashtables.html 
;;;; -----------------------------------

(define *op-table* (make-hash))

(define (put op type item)
  (if (not (hash-has-key? *op-table* op))
	  (hash-set! *op-table* op (make-hash))
	  nil)
  (hash-set! (hash-ref *op-table* op) type item))

(define (get op type)
  (if (not (hash-has-key? *op-table* op))
	  (error "Bad key -- OPERATION" op)
	  (if (not (hash-has-key? (hash-ref *op-table* op) type))
		  (error "Bad key -- TYPE" type)
		  (hash-ref (hash-ref *op-table* op) type))))

;;;; -----------------------------------
;;;; type-tag and apply-generic system
;;;; -----------------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
		  ; argsはリストで渡されるので
		  ; contents手続きをmapしてprocをapplyで適用する
		  (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))




;;;;; ex 2.74


;;; a. 指定された事業所ファイルから従業員のレコードを返すget-recordを実装せよ

; employee-nameにタグがないので、apply-genericは使えない
; (またcompany-fileがタグを持つのも変な気もする)

(define (get-record file name)
  (let* ((tag (type-tag file))
		 (record ((get 'get-record tag) (contents file) name)))
	(if (null? record)
		nil
		(attach-tag tag record))))

;;; b. 従業員のレコードから給与情報を返すget-salaryを実装せよ

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

;;; c. 全ての事業所ファイルに対し従業員のレコードを返すfind-employee-recordを実装せよ

(define (find-employee-record files name)
  (filter pair? (map (lambda (file) (get-record file name)) files)))

;;; d.

;; original database
(define *tokyo-office-file*
'(((Hiroshi Nakajima) . 1200)
  ((Katsuo Isono) . 1500)
  ((Hanako Hanazawa) . 1400)
  ((Kaori Ohzora) . 1800)))
  

;; accessor package
(define (install-tokyo-office-package)
  (define (name-record record) (car record))
  (define (salary-record record) (cdr record))
  (define (get-record file name)
	(cond ((null? file) nil)
		  ((equal? name (name-record (car file))) (car file))
		  (else (get-record (cdr file) name))))
  (define (get-salary record)
	(salary-record record))

  ;; interface
  (put 'get-record 'tokyo get-record)
  (put 'get-salary 'tokyo get-salary)
  'done)

(install-tokyo-office-package)

;; append tag to original database file
(define *tokyo-office-file*
  (attach-tag 'tokyo *tokyo-office-file*))


;; original database of another office
(define *osaka-office-file*
'((1 (Namihei Isono) 3600)
  (2 (Masuo Fuguta) 2400)
  (3 (Nanbutsu Isasaka) 4500)))

;; accessor package
(define (install-osaka-office-package)
  (define (id-record record) (car record))
  (define (name-record record) (cadr record))
  (define (salary-record record) (caddr record))
  (define (get-record file name)
	(cond ((null? file) nil)
		  ((equal? name (name-record (car file))) (car file))
		  (else (get-record (cdr file) name))))
  (define (get-salary record)
	(salary-record record))

  ;; interface
  (put 'get-record 'osaka get-record)
  (put 'get-salary 'osaka get-salary)
  'done)

(install-osaka-office-package)

;; append tag to original database file
(define *osaka-office-file*
  (attach-tag 'osaka *osaka-office-file*))


;;; test

; racket@> (get-salary (get-record *tokyo-office-file* '(Katsuo Isono)))
; 1500
; 
; racket@> (get-salary (get-record *osaka-office-file* '(Masuo Fuguta)))
; 2400
;
; racket@> (get-record *tokyo-office-file* '(Wakeme Isono))
; '()


; racket@> (find-employee-record
; 		  (list *tokyo-office-file* *osaka-office-file*)
; 		  '(Katsuo Isono))
; '((tokyo (Katsuo Isono) . 1500))


; racket@> (find-employee-record
; 		  (list *tokyo-office-file* *osaka-office-file*)
; 		  '(Namihei Isono))
; '((osaka 1 (Namihei Isono) 3600))

; racket@> (find-employee-record
; 		  (list *tokyo-office-file* *osaka-office-file*)
; 		  '(Wakeme Isono))
; '()


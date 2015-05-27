;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Nondeterminsm usint macro amb
;;;      T.Shido
;;;      November 15, 2005
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; abbreviation for call-with-current-continuation
(define call/cc call-with-current-continuation)

;;; This function is re-assigned in `choose' and `fail' itself.
(define fail #f)


;;; nondeterminsm macro operator
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
	(lambda (cc)
	  (set! fail
		(lambda ()
		  (set! fail fail0)
		  (cc (amb b ...))))
	  (cc a)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for MIT-Scheme only
; use it if you don't like warning during compilation
; (define-syntax amb
;   (sc-macro-transformer
;    (lambda (exp env)
;      (if (null? (cdr exp))
;          `(fail)
;        `(let ((fail0 fail))
;           (call/cc
;            (lambda (cc)
;              (set! fail
;                    (lambda ()
;                      (set! fail fail0)
;                      (cc (amb ,@(map (lambda (x)
;                                        (make-syntactic-closure env '() x))
;                                      (cddr exp))))))
;              (cc ,(make-syntactic-closure env '() (second exp))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; function for nondeterminsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define (choose . ls)
;   (if (null? ls)
;       (fail)
;     (let ((fail0 fail))
;       (call/cc
;        (lambda (cc)
;          (begin
;           (set! fail
;                 (lambda ()
;                   (set! fail fail0)
;                   (cc (apply choose (cdr ls)))))
;           (cc (car ls))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returning all possibilities
(define-syntax set-of
  (syntax-rules () 
    ((_ s) 
      (let ((acc '())) 
        (amb (let ((v s)) 
               (set! acc (cons v acc)) 
               (fail)) 
             (reverse acc))))))

;;; if not pred backtrack
(define (assert pred)
  (or pred (amb)))

;;; returns arbitrary number larger or equal to n
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;;; returns arbitrary number between a and b
(define (number-between a b)
  (let loop ((i a))
    (if (> i b)
        (amb)
      (amb i (loop (+ i 1))))))


;;;;;;;;;;;; misc
(define (gen-prime n)
  (let ((i (number-between  2 n)))
    (assert (prime? i))
    i))

(define (prime? n)
  (let ((m (sqrt n)))
    (let loop ((i 2))
      (or (< m i)
          (and (not (zero? (modulo n i)))
               (loop (+ i (if (= i 2) 1 2))))))))

(define (sum-prime n)
  (let* ((i (number-between 1 n))
         (j (number-between i n)))
    (assert (prime? (+ i j)))
    (list i j)))


(define (sq x) (* x x))

(define (pythag i j k)
  (assert (= (sq k) (+ (sq i) (sq j))))
  (list i j k))

;;; small functions for SICP Exercise 4.42
(define (xor a b)
  (if a (not b) b))

(define (all-different? . ls)
  (let loop ((obj (car ls)) (ls (cdr ls)))
    (or (null? ls)
        (and (not (memv obj ls))
             (loop (car ls) (cdr ls))))))

;;; SICP Exercise 4.42
(define (girls-exam)
  (let ((kitty (number-between 1 5))
        (betty (number-between 1 5)))
    (assert (xor (= kitty 2) (= betty 3)))
    (let ((mary (number-between 1 5)))
      (assert (xor (= kitty 2) (= mary 4)))
      (assert (xor (= mary 4) (= betty 1)))
      (let ((ethel (number-between 1 5))
            (joan (number-between 1 5)))
        (assert (xor (= ethel 1) (= joan 2)))
        (assert (xor (= joan 3) (= ethel 5)))
        (assert (all-different? kitty betty ethel joan mary))
        (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))))

;;; Bad answer for ex 4.42
(define (girls-exam-x)
  (let ((kitty (number-between 1 5))
        (betty (number-between 1 5))
        (mary (number-between 1 5))
        (ethel (number-between 1 5))
        (joan (number-between 1 5)))
    (assert (xor (= kitty 2) (= betty 3)))
    (assert (xor (= kitty 2) (= mary 4)))
    (assert (xor (= mary 4) (= betty 1)))
    (assert (xor (= ethel 1) (= joan 2)))
    (assert (xor (= joan 3) (= ethel 5)))
    (assert (all-different? kitty betty ethel joan mary))
    (map list '(kitty betty ethel joan mary) (list kitty betty ethel joan mary))))


;;; to show cpu time
(define-syntax cpu-time/sec
  (syntax-rules ()
    ((_ s)
     (with-timings
	 (lambda () s)
       (lambda (run-time gc-time real-time)
	 (write (internal-time/ticks->seconds run-time))
	 (write-char #\space)
	 (write (internal-time/ticks->seconds gc-time))
	 (write-char #\space)
	 (write (internal-time/ticks->seconds real-time))
	 (newline))))))


;;; initializing fail
(call/cc
 (lambda (cc)
   (set! fail
         (lambda ()
           (cc 'no-choise)))))


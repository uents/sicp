;;;; #lang racket

(module mutable-lists racket
  (provide (all-defined-out))

  ;; Datatypes - Mutable Pairs and Lists
  (define pair? mpair?)
  (define cons mcons)
  (define car mcar)
  (define cdr mcdr)
  (define (caar p) (mcar (mcar p)))
  (define (cadr p) (mcar (mcdr p)))
  (define (cdar p) (mcdr (mcar p)))
  (define (cddr p) (mcdr (mcdr p)))
  (define (caaar p) (mcar (mcar (mcar p))))
  (define (caadr p) (mcar (mcar (mcdr p))))
  (define (cadar p) (mcar (mcdr (mcar p))))
  (define (caddr p) (mcar (mcdr (mcdr p))))
  (define (cdaar p) (mcdr (mcar (mcar p))))
  (define (cdadr p) (mcdr (mcar (mcdr p))))
  (define (cddar p) (mcdr (mcdr (mcar p))))
  (define (cdddr p) (mcdr (mcdr (mcdr p))))
  (define set-car! set-mcar!)
  (define set-cdr! set-mcdr!)

  ;; Compatibility Collection - Mutable List Functions
  (require compatibility/mlist)
  (define list? mlist?)
  (define list mlist)
  (define length mlength)
  (define list-ref mlist-ref)
  (define append mappend)
  (define map mmap)
  (define memq mmemq)
  (define assoc massoc)
  )

(require 'mutable-lists)

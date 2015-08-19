
(define primitive-procedures
  (list (list '= =)
		(list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '< <)
		(list '> >)		
		))

(define (primitive-procedure-names)
  (map car
	   primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
	   primitive-procedures))

(define (primitive-object-proc object)
  (cadr object))


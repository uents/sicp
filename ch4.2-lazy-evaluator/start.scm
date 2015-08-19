;;;; #lang racket

(require r5rs)
(load "ch4-leval.scm")
(define the-global-environment (setup-environment))
(driver-loop)

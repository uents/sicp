
(require r5rs)
(load "../streams.scm")
(load "ch4-query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

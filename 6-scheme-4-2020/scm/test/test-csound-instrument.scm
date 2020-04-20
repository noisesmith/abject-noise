(define-module (test test-csound-instrument))

(use-modules (srfi srfi-64)
             (csound instrument))

(test-begin "ports-test")
(let ((p (ports #h(#:amp #f #:hz #f #:tab #f)
                '(#:sig))))
  (test-assert "ports creation"
               p))
(test-end "ports-test")

(test-begin "node-test")
(let* ((-node (@@ (csound instrument) node)) ; get the private fn
       (n (-node (ports #h(#:amp #f #:hz #f #:tab #f)
                        '(#:sig)))))
  (test-assert "node creation"
               n))
(test-end "node-test")

(test-begin "insert-test")
(let ((i (insert #:sin (ports #h(#:amp #f #:hz #f #:tab #f)
                              '(#:sig)))))
  (test-assert "insertion of a node into an instrument"
               i))
(test-end "insert-test")

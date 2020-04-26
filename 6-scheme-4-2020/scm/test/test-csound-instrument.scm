(define-module (test test-csound-instrument))

(use-modules (srfi srfi-64)
             (noisesmith clojure)
             (csound instrument))

(test-begin "csound-instrument-test")
(let ((p (node #:in #h(#:amp #f #:hz #f #:tab #f)
               #:out '(#:sig))))
  (test-assert
    "node creation"
    p))

(define standard-ports
  (node #:out #h(#:sig #f) #:in #h(#:amp #f #:hz #f #:tab #f)))

(test-assert
  "creation of an instrument via insertion"
  (insert #:sin standard-ports))

(define i ; am!
  (~> (insert #:sin standard-ports)
      (insert #:tri standard-ports)
      (patch (plug #:sin #:sig)
             (plug #:tri #:amp))))

(test-assert
  "patching one node into another"
  i)

(test-equal
  "patched connection present"
  (get-in i '(#:graph #:tri #:in #:amp))
  (plug #:sin #:sig))

(test-end "csound-instrument-test")

(exit (test-runner-fail-count (test-runner-get)))

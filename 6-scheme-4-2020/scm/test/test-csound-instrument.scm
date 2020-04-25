(define-module (test test-csound-instrument))

(use-modules (srfi srfi-64)
             (noisesmith clojure)
             (csound instrument))

(test-begin "csound-instrument-test")
(let ((p (node #:in #h(#:amp #f #:hz #f #:tab #f)
                #:out '(#:sig))))
  (test-assert "node creation"
               p))

(let ((i (insert #:sin (node #:out '(#:sig)
                              #:in #h(#:amp #f #:hz #f #:tab #f)))))
  (test-assert "insertion of a node into an instrument"
               i))

(let* ((standard-ports (node #:out '#(#:sig) #:in #h(#:amp #f #:hz #f #:tab #f)))
       (i (~> (insert #:sin standard-ports)
              (insert #:tri standard-ports)
              (patch (plug #:sin #:sig) (plug #:tri #:amp))))) ; am!
  (test-assert "patching one node into another"
               i)
  (test-assert "patched connection present"
               (equal? (get-in i '(#:graph #:tri #:in #:amp))
                       (plug #:sin #:sig))))
(test-end "csound-instrument-test")

(exit (test-runner-fail-count (test-runner-get)))

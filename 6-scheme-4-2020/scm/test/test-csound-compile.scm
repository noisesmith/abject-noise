(define-module (test test-csound-compile))

(use-modules (srfi srfi-64)
             (noisesmith clojure)
             (csound compile))

(test-begin "csound-compile-test")

(test-assert "place holder"
             #t)

(test-end "csound-compile-test")

(exit (test-runner-fail-count (test-runner-get)))

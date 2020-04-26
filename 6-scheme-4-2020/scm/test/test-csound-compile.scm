(define-module (test test-csound-compile))

(use-modules (srfi srfi-64)
             (noisesmith clojure)
             (csound compile)
             (oop goops))

(test-begin "csound-compile-test")

(test-equal
  "compile a string"
  "foo"
  (compile "foo"))


(test-equal
  "compile a number"
  "2"
  (compile 2))

(test-equal
  "compile a list"
  "1 2.0 foo"
  (compile '(1 (2.0 "foo"))))

(test-equal
  "compile an assignment"
  "kv0, kv1   oscil kamp, 440.0, 1"
  (compile (make <assignment>
                 #:lvals '("kv0" "kv1")
                 #:opcode "oscil"
                 #:parameters '("kamp" 440.0 1))))

(test-end "csound-compile-test")

(exit (test-runner-fail-count (test-runner-get)))

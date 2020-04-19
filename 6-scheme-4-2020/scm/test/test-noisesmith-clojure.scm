(define-module (test test-noisesmith-clojure))

(use-modules (srfi srfi-64)
             (oop goops) ; for the class-of function
             (noisesmith clojure))

(test-begin "ht-test")

(test-assert
  "ht creation"
  (ht "dog" 1 #:pony #f))

(let ((h (test-read-eval-string "#h(#:a 0 #:b 1)")))
  (test-assert
    "ht reader"
    (equal? (class-of h) (@@ (noisesmith clojure) <ht>))))

(let ((h1 #h(1 2 2 1))
      (h2 #h(2 1 1 2)))
  (test-assert
    "ht unordered equality"
    (equal? h1 h2)))

(test-end "ht-test")

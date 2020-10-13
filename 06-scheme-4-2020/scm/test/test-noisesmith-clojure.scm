(define-module (test test-noisesmith-clojure))

(use-modules (srfi srfi-64)
             (oop goops) ; for the class-of function
             (noisesmith debug)
             (noisesmith clojure))

(define unit-test?
  (equal? (cdr (command-line))
          '("unit")))

(define event-capture
  (->catalog))

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
  ; (event-capture #:ordering #h(#:h1 h1 #:h2 h2))
  (test-assert
    "ht unordered equality"
    (equal? h1 h2)))

(let ((m #h(#:a #h(#:b #h(#:c 1))))
      (key-path '(#:a #:b #:c)))
  (test-assert "update-in of ht"
               (equal? 2
                       (-> m
                           (update-in key-path 1+)
                           (get-in key-path)))))

(test-end "ht-test")

(if unit-test?
  (exit (test-runner-fail-count (test-runner-get))))
(test-runner-reset (test-runner-current))

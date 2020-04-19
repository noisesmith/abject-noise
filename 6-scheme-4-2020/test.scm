(define-module (test))
(use-modules (srfi srfi-64))

(use-modules (srfi srfi-64))

(define-syntax test-via-module
  (syntax-rules
    ()
    ((test-via-module test-name module parent-module body ...)
     (dynamic-wind
       (lambda ()
         (test-begin test-name)
         (set-current-module (resolve-module (quote module))))
       (lambda ()
         (use-modules (srfi srfi-64))
         body ...)
       (lambda ()
         (test-end test-name)
         (set-current-module parent-module))))))

(define-syntax test-in-module
  (syntax-rules
    ()
    ((test-in-module test-name module body ...)
     (test-via-module test-name module (current-module) body ...))))

(test-in-module
  "noisesmith-clojure-test"
  (noisesmith clojure)
  ;; ht
  (let ((m (ht)))
    (test-assert
      "ht-creation"
      ht)))

(test-in-module
  "csound-instrument-compile-test"
  (csound instrument)
  ;; compile
  (let* ((datum "s")
         (s (compile datum)))
    (test-assert
      "compile a string"
      (equal? datum s))))



(test-in-module
  "csound-instrument-insert-test"
  (csound instrument)
  ;; plug
  (let* ((ins (make <instrument>))
         (p (ports '(#:sig) (ht #:amp #f #:freq #f #:tab #f)))
         (ins* (insert ins #:foo p)))
    (test-assert
      "instrument creation"
      ins)
    (test-assert
      "unit insertion"
      ins*)))

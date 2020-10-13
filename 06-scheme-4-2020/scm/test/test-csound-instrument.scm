(define-module (test test-csound-instrument))

(use-modules (ice-9 format)
             (srfi srfi-64)
             (noisesmith clojure)
             (noisesmith debug)
             (csound instrument))

(define unit-test?
  (equal? (cdr (command-line))
          '("unit")))

(define debug (->catalog))

(test-begin "csound-instrument-test")
(define standard-ports
  (->node #:in #h(#:amp #f #:hz #f #:tab #f)
          #:out #h(#:sig "a")
          #:formatter (lambda (ins outs)
                        (format #f
                                "~a oscil ~a, ~a, ~a\n"
                                (get outs #:sig)
                                (get ins #:amp)
                                (get ins #:hz)
                                (get ins #:tab)))))

(test-assert
  "creation of an instrument via insertion"
  (insert #:sin standard-ports))

(define i ; am!
  (-> (insert #:sin standard-ports)
      (insert #:tri standard-ports)
      (patch (->plug #:sin #:sig)
             (->plug #:tri #:amp))))

(define g
  (let ((graph (@@ (csound instrument) graph)))
    (graph i)))

(test-assert
  "patching one node into another"
  i)

(test-equal
  "patched connection present"
  (get-in i '(#:graph #:tri #:in #:amp))
  (->plug #:sin #:sig))


(let* ((derive-input-map (@@ (csound instrument) derive-input-map))
       (tokenize-graph (@@ (csound instrument) tokenize-graph))
       (m (reduce-kv tokenize-graph g)))
  (test-equal
    "derive-input-map for getting plug names"
    #h((->plug #:tri #:sig)
       "atri_sig"
       (->plug #:sin #:sig)
       "asin_sig")
    (derive-input-map m)))

(let ((split-ready-nodes (@@ (csound instrument) split-ready-nodes)))
  (test-equal
    "split-ready-nodes resolves correctly"
    (list #h() g)
    (split-ready-nodes g)))

(define all-hooked-up
  (-> i
      (patch 440 (->plug #:tri #:hz))
      (patch 2 (->plug #:tri #:tab))
      (patch "0dbfs" (->plug #:sin #:amp))
      (patch 620 (->plug #:sin #:hz))
      (patch 1 (->plug #:sin #:tab))))

(define expected-instrument
  "          instr 1
asin_sig oscil 0dbfs, 620, 1
 atri_sig oscil asin_sig, 440, 2
          endin
")

(test-equal
  "compiled result"
  expected-instrument
  (compile all-hooked-up 1))

(test-end "csound-instrument-test")
(if unit-test?
  (exit (test-runner-fail-count (test-runner-get))))
(test-runner-reset (test-runner-current))

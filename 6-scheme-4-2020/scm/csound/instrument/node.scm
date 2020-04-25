(define-module (csound instrument node)
               #:export (node in out <node>)
               #:use-module (noisesmith clojure)
               #:re-export (ht))
(use-modules
  (oop goops))

(define-class
  ;; an individual node in an instrument graph
  <node> ()
  (in
    #:init-keyword #:in
    #:getter in)
  (out
    #:init-keyword #:out
    #:getter out))

(define (node . args)
  (apply make <node> args))

(define-method
  (write (n <node>) port)
  (display "[<node> :in=" port)
  (write (in n) port)
  (display ", :out=" port)
  (write (out n) port)
  (display "]" port))

(define-method
  (get (n <node>) k)
  (get #h(#:in (in n) #:out (out n)) k))


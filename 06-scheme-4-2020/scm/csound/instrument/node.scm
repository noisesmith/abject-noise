(define-module (csound instrument node)
               #:export (->node in out <node>)
               #:use-module (noisesmith clojure)
               #:re-export (ht))
(use-modules
  (csound compile)
  (oop goops))

(define-class
  ;; an individual node in an instrument graph
  <node> ()
  (in
    #:init-keyword #:in
    #:getter in)
  (out
    #:init-keyword #:out
    #:getter out)
  (formatter
    #:init-keyword #:formatter
    #:getter formatter))

(define-method
  (compile (n <node>))
  ((formatter n) (in n) (out n)))

(define (->node . args)
  (apply make <node> args))

(define-method
  (write (n <node>) port)
  (display "(->node #:in " port)
  (write (in n) port)
  (display " #:out " port)
  (write (out n) port)
  (display ")" port))

(define-method
  (get (n <node>) k)
  (get #h(#:in (in n) #:out (out n)) k))

(define-method
  (get (n <node>) k default)
  (get #h(#:in (in n) #:out (out n)) k default))

(define-method
  (equal? (n1 <node>) (n2 <node>))
  (and (equal? (out n1) (out n2))
       (equal? (in n1) (in n2))))

(define-method
  (assj (n <node>) k v)
  (cond ((equal? #:in k)
         (->node #:in v #:out (out n) #:formatter (formatter n)))
        ((equal? #:out k)
         (->node #:in (in n) #:out v #:formatter (formatter n)))
        (#t n)))

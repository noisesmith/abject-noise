(define-module (csound instrument plug)
               #:export (->plug <plug> node slot)
               #:use-module (noisesmith clojure)
               #:re-export (ht))

(use-modules (oop goops))

(define-class
  <plug> ()
  (node
    #:init-keyword #:node
    #:getter node)
  (slot
    #:init-keyword #:slot
    #:getter slot))

(define (->plug node slot)
  (make <plug>
        #:node node
        #:slot slot))

(define-method
  (write (p <plug>) port)
  (display "(->plug #:node " port)
  (write (node p) port)
  (display ", #:slot " port)
  (write (slot p) port)
  (display ")" port))

(define-method
  (equal? (p1 <plug>) (p2 <plug>))
  (and (equal? (node p1) (node p2))
       (equal? (slot p1) (slot p2))
       #t))


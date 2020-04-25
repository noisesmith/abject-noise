(define-module (csound compile)
               #:export (compile)
               #:use-module (noisesmith clojure)
               #:re-export (ht))
(use-modules
  (ice-9 format)
  (oop goops))

(define-generic compile)

(define-method
  (compile (unit <string>))
  unit)

(define-method
  (compile (unit <number>))
  (compile (number->string unit)))

(define-method
  (compile (unit <list>))
  (string-join (map compile unit) " "))

(define-class
  <assignment> ()
  (lvals
    #:init-keyword #:lvals
    #:getter lvals)
  (opcode
    #:init-keyword #:opcode
    #:getter opcode)
  (parameters
    #:init-keyword #:parameters
    #:getter parameters))

(define (paramlist l)
  (string-join (map compile l) ", "))

(define-method
  (compile (unit <assignment>))
  (format #f "~10a ~a ~a"
          (paramlist (lvals unit))
          (compile (opcode unit))
          (paramlist (parameters unit))))

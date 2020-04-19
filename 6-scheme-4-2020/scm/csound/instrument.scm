(define-module (csound instrument)
               #:export (insert patch plug ports)
               #:use-module (noisesmith clojure)
               #:re-export (ht))

(use-modules
  (ice-9 format)
  (oop goops)
  (csound csound))

; an instrument compiler

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

(define-class
  <instrument> ()
  (synthesis-node-graph
    #:init-keyword #:graph
    #:init-form (ht)
    #:getter graph))

(define-method
  (write (i <instrument>) port)
  (write (format #f "instrument: graph {~a}" (graph i))
           port))

(define-class
  ;; an individual node in an instrument graph
  <node> ()
  (in
    #:init-keyword #:in
    #:accessor in)
  (out
    #:init-keyword #:out
    #:accessor out))

(define-class
  <ports> ()
  ;; a list of #:keys for input port identifiers
  (input-list
    #:init-keyword #:in
    #:getter in)
  ;; a list of #:keys for output port identifiers
  (output-list
    #:init-keyword #:out
    #:getter out))

(define (ports outs ins)
  (make <ports>
        #:in ins
        #:out outs))

(define-method
  (node (ports <ports>))
  (make <node>
        #:in (in ports)
        #:out (out ports)))

(define-method
  (insert (i <instrument>) (n <top>) (p <ports>))
  (let* ((new-node (node p))
         (connections (conj (graph i) n new-node)))
    (make <instrument>
          #:graph connections)))

(define-method
  (insert (n <top>) (p <ports>))
  (insert (make <instrument>) n p))

(define-class
  <plug> ()
  (node
    #:init-keyword #:node
    #:getter node)
  (slot
    #:init-keyword #:slot
    #:getter slot))

(define (plug node slot)
  (make <plug>
        #:node node
        #:slot slot))

(define-method
  (patch (i <instrument>)
         (input <plug>)
         (output <plug>))
  (let* ((target-entry (get (graph i) (node output)))
         (connected (conj (in target-entry) (slot output) input))
         (new-node (make <node>
                         #:in connected
                         #:out (out target-entry))))
    (make <instrument>
          #:graph (conj (graph i) (node output) new-node))))

(define (normalize instrument)
  "an instrument")

(define-method
  (compile (unit <instrument>) n)
  (format #f "          instr ~a\n~a\n          endin\n"
          n
          (compile (normalize unit))))

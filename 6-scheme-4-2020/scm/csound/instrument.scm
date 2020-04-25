(define-module (csound instrument)
               #:export (insert patch plug normalize <instrument>)
               #:use-module (noisesmith clojure)
               #:use-module (csound instrument node)
               #:re-export (ht node))
(use-modules
  (csound compile)
  (oop goops))

(define-class
  <instrument> ()
  (synthesis-node-graph
    #:init-keyword #:graph
    #:init-form (ht)
    #:getter graph))

(define-method
  (get (i <instrument>) k)
  (get #h(#:graph (graph i)) k))

(define-method
  (write (i <instrument>) port)
  (display "[<instrument> :graph=" port)
  (write (graph i) port)
  (display "]" port))

(define-method
  (insert (i <instrument>) (t <top>) (n <node>))
  (let ((connections (conj (graph i) t n)))
    (make <instrument>
          #:graph connections)))

(define-method
  (insert (t <top>) (n <node>))
  (insert (make <instrument>) t n))

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
  (write (p <plug>) port)
  (display "[<plug> :node=" port)
  (write (node p) port)
  (display ", :slot=" port)
  (write (slot p) port)
  (display "]" port))

(define-method
  (equal? (p1 <plug>) (p2 <plug>))
  (and (equal? (node p1) (node p2))
       (equal? (slot p1) (slot p2))))

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
  "an instrument"
  ;; loop, emit/remove each node in the graph where all inputs are string/number,
  ;; update all nodes with input from emitted item with string of variable created
  ;; until nodes are empty or (error case) nodes are unresolvable
  )

(define-method
  (compile (unit <instrument>) n)
  (format #f "          instr ~a\n~a\n          endin\n"
          n
          (compile (normalize unit))))

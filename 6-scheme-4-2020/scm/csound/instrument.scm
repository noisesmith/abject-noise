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
  (get (i <instrument>) k)
  (get #h(#:graph (graph i)) k))

(define-method
  (write (i <instrument>) port)
  (display "[<instrument> :graph=" port)
  (write (graph i) port)
  (display "]" port))

(define-class
  ;; an individual node in an instrument graph
  <node> ()
  (compile-fn
    #:init-keyword #:compile-fn)
  (compilef-internal
    #:allocation #:virtual
    #:accessor compile-fn
    #:slot-ref (lambda (instance)
                 (slot-ref instance 'compile-fn))
    #:slot-set! (lambda (instance f)
                  (make <node>
                        #:compile-fn f
                        #:in (in instance)
                        #:out (out instance))))
  (in
    #:init-keyword #:in
    #:accessor in)
  (out
    #:init-keyword #:out
    #:accessor out))

(define-method
  (write (n <node>) port)
  (display "[<node> :in=" port)
  (write (in n) port)
  (display ", :out=" port)
  (write (out n) port)
  (display "]" port))

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

(define-method
  (write (p <ports>) port)
  (display "[<ports> :in=" port)
  (write (in p) port)
  (display ", :out=" port)
  (write (out p) port)
  (display "]" port))

(define (ports . slots)
  (apply make <ports> slots))

(define-method
  ;; TODO - will need a compilation function too
  (node (ports <ports>))
  (make <node>
        #:in (in ports)
        #:out (out ports)))

(define-method
  (get (n <node>) k)
  (get #h(#:in (in n) #:out (out n)) k))

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

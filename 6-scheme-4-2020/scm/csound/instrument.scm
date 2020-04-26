(define-module (csound instrument)
               #:export (insert patch plug normalize)
               #:use-module (noisesmith clojure)
               #:use-module (csound instrument node)
               #:re-export (ht node))
(use-modules
  (csound compile)
  (oop goops)
  (srfi srfi-1))

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
  (let ((connections (assj (graph i) t n)))
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
         (connected (assj (in target-entry) (slot output) input))
         (new-node (make <node>
                         #:in connected
                         #:out (out target-entry))))
    (make <instrument>
          #:graph (assj (graph i) (node output) new-node))))

(define (named graph k)
  (assj (get graph k)
        ;; TODO - this is a simplification, we need something that
        ;; understands rates and such...
        #:name ((comp symbol->string keyword->symbol) k)))

(define (ready-nodes i)
  (letrec ((g (graph i))
           (all-inputs-ready
             (lambda (inputs)
               (if (= inputs '())
                 #t
                 (let ((i (car inputs)))
                   (and (or (string? i)
                            (number? i))
                        (all-inputs-ready (cdr i)))))))
           (node-ready?
             (lambda (k)
               (let ((inputs (vals (in (get g k)))))
                 (all-inputs-ready inputs)))))
    (fold (lambda (k m)
            (if (not (node-ready? k))
              m
              (assj m k (named g k))))
          g
          (filter node-ready?
                  (keys g)))))

(define (update-input removed input-hash)
  (fold (lambda (kv updated)
          (let ((s (get (cdr kv) #:name)))
            (assj updated (car kv) s)))
        input-hash
        (seq removed)))

(define (update-inputs removed remaining)
  (fold (lambda (kv pruned)
          (let ((k (car kv))
                (v (cdr kv)))
            (assj pruned
                  (car kv)
                  (update-input removed (cdr kv)))))
        remaining
        (seq removed)))

(define-method
  (normalize (i <instrument>))
  ;; loop
  (if (empty? i)
    ;; until nodes are empty or (error case) nodes are unresolvable
    '()
    ;; find elements that are "ready" - no inputs aside from strings / numbers
    (let* ((ready (ready-nodes i))
           ;; emit/remove each node in the graph where all inputs are string/number,
           (remaining (apply disj (graph i) (keys ready)))
           ;; update all nodes with input from emitted item with string of variable created
           (pruned (update-inputs ready remaining)))
      (cons ready
            (normalize pruned)))))

(define-method
  (compile (unit <instrument>) n)
  (format #f "          instr ~a\n~a\n          endin\n"
          n
          (compile (normalize unit))))

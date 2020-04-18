(define-module (csound instrument)
               #:export (instrument))

(use-modules
  (ice-9 format)
  (ice-9 vlist)
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
    #:init-form (alist->vhash '())
    #:getter graph))

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
  (insert (i <instrument>) (n <top>) (p <ports>))
  (let* ((node (alist->vhash '()))
         (node (vhash-cons #:in (map list (in p)) node))
         (node (vhash-cons #:out (out p) node))
         (connections (vhash-cons n node (graph i))))
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

;(define-method
;  (connect (i <instrument>) (input <plug>) (output <plug>))
;  (let ((target-entry (vhash-assoc (graph i) (node output)))
;        (target-slot (vhash-assoc (cdr target-entry)))
;        (new-node (vhash-assoc (cdr target-entry)))
;        ))
;  (make <instrument>
;        #:graph (vhash-cons target-node target-connections (graph i))))

(define (normalize instrument)
  "an instrument")

(define-method
  (compile (unit <instrument>) n)
  (format #f "          instr ~a\n~a\n          endin\n"
          n
          (compile (normalize unit))))

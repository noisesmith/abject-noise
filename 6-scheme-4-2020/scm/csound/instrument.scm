(define-module (csound instrument)
               #:export (get-positional insert new-positional normalize patch)
               #:use-module (noisesmith clojure)
               #:use-module (csound compile)
               #:use-module (csound instrument node)
               #:use-module (csound instrument plug)
               #:re-export (ht ->node compile ->plug))
(use-modules
  (ice-9 match)
  (noisesmith debug)
  (oop goops)
  (srfi srfi-1)
  (srfi srfi-26))

(define debug (->catalog))

(define-class
  <instrument> ()
  (synthesis-node-graph
    #:init-keyword #:graph
    #:init-form (ht)
    #:getter graph)
  (positionals
    #:init-keyword #:positionals
    #:init-form '(ins start dur)
    #:getter positionals))

(define-method
  (assj (i <instrument>) k v)
  (cond ((equal? k #:graph)
         (make <instrument> #:graph v #:positionals (positionals i)))
        ((equal? k #:positionals)
         (make <instrument> #:positionals v #:graph (graph i)))
        (else i)))

(define-method
  (get (i <instrument>) k)
  (get #h(#:graph (graph i)) k))

(define-method
  (get (i <instrument>) k default-fn)
  (get #h(#:graph (graph i)) k default-fn))

(define-method
  (write (i <instrument>) port)
  (display "[<instrument> :graph=" port)
  (write (graph i) port)
  (display "]" port))

(define-method
  (empty? (i <instrument>))
  (empty? (graph i)))

(define-method
  (insert (i <instrument>) (t <top>) (n <node>))
  (let ((connections (assj (graph i) t n)))
    (make <instrument>
          #:graph connections)))

(define-method
  (insert (t <top>) (n <node>))
  (insert (make <instrument>) t n))

(define-method
  (patch (i <instrument>)
         (input <plug>)
         (output <plug>))
  (let* ((target-entry (get (graph i) (node output)))
         (connected (assj (in target-entry) (slot output) input))
         (new-node (assj target-entry #:in connected)))
    (make <instrument>
          #:graph (assj (graph i) (node output) new-node))))

(define-method
  (patch (i <instrument>) (input <number>) (output <plug>))
  (update-in i (list #:graph (node output) #:in)
             assj (slot output) input))

(define-method
  (patch (i <instrument>) (input <string>) (output <plug>))
  (update-in i (list #:graph (node output) #:in)
             assj (slot output) input))

(define-method
  (patch (i <instrument>) (target <keyword>) (plugs-keys <list>))
  (if (null? plugs-keys)
      i
      (-> i
          (patch (car plugs-keys)
                 (->plug target (cadr plugs-keys)))
          (patch target (cddr plugs-keys)))))

(define (inputs-ready? ready? _ v)
  (and ready?
       (or (string? v)
           (number? v))))

(define (split-ready-nodes g)
  (reduce-kv (match-lambda*
               (((ready not-ready) k v)
                (if (reduce-kv inputs-ready? #t (in v))
                    (list (assj ready k v) not-ready)
                    (list ready (assj not-ready k v)))))
             (list #h() #h())
             g))

(define (map-to-token node-key)
  (lambda (new-mappings output-key string-token)
    (assj new-mappings
          (->plug node-key output-key)
          string-token)))

(define (derive-input-map nodes)
  (reduce-kv (lambda (mapping node-key synth-node)
               (hmerge mapping
                       (reduce-kv (map-to-token node-key)
                                  (out synth-node))))
             nodes))

;; TODO - FIXME - maybe we want to find more than just keywords here
(define (free-keys expression-tree)
  (if (null? expression-tree)
      '()
      (append-map! (lambda (form)
                     (cond ((keyword? form)
                            (list form))
                           ((list? form)
                            (free-keys form))
                           (else '())))
                   expression-tree)))

(define (normalize-input expr positionals subs)
  (cond ((null? expr)
         "")
        ((keyword? expr)
         (get subs expr (constantly expr)))
        ((symbol? expr)
         (get positionals expr (constantly expr)))
        ((number? expr)
         (number->string expr))
        ;; all cases from here down expect a list
        ((and (list? expr)
              (null? (free-keys expr)))
         (fmt:format #f "(~a)"
                     (string-join (map (lambda (e)
                                         (normalize-input e positionals subs))
                                       (cdr expr))
                                  (fmt:format #f " ~a "
                                              (normalize-input (car expr)
                                                               positionals
                                                               subs)))))
        ;; TODO - FIXME - how to ensure input is normalized after mapping here?
        ((list? expr)
         (map normalize-input expr))))

(define (update-input input-map)
  (lambda (inputs tag input)
    ;; look up the plug, or leave as is
    (assj inputs tag
          (normalize-input input positionals input-map))))

(define (attach-ready-inputs ready positionals)
    (lambda (m k v)
      (assj m k
            (update-in v '(#:in)
                      (cut reduce-kv (update-input (derive-input-map ready) positionals)
                           <>)))))

(define (stringify-value tag)
  (lambda (m out-name prefix)
    (assj m out-name (string-append prefix (name tag) "_" (name out-name)))))

(define (name-outputs m k)
  (reduce-kv (stringify-value k) m))

(define (param n)
  (string-append "p" (number->string n)))

(define (new-positional instrument . ks)
  "Returns a new instrument with the positional arg registered."
  ;; can't use update-in here because get short-circuits to the nodes
  (assj instrument #:positionals
        (append (positionals instrument) ks)))

(define (get-positionals instrument)
  ;; create a mapping from named positional to index
  (car
    (fold (lambda (k acc)
            (let ((m (car acc))
                  (n (cdr acc)))
              (cons (assj m k n)
                    (1+ n))))
          (cons #h() 1)
          (positionals instrument))))

(define (tokenize-graph)
  "Creates strings for each node output.
  Turns each registered positional arg into a symbol lookup."
  (lambda (m k v)
    (assj m k
          (-> v
              (update-in '(#:out) name-outputs k)))))

(define (normalize graph positionals)
  ;; loop
  (if (empty? graph)
      ;; until nodes are empty or (error case) nodes are unresolvable
      '()
      (match-let (((ready remaining) (split-ready-nodes graph)))
                 (if (empty? ready)
                     (throw 'incomplete-compile #h(#:non-ready graph))
                     ;; emit/remove each node in the graph where all inputs are string/number,
                     (catch 'incomplete-compile
                            (cut let ((pruned (reduce-kv (attach-ready-inputs ready positionals)
                                                         remaining)))
                                 (cons ready
                                       (normalize pruned positionals)))
                            (lambda (_ . e)
                              (throw 'incomplete-compile (cons ready e))))))))

(define-method
  (compile (unit <instrument>) n)
  (catch 'incomplete-compile
         (cut format #f "          instr ~a\n~a          endin\n"
              n
              (->> (graph unit)
                   (reduce-kv (tokenize-graph))
                   (normalize (get-positionals unit))
                   (map vals)
                   (compile)))
         (lambda (_ . e)
           (string-append "no nodes ready! \n"
                          (call-with-output-string (lambda (p)
                                                     (write e p)))))))

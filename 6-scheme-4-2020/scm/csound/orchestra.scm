(define-module (csound orchestra)
               #:export (=-expr gendyx insert-curve interject outs ->ports tab:a timeinsts)
               #:use-module (csound instrument)
               #:use-module ((csound instrument plug)
                                    #:renamer (symbol-prefix-proc 'plug:))
               #:use-module ((ice-9 format)
                             #:select (format)
                             #:renamer (symbol-prefix-proc 'fmt:))
               #:use-module ((noisesmith clojure))
               #:use-module ((noisesmith debug)
                             #:select (->catalog)
                             #:renamer (symbol-prefix-proc 'dbg:))
               #:use-module ((srfi srfi-1)))

(define debug (dbg:->catalog))

(define (in-ht in-list)
  (fold (lambda (k h)
          (assj h k #f))
        #h()
        in-list))

(define (format outs name ins)
  (fmt:format #f "~15a ~a ~a\n"
              (string-join (map compile outs) ", ")
              name
              (string-join (map compile ins) ", ")))

(define (->ports outs name ins . params)
  (-> (->node #:in (in-ht ins)
              #:out (apply ht outs)
              #:formatter (lambda (i o)
                            (let ((out-keys (key-list outs)))
                              (format (map (lambda (k) (get o k)) out-keys)
                                      name
                                      (map (lambda (k) (get i k)) ins)))))
      (update-in '(#:in)
                 (if (eq? params '())
                     identity
                     (lambda (inputs)
                       (hmerge inputs (car params)))))))

(define gendyx
  (->ports '(#:sig "a")
           "gendyx"
           '(#:amp #:ampdist #:durdist
             #:adpar #:ddpar #:minfreq #:maxfreq
             #:ampscl #:durscl #:curveup #:curvedown #:initcps #:num)))

(define outs
  (->ports '()
           "outs"
           '(#:l #:r)))

(define tab:a
  (->ports '(#:sig "a")
           "table3"
           '(#:index #:fn #:mode #:off #:wrap)
           #h(#:mode 1
              #:off 0
              #:wrap 1)))

(define timeinsts
  (->ports '(#:t "k")
           "timeinsts"
           '()))

(define (format-to-infix expr subs)
  (cond ((eq? expr '())
         "")
        ((keyword? expr)
         (get subs expr))
        ((symbol? expr)
         (symbol->string expr))
        ((number? expr)
         (number->string expr))
        (else
          (fmt:format #f "(~a)"
                      (string-join (map (lambda (e)
                                          (format-to-infix e subs))
                                        (cdr expr))
                                   (fmt:format #f " ~a "
                                               (format-to-infix (car expr)
                                                                subs)))))))

(define (free-keys expression-tree)
  (if (eq? expression-tree '())
      expression-tree
      (append-map! (lambda (form)
                     (cond ((keyword? form)
                            (list form))
                           ((list? form)
                            (free-keys form))
                           (else '())))
                   expression-tree)))

(define (=-expr name/type expression-list)
  (let ((args (free-keys expression-list)))
    (->node #:in (in-ht args)
            #:out (apply ht name/type)
            #:formatter (lambda (i o)
                          (fmt:format #f "~15a = ~a\n"
                                      (get o (car name/type))
                                      (format-to-infix expression-list i))))))
(define timer
  timeinsts)

(define phase
  (=-expr '(#:v "k")
          '(/ #:time p3)))

(define (interject instrument target-plug in-plug out-plug)
  "steal the source from some input and process it, providing a new input in its place"
  (let* ((target-node (plug:node target-plug))
         (target-input (plug:slot target-plug))
         (target (get-in instrument `(#:graph ,target-node #:in ,target-input))))
    (-> instrument
        (patch target in-plug)
        (patch out-plug target-node))))

(define (insert-curve ins table-number out-key out-bind min-val max-val)
  "`out-key` will be mapped to a node producing a value ranging form min-val to max-val,
  following the curve of table table-number.

  Inserts multiple generic/reused nodes:
    - #:curve_phase - a value that sweeps from 0 to 1 across the duration of the note.
    - #:curve_timer - a value that tracks the ltime in seconds since the note started.
    - curve_<table-number> - assigned to a read of a table across the duration of the note.

  specific:
    - out-key a target consuming <out-key>_curve and producing the value to use."
  (let ((curve-key (keyword "curve_" (number->string table-number))))
    (-> ins
        (insert out-key (=-expr out-bind
                                `(+ ,min-val
                                    (* (- ,max-val ,min-val))
                                       #:x)))
        (insert curve-key tab:a)
        ;; the hookups
        (patch (->plug curve-key #:sig)
               (->plug out-key #:x))
        (patch curve-key
               (list (->plug #:curve_phase #:v) #:index
                     table-number #:fn))
        ;; these last two only need to exist once, they are "generic"
        ;; this function is idempotent over #:curve_phase and #:curve_timer
        (insert #:curve_phase phase)
        (insert #:curve_timer timer)
        (patch (->plug #:curve_timer #:t)
               (->plug #:curve_phase #:time)))))


(define-module (csound orchestra)
               #:export (=-expr gendyx insert-curve interject outs ->ports tab:a timeinsts)
               #:use-module (csound instrument)
               #:use-module ((ice-9 format)
                             #:select (format)
                             #:renamer (symbol-prefix-proc 'fmt:))
               #:use-module ((noisesmith clojure))
               #:use-module ((srfi srfi-1))
               #:use-module ((noisesmith debug)
                             #:select (->catalog)
                             #:renamer (symbol-prefix-proc 'dbg:)))

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

(define (=-expr name/type expression-list args)
  (->node #:in (in-ht args)
          #:out (apply ht name/type)
          #:formatter (lambda (i o)
                        (fmt:format #f "~15a = ~a\n"
                                    (get o (car name/type))
                                    (format-to-infix expression-list i)))))
(define timer
  timeinsts)

(define phase
  (=-expr '(#:v "k")
          '(/ #:time p3)
          '(#:time)))


(define (insert-curve ins table-number out-key out-node)
  (let ((curve-key (keyword out-key "_curve")))
    (-> ins
        (insert out-key out-node)
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

(define (interject instrument target-plug in-plug out-plug)
  "steal the source from some input and process it, providing a new input in its place"
  (let* ((target-node (node target-plug))
         (target-input (slot target-plug))
         (target (get-in instrument `(#:graph ,target-node #:in ,target-input))))
    (-> instrument
        (patch target in-plug)
        (patch out-plug target-node))))

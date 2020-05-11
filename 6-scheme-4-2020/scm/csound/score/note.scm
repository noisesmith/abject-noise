(define-module (csound score note)
               #:export (note)
               #:use-module (csound csound)
               #:use-module (ice-9 format)
               #:use-module (noisesmith clojure)
               #:use-module (oop goops)
               #:use-module (srfi srfi-1))

(define-class
  <note> ()
  (description
    #:init-keyword #:description
    #:init-value "a note"
    #:getter description)
  (instrument
    #:init-keyword #:instrument
    #:getter instrument)
  (args
    #:init-keyword #:args
    #:getter args)
  (arg-keyseq
    #:init-keyword #:arg-keyseq
    #:getter arg-keyseq))

(define-method
  (emit (note <note>) time duration amplitude)
  (format #t "i~a ~a ~a ~a"
          (instrument note)
          time
          duration
          amplitude)
  (for-each (lambda (p)
              (format #t " ~a" (get (args note) p)))
            (arg-keyseq note))
  (format #t " ; ~a\n" (description note)))

(define (note desc instrument . args)
  (make <note> #:description desc #:instrument instrument
        #:args (apply ht args)
        #:arg-keyseq (key-list args)))

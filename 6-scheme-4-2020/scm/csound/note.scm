(define-module (csound note)
               #:export (<note>))

(use-modules
  (ice-9 format)
  (oop goops)
  (csound csound))

(define-class
  <note> ()
  (description
    #:init-keyword #:description
    #:init-value "a note"
    #:getter description)
  (instrument
    #:init-keyword #:instrument
    #:getter instrument)
  (max-hz
    #:init-keyword #:max-hz
    #:getter max-hz)
  (min-hz
    #:init-keyword #:min-hz
    #:getter min-hz))

(define-method
  (emit (note <note>) time duration amplitude)
  (format #t "i~a ~a ~a ~a"
          (instrument note)
          time
          duration
          amplitude)
  (format #t " ~a ~a"
          (min-hz note)
          (max-hz note))
  (format #t " ; ~a\n" (description note)))

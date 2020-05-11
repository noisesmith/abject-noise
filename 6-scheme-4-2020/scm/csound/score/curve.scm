(define-module (csound score curve)
               #:export (curve curve-shape bp))

(use-modules
  (oop goops)
  (ice-9 format)
  (csound csound))

(define-class
  <curve> ()
  (description
    #:init-keyword #:description
    #:init-value "a curve"
    #:getter description)
  (table-number
    #:init-keyword #:table-number
    #:getter table-number)
  (size
    #:init-keyword #:size
    #:getter size)
  (data
    #:init-keyword #:data
    #:getter data))

(define-class
  <breakpoint> ()
  (duration
    #:init-keyword #:duration
    #:getter duration)
  (value
    #:init-keyword #:value
    #:getter value))

(define (bp d v)
  (make <breakpoint>
        #:duration d
        #:value v))

(define (curve-shape init . breaks)
  (make <curve-data>
        #:init init
        #:breakpoints breaks))

(define-class
  <curve-data> ()
  (init
    #:init-keyword #:init
    #:getter init)
  (breakpoints
    #:init-keyword #:breakpoints
    #:getter breakpoints))

(define-method
  (emit (curve <curve>))
  (let* ((bps (breakpoints (data curve)))
         (dt (apply + 0.0 (map duration bps)))
         (norm (/ (size curve) dt)))
    (format #t "\nf ~a 0 ~a 5 ~a"
            (table-number curve)
            (size curve)
            (init (data curve)))
    (map (lambda (bp)
           (format #t " ~a ~a"
                   (inexact->exact (floor (* norm (duration bp))))
                   (value bp)))
         bps)
    (format #t " ; ~a\n" (description curve))))

(define (curve desc . args)
  (apply make <curve> #:description desc
         args))

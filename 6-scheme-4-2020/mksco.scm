#!/usr/local/bin/guile
!#

(use-modules (oop goops)
             (ice-9 format))

(define-class
  <note> ()
  (description
    #:init-keyword #:description
    #:init-value "a note"
    #:getter -description)
  (instrument
    #:init-keyword #:instrument
    #:getter -instrument)
  (max-hz
    #:init-keyword #:max-hz
    #:getter -max-hz)
  (min-hz
    #:init-keyword #:min-hz
    #:getter -min-hz))

(define-method
  (emit (note <note>) time duration amplitude)
  (format #t "i~a ~a ~a ~a"
          (-instrument note)
          time
          duration
          amplitude)
  (format #t " ~a ~a"
          (-min-hz note)
          (-max-hz note))
  (format #t " ; ~a\n" (-description note)))

(define-class
  <curve> ()
  (description
    #:init-keyword #:description
    #:init-value "a curve"
    #:getter -description)
  (table-number
    #:init-keyword #:table-number
    #:getter -table-number)
  (size
    #:init-keyword #:size
    #:getter -size)
  (data
    #:init-keyword #:data
    #:getter -data))

(define-class
  <breakpoint> ()
  (duration
    #:init-keyword #:duration
    #:getter -duration)
  (value
    #:init-keyword #:value
    #:getter -value))

(define (bp d v)
  (make <breakpoint>
        #:duration d
        #:value v))

(define-class
  <curve-data> ()
  (init
    #:init-keyword #:init
    #:getter -init)
  (breakpoints
    #:init-keyword #:breakpoints
    #:getter -breakpoints))

(define-method
  (emit (curve <curve>))
  (let* ((bps (-breakpoints (-data curve)))
         (dt (apply + 0.0 (map -duration bps)))
         (norm (/ (-size curve) dt)))
    (format #t "\nf ~a 0 ~a 5 ~a"
            (-table-number curve)
            (-size curve)
            (-init (-data curve)))
    (map (lambda (bp)
           (format #t " ~a ~a"
                   (inexact->exact (floor (* norm (-duration bp))))
                   (-value bp)))
         bps)
    (format #t " ; ~a\n" (-description curve))))

;; Ideas:
; create tables for curves for each K param (and read from them in the instrument)
; paramaterize more controls
; filter (resonant filters...)

(define curve1
  (make <curve>
        #:description "a simple curve"
        #:table-number 1
        #:size 1024
        #:data (make <curve-data>
                     #:init 0.1
                     #:breakpoints (list (bp 1 1.0)
                                         (bp 1 10.0)
                                         (bp 5 0.1)))))

(define e1
  (make <note>
        #:description "test event"
        #:instrument 1
        #:min-hz 1.1
        #:max-hz 1))

(define (gen-sco)
   (emit curve1)
   (emit e1 0 10 0.1))

(gen-sco)

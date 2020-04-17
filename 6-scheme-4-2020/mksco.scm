#!/usr/local/bin/guile
!#

(add-to-load-path (string-append
                    (dirname (current-filename))
                    "/scm"))

(use-modules (oop goops)
             (csound csound)
             (csound note)
             (csound curve))

;; Ideas:
; create tables for curves for each K param (and read from them in the instrument)
; paramaterize more controls
; filter (resonant filters...)
; https://www.gnu.org/software/guile/manual/html_node/Atomics.html
; ffi (jack, csound, threads etc.)

(define curve1
  (make <curve>
        #:description "a simple curve"
        #:table-number 1
        #:size 1024
        #:data (curve-shape 0.1
                            (bp 1 1.0)
                            (bp 1 10.0)
                            (bp 5 0.1))))

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

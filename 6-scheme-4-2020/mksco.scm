#!/usr/local/bin/guile
!#

(use-modules (srfi srfi-9)
             (ice-9 format))

(define-record-type <note>
  (make-note description
             instrument time duration amplitude
             min-hz max-hz)
  note?
  (description note-description set-note-description!)
  (instrument note-instrument set-note-instrument!)
  (time note-time set-note-time!)
  (duration note-duration set-note-duration!)
  (amplitude note-amplitude set-note-amplitude!)
  (max-hz note-max-hz set-note-max-hz!)
  (min-hz note-min-hz set-note-min-hz!))

(define (emit-note note)
  (format #t "i~a ~a ~a ~a  "
          (note-instrument note)
          (note-time note)
          (note-duration note)
          (note-amplitude note))
  (format #t "~a ~a "
          (note-min-hz note)
          (note-max-hz note))
  (format #t "; ~a\n" (note-description note)))

;; Ideas:
; create tables for curves for each K param (and read from them in the instrument)
; paramaterize more controls
; filter (resonant filters...)
(define e1
  (let ((max-hz 1)
        (min-hz 1.1))
    (make-note "test event" 1 0 10 0.1
               min-hz
               max-hz)))

(define (gen-args)
   (emit-note e1))

(gen-args)

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

(define-record-type <curve>
  (make-curve description
              n size data)
  curve?
  (description curve-description)
  (n curve-n)
  (size curve-size)
  (data curve-data))

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

(define (emit-curve curve)
  ;; a breakpoint is a (curve-time . final-value) cons pair
  ;; the total duration will be normalized from total curve times to the size
  (let* ((data (curve-data curve))
         (init (car data))
         (breakpoints (cdr data))
         (total (apply + 0.0 (map car breakpoints)))
         (norm (/ (curve-size curve) total)))
    (format #t "\nf ~a 0 ~a 5 ~a"
            (curve-n curve) (curve-size curve) init)
    (map (lambda (bp)
           (format #t " ~a ~a"
                   (inexact->exact (floor (* norm (car bp))))
                   (cdr bp)))
         breakpoints)
    (format #t " ; ~a\n" (curve-description curve))))

;; Ideas:
; create tables for curves for each K param (and read from them in the instrument)
; paramaterize more controls
; filter (resonant filters...)

(define curve1
  (make-curve "a simple curve" 1 1024 '(0.1 (1 . 1.0) (5 . 0.1))))

(define e1
  (let ((max-hz 1)
        (min-hz 1.1))
    (make-note "test event" 1 0 10 0.1
               min-hz
               max-hz)))

(define (gen-sco)
   (emit-curve curve1)
   (emit-note e1))

(gen-sco)

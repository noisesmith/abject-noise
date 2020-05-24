(define-module (composition one)
               #:export (gen-orc gen-sco))

(use-modules (csound csound)
             ((csound instrument)
              #:select (compile patch ->plug insert new-positional)
              #:renamer (symbol-prefix-proc 'ins:))
             ((csound orchestra)
              #:renamer (symbol-prefix-proc 'orc:))
             (csound score curve)
             (csound score note)
             (noisesmith clojure))

(define gendy-index 1)
(define min-freq-table 1)
(define max-freq-table 2)

;; orchestra stuff
(define (headers params)
  (format #f "sr      = ~a\nksmps   = ~a\nnchnls  = ~a\n0dbfs   = ~a\n"
          (get params #:sr (constantly 48000))
          (get params #:ksmps (constantly 1))
          (get params #:nchnls (constantly 2))
          (get params #:0dbfs (constantly 1))))

(define gendyx
  (update-in orc:gendyx '(#:in) hmerge
             #h(#:ampdist 2 #:adpar 0.4 #:ampscl 1.0
                #:durdist 3 #:ddpar 0.2 #:durscl 1.0
                #:curveup 2 #:curvedown 0.1
                #:initcps 24 #:num 12)))

(define gendy-instrument
  (-> (ins:insert #:gendyx gendyx)
      (ins:new-positional 'amp 'min-freq 'max-freq)
      (orc:insert-curve min-freq-table #:minfreq '(#:v "k") '(* min-freq 0.9) 'min-freq)
      (orc:insert-curve max-freq-table #:maxfreq '(#:v "k") '(* max-freq 0.9) 'max-freq)
      (ins:patch (ins:->plug #:minfreq #:v)
                 (ins:->plug #:gendyx #:minfreq))
      (ins:patch (ins:->plug #:maxfreq #:v)
                 (ins:->plug #:gendyx #:maxfreq))
      (ins:insert #:outs orc:outs)
      (ins:patch #:gendyx '("ampdbfs(p4)" #:amp))
      (ins:patch #:outs (list (ins:->plug #:gendyx #:sig) #:l
                              (ins:->plug #:gendyx #:sig) #:r))))

(define (gen-orc . args)
  (display (headers #h()))
  (display "\n")
  (display (ins:compile gendy-instrument gendy-index)))

;; score stuff
(define min-freq-curve
  (curve "curve for minfreq"
         #:table-number min-freq-table
         #:size 1024
         #:data (curve-shape 0.1
                             (bp 1 1.0)
                             (bp 1 10.0)
                             (bp 5 0.1))))

(define max-freq-curve
  (curve "curve for maxfreq"
         #:table-number max-freq-table
         #:size 1024
         #:data (curve-shape 0.1
                             (bp 5 1.0)
                             (bp 1 10.0)
                             (bp 1 0.1))))

(define gendy-long-drone
  (note "extended gendy low frequency for accompaniment" gendy-index
        #:min-hz 20.1
        #:max-hz 30.3))

(define (gendy-long-melodies count start dur amp)
  (if (= count 0)
      '()
      (let* ((s start)
             (d (/ dur count))
             (ns (+ s d))
             (nd (- dur d))
             (df (+ 2.9 (* 0.0002 (random 3000))))
             (sf (+ 0.8 (* 0.0002 (random 1000))))
             (mnh (+ 900 (* 0.113 (random 1000))))
             (mxh (+ 1000 (* 0.15 (random 1010)))))
        (cons (list (note "granulated gendy tone" gendy-index
                          #:min-hz mnh
                          #:max-hz mxh)
                    (* s sf)
                    (* d df) amp)
              (gendy-long-melodies (- count 1) ns nd amp)))))

(define (gen-sco . args)
   (emit min-freq-curve)
   (emit max-freq-curve)
   (emit gendy-long-drone
         0 1000 5)
   (for-each (lambda (e) (apply emit e))
             (gendy-long-melodies 5000 0 1000.0 0)))

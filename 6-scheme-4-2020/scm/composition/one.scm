(define-module (composition one)
               #:export (gen-orc gen-sco))

(use-modules (csound csound)
             ((csound instrument)
              #:select (compile patch ->plug insert)
              #:renamer (symbol-prefix-proc 'ins:))
             ((csound orchestra)
              #:renamer (symbol-prefix-proc 'orc:))
             (csound score curve)
             (csound score note)
             (noisesmith clojure))

(define gendy-index 1)
(define min-freq-table 1)

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

(define minfreq
  (orc:=-expr '(#:v "k")
              '(+ (* p5 0.9)
                  (* #:x p5 0.1))
              '(#:x)))

(define gendy-instrument
  (-> (ins:insert #:gendyx gendyx)
      (orc:insert-curve min-freq-table #:minfreq minfreq)
      (ins:patch (ins:->plug #:minfreq #:v)
                 (ins:->plug #:gendyx #:minfreq))
      (ins:insert #:outs orc:outs)
      (ins:patch #:gendyx '("ampdbfs(p4)" #:amp
                            ;"p5" #:minfreq
                            "p6" #:maxfreq))
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

(define gendy-long-drone
  (note "extended gendy for accompaniment" gendy-index
        #:min-hz 100.1
        #:max-hz 100.3))

(define (gen-sco . args)
   (emit min-freq-curve)
   (emit gendy-long-drone
         0 1000 -2))

(define-module (composition one)
               #:export (gen-orc gen-sco))

(use-modules (csound csound)
             (csound curve)
             ((csound instrument)
              #:select (compile patch ->plug insert)
              #:renamer (symbol-prefix-proc 'ins:))
             (csound note)
             ((csound orchestra)
              #:renamer (symbol-prefix-proc 'orc:))
             (noisesmith clojure))

(define gendy-index 1)
(define curve-table 1)

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

(define timer
  orc:timeinsts)

(define phase
  (orc:=-expr '(#:v "k")
              '(/ #:time p3)
              '(#:time)))

(define freq-curve
  (update-in orc:tab:a '(#:in) hmerge
             #h(#:fn curve-table)))

(define minfreq
  (orc:=-expr '(#:minfreq "k")
              '(+ (* p5 0.9)
                  (* #:x p5 0.1))
              '(#:x)))

(define (insert-curve ins)
  (-> ins
      (ins:insert #:minfreq minfreq)
      (ins:insert #:freq-curve freq-curve)
      (ins:insert #:phase phase)
      (ins:insert #:timer timer)
      (ins:patch (ins:->plug #:freq-curve #:sig)
                 (ins:->plug #:minfreq #:x))
      (ins:patch (ins:->plug #:phase #:v)
                 (ins:->plug #:freq-curve #:index))
      (ins:patch (ins:->plug #:timer #:t)
                 (ins:->plug #:phase #:time))))

(define gendy-instrument
  (-> (ins:insert #:gendyx gendyx)
      (insert-curve)
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
(define curve1
  (curve "a simple curve"
         #:table-number curve-table
         #:size 1024
         #:data (curve-shape 0.1
                             (bp 1 1.0)
                             (bp 1 10.0)
                             (bp 5 0.1))))

(define e1
  (note "test event"
        #:instrument gendy-index
        #:min-hz 100.1
        #:max-hz 100.3))

(define (gen-sco . args)
   (emit curve1)
   (emit e1 0 10 -5))

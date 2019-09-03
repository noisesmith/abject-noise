(fn sco-line
  [p +t *d al ar]
  (print "i" p.instr
         (+ p.t +t)
         (* p.d *d)
         (+ p.al al)
         (+ p.ar ar)
         p.mod1
         p.car1
         p.idx
         p.car
         p.idx1
         p.gain))

(local defaults
 {:instr 1
  :t 0
  :d 1
  :al 0
  :ar 0})


(fn with-defaults
  [m]
  (each [k v (pairs defaults)]
    (tset m k v))
  m)

(local params
  (with-defaults
    {:mod1 118.040092
     :car1 223.830730
     :idx  122.494429
     :car    0.000000
     :idx1 278.396428
     :gain   3.589744}))

(local whine
  (with-defaults
    {:mod1 6280.623674
     :car1 1084.409809
     :idx  4877.505600
     :car  9040.089369
     :idx1  583.518922
     :gain   21.794871}))

(local whine2
  (with-defaults
    {:mod1 7182.627916
     :car1  877.951008
     :idx  3017.817438
     :car  3006.681472
     :idx1  518.930972
     :gain    6.474840}))

(fn buzz-table
  []
  "f 1 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05")

(fn ftables
  []
  (print (buzz-table)))

(ftables)

(sco-line whine 0 20 -10 -10)
(sco-line params 10 10 -10 -15)
(sco-line whine2 14 10 -10 -10)

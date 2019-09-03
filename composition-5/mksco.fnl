(fn sco-line
  [p +t *d al ar]
  (print "i" p.instr
         (+ p.t +t)
         (* p.d *d)
         (+ p.al al)
         (+ p.ar ar)
         p.amod1
         p.amod1
         p.acar1
         p.aidx
         p.acar
         p.aidx1
         p.again))

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
   {:amod1 118.040092
    :acar1 223.830730
    :aidx  122.494429
    :acar    0.000000
    :aidx1 278.396428
    :again   3.589744}))

(local whine
  (with-defaults
    {:amod1 6280.623674
     :acar1 1084.409809
     :aidx  4877.505600
     :acar  9040.089369
     :aidx1 583.518922
     :again 21.794871}))

(fn buzz-table
  []
  "f 1 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05")

(fn ftables
  []
  (print (buzz-table)))

(ftables)

(sco-line whine 0 20 -10 -10)
(sco-line params 10 10 -10 -15)

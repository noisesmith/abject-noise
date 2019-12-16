(fn set-db
  [t n]
  (.. "i 2 " t " 0 " n))

(fn event
  [params]
  (table.concat ["i"
                 params.base.instr
                 (+ params.base.t params.+t)
                 (* params.base.d params.*d)
                 (+ params.base.al params.al)
                 (+ params.base.ar params.ar)
                 params.base.mod1
                 params.base.car1
                 params.base.idx
                 params.base.car
                 params.base.idx1
                 params.base.gain]
                " "))

(local defaults
 {:instr 1
  :t 0
  :d 1
  :al 0
  :ar 0})


(fn with-defaults
  [m]
  (let [result {}]
    (each [k v (pairs defaults)]
          (tset result k v))
    (each [k v (pairs m)]
          (tset result k v))
    result))

(fn tupdate
  [m up]
  (local t {})
  (each [k v (pairs m)]
    (let [updatef (. up k)]
      (if updatef
        (tset t k (updatef v k m t))
        (tset t k v))))
  t)

(fn rnd
  [low high]
  (let [offset (or low -1)
        maximum (or high 1)
        scale (- maximum offset)
        r (math.random)]
    (+ offset
       (* scale r r r))))

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
  (buzz-table))

(fn elaborate
  [p opts]
  (let [deach (/ opts.duration opts.n)
        res []]
    (var active p)
    (for [i 1 opts.n]
      (let [d (rnd 0.1 deach)
            t (+ opts.start
                 (* i deach)
                 (rnd (- d) d))
            wiggle (fn [x]
                     (rnd (* x 0.9)
                          (* x (/ 1 0.9))))
            parameters (tupdate active
                                {:mod1 wiggle
                                 :car1 wiggle
                                 :idx wiggle
                                 :car wiggle
                                 :idx1 wiggle
                                 :gain wiggle})]
        (if opts.drunk (set active parameters))
        (table.insert res
                      {:base parameters
                       :+t t
                       :*d (* d opts.density)
                       :al (rnd -20 -5)
                       :ar (rnd -20 -5)})))
    res))

(fn print-each
  [t]
  (each [i v (ipairs t)]
        (if (= (type v) :table)
          (print-each v)
          (print v))))

(fn solo
  [m]
  (let [queue (fn [] 0)
        m- (tupdate m {:+t queue})]
    (tset m- :base (tupdate m.base {:t queue}))
    m-))

(fn mute
  [...]
  "")

(fn mp
  [f t]
  (let [res []]
    (each [_ v (ipairs t)]
          (table.insert res (f v)))
    res))

(fn main
  []
  (let [library {:ft (ftables)
                 :db (set-db 0 -20)
                 :whine (event {:base whine :+t 0 :*d 20 :al -10 :ar -10})
                 :basic (event {:base params :+t 10 :*d 10 :al -10 :ar -15})
                 :whine2 (event {:base whine2 :+t 14 :*d 10 :al -10 :ar -10})
                 :chorus (elaborate whine {:start 5
                                           :duration 30
                                           :n 800
                                           :density 12.3})}
        composition [library.db
                     library.ft
                     library.whine
                     library.basic
                     library.whine2
                     (mp event library.chorus)]
        ;; the following goes wrong - the drunken walk shrinks a value
        ;; down to zero...
        test-data [library.ft
                   library.db
                   (mp event
                       (elaborate whine {:start 0
                                         :duration 1000
                                         :n 2400
                                         :density 12.3
                                         :drunk true}))]]
    ;(print-each [library.ft (event (solo (. library.chorus 6)))])
    ;(print-each test-data)
    (print-each composition)))

(main)

;; (local fennel (require :fennel))
;; (local s (fennel.dofile "mksco.fnl"))

{:print-each print-each
 :set-db set-db
 :defaults defaults
 :with-defaults with-defaults
 :tupdate tupdate
 :rnd rnd
 :params params
 :whine whine
 :whine2 whine2
 :buzz-table buzz-table
 :ftables ftables
 :elaborate elaborate}

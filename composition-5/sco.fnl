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

{:set-db set-db
 :event event
 :defaults defaults
 :with-defaults with-defaults
 :tupdate tupdate
 :rnd rnd
 :elaborate elaborate
 :print-each print-each
 :solo solo
 :mute mute
 :mp mp}

(ns noisesmith.generative)

(def calc-scale
  (memoize (fn [upper-limit slope]
             (/ upper-limit
                (Math/pow upper-limit slope)))))

(def not-zero (Math/nextUp 0.0))

(defn curve
  ([a b]
   (curve a b nil))
  ([a b slope]
   (curve {:mn (min a b) :mx (max a b) :slope slope}))
  ([{:keys [mn mx slope]}]
   (let [mn (or (some-> mn (double))
                0.0)
         mx (or (some-> mx (double))
                1.0)
         upper-limit (- mx mn)
         lower-limit mn
         slope (or (some-> slope (double))
                   Math/E)
         scale (calc-scale upper-limit slope)]
     (+ lower-limit
        (* scale
           (Math/pow (+ not-zero
                        (rand upper-limit))
                     slope))))))

(defn translate
  [[a b] [x y] term]
  (let [lowest-output x
        lowest-input a
        in-range (- b a)
        out-range (- y x)
        scale (/ out-range in-range)]
    (-> term
        (- lowest-input)
        (* scale)
        (+ lowest-output))))

(defmacro locals
  []
  (into {}
        (map (juxt keyword identity))
        (keys &env)))

(defn interpolate
  "given a `sorted-set-by first` of time/value pairs,
  linearly interpolates between values for a given time"
  [table x]
  {:pre [(<= (ffirst table) x)
         (<= x (first (last table)))]}
  (let [lookup [x nil]
        lower (first (rsubseq table <= lookup))
        higher (first (subseq table >= lookup))]
    (let [low-distance (- x (first lower))
          high-distance (- (first higher) x)]
      (cond (zero? low-distance) (second lower)
            (zero? high-distance) (second higher)
            :else
            (let [error (+ low-distance high-distance)
                  low-factor (/ high-distance
                                error)
                  high-factor (/ low-distance
                                 error)]
              (+ (* low-factor (double (second lower)))
                 (* high-factor (double (second higher)))))))))

(defn ->table
  [pairs]
  (apply sorted-set-by
         (fn [[a _] [b _]] (compare a b))
         pairs))

(defn scale
  [table n]
  (into (empty table)
        (map (fn [[t v]]
               [t (* v n)]))
        table))

(defn offset
  [table n]
  (into (empty table)
        (map (fn [[t v]]
               [t (+ n v)]))
        table))

(def smple
  (->table [[0 0]
            [10 1]
            [20 0]]))

;; TODO - four point interpolation

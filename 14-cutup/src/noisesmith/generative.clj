(ns noisesmith.generative)

(def calc-scale
  (memoize (fn [upper-limit slope]
             (/ upper-limit
                (Math/pow upper-limit slope)))))

(def not-zero (Math/nextUp 0.0))

(defn curve
  [{:keys [mn mx slope]}]
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
                    slope)))))

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

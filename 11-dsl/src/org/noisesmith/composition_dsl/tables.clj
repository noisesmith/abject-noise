(ns org.noisesmith.composition-dsl.tables)

;;;* dsl for generating and editing csound function tables
;; ** what clojure data format(s) should be supported?
;  *** some sort of "breakpoint" list, where the breakpoint can describe
;  *** things like a curve type as it progresses to the next value?
;  *** alternating values and curve types?
;  *** polynomial or function of time to value?
;  *** recursive generator?
;; ** what operations should be supported:
;  *** composition of operators over curves (like pipeline processing)
;  *** compression or expansion or shift of shapes (in magnitude or time)
;  *** insertion of new values into a curve by sum, replacement, mult, etc.
;  *** multi dimensional curves?
;; ** what gets emitted to csound?
;  *** GEN-02 literal inline data

; each parameter has a "curve" object
; each curve can be chained with modification layers
;   * convolve parameter against some f and another curve
;   eg. add to parameter, multiply parameter

(defmulti realize :curve)

(defmethod realize :default
  [{:keys [value]} _t]
  value)

(defmethod realize :line
  [{:keys [from to]} t]
  (+ from
     (* t
        (- to from))))

(defmethod realize :segmented
  [{:keys [breakpoints len] t}]
  (let [l (or len (apply + (map second breakpoints)))
        ts (* t l)]
    (loop [[[v steps] & bps] breakpoints
           pos 0]
      (if (>= pos ts)
        v
        (recur bps (+ pos steps))))))

;; TODO - there's a way to make this much simpler with linear algebra
;; TODO - there's a way to do a binary search rather than linear
;; TODO - there's a way to do this faster when realizing all values of t
(defmethod realize :ramped
  [{:keys [breakpoints len] t}]
  (let [l (or len (apply + (map second breakpoints)))
        ts (* t l)
        rescale (/ (double l))]
    (loop [[[v steps] & [[next-v _] :as bps]] breakpoints
           pos 0]
      (let [next-position (+ pos steps)]
        (if (>= next-position
                ts)
          (let [reach (- ts pos)
                fraction (* (/ l) reach)]
            ;; traversing linearly from v
            (+ v
               ;; by a factor rescaled from length of bps total
               ;; to 0-1 range of t
               (* rescale
                  ;; reflecting how far we've moved proportionally from v to next-v
                  reach
                  ;; with maximum value being epsilon below next-v
                  (- next-v v))))
          (recur bps next-position))))))

(defn interpolate
  [index [v1 step1] [v2 step2]]
  (let [step (- step2 step1)
        placement (/ (- index step1)
                     step)]
    (+ v1
       (* placement v2))))

(defn prev-and-next
  [index breakpoints]
  (let [pairs (partition 2 1 breakpoints)]
    (-> pairs
        (drop-while #(<= (second %)
                         index))
        first)))

(defmethod realize :ramped2
  [{:keys [breakpoints len] t}]
  (let [l (or len (apply + (map second breakpoints)))
        ts (* t l)]
    (apply interpolate ts (prev-and-next ts breakpoints))))

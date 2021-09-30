(ns org.noisesmith.composition-tool)

;; events are the building blocks of a system
;; this system uses the properties of events as engaged by human listeners
;; this engagement is encouraged by providing small frustrations that lead to rewards
;; as those frustrations are resolved

;; this uses an adversarial model, but its goal is cooperative - a shared flow
;; between the designer (myself) and a listener

;; aesthetically, I'm looking for "music making machine that is made more interesting
;; to listen to by the fact that it almost or mostly works"
;; intentional flaws in the system are present to help the listener understand
;; on some intuitive level if not conscious, the mechanisms in play

(def attack
  {:reach 1
   :lead 1
   :stall 1
   :accuracy 0.5
   :power 1
   ;; -1 down / 0 middle / 1 up
   :x-position 0
   :x-direction 0
   :y-position 0
   :y-direction 1})

(def body
  {:x-position 0
   :x-direction 0
   :y-position 0
   :y-direction 1})

;; a composition has a series of bodies that move and attack one another
;; each attack is mapped to separate sounds for hit vs. miss

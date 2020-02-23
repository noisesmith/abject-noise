(local sco (require :sco))

(fn buzz-table
  [n]
  (.. "f " n " 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05"))

(local params
  (sco.with-defaults
    {:mod1 118.040092
     :car1 223.830730
     :idx  122.494429
     :car    0.000000
     :idx1 278.396428
     :gain   3.589744}))

(local whine
  (sco.with-defaults
    {:mod1 6280.623674
     :car1 1084.409809
     :idx  4877.505600
     :car  9040.089369
     :idx1  583.518922
     :gain   21.794871}))

(local whine2
  (sco.with-defaults
    {:mod1 7182.627916
     :car1  877.951008
     :idx  3017.817438
     :car  3006.681472
     :idx1  518.930972
     :gain    6.474840}))

(fn ftables
  []
  (buzz-table 1))

(local library
       {:ft (ftables)
        :db (sco.set-db 0 -20)
        :whine (sco.event {:base whine :+t 0 :*d 20 :al -10 :ar -10})
        :basic (sco.event {:base params :+t 10 :*d 10 :al -10 :ar -15})
        :whine2 (sco.event {:base whine2 :+t 14 :*d 10 :al -10 :ar -10})
        :chorus (sco.elaborate whine {:start 5
                                      :duration 30
                                      :n 800
                                      :density 12.3})})

(local composition
       [library.ft
        library.db
        library.whine
        library.basic
        library.whine2
        (sco.mp sco.event library.chorus)])


;; the following goes wrong - the drunken walk shrinks a value
;; down to zero...
(local drunken-walk-test
       [library.ft
        library.db
        (sco.mp sco.event
                (sco.elaborate whine {:start 0
                                      :duration 1000
                                      :n 2400
                                      :density 12.3
                                      :drunk true}))])

(fn isolated-chorus
  [idx]
  [library.ft
   (sco.event (sco.solo (. library.chorus idx)))])


  ;(sco.print-each drunken-walk-test)
  ;(sco.print-each composition)
  ;(sco.print-each (isolated-chorus 6))

{:sco sco
 :params params
 :whine whine
 :whine2 whine2
 :buzz-table buzz-table
 :ftables ftables
 :library library}

(local sco (require :sco))

(fn sine-table
  [n]
  (.. "f " n " 0 32768 10 1"))

(local
 composition2
 [(sine-table 1)
  (sco.set-piece
   {:t 0 :d 10}
   {:car 1000
    :al -5 :ar -10}
   {:car 30
    :al -10 :ar -5})
  (sco.set-piece
   {:t 6 :d 10}
   {:car 1000
    :car1 11
    :idx 10
    :al -10 :ar -5}
   {:car 30
    :car1 3
    :idx 1
    :al -5 :ar -10})
  (sco.set-piece
   {:t 12 :d 20}
   {:car 1000
    :car1 13
    :idx 12
    :al -10 :ar -10}
   {:car 30
    :car1 5
    :idx 2
    :al -5 :ar -5})
  (sco.set-piece
   {:t 26 :d 40}
   {:car 1000
    :car1 13
    :idx 12
    :mod1 333.7
    :idx1 23.3
    :al -10 :ar -10}
   {:car 30
    :car1 5
    :idx 2
    :mod1 8.2
    :idx1 3.3
    :al -5 :ar -5})
  (sco.set-piece
   {:t 47 :d 50}
   {:car 1000
    :car1 23
    :idx 213.4
    :mod1 333.7
    :idx1 43.3
    :al -10 :ar -10}
   {:car 30
    :car1 8
    :idx 5
    :mod1 8.2
    :idx1 18.3
    :al -5 :ar -5})
  (sco.set-piece
   {:t 84 :d 50}
   {:car 1000
    :car1 23
    :idx 18
    :mod1 333.7
    :idx1 43.3
    :al -10 :ar -10}
   {:car 30
    :car1 8
    :idx 5
    :mod1 8.2
    :idx1 8.3
    :al -5 :ar -5})
  (sco.set-piece
   {:t 100 :d 10}
   {:car 501.12
    :car1 23
    :idx 18
    :mod1 333.7
    :idx1 43.3
    :al -10 :ar -10}
   {:car 57.12
    :car1 8
    :idx 5
    :mod1 8.2
    :idx1 8.3
    :al -5 :ar -5})
  (sco.set-piece
   {:t 110 :d 10}
   {:car 482.44
    :car1 23
    :idx 18
    :mod1 333.7
    :idx1 43.3
    :al -10 :ar -10}
   {:car 63.12
    :car1 8
    :idx 5
    :mod1 8.2
    :idx1 8.3
    :al -5 :ar -5})
  (sco.set-piece
   {:t 120 :d 10}
   {:car 501.12
    :car1 23
    :idx 18
    :mod1 333.7
    :idx1 43.3
    :al -10 :ar -10}
   {:car 57.12
    :car1 8
    :idx 5
    :mod1 8.2
    :idx1 8.3
    :al -5 :ar -5})
  {:t 120 :d 20
   :car 57.12
   :car1 8
   :idx 5
   :mod1 8.2
   :idx1 8.3
   :al -5 :ar -5}
  {:t 120 :d 10
   :car 501.12
   :car1 23
   :idx 18
   :mod1 333.7
   :idx1 43.3
   :al -10 :ar -10}
  {:t 130 :d 10
   :car 482.44
   :car1 23
   :idx 18
   :mod1 383.7
   :idx1 63.3
   :al -10 :ar -10}
  (sco.set-piece
   {:t 130 :d 10}
   {:car 482.44
    :car1 23
    :idx 18
    :mod1 383.7
    :idx1 63.3
    :al -10 :ar -10}
   {:car 63.12
    :car1 8
    :idx 5
    :mod1 11.2
    :idx1 18.3
    :al -5 :ar -5})
   {:t 140 :d 69
    :car 44.414
    :car1 21.34
    :idx 12.33
    :mod1 16.3382
    :idx1 2.23
    :al -5 :ar -5}])

(fn main
  []
  (sco.print-each composition2)
  (print))

(main)

{:sine-table sine-table
 }
;; (local fennel (require :fennel))
;; (local s (fennel.dofile "mksco.fnl"))

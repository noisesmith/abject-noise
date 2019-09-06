(fn keyset
  [t]
  (local res {})
  (each [k _ (pairs t)]
    (tset res k true))
  res)

;; sort by in-degree
;; cut loops (?) -  in v2
;; select, emit, remove root nodes
;; repeat, appending any new roots, until empty

(fn sort-by-in-degree
  [g]
  (local degrees {})
  (each [k _ (pairs g)]
    (tset degrees k 0))
  (each [node edges (pairs g)]
    (each [_ edge (ipairs edges)]
      (tset degrees edge (+ (. degrees edge) 1))))
  (local deg [])
  (each [node count (pairs degrees)]
    (table.insert deg [node count]))
  (table.sort deg (fn [[_ v1] [_ v2]] (< v1 v2)))
  deg)

(fn cut-loops
  [g result]
  {})

(fn shallow-copy
  [t]
  (local result {})
  (each [k v (pairs t)]
    (tset result k v))
  result)

(fn toposort
  [g]
  "topologically sorts the directed graph g"
  (local graph (shallow-copy g))
  (local result [])
  (var proc-count 0)
  (var done? false)
  (while (not done?)
    (let [to-visit (sort-by-in-degree graph)]
      (each [_ [k count] (ipairs to-visit)]
        (when (= count 0)
          (table.insert result [k (. graph k)])
          (tset graph k nil)))
      (cut-loops graph result)
      (when (= (# result) proc-count)
        (set done? true))
      (set proc-count (# result))))
  result)

(fn connections->rgraph
  [con]
  "input is the connections format:
  {id {sym number
       other-sym [out-index other-id]}
   other-id {}}
  it maps synth units to their inputs

  output is a standard adjacency list,
  with directions of edges reversed in relation
  to the data flow of the synth graph")

(fn compile
  [opcodes nodes edges]
  "takes three tables
  first the opcode definitions
  {:oscil {:out [:asig] :in [:iamp :ihz]}
   :outs {:out [] :in [:aleft :aright]}}

  then the node definitions
  {0 {:op :oscil}
   1 {:op :outs}}

  finally connections
  {0 {:iamp 0.7
      :ihz 440}
   1 {:left [0 :sig]
      :right [0 :sig]}}

  and produces valid csound instr code:
  a0 oscil 0.7 440
     outs a0, a0"
  )

(local itypes
  {:a {:rate [:audio]}
   :f {:rate [:fsig]}
   :h {:rate [:init] :default 127}
   :i {:rate [:init]}
   :j {:rate [:init] :default -1}
   :J {:rate [:kontrol] :pairs true}
   :k {:rate [:kontrol]}
   :K {:rate [:kontrol :init]}
   :m {:rate [:init] :min-count 0}
   :M {:rate [:audio] :max-count 9}
   :N {:rate [:audio :kontrols] :min-count 0}
   :o {:rate [:init] :default 0}
   :O {:rate [:kontrol] :default 0}
   :p {:rate [:init] :default 1}
   :P {:rate [:kontrol] :default 1}
   :q {:rate [:kontrol] :default 10}
   :S {:rate [:string]}
   :T {:rate [:kontrol :string]}
   :U {:rate [:audio :kontrol] :init true}
   :v {:rate :init :default 0.5}
   :V {:rate [:kontrol] :default 0.5}
   :x {:rate [:audio :kontrol]}
   :y {:rate [:audio] :min-count 0}
   :z {:rate [:kontrol] :min-count 0}
   :Z {:rate [:kontrol :audio] :min-count 0}})

(local otypes
   {:a {:rate [:audio]}
    :f {:rate [:fsig]}
    :F {:rate [:fsig] :min-count 0}
    :i {:rate [:init]}
    :I {:rate [:init] :optional true}
    :k {:rate [:kontrol]}
    :K {:rate [:init :kontrol]}
    :m {:rate [:audio] :optional true}
    :M {:rate [:audios] :max-count 9}
    :s {:rate [:kontrol :audio]}
    :S {:rate [:string]}
    :X {:rate [:audio] :implicit true}
    :z {:rate [:kontrol] :optional true}})

(local unknowns {})

(fn expand-types
  [s t op]
  (when s
    (let [types []]
      (for [i 1 (# s)]
        (let [sym (string.sub s i i)
              found (. t sym)]
          (when (not found)
            (let [unks (or (. unknowns sym) {})]
              (tset unks op s)
              (tset unknowns sym unks)))
          (table.insert types (or found sym))))
      types)))

(fn opcode-data
  []
  (let [opcs (io.popen "csound -z1 2>&1")
        opcodes {}]
    (var found? false)
    (while (not found?)
      (let [line (: opcs :read "*l")
            opcode-count (string.match line "^(%d+) opcodes$")]
        (if opcode-count
          (set found? true))))
    (var done? false)
    (while (not done?)
      (let [line (: opcs :read "*l")]
        (if (not line)
          (set done? true)
          (let [(op out-types in-types) (string.match line "^(%a+) +(%a+) +(%a+)$")
                op-data {:out (expand-types out-types otypes op)
                         :in (expand-types in-types itypes op)}]
            (if op
              (if (. opcodes op)
                (table.insert (. opcodes op) op-data)
                (tset opcodes op [op-data])))))))
    opcodes))

{:keyset keyset
 :sort-by-in-degree sort-by-in-degree
 :toposort toposort
 :compile compile
 :opcode-data opcode-data
 :unknowns unknowns}

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
  {:a :audio
   :i :init
   :f :fsig
   :h {:rate :init :default 127}
   :j {:rate :init :default -1}
   :J {:rate :kontrol :pairs true}
   :k :kontrol
   :O {:rate :kontrol :default 0}
   :P {:rate :kontrol :default 1}
   :q {:rate :kontrol :default 10}
   :V {:rate :kontrol :default 0.5}
   :K [:kontrol :init]
   :m {:rate :init :min-count 0}
   :M {:rate :audio :max-count 9}
   :N {:rate [:audio :kontrols] :min-count 0}
   :o {:rate :init :default 0}
   :p {:rate :init :default 1}
   :S :string
   :T [:kontrol :string]
   :U {:rate [:audio :kontrol] :init true}
   :v {:rate :init :default 0.5}
   :x [:audio :kontrol]
   :y {:rate :audio :min-count 0}
   :z {:rate :kontrol :min-count 0}
   :Z {:rate [:kontrol :audio] :min-count 0}})

(local otypes
   {:a :audio
    :f :fsig
    :F {:rate :fsig :min-count 0}
    :i :init
    :I {:rate :init :optional true}
    :k :kontrol
    :K [:init :kontrol]
    :m {:rate :audio :optional true}
    :M {:rate :audios :max-count 9}
    :s {:kontrol :audio}
    :S :string
    :X {:rate :audio :implicit true}
    :z {:rate :kontrol :optional true}})

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

{:compile compile
 :opcode-data opcode-data
 :unknowns unknowns}

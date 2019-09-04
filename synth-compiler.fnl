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
   :j :init--1
   :k :kontrol
   :O :kontrol-0
   :P :kontrol-1
   :V :kontrol-0_5
   :K :kontrol-init
   :o :init-0
   :p :init-1
   :S :string})

(local otypes
   {:a :audio
    :i :init
    :k :kontrol
    :K :kontrol-init})

(local unknowns {})

(fn expand-types
  [s t op]
  (when s
    (let [types []]
      (for [i 1 (# s)]
        (let [sym (string.sub s i i)
              found (. t sym)]
          (when (not found)
            (let [unks (or (. unknowns sym) [])]
              (table.insert unks op)
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

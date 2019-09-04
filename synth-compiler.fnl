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
                op-data {:out out-types :in in-types}]
            (if op
              (if (. opcodes op)
                (table.insert (. opcodes op) op-data)
                (tset opcodes op [op-data])))))))
    opcodes))

{:compile compile
 :opcode-data opcode-data}

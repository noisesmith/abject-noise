(fn compile
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
  [opcodes nodes edges])

(fn opcode-data
  []
  (let [opcs (io.popen "csound -z1 2>&1")
        opcodes {}]
    (var done? false)
    (while (not done?)
      (let [line (: opcs :read "*l")]
        (if (not line)
          (set! done? true)
          ;; TODO - actually process here
          ;;  put something in the opcodes table for matching lines
          (print line))))))

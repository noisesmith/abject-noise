;;; composition.csd
<CsoundSynthesizer>
<CsOptions>
;;; cli opts
-Lstdin
-+rtaudio=jack
-odac
-iadc
-b 1024
-B 2048

</CsOptions>
<CsInstruments>

;;; orc headers
  sr         =  48000
  ksmps      =  512
  nchnls     =  4
  0dbfs      =  1

  gidb       = -6

;;; udos
    opcode lna, a,iiiii
  imn, imx, iin, iiout, idur \
    xin
  aout linen (imx-imn), idur*iin, idur, idur*iiout
         ; between multiplying by (mx-mn), and adding mn back to out,
         ; we effectively the linen output between mn and mx
    xout aout+imn
    endop


#define FMS #
;;; synth body
  adb        lna -65, p4, 0.1, 0.25, p3
  amod       poscil3 aidx1, amod1
  a1         poscil3 aidx, acar1+amod
  aclean     poscil3 ampdb(again), acar+a1, 1
  adist      limit aclean, -1, 1
  al         = adist*ampdbfs(adb+p4+gidb)
  ar         = adist*ampdbfs(adb+p5+gidb)
             outq al, ar, al, ar
#



      instr wobble_a
    amod1   =       118.040092
    amod1   =       83.518930
    acar1   =       223.830730
    aidx    =       122.494429
    acar    =       0.000000
    aidx1   =       278.396428
    again   =       3.589744
    $FMS
      endin

      instr dumb_machine
    amod1   =       485.523373
    acar1   =       457.683742
    aidx    =       502.227187
    acar    =       207.126945
    aidx1   =       645.879745
    again   =       0.000000
    $FMS
      endin


      instr evil_buzz
    amod1   =       951.002240
    acar1   =       743.875265
    aidx    =       1000.000000
    acar    =       207.126945
    aidx1   =       732.739449
    again   =       0.000000
    $FMS
      endin

</CsInstruments>
<CsScore>

;;; score
;; a buzz harmonic
f 1 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05

i "wobble_a"        0     20   -15  -5
i "wobble_a"        2.3   20    -5 -15
i "dumb_machine"    4      8    -8  -9
i "evil_buzz"       8     30    -4  -3
i "dumb_machine"   12      8    -3  -4
i "dumb_machine"   22      8   -10 -10


e
</CsScore>
</CsoundSynthesizer>


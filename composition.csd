// composition.csd
<CsoundSynthesizer>
<CsOptions>
-Lstdin
-+rtaudio=jack
-odac
-iadc
-b 1024
-B 2048

</CsOptions>
<CsInstruments>

  sr         =  48000
  ksmps      =  512
  nchnls     =  4
  0dbfs      =  1


    opcode lna, a,iiiii
  imn, imx, iin, iiout, idur \
    xin
  aout linen (imx-imn), idur*iin, idur, idur*iiout
         ; between multiplying by (mx-mn), and adding mn back to out,
         ; we effectively the linen output between mn and mx
    xout aout+imn
    endop


    instr    1000 ; fm generator
  amod1      = 3060.3
  acar1      = 503.293880
  aidx       = 402
  acar       = 0.02
  aidx1      lna 70, 121, 0.92, 0.08, p3
  again       lna -0.5, 1.0525, 0.9, 0.1, p3
  adb         lna -65, -15, 0.01, 0.25, p3
  amod       poscil3 aidx1, amod1
  a1         poscil3 aidx, acar1+amod
  aclean     poscil3 ampdb(again), acar+a1
  adist      limit aclean, -1, 1
  aout       = adist*ampdbfs(adb)
             outq aout, aout, aout, aout
    endin


    instr    1100 ; another fm generator
  amod1      lna 0, 3060.3, 0.5, 0.5, p3
  acar1      lna 503.293880, 0, 0.7, 0.3, p3
  aidx       = 402
  acar       = 0.2
  aidx1      lna 70, 121, 0.92, 0.08, p3
  again      lna -0.5, 1.0525, 0.9, 0.1, p3
  adb        lna -65, -15, 0.01, 0.25, p3

  amod       poscil3 aidx1, amod1
  a1         poscil3 aidx, acar1+amod
  aclean     poscil3 ampdb(again), acar+a1
  adist      limit aclean, -1, 1
  aout       = adist*ampdbfs(adb)
             outq aout, aout, aout, aout
    endin



</CsInstruments>
<CsScore>

i 1000 0 100
i 1100 10.14 89


e
</CsScore>
</CsoundSynthesizer>

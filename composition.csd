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


    instr    1000 ; fm generator
  aidx1      linen 3006, p3, 0, 0
  kmod1      = 306
  kcar1      = 500
  kidx       = 400
  kcar       = 200
  adist      linen 10, 0.6, p3-1, 0.4
  adb        linen 50, 0.1, p3-0.6, 0.5
  amod1      poscil3 aidx1+70, kmod1
  a1         poscil3 kidx, kcar1+amod1
  aclean     poscil3 ampdbfs(adist), kcar+a1
  adist      limit aclean, -0dbfs, 0dbfs
  aout       = adist*ampdbfs(adb-65)
             outq aout, aout, aout, aout
    endin

</CsInstruments>
<CsScore>

i 1000 0 10

e
</CsScore>
</CsoundSynthesizer>

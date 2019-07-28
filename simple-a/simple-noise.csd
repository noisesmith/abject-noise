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

  sr	     =  48000
  ksmps	     =  512
  nchnls     =  1
  0dbfs	     =  1


  gilisten   OSCinit 4444

    instr    1000 ; noise generator

  klow       init 0.5
  khigh      init 0.5
next:

  kk         OSClisten gilisten, "/filter_range", "ff", klow, khigh

if (kk == 0) goto done

             ;; printks "klow = %f, khigh = %f\\n", \
             ;;         0, klow, khigh
             kgoto   next
done:
    endin
turnon 1000

</CsInstruments>
<CsScore>
f 0 360000000 ;Dummy f-table

e
</CsScore>
</CsoundSynthesizer>

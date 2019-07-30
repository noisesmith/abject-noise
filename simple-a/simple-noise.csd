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
  nchnls     =  2
  0dbfs     =  1


  gilisten   OSCinit 4444

    instr    1000 ; noise generator

  klow       init 0.5
  khigh      init 0.5
  kamp        init 0.7

;; filter
filter_next:
  kk         OSClisten gilisten, "/filter_range", "ff", klow, khigh
if (kk == 0) goto filter_done
             kgoto   filter_next
filter_done:
;; amp
amp_next:
  kk         OSClisten gilisten, "/amp", "f", kamp
if (kk == 0) goto amp_done
             kgoto   amp_next
amp_done:
  kdb        port (1-kamp)*-95, 0.5
  klf        port (klow*klow*klow)*500, 0.5
  klh        port (khigh*khigh*khigh)*900, 0.5
  kc         = (klf+klh)/2
  kspread    = klh-klf
  anoise     fractalnoise 1, 2
  afil       resonr anoise, kc, kspread
  acont      butlp afil, 900
  acomp      compress afil, acont, 0, 48, 60, 4, 0.01, 0.1, 0.1
  aout       = acomp*ampdbfs(kdb)
             outs aout, aout
    endin

turnon 1000

</CsInstruments>
<CsScore>
f 0 360000000 ;Dummy f-table

e
</CsScore>
</CsoundSynthesizer>

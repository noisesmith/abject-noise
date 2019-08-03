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
  0dbfs      =  1


  gilisten   OSCinit 4445

    opcode oscvar, k, Siii
  Spath, iini, ilo, ihi \
             xin
  kin        init iini
next:
  kk         OSClisten gilisten, Spath, "f", kin
if (kk == 0) goto done
             kgoto next
done:
    iscale   = ihi - ilo
    kval     port (kin*iscale)+ilo, 0.15
             xout kval
    endop

    instr    1000 ; fm generator
  kcar       oscvar "/car", 0.7, 0.01, 14
  kmod       oscvar "/mod", 0.7, 0.01, 14
  kidx       oscvar "/idx", 0.7, 0.01, 14
  kdb        oscvar "/amp", 0, -95, 0
  amod       poscil3 2^kidx, 2^kmod
  aout       poscil3 ampdbfs(kdb), (2^kcar)+amod
             outs aout, aout
    endin

turnon 1000

</CsInstruments>
<CsScore>
f 0 360000000 ;Dummy f-table

e
</CsScore>
</CsoundSynthesizer>

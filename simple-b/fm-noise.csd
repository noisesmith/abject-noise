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
  kidx       oscvar "/idx", 0.7, 0.01, 24
  kcar1      oscvar "/car1", 0.7, 0.01, 14
  kmod1      oscvar "/mod1", 0.7, 0.01, 14
  kidx1      oscvar "/idx1", 0.7, 0.01, 24
  kdist      oscvar "/distortion", 0, 0, 65
  kdb        oscvar "/amp", 0, -95, 0
  kmute      oscvar "/mute", 0, 1, 0
  amod1      poscil3 2^kidx1, 2^kmod1
  a1         poscil3 2^kidx, (2^kcar1)+amod1
  aclean     poscil3 ampdbfs(kdist), (2^kcar)+a1
  adist      limit aclean, -0dbfs, 0dbfs
  aout       = adist*ampdbfs(kdb)*kmute
             outs aout, aout
    endin

turnon 1000

</CsInstruments>
<CsScore>
f 0 360000000 ;Dummy f-table

e
</CsScore>
</CsoundSynthesizer>

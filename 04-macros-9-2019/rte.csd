;;; rte.csd
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


;;; debug / audition
  gilisten   OSCinit 4445

    opcode oscvar, a, Siii
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
             xout a(kval)
    endop

    opcode oscvark, k, Siii
  Spath, iini, ilo, ihi \
             xin
  kin        init iini
next:
  kk         OSClisten gilisten, Spath, "f", kin
if (kk == 0) goto done
             kgoto next
done:
    iscale   = ihi - ilo
             xout (kin*iscale)+ilo
    endop

    instr 9999
    amod1  oscvar "/mod1", 0, 0,10000
    acar1  oscvar "/car1", 0, 0, 1800
    aidx   oscvar "/idx", 0, 0, 10000
    acar   oscvar "/car", 0, 0, 18000
    aidx1  oscvar "/idx1", 0, 0, 1000
    again  oscvar "/gain", 0, 0, 30
    adb    oscvar "/amp", -65, -65, 0
    kmute  oscvark "/mute", 0, 1, 0
    kdisp  oscvark "/display", 0, 0, 1
    kdp    changed kdisp
           printf \
{{
    amod1   =       %020f
    acar1   =       %020f
    aidx    =       %020f
    acar    =       %020f
    aidx1   =       %020f
    again   =       %020f
}}, \
                  kdisp*kdp, k(amod1),  k(acar1), k(aidx),  k(acar),  k(aidx1),   k(again)
    kexit  oscvark "/exit", 0, 0, 1
           scoreline "e", kexit
    amod   poscil3 aidx1, amod1
    a1     poscil3 aidx, acar1+amod
    aclean poscil3 ampdb(again), acar+a1, 1
    adist  limit aclean, -1, 1
    aout   = adist*ampdbfs(adb)*kmute
           outq aout, aout, aout, aout
    endin


</CsInstruments>
<CsScore>

;; a buzz harmonic
f 1 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05

;; for playing around
i 9999 0 360000

e
</CsScore>
</CsoundSynthesizer>


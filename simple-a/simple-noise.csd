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


  gilisten   OSCinit 4444

    opcode oscvar1, k, SSi
  Spath, Stypes, iini \
             xin
  kval       init iini
next:
  kk         OSClisten gilisten, Spath, Stypes, kval
if (kk == 0) goto done
             kgoto next
done:
             xout kval
    endop

    opcode oscvar2, kk, SSii
  Spath, Stypes, iini1, iini2 \
             xin
  kval1      init iini1
  kval2      init iini2
next:
  kk         OSClisten gilisten, Spath, Stypes, kval1, kval2
if (kk == 0) goto done
             kgoto next
done:
             xout kval1, kval2
    endop

    instr    1000 ; noise generator
  klo, khi   oscvar2 "/filter_range", "ff", 0.5, 0.5
  kamp       oscvar1 "/amp", "f", 0.7
  kdb        port (1-kamp)*-95, 0.5
  klf        port (klo*klo*klo)*500, 0.5
  klh        port (khi*khi*khi)*900, 0.5
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

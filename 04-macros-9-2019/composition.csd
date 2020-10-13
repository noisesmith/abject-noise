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

      instr 1
    adb     lna -65, 1, 0.1, 0.25, p3
    agl     = ampdbfs(adb+p4+gidb)
    agr     = ampdbfs(adb+p5+gidb)
    imod1   = p6
    icar1   = p7
    iidx    = p8
    icar    = p9
    iidx1   = p10
    igain   = p11
    amod    poscil3 iidx1, imod1
    a1      poscil3 iidx, icar1+amod
    aclean  poscil3 ampdb(igain), icar+a1, 1
    adist   limit aclean, -1, 1
    al      = adist*agl
    ar      = adist*agr
             outq al, ar, al, ar
      endin


      instr wobble_a
    amod1   =     118.040092
    amod1   =      83.518930
    acar1   =     223.830730
    aidx    =     122.494429
    acar    =       0.000000
    aidx1   =     278.396428
    again   =       3.589744
    $FMS
      endin

      instr dumb_machine
    amod1   =     485.523373
    acar1   =     457.683742
    aidx    =     502.227187
    acar    =     207.126945
    aidx1   =     645.879745
    again   =       0.000000
    $FMS
      endin


      instr evil_buzz
    amod1   =     951.002240
    acar1   =     743.875265
    aidx    =    1000.000000
    acar    =     207.126945
    aidx1   =     732.739449
    again   =       0.000000
    $FMS
      endin

      instr wohwah
    amod1   =       838.530064
    acar1   =       590.200424
    aidx    =       216.035634
    acar    =      3001.180400
    aidx1   =       942.093551
    again   =         0.000000
    $FMS
      endin

      instr yih
    amod1   =       0000000010000.000000
    acar1   =       0000000000465.033406
    aidx    =       0000000007594.654560
    acar    =       0000000003788.418800
    aidx1   =       0000000000129.175946
    again   =       0000000000000.000000
    $FMS
      endin

</CsInstruments>
<CsScore>

;;; score
;; a buzz harmonic
f 1 0 32768 10 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.05

i	1	0	10	-10	-15	118.040092	118.040092	223.83073	122.494429	0278.396428	3.589744

; p. "
; i "wobble_a"        2.3   20       -5 -15
; i "dumb_machine"    4      8       -8  -9
; i "evil_buzz"       8     30       -4  -3
; i "dumb_machine"   12      8       -3  -4
; i "dumb_machine"   22      8      -10 -10

; i "wohwah"          1      0.3     -5  -5
; i "wohwah"          1.4    0.3    -15  -5
; i "wohwah"          1.5    0.1     -5 -15

; i "yih"             0.1    0.8     -8  -8
; i "yih"             1.1    0.8     -8  -8
; i "yih"             2.9    3.0     -4  -4

; i "wohwah"          4.2    5.2      0   0

e
</CsScore>
</CsoundSynthesizer>


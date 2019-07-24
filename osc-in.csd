<CsoundSynthesizer>
; network_recv.csd
<CsOptions>
; Select audio/midi flags here according to platform
; Audio out   Audio in
-Lstdin
-+rtaudio=jack
-odac
-iadc    ;;;RT audio I/O
-b 1024
-B 2048
</CsOptions>
<CsInstruments>

  sr	    =  48000
  ksmps	    =  512
  nchnls    =  1
  0dbfs	    =  1


  gilisten  OSCinit   4444

    instr   1000

  kst	    init      0
  kch	    init      0
  kd1	    init      0
  kd2	    init      0

next:

  kk	    OSClisten	gilisten, "/foo/", "iiii", kst, kch, kd1, kd2

if (kk == 0) goto done

printks "kst = %i, kch = %i, kd1 = %i, kd2 = %i\\n", \
         0, kst, kch, kd1, kd2

	kgoto	next  ;Process all events in queue

done:
    endin


turnon 1000

</CsInstruments>
<CsScore>
f 0 3600  ;Dummy f-table
e
</CsScore>
</CsoundSynthesizer>

<CsoundSynthesizer>
<CsOptions>
</CsOptions>
; ==============================================
<CsInstruments>

sr	= 48000
ksmps	= 1
nchnls	= 2
0dbfs	= 1

instr 1

kamp = ampdbfs(p4)

;  (0 LINEAR)(1 CAUCHY)(2 LOGIST)(3 HYPERBCOS)(4 ARCSINE)(5 EXPON)
;  (6 SINUS (external k-rate signal -- see kadpar))
kampdist = 2
kdurdist = 3
kadpar = 0 ; would modulate kampdist if it = 6
kddpar = 0 ; would modulate kdurdist if it = 6

kminfreq = 0
kmaxfreq = 20000
kampscl  = 1.0
kdurscl  = 1.0
; (0 step)(<1 concave)(1: linear)(>1 convex)
kcurveup = 0
kcurvedown = 0
iinitcps = 24
knum = 12

asig gendyx kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, \
            kampscl, kdurscl, kcurveup, kcurvedown, iinitcps, knum

    outs asig, asig
endin

</CsInstruments>
; ==============================================
<CsScore>

i 1 0 10 0.5

</CsScore>
</CsoundSynthesizer>


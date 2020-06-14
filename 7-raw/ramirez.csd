<CsoundSynthesizer>
<CsOptions>
-odac
-Lstdin
-d
</CsOptions>
; ==============================================
<CsInstruments>

sr	=	48000
ksmps	=	1024
nchnls	=	2
0dbfs	=	1

gagain	= ampdbfs(-10)
gal	= 0
gar	= 0

	instr 1
kal	= 1 ; 0-1
kar	= 1 ; 0-1
kampdst	= 0 ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kdurdst	= 0 ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kadpar	= 0.2 ; 0.0001 - 1.
kddpar	= 0.2 ; 0.0001 - 1.
kminhz	= 1000.1
kmaxhz	= 3000.3
kampscl	= 0.3 ; 0 - 1
kdurscl	= 0.3 ; 0 - 1
araw	gendy	1, \
		kampdst, kdurdst, kadpar, kddpar, \
		kminhz, kmaxhz, kampscl, kdurscl
kpre	= ampdb(10)
kpost	= ampdb(10)
kshape1	= 0.01 ; 0=flat clip
kshape2	= 0.01 ; 0=flat clip
imode	= 1 ; 0dbfs range
adist	distort1 araw, kpre, kpost, kshape1, kshape2, imode
alow	mode adist, 80, 10
alow	dcblock alow ; trying to get bakc some dynamic range
ahigh	mode adist, 5000, 10
klow	= ampdb(-50)
khigh	= ampdb(-20)
kdry	= ampdb(-10)
asig	= klow*alow + khigh*ahigh + kdry*araw
krev	= ampdb(-5)
gal	+= asig*kal*krev
gar	+= asig*kar*krev
	outs gagain*kal*asig, gagain*kar*asig
	endin

	instr 999
kamp	= ampdb(-10)
kfblvl	= 0.9 ; 0-1
kfco	= 18000
ipitchm	= 8
al,ar	reverbsc gal, gar, kfblvl, kfco, sr, ipitchm
	outs (al*kamp), (ar*kamp)
gal	= 0
gar	= 0
	endin

	instr 1999 ; buggy, slow - bad params?
kamp	= ampdb(-70)
igdir	= -2 ; gen routine for literal table values
		; hz, radius(<1), phase(rad)
itexc	ftgen 0, 0, 8, igdir, \
		0.001, 0.3, 0, \
		0.001, 0.3, 0
itout	ftgen 0, 0, 8, igdir, \
		1, 0.3, 0, \
		1, 0.3, 0
kbndry	= 0 ; 0 = free, 1 = clamped, 2 = pivoting
iaspect	= 1 ; <=1
istiff	= 0.9
idecay	= 0.2 ; 30db time
iloss	= 0.001 ; hf damping
al,ar	platerev itexc, itout, kbndry, iaspect, istiff, idecay, iloss, gal, gar
	outs (al*kamp), (ar*kamp)
gal	= 0
gar	= 0
	endin

</CsInstruments>
; ==============================================
<CsScore>
i 1   0 100
i 999 0 -1
s 120


</CsScore>
</CsoundSynthesizer>


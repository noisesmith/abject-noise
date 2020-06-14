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

itmxp1	ftgen	101, 0, 8, -2, \ ; n, when, size, gen
		1, 1, \ ; l,r
		-50, -20, -10, -5 ; low,high,dry,rev

ittxt1	ftgen	102, 0, 8, -2, \
		1000.1, 3000.3, \ ; hz
		10, 10, \ ; gain
		80,10, 5000,10 ; low, high

itwrd1	ftgen	103, 0, 8, -2, \
		0,0,0,0,0.3,0.3, \ ; gendy weird
		0.01,0.01 ; dist weird

	instr 1
	tb0_init p4 ; mix / pan
	tb1_init p5 ; texture
	tb2_init p6 ; weirdness
kal	= tb0(0) ; 0-1
kar	= tb0(1) ; 0-1
kampdst	= tb2(0) ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kdurdst	= tb2(1) ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kadpar	= tb2(2) ; 0.0001 - 1.
kddpar	= tb2(3) ; 0.0001 - 1.
kminhz	= tb1(0)
kmaxhz	= tb1(1)
kampscl	= tb2(4) ; 0 - 1
kdurscl	= tb2(5) ; 0 - 1
araw	gendy	1, \
		kampdst, kdurdst, kadpar, kddpar, \
		kminhz, kmaxhz, kampscl, kdurscl
kpre	= ampdb(tb1(2))
kpost	= ampdb(tb1(3))
kshape1	= tb2(6) ; 0=flat clip
kshape2	= tb2(7) ; 0=flat clip
imode	= 1 ; 0dbfs range
adist	distort1 araw, kpre, kpost, kshape1, kshape2, imode
alow	mode adist, tb1(4), tb1(5)
alow	dcblock alow ; trying to get bakc some dynamic range
ahigh	mode adist, tb1(6), tb1(7)
klow	= ampdb(tb0(2))
khigh	= ampdb(tb0(3))
kdry	= ampdb(tb0(4))
asig	= klow*alow + khigh*ahigh + kdry*araw
krev	= ampdb(tb0(5))
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


</CsInstruments>
; ==============================================
<CsScore>
i 1   0 100 101 102 103
i 999 0 -1
s 120


</CsScore>
</CsoundSynthesizer>


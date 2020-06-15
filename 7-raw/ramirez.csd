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

gkgain	init ampdbfs(-10)
gal	init 0
gar	init 0


	instr 1
im	= p4
it	= p5
iw	= p6
kal	= ampdb(tab:k(0, im))
kar	= ampdb(tab:k(1, im))
kampdst	= tab:k(0, iw) ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kdurdst	= tab:k(1, iw) ; 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kadpar	= tab:k(2, iw) ; 0.0001 - 1.
kddpar	= tab:k(3, iw) ; 0.0001 - 1.
kminhz	= tab:k(0, it)
kmaxhz	= tab:k(1, it)
kampscl	= tab:k(4, iw) ; 0 - 1
kdurscl	= tab:k(5, iw) ; 0 - 1
araw	gendy	1, \
		kampdst, kdurdst, kadpar, kddpar, \
		kminhz, kmaxhz, kampscl, kdurscl
kpre	= ampdb(tab:k(2, it))
kpost	= ampdb(tab:k(3, it))
kshape1	= tab:k(6, iw) ; 0=flat clip
kshape2	= tab:k(7, iw) ; 0=flat clip
imode	= 1 ; 0dbfs range
adist	distort1 araw, kpre, kpost, kshape1, kshape2, imode
alow	mode adist, tab:k(4, it), tab:k(5, it)
alow	dcblock alow ; trying to get bakc some dynamic range
ahigh	mode adist, tab:k(6, it), tab:k(7, it)
klow	= ampdb(tab:k(2, im))
khigh	= ampdb(tab:k(3, im))
kdry	= ampdb(tab:k(4, im))
asig	= klow*alow + khigh*ahigh + kdry*araw
krev	= ampdb(tab:k(5, im))
gal	+= asig*kal*krev
gar	+= asig*kar*krev
kdirect	= ampdb(tab:k(6, im))
	outs kdirect*gkgain*kal*asig, kdirect*gkgain*kar*asig
	endin

	instr 999
kamp	= ampdb(-10)
kfblvl	= 0.9 ; 0-1
kfco	= 18000
ipitchm	= 8
al,ar	reverbsc gal, gar, kfblvl, kfco, sr, ipitchm
	outs gkgain*kamp*al, gkgain*kamp*ar
gal	= 0
gar	= 0
	endin


</CsInstruments>
; ==============================================
<CsScore>
; using ftables here to define parameter sets
f 101 0 7 -2 \
  0 0 \ ; l,r
  -50 -20 -10 -5 0; low,high,dry,rev,clean

f 102 0 8 -2 \
  1000.1 3000.3 \ ; hz
  10 10 \ ; gain
  80 10  5000 10 ; low, high

f 103 0 8 -2 \
  0 0 0 0 0.3 0.3 \ ; gendy weird
  0.01 0.01 ; dist weird

i 1   0.1 100 101 102 103

i 999 0 -1
s 120


</CsScore>
</CsoundSynthesizer>


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

gkgain	init ampdbfs(-18)
gal	init 0
gar	init 0
gidebug = 0


	instr 1
igain	= ampdb(p4)
im	= p5 ; table for mix / gain params - all passed through ampdb
it	= p6 ; table for texture params
iw	= p7 ; table for "weird" params - hard to categorize or not understood
kal	= ampdb(tab:k(0, im))
kar	= ampdb(tab:k(1, im))
;; dstparam - 0:linear 1:cauchy 2:logist 3:hyperbcos 4:arcsine 5:expon 6:table/kadpar
kampdst	= tab:k(0, iw) ; dstparam
kdurdst	= tab:k(1, iw) ; dstparam
kadpar	= tab:k(2, iw) ; 0.0001 - 1.
kddpar	= tab:k(3, iw) ; 0.0001 - 1.
kminhz	= tab:k(0, it) ; hz
kmaxhz	= tab:k(1, it) ; hz
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
alow	mode adist, tab:k(4, it), tab:k(5, it) ; hz, q
alow	dcblock alow ; trying to get bakc some dynamic range
ahigh	mode adist, tab:k(6, it), tab:k(7, it) ; hz, q
ahigh	dcblock ahigh
klow	= ampdb(tab:k(2, im))
khigh	= ampdb(tab:k(3, im))
kdry	= ampdb(tab:k(4, im))
asig	= igain*(klow*alow + khigh*ahigh + kdry*araw)
krev	= ampdb(tab:k(5, im))
gal	+= asig*kal*krev
gar	+= asig*kar*krev
kdirect	= ampdb(tab:k(6, im))
al	limit kdirect*gkgain*kal*asig, -1, 1
ar	limit kdirect*gkgain*kar*asig, -1, 1
	outs al, ar
	endin

;; overwrites some table to superimpose a curve
;; don't use recursively! it clobbers the table
	instr 2, curve
itb	= p4
iidx	= p5
kidx	= p5
ipeak	= p6
iin	= p7*p3
iout	= p8*p3
iv	tab_i iidx, itb
kcurve	linseg iv, iin, iv+ipeak, iout, iv
	tabw kcurve, kidx, itb
	endin

;; makes a new table, a copy of some original
;; id of result table must be passed in
	instr 3, copytab
itsrc	= p4
itdest	= p5
itsize  = ftlen(itsrc)
i_	ftgen itdest, 0, itsize, -2, 0
	tableicopy itdest, itsrc
itdone	pcount
itdx	= 6
NEXT:
	cigoto (itdx >= itdone), DONE
ival	= p(itdx+1)
iidx	= p(itdx)
Sprefix = "log from instr 3 / copytab "
SSep    = "%%%%%%%"
	printf_i "%s %s writing %f to %d of table %d\n", gidebug, \
	          Sprefix, SSep, ival, iidx, itdest
	tabw_i ival, iidx, itdest
itdx    = itdx+2
	igoto NEXT
DONE:
	endin

	instr 4, debugtab
it	= p4
isz	= ftlen(it)
iidx	= 0
inxtbrk = 5
Sprefix = "log from instr 4 / debugtab"
SSep    = "%%%%%%%"
	printf_i "%s %s time: %f %s dump of table %d\n", 1, \
	          Sprefix, SSep, \
	          p2, SSep, it
NEXT:
	cigoto (iidx == isz), DONE
	printf_i "%d: %0.010f", 1, iidx, tab_i(iidx, it)
	printf_i ", ", (iidx == p(inxtbrk) ? 0 : 1)
	printf_i "\n...	", (iidx == p(inxtbrk) ? 1 : 0)
inxtbrk = iidx == p(inxtbrk) ? inxtbrk+1 : inxtbrk
iidx    = iidx+1
	igoto NEXT
DONE:
	printf_i "\n", 1
	endin

	instr 999
kamp	= ampdb(-10)
kfblvl	= 0.9 ; 0-1
kfco	= 18000
ipitchm	= 8
arl,arr	reverbsc gal, gar, kfblvl, kfco, sr, ipitchm
al	limit gkgain*kamp*arl, -1, 1
ar	limit gkgain*kamp*arr, -1, 1
	outs al, ar
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

f 201 0 8 -2 \ ; squeal!
  1000.1 3000.3 \ ; hz
  10 10 \ ; gain
  80 10  5000 10 ; low, high

f 301 0 8 -2 \
  0 0 0 0 0.3 0.3 \ ; gendy weird
  0.01 0.01 ; dist weird

f 202 0 8 -2 \ ; rumble
  0.09 30.1 \
  0 15 \
  30 20 1000 3

f 203 0 8 -2 \ ; bleat
  0.09 300.1 \
  10 15 \
  90 20 4500 40

f 302 0 8 -2 \
  5 5 0.7 0.8 0.3 0.9 \ ; gendy weird
  0.01 0.01 ; dist weird

i "curve" 0 100 201 4 100 0.5 0.5
i1   0 100   -15 101 202 301

i1   3  15   -10 101 203 301
i1  10   1.3   0 101 201 301
i1  31  95     0 101 203 301
i1  40  10    10 101 201 302
i1  90  20    10 101 201 302

i "copytab" 0 0 101 102 \
   0 -75 1 -75 \
   4  10 5 10
i "copytab" 0 0 203 204 \
   0 200.1 \
   1 300.3 \
f 303 0 8 -2
i "copytab" 0 0 302 303 \
   6 0.9 \
   7 0.9

;i "debugtab" 140   0 102   1
;i "debugtab" 190   0 102   1
;i "debugtab" 240   0 102   1

i "curve" 110 100 102   0   55 0.1 0.9
i "curve" 110 100 102   1   55 0.9 0.1
i "curve" 110 100 204   0  820 0.5 0.5
i "curve" 110 100 204   1  990 0.5 0.5
i "curve" 110 100 204   5   90 0.5 0.5
i1 110 100     0 102 204 303

i999 0 -1
s 230

</CsScore>
</CsoundSynthesizer>
; $ csound ramirez.csd -o RZ.wav&& mplayer RZ.wav

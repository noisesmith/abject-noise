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
im	= p5
it	= p6
iw	= p7
#include "tab-gend.ins"
	outs al, ar
	endin

;; overwrites some table to superimpose a curve
;; don't use recursively! it clobbers the table
	instr curve
itb	= p4
iidx	= p5
ipeak	= p6
ipt	= p7
#include "tab-curve.ins"
	endin

	instr wiggle
itb	= p4
iidx	= p5
idepth	= p6
iptab	= p7
#include "tab-wiggle.ins"
	endin

;; makes a new table, a copy of some original
;; id of result table must be passed in
	instr copytab
itsrc	= p4
itdest	= p5
#include "tab-copy.ins"
	endin

	instr debugtab
iwhen	= p2
itab	= p4
isz	= ftlen(itab)
iidx	= 0
inxtbrk = 5
#include "tab-debug.ins"
	endin

	instr 999
arevl	= gal
gal	= 0
arevr	= gar
gar	= 0
#include "reverb.ins"
	outs al, ar
	endin


</CsInstruments>
; ==============================================
<CsScore>
; using ftables here to define parameter sets
#include "base-tables.sco"

i "curve" 0 100 201 4 100 0.5
i1   0 100   -15 101 202 301

i1   3  15   -10 101 203 301
i1  10   1.3   0 101 201 301
i1  31  95     0 101 203 301
i1  40  10    10 101 201 302
i1  90  20    10 101 201 302


f 400 0 8 -2 \
	1 100 \ ; depth
	333.22 272 \ ; carrier
	72      49.021 \ ; modulation speed
	100,   100.3 ; index
i "wiggle" 110 100 204  4  955 400
i "curve" 110 100  102  0   55 0.1
i "curve" 110 100  102  1   55 0.9
i "curve" 110 100  204  0  820 0.5
i "curve" 110 100  204  1  990 0.5
i "curve" 110 100  204  5    9 0.5
i1 110 100     0 102 204 303

f 400 0 8 -2 \
	1 100 \ ; depth
	333.22 272 \ ; carrier
	72      49.021 \ ; modulation speed
	100,   100.3 ; index

i999 0 -1

</CsScore>
</CsoundSynthesizer>

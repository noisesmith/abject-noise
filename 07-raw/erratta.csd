<CsoundSynthesizer>
<CsOptions>
</CsOptions>
; ==============================================
<CsInstruments>

sr	=	48000
ksmps	=	1
;nchnls	=	2
0dbfs	=	1

instr 1	


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
endin

</CsInstruments>
; ==============================================
<CsScore>



</CsScore>
</CsoundSynthesizer>


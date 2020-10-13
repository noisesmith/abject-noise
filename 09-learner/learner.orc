	sr	= 44100
	kr	= 1
	nchnls	= 2
	0dbfs	= 1

	gigain	= -15
		seed 0

opcode	pos, i,iiii
	idur,irange,ipos,iwiggle xin
	ifree	= irange-idur
	ibase	= ipos*(1-iwiggle)
	ivari	random 0, iwiggle
	xout ifree*ivari
endop

instr	learn_write
	idur	= p3*sr
	idest	= p4
	ipos	= p5
	istr	= ampdb(p6)
;; some position near ipos, between 0 and size of dest
	itsize		tableng idest
	iwritestart	pos idur, itsize, ipos, 0.3
	print iwritestart
		asig		inch 1
		aphase		line 0, p3, idur
		kenvshape	linen 65, 0.2, idur, 0.2
		kenv		= ampdbfs(istr + (65 - kenvshape))
		ifttmp		ftgentmp 0, 0, idur, -2, 0, idur
		awrite		= asig*kenv
	tabw	awrite, aphase, ifttmp
		kt	timeinsts
		kdone	= kt < (p3 - (ksmps/sr)) ? 0 : 1
	ckgoto	(kdone == 0), exit
		kidx = 0
write:
		kv	tab kidx, ifttmp
		kdi	= kidx+iwritestart
		ko	tab kdi, idest
			tabw kv+ko, kdi, idest
		kidx	= kidx+1
		ckgoto (kidx < idur), write
exit:
endin

instr	learn_read
	idur	= p3*sr
	isource	= p4
	ipos	= p5
	il	= p6
	ir	= p7
;; generate some position near ipos, between 0 and size of source
	itsize tableng isource ; get the size of the table
	ireadstart	pos idur, itsize, ipos, 0.3
	print ireadstart
;; make a phasor across the table
		aphase	line ireadstart, p3, ireadstart+idur
;; read via the phasor
	asig	= tab:a(aphase, isource, 0)
;; make an envelope matching idur duration
		kenvshape	linen 65, 0.2, idur, 0.2 ; 0 -> 65 -> 0
		kenv		= (65 - kenvshape) ; -65 -> 0 -> -65
;; multiply the table data by the envelope / spacialize
		al		= asig*ampdbfs(kenv+il+gigain)
		ar		= asig*ampdbfs(kenv+ir+gigain)
;; output
	outs al, ar
endin

  sr	= 44100
  kr	= 1
  nchnls	= 2
  0dbfs	= 1

  gigain = -15

instr learn_read
	idur	= p3*sr
	isource	= p4
	ipos	= p5
	il	= p6
	ir	= p7
;; generate some position near ipos, between 0 and size of source
		itsize tableng isource ; get the size of the table
		imaxstart = itsize - idur ; get the max position for this duration
	ivrange = imaxstart ; from zero to this, valid start indexes
		ideviation = 0.3 ; 0-1, factor for how far we deviate from specific position
		ioffset random ideviation*-1, ideviation
		irealpos = (ipos*(1-ideviation))+(ipos*ioffset)
	ireadstart = irealpos*ivrange ; between 0 -> ivrange, near ipos
	print ireadstart
;; make a phasor across the table
	aphase line ireadstart, p3, ireadstart+idur
;; read via the phasor
	asig	= tab:a(aphase, isource, 0)
;; make an envelope matching idur duration
		kenvshape linen 65, 0.2, idur, 0.2 ; 0 -> 65 -> 0
	kenv = (65 - kenvshape) ; -65 -> 0 -> -65
;; multiply the table data by the envelope / spacialize
	al = asig*ampdbfs(kenv+il+gigain)
	ar = asig*ampdbfs(kenv+ir+gigain)
;; output
	outs al, ar
endin

instr learn_write
	idest	= p5
	ipos	= p6
;; some position near ipos, between 0 and size of dest
endin

0dbfs = 1
nchnls = 2

;; TODO - expect params to be in log form, need to expand (ampdb, etc.)
	instr 1
kp_lookup	line 0, p3, 1

itflag		init 1 ; 0 - 1 normalized lookup
kamp		tab kp_lookup, p4, itflag
khz		tab kp_lookup, p5, itflag

asig 		poscil kamp, khz
		outs asig, asig
	endin

RT_ARGS=-Lstdin -+rtaudio=jack -odac -iadc -b1024 -B2048
default: compo.sco
	csound ${RT_ARGS} compo.orc $<

compo.sco: mksco.fnl
	fennel $< > $@

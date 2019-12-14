FENNEL=../fennel-dist/fennel
RT_ARGS=-Lstdin -+rtaudio=jack -odac -iadc -b1024 -B2048

default: compo.sco
	csound ${RT_ARGS} compo.orc $<

compo.sco: mksco.fnl
	${FENNEL} $< > $@

render: abject.mp3

abject.mp3: abject.wav
	lame $<

abject-raw.wav: compo.sco
	csound -W -o $@ compo.orc $<

abject.wav: abject-raw.wav
	sox $< $@ channels 2

repl:
	rlwrap ${FENNEL} --repl repl.fnl

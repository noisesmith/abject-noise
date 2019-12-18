FENNEL=../fennel-dist/fennel

PLATFORM=$(shell uname)
ifeq ($(PLATFORM),Darwin)
  RT=portaudio
else
  RT=jack
endif

RT_ARGS=-Lstdin -+rtaudio=$(RT) -odac -iadc -b1024 -B2048

default: compo.sco
	csound ${RT_ARGS} compo.orc $<

compo.sco: mksco.fnl sco.fnl
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

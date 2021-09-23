listen: test.wav
	mplayer -ao jack test.wav

mp3: polished.mp3

polished.mp3: polished.wav
	lame polished.wav polished.mp3

polished.wav: test.wav
	sox test.wav polished.wav norm

test.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup.sco -o test.wav

test-old.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup-old.sco -o test.wav

cutup.sco: src/noisesmith/compo.clj src/noisesmith/generative.clj
	clojure -m noisesmith.compo > cutup.sco

cutup-old.sco: src/noisesmith/egregore.clj src/noisesmith/generative.clj
	clojure -m noisesmith.egregore > cutup-old.sco

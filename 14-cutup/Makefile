listen: test.wav
	mplayer -ao jack test.wav

mp3: polished.mp3

polished.mp3: polished.wav
	lame polished.wav polished.mp3

polished.wav: test.wav
	sox test.wav polished.wav norm

test.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup.sco -o test.wav

cutup.sco: src/noisesmith/egregore.clj
	clojure -m noisesmith.egregore > cutup.sco

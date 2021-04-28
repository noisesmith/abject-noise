listen: test.wav
	mplayer -ao jack test.wav

test.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup.sco -o test.wav

cutup.sco: src/noisesmith/egregore.clj
	clojure -m noisesmith.egregore > cutup.sco

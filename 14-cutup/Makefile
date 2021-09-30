listen: source-a.wav
	mplayer -ao jack source-a.wav

mp3: polished.mp3

polished.mp3: polished.wav
	lame polished.wav polished.mp3

polished.wav: test.wav
	sox test.wav polished.wav norm

source-a.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup.sco -o source-a.wav

test-old.wav: cs/cutup.orc cutup.sco
	csound cs/cutup.orc cutup-old.sco -o test.wav

cutup.sco: src/noisesmith/source_1.clj src/noisesmith/generative.clj
	clojure --report stderr -m noisesmith.source-1 > cutup.sco

cutup-old.sco: src/noisesmith/egregore.clj src/noisesmith/generative.clj
	clojure --report stderr -m noisesmith.egregore > cutup-old.sco

clean:
	rm -f *.wav *.mp3 *.sco

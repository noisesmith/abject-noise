test0.wav: 0-learner_read.sco learner.orc beats.wav
	csound learner.orc 0-learner_read.sco -o test0.wav && ./show-amp test0.wav

test1.wav: 1-learner_rw.sco learner.orc drone.wav
	csound -i drone.wav learner.orc 1-learner_rw.sco -o test1.wav && ./show-amp test1.wav

test2.wav: 2-learner_rw.sco learner.orc drone.wav
	csound -i drone.wav learner.orc 2-learner_rw.sco -o test2.wav && ./show-amp test2.wav

tests: test0.wav test1.wav test2.wav

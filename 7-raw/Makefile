FORMAT=.aiff
UTIL_INS=tab-copy.ins tab-debug.ins
FX_INS=reverb.ins tab-curve.ins tab-wiggle.ins
GEN_INS=tab-gend.ins
INSTRUMENTS=$(UTIL_INS) $(FX_INS) $(GEN_INS)
SCO=base-tables.sco
GET_AMPS=2>&1 | tee render.log | grep '^B'

soundfile: ramirez.aiff

ramirez.aiff:	ramirez.csd $(INSTRUMENTS) $(SCO)
	csound ramirez.csd -o ramirez$(FORMAT) $(GET_AMPS)


video:	soundfile
	./specvid ramirez.aiff ramirez.mp4

clean:
	rm -f *.wav *.mp4 *.log *.aiff

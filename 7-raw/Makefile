FORMAT=aiff
SF=ramirez.$(FORMAT)
LOGFILE=render.log

UTIL_INS=tab-copy.ins tab-debug.ins
FX_INS=reverb.ins tab-curve.ins tab-wiggle.ins
GEN_INS=tab-gend.ins
INSTRUMENTS=$(UTIL_INS) $(FX_INS) $(GEN_INS)

SCO=base-tables.sco

GET_AMPS=2>&1 | tee $(LOGFILE) | grep '^B'
MESSAGE_DB=-m167

soundfile: ramirez.aiff

log: ramirez.aiff
	less -r $(LOGFILE)

ramirez.aiff:	ramirez.csd $(INSTRUMENTS) $(SCO)
	csound ramirez.csd $(MESSAGE_DB) -o $(SF) $(GET_AMPS)

video:	soundfile
	./specvid ramirez.aiff ramirez.mp4

clean:
	rm -f *.wav *.mp4 *.log *.aiff

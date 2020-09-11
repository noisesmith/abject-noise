FORMAT=.aiff
NAME=ramirez
AUDIO_FILE=$(NAME)$(FORMAT)
VIDEO_FILE=$(NAME).mp4
SOURCE_FILE=$(NAME).csd
MPLAYER_FLAGS=-ao jack 2>/dev/null
UTIL_INS=tab-copy.ins tab-debug.ins
FX_INS=reverb.ins tab-curve.ins tab-wiggle.ins
GEN_INS=tab-gend.ins
INSTRUMENTS=$(UTIL_INS) $(FX_INS) $(GEN_INS)
SCO=base-tables.sco
GET_AMPS=2>&1 | tee render.log | grep '^B'

listen: $(AUDIO_FILE)
	mplayer $(MPLAYER_FLAGS) $(AUDIO_FILE)

watch: $(VIDEO_FILE)
	mplayer $(MPLAYER_FLAGS) $(VIDEO_FILE)

$(AUDIO_FILE):	$(SOURCE_FILE) $(INSTRUMENTS) $(SCO)
	csound $(SOURCE_FILE) -o $(AUDIO_FILE) $(GET_AMPS)

video: $(VIDEO_FILE)

$(VIDEO_FILE):	$(AUDIO_FILE)
	rm $(VIDEO_FILE)
	./specvid $(AUDIO_FILE) $(VIDEO_FILE)

clean:
	rm -f *.wav *.mp4 *.log *.aiff

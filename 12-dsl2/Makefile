PROJECT = noisesmith.csound
PATH_PREFIX = src/noisesmith/csound
COMPOSITION = $(PATH_PREFIX).clj
HELPERS = 
# HELPERS += $(PATH_PREFIX)/generate.clj

test.wav: test.sco
	csound simple.orc test.sco

test.sco: $(COMPOSITION) $(HELPERS)
	clj -m $(PROJECT) | tee test.sco

test: csound-test

csound-test:
	bin/test $(PREFIX).csound-test

clean:
	rm -f test.wav test.sco

PREFIX = org.noisesmith.composition-dsl
PATH_PREFIX = src/org/noisesmith/composition_dsl
COMPOSITION = $(PATH_PREFIX)/composition.clj
HELPERS = $(PATH_PREFIX)/emit.clj
HELPERS += $(PATH_PREFIX)/generate.clj
HELPERS += $(PATH_PREFIX)/operations.clj
HELPERS += $(PATH_PREFIX)/tables.clj

test.wav: test.sco
	csound simple.orc test.sco

test.sco: $(COMPOSITION) $(HELPERS)
	clj -m $(PREFIX).composition | tee test.sco

test: operations-test generate-test emit-test

operations-test:
	bin/test $(PREFIX).operations-test

generate-test:
	bin/test $(PREFIX).generate-test

emit-test:
	bin/test $(PREFIX).emit-test

clean:
	rm test.wav test.sco

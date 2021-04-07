COMPOSITION = src/org/noisesmith/composition_dsl/composition.clj
HELPERS = src/org/noisesmith/composition_dsl/emit.clj
HELPERS += src/org/noisesmith/composition_dsl/generate.clj
HELPERS += src/org/noisesmith/composition_dsl/operations.clj
HELPERS += src/org/noisesmith/composition_dsl/tables.clj

test.wav: test.sco
	csound simple.orc test.sco

test.sco: $(COMPOSITION) $(HELPERS)
	clj -m org.noisesmith.composition-dsl.composition > test.sco

test: operations-test generate-test emit-test

operations-test:
	bin/test org.noisesmith.composition-dsl.operations-test

generate-test:
	bin/test org.noisesmith.composition-dsl.generate-test

emit-test:
	bin/test org.noisesmith.composition-dsl.emit-test


AS=as -g
LD=ld
DEBUG=2>&1 | xxd
TESTS=0-test1 1-test-halfword 2-test-write-hex

tests: $(TESTS)
	./0-test1 && ./1-test-halfword && ./2-test-write-hex && echo 'all passed'

0-test1: 0-test1.o
1-test-halfword: 1-test-halfword.o
2-test-write-hex: 2-test-write-hex.o

%.o: %.s
	$(AS) $< -o $@
%: %.o
	$(LD) $< -o $@

clean:
	rm -f a.out *.o $(TESTS)

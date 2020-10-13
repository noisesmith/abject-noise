AS=as -g
LD=ld

uppercase: uppercase.o
hello: hello.o

%.o: %.s
	$(AS) $< -o $@
%: %.o
	$(LD) $< -o $@
clean:
	rm -f a.out *.o hello uppercase

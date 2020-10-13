AS=as -g
LD=ld

mem-demo.o: example/mem.s
	$(AS) example/mem.s -o mem-demo.o

%.o: %.s
	$(AS) $< -o $@
%: %.o
	$(LD) $< -o $@

clean:
	rm -f a.out *.o life mem-demo

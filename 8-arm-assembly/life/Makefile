AS=as -g
LD=ld

debug: life
	./debugger ./life

life: life.o

life.o: life.s life_data.s life_steps.s debug.s
	$(AS) debug.s life_data.s life_steps.s life.s -o $@

%.o: %.s
	$(AS) $< -o $@
%: %.o
	$(LD) $< -o $@

mem-demo.o: example/mem.s
	$(AS) example/mem.s -o mem-demo.o

clean:
	rm -f a.out *.o life mem-demo

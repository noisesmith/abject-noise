AS=as -g
LD=ld
SOURCES=debug.s life_data.s life_steps.s life.s

debug: life
	./debugger ./life

life: life.o

life.o: $(SOURCES)
	$(AS) $(SOURCES) -o $@

%.o: %.s
	$(AS) $< -o $@
%: %.o
	$(LD) $< -o $@

mem-demo.o: example/mem.s
	$(AS) example/mem.s -o mem-demo.o

clean:
	rm -f a.out *.o life mem-demo

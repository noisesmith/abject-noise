AS=arm-none-eabi-as -g
LD=arm-none-eabi-ld
RUN=qemu-arm -singlestep

life.o: life.s life_data.s
	$(AS) life.s life_data.s -o $@

%.o: %.s
	$(AS) $< -o $@
%.arm: %.o
	$(LD) $< -o $@
%.run: %.arm
	$(RUN) -g 1234 $<

clean:
	rm -f *.o *.arm

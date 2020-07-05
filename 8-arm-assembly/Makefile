%.o: %.s
	arm-none-eabi-as $< -g -o $@
%.arm: %.o
	arm-none-eabi-ld $< -o $@
%.run: %.arm
	qemu-arm -singlestep -g 1234 $<

clean:
	rm -f *.o *.arm

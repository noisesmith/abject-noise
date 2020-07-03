%.o: %.s
	arm-none-eabi-as $< -g -o $@
%.arm: %.o
	arm-none-eabi-ld $< -o $@
%.run: %.arm
	qemu-arm -singlestep -g 1234 $<

#0-pc-demo-run: 0-pc-demo.arm
#	qemu-arm -singlestep -g 1234 $<
# gdb-multiarch executable
# target remote localhost:1234
# layout regs

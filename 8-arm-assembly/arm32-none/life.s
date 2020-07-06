# the game of LIFE by conway, represented in ARM assembler
#
.text
.global _start

_start:
	// push	{r11, lr}	// prologue:	save frame pointer, lr
	// add	r11, sp, #0	//		setting the bottom of the stack frame
	// sub	sp, sp, #16	//		allocating stack
	mov	r0, #1		// set up call: fd to print to
	bl	print_board
	mov	r1, r0		// result of print_board
	// sub	sp, r11, #0	// epilogue: reset the stack pointer
	mov	r0, #0		// exit success
	b	_exit		//
_exit:
	mov r7, #1 // system call register, exit index
	swi #0

print_board: // args: fd
	push	{r11}		// prologue:	save the frame pointer
	add	r11, sp, #0	//		set the bottom of the stack frame
	sub	sp, sp,	#12	//		allocating stack space
	mov	r9, r0		// fd to print to, our only arg
// a loop, we process the byte at one address (8 board locations) per cycle
	ldr	r0, the_board	// loop prelude:	get the board
	ldr	r1, the_buffer	//			get the buffer
	mov	r2, #0		//			initial offset into the board
_do_line:			// loop entry
	mov	r3, #0		// offset within our output line / index into
				// the_buffer, cycles 0-63
_do_byte:
	ldrb	r4, [r0, r2]	// byte from the board
	mov	r5, #1		// mask to get our bit from the byte
_do_bit:
	// fill one
	mov	r7, #'X		// x if bit set
	and	r6, r4, r5	// test via mask
	moveq	r7, #' 		// space character if bit not set
	lsl	r5, #1		// next bit
	cmp	r5, #0b100000000 // see if we are done with this byte, 9th bit set
	bne	_do_bit
	strb	r7, [r1, r3]	// set buffer
	add	r3, #1		// next position in out buffer
	add	r2, #1		// next input byte
	cmp	r3, 64
	// conditional eq print 65 bytes (line)
	beq	_do_line
	cmp	r2, #512
	bne	_do_byte	// only need to do this explicitly if we are
				// not jumping to the next line (falls through)
	// fall through when done
	add	sp, r11, #0	//	epilogue:	adjusting the stack pointer
	pop	{r11}		//			restoring the frame pointer
	bx	lr		//			jump back via lr
	
	

the_board: .word board
the_buffer: .word print_buffer

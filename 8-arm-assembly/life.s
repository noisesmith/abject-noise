# the game of LIFE by conway, represented in ARM assembler
#
.data
board:	.space 512 // 64 x 8 byte longs

.text
.global start

_start:
	push	{r11, lr}   // prologue: save frame pointer, lr
	add	r11, sp, #0 // 	         setting the bottom of the stack frame
	sub	sp, sp, #16 //		 allocating stack
	mov	r0, #1      // set up call: fd to print to
	bl	print_board
	sub	sp, r11, #0 // epilogue: reset the stack pointer
	// pop	{r11, pc}   // 		 restore the frame pointer from the stack
	b	_exit        // exit with the value returned by print_board (r0)
	// mov	r1, #0x290
	// add	r1, r1, #0xa // since 666 won't fit in a single numeric literal
	// ldr	r2, the_board
	// str	r1, [r2] // store 666 into the first cell
	// ldr	r0, [r2] // load 666 from that same location
	// mov	r0, r2
_exit:
	mov r7, #1 // system call register, exit index
	swi #0

print_board: // args: fd
      push	{r11}		// prologue: save the frame pointer
      add	r11, sp, #0	//	     set the bottom of the stack frame
      sub	sp, sp,	#12	//	     allocating stack space
      mov	r9, r0		// fd to print to, our only arg
      ldr	r2, the_board   // get the board
 // a loop, we process one address (N board locations) per cycle
      mov	r0, #0		// prelude to loop - r0 holds inner index
      mov	r1, #0		//		   - r1 holds the outer index
_do_N:
_do_one: // inner loop, for each bit of the board
      // TODO - maybe instead of a nested loop we want a subindex that uses modulo
      // ldrb	rN, [r2, r0, r1] // one byte, with indexing
      add	sp, r11, #0	// epilogue: adjusting the stack pointer
      pop	{r11}		//	     restoring the frame pointer
      bx	lr		// 	     jump back via lr
	
	

the_board: .word board

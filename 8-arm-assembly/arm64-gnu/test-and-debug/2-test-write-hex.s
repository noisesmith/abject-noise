/* attempt to put the contents of a number into a buffer as hex */
.macro debug reg, prefix
	/* save data from caller */
	sub	sp, sp, #040
	stnp	x0, x1, [sp]
	stnp	x2, x3, [sp, #020]
	str	x8, [sp, #040]
	/* set up data to print on stack */
	sub	sp, sp,#020
	mov	x3, \prefix
	str	x3, [sp]
	str	\reg, [sp, #010]
	mov	x0, #2
	mov	x1, sp
	mov	x2, #16
	mov	x8, #64
	svc	0
	add	sp, sp,#020
	/* restore data from caller */
	ldr	x8, [sp, #040]
	ldnp	x2, x3, [sp, #020]
	ldnp	x0, x1, [sp]
	sub	sp, sp, #040
	/* debug */
.endm

.global _start
_start:
						//; set up data for conversion
	ldr	x1, =constant			//; addr of answer, in 64 bits
	ldr	x1, [x1]			//; the answer
	//; x1 is correct
	ldr	x2, =buffer			//; buffer to write to
	ldr	x3, =buffer_end			//; end of buffer to write to
	subs	x3, x3, #1			//; don't write over the newline
	sub	x5, x3, x4			//; size of the output buffer - write offset
	mov	x6, #15				//; index of nibble (0 < n <= top)
convert:					//;   extracting 16 nibbles (16*4) 64 bits
	lsl	x7, x6, #2			//; go from nibbles to bits
	add	x8, x7, #-64			//; subtract from 64 for num left bits to clear
	lsl	x7, x1, x8			//; clear out to the left
	lsr	x8, x7, #(64-4)			//; isolate a nibble as the lowest
	add	x7, x8, #'0'			//; get ascii for number
	add	x8, x7,  #('a' - '9' - 1)
	cmp	x7, #('9' + 1)
	csel	x7, x7, x8, lt
	add	x8, x6, #2			//; provides room for the 0x prefix
	strb	w7, [x2, x8]
	subs	x6, x6, 1
	cmp	xzr, x6
	b.le	convert

	//; set up verification
	ldr	x1, =expected
	ldr	x2, =buffer
	mov	x3, #0			//; index in buffers
	ldr	x4, =buffer_end
	sub	x4, x4, x2		//; total size to verify
verify_loop:
	ldrb	w4, [x1, x3]		//; the value expected
	ldrb	w0, [x2, x3]		//; the value written
	cmp	x0, x4			//; we should have the right byte here
	b.ne	exit_fail		//; jump to failure if they are not the same
	add	x3, x3, #1
	cmp	x3, x4
	b.lt	verify_loop
	mov	x0,#0 			//; success
	mov	x8,#93			//; exit
	svc	0
exit_fail:
	add	x5, x3, #1		//; index of failure, +1 for non-zero output
	mov	x8,#64			//; write
	mov	x0,#2			//; stderr
	ldr	x1,=failure		//; message
	ldr	x2,=done		//; end of message
	sub	x2, x2, x1		//; byte count
	svc	0
	mov	x0, #2			//; stderr
	ldr	x1, =display_expected	//; expected message
	ldr	x2, =expected_end
	sub	x2, x2, x1
	svc	0
	mov	x0, #2			//; stderr
	ldr	x1, =display_buffer	//; actual message
	ldr	x2, =buffer_end
	sub	x2, x2, x1
	svc	0
	mov	x8,#93
	mov	x0,x5
	svc	0

.data
failure: .ascii "verificatin failure in test-write-hex.s\n"
done:
display_buffer: .ascii "actual: "
buffer: .ascii "0xFAILURE!FAILURE!\n"
buffer_end:
display_expected: .ascii "expected: "
expected: .ascii "0x0123456789abcdef\n"
expected_end:
constant: .quad 0x0123456789abcdef //;0xfedfedfedfedfedf //;

//; optional main, for debugging
/* .text                                   	*/
/* .global _start                          	*/
/* _start:                                 	*/
/* 	ldr	x0, =constant              	*/
/* 	ldr	x0, [x0]                   	*/
/* 	mov	x1, #1                     	*/
/* 	mov	x2, '\n'                   	*/
/* 	bl	_write_hex                 	*/
/* 	mov	x8, #93                    	*/
/* 	mov	x0, #0                     	*/
/* 	svc	0                          	*/
/* .data                                   	*/
/* constant: .quad 0xfeedbeefcafebabe      	*/

.text
.global _write_hex
_write_hex:	//; prints the hex repr of the arg
		//; x0 thing to print
		//; x1 port to print to
		//; w2 suffix
	//; sub	sp, sp, #020
	//; str	lr, [sp]
	ldr	x3, =buffer			//; buffer to write to
	ldr	x4, =buffer_end			//; end of buffer to write to
	add	x3, x3, #2			//; don't write over the prefix
	sub	x4, x4, #1			//; don't write over the suffix
	mov	x5, #15				//; index of nibble (0 < n <= top)
						//; because the number is little-endian
						//; but humans write numbers big-endian

convert:					//;   extracting 16 nibbles (16*4) 64 bits
	lsl	x6, x5, #2			//; go from nibbles to bits
	add	x6, x6, #-64			//; subtract from 64 for num left bits to clear
	lsl	x6, x0, x6			//; clear out to the left
	lsr	x6, x6, #(64-4)			//; isolate a nibble as the lowest

	add	x7, x6, #'0'			//; get ascii for number
	add	x8, x6, #('a' - 10)		//; or a-f if over 9
	cmp	x6, #10
	csel	x6, x7, x8, lt			//; pick 0-9 or a-f
	strb	w6, [x3, x5]			//; store the byte
	subs	x5, x5, 1			//; move backward (little endian!)
	cmp	xzr, x5
	b.le	convert

	strb	w2, [x4, #1]			//; set the suffix
	ldr	x3, =buffer
	ldr	x4, =buffer_end
						//; syscall args
	mov	x0, x1				//; set the port
	mov	x1, x3				//; set the data buffer
	sub	x2, x4, x3			//; set the byte count
	mov	x8, #64
	svc	0
	//; ldr	lr, [sp]
	//; add	sp, #020
	ret

.data
buffer: .ascii "0x___-___-___-___-\n"
buffer_end:

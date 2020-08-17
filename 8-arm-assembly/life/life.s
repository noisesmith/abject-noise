// the game of LIFE by conway, represented in ARM assembler
.text
.global _start
_start: // {{{1
	sub	sp, sp, #020
	str	lr, [sp]
	mov	x0, #0				//; rule iterations
	str	x0, [sp, #010]
_iterate_rules: // {{{2
		mov	x0, #1			//; set up print_board call
		ldr	x1, =debug_board		//; ...
		mov	x2, #64			//; ...
		mov	x3, #64			//; ...
	bl	print_board
		ldr	x0, =normal_rules	//; set up walk_board call
		ldr	x1, =debug_board		//; ...
		mov	x2, #64			//; ...
		mov	x3, #64			//; ...
	bl	walk_board
		ldr	x0, [sp, #010]
		add	x0, x0, #1
		str	x0, [sp, #010]
		cmp	x0, #88
		b.lt	_iterate_rules
	ldr	lr, [sp]
	//; mov	x1, x0			//; result of print_board
	mov	x0, #0			//; exit success
_exit: // {{{2
	ldr	lr, [sp]
	add	sp, sp, #020
	mov	x8, #93			//; system call exit
	svc 0

print_board:	//; x0: output port // {{{1
		//; x1: data location
		//; x2: columns (in bits)
		//; x3: rows
		//; -> x0 success
	sub	sp, sp, #060
	str	lr, [sp]
	stp	x0, x1, [sp, #010]		//; out
	stp	x2, x3, [sp, #030]		//; cols, rows (unused)
	      mul	x4, x2, x3		//; size of board in bits
	      mov	x5, #0			//; read index in quads
	stp	x4, x5, [sp, #050]		//; total bits, idx
_next_line:				// {{{2
	ldr	x0, [sp, #060]		//; position in quads
	      lsl	x1, x0, #3		//; quads to bits
	      ldr	x2, [sp, #050]		//; size in bits
	cmp	x1, x2			//; have we hit the size?
	b.ge	_last_line
	      ldr	x1, [sp, #020]		//; data location
	      add	x1, x1, x0
	ldp	x2, x3, [x1]		//; next quad, and the one after
	      add	x0, x0, #8
	str	x0, [sp, #060]
	      mov	x0, x2			//; setting up process_line call
	      mov	x1, x3
	      mov	x2, #0			//; TODO - eventually this needs calculation
	      mov	x3, #64			//; TODO - eventually this needs calculation
	      ldr	x4, =print_buffer
	bl	process_line

					//; printing time
	ldr	x1, =print_buffer
	ldr	x2, [sp, #030]
	add	x2, x2, #1
	ldr	x0, [sp, #010]		//; the out stream
	mov	x8, #64
	svc 0

	b	_next_line
_last_line:				// {{{2
	ldr	lr, [sp]
	add	sp, sp, #060
	mov	x0, #0
	ret

process_line:	//; x0,x1 - data - consumed as a contiguous array of bits // {{{1
		//; x2 - offset within data to start
		//; x3 - count of bits to consume
		//; x4 - buffer to fill
		//; -> x0 success
	sub	sp, sp, #020
	str	lr, [sp]

	mov	x5, x0			//; which data source currently accessed
	mov	x6, x1			//; position in that source
	mov	x7, #0			//; bits consumed

_fill_cell:				// {{{2
//; upgrade to next data source?
	//;add	x6, x6, #1
	//;cmp	x6, #64
	//;csel	x6, xzr, x6, eq		//; move to offset zero after 64
	//;csel	x5, x0, x1, eq		//; move to next data source after 64
//; write 'X' or ' ' to output and increment
	mov	x0, x5
	mov	x1, x7
	bl	single_bit
	strb	w0, [x4, x7]
//; recur
	add	x7, x7, #1
	cmp	x7, x3
	b.lt	_fill_cell
	mov	w8, #'\n'		//; fill one more, newline
	//; little endian, so reverse the last four bits
	strb	w8, [x4, x7]
_exit_process_line:			//; {{{2
	ldr	lr, [sp]
	add	sp, sp, #020
	ret

single_bit: //; x0 a 64 bit value	//; {{{1
	    //; x1 a value 1-64
	    //; -> x0 '.' or 'X' based on logical order of bits in a quad

	rev	x0, x0			//; reverse the bytes
	rbit	x0, x0			//; reverse the bits
	mov	x9, #1			//; create the mask
	lsl	x10, x9, x1		//; offset mask by index
	mov	w8, #'.'
	mov	w9, #'X'
	ands	x12, x0, x10		//; test current bit
	csel	w0, w8, w9, eq		//; select printable symbol based on zero result

	ret

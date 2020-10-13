.global normal_rules
.global walk_board

normal_rules:	//; x0: cell current	{{{1
		//; x1 ... x8: neighbors starting above, clockwise
		//;
		//; x8  x1 x2
		//; x7  x0 x3
		//; x6  x5 x4
		//; -> x0 gets new state of cell (0/1)
      sub	sp, sp, #020		// {{{2 stack setup
      str	lr, [sp]
      mov	x9, #0			//; live neighbor count
      cmp	x1, #0
      cinc	x9, x9, ne		//; increment live neighbor count for positive x1
      cmp	x2, #0
      cinc	x9, x9, ne
      cmp	x3, #0
      cinc	x9, x9, ne
      cmp	x4, #0
      cinc	x9, x9, ne
      cmp	x5, #0
      cinc	x9, x9, ne
      cmp	x6, #0
      cinc	x9, x9, ne
      cmp	x7, #0
      cinc	x9, x9, ne
      cmp	x8, #0
      cinc	x9, x9, ne
      cmp	x9, #3
      b.eq	_live			//; 3 neighbors -> live (1)
      cmp	x9, #2
      b.eq	_done			//; 2 neighbors -> unchanged, else dead
      mov	x0, #0
      ldr	lr, [sp]
      add	sp, sp, #020
      ret
_live:		// {{{2
      mov	x0, #1
_done:		// {{{2
      ldr	lr, [sp]
      add	sp, sp, #020		// {{{2 stack teardown
      ret

//; gathers neighbor cells into registers, passes them to the rule function in x0
//; updates the board in that position based on the return value
walk_board:	//; x0: rule function (takes cell + neighbors, returns 1/0 // {{{1
		//; x1: the storage for the board
		//; x2: columns (in bits) / currently only 64 supported
		//; x3: rows
		//; -> x0 success
// stack offsets {{{2
.equ	rule_function, 010
.equ	storage, 020
.equ	columns, 030
.equ	rows, 040
.equ	row, 050
.equ	column, 060
.equ	prev_row, 070
.equ	this_row, 0100
.equ	next_row, 0110
.equ	scratch_row, 0120
.equ	column_mask, 0130
.equ	total_stack, 0140
	sub	sp, sp, total_stack // stack setup {{{2
	str	lr, [sp]
	stp	x0, x1, [sp, rule_function]
	stp	x2, x3, [sp, columns]

	mov	x5, #0			//; row index
	mov	x6, #0			//; column index
	stp	x5, x6, [sp, row]
_one_row:	// {{{2
	ldr	x0, [sp, storage]
	ldp	x2, x1, [sp, rows]	//; get the max and current row index
	lsl	x1, x1, #3		//; bytes to words offset

	bl	rows

	stp	x0, x1, [sp, prev_row]	//; previous and current row, stored
	stp	x2, x1, [sp, this_row]	//; next and scratch row, stored


	//; build up args to rules function
_one_column:	// {{{2
	ldr	x0, [sp, column]	//; column index
	ldr	x1, [sp, columns]

	bl	column_masks		//; x0: prev, x1: current, x2: next columns
	str	x1, [sp, column_mask]

	ldp	x9, x10, [sp, prev_row]	//; get the prev, current row data
	ldr	x11, [sp, next_row]	//; get the next row data

	//; set the registers for the call
	//;   one for each of 9 cells in a square
	and	x8, x9, x0	//; upper left cell
	and	x7, x10, x0	//; left cell
	and	x6, x11, x0	//; lower left
	and	x5, x11, x1	//; below
	and	x4, x11, x2	//; lower right
	and	x3, x10, x2	//; right
	and	x2, x9, x2	//; upper right
	and	x1, x9, x1	//; above
	and	x0, x10, x0	//; current

	ldr	x9, [sp, rule_function]
	blr	x9			//; call rule function

	ldr	x1, [sp, column_mask]
	cmp	x0, #0			//; did the rules return zero?
	csel	x1, xzr, x1, eq		//; if so, we combine with zero
					//; otherwise, the column mask
	ldr	x2, [sp, scratch_row]	//; retrieve the next value of current row
	orr	x3, x2, x1		//; set bit if rule set bit
	str	x3, [sp, scratch_row]		//; save the new row

	ldr	x6, [sp, column]	//; get old column index
	add	x6, x6, #1		//; next column
	str	x6, [sp, column]	//; store new row index
	ldr	x3, [sp, columns]
	cmp	x6, x3
	b.lt	_one_column

	//; TODO - store the right number of new rows, de-scrambled and correctly calculated
	ldr	x2, [sp, scratch_row]		//; get the newly calculated row
	ldr	x5, [sp, row]
	lsl	x6, x5, #3		//; bytes to words, x8
	ldr	x3, [sp, storage]

	str	x2, [x3, x6]		//; store new row
	add	x5, x5, #1		//; next row
	str	x5, [sp, row]
	ldr	x4, [sp, rows]		//; max row index
	cmp	x5, x4
	b.lt	_one_row

	ldr	lr, [sp]
	add	sp, sp, total_stack // {{{2 stack teardown
	ret


column_masks:	//; x0: column index 	// {{{1
		//; x1: max column
		//; --> x0, x1, x2 - masks for prev, current, next column
	sub	sp, sp, #020 // stack setup {{{2
	str	lr, [sp]

// data setup {{{2
	//; some registers to reuse
	mov	x3, x0			//; our index
	sub	x4, x1, #1		//; width to max column
	mov	x11, #1			//; single bit set, for masking with

// calculation {{{2
	//; previous column // {{{3
	sub	x0, x3, #1		//; previous column offset
	cmp	x0, xzr
	csel	x0, x0, x4, ge		//; if below 0, grab the max
	lsl	x0, x11, x0		//; turn column offset into bit mask

	//; current column // {{{3
	lsl	x1, x11, x3

	//; next column // {{{3
	add	x2, x3, #1		//; next column offset
	cmp	x2, x4			//; check offset against max
	csel	x2, x2, xzr, lt		//; index 0 if over the max
	lsl	x2, x11, x2		//; turn index into mask

	ldr	lr, [sp]
	add	sp, sp, #020 // stack teardown {{{2
	ret

rows:		//; x0: row storage	// {{{1
		//; x1: row index
		//; x2: max row
		//; --> x0, x1, x2 - prev, current, next row
	sub	sp, sp, #020 // stack setup {{{2
	str	lr, [sp]

// data setup {{{2
	//; some set registers to reuse
	mov	x5, x0
	mov	x6, x1
	sub	x3, x2, #1		//; maximum row offset

// calculation {{{2
	//; previous row // {{{3
	sub	x4, x6, #1		//; get the previous row index
	cmp	x4, xzr
	csel	x4, x4, x3, lo		//; rotate to max offset if offset was below 0
	ldr	x0, [x5, x4]		//; get the data at the previous row
	//;rev	x0, x0			//; un-little-endian
	//;rbit	x0, x4

	//; current row // {{{3
	ldr	x1, [x5, x6]
	//;rev	x1, x1
	//;rbit	x1, x1

	//; next row // {{{3
	add	x4, x6, #1
	cmp	x4, x3			//; see if it's greater than the max row
	csel	x4, x4, xzr, ge		//; wrap to 0 if yes
	ldr	x2, [x5, x4]		//; grab the row
	//;rev	x2, x2			//; un-little-endian
	//;rbit	x2, x2

	ldr	lr, [sp]
	add	sp, sp, #020 // stack teardown {{{2
	ret


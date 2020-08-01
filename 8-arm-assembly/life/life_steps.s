.global normal_rules
.global walk_board

normal_rules:	//; x0: cell current
		//; x1 ... x8: neighbors starting above, clockwise
		//;
		//; x8  x1 x2
		//; x7  x0 x3
		//; x6  x5 x4
		//; -> x0 gets new state of cell (0/1)
      sub	sp, sp, #020
      str	lr, [sp]
      mov	x9, #0			//; live neighbor count
      cmp	x1, #0
      cinc	x9, x9, gt		//; increment live neighbor count for positive x1
      cmp	x2, #0
      cinc	x9, x9, gt
      cmp	x3, #0
      cinc	x9, x9, gt
      cmp	x4, #0
      cinc	x9, x9, gt
      cmp	x5, #0
      cinc	x9, x9, gt
      cmp	x6, #0
      cinc	x9, x9, gt
      cmp	x7, #0
      cinc	x9, x9, gt
      cmp	x8, #0
      cinc	x9, x9, gt
      cmp	x9, #3
      b.eq	_live			//; 3 neighbors -> live (1)
      cmp	x9, #2
      b.eq	_done			//; 2 neighbors -> unchanged, else dead
      mov	x0, #0
      ldr	lr, [sp]
      add	sp, sp, #020
      ret
_live:
      mov	x0, #1
_done:
      ldr	lr, [sp]
      add	sp, sp, #020
      ret

//; gathers neighbor cells into registers, passes them to the rule function in x0
//; updates the board in that position based on the return value
walk_board:	//; x0: rule function (takes cell + neighbors, returns 1/0
		//; x1: the storage for the board
		//; x2: columns (in bits) / currently only 64 supported
		//; x3: rows
		//; -> x0 success
	sub	sp, sp, #0120
	str	lr, [sp]
	stp	x0, x1, [sp, #010]
	stp	x2, x3, [sp, #030]

	mov	x5, #0			//; row index
	mov	x6, #0			//; column index
	stp	x5, x6, [sp, #050]
_one_row:
	ldr	x0, [sp, #020]		//; get board storage
	ldr	x1, [sp, #050]		//; get the row index
	ldr	x2, [sp, #040]		//; get the max row index

	bl	rows

	stp	x0, x1, [sp, #070]		//; previous and current row, stored
	stp	x2, x1, [sp, #0100]		//; next and scratch row, stored

	ldr	x5, [sp, #050]		//; get the row index
	add	x6, x5, #1		//; calculate the next row index
	str	x6, [sp, #050]		//; store the next row index


	//; build up args to rules function
_one_column:
	ldr	x0, [sp, #060]		//; column index
	ldr	x1, [sp, #030]		//; max column

	bl	column_masks		//; x0: prev, x1: current, x2: next columns

	ldp	x9, x10, [sp, #070]		//; get the prev, current row data
	ldr	x11, [sp, #0100]			//; get the next row data

	//; set the registers for the call
	//;   one for each of 9 cells in a square
	and	x8, x9, x0			//; upper left cell
	and	x7, x10, x0			//; left cell
	and	x6, x11, x0			//; lower left
	and	x5, x11, x1			//; below
	and	x4, x11, x2			//; lower right
	and	x3, x10, x2			//; right
	and	x2, x9, x2			//; upper right
	and	x1, x9, x1			//; above
	and	x0, x10, x0			//; current

	ldr	x9, [sp, #010]		//; retrieve rule function
	blr	x9			//; call rule function

	//; TODO - use x0 from rule function call to update row in progress

	ldr	x6, [sp, #060]		//; column index
	add	x6, x6, #1		//; next column
	str	x6, [sp, #060]		//; store new row index
	ldr	x3, [sp, #030]		//; max column
	cmp	x6, x3
	b.lt	_one_column

	ldr	x5, [sp, #050]		//; row index
	add	x5, x5, #1		//; next row
	str	x5, [sp, #050]
	ldr	x4, [sp, #040]		//; max row index
	cmp	x5, x4
	b.lt	_one_row

	ldr	lr, [sp]
	add	sp, sp, #0120
	ret


column_masks:	//; x0: column index
		//; x1: max column
		//; --> x0, x1, x2 - masks for prev, current, next column
	sub	sp, sp, #020
	str	lr, [sp]

	//; some registers to reuse
	mov	x3, x0			//; our index
	sub	x4, x1, #1		//; width to max column
	mov	x11, #1			//; single bit set, for masking with

	//; previous column
	sub	x0, x3, #1		//; previous column offset
	cmp	x0, xzr
	csel	x0, x0, x4, ge		//; if below 0, grab the max
	lsl	x0, x11, x0		//; turn column offset into bit mask

	//; current colun
	lsl	x1, x11, x3

	//; next column
	add	x2, x3, #1		//; next column offset
	cmp	x2, x4			//; check offset against max
	csel	x2, x2, xzr, lt		//; index 0 if over the max
	lsl	x2, x11, x2		//; turn index into mask

	ldr	lr, [sp]
	add	sp, sp, #020
	ret

rows:		//; x0: row storage
		//; x1: row index
		//; x2: max row
		//; --> x0, x1, x2 - prev, current, next row
	sub	sp, sp, #020
	str	lr, [sp]

	//; some set registers to reuse
	mov	x5, x0
	mov	x6, x1
	sub	x3, x2, #1		//; maximum row offset

	//; previous row
	sub	x4, x6, #1		//; get the previous row index
	cmp	x4, xzr
	csel	x4, x4, x3, lo		//; rotate to max offset if offset was below 0
	ldr	x0, [x5, x4]		//; get the data at the previous row
	rev	x0, x0			//; un-little-endian
	rbit	x0, x4

	//; current row
	ldr	x1, [x5, x6]
	rev	x1, x1
	rbit	x1, x1

	//; next row
	add	x4, x6, #1
	cmp	x4, x3			//; see if it's greater than the max row
	csel	x4, x4, xzr, ge		//; wrap to 0 if yes
	ldr	x4, [x5, x4]		//; grab the row
	rev	x4, x4			//; un-little-endian
	rbit	x2, x4

	ldr	lr, [sp]
	add	sp, sp, #020
	ret


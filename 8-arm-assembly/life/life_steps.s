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
_one_row:	//; x4 <- 64 cells of previous row (previous with wrap)
		//; x5 <- 64 cells of row
		//; x6 <- 64 cells of next row

		//; x15 <- current row offset
	ldr	x15, [sp, #050]		//; get the row index
	ldr	x0, [sp, #020]
	sub	x4, x15, #1		//; get the previous row index
		//; x3 <- maximum row offset
	ldr	x3, [sp, #040]		//; get the maximum row count
	sub	x3, x3, #1		//; turn into maximum offset
	cmp	x4, xzr
	csel	x4, x4, x3, lo		//; rotate to max offset if offset was below 0
	ldr	x4, [x0, x4]		//; get the data at the previous row
	rev	x4, x4			//; un-little-endian
	rbit	x4, x4
	ldr	x5, [x0, x15]		//; get the data at our row
	rev	x5, x5			//; un-little-endian
	rbit	x5, x5
	add	x6, x15, #1		//; get the next row index
	str	x6, [sp, #050]
	cmp	x6, x3			//; see if it's greater than the max row
	csel	x6, x6, xzr, ge		//; wrap to 0 if yes
	ldr	x6, [x0, x6]		//; grab the row
	rev	x6, x6			//; un-little-endian
	rbit	x6, x6
	stp	x4, x5, [sp, #070]		//; previous and current row, stored
	stp	x6, x5, [sp, #0100]		//; next and scratch row, stored

	//; build up args to rules function
_one_column:	//; x0 <- mask for previous column
		//; x1 <- mask for current column
		//; x2 <- mask for next column
	ldr	x7, [sp, #060]		//; column index
	subs	x0, x7, #1		//; previous column
		//; x8 <- max column
	ldr	x8, [sp, #030]		//; max column
	sub	x8, x8, #1		//; max offset of column
	cmp	x0, xzr
	csel	x0, x0, x8, ge		//; if below 0, grab the max
	//;	x11 <- a single bit for shift / mask
	mov	x11, #1			//; single bit set
	lsl	x0, x11, x0		//; turn column offset into bit mask
	lsl	x1, x11, x7		//; current column mask
	add	x2, x7, #1		//; next column
	cmp	x2, x8			//; check offset against max
	csel	x2, x2, xzr, lt		//; index 0 if over the max
	lsl	x2, x11, x2		//; turn index into mask

	add	x7, x7, #1		//; next column
	str	x7, [sp, #060]		//; store state

	//; the building blocks are acquired,
	//; build the call to the rule function via bit masks
	//; x9 <- previous row, as bits
	//; x10 <- current row, as bits
	//; x11 <- next row, as bits
	ldp	x9, x10, [sp, #070]		//; get the prev, current row data
	ldr	x11, [sp, #011]			//; get the next row data

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
	//; TODO - use x0 to update row in progress
	ldr	x6, [sp, #070]		//; index across board
	ldr	x3, [sp, #040]		//; max row count
	add	x6, x6, #1
	str	x6, [sp, #070]
	cmp	x6, x3
	b.lt	_one_column
	ldr	x5, [sp, #060]
	ldr	x2, [sp, #030]
	add	x5, x5, #1
	str	x5, [sp, #060]
	cmp	x5, x2
	b.lt	_one_row

	ldr	lr, [sp]
	add	sp, sp, #0120
	ret

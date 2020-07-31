.global normal_rules
.global normal_rules_stack
.global walk_board
.global walk_board_stack

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
      mov	x10, #1
      cmp	x9, #3
      b.eq	_live			//; 3 neighbots -> live (1)
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

normal_rules_stack: //; x0 cell current
		    //; x1 memory locations with the 8 neighbors, starting with above
		    //;
		    //; p+7 p+0 p+1
		    //; p+6 x0  p+2
		    //; p+5 p+4 p+3
	sub	sp, sp, #020
	str	lr, [sp]
	mov	x2, #0				//; offset
	mov	x3, #0				//; total
_collect_cells:
	ldr	x4, [sp, x2]
	add	x2, x2, #1
	cmp	x4, #0
	cinc	x3, x3, gt
	cmp	x3, #4
	b.gt	_norm_dead
	cmp	x2, #8
	b.le	_collect_cells
	cmp	x3, #2
	b.eq	_norm_live
	cmp	x3, #3
	b.eq	_norm_same
_norm_dead:	//; fallback, all cases but 2/3 neighbors
	ldr	lr, [sp]
	add sp, sp, #020
	mov x0, 0
	ret
_norm_same:
	ldr	lr, [sp]
	add	sp, sp, #020
	ret
_norm_live:
	ldr	lr, [sp]
	add	sp, sp, #020
	mov	x0, #1
	ret

//; gathers neighbor cells into registers, passes them to the rule function in x0
//; updates the board in that position based on the return value
walk_board:	//; x0: rule function (takes cell + neighbors, returns 1/0
		//; x1: the storge for the board
		//; x2: columns (in bits) / currently only 64 supported
		//; x3: rows
		//; -> x0 success
	sub	sp, sp, #0100
	str	lr, [sp]
	stp	x0, x1, [sp, #010]
	stp	x2, x3, [sp, #030]

	mov	x5, #0			//; row index
	mov	x6, #0			//; column index
	mov	x7, #63
	mov	x8, #8
	mul	x7, x7, x8

_one_row:
	ldr	x12, [x1, x7]		//; get the first 64 cells of the last row
	rev	x12, x12
	rbit	x12, x12		//; un-little-endian
	//; the target this time
	ldr	x13, [x1, #0]		//; get the first 64 cells of the first row (target)
	rev	x13, x13
	rbit	x13, x13
	//; then the next
	ldr	x14, [x1, #1]		//; get the first 64 cells of the second row
	rev	x14, x14
	rbit	x14, x14

	str	x13, [sp, #050]		//; scratch row - will contain the new values
	//; build up args to rules function

_one_column:
	mov	x8, #1			//; used in constructing a series of masks

	sub	x15, x6, #1		//; left of cell
	cmp	x15, #0
	csel	x15, x15, x2, lt
	lsl	x15, x8, x15		//; turn index into mask

	lsl	x16, x8, x6		//; column of cell

	add	x17, x6, #1		//; right of cell
	cmp	x17, x2
	csel	x17, x17, xzr, eq
	lsl	x17, x8, x17

	add	x6, x6, #1
	str	x5, [sp, #060]
	str	x6, [sp, #070]

	and	x0, x13, x16		//; cell
	and	x1, x12, x16		//; above
	and	x2, x12, x17		//; upper right
	and	x3, x13, x17		//; right
	and	x4, x14, x17		//; lower right
	and	x5, x14, x16		//; below
	and	x6, x14, x15		//; lower left
	and	x7, x13, x15		//; left
	and	x8, x12, x15		//; upper left

	ldr	x9, [sp, #010]		//; retrieve rule function
	blr	x9			//; call rule function
	ldr	x6, [sp, #070]		//; index across board
	ldr	x3, [sp, #040]
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
	add	sp, sp, #0100
	ret

//; like walk_board, but passes args as adjacent values in memory (stack)
walk_board_stack:	//; x0: rule function (takes cell + neighbors, returns 1/0
		//; x1: the storge for the board
		//; x2: columns (in bits)
		//; x3: rows
		//; -> x0 success
	sub	sp, sp, #020
	str	lr, [sp]
	mov	x9, x0				//; set aside walking function
	//; build up args to rules function
	blr	x9
	ldr	lr, [sp]
	add	sp, sp, #020
	ret

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
	sub	sp, sp, #060
	str	lr, [sp]
	stp	x0, x1, [sp, #010]
	stp	x2, x3, [sp, #030]
	mov	x5, #0			//; row index
	mov	x6, #0			//; column index
	sub	x3, x3, #1		//; the next to last row
	lsr	x2, x2, #6
	mul	x0, x2, x3		//; the first cell in the last row
	ldr	x8, [x1, x0]		//; get the first 64 cells of the last row
	ldr	x9, [x1, #0]		//; get the first 64 cells of the first row (target)
	ldr	x10, [x1, #1]		//; get the first 64 cells of the second row
	str	x9, [sp, #050]		//; scratch row - will contain the new values
	//; build up args to rules function
	mov	x11, x6			//; calculate my offset from left (using x6 / mod x2)
	sub	x11, x11, #1		//; to the left of our index
	and	x11, x11, #0b111111	//; 0-63 only - aka #0x3f
	debug	x11, #1
	mov	x12, #1			//; single set bit to shift
	lsr	x11, x12, x11		//; shift by column
	debug	x11, #2
	and	x13, x11, x8
		//; AND with row(n)
		//; put into register (zero / non-zero suffices)
	ldr	x9, [sp, #010]		//; retrieve, call walking function
	blr	x9
	ldr	lr, [sp]
	add	sp, sp, #060
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

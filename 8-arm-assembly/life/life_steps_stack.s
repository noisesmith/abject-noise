.global normal_rules
.global walk_board

normal_rules: //; x0 cell current
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

//; like walk_board, but passes args as adjacent values in memory (stack)
walk_board:	//; x0: rule function (takes cell + neighbors, returns 1/0
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

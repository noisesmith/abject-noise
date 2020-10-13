/* attempt to get the two half words out of a byte */
.global _start
_start:
	mov	x1, #0xaf
	lsl	x2, x1, #(64-4)
	lsr	x2, x2, #(64-4)
	mov	x0, x2			//; thing to test, and return value on failure
	cmp	x0, #0xf		//; we should have just the smaller half word
	b.ne	exit_fail
	lsl	x2, x1, #(64-8)		//; now we get the second (next higher) half would
	lsr	x2, x2, #(64-4)
	mov	x0, x2
	cmp	x0, #0xa
	b.ne	exit_fail
	mov	x0,#0 			//; success
	mov	x8,#93			//; exit
	svc	0
exit_fail:
	mov	x5,x0			//; stash failure code
	mov	x8,#64			//; write
	mov	x0,#2			//; stderr
	ldr	x1,=failure		//; message
	ldr	x2,=done		//; end of message
	sub	x2, x2, x1		//; byte count
	svc	0
	mov	x8,#93
	mov	x0,x5
	svc	0

.data
failure: .ascii "Failure in test-halfword.s\n"
done:

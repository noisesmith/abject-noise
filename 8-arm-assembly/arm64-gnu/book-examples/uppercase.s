.global _start
_start:
	ldr x4, =input
	ldr x3, =output
loop:
	ldrb	w5, [x4], #1	// read and incrememt
	cmp	w5, #'z'
	b.gt	cont
	cmp	w5, #'a'
	b.lt	cont
	sub	w5, w5, #('a' - 'A')
cont:
	strb	w5, [x3], #1	// store and incrememt
	cmp	w5, #0		// look for null byte
	b.ne	loop

	mov	x0, #1		// stdout
	ldr	x1, =output
	sub	x2, x3, x1	// diff between pointers
	mov	x8, #64		// write syscall
	svc	0

	mov	x0, #0		// success
	mov	x8, #93		// exit syscall
	svc	0

.data
input: .asciz "This is the input to this program.\n"
output: .fill 255, 1, 0

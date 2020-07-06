.global _start

_start:	mov	x0,	#1		// stdout
	ldr	x1,	=hello		// string to print
	mov	x2,	#14		// len
	mov	x8,	#64		// write syscall
	svc	0			// call

	mov	x0,	#0		// exit code
	mov	x8,	#93		// exit syscall
	svc	0

.data
hello:	.ascii	"hello, world.\n"

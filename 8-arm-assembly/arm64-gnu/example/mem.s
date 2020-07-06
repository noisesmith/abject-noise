.global _start
_start:
	ldr x1, =foo
	ldr x0, [x1]
	// return 0
	mov x8, #93
	mov x0, #0
	svc 0

.data
foo: .quad 0xffeeddccaa998877, 0x66554433221100fe

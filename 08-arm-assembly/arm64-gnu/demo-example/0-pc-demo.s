.section .text
.global _start

_start: mov r0, pc
	mov r1, #2
	add r2, r1, r1

	mov r0, r2 /* exit with the value of r2 */
	b _exit

_exit:
	mov r7, #1 /* system call register, exit index */
	swi #0
	

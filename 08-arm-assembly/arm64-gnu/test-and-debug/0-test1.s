/* just establishing that one can compile and exit success */
.global _start
_start:
	mov x0,#0 			//; success
	mov x8,#93			//; exit
	svc 0

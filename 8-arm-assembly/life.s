; the game of LIFE by conway, represented in ARM assembler
;

;;; example, loading and storing memory
	 mov		r1, #0x00000290
	 ;		since 666 won't fit in a single numeric literal
	 add		r1, r1, #0x0000000a
	 ;		store 666 into random memory location
	 mov		r2, #0x00011000
	 str		r1, [r2]
	 ;		load 666 from that same location
	 ldr		r0, [r2]

_output_cell:
brk

.macro wat
	//; bail with whatever is in x0 as return code
	mov	x8, #93
	svc	0
.endm

/* attempt to put the contents of a number into a buffer as hex */
/* prefix is printed, so that the two 8 byte values together become a single line in xxd */
.macro debug reg, prefix
	/* save data from caller */
	sub	sp, sp, #060
	str	x0, [sp]
	str	x1, [sp, #010]
	str	x2, [sp, #020]
	str	x3, [sp, #030]
	str	x8, [sp, #040]
	str	\reg, [sp, #050]
	/* set up data to print on stack */
	ldr	x0, [sp, #050]
	sub	sp, sp,#020
	mov	x3, \prefix
	str	x3, [sp]
	str	x0, [sp, #010]
	mov	x0, #2
	mov	x1, sp
	mov	x2, #16
	mov	x8, #64
	svc	0
	add	sp, sp,#020
	/* restore data from caller */
	ldr	\reg, [sp, #050]
	ldr	x8, [sp, #040]
	ldr	x3, [sp, #030]
	ldr	x2, [sp, #020]
	ldr	x1, [sp, #010]
	ldr	x0, [sp]
	add	sp, sp, #060
	/* debug */
.endm

//; .macro print_debug input=x0
//; 	sub	sp, sp, #090
//; 	stnp	x0, x1, [sp, #000]	//; store our first registers
//; 	stnp	x2, x3, [sp, #020]
//; 	stnp	x4, x5, [sp, #040]
//; 	str	x6, x7, [sp, #060]
//; 	str	x8, [sp, #080]		//; all nine of them
//; 	ldr	x1, =debug		//; thing to print
//; 	mov	x5, #0			//; byte of input
//; 	add	x6, x1, #7		//; write position in output string
//; 1:					//; fill two output bytes with ascii hex byte
//; 	mov	x4, \input		//;
//; 2:					//; get one byte of a word into our output
//; 	lsl	x7, x5, #4		//; bytes->nibbles
//; 	sub	x7, #64, x7		//; get a leftshift count (64 bits, bits used)
//; 	lsl	x4, x4, x7		//; clear nibbles to the left of this
//; 	mov	x7, #(7*8)		//; top nibble offset
//; 	add	x7, x7, 4		//; dialed in...
//; 	lsr	x4, x4, #x		//; make this nibble the first nibble
//; 	mov	x7, #'0'
//; 	sub	x8, #'a', #10		//; offset from #10 to 'a' as a constant
//; 	cmp	x4, #9
//; 	csel	x7, x7, x8		//; add offset up to '0', or 'a' if over 9
//; 	add	x4, x4, x7		//; make the ascii digit
//; 	strw	x4, [x1, #6]            //;
//; 	add	x5, x5, #1		//; next nibble
//; 	cmp	x5, #17			//; last nibble
//; 	b.eq	3:f
//; 	ands	x4, x5, #1		//; odd?
//; 	b.eq	1:b
//; 	b.lt	1:b
//; 3:
//; 	mov	x0, #2	      		//;  our fd - stderr
//; 	mov	x2, #65			//; buffer size
//; 	mov	x8, #64			//; write syscall
//; 	svc	0                       //;
//; 	ldr	x8, [sp, #080]          //;
//; 	ldnp	x6, x7, [sp, #060]      //;
//; 	ldnp	x4, x5, [sp, #040]      //;
//; 	ldnp	x2, x3, [sp, #020]      //;
//; 	ldnp	x0, x1, [sp, #000]      //;
//; 	add	sp, sp, #090            //;
//; .endm                                   //;
//;                                         //;
.data                                   //;
debug: .ascii "debug ________________\n"//;
endebug:


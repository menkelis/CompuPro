;	MTEST
;	Michael Enkelis	2023
;
;	  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A
;	|23 22 21 20 19 18 17 16|15 14 13 12 11 10 09 08|07 06 05 04 03 02 01 00|
;   |0  0 [ Board ] [   U   |             M         |      L               ]|
;   |                       |                       |                       |
;   |                       |                       |                       |
;	      A21..A19  A18...A16	Last Address+1
;		  0	000		000			080000
;		  1	001		001			100000
;		  2	010		010			200000
;		  3	011		011			180000
;		  4	100		100			400000
;		  5	101		101			280000
;		  6	110		110			300000
;		  7	111		111			380000
;		    ^^^
;		    +--- Board
;
;
; MTEST [-Bsize][-P -Q] [-F[pat] | -R | -V[pat]]
; -B=Block size
; -F=Fixed pattern. Default pattern is random unless [pat]
; -R=Random pattern
; -V=Verify pattern. Default pattern is random unless [pat]
; -P=Non-paged, Checks for pause/control-C
; -Q=Quick, Skip F000 blocks
; No args runs Random,Fixed tests

hdata		equ		0C6H		;Data port
haddr		equ		0C7H		;Address port

secsize		equ		128			;Bytes per sector
blksize		equ		1024

; BLOCK = (512*1024)/blksize

BDOS		equ		005H

			org		100H		;TPA

			jmp		past
boards:		db		1			;Number of M-DRIVE/H boards
past:		lxi		sp,stack
if 0
	lxi	h,80h
	mvi		m,5
	inx		h
	mvi		m,' '
	inx		h
	mvi		m,'-'
	inx		h
	mvi		m,'F'
	inx		h
	mvi		m,'0'
	inx		h
	mvi		m,'0'
endif
			lxi		h,0080h
			mov		c,m
			mvi		b,0
			push	h
			inx		h
			dad		b
			mvi		m,'$'
			pop		h
scan:		inx		h
			mov		a,m
			cpi		' '
			jz		scan
			cpi		'-'
			jz		scan
			cpi		'$'
			jz		All$Tests
			cpi		'B'			;Block size
			jnz		Not$B
			inx		h
			dcr		c
			jz		Bad$cmd
			mov		a,m
			call	hexcon
			jnc		Bad$cmd		;No block size
			call	get$hex		;Get block size
			mvi		e,0
			mov		a,d
			ora		e			;Test for 0000
			jz		Bad$cmd
			xchg
			shld	block$size	;Set block size
			xchg
			jmp		scan$nxt
Not$B:		cpi		'F'			;Fixed pattern
			jnz		Not$F
			inx		h
			dcr		c
			jz		Bad$cmd
			mov		a,m
			call	hexcon
			jnc		Fixed		;No pattern
			call	get$hex		;Get pattern
			mov		a,e
			sta		value
			lda		flags
			ori		00000001b	;Set flag
			sta		flags
			jmp		Fixed
Not$F:		cpi		'P'			;Non-Paged
			jnz		Not$P
			lda		flags
			ori		10000000b	;Set flag
			sta		flags
			jmp		scan$nxt
Not$P:		cpi		'Q'			;Quick
			jnz		Not$Q
			lda		flags
			ori		01000000b	;Set flag
			sta		flags
			jmp		scan$nxt
Not$Q:		cpi		'R'			;Random pattern
			jz		Random
			cpi		'V'			;Verify [pattern]
			jnz		scan$nxt
			inx		h
			dcr		c
			jz		Bad$cmd
			mov		a,m
			call	hexcon
			jnc		Verify		;No pattern
			call	get$hex		;Get pattern
			mov		a,e
			sta		value
			lda		flags
			ori		00000001b	;Set flag
			sta		flags			
			jmp		Verify
scan$nxt:	dcr		c
			jnz		scan
; Fall into All$Tests
All$Tests:	call	Random
			call	Fixed
			jmp		0			;warm boot
Bad$cmd:	lxi		d,bad$msg
			call	print
			jmp		0			;warm boot
;
; Fixed M-Drive/H with random data and verify
Random:		mvi		a,0			;Use random patterns
			sta		pattern
			call	clr$addr
			call	Info$Block
			call	Test$RAM
			jmp		0			;warm boot
;
; Fixed M-Drive/H with 0E5h and verify
Fixed:		mvi		a,0E5h		;Use fixed pattern
			sta		pattern
			lda		flags
			ani		00000001b	;Test flag
			jz		Fixed$1
			lda		value
			sta		pattern		;Use fixed pattern
Fixed$1		call	clr$addr
			call	Info$Block
			call	Test$RAM
			jmp		0			;warm boot
;
; Verify M-Drive/H with user pattern
Verify:		mvi		a,0			;Use random patterns
			sta		pattern
			lda		flags
			ani		00000001b	;Test flag
			jz		Verify$1
			lda		value
			sta		pattern		;Use fixed pattern
Verify$1:	call	clr$addr
			call	Info$Block
			call	Verify$RAM
			jmp		0			;warm boot
;
; M-DRIVE/H memory test
; Fill M-DRIVE/H with test patterns
Test$RAM:	call	Setup$vals
			call	Save$addr	;Save start address of block
			call	Fill$mdrive	;Fill MDRIVE
;
; Wait some time to check for memory fade
;
			lxi		d,delay$msg
			call	print
			lxi		b,10		;Set delay time
			call	Time$Delay
;
; Read M-DRIVE/H and verify test patterns
			call	Setup$vals
			call	Rest$addr	;Restore start address of block
			call	Read$mdrive	;Read MDRIVE, verify data
;
; Do next block if MDRIVE not completed
			call	Drive$done
			jnz		Test$RAM	;Tested all MDRIVE's
			ret
;
; Verify M-Drive/H with user pattern
verify$RAM:	call	Setup$vals
			call	Read$mdrive	;Read and Test MDRIVE
			call	crlf
;
; Do next block if MDRIVE not completed
			call	Drive$done
			jnz		Verify$RAM	;Read all MDRIVE's
			ret
;
; Clear 22-bit address, saved 22-bit address
clr$addr:	lxi		h,address
			mvi		c,2*3
			xra		a
clr$nxt:	mov		m,a
			inx		h
			dcr		c
			jnz		clr$nxt
			ret
; Save 22-bit address
Save$addr:	lxi		h,address
			lxi		d,Saddress
			jmp		Copy$addr
; Restore saved 22-bit address
Rest$addr:	lxi		h,Saddress
			lxi		d,address
Copy$addr:	mvi		c,3
Copy$Nxt:	mov		a,m		;HL=Source
			stax	d		;DE=Dest
			inx		h
			inx		d
			dcr		c
			jnz		Copy$Nxt
			ret				;Done
;
; Reset test pattern, counters, number of boards
Setup$vals:	lhld	seed		;Init random number
			shld	random$num
			call	rndm		;Init random number
			lhld	block$size
			shld	blk$cnt
			ret
;
; Read and Test M-DRIVE/H
Read$mdrive:
			mvi		a,0
			sta		oflag
			call	break$chk	;Clear console
			call	Info$Test
;
Read$Nxt:	call	break$chk	;Check for exit
			jz		Read$Go
			cpi		'C'-'@'		;Control-C
			jnz		Read$Go		;Nope
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			jmp		0			;Warm boot
Read$Go:	lda		address+1
			cma
			out		0ffh		;Out to IMSAI lights
			call	RT$Block	;One sector
			call	Block$Done	;One block
			jnz		Read$Nxt	;Nope

; All blocks have been done
Read$Done:	mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			ret
;
; Read/Test Data Block
RT$Block:	call	Set$mdrive
			mvi		c,secsize
Read$loop:	call	rndm		;Generate random number
			mov		e,a			;Save as test byte
			in		hdata
			mov		d,a			;Save what read byte
			cmp		e			;Good byte?
			cnz		err$report	;No...
			call	Bump$Address
			dcr		c			;Block done?
			jnz		Read$loop	;No
			ret
;
; Report bad read
; Expects: C= -sector, B=Expected, A=Read
err$report:	push	b			;Save regs
			push	d			;Save test/read
			lda		oflag
			ana		a
			jnz		No$hdr
			inr		a
			sta		oflag		;oflag=1
			lxi		d,header
			call	print
; Print Address
No$hdr:		call	Disp$Address
			mvi		b,6
			call	blanks
; Print Pattern Wrote
			pop		d			;Restore test/read
			mov		a,e			;A=test byte wrote
			call	Hexout
			mvi		b,4
			call	blanks
			mov		a,e			;A=test byte wrote
			call	Binout
			mvi		b,5
			call	blanks
; Print Pattern Read
			mov		a,d			;A=byte read
			call	Hexout
			mvi		b,4
			call	blanks
			mov		a,d			;A=byte read
			call	Binout
			mvi		b,5
			call	blanks
			mov		a,e			;A=test byte wrote
			xra		d			;What bits to check
			call	Binout
			call	crlf
			pop		b			;Restore regs
; Page full output
			lda		flags
			ani		10000000b	;Test flag
			ana		a
			jz		Page$full
; Test for pause
			call	break$chk
			rz
			cpi		'C'-'@'		;Control-C
			jz		0			;Yes..Warm boot
Wait$Char:	call	break$chk	;Wait for character
			jz		Wait$Char	;Nope
; Test for exit
Not$cP:		cpi		'C'-'@'		;Control-C
			rnz					;Nope
			jmp		0			;Warm boot

Page$full:	lda		oflag
			inr		a
			cpi		22
			sta		oflag
			rnz
			mvi		a,0
			sta		oflag
; Check for program pause/terminate
			lxi		d,cont$msg
			call	print
Pause:		call	break$chk
			jz		Pause		;No keypress
			cpi		'C'-'@'		;Control-C
			jnz		crlf		;Nope
			jmp		0			;Warm boot
;
; Fill M-DRIVE/H with test data
Fill$mdrive:
			call	Info$Fill
Fill$Nxt:	call	break$chk	;Asking for exit?
			jz		Fill$Go
			cpi		'C'-'@'		;Control-C
			jnz		Fill$Go		;Nope
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			jmp		0			;Warm boot
Fill$Go:	lda		address+1
			cma
			out		0ffh		;Out to IMSAI lights
			call	Write$Block
			call	Block$Done
			jnz		Fill$Nxt

; All blocks have been done
Fill$Done:
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			call	Disp$EAddress
			ret
;
; Write data block
Write$Block:
			call	Set$mdrive	;Set MDRIVE memory address
			mvi		c,secsize
Write$loop:	call	rndm		;Generate test byte
			out		hdata		;Send to MDRIVE
			call	Bump$Address
			dcr		c			;Block done?
			jnz		Write$loop	;No
			ret
;
; Return Acc=0 on end of block
Block$Done:	lhld	blk$cnt
			dcx		h
			shld	blk$cnt
			mov		a,h
			ora		l
			ret					;A=0 when block is done
;
; Update 22-bit address
Bump$Address:
			lxi		h,address+2	;Point to L
			inr		m			;A14..A00
			rnz
			dcx		h			;Point to M
			inr		m			;A08..A15
			rnz
			dcx		h			;Point to U
			inr		m			;A16..A22
			ret
;
; Return A=0 if all MDRIVE boards done
Drive$Done:	lxi		h,address	;Point to U
			lda		flags
			ani		01000000b	;Test flag
			jz		Drive$Nxt
; Bump by 64K
			mov		a,m			;Get U
			ani		00111000B	;A21..A19
			mov		b,a
			ani		00000111B	;A18..A16
			inr		a
			ora		b
			mov		m,a			;Set U
;
; Test for drive done
Drive$Nxt:	lda		boards		;A=number of boards to test
			rlc
			rlc
			rlc
			ani		00111000B	;Mask bits, A21..A19
			mov		b,a
			mov		a,m			;Get U
			ani		00111000B	;Mask bits A21..A19
			cmp		b
			ret					;A=0 when drive is done
;
; Send 22-bit starting address to M-DRIVE/H card
; Uses: HL,A
Set$mdrive:	lxi		h,address+2
			mov		a,m			;A =A21..A16
			out		haddr
			dcx		h
			mov		a,m			;A =A15..A08
			out		haddr
			dcx		h
			mov		a,m			;A =A07..A00
			out		haddr
			ret
;
; Generate random number in reg. A
; Uses: A
; Returns: A
rndm:
			lda		flags
			ani		00000001b	;Test flag
			lda		pattern
			rnz					;Use fixed pattern
			push	h
			lhld	random$num	;Get psudo-random number
			mov		a,h			;Look at H
			ani		0B4h		;Mask bits
			ana		a			;Clear CY
			jpe		peve		;Jump if even
			stc
peve:		mov		a,l			;Look at L
			ral					;Rotate CY in
			mov		l,a			;Restore L
			mov		a,h			;Look at H
			ral					;Rotate CY in
			mov		h,a			;Restore H
			shld	random$num	;Update psudo-random number
			pop		h
			ret					;A=random number
;
; Time Delay
; Entry: <BC>=time delay
; Uses: A, BC, HL
Time$Delay:	lxi		h,0
Wait:		dcx		h
			mov		a,h
			ora		l
			jnz		Wait
			dcx		b
			mov		a,b
			ora		c
			jnz		Time$Delay
			ret
;
Disp$EAddress:
			mvi		a,'-'
			call	conout
			lxi		h,address
			mov		a,m			;A =A19..A22
			dcr		a
			call	Hexout
			inx		h
			mov		a,m			;A =A15..A08
			dcr		a
			call	Hexout
			inx		h
			mov		a,m			;A =A07..A00
			dcr		a
			jmp		Hexout
;
; Display 22-bit address
Disp$Address:
			lxi		h,address
			mov		a,m			;A =A19..A22
			call	Hexout
			inx		h
			mov		a,m			;A =A15..A08
			call	Hexout
			inx		h
			mov		a,m			;A =A07..A00
			jmp		Hexout
;
Info$Fill:	call	crlf
			lxi		d,fill$msg
			call	print
			jmp		Disp$Address
;
Info$Test:	lxi		d,test$msg
			jmp		print
;
Info$block:	lxi		d,blk$msg
			call	print
			lhld	block$size
			mov		a,h
			call	Hexout
			mov		a,l
			call	HexoutB
			lxi		d,pat$msg
			call	print
			lda		flags
			ani		00000001b	;Test flag
			jz		Info$Rndm
			lda		pattern
			call	HexoutB
			jmp		crlf
Info$Rndm:	lxi		d,rnd$msg
			call	print
;	Fall into CRLF
;
crlf		mvi		a,0DH
			call	conout
			mvi		a,0AH
			jmp		conout
;
; Output <B> blanks
blanks:		call	space
			dcr		b
			jnz		blanks
			ret
;
; Output <A> as binary
Binout:		push	b
			mov		b,a
			mvi		c,8
Bin$Nxt:	mov		a,b
			rlc
			mov		b,a
			mvi		a,'0'		;A=0
			jnc		Bin$0
			inr		a			;A=1
Bin$0		call	conout
			dcr		c
			jnz		Bin$Nxt
			pop		b
			ret
;
;
HexoutB:	call	hexout
;	Fall into SPACE
;
space:		mvi		a,' '
			jmp		conout
;
;***Subroutine*************************************
;Scan past spaces and get a hex value
;On Entry:
;  de=address of next item in the input line buffer
;On Exit:
;  hl=value of last 4 digits found, defaults to 0
;  Z set
;  de advanced past last digit found
;  carry clear if value found
;  carry set if no value found
;**************************************************
get$hex:	xchg
			lxi		h,0			;default & initial value
FHEXLP:		ldax	d
			cpi		' '
			jz		sspace
			call	HEXCON		;convert a to binary
			jnc		HEXERR		;not valid hexidecimal value?
			dad		h			;make room for the new digit
			dad		h
			dad		h
			dad		h
			add		l
			mov		l,a			;move new digit in
sspace:		inx		d			;bump the pointer
			dcr		c
			jp		FHEXLP
			xchg
			xra		a			;A=0
			ret					;A=0 for exit, keep carry clear
HEXERR:		xchg
			stc
			ret
;
;***Subroutine************************
;Convert ASCII hex digit to binary
;On Entry:
;  a=chr to convert
;On Exit:
;  a=binary
;  Carry set if OK, clear if bogus chr
;*************************************
hexcon:	sui		'0'			;remove ASCII bias
		cpi		10
		rc					;if 0-9 then we're done

		sui		9+('A'-'9')	;should be 0-5 now
		cpi		6			;gap chr or too high?
		rnc					;error: return W/O carry

		sui		0F6h		;add 0Ah, set Carry
		ret
;
;********************************************
;	SUBROUTINE TO DISPLAY HEX NUMBER		*
;********************************************
Hexout:		push	psw
			rar
			rar
			rar
			rar
			call	hexout1
			pop		psw
hexout1:	ani		00FH
			sui		00AH
			jm		hexout2
			adi		007H
hexout2:	adi		03AH
;
;********************************************
;	SUBROUTINE TO DISPLAY ON CONSOLE		*
;********************************************
conout:		push	psw
			push	b
			push	d
			push	h
			mov		e,a
			mvi		c,2
			call	BDOS
			pop		h
			pop		d
			pop		b
			pop		psw
			ret
;
;********************************************
;	SUBROUTINE TO PRINT STRING (DE)			*
;********************************************
print:		push	psw
			push	b
			push	d
			push	h
			mvi		c,9
			call	BDOS
			pop		h
			pop		d
			pop		b
			pop		psw
			ret
;
;********************************************
;	SUBROUTINE TO READ CONSOLE				*
;********************************************
break$chk:
			push	b
			push	d
			push	h
			mvi		e,0ffh
			mvi		c,6
			call	BDOS
			ana		a			;A=0, No keypress
			pop		h
			pop		d
			pop		b
			ret
;
bad$msg:	db	'Bad command line$'
fill$msg:	db	'Filling block: $'
test$msg:	db	'Testing block$'
blk$msg:	db	'Block size: $'
pat$msg:	db	'Pattern: $'
rnd$msg:	db	'Random $'
delay$msg:	db	'  Delaying  $'
cont$msg:	db	'Continue - ^C Exit$'
header:		db	0Dh,0Ah,'Address     Wrote (binary)     Read  (binary)'
			db	'     - Bits -',0Dh,0Ah,'$'
;
; Vars
flags:		db		0h
value:		db		0h
pattern:	db		0h
random$num:	dw		0h
seed:		dw		5A5Ah		;Init random number
block$size:	dw		blksize
blk$cnt:	ds		2
oflag:		ds		1
address:	ds		3
Saddress:	ds		3
			ds		40
stack:		equ		$

		end

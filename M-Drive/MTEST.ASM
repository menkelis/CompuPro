;	MTEST
;	Michael Enkelis	2023
;
;	  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A  A
;	|23 22 21 20 19 18 17 16|15 14 13 12 11 10 09 08|07 06 05 04 03 02 01 00|
;   |0  0 [ Board ] [   U   |             M         |      L               ]|
;   |                       |                       |                       |
;   |                       |                       |                       |
;	    # A21..A19  A18...A16	Address Range
;		1	000		000			000000 - 07FFFF
;		2	001		001			000000 - 0FFFFF
;		3	010		010			000000 - 17FFFF
;		4	011		011			000000 - 1FFFFF
;		5	100		100			000000 - 27FFFF
;		6	101		101			000000 - 2FFFFF
;		7	110		110			000000 - 37FFFF
;		8	111		111			000000 - 3FFFFF
;		    ^^^
;		    +--- Board
;
;
; MTEST [-P -Q] [ -B[num] -F[pat] | -R | -V[pat] ]
; -F=Fixed pattern. Default pattern is random unless [pat]
; -R=Random pattern
; -V=Verify pattern. Default pattern is random unless [pat]
; -P=Non-paged, Checks for pause/control-C
; -Q=Quick, Skip F000 blocks
; -B=Number of boards, 1 to 8
; No args runs Random,Fixed tests

hdata		equ		0C6H		;Data port
haddr		equ		0C7H		;Address port

BDOS		equ		005H

			org		100H		;TPA

			lxi		sp,stack
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
			cpi		'B'			;Boards
			jnz		Not$B
			inx		h
			dcr		c
			jz		Bad$cmd		;No arg?
			mov		a,m
			call	hexcon
			jnc		scan$nxt	;Invalid value
			call	get$hex		;Get boards
			mov		a,e			;Get value
			cpi		0			;0?
			jz		Bad$cmd		;Yes
			cpi		8+1			;>8?
			jnc		Bad$cmd		;Yes
			sta		boards		;Set value
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
			call	Calc$Blks	;Find last block address
			call	Info$Pats	;Info on test patterns
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
Fixed$1:	call	Calc$Blks	;Find last block address
			call	Info$Pats	;Info on test patterns
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
Verify$1:	call	Calc$Blks	;Find last block address
			call	Setup$vals
			call	Info$Pats	;Info on test patterns
			call	Info$Vrfy	;Inform verify only
			call	Read$mdrive	;Read and Test MDRIVE
			jmp		0			;warm boot
;
; M-DRIVE/H memory test
; Fill M-DRIVE/H with test patterns
; Repeats until user break (CONTROL-C)
Test$RAM:
			call	Setup$vals
			call	Info$Pass
			call	Info$Fill	;Inform filling MDRIVE
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
			call	Setup$vals	;Reset vals
			call	Info$Test
			call	Read$mdrive	;Read MDRIVE, verify data
;
; Next pass
			lhld	random$num
			shld	seed		;Set new seed
			call	crlf
			lhld	pass
			inx		h
			shld	pass		;Update pass counter
			jmp		Test$RAM

;
; Calculate size of MDRIVE from #boards
; Save as block$size, and last address as Address+3
Calc$Blks:	call	clr$addr	;Clear 22-bit address
			lda		boards		;A=# boards: 1-8
			lxi		h,0			;HL=0
			lxi		d,1000h		;DE=4k
Set$Size:	dad		d			;HL=HL+DE
			dcr		a			;Count board
			jnz		Set$Size
			shld	block$size
			ret
;
; Setup Fill Vals
; Reset test pattern, address
Setup$vals:
			lhld	seed		;Init random number
			shld	random$num
			call	rndm		;Init random number
;
; Clear 22-bit address
clr$addr:	lxi		h,address
			mvi		c,3
			xra		a
clr$nxt:	mov		m,a
			inx		h
			dcr		c
			jnz		clr$nxt
			ret
;
; Fill M-DRIVE/H with test data
Fill$mdrive:
; Set M-DRIVE/H counters
			xra		a
			out		haddr		;U
			out		haddr		;M
			out		haddr		;L
			lhld	block$size
			xchg				;DE=Block size
Fill$Nxt:	call	break$chk	;Asking for exit?
			jz		Fill$Go
			cpi		'C'-'@'		;Control-C
			jnz		Fill$Go		;Nope
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			jmp		0			;Warm boot
Fill$Go:	mvi		c,80h
Fill$Byte:	mov		a,e
			out		0ffh		;Out to IMSAI lights
			call	rndm		;A=test byte
			out		hdata
			dcr		c
			jnz		Fill$Byte
			dcx		d
			mov		a,d
			ora		e
			jnz		Fill$Nxt	;Nope
; All blocks have been done
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			ret
;
; Read and Test M-DRIVE/H
Read$mdrive:
			call	break$chk	;Clear console
			xra		a			;A=0
			sta		oflag
; Set M-DRIVE/H counters
			out		haddr		;U
			out		haddr		;M
			out		haddr		;L
			lhld	block$size
			xchg				;DE=Block size
Read$Nxt:	call	break$chk	;Check for exit
			jz		Read$Go
			cpi		'C'-'@'		;Control-C
			jnz		Read$Go		;Nope
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			jmp		0			;Warm boot
Read$Go:	mvi		c,80h
Read$Byte:	mov		a,e
			out		0ffh		;Out to IMSAI lights
			in		hdata
			mov		l,a			;L=data read
			call	rndm		;A=expected byte
			mov		h,a
			cmp		l			;Good byte?
			cnz		Err$Report	;No...
			call	Bump$Address
			dcr		c
			jnz		Read$Byte
			dcx		d
			mov		a,d
			ora		e
			jnz		Read$Nxt	;Nope
; All blocks have been done
			mvi		a,0ffh
			out		0ffh		;Out to IMSAI lights
			ret
;
; Report bad read
; Expects: L=Read, H=Expected
; Saves: BC,DE
Err$Report:
			push	b
			push	d
			push	h			;Save pattern
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
			pop		h			;Restore test/read
			mov		a,h			;A=test byte wrote
			call	Hexout
			mvi		b,4
			call	blanks
			mov		a,h			;A=test byte wrote
			call	Binout
			mvi		b,5
			call	blanks
; Print Pattern Read
			mov		a,l			;A=byte read
			call	Hexout
			mvi		b,4
			call	blanks
			mov		a,l			;A=byte read
			call	Binout
			mvi		b,5
			call	blanks
			mov		a,l			;A=test byte wrote
			xra		h			;What bits to check
			call	Binout
			call	crlf
			pop		d			;Restore regs
			pop		b

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
; Generate random number in reg. A
; Uses: A
; Saves: HL
; Returns: A
rndm:
			lda		flags
			ani		00000001b	;Test flag
			lda		pattern
			rnz					;Use fixed pattern
			push	h			;Save HL
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
			pop		h			;Restore HL
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
; Display 000000-xxFFFF
Disp$Block:
			lxi		d,blkb$msg
			call	print
			lda		boards		;A=1 - 8
			rlc
			rlc
			rlc
			dcr		a			;Boards start at zero
			ori		7h			;Bits A19-A16 = 1
			call	Hexout
			lxi		d,blke$msg
			jmp		print
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
Info$Pass:	lxi		d,pass$msg
			call	print
			lhld	pass
			mov		a,h
			call	Hexout
			mov		a,l
			jmp		Hexout
;
Info$Fill:	call	crlf
			lxi		d,fill$msg
			call	print
			jmp		Disp$Block
;
Info$Test:	lxi		d,test$msg
			jmp		print
;
Info$Vrfy:	lxi		d,vrfy$msg
			call	print
			jmp		Disp$Block
;
Info$Pats:	lxi		d,pat$msg
			call	print
			lda		flags
			ani		00000001b	;Test flag
			jz		Info$Rndm
			lda		pattern
			call	Hexout
			jmp		crlf
;
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
pass$msg:	db	'Pass: $'
blkb$msg:	db	'block: 000000-$'
blke$msg:	db	'FFFF$'
fill$msg:	db	'Filling $'
vrfy$msg:	db	'Verify $'
test$msg:	db	'Testing$'
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
pass:		dw		0			;Number of passes
boards:		db		1			;Number of M-DRIVE/H boards
block$size:	ds		2
oflag:		ds		1
Address:	ds		3
			ds		40
stack:		equ		$

		end

; L-MDRIVE
; V1.0 2023 Michael Enkelis
; Some code was derived from CPMOVE written by Mike Douglas 

; Usage: MDRIVE -Drive Address
;
; Drive: A - P
; Address: >CP/M

;-----------------------------------------------------------------------------
; Start of program
;-----------------------------------------------------------------------------

mdriveh		equ		80h		;Flag M-Drive/H
warm$boot	equ		0000h
bdos		equ		0005h
DMABUF		equ		0080h
STACK		equ		800H

		ORG		0100H
main:	equ		$
		jmp		past
; VALUES FILLED IN BY G-MDRIVE.
RELBAS:		DS		2		;RELOCATION BASE
MDSIZE:		DS		2		;SIZE OF MDISK DRIVER
BOARDS:		DB		1		;NUMBER OF M-DRIVE/H BOARDS
past:	lxi		sp,stack
		call	findBIOS	;Get address of BIOS vectors
		call	findBOARDS	;Get number of M-DRIVE boards
		call	getargs		;Process command line
		call	validate	;Validate parameters
		call	loaded		;Check for existing driver
		call	relocate	;Relocate driver code
		call	getvec		;Set BIOS vectors
		call	setdrv		;Set drive
		call	setdph		;Set DPH
		call	setdpb		;Set DPB
		call	movmem		;Copy driver to run location
		call	setvec		;Set BIOS to use driver
		jmp		warm$boot	;Warm-Boot
;
findBIOS:
		lhld	warm$boot+1
		mov		a,h			;A=Start of BIOS
		cpi		0FCh		;BIOS > 63K?
		jnc		BIG$CPM		;Yes, Report no room for driver
		lxi		d,0015h		;DE=Offset from WBOOT to HOME
		dad		d
		shld	BIOS		;Address of BIOS function HOME
		ret
;
; Number of M-DRIVE/H boards (1 - 8)
findBOARDS:
		ret
;
getargs:
		lxi		h,DMABUF
		push	h
		mov		c,m			;C=byte count
		mvi		b,0
		inx		h
		dad		b
		mvi		m,'$'		;Mark END-OF-BUFFER
		pop		d
scan:	call	SSNEXT
		rc					;No more args
		cpi		'-'			;Look for flag to set DRIVE
		jnz		isHEX
		inx		d
		dcr		c
		jm		BAD$CMD
		ldax	d			;Get DRIVE letter
		sta		UNIT
		jmp		scan
isHEX:	call	FNDHEX		;Convert buffer to HEX
		jc		scan		;Not HEX
		shld	HIMEM
		jmp		scan
;
validate:
; Valid drive range: A - P
		lda		UNIT		;Ascii UNIT
		ana		a			;No drive specified?
		jm		WHT$DRV		;Yes, report error
		sui		'A'			;Convert to binary DRIVE
		cpi		001H		;Too small?
		jc		INV$DRV		;Yes, report error
		cpi		010H		;Too large?
		jnc		INV$DRV		;Yes, report error
; Valid driver load: > base of CP/M
		lhld	HIMEM		;Drive load address
		lda		BDOS+2		;A=BDOS location (base of CP/M)
		cmp		h			;Are we loading driver below CP/M?
		jnc		INV$MEM		;Yes, report error
		dcr		l			;Test for XX00
		jp		INV$MEM		;Not zero, report error
		ret
;
; Check for existing driver
loaded:
		lda		UNIT		;Recover UNIT
		sui		'A'			;Convert to binary DRIVE
		mov		c,a			;Into reg C
		lhld	BIOS		;Point to base of BIOS
		lxi		d,003h		;Offset to SELDSK
		dad		d
		lxi		d,return
		push	d			;Stack return address
		pchl				;Call BIOS
							;Return HL=0 if invalid drive else HL=DPH
return:	mov		a,h			;Test if HL = 0
		ora		l
		rz					;Invalid DRIVE selected, no driver loaded.
		lxi		b,00Ah		;BC=Offset to DPB in DPH
		dad		b			;HL=ptr to DPB
		mov		a,m
		inx		h
		mov		h,m
		mov		l,a			;HL=ptr to DPB
		dcx		h			;HL=ptr to mdrive$type
		mov		a,m			;Fetch M-Drive type flag
		cpi		mdriveh		;Is this hardware M-Drive?
		rnz					;Nope
		jmp		DUP$DRV		;Report driver is loaded
;
;
relocate:
; INIT FOR RELOCATION
		LXI		H,MDSIZE
		MOV		C,M
		INX		H
		MOV		B,M			;B,C CONTAINS MODULE SIZE

; H,L CONTAINS THE BASE OF THE RELOCATION AREA
		LHLD	RELBAS		;RELOCATION BASE
		push	h			;Push base of bitmap
		lhld	himem		;H,L CONTAINS HIMEMORY ADDRESS
;
; REGISTER H CONTAINS BIAS VALUE
		LXI		D,MODULE	;DE=Base of module
REL0:	MOV		A,B			;BC=0?
		ORA		C
		JZ		ENDREL
;
; NOT END OF THE RELOCATION, MAY BE INTO NEXT BYTE OF BIT MAP
		DCX		B			;COUNT LENGTH DOWN
		MOV		A,E
		ANI		111B		;0 CAUSES FETCH OF NEXT BYTE
		JNZ		REL1
;
; FETCH BIT MAP FROM STACKED ADDRESS
		XTHL				;HL=bitmap, Stack=HL
		MOV		A,M			;NEXT 8 BITS OF MAP
		INX		H
		XTHL				;Bitmap GOES BACK TO STACK
		MOV		L,A			;L HOLDS THE MAP AS WE PROCESS 8 LOCATIONS
REL1:	MOV		A,L
		RAL					;CY SET TO 1 IF RELOCATION NECESSARY
		MOV		L,A			;BACK TO L FOR NEXT TIME AROUND
		JNC		REL2		;SKIP RELOCATION IF CY=0
;
; CURRENT ADDRESS REQUIRES RELOCATION
		LDAX	D
		ADD		H			;APPLY BIAS IN H
		STAX	D
		JMP		REL2
;
REL2:	INX		D			;TO NEXT ADDRESS
		JMP		REL0		;FOR ANOTHER BYTE TO RELOCATE
;
ENDREL:	;END OF RELOCATION
		POP		D			;CLEAR STACKED ADDRESS
		ret
;
; Set flags in driver
setdrv:	lhld	DRIVE
		shld	SIZE		;Get actual driver code size
		lda		UNIT
		sui		'A'
		mov		l,a
		mvi		h,mdriveh
		shld	DRIVE		;Set M-DRIVE/H flag
		ret
;
; Set DPB in driver
setdpb:	lda		BOARDS		;Number of M-DRIVE/H boards
		dcr		a			;Range: 0 - 7
		mov		l,a
		mvi		h,0			;HL=Offset
		dad		h			;*2
		dad		h			;*4
		dad		h			;*8
		dad		h			;*16
		lxi		d,DPBTBL	;HL=Base of DPB table
		dad		d			;HL=DPBTBL pointer
		lxi		d,DPB		;DE=DPB in module
		mvi		c,15		;C=Length of DPB
setnxt:	mov		a,m			;Get byte from table
		stax	d			;Put into module
		inx		h
		inx		d
		dcr		c
		jnz		setnxt
		ret
SIB		equ		2048/128	;Sectors in Memory Drive Data Block
DPBTBL:
;	1 board
		dw		8			;SPT
		db		4,15,1		;BSH, BLH, EXM
		dw	(((512-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	2 boards
		dw		16			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((1024-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	3 boards
		dw		24			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((1536-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	4 boards
		dw		32			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((2048-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	5 boards
		dw		40			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((2560-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	6 boards
		dw		48			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((3072-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	7 boards
		dw		56			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((3584-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;	8 boards
		dw		64			;SPT
		db		4,15,0		;BSH, BLH, EXM
		dw	(((4096-4)*8)/SIB)-1	;DSM
		dw		128-1		;DRM
		db		0C0h,00h	;AL0, AL1
		dw		0			;CKS
		dw		4			;OFF
		db		0			;End of table
;
; Set DPH in driver
setdph:	lda		0004h		;Current drive
		mov		c,a
		lhld	BIOS
		lxi		d,0003h		;DE=Offset from WBOOT to SELDSK
		dad		d			;HL=address of BIOS function SELDSK
		lxi		d,retdph
		push	d			;Return address
		pchl				;Call BIOS function
retdph:	lxi		d,0008h
		dad		d			;Point to address of DIRBUF
		mov		e,m
		inx		h
		mov		d,m
		xchg				;HL=DIRBUF
		shld	dph+0008h	;Set driver DPH entry for DIRBUF
		ret
;
; Copy FROM BIOS to mdisk, 8 jump vectors
getvec:	lhld	BIOS
		lxi		d,GO$BIOS
							;HL=src, DE=dest
		mvi		c,7			;C=number of JMP XXXX to copy
		call	getjmp
		inx		h			;Skip
		inx		h			; Over
		inx		h			;  LISTST, Pointing to SECTRAN
		mvi		c,1			;C=number of JMP XXXX to copy
		call	getjmp
		ret
getjmp:	mov		a,m
		stax	d
		inx		h
		inx		d
		mov		a,m
		stax	d
		inx		h
		inx		d
		mov		a,m
		stax	d
		inx		h
		inx		d
		dcr		c
		jnz		getjmp
		ret
;
; Move image into hi-memory
movmem:	lhld	SIZE		;Size of driver module
		mov		c,l
		mov		b,h			;B,C CONTAINS MODULE SIZE
		lxi		d,MODULE	;D,E = Base of module
		lhld	himem		;H,L = Run address
movlp:	mov		a,b			;BC=0?
		ora		c
		rz
		ldax	d			;Get byte of driver
		mov		m,a			;Save in hi-memory
		inx		h
		inx		d
		dcx		b
		jmp		movlp
;
; Copy FROM mdisk to BIOS 8 new jump vectors
setvec:	lhld	BIOS
		xchg				;DE=pointer to BIOS:HOME
		lhld	himem		;HL=pointer to DRIVER:HOME
		mvi		c,7			;C=number of JMP XXXX to create
		call	setjmp
		inx		d			;Skip
		inx		d			; Over
		inx		d			;  LISTST, Pointing to SECTRAN
		mvi		c,1			;C=number of JMP XXXX to create
		call	setjmp
		ret
; Set JMP vector
; HL=src, DE=dest
setjmp:	mvi		a,jmp		;JMP
		stax	d			;Offset=0
		mov		a,l			;A=Current low byte
		inx		d			;Offset=1
		inx		h
		stax	d			;Low byte of address
		inx		d			;Offset=2
		inx		h
		mov		a,h			;A=HIMEM
		stax	d			;High byte of address
		inx		d			;Offset to next JMP
		inx		h
		dcr		c
		jnz		setjmp
		ret

;
;********************************************
;	SUBROUTINE TO CR LF						*
;********************************************
crlf:	mvi		a,0Dh
		call	conout
		mvi		a,0Ah
		jmp		conout
;
;********************************************
;	SUBROUTINE TO DISPLAY HEX NUMBER		*
;********************************************
;
hexoutB:call	hexout
space:	mvi		a,' '
		jmp		conout

hexout:	push	psw
		rar
		rar
		rar
		rar
		call	hexout1
		pop		psw
hexout1:ani		00FH
		sui		00AH
		jm		hexout2
		adi		007H
hexout2:adi		03AH
		jmp		conout
;
;
;***Subroutine*************************************
;Scan past spaces in line buffer, looking for the
;first non-space character
;On Entry:
;  de=starting address within the input line buffer
;On Exit:
;  Carry set if no chr or only control chrs found
;  a=character value if found, 0 if end of line
;  de advanced past spaces
;**************************************************

SSPACE:	ldax	d			;get next character
		cpi		' '			;another space?
							;carry set for any ctrl chr
		rnz					;carry clear for all else
SSNEXT:	inx		d			;next scan address
		dcr		c			;End of buffer
		jp		SSPACE		;No, keep skiping
		xra		a			;A=0
		stc					;No more args, report error
		ret
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
FNDHEX:	lxi		h,0			;default & initial value
FHEXLP:
		call	SSPACE		;skip spaces, get 1st digit
		rc					;carry set if no digits

		dad		h			;make room for the new digit
		dad		h
		dad		h
		dad		h

		call	HEXCON		;convert a to binary
		jnc		HEXERR		;not valid hexidecimal value?

		add		l
		mov		l,a			;move new digit in

		inx		d			;bump the pointer
		dcr		c
		jp		FHEXLP

		xra		a			;A=0
		ret					;A=0 for exit, keep carry clear
HEXERR:	stc
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
; Report Command line error
;
BAD$CMD:
		lxi		d,err0		;DE=Message
		jmp		BAIL		;Print message and exit program
;
; Report Invalid Drive
;
INV$DRV:
		lxi		d,err1		;DE=Message
		jmp		BAIL		;Print message and exit program
;
; Report No Drive on command line
;
WHT$DRV:
		lxi		d,err2		;DE=Message
		jmp		BAIL		;Print message and exit program
;
; Report Invalid driver load address
;
INV$MEM:
		lxi		d,err3		;DE=Message
		jmp		BAIL		;Print message and exit program
;
; Report No Room above CP/M for driver
;
BIG$CPM:
		lxi		d,err4		;DE=Message
		jmp		BAIL
;
; Report Driver Loaded
;
DUP$DRV:
		lxi		d,err5		;DE=Message
;
;
; Print Message (DE), Fall into EXIT
;
BAIL:
		call	print
;
; Exit program
;
EXIT:
		jmp		warm$boot	;BIOS: Warm Boot
;
;********************************************
;	SUBROUTINE TO DISPLAY ON CONSOLE		*
;********************************************
conout:	push	psw
		push	b
		push	d
		push	h
		mov		e,a
		mvi		c,2
		call	bdos
		pop		h
		pop		d
		pop		b
		pop		psw
		ret
;
;
; Print termenited message pointed by DE
;
print:	push	psw
		push	b
		push	d
		push	h
		mvi		c,009H		;BDOS function PRINT$STRING
		call	bdos
		pop		h
		pop		d
		pop		b
		pop		psw
		ret
;
err0:
		db	'INVALID COMMAND LINE'
		db	0Dh,0Ah,'$'
err1:
		db	'INVALID DRIVE'
		db	0Dh,0Ah,'$'
err2:
		db	'NO DRIVE SPECIFIED'
		db	0Dh,0Ah,'$'
err3:
		db	'INVALID DRIVER LOAD ADDRESS'
		db	0Dh,0Ah,'$'

err4:	db	'NO ROOM ABOVE CP/M FOR DRIVER'
		db	0Dh,0Ah,'$'

err5:	db	'DRIVER ALREADY LOADED'
		db	0Dh,0Ah,'$'
;
; DATA	AREA
BIOS:	DW		0
UNIT:	DB		0FFh
HIMEM:	DW		000h
SIZE:	DW		0

MDRIVEL:	equ		$-MAIN

;
; M-DRIVE/H Driver
;
MODULE	ORG	500H
MDRIVE	DS		8*3
GO$BIOS:DS		8*3
DRIVE:	DS		2		;Drive, M-DISK/H flag
DPB:	DS		14
DPH:	DS		16
	end

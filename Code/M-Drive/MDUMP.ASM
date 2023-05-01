BDOS	equ		0005h

warm$boot	equ		0000h
secsize		equ		128				;Size of sector in bytes
spt			equ		8
DMABUF		equ		0080h
BUFDIR		equ		DMABUF
MEMDRV		equ		'M'
MRTRY		equ		10				;MAX RETRY COUNT
mdHOME		equ		00h
mdSELDSK	equ		03h
mdSETTRK	equ		06h
mdSETSEC	equ		09h
mdSETDMA	equ		0Ch
mdREAD		equ		0Fh
mdWRITE		equ		12h
	ORG	0100h
	lxi		sp,STACK
	lxi		h,MDRIVE
	shld	driver
	lxi		h,DRIVE
	mvi		m,MEMDRV-'A'
	inx		h
	mvi		m,80h
	lxi		h,4
	shld	trk
	lxi		h,0
	shld	sec
	lxi		h,DMABUF
	shld	dma

	mvi		c,MEMDRV-'A'
	mvi		b,0
	mvi		a,mdSELDSK		;Select drive
	call	DO$BIOS
	mov		a,h
	ora		l
	jz		SEL$ERR

	mvi		a,mdHOME		;Home
	call	DO$BIOS

	lhld	dma				;DMA buffer
	mov		b,h
	mov		c,l
	mvi		a,mdSETDMA		;Set DMA
	call	DO$BIOS

Nxt$T:
	lhld	trk				;Track #
	mov		b,h
	mov		c,l
	mvi		a,mdSETTRK		;Set Sector
	call	DO$BIOS

Nxt$S:
	lhld	sec				;Sector #
	mov		b,h
	mov		c,l
	mvi		a,mdSETSEC		;Read sector
	call	DO$BIOS

	nop
	nop
	nop
	mvi		a,mdREAD		;Read sector
	call	DO$BIOS
	nop
	nop
	nop
	ana		a
	jnz		RED$ERR
	nop
	nop
	nop
	lhld	dma
	call	Dump$hex
	lhld	sec
	inx		h
	shld	sec
	mov		a,l
	cpi		8
	jnz		Nxt$S

	call	crlf
	jmp		warm$boot			;warm boot

Dump$hex:
	lxi		d,Trk$num
	call	print
	lhld	trk
	mov		a,h
	call	Hexout
	mov		a,l
	call	Hexout
	lxi		d,Sec$num
	call	print
	lhld	sec
	mov		a,h
	call	Hexout
	mov		a,l
	call	Hexout

	mvi		c,128
	mvi		b,15
Dump$0:
	call	crlf
	mov		a,h
	call	Hexout
	mov		a,l
	call	HexoutB
	call	space
Dump$1:
	mov		a,m
	inx		h
	call	HexoutB
	dcr		c
	jz		crlf
	dcr		b
	jnz		Dump$1
	mvi		b,15
	jmp		Dump$0
;
; Entry: A=BIOS function
;
DO$BIOS:
	push	psw
	lxi		d,do$msg
	call	print
	mvi		a,'A'
	call	conout
	mvi		a,'='
	call	conout
	pop		psw
	push	psw
	call	HexoutB
	mvi		a,'B'
	call	conout
	mvi		a,'C'
	call	conout
	mvi		a,'='
	call	conout
	mov		a,b
	call	Hexout
	mov		a,c
	call	Hexout
	pop		psw
	lhld	DRIVER
	mov		l,a
	pchl			;INFO: index jump


crlf:
	mvi		a,0dh
	call	conout
	mvi		a,0ah
	jmp		conout
;
HexoutB:
	call	hexout
;	Fall into SPACE
;
space:
	mvi		a,' '
	jmp		conout
;
;********************************************
;	SUBROUTINE TO DISPLAY HEX NUMBER		*
;********************************************
Hexout:
	push	psw
	rar
	rar
	rar
	rar
	call	hexout1
	pop		psw
hexout1:
	ani		00FH
	sui		00AH
	jm		hexout2
	adi		007H
hexout2:
	adi		03AH
; Fall into conout
;
;********************************************
;	SUBROUTINE TO DISPLAY ON CONSOLE		*
;********************************************
conout:
	push	psw
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
print:
	push	psw
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

SEL$ERR:
	lxi		d,SEL$MSG
	jmp		bail
RED$ERR:
	lxi		d,RED$MSG
bail:
	call	print
	call	Hexout
	jmp		warm$boot
;
NOTHING:
	xra		a
	mov		h,a
	mov		l,a
	ret

SEL$MSG		db	0dh,0ah,'Select Error:','$'
RED$MSG		db	0dh,0ah,'Read Error:','$'
Do$msg:		db	0dh,0ah,'BIOS: $'
Trk$num		db	0dh,0ah,'Track: $'
Sec$num		db	0dh,0ah,'Sector: $'
driver:		dw	0
trk:		dw	0
sec:		dw	0
dma:		dw	0
			ds	40
stack		equ	$

		org		400h
HBASE:	EQU	0C6h
HDATA:	EQU	HBASE
HADDR:	EQU	HBASE+1
HMDSIB:	EQU	2048/secsize		;SECTORS IN MEMORY DRIVE DATA BLOCK
HMDDSM:	EQU	((512-4)*spt)/HMDSIB
;
MDRIVE:	JMP		HOME		;HOME
		JMP		SELDSK		;SELECT DISK
		JMP		SETTRK		;SET TRACK
		JMP		SETSEC		;SET SECTOR
		JMP		SETDMA		;SET DMA
		JMP		READ		;READ
		JMP		WRITE		;WRITE
		JMP		SECTRN		;SECT0R TRANSLATE
;
BIOSHOM:JMP		NOTHING		;HOME
BIOSSEL:JMP		NOTHING		;SELECT DISK
BIOSTRK:JMP		NOTHING		;SET TRACK
BIOSSEC:JMP		NOTHING		;SET SECTOR
BIOSDMA:JMP		NOTHING		;SET DMA
BIOSRED:JMP		NOTHING		;READ
BIOSWRT:JMP		NOTHING		;WRITE
BIOSTRN:JMP		NOTHING		;SECT0R TRANSLATE
;
DRIVE:	DW		MDISKL		;MEMDRV-'A'
							;HDRIVE TYPE FOR MFORM
							;DO NOT CHANGE VALUE OR LOCATION
;
;	Disk Parameter Block
;
HMD$DPB:					;SET FOR 1 BOARD, 8 SPT
		DW	spt				;SPT* (SECTORS PER TRACK, THIS AND DSM ...
							;...MUST BE INCREASED FOR MORE BOARDS)
		DB	4,15,1			;BSH, BLH, EXM
		DW	HMDDSM-1		;DSM*
		DW	128-1			;DRM
		DB	0C0h,00h		;AL0, AL1 (RESERVED DIR BLOCKS, MUST MATCH ...
							; ... NUMBER OF DIRECTORY ENTRIES)
		DW	0				;CKS (CHECK SUM), This is always zero.
		DW	4				;OFF (NUMBER OF RESERVED TRAGKS FOR PARITY CHECK
							;	THIS IS ALWAYS 4)
;
;	Disk Parameter Header
;
HMD$DPH:
		DW	0				;SECT0R TRANSLATE TABLE
		DW	0,0,0			;SCRATCH
		DW	BUFDIR			;DIRECT0RY BUFFER
		DW	HMD$DPB			;DPB BLOCK
		DW	0				;CHECKSUM VECTOR
		DW	ALV				;ALL0CATI0N VECTOR
;
;
;	S E L E C T   D I S K   D RI V E

;	Select the disk drive for subsequent disk transfers and return
;	the address of the DPH for the memory drive.
;	ENTRY -- C = disk selection value
;	  DE and l = 0 must determine disk type
;	           = 1 type already determined (only needed for floppies)
;
;	EXIT -- HL = DPH address of the memory drive.
;	        If memory drive not selected, control is passed to BIOS
;
SELDSK:	LXI		H,CIOPB+0
		XRA		A			;CLEAR A
		MOV		M,A			;CLEAR MEMORY DRIVE FLAG
		LDA		DRIVE		;
		SUB		C			;IS MEMORY DRIVE SELECTED
		JNZ		BIOSSEL		;IF NOT GO BACK TO BIOS
		CMA					;A=-1
		MOV		M,A			;SET MEMORY DRIVE AS SELECTED
		LXI		H,HMD$DPH	;GET DPH ADDRESS
		RET					;AND DONE
;
;	H O M E120
;	Set current track to 0
;
HOME:	LXI		H,0
		SHLD	CIOPB+2		;SET TRACK AS 0
		JMP		BIOSHOM		;ON TO BIOS
;
;	S  E  T   T R A C K
;
;	Set track number.  The track number 1s save for later use during
;	a disk transfer.  The track is save whether or not the memory disk
;	is currently selected.
;
;	ENTRY -- BC = Track number.
;
SETTRK:	MOV		L,C
		MOV		H,B
		SHLD	CIOPB+2
		JMP		BIOSTRK		;ON TO BIOS
;
;	S E T	S E C T 0 R
;
;	Set	sector number.  The sector number is save for later use during
;	a disk transfer.  The sector is saved whether or not the memory
;	disk 1s current1y selected.
;
;	ENTRY -- BC = Sector number.

SETSEC:	MOV		A,C
		STA		CIOPB+4		;Set sector
		JMP		BIOSSEC		;ON TO BIOS
;
;	S E T   D M A   A D D R E S S
;
;	Set the address for the next DMA transfer.
;	The DMA address is saved for later use during a disk transfer.
;	The address is saved whether or not the memory disk ia
;	currently selected.
;
;	ENTRY -- BC = Disk memory address.

SETDMA:	MOV		L,C
		MOV		H,B
		SHLD	DMAADR		;Set DMA
		JMP		BIOSDMA		;ON TO BIOS
;
;	S  E  C  T O R   T R A N S L A T E
;
;	Translate the sector number from logical to physical.
;
;	ENTRY	DE = 0, no translatlon required.
;           DE = Translation table address.
;           BC = Number to translate.
;
SECTRN:	MOV		A,D
		ORA		E
		RZ					;No translation needed.
		JMP		BIOSTRN		;Let BIOS translate.
;
;	R E A D
;
;	Read the currently selected track,sector from the currently
;	selected drive.
;
READ:	LDA		CIOPB+0		;SELFLG
		ORA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSRED		;ON TO BIOS
		CMA					;A=0 for READ
		STA		CIOPB+1		;TO COMMAND BUFFER
		JMP		MDFNL		;AND GO PROCESS
;
;	W R I T E
;
;	Write to the current1y selected drive at the current track
;	and sector.
;
WRITE:	LDA		CIOPB+0		;SELFLG
		ORA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSWRT		;ON TO BIOS
		STA		CIOPB+1		;A=-1 for WRITE
;	Fall into MDFNL
;
;	MDFNL - Hard memory disk final command processing
;	ENTRY -- CIOPB+0 = COMMAND
;			 CIOPB+2 = TRACK
;			 CIOPB+4 = SECTOR
;
MDFNL:	MVI		A,MRTRY		;Set retry count
MDFNL1:	STA		CIOPB+5
		LHLD	CIOPB+2		;GET TRACK
		DAD		H			;*2
		DAD		H			;*4
		DAD		H			;*8
		DAD		H			;*l6
		DAD		H			;*32
		DAD		H			;*64
		DAD		H			;*128
		CALL	SETMDRV		;SET HDRIVE BOARD VIA <A>,<HL>
		MVI		C, secsize	;<C> = 128 BYTE'S PER SECTOR
		MOV		B,C			; also into <B> for parity check
		LHLD	DMAADR		;DIRECTLY TO CPM BUFFER
		LDA		CIOPB+1		;GET COMMAND
		ANA		A			;A=0 for READ or -1 for WRITE
		JNZ		M$WRITE		;IF WRITE COMMAND JUMP
;
;		Fall into M$READ
;
;	GET A 128 BYTE BLOCK FROM M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C> IS NUMBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO PUT DATA
;
;	EXIT -- <A> = 0 No errors, 1 READ error.
M$READ:	IN		HDATA		;GET DATA BYTE
		MOV		M,A			;PUT TO DMA BLOCK
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		INX		H			;UP POINTER
		DCR		C			;C0UNT THIS MOVE
		JNZ		M$READ		;AND DO AGAIN
		CALL	PARITY		;D0 PARITY CHECK
		IN		HDATA		;GET PARITY BYTE
		SUB		B			; 0 IF NO ERROR
		RZ					;	THEN RETURN
		LDA		CIOPB+5		;GET RETRY COUNT
		DCR		A			;REPLACE WITH NEW COUNT
		JNZ		MDFNL1		;
		ORI		1			;Set ERROR flag
		RET					;RETURN WHEN DONE
;
;	PUT A BLOCK OF DATA INTO M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C>  IS NUNBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO GET DATA FROM
;
;	EXIT -- <A> = 0 No errors.
M$WRITE:MOV		A,M			;GET BYTE FROH BUFFER
		OUT		HDATA		;TO MDRIVE BOARD
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		INX		H
		DCR		C			;COUNT THIS MOVE
		JNZ		M$WRITE		;IF DONE WITH 128 OR <C> COUNT
		CALL	PARITY		;SELECT PARITY BYTE LOCATION
		MOV		A,B
		OUT		HDATA		;WRITE PARITY BYTE TO HEMORY DISK
		XRA		A			;AND SHOW NO ERROR
		RET					;RETURN
;
;	FIND LOCATION IF PARITY BYTE FOR TRACK IN CIOPB+2
;	AND THE SECTOR IN CIOPB+4
PARITY:	LHLD	CIOPB+2		;Get track

;	Set	M-DRIVE/H board to address In <A>,<HL>
SETMDRV:LDA		CIOPB+4		;Get sector, Starts at 0
		OUT		HADDR		;<A>: to M-DRIVE/H board
		MOV		A,H			;<H>: is always 0 or 1
		OUT		HADDR
		MOV		A,L			;<L>: Upper bit's picks track 0-3
		OUT		HADDR		; remainder picks sector and byte
		RET
;
MDISKL:	EQU		$-MDRIVE	;LENGTH OF MDRIVE BIOS
;
;	Flags and Buffers
;
;
;	CIOPB+0 = SELFLG
;	CIOPB+1 = COMMAND
;	CIOPB+2 = TRACK
;	CIOPB+4 = SECTOR
;	CIOPB+5 = MRETRY

CIOPB:	DS		8			;COMMAND BUFFER,TRACK,SECTOR
DMAADR:	DS		2			;ADDRESS OF DHA BUFFER
ALV:	DS		(((HMDDSM*spt)+7)/spt)+1

	END

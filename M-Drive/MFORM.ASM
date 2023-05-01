true	equ	-1
false	equ	not true
debug	equ	false

secsize		equ	128				;Size of sector in bytes
dirbytes	equ	32				;Size of dir entry
mdriveh		equ	80h				;Flag M-Drive/H

warm$boot	equ		0000h
bdos		equ		0005h
DMABUF		equ		0080h

;BIOS Offsets
CBOOT		equ		0000h
WBOOT		equ		cboot+03h
IOCONST		equ		cboot+06h
IOCONIN		equ		cboot+09h
IOCONOUT	equ		cboot+0Ch
IOLIST		equ		cboot+0fh
IOPUNCH		equ		cboot+12h
IOREADER	equ		cboot+15h
HOME		equ		cboot+18h
SELDSK		equ		cboot+1Bh
SETTRK		equ		cboot+1Eh
SETSEC		equ		cboot+21h
SETDMA		equ		cboot+24h
READ		equ		cboot+27h
WRITE		equ		cboot+2Ah
IOLISTST	equ		cboot+2Dh

	org		100H		;TPA
;
begin:
	lxi		sp,STACK
	lxi		h,DMABUF	;HL=Command line buffer
	push	h
if debug
	push	h
	mvi		m,2
	inx		h
	mvi		m,'M'
	inx		h
	mvi		m,'!'
	pop		h
endif
	mov		c,m			;C=byte count
	mvi		b,0
	inx		h
	dad		b
	mvi		m,'$'		;Mark END-OF-BUFFER
	pop		h
scan:
	inx		h
	dcr		c
	jm		WHT$DRV		;No more args, report error
	mov		a,m			;Fetch byte from buffer
	cpi		' '			;Skip blanks
	jz		scan
	cpi		'$'			;End of buffer?
	jz		WHT$DRV		;Yes, report error
	sui		'A'			;Convert from ASCII to binary
	cpi		010H		;Too large?
	jnc		INV$DRV		;Yes, report error
	sta		DRIVE		;Save as DRIVE
	inx		h			;Fetch next
	mov		a,m			;Byte from buffer
	cpi		'!'			;Force format flag?
	cnz		ASK$YES		;Nope, Ask for permission.
						;Sets FMT$FLAG = -1 if permission="Y"
	lda		DRIVE		;Recover DRIVE
	mov		c,a			;Into reg C
	mvi		a,SELDSK
	call	DO$BIOS		;BIOS
						;Return HL=0 if invalid drive else HL=DPH
	mov		a,h
	ora		l
	jz		NOT$MDRV	;Invalid DRIVE selected
	lxi		b,0000Ah	;BC=Offset to DPB in DPH
	dad		b			;HL=ptr to DPB
	mov		a,m
	inx		h
	mov		h,m
	mov		l,a			;HL=ptr to DPB
	dcx		h			;HL=ptr to mdrive$type
	mov		a,m			;Fetch M-Drive type flag
	cpi		mdriveh		;Is this hardware M-Drive?
	jc		NOT$MDRV	;Nope
	push	h			;Save ptr to mdrive$type
	lda		FMT$FLAG
	ora		a			;A= -1 if allowed to format drive
	jnz		FMT$MDRIVE	;Start unconditional drive format
	jmp		CHK$MDRIVE	;Test if drive is formatted
;
; M-Drive formatted signature
;
SIGNATURE:
	db	04CH
	db	04CH
	db	04FH
SIG$LEN	equ	$-SIGNATURE
;
; Check if M-Drive is already formatted
;
CHK$MDRIVE:
	mvi		c,000H		;BC=Sector number (0)
	mvi		a,SETSEC
	call	DO$BIOS		;BIOS
	mvi		c,003H		;BC=Track number (3)
	mvi		a,SETTRK
	call	DO$BIOS		;BIOS
	lxi		h,BLK		;HL=BUF-secsize
	shld	DMA			;Store DMA pointer
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA buffer
	call	DO$BIOS		;BIOS
	mvi		a,READ		;Read sector
	call	DO$BIOS		;BIOS
	jnz		FMT$MDRIVE	;Had a read error?
	lhld	DMA			;HL=DMA buffer pointer
	lxi	b,	secsize-SIG$LEN
	dad	b				;Point to signature area
	mvi		e,SIG$LEN	;E=Length of SIG
	lxi		b,SIGNATURE	;BC=Drive formatted SIG
TEST$SIG:
	ldax	b			;Fetch flag byte from SIG
	cmp		m			;Match signature byte?
	jnz		FMT$MDRIVE	;Nope, Safe to format M-Drive
	inx		h			;Next byte in Data block
	inx		b			;Next byte in SIG
	dcr		e			;Count bytes tested
	jnz		TEST$SIG	;All done?
	jmp		FMT$MDRV	;Report M-Drive is formatted
;
; Begin formatting M-Drive
;
FMT$MDRIVE:
	lxi		d,msg1
	call	print
	pop		h			;Restore ptr to mdrive$type
	inx		h			;Now point to DPB: SPT
	mov		e,m			;E=Low byte
	inx		h
	mov		d,m			;D=High byte
	xchg				;HL=SPT
	shld	SPT			;Save Sectors Per Track
	xchg				;HL=DPB
	lxi		b,00006H	;BC=Offset to DPB:DSM
	dad		b
	mov		e,m			;E=Low byte DPB:DSM
	inx		h
	mov		d,m			;D=High byte DPB:DSM
	xchg				;HL=DSM, DE=ptr to DPB:DSM+1
	inx		h			;HL=HL+1
	dad		h			;*2
	dad		h			;*4
	mov		l,h			;L=Value of H
	mvi		h,000H		;H=0
	dad		h			;*2
	dad		h			;*4
	dad		h			;*8
	dad		h			;*16
	shld	SIZE		;Save size of M-DRIVE/H
	xchg				;HL=DSM+1
	lxi		b,00005H	;BC=Offset to DPB:OFFSET
	dad		b
	mov		e,m			;E=Low byte DPB:OFFSET
	inx		h
	mov		d,m			;D=High byte DPB:OFFSET
	xchg				;HL=DPB:OFFSET, DE=DPB:OFFSET+2
	shld	OFFSET		;Save HL (reserved tracks)
	xchg				;HL=DPB:OFFSET+2
	lxi		b,00000H	;BC=0 (Sector number)
	lhld	SIZE		;HL=M-DRIVE/H size
	shld	DBLK		;Save size
	lxi		h,BLK		;HL=BUF-secsize
	shld	DMA			;Set DMA pointer
ReadLp:
	push	d			;Save Track number
	push	b			;Save Sector number
	mov		c,e
	mov		b,d
	mvi		a,SETTRK	;BC=Track number
	call	DO$BIOS		;BIOS
	pop		b			;Restore Sector number
	push	b			;Save Sector number
	mvi		a,SETSEC	;BC=Sector number
	call	DO$BIOS		;BIOS
	lhld	DMA			;HL=Start of Data Buffer
	lxi		b,secsize	;Size of sector
	dad		b			;HL=HL+secsize
	shld	DMA			;Save next Data Block pointer
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA BUFFER
	call	DO$BIOS		;BIOS
	mvi		a,READ		;READ sector
	call	DO$BIOS		;BIOS
	pop		b			;Restore Sector number
	pop		d			;Restore Track number
	inx		b			;Next Sector
	lhld	SPT			;Get Sectors Per Track
	stc					;Set carry flag=1
	mov		a,l
	sbb		c
	mov		a,h
	sbb		b			;Read Full Track?
	jnc		ReadNxt		;Nope, contine
	lxi		b,00000H	;Clear Sector number
	inx		d			;Next Track
ReadNxt:
	lhld	DBLK		;Curent Data block
	dcx		h			;Count block
	shld	DBLK		;Save Data blocks left
	mov		a,h
	ora		l			;All Data blocks done?
	jnz		ReadLp		;Nope, Continue
;
	lhld	SIZE		;HL=size M-DRIVE/H
	dad		h			;*2
	dad		h			;*4
	mov		b,h
	mov		c,l			;BC= count of blocks
	lxi		h,DIR		;HL=BUF-dirbytes
	lxi		d,dirbytes	;DE=Size of Directory Entry
FmtNxt:
	dad		d			;HL=HL+dirbytes
	mvi		m,0E5H		;Set block formatted
	dcx		b			;Count block done
	mov		a,c
	ora		b			;All blocks marked?
	jnz		FmtNxt		;Nope
;
	lhld	OFFSET
	xchg
	lxi		b,00000H	;BC=0
	lhld	SIZE		;Size of M-DRIVE/H
	shld	DBLK		;Set blocks counter
	lxi		h,BLK		;HL=BUF-secsize
	shld	DMA			;Set DMA pointer
WritLp:
	push	d			;Save Track number
	push	b			;Save Sector number
	mov		c,e
	mov		b,d
	mvi		a,SETTRK	;BC=Track number 01EH
	call	DO$BIOS		;BIOS
	pop		b			;Restore Sector number
	push	b			;Save Sector number
	mvi		a,SETSEC	;BC=Sector number 021H
	call	DO$BIOS		;BIOS
	lhld	DMA			;Get DMA pointer
	lxi		b,secsize	;Size of sector
	dad		b			;HL=HL+secsize
	shld	DMA			;Set new DMA pointer
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA buffer 024H
	call	DO$BIOS
	mvi		a,WRITE
	call	DO$BIOS		;BIOS
	pop		b			;Restore Sector number
	pop		d			;Restore Track number
	inx		b			;Next Sector
	lhld	SPT			;Get Sectors Per Track value
	stc					;Set carry flag=1
	mov		a,l
	sbb		c
	mov		a,h
	sbb		b
	jnc		WrtNxt		;Write Full Track?
	lxi		b,00000H	;Clear Sector number
	inx		d			;Next Track
WrtNxt:
	lhld	DBLK
	dcx		h
	shld	DBLK
	mov		a,h
	ora		l			;All blocks writen?
	jnz		WritLp		;Nope, continue
;
; Write SIGNATURE block to mark M-Drive formatted
;
	mvi		c,000H		;Set Sector = 0
	mvi		a,SETSEC	;BC=Sector number
	call	DO$BIOS		;BIOS
	mvi		c,003H		;Set Track  = 3
	mvi		a,SETTRK	;BC=Track number
	call	DO$BIOS
	lxi		h,BLK		;HL=BUF-secsize
	shld	DMA			;Set DMA pointer
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA buffer
	call	DO$BIOS
	mvi		a,READ
	call	DO$BIOS		;Read sector
	lhld	DMA
	lxi		b,secsize-SIG$LEN
	dad		b			;Point to signature area
	lxi		b,SIGNATURE
	mvi		e,SIG$LEN
SET$SIG:				;Store new signature block
	ldax	b
	mov		m,a
	inx		h
	inx		b
	dcr		e
	jnz		SET$SIG
	mvi		a,WRITE
	call	DO$BIOS
	jmp		EXIT		;Format complete, exit program
;
; Entry: A=BIOS function
;
DO$BIOS:
if debug
	xra		a
	lxi		h,DPH
	ret
endif ;debug
	lhld	warm$boot+1
	mov		l,a
	pchl			;INFO: index jump
;
; Ask user to enter "Y" on keyboard
; FMT$FLAG
;   0      User did NOT enter "Y"
;  255     User entered "Y"
;
ASK$YES:
	lxi		d,msg2		;DE=Message
	call	print		;Print message
WAIT:
	mvi		c,0006H		;BDOS function DIRECT$CONSOLE$I/O
	mvi		e,0ffH		;Input
	call	bdos		;A=0 No character
	ani		0DFH		;Force upper-case
	jz		WAIT
	cpi		'Y'			;Was it expected?
	jnz		EXIT		;No, exit program
	mvi		a,0FFH		;A=255
	sta		FMT$FLAG	;Set FMT$FLAG
	lxi		d,crlf
	jmp		print
;
FMT$FLAG:
	db	000H			;Default: 00
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
; Report No M-Drive support found
;
NOT$MDRV:
	lxi		d,err3		;DE=Message
	jmp		BAIL		;Print message and exit program
;
; Report M-Drive Formatted
;
FMT$MDRV:
	lxi		d,err4		;DE=Message
;
; Print Message (DE), Fall into EXIT
;
BAIL:
	call	print
;
; Exit program
;
EXIT:
	jmp		0000h		;BIOS: Warm Boot
;
err1:
	db	'INVALID DRIVE'
	db	0Dh,0Ah,'$'
err2:
	db	'NO DRIVE SPECIFIED'
	db	0Dh,0Ah,'$'
err3:
	db	'DRIVE SPECIFIED IS NOT A MEMORY DRIVE'
	db	0Dh,0Ah,'$'
err4:
	db	'MEMORY DRIVE IS ALREADY FORMATTED!!'
	db	0Dh,0Ah,'$'
msg1:
	db	0Dh,0Ah
	db	'FORMATTING MEMORY DRIVE'
	db	0Ah,0Dh,'$'
msg2:
	db	'CONFIRM DESTROY ALL FILES ON THE MEMORY DRIVE (Y or N)? '
	db	'$'
crlf:
	db	0Dh,0Ah,'$'

;
; Print termenited message pointed by DE
;
print:
	push	psw
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
; VARS
;
DRIVE:
	db	0
SIZE:
	dw	0
SPT:
	dw	0
DBLK:
	dw	0
OFFSET:
	dw	0
DMA:
	dw	0
	ds	40H
STACK:	equ	$
if debug
sib		equ	2048/secsize	;Sectors in memory drive data block
dsm		equ	((512-4)*8)/sib
; Boards	DSM						SIZE
;	1		(((512-4)*8)/SIB)-1		253
;	2		(((1024-4)*8)/SIB)-1	509
;	3		(((1536-4)*8)/SIB)-1	765
;	4		(((2048-4)*8)/SIB)-1	1021
;
;	Disk Parameter Block
;
		DB	mdriveh			;M-DRIVE/H type flag
DPB:						;SET FOR 1 BOARD, 8 SPT
		DW	8				;SPT (SECT0RS PER TRACK, THIS AND DSM ...
							;...HUST BE INCREASED FOR MORE BOARDS)
		DB	4,15,1			;BSH, BLH, EXM
		DW	DSM-1			;DSM-1
		DW	128-1			;DRM-1
		DB	0C0h,00h		;AL0, AL1 (RESERVED DIR BLOCKS, MUST MATCH ...
							; ... NUMBER OF DIRECTORY ENTRIES)
		DW	0				;CKS (CHECK SUM), This is always zero.
		DW	4				;OFF (NUMBER OF RESERVED TRAGKS FOR PARITY CHECK
							;	THIS IS ALWAYS 4)
;
;	Disk Parameter Header
;
DPH:
		DW	0				;SECT0R TRANSLATE TABLE
		DW	0,0,0			;SCRATCH
		DW	80h				;DIRECT0RY BUFFER
		DW	DPB				;DPB BLOCK
		DW	0				;CHECKSUM VECTOR
		DW	0				;ALL0CATI0N VECTOR
endif ;debug
;
BUF		org	500H
BLK:	equ	buf-secsize		;480H
DIR:	equ	buf-dirbytes	;4E0H
	end

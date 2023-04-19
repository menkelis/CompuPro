;
;	Disassembled by:
;		DASMx object code disassembler
;		(c) Copyright 1996-2003   Conquest Consultants
;		Version 1.40 (Oct 18 2003)
;
;	File:		mform.com
;
;	Size:		768 bytes
;	Checksum:	EC78
;	CRC-32:		151F475F
;
;	Date:		Tue Apr 18 11:13:04 2023
;
;	CPU:		Intel 8080 (MCS-80/85 family)
;
;
;
secsize		equ	128				;Size of sector in bytes
hmdsib		equ	2048/secsize	;Sectors in memory drive data block
hmddsm		equ	((512-4)*8)/hmdsib
mdriveh		equ	80h				;M-Drive/H

dph			org	0
			ds	2				;sector translate table vec
			ds	6				;scratch
			ds	2				;directory buffer
dph$dpb		ds	2				;DPB block vec
			ds	2				;checksum vec
			ds	2				;allocation vec
o$dpbtbl	equ		dph$dpb-dph	;Offset to DPB in DPH

dpb			org	0
mdrive$type	ds	1				;mdrive type
			ds	1				;sectors per track
			ds	3				;bsh, blm, exm
			ds	2		;Reserved dir blocks, ents
			ds	2
			ds	2				;check sum vector
			ds	2				;# of reserved tracks for parity check

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

	org	00100H
;
begin:
	lxi		sp,STACK
	lxi		h,DMABUF
	mov		c,m			;C=byte count
scan:
	inx		h
	dcr		c
	jm		WHT$DRV		;No more args, report error
	mov		a,m			;Fetch byte from buffer
	cpi		' '			;Skip blanks
	jz		scan
	cpi		'$'			;End of buffer?
	jz		WHT$DRV		;Yes, report error
	ani		05FH		;Force upper-case
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
	lxi		b,o$dpbtbl
	dad		b			;HL=ptr to DPB in DPH
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
	lxi		h,DATA$BLK	;HL=Data block
	shld	DMA			;Store address of Data block
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA buffer address
	call	DO$BIOS		;BIOS
	mvi		a,READ		;Read sector
	call	DO$BIOS		;BIOS
	lhld	DMA			;HL=Start of Data block
	lxi	b,	secsize-SIG$LEN	;0007DH	;BC=Offset
	dad	b				;HL=Offset into data block
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
; by first reading track
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
	shld	CUR$DBLK	;Save Data block offset (# DIR entries)
	xchg				;HL=DSM+1
	lxi		b,00005H	;BC=Offset to DPB:OFFSET
	dad		b
	mov		e,m			;E=Low byte DPB:OFFSET
	inx		h
	mov		d,m			;D=High byte DPB:OFFSET
	xchg				;HL=DPB:OFFSET, DE=DPB:OFFSET+2
	shld	OFFSET		;Save HL as offset
	xchg				;HL=DPB:OFFSET+2
	lxi		b,00000H	;BC=0 (Sector number)
	lhld	CUR$DBLK	;HL=Data block offset
	shld	END$DBLK	;Save as Curent Data block
	lxi		h,DATA$BLK	;HL=00480H
	shld	DMA			;Start of Data block
L01D0:
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
	lhld	DMA			;HL=Start of Data block
	lxi		b,secsize	;Size of sector
	dad		b			;HL=HL+secsize
	shld	DMA			;Save as Data block
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA BUFFER
	call	DO$BIOS		;BIOS
	mvi		a,READ		;READ sector
	call	DO$BIOS		;BIOS
	pop		b			;Restore Sector number
	pop		d			;Restore Track number
	inx		b			;Next Sector
	lhld	SPT			;Get Sectors Per Track value
	stc					;Set carry flag=1
	mov		a,l
	sbb		c
	mov		a,h
	sbb		b			;Read Full Track?
	jnc		L0208		;Nope, contine
	lxi		b,00000H	;Clear Sector number
	inx		d			;Next Track
L0208:
	lhld	END$DBLK	;Curent Data block
	dcx		h
	shld	END$DBLK	;Curent Data block
	mov		a,h
	ora		l			;All Data blocks done?
	jnz		L01D0		;Nope, Continue
;
	lhld	CUR$DBLK	;HL=Data block offset
	dad		h			;*2
	dad		h			;*4
	mov		b,h
	mov		c,l			;BC=HL
	lxi		h,DATA$DIR	;004E0H	;HL=Start of Directory Area
	lxi		d,32		;DE=Size of Directory Entry ;00020H
L0221:
	dad		d
	mvi		m,0E5H		;Empty sector fill value
	dcx		b
	mov		a,c
	ora		b			;BC=0?
	jnz		L0221		;Nope
;
	lhld	OFFSET
	xchg
	lxi		b,00000H	;BC=0
	lhld	CUR$DBLK
	shld	END$DBLK
	lxi		h,DATA$BLK	;00480H
	shld	DMA
L023D:
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
	lhld	DMA
	lxi		b,secsize	;Size of sector
	dad		b			;HL=HL+secsize
	shld	DMA
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
	jnc		L0275		;Write Full Track?
	lxi		b,00000H	;Clear Sector number
	inx		d			;Next Track
L0275:
	lhld	END$DBLK
	dcx		h
	shld	END$DBLK
	mov		a,h
	ora		l			;All blocks writen?
	jnz		L023D		;Nope, continue
;
; Write SIGNATURE block to mark M-Drive formatted
;
	mvi		c,000H		;Set Sector = 0
	mvi		a,SETSEC	;BC=Sector number 021H
	call	DO$BIOS		;BIOS
	mvi		c,003H		;Set Track = 3
	mvi		a,SETTRK	;BC=Track number 01EH
	call	DO$BIOS
	lxi		h,DATA$BLK	;00480H
	shld	DMA
	mov		b,h
	mov		c,l
	mvi		a,SETDMA	;BC=DMA buffer 024H
	call	DO$BIOS
	mvi		a,READ
	call	DO$BIOS		;BIOS
	lhld	DMA
	lxi		b,secsize-SIG$LEN	;0007DH
	dad		b
	lxi		b,SIGNATURE
	mvi		e,SIG$LEN
SET$SIG:
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
	mvi		a,IOCONIN	;Read console 009H
	call	DO$BIOS		;BIOS
	ani		0DFH		;Force upper-case
	cpi		'Y'			;Was it expected?
	jnz		EXIT		;No, exit program
	mvi		a,0FFH		;A=255
	sta		FMT$FLAG	;Set FMT$FLAG
	ret
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
	db	0Dh,0Ah,0
err2:
	db	'NO DRIVE SPECIFIED'
	db	0Dh,0Ah,0
err3:
	db	'DRIVE SPECIFIED IS NOT A MEMORY DRIVE'
	db	0Dh,0Ah,0
err4:
	db	'MEMORY DRIVE IS ALREADY FORMATTED!!'
	db	0Dh,0Ah,0
msg1:
	db	0Dh,0Ah
	db	'FORMATTING MEMORY DRIVE'
	db	0Ah,0Dh,0
msg2:
	db	'CONFIRM DESTROY ALL FILES ON THE MEMORY DRIVE (Y or N)? '
	db	0
;
; Print null termenited message pointed by DE
;
print:
	ldax	d			;Get character
	ora		a			;Test for null byte
	rz					;Yes, return
	inx		d			;Point to next character
	call	putchar		;Print to console
	jmp		print		;Continue
;
; Send character in <A> to console
; Preserve all regs
;
putchar:
	push	psw
	push	b
	push	d
	push	h
	mov		e,a
	mvi		c,002H		;BDOS function PRINT
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
CUR$DBLK:
	dw	0
END$DBLK:
	dw	0
SPT:
	dw	0
OFFSET:
	dw	0
DMA:
	dw	0
	org	0042DH
STACK	equ	$
	org	00480H
DATA$BLK	equ	$
	org	DATA$BLK + 60h
DATA$DIR	equ	$
	end

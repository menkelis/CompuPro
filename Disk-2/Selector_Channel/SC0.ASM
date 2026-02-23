; Boot EPROM
; 2716 with 8x256 byte boot programs.
; Boot	CPU		Console
; ----	----	-----
;  0	8080	Interfacer-1
;  1	8080	Bit-Bang
;  2	8080	System Support
;  3	8080	Interfacer-3,4
;  4	8088	Interfacer-1
;  5	8088	Bit-Bang
;  6	8088	System Support
;  7	8088	Interfacer-3,4

; Set Console Device Code
; 0=Interfacer-1
; 1=Bit Bang
; 2=System Support
; 3=Interfacer-3,4
CONSOLE	equ	0

;	CompuPro DISK 2 Equates:
;
D2$PORT	EQU	0C8h	;Base port address
D2$CNTL	EQU	D2$PORT	;Control port
D2$STAT	EQU	D2$PORT	;Status port is same as control
D2$DATA	EQU	D2$PORT+1	;Data port (I/O)
;
; Disk 2 Commands:
D2$READ	EQU	11001000b	;Read data command
D2$WRT	EQU	11010000b	;Write data command
D2$TIME	EQU	00000110b	;Time out
D2$STRB	EQU	10000000b	;Drive strobe
D2$HEAD	EQU	10010000b	;Head register
D2$CYL	EQU	10001000b	;Cylinder register
D2$SEC	EQU	10011000b	;Sector register
D2$SIN	EQU	10100000b	;Step in
D2$SOU	EQU	10000000b	;Step out
D2$RST	EQU	00000100b	;Reset drive command bit
D2$ATTN	EQU	10000000b	;Acknowledge end of state machine sequence (interrupt)
;
; Disk 2 Status Bits:
;  ATTN	EQU	10000000b	;State machine activity bit
D2$TOUT	EQ	 01000000b	;Timeout error bit
D2$CRC	EQU	00100000b	;CRC error on data sector bit
D2$OVR	EQU	00010000b	;Overrun on data transfer error bit
D2$NRDY	EQU	00001000b	;Drive not ready status bit
D2$SEKD	EQU	00000100b	;Seek done status bit
D2$WRTF	EQU	00000010b	;Write fault (negative true status)
D2$CYL0	EQU	00000001b	;Cylinder 0 status bit (negative true status)
;
; Selector Channel:
SELCHAN	EQU	0F0h		;Selector channel port
SELPRI	EQU	10			;Disk priority (as a DMA device, switches set)
SELBYT	EQU	2Fh-SELPRI	;Selector channel command byte
;
	org	00000H
BEGIN:
	lxi	sp,STACK 			;Load SP
	mvi	b,002H				;Set drive READY retry count
L0005:
	mvi	a,D2$STRB			;Select REG: DRIVE
	out	D2$CNTL				;Write to control port
	mvi	a,10H				;DRIVE=0
	out	D2$DATA				;Write data
	in	D2$STAT				;Read status
	ani	D2$ATTN+D2$NRDY		;BUSY* and NOT READY (Active low)
	cpi	D2$ATTN				;BUSY* cleared?
	jz	L002F				;Yes, Check track zero
	dcr	b					;Loop count completed?
	jnz	L0005				;No, try again
;
; Begin drive spin up time delay
	lxi	b,001F4H			;Set delay
	lxi	h,BEGIN				;ROM re-entry point
L0020:
	mvi	a,0C0H				;Set inner loop
L0022:
	dcx	b					;Waste some time
	inx	b					;Waste more time
	dcr	a					;Inner loop completed?
	jnz	L0022				;Nope
	dcx	b					;Count delay
	mov	a,c
	ora	b					;Delay done?
	jnz	L0020				;Nope
	pchl					;Jump to ROM re-entry
;
; Drive READY, Issue DRIVE RESET command
L002F:
	mvi	a,D2$RST			;Select REG: RESET
	out	D2$CNTL				;Write to control port
	in	D2$STAT				;Read status
	ani	D2$CYL0				;TRACK0 (Active low)
	jz	L004D				;Yes, At track zero
;
; Issue DRIVE RESET + STEP DIRECTION command
	mvi	a,D2$SOU+D2$RST		;Select REG:RESET + STEP Direction=OUT
	out	D2$CNTL				;Write to control port
	mvi	b,00H				;Set STEP pulse counter
L0040:
	in	D2$DATA				;Send STEP pulse
	dcr	b					;Count STEP pulse
	jnz	L0040				;Repeat until count=0
; Wait for stacked STEP's to complete
L0046:
	in	D2$STAT				;Read status
	ani	D2$SEKD				;Seek Done (Active low)
	jnz	L0046				;No if status bit high
;
; Drive is at Track-0
L004D:
	mvi	b,00AH				;Set READ data RETRY count
;
; Select DRIVE0,CYL=0,HEAD=0,SECTOR=0
L004F:
	mvi	a,D2$STRB			;Select REG: DRIVE
	out	D2$CNTL				;Write to control port
	mvi	a,10H				;Drive=0
	out	D2$DATA				;Write data

	mvi	a,D2$CYL			;Select REG: CYL
	out	D2$CNTL				;Write to control port
	xra	a					;CYL=0
	out	D2$DATA				;Write data

	mvi	a,D2$HEAD			;Select REG: HEAD
	out	D2$CNTL				;Write to control port
	xra	a					;HEAD=0
	out	D2$DATA				;Write data

	mvi	a,D2$SEC			;Select REG: SECTOR
	out	D2$CNTL				;Write to control port
	xra	a					;SECTOR=0
	out	D2$DATA				;Write data
;
; Program Selector Channel
	lxi	h,0100H				;DMA Address of 2nd loader
	in	SELCHAN				;Hold Selector Channel
	xra	a					;Extended Address Byte: A23-A16
	out	SELCHAN				;Write data
	mov	a,h					;High Address Byte: A15-A8
	out	SELCHAN				;Write data
	mov	a,l					;Low Address Byte: A7-A0
	out	SELCHAN				;Write data
	mvi	a,1010$0101B		;Mode:1010  PRI:0101 (A5)
	out	SELCHAN				;Write data
;
; Tell Disk-2 GO
	mvi	a,D2$READ			;Command: Read Data
	out	D2$CNTL				;Write to control port
;
; Wait for command compleation
L0082:
	in	D2$STAT				;Read status
	ani	D2$ATTN				;Busy* (Active low)
	jnz	L0082				;Wait for low
; Get ERROR status
	in	D2$STAT				;Read status
	ani	D2$TOUT+D2$OVR		;Check error bits: TIMEOUT and OVERRUN
	jz	L0097				;No read errors?
; READ Error detected
	dcr	b					;Update retry counter
	jnz	L004F				;OK to retry?
	jmp	BEGIN				;No, ROM re-entry
;
; Set CONSOLE port flag, Start 2nd loader.
L0097:
	mvi	c,CONSOLE			;Console device
	call	0100H			;Pass to 2nd loader
	DW		0,0,0,0,0		;Stack space
STACK	equ	$
	END

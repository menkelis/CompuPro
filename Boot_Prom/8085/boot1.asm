; PROGRAM NAME:	BOOT1.ASM
;
;	Boot: 0	Attempt to boot 8" drive 0, if not ready attempt to boot from DISK3.
;	Boot: 1	Always boot from DISK3.
;	Boot: 2 Attempt to boot 8" drive 0, if not ready attempt to boot 5.25" drive 2.
;	Boot: 3 Attempt to boot 5.25" drive 2, if not ready attempt to boot from DISK3.
;
; PURPOSE:	2nd routine for Disk 1A boot rom:  Hard Disk initial program loader
;		(IPL), which consequently will load the remainder of the operating system
;		upon execution entry.
;
; Disk layout Definition.
;
;  Cylinder 0,  Head 0,  hard disk = 9 X 1024 byte sectors.
;
;	The code is loaded starting XBOOT and may be
; any type of secondary loader or the runtime operating system itself.
;
;
; EQUATED CONSTANTS:
;
ENTRY	EQU	100h			;Org address (= 0, for ROM)
HIRAM	EQU	8000h			;RAM offset of ROM so loader doesn't overlay 2nd page
RBOOT	SET	ENTRY+ROM+HIRAM	;end of PROM in RAM
XBOOT	EQU	100h			;Base Location of second phase loader in RAM
MAP		EQU	XBOOT+400h		;allocation map address for bad DISK3 sectors

; Drive specific constants. (Set to handle nearly any drive)
SRT		EQU	16 - 10			;10 millisecond step rate for controller value
HUT		EQU	240 / 16		;Head unload delay time in milliseconds (240)
HDLT	EQU	(35 + 1) / 2	;Head load time in milliseconds (35)
;
; controller port for Disk 1A at 0C0h
FDPORT	EQU	0C0h
NS$128	EQU	26				; 128 byte sectors / track	-- 3.25K
NS$256	EQU	26				; 256 byte sectors / track	-- 6.5K
NS$512	EQU	15				; 512 byte sectors / track	-- 7.5K
NS1024	EQU	 8				;1024 byte sectors / track	-- 8K bytes total
;
FDCS	equ	00h
FDCD	equ	01h
FDMA	equ	02h
INTS	equ	02h
FDON	equ	03h
FD$SPEC	equ	03h
FD$RDAT	equ	06h
FD$RECA	equ	07h
FD$EOC	equ	80h
FD$RSTS	equ	08h
;
;	DISK3 equates follow:
D3PORT	EQU 90h				; DISK3 IO port
D3NOP	EQU	0				; DISK3 no-op command
D3RDFL	EQU 1				; DISK3 read flag
D3GLBAL	EQU	2				; DISK3 global command
D3SPEC	EQU	3				; DISK3 specify command
D3MAP	EQU	4				; DISK3 bad map command
D3HOME	EQU	5				; DISK3 home command
D3RWCMD	EQU	8				; DISK3 read/write command
;
;	ROM Bootstrap loader.

;NOTE: BOOTSTRAP IS LOADED INTO
;      AND RUNS AT LOCATION 8000

	org	ENTRY				;Base address of bootstrap PROM
;
	nop
	nop
	nop
	nop
	lxi		sp,HIRAM
	lxi		b,04000H		;Init delay count
WAIT	equ	$-ENTRY
	xthl					;Harmless time comsuming instructions
	xthl
	dcx		b				;Bump count
	mov		a,b
	ora		c				;Test if done
	jnz		WAIT			;Loop until time passes
;
; Read ROM and write to RAM
	lxi		h,RBOOT-HIRAM-ENTRY	;Point to end of ROM
	lxi		d,RBOOT-ENTRY		;Point to end of RAM
ROMOVE	equ	$-ENTRY
	mov		a,m				;Read ROM byte
	stax	d				;Write to RAM
	dcx		d				;Point to next byte
	dcx		h				;Bump count
	mov		a,l
	ora		h
	jnz		ROMOVE			;Loop if more bytes left to move
;
; Load DISK3 IOPB address vector with first IOPB address
	lxi		d,5DH			;DISK3 IOPB address vector
	mvi		a,IOPB1 and 0FFh;LOB of first IOPB
	stax	d				;Write to vector
	inx		d				;Point to next byte
	mvi		a,IOPB1/256		;HOB of first IOPB
	stax	d				;Write to vector
	inx		d				;Point to next byte
	mvi		a,0				;Extended address offset
	stax	d				;Write to vector
	lxi		d,50H			;DISK3 Command byte
	stax	d				;Write D3NOP
	inx		d				;Point to Status
	inx		d				;Point to Drive
	stax	d				;Write 0
	jmp		START
;
; BOOT
START	equ	$+HIRAM-ENTRY
	MVI		A,0FEh			;disable ROM, enable RAM
	OUT		FDPORT+FDON
	mvi		a,1				;DISK3 board reset byte
	out		D3PORT			;Send it
	xra		a				;DISK3 ATTENTION byte
	sta		IOPB1S			;Clear STATUS
	out		D3PORT			;Send it
;
	lxi		b,01400H		;Init delay count
DELAY1	equ	$+HIRAM-ENTRY
	dcx		b				;Bump count
	mov		a,b
	ora		c				;Test if done
	jnz		DELAY1			;Loop until time passes
	lda		IOPB1S			;Get STATUS
	ora		a				;Check if DISK3 present
	jm		DOHBT			;Successful no-op, continue with hardboot
	xra		a
	sta		IOPB1S			;Clear STATUS
	out		D3PORT			;DISK2 ATTENTION byte
;
	lxi		b,01400H		;Init delay count
DELAY2	equ	$+HIRAM-ENTRY
	dcx		b				;Bump count
	mov		a,b
	ora		c				;Test if done
	jnz		DELAY2			;Loop until time passes
	lda		IOPB1S			;Get STATUS
	ora		a				;Check if DISK3 present
	jm		DOHBT			;Successful no-op, continue with hard boot
	jmp		START			;Try again
;
; Execute DISK3 command subroutine
EXECUT	equ	$+HIRAM-ENTRY
	xra		a				;Null byte
	stax	d				;Clear STATUS
	out		D3PORT			;Nudge DISK3
XX1		equ	$+HIRAM-ENTRY
	ldax	d				;Get STATUS
	ora		a				;Check if BUSY
	jz		XX1				;If so, try again
	rlc						;Test ERROR bit
	rm						;No error
	pop		b				;Otherwise, fix stack
	jmp		START			;And try again
;
; Continue hard boot
DOHBT	equ	$+HIRAM-ENTRY
	lxi		d,IOPB2S		;Point to global status byte
	call	EXECUT			;Do global command

	lxi		d,IOPB3S		;Point to home status byte
	call	EXECUT			;Do home head command

	lxi		d,IOPB4S		;Point to read marker status byte
	lhld	IOPB4+10		;Point to data address for format check
	call	EXECUT			;Do read command
	lxi		d,COMP			;Point to 'Compupro'
	mvi		b,4				;Compare 4 characters
TEST	equ	$+HIRAM-ENTRY
	ldax	d				;'COMP' character in A
	cmp		m				;Compare header character
	jnz		START			;If not equal, then DISK3 not formatted
	inx		h				;Point to next header byte
	inx		d				;Point to next 'Compupro' byte
	dcr		b				;Bump count
	jnz		TEST			;More characters?

	lxi		d,IOPB5S		;Point to specify status byte
	call	EXECUT			;Do specify parameter command

	lxi		d,IOPB6S		;Point to bad map status byte
	call	EXECUT			;Do bad map command

	lxi		d,IOPB7S		;Point to read sectors status byte
	lhld	IOPB7+10		;Point to data address for loader check
	call	EXECUT			;Do read command
	mov		a,m				;Possible loader byte
	cpi		0E5H			;Is loader there, or merely formatted?
	jz		START			;No loader found, try again
;
; successful hard boot!
;
;************************************************
;*	EXIT ON SUCCESSFUL READ OPERATION	*
;************************************************
;
	in		FDPORT+INTS		;Get sense switch value
	rrc
	rrc						;divide by 4
	ani		1				;mask 4
	ori		2				;add 2 for boot switch value
	mov		c,a				;Boot switch value passed to loader
	jmp		XBOOT			;second phase loader
;	org	XBOOT				;Cold boot code starts in RAM here
	db	'RR2'
;
;************************************************
;*	FIXED STORAGE FOR DISK PARAMETERS	*
;************************************************
	ORG	($ and 0FFF0h) + 16	;Next Segment boundary
;
COMP	equ	$+HIRAM-ENTRY
	db	'Comp'
;
IOPB1	equ	$+HIRAM-ENTRY
	db	D3NOP				;DISK3 present check
IOPB1S	equ	$+HIRAM-ENTRY
	db	0,0,0,0,0,0,0,0,0,0,0,0
	db	IOPB2 and 0FFh,IOPB2/256,0
IOPB2	equ	$+HIRAM-ENTRY
	db	D3GLBAL				;Global information
IOPB2S	equ	$+HIRAM-ENTRY
	db	0,0,0,0,0,0,0,0,0,0,0,0
	db	IOPB3 and 0FFh,IOPB3/256,0
IOPB3	equ	$+HIRAM-ENTRY
	db	D3HOME				;Home head
IOPB3S	equ	$+HIRAM-ENTRY
	db	0,0,0,0,0,0,0,0,0,0,0,0
	db	IOPB4 and 0FFh,IOPB4/256,0
IOPB4	EQU	$+HIRAM-ENTRY
	DB	D3RWCMD				;read marker into XBOOT
IOPB4S	EQU	$+HIRAM-ENTRY
	DB	0,0,D3RDFL,0,0,0,0,2,0
	db	XBOOT and 0FFh,XBOOT/256,0
	DB	IOPB5 and 0FFh,IOPB5/256,0
IOPB5	EQU	$+HIRAM-ENTRY
	DB	D3SPEC				;specify parameters
IOPB5S	EQU	$+HIRAM-ENTRY
 	DB	0,0,0,0,0,0,0,0,0
	db	XBOOT+10h and 0FFh,XBOOT/256,0
	DB	IOPB6 and 0FFh,IOPB6/256,0
IOPB6	EQU	$+HIRAM-ENTRY
	DB	D3MAP				;allocation map for bad sectors
IOPB6S	EQU	$+HIRAM-ENTRY
	DB	0,0,0,0,0,0,0,0,0
	db	MAP and 0FFh,MAP/256,0
	DB	IOPB7 and 0FFh,IOPB7/256,0
IOPB7	EQU	$+HIRAM-ENTRY
	DB	D3RWCMD				;read 9 1024-B sectors into XBOOT
IOPB7S	EQU	$+HIRAM-ENTRY
	DB	0,0,D3RDFL,0,0,0,0,9,0
	db	XBOOT and 0FFh,XBOOT/256,0
	db	50h,0,0
;
	ORG	($ and 0FFF0h) + 16	;Next Segment boundary
ROM	end

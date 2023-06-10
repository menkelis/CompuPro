; PROGRAM NAME:	BOOT2.ASM
;
;	Boot: 0	Attempt to boot 8" drive 0, if not ready attempt to boot from DISK3.
;	Boot: 1	Always boot from DISK3.
;	Boot: 2 Attempt to boot 8" drive 0, if not ready attempt to boot 5.25" drive 2.
;	Boot: 3 Attempt to boot 5.25" drive 2, if not ready attempt to boot from DISK3.
;
; PURPOSE:	1st routine for Disk 1A boot rom:  8 inch Floppy Disk or Hard
;		Disk initial program loader (IPL), which consequently will load
;		the remainder of the operating system upon execution entry.
;
; Disk layout Definition.
;
;  Cylinder 0,  Head 0,  8 inch disk = 26 X 128 byte single density sectors.
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

	ORG	ENTRY				;Base address of bootstrap PROM
;
	NOP
	NOP
	NOP
	NOP						;8080 power up delay opcodes
	lxi		sp,HIRAM
INIT:
	MVI		A,0FFh			;Turn on the disk motors, leave PROM active
	OUT		FDPORT+FDON
;
; Loop on total error to this point.
	LXI		B,4000h			;Init delay count
WAIT	EQU $-ENTRY
	XTHL
	XTHL					;Harmless time consuming instructions
	DCX		B
	MOV		A,B
	ORA 	C				;Bump count, test if done
	JNZ		WAIT			;Loop until the time passes
; read ROM and write to RAM
	LXI		H,RBOOT-HIRAM-ENTRY	;point to end of ROM
	LXI		D,RBOOT-ENTRY		;point to end of RAM
ROMOVE	EQU	$-ENTRY
	MOV		A,M				;read ROM byte
	STAX	D				;write to RAM
	DCX		D				;point to next byte
	DCX 	H
	MOV		A,L
	ORA 	H				;bump count (also pointer)
	JNZ 	ROMOVE			;loop if more bytes left to move
	JMP		START			;jump to code located in high RAM
;
; Delay subroutine
DELAY	EQU	$+HIRAM-ENTRY	; delay subroutine
	LXI		B,0C000h		;Init delay count
DELAY0	EQU	$+HIRAM-ENTRY
	XTHL
	XTHL					;Let command settle
	DCX 	B
	MOV 	A,B
	ORA 	C				;Bump count, test if done
	JNZ		DELAY0			;Loop until the time passes
	RET
;
; BOOT
START	EQU	$+HIRAM-ENTRY
	lxi		d,DRV$8			;Point to 8" drive data block
START1	EQU	$+HIRAM-ENTRY
	MVI		A,0FEh			;disable ROM, enable RAM
	OUT		FDPORT+FDON
	ldax	d				;Fetch drive select byte
	OUT		FDPORT+FDCS		;Send to controller
	call	DELAY			;Switch settle time
;
; Load Specify Command.
	inx	d					;Point to specify command sequence
	mvi	b,LSPEC				;Length in "B"
SPEC1	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;See if ready to accept next byte
	ORA		A
	JP		SPEC1			;Wait if not ready
	LDAX	D				;Load command byte
	OUT 	FDPORT+FDCD		;to controller command port
	INX		D				;Point to next byte
	DCR		B				;Bump count until all loaded
	JNZ		SPEC1			;Loop if more to do
	call	DELAY
;
; Recalibrate drive.
	MVI		B,LRECAL		;Length of command in "B"
RCAL1	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;See if ready to accept next byte
	ORA		A
	JP		RCAL1			;Wait if not ready
	LDAX	D				;Load command byte
	OUT 	FDPORT+FDCD		;to controller command port
	INX		D				;Point to next byte
	DCR		B				;Bump count until all loaded
	JNZ		RCAL1			;Loop if more to command load
;
RCAL2	EQU	$+HIRAM-ENTRY
	IN		FDPORT+INTS		;See if command execution completed
	ORA		A
	JP		RCAL2			;Wait if not
;
; Verify successful completion of recalibrate command.
	MVI		A,FD$RSTS		;"Read Status" command
	OUT		FDPORT+FDCD		; to controller
	CALL	DELAY			; blink drive light
RCAL3	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;See if command accepted
	ORA		A
	JP		RCAL3			;Loop until ready
	IN		FDPORT+FDCD		;Get first resulting "Status Byte"
	XRI		20h				;Flip "drive ready" status bit
	MOV		C,A				;Put result in "C"
;
RCAL4	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;Get command execution status again
	ORA		A
	JP		RCAL4			;See if seek complete
	IN		FDPORT+FDCD		;Get second "execution" result byte
	ORA		C				;Combine the two result status bytes
	JNZ		D5BOOT			;Loop to 5.25 drive 2 boot if error
;
; Execute read operation sequence until loaded successfully.
	MVI		B,LDMA			;Length of DMA data
;
; Output beginning DMA address.
ADDR	EQU	$+HIRAM-ENTRY
	LDAX	D				;Get byte of extended address
	OUT		FDPORT+FDMA		;Send to DMA port of DISK 1
	INX		D				;Next byte to xfer
	DCR		B				;Bump count
	JNZ		ADDR			;Loop until all 3 bytes loaded
;
; Read all the data on track 0 in to get the cold boot loader.
	MVI		B,LREAD			;Load "B" with command lenght
READ1	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;Get controller status
	ORA		A
	JP		READ1			;Wait until controller ready for another byte
	LDAX	D				;load command byte
	OUT 	FDPORT+FDCD		; to controller
	INX		D				;Point to next byte to load
	DCR		B				;Bump command load count
	JNZ		READ1			;Loop if more bytes to load
;
READ2	EQU	$+HIRAM-ENTRY
	IN		FDPORT+INTS		;See if interrupt active (command complete)
	ORA		A
	JP		READ2			;Loop until so
;
READ3	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;See if ready to read status
	ORA		A
	JP		READ3			;Wait if not
	IN		FDPORT+FDCD		;Get resulting status of read operation
	SUI		40h				;Remove "abnormal ending" status bit
	ani		0FCH			;Mask bits
	MOV		H,A				;Put result in "H"
READ4	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;Get second status byte
	ORA		A
	JP		READ4			;Loop until ready
	IN		FDPORT+FDCD		;Second status byte in "A"
	XRI		FD$EOC			;Remove "End of Cylinder" bit
	MOV		L,A				;Put result in "L"
;
	MVI		B,7-2			;Count of remaining status bytes (ignored)
READ5	EQU	$+HIRAM-ENTRY
	IN		FDPORT+FDCS		;See if controller ready
	ORA		A
	JP		READ5			;Loop if not
	IN		FDPORT+FDCD		;Read status byte, ignore it
	DCR		B				;Bump remaining count
	JNZ		READ5			;Wait until all done
; If "Abnormal Ending" caused by "End of Cylinder" error, then read is valid.
	MOV		A,L
	ORA		H				;Combine the two significant status bytes
	JZ		BOOTVEC			;Exit from boot PROM if successful
							;otherwise, fall through to 5.25" drive 2 boot

D5BOOT	equ	$+HIRAM-ENTRY
	mov		a,e
	cpi		DATA5 and 0FFh	;low DATA5 = 018H
	jz		START			;Yes, retry 8" drive
	cpi		LAST5 and 0FFh	;low LAST5 = 024H
	jz		START			;Yes, retry 8" drive
	lxi		d,DRV$5			;Point to 5.25" drive data block
	jmp		START1			;Try 5.25" drive
;
;************************************************
;*	EXIT ON SUCCESSFUL READ OPERATION	*
;************************************************
;
BOOTVEC	EQU	$+HIRAM-ENTRY
	IN		FDPORT+INTS		;sense switch value
							;if bit 4 on then Interfacer 3/4
							;if bit 4 off then SS
	RRC
	RRC						;divide by 4
	ANI		1				;mask 4
	ORI		2				;add 2 for boot switch value
	MOV		C,A				;Boot switch value passed to loader
	JMP		XBOOT			;second phase loader
;	org	XBOOT				;Cold boot code starts in RAM here
	db	'RR3'
;
;************************************************
;*	FIXED STORAGE FOR DISK PARAMETERS	*
;************************************************
	ORG	($ and 0FFF0h) + 16	;Next Segment boundary
;
DRV$8	equ	$+HIRAM-ENTRY
	db	000H				;Select 8" drive
;
SPEC	EQU	$+HIRAM-ENTRY
	DB	FD$SPEC				;Floppy disk specification command
	DB	08Fh
	DB	HDLT SHL 1
LSPEC	EQU	HIRAM+$-ENTRY-SPEC	;Length of specification sequence
;
RECAL	EQU	$+HIRAM-ENTRY
	DB	FD$RECA				;Recalibrate command (home to track 0)
	DB	0					;Drive 0
LRECAL	EQU	HIRAM+$-ENTRY-RECAL	;Command length
;
; Function data for controller to boot
DATA	EQU	$+HIRAM-ENTRY
	DB	0					;Extended Address
	DB	XBOOT/256			;Base address of BOOT loader
	DB	XBOOT and 0FFh
LDMA	EQU	HIRAM+$-ENTRY-DATA	;Length of DMA to load
;
; Try to read disk as single density 128 byte sectors.
READ8	EQU	$+HIRAM-ENTRY
	DB	FD$RDAT				;Read sector(s) command for 8272 controller
	DB	0					;Head select, Drive select = 0
	DB	0					;Cylinder #0
	DB	0					;Head #0
	DB	1					;Starting Record (sector)
	DB	0					;"N" parameter (128 byte sectors)
	DB	NS$128				;Read to end of track = 3.25K bytes
	DB	07h					;GPL (Gap length)
	DB	128					;DTL (Data length)
LREAD	EQU	HIRAM+$-ENTRY-READ8	;Length of parameter block to xmit
;
DRV$5	EQU	$+HIRAM-ENTRY
	DB	028H				;Select 5.25" drive
SPEC5	EQU	$+HIRAM-ENTRY
	DB	FD$SPEC				;Floppy disk specification command
	DB	0DFh
	DB	01EH
RECAL5	EQU	$+HIRAM-ENTRY
	DB	FD$RECA				;Recalibrate command (home to track 0)
	DB	002H				;Drive 2
DATA5	EQU	$+HIRAM-ENTRY
	DB	0					;Extended Address
	DB	XBOOT/256			;Base address of BOOT loader
	DB	XBOOT and 0FFh
READ	EQU	$+HIRAM-ENTRY
	DB	FD$RDAT				;Read sector(s) command for 8272 controller
	DB	0					;Head select, Drive select = 0
	DB	0					;Cylinder #0
	DB	0					;Head #0
	DB	1					;Starting Record (sector)
	DB	0					;"N" parameter (128 byte sectors)
	DB	NS$128				;Read to end of track = 3.25K bytes
	DB	07h					;GPL (Gap length)
	DB	128					;DTL (Data length)
LAST5	EQU	$+HIRAM-ENTRY
;
	ORG	($ and 0FFF0h) + 16	;Next Segment boundary
ROM	end
